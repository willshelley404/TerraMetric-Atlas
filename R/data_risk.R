# ─────────────────────────────────────────────────────────────────────────────
# R/data_risk.R — data fetchers and cache for the Regional Risk map
# ─────────────────────────────────────────────────────────────────────────────

# ── Cache: process memory + .rds on disk, per-key TTL, stale-on-error ────────
.RISK_CACHE_ENV <- new.env(parent = emptyenv())

risk_cache_dir <- function() {
  d <- tryCatch({
    p <- tools::R_user_dir("TerraMetricAtlas", "cache")
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(p)) p else NULL
  }, error = function(e) NULL)
  d %||% tempdir()
}

risk_cache_clear_memory <- function() {
  rm(list = ls(.RISK_CACHE_ENV, all.names = TRUE), envir = .RISK_CACHE_ENV)
  invisible(NULL)
}

.risk_cache_out <- function(entry, stale) {
  v <- entry$value
  attr(v, "fetched_at") <- entry$fetched_at
  attr(v, "stale") <- stale
  v
}

#' Return a cached value, refreshing it via fetch_fn() when older than ttl_days.
#' A failed refresh (error or NULL) falls back to the expired copy, flagged stale.
#' A value may carry attr "ttl_days" to shorten its own lifetime (e.g. a partial
#' pull). fail_ttl_days > 0 remembers a failure with no fallback in process
#' memory, so later sessions skip the fetch until it expires.
risk_cached <- function(key, ttl_days, fetch_fn, cache_dir = risk_cache_dir(),
                        now = Sys.time(), fail_ttl_days = 0) {
  path <- file.path(cache_dir, paste0(key, ".rds"))
  age <- function(t) as.numeric(difftime(now, t, units = "days"))
  fresh <- function(e) !is.null(e) && !is.null(e$value) &&
    age(e$fetched_at) < (attr(e$value, "ttl_days") %||% ttl_days)

  fail_key <- paste0(key, "__failed")
  failed_at <- .RISK_CACHE_ENV[[fail_key]]
  entry <- .RISK_CACHE_ENV[[key]]
  if (fresh(entry)) return(.risk_cache_out(entry, FALSE))
  if (is.null(entry) && !is.null(failed_at) && age(failed_at) < fail_ttl_days) return(NULL)

  if (is.null(entry) && file.exists(path)) {
    entry <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(entry)) assign(key, entry, envir = .RISK_CACHE_ENV)
    if (fresh(entry)) return(.risk_cache_out(entry, FALSE))
  }

  value <- tryCatch(fetch_fn(), error = function(e) {
    message(sprintf("[risk cache] %s refresh failed: %s", key, conditionMessage(e)))
    NULL
  })
  if (!is.null(value)) {
    new_entry <- list(value = value, fetched_at = now)
    assign(key, new_entry, envir = .RISK_CACHE_ENV)
    if (exists(fail_key, envir = .RISK_CACHE_ENV, inherits = FALSE)) rm(list = fail_key, envir = .RISK_CACHE_ENV)
    tryCatch(saveRDS(new_entry, path), error = function(e)
      message(sprintf("[risk cache] could not write %s: %s", path, conditionMessage(e))))
    return(.risk_cache_out(new_entry, FALSE))
  }

  if (!is.null(entry)) return(.risk_cache_out(entry, TRUE))
  if (fail_ttl_days > 0) assign(fail_key, now, envir = .RISK_CACHE_ENV)
  NULL
}

# ── ACS year: newest 5-year release available, from the calendar ────────────
.acs_preferred_year <- function(today = Sys.Date()) as.integer(format(today, "%Y")) - 1L

#' Newest year in preferred..preferred-3 for which probe(year) is TRUE, else NULL.
.probe_acs_year <- function(preferred, probe) {
  for (yr in preferred - 0:3) {
    if (isTRUE(tryCatch(probe(yr), error = function(e) FALSE))) return(as.integer(yr))
  }
  NULL
}

.acs_probe_census <- function(yr) {
  suppressMessages(suppressWarnings(tidycensus::get_acs(
    geography = "us", variables = "B01003_001", year = yr, survey = "acs5")))
  TRUE
}

#' ACS 5-year vintage to use. A successful probe is cached 30 days; a failed one
#' is not cached (falls back to current year - 2 for this call only).
risk_acs_year <- function(probe = .acs_probe_census, cache_dir = risk_cache_dir(),
                          today = Sys.Date()) {
  y <- risk_cached("acs_year_v2", 30, cache_dir = cache_dir, fetch_fn = function() {
    .probe_acs_year(.acs_preferred_year(today), probe)
  })
  if (is.null(y)) as.integer(format(today, "%Y")) - 2L else as.integer(y)
}

# ── BLS LAUS county unemployment rate (API v2) ───────────────────────────────
.BLS_TS_URL <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

laus_series_ids <- function(fips) paste0("LAUCN", fips, "0000000003")

#' Tidy a BLS Results$series list: monthly rows with numeric values only.
parse_bls_laus <- function(results) {
  rows <- lapply(results, function(s) {
    if (length(s$data) == 0) return(NULL)
    data.frame(
      fips   = substr(s$seriesID, 6, 10),
      year   = vapply(s$data, function(d) as.character(d$year), ""),
      period = vapply(s$data, function(d) as.character(d$period), ""),
      value  = vapply(s$data, function(d) as.character(d$value), ""),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df)) {
    return(tibble::tibble(fips = character(), date = as.Date(character()), rate = numeric()))
  }
  df <- df[df$period != "M13" & grepl("^M\\d{2}$", df$period), ]
  df$rate <- suppressWarnings(as.numeric(df$value))
  df <- df[!is.na(df$rate), ]
  tibble::tibble(
    fips = df$fips,
    date = as.Date(paste0(df$year, "-", substr(df$period, 2, 3), "-01")),
    rate = df$rate
  )
}

.bls_laus_request <- function(ids, start_year, end_year, key) {
  httr2::request(.BLS_TS_URL) |>
    httr2::req_body_json(list(
      seriesid = as.list(ids), startyear = as.character(start_year),
      endyear = as.character(end_year), registrationkey = key
    )) |>
    httr2::req_timeout(30)
}

.bls_ok_results <- function(resp) {
  if (!inherits(resp, "httr2_response")) return(NULL)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body) || !identical(body$status, "REQUEST_SUCCEEDED")) return(NULL)
  body$Results$series
}

.default_perform <- function(reqs, max_active) {
  httr2::req_perform_parallel(reqs, max_active = max_active, on_error = "continue",
                              progress = FALSE)
}

#' Monthly county unemployment rates for the last `years` years, cached 7 days.
#' Sends 50 series per call, up to max_active in flight. If at least half the
#' chunks succeed, the failed ones are retried once; otherwise (quota exhausted,
#' outage) there are no retries, NULL is returned and the failure is remembered
#' for an hour so other sessions don't spend more quota. A pull that is still
#' incomplete after retries is marked attr "partial" and kept for 6 hours only.
fetch_laus_county <- function(fips, years = 3, key = Sys.getenv("BLS_API_KEY"),
                              max_active = 8, cache_dir = risk_cache_dir(),
                              perform = .default_perform,
                              perform_one = httr2::req_perform) {
  # Key carries the county count so a partial pull never stands in for the full set.
  risk_cached(sprintf("laus_county_n%d", length(fips)), 7, cache_dir = cache_dir,
              fail_ttl_days = 1 / 24, fetch_fn = function() {
    if (!nzchar(key)) { message("[LAUS] BLS_API_KEY not set"); return(NULL) }
    end_year <- as.integer(format(Sys.Date(), "%Y"))
    chunks <- split(laus_series_ids(fips), ceiling(seq_along(fips) / 50))
    reqs <- lapply(chunks, .bls_laus_request, end_year - years + 1, end_year, key)
    results <- lapply(perform(reqs, max_active = max_active), .bls_ok_results)
    failed <- vapply(results, is.null, logical(1))
    if (mean(failed) > 0.5) {
      message(sprintf("[LAUS] %d/%d BLS calls failed; not retrying", sum(failed), length(failed)))
      return(NULL)
    }
    for (i in which(failed)) {
      # results[i] <- list(x), not results[[i]] <- x: assigning NULL via [[ ]] deletes the slot.
      results[i] <- list(.bls_ok_results(tryCatch(perform_one(reqs[[i]]), error = function(e) NULL)))
    }
    ok <- !vapply(results, is.null, logical(1))
    message(sprintf("[LAUS] %d/%d BLS calls succeeded", sum(ok), length(ok)))
    out <- parse_bls_laus(unlist(results[ok], recursive = FALSE))
    if (!all(ok)) {
      attr(out, "partial") <- TRUE
      attr(out, "ttl_days") <- 0.25
    }
    out
  })
}

# ── County base: shapes + CBSA crosswalk + ACS county metrics ────────────────
#' County → CBSA (metro and micro) by each county's point on surface. CBSAs are
#' unions of counties, so this is exact; planar CRS avoids lon/lat warnings.
county_cbsa_xwalk <- function(counties_sf, cbsa_sf) {
  pts <- sf::st_transform(counties_sf[, "GEOID"], 5070)
  sf::st_agr(pts) <- "constant"
  pts <- sf::st_point_on_surface(pts)
  cb <- sf::st_transform(cbsa_sf, 5070) |>
    dplyr::transmute(cbsa_geoid = GEOID,
                     cbsa_name = sub(",.*$", "", NAME),
                     cbsa_type = dplyr::case_when(LSAD == "M1" ~ "metro",
                                                  LSAD == "M2" ~ "micro area",
                                                  TRUE ~ NA_character_))
  j <- suppressWarnings(sf::st_join(pts, cb, join = sf::st_within, left = TRUE))
  j <- sf::st_drop_geometry(j)
  tibble::as_tibble(j[!duplicated(j$GEOID), c("GEOID", "cbsa_geoid", "cbsa_name", "cbsa_type")])
}

.ACS_METRIC_COLS <- c("pop", "med_age", "unemp_rate", "lfp_rate", "poverty_rate",
                      "vacancy_rate", "rent_burden_rate", "bachelors_plus_rate",
                      "med_income", "per_cap_income", "med_home_val", "med_rent")

#' All US counties (50 states + DC) with names, state, CBSA and ACS context metrics.
fetch_county_base <- function(year = NULL) {
  if (is.null(year)) {
    year <- risk_acs_year()
  }
  shapes <- risk_cached("county_shapes_2024", 90, function() {
    tigris::counties(cb = TRUE, resolution = "20m", year = 2024, progress_bar = FALSE) |>
      sf::st_transform(4326) |>
      dplyr::filter(as.integer(STATEFP) <= 56) |>
      dplyr::transmute(GEOID, name = paste0(NAMELSAD, ", ", STUSPS),
                       state_fips = STATEFP, state_abbr = STUSPS, state_name = STATE_NAME)
  })
  if (is.null(shapes)) return(NULL)
  xwalk <- risk_cached("cbsa_xwalk_2023", 90, function() {
    cbsa <- tigris::core_based_statistical_areas(cb = TRUE, resolution = "20m",
                                                 year = 2023, progress_bar = FALSE)
    county_cbsa_xwalk(shapes, cbsa)
  })
  acs <- risk_cached(sprintf("county_acs_%d", year), 30, function() {
    tidycensus::get_acs(geography = "county", variables = ACS_VARS, year = year,
                        survey = "acs5", output = "wide") |>
      .derive_metrics() |>
      dplyr::select(GEOID, dplyr::all_of(.ACS_METRIC_COLS))
  })

  out <- shapes
  if (!is.null(xwalk)) {
    out <- dplyr::left_join(out, xwalk, by = "GEOID")
  } else {
    out$cbsa_geoid <- NA_character_; out$cbsa_name <- NA_character_
    out$cbsa_type <- NA_character_
  }
  if (!is.null(acs)) {
    out <- dplyr::left_join(out, tibble::as_tibble(acs), by = "GEOID")
  } else {
    for (col in .ACS_METRIC_COLS) out[[col]] <- NA_real_
  }
  attr(out, "acs_year") <- year
  attr(out, "fetched_at") <- attr(shapes, "fetched_at")
  out
}

# ── Tracts: one state at a time, ACS values + shapes + MOE flags ─────────────
fetch_state_tracts <- function(state_fips, year = NULL) {
  if (is.null(year)) {
    year <- risk_acs_year()
  }
  # Bump the "v" in the key whenever the tract columns change, so old disk copies are ignored.
  risk_cached(sprintf("tracts_v3_%s_%d", state_fips, year), 30, function() {
    tidycensus::get_acs(geography = "tract", state = state_fips, variables = ACS_VARS,
                        year = year, survey = "acs5", output = "wide",
                        geometry = TRUE, cb = TRUE) |>
      .derive_metrics() |>
      tract_moe_flags() |>
      sf::st_transform(4326) |>
      dplyr::transmute(GEOID, tract_name = gsub("; ", ", ", NAME),
                       county_geoid = substr(GEOID, 1, 5),
                       dplyr::across(dplyr::all_of(.ACS_METRIC_COLS)),
                       dplyr::across(dplyr::ends_with("_moe")),
                       dplyr::across(dplyr::starts_with("noisy")))
  })
}
