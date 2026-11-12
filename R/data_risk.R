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
risk_cached <- function(key, ttl_days, fetch_fn, cache_dir = risk_cache_dir(),
                        now = Sys.time()) {
  path <- file.path(cache_dir, paste0(key, ".rds"))
  fresh <- function(e) !is.null(e) &&
    as.numeric(difftime(now, e$fetched_at, units = "days")) < ttl_days

  entry <- .RISK_CACHE_ENV[[key]]
  if (fresh(entry)) return(.risk_cache_out(entry, FALSE))

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
    tryCatch(saveRDS(new_entry, path), error = function(e)
      message(sprintf("[risk cache] could not write %s: %s", path, conditionMessage(e))))
    return(.risk_cache_out(new_entry, FALSE))
  }

  if (!is.null(entry)) return(.risk_cache_out(entry, TRUE))
  NULL
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
    httr2::req_timeout(120)
}

.bls_ok_results <- function(resp) {
  if (!inherits(resp, "httr2_response")) return(NULL)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body) || !identical(body$status, "REQUEST_SUCCEEDED")) return(NULL)
  body$Results$series
}

#' Monthly county unemployment rates for the last `years` years, cached 7 days.
#' Sends 50 series per call, up to max_active in flight; failed chunks are
#' retried once. Returns NULL when fewer than half of the chunks succeed.
fetch_laus_county <- function(fips, years = 3, key = Sys.getenv("BLS_API_KEY"),
                              max_active = 8) {
  # Key carries the county count so a partial pull never stands in for the full set.
  risk_cached(sprintf("laus_county_n%d", length(fips)), 7, function() {
    if (!nzchar(key)) { message("[LAUS] BLS_API_KEY not set"); return(NULL) }
    end_year <- as.integer(format(Sys.Date(), "%Y"))
    chunks <- split(laus_series_ids(fips), ceiling(seq_along(fips) / 50))
    reqs <- lapply(chunks, .bls_laus_request, end_year - years + 1, end_year, key)
    resps <- httr2::req_perform_parallel(reqs, max_active = max_active,
                                         on_error = "continue", progress = FALSE)
    results <- lapply(resps, .bls_ok_results)
    for (i in which(vapply(results, is.null, logical(1)))) {
      results[[i]] <- .bls_ok_results(tryCatch(httr2::req_perform(reqs[[i]]),
                                               error = function(e) NULL))
    }
    ok <- !vapply(results, is.null, logical(1))
    message(sprintf("[LAUS] %d/%d BLS calls succeeded", sum(ok), length(ok)))
    if (sum(ok) < 0.5 * length(ok)) return(NULL)
    parse_bls_laus(unlist(results[ok], recursive = FALSE))
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
    year <- as.integer(risk_cached("acs_year", 30, function() .best_acs_year(2024)))
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
