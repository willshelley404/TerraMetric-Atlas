# ─────────────────────────────────────────────────────────────────────────────
# R/data_risk.R — data fetchers and cache for the Regional Risk map
# ─────────────────────────────────────────────────────────────────────────────

# ── Cache: process memory + .rds on disk, per-key TTL, stale-on-error ────────
# Lookup order: memory -> disk cache -> starter copy shipped in the repo
# (data/risk_seed/, see risk_seed_update()) -> fetch. A stale starter copy is
# served at once while a background R process (callr) refreshes the disk cache.
.RISK_CACHE_ENV <- new.env(parent = emptyenv())
.RISK_BG_JOBS <- new.env(parent = emptyenv())

risk_cache_dir <- function() {
  opt <- getOption("risk.cache_dir")
  if (!is.null(opt) && nzchar(opt)) return(opt)
  d <- tryCatch({
    p <- tools::R_user_dir("TerraMetricAtlas", "cache")
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(p)) p else NULL
  }, error = function(e) NULL)
  d %||% tempdir()
}

#' Starter-copy directory (relative to the app root); option risk.seed_dir = ""
#' disables it (the background refresher does this so it always fetches).
risk_seed_dir <- function() {
  d <- getOption("risk.seed_dir", file.path("data", "risk_seed"))
  if (is.null(d) || !nzchar(d)) NULL else d
}

risk_cache_clear_memory <- function() {
  rm(list = ls(.RISK_CACHE_ENV, all.names = TRUE), envir = .RISK_CACHE_ENV)
  invisible(NULL)
}

.risk_cache_out <- function(entry, stale) {
  v <- entry$value
  attr(v, "fetched_at") <- entry$fetched_at
  attr(v, "stale") <- stale
  attr(v, "seed") <- isTRUE(entry$seed)
  v
}

.risk_seed_entry <- function(key, seed_dir) {
  if (is.null(seed_dir)) return(NULL)
  path <- file.path(seed_dir, paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  e <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(e) || is.null(e$value)) return(NULL)
  e$seed <- TRUE
  e
}

#' Return a cached value, refreshing it via fetch_fn() when older than ttl_days.
#' A failed refresh (error or NULL) falls back to the expired copy, flagged stale.
#' A value may carry attr "ttl_days" to shorten its own lifetime (e.g. a partial
#' pull). fail_ttl_days > 0 remembers a failure with no fallback in process
#' memory, so later sessions skip the fetch until it expires.
#' bg = list(id, fn, args): when a stale starter copy is all there is, serve it
#' and refresh by running fn(args) in a background R process instead of blocking.
risk_cached <- function(key, ttl_days, fetch_fn, cache_dir = risk_cache_dir(),
                        now = Sys.time(), fail_ttl_days = 0, seed_dir = risk_seed_dir(),
                        bg = NULL, bg_start = .risk_bg_start) {
  path <- file.path(cache_dir, paste0(key, ".rds"))
  age <- function(t) as.numeric(difftime(now, t, units = "days"))
  fresh <- function(e) !is.null(e) && !is.null(e$value) &&
    age(e$fetched_at) < (attr(e$value, "ttl_days") %||% ttl_days)

  fail_key <- paste0(key, "__failed")
  failed_at <- .RISK_CACHE_ENV[[fail_key]]
  entry <- .RISK_CACHE_ENV[[key]]
  if (fresh(entry)) return(.risk_cache_out(entry, FALSE))
  if (is.null(entry) && !is.null(failed_at) && age(failed_at) < fail_ttl_days) return(NULL)

  if ((is.null(entry) || isTRUE(entry$seed)) && file.exists(path)) {
    disk <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(disk)) { entry <- disk; assign(key, entry, envir = .RISK_CACHE_ENV) }
    if (fresh(entry)) return(.risk_cache_out(entry, FALSE))
  }

  if (is.null(entry)) {
    entry <- .risk_seed_entry(key, seed_dir)
    if (!is.null(entry)) {
      assign(key, entry, envir = .RISK_CACHE_ENV)
      if (fresh(entry)) return(.risk_cache_out(entry, FALSE))
    }
  }

  if (!is.null(bg) && isTRUE(entry$seed)) {
    bg_failed <- .RISK_CACHE_ENV[[paste0(key, "__bgfail")]]
    if (is.null(bg_failed) || age(bg_failed) >= 1 / 24) bg_start(bg, key)
    return(.risk_cache_out(entry, TRUE))
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

# ── Background refresh of stale starter copies (separate R process) ──────────
#' Start (or join) background job bg$id, which runs bg$fn(bg$args) in a fresh R
#' process with the starter copy disabled, so it fetches and writes the disk cache.
.risk_bg_start <- function(bg, key) {
  job <- .RISK_BG_JOBS[[bg$id]]
  if (!is.null(job)) {
    job$keys <- union(job$keys, key)
    assign(bg$id, job, envir = .RISK_BG_JOBS)
    return(invisible(job))
  }
  if (!requireNamespace("callr", quietly = TRUE) || !requireNamespace("later", quietly = TRUE)) {
    return(invisible(NULL))
  }
  message(sprintf("[risk cache] background refresh started: %s", bg$id))
  proc <- callr::r_bg(function(app_dir, fn, args, cache_dir) {
    setwd(app_dir)
    options(risk.seed_dir = "", risk.cache_dir = cache_dir, tigris_use_cache = TRUE)
    suppressPackageStartupMessages({ library(dplyr); library(sf) })   # sf: joins on shapes
    for (f in c("data_census.R", "data_risk.R", "risk_metrics.R", "map_risk.R")) {
      source(file.path("R", f))
    }
    invisible(do.call(fn, args))
    TRUE
  }, args = list(app_dir = getwd(), fn = bg$fn, args = bg$args, cache_dir = risk_cache_dir()))
  assign(bg$id, list(proc = proc, keys = key), envir = .RISK_BG_JOBS)
  later::later(function() .risk_bg_poll(), 2)
  invisible(NULL)
}

#' For each finished background job: promote the fresh disk copies into memory,
#' or, if the job wrote nothing, hold off restarting that key for an hour.
.risk_bg_poll <- function(cache_dir = risk_cache_dir(), reschedule = TRUE) {
  for (id in ls(.RISK_BG_JOBS)) {
    job <- .RISK_BG_JOBS[[id]]
    if (isTRUE(tryCatch(job$proc$is_alive(), error = function(e) FALSE))) next
    rm(list = id, envir = .RISK_BG_JOBS)
    for (k in job$keys) {
      p <- file.path(cache_dir, paste0(k, ".rds"))
      e <- if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
      if (!is.null(e) && !isTRUE(e$seed)) {
        assign(k, e, envir = .RISK_CACHE_ENV)
        message(sprintf("[risk cache] background refresh done: %s", k))
      } else {
        assign(paste0(k, "__bgfail"), Sys.time(), envir = .RISK_CACHE_ENV)
        message(sprintf("[risk cache] background refresh wrote nothing: %s", k))
      }
    }
  }
  if (reschedule && length(ls(.RISK_BG_JOBS))) later::later(function() .risk_bg_poll(), 2)
  invisible(NULL)
}

#' Copy the seedable entries of the local cache into the repo's starter copy.
#' Run before a deploy:  Rscript -e "source('global.R'); risk_seed_update()"
risk_seed_update <- function(cache_dir = risk_cache_dir(),
                             seed_dir = file.path("data", "risk_seed")) {
  files <- list.files(cache_dir, pattern = "[.]rds$")
  keep <- grepl(paste0("^(acs_year_v2|county_shapes_\\d+|cbsa_xwalk_\\d+|county_acs_\\d+|",
                       "pep_vintage_v1|fred_sectors_v1|laus_areas_v1)[.]rds$"), files)
  # Keys named <prefix><number>: keep only the highest number of each kind.
  for (prefix in c("laus_county_n", "laus_metro_n", "pep_county_", "county_ind_")) {
    m <- grep(sprintf("^%s\\d+[.]rds$", prefix), files, value = TRUE)
    if (length(m)) {
      n <- as.integer(sub("[.]rds$", "", substring(m, nchar(prefix) + 1L)))
      keep <- keep | files == m[which.max(n)]
    }
  }
  dir.create(seed_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(cache_dir, files[keep]), file.path(seed_dir, files[keep]), overwrite = TRUE)
  files[keep]
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
  y <- risk_cached("acs_year_v2", 30, cache_dir = cache_dir,
                   bg = list(id = "acs_year", fn = "risk_acs_year", args = list()),
                   fetch_fn = function() {
    .probe_acs_year(.acs_preferred_year(today), probe)
  })
  if (is.null(y)) as.integer(format(today, "%Y")) - 2L else as.integer(y)
}

# ── BLS LAUS county unemployment rate (API v2) ───────────────────────────────
.BLS_TS_URL <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

laus_series_ids <- function(fips) paste0("LAUCN", fips, "0000000003")

#' Tidy a BLS Results$series list: monthly rows with numeric values only. The
#' area id (still named `fips`) is the 5 characters at `id_start` of the series
#' id: 6 for counties (LAUCN<fips>), 8 for metros (LAUMT<state2><cbsa5>...).
parse_bls_laus <- function(results, id_start = 6L) {
  rows <- lapply(results, function(s) {
    if (length(s$data) == 0) return(NULL)
    data.frame(
      fips   = substr(s$seriesID, id_start, id_start + 4L),
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

#' Chunked BLS pull shared by the county and metro fetchers: 50 series per call,
#' up to max_active in flight. If at least half the chunks succeed, the failed
#' ones are retried once; otherwise (quota exhausted, outage) there are no
#' retries and NULL is returned. A pull still incomplete after retries is marked
#' attr "partial" with a 6 hour ttl. Returns NULL without a key.
.laus_pull <- function(ids, years, key, max_active, perform, perform_one, id_start) {
  if (!nzchar(key)) { message("[LAUS] BLS_API_KEY not set"); return(NULL) }
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  chunks <- split(ids, ceiling(seq_along(ids) / 50))
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
  out <- parse_bls_laus(unlist(results[ok], recursive = FALSE), id_start)
  if (!all(ok)) {
    attr(out, "partial") <- TRUE
    attr(out, "ttl_days") <- 0.25
  }
  out
}

#' Monthly county unemployment rates for the last `years` years, cached 7 days.
#' See .laus_pull for the chunk and retry rules; a failed pull (NULL) is
#' remembered for an hour so other sessions don't spend more quota.
fetch_laus_county <- function(fips, years = 3, key = Sys.getenv("BLS_API_KEY"),
                              max_active = 8, cache_dir = risk_cache_dir(),
                              perform = .default_perform,
                              perform_one = httr2::req_perform) {
  # Key carries the county count so a partial pull never stands in for the full set.
  risk_cached(sprintf("laus_county_n%d", length(fips)), 7, cache_dir = cache_dir,
              fail_ttl_days = 1 / 24,
              bg = list(id = "laus", fn = "fetch_laus_county", args = list(fips = fips)),
              fetch_fn = function() {
    .laus_pull(laus_series_ids(fips), years, key, max_active, perform, perform_one, 6L)
  })
}

# ── BLS LAUS metro (CBSA) unemployment rate ──────────────────────────────────
.BLS_LA_AREA_URL <- "https://download.bls.gov/pub/time.series/la/la.area"

#' la.area lines (header first, tab-delimited) -> metropolitan areas only
#' (area_type_code "B"; micro and divisions are dropped). CBSA = area_code[5:9].
parse_laus_areas <- function(lines) {
  empty <- tibble::tibble(cbsa_geoid = character(), cbsa_title = character(),
                          series_id = character())
  lines <- sub("\r$", "", lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2) return(empty)
  d <- utils::read.delim(text = paste(lines, collapse = "\n"), colClasses = "character",
                         quote = "", comment.char = "", strip.white = TRUE)
  d <- d[d$area_type_code == "B", , drop = FALSE]
  if (!nrow(d)) return(empty)
  tibble::tibble(cbsa_geoid = substr(d$area_code, 5, 9), cbsa_title = d$area_text,
                 series_id = paste0("LAU", d$area_code, "03"))
}

#' BLS metro area list, cached 90 days. NULL on failure (not cached).
fetch_laus_areas <- function() {
  risk_cached("laus_areas_v1", 90,
              bg = list(id = "laus_areas", fn = "fetch_laus_areas", args = list()),
              fetch_fn = function() {
    resp <- httr2::request(.BLS_LA_AREA_URL) |>
      httr2::req_user_agent(.RISK_UA) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()
    a <- parse_laus_areas(strsplit(httr2::resp_body_string(resp), "\r?\n")[[1]])
    if (nrow(a) == 0) NULL else a
  })
}

#' Monthly metro unemployment rates (`fips` holds the CBSA code), cached 7 days.
#' Same chunk, retry and failure-memory rules as the county pull (8 calls for ~390 metros).
fetch_laus_metro <- function(series_ids, years = 3, key = Sys.getenv("BLS_API_KEY"),
                             max_active = 8, cache_dir = risk_cache_dir(),
                             perform = .default_perform,
                             perform_one = httr2::req_perform) {
  risk_cached(sprintf("laus_metro_n%d", length(series_ids)), 7, cache_dir = cache_dir,
              fail_ttl_days = 1 / 24,
              bg = list(id = "laus_metro", fn = "fetch_laus_metro",
                        args = list(series_ids = series_ids)),
              fetch_fn = function() {
    .laus_pull(series_ids, years, key, max_active, perform, perform_one, 8L)
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
  bg <- list(id = "county_base", fn = "fetch_county_base", args = list(year = year))
  shapes <- risk_cached("county_shapes_2024", 90, bg = bg, fetch_fn = function() {
    tigris::counties(cb = TRUE, resolution = "20m", year = 2024, progress_bar = FALSE) |>
      sf::st_transform(4326) |>
      dplyr::filter(as.integer(STATEFP) <= 56) |>
      dplyr::transmute(GEOID, name = paste0(NAMELSAD, ", ", STUSPS),
                       state_fips = STATEFP, state_abbr = STUSPS, state_name = STATE_NAME)
  })
  if (is.null(shapes)) return(NULL)
  xwalk <- risk_cached("cbsa_xwalk_2023", 90, bg = bg, fetch_fn = function() {
    cbsa <- tigris::core_based_statistical_areas(cb = TRUE, resolution = "20m",
                                                 year = 2023, progress_bar = FALSE)
    county_cbsa_xwalk(shapes, cbsa)
  })
  acs <- risk_cached(sprintf("county_acs_%d", year), 30, bg = bg, fetch_fn = function() {
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

# ── Census PEP county estimates (migration) ──────────────────────────────────
.RISK_UA <- "TerraMetricAtlas/1.0 (research)"

.pep_url <- function(vintage) {
  sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%d/counties/totals/co-est%d-alldata.csv",
          as.integer(vintage), as.integer(vintage))
}

#' A real vintage answers 200 text/csv with a SUMLEV header. An unpublished one
#' returns a 200 HTML page, so status alone proves nothing.
.pep_resp_ok <- function(resp) {
  if (!inherits(resp, "httr2_response") || httr2::resp_status(resp) != 200L) return(FALSE)
  if (!startsWith(httr2::resp_content_type(resp) %||% "", "text/csv")) return(FALSE)
  head <- rawToChar(utils::head(httr2::resp_body_raw(resp), 2000L))
  grepl("SUMLEV", strsplit(head, "[\r\n]")[[1]][1], fixed = TRUE, useBytes = TRUE)
}

# Downloads the whole file (~2 MB); httr2 has no cheap first-chunk read here.
.pep_probe <- function(vintage) {
  resp <- httr2::request(.pep_url(vintage)) |>
    httr2::req_user_agent(.RISK_UA) |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  .pep_resp_ok(resp)
}

#' Newest PEP vintage published (current year down to year - 3), cached 30 days.
#' A failed probe (NULL) is not cached.
risk_pep_vintage <- function(probe = .pep_probe, today = Sys.Date(),
                             cache_dir = risk_cache_dir()) {
  v <- risk_cached("pep_vintage_v1", 30, cache_dir = cache_dir,
                   bg = list(id = "pep_vintage", fn = "risk_pep_vintage", args = list()),
                   fetch_fn = function() {
    .probe_acs_year(as.integer(format(today, "%Y")), probe)
  })
  if (is.null(v)) NULL else as.integer(v)
}

#' County migration and population change for one PEP vintage, cached 30 days.
#' NULL when no vintage can be found or the download fails.
fetch_pep_county <- function(vintage = NULL) {
  if (is.null(vintage)) vintage <- risk_pep_vintage()
  if (is.null(vintage)) return(NULL)
  vintage <- as.integer(vintage)
  risk_cached(sprintf("pep_county_%d", vintage), 30,
              bg = list(id = "pep", fn = "fetch_pep_county", args = list(vintage = vintage)),
              fetch_fn = function() {
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    httr2::request(.pep_url(vintage)) |>
      httr2::req_user_agent(.RISK_UA) |>
      httr2::req_timeout(120) |>
      httr2::req_perform(path = f)
    raw <- readr::read_csv(f, col_types = readr::cols(.default = "c"),
                           locale = readr::locale(encoding = "latin1"), progress = FALSE)
    parse_pep_county(raw, vintage)
  })
}

# ── FRED sector payrolls (spec §9.2) ─────────────────────────────────────────

#' Monthly SA payrolls for the 13 sector series, TEMPHELPS (optional) and the
#' NBER USREC flag since `start`, cached 1 day. NULL when there is no API key or
#' any sector series / USREC fails.
fetch_fred_sectors <- function(start = as.Date("2000-01-01")) {
  required <- c(RISK_SECTORS$fred, "USREC")
  risk_cached("fred_sectors_v1", 1, fail_ttl_days = 1 / 24,
              bg = list(id = "fred_sectors", fn = "fetch_fred_sectors", args = list()),
              fetch_fn = function() {
    if (!nzchar(Sys.getenv("FRED_API_KEY"))) return(NULL)
    pull <- function(s) {
      r <- fredr::fredr(series_id = s, observation_start = start)
      tibble::tibble(series_id = s, date = as.Date(r$date), value = as.numeric(r$value))
    }
    got <- lapply(c(required, "TEMPHELPS"), function(s)
      tryCatch(pull(s), error = function(e) {
        message(sprintf("[risk] FRED %s failed: %s", s, conditionMessage(e)))
        NULL
      }))
    names(got) <- c(required, "TEMPHELPS")
    if (any(vapply(got[required], is.null, logical(1)))) return(NULL)
    dplyr::bind_rows(got)
  })
}

# ── County resident industry mix (ACS C24030, spec §9.2) ─────────────────────

#' Employed residents by industry group per county, cached 30 days. `emp_<group>` is
#' the male plus female C24030 estimate; `emp_total` is C24030_001. NULL on failure.
fetch_county_industry <- function(year = NULL) {
  if (is.null(year)) year <- risk_acs_year()
  if (is.null(year)) return(NULL)
  out <- risk_cached(sprintf("county_ind_%d", year), 30,
              bg = list(id = "county_ind", fn = "fetch_county_industry",
                        args = list(year = year)),
              fetch_fn = function() {
    vars <- c("C24030_001", RISK_SECTORS$acs_m, RISK_SECTORS$acs_f)
    raw <- tidycensus::get_acs(geography = "county", variables = vars, year = year,
                               survey = "acs5", output = "wide")
    est <- function(v) raw[[paste0(v, "E")]]
    res <- tibble::tibble(GEOID = raw$GEOID, emp_total = est("C24030_001"))
    for (i in seq_len(nrow(RISK_SECTORS))) {
      res[[paste0("emp_", RISK_SECTORS$group[i])]] <-
        est(RISK_SECTORS$acs_m[i]) + est(RISK_SECTORS$acs_f[i])
    }
    res
  })
  if (!is.null(out)) attr(out, "acs_year") <- year
  out
}
