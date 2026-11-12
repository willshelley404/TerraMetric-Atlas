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
