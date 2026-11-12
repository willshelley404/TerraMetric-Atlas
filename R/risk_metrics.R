# ─────────────────────────────────────────────────────────────────────────────
# R/risk_metrics.R — pure metric functions for the Regional Risk map (no I/O)
# ─────────────────────────────────────────────────────────────────────────────

# Tunable constants (spec §9). Change here, nowhere else.
RISK_CFG <- list(
  momentum_rising_pp  = 0.5,
  momentum_trigger_pp = 1.0,
  laus_min_coverage   = 0.9,
  laus_min_months     = 2,
  moe_flag_ratio      = 0.5
)

# ── Ranking helpers ──────────────────────────────────────────────────────────
pct_rank <- function(x) 100 * dplyr::percent_rank(x)

#' "Top 12% nationally" / "Bottom 30% in Georgia". `w` is pct oriented so that
#' high means worse; NA stays NA.
rank_label <- function(pct, frame_label, higher_is_worse = TRUE) {
  w <- if (higher_is_worse) pct else 100 - pct
  out <- ifelse(w >= 50,
                sprintf("Top %d%% %s", as.integer(pmax(1, ceiling(100 - w))), frame_label),
                sprintf("Bottom %d%% %s", as.integer(pmax(1, ceiling(w))), frame_label))
  out[is.na(w)] <- NA_character_
  out
}

# ── Stress now: Sahm-style YoY unemployment momentum (spec §9.1) ─────────────
#' County data is not seasonally adjusted, so the latest 3 months are compared
#' with the same 3 months a year earlier. Each window needs laus_min_months.
laus_momentum <- function(laus, cfg = RISK_CFG) {
  laus <- dplyr::filter(laus, !is.na(rate))
  counts <- dplyr::count(laus, date)
  ref <- max(counts$date[counts$n >= cfg$laus_min_coverage * max(counts$n)])
  back <- seq(ref, by = "-1 month", length.out = 15)
  now_m <- back[1:3]
  ago_m <- back[13:15]

  laus %>%
    dplyr::group_by(fips) %>%
    dplyr::summarise(
      n_now = sum(date %in% now_m),
      n_ago = sum(date %in% ago_m),
      rate_now = if (n_now > 0) mean(rate[date %in% now_m]) else NA_real_,
      rate_yr_ago = if (n_ago > 0) mean(rate[date %in% ago_m]) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      rate_now    = ifelse(n_now >= cfg$laus_min_months, round(rate_now, 2), NA_real_),
      rate_yr_ago = ifelse(n_ago >= cfg$laus_min_months, round(rate_yr_ago, 2), NA_real_),
      momentum_pp = round(rate_now - rate_yr_ago, 2),
      flag = dplyr::case_when(
        is.na(momentum_pp) ~ NA_character_,
        momentum_pp >= cfg$momentum_trigger_pp ~ "Trigger",
        momentum_pp >= cfg$momentum_rising_pp ~ "Rising",
        TRUE ~ "None"
      ),
      ref_month = ref
    ) %>%
    dplyr::select(fips, ref_month, rate_now, rate_yr_ago, momentum_pp, flag)
}

# ── County layers: join Stress now, add national percentiles, status/as-of ───
build_county_layers <- function(county_base, momentum, laus_stale = FALSE) {
  out <- county_base
  if (is.null(momentum)) {
    out$ref_month <- as.Date(NA); out$rate_now <- NA_real_; out$rate_yr_ago <- NA_real_
    out$momentum_pp <- NA_real_; out$flag <- NA_character_
  } else {
    out <- dplyr::left_join(out, dplyr::rename(momentum, GEOID = fips), by = "GEOID")
  }
  for (id in names(RISK_LAYERS)) {
    col <- RISK_LAYERS[[id]]$col
    out[[paste0("pct_", col)]] <- pct_rank(out[[col]])
  }

  yr <- attr(county_base, "acs_year")
  ids <- names(RISK_LAYERS)
  status <- stats::setNames(rep("ok", length(ids)), ids)
  asof <- stats::setNames(rep(sprintf("ACS %d–%d", yr - 4, yr), length(ids)), ids)
  if (is.null(momentum)) {
    status[["stress_now"]] <- "unavailable"
    asof[["stress_now"]] <- NA_character_
  } else {
    asof[["stress_now"]] <- format(max(momentum$ref_month, na.rm = TRUE), "%b %Y")
    if (isTRUE(laus_stale)) status[["stress_now"]] <- "stale"
  }
  attr(out, "layer_status") <- status
  attr(out, "asof") <- asof
  attr(out, "acs_year") <- yr
  out
}
