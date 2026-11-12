# ─────────────────────────────────────────────────────────────────────────────
# R/risk_metrics.R — pure metric functions for the Regional Risk map (no I/O)
# ─────────────────────────────────────────────────────────────────────────────

# Tunable constants (spec §9). Change here, nowhere else.
RISK_CFG <- list(
  momentum_rising_pp  = 0.5,
  momentum_trigger_pp = 1.0,
  laus_min_coverage   = 0.9,
  laus_min_months     = 2,
  # Flag a tract value when its 90% MOE exceeds the estimate itself (CV > ~61%,
  # "very unreliable"). 0.5 flagged ~2/3 of tracts in TN+GA, which says nothing.
  moe_flag_ratio      = 1.0
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
#' County data is not seasonally adjusted, so each of the latest 3 months is
#' compared with the same calendar month a year earlier. Only months present in
#' BOTH years count (pairwise), so a gap such as October 2025 (never collected)
#' cannot compare 3 months against 2. At least laus_min_months pairs are needed.
laus_momentum <- function(laus, cfg = RISK_CFG) {
  laus <- dplyr::filter(laus, !is.na(rate))
  counts <- dplyr::count(laus, date)
  ref <- max(counts$date[counts$n >= cfg$laus_min_coverage * max(counts$n)])
  back <- seq(ref, by = "-1 month", length.out = 15)
  pairs <- data.frame(now_date = back[1:3], ago_date = back[13:15])

  now <- dplyr::inner_join(laus, pairs, by = c(date = "now_date"))
  paired <- dplyr::inner_join(now, dplyr::rename(laus, rate_ago = rate),
                              by = c("fips", ago_date = "date"))
  stats <- paired %>%
    dplyr::group_by(fips) %>%
    dplyr::summarise(n_pairs = dplyr::n(), rate_now = mean(rate),
                     rate_yr_ago = mean(rate_ago), .groups = "drop")

  tibble::tibble(fips = unique(laus$fips)) %>%
    dplyr::left_join(stats, by = "fips") %>%
    dplyr::mutate(
      ok = !is.na(n_pairs) & n_pairs >= cfg$laus_min_months,
      rate_now    = ifelse(ok, round(rate_now, 2), NA_real_),
      rate_yr_ago = ifelse(ok, round(rate_yr_ago, 2), NA_real_),
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

# ── Tract margins of error (spec §5 anti-goal: flag noisy tract values) ──────
.ACS_RATE_COLS <- c("unemp_rate", "lfp_rate", "poverty_rate", "vacancy_rate",
                    "rent_burden_rate", "bachelors_plus_rate", "med_income",
                    "per_cap_income", "med_home_val", "med_rent", "med_age")

#' Adds poverty_rate_moe / rent_burden_rate_moe (percentage points) and `noisy`
#' (either MOE above cfg$moe_flag_ratio of its estimate). Zero denominators -> NA.
tract_moe_flags <- function(df, cfg = RISK_CFG) {
  for (col in intersect(.ACS_RATE_COLS, names(df))) {
    df[[col]][!is.finite(df[[col]])] <- NA
  }
  finite_or_na <- function(x) ifelse(is.finite(x), x, NA_real_)
  col <- function(n) if (n %in% names(df)) df[[n]] else rep(NA_real_, nrow(df))
  pooled <- function(stems) {   # sum of several bins and their root-sum-of-squares MOE
    list(e = Reduce(`+`, lapply(paste0(stems, "E"), col)),
         m = sqrt(Reduce(`+`, lapply(paste0(stems, "M"), function(n) col(n)^2))))
  }
  rate_moe <- function(num, num_moe, den, den_moe) {
    suppressWarnings(100 * tidycensus::moe_prop(num, den, num_moe, den_moe))
  }
  rb <- pooled(c("rb_30_34", "rb_35_39", "rb_40_49", "rb_50_plus"))
  ba <- pooled(c("bachelors", "masters", "professional", "doctorate"))
  moes <- list(
    unemp_rate = rate_moe(col("unemployedE"), col("unemployedM"),
                          col("labor_forceE"), col("labor_forceM")),
    lfp_rate = rate_moe(col("labor_forceE"), col("labor_forceM"),
                        col("total_popE"), col("total_popM")),
    poverty_rate = rate_moe(col("poverty_countE"), col("poverty_countM"),
                            col("poverty_universeE"), col("poverty_universeM")),
    vacancy_rate = rate_moe(col("vacant_unitsE"), col("vacant_unitsM"),
                            col("housing_unitsE"), col("housing_unitsM")),
    rent_burden_rate = rate_moe(rb$e, rb$m, col("rent_burden_universeE") - col("rb_not_computedE"),
                                col("rent_burden_universeM")),
    bachelors_plus_rate = rate_moe(ba$e, ba$m, col("education_total_25plusE"),
                                   col("education_total_25plusM")),
    med_income = col("median_hh_incomeM"),
    med_home_val = col("median_home_valueM"),
    med_rent = col("median_rentM")
  )
  over <- function(moe, est) {
    r <- moe / est > cfg$moe_flag_ratio
    !is.na(r) & r
  }
  df$noisy <- rep(FALSE, nrow(df))
  for (v in names(moes)) {
    if (!v %in% names(df)) next
    m <- ifelse(is.na(df[[v]]), NA_real_, finite_or_na(moes[[v]]))
    df[[paste0(v, "_moe")]] <- m
    df[[paste0("noisy_", v)]] <- over(m, df[[v]])
    df$noisy <- df$noisy | df[[paste0("noisy_", v)]]
  }
  df
}

# ── Tract ranking within labor market (CBSA) and state (spec §9.4) ───────────
.tract_vars <- function() {
  keep <- vapply(RISK_LAYERS, function(l) "tract" %in% l$levels, logical(1))
  vapply(RISK_LAYERS[keep], `[[`, "", "col")
}

.plain_df <- function(x) if (inherits(x, "sf")) sf::st_drop_geometry(x) else x

#' Frame for a clicked county: its CBSA, or the non-CBSA counties of its state.
.tract_frame <- function(county_geoid, cb) {
  row <- cb[cb$GEOID == county_geoid, ][1, ]
  if (!is.na(row$cbsa_geoid)) {
    list(counties = cb$GEOID[!is.na(cb$cbsa_geoid) & cb$cbsa_geoid == row$cbsa_geoid],
         label = sprintf("in %s %s", row$cbsa_name, row$cbsa_type))
  } else {
    list(counties = cb$GEOID[is.na(cb$cbsa_geoid) & cb$state_fips == row$state_fips],
         label = sprintf("in non-metro %s", row$state_name))
  }
}

rank_tracts <- function(tracts_all, county_base, frame_counties, vars) {
  cb <- .plain_df(county_base)
  fr <- .tract_frame(frame_counties[1], cb)
  st <- substr(tracts_all$GEOID, 1, 2)
  in_frame <- tracts_all$county_geoid %in% frame_counties
  for (v in intersect(vars, names(tracts_all))) {
    x <- tracts_all[[v]]
    tracts_all[[paste0("st_pct_", v)]] <- stats::ave(x, st, FUN = pct_rank)
    lm <- rep(NA_real_, length(x))
    lm[in_frame] <- pct_rank(x[in_frame])
    tracts_all[[paste0("lm_pct_", v)]] <- lm
  }
  state_names <- stats::setNames(cb$state_name, cb$state_fips)
  tracts_all$frame_label <- fr$label
  tracts_all$state_label <- paste("in", unname(state_names[st]))
  tracts_all
}

#' Tracts for the labor market around a clicked county, ranked within that
#' market and within each tract's state. States that fail to load are reported.
tracts_for_county <- function(county_geoid, county_base, fetch = fetch_state_tracts,
                              vars = .tract_vars()) {
  cb <- .plain_df(county_base)
  fr <- .tract_frame(county_geoid, cb)
  states <- unique(c(substr(county_geoid, 1, 2), substr(fr$counties, 1, 2)))
  got <- lapply(states, function(s) tryCatch(fetch(s), error = function(e) NULL))
  failed <- vapply(got, is.null, logical(1))
  out <- list(tracts = NULL, frame_label = fr$label, missing_states = states[failed])
  if (all(failed)) return(out)
  ranked <- rank_tracts(do.call(rbind, got[!failed]), cb, fr$counties, vars)
  out$tracts <- ranked[ranked$county_geoid %in% fr$counties, ]
  out
}

# ── County layers: join Stress now, add national percentiles, status/as-of ───
#' laus_loading = TRUE marks Stress now as still loading (progressive tab load:
#' the map draws from shapes + ACS first, BLS fills in when it arrives).
build_county_layers <- function(county_base, momentum, laus_stale = FALSE,
                                laus_loading = FALSE, laus_partial = FALSE) {
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
    status[["stress_now"]] <- if (isTRUE(laus_loading)) "loading" else "unavailable"
    asof[["stress_now"]] <- NA_character_
  } else {
    asof[["stress_now"]] <- format(max(momentum$ref_month, na.rm = TRUE), "%b %Y")
    if (isTRUE(laus_partial)) status[["stress_now"]] <- "partial"
    if (isTRUE(laus_stale)) status[["stress_now"]] <- "stale"
  }
  attr(out, "layer_status") <- status
  attr(out, "asof") <- asof
  attr(out, "acs_year") <- yr
  out
}

# ── Migration (Census PEP county estimates) ──────────────────────────────────
#' 2025 -> "2024–25". PEP years run July to July.
pep_year_label <- function(y) sprintf("%d\u2013%02d", as.integer(y) - 1L, as.integer(y) %% 100L)

#' Tidy a PEP co-est{V}-alldata read (all columns character): counties only
#' (incl. CT planning regions), one row per GEOID. Rates are per 1,000 residents.
parse_pep_county <- function(raw, vintage) {
  v <- as.integer(vintage)
  yrs <- 2021:v
  num <- function(col) as.numeric(raw[[col]])
  keep <- raw$SUMLEV == "050" & as.integer(raw$STATE) <= 56
  raw <- raw[keep, , drop = FALSE]

  dom <- vapply(paste0("DOMESTICMIG", yrs), num, numeric(nrow(raw)))
  dom <- matrix(dom, nrow = nrow(raw))
  base <- num("ESTIMATESBASE2020")
  pop <- num(paste0("POPESTIMATE", v))
  out <- tibble::tibble(
    GEOID         = paste0(raw$STATE, raw$COUNTY),
    mig_dom_rate  = num(paste0("RDOMESTICMIG", v)),
    mig_intl_rate = num(paste0("RINTERNATIONALMIG", v)),
    mig_net_rate  = num(paste0("RNETMIG", v)),
    nat_chg_rate  = num(paste0("RNATURALCHG", v)),
    mig_dom_cum   = 1000 * rowSums(dom) / base,
    pop_chg_pct   = 100 * (pop / base - 1),
    mig_dom_n     = num(paste0("DOMESTICMIG", v)),
    pop_avg       = (num(paste0("POPESTIMATE", v - 1L)) + pop) / 2
  )
  for (y in yrs) out[[paste0("mig_dom_rate_", y)]] <- num(paste0("RDOMESTICMIG", y))
  out <- dplyr::mutate(out, dplyr::across(-GEOID, \(x) round(x, 2)))
  attr(out, "vintage") <- v
  out
}
