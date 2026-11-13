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
  moe_flag_ratio      = 1.0,
  # Sector risk weights (spec §9.2)
  sector_momentum_months = 6,
  recession_since     = as.Date("2000-01-01"),
  peak_lead_months    = 6,    # a payroll peak can precede the NBER start
  trough_lag_months   = 24,   # payrolls keep falling after the NBER end
  w_structural        = 0.5,
  w_current           = 0.5,
  # LODES has no usable coverage here: AK is thin, MI is missing from LODES8.
  lodes_exclude_states = c("02", "26"),
  # Assistance cushion: mean of available percentiles, at least this many inputs.
  cushion_min_inputs  = 2,
  # Credit score needs this many of its 4 components, else "Insufficient data".
  credit_min_components = 3,
  # Now vs ahead: a county is "bad" on a measure at or above this national percentile
  # of badness (worst third).
  quad_bad_pct = 100 * 2 / 3
)

# Now vs ahead quadrants, in code order (the layer's value is the position here).
QUAD_LABELS <- c("Healthy", "Recovering", "Worsening", "Bad now and ahead")

# FRED payroll series and ACS C24030 detail lines per industry group. `cns` are
# the LEHD-style sector codes Phase 3 maps to tract exposure.
RISK_SECTORS <- data.frame(
  group = c("ag_mining", "construction", "manufacturing", "wholesale", "retail",
            "transport_util", "information", "finance_re", "prof_admin",
            "educ_health", "arts_food", "other_services", "public_admin"),
  label = c("Agriculture & mining", "Construction", "Manufacturing", "Wholesale trade",
            "Retail trade", "Transportation & utilities", "Information",
            "Finance & real estate", "Professional & admin services",
            "Education & health", "Leisure & hospitality", "Other services",
            "Public administration"),
  fred  = c("USMINE", "USCONS", "MANEMP", "USWTRADE", "USTRADE", "CES4300000001",
            "USINFO", "USFIRE", "USPBS", "USEHS", "USLAH", "USSERV", "USGOVT"),
  acs_m = c("C24030_003", "C24030_006", "C24030_007", "C24030_008", "C24030_009",
            "C24030_010", "C24030_013", "C24030_014", "C24030_017", "C24030_021",
            "C24030_024", "C24030_027", "C24030_028"),
  acs_f = c("C24030_030", "C24030_033", "C24030_034", "C24030_035", "C24030_036",
            "C24030_037", "C24030_040", "C24030_041", "C24030_044", "C24030_048",
            "C24030_051", "C24030_054", "C24030_055"),
  cns   = c("01,02", "04", "05", "06", "07", "03,08", "09", "10,11", "12,13,14",
            "15,16", "17,18", "19", "20"),
  stringsAsFactors = FALSE
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

# Columns whose state rank comes from a state-frame twin, not the frame value.
.ST_RANK_SRC <- c(assist_score = "assist_score_st", assist_cushion = "assist_cushion_st")

rank_tracts <- function(tracts_all, county_base, frame_counties, vars) {
  cb <- .plain_df(county_base)
  fr <- .tract_frame(frame_counties[1], cb)
  st <- substr(tracts_all$GEOID, 1, 2)
  in_frame <- tracts_all$county_geoid %in% frame_counties
  for (v in intersect(vars, names(tracts_all))) {
    x <- tracts_all[[v]]
    src <- unname(.ST_RANK_SRC[v])
    sx <- if (!is.na(src) && src %in% names(tracts_all)) tracts_all[[src]] else x
    tracts_all[[paste0("st_pct_", v)]] <- stats::ave(sx, st, FUN = pct_rank)
    lm <- rep(NA_real_, length(x))
    lm[in_frame] <- pct_rank(x[in_frame])
    tracts_all[[paste0("lm_pct_", v)]] <- lm
  }
  state_names <- stats::setNames(cb$state_name, cb$state_fips)
  tracts_all$frame_n <- ifelse(in_frame, sum(in_frame), NA_integer_)
  tracts_all$frame_label <- fr$label
  tracts_all$state_label <- paste("in", unname(state_names[st]))
  tracts_all
}

#' Tracts for the labor market around a clicked county, ranked within that
#' market and within each tract's state. States that fail to load are reported.
#' `fetch_jobs(state_fips, acs_geoids)` returns that state's job table (attr "source")
#' or NULL; `sources` maps each loaded state to its source text. The assistance
#' columns are ranked on top of `vars`, so the drill-down works before the layers
#' are registered.
tracts_for_county <- function(county_geoid, county_base, fetch = fetch_state_tracts,
                              fetch_jobs = .tract_jobs,
                              weights = attr(county_base, "sector_weights"),
                              vars = .tract_vars()) {
  cb <- .plain_df(county_base)
  fr <- .tract_frame(county_geoid, cb)
  states <- unique(c(substr(county_geoid, 1, 2), substr(fr$counties, 1, 2)))
  got <- lapply(states, function(s) tryCatch(fetch(s), error = function(e) {
    message("[Risk] tracts ", s, " failed: ", conditionMessage(e))
    NULL
  }))
  failed <- vapply(got, is.null, logical(1))
  out <- list(tracts = NULL, frame_label = fr$label, missing_states = states[failed],
              sources = character(0))
  if (all(failed)) return(out)
  ok <- states[!failed]
  got <- got[!failed]
  jobs <- lapply(seq_along(ok), function(i) {
    j <- tryCatch(fetch_jobs(ok[i], got[[i]]$GEOID), error = function(e) {
      message("[Risk] jobs ", ok[i], " failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(j)) return(NULL)
    src <- attr(j, "source")
    j <- tibble::as_tibble(as.data.frame(j))
    j$assist_source <- rep(if (is.null(src)) NA_character_ else src, nrow(j))
    j
  })
  out$sources <- stats::setNames(
    vapply(jobs, function(j) if (is.null(j)) NA_character_ else j$assist_source[1], ""), ok)
  jobs_all <- if (all(vapply(jobs, is.null, logical(1)))) NULL else dplyr::bind_rows(jobs)
  tr <- assistance_tracts(do.call(rbind, got), jobs_all, weights, county_base, fr$counties)
  rank_vars <- union(vars, c("assist_score", "assist_exposure", "assist_cushion",
                             "assist_workers", "weak_share", "low_wage_share"))
  ranked <- rank_tracts(tr, cb, fr$counties, rank_vars)
  out$tracts <- ranked[ranked$county_geoid %in% fr$counties, ]
  z <- zips_by_polygon(out$tracts, nearest = TRUE)
  out$tracts$zip_list <- z$zip_list
  out$tracts$zip_n <- z$zip_n
  out$tracts$zip_nearest <- z$zip_nearest
  out
}

# ── County layers: join Stress now, add national percentiles, status/as-of ───
#' loading: component names still loading ("stress_now", "migration", "stress_ahead");
#' progressive tab load: the map draws from shapes + ACS first, the rest fills in.
#' migration: parse_pep_county() output (attr "stale" from risk_cached) or NULL.
#' stress_ahead: stress_ahead() output with attr "stale" (set by the caller when FRED or
#' ACS came from a stale cache), or NULL.
#' sector_weights: sector_weights() output (needs FRED only, so it survives a missing ACS
#' industry pull); falls back to attr(stress_ahead, "weights").
#' metro_momentum: laus_momentum() on the metro pull (fips = CBSA code) or NULL; joined
#' onto counties by cbsa_geoid as metro_momentum_pp / metro_rate_now / metro_ref_month.
#' credit: list(cfpb, urban, hpi, msad), the raw fetch_cfpb_mortgage() / fetch_urban_debt() /
#' fetch_fhfa_hpi() / fetch_msad_xwalk() results (any may be NULL; attr "stale" on each is
#' honoured), or NULL before the lazy credit load. Per-county tables are derived here.
#' credit_requested: FALSE until the credit sources have been asked for; credit-lens layers
#' are then "not_loaded" rather than "unavailable" (a real failure).
build_county_layers <- function(county_base, momentum, laus_stale = FALSE,
                                laus_partial = FALSE,
                                migration = NULL, loading = character(0),
                                stress_ahead = NULL, metro_momentum = NULL,
                                sector_weights = NULL, credit = NULL,
                                credit_requested = TRUE) {
  out <- county_base
  if (is.null(momentum)) {
    out$ref_month <- as.Date(NA); out$rate_now <- NA_real_; out$rate_yr_ago <- NA_real_
    out$momentum_pp <- NA_real_; out$flag <- NA_character_
  } else {
    out <- dplyr::left_join(out, dplyr::rename(momentum, GEOID = fips), by = "GEOID")
  }
  if (is.null(migration)) {
    for (col in c("mig_dom_rate", "mig_intl_rate", "mig_net_rate", "nat_chg_rate",
                  "mig_dom_cum", "pop_chg_pct", "mig_dom_n", "pop_avg")) out[[col]] <- NA_real_
  } else {
    out <- dplyr::left_join(out, migration, by = "GEOID")
  }
  if (is.null(stress_ahead)) {
    for (col in c("proj_pct", "us_proj_pct", "stress_ahead_pp", paste0("sa_drv", 1:3, "_pp"),
                  paste0("sa_lift", 1:3, "_pp"),
                  paste0("share_", RISK_SECTORS$group), "emp_total")) out[[col]] <- NA_real_
    for (col in c(paste0("sa_drv", 1:3, "_label"), paste0("sa_lift", 1:3, "_label")))
      out[[col]] <- NA_character_
  } else {
    out <- dplyr::left_join(out, tibble::as_tibble(stress_ahead), by = "GEOID")
  }
  if (is.null(metro_momentum)) {
    out$metro_momentum_pp <- NA_real_; out$metro_rate_now <- NA_real_
    out$metro_ref_month <- as.Date(NA)
  } else {
    mm <- tibble::tibble(cbsa_geoid = metro_momentum$fips,
                         metro_momentum_pp = metro_momentum$momentum_pp,
                         metro_rate_now = metro_momentum$rate_now,
                         metro_ref_month = metro_momentum$ref_month)
    out <- dplyr::left_join(out, mm, by = "cbsa_geoid")
  }
  # Not %||%: the app redefines it, and a data frame is not a scalar.
  weights <- if (!is.null(sector_weights)) sector_weights else
    if (is.null(stress_ahead)) NULL else attr(stress_ahead, "weights")
  # Derived layers are computed below from the base percentiles (assistance now, credit later).
  is_derived <- vapply(RISK_LAYERS, function(l) isTRUE(l$derived), TRUE)
  add_pct <- function(out, ids) {
    for (id in ids) {
      col <- RISK_LAYERS[[id]]$col
      out[[paste0("pct_", col)]] <- pct_rank(out[[col]])
    }
    out
  }
  out <- add_pct(out, names(RISK_LAYERS)[!is_derived])
  # Job pressure needs the Stress now / ahead percentiles above; it does not need the
  # sector weights, so it exists even when assistance is unavailable.
  jp <- job_pressure(out)
  out$job_pressure <- as.numeric(jp)
  out$job_pressure_n <- attr(jp, "n_inputs")
  out$job_pressure_from <- attr(jp, "inputs")
  out <- county_assistance(out, weights)
  out <- county_credit(out, credit)
  # While the credit sources load, a two-input tax base would change when HPI arrives.
  out$tax_base <- if ("credit" %in% loading) rep(NA_real_, nrow(out)) else tax_base_proxy(out)
  out <- county_now_vs_ahead(out)
  out <- add_pct(out, names(RISK_LAYERS)[is_derived])
  out$pct_weak_share <- pct_rank(out$weak_share)
  out$pct_pop_chg_pct <- pct_rank(out$pop_chg_pct)    # tax base driver colour

  yr <- attr(county_base, "acs_year")
  ids <- names(RISK_LAYERS)
  status <- stats::setNames(rep("ok", length(ids)), ids)
  asof <- stats::setNames(rep(sprintf("ACS %d–%d", yr - 4, yr), length(ids)), ids)
  if (is.null(momentum)) {
    status[["stress_now"]] <- if ("stress_now" %in% loading) "loading" else "unavailable"
    asof[["stress_now"]] <- NA_character_
  } else {
    asof[["stress_now"]] <- format(max(momentum$ref_month, na.rm = TRUE), "%b %Y")
    if (isTRUE(laus_partial)) status[["stress_now"]] <- "partial"
    if (isTRUE(laus_stale)) status[["stress_now"]] <- "stale"
  }
  v <- if (is.null(migration)) NULL else attr(migration, "vintage")
  for (id in ids[vapply(RISK_LAYERS, function(l) identical(l$lens, "migration"), TRUE)]) {
    status[[id]] <- if ("migration" %in% loading) "loading" else
      if (is.null(migration)) "unavailable" else
        if (isTRUE(attr(migration, "stale"))) "stale" else "ok"
    asof[[id]] <- if (is.null(v)) NA_character_ else if (id == "mig_dom_cum")
      sprintf("V%d, 2020–%s", v, substr(v, 3, 4)) else sprintf("V%d, %s", v, pep_year_label(v))
  }
  status[["stress_ahead"]] <- if ("stress_ahead" %in% loading) "loading" else
    if (is.null(stress_ahead)) "unavailable" else
      if (isTRUE(attr(stress_ahead, "stale"))) "stale" else
        if (isTRUE(attr(stress_ahead, "partial"))) "partial" else "ok"
  # Now vs ahead rests on both stress layers: the weakest of the two decides its status.
  qs <- status[c("stress_now", "stress_ahead")]
  status[["now_vs_ahead"]] <- if (any(qs == "loading")) "loading" else
    if (any(qs == "unavailable")) "unavailable" else if (any(qs == "stale")) "stale" else
      if (any(qs == "partial")) "partial" else "ok"
  rm_sa <- if (is.null(stress_ahead)) NULL else attr(stress_ahead, "ref_month")
  asof[["stress_ahead"]] <- if (is.null(rm_sa) || is.na(rm_sa)) NA_character_ else
    sprintf("CES %s · ACS %d–%d", format(rm_sa, "%b %Y"), yr - 4, yr)
  asof[["now_vs_ahead"]] <- if (is.na(asof[["stress_now"]]) || is.na(asof[["stress_ahead"]]))
    NA_character_ else sprintf("LAUS %s · %s", asof[["stress_now"]], asof[["stress_ahead"]])
  # Assistance needs the county industry mix as well as the weights; it is stale or
  # partial when either input is.
  ces <- if (is.null(weights)) NULL else attr(weights, "ref_month")
  # Job pressure feeds the score, so Stress now / ahead staleness carries over.
  sa_stale <- isTRUE(attr(stress_ahead, "stale")) || isTRUE(attr(weights, "stale")) ||
    any(status[c("stress_now", "stress_ahead")] == "stale")
  sa_partial <- isTRUE(attr(stress_ahead, "partial")) || isTRUE(attr(weights, "partial")) ||
    any(status[c("stress_now", "stress_ahead")] == "partial") ||
    .one_job_input_gone(status[c("stress_now", "stress_ahead")])
  for (id in names(RISK_LAYERS)[vapply(RISK_LAYERS, function(l) identical(l$lens, "assistance"), TRUE)]) {
    status[[id]] <- if ("stress_ahead" %in% loading) "loading" else
      if (is.null(weights) || is.null(stress_ahead)) "unavailable" else
        if (sa_stale) "stale" else if (sa_partial) "partial" else "ok"
    asof[[id]] <- if (!status[[id]] %in% c("ok", "stale", "partial")) NA_character_ else
      paste0(sprintf("ACS %d–%d", yr - 4, yr),
             if (length(ces) == 1 && !is.na(ces)) format(ces, " · CES %b %Y") else "")
  }
  cs <- credit_status_asof(out, credit, loading, asof[["stress_now"]], asof[["stress_ahead"]],
                           v, isTRUE(attr(migration, "stale")), credit_requested,
                           status[c("stress_now", "stress_ahead")])
  status[names(cs$status)] <- cs$status
  asof[names(cs$asof)] <- cs$asof
  attr(out, "layer_status") <- status
  attr(out, "asof") <- asof
  attr(out, "acs_year") <- yr
  attr(out, "sector_weights") <- weights
  attr(out, "credit_info") <- cs$info
  attr(out, "loading") <- loading
  if (!is.null(v)) attr(out, "pep_vintage") <- v
  out
}

#' Now vs ahead: Stress now and Stress ahead on one "how bad" scale (national percentile,
#' high = worse; stress ahead is flipped because a low job-growth gap is the bad end), then
#' split at `cfg$quad_bad_pct`. Needs pct_momentum_pp and pct_stress_ahead_pp.
#' Adds now_vs_ahead (1 Healthy, 2 Recovering = bad now only, 3 Worsening = bad ahead
#' only, 4 bad on both; NA if either measure is missing) and quad_now_pct / quad_ahead_pct.
county_now_vs_ahead <- function(out, cfg = RISK_CFG) {
  col <- function(nm) if (nm %in% names(out)) as.numeric(out[[nm]]) else rep(NA_real_, nrow(out))
  now <- col("pct_momentum_pp")
  ahead <- 100 - col("pct_stress_ahead_pp")
  bad_now <- now >= cfg$quad_bad_pct
  bad_ahead <- ahead >= cfg$quad_bad_pct
  code <- 1 + bad_now + 2 * bad_ahead
  code[is.na(now) | is.na(ahead)] <- NA_real_
  out$now_vs_ahead <- code
  out$quad_now_pct <- now
  out$quad_ahead_pct <- ahead
  out
}

#' County assistance values (national frame): exposure to weakening sectors from the
#' county industry mix (jobs = share x emp_total), cushion from rent burden and poverty,
#' and the score from those plus `out$job_pressure` (set by build_county_layers).
#' All NA when `weights` is NULL or the county shares are missing.
county_assistance <- function(out, weights) {
  n <- nrow(out)
  d <- .plain_df(out)
  total <- if ("emp_total" %in% names(d)) as.numeric(d$emp_total) else rep(NA_real_, n)
  jobs <- tibble::tibble(GEOID = d$GEOID, jobs_total = total)
  for (i in seq_len(nrow(RISK_SECTORS))) {
    jobs[[.JOB_COLS[i]]] <- as.numeric(d[[paste0("share_", RISK_SECTORS$group[i])]]) * total
  }
  e <- exposure_from_jobs(jobs, weights)
  out$exposure <- e$exposure
  out$exp_workers <- e$exp_workers
  out$weak_share <- e$weak_share
  s <- assistance_score(e$exposure, d[c("rent_burden_rate", "poverty_rate")],
                        out$job_pressure, rep("us", n))
  # Without exposure the layers are "unavailable"; keep the cushion consistent with that.
  blank <- is.null(weights) || all(is.na(e$exposure))
  out$assist_score <- s$score
  out$assist_exposure <- round(100 * e$exposure, 1)
  out$assist_cushion <- if (blank) rep(NA_real_, n) else s$pct_cushion
  out$assist_workers <- e$exp_workers
  out
}

# Job pressure rests on Stress now and Stress ahead: one of them unavailable (e.g. the
# BLS quota) or still loading leaves it resting on the other alone.
.job_input_gone <- function(st) st %in% c("unavailable", "loading")
.one_job_input_gone <- function(st) sum(.job_input_gone(st)) == 1L
.job_inputs_gone <- function(st) all(.job_input_gone(st))

# Which raw credit sources exist (CFPB, Urban, FHFA); `[[` on a NULL list is NULL.
.credit_has <- function(credit) vapply(c(cfpb = "cfpb", urban = "urban", hpi = "hpi"),
                                       function(nm) !is.null(credit[[nm]]), TRUE)

#' Credit lens columns for the county layers (names as in credit_components()), plus
#' consumer_del (mean of card and auto, the consumer layer's value). Derives the
#' per-county CFPB and HPI tables from the raw fetch results in `credit`. All NA when no
#' source is present, so values always match the layer status.
county_credit <- function(out, credit) {
  cb <- .plain_df(out)
  n <- nrow(cb)
  if (!any(.credit_has(credit))) {
    na <- rep(NA_real_, n); nc <- rep(NA_character_, n)
    blank <- list(cr_mortgage = na, cr_consumer = na, cr_jobs = na, cr_collateral = na,
                  credit_n = rep(NA_integer_, n), credit_score = na, credit_missing = nc,
                  dq90 = na, dq3089_chg = na, cfpb_source = nc, cfpb_month = nc,
                  card_del = na, auto_del = na, consumer_del = na, hpi_yoy = na,
                  hpi_source = nc, hpi_q = nc)
    for (col in names(blank)) out[[col]] <- blank[[col]]
    return(out)
  }
  cf <- credit[["cfpb"]]
  cfpb <- if (is.null(cf)) NULL else cfpb_county_components(
    county_ids = cb$GEOID, cbsa_of = cb$cbsa_geoid, cbsa_type_of = cb$cbsa_type,
    state_of = cb$state_fips, county90 = cf$county90, county3089 = cf$county3089,
    metro90 = cf$metro90, metro3089 = cf$metro3089,
    state90 = cf$state90, state3089 = cf$state3089)
  hpi <- if (is.null(credit[["hpi"]])) NULL else
    county_hpi(cb, credit[["hpi"]], credit[["msad"]])
  comps <- credit_components(cb, cfpb, credit[["urban"]], hpi)
  comps$consumer_del <- .avail_mean(comps$card_del, comps$auto_del)
  for (col in setdiff(names(comps), "GEOID")) out[[col]] <- comps[[col]]
  out
}

#' Status and as-of of the credit-lens layers and tax_base: list(status, asof, info).
#' Loading wins; a layer never asked for is "not_loaded"; a layer is unavailable without
#' its own source or without any value; stale/partial follow the layer's own inputs only
#' (mortgage: CFPB; consumer: Urban; collateral: FHFA + metro divisions; jobs: the Stress
#' now / ahead statuses `stress_status`; score: all of those; tax base: PEP + FHFA),
#' stale beating partial. The Urban as-of is the release month of its data folder, never
#' the data month. jobs_now / jobs_ahead are the Stress now / ahead as-ofs, v the PEP
#' vintage, mig_stale whether PEP came from a stale cache.
credit_status_asof <- function(out, credit, loading, jobs_now, jobs_ahead, v, mig_stale,
                               credit_requested = TRUE,
                               stress_status = c(stress_now = "ok", stress_ahead = "ok")) {
  ids <- c("credit_score", "credit_mortgage", "credit_consumer", "credit_jobs",
           "credit_collateral", "tax_base")
  has <- .credit_has(credit)
  src_stale <- function(...) any(vapply(c(...),
                                        function(nm) isTRUE(attr(credit[[nm]], "stale")), TRUE))
  jobs_state <- if (.job_inputs_gone(stress_status)) "unavailable" else
    if (any(stress_status == "stale")) "stale" else
      if (any(stress_status == "partial") || .one_job_input_gone(stress_status)) "partial" else "ok"
  own <- list(
    credit_mortgage = if (src_stale("cfpb")) "stale" else "ok",
    credit_consumer = if (src_stale("urban")) "stale" else "ok",
    credit_collateral = if (src_stale("hpi", "msad")) "stale" else "ok",
    credit_jobs = jobs_state,
    tax_base = if (mig_stale || src_stale("hpi", "msad")) "stale" else "ok")
  worst <- function(x) if ("stale" %in% x) "stale" else if ("partial" %in% x) "partial" else "ok"
  own$credit_score <- worst(c(unlist(own[c("credit_mortgage", "credit_consumer", "credit_jobs",
                                           "credit_collateral")]), jobs_state))
  some <- function(col) any(!is.na(out[[col]]))
  avail <- c(credit_score = some("credit_score"),
             credit_mortgage = has[["cfpb"]] && some("dq90"),
             credit_consumer = has[["urban"]] && some("consumer_del"),
             credit_jobs = any(has) && some("cr_jobs"),
             credit_collateral = has[["hpi"]] && some("hpi_yoy"),
             tax_base = some("tax_base"))
  not_loaded <- is.null(credit) && !isTRUE(credit_requested)
  status <- vapply(ids, function(id) {
    load_now <- if (id == "tax_base") any(c("credit", "migration") %in% loading) else
      "credit" %in% loading
    if (load_now) return("loading")
    if (id != "tax_base" && not_loaded) return("not_loaded")
    if (!avail[[id]]) return("unavailable")
    own[[id]]
  }, "")
  mon <- function(ym) format(as.Date(paste0(ym, "-01")), "%b %Y")
  thru <- credit[["cfpb"]]$thru
  cfpb_m <- if (has[["cfpb"]] && length(thru) == 1 && !is.na(thru)) mon(thru) else NA_character_
  ua <- attr(credit[["urban"]], "asof")
  urban_rel <- if (!has[["urban"]]) NA_character_ else
    if (length(ua) == 1 && !is.na(ua)) paste("released", ua) else "release date unknown"
  q <- out$hpi_q[!is.na(out$hpi_q)]
  fhfa_q <- if (length(q)) max(q) else NA_character_
  jobs <- c(jobs_now, jobs_ahead); jobs <- jobs[!is.na(jobs)]
  # A source that failed for every county leaves the score on fewer components.
  gone <- c(CFPB = !has[["cfpb"]], "Urban Institute" = !has[["urban"]], FHFA = !has[["hpi"]])
  gone_note <- if (any(gone)) sprintf(" (%d of 4 components: %s unavailable)", 4L - sum(gone),
                                      paste(names(gone)[gone], collapse = ", ")) else ""
  asof <- c(
    credit_score = paste0(paste(c(if (!is.na(cfpb_m)) paste("CFPB", cfpb_m),
                           if (!is.na(urban_rel)) paste0("Urban Institute (", urban_rel, ")"),
                           if (!is.na(fhfa_q)) paste("FHFA", fhfa_q),
                           if (some("cr_jobs")) c(if (!is.na(jobs_now)) paste("BLS", jobs_now),
                                                  if (!is.na(jobs_ahead)) jobs_ahead)),
                         collapse = " · "), gone_note),
    credit_mortgage = if (is.na(cfpb_m)) NA_character_ else paste("CFPB thru", cfpb_m),
    credit_consumer = urban_rel,
    credit_jobs = if (length(jobs)) paste(jobs, collapse = " · ") else NA_character_,
    credit_collateral = if (is.na(fhfa_q)) NA_character_ else paste("FHFA", fhfa_q),
    tax_base = if (is.null(v)) NA_character_ else paste0(
      "PEP V", v, if (!is.na(fhfa_q)) paste(" · FHFA", fhfa_q) else
        if (not_loaded) " (home prices not loaded)" else " (home prices unavailable)"))
  asof[!status %in% c("ok", "stale", "partial")] <- NA_character_
  list(status = status, asof = asof,
       info = list(loaded = !is.null(credit), has = has, thru = cfpb_m, urban_release = urban_rel))
}

#' Net domestic migration per 1,000 residents for each CBSA, population-weighted
#' over its counties. NA when none of its counties has data.
cbsa_migration <- function(county_layers) {
  d <- sf::st_drop_geometry(county_layers)
  d <- d[!is.na(d$cbsa_geoid), , drop = FALSE]
  d |>
    dplyr::group_by(cbsa_geoid) |>
    dplyr::summarise(
      mig_dom_rate = if (all(is.na(mig_dom_n)) || all(is.na(pop_avg))) NA_real_ else
        round(1000 * sum(mig_dom_n, na.rm = TRUE) / sum(pop_avg, na.rm = TRUE), 1),
      .groups = "drop")
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

# ── Stress ahead: sector momentum and risk weights (spec §9.2) ───────────────
# FRED monthly dates are always the 1st, so month arithmetic is exact.
.months_back <- function(d, k) {
  lt <- as.POSIXlt(d)
  lt$mon <- lt$mon - k
  as.Date(lt)
}

# Annualized k-month % change at every observation (NA where the base is absent).
.momentum_series <- function(date, value, months) {
  base <- value[match(.months_back(date, months), date)]
  100 * ((value / base)^(12 / months) - 1)
}

#' Latest annualized change per series: v[t] vs the observation `months` earlier.
sector_momentum <- function(fred, months = RISK_CFG$sector_momentum_months) {
  fred <- dplyr::filter(fred, series_id != "USREC", !is.na(value))
  rows <- lapply(split(fred, fred$series_id), function(d) {
    d <- d[order(d$date), ]
    n <- nrow(d)
    tibble::tibble(series_id = d$series_id[1],
                   m_pct = .momentum_series(d$date, d$value, months)[n],
                   ref_month = d$date[n])
  })
  dplyr::bind_rows(rows)
}

#' Runs of consecutive USREC == 1 months that start on/after `since`.
recession_episodes <- function(fred, since = RISK_CFG$recession_since) {
  rec <- fred[fred$series_id == "USREC" & !is.na(fred$value) & fred$value == 1, ]
  rec <- rec[order(rec$date), ]
  if (nrow(rec) == 0) return(data.frame(start = as.Date(character()), end = as.Date(character())))
  lt <- as.POSIXlt(rec$date)
  run <- cumsum(c(TRUE, diff(lt$year * 12 + lt$mon) != 1))
  ep <- data.frame(start = as.Date(as.vector(tapply(rec$date, run, min)), origin = "1970-01-01"),
                   end   = as.Date(as.vector(tapply(rec$date, run, max)), origin = "1970-01-01"))
  ep <- ep[ep$start >= since, , drop = FALSE]
  rownames(ep) <- NULL
  ep
}

# Mean peak-to-trough % loss across recessions; 0 for a series that never fell.
.structural_decline <- function(d, episodes, cfg) {
  losses <- vapply(seq_len(nrow(episodes)), function(i) {
    win <- d[d$date >= .months_back(episodes$start[i], cfg$peak_lead_months) &
             d$date <= episodes$end[i], ]
    if (nrow(win) == 0) return(NA_real_)
    pk <- which.max(win$value)
    after <- d[d$date >= win$date[pk] &
               d$date <= .months_back(episodes$end[i], -cfg$trough_lag_months), ]
    100 * (1 - min(after$value) / win$value[pk])
  }, numeric(1))
  if (all(is.na(losses))) NA_real_ else mean(losses, na.rm = TRUE)
}

.rescale01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || r[2] - r[1] == 0) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

#' Risk weight per industry group: half "how badly it fell in past recessions"
#' (structural), half "how weak it is now versus its own history" (current_z).
#' Attributes: ref_month (latest sector obs), temphelp_m_pct (NA if absent).
sector_weights <- function(fred, cfg = RISK_CFG) {
  episodes <- recession_episodes(fred, cfg$recession_since)
  months <- cfg$sector_momentum_months
  mom <- sector_momentum(fred, months)

  rows <- lapply(RISK_SECTORS$fred, function(s) {
    d <- fred[fred$series_id == s & !is.na(fred$value), ]
    d <- d[order(d$date), ]
    hist <- .momentum_series(d$date, d$value, months)
    hist <- -hist[d$date >= cfg$recession_since & !is.na(hist)]
    m_now <- mom$m_pct[mom$series_id == s]
    if (length(m_now) == 0) m_now <- NA_real_
    sdv <- if (length(hist) > 1) stats::sd(hist) else NA_real_
    # No momentum now is unknown (NA). Flat or constant-growth history has no spread;
    # call that "not unusual" (0).
    z <- if (is.na(m_now)) NA_real_ else
      if (is.na(sdv) || sdv < 1e-8) 0 else (-m_now - mean(hist)) / sdv
    tibble::tibble(fred = s, m_pct = m_now,
                   structural = .structural_decline(d, episodes, cfg), current_z = z)
  })
  out <- dplyr::left_join(RISK_SECTORS[c("group", "label", "fred")],
                          dplyr::bind_rows(rows), by = "fred")
  cur <- .rescale01(out$current_z)
  # A sector with no current reading is weighed on its recession history alone.
  out$w <- ifelse(is.na(cur), .rescale01(out$structural),
                  cfg$w_structural * .rescale01(out$structural) + cfg$w_current * cur)
  out <- tibble::as_tibble(out)
  attr(out, "ref_month") <- suppressWarnings(
    max(mom$ref_month[mom$series_id %in% RISK_SECTORS$fred]))
  tp <- mom$m_pct[mom$series_id == "TEMPHELPS"]
  attr(out, "temphelp_m_pct") <- if (length(tp)) tp else NA_real_
  out
}

#' Shift-share: each county's resident industry mix times national sector payroll
#' momentum, as a gap versus the same calculation on the US mix (pp of annualized
#' job growth). Drivers are the 3 sectors with the most negative (share - US share)
#' x momentum. Counties with no employment get NA throughout, never NaN.
stress_ahead <- function(ind, weights) {
  groups <- RISK_SECTORS$group
  labels <- RISK_SECTORS$label
  m <- weights$m_pct[match(groups, weights$group)]
  partial <- anyNA(m)
  m[is.na(m)] <- 0                        # counted as neutral, but the result is flagged
  emp <- as.matrix(as.data.frame(ind)[paste0("emp_", groups)])
  total <- ind$emp_total
  has <- !is.na(total) & total > 0
  share <- emp / ifelse(has, total, NA_real_)
  us_share <- colSums(emp[has, , drop = FALSE], na.rm = TRUE) / sum(total[has])

  proj <- as.vector(share %*% m)
  us_proj <- sum(us_share * m)
  contrib <- sweep(sweep(share, 2, us_share), 2, m, `*`)

  out <- tibble::tibble(GEOID = ind$GEOID, proj_pct = proj, us_proj_pct = us_proj,
                        stress_ahead_pp = proj - us_proj)   # unrounded: display rounds, ranking must not
  # Top 3 sectors by contribution: worst first (drags), best first (lifts).
  top3 <- function(decreasing) t(apply(contrib, 1, function(r) {
    if (anyNA(r)) return(rep(NA_integer_, 3))
    order(r, decreasing = decreasing)[1:3]
  }))
  add_top <- function(out, prefix, idx_mat, keep) {
    for (k in 1:3) {
      idx <- idx_mat[, k]
      pp <- round(contrib[cbind(seq_len(nrow(contrib)), idx)], 2)
      ok <- !is.na(pp) & keep(pp)
      out[[sprintf("%s%d_label", prefix, k)]] <- ifelse(ok, labels[idx], NA_character_)
      out[[sprintf("%s%d_pp", prefix, k)]] <- ifelse(ok, pp, NA_real_)
    }
    out
  }
  out <- add_top(out, "sa_drv", top3(FALSE), function(pp) pp < 0)    # only real drags
  out <- add_top(out, "sa_lift", top3(TRUE), function(pp) pp > 0)    # shown when none drag
  for (i in seq_along(groups)) out[[paste0("share_", groups[i])]] <- share[, i]
  out$emp_total <- as.numeric(total)   # share x emp_total recovers jobs for the assistance lens
  for (a in c("ref_month", "temphelp_m_pct")) attr(out, a) <- attr(weights, a)
  attr(out, "acs_year") <- attr(ind, "acs_year")
  attr(out, "partial") <- partial
  out
}

# ── LODES residence-area workers (Phase 3 assistance lens) ───────────────────
#' Sum a block-level RAC table (h_geocode, C000, CA01, CE01, CNS01..CNS20) to
#' tracts. GEOID is the first 11 characters of h_geocode (must be character).
aggregate_lodes_rac <- function(blocks) {
  cols <- c("C000", "CA01", "CE01", sprintf("CNS%02d", 1:20))
  b <- as.data.frame(blocks)
  sums <- rowsum(b[cols], substr(as.character(b$h_geocode), 1, 11), na.rm = TRUE)
  tibble::as_tibble(cbind(GEOID = rownames(sums), sums, stringsAsFactors = FALSE),
                    .name_repair = "minimal")
}

#' LODES still keys Connecticut tracts by the old counties (09001-09015); ACS uses
#' planning regions (091xx). Match on the 6-digit tract code, skip "99" water
#' tracts, and drop CT rows with no ACS match. Other states pass through.
lodes_ct_remap <- function(lodes, acs_geoids) {
  ct <- acs_geoids[startsWith(acs_geoids, "09") & substr(acs_geoids, 6, 7) != "99"]
  is_ct <- startsWith(lodes$GEOID, "09")
  hit <- match(substr(lodes$GEOID, 6, 11), substr(ct, 6, 11))
  new <- lodes$GEOID
  new[is_ct] <- ct[hit[is_ct]]
  lodes$GEOID <- new
  lodes <- lodes[!(is_ct & is.na(lodes$GEOID)), , drop = FALSE]
  # Two old counties can share a tract code; fold them so GEOID stays unique.
  if (anyDuplicated(lodes$GEOID)) {
    num <- vapply(lodes, is.numeric, logical(1))
    sums <- rowsum(lodes[num], lodes$GEOID, reorder = FALSE)
    lodes <- tibble::as_tibble(cbind(GEOID = rownames(sums), sums, stringsAsFactors = FALSE),
                               .name_repair = "minimal")
  }
  lodes
}

# ── Assistance: exposure, cushion, job pressure, score ───────────────────────
# Job tables carry GEOID, jobs_total, jobs_<group> x13, low_wage_share and
# young_share. Zero jobs yields NA (never NaN/Inf).
.JOB_COLS <- paste0("jobs_", RISK_SECTORS$group)

.safe_pct <- function(num, den) {
  out <- 100 * num / den
  out[!is.finite(out)] <- NA_real_
  out
}

#' Sum LODES CNS columns into the 13 industry groups (RISK_SECTORS$cns).
lodes_group_jobs <- function(lodes) {
  d <- as.data.frame(lodes)
  out <- tibble::tibble(GEOID = as.character(d$GEOID), jobs_total = as.numeric(d$C000))
  for (i in seq_len(nrow(RISK_SECTORS))) {
    cols <- paste0("CNS", strsplit(RISK_SECTORS$cns[i], ",")[[1]])
    out[[.JOB_COLS[i]]] <- rowSums(d[cols], na.rm = TRUE)
  }
  out$low_wage_share <- .safe_pct(d$CE01, d$C000)
  out$young_share    <- .safe_pct(d$CA01, d$C000)
  out
}

#' Same shape from ACS industry counts (emp_total, emp_<group>). The ACS has no
#' wage or age bands, so those shares stay NA.
acs_group_jobs <- function(ind) {
  d <- as.data.frame(ind)
  out <- tibble::tibble(GEOID = as.character(d$GEOID), jobs_total = as.numeric(d$emp_total))
  for (i in seq_len(nrow(RISK_SECTORS))) {
    out[[.JOB_COLS[i]]] <- as.numeric(d[[paste0("emp_", RISK_SECTORS$group[i])]])
  }
  out$low_wage_share <- NA_real_
  out$young_share    <- NA_real_
  out
}

#' Exposure = job-share-weighted mean of sector weights (0..1); exp_workers is
#' the same weighting on headcount (an index-like count, not a layoff forecast);
#' weak_share = % of jobs in sectors with negative recent momentum (m_pct < 0).
#' An NA sector weight counts as 0 and an NA m_pct is not counted as weak, so a
#' sector missing from `weights` lowers exposure.
exposure_from_jobs <- function(jobs, weights) {
  na <- rep(NA_real_, nrow(jobs))
  out <- tibble::tibble(GEOID = jobs$GEOID, exposure = na, exp_workers = na, weak_share = na)
  if (is.null(weights) || nrow(jobs) == 0) return(out)
  idx <- match(RISK_SECTORS$group, weights$group)
  w <- weights$w[idx]
  weak <- !is.na(weights$m_pct[idx]) & weights$m_pct[idx] < 0
  J <- as.matrix(as.data.frame(jobs)[.JOB_COLS])
  J[is.na(J)] <- 0
  ok <- !is.na(jobs$jobs_total) & jobs$jobs_total > 0
  wj <- J %*% ifelse(is.na(w), 0, w)
  out$exposure    <- ifelse(ok, as.numeric(wj) / jobs$jobs_total, NA_real_)
  out$exp_workers <- ifelse(ok, round(as.numeric(wj)), NA_real_)
  out$weak_share  <- ifelse(ok, 100 * as.numeric(J %*% as.numeric(weak)) / jobs$jobs_total,
                            NA_real_)
  out
}

#' County job pressure, a 0..1 index (not a percentile): the mean of the available
#' Stress-now percentile and the inverted Stress-ahead percentile (lower Stress ahead
#' = more pressure), divided by 100. The Credit lens uses pct_rank(job_pressure) as its
#' component. Attributes: "n_inputs" (integer, 0-2 inputs used per county) and
#' "inputs" ("both", "now", "ahead" or NA).
job_pressure <- function(county_layers) {
  n <- nrow(county_layers)
  now <- if ("pct_momentum_pp" %in% names(county_layers))
    as.numeric(county_layers$pct_momentum_pp) / 100 else rep(NA_real_, n)
  ahead <- if ("pct_stress_ahead_pp" %in% names(county_layers))
    (100 - as.numeric(county_layers$pct_stress_ahead_pp)) / 100 else rep(NA_real_, n)
  out <- rowMeans(cbind(now, ahead), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  n <- as.integer(!is.na(now)) + as.integer(!is.na(ahead))
  attr(out, "n_inputs") <- n
  attr(out, "inputs") <- ifelse(n == 2L, "both", ifelse(n == 0L, NA_character_,
                                                       ifelse(is.na(ahead), "now", "ahead")))
  out
}

# pct_rank within a grouping vector; NA group or NA value stays NA.
.pct_in_group <- function(x, group) {
  out <- rep(NA_real_, length(x))
  for (g in unique(group[!is.na(group)])) {
    i <- which(group == g)
    out[i] <- pct_rank(x[i])
  }
  out[is.nan(out)] <- NA_real_
  out
}

#' Assistance score = geometric mean of exposure percentile, cushion percentile
#' (higher = thinner) and job pressure J, ranked within `group` (the frame id).
#' Cushion needs at least RISK_CFG$cushion_min_inputs available inputs.
assistance_score <- function(exposure, cushion_inputs, J, group, cfg = RISK_CFG) {
  pct_exposure <- .pct_in_group(exposure, group)
  pcts <- vapply(as.data.frame(cushion_inputs), .pct_in_group, numeric(length(exposure)),
                 group = group)
  pcts <- matrix(pcts, nrow = length(exposure))
  n_ok <- rowSums(!is.na(pcts))
  cushion <- rowMeans(pcts, na.rm = TRUE)
  cushion[n_ok < cfg$cushion_min_inputs] <- NA_real_
  pct_cushion <- .pct_in_group(cushion, group)
  score <- round(100 * ((pct_exposure / 100) * (pct_cushion / 100) * J)^(1 / 3), 1)
  tibble::tibble(pct_exposure = pct_exposure, cushion = cushion,
                 pct_cushion = pct_cushion, score = score)
}

# ── Assistance in the tract drill-down ───────────────────────────────────────
#' Job table for one state: LODES when available, else ACS resident industry, else
#' NULL. attr "source" carries the text shown beside the score.
.tract_jobs <- function(state_fips, acs_geoids) {
  lodes <- fetch_lodes_rac(state_fips)
  if (!is.null(lodes)) {
    yr <- attr(lodes, "lodes_year")   # read before remap: `[` drops attributes
    jobs <- lodes_group_jobs(lodes_ct_remap(lodes, acs_geoids))
    attr(jobs, "source") <- if (is.null(yr)) "LODES" else sprintf("LODES %s", yr)
    return(jobs)
  }
  ind <- fetch_state_tract_industry(state_fips)
  if (is.null(ind)) return(NULL)
  y <- attr(ind, "acs_year")
  why <- if (state_fips %in% RISK_CFG$lodes_exclude_states) "incomplete for this state" else
    "unavailable"
  jobs <- acs_group_jobs(ind)
  attr(jobs, "source") <- sprintf("ACS %d\u2013%d (LODES %s)", y - 4, y, why)
  jobs
}

#' Add the assistance columns to tracts (all loaded states, any frame).
#' `jobs`: bound job tables (GEOID, jobs_*, shares, optional assist_source) or NULL.
#' Exposure and workers do not depend on the frame, so every tract gets them; the
#' cushion percentile and score rank within the frame (tracts in `frame_counties`,
#' NA elsewhere), and the `_st` twins rank within each state. NULL `weights` blanks
#' every numeric assist_* column.
assistance_tracts <- function(tracts, jobs, weights, county_layers, frame_counties) {
  n <- nrow(tracts)
  na <- rep(NA_real_, n)
  idx <- if (is.null(jobs)) rep(NA_integer_, n) else match(tracts$GEOID, jobs$GEOID)
  jb <- tibble::tibble(GEOID = tracts$GEOID, jobs_total = na)
  for (col in .JOB_COLS) jb[[col]] <- na
  jb$low_wage_share <- na
  jb$young_share <- na
  if (!is.null(jobs)) {
    for (col in setdiff(names(jb), "GEOID")) {
      if (col %in% names(jobs)) jb[[col]] <- jobs[[col]][idx]
    }
  }
  e <- exposure_from_jobs(jb, weights)
  tracts$exposure <- e$exposure
  tracts$exp_workers <- e$exp_workers
  tracts$weak_share <- e$weak_share
  tracts$low_wage_share <- jb$low_wage_share
  tracts$young_share <- jb$young_share
  tracts$assist_source <- if (!is.null(jobs) && "assist_source" %in% names(jobs))
    jobs$assist_source[idx] else rep(NA_character_, n)
  jp <- job_pressure(county_layers)
  at <- match(tracts$county_geoid, county_layers$GEOID)
  tracts$job_pressure <- as.numeric(jp)[at]
  tracts$job_pressure_n <- attr(jp, "n_inputs")[at]
  tracts$job_pressure_from <- attr(jp, "inputs")[at]

  col_or_na <- function(v) if (v %in% names(tracts)) as.numeric(tracts[[v]]) else na
  cushion_in <- data.frame(low_wage_share = tracts$low_wage_share,
                           young_share = tracts$young_share,
                           rent_burden_rate = col_or_na("rent_burden_rate"),
                           poverty_rate = col_or_na("poverty_rate"))
  frame_grp <- ifelse(tracts$county_geoid %in% frame_counties, "frame", NA_character_)
  fr <- assistance_score(e$exposure, cushion_in, tracts$job_pressure, frame_grp)
  st <- assistance_score(e$exposure, cushion_in, tracts$job_pressure,
                         substr(tracts$GEOID, 1, 2))
  blank <- is.null(weights)
  tracts$assist_score <- fr$score
  tracts$assist_exposure <- round(100 * e$exposure, 1)
  tracts$assist_cushion <- if (blank) na else fr$pct_cushion
  tracts$assist_workers <- e$exp_workers
  tracts$assist_score_st <- st$score
  tracts$assist_cushion_st <- if (blank) na else st$pct_cushion
  tracts
}

# ── CFPB mortgage delinquency (Phase 4 credit lens) ──────────────────────────

#' Wide CFPB file (one column per YYYY-MM, all character) -> long tibble(id, date, pct).
#' `id_col` is "FIPSCode" (quotes stripped) or "CBSACode"; National and id-less rows drop.
parse_cfpb_wide <- function(df, id_col) {
  months <- grep("^\\d{4}-\\d{2}$", names(df), value = TRUE)
  id <- gsub("[' ]", "", as.character(df[[id_col]]))
  keep <- !is.na(id) & nzchar(id) & !grepl("^-+$", id)
  if ("RegionType" %in% names(df)) keep <- keep & !(df$RegionType %in% "National")
  if (!any(keep) || !length(months)) {
    return(tibble::tibble(id = character(), date = as.Date(character()), pct = numeric()))
  }
  m <- as.matrix(df[keep, months, drop = FALSE])
  out <- tibble::tibble(
    id = rep(id[keep], times = length(months)),
    date = rep(as.Date(paste0(months, "-01")), each = sum(keep)),
    pct = suppressWarnings(as.numeric(m)))
  out[!is.na(out$pct), ]
}

#' Per-county CFPB inputs for the credit score. Lookup order per county: its own
#' county row, then its CBSA's metro row, then the state's non-metro row ("{ss}-non").
#' CFPB's metro file covers metropolitan areas only and its non-metro row covers micro
#' plus rural, so the state row applies only to counties with no CBSA or a micro one
#' (`cbsa_type_of` = "micro area"). Counties still without a value take the state row
#' (state90 / state3089, optional; source "state") as a last resort: metro-CBSA counties
#' missing from the metro file and states with no non-metro row.
#' dq90 = 90+ days late at the 90+ file's latest month d; dq3089_chg = 30-89 at d minus
#' 30-89 at d - 12 months (pp), from the same source row; NA if either month is absent.
cfpb_county_components <- function(county_ids, cbsa_of, cbsa_type_of, state_of,
                                   county90, county3089, metro90, metro3089,
                                   state90 = NULL, state3089 = NULL) {
  n <- length(county_ids)
  cbsa_of <- rep_len(as.character(cbsa_of), n)
  cbsa_type_of <- rep_len(as.character(cbsa_type_of), n)
  state_of <- rep_len(as.character(state_of), n)
  key <- function(tbl) paste(tbl$id, format(tbl$date))
  latest <- function(tbl) if (nrow(tbl)) max(tbl$date) else as.Date(NA)
  back12 <- function(d) if (is.na(d)) d else seq(d, by = "-1 year", length.out = 2)[2]
  at <- function(tbl, ids, d) {
    if (is.na(d)) return(rep(NA_real_, length(ids)))
    tbl$pct[match(paste(ids, format(d)), key(tbl))]
  }

  metro_ids <- ifelse(!is.na(cbsa_of) & !grepl("-non$", cbsa_of) &
                        cbsa_of %in% metro90$id, cbsa_of, NA_character_)
  nonmetro_ok <- is.na(cbsa_of) | cbsa_type_of %in% "micro area"
  nonmetro_ids <- ifelse(nonmetro_ok & !is.na(state_of) & nzchar(state_of),
                         paste0(state_of, "-non"), NA_character_)
  levels <- list(
    list(src = "county", ids = as.character(county_ids), t90 = county90, t3089 = county3089),
    list(src = "metro", ids = metro_ids, t90 = metro90, t3089 = metro3089),
    list(src = "state non-metro", ids = nonmetro_ids, t90 = metro90, t3089 = metro3089))
  if (!is.null(state90) && !is.null(state3089)) {
    # Last resort: the state's all-areas row, for counties no finer level covers.
    levels[[4]] <- list(src = "state", ids = ifelse(!is.na(state_of) & nzchar(state_of), state_of, NA_character_),
                        t90 = state90, t3089 = state3089)
  }

  out <- tibble::tibble(GEOID = as.character(county_ids), dq90 = NA_real_,
                        dq3089_chg = NA_real_, cfpb_source = NA_character_,
                        cfpb_month = as.Date(NA))
  for (lv in levels) {
    d <- latest(lv$t90)
    v90 <- at(lv$t90, lv$ids, d)
    take <- is.na(out$cfpb_source) & !is.na(v90)
    if (!any(take)) next
    chg <- at(lv$t3089, lv$ids, d) - at(lv$t3089, lv$ids, back12(d))
    out$dq90[take] <- v90[take]
    out$dq3089_chg[take] <- round(chg[take], 2)
    out$cfpb_source[take] <- lv$src
    out$cfpb_month[take] <- d
  }
  out
}

# ── Urban consumer delinquency + FHFA HPI (Phase 4 credit lens) ──────────────
#' Urban county_overall.csv (character columns) -> percent of consumers with a
#' delinquent card / auto-retail debt. Values are fractions; suppressed cells are
#' the string "NA" and become NA.
parse_urban_county <- function(df) {
  num <- function(x) 100 * suppressWarnings(as.numeric(x))
  id <- trimws(as.character(df$id))
  id <- ifelse(grepl("^[0-9]{1,5}$", id), sprintf("%05d", as.integer(id)), id)
  tibble::tibble(GEOID = id, card_del = num(df$carddelrate), auto_del = num(df$autoretdelrate))
}

#' FHFA quarterly HPI (headerless, raw character columns) -> latest year-over-year
#' % change per code. Metro files have 6 columns (name, code, year, quarter, index,
#' se), state files 4 (abbr, year, quarter, index). Uses each code's latest quarter
#' with an index ("-" is missing) against the same quarter a year earlier.
hpi_yoy <- function(df) {
  off <- if (ncol(df) >= 6) 1L else 0L
  d <- tibble::tibble(
    code = trimws(as.character(df[[1 + off]])),
    year = suppressWarnings(as.integer(df[[2 + off]])),
    q = suppressWarnings(as.integer(df[[3 + off]])),
    idx = suppressWarnings(as.numeric(df[[4 + off]])))
  d <- d[!is.na(d$idx) & !is.na(d$year) & !is.na(d$q), ]
  d$t <- d$year * 4L + d$q
  d <- d[order(d$code, -d$t), ]
  last <- d[!duplicated(d$code), ]
  prev <- d$idx[match(paste(last$code, last$t - 4L), paste(d$code, d$t))]
  tibble::tibble(code = last$code,
                 hpi_yoy = round(100 * (last$idx / prev - 1), 2),
                 hpi_q = sprintf("%d Q%d", last$year, last$q))
}

#' County HPI for the credit lens. Metro counties take their metro division's value
#' when the county has one present in the metro file, else the CBSA's; micro, non-CBSA
#' and metros missing from the file take the state value. `hpi` = list(metro, state)
#' from fetch_fhfa_hpi(); `msad_xwalk` = tibble(GEOID, msad_code) or NULL.
county_hpi <- function(cb, hpi, msad_xwalk = NULL) {
  cb <- sf::st_drop_geometry(cb)
  pick <- function(tbl, codes) {
    ok <- tbl[!is.na(tbl$hpi_yoy), ]
    i <- match(codes, ok$code)
    list(yoy = ok$hpi_yoy[i], q = ok$hpi_q[i])
  }
  n <- nrow(cb)
  is_metro <- cb$cbsa_type %in% "metro"
  msad <- if (is.null(msad_xwalk)) rep(NA_character_, n) else
    msad_xwalk$msad_code[match(cb$GEOID, msad_xwalk$GEOID)]
  div <- pick(hpi$metro, ifelse(is_metro, msad, NA_character_))
  met <- pick(hpi$metro, ifelse(is_metro, as.character(cb$cbsa_geoid), NA_character_))
  st <- pick(hpi$state, as.character(cb$state_abbr))
  src <- ifelse(!is.na(div$yoy), "metro division",
         ifelse(!is.na(met$yoy), "metro",
         ifelse(!is.na(st$yoy), "state", NA_character_)))
  tibble::tibble(
    GEOID = cb$GEOID,
    hpi_yoy = dplyr::coalesce(div$yoy, met$yoy, st$yoy),
    hpi_source = src,
    hpi_q = dplyr::coalesce(div$q, met$q, st$q))
}

# ── Credit components, score and tax base proxy (Phase 4) ────────────────────
# percent_rank gives NaN for a lone value; keep missing as NA, never NaN.
.pr <- function(x) {
  r <- pct_rank(as.numeric(x))
  r[is.nan(r)] <- NA_real_
  r
}

# Row-wise mean of the available columns; NA when none.
.avail_mean <- function(...) {
  m <- rowMeans(cbind(...), na.rm = TRUE)
  m[is.nan(m)] <- NA_real_
  m
}

# Column `col` of a per-county table `tbl`, aligned to `ids` by GEOID; NA if absent.
.by_geoid <- function(tbl, col, ids, na = NA_real_) {
  if (is.null(tbl) || !col %in% names(tbl)) return(rep(na, length(ids)))
  tbl[[col]][match(ids, tbl$GEOID)]
}

#' Credit lens components, one row per county in `cl`, each a national percentile
#' (0-100, high = more credit risk). `cfpb`, `urban`, `hpi` are the per-county tables
#' from cfpb_county_components(), parse_urban_county() and county_hpi(), joined by
#' GEOID; any may be NULL. cr_mortgage re-ranks the mean of the level and change
#' percentiles; cr_jobs is pct_rank(cl$job_pressure) (job_pressure is already computed).
#' credit_score is the mean of available components, NA below
#' RISK_CFG$credit_min_components; credit_missing names the absent ones ("" if none).
credit_components <- function(cl, cfpb = NULL, urban = NULL, hpi = NULL,
                              cfg = RISK_CFG) {
  cl <- .plain_df(cl)
  ids <- as.character(cl$GEOID)
  dq90 <- as.numeric(.by_geoid(cfpb, "dq90", ids))
  dq_chg <- as.numeric(.by_geoid(cfpb, "dq3089_chg", ids))
  card <- as.numeric(.by_geoid(urban, "card_del", ids))
  auto <- as.numeric(.by_geoid(urban, "auto_del", ids))
  yoy <- as.numeric(.by_geoid(hpi, "hpi_yoy", ids))

  # Averaged inputs are ranked again so every component is a true national percentile.
  cr_mortgage <- .pr(.avail_mean(.pr(dq90), .pr(dq_chg)))
  cr_consumer <- .pr(.avail_mean(card, auto))
  cr_jobs <- if ("job_pressure" %in% names(cl)) .pr(cl$job_pressure) else
    rep(NA_real_, length(ids))
  cr_collateral <- 100 - .pr(yoy)

  comps <- cbind(cr_mortgage, cr_consumer, cr_jobs, cr_collateral)
  labels <- c("mortgage", "consumer", "job pressure", "collateral")
  n <- rowSums(!is.na(comps))
  score <- round(.avail_mean(comps), 1)
  score[n < cfg$credit_min_components] <- NA_real_
  missing <- apply(is.na(comps), 1, function(m) paste(labels[m], collapse = ", "))

  tibble::tibble(
    GEOID = ids,
    cr_mortgage = cr_mortgage, cr_consumer = cr_consumer,
    cr_jobs = cr_jobs, cr_collateral = cr_collateral,
    credit_n = as.integer(n), credit_score = score, credit_missing = unname(missing),
    dq90 = dq90, dq3089_chg = dq_chg,
    cfpb_source = as.character(.by_geoid(cfpb, "cfpb_source", ids, NA_character_)),
    cfpb_month = as.character(.by_geoid(cfpb, "cfpb_month", ids, NA_character_)),
    card_del = card, auto_del = auto,
    hpi_yoy = yoy,
    hpi_source = as.character(.by_geoid(hpi, "hpi_source", ids, NA_character_)),
    hpi_q = as.character(.by_geoid(hpi, "hpi_q", ids, NA_character_)))
}

#' Tax base pressure proxy (0-100, high = weaker base): mean of inverted national
#' percentiles of net domestic migration rate, population change and HPI change.
#' Needs at least 2 of the 3; hpi_yoy may be absent. A proxy, not a bond default model.
tax_base_proxy <- function(cl) {
  cl <- .plain_df(cl)
  inv <- function(col) {
    if (!col %in% names(cl)) return(rep(NA_real_, nrow(cl)))
    100 - .pr(cl[[col]])
  }
  parts <- cbind(inv("mig_dom_rate"), inv("pop_chg_pct"), inv("hpi_yoy"))
  out <- round(.avail_mean(parts), 1)
  out[rowSums(!is.na(parts)) < 2] <- NA_real_
  out
}

# ── Regional risk context for the Metro AI ────────────────────────────────────
#' The county picked on the Regional Risk map, for the LLM prompt: each layer's own value
#' with its percentile among all US counties (100 = highest value, not necessarily worst).
#' NULL when no county is given or it is not in `county_layers`. NA values are dropped.
county_risk_context <- function(county_layers, county_geoid) {
  if (is.null(county_geoid) || length(county_geoid) != 1 || is.na(county_geoid)) return(NULL)
  d <- .plain_df(county_layers)
  r <- d[d$GEOID == county_geoid, , drop = FALSE]
  if (nrow(r) == 0) return(NULL)
  r <- r[1, , drop = FALSE]
  v <- function(nm, dg = 1) {
    x <- if (nm %in% names(r)) suppressWarnings(as.numeric(r[[nm]])) else NA_real_
    if (length(x) != 1 || is.na(x)) return(NULL)
    out <- list(value = round(x, dg))
    p <- if (paste0("pct_", nm) %in% names(r)) as.numeric(r[[paste0("pct_", nm)]]) else NA_real_
    if (length(p) == 1 && !is.na(p)) out$us_percentile <- round(p)
    out
  }
  keep <- function(x) x[!vapply(x, is.null, TRUE)]
  status <- attr(county_layers, "layer_status"); asof <- attr(county_layers, "asof")
  gaps <- c(
    if (!is.null(status) && any(status[c("credit_score")] %in% "not_loaded", na.rm = TRUE))
      "Credit layers not loaded for this county (open the Credit lens first).")
  out <- list(
    county = r$name, state = if ("state_name" %in% names(r)) r$state_name,
    metro = if (!is.na(r$cbsa_geoid)) paste(r$cbsa_name, r$cbsa_type) else "not in a metro/micro area",
    population = round(as.numeric(r$pop)),
    note = paste("value = this county's reading; us_percentile = share of US counties below it.",
                 "Stress ahead is the projected job-growth gap vs. the US (negative = worse).",
                 "Assistance and credit scores run 0-100, higher = more need/risk."),
    unemployment = keep(list(
      rate_now_pct = v("rate_now"), change_vs_year_ago_pp = v("momentum_pp", 2))),
    stress_ahead_projected_job_growth_gap_pp = v("stress_ahead_pp", 2),
    now_vs_ahead_quadrant = {
      q <- if ("now_vs_ahead" %in% names(r)) suppressWarnings(as.numeric(r$now_vs_ahead)) else NA
      if (length(q) != 1 || is.na(q)) NULL else QUAD_LABELS[q]
    },
    now_vs_ahead_reason = {
      w <- .quad_why(r)
      keepw <- c(summary = w$headline, now = w$now, ahead = w$ahead)
      keepw <- keepw[!is.na(keepw)]
      if (length(keepw)) as.list(keepw) else NULL
    },
    assistance_need = keep(list(
      score_0_100 = v("assist_score", 0), exposure_to_weakening_sectors_index = v("assist_exposure", 0),
      thin_cushion_percentile = v("assist_cushion", 0),
      share_of_jobs_in_weakening_sectors_pct = v("weak_share"),
      exposure_weighted_workers_index_headcount = v("assist_workers", 0),
      rent_burden_pct = v("rent_burden_rate"), poverty_pct = v("poverty_rate"),
      median_household_income = v("med_income", 0))),
    credit = keep(list(
      risk_score_0_100 = v("credit_score", 0), mortgages_90_plus_days_late_pct = v("dq90", 2),
      card_delinquent_pct = v("card_del"), auto_delinquent_pct = v("auto_del"),
      home_prices_yoy_pct = v("hpi_yoy"))),
    migration_and_population = keep(list(
      net_domestic_per_1000 = v("mig_dom_rate"), net_international_per_1000 = v("mig_intl_rate"),
      natural_change_per_1000 = v("nat_chg_rate"), population_change_since_2020_pct = v("pop_chg_pct"))),
    tax_base_pressure_0_100 = v("tax_base", 0),
    data_gaps = if (length(gaps)) as.list(gaps))
  out <- out[!vapply(out, function(e) is.null(e) || length(e) == 0, TRUE)]
  if (!is.null(asof)) out$as_of <- as.list(unique(stats::na.omit(unname(
    asof[intersect(c("stress_now", "assist_score", "credit_score", "mig_dom"), names(asof))]))))
  out
}

#' What the Regional Risk map knows about one metro, as a list for the LLM prompt: its
#' counties' layers rolled up (population-weighted) and set against the US, the largest
#' counties with their own values, and notes on anything missing. NULL when the CBSA has
#' no counties in `county_layers`. NA values are dropped, never sent as "NA".
metro_risk_context <- function(county_layers, cbsa_geoid, selected_county = NULL) {
  d <- .plain_df(county_layers)
  if (is.null(d) || !"cbsa_geoid" %in% names(d)) return(NULL)
  m <- d[!is.na(d$cbsa_geoid) & d$cbsa_geoid == cbsa_geoid, , drop = FALSE]
  if (nrow(m) == 0) return(NULL)

  col <- function(df, nm) if (nm %in% names(df)) as.numeric(df[[nm]]) else rep(NA_real_, nrow(df))
  wmean <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
  }
  # metro value (weighted by `w`), the same measure for all US counties, and the gap
  roll <- function(nm, w = "pop", digits = 1) {
    mv <- wmean(col(m, nm), col(m, w))
    uv <- wmean(col(d, nm), col(d, w))
    if (is.na(mv)) return(NULL)
    drop_na(list(metro = round(mv, digits), us = round(uv, digits),
                 vs_us = round(mv - uv, digits)))
  }
  drop_na <- function(x) x[!vapply(x, function(e) is.null(e) || (length(e) == 1 && is.na(e)), TRUE)]

  asof <- attr(county_layers, "asof"); status <- attr(county_layers, "layer_status")
  layer_meta <- function(ids) {
    ids <- ids[ids %in% names(status)]
    list(as_of = unique(stats::na.omit(unname(asof[ids]))), status = unique(unname(status[ids])))
  }

  # Unemployment: the metro's own BLS series, plus how its counties are moving.
  mom <- col(m, "momentum_pp")
  unemp <- drop_na(list(
    metro_rate_now_pct = if (all(is.na(col(m, "metro_rate_now")))) NULL else round(col(m, "metro_rate_now")[1], 1),
    metro_change_vs_year_ago_pp = if (all(is.na(col(m, "metro_momentum_pp")))) NULL else round(col(m, "metro_momentum_pp")[1], 2),
    metro_ref_month = if (all(is.na(m$metro_ref_month))) NULL else format(m$metro_ref_month[1], "%b %Y"),
    county_change_vs_year_ago_pp_pop_weighted = if (all(is.na(mom))) NULL else round(wmean(mom, col(m, "pop")), 2),
    counties_with_data = sum(!is.na(mom)),
    counties_rising_over_0.5pp = sum(mom > 0.5, na.rm = TRUE),
    counties_at_sahm_trigger = sum(m$flag %in% "Trigger"),
    us_county_change_pp_pop_weighted = round(wmean(col(d, "momentum_pp"), col(d, "pop")), 2)))
  if (length(unemp)) unemp$as_of <- layer_meta("stress_now")$as_of

  # Industry mix: the biggest resident-industry shares (employment-weighted).
  shares <- vapply(RISK_SECTORS$group, function(g) wmean(col(m, paste0("share_", g)), col(m, "emp_total")),
                   numeric(1))
  shares <- sort(shares[!is.na(shares)], decreasing = TRUE)
  mix <- if (length(shares)) as.list(stats::setNames(
    round(100 * utils::head(shares, 5), 1), RISK_SECTORS$label[match(names(utils::head(shares, 5)), RISK_SECTORS$group)]))

  ahead <- roll("stress_ahead_pp", digits = 2)
  assist <- drop_na(list(
    assistance_need_score_0_100 = roll("assist_score"),
    exposure_to_weakening_sectors_index = roll("assist_exposure"),
    thin_cushion_percentile = roll("assist_cushion"),
    share_of_jobs_in_weakening_sectors_pct = if (is.null(roll("weak_share", "emp_total"))) NULL else
      roll("weak_share", "emp_total"),
    exposure_weighted_workers_index_headcount = if (all(is.na(col(m, "assist_workers")))) NULL else
      round(sum(col(m, "assist_workers"), na.rm = TRUE)),
    job_pressure_0_1 = roll("job_pressure", digits = 2),
    rent_burden_pct = roll("rent_burden_rate"), poverty_pct = roll("poverty_rate")))
  credit <- drop_na(list(
    credit_risk_score_0_100 = roll("credit_score"),
    mortgages_90_plus_days_late_pct = roll("dq90", digits = 2),
    card_delinquent_pct = roll("card_del"), auto_delinquent_pct = roll("auto_del"),
    home_prices_yoy_pct = roll("hpi_yoy")))
  migr <- drop_na(list(
    net_domestic_per_1000 = {
      dn <- col(m, "mig_dom_n"); pa <- col(m, "pop_avg")
      if (all(is.na(dn)) || all(is.na(pa))) NULL else
        list(metro = round(1000 * sum(dn, na.rm = TRUE) / sum(pa, na.rm = TRUE), 1),
             us = round(wmean(col(d, "mig_dom_rate"), col(d, "pop_avg")), 1))
    },
    net_international_per_1000 = roll("mig_intl_rate", "pop_avg"),
    natural_change_per_1000 = roll("nat_chg_rate", "pop_avg"),
    population_change_since_2020_pct = roll("pop_chg_pct"),
    domestic_migration_since_2020_per_1000 = roll("mig_dom_cum", "pop_avg")))
  tax <- roll("tax_base")

  # Largest counties with their own readings.
  top <- m[order(-col(m, "pop"), na.last = TRUE), , drop = FALSE]
  top <- utils::head(top, 10)
  counties <- lapply(seq_len(nrow(top)), function(i) {
    r <- top[i, , drop = FALSE]
    v <- function(nm, dg = 1) { x <- col(r, nm); if (is.na(x)) NULL else round(x, dg) }
    drop_na(list(county = r$name, population = v("pop", 0),
                 unemployment_pct = v("rate_now"), change_vs_year_ago_pp = v("momentum_pp", 2),
                 stress_ahead_pp = v("stress_ahead_pp", 2), assistance_score = v("assist_score", 0),
                 credit_score = v("credit_score", 0),
                 net_domestic_migration_per_1000 = v("mig_dom_rate")))
  })

  not_ok <- function(ids, what) {
    st <- status[intersect(ids, names(status))]
    if (!length(st)) return(NULL)
    if (any(st == "not_loaded")) return(sprintf("%s not loaded yet (open the Credit lens to load it) — not included.", what))
    if (all(st %in% c("unavailable", "loading"))) return(sprintf("%s unavailable — not included.", what))
    if (any(st %in% c("stale", "partial"))) return(sprintf("%s is %s.", what, paste(unique(st[st %in% c("stale", "partial")]), collapse = "/")))
    NULL
  }
  notes <- c(
    not_ok("stress_now", "County unemployment (BLS)"), not_ok("stress_ahead", "Stress ahead"),
    not_ok("assist_score", "Assistance layers"), not_ok("credit_score", "Credit layers"),
    not_ok("mig_dom", "Migration"), not_ok("tax_base", "Tax base pressure"),
    if (all(is.na(col(m, "metro_momentum_pp")))) "Metro-level unemployment series missing; county figures only.")

  out <- list(
    counties_in_metro = nrow(m),
    selected_county = county_risk_context(county_layers, selected_county),
    note = paste("Values are population-weighted over the metro's counties unless stated;",
                 "'us' is the same measure over all US counties; vs_us is metro minus US.",
                 "Stress ahead is the projected job-growth gap vs. the US (negative = worse).",
                 "Assistance and credit scores run 0-100, higher = more need/risk."),
    unemployment = if (length(unemp)) unemp,
    industry_mix_share_of_jobs_pct = mix,
    stress_ahead_projected_job_growth_gap_pp = if (!is.null(ahead)) c(ahead, list(as_of = layer_meta("stress_ahead")$as_of)),
    assistance_need = if (length(assist)) c(assist, list(as_of = layer_meta("assist_score")$as_of)),
    credit = if (length(credit)) c(credit, list(as_of = layer_meta("credit_score")$as_of)),
    migration_and_population = if (length(migr)) migr,
    tax_base_pressure_0_100 = tax,
    largest_counties = counties,
    data_gaps = if (length(notes)) as.list(notes))
  out[!vapply(out, is.null, TRUE)]
}

#' Which metro or micro area the Metro AI should analyse: the "Jump to metro" pick, else the
#' metro of the county selected on the map. The selected county is only passed on when it is
#' inside that area. NULL when neither points at a CBSA in `county_layers`.
metro_ai_target <- function(county_layers, jump = NULL, selected_county = NULL) {
  d <- .plain_df(county_layers)
  if (is.null(d) || !"cbsa_geoid" %in% names(d)) return(NULL)
  has <- function(x) length(x) == 1 && !is.na(x) && nzchar(x)
  src <- "jump"
  geoid <- if (has(jump)) jump else NULL
  if (is.null(geoid) && has(selected_county)) {
    g <- d$cbsa_geoid[d$GEOID == selected_county]
    if (length(g) && !is.na(g[1])) { geoid <- g[1]; src <- "map" }
  }
  if (is.null(geoid)) return(NULL)
  m <- d[!is.na(d$cbsa_geoid) & d$cbsa_geoid == geoid, , drop = FALSE]
  if (nrow(m) == 0) return(NULL)
  list(geoid = geoid, name = m$cbsa_name[1], type = m$cbsa_type[1], source = src,
       counties = nrow(m),
       selected_county = if (has(selected_county) && selected_county %in% m$GEOID) selected_county)
}
