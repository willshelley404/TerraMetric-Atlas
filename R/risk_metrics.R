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
  w_current           = 0.5
)

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
#' loading: component names still loading ("stress_now", "migration", "stress_ahead");
#' progressive tab load: the map draws from shapes + ACS first, the rest fills in.
#' migration: parse_pep_county() output (attr "stale" from risk_cached) or NULL.
#' stress_ahead: stress_ahead() output with attr "stale" (set by the caller when FRED or
#' ACS came from a stale cache), or NULL.
#' sector_weights: sector_weights() output (needs FRED only, so it survives a missing ACS
#' industry pull); falls back to attr(stress_ahead, "weights").
#' metro_momentum: laus_momentum() on the metro pull (fips = CBSA code) or NULL; joined
#' onto counties by cbsa_geoid as metro_momentum_pp / metro_rate_now / metro_ref_month.
build_county_layers <- function(county_base, momentum, laus_stale = FALSE,
                                laus_partial = FALSE,
                                migration = NULL, loading = character(0),
                                stress_ahead = NULL, metro_momentum = NULL,
                                sector_weights = NULL) {
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
                  paste0("share_", RISK_SECTORS$group))) out[[col]] <- NA_real_
    for (col in paste0("sa_drv", 1:3, "_label")) out[[col]] <- NA_character_
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
  for (id in names(RISK_LAYERS)) {
    col <- RISK_LAYERS[[id]]$col
    out[[paste0("pct_", col)]] <- pct_rank(out[[col]])
  }

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
  rm_sa <- if (is.null(stress_ahead)) NULL else attr(stress_ahead, "ref_month")
  asof[["stress_ahead"]] <- if (is.null(rm_sa) || is.na(rm_sa)) NA_character_ else
    sprintf("CES %s · ACS %d–%d", format(rm_sa, "%b %Y"), yr - 4, yr)
  attr(out, "layer_status") <- status
  attr(out, "asof") <- asof
  attr(out, "acs_year") <- yr
  attr(out, "sector_weights") <- sector_weights %||%
    (if (is.null(stress_ahead)) NULL else attr(stress_ahead, "weights"))
  if (!is.null(v)) attr(out, "pep_vintage") <- v
  out
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
                        stress_ahead_pp = round(proj - us_proj, 2))
  worst <- t(apply(contrib, 1, function(r) {
    if (anyNA(r)) return(rep(NA_integer_, 3))
    order(r)[1:3]
  }))
  for (k in 1:3) {
    idx <- worst[, k]
    pp <- round(contrib[cbind(seq_len(nrow(contrib)), idx)], 2)
    drag <- !is.na(pp) & pp < 0           # only real drags are listed
    out[[sprintf("sa_drv%d_label", k)]] <- ifelse(drag, labels[idx], NA_character_)
    out[[sprintf("sa_drv%d_pp", k)]] <- ifelse(drag, pp, NA_real_)
  }
  for (i in seq_along(groups)) out[[paste0("share_", groups[i])]] <- share[, i]
  for (a in c("ref_month", "temphelp_m_pct")) attr(out, a) <- attr(weights, a)
  attr(out, "acs_year") <- attr(ind, "acs_year")
  attr(out, "partial") <- partial
  out
}
