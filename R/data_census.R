# ─────────────────────────────────────────────────────────────────────────────
# R/data_census.R — Census/ACS + tigris choropleth + metro analysis
# ─────────────────────────────────────────────────────────────────────────────

options(tigris_use_cache = TRUE)

# ── ACS variables ─────────────────────────────────────────────────────────────
ACS_VARS <- c(
  # Population
  total_pop              = "B01003_001",
  median_age             = "B01002_001",
  # Labor market
  labor_force            = "B23025_002",
  employed               = "B23025_004",
  unemployed             = "B23025_005",
  not_in_labor_force     = "B23025_007",
  # Income
  median_hh_income       = "B19013_001",
  per_capita_income      = "B19301_001",
  # Poverty
  poverty_universe       = "B17001_001",
  poverty_count          = "B17001_002",
  # Housing
  housing_units          = "B25001_001",
  occupied_units         = "B25002_002",
  vacant_units           = "B25002_003",
  median_home_value      = "B25077_001",
  median_rent            = "B25064_001",
  # Rent burden: renters paying >=30% of income (B25070 bins 30-34.9 ... 50+)
  rent_burden_universe   = "B25070_001",
  rb_30_34               = "B25070_007",
  rb_35_39               = "B25070_008",
  rb_40_49               = "B25070_009",
  rb_50_plus             = "B25070_010",
  rb_not_computed        = "B25070_011",
  # Education (human capital)
  education_total_25plus = "B15003_001",
  bachelors              = "B15003_022",
  masters                = "B15003_023",
  professional           = "B15003_024",
  doctorate              = "B15003_025"
)

# ── Territory exclusion pattern (used everywhere) ──────────────────────────────
.TERRITORY_PAT <- "Puerto Rico|\\bPR\\b|Guam|Virgin Islands|Samoa|Mariana"

# ── Year detection ─────────────────────────────────────────────────────────────
.best_acs_year <- function(preferred = 2024) {
  for (yr in c(preferred, preferred - 1, preferred - 2, preferred - 3)) {
    ok <- tryCatch({
      tidycensus::get_acs(geography="us", variables="B01003_001",
                          year=yr, survey="acs5", geometry=FALSE)
      TRUE
    }, error=function(e) FALSE, warning=function(w) FALSE)
    if (ok) { message(sprintf("[ACS] Using year %d", yr)); return(yr) }
  }
  2022L
}

# ── Derived metrics from raw ACS columns ──────────────────────────────────────
.derive_metrics <- function(df) {
  df %>% dplyr::mutate(
    unemp_rate      = round(unemployedE / labor_forceE * 100,                      2),
    lfp_rate        = round(labor_forceE / total_popE * 100,                       2),
    poverty_rate    = round(poverty_countE / poverty_universeE * 100,              2),
    vacancy_rate    = round(vacant_unitsE / housing_unitsE * 100,                  2),
    rent_burden_rate= round((rb_30_34E + rb_35_39E + rb_40_49E + rb_50_plusE) /
                             (rent_burden_universeE - rb_not_computedE) * 100,     2),
    bachelors_plus_rate = round((bachelorsE + mastersE + professionalE + doctorateE) /
                                 education_total_25plusE * 100,                    2),
    med_income      = median_hh_incomeE,
    per_cap_income  = per_capita_incomeE,
    med_home_val    = median_home_valueE,
    med_rent        = median_rentE,
    med_age         = median_ageE,
    pop             = total_popE
  )
}

# ── Metro ACS fetch ────────────────────────────────────────────────────────────
fetch_metro_acs <- function(year = NULL, survey = "acs5") {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(key) == 0) return(NULL)

  tryCatch({
    tidycensus::census_api_key(key, install=FALSE, overwrite=TRUE)
    if (is.null(year)) year <- .best_acs_year(2024)

    message(sprintf("[Census] Metro ACS %s year %d...", survey, year))

    raw <- tidycensus::get_acs(
      geography = "metropolitan statistical area/micropolitan statistical area",
      variables = ACS_VARS, year=year, survey=survey, geometry=FALSE, output="wide"
    )

    # Compute US national averages for vs_us comparisons
    us_raw <- tidycensus::get_acs(
      geography="us", variables=ACS_VARS, year=year, survey=survey,
      geometry=FALSE, output="wide"
    ) %>% .derive_metrics()

    us_avg <- list(
      unemp_rate          = us_raw$unemp_rate[1],
      lfp_rate            = us_raw$lfp_rate[1],
      poverty_rate        = us_raw$poverty_rate[1],
      vacancy_rate        = us_raw$vacancy_rate[1],
      rent_burden_rate    = us_raw$rent_burden_rate[1],
      bachelors_plus_rate = us_raw$bachelors_plus_rate[1],
      med_income          = us_raw$med_income[1],
      med_home_val        = us_raw$med_home_val[1],
      med_rent            = us_raw$med_rent[1]
    )

    result <- raw %>%
      # Filter territories BEFORE rename — NAME still available here
      dplyr::filter(!grepl(.TERRITORY_PAT, NAME, ignore.case=TRUE)) %>%
      .derive_metrics() %>%
      dplyr::transmute(
        GEOID, metro = NAME,
        cbsa_type = ifelse(grepl("Micro Area$", NAME), "micro area", "metro"),
        pop, med_age,
        unemp_rate, lfp_rate, poverty_rate, vacancy_rate,
        rent_burden_rate, bachelors_plus_rate,
        med_income, per_cap_income, med_home_val, med_rent,
        # vs_us deltas
        unemp_vs_us          = round(unemp_rate - (us_avg$unemp_rate %||% NA), 2),
        lfp_vs_us            = round(lfp_rate - (us_avg$lfp_rate %||% NA), 2),
        poverty_vs_us        = round(poverty_rate - (us_avg$poverty_rate %||% NA), 2),
        income_vs_us         = round(med_income - (us_avg$med_income %||% NA), 0),
        edu_vs_us            = round(bachelors_plus_rate - (us_avg$bachelors_plus_rate %||% NA), 2)
      ) %>%
      # Metro and micro areas both stay; the 250k+ cut is applied where a table needs it.
      dplyr::filter(!is.na(unemp_rate),
                    # Belt-and-suspenders: now `metro` column exists
                    !grepl(.TERRITORY_PAT, metro, ignore.case=TRUE)) %>%
      # Shorten long CBSA names
      dplyr::mutate(metro = stringr::str_remove(metro, ",.*$")) %>%
      dplyr::arrange(dplyr::desc(pop))

    attr(result, "us_avg") <- us_avg
    result

  }, error=function(e) {
    message(sprintf("[Census] Metro fetch failed: %s", conditionMessage(e)))
    NULL
  })
}

# ── Build structured metro summary for LLM ─────────────────────────────────────
build_metro_summary <- function(metro_row, us_avg, kpis = NULL, risk = NULL, peers = NULL) {
  r <- metro_row

  # Where the metro sits against the US average. This is a level comparison from one
  # ACS vintage, NOT a change over time, so it is labelled that way for the model.
  get_trend <- function(val, ref, higher_good = TRUE) {
    if (length(val) != 1 || length(ref) != 1 || is.na(val) || is.na(ref)) return("no comparison")
    diff <- val - ref
    if (abs(diff) < 0.005 * abs(ref)) return("in line with US")
    if ((diff > 0) == higher_good) "better than US" else "worse than US"
  }
  cbsa_type <- if ("cbsa_type" %in% names(r) && !is.na(r$cbsa_type[1])) as.character(r$cbsa_type[1]) else NA_character_

  # Compute strengths and risks
  strengths <- character(0)
  risks     <- character(0)

  u <- as.numeric(r$unemp_rate)
  lfp <- as.numeric(r$lfp_rate)
  pov <- as.numeric(r$poverty_rate)
  inc <- as.numeric(r$med_income)
  rb  <- as.numeric(r$rent_burden_rate)
  edu <- as.numeric(r$bachelors_plus_rate)
  vac <- as.numeric(r$vacancy_rate)
  rent <- as.numeric(r$med_rent)

  if (!is.na(u)   && u   < (us_avg$unemp_rate %||% 4)) strengths <- c(strengths, "Below-average unemployment")
  if (!is.na(inc) && inc > (us_avg$med_income %||% 70000)) strengths <- c(strengths, "Above-average household income")
  if (!is.na(edu) && edu > (us_avg$bachelors_plus_rate %||% 33)) strengths <- c(strengths, "High educational attainment")
  if (!is.na(lfp) && lfp > (us_avg$lfp_rate %||% 63)) strengths <- c(strengths, "Strong labor force participation")
  if (!is.na(pov) && pov < (us_avg$poverty_rate %||% 12)) strengths <- c(strengths, "Low poverty rate")

  if (!is.na(rb)  && rb  > (us_avg$rent_burden_rate %||% 50) + 3)
    risks <- c(risks, "Housing affordability pressure (rent burden >3 pp above US)")
  if (!is.na(u)   && u   > (us_avg$unemp_rate %||% 4) + 1) risks <- c(risks, "Elevated unemployment")
  if (!is.na(lfp) && lfp < (us_avg$lfp_rate %||% 63) - 2) risks <- c(risks, "Weak labor force participation — hidden slack")
  if (!is.na(pov) && pov > (us_avg$poverty_rate %||% 12) + 3) risks <- c(risks, "High poverty concentration")
  if (!is.na(vac) && vac > 10) risks <- c(risks, "Elevated housing vacancy — demand softening")
  if (!is.na(inc) && inc < (us_avg$med_income %||% 70000) - 10000) risks <- c(risks, "Below-average household income")

  # National macro backdrop (same for every metro); unavailable readings are dropped, not sent as NA.
  mkt <- if (!is.null(kpis)) {
    ok <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
    drop_empty(list(
      note           = "National readings, identical for every area.",
      oil_price      = if (ok(kpis$oil_price)) kpis$oil_price,
      fed_funds_rate = if (ok(kpis$fed_funds)) kpis$fed_funds,
      mortgage_rate  = if (ok(kpis$mortgage30)) kpis$mortgage30,
      cpi_yoy        = if (ok(kpis$cpi_yoy)) kpis$cpi_yoy,
      volatility     = if (ok(kpis$vix)) {
        if (kpis$vix > 25) "elevated" else if (kpis$vix > 18) "moderate" else "low"
      }))
  } else list()

  out <- list(
    geography = drop_empty(list(
      name = as.character(r$metro),
      type = if (is.na(cbsa_type)) "metro or micro area (CBSA)" else cbsa_type,
      note = "ACS 5-year estimates; vs_us is metro minus US, relative_to_us is a level comparison, not a time trend.")),
    macro = list(
      population = list(value=as.integer(r$pop)),
      median_age = list(value=as.numeric(r$med_age))
    ),
    labor = list(
      unemployment_rate = list(
        value  = round(u / 100, 3),
        vs_us  = round((as.numeric(r$unemp_vs_us) %||% NA) / 100, 3),
        relative_to_us = get_trend(u, us_avg$unemp_rate, higher_good=FALSE)
      ),
      labor_force_participation = list(
        value  = round(lfp / 100, 3),
        vs_us  = round((as.numeric(r$lfp_vs_us) %||% NA) / 100, 3),
        relative_to_us = get_trend(lfp, us_avg$lfp_rate, higher_good=TRUE)
      )
    ),
    income = list(
      median_hh_income = list(
        value  = as.integer(inc),
        vs_us  = as.integer(as.numeric(r$income_vs_us) %||% NA),
        relative_to_us = get_trend(inc, us_avg$med_income)
      ),
      per_capita_income = list(value=as.integer(r$per_cap_income))
    ),
    housing = list(
      median_home_value  = list(value=as.integer(r$med_home_val)),
      median_rent        = list(value=as.integer(rent)),
      vacancy_rate       = list(value=round(vac/100, 3),
                                 relative_to_us=get_trend(vac, us_avg$vacancy_rate %||% 7, FALSE)),
      rent_burden_rate   = list(value=round(rb/100, 3),
                                 relative_to_us=get_trend(rb, us_avg$rent_burden_rate %||% 50, FALSE))
    ),
    poverty = list(
      poverty_rate = list(
        value = round(pov/100, 3),
        vs_us = round((as.numeric(r$poverty_vs_us)%||%NA)/100, 3),
        relative_to_us = get_trend(pov, us_avg$poverty_rate%||%12, FALSE)
      )
    ),
    human_capital = list(
      bachelors_plus_rate = list(
        value = round(edu/100, 3),
        vs_us = round((as.numeric(r$edu_vs_us)%||%NA)/100, 3),
        relative_to_us = get_trend(edu, us_avg$bachelors_plus_rate%||%33)
      )
    ),
    market = mkt,
    signals = list(strengths=strengths, risks=risks)
  )
  # Regional Risk map layers rolled up to this metro (absent when the map data is not loaded)
  if (!is.null(risk)) out$regional_risk <- risk
  # Rank among the same kind of area (micro areas against micro areas, metros against metros)
  if (!is.null(peers)) out$peer_comparison <- peers
  prune_na(out)
}

# Drop NA readings (and lists emptied by that) so the model never sees "NA".
prune_na <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, prune_na)
  gone <- vapply(x, function(e) (is.atomic(e) && length(e) == 1 && is.na(e)) ||
                   (is.list(e) && length(e) == 0), TRUE)
  x[!gone]
}

drop_empty <- function(x) x[!vapply(x, function(e) is.null(e) || length(e) == 0, TRUE)]

#' One metro's ACS readings against its peer group in `metro_data`: share of same-type
#' areas with a LOWER value (so 90 = higher than 90% of peers, not necessarily better),
#' and the peer median. NULL when the CBSA is not in the data. Peers are the same
#' `cbsa_type` when the data carries it, otherwise all areas.
metro_peer_context <- function(metro_data, geoid) {
  if (is.null(metro_data) || !"GEOID" %in% names(metro_data)) return(NULL)
  row <- metro_data[metro_data$GEOID == geoid, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row <- row[1, , drop = FALSE]
  has_type <- "cbsa_type" %in% names(metro_data) && !is.na(row$cbsa_type)
  peers <- if (has_type) metro_data[metro_data$cbsa_type %in% row$cbsa_type, , drop = FALSE] else metro_data
  metrics <- c(population = "pop", unemployment_rate_pct = "unemp_rate", poverty_rate_pct = "poverty_rate",
               median_household_income = "med_income", median_home_value = "med_home_val",
               rent_burden_pct = "rent_burden_rate", bachelors_plus_pct = "bachelors_plus_rate",
               labor_force_participation_pct = "lfp_rate")
  per <- lapply(metrics, function(cn) {
    v <- suppressWarnings(as.numeric(row[[cn]])); pv <- suppressWarnings(as.numeric(peers[[cn]]))
    if (!cn %in% names(peers) || length(v) != 1 || is.na(v) || sum(!is.na(pv)) < 5) return(NULL)
    list(value = round(v, 1), peer_median = round(stats::median(pv, na.rm = TRUE), 1),
         percentile_among_peers = round(100 * mean(pv < v, na.rm = TRUE)))
  })
  per <- per[!vapply(per, is.null, TRUE)]
  if (!length(per)) return(NULL)
  list(peer_group = if (has_type) paste0(row$cbsa_type, "s") else "all metro and micro areas",
       peer_count = nrow(peers),
       note = "percentile_among_peers = share of peer areas with a lower value (100 = highest, not necessarily best).",
       metrics = per)
}

#' The ACS row for a CBSA, or an all-NA row carrying the map's name and type when ACS has
#' none (so the analysis can still run on the Regional Risk layers alone).
metro_acs_row <- function(metro_data, geoid, name = NA_character_, type = NA_character_) {
  if (!is.null(metro_data) && "GEOID" %in% names(metro_data)) {
    hit <- metro_data[metro_data$GEOID == geoid, , drop = FALSE]
    if (nrow(hit) > 0) return(hit[1, , drop = FALSE])
  }
  num <- c("pop", "med_age", "unemp_rate", "lfp_rate", "poverty_rate", "vacancy_rate",
           "rent_burden_rate", "bachelors_plus_rate", "med_income", "per_cap_income",
           "med_home_val", "med_rent", "unemp_vs_us", "lfp_vs_us", "poverty_vs_us",
           "income_vs_us", "edu_vs_us")
  out <- data.frame(GEOID = geoid, metro = name, cbsa_type = type, stringsAsFactors = FALSE)
  for (n in num) out[[n]] <- NA_real_
  out
}

# ── Metro AI prompt builder ────────────────────────────────────────────────────
build_metro_prompt <- function(summary_obj, news_headlines = "N/A") {
  paste0(
    "You are a senior macroeconomic analyst specializing in regional US economies.
",
    "Analyze the structured economic data below for one metropolitan or micropolitan area.
",
    "Focus on key drivers, risks, and forward-looking implications relative to US national averages.

",
    "How to read the data:
",
    "- geography.type says whether this is a metro or a micro area. For a micro area, expect a small, ",
    "concentrated local economy: one or two employers or industries can dominate, the ACS estimates ",
    "are noisier, and the figures are mostly county-level. Say so where it matters; do not treat it as a big-city economy.
",
    "- peer_comparison ranks the area against others of the SAME type; use it to say whether it is ",
    "strong or weak for what it is, not only against the US.
",
    "- relative_to_us is a level comparison with the US average from one ACS vintage, not a trend over time. ",
    "Real movement is in regional_risk: unemployment change vs a year ago, and stress_ahead, the projected ",
    "job-growth gap vs the US, which is the basis for the outlook.
",
    "- regional_risk sections carry as_of dates. Cite them when they matter, and mention anything listed in data_gaps ",
    "instead of guessing at missing layers.
",
    "- If regional_risk.selected_county is present, the user picked that county on the map: ",
    "cover it explicitly (how it differs from the rest of the area and from the US).
",
    "- Use only figures present in the data. Never invent a number.

",
    "Economic Data (JSON):
",
    jsonlite::toJSON(summary_obj, auto_unbox=TRUE, pretty=TRUE),
    "

Recent National Economic News:
", news_headlines,
    "

Provide output in EXACTLY these sections (concise, specific, no filler):",
    "
## Executive Summary
2-3 sentences. Lead with the most distinctive economic characteristic.",
    "
## Key Strengths
Bullet list. Only genuine relative advantages (vs US and vs peers).",
    "
## Key Risks
Bullet list. Only genuine concerns with evidence from the data.",
    "
## Economic Outlook (3-6 months)
Build on stress ahead, unemployment momentum, credit and assistance signals; ",
    "link national conditions (rates, inflation, news) to local impact.",
    "
## Watch Items
Top 2-3 metrics to monitor with specific threshold values.",
    "
## Data Notes
One or two lines: anything missing, stale, or less reliable (e.g. micro-area sampling noise)."
  )
}

# ── Choropleth map config ──────────────────────────────────────────────────────
VAR_CFG <- list(
  unemp_rate          = list(label="Unemployment Rate",    fmt="%",  pal="RdYlGn", rev=TRUE),
  poverty_rate        = list(label="Poverty Rate",         fmt="%",  pal="RdYlGn", rev=TRUE),
  lfp_rate            = list(label="Labor Force Part.",    fmt="%",  pal="RdYlGn", rev=FALSE),
  med_income          = list(label="Median HH Income",     fmt="$",  pal="YlGn",   rev=FALSE),
  med_home_val        = list(label="Median Home Value",    fmt="$",  pal="YlGn",   rev=FALSE),
  vacancy_rate        = list(label="Housing Vacancy Rate", fmt="%",  pal="RdYlGn", rev=TRUE),
  rent_burden_rate    = list(label="Rent Burden Rate",     fmt="%",  pal="RdYlGn", rev=TRUE),
  bachelors_plus_rate = list(label="Bachelor's+ Rate",     fmt="%",  pal="YlGn",   rev=FALSE),
  med_rent            = list(label="Median Rent",          fmt="$",  pal="YlOrRd", rev=TRUE)
)
