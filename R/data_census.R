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
  # Rent burden
  rent_burden_30_34      = "B25070_007",
  rent_burden_35_plus    = "B25070_008",
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
    rent_burden_rate= round((rent_burden_30_34E + rent_burden_35_plusE) /
                             occupied_unitsE * 100,                                2),
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

# ── State ACS fetch ────────────────────────────────────────────────────────────
fetch_state_acs <- function(year = NULL, survey = "acs5") {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(key) == 0) { message("[Census] CENSUS_API_KEY not set"); return(NULL) }

  tryCatch({
    tidycensus::census_api_key(key, install=FALSE, overwrite=TRUE)
    if (is.null(year)) year <- .best_acs_year(2024)

    message(sprintf("[Census] State ACS %s year %d (no geometry)...", survey, year))

    acs_raw <- tidycensus::get_acs(
      geography = "state", variables = ACS_VARS,
      year = year, survey = survey, geometry = FALSE, output = "wide"
    )

    # 50 states + DC FIPS codes
    VALID_FIPS <- c(
      "01","02","04","05","06","08","09","10","11","12","13","15","16","17",
      "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
      "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
      "47","48","49","50","51","53","54","55","56"
    )

    acs_df <- acs_raw %>%
      dplyr::filter(GEOID %in% VALID_FIPS) %>%
      .derive_metrics() %>%
      dplyr::transmute(
        GEOID, state = NAME, pop, med_age,
        unemp_rate, lfp_rate, poverty_rate, vacancy_rate,
        rent_burden_rate, bachelors_plus_rate,
        med_income, per_cap_income, med_home_val, med_rent
      ) %>%
      dplyr::filter(!is.na(unemp_rate))

    message("[Census] Fetching state shapefile via tigris...")
    states_sf <- tigris::states(cb=TRUE, resolution="20m", year=min(year, 2023)) %>%
      sf::st_transform(4326) %>%
      dplyr::filter(GEOID %in% VALID_FIPS) %>%
      dplyr::select(GEOID, geometry)

    result <- states_sf %>%
      dplyr::left_join(acs_df, by="GEOID") %>%
      dplyr::filter(!is.na(state))

    message(sprintf("[Census] State sf ready: %d features", nrow(result)))
    result

  }, error=function(e) {
    message(sprintf("[Census] State fetch failed: %s", conditionMessage(e)))
    NULL
  })
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
        GEOID, metro = NAME, pop, med_age,
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
      dplyr::filter(!is.na(unemp_rate), pop > 250000,
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
build_metro_summary <- function(metro_row, us_avg, kpis = NULL) {
  r <- metro_row

  get_trend <- function(val, ref, higher_good = TRUE) {
    if (is.na(val) || is.na(ref)) return("stable")
    diff <- val - ref
    if (abs(diff) < 0.005 * abs(ref)) return("stable")
    if (diff > 0) return(if (higher_good) "increasing" else "declining")
    return(if (higher_good) "declining" else "improving")
  }

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

  if (!is.na(rb)  && rb  > 30) risks <- c(risks, "Housing affordability pressure (rent burden >30%)")
  if (!is.na(u)   && u   > (us_avg$unemp_rate %||% 4) + 1) risks <- c(risks, "Elevated unemployment")
  if (!is.na(lfp) && lfp < (us_avg$lfp_rate %||% 63) - 2) risks <- c(risks, "Weak labor force participation — hidden slack")
  if (!is.na(pov) && pov > (us_avg$poverty_rate %||% 12) + 3) risks <- c(risks, "High poverty concentration")
  if (!is.na(vac) && vac > 10) risks <- c(risks, "Elevated housing vacancy — demand softening")
  if (!is.na(inc) && inc < (us_avg$med_income %||% 70000) - 10000) risks <- c(risks, "Below-average household income")

  # Market context from macro
  mkt <- if (!is.null(kpis)) list(
    sp500_trend    = if (!is.null(kpis$vix) && !is.na(kpis$vix) && kpis$vix < 20) "stable-positive" else "volatile",
    oil_price      = kpis$oil_price %||% NA,
    oil_trend      = "see national data",
    fed_funds_rate = kpis$fed_funds %||% NA,
    mortgage_rate  = kpis$mortgage30 %||% NA,
    cpi_yoy        = kpis$cpi_yoy %||% NA,
    volatility     = if (!is.null(kpis$vix) && !is.na(kpis$vix)) {
      if (kpis$vix > 25) "elevated" else if (kpis$vix > 18) "moderate" else "low"
    } else "unknown"
  ) else list()

  list(
    geography = list(name=as.character(r$metro), type="MSA/CBSA"),
    macro = list(
      population = list(value=as.integer(r$pop)),
      median_age = list(value=as.numeric(r$med_age))
    ),
    labor = list(
      unemployment_rate = list(
        value  = round(u / 100, 3),
        vs_us  = round((as.numeric(r$unemp_vs_us) %||% NA) / 100, 3),
        trend  = get_trend(u, us_avg$unemp_rate, higher_good=FALSE)
      ),
      labor_force_participation = list(
        value  = round(lfp / 100, 3),
        vs_us  = round((as.numeric(r$lfp_vs_us) %||% NA) / 100, 3),
        trend  = get_trend(lfp, us_avg$lfp_rate, higher_good=TRUE)
      )
    ),
    income = list(
      median_hh_income = list(
        value  = as.integer(inc),
        vs_us  = as.integer(as.numeric(r$income_vs_us) %||% NA),
        trend  = get_trend(inc, us_avg$med_income)
      ),
      per_capita_income = list(value=as.integer(r$per_cap_income))
    ),
    housing = list(
      median_home_value  = list(value=as.integer(r$med_home_val)),
      median_rent        = list(value=as.integer(rent)),
      vacancy_rate       = list(value=round(vac/100, 3),
                                 trend=get_trend(vac, us_avg$vacancy_rate %||% 7, FALSE)),
      rent_burden_rate   = list(value=round(rb/100, 3),
                                 trend=get_trend(rb, 30, FALSE))
    ),
    poverty = list(
      poverty_rate = list(
        value = round(pov/100, 3),
        vs_us = round((as.numeric(r$poverty_vs_us)%||%NA)/100, 3),
        trend = get_trend(pov, us_avg$poverty_rate%||%12, FALSE)
      )
    ),
    human_capital = list(
      bachelors_plus_rate = list(
        value = round(edu/100, 3),
        vs_us = round((as.numeric(r$edu_vs_us)%||%NA)/100, 3),
        trend = get_trend(edu, us_avg$bachelors_plus_rate%||%33)
      )
    ),
    market = mkt,
    signals = list(strengths=strengths, risks=risks)
  )
}

# ── Metro AI prompt builder ────────────────────────────────────────────────────
build_metro_prompt <- function(summary_obj, news_headlines = "N/A") {
  paste0(
    "You are a senior macroeconomic analyst specializing in regional US economies.\n",
    "Analyze the structured economic data below for a specific metro area.\n",
    "Focus on key drivers, risks, and forward-looking implications relative to US national averages.\n\n",
    "Economic Data (JSON):\n",
    jsonlite::toJSON(summary_obj, auto_unbox=TRUE, pretty=TRUE),
    "\n\nRecent National Economic News:\n", news_headlines,
    "\n\nProvide output in EXACTLY these sections (concise, specific, no filler):",
    "\n## Executive Summary\n2-3 sentences. Lead with the most distinctive economic characteristic.",
    "\n## Key Strengths\nBullet list. Only genuine relative advantages vs US average.",
    "\n## Key Risks\nBullet list. Only genuine concerns with evidence from the data.",
    "\n## Economic Outlook (3-6 months)\nLink macro conditions (rates, inflation, national trends) to local impact.",
    "\n## Watch Items\nTop 2-3 metrics to monitor with specific threshold values."
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

build_state_map <- function(state_sf, variable="unemp_rate") {
  if (is.null(state_sf) || nrow(state_sf)==0) { message("[Map] state_sf empty"); return(NULL) }
  if (!variable %in% names(state_sf)) {
    message(sprintf("[Map] variable '%s' not found", variable)); return(NULL)
  }

  cfg  <- VAR_CFG[[variable]] %||% VAR_CFG[["unemp_rate"]]
  vals <- as.numeric(state_sf[[variable]])

  message(sprintf("[Map] %s — %d states, range [%.1f, %.1f]",
                  variable, sum(!is.na(vals)), min(vals,na.rm=TRUE), max(vals,na.rm=TRUE)))

  colour_pal <- leaflet::colorBin(palette=cfg$pal, domain=vals, bins=7,
                                   reverse=cfg$rev, na.color="#555566")

  fmt_val <- function(v) {
    if (is.na(v)) return("N/A")
    if (cfg$fmt=="$") paste0("$", format(round(v), big.mark=","))
    else paste0(round(v,1),"%")
  }

  popup_vec <- vapply(seq_len(nrow(state_sf)), function(i) {
    r <- state_sf[i,]
    paste0(
      "<div style='font-family:Inter,sans-serif;background:#1e2640;color:#e0e0e0;",
      "padding:14px 16px;border-radius:8px;min-width:220px;line-height:1.9;'>",
      "<b style='color:#00b4d8;font-size:15px;'>", htmltools::htmlEscape(as.character(r$state)), "</b><br/>",
      "<table style='width:100%;font-size:12px;'>",
      "<tr><td style='color:#9aa3b2;'>", cfg$label, "</td>",
          "<td style='font-weight:600;'>", fmt_val(as.numeric(r[[variable]])), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Unemp. Rate</td><td>",   fmt_val(as.numeric(r$unemp_rate)),   "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>LFP Rate</td><td>",      fmt_val(as.numeric(r$lfp_rate)),      "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Poverty Rate</td><td>",  fmt_val(as.numeric(r$poverty_rate)),  "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Rent Burden</td><td>",   fmt_val(as.numeric(r$rent_burden_rate)), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Median Income</td><td>",
          if(!is.na(r$med_income)) paste0("$",format(round(as.numeric(r$med_income)),big.mark=",")) else "N/A",
          "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Bach.+ Rate</td><td>",   fmt_val(as.numeric(r$bachelors_plus_rate)), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Population</td><td>",    format(as.integer(r$pop),big.mark=","), "</td></tr>",
      "</table></div>"
    )
  }, character(1))

  label_vec <- paste0(state_sf$state, ": ", vapply(vals, fmt_val, character(1)))

  lbl_fmt <- if (cfg$fmt=="$") leaflet::labelFormat(prefix="$", big.mark=",", between=" \u2013 $")
             else leaflet::labelFormat(suffix="%", between=" \u2013 ")

  leaflet::leaflet(state_sf, options=leaflet::leafletOptions(zoomControl=TRUE)) %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter",
                              options=leaflet::providerTileOptions(opacity=0.95)) %>%
    leaflet::addPolygons(
      fillColor=colour_pal(vals), fillOpacity=0.82,
      color="#0f1117", weight=0.8, smoothFactor=1,
      popup=popup_vec, label=label_vec,
      labelOptions=leaflet::labelOptions(
        style=list("background"="#1e2640","color"="#e0e0e0","border"="none","padding"="6px 10px"),
        direction="auto"),
      highlightOptions=leaflet::highlightOptions(
        color="#00b4d8", weight=2.5, fillOpacity=0.95, bringToFront=TRUE)
    ) %>%
    leaflet::addLegend(pal=colour_pal, values=vals, opacity=0.9,
                       title=cfg$label, position="bottomright", labFormat=lbl_fmt) %>%
    leaflet::setView(lng=-96, lat=38, zoom=4)
}

# ── Metro bubble map ───────────────────────────────────────────────────────────
METRO_VAR_CFG <- VAR_CFG[c("unemp_rate","poverty_rate","lfp_rate",
                             "med_income","med_home_val","rent_burden_rate",
                             "bachelors_plus_rate")]

fetch_metro_sf <- function(metro_df, year=2023) {
  if (is.null(metro_df) || nrow(metro_df)==0) return(NULL)
  tryCatch({
    message("[Metro map] Fetching CBSA shapefile...")
    cbsa_sf <- tigris::core_based_statistical_areas(cb=TRUE, resolution="20m", year=year) %>%
      sf::st_transform(4326) %>%
      dplyr::filter(!grepl(.TERRITORY_PAT, NAME, ignore.case=TRUE))

    centroids <- cbsa_sf %>% sf::st_centroid() %>%
      dplyr::mutate(lon=sf::st_coordinates(geometry)[,1],
                    lat=sf::st_coordinates(geometry)[,2]) %>%
      sf::st_drop_geometry() %>% dplyr::select(GEOID, lon, lat)

    metro_df %>% dplyr::left_join(centroids, by="GEOID") %>%
      dplyr::filter(!is.na(lon), !is.na(lat))

  }, error=function(e) { message("[Metro SF] ", e$message); NULL })
}

build_metro_bubble_map <- function(metro_sf_df, variable="unemp_rate", min_pop=500000) {
  if (is.null(metro_sf_df) || nrow(metro_sf_df)==0) return(NULL)

  cfg <- METRO_VAR_CFG[[variable]] %||% METRO_VAR_CFG[["unemp_rate"]]

  df <- metro_sf_df %>%
    dplyr::filter(pop >= min_pop, !is.na(.data[[variable]]),
                  !is.na(lon), !is.na(lat),
                  !grepl(.TERRITORY_PAT, metro, ignore.case=TRUE))

  if (nrow(df)==0) return(NULL)
  vals <- as.numeric(df[[variable]])

  colour_pal <- leaflet::colorBin(palette=cfg$pal, domain=vals, bins=7,
                                   reverse=cfg$rev, na.color="#555566")
  radius <- sqrt(df$pop / max(df$pop, na.rm=TRUE)) * 42 + 5

  fmt_val <- function(v) {
    if (is.na(v)) return("N/A")
    if (cfg$fmt=="$") paste0("$",format(round(v),big.mark=",")) else paste0(round(v,1),"%")
  }

  popup_vec <- vapply(seq_len(nrow(df)), function(i) {
    r <- df[i,]
    paste0(
      "<div style='font-family:Inter,sans-serif;background:#1e2640;color:#e0e0e0;",
      "padding:14px 16px;border-radius:8px;min-width:230px;line-height:1.9;'>",
      "<b style='color:#00b4d8;font-size:14px;'>", htmltools::htmlEscape(as.character(r$metro)), "</b><br/>",
      "<table style='width:100%;font-size:12px;'>",
      "<tr><td style='color:#9aa3b2;'>", cfg$label, "</td><td style='font-weight:600;'>",
          fmt_val(as.numeric(r[[variable]])), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Population</td><td>",   format(as.integer(r$pop),big.mark=","), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Unemp. Rate</td><td>",  fmt_val(as.numeric(r$unemp_rate)),   "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>LFP Rate</td><td>",     fmt_val(as.numeric(r$lfp_rate)),     "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Poverty Rate</td><td>", fmt_val(as.numeric(r$poverty_rate)), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Median Income</td><td>",
          if(!is.na(r$med_income)) paste0("$",format(round(as.numeric(r$med_income)),big.mark=",")) else "N/A",
          "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Median Rent</td><td>",
          if(!is.na(r$med_rent)) paste0("$",format(round(as.numeric(r$med_rent)),big.mark=",")) else "N/A",
          "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Rent Burden</td><td>",  fmt_val(as.numeric(r$rent_burden_rate)), "</td></tr>",
      "<tr><td style='color:#9aa3b2;'>Bach.+ Rate</td><td>",  fmt_val(as.numeric(r$bachelors_plus_rate)), "</td></tr>",
      "</table></div>"
    )
  }, character(1))

  lbl_fmt <- if (cfg$fmt=="$") leaflet::labelFormat(prefix="$",big.mark=",",between=" \u2013 $")
             else leaflet::labelFormat(suffix="%", between=" \u2013 ")

  leaflet::leaflet(options=leaflet::leafletOptions(zoomControl=TRUE)) %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter",
                              options=leaflet::providerTileOptions(opacity=0.95)) %>%
    leaflet::addCircleMarkers(
      lng=df$lon, lat=df$lat, radius=radius,
      fillColor=colour_pal(vals), fillOpacity=0.82,
      color="#0f1117", weight=1, popup=popup_vec,
      label=paste0(df$metro, ": ", vapply(vals, fmt_val, character(1))),
      labelOptions=leaflet::labelOptions(
        style=list("background"="#1e2640","color"="#e0e0e0","border"="none","padding"="6px 10px"),
        direction="auto")
    ) %>%
    leaflet::addLegend(pal=colour_pal, values=vals, opacity=0.9,
                       title=cfg$label, position="bottomright", labFormat=lbl_fmt) %>%
    leaflet::setView(lng=-96, lat=38, zoom=4)
}
