# ─────────────────────────────────────────────────────────────────────────────
# R/data_census.R — Census/ACS + tigris choropleth + metro analysis
# ─────────────────────────────────────────────────────────────────────────────

options(tigris_use_cache = TRUE) # cache shapefiles between sessions

ACS_VARS <- c(
  total_pop = "B01003_001",
  labor_force = "B23025_002",
  employed = "B23025_004",
  unemployed = "B23025_005",
  median_hh_income = "B19013_001",
  poverty_universe = "B17001_001",
  poverty_count = "B17001_002",
  housing_units = "B25001_001",
  vacant_units = "B25002_003",
  median_home_value = "B25077_001"
)

# ── Year detection ─────────────────────────────────────────────────────────────
.best_acs_year <- function(preferred = 2023) {
  for (yr in c(preferred, preferred - 1, preferred - 2, preferred - 3)) {
    ok <- tryCatch(
      {
        tidycensus::get_acs(
          geography = "us",
          variables = "B01003_001",
          year = yr,
          survey = "acs5",
          geometry = FALSE
        )
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (ok) {
      message(sprintf("[ACS] Using year %d", yr))
      return(yr)
    }
  }
  2022L # hard fallback
}

# ── State-level ACS (NO geometry from tidycensus — use tigris instead) ─────────
fetch_state_acs <- function(year = NULL, survey = "acs5") {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(key) == 0) {
    message("[Census] CENSUS_API_KEY not set — map disabled")
    return(NULL)
  }

  tryCatch(
    {
      tidycensus::census_api_key(key, install = FALSE, overwrite = TRUE)
      if (is.null(year)) {
        year <- .best_acs_year(2023)
      }

      message(sprintf(
        "[Census] Fetching ACS %s for %d (no geometry)...",
        survey,
        year
      ))

      # --- ACS tabular data (geometry=FALSE avoids sf/tigris conflicts) ----------
      acs_raw <- tidycensus::get_acs(
        geography = "state",
        variables = ACS_VARS,
        year = year,
        survey = survey,
        geometry = FALSE,
        output = "wide"
      )

      acs_df <- acs_raw %>%
        transmute(
          GEOID = GEOID,
          state = NAME,
          pop = total_popE,
          labor_force = labor_forceE,
          unemployed = unemployedE,
          unemp_rate = round((unemployedE / labor_forceE) * 100, 2),
          poverty_rate = round((poverty_countE / poverty_universeE) * 100, 2),
          vacancy_rate = round((vacant_unitsE / housing_unitsE) * 100, 2),
          lfp_rate = round((labor_forceE / total_popE) * 100, 2),
          med_income = median_hh_incomeE,
          med_home_val = median_home_valueE
        ) %>%
        filter(!is.na(unemp_rate), !is.na(pop), pop > 0)

      # --- Shapefile from tigris (much more reliable than tidycensus geometry) ---
      message("[Census] Fetching state shapefile from tigris...")
      states_sf <- tigris::states(
        cb = TRUE,
        resolution = "20m",
        year = year
      ) %>%
        sf::st_transform(4326) %>%
        # Drop territories / non-contiguous if desired (keep all by default)
        select(GEOID, geometry)

      # --- Join ACS data onto geometry ------------------------------------------
      result <- states_sf %>%
        dplyr::left_join(acs_df, by = "GEOID") %>%
        filter(!is.na(state), !is.na(unemp_rate))

      message(sprintf("[Census] State sf ready: %d features", nrow(result)))
      result
    },
    error = function(e) {
      message(sprintf("[Census] State fetch failed: %s", conditionMessage(e)))
      NULL
    }
  )
}

# ── Metro-level ACS (tabular only, no geometry) ────────────────────────────────
fetch_metro_acs <- function(year = NULL, survey = "acs5") {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (nchar(key) == 0) {
    return(NULL)
  }

  tryCatch(
    {
      tidycensus::census_api_key(key, install = FALSE, overwrite = TRUE)
      if (is.null(year)) {
        year <- .best_acs_year(2023)
      }

      message(sprintf("[Census] Fetching metro ACS %s for %d...", survey, year))

      raw <- tidycensus::get_acs(
        geography = "metropolitan statistical area/micropolitan statistical area",
        variables = ACS_VARS,
        year = year,
        survey = survey,
        geometry = FALSE,
        output = "wide"
      )

      raw %>%
        transmute(
          GEOID = GEOID,
          metro = NAME,
          pop = total_popE,
          unemp_rate = round((unemployedE / labor_forceE) * 100, 2),
          poverty_rate = round((poverty_countE / poverty_universeE) * 100, 2),
          vacancy_rate = round((vacant_unitsE / housing_unitsE) * 100, 2),
          lfp_rate = round((labor_forceE / total_popE) * 100, 2),
          med_income = median_hh_incomeE,
          med_home_val = median_home_valueE
        ) %>%
        filter(!is.na(unemp_rate), pop > 250000) %>%
        # Clean up long CBSA names (drop ", state-state" suffix)
        mutate(metro = stringr::str_remove(metro, ",.*$")) %>%
        arrange(desc(pop))
    },
    error = function(e) {
      message(sprintf("[Census] Metro fetch failed: %s", conditionMessage(e)))
      NULL
    }
  )
}

# ── Choropleth map builder ─────────────────────────────────────────────────────
VAR_CFG <- list(
  unemp_rate = list(
    label = "Unemployment Rate",
    fmt = "%",
    pal = "RdYlGn",
    rev = TRUE,
    desc = "Higher = worse (red)"
  ),
  poverty_rate = list(
    label = "Poverty Rate",
    fmt = "%",
    pal = "RdYlGn",
    rev = TRUE,
    desc = "Higher = worse (red)"
  ),
  lfp_rate = list(
    label = "Labor Force Part.",
    fmt = "%",
    pal = "RdYlGn",
    rev = FALSE,
    desc = "Higher = better (green)"
  ),
  med_income = list(
    label = "Median HH Income",
    fmt = "$",
    pal = "YlGn",
    rev = FALSE,
    desc = "Higher = better (green)"
  ),
  med_home_val = list(
    label = "Median Home Value",
    fmt = "$",
    pal = "YlGn",
    rev = FALSE,
    desc = "Higher values (green)"
  ),
  vacancy_rate = list(
    label = "Housing Vacancy",
    fmt = "%",
    pal = "RdYlGn",
    rev = TRUE,
    desc = "Higher = worse (red)"
  )
)

build_state_map <- function(state_sf, variable = "unemp_rate") {
  if (is.null(state_sf) || nrow(state_sf) == 0) {
    message("[Map] state_sf is NULL or empty")
    return(NULL)
  }
  if (!variable %in% names(state_sf)) {
    message(sprintf(
      "[Map] variable '%s' not in sf columns: %s",
      variable,
      paste(names(state_sf), collapse = ", ")
    ))
    return(NULL)
  }

  cfg <- VAR_CFG[[variable]] %||% VAR_CFG[["unemp_rate"]]
  vals <- as.numeric(state_sf[[variable]])

  message(sprintf(
    "[Map] Building choropleth: %s, %d states, range [%.1f, %.1f]",
    variable,
    sum(!is.na(vals)),
    min(vals, na.rm = TRUE),
    max(vals, na.rm = TRUE)
  ))

  # Colour palette mapped to numeric values (NOT to hex strings)
  colour_pal <- colorBin(
    palette = cfg$pal,
    domain = vals,
    bins = 7,
    reverse = cfg$rev,
    na.color = "#555566"
  )

  # Pre-build popups as plain character vector
  popup_vec <- vapply(
    seq_len(nrow(state_sf)),
    function(i) {
      r <- state_sf[i, ]
      v <- as.numeric(r[[variable]])
      val_str <- if (is.na(v)) {
        "N/A"
      } else if (cfg$fmt == "$") {
        paste0("$", format(round(v), big.mark = ","))
      } else {
        paste0(round(v, 1), "%")
      }

      inc_str <- if (!is.na(r$med_income)) {
        paste0("$", format(round(as.numeric(r$med_income)), big.mark = ","))
      } else {
        "N/A"
      }
      home_str <- if (!is.na(r$med_home_val)) {
        paste0("$", format(round(as.numeric(r$med_home_val)), big.mark = ","))
      } else {
        "N/A"
      }

      paste0(
        "<div style='font-family:Inter,sans-serif;background:#1e2640;color:#e0e0e0;",
        "padding:14px 16px;border-radius:8px;min-width:210px;line-height:1.9;'>",
        "<b style='color:#00b4d8;font-size:15px;'>",
        htmltools::htmlEscape(as.character(r$state)),
        "</b><br/>",
        "<table style='width:100%;font-size:12px;'>",
        "<tr><td style='color:#9aa3b2;'>",
        cfg$label,
        "</td>",
        "<td style='color:#e0e0e0;font-weight:600;'>",
        val_str,
        "</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Unemp. Rate</td>",
        "<td>",
        round(as.numeric(r$unemp_rate), 1),
        "%</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Poverty Rate</td>",
        "<td>",
        round(as.numeric(r$poverty_rate), 1),
        "%</td></tr>",
        "<tr><td style='color:#9aa3b2;'>LFP Rate</td>",
        "<td>",
        round(as.numeric(r$lfp_rate), 1),
        "%</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Med. Income</td>",
        "<td>",
        inc_str,
        "</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Med. Home Val.</td>",
        "<td>",
        home_str,
        "</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Population</td>",
        "<td>",
        format(as.integer(r$pop), big.mark = ","),
        "</td></tr>",
        "</table></div>"
      )
    },
    character(1)
  )

  # Legend label formatter
  lbl_fmt <- if (cfg$fmt == "$") {
    labelFormat(prefix = "$", big.mark = ",", between = " – $")
  } else {
    labelFormat(suffix = "%", between = " – ")
  }

  leaflet(state_sf, options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles(
      "CartoDB.DarkMatter",
      options = providerTileOptions(opacity = 0.95)
    ) %>%
    addPolygons(
      fillColor = colour_pal(vals), # direct vector: safe, explicit, correct
      fillOpacity = 0.82,
      color = "#0f1117",
      weight = 0.8,
      smoothFactor = 1,
      popup = popup_vec,
      label = ~ paste0(
        state,
        ": ",
        round(vals, 1),
        if (cfg$fmt == "%") "%" else ""
      ),
      labelOptions = labelOptions(
        style = list(
          "background" = "#1e2640",
          "color" = "#e0e0e0",
          "border" = "none",
          "padding" = "6px 10px"
        ),
        direction = "auto"
      ),
      highlightOptions = highlightOptions(
        color = "#00b4d8",
        weight = 2.5,
        fillOpacity = 0.95,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = colour_pal,
      values = vals, # pass the NUMERIC vector — NOT a formula/hex column
      opacity = 0.9,
      title = cfg$label,
      position = "bottomright",
      labFormat = lbl_fmt
    ) %>%
    setView(lng = -96, lat = 38, zoom = 4)
}
