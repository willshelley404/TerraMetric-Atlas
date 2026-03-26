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
        filter(
          !is.na(unemp_rate),
          !is.na(pop),
          pop > 0,
          GEOID %in%
            c(
              "01",
              "02",
              "04",
              "05",
              "06",
              "08",
              "09",
              "10",
              "11",
              "12",
              "13",
              "15",
              "16",
              "17",
              "18",
              "19",
              "20",
              "21",
              "22",
              "23",
              "24",
              "25",
              "26",
              "27",
              "28",
              "29",
              "30",
              "31",
              "32",
              "33",
              "34",
              "35",
              "36",
              "37",
              "38",
              "39",
              "40",
              "41",
              "42",
              "44",
              "45",
              "46",
              "47",
              "48",
              "49",
              "50",
              "51",
              "53",
              "54",
              "55",
              "56"
            )
        )

      # --- Shapefile from tigris — filter to 50 states + DC only ---------------
      # FIPS codes > 56 are territories (PR=72, GU=66, VI=78, AS=60, MP=69)
      VALID_STATE_FIPS <- c(
        "01",
        "02",
        "04",
        "05",
        "06",
        "08",
        "09",
        "10",
        "11",
        "12",
        "13",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "21",
        "22",
        "23",
        "24",
        "25",
        "26",
        "27",
        "28",
        "29",
        "30",
        "31",
        "32",
        "33",
        "34",
        "35",
        "36",
        "37",
        "38",
        "39",
        "40",
        "41",
        "42",
        "44",
        "45",
        "46",
        "47",
        "48",
        "49",
        "50",
        "51",
        "53",
        "54",
        "55",
        "56"
      ) # 11 = DC

      message("[Census] Fetching state shapefile from tigris...")
      states_sf <- tigris::states(
        cb = TRUE,
        resolution = "20m",
        year = year
      ) %>%
        sf::st_transform(4326) %>%
        filter(GEOID %in% VALID_STATE_FIPS) %>%
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
      ) |>
        filter(!str_detect(NAME, "San Juan"))

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
        filter(
          !is.na(unemp_rate),
          pop > 250000,
          # Exclude Puerto Rico (72***) and other territory CBSAs
          # US territory FIPS state prefixes: PR=72, GU=66, VI=78, AS=60, MP=69
          !grepl("^(72|66|78|60|69)", GEOID)
        ) %>%
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

  # Colour palette mapped to numeric values
  colour_pal <- leaflet::colorBin(
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

  # Label text (hover tooltip)
  label_vec <- paste0(
    state_sf$state,
    ": ",
    ifelse(
      is.na(vals),
      "N/A",
      ifelse(
        cfg$fmt == "$",
        paste0("$", format(round(vals), big.mark = ",")),
        paste0(round(vals, 1), "%")
      )
    )
  )

  # Legend label formatter
  lbl_fmt <- if (cfg$fmt == "$") {
    leaflet::labelFormat(prefix = "$", big.mark = ",", between = " \u2013 $")
  } else {
    leaflet::labelFormat(suffix = "%", between = " \u2013 ")
  }

  # Build map with fully-namespaced leaflet calls
  leaflet::leaflet(
    state_sf,
    options = leaflet::leafletOptions(zoomControl = TRUE)
  ) %>%
    leaflet::addProviderTiles(
      "CartoDB.DarkMatter",
      options = leaflet::providerTileOptions(opacity = 0.95)
    ) %>%
    leaflet::addPolygons(
      fillColor = colour_pal(vals),
      fillOpacity = 0.82,
      color = "#0f1117",
      weight = 0.8,
      smoothFactor = 1,
      popup = popup_vec,
      label = label_vec,
      labelOptions = leaflet::labelOptions(
        style = list(
          "background" = "#1e2640",
          "color" = "#e0e0e0",
          "border" = "none",
          "padding" = "6px 10px"
        ),
        direction = "auto"
      ),
      highlightOptions = leaflet::highlightOptions(
        color = "#00b4d8",
        weight = 2.5,
        fillOpacity = 0.95,
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      pal = colour_pal,
      values = vals,
      opacity = 0.9,
      title = cfg$label,
      position = "bottomright",
      labFormat = lbl_fmt
    ) %>%
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
}

# ── Metro bubble map builder ───────────────────────────────────────────────────
# Uses tigris core-based statistical areas for CBSA centroids
METRO_VAR_CFG <- list(
  unemp_rate = list(
    label = "Unemployment Rate",
    fmt = "%",
    pal = "RdYlGn",
    rev = TRUE
  ),
  poverty_rate = list(
    label = "Poverty Rate",
    fmt = "%",
    pal = "RdYlGn",
    rev = TRUE
  ),
  lfp_rate = list(
    label = "Labor Force Part.",
    fmt = "%",
    pal = "RdYlGn",
    rev = FALSE
  ),
  med_income = list(
    label = "Median HH Income",
    fmt = "$",
    pal = "YlGn",
    rev = FALSE
  ),
  med_home_val = list(
    label = "Median Home Value",
    fmt = "$",
    pal = "YlGn",
    rev = FALSE
  )
)

#' Fetch CBSA centroids from tigris and join ACS metro data
fetch_metro_sf <- function(metro_df, year = 2023) {
  if (is.null(metro_df) || nrow(metro_df) == 0) {
    return(NULL)
  }
  tryCatch(
    {
      message("[Metro map] Fetching CBSA shapefile from tigris...")
      cbsa_sf <- tigris::core_based_statistical_areas(
        cb = TRUE,
        resolution = "20m",
        year = year
      ) %>%
        sf::st_transform(4326)

      # Compute centroid for each CBSA (point geometry for bubble map)
      # Exclude territory CBSAs (PR=72, GU=66, VI=78, AS=60, MP=69)
      cbsa_centroids <- cbsa_sf %>%
        filter(!grepl("^(72|66|78|60|69)", GEOID)) %>%
        sf::st_centroid() %>%
        mutate(
          lon = sf::st_coordinates(geometry)[, 1],
          lat = sf::st_coordinates(geometry)[, 2]
        ) %>%
        sf::st_drop_geometry() %>%
        select(GEOID, lon, lat)

      # Join with ACS metro data on GEOID
      metro_df %>%
        dplyr::left_join(cbsa_centroids, by = "GEOID") %>%
        filter(!is.na(lon), !is.na(lat))
    },
    error = function(e) {
      message(sprintf(
        "[Metro map] CBSA centroid fetch failed: %s",
        conditionMessage(e)
      ))
      NULL
    }
  )
}

#' Build metro-level bubble map
build_metro_bubble_map <- function(
  metro_sf_df,
  variable = "unemp_rate",
  min_pop = 500000
) {
  if (is.null(metro_sf_df) || nrow(metro_sf_df) == 0) {
    return(NULL)
  }

  cfg <- METRO_VAR_CFG[[variable]] %||% METRO_VAR_CFG[["unemp_rate"]]

  df <- metro_sf_df %>%
    filter(
      pop >= min_pop,
      !is.na(.data[[variable]]),
      !is.na(lon),
      !is.na(lat),
      # Belt-and-suspenders: exclude any territory CBSAs that slipped through
      !grepl("^(72|66|78|60|69)", GEOID)
    )

  if (nrow(df) == 0) {
    return(NULL)
  }

  vals <- as.numeric(df[[variable]])

  colour_pal <- leaflet::colorBin(
    palette = cfg$pal,
    domain = vals,
    bins = 7,
    reverse = cfg$rev,
    na.color = "#555566"
  )

  # Radius: proportional to sqrt(population) — keeps large metros visible
  max_pop <- max(df$pop, na.rm = TRUE)
  radius <- sqrt(df$pop / max_pop) * 40 + 6 # range ~6–46 px

  # Format value for display
  fmt_val <- function(v) {
    if (is.na(v)) {
      return("N/A")
    }
    if (cfg$fmt == "$") {
      paste0("$", format(round(v), big.mark = ","))
    } else {
      paste0(round(v, 1), "%")
    }
  }

  popup_vec <- vapply(
    seq_len(nrow(df)),
    function(i) {
      r <- df[i, ]
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
        "padding:14px 16px;border-radius:8px;min-width:220px;line-height:1.9;'>",
        "<b style='color:#00b4d8;font-size:14px;'>",
        htmltools::htmlEscape(as.character(r$metro)),
        "</b><br/>",
        "<table style='width:100%;font-size:12px;'>",
        "<tr><td style='color:#9aa3b2;'>",
        cfg$label,
        "</td>",
        "<td style='color:#e0e0e0;font-weight:600;'>",
        fmt_val(as.numeric(r[[variable]])),
        "</td></tr>",
        "<tr><td style='color:#9aa3b2;'>Population</td>",
        "<td>",
        format(as.integer(r$pop), big.mark = ","),
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
        "</table></div>"
      )
    },
    character(1)
  )

  lbl_fmt <- if (cfg$fmt == "$") {
    leaflet::labelFormat(prefix = "$", big.mark = ",", between = " \u2013 $")
  } else {
    leaflet::labelFormat(suffix = "%", between = " \u2013 ")
  }

  leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
    leaflet::addProviderTiles(
      "CartoDB.DarkMatter",
      options = leaflet::providerTileOptions(opacity = 0.95)
    ) %>%
    leaflet::addCircleMarkers(
      lng = df$lon,
      lat = df$lat,
      radius = radius,
      fillColor = colour_pal(vals),
      fillOpacity = 0.80,
      color = "#0f1117",
      weight = 1,
      popup = popup_vec,
      label = paste0(df$metro, ": ", vapply(vals, fmt_val, character(1))),
      labelOptions = leaflet::labelOptions(
        style = list(
          "background" = "#1e2640",
          "color" = "#e0e0e0",
          "border" = "none",
          "padding" = "6px 10px"
        ),
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = colour_pal,
      values = vals,
      opacity = 0.9,
      title = cfg$label,
      position = "bottomright",
      labFormat = lbl_fmt
    ) %>%
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
}
