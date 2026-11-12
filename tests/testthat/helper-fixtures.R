# Synthetic geography shared by the county and tract tests.
sq <- function(x0, y0, w = 1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w),
                            c(x0, y0 + w), c(x0, y0))))
}

fx_counties_sf <- function() {
  sf::st_sf(GEOID = c("13295", "09110"),
            geometry = sf::st_sfc(sq(-85, 34), sq(-73, 41), crs = 4326))
}

fx_cbsa_sf <- function() {
  sf::st_sf(GEOID = "16860", NAME = "Chattanooga, TN-GA", LSAD = "M1",
            geometry = sf::st_sfc(sq(-85.5, 33.5, 2), crs = 4326))
}

fx_county_base <- function() {
  b <- fx_counties_sf()
  b$name <- c("Walker County, GA", "Capitol Planning Region, CT")
  b$state_fips <- c("13", "09"); b$state_abbr <- c("GA", "CT")
  b$state_name <- c("Georgia", "Connecticut")
  b$cbsa_geoid <- c("16860", NA); b$cbsa_name <- c("Chattanooga", NA)
  b$cbsa_type <- c("metro", NA)
  b$pop <- c(69000, 980000); b$med_age <- c(41, 40)
  b$unemp_rate <- c(3.5, 5.1); b$lfp_rate <- c(60, 66); b$poverty_rate <- c(14, 10)
  b$vacancy_rate <- c(9, 7); b$rent_burden_rate <- c(48, 52)
  b$bachelors_plus_rate <- c(20, 42); b$med_income <- c(60000, 85000)
  b$per_cap_income <- c(30000, 47000); b$med_home_val <- c(190000, 310000)
  b$med_rent <- c(900, 1300)
  attr(b, "acs_year") <- 2024
  b
}

fx_momentum <- function() {
  tibble::tibble(fips = c("13295", "09110"), ref_month = as.Date("2026-09-01"),
                 rate_now = c(3.2, 5.37), rate_yr_ago = c(3.37, 4.27),
                 momentum_pp = c(-0.17, 1.1), flag = c("None", "Trigger"))
}

fx_migration <- function() {
  m <- tibble::tibble(
    GEOID = c("13295", "09110"),
    mig_dom_rate = c(8.2, -0.11), mig_intl_rate = c(1.0, 4.78), mig_net_rate = c(9.2, 4.67),
    nat_chg_rate = c(-1.5, 0.23), mig_dom_cum = c(40.1, -12.0), pop_chg_pct = c(4.0, 1.2),
    mig_dom_n = c(560, -110), pop_avg = c(69000, 990000),
    mig_dom_rate_2021 = c(3, -2), mig_dom_rate_2022 = c(10, -4), mig_dom_rate_2023 = c(12, -1),
    mig_dom_rate_2024 = c(6, 0.5), mig_dom_rate_2025 = c(8.2, -0.11))
  attr(m, "vintage") <- 2025
  m
}
