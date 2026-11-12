rk_county_base <- function() {
  data.frame(
    GEOID = c("47065", "13295", "13121", "13001", "13003"),
    state_fips = c("47", "13", "13", "13", "13"),
    state_name = c("Tennessee", "Georgia", "Georgia", "Georgia", "Georgia"),
    cbsa_geoid = c("16860", "16860", "12060", NA, NA),
    cbsa_name = c("Chattanooga", "Chattanooga", "Atlanta-Sandy Springs-Roswell", NA, NA),
    cbsa_type = c("metro", "metro", "metro", NA, NA),
    stringsAsFactors = FALSE
  )
}

rk_tracts <- function(county, pov) {
  n <- length(pov)
  sf::st_sf(GEOID = paste0(county, sprintf("%06d", seq_len(n))),
            county_geoid = county, poverty_rate = pov, noisy = FALSE,
            geometry = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(c(i, i))),
                                  crs = 4326))
}

rk_no_jobs <- function(state_fips, acs_geoids) NULL

rk_fetch <- function(missing = character()) {
  states <- list(
    "47" = rk_tracts("47065", c(10, 20)),
    "13" = rbind(rk_tracts("13295", c(30, 40)), rk_tracts("13121", c(5, 50)),
                 rk_tracts("13001", c(12, NA)), rk_tracts("13003", c(22)))
  )
  function(state_fips, year = NULL) if (state_fips %in% missing) NULL else states[[state_fips]]
}

test_that("metro tracts rank across state lines", {
  r <- tracts_for_county("47065", rk_county_base(), fetch = rk_fetch(),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  expect_equal(r$frame_label, "in Chattanooga metro")
  expect_setequal(r$tracts$county_geoid, c("47065", "13295"))
  t <- r$tracts[order(r$tracts$poverty_rate), ]
  expect_equal(round(t$lm_pct_poverty_rate, 1), c(0, 33.3, 66.7, 100))
  expect_equal(unique(t$frame_label), "in Chattanooga metro")
  expect_equal(length(r$missing_states), 0)
})

test_that("state rank uses whole state", {
  r <- tracts_for_county("13295", rk_county_base(), fetch = rk_fetch(),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  ga <- r$tracts[r$tracts$county_geoid == "13295", ]
  ga_all <- c(30, 40, 5, 50, 12, NA, 22)
  expected <- pct_rank(ga_all)[1:2]
  expect_equal(ga$st_pct_poverty_rate[order(ga$poverty_rate)], expected)
  expect_equal(unique(ga$state_label), "in Georgia")
  tn <- r$tracts[r$tracts$county_geoid == "47065", ]
  expect_equal(sort(tn$st_pct_poverty_rate), c(0, 100))
  expect_equal(unique(tn$state_label), "in Tennessee")
})

test_that("non-CBSA tracts rank within state non-CBSA frame", {
  r <- tracts_for_county("13001", rk_county_base(), fetch = rk_fetch(),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  expect_equal(r$frame_label, "in non-metro Georgia")
  expect_setequal(unique(r$tracts$county_geoid), c("13001", "13003"))
  has <- !is.na(r$tracts$poverty_rate)
  expect_false(any(is.na(r$tracts$lm_pct_poverty_rate[has])))
  expect_equal(sort(r$tracts$lm_pct_poverty_rate[has]), c(0, 100))
})

test_that("missing state is reported, others still ranked", {
  r <- tracts_for_county("47065", rk_county_base(), fetch = rk_fetch(missing = "13"),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  expect_equal(r$missing_states, "13")
  expect_equal(unique(r$tracts$county_geoid), "47065")
  expect_equal(sort(r$tracts$lm_pct_poverty_rate), c(0, 100))
})

test_that("all states missing returns NULL tracts", {
  r <- tracts_for_county("47065", rk_county_base(), fetch = rk_fetch(missing = c("13", "47")),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  expect_null(r$tracts)
  expect_setequal(r$missing_states, c("47", "13"))
})

test_that("tracts carry the size of their comparison frame", {
  r <- tracts_for_county("47065", rk_county_base(), fetch = rk_fetch(),
                         fetch_jobs = rk_no_jobs, vars = "poverty_rate")
  expect_equal(unique(r$tracts$frame_n), nrow(r$tracts))
})
