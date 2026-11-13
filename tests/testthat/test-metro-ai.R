# Metro AI follows "Jump to metro" and covers micro areas as well as metros.
us_fx <- list(unemp_rate = 4, med_income = 70000, lfp_rate = 63, poverty_rate = 12,
              bachelors_plus_rate = 33, rent_burden_rate = 50, vacancy_rate = 7)

fx_acs_metros <- function(n_micro = 8) {
  mk <- function(geoid, type, i) data.frame(
    GEOID = geoid, metro = paste("Area", geoid), cbsa_type = type, pop = 20000 * i,
    med_age = 40, unemp_rate = 3 + i / 10, lfp_rate = 60, poverty_rate = 10 + i,
    vacancy_rate = 8, rent_burden_rate = 45 + i, bachelors_plus_rate = 15 + i,
    med_income = 50000 + 1000 * i, per_cap_income = 28000, med_home_val = 150000 + 5000 * i,
    med_rent = 800, unemp_vs_us = -0.5, lfp_vs_us = -3, income_vs_us = -10000,
    poverty_vs_us = 2, edu_vs_us = -13, stringsAsFactors = FALSE)
  micro <- do.call(rbind, lapply(seq_len(n_micro), function(i) mk(sprintf("3%04d", i), "micro area", i)))
  metro <- do.call(rbind, lapply(1:3, function(i) mk(sprintf("1%04d", i), "metro", i * 20)))
  rbind(micro, metro)
}

test_that("peer context ranks a micro area against other micro areas only", {
  d <- fx_acs_metros()
  p <- metro_peer_context(d, "30008")                # the highest-poverty micro
  expect_equal(p$peer_group, "micro areas")
  expect_equal(p$peer_count, 8)
  expect_equal(p$metrics$poverty_rate_pct$percentile_among_peers, 88)   # 7 of 8 below
  # same row ranked among all areas would be far lower: peers must be the micros
  expect_true(p$metrics$population$peer_median < 200000)
  expect_null(metro_peer_context(d, "99999"))
  expect_null(metro_peer_context(NULL, "30008"))
})

test_that("peer context is skipped when the peer group is too small to rank against", {
  expect_null(metro_peer_context(fx_acs_metros(n_micro = 3)[c(1:3, 4), ], "30001"))
})

test_that("an area ACS does not have still gets a row carrying its map name and type", {
  d <- fx_acs_metros()
  expect_equal(metro_acs_row(d, "30002")$GEOID, "30002")
  r <- metro_acs_row(d, "77777", "Nowhere", "micro area")
  expect_equal(r$metro, "Nowhere"); expect_equal(r$cbsa_type, "micro area")
  expect_true(is.na(r$unemp_rate))
  s <- build_metro_summary(r, us_fx)
  expect_equal(s$geography$type, "micro area")
  expect_false(grepl('"NA"|NaN', as.character(jsonlite::toJSON(s, auto_unbox = TRUE))))
})

test_that("summary names the area type and compares levels, never claims a trend", {
  d <- fx_acs_metros()
  s <- build_metro_summary(d[d$GEOID == "30002", ], us_fx,
                           peers = metro_peer_context(d, "30002"))
  expect_equal(s$geography$type, "micro area")
  expect_equal(s$labor$unemployment_rate$relative_to_us, "better than US")
  expect_null(s$labor$unemployment_rate$trend)
  expect_false(grepl("increasing|declining|improving", jsonlite::toJSON(s, auto_unbox = TRUE)))
  expect_false(is.null(s$peer_comparison))
  expect_null(build_metro_summary(d[1, ], us_fx)$peer_comparison)
})

test_that("macro block drops unavailable readings and the old placeholder fields", {
  s <- build_metro_summary(fx_acs_metros()[1, ], us_fx, kpis = list(fed_funds = 4.1, vix = 21))
  expect_equal(s$market$fed_funds_rate, 4.1)
  expect_equal(s$market$volatility, "moderate")
  expect_null(s$market$oil_price)
  expect_null(s$market$oil_trend)
  expect_false(grepl('"NA"', as.character(jsonlite::toJSON(s, auto_unbox = TRUE))))
})

test_that("the prompt tells the model how to treat micro areas and what the outlook rests on", {
  p <- build_metro_prompt(list(geography = list(type = "micro area")), "headline")
  expect_match(p, "micro area", fixed = TRUE)
  expect_match(p, "stress_ahead", fixed = TRUE)
  expect_match(p, "peer_comparison", fixed = TRUE)
  expect_match(p, "## Data Notes", fixed = TRUE)
})

test_that("the AI target follows Jump to metro, else the clicked county's metro", {
  cl <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration(),
                            stress_ahead = fx_stress_ahead(), credit_requested = FALSE)
  t1 <- metro_ai_target(cl, jump = "16860")
  expect_equal(t1$geoid, "16860"); expect_equal(t1$source, "jump")
  expect_equal(t1$name, "Chattanooga"); expect_equal(t1$type, "metro")
  expect_null(t1$selected_county)

  t2 <- metro_ai_target(cl, jump = "", selected_county = "13295")
  expect_equal(t2$geoid, "16860"); expect_equal(t2$source, "map")
  expect_equal(t2$selected_county, "13295")

  # jump wins; a county outside the jumped metro is not carried along
  t3 <- metro_ai_target(cl, jump = "16860", selected_county = "09110")
  expect_equal(t3$source, "jump"); expect_null(t3$selected_county)

  expect_null(metro_ai_target(cl, jump = "", selected_county = "09110"))   # not in any CBSA
  expect_null(metro_ai_target(cl, jump = NULL, selected_county = NULL))
  expect_null(metro_ai_target(cl, jump = "99999"))
})

test_that("a micro area flows through the risk rollup the same way as a metro", {
  b <- fx_county_base()
  b$cbsa_geoid[1] <- "99001"; b$cbsa_name[1] <- "Small Town"; b$cbsa_type[1] <- "micro area"
  cl <- build_county_layers(b, fx_momentum(), migration = fx_migration(),
                            stress_ahead = fx_stress_ahead(), credit_requested = FALSE)
  ctx <- metro_risk_context(cl, "99001")
  expect_equal(ctx$counties_in_metro, 1)
  expect_equal(metro_ai_target(cl, jump = "99001")$type, "micro area")
})
