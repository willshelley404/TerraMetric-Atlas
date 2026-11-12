test_that("side panel lists as-of and source per row", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  h <- as.character(risk_side_panel(l[l$GEOID == "09110", ], "county", l))
  for (s in c("Capitol Planning Region, CT", "BLS LAUS", "Sep 2026",
              "Census ACS 5-yr", "ACS 2020–2024", "Trigger", "5.4% now vs 4.3% a year ago"))
    expect_true(grepl(s, h, fixed = TRUE), info = s)
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), length(RISK_LAYERS) + 1)  # + header
})

test_that("tract side panel shows labor-market and state percentiles and MOE note", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  t <- (data.frame(
    GEOID = "13295020300", county_geoid = "13295",
    tract_name = "Census Tract 203, Walker County, Georgia",
    poverty_rate = 18, poverty_rate_moe = 20, noisy_poverty_rate = TRUE,
    lm_pct_poverty_rate = 92, st_pct_poverty_rate = 85,
    frame_label = "in Chattanooga metro", state_label = "in Georgia"))
  h <- as.character(risk_side_panel(t, "tract", l))
  for (s in c("Labor market", "State", "Top 8%", "Top 15%", "18.0% ± 20.0",
              "margin of error is larger than the value"))
    expect_true(grepl(s, h, fixed = TRUE), info = s)
  expect_false(grepl("BLS LAUS", h, fixed = TRUE))   # county-only layer not listed
})

test_that("side panel reports missing states", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l, missing_states = "13"))
  expect_true(grepl("Tract data unavailable for: Georgia", h, fixed = TRUE))
})
