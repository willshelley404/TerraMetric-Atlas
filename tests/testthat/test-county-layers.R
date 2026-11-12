test_that("xwalk assigns CBSA by point on surface", {
  x <- county_cbsa_xwalk(fx_counties_sf(), fx_cbsa_sf())
  expect_equal(x$cbsa_geoid[x$GEOID == "13295"], "16860")
  expect_equal(x$cbsa_name[x$GEOID == "13295"], "Chattanooga")
  expect_equal(x$cbsa_type[x$GEOID == "13295"], "metro")
  expect_true(is.na(x$cbsa_geoid[x$GEOID == "09110"]))
})

test_that("Connecticut planning region joins", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  expect_equal(l$momentum_pp[l$GEOID == "09110"], 1.1)
  expect_equal(l$flag[l$GEOID == "09110"], "Trigger")
})

test_that("missing LAUS leaves Stress now NA and marks layer unavailable", {
  l <- build_county_layers(fx_county_base(), NULL)
  expect_true(all(is.na(l$momentum_pp)))
  expect_equal(attr(l, "layer_status")[["stress_now"]], "unavailable")
  expect_equal(attr(l, "layer_status")[["poverty_rate"]], "ok")
})

test_that("stale LAUS marks Stress now stale", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), laus_stale = TRUE)
  expect_equal(attr(l, "layer_status")[["stress_now"]], "stale")
})

test_that("partial LAUS marks Stress now partial in status and legend", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), laus_partial = TRUE)
  expect_equal(attr(l, "layer_status")[["stress_now"]], "partial")
  m <- build_risk_county_map(l, "stress_now")
  title <- Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]$title
  expect_true(grepl("partial", title, fixed = TRUE))
})

test_that("pct columns exist for every layer", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  cols <- vapply(RISK_LAYERS, `[[`, "", "col")
  expect_true(all(paste0("pct_", cols) %in% names(l)))
  expect_equal(l$pct_momentum_pp, c(0, 100))
})

test_that("as-of labels per layer", {
  a <- attr(build_county_layers(fx_county_base(), fx_momentum()), "asof")
  expect_equal(a[["stress_now"]], "Sep 2026")
  expect_equal(a[["poverty_rate"]], "ACS 2020–2024")
})
