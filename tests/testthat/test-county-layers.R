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

test_that("sector weights are kept without a Stress ahead component", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = NULL,
                           sector_weights = fx_weights())
  expect_equal(attr(l, "sector_weights"), fx_weights())
  expect_equal(attr(l, "layer_status")[["stress_ahead"]], "unavailable")
  # Backward compatible: weights riding on stress_ahead are used when no argument is given.
  expect_equal(attr(build_county_layers(fx_county_base(), fx_momentum(),
                                        stress_ahead = fx_stress_ahead()), "sector_weights"),
               fx_weights())
  expect_null(attr(build_county_layers(fx_county_base(), fx_momentum()), "sector_weights"))
})

test_that("one loading vector drives every component status", {
  l <- build_county_layers(fx_county_base(), NULL, loading = c("stress_now", "migration"))
  st <- attr(l, "layer_status")
  expect_equal(st[["stress_now"]], "loading")
  expect_equal(st[["mig_dom"]], "loading")
  expect_equal(st[["poverty_rate"]], "ok")
  # Data wins over a leftover loading flag only when the part is absent; present data is ok.
  l2 <- build_county_layers(fx_county_base(), fx_momentum(), loading = character(0))
  expect_equal(attr(l2, "layer_status")[["stress_now"]], "ok")
})

test_that("sector weights survive the app's own %||% definitions", {
  # global.R / synopsis_tier1.R redefine %||% with scalar semantics; a data frame must pass.
  app_op <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
  assign("%||%", app_op, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  l <- build_county_layers(fx_county_base(), fx_momentum(), sector_weights = fx_weights())
  expect_equal(attr(l, "sector_weights"), fx_weights())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_false(grepl("Industry mix", h, fixed = TRUE))   # weights alone: no industry shares
})
