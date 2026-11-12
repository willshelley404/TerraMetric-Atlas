map_calls <- function(m, method) Filter(function(cl) cl$method == method, m$x$calls)

test_that("county map has one polygon per county with GEOID layerIds", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  m <- build_risk_county_map(l, "stress_now")
  expect_s3_class(m, "leaflet")
  polys <- map_calls(m, "addPolygons")
  expect_length(polys, 1)
  expect_equal(polys[[1]]$args[[2]], c("13295", "09110"))   # layerId
  expect_equal(polys[[1]]$args[[3]], "counties")            # group
})

test_that("legend title carries source and as-of", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "stress_now")
  title <- map_calls(m, "addLegend")[[1]]$args[[1]]$title
  expect_true(grepl("BLS LAUS, Sep 2026", title, fixed = TRUE))
})

test_that("ranks describe the value itself; pill is red only on the bad side", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "med_income")
  labels <- vapply(map_calls(m, "addPolygons")[[1]]$args[[7]], as.character, "")
  # CT ($85k) is the higher income, GA ($60k) the lower
  expect_true(grepl("Top 1% nationally", labels[2], fixed = TRUE))
  expect_true(grepl("Bottom 1% nationally", labels[1], fixed = TRUE))
  expect_true(grepl("#5c1f2a", labels[1], fixed = TRUE))    # low income: bad side, red pill
  expect_false(grepl("#5c1f2a", labels[2], fixed = TRUE))   # high income: neutral pill
})

test_that("legend is compact: two-line title, one-decimal labels", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "poverty_rate")
  lg <- map_calls(m, "addLegend")[[1]]$args[[1]]
  expect_true(grepl("<br>", lg$title, fixed = TRUE))
  expect_false(any(grepl("\\.\\d{2}", unclass(lg$labels))))
})

test_that("base tiles need no API key", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "poverty_rate")
  methods <- vapply(m$x$calls, `[[`, "", "method")
  expect_true("addTiles" %in% methods)
  expect_false("addProviderTiles" %in% methods)
})

test_that("Stress now legend uses readable open-ended bins", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "stress_now")
  labs <- as.character(unclass(map_calls(m, "addLegend")[[1]]$args[[1]]$labels))  # leaflet wraps in I()
  expect_equal(labs[c(1, 7)], c("below −1.0 pp", "above +1.0 pp (Sahm-style trigger)"))
  expect_false(any(grepl("50", labs)))
})

test_that("unavailable layer legend says unavailable", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), NULL), "stress_now")
  title <- map_calls(m, "addLegend")[[1]]$args[[1]]$title
  expect_true(grepl("unavailable", title, fixed = TRUE))
})

test_that("Stress now shows loading while BLS data is still coming", {
  l <- build_county_layers(fx_county_base(), NULL, laus_loading = TRUE)
  expect_equal(attr(l, "layer_status")[["stress_now"]], "loading")
  title <- map_calls(build_risk_county_map(l, "stress_now"), "addLegend")[[1]]$args[[1]]$title
  expect_true(grepl("loading county unemployment", title, fixed = TRUE))
  labels <- vapply(map_calls(build_risk_county_map(l, "stress_now"), "addPolygons")[[1]]$args[[7]],
                   as.character, "")
  expect_true(grepl("Loading", labels[1], fixed = TRUE))
})

test_that("stale LAUS legend says (stale)", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum(), TRUE),
                             "stress_now")
  title <- map_calls(m, "addLegend")[[1]]$args[[1]]$title
  expect_true(grepl("(stale)", title, fixed = TRUE))
})

test_that("tooltips carry rank pill, metro subtitle and Stress now flag", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "stress_now")
  # addPolygons args: polygons, layerId, group, options, popup, popupOptions, label, ...
  labels <- vapply(map_calls(m, "addPolygons")[[1]]$args[[7]], as.character, "")
  expect_true(grepl("Chattanooga metro", labels[1], fixed = TRUE))
  expect_true(grepl("nationally", labels[1], fixed = TRUE))
  expect_true(grepl("+1.1 pp · Sahm-style trigger", labels[2], fixed = TRUE))
  expect_false(grepl("NA metro", labels[2], fixed = TRUE))
})
