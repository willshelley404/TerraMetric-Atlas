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

test_that("unavailable layer legend says unavailable", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), NULL), "stress_now")
  title <- map_calls(m, "addLegend")[[1]]$args[[1]]$title
  expect_true(grepl("unavailable", title, fixed = TRUE))
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
  expect_true(grepl("+1.1 pp · Trigger", labels[2], fixed = TRUE))
  expect_false(grepl("NA metro", labels[2], fixed = TRUE))
})
