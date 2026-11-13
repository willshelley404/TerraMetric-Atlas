fx_tracts <- function() {
  sf::st_sf(
    GEOID = c("13295020300", "13295020400"), county_geoid = "13295",
    tract_name = c("Census Tract 203, Walker County, Georgia",
                   "Census Tract 204, Walker County, Georgia"),
    poverty_rate = c(18, 9), poverty_rate_moe = c(20, 3),
    noisy_poverty_rate = c(TRUE, FALSE), noisy_rent_burden_rate = c(FALSE, FALSE),
    lm_pct_poverty_rate = c(92, 30), st_pct_poverty_rate = c(85, 25),
    frame_label = "in Chattanooga metro", state_label = "in Georgia",
    geometry = sf::st_sfc(sq(-85.2, 34.5, 0.1), sq(-85.0, 34.5, 0.1), crs = 4326)
  )
}

fx_layers <- function() build_county_layers(fx_county_base(), fx_momentum())
calls_in_group <- function(m, group) {
  Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], group), m$x$calls)
}

test_that("tract layer uses tract GEOID layerIds and dims counties", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "poverty_rate"),
                       fx_tracts(), "poverty_rate", fx_layers())
  tr <- calls_in_group(m, "tracts")
  expect_length(tr, 1)
  expect_equal(tr[[1]]$args[[2]], c("13295020300", "13295020400"))
  co <- calls_in_group(m, "counties")
  expect_equal(co[[length(co)]]$args[[4]]$fillOpacity, 0.25)
  # On a leaflet object fitBounds is stored in x$fitBounds (on a proxy it is a call).
  expect_equal(unlist(m$x$fitBounds)[1:4], c(34.5, -85.2, 34.6, -84.9), tolerance = 1e-6)
})

test_that("noisy tracts get dashed outline", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "poverty_rate"),
                       fx_tracts(), "poverty_rate", fx_layers())
  o <- calls_in_group(m, "tracts")[[1]]$args[[4]]
  expect_equal(o$dashArray, c("4", NA))
  expect_equal(o$color, c("#fcc419", "#0f1117"))
})

test_that("noise flag follows the layer being shown", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "rent_burden_rate"),
                       transform(fx_tracts(), rent_burden_rate = c(40, 50)),
                       "rent_burden_rate", fx_layers())
  expect_true(all(is.na(calls_in_group(m, "tracts")[[1]]$args[[4]]$dashArray)))
})

test_that("fit = FALSE keeps the current view (layer switch while drilled in)", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "poverty_rate"),
                       fx_tracts(), "poverty_rate", fx_layers(), fit = FALSE)
  expect_null(m$x$fitBounds)
  expect_length(calls_in_group(m, "tracts"), 1)
})

test_that("county-only layer still zooms to the frame when drilling in", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "stress_now"),
                       fx_tracts(), "stress_now", fx_layers())
  expect_false(is.null(m$x$fitBounds))
})

test_that("fit_to_counties zooms to the given counties, not the whole frame", {
  m <- fit_to_counties(build_risk_county_map(fx_layers(), "poverty_rate"), fx_layers(), "13295")
  expect_equal(unlist(m$x$fitBounds)[1:4], c(34, -85, 35, -84), tolerance = 1e-6)
})

test_that("county-only layer adds no tracts", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "stress_now"),
                       fx_tracts(), "stress_now", fx_layers())
  expect_length(calls_in_group(m, "tracts"), 0)
  co <- calls_in_group(m, "counties")
  expect_equal(co[[length(co)]]$args[[4]]$fillOpacity, 0.82)
})

test_that("tract tooltip shows labor-market rank and state rank", {
  m <- add_tract_layer(build_risk_county_map(fx_layers(), "poverty_rate"),
                       fx_tracts(), "poverty_rate", fx_layers())
  labels <- vapply(calls_in_group(m, "tracts")[[1]]$args[[7]], as.character, "")
  expect_true(grepl("Top 8% in Chattanooga metro", labels[1], fixed = TRUE))
  expect_true(grepl("Top 15% in Georgia", labels[1], fixed = TRUE))
  expect_true(grepl("High margin of error", labels[1], fixed = TRUE))
  expect_false(grepl("High margin of error", labels[2], fixed = TRUE))
})

test_that("reset_to_us clears tracts and restores the US view", {
  m <- reset_to_us(build_risk_county_map(fx_layers(), "poverty_rate"), fx_layers(), "poverty_rate")
  methods <- vapply(m$x$calls, `[[`, "", "method")
  expect_true("clearGroup" %in% methods)
  co <- calls_in_group(m, "counties")
  expect_equal(co[[length(co)]]$args[[4]]$fillOpacity, 0.82)
  expect_equal(m$x$setView[[1]], c(38, -96))
})
