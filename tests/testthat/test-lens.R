fx_one_tract <- function() {
  sf::st_sf(
    GEOID = "13295020300", county_geoid = "13295", tract_name = "Census Tract 203",
    poverty_rate = 18, poverty_rate_moe = 2, noisy_poverty_rate = FALSE,
    lm_pct_poverty_rate = 92, st_pct_poverty_rate = 85,
    frame_label = "in Chattanooga metro", state_label = "in Georgia",
    geometry = sf::st_sfc(sq(-85.2, 34.5, 0.1), crs = 4326))
}

test_that("every layer has a lens and every lens has layers", {
  expect_true(all(vapply(RISK_LAYERS, function(l) l$lens %in% RISK_LENSES, TRUE)))
  expect_true(all(vapply(RISK_LENSES, function(z) length(risk_lens_choices(z)) > 0, TRUE)))
})

test_that("every lens default is one of its own layers and county-only layers draw no tracts", {
  for (z in RISK_LENSES) expect_true(risk_lens_default(z) %in% risk_lens_choices(z))
  expect_equal(unname(risk_lens_choices("context")[1]), "stress_now")
  expect_equal(names(risk_lens_choices("context"))[1], RISK_LAYERS$stress_now$label)
  m <- add_tract_layer(leaflet::leaflet(), fx_one_tract(), "stress_now",
                       build_county_layers(fx_county_base(), fx_momentum()))
  tr <- Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], "tracts"),
               m$x$calls)
  expect_length(tr, 0)
})

test_that("pp pill is keyed to sign", {
  expect_equal(.rank_bad(c(90, 10), RISK_LAYERS$stress_now, c(-0.1, 0.2)), c(FALSE, TRUE))
  cfg <- RISK_LAYERS$stress_now; cfg$higher_is_worse <- FALSE
  expect_equal(.rank_bad(c(90, 10), cfg, c(-0.1, 0.2)), c(TRUE, FALSE))
  cfg$higher_is_worse <- NA
  expect_equal(.rank_bad(c(90, 10), cfg, c(-0.1, 0.2)), c(FALSE, FALSE))
  cfg$fmt <- "per1k"
  expect_equal(.rank_bad(c(90, 10), cfg, c(-0.1, 0.2)), c(FALSE, FALSE))
})

test_that("binned legend labels", {
  expect_equal(.bin_labels(c(-1000, -10, -5, 5, 10, 1000), "per1k"),
               c("below −10", "−10 to −5", "−5 to +5", "+5 to +10", "above +10"))
  expect_equal(.bin_labels(c(-50, -1, -0.5, 0.5, 1, 50), "pp"),
               c("below −1.0", "−1.0 to −0.5", "−0.5 to +0.5", "+0.5 to +1.0", "above +1.0"))
})

test_that("a binned layer uses its bins and bin_labels in the legend", {
  expect_equal(RISK_LAYERS$stress_now$bin_labels[6:7],
               c("+0.5 to +1.0 (rising)", "above +1.0 pp (Sahm-style trigger)"))
  expect_length(RISK_LAYERS$stress_now$bin_labels, length(RISK_LAYERS$stress_now$bins) - 1)
  pal <- .risk_palette(list(fmt = "per1k", pal = "RdBu", rev = FALSE,
                            bins = c(-1000, -5, 5, 1000)), c(-20, 0, 20))
  expect_equal(length(unique(pal(c(-20, 0, 20)))), 3)
})

test_that("Sahm-style wording", {
  m <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), "stress_now")
  labs <- as.character(unclass(Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]$labels))
  expect_true(any(grepl("Sahm-style", labs, fixed = TRUE)))
  tips <- vapply(Filter(function(cl) cl$method == "addPolygons", m$x$calls)[[1]]$args[[7]],
                 as.character, "")
  expect_true(grepl("Sahm-style trigger", tips[2], fixed = TRUE))
  expect_false(grepl("Sahm-style", tips[1], fixed = TRUE))
})

test_that("standing lines render above footer", {
  h <- as.character(risk_tooltips_vec("X", value_text = "1", footer = "F",
                                      standing = "<div>S1</div>")[[1]])
  expect_true(grepl("S1", h, fixed = TRUE))
  expect_lt(regexpr("S1", h, fixed = TRUE), regexpr(">F<", h, fixed = TRUE))
  d <- as.character(risk_tooltips_vec("X", value_text = "1", footer = "F",
                                      drivers_html = "<div>DRV</div>", standing = "<div>S1</div>")[[1]])
  expect_lt(regexpr("DRV", d, fixed = TRUE), regexpr("S1", d, fixed = TRUE))
})
