# Assistance lens: county values, registry, tooltips, side panel.

al_layers <- function(loading = character(0), ...) {
  build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead(),
                      loading = loading, ...)
}

# One-tract frame with assistance columns (low-wage share optional).
al_tracts <- function(low_wage = c(35, 20), noisy_pov = c(TRUE, FALSE)) {
  sf::st_sf(
    GEOID = c("13295020300", "13295020400"), county_geoid = "13295",
    tract_name = c("Census Tract 203, Walker County, Georgia",
                   "Census Tract 204, Walker County, Georgia"),
    assist_score = c(72, 31), assist_exposure = c(60, 40), assist_cushion = c(80, 20),
    assist_workers = c(1234.4, 800), assist_source = "LODES 2024",
    weak_share = c(45, 20), low_wage_share = low_wage, rent_burden_rate = c(55, 35),
    poverty_rate = c(18, 9), rent_burdened_hh = c(210, 90), job_pressure = 0.82,
    noisy_poverty_rate = noisy_pov, noisy_rent_burden_rate = c(FALSE, FALSE),
    lm_pct_assist_score = c(90, 30), st_pct_assist_score = c(85, 25),
    lm_pct_assist_exposure = c(80, 20), st_pct_assist_exposure = c(70, 20),
    lm_pct_assist_cushion = c(95, 10), st_pct_assist_cushion = c(90, 10),
    lm_pct_assist_workers = c(70, 40), st_pct_assist_workers = c(60, 30),
    lm_pct_weak_share = c(95, 20), lm_pct_low_wage_share = c(80, 30),
    lm_pct_rent_burden_rate = c(90, 40),
    frame_label = "in Chattanooga metro", state_label = "in Georgia",
    geometry = sf::st_sfc(sq(-85.2, 34.5, 0.1), sq(-85.0, 34.5, 0.1), crs = 4326)
  )
}

al_tract_labels <- function(tr, layer = "assist_score") {
  m <- add_tract_layer(build_risk_county_map(al_layers(), layer), tr, layer, al_layers())
  Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], "tracts"),
         m$x$calls)[[1]]
}

test_that("county assistance computed when weights present, loading otherwise", {
  l <- al_layers()
  expect_false(anyNA(l$assist_score))
  expect_true(all(c("exposure", "exp_workers", "weak_share", "assist_exposure",
                    "assist_cushion", "assist_workers") %in% names(l)))
  expect_false(anyNA(l$exposure[l$GEOID %in% c("13295", "09110")]))
  # Jobs are share x emp_total, so workers match the weighted headcount of the fixture.
  ind <- fx_industry()
  w <- fx_weights()
  wj <- as.matrix(as.data.frame(ind)[paste0("emp_", RISK_SECTORS$group)]) %*%
    w$w[match(RISK_SECTORS$group, w$group)]
  expect_equal(l$assist_workers[l$GEOID == "13295"], round(as.numeric(wj)[1]))
  st <- attr(l, "layer_status")
  expect_equal(st[["assist_score"]], "ok")
  expect_match(attr(l, "asof")[["assist_score"]], "^ACS 2020–2024 · CES Oct 2026$")
  expect_equal(attr(l, "sector_weights"), fx_weights())

  ld <- al_layers(loading = "stress_ahead")
  expect_equal(attr(ld, "layer_status")[["assist_score"]], "loading")
  expect_equal(attr(ld, "layer_status")[["assist_workers"]], "loading")

  nw <- build_county_layers(fx_county_base(), fx_momentum())
  expect_true(all(is.na(nw$assist_score)))
  expect_equal(attr(nw, "layer_status")[["assist_score"]], "unavailable")
})

test_that("county assistance uses national percentiles and job pressure", {
  l <- al_layers()
  expect_equal(l$job_pressure, job_pressure(l))
  d <- sf::st_drop_geometry(l)
  e <- assistance_score(d$exposure, d[c("rent_burden_rate", "poverty_rate")], d$job_pressure,
                        rep("us", nrow(d)))
  expect_equal(l$assist_score, e$score)
  expect_equal(l$assist_cushion, e$pct_cushion)
})

test_that("stress_ahead keeps emp_total", {
  expect_equal(fx_stress_ahead()$emp_total, fx_industry()$emp_total)
})

test_that("every assistance layer is in the assistance lens at both levels", {
  ids <- c("assist_score", "assist_exposure", "assist_cushion", "assist_workers")
  expect_equal(unname(risk_lens_choices("assistance")), ids)
  for (id in ids) {
    expect_equal(RISK_LAYERS[[id]]$lens, "assistance")
    expect_equal(RISK_LAYERS[[id]]$levels, c("county", "tract"))
  }
  expect_equal(RISK_LENSES, c("Assistance" = "assistance", "Migration" = "migration",
                              "Context" = "context"))
  expect_true(all(ids %in% .tract_vars()))
  expect_equal(RISK_LAYERS$assist_score$bins, c(0, 20, 35, 50, 65, 80, 101))
})

test_that("workers format with thousands separator", {
  expect_equal(.risk_fmt(12345.4, "n"), "12,345")
  expect_equal(.risk_fmt(c(7.6, NA), "n"), c("8", NA))
  expect_equal(.risk_fmt(61.7, "score"), "62")
})

test_that("assistance layers draw at the county level, also while loading", {
  for (id in c("assist_score", "assist_exposure", "assist_cushion", "assist_workers")) {
    m <- build_risk_county_map(al_layers(), id)
    expect_true("addLegend" %in% vapply(m$x$calls, `[[`, "", "method"))
    m2 <- build_risk_county_map(al_layers(loading = "stress_ahead"), id)
    expect_true("addLegend" %in% vapply(m2$x$calls, `[[`, "", "method"))
    m3 <- build_risk_county_map(build_county_layers(fx_county_base(), fx_momentum()), id)
    expect_true("addLegend" %in% vapply(m3$x$calls, `[[`, "", "method"))
  }
})

test_that("county tooltip shows drivers and flags missing job pressure", {
  l <- al_layers()
  tip <- as.character(.county_tooltips(l[l$GEOID == "13295", ], "assist_score",
                                       RISK_LAYERS$assist_score,
                                       attr(l, "asof")[["assist_score"]]))
  for (s in c("Workers in weakening sectors", "Renters cost-burdened", "Poverty rate",
              "LODES / ACS × CES sector weights"))
    expect_true(grepl(s, tip, fixed = TRUE), info = s)
  expect_false(grepl("Job pressure: missing", tip, fixed = TRUE))
  l$job_pressure <- NA_real_
  tip2 <- as.character(.county_tooltips(l[l$GEOID == "13295", ], "assist_score",
                                        RISK_LAYERS$assist_score, "x"))
  expect_true(grepl("Job pressure: missing (BLS/FRED unavailable)", tip2, fixed = TRUE))
  j <- job_pressure(al_layers())[al_layers()$GEOID == "13295"]
  expect_true(grepl(sprintf("Job pressure: %.0f/100 (mean of Stress now and Stress ahead percentiles)", 100 * j),
                    tip, fixed = TRUE))
  expect_false(grepl("percentile<", tip, fixed = TRUE))
})

test_that("tract tooltip shows 4 drivers with workers last", {
  lab <- as.character(al_tract_labels(al_tracts())$args[[7]][[1]])
  pos <- vapply(c("Workers in weakening sectors", "Low-wage workers (≤$1,250/mo)",
                  "Renters cost-burdened", "≈ Exposure-weighted workers"),
                function(s) regexpr(s, lab, fixed = TRUE)[[1]], 1)
  expect_true(all(pos > 0))
  expect_true(all(diff(pos) > 0))
  expect_true(grepl("1,234", lab, fixed = TRUE))
  # Values above the median are red.
  expect_true(grepl("color:#ff6b6b;\">45.0%", lab, fixed = TRUE))
  lab2 <- as.character(al_tract_labels(al_tracts())$args[[7]][[2]])
  expect_false(grepl("color:#ff6b6b;\">20.0%", lab2, fixed = TRUE))
})

test_that("an ACS tract (no wage band) omits the low-wage line but keeps workers", {
  lab <- as.character(al_tract_labels(al_tracts(low_wage = NA_real_))$args[[7]][[1]])
  expect_false(grepl("Low-wage", lab, fixed = TRUE))
  expect_true(grepl("Workers in weakening sectors", lab, fixed = TRUE))
  expect_true(grepl("≈ Exposure-weighted workers", lab, fixed = TRUE))
})

test_that("risk_drivers_vec shows the first max_rows present rows", {
  v <- list("1", NA_character_, "3", "4", "5")
  h <- risk_drivers_vec(paste0("L", 1:5), v, rep(list(FALSE), 5), n = 1, max_rows = 3)
  expect_equal(lengths(regmatches(h, gregexpr("<span>L", h))), 3)
  expect_false(grepl("L5", h, fixed = TRUE))
  h2 <- risk_drivers_vec(paste0("L", 1:5), v, rep(list(FALSE), 5), n = 1, max_rows = 4)
  expect_true(grepl("L5", h2, fixed = TRUE))
  # Default stays 3.
  h3 <- risk_drivers_vec(paste0("L", 1:5), as.list(as.character(1:5)), rep(list(FALSE), 5), n = 1)
  expect_equal(lengths(regmatches(h3, gregexpr("<span>L", h3))), 3)
})

test_that("assistance noise flag uses cushion inputs", {
  cl <- al_tract_labels(al_tracts(noisy_pov = c(TRUE, FALSE)))
  o <- cl$args[[4]]
  expect_equal(o$dashArray, c("4", NA))
  expect_true(grepl("High margin of error", as.character(cl$args[[7]][[1]]), fixed = TRUE))
  expect_false(grepl("High margin of error", as.character(cl$args[[7]][[2]]), fixed = TRUE))
  # Rent burden alone also flags.
  tr <- al_tracts(noisy_pov = c(FALSE, FALSE))
  tr$noisy_rent_burden_rate <- c(FALSE, TRUE)
  expect_equal(al_tract_labels(tr)$args[[4]]$dashArray, c(NA, "4"))
})

test_that("noisy_cols defaults to the layer's own noisy column", {
  tr <- al_tracts(); tr$noisy_poverty_rate <- c(TRUE, FALSE)
  tr$poverty_rate_moe <- c(20, 3)
  m <- add_tract_layer(build_risk_county_map(al_layers(), "poverty_rate"), tr, "poverty_rate",
                       al_layers())
  o <- Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], "tracts"),
              m$x$calls)[[1]]$args[[4]]
  expect_equal(o$dashArray, c("4", NA))
})

al_panel_tract <- function(assist_source = "LODES 2024", ...) {
  t <- sf::st_drop_geometry(al_tracts())[1, ]
  t$assist_source <- assist_source
  as.character(risk_side_panel(t, "tract", al_layers(), ...))
}

test_that("side panel names the industry source and count caveat", {
  h <- al_panel_tract()
  for (s in c("Industry mix source: LODES 2024", "not a layoff forecast",
              "Exposure-weighted workers: 1,234", "Rent-burdened renter households: 210",
              "ACS estimate",
              "County job pressure: 82/100 (mean of Stress now and Stress ahead percentiles)"))
    expect_true(grepl(s, h, fixed = TRUE), info = s)
  # Extra blocks are divs; the table keeps one row per tract layer plus the header.
  tract_layers <- sum(vapply(RISK_LAYERS, function(l) "tract" %in% l$levels, TRUE))
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), tract_layers + 1)
  l <- al_layers()
  hc <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_equal(lengths(regmatches(hc, gregexpr("<tr", hc))), length(RISK_LAYERS) + 1)
})

test_that("county side panel names the ACS source and omits tract-only lines", {
  l <- al_layers()
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_true(grepl("Industry mix source: ACS 2020–2024 (county)", h, fixed = TRUE))
  expect_true(grepl("not a layoff forecast", h, fixed = TRUE))
  expect_true(grepl("County job pressure:", h, fixed = TRUE))
  expect_false(grepl("Rent-burdened renter households", h, fixed = TRUE))
})

test_that("tract side panel falls back to the state's source when the tract has none", {
  h <- al_panel_tract(assist_source = NA_character_,
                      sources = c("13" = "LODES 2024"))
  expect_true(grepl("Industry mix source: LODES 2024", h, fixed = TRUE))
})

test_that("mixed LODES/ACS frames list the source per state", {
  h <- al_panel_tract(sources = c("13" = "LODES 2024", "47" = "ACS 2020–2024 (LODES unavailable)"))
  expect_true(grepl("Industry mix source by state: Georgia — LODES 2024; ", h, fixed = TRUE))
  expect_true(grepl("(cushion uses 2 inputs there)", h, fixed = TRUE))
  h1 <- al_panel_tract(sources = c("13" = "LODES 2024", "47" = "LODES 2024"))
  expect_false(grepl("by state", h1, fixed = TRUE))
})

test_that("assistance side panel works under the app's %||%", {
  # The app's final definition (synopsis_tier3.R): only the first element is checked.
  app_op <- function(a, b) {
    if (is.null(a)) return(b)
    ok <- tryCatch(!is.na(a[[1]]), error = function(e) TRUE)
    if (isTRUE(ok) && length(a) > 0) a else b
  }
  assign("%||%", app_op, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  l <- al_layers()
  expect_s3_class(risk_side_panel(l[l$GEOID == "13295", ], "county", l), "shiny.tag")
  expect_equal(attr(l, "sector_weights"), fx_weights())
  expect_false(is.null(al_tract_labels(al_tracts())))
})

test_that("assistance side panel words the dashed-outline note as inputs", {
  h <- al_panel_tract()
  expect_true(grepl("margin of error is larger than the value for an input (rent burden or poverty) to: Assistance need score",
                    h, fixed = TRUE))
  expect_false(grepl("larger than the value for Assistance", h, fixed = TRUE))
})

test_that("mixed noisy tract lists ACS and assistance labels in their own sentences", {
  l <- al_layers()
  t <- sf::st_drop_geometry(al_tracts())[1, ]
  t$assist_source <- "LODES 2024"; t$poverty_rate_moe <- 20
  h <- as.character(risk_side_panel(t, "tract", l))
  sents <- regmatches(h, gregexpr("Dashed outline: the margin of error[^<]*", h))[[1]]
  expect_length(sents, 2)
  plain <- sents[!grepl("an input", sents, fixed = TRUE)]
  inp <- sents[grepl("an input", sents, fixed = TRUE)]
  expect_true(grepl("Poverty Rate", plain, fixed = TRUE))
  expect_false(grepl("Assistance need score", plain, fixed = TRUE))
  expect_true(grepl("Assistance need score", inp, fixed = TRUE))
  expect_false(grepl("Poverty Rate", inp, fixed = TRUE))
})

test_that("tract legend of a binned layer uses its bin labels", {
  m <- add_tract_layer(build_risk_county_map(al_layers(), "assist_score"), al_tracts(),
                       "assist_score", al_layers())
  lg <- Filter(function(cl) cl$method == "addLegend", m$x$calls)
  labs <- as.character(unclass(lg[[length(lg)]]$args[[1]]$labels))
  expect_equal(labs, RISK_LAYERS$assist_score$bin_labels)
  expect_true(grepl("tracts in Chattanooga metro", lg[[length(lg)]]$args[[1]]$title, fixed = TRUE))
})
