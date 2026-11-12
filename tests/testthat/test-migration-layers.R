mig_labels <- function(m) {
  poly <- Filter(function(cl) cl$method == "addPolygons", m$x$calls)[[1]]
  vapply(poly$args[[7]], as.character, "")
}

test_that("migration joins and has as-of", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  expect_equal(l$mig_dom_rate[l$GEOID == "09110"], -0.11)
  expect_equal(attr(l, "asof")[["mig_dom"]], "V2025, 2024–25")
  expect_equal(attr(l, "asof")[["mig_dom_cum"]], "V2025, 2020–25")
  expect_equal(attr(l, "pep_vintage"), 2025)
  expect_equal(attr(l, "layer_status")[["mig_dom"]], "ok")
})

test_that("stale migration is marked stale", {
  mg <- fx_migration(); attr(mg, "stale") <- TRUE
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = mg)
  expect_equal(attr(l, "layer_status")[["mig_net"]], "stale")
})

test_that("missing PEP marks migration unavailable, loading when asked", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = NULL)
  expect_equal(attr(l, "layer_status")[["mig_dom"]], "unavailable")
  expect_true(all(is.na(l$mig_dom_rate)))
  l2 <- build_county_layers(fx_county_base(), fx_momentum(), migration = NULL,
                            loading = "migration")
  expect_equal(attr(l2, "layer_status")[["mig_dom"]], "loading")
  expect_equal(attr(l2, "layer_status")[["stress_now"]], "ok")
})

test_that("standing line on every lens", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  labs <- mig_labels(build_risk_county_map(l, "poverty_rate"))
  expect_true(grepl("Net domestic migration: +8.2 per 1,000 (2024–25)", labs[1], fixed = TRUE))
})

test_that("no standing line when PEP missing", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = NULL)
  labs <- mig_labels(build_risk_county_map(l, "poverty_rate"))
  expect_false(any(grepl("Net domestic migration:", labs, fixed = TRUE)))
})

test_that("migration pill never red and drivers listed", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  labs <- mig_labels(build_risk_county_map(l, "mig_dom"))
  for (s in c("for in-migration", "Natural change (births − deaths)", "International"))
    expect_true(all(grepl(s, labs, fixed = TRUE)), info = s)
  expect_false(any(grepl("#5c1f2a", labs, fixed = TRUE)))
  expect_false(any(grepl("#ff6b6b", labs, fixed = TRUE)))
  expect_true(grepl("+8.2 per 1,000", labs[1], fixed = TRUE))
})

test_that("migration layers are registered with a per1k diverging palette", {
  ids <- c("mig_dom", "mig_intl", "mig_net", "mig_dom_cum")
  expect_equal(unname(risk_lens_choices("migration")), ids)
  expect_equal(risk_lens_default("migration"), "mig_dom")
  expect_equal(unname(RISK_LENSES), c("migration", "context"))
  expect_equal(.risk_fmt(c(5.5, -0.11, NA), "per1k"),
               c("+5.5 per 1,000", "-0.1 per 1,000", NA))
})

test_that("diverging palette centred on zero", {
  pal <- .risk_palette(RISK_LAYERS$mig_dom, c(-3, 3))
  expect_equal(pal(-0.1), pal(0.1))
  expect_false(pal(-0.1) == pal(-6))
  expect_false(pal(-6) == pal(6))
  expect_false(pal(0.1) == pal(6))
})

test_that("migration legend uses per-1,000 bin labels", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  m <- build_risk_county_map(l, "mig_dom")
  lg <- Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]
  expect_true(grepl("Census PEP, V2025, 2024–25", lg$title, fixed = TRUE))
  labs <- as.character(unclass(lg$labels))
  expect_equal(labs[c(1, 5, 9)], c("below −10", "−0.5 to +0.5", "above +10"))
})

test_that("risk_drivers_vec matches the single drivers block", {
  v <- risk_drivers_vec(c("A", "B"), list(c("1", "2"), c("x", "y")),
                        list(c(TRUE, FALSE), c(FALSE, FALSE)))
  expect_length(v, 2)
  d <- data.frame(label = c("A", "B"), value_text = c("1", "x"), worse = c(TRUE, FALSE))
  expect_equal(as.character(risk_tooltips_vec("T", value_text = "v", footer = "f",
                                              drivers_html = v[1])[[1]]),
               as.character(risk_tooltip_html("T", value_text = "v", footer = "f", drivers = d)))
  expect_true(grepl("#ff6b6b", v[1], fixed = TRUE))
  expect_false(grepl("#ff6b6b", v[2], fixed = TRUE))
})

test_that("side panel shows migration chart and population change", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  for (s in c("2020–21 to 2024–25", "Population change 2020–25: +4.0%", "Census PEP V2025",
              "Net domestic migration: +8.2 per 1,000 (2024–25)", "#998ec3"))
    expect_true(grepl(s, h, fixed = TRUE), info = s)
  h2 <- as.character(risk_side_panel(l[l$GEOID == "09110", ], "county", l))
  expect_true(grepl("#f1a340", h2, fixed = TRUE))
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), length(RISK_LAYERS) + 1)
})

test_that("side panel without PEP has no migration blocks", {
  l <- build_county_layers(fx_county_base(), fx_momentum())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_false(grepl("Population change", h, fixed = TRUE))
  expect_false(grepl("Net domestic migration:", h, fixed = TRUE))
})

test_that("CBSA migration is population-weighted", {
  cl <- data.frame(cbsa_geoid = c("X", "X", "Y", NA),
                   mig_dom_n = c(100, -50, NA, 5), pop_avg = c(10000, 40000, NA, 100))
  m <- cbsa_migration(cl)
  expect_equal(m$mig_dom_rate[m$cbsa_geoid == "X"], 1.0)
  expect_true(is.na(m$mig_dom_rate[m$cbsa_geoid == "Y"]))
  expect_false(NA %in% m$cbsa_geoid)
})

test_that("cumulative layer labels its latest-year drivers with the year", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())
  cum <- mig_labels(build_risk_county_map(l, "mig_dom_cum"))
  for (s in c("Domestic (2024–25)", "International (2024–25)", "Natural change (2024–25)"))
    expect_true(all(grepl(s, cum, fixed = TRUE)), info = s)
  one <- mig_labels(build_risk_county_map(l, "mig_dom"))
  expect_false(any(grepl("(2024–25)</span>", one, fixed = TRUE)))
})

test_that("migration layers carry their own loading text", {
  for (id in c("mig_dom", "mig_intl", "mig_net", "mig_dom_cum"))
    expect_equal(RISK_LAYERS[[id]]$loading_text, "loading Census population estimates…")
  l <- build_county_layers(fx_county_base(), fx_momentum(), loading = "migration")
  title <- Filter(function(cl) cl$method == "addLegend",
                  build_risk_county_map(l, "mig_net")$x$calls)[[1]]$args[[1]]$title
  expect_true(grepl("loading Census population estimates", title, fixed = TRUE))
})

test_that("risk_drivers_vec treats a NULL (missing column) value as all-NA", {
  v <- risk_drivers_vec(c("A", "B"), list(c("1", "2"), NULL), list(c(TRUE, FALSE), FALSE))
  expect_length(v, 2)
  expect_true(all(grepl("<span>A</span>", v, fixed = TRUE)))
  expect_false(any(grepl("<span>B</span>", v, fixed = TRUE)))
  expect_equal(risk_drivers_vec(c("A", "B"), list(NULL, c(NA_character_, NA)),
                                list(FALSE, FALSE), n = 2), c("", ""))
})
