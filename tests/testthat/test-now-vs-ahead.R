map_calls_nva <- function(m, method) Filter(function(cl) cl$method == method, m$x$calls)

nva_layers <- function(...) build_county_layers(fx_county_base(), fx_momentum(),
                                                 migration = fx_migration(),
                                                 stress_ahead = fx_stress_ahead(), ...)

# pct_momentum_pp: high = worse. pct_stress_ahead_pp: high = better job growth (low = worse).
nva_frame <- function() data.frame(
  GEOID = c("a", "b", "c", "d", "e", "f"),
  pct_momentum_pp     = c(10, 90, 10, 90, NA, 50),
  pct_stress_ahead_pp = c(90, 90, 10, 10, 10, 10))

test_that("quadrants split at the worst third on each measure", {
  q <- county_now_vs_ahead(nva_frame())
  # a: fine now, fine ahead; b: bad now only; c: bad ahead only; d: both; e: missing
  expect_equal(q$now_vs_ahead, c(1, 2, 3, 4, NA, 3))
  expect_equal(q$quad_ahead_pct, c(10, 10, 90, 90, 90, 90))   # flipped: high = worse
  expect_equal(QUAD_LABELS[q$now_vs_ahead[1:4]],
               c("Healthy", "Recovering", "Worsening", "Bad now and ahead"))
})

test_that("a missing stress layer leaves the quadrant NA, not Healthy", {
  d <- nva_frame(); d$pct_stress_ahead_pp <- NA_real_
  expect_true(all(is.na(county_now_vs_ahead(d)$now_vs_ahead)))
  expect_true(all(is.na(county_now_vs_ahead(nva_frame()[0, ])$now_vs_ahead)))
})

test_that("the layer is registered, categorical and county-only", {
  cfg <- RISK_LAYERS$now_vs_ahead
  expect_equal(cfg$lens, "context")
  expect_false("tract" %in% cfg$levels)
  expect_length(cfg$colors, length(cfg$bins) - 1)
  expect_length(cfg$bin_labels, length(cfg$bins) - 1)
  expect_true("now_vs_ahead" %in% unname(risk_lens_choices("context")))
  expect_equal(.risk_fmt(c(1, 3, NA), "quadrant"), c("Healthy", "Worsening", NA))
})

test_that("build_county_layers adds the layer with a status that follows both inputs", {
  cl <- nva_layers()
  expect_true(all(c("now_vs_ahead", "quad_now_pct", "pct_now_vs_ahead") %in% names(cl)))
  expect_true(RISK_LAYERS$now_vs_ahead$col %in% names(cl))
  st <- attr(cl, "layer_status"); ao <- attr(cl, "asof")
  expect_true("now_vs_ahead" %in% names(st))
  expect_true("now_vs_ahead" %in% names(ao))
  # Without stress ahead the layer is unavailable and every county is NA.
  noahead <- build_county_layers(fx_county_base(), fx_momentum())
  expect_equal(attr(noahead, "layer_status")[["now_vs_ahead"]], "unavailable")
  expect_true(all(is.na(noahead$now_vs_ahead)))
  loading <- build_county_layers(fx_county_base(), fx_momentum(), loading = "stress_ahead")
  expect_equal(attr(loading, "layer_status")[["now_vs_ahead"]], "loading")
})

test_that("the map draws the quadrant colours, worst first in the legend", {
  cl <- nva_layers()
  cl$now_vs_ahead <- c(4, 2)[seq_len(nrow(cl))]
  m <- build_risk_county_map(cl, "now_vs_ahead")
  poly <- map_calls_nva(m, "addPolygons")
  expect_length(poly, 1)
  fills <- unlist(poly[[1]]$args[[4]]$fillColor)
  expect_equal(unname(toupper(fills)), toupper(RISK_LAYERS$now_vs_ahead$colors[c(4, 2)[seq_along(fills)]]))
  lg <- map_calls_nva(m, "addLegend")[[1]]$args[[1]]
  expect_match(as.character(unlist(lg$labels))[1], "Bad now and ahead")
})

test_that("hover shows the quadrant and where the county sits on each measure", {
  d <- county_now_vs_ahead(nva_frame())
  d$name <- paste("County", d$GEOID)
  d$momentum_pp <- 0.6; d$stress_ahead_pp <- -0.2
  cfg <- RISK_LAYERS$now_vs_ahead
  for (detail in c(TRUE, FALSE)) {
    tip <- as.character(.county_tooltips(d, "now_vs_ahead", cfg, "Sep 2026", "ok", detail)[[4]])
    expect_match(tip, "Bad now and ahead", fixed = TRUE)
    expect_match(tip, "Now: worst 10% · Ahead: worst 10%", fixed = TRUE)
  }
  full <- as.character(.county_tooltips(d, "now_vs_ahead", cfg, "Sep 2026", "ok", TRUE)[[4]])
  expect_match(full, "+0.6 pp", fixed = TRUE)
  expect_match(full, "-0.20 pp", fixed = TRUE)
  expect_true(is.na(.quad_rank_text(d)[5]))     # one measure missing: no pill text
})

test_that("the table names the quadrant, worst first, without percentile columns", {
  cl <- nva_layers()
  cl$now_vs_ahead <- c(2, 4)[seq_len(nrow(cl))]
  t <- risk_county_table(cl, "now_vs_ahead")
  expect_equal(t$Value[1], "Bad now and ahead")
  expect_false(any(c("US pct", "State pct") %in% names(t)))
})

test_that("the side panel row and the county AI context carry the quadrant", {
  cl <- nva_layers()
  cl$now_vs_ahead <- 3
  html <- as.character(risk_side_panel(cl[1, ], "county", cl))
  expect_match(html, "Now vs ahead", fixed = TRUE)
  expect_match(html, "Worsening", fixed = TRUE)
  ctx <- county_risk_context(cl, sf::st_drop_geometry(cl)$GEOID[1])
  expect_equal(ctx$now_vs_ahead_quadrant, "Worsening")
})

# ── Why a county is in its quadrant ──────────────────────────────────────────
why_frame <- function() {
  d <- data.frame(
    name = paste("County", 1:4),
    now_vs_ahead = c(1, 2, 3, 4),
    momentum_pp = c(-0.2, 0.9, 0.0, 0.6), rate_now = c(3.1, 5.2, 4.0, 6.0),
    stress_ahead_pp = c(0.3, 0.2, -0.4, -0.5),
    quad_now_pct = c(20, 80, 40, 95), quad_ahead_pct = c(10, 30, 85, 95),
    stringsAsFactors = FALSE)
  d$sa_drv1_label <- c(NA, NA, "Manufacturing", "Retail trade")
  d$sa_drv1_pp <- c(NA, NA, -0.31, -0.22)
  d$sa_drv2_label <- c(NA, NA, "Retail trade", NA)
  d$sa_drv2_pp <- c(NA, NA, -0.12, NA)
  d$sa_lift1_label <- c("Education & health", "Information", NA, NA)
  d$sa_lift1_pp <- c(0.2, 0.1, NA, NA)
  d
}

test_that("the reason names the numbers and the sectors behind each half", {
  w <- .quad_why(why_frame())
  expect_match(w$now[2], "+0.9 pp vs a year ago, now 5.2% (worst 20% of US counties)", fixed = TRUE)
  expect_match(w$ahead[3], "-0.40 pp", fixed = TRUE)
  expect_match(w$ahead[3], "dragged down by Manufacturing (−0.31), Retail trade (−0.12)", fixed = TRUE)
  expect_match(w$ahead[1], "lifted by Education & health (+0.20)", fixed = TRUE)
  expect_match(w$ahead[1], "better half of US counties", fixed = TRUE)
  expect_match(w$headline[3], "stress may build", fixed = TRUE)
  expect_match(w$headline[2], "may be recovering", fixed = TRUE)
  expect_equal(w$short[3], "Unemp. +0.0 pp · drag: Manufacturing, Retail trade")
  expect_equal(w$short[2], "Unemp. +0.9 pp · lift: Information")
})

test_that("the reason is NA, never 'NA', when the quadrant or inputs are missing", {
  d <- why_frame(); d$now_vs_ahead[1] <- NA; d$momentum_pp[2] <- NA
  w <- .quad_why(d)
  expect_true(is.na(w$headline[1]) && is.na(w$short[1]))
  expect_true(is.na(w$now[2]))
  expect_false(any(grepl("NA", unlist(w[3:4, ]), fixed = TRUE)))
  expect_equal(nrow(.quad_why(why_frame()[0, ])), 0)
})

test_that("hover carries one reason line, the full block the headline", {
  d <- why_frame(); cfg <- RISK_LAYERS$now_vs_ahead
  compact <- as.character(.county_tooltips(d, "now_vs_ahead", cfg, "Sep 2026", "ok", FALSE)[[3]])
  expect_match(compact, "drag: Manufacturing, Retail trade", fixed = TRUE)
  full <- as.character(.county_tooltips(d, "now_vs_ahead", cfg, "Sep 2026", "ok", TRUE)[[3]])
  expect_match(full, "stress may build", fixed = TRUE)
})

test_that("the side panel explains the quadrant and the AI context gets the reason", {
  cl <- nva_layers()
  cl$now_vs_ahead <- 3
  cl$sa_drv1_label <- "Manufacturing"; cl$sa_drv1_pp <- -0.31
  html <- as.character(risk_side_panel(cl[1, ], "county", cl))
  expect_match(html, "Why: Worsening", fixed = TRUE)
  expect_match(html, "Manufacturing", fixed = TRUE)
  ctx <- county_risk_context(cl, sf::st_drop_geometry(cl)$GEOID[1])
  expect_match(ctx$now_vs_ahead_reason$ahead, "Manufacturing", fixed = TRUE)
  expect_false(grepl('"NA"', as.character(jsonlite::toJSON(ctx, auto_unbox = TRUE)), fixed = TRUE))
  # Tracts do not get the county block.
  expect_null(.quad_panel(data.frame(GEOID = "x", now_vs_ahead = NA_real_)))
})
