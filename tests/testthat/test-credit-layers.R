# Credit lens and Tax base layer: build_county_layers(credit =), registry, tooltips,
# side panel.

# Walker GA (metro 16860), Hamilton TN (same metro, no county row), Capitol CT (no CBSA),
# Appling GA (no CBSA).
cr_base <- function() {
  b <- fx_county_base()
  b <- b[c(1, 1, 2, 2), ]
  b$GEOID <- c("13295", "47065", "09110", "13001")
  b$name <- c("Walker County, GA", "Hamilton County, TN", "Capitol Planning Region, CT",
              "Appling County, GA")
  b$state_fips <- c("13", "47", "09", "13"); b$state_abbr <- c("GA", "TN", "CT", "GA")
  b$state_name <- c("Georgia", "Tennessee", "Connecticut", "Georgia")
  b$cbsa_geoid <- c("16860", "16860", NA, NA); b$cbsa_name <- c("Chattanooga", "Chattanooga", NA, NA)
  b$cbsa_type <- c("metro", "metro", NA, NA)
  b$poverty_rate <- c(14, 12, 10, 20); b$rent_burden_rate <- c(48, 45, 52, 40)
  b$geometry <- sf::st_sfc(sq(-85, 34), sq(-85, 35.2), sq(-73, 41), sq(-82.5, 31.7), crs = 4326)
  b
}

# Momentum for every county but Capitol: its job pressure is missing.
cr_momentum <- function() {
  tibble::tibble(fips = c("13295", "47065", "13001"), ref_month = as.Date("2026-09-01"),
                 rate_now = c(3.2, 3.9, 4.4), rate_yr_ago = c(3.4, 3.5, 3.8),
                 momentum_pp = c(-0.2, 0.4, 0.6), flag = "None")
}

cr_migration <- function() {
  m <- tibble::tibble(GEOID = c("13295", "47065", "09110", "13001"),
                      mig_dom_rate = c(8.2, 1.5, -0.11, -4), mig_intl_rate = c(1, 0.5, 4, 0),
                      mig_net_rate = c(9.2, 2, 3.9, -4), nat_chg_rate = c(-1, 0, 0.2, -2),
                      mig_dom_cum = c(40, 8, -12, -20), pop_chg_pct = c(4, 2, 1.2, -3),
                      mig_dom_n = 1, pop_avg = 1)
  attr(m, "vintage") <- 2025
  m
}

cr_tbl <- function(ids, old, new) {
  tibble::tibble(id = rep(ids, 2), date = rep(as.Date(c("2025-03-01", "2026-03-01")),
                                              each = length(ids)), pct = c(old, new))
}

# Raw fetch results: Walker has a county CFPB row and a metro-division HPI; Hamilton falls
# back to the metro; Capitol and Appling to the state non-metro row.
cr_credit <- function(cfpb = TRUE, urban = TRUE, hpi = TRUE, msad = TRUE) {
  list(
    cfpb = if (cfpb) list(
      county90 = cr_tbl("13295", 1.0, 1.4), county3089 = cr_tbl("13295", 1.8, 1.9),
      metro90 = cr_tbl(c("16860", "09-non", "13-non"), c(1, 2, 3), c(2.6, 2.5, 3.2)),
      metro3089 = cr_tbl(c("16860", "09-non", "13-non"), c(2, 2, 2), c(2.3, 2.15, 2.9)),
      thru = "2026-03"),
    urban = if (urban) {
      u <- tibble::tibble(GEOID = c("13295", "47065", "13001"), card_del = c(10, 12, 20),
                          auto_del = c(3, 4, 8))
      attr(u, "asof") <- "Oct 2026"
      u
    },
    hpi = if (hpi) list(
      metro = tibble::tibble(code = c("16860", "99999"), hpi_yoy = c(3.1, 6.2),
                             hpi_q = c("2026 Q1", "2026 Q1")),
      state = tibble::tibble(code = c("GA", "TN", "CT"), hpi_yoy = c(2, 1, -1.5),
                             hpi_q = c("2026 Q1", "2025 Q4", "2026 Q1"))),
    msad = if (msad) tibble::tibble(GEOID = "13295", msad_code = "99999"))
}

cr_layers <- function(credit = cr_credit(), loading = character(0), migration = cr_migration(),
                      ...) {
  build_county_layers(cr_base(), cr_momentum(), migration = migration, credit = credit,
                      loading = loading, ...)
}

# Default layers have no Stress ahead (unavailable); these carry one for the three counties
# with momentum, ranked so job pressure keeps its Stress-now-only values (0, .5, 1).
cr_sa <- function() {
  sa <- fx_stress_ahead()
  out <- rbind(sa[1, ], sa[1, ], sa[1, ])
  out$GEOID <- c("13295", "47065", "13001")
  out$stress_ahead_pp <- c(3, 2, 1)
  for (a in c("ref_month", "temphelp_m_pct", "acs_year", "partial", "weights"))
    attr(out, a) <- attr(sa, a)
  out
}

cr_layers_sa <- function(...) cr_layers(..., stress_ahead = cr_sa())

cr_ids <- c("credit_score", "credit_mortgage", "credit_consumer", "credit_jobs",
            "credit_collateral")

cr_labels <- function(l, id) {
  m <- build_risk_county_map(l, id)
  pol <- Filter(function(cl) cl$method == "addPolygons", m$x$calls)[[1]]
  setNames(vapply(pol$args[[7]], as.character, ""), l$GEOID)
}

cr_legend_title <- function(l, id) {
  m <- build_risk_county_map(l, id)
  Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]$title
}

cr_panel <- function(l, geoid = "13295") {
  as.character(risk_side_panel(l[l$GEOID == geoid, ], "county", l))
}

test_that("credit status loading then ok; unavailable when all sources fail", {
  st <- function(l, id) attr(l, "layer_status")[[id]]
  ld <- cr_layers_sa(NULL, loading = "credit")
  for (id in cr_ids) expect_equal(st(ld, id), "loading", info = id)
  ok <- cr_layers_sa()
  for (id in cr_ids) expect_equal(st(ok, id), "ok", info = id)
  none <- cr_layers_sa(cr_credit(FALSE, FALSE, FALSE, FALSE))
  for (id in cr_ids) expect_equal(st(none, id), "unavailable", info = id)
  expect_equal(st(cr_layers_sa(NULL), "credit_score"), "unavailable")
  # Values match the status: nothing is shaded for an unavailable layer.
  expect_true(all(is.na(none$credit_score)) && all(is.na(none$cr_jobs)))
  # A component whose own source failed is unavailable; the rest still work.
  nu <- cr_layers_sa(cr_credit(urban = FALSE))
  expect_equal(st(nu, "credit_consumer"), "unavailable")
  expect_equal(st(nu, "credit_score"), "ok")
  expect_equal(st(nu, "credit_mortgage"), "ok")
})

test_that("credit lens layers are county-only and in the credit lens", {
  expect_equal(unname(risk_lens_choices("credit")), cr_ids)
  for (id in cr_ids) {
    expect_equal(RISK_LAYERS[[id]]$lens, "credit", info = id)
    expect_equal(RISK_LAYERS[[id]]$levels, "county", info = id)
  }
  expect_equal(RISK_LENSES, c("Assistance" = "assistance", "Credit" = "credit",
                              "Migration" = "migration", "Context" = "context"))
  expect_equal(RISK_LAYERS$credit_score$bins, c(0, 20, 35, 50, 65, 80, 101))
  expect_equal(RISK_LAYERS$credit_collateral$bins, c(-50, -5, -2, 0, 2, 5, 50))
  cg <- RISK_LAYERS$credit_collateral
  expect_equal(c(cg$pal, cg$rev, cg$higher_is_worse, cg$digits), c("RdBu", FALSE, FALSE, 1))
  expect_equal(vapply(RISK_LAYERS[cr_ids], `[[`, "", "col"),
               c(credit_score = "credit_score", credit_mortgage = "dq90",
                 credit_consumer = "consumer_del", credit_jobs = "cr_jobs",
                 credit_collateral = "hpi_yoy"))
  tb <- RISK_LAYERS$tax_base
  expect_equal(c(tb$lens, tb$levels, tb$col), c("context", "county", "tax_base"))
  expect_equal(risk_lens_choices("context")[["Tax base pressure (proxy — not a bond default model)"]],
               "tax_base")
  # Drilled into a county, a credit layer still draws tracts (coloured by the fallback layer).
  tr <- sf::st_sf(GEOID = "13295020300", county_geoid = "13295", tract_name = "T",
                  geometry = sf::st_sfc(sq(-85.2, 34.5, 0.1), crs = 4326))
  m <- add_tract_layer(leaflet::leaflet(), tr, "credit_score", cr_layers())
  expect_length(Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], "tracts"),
                       m$x$calls), 1)
  # Every layer still has its percentile column.
  cols <- vapply(RISK_LAYERS, `[[`, "", "col")
  expect_true(all(paste0("pct_", cols) %in% names(cr_layers())))
  expect_true(all(paste0("pct_", cols) %in% names(build_county_layers(cr_base(), cr_momentum()))))
})

test_that("credit components are joined to the layers", {
  l <- cr_layers()
  expect_equal(l$dq90, c(1.4, 2.6, 2.5, 3.2))
  expect_equal(l$cfpb_source, c("county", "metro", "state non-metro", "state non-metro"))
  expect_equal(l$consumer_del, c(6.5, 8, NA, 14))
  expect_equal(l$hpi_yoy, c(6.2, 3.1, -1.5, 2))
  expect_equal(l$hpi_source, c("metro division", "metro", "state", "state"))
  expect_equal(l$hpi_q[2:3], c("2026 Q1", "2026 Q1"))
  expect_equal(l$credit_missing[3], "consumer, job pressure")
  expect_true(is.na(l$credit_score[3]))
  expect_false(any(is.na(l$credit_score[-3])))
  expect_equal(l$cr_jobs, pct_rank(l$job_pressure))   # re-ranked: a true percentile
  expect_equal(range(l$cr_jobs, na.rm = TRUE), c(0, 100))
})

test_that("credit as-of labels follow the spec and never call the Urban release a data month", {
  a <- attr(cr_layers(), "asof")
  expect_equal(a[["credit_score"]], paste0(
    "CFPB Mar 2026 · Urban Institute (released Oct 2026) · FHFA 2026 Q1 · BLS Sep 2026"))
  expect_equal(a[["credit_mortgage"]], "CFPB thru Mar 2026")
  expect_equal(a[["credit_consumer"]], "released Oct 2026")
  expect_equal(RISK_LAYERS$credit_consumer$source, "Urban Institute (Debt in America)")
  expect_equal(a[["credit_collateral"]], "FHFA 2026 Q1")
  expect_equal(a[["credit_jobs"]], "Sep 2026")
  expect_equal(a[["tax_base"]], "PEP V2025 · FHFA 2026 Q1")
  lg <- cr_legend_title(cr_layers(), "credit_consumer")
  expect_true(grepl("released Oct 2026", lg, fixed = TRUE))
  expect_false(grepl("Urban Institute (Debt in America), Oct 2026", lg, fixed = TRUE))
  # Sources missing from the label when unavailable.
  a2 <- attr(cr_layers(cr_credit(urban = FALSE)), "asof")
  expect_equal(a2[["credit_score"]],
               "CFPB Mar 2026 · FHFA 2026 Q1 · BLS Sep 2026 (3 of 4 components: Urban Institute unavailable)")
})

test_that("credit_jobs as-of joins the Stress now and Stress ahead as-ofs", {
  l <- cr_layers(stress_ahead = fx_stress_ahead())
  a <- attr(l, "asof")
  expect_equal(a[["credit_jobs"]], paste(a[["stress_now"]], a[["stress_ahead"]], sep = " · "))
})

test_that("score tooltip names the CFPB fallback and the missing components", {
  l <- cr_layers()
  tip <- cr_labels(l, "credit_score")
  # Appling: state non-metro estimate and its three strongest drivers
  expect_true(grepl("(state non-metro est.)", tip[["13001"]], fixed = TRUE))
  expect_true(grepl("Driven by", tip[["13001"]], fixed = TRUE))
  # Hamilton: metro estimate
  expect_true(grepl("(metro est.)", tip[["47065"]], fixed = TRUE))
  # Walker has its own county row: no estimate note on the mortgage line
  expect_false(grepl("est.)", sub("Home prices.*", "", tip[["13295"]]), fixed = TRUE))
  # Capitol: insufficient data, missing components named, no score
  expect_true(grepl("Insufficient data", tip[["09110"]], fixed = TRUE))
  expect_true(grepl("Missing: consumer, job pressure", tip[["09110"]], fixed = TRUE))
  expect_false(grepl("Missing:", tip[["13295"]], fixed = TRUE))
})

test_that("score tooltip lists the top 3 components in natural units, red from the 50th pct", {
  l <- cr_layers()
  d <- sf::st_drop_geometry(l)
  tip <- cr_labels(l, "credit_score")[["13001"]]
  rows <- regmatches(tip, gregexpr("<span>[^<]*</span><span style=\"font-weight:600;color:[^\"]*\">[^<]*</span>", tip))[[1]]
  expect_length(rows, 3)
  # Appling: card/auto 14.0% and 3.2% mortgage, +2.0% prices
  i <- which(d$GEOID == "13001")
  pct <- c(d$cr_mortgage[i], d$cr_consumer[i], d$cr_jobs[i], d$cr_collateral[i])
  top <- order(-pct)[1:3]
  lab <- c("Mortgages 90+ late", "Card &amp; auto delinquent", "Job pressure", "Home prices YoY")
  for (k in seq_along(top)) expect_true(grepl(lab[top[k]], rows[k], fixed = TRUE), info = k)
  colour <- function(r) sub('.*color:([^";]*)[";].*', "\\1", r)
  expect_equal(unname(vapply(rows, colour, "")),
               ifelse(pct[top] >= 50, .RISK_COL$worse, .RISK_COL$text))
  # natural units
  expect_true(grepl("3.2% (state non-metro est.)", tip, fixed = TRUE))
  if (2 %in% top) expect_true(grepl("14.0%", tip, fixed = TRUE))
  if (3 %in% top) expect_true(grepl(" pct", tip, fixed = TRUE))
  if (4 %in% top) expect_true(grepl("+2.0%", tip, fixed = TRUE))
})

test_that("a score with 3 of 4 components says which one is missing", {
  l <- cr_layers(cr_credit(urban = FALSE))
  tip <- cr_labels(l, "credit_score")[["13295"]]
  expect_true(grepl("Based on 3 of 4 components (missing: consumer)", tip, fixed = TRUE))
})

test_that("component tooltips show their own source and as-of per county", {
  l <- cr_layers()
  mort <- cr_labels(l, "credit_mortgage")
  expect_true(grepl("CFPB Mar 2026 · county data", mort[["13295"]], fixed = TRUE))
  expect_true(grepl("CFPB Mar 2026 · metro estimate", mort[["47065"]], fixed = TRUE))
  expect_true(grepl("CFPB Mar 2026 · state non-metro estimate", mort[["13001"]], fixed = TRUE))
  col <- cr_labels(l, "credit_collateral")
  expect_true(grepl("FHFA 2026 Q1 · metro division", col[["13295"]], fixed = TRUE))
  expect_true(grepl("FHFA 2026 Q1 · metro<", col[["47065"]], fixed = TRUE))
  expect_true(grepl("FHFA 2026 Q1 · state<", col[["13001"]], fixed = TRUE))
  expect_true(grepl("+6.2%", col[["13295"]], fixed = TRUE))
  con <- cr_labels(l, "credit_consumer")
  expect_true(grepl("Card delinquent", con[["13295"]], fixed = TRUE))
  expect_true(grepl("Insufficient data", con[["09110"]], fixed = TRUE))
  expect_true(grepl("Urban Institute", con[["13295"]], fixed = TRUE))
  jobs <- cr_labels(l, "credit_jobs")
  expect_true(grepl("Job pressure:", jobs[["13295"]], fixed = TRUE))
})

test_that("the job pressure explanation quotes the 0..100 index, not its re-ranked percentile", {
  l <- cr_layers()
  l$job_pressure[l$GEOID == "47065"] <- 0.9
  expect_true(grepl("Job pressure: 90/100", cr_labels(l, "credit_jobs")[["47065"]], fixed = TRUE))
  expect_true(grepl("Job pressure</b>\\s+90/100", cr_panel(l, "47065")))
  expect_true(grepl(sprintf("risk percentile %.0f", l$cr_jobs[l$GEOID == "47065"]),
                    cr_panel(l, "47065"), fixed = TRUE))
})

test_that("collateral pill is red for falling prices, not for rising ones", {
  cfg <- RISK_LAYERS$credit_collateral
  expect_equal(.rank_bad(c(90, 10), cfg, c(3.2, -0.6)), c(FALSE, TRUE))
  expect_equal(.risk_fmt(c(3.24, -0.6, 0.02, NA), "pct_signed"), c("+3.2%", "-0.6%", "+0.0%", NA))
  expect_equal(.bin_labels(cfg$bins, cfg$fmt, 1)[2:3], c("−5.0 to −2.0", "−2.0 to 0"))
})

test_that("tax base layer is labelled proxy everywhere", {
  l <- cr_layers()
  cfg <- RISK_LAYERS$tax_base
  expect_true(grepl("proxy", cfg$label, fixed = TRUE))
  lg <- cr_legend_title(l, "tax_base")
  expect_true(grepl("proxy — not a bond default model", lg, fixed = TRUE))
  tip <- cr_labels(l, "tax_base")
  for (g in l$GEOID) {
    expect_true(grepl("proxy — not a bond default model", tip[[g]], fixed = TRUE), info = g)
  }
  expect_true(grepl("Net domestic migration", tip[["13295"]], fixed = TRUE))
  expect_true(grepl("Population change 2020–25", tip[["13295"]], fixed = TRUE))
  expect_true(grepl("Home prices YoY", tip[["13295"]], fixed = TRUE))
  # legend and footer also say proxy while loading, unavailable or without HPI
  expect_true(grepl("proxy — not a bond default model", cr_legend_title(cr_layers(NULL, "credit"), "tax_base"),
                    fixed = TRUE))
})

test_that("tax base home price driver says when the value is a state estimate", {
  tip <- cr_labels(cr_layers(), "tax_base")
  hp <- function(t) regmatches(t, regexpr("Home prices YoY</span><span[^>]*>[^<]*", t))
  expect_true(grepl("+2.0% (state est.)", hp(tip[["13001"]]), fixed = TRUE))   # Appling: state
  expect_false(grepl("est.", hp(tip[["13295"]]), fixed = TRUE))                # Walker: division
  expect_false(grepl("est.", hp(tip[["47065"]]), fixed = TRUE))                # Hamilton: metro
})

test_that("tax base follows its inputs", {
  st <- function(l) attr(l, "layer_status")[["tax_base"]]
  expect_equal(st(cr_layers()), "ok")
  # PEP present without credit: ok, HPI simply missing
  no_credit <- cr_layers(NULL)
  expect_equal(st(no_credit), "ok")
  expect_false(anyNA(no_credit$tax_base))
  expect_equal(attr(no_credit, "asof")[["tax_base"]], "PEP V2025 (home prices unavailable)")
  # Credit sources all failed: same
  expect_equal(st(cr_layers(cr_credit(FALSE, FALSE, FALSE, FALSE))), "ok")
  # While credit loads the layer is loading and shows no half-computed values
  ld <- cr_layers(NULL, loading = "credit")
  expect_equal(st(ld), "loading")
  expect_true(all(is.na(ld$tax_base)))
  # Without PEP and credit there is nothing to score
  expect_equal(st(cr_layers(NULL, migration = NULL)), "unavailable")
  expect_equal(st(cr_layers(migration = NULL)), "unavailable")
  expect_equal(st(cr_layers(NULL, loading = "migration")), "loading")
  # Tax base equals the proxy over the layer columns
  l <- cr_layers()
  expect_equal(l$tax_base, tax_base_proxy(l))
})

test_that("a stale source marks only the layers that rest on it", {
  st <- function(l) attr(l, "layer_status")
  cr <- cr_credit(); attr(cr$hpi, "stale") <- TRUE
  l <- cr_layers_sa(cr)
  for (id in c("credit_score", "credit_collateral", "tax_base")) expect_equal(st(l)[[id]], "stale", info = id)
  for (id in c("credit_mortgage", "credit_consumer", "credit_jobs"))
    expect_equal(st(l)[[id]], "ok", info = id)          # FHFA staleness is not CFPB's
  expect_true(grepl("(stale)", cr_legend_title(l, "credit_score"), fixed = TRUE))
  # asof is kept so the legend still says what is stale
  expect_false(is.na(attr(l, "asof")[["credit_score"]]))
  cr <- cr_credit(); attr(cr$cfpb, "stale") <- TRUE
  l <- cr_layers_sa(cr)
  expect_equal(st(l)[["credit_mortgage"]], "stale")
  expect_equal(st(l)[["credit_score"]], "stale")
  expect_equal(st(l)[["credit_collateral"]], "ok")
  expect_equal(st(l)[["tax_base"]], "ok")
  cr <- cr_credit(); attr(cr$urban, "stale") <- TRUE
  expect_equal(st(cr_layers_sa(cr))[["credit_consumer"]], "stale")
  cr <- cr_credit(); attr(cr$msad, "stale") <- TRUE
  expect_equal(st(cr_layers_sa(cr))[["credit_collateral"]], "stale")
  # PEP staleness reaches the tax base only
  mig <- cr_migration(); attr(mig, "stale") <- TRUE
  l <- cr_layers_sa(migration = mig)
  expect_equal(st(l)[["tax_base"]], "stale")
  expect_equal(st(l)[["credit_score"]], "ok")
  # A FALSE stale flag is ok
  cr2 <- cr_credit(); attr(cr2$cfpb, "stale") <- FALSE
  expect_equal(st(cr_layers_sa(cr2))[["credit_score"]], "ok")
})

test_that("job pressure inputs carry their staleness into the credit and assistance layers", {
  st <- function(l) attr(l, "layer_status")
  l <- cr_layers(laus_stale = TRUE)
  expect_equal(st(l)[["stress_now"]], "stale")
  expect_equal(st(l)[["credit_jobs"]], "stale")
  expect_equal(st(l)[["credit_score"]], "stale")
  for (id in c("credit_mortgage", "credit_consumer", "credit_collateral", "tax_base"))
    expect_equal(st(l)[[id]], "ok", info = id)
  l <- cr_layers(laus_partial = TRUE)
  expect_equal(st(l)[["credit_jobs"]], "partial")
  expect_equal(st(l)[["credit_score"]], "partial")
  expect_true(grepl("(partial: some job inputs", cr_legend_title(l, "credit_jobs"), fixed = TRUE))
  sa <- fx_stress_ahead(); attr(sa, "partial") <- TRUE
  l <- cr_layers(stress_ahead = sa)
  expect_equal(st(l)[["stress_ahead"]], "partial")
  expect_equal(st(l)[["credit_jobs"]], "partial")
  expect_equal(st(l)[["credit_score"]], "partial")
  # stale beats partial
  l <- cr_layers(stress_ahead = sa, laus_stale = TRUE)
  expect_equal(st(l)[["credit_jobs"]], "stale")
  # the assistance layers rest on job pressure too
  a <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead(),
                           laus_stale = TRUE)
  expect_equal(st(a)[["assist_score"]], "stale")
  a <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead(),
                           laus_partial = TRUE)
  expect_equal(st(a)[["assist_score"]], "partial")
})

test_that("one missing job input (BLS quota) makes the job-fed layers partial, both missing unavailable", {
  st <- function(l) attr(l, "layer_status")
  build <- function(momentum = cr_momentum(), sa = fx_stress_ahead(), ...)
    build_county_layers(cr_base(), momentum, migration = cr_migration(), credit = cr_credit(),
                        stress_ahead = sa, ...)
  # Stress now unavailable, Stress ahead fine
  s <- st(build(momentum = NULL))
  expect_equal(s[["stress_now"]], "unavailable")
  for (id in c("credit_jobs", "credit_score", "assist_score", "assist_exposure"))
    expect_equal(s[[id]], "partial", info = id)
  for (id in c("credit_mortgage", "credit_consumer", "credit_collateral"))
    expect_equal(s[[id]], "ok", info = id)
  expect_true(grepl("(partial: some job inputs", cr_legend_title(build(momentum = NULL), "credit_jobs"),
                    fixed = TRUE))
  # Stress now still loading counts the same
  s <- st(build(momentum = NULL, loading = "stress_now"))
  expect_equal(s[["credit_jobs"]], "partial")
  expect_equal(s[["credit_score"]], "partial")
  # Stress ahead unavailable, Stress now fine
  s <- st(build(sa = NULL))
  expect_equal(s[["stress_ahead"]], "unavailable")
  expect_equal(s[["credit_jobs"]], "partial")
  expect_equal(s[["credit_score"]], "partial")
  # Both gone: no job pressure at all; the score keeps its normal 3-of-4 logic
  l <- build(momentum = NULL, sa = NULL)
  s <- st(l)
  expect_equal(s[["credit_jobs"]], "unavailable")
  expect_equal(s[["credit_score"]], "ok")
  expect_true(all(is.na(l$cr_jobs)) && !is.na(l$credit_score[1]))
  # stale still beats partial
  sa <- fx_stress_ahead(); attr(sa, "stale") <- TRUE
  expect_equal(st(build(momentum = NULL, sa = sa))[["credit_jobs"]], "stale")
})

test_that("the score as-of names the BLS and CES months and any whole source that failed", {
  a <- attr(cr_layers(), "asof")
  expect_equal(a[["credit_score"]], paste0(
    "CFPB Mar 2026 · Urban Institute (released Oct 2026) · FHFA 2026 Q1 · BLS Sep 2026"))
  a <- attr(cr_layers(stress_ahead = fx_stress_ahead()), "asof")
  expect_equal(a[["credit_score"]], paste0(
    "CFPB Mar 2026 · Urban Institute (released Oct 2026) · FHFA 2026 Q1 · BLS Sep 2026 · ",
    a[["stress_ahead"]]))
  expect_false(grepl("BLS/CES", a[["credit_score"]], fixed = TRUE))
  # a whole source missing is said in the legend text, not only in the tooltips
  a1 <- attr(cr_layers(cr_credit(urban = FALSE)), "asof")[["credit_score"]]
  expect_true(grepl("(3 of 4 components: Urban Institute unavailable)", a1, fixed = TRUE))
  a2 <- attr(cr_layers(cr_credit(cfpb = FALSE)), "asof")[["credit_score"]]
  expect_true(grepl("(3 of 4 components: CFPB unavailable)", a2, fixed = TRUE))
  a3 <- attr(cr_layers(cr_credit(hpi = FALSE)), "asof")[["credit_score"]]
  expect_true(grepl("(3 of 4 components: FHFA unavailable)", a3, fixed = TRUE))
  expect_false(grepl("components", attr(cr_layers(), "asof")[["credit_score"]], fixed = TRUE))
  expect_true(grepl("Urban Institute unavailable", cr_legend_title(cr_layers(cr_credit(urban = FALSE)),
                                                                   "credit_score"), fixed = TRUE))
})

test_that("a credit layer is not_loaded (not unavailable) until the credit sources are requested", {
  l <- cr_layers(NULL, credit_requested = FALSE)
  for (id in cr_ids) expect_equal(attr(l, "layer_status")[[id]], "not_loaded", info = id)
  expect_true(is.na(attr(l, "asof")[["credit_score"]]))
  # requested but failed is a real failure
  for (id in cr_ids) expect_equal(attr(cr_layers(NULL), "layer_status")[[id]], "unavailable")
  expect_equal(attr(l, "asof")[["tax_base"]], "PEP V2025 (home prices not loaded)")
  h <- cr_panel(l)
  expect_true(grepl("not loaded — open the Credit lens", h, fixed = TRUE))
  expect_false(grepl(">unavailable<", regmatches(h, regexpr("Household credit risk score.*?</tr>", h)),
                     fixed = TRUE))
  # loading wins over not_loaded
  expect_equal(attr(cr_layers(NULL, loading = "credit", credit_requested = FALSE),
                    "layer_status")[["credit_score"]], "loading")
  # a not-yet-loaded layer reads as loading in the legend and tooltips, never as an error
  expect_true(grepl("loading", cr_legend_title(l, "credit_score"), fixed = TRUE))
  expect_true(all(grepl("Loading…", cr_labels(l, "credit_score"), fixed = TRUE)))
})

test_that("tax base loading text names what it is waiting for", {
  txt <- function(l) .legend_title(risk_layer_cfg("tax_base", l), "loading", NA)
  expect_true(grepl("loading CFPB", txt(cr_layers(NULL, loading = "credit")), fixed = TRUE))
  expect_true(grepl("loading Census population estimates",
                    txt(cr_layers(NULL, loading = "migration")), fixed = TRUE))
  # both: the credit text
  expect_true(grepl("loading CFPB", txt(cr_layers(NULL, loading = c("credit", "migration"))),
                    fixed = TRUE))
  expect_true(grepl("loading Census population estimates",
                    cr_legend_title(cr_layers(NULL, loading = "migration"), "tax_base"), fixed = TRUE))
})

test_that("long legend titles wrap", {
  lg <- cr_legend_title(cr_layers(), "tax_base")
  expect_true(grepl("max-width:260px;white-space:normal", lg, fixed = TRUE))
  expect_true(grepl("max-width:260px", cr_legend_title(cr_layers(), "credit_score"), fixed = TRUE))
})

test_that("the loading legend replaces only the legend control", {
  m <- risk_loading_legend(leaflet::leaflet(), "credit_score", cr_layers(NULL, loading = "credit"))
  methods <- vapply(m$x$calls, `[[`, "", "method")
  expect_equal(methods, c("removeControl", "addControl", "addControl"))
  ctl <- m$x$calls[[2]]$args
  expect_true(grepl("showing previous layer until loaded", ctl[[1]], fixed = TRUE))
  # a style control dims the old polygons and hides their tooltips
  dim <- m$x$calls[[3]]$args
  expect_true(grepl("#risk_map .leaflet-overlay-pane{opacity:.35}", dim[[1]], fixed = TRUE))
  expect_true(grepl("#risk_map .leaflet-tooltip-pane{display:none}", dim[[1]], fixed = TRUE))
  expect_equal(dim[[3]], "risk_loading_dim")
  expect_equal(dim[[4]], "")     # no "info legend" class: it would draw an empty box
  expect_true(grepl("Household credit risk score", ctl[[1]], fixed = TRUE))
  expect_true(grepl("loading CFPB, Urban Institute and FHFA data", ctl[[1]], fixed = TRUE))
  expect_equal(ctl[[2]], "bottomright")
  expect_equal(ctl[[3]], "risk_legend")
  # the tax base asks for population estimates when that is what is loading
  m2 <- risk_loading_legend(leaflet::leaflet(), "tax_base", cr_layers(NULL, loading = "migration"))
  expect_true(grepl("Census population estimates", m2$x$calls[[2]]$args[[1]], fixed = TRUE))
  expect_true(.is_credit_layer("tax_base") && .is_credit_layer("credit_jobs"))
  expect_false(.is_credit_layer("stress_now") || .is_credit_layer("mig_dom") || .is_credit_layer("assist_score"))
})

test_that("tax base population-change driver is red for a shrinking county", {
  l <- cr_layers()
  expect_true("pct_pop_chg_pct" %in% names(l))
  tip <- cr_labels(l, "tax_base")
  red <- function(t) grepl("Population change 2020–25</span><span style=\"font-weight:600;color:#ff6b6b;\">", t, fixed = TRUE)
  expect_true(red(tip[["13001"]]))     # Appling: -3.0%, the lowest
  expect_false(red(tip[["13295"]]))    # Walker: +4.0%, the highest
})

test_that("a county in a state without a CFPB non-metro row has no mortgage value", {
  # CFPB's metro file has no "09-non" row (Connecticut): NA is honest, not a fallback bug.
  cr <- cr_credit()
  cr$cfpb$metro90 <- cr$cfpb$metro90[cr$cfpb$metro90$id != "09-non", ]
  cr$cfpb$metro3089 <- cr$cfpb$metro3089[cr$cfpb$metro3089$id != "09-non", ]
  l <- cr_layers(cr)
  expect_true(is.na(l$dq90[l$GEOID == "09110"]))
  expect_match(l$credit_missing[l$GEOID == "09110"], "mortgage")
})

test_that("a state-level CFPB row fills the gap and is labelled a state estimate", {
  cr <- cr_credit()
  cr$cfpb$metro90 <- cr$cfpb$metro90[cr$cfpb$metro90$id != "09-non", ]
  cr$cfpb$metro3089 <- cr$cfpb$metro3089[cr$cfpb$metro3089$id != "09-non", ]
  cr$cfpb$state90 <- cr_tbl(c("09", "13"), c(1.1, 1.3), c(1.3, 1.5))
  cr$cfpb$state3089 <- cr_tbl(c("09", "13"), c(1.7, 2.6), c(2.1, 2.9))
  l <- cr_layers_sa(cr)
  i <- l$GEOID == "09110"
  expect_equal(c(l$dq90[i], l$cfpb_source[i]), c("1.3", "state"))
  expect_equal(l$dq3089_chg[i], 0.4)
  # finer levels still win: Appling keeps the Georgia non-metro row, not the state row
  expect_equal(l$cfpb_source[l$GEOID == "13001"], "state non-metro")
  # labels name it in the tooltip, the component tooltip and the side panel
  mort <- cr_labels(l, "credit_mortgage")[["09110"]]
  expect_true(grepl("CFPB Mar 2026 · state estimate", mort, fixed = TRUE))
  expect_true(grepl("state estimate", cr_panel(l, "09110"), fixed = TRUE))
  expect_equal(.cfpb_note("state"), " (state est.)")
  # CT still lacks consumer and job pressure here, so its score stays insufficient
  expect_true(is.na(l$credit_score[i]))
  expect_match(l$credit_missing[i], "consumer")
  expect_false(grepl("mortgage", l$credit_missing[i], fixed = TRUE))
})

test_that("derived assistance layers go stale/partial with their inputs", {
  base <- function(sa = fx_stress_ahead(), w = NULL) {
    build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa, sector_weights = w)
  }
  st <- function(l) attr(l, "layer_status")[["assist_score"]]
  expect_equal(st(base()), "ok")
  sa <- fx_stress_ahead(); attr(sa, "stale") <- TRUE
  l <- base(sa)
  expect_equal(st(l), "stale")
  expect_false(is.na(attr(l, "asof")[["assist_score"]]))
  expect_true(grepl("(stale)", .legend_title(RISK_LAYERS$assist_score, "stale", "x"), fixed = TRUE))
  sa <- fx_stress_ahead(); attr(sa, "partial") <- TRUE
  expect_equal(st(base(sa)), "partial")
  w <- fx_weights(); attr(w, "stale") <- TRUE
  expect_equal(st(base(w = w)), "stale")
  w <- fx_weights(); attr(w, "partial") <- TRUE
  expect_equal(st(base(w = w)), "partial")
  # stale wins over partial
  sa <- fx_stress_ahead(); attr(sa, "partial") <- TRUE; attr(w, "stale") <- TRUE
  expect_equal(st(base(sa, w)), "stale")
  # loading and unavailable still win
  expect_equal(attr(build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa,
                                        loading = "stress_ahead"), "layer_status")[["assist_score"]],
               "loading")
  expect_equal(st(build_county_layers(fx_county_base(), fx_momentum())), "unavailable")
  # all four derived assistance layers follow
  sa <- fx_stress_ahead(); attr(sa, "stale") <- TRUE
  s <- attr(base(sa), "layer_status")
  expect_equal(unname(s[c("assist_score", "assist_exposure", "assist_cushion", "assist_workers")]),
               rep("stale", 4))
})

test_that("side panel lists credit components with sources", {
  l <- cr_layers()
  h <- cr_panel(l)
  for (s in c("Credit components", "CFPB", "FHFA", "Urban Institute", "metro division",
              "2026 Q1", "released Oct 2026", "Mar 2026"))
    expect_true(grepl(s, h, fixed = TRUE), info = s)
  expect_false(grepl("Urban Institute data unavailable", h, fixed = TRUE))
  # Hamilton: CFPB metro estimate, state-level HPI vintage not mixed up with Walker's
  h2 <- cr_panel(l, "47065")
  expect_true(grepl("metro estimate", h2, fixed = TRUE))
  # Appling: state-level
  expect_true(grepl("state non-metro estimate", cr_panel(l, "13001"), fixed = TRUE))
  # Capitol: insufficient data, named missing components
  h3 <- cr_panel(l, "09110")
  expect_true(grepl("Insufficient data", h3, fixed = TRUE))
  expect_true(grepl("consumer, job pressure", h3, fixed = TRUE))
  # No Urban: the score says so
  hu <- cr_panel(cr_layers(cr_credit(urban = FALSE)))
  expect_true(grepl("Urban Institute data unavailable — score uses 3 components", hu, fixed = TRUE))
  # No credit data yet: no block
  expect_false(grepl("Credit components", cr_panel(cr_layers(NULL)), fixed = TRUE))
})

test_that("side panel groups rows by lens", {
  l <- cr_layers()
  h <- cr_panel(l)
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))),
               length(RISK_LAYERS) + 1 + length(RISK_LENSES))
  pos <- vapply(names(RISK_LENSES), function(z) regexpr(paste0(">", z, "<"), h, fixed = TRUE)[1], 0)
  expect_false(anyNA(pos))
  expect_true(all(pos > 0))
  expect_equal(order(pos), seq_along(pos))
  # Each lens header precedes its layers' rows.
  for (id in names(RISK_LAYERS)) {
    lens <- RISK_LAYERS[[id]]$lens
    row <- regexpr(htmltools::htmlEscape(RISK_LAYERS[[id]]$label), h, fixed = TRUE)[1]
    expect_gt(row, pos[[names(RISK_LENSES)[RISK_LENSES == lens]]], label = id)
  }
  # Credit and tax base values appear in the table
  expect_true(grepl("Household credit risk score", h, fixed = TRUE))
  expect_true(grepl("Tax base pressure (proxy — not a bond default model)", h, fixed = TRUE))
})

test_that("a tract side panel shows tract-level as-of for assistance rows", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  t <- data.frame(GEOID = "13295020300", county_geoid = "13295", tract_name = "Census Tract 203",
                  assist_score = 72, assist_source = "LODES 2024", poverty_rate = 18,
                  lm_pct_assist_score = 90, st_pct_assist_score = 85,
                  lm_pct_poverty_rate = 92, st_pct_poverty_rate = 85,
                  frame_label = "in Chattanooga metro", state_label = "in Georgia")
  h <- as.character(risk_side_panel(t, "tract", l))
  expect_true(grepl("LODES 2024 / ACS 2020–2024 · CES Oct 2026", h, fixed = TRUE))
  # The county as-of is a different string; the tract panel must not show it for assistance
  county_asof <- attr(l, "asof")[["assist_score"]]
  row <- regmatches(h, regexpr("Assistance need score.*?</tr>", h))
  expect_false(grepl(paste0(">", county_asof, "<"), row, fixed = TRUE))
  # Non-assistance rows keep the county as-of
  prow <- regmatches(h, regexpr("Poverty Rate.*?</tr>", h))
  expect_true(grepl("ACS 2020–2024", prow, fixed = TRUE))
  # Tract falls back to the frame's source for its state
  t2 <- t; t2$assist_source <- NA_character_
  h2 <- as.character(risk_side_panel(t2, "tract", l, sources = c("13" = "LODES 2023")))
  expect_true(grepl("LODES 2023 / ACS 2020–2024", h2, fixed = TRUE))
  # Without sector weights it says unavailable
  nw <- build_county_layers(fx_county_base(), fx_momentum())
  hn <- as.character(risk_side_panel(t, "tract", nw))
  expect_true(grepl("unavailable", regmatches(hn, regexpr("Assistance need score.*?</tr>", hn)),
                    fixed = TRUE))
})

test_that("tract legend of an assistance layer goes stale with the sector weights", {
  w <- fx_weights(); attr(w, "stale") <- TRUE
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead(),
                           sector_weights = w)
  sa <- .tract_status_asof(data.frame(assist_source = "LODES 2024"), RISK_LAYERS$assist_score,
                           "assist_score", l)
  expect_equal(sa$status, "stale")
  expect_equal(.tract_status_asof(data.frame(assist_source = "LODES 2024"), RISK_LAYERS$assist_score,
                                  "assist_score",
                                  build_county_layers(fx_county_base(), fx_momentum(),
                                                      stress_ahead = fx_stress_ahead()))$status, "ok")
})

test_that("credit layers work under the app's scalar %||%", {
  app_op <- function(a, b) {
    if (is.null(a)) return(b)
    ok <- tryCatch(!is.na(a[[1]]), error = function(e) TRUE)
    if (isTRUE(ok) && length(a) > 0) a else b
  }
  assign("%||%", app_op, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  l <- cr_layers()
  expect_equal(l$dq90, c(1.4, 2.6, 2.5, 3.2))
  for (id in c(cr_ids, "tax_base")) expect_length(cr_labels(l, id), 4)
  expect_s3_class(risk_side_panel(l[l$GEOID == "13295", ], "county", l), "shiny.tag")
})

test_that("drawing county polygons removes the loading dim control", {
  l <- cr_layers()
  m <- .add_county_polygons(leaflet::leaflet(), l, "stress_now")
  rm_calls <- Filter(function(cl) cl$method == "removeControl", m$x$calls)
  expect_true("risk_loading_dim" %in% vapply(rm_calls, function(cl) cl$args[[1]], ""))
  # loading dim goes away before the polygons are added
  methods <- vapply(m$x$calls, `[[`, "", "method")
  ids <- vapply(m$x$calls, function(cl) if (cl$method == "removeControl") cl$args[[1]] else "", "")
  expect_lt(which(ids == "risk_loading_dim"), which(methods == "addPolygons"))
})
