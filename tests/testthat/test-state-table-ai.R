fx_tracts_for_table <- function() {
  sf::st_sf(GEOID = c("13295000100", "13295000200"), tract_name = c("Tract 1", "Tract 2"),
            poverty_rate = c(10, 30), lm_pct_poverty_rate = c(0, 100),
            st_pct_poverty_rate = c(10, 90), frame_label = "in Chattanooga metro",
            geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326))
}

make_metro_row_for_ai <- function() {
  data.frame(metro = "Chattanooga", GEOID = "16860", pop = 570000, med_age = 40,
             unemp_rate = 3.5, lfp_rate = 60, poverty_rate = 14, med_income = 60000,
             per_cap_income = 30000, med_home_val = 190000, med_rent = 900,
             vacancy_rate = 9, rent_burden_rate = 48, bachelors_plus_rate = 20,
             unemp_vs_us = -0.5, lfp_vs_us = -3, income_vs_us = -10000, poverty_vs_us = 2,
             edu_vs_us = -13, stringsAsFactors = FALSE)
}

map_calls <- function(m, method) Filter(function(cl) cl$method == method, m$x$calls)

fx_layers <- function(...) build_county_layers(fx_county_base(), fx_momentum(),
                                                 migration = fx_migration(),
                                                 stress_ahead = fx_stress_ahead(), ...)
labels_of <- function(m, group) {
  calls <- Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], group),
                  m$x$calls)
  if (!length(calls)) return(NULL)
  vapply(calls[[1]]$args[[7]], as.character, "")
}

# ── compact hover ─────────────────────────────────────────────────────────────
test_that("compact hover shows name, value and rank only", {
  old <- options(risk.hover_detail = FALSE); on.exit(options(old))
  m <- build_risk_county_map(fx_layers(), "med_income")
  lab <- labels_of(m, "counties")
  expect_true(grepl("Walker County, GA", lab[1], fixed = TRUE))
  expect_true(grepl("Bottom 1% nationally", lab[1], fixed = TRUE))
  expect_true(grepl("white-space:nowrap", lab[1], fixed = TRUE))
  expect_false(grepl("Driven by", lab[1], fixed = TRUE))
  expect_false(grepl("Census ACS", lab[1], fixed = TRUE))       # no source footer
  expect_false(grepl("Chattanooga", lab[1], fixed = TRUE))       # no metro line
})

test_that("compact hover keeps the Loading text while a layer loads", {
  tip <- as.character(.county_tooltips(fx_layers()[1, ], "stress_now", RISK_LAYERS$stress_now,
                                       NA, "loading", detail = FALSE))
  expect_true(grepl("Loading", tip, fixed = TRUE))
})

# ── state view ────────────────────────────────────────────────────────────────
test_that("a state view draws only that state's counties as interactive", {
  old <- options(risk.hover_detail = FALSE); on.exit(options(old))
  l <- fx_layers()
  l$state_fips <- c("13", "13"); l$state_name <- "Georgia"      # both in one state ...
  l$state_fips[2] <- "09"; l$state_name[2] <- "Connecticut"     # ... then split again
  m <- build_risk_county_map(risk_view(l, "13"), "med_income")
  polys <- map_calls(m, "addPolygons")
  fg <- Filter(function(cl) identical(cl$args[[3]], "counties"), polys)[[1]]
  bg <- Filter(function(cl) identical(cl$args[[3]], "counties_bg"), polys)[[1]]
  expect_equal(fg$args[[2]], "13295")
  expect_length(fg$args[[7]], 1)
  expect_false(isTRUE(bg$args[[8]]$interactive))     # backdrop passes clicks through
  expect_null(bg$args[[2]])                          # and has no layerIds
})

test_that("hover ranks are within the state when one is selected", {
  old <- options(risk.hover_detail = FALSE); on.exit(options(old))
  l <- fx_layers()
  l$state_fips <- "13"; l$state_name <- "Georgia"
  m <- build_risk_county_map(risk_view(l, "13"), "med_income")
  lab <- labels_of(m, "counties")
  expect_true(grepl("in Georgia", lab[2], fixed = TRUE))
  expect_true(grepl("Top 1% in Georgia", lab[2], fixed = TRUE))
})

test_that("no state view means no backdrop group", {
  m <- build_risk_county_map(risk_view(fx_layers(), ""), "med_income")
  expect_null(labels_of(m, "counties_bg"))
  m2 <- build_risk_county_map(fx_layers(), "med_income")
  expect_length(map_calls(m2, "addPolygons"), 1)
})

test_that("rescaling to a state drops the fixed bins from the legend", {
  l <- fx_layers()
  fixed <- map_calls(build_risk_county_map(risk_view(l, "13"), "stress_now"), "addLegend")[[1]]
  resc <- map_calls(build_risk_county_map(risk_view(l, "13", TRUE), "stress_now"), "addLegend")[[1]]
  expect_true(any(grepl("Sahm", unclass(fixed$args[[1]]$labels))))
  expect_false(any(grepl("Sahm", unclass(resc$args[[1]]$labels))))
  expect_true(grepl("Georgia", resc$args[[1]]$title, fixed = TRUE))
})

test_that("fit_to_state zooms to a state, fixes Alaska, and resets for no state", {
  l <- fx_layers()
  m <- fit_to_state(leaflet::leaflet(), l, "13")
  expect_false(is.null(m$x$fitBounds))
  ak <- fit_to_state(leaflet::leaflet(), l, "02")
  expect_equal(ak$x$setView[[1]], c(63.5, -150))
  us <- fit_to_state(leaflet::leaflet(), l, "")
  expect_equal(us$x$setView[[1]], c(38, -96))
})

# ── data table ────────────────────────────────────────────────────────────────
test_that("county table lists worst first and ranks within the state", {
  l <- fx_layers()
  all_rows <- risk_county_table(l, "stress_now")
  expect_equal(all_rows$GEOID, c("09110", "13295"))        # +1.1 pp before -0.17 pp
  expect_equal(all_rows$County, c("Capitol Planning Region", "Walker County"))
  expect_equal(all_rows$Metro, c("—", "Chattanooga"))
  ga <- risk_county_table(l, "stress_now", "13")
  expect_equal(ga$GEOID, "13295")
  expect_equal(ga$State, "Georgia")
  expect_true(is.na(ga$`State pct`))                        # no rank for a lone county
  expect_equal(ga$`US pct`, 0)
  expect_true(all(c("Unemp. now (%)", "Stress ahead (pp)", "Credit score", "Population")
                  %in% names(ga)))
})

test_that("county table puts low values first for a lower-is-worse layer", {
  t <- risk_county_table(fx_layers(), "stress_ahead")
  expect_equal(t$Value, sort(t$Value))
})

test_that("tract table carries the metro and state ranks", {
  tr <- fx_tracts_for_table()
  t <- risk_tract_table(tr, "poverty_rate")
  expect_equal(names(t)[1:5], c("GEOID", "Tract", "Value", "Metro pct", "State pct"))
  expect_equal(t$Value, sort(t$Value, decreasing = TRUE))
})

test_that("layer help is one line and never empty", {
  for (id in names(RISK_LAYERS)) {
    h <- risk_layer_help(id)
    expect_true(nzchar(h), info = id)
    expect_lt(nchar(h), 320)
  }
  expect_true(any(grepl("Dashed outline", risk_guide_items(), fixed = TRUE)))
})

# ── Metro AI context ──────────────────────────────────────────────────────────
test_that("metro risk context rolls the map layers up to the metro", {
  ctx <- metro_risk_context(fx_layers(credit_requested = FALSE), "16860")
  expect_equal(ctx$counties_in_metro, 1)
  expect_equal(ctx$unemployment$county_change_vs_year_ago_pp_pop_weighted, -0.17)
  expect_equal(ctx$unemployment$counties_at_sahm_trigger, 0)
  expect_equal(ctx$unemployment$us_county_change_pp_pop_weighted,
               round(weighted.mean(c(-0.17, 1.1), c(69000, 980000)), 2))
  expect_equal(ctx$migration_and_population$net_domestic_per_1000$metro, 8.1)
  expect_equal(ctx$largest_counties[[1]]$county, "Walker County, GA")
  expect_true("stress_ahead_projected_job_growth_gap_pp" %in% names(ctx))
  expect_true("assistance_need" %in% names(ctx))
  expect_true(length(ctx$industry_mix_share_of_jobs_pct) > 0)
})

test_that("metro risk context names what is missing and never emits NA", {
  ctx <- metro_risk_context(fx_layers(credit_requested = FALSE), "16860")
  expect_true(any(grepl("Credit layers not loaded", unlist(ctx$data_gaps), fixed = TRUE)))
  expect_false("credit" %in% names(ctx))
  json <- as.character(jsonlite::toJSON(ctx, auto_unbox = TRUE))
  expect_false(grepl('"NA"', json, fixed = TRUE))
  expect_false(grepl("NaN", json, fixed = TRUE))
})

test_that("the selected county is added to the metro context, never NA", {
  cl <- fx_layers(credit_requested = FALSE)
  gid <- sf::st_drop_geometry(cl)$GEOID[1]
  ctx <- metro_risk_context(cl, "16860", selected_county = gid)
  sc <- ctx$selected_county
  expect_false(is.null(sc))
  expect_equal(sc$county, sf::st_drop_geometry(cl)$name[1])
  expect_false(grepl('"NA"|NaN', as.character(jsonlite::toJSON(sc, auto_unbox = TRUE))))
  expect_null(metro_risk_context(cl, "16860")$selected_county)
  expect_null(county_risk_context(cl, "00000"))
  expect_null(county_risk_context(cl, NULL))
  expect_true(grepl("selected_county", build_metro_prompt(list()), fixed = TRUE))
})

test_that("metro risk context is NULL for a metro the map does not know", {
  expect_null(metro_risk_context(fx_layers(), "99999"))
})

test_that("the summary and prompt carry the regional risk block only when given", {
  row <- make_metro_row_for_ai()
  us <- list(unemp_rate = 4, med_income = 70000, lfp_rate = 63, poverty_rate = 12,
             bachelors_plus_rate = 33, rent_burden_rate = 50, vacancy_rate = 7)
  ctx <- metro_risk_context(fx_layers(), "16860")
  with_risk <- build_metro_summary(row, us, risk = ctx)
  expect_equal(with_risk$regional_risk, ctx)
  expect_null(build_metro_summary(row, us)$regional_risk)
  prompt <- build_metro_prompt(with_risk, "headline")
  expect_true(grepl("regional_risk", prompt, fixed = TRUE))
  expect_true(grepl("Walker County, GA", prompt, fixed = TRUE))
  expect_true(grepl("data_gaps", prompt, fixed = TRUE))
})


test_that("legends list the worst end first", {
  labs <- function(id) as.character(unclass(
    map_calls(build_risk_county_map(fx_layers(), id), "addLegend")[[1]]$args[[1]]$labels))
  expect_match(labs("stress_ahead")[1], "below")       # low is worse: lowest on top
  expect_match(labs("stress_now")[1], "above")         # high is worse: highest on top
  # a quantile-binned (ACS) legend is built the same way, not left to leaflet's order
  p <- labs("poverty_rate")
  expect_equal(p, p[order(-as.numeric(sub("^([-0-9.]+).*", "\\1", p)))])
})

test_that("the middle bin of a diverging palette is a visible neutral, not near-white", {
  for (id in c("stress_now", "stress_ahead")) {
    cfg <- RISK_LAYERS[[id]]
    b <- cfg$bins
    mids <- (b[-1] + b[-length(b)]) / 2
    cols <- .risk_palette(cfg, NULL)(mids)
    expect_equal(toupper(cols[4]), toupper(.RISK_NEUTRAL), info = id)
    expect_false(any(toupper(cols) %in% c("#F7F7F7", "#FFFFFF")), info = id)
  }
  # red is the at-risk end in both: rising unemployment, falling projected job growth
  sn <- RISK_LAYERS$stress_now; sa <- RISK_LAYERS$stress_ahead
  expect_equal(.risk_palette(sn, NULL)(50), "#B2182B")    # strongest rise
  expect_equal(.risk_palette(sa, NULL)(-50), "#B2182B")   # weakest outlook
  expect_equal(.risk_palette(sa, NULL)(50), "#2166AC")
})
