# Metro LAUS: area list, CBSA series parse, pull rules, and the metro line.
metro_tmp <- function() { d <- tempfile("lausm"); dir.create(d); d }
bls_resp_metro <- function(req, ok = TRUE) {
  ids <- unlist(req$body$data$seriesid)
  body <- list(status = if (ok) "REQUEST_SUCCEEDED" else "REQUEST_NOT_PROCESSED",
               Results = list(series = lapply(ids, function(i) list(
                 seriesID = i, data = list(list(year = "2026", period = "M09", value = "4.0"))))))
  httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
}

metro_poly_labels <- function(m, group = "counties") {
  poly <- Filter(function(cl) cl$method == "addPolygons" && identical(cl$args[[3]], group),
                 m$x$calls)[[1]]
  vapply(poly$args[[7]], as.character, "")
}

metro_mom <- function(...) {
  tibble::tibble(fips = "16860", ref_month = as.Date("2026-09-01"), rate_now = 3.9,
                 rate_yr_ago = 3.6, momentum_pp = 0.3, flag = "None", ...)
}

test_that("area list keeps metros, builds series ids", {
  a <- parse_laus_areas(readLines("fixtures/la_area_sample.txt", warn = FALSE))
  expect_equal(nrow(a), 2)
  expect_equal(a$series_id[a$cbsa_geoid == "16860"], "LAUMT471686000000003")
  expect_true("19340" %in% a$cbsa_geoid)
  expect_match(a$cbsa_title[a$cbsa_geoid == "16860"], "^Chattanooga")
})

test_that("metro series parse CBSA at position 8", {
  r <- list(list(seriesID = "LAUMT471686000000003",
                 data = list(list(year = "2026", period = "M09", value = "3.9"))))
  expect_equal(parse_bls_laus(r, id_start = 8L)$fips, "16860")
})

# 8 chunks of 50 metro series; `fail` marks the chunks the fake BLS rejects.
run_metro_pull <- function(fail) {
  risk_cache_clear_memory()
  ids <- sprintf("LAUMT%02d%05d000000003", 1:400 %% 99, 1:400)
  retries <- 0
  msgs <- testthat::capture_messages(res <- fetch_laus_metro(
    ids, key = "test", cache_dir = metro_tmp(),
    perform = function(reqs, ...) Map(function(r, bad) bls_resp_metro(r, !bad), reqs, fail),
    perform_one = function(req) { retries <<- retries + 1; bls_resp_metro(req, TRUE) }))
  list(res = res, retries = retries, msgs = msgs)
}

test_that("metro pull reuses county retry rules: >50% failed gives NULL, no retries", {
  r <- run_metro_pull(c(rep(TRUE, 5), rep(FALSE, 3)))
  expect_null(r$res)
  expect_equal(r$retries, 0)
  expect_true(any(grepl("5/8 BLS calls failed; not retrying", r$msgs, fixed = TRUE)))
  expect_false(any(grepl("refresh failed", r$msgs, fixed = TRUE)))
})

test_that("metro pull retries the few failed chunks once and is complete", {
  r <- run_metro_pull(c(TRUE, rep(FALSE, 5), TRUE, FALSE))
  expect_equal(r$retries, 2)
  expect_false(is.null(r$res))
  expect_equal(length(unique(r$res$fips)), 400)
  expect_false(isTRUE(attr(r$res, "partial")))
  expect_false(any(grepl("refresh failed", r$msgs, fixed = TRUE)))
})

test_that("metro pull returns CBSA codes in fips", {
  risk_cache_clear_memory()
  ids <- c("LAUMT471686000000003", "LAUMT171934000000003")
  res <- fetch_laus_metro(ids, key = "test", cache_dir = metro_tmp(),
                          perform = function(reqs, ...) lapply(reqs, bls_resp_metro))
  expect_setequal(res$fips, c("16860", "19340"))
})

test_that("metro line on CBSA counties", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), metro_momentum = metro_mom())
  txt <- "Chattanooga metro: +0.3 pp unemployment vs. a year ago (Sep 2026)"
  labs <- metro_poly_labels(build_risk_county_map(l, "stress_now"))
  expect_true(grepl(txt, labs[1], fixed = TRUE))
  expect_false(grepl("pp unemployment vs. a year ago", labs[2], fixed = TRUE))
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_true(grepl(txt, h, fixed = TRUE))
  # Shown on every lens, and kept out of the table rows.
  expect_true(grepl(txt, metro_poly_labels(build_risk_county_map(l, "poverty_rate"))[1],
                    fixed = TRUE))
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), length(RISK_LAYERS) + 1 + length(RISK_LENSES))
})

test_that("metro line sits after the migration line", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration(),
                           metro_momentum = metro_mom())
  lab <- metro_poly_labels(build_risk_county_map(l, "stress_now"))[1]
  expect_lt(regexpr("Net domestic migration", lab, fixed = TRUE),
            regexpr("Chattanooga metro:", lab, fixed = TRUE))
})

test_that("tract tooltip and tract side panel carry the metro line of their county", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), metro_momentum = metro_mom())
  t <- sf::st_sf(
    GEOID = "13295020300", county_geoid = "13295",
    tract_name = "Census Tract 203, Walker County, Georgia",
    poverty_rate = 18, poverty_rate_moe = 2, noisy_poverty_rate = FALSE,
    lm_pct_poverty_rate = 92, st_pct_poverty_rate = 85,
    frame_label = "in Chattanooga metro", state_label = "in Georgia",
    geometry = sf::st_sfc(sq(-85.2, 34.5, 0.1), crs = 4326))
  txt <- "Chattanooga metro: +0.3 pp unemployment vs. a year ago (Sep 2026)"
  m <- add_tract_layer(build_risk_county_map(l, "poverty_rate"), t, "poverty_rate", l)
  expect_true(grepl(txt, metro_poly_labels(m, "tracts"), fixed = TRUE))
  expect_true(grepl(txt, as.character(risk_side_panel(t, "tract", l)), fixed = TRUE))
})

test_that("no metro line without metro data", {
  needle <- "pp unemployment vs. a year ago"
  l <- build_county_layers(fx_county_base(), fx_momentum(), metro_momentum = NULL)
  expect_false(any(grepl(needle, metro_poly_labels(build_risk_county_map(l, "stress_now")),
                         fixed = TRUE)))
  expect_false(grepl(needle, as.character(risk_side_panel(l[1, ], "county", l)), fixed = TRUE))
  # CBSA missing from the metro pull: no line, no "NA pp".
  other <- metro_mom(); other$fips <- "99999"
  l2 <- build_county_layers(fx_county_base(), fx_momentum(), metro_momentum = other)
  labs <- metro_poly_labels(build_risk_county_map(l2, "stress_now"))
  expect_false(any(grepl(needle, labs, fixed = TRUE)))
  expect_false(any(grepl("NA pp", labs, fixed = TRUE)))
  expect_false(grepl("NA pp", as.character(risk_side_panel(l2[1, ], "county", l2)), fixed = TRUE))
})

test_that(".metro_line is empty when any input is NA", {
  expect_equal(.metro_line(c("A", NA, "C"), c(0.5, 0.1, NA), as.Date(rep("2026-09-01", 3)))[2:3],
               c("", ""))
  expect_equal(.metro_line("A", 0.5, as.Date(NA)), "")
})
test_that("a CBSA name with & and < is HTML-escaped exactly once", {
  b <- fx_county_base(); b$cbsa_name[1] <- "A&B <Co>"
  l <- build_county_layers(b, fx_momentum(), metro_momentum = metro_mom())
  esc <- "A&amp;B &lt;Co&gt; metro: +0.3 pp"
  lab <- metro_poly_labels(build_risk_county_map(l, "stress_now"))[1]
  expect_true(grepl(esc, lab, fixed = TRUE))
  expect_false(grepl("&amp;amp;|&amp;lt;", lab))
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_true(grepl(esc, h, fixed = TRUE))
  expect_false(grepl("&amp;amp;|&amp;lt;", h))
})

