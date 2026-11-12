cfpb_fx <- function(name, id_col) {
  df <- readr::read_csv(test_path("fixtures", name),
                        col_types = readr::cols(.default = "c"), progress = FALSE)
  parse_cfpb_wide(df, id_col)
}

test_that("links pick newest thru and https", {
  html <- paste(readLines(test_path("fixtures", "cfpb_page.html"), warn = FALSE),
                collapse = "\n")
  ln <- cfpb_links(html)
  expect_equal(ln$thru, "2026-03")
  urls <- unlist(ln[c("county_90", "county_3089", "metro_90", "metro_3089")])
  expect_length(urls, 4)
  expect_true(all(startsWith(urls, "https://files.consumerfinance.gov/")))
  expect_true(all(grepl("thru-2026-03[.]csv$", urls)))
  expect_match(ln$county_3089, "CountyMortgagesPercent-30-89DaysLate", fixed = FALSE)
})

test_that("a type with no link comes back NA", {
  ln <- cfpb_links("<a href=\"https://files.consumerfinance.gov/x/CountyMortgagesPercent-90-plusDaysLate-thru-2026-03.csv\">")
  expect_true(is.na(ln$metro_90))
  expect_true(is.na(ln$thru))
})

test_that("FIPS quotes stripped, national row dropped", {
  cty <- cfpb_fx("cfpb_county_90.csv", "FIPSCode")
  expect_true("01003" %in% cty$id)
  expect_false("-----" %in% cty$id)
  expect_false(any(grepl("'", cty$id)))
  expect_s3_class(cty$date, "Date")
  expect_equal(sort(unique(cty$date))[c(1, 14)], as.Date(c("2025-02-01", "2026-03-01")))
  expect_equal(cty$pct[cty$id == "13121" & cty$date == as.Date("2026-03-01")], 1.8)
  met <- cfpb_fx("cfpb_metro_90.csv", "CBSACode")
  expect_setequal(met$id, c("12060", "16860", "13-non"))
})

test_that("CFPB falls back county -> metro -> state non-metro with source", {
  c90 <- cfpb_fx("cfpb_county_90.csv", "FIPSCode")
  c3089 <- cfpb_fx("cfpb_county_3089.csv", "FIPSCode")
  m90 <- cfpb_fx("cfpb_metro_90.csv", "CBSACode")
  m3089 <- cfpb_fx("cfpb_metro_3089.csv", "CBSACode")
  out <- cfpb_county_components(
    county_ids = c("13121", "13295", "13001", "99001"),
    cbsa_of = c("12060", "16860", NA, "99999"),
    cbsa_type_of = c("metro", "metro", NA, "metro"),
    state_of = c("13", "13", "13", "99"),
    c90, c3089, m90, m3089)
  expect_equal(out$GEOID, c("13121", "13295", "13001", "99001"))
  expect_equal(out$cfpb_source, c("county", "metro", "state non-metro", NA))
  # Fulton: county row. 90+ latest 1.8; 30-89 1.8 (2026-03) - 1.9 (2025-03)
  expect_equal(out$dq90[1], 1.8)
  expect_equal(out$dq3089_chg[1], 1.8 - 1.9, tolerance = 1e-9)
  # Walker: absent from the county file, CBSA 16860 -> Chattanooga metro row
  expect_equal(out$dq90[2], 1.1)
  expect_equal(out$dq3089_chg[2], 1.6 - 1.7, tolerance = 1e-9)
  # Appling: non-CBSA GA -> 13-non row
  expect_equal(out$dq90[3], 1.2)
  expect_equal(out$dq3089_chg[3], 2.5 - 2.4, tolerance = 1e-9)
  # no county row, metro-type CBSA not in the metro file -> NA, no state fallback
  expect_true(is.na(out$dq90[4]) && is.na(out$dq3089_chg[4]))
  expect_equal(out$cfpb_month[1:3], rep(as.Date("2026-03-01"), 3))
  expect_true(is.na(out$cfpb_month[4]))
})

test_that("state non-metro applies to micro-area or no-CBSA counties, never metro ones", {
  c90 <- cfpb_fx("cfpb_county_90.csv", "FIPSCode")
  c3089 <- cfpb_fx("cfpb_county_3089.csv", "FIPSCode")
  m90 <- cfpb_fx("cfpb_metro_90.csv", "CBSACode")
  m3089 <- cfpb_fx("cfpb_metro_3089.csv", "CBSACode")
  out <- cfpb_county_components(c("13001", "13003", "13005"), c("99999", "99998", NA),
                                c("micro area", "metro", NA), c("13", "13", "13"),
                                c90, c3089, m90, m3089)
  expect_equal(out$cfpb_source, c("state non-metro", NA, "state non-metro"))
  expect_equal(out$dq90, c(1.2, NA, 1.2))
  expect_true(is.na(out$dq3089_chg[2]) && is.na(out$cfpb_month[2]))
})

test_that("12-month change is anchored to the 90+ latest month, NA if a month is absent", {
  c90 <- cfpb_fx("cfpb_county_90.csv", "FIPSCode")
  c3089 <- cfpb_fx("cfpb_county_3089.csv", "FIPSCode")
  m90 <- cfpb_fx("cfpb_metro_90.csv", "CBSACode")
  m3089 <- cfpb_fx("cfpb_metro_3089.csv", "CBSACode")
  call <- function(t3089) cfpb_county_components("13121", "12060", "metro", "13",
                                                 c90, t3089, m90, m3089)
  # 30-89 lacks the 90+ latest month: no mixing with its own latest month
  expect_true(is.na(call(c3089[c3089$date != as.Date("2026-03-01"), ])$dq3089_chg))
  # 30-89 lacks the month 12 months back
  expect_true(is.na(call(c3089[c3089$date != as.Date("2025-03-01"), ])$dq3089_chg))
  expect_equal(call(c3089)$dq90, 1.8)
})

test_that("a county the 30-89 file lacks keeps dq90 and gets NA change", {
  c90 <- cfpb_fx("cfpb_county_90.csv", "FIPSCode")
  c3089 <- cfpb_fx("cfpb_county_3089.csv", "FIPSCode")
  c3089 <- c3089[c3089$id != "47065", ]
  m90 <- cfpb_fx("cfpb_metro_90.csv", "CBSACode")
  m3089 <- cfpb_fx("cfpb_metro_3089.csv", "CBSACode")
  out <- cfpb_county_components("47065", "16860", "metro", "47", c90, c3089, m90, m3089)
  expect_equal(out$cfpb_source, "county")
  expect_equal(out$dq90, 0.6)
  expect_true(is.na(out$dq3089_chg))
})

test_that("fetch keeps 13 months and returns NULL when a file fails", {
  html <- paste(readLines(test_path("fixtures", "cfpb_page.html"), warn = FALSE),
                collapse = "\n")
  fx <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE),
                             collapse = "\n")
  texts <- c(county_90 = fx("cfpb_county_90.csv"), county_3089 = fx("cfpb_county_3089.csv"),
             metro_90 = fx("cfpb_metro_90.csv"), metro_3089 = fx("cfpb_metro_3089.csv"))
  get_text <- function(url) {
    if (grepl("download-the-data", url)) return(html)
    kind <- if (grepl("MetroArea", url)) "metro" else "county"
    sev <- if (grepl("90-plus", url)) "90" else "3089"
    texts[[paste0(kind, "_", sev)]]
  }
  out <- .cfpb_build(get_text)
  expect_equal(out$thru, "2026-03")
  expect_equal(length(unique(out$county90$date)), 13)
  expect_equal(min(out$county90$date), as.Date("2025-03-01"))
  expect_true("01003" %in% out$county90$id)
  expect_message(
    res <- .cfpb_build(function(url) if (grepl("Metro.*30-89", url)) stop("boom") else get_text(url)),
    "cfpb")
  expect_null(res)
})

test_that("thru is the oldest of the four chosen files", {
  b <- "https://files.consumerfinance.gov/d/"
  html <- paste0('<a href="', b, c("CountyMortgagesPercent-90-plusDaysLate-thru-2026-03.csv",
                                    "CountyMortgagesPercent-30-89DaysLate-thru-2026-03.csv",
                                    "MetroAreaMortgagesPercent-90-plusDaysLate-thru-2025-12.csv",
                                    "MetroAreaMortgagesPercent-30-89DaysLate-thru-2025-12.csv"), '">')
  expect_equal(cfpb_links(paste(html, collapse = "
"))$thru, "2025-12")
})

test_that("an NA RegionType does not crash the parser", {
  df <- data.frame(RegionType = c("County", NA), State = "AL", Name = "x",
                   FIPSCode = c("'01003'", "'01005'"), `2026-03` = c("0.8", "0.9"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(parse_cfpb_wide(df, "FIPSCode")$id, c("01003", "01005"))
})

# ── Urban consumer delinquency ───────────────────────────────────────────────
urban_fx <- function() {
  readr::read_csv(test_path("fixtures", "urban_county_overall.csv"),
                  col_types = readr::cols(.default = "c"), progress = FALSE)
}

test_that("Urban path parsed and NA strings handled", {
  js <- paste(readLines(test_path("fixtures", "urban_main_snippet.js"), warn = FALSE),
              collapse = "\n")
  expect_equal(urban_path(js), "data/20261022-update/")
  expect_null(urban_path("var other=1;"))
  expect_null(urban_path("var pathFiles=\"../../etc/\";"))
  # declaration keyword, spacing and quote style vary between bundle builds
  expect_equal(urban_path("let pathFiles = 'data/20261022-update/';"), "data/20261022-update/")
  expect_equal(urban_path("x;const pathFiles =\"data/a-update/\""), "data/a-update/")
  expect_equal(urban_path("var pathFiles= 'data/a/' ;"), "data/a/")
  expect_null(urban_path("let pathFiles = '/etc/passwd';"))
  expect_null(urban_path("const pathFiles = 'data/../x/';"))
  expect_null(urban_path("var pathFiles = \"data/a';"))     # mismatched quotes

  out <- parse_urban_county(urban_fx())
  expect_named(out, c("GEOID", "card_del", "auto_del"))
  expect_equal(out$card_del[out$GEOID == "47065"], 5.44018, tolerance = 1e-6)
  expect_equal(out$card_del[out$GEOID == "47065"], 5.44, tolerance = 1e-3)
  expect_true(is.na(out$card_del[out$GEOID == "02013"]))
  expect_true(is.na(out$auto_del[out$GEOID == "02013"]))
  expect_true("09001" %in% out$GEOID)           # CT old counties are left as-is
  expect_type(out$card_del, "double")
})

test_that("Urban as-of comes from the update folder date", {
  expect_equal(urban_asof("data/20261022-update/"), "Oct 2026")
  expect_true(is.na(urban_asof("data/latest/")))
  expect_true(is.na(urban_asof(NULL)))
})

test_that("Urban build reads JS -> path -> csv, tags as-of, NULL on failure", {
  js <- paste(readLines(test_path("fixtures", "urban_main_snippet.js"), warn = FALSE),
              collapse = "\n")
  csv <- paste(readLines(test_path("fixtures", "urban_county_overall.csv"), warn = FALSE),
               collapse = "\n")
  seen <- character()
  get <- function(url) { seen <<- c(seen, url); if (grepl("[.]js$", url)) js else csv }
  out <- .urban_build(get)
  expect_equal(nrow(out), 5)
  expect_equal(attr(out, "asof"), "Oct 2026")
  expect_match(seen[2], "debt-interactive-map/data/20261022-update/county_overall.csv$")

  expect_message(bad <- .urban_build(function(url) "var nothing=1;"), "urban")
  expect_null(bad)
  expect_message(err <- .urban_build(function(url) stop("boom")), "boom")
  expect_null(err)
})

# ── FHFA HPI ─────────────────────────────────────────────────────────────────
fhfa_fx <- function(name) {
  readr::read_csv(test_path("fixtures", name), col_names = FALSE,
                  col_types = readr::cols(.default = "c"), na = character(), progress = FALSE)
}

test_that("HPI yoy uses same quarter a year earlier, skips dashes", {
  m <- hpi_yoy(fhfa_fx("fhfa_metro.csv"))
  expect_named(m, c("code", "hpi_yoy", "hpi_q"))
  expect_setequal(m$code, c("16860", "16984"))
  # 2026 Q2 414.74 vs 2025 Q2 406.94; the 1975 "-" row and row order are irrelevant
  expect_equal(m$hpi_yoy[m$code == "16860"], round(100 * (414.74 / 406.94 - 1), 2))
  expect_equal(m$hpi_yoy[m$code == "16860"], 1.92)
  expect_equal(m$hpi_q[m$code == "16860"], "2026 Q2")
  s <- hpi_yoy(fhfa_fx("fhfa_state.csv"))
  expect_setequal(s$code, c("TN", "GA", "IL", "CT"))
  expect_equal(s$hpi_yoy[s$code == "TN"], round(100 * (705.37 / 687.53 - 1), 2))

  # latest quarter is "-": fall back to the latest quarter with an index
  d <- data.frame(X1 = "A", X2 = "1", X3 = c("2025", "2025", "2026", "2026"),
                  X4 = c("1", "2", "1", "2"), X5 = c("100", "110", "121", "-"),
                  X6 = "( 1.0)", stringsAsFactors = FALSE)
  r <- hpi_yoy(d)
  expect_equal(r$hpi_q, "2026 Q1")
  expect_equal(r$hpi_yoy, 21)
  # no same quarter a year earlier: NA yoy, not a wrong number
  d2 <- d[d$X3 == "2026", ]
  expect_true(is.na(hpi_yoy(d2)$hpi_yoy))
})

hpi_fx <- function() {
  list(metro = hpi_yoy(fhfa_fx("fhfa_metro.csv")), state = hpi_yoy(fhfa_fx("fhfa_state.csv")))
}

test_that("FHFA uses metro division for divided CBSAs, state for micro/rural", {
  cb <- tibble::tibble(
    GEOID = c("17031", "13295", "47999", "09110", "13555", "13777"),
    cbsa_geoid = c("16980", "16860", "99001", NA, "99002", "99003"),
    cbsa_type = c("metro", "metro", "micro area", NA, "metro", "metro"),
    state_abbr = c("IL", "GA", "TN", "CT", "GA", "GA"))
  xw <- tibble::tibble(GEOID = c("17031", "13777"), msad_code = c("16984", "99999"))
  out <- county_hpi(cb, hpi_fx(), xw)
  expect_named(out, c("GEOID", "hpi_yoy", "hpi_source", "hpi_q"))
  expect_equal(out$GEOID, cb$GEOID)
  expect_equal(out$hpi_source,
               c("metro division", "metro", "state", "state", "state", "state"))
  chi <- hpi_fx()$metro
  expect_equal(out$hpi_yoy[1], chi$hpi_yoy[chi$code == "16984"])
  expect_equal(out$hpi_yoy[2], chi$hpi_yoy[chi$code == "16860"])
  expect_equal(out$hpi_yoy[3], round(100 * (705.37 / 687.53 - 1), 2))   # TN state
  expect_equal(out$hpi_q, rep("2026 Q2", 6))

  # no crosswalk: a divided metro absent from the file drops to state
  out2 <- county_hpi(cb, hpi_fx(), NULL)
  expect_equal(out2$hpi_source[1], "state")
  expect_equal(out2$hpi_yoy[1], round(100 * (565.87 / 533.90 - 1), 2))   # IL state
  # state missing too: NA with no source
  out3 <- county_hpi(cb[1, ], list(metro = hpi_fx()$metro, state = hpi_fx()$state[0, ]), NULL)
  expect_true(is.na(out3$hpi_yoy)); expect_true(is.na(out3$hpi_source))
})

test_that("county_hpi works under the app's scalar %||% and an sf input", {
  app_op <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
  assign("%||%", app_op, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  cb <- fx_county_base()
  out <- county_hpi(cb, hpi_fx(), NULL)
  expect_equal(out$hpi_source, c("metro", "state"))
})

# ── Metro-division crosswalk ─────────────────────────────────────────────────
test_that("msad_xwalk joins counties to metro divisions by point on surface", {
  counties <- sf::st_sf(GEOID = c("17031", "13295", "09110"),
                        geometry = sf::st_sfc(sq(-88, 41), sq(-85, 34), sq(-73, 41), crs = 4326))
  md <- sf::st_sf(CBSAFP = "16980", METDIVFP = "16984", GEOID = "1698016984",
                  geometry = sf::st_sfc(sq(-89, 40, 3), crs = 4326))
  out <- msad_xwalk(counties, md)
  expect_named(out, c("GEOID", "msad_code"))
  expect_equal(out$GEOID, "17031")
  expect_equal(out$msad_code, "16984")
})

test_that("msad build is NULL (with a log line) when tigris lacks metro_divisions", {
  expect_message(out <- .msad_build(fx_county_base(), NULL), "metro_divisions")
  expect_null(out)
  md <- sf::st_sf(CBSAFP = "16860", METDIVFP = "16861", GEOID = "1686016861",
                  geometry = sf::st_sfc(sq(-85.5, 33.5, 2), crs = 4326))
  got <- .msad_build(fx_county_base(), function() md)
  expect_equal(got$msad_code, "16861")
  expect_message(bad <- .msad_build(fx_county_base(), function() stop("offline")), "offline")
  expect_null(bad)
})
