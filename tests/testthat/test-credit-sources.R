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
