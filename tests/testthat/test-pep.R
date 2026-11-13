pep_fixture <- function() {
  readr::read_csv(test_path("fixtures", "pep_sample.csv"),
                  col_types = readr::cols(.default = "c"),
                  locale = readr::locale(encoding = "latin1"), progress = FALSE)
}

test_that("parses counties only, keeps CT planning regions", {
  out <- parse_pep_county(pep_fixture(), 2025)
  expect_equal(nrow(out), 3)
  expect_true("09110" %in% out$GEOID)
  expect_true("47" %in% substr(out$GEOID, 1, 2))
  expect_false("47000" %in% out$GEOID)
  expect_equal(attr(out, "vintage"), 2025)
})

test_that("latest-year rates and cumulative since 2020", {
  out <- parse_pep_county(pep_fixture(), 2025)
  h <- out[out$GEOID == "47065", ]
  expect_equal(h$mig_dom_rate, 5.5)
  expect_equal(h$mig_net_rate, 7.88)
  expect_equal(h$mig_intl_rate, 2.38)
  expect_equal(h$nat_chg_rate, 1.22)
  # 1000 * (1500 + 5005 + 3807 + 3033 + 2140) / 366840
  expect_equal(h$mig_dom_cum, round(1000 * 15485 / 366840, 2))
  expect_equal(h$pop_chg_pct, round(100 * (390833 / 366840 - 1), 2))
  expect_equal(h$mig_dom_n, 2140)
  expect_equal(h$pop_avg, round(mean(c(387268, 390833)), 2))
  expect_true(all(paste0("mig_dom_rate_", 2021:2025) %in% names(out)))
  expect_equal(h$mig_dom_rate_2022, 13.43)
  expect_true(all(vapply(out[setdiff(names(out), "GEOID")], is.numeric, logical(1))))
  expect_equal(out$mig_dom_rate[out$GEOID == "09110"], -0.11)
})

test_that("latin1 county names do not break the read", {
  raw <- pep_fixture()
  expect_true(any(raw$CTYNAME == "Do\u00f1a Ana County"))
})

test_that("HTML 200 is not a vintage", {
  html <- httr2::response(200, headers = list("content-type" = "text/html"),
                          body = charToRaw("<html>"))
  csv <- httr2::response(200, headers = list("content-type" = "text/csv"),
                         body = charToRaw("SUMLEV,STATE\n040,01\n"))
  csv_nohdr <- httr2::response(200, headers = list("content-type" = "text/csv"),
                               body = charToRaw("a,b\n1,2\n"))
  not_found <- httr2::response(404, headers = list("content-type" = "text/csv"),
                               body = charToRaw("SUMLEV"))
  expect_false(.pep_resp_ok(html))
  expect_true(.pep_resp_ok(csv))
  expect_false(.pep_resp_ok(csv_nohdr))
  expect_false(.pep_resp_ok(not_found))
  risk_cache_clear_memory()
  d <- tempfile("pep"); dir.create(d)
  expect_equal(risk_pep_vintage(probe = function(v) v == 2025,
                                today = as.Date("2026-11-12"), cache_dir = d), 2025L)
})

test_that("no vintage found returns NULL and is not cached", {
  risk_cache_clear_memory()
  d <- tempfile("pep"); dir.create(d)
  expect_null(risk_pep_vintage(probe = function(v) FALSE,
                               today = as.Date("2026-11-12"), cache_dir = d))
  expect_length(list.files(d), 0)
  expect_equal(risk_pep_vintage(probe = function(v) v == 2024,
                                today = as.Date("2026-11-12"), cache_dir = d), 2024L)
})

test_that("pep url and year label", {
  expect_equal(.pep_url(2025),
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/totals/co-est2025-alldata.csv")
  expect_equal(pep_year_label(2025), "2024\u201325")
  expect_equal(pep_year_label(2000), "1999\u201300")
})
