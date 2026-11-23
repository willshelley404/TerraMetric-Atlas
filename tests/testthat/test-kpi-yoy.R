suppressWarnings(suppressPackageStartupMessages({ library(lubridate); library(glue); library(purrr) }))
source(file.path("..", "..", "R", "data_fred.R"), local = FALSE)
source(file.path("..", "..", "R", "llm_insights.R"), local = FALSE)

# Monthly index rising 0.25%/month, with Oct 2025 missing (the 2025 shutdown gap)
cpi_fx <- function() {
  d <- seq(as.Date("2023-01-01"), as.Date("2026-10-01"), by = "month")
  df <- data.frame(date = d, value = 300 * 1.0025^(seq_along(d) - 1))
  df[df$date != as.Date("2025-10-01"), ]
}

test_that("YoY skips back to the latest month with a true year-ago match", {
  v <- yoy_val(list(CPI = cpi_fx()), "CPI")
  expect_equal(attr(v, "as_of"), as.Date("2026-09-01"))
  expect_equal(as.numeric(v), 100 * (1.0025^12 - 1), tolerance = 1e-9)   # a 12-month change, not 13
  full <- cpi_fx(); full <- rbind(full, data.frame(date = as.Date("2025-10-01"), value = 1))
  expect_equal(attr(yoy_val(list(CPI = full), "CPI"), "as_of"), as.Date("2026-10-01"))
  expect_true(is.na(yoy_val(list(CPI = cpi_fx()[1:5, ]), "CPI")))
})

test_that("daily series still match within a week; GDP keeps its old quarterly behavior", {
  d <- seq(as.Date("2024-11-01"), as.Date("2025-11-21"), by = "day")
  d <- d[!format(d, "%u") %in% c("6", "7")]                              # weekdays only
  oil <- data.frame(date = d, value = seq(70, 90, length.out = length(d)))
  expect_false(is.na(yoy_val(list(OIL = oil), "OIL")))
  gdp <- data.frame(date = seq(as.Date("2024-01-01"), by = "quarter", length.out = 8), value = 100 + 0:7)
  expect_false(is.na(yoy_val(list(GDPC1 = gdp), "GDPC1", lag_months = 4, tol_days = 60, max_back = 0)))
})

test_that("build_kpis keeps values numeric and reports the reference month", {
  k <- build_kpis(list(CPIAUCNS = cpi_fx(), CPILFENS = cpi_fx()))
  expect_null(attributes(k$cpi_yoy))
  expect_equal(k$cpi_yoy_asof, as.Date("2026-09-01"))
  expect_null(k$ppi_yoy_asof)
  expect_equal(.kpi_month(k$cpi_yoy_asof), " (Sep 2026)")
  expect_equal(.kpi_month(NULL), "")
})

test_that("1-year market returns come from the close history", {
  d <- seq(as.Date("2025-01-02"), as.Date("2026-11-20"), by = "day")
  m <- data.frame(symbol = "SMH", date = d, close = seq(100, 200, length.out = length(d)))
  last <- m$close[nrow(m)]; prior <- m$close[m$date == as.Date("2025-11-20")]
  expect_equal(ret_1y(m, "SMH"), 100 * (last / prior - 1))
  expect_true(is.na(ret_1y(m[m$date > as.Date("2026-06-01"), ], "SMH")))   # under a year of history
  expect_true(is.na(ret_1y(NULL, "SMH")))
})
