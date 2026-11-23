library(testthat)
assign("FORECAST_SERIES", list(UNRATE = list(name = "Unemployment Rate", unit = "%")), envir = globalenv())
source(file.path("..", "..", "R", "export.R"), local = FALSE)

fake_fc <- function() {
  list(UNRATE = list(data = data.frame(
    ds = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
    y = c(4.1, 4.2, NA), yhat = c(4.1, 4.2, 4.3),
    yhat_lower = c(NA, NA, 4.0), yhat_upper = c(NA, NA, 4.6),
    is_forecast = c(FALSE, FALSE, TRUE)
  )))
}

test_that("forecast series is long with actual/forecast split", {
  d <- export_forecast_series(fake_fc())
  expect_equal(nrow(d), 3)
  expect_equal(d$type, c("actual", "actual", "forecast"))
  expect_true(is.na(d$forecast[1]))
  expect_equal(d$upper[3], 4.6)
})

test_that("news export keeps title, source and url", {
  n <- data.frame(title = "T", source_name = "S", url = "http://x", published = Sys.time(),
                  description = "d", relevance_score = 3, hours_ago = 2)
  d <- export_news(n)
  expect_true(all(c("title", "source_name", "url") %in% names(d)))
  expect_false("hours_ago" %in% names(d))
  expect_null(export_news(NULL))
})

test_that("outlook summary and drivers handle a real-shaped outlook and errors", {
  o <- list(score = 6.2, regime = list(label = "Expansion"), as_of = Sys.Date(),
            recession_prob = list(prob = 21.5, tier = list(label = "Moderate"),
                                  model_type = "Ridge", base_rate = 0.1, train_end = as.Date("2025-12-01"),
                                  factor_contributions = data.frame(
                                    name = c("a", "b"), raw_value = c(1, 2), z_score = c(.5, -1),
                                    beta = c(.2, .3), contribution = c(.1, -.3), data_missing = c(FALSE, FALSE))),
            gdp_forecast = list(point = 2.1, ci_lo = 0.5, ci_hi = 3.7, method = "m"),
            gdpnow = NULL,
            components = list(labor = list(label = "Labor", weight = 2.5, score = 0.4, detail = "x")))
  s <- export_outlook_summary(o)
  expect_equal(s$value[s$metric == "Recession probability"], "21.5")
  expect_false("GDPNow" %in% s$metric)
  expect_equal(export_recession_drivers(o)$predictor, c("b", "a"))
  expect_equal(nrow(export_outlook_components(o)), 1)
  expect_null(export_outlook_summary(list(.error = TRUE)))
  expect_null(export_outlook_summary(NULL))
})

test_that("ai report is NULL when nothing generated; renders tables, escapes scripts", {
  expect_null(export_ai_html())
  h <- export_ai_html(
    insights = "## Hi

| a | b |
|---|---|
| **1** | 2 |

<script>alert(1)</script>",
    chat = list(list(role = "user", content = "Q <b>?"), list(role = "assistant", content = "A.")),
    snapshot = data.frame(metric = c("Recession probability", "Regime"), value = c("10.9", "Expansion"),
                          unit = c("%", ""), stringsAsFactors = FALSE),
    provider = "gemini", model = "m")
  expect_match(h, "<table>", fixed = TRUE)
  expect_false(grepl("<script>alert", h, fixed = TRUE))
  expect_match(h, "Recession probability", fixed = TRUE)
  expect_match(h, "Q &lt;b&gt;?", fixed = TRUE)
  expect_match(h, "gemini / m", fixed = TRUE)
})

test_that("export_sheets drops empties; workbook writes and reads back", {
  skip_if_not_installed("openxlsx")
  s <- export_sheets(news = data.frame(title = "T", url = "http://x.test", description = "d"),
                     outlook = list(score = 6.2, regime = list(label = "Expansion"), as_of = Sys.Date(),
                                    recession_prob = list(prob = 10.9, tier = list(label = "Low")),
                                    gdp_forecast = list(point = 2.1, ci_lo = .5, ci_hi = 3.7, method = "m")))
  expect_equal(names(s), c("Outlook summary", "News"))
  f <- tempfile(fileext = ".xlsx")
  export_workbook(s, f)
  expect_equal(openxlsx::getSheetNames(f), c("Contents", "Outlook summary", "News"))
  # The mixed value column must hold real numbers for numeric rows, not text.
  d <- tempfile(); utils::unzip(f, exdir = d)
  xml <- paste(readLines(file.path(d, "xl", "worksheets", "sheet2.xml"), warn = FALSE), collapse = "")
  cell <- regmatches(xml, regexpr('<c r="B4"[^>]*>', xml))
  expect_false(grepl('t="s"|inlineStr|t="str"', cell))
  expect_equal(export_slug("Forecast series"), "forecast-series")
})
