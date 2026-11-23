suppressWarnings(suppressPackageStartupMessages({ library(glue); library(purrr) }))
source(file.path("..", "..", "R", "data_forward.R"), local = FALSE)
source(file.path("..", "..", "R", "data_news.R"), local = FALSE)
source(file.path("..", "..", "R", "ai_charts.R"), local = FALSE)
source(file.path("..", "..", "R", "llm_insights.R"), local = FALSE)

# Formatters and ticker labels live in global.R, which also starts the app; pull just those.
local({
  exprs <- parse(file.path("..", "..", "global.R"))
  want <- c("fmt_pct", "fmt_num", "fmt_dollar", "TICKER_LABELS")
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% want)
      eval(e, globalenv())
  }
})

test_that("insights prompt is forecast-first and carries forward context", {
  fwd <- list(signals = build_forward_signals(list(), kpis = list(t2yr = 3.5, fed_funds = 3.9)),
              calendar = data.frame(date = as.Date("2026-12-10"), event = "CPI"))
  p <- build_insights_prompt(kpis = list(unemp_rate = 4.4), forward_ctx = fwd,
                             today = as.Date("2026-11-23"))
  for (h in c("## Bottom Line", "## What Markets Price vs. Our View", "## Leading Signals",
              "## Themes Gaining Momentum", "## Catalyst Calendar", "## Scenarios", "## Calls to Track"))
    expect_match(p, h, fixed = TRUE)
  expect_match(p, "UPCOMING CATALYSTS", fixed = TRUE)
  expect_match(p, "CPI", fixed = TRUE)
  expect_match(p, "1.6 25bp cuts priced", fixed = TRUE)
  expect_false(grepl("## Previous Calls Review", p, fixed = TRUE))
})

test_that("previous calls are included and graded when present", {
  prev <- list(made_at = as.POSIXct("2026-11-01", tz = "UTC"), calls = "- **By 2027-01-31:** UNRATE above 4.5%")
  p <- build_insights_prompt(kpis = list(), prev_calls = prev)
  expect_match(p, "YOUR PREVIOUS CALLS (made 2026-11-01)", fixed = TRUE)
  expect_match(p, "UNRATE above 4.5%", fixed = TRUE)
  expect_match(p, "## Previous Calls Review", fixed = TRUE)
})

test_that("a model that just failed is skipped for a few minutes", {
  t0 <- as.POSIXct("2026-11-23 15:00", tz = "UTC")
  expect_false(llm_is_down("gemini", "test-model", t0))
  llm_mark_down("gemini", "test-model", t0)
  expect_true(llm_is_down("gemini", "test-model", t0 + 60))
  expect_false(llm_is_down("gemini", "test-model", t0 + 60 * (LLM_DOWN_MINUTES + 1)))
  expect_false(llm_is_down("groq", "test-model", t0 + 60))
})

test_that("metro prompt adds national forward context and local-impact section", {
  p <- build_metro_prompt(list(geography = list(type = "metro area")), "headline", "UPCOMING CATALYSTS x")
  expect_match(p, "National forward-looking context", fixed = TRUE)
  expect_match(p, "## National Themes, Local Impact", fixed = TRUE)
  expect_false(grepl("National forward-looking", build_metro_prompt(list(), "h"), fixed = TRUE))
})
