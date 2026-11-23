source(file.path("..", "..", "R", "data_forward.R"), local = FALSE)
source(file.path("..", "..", "R", "ai_charts.R"), local = FALSE)
source(file.path("..", "..", "R", "export.R"), local = FALSE)

today <- as.Date("2026-11-23")
monthly <- function(values, end = as.Date("2026-10-01")) {
  dates <- seq(end, by = "-1 month", length.out = length(values))
  data.frame(date = rev(dates), value = values)
}
fx_data <- list(
  fred = list(
    CPIAUCNS = monthly(100 * 1.003^(0:47)),          # ~3.66% YoY every month
    UNRATE   = monthly(seq(3.8, 4.6, length.out = 48)),
    ICSA     = monthly(seq(200000, 240000, length.out = 48)),
    PAYEMS   = monthly(150000 + 100 * (0:47)),
    PERMIT   = monthly(seq(1500, 1295, length.out = 48)),
    GDPC1    = data.frame(date = seq(as.Date("2023-01-01"), by = "quarter", length.out = 15),
                          value = 20000 * 1.005^(0:14))
  ),
  fwd = list(T5YIE = monthly(seq(2.4, 2.18, length.out = 36)))
)

test_that("series transforms come from the loaded data", {
  cpi <- chart_series_data("cpi_yoy", fx_data, years = 3, today = today)
  expect_equal(round(tail(cpi$value, 1), 2), round(100 * (1.003^12 - 1), 2))
  expect_true(min(cpi$date) >= today - round(365.25 * 3))
  expect_equal(tail(chart_series_data("init_claims", fx_data, today = today)$value, 1), 240)
  expect_equal(tail(chart_series_data("payrolls_chg", fx_data, today = today)$value, 1), 100)
  expect_equal(round(tail(chart_series_data("gdp_saar", fx_data, today = today)$value, 1), 2),
               round(100 * (1.005^4 - 1), 2))
  expect_equal(tail(chart_series_data("breakeven_5y", fx_data, today = today)$value, 1), 2.18)
  expect_null(chart_series_data("nfci", fx_data, today = today))     # not loaded
  expect_null(chart_series_data("nope", fx_data, today = today))
})

calls_text <- paste0(
  "## Bottom Line\nx\n\n## Calls to Track\n",
  '- **By Dec 10:** CPI YoY above 3.5% (now 3.66%) — confidence: high. [chart: {"series": "cpi_yoy", "threshold": 3.5, "by": "2026-12-10"}]\n',
  '- **By Jan 9, 2027:** Unemployment rate between 4.5% and 4.8% (now 4.6%) — confidence: medium. [chart: {"type": "scatter", "x": "init_claims", "y": "unemp_rate"}]\n',
  "- **By Dec 23:** Building permits below 1.20M ann. (now 1,295K) — confidence: medium.\n",
  '- **By Dec 31:** Something unchartable — confidence: medium. [chart: {"series": "made_up"}]\n')

test_that("chart tags are parsed, validated, and fall back to the metric the call names", {
  pc <- parse_call_charts(calls_text)
  expect_length(pc, 4)
  expect_equal(pc[[1]]$spec$series, "cpi_yoy")
  expect_false(grepl("[chart:", pc[[1]]$call, fixed = TRUE))
  expect_false(grepl("**", pc[[1]]$call, fixed = TRUE))

  s1 <- resolve_chart_spec(pc[[1]]$spec, pc[[1]]$call, today)
  expect_equal(s1[c("series", "threshold")], list(series = "cpi_yoy", threshold = 3.5))
  expect_equal(s1$by, as.Date("2026-12-10"))
  expect_equal(resolve_chart_spec(pc[[2]]$spec, pc[[2]]$call, today)$type, "scatter")

  s3 <- resolve_chart_spec(pc[[3]]$spec, pc[[3]]$call, today)      # no tag -> from the text
  expect_equal(s3$series, "permits")
  expect_equal(s3$by, as.Date("2026-12-23"))
  expect_equal(s3$threshold, 1.2)
  expect_null(resolve_chart_spec(pc[[4]]$spec, pc[[4]]$call, today))

  expect_equal(.parse_call_thresholds("between 4.5% and 4.8%"), c(4.5, 4.8))
  expect_equal(.parse_call_date("By Jan 9, 2027: x", today), as.Date("2027-01-09"))
  expect_equal(.parse_call_date("By Jan 9: x", today), as.Date("2027-01-09"))   # rolls to next year
  expect_true(is.na(.parse_call_date("no date here", today)))
  expect_equal(.match_series("core cpi to cool"), "core_cpi_yoy")
})

test_that("charts are built from data, off-scale thresholds dropped", {
  ch <- build_call_charts(calls_text, fx_data, today)
  expect_length(ch, 3)
  expect_equal(ch[[3]]$spec$series, "permits")
  expect_length(ch[[3]]$spec$threshold, 0)          # 1.2 vs a series in thousands
  expect_named(ch[[2]]$data, c("date", "x", "y"))
  expect_length(build_call_charts("## Bottom Line\nno calls", fx_data, today), 0)
})

test_that("SVG renders line and scatter charts without bad numbers", {
  ch <- build_call_charts(calls_text, fx_data, today)
  line <- ai_chart_svg(ch[[1]])
  expect_match(line, "<polyline", fixed = TRUE)
  expect_match(line, "stroke-dasharray=\"5 4\"", fixed = TRUE)
  expect_match(line, "by Dec 10", fixed = TRUE)
  sc <- ai_chart_svg(ch[[2]])
  expect_match(sc, "<circle", fixed = TRUE)
  for (s in c(line, sc)) expect_false(grepl("NaN|NA,|Inf", s))
  yl <- regmatches(line, gregexpr('text-anchor="end" fill="var\\(--mut\\)">[0-9.-]+<', line))[[1]]
  expect_gt(length(unique(yl)), 3)   # flat series still gets distinct y tick labels
})

test_that("plotly charts build for the app", {
  skip_if_not_installed("plotly")
  suppressWarnings(suppressPackageStartupMessages(library(plotly)))
  local({
    for (e in parse(file.path("..", "..", "global.R")))
      if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c("dark_theme", "DATE_RANGE_JS"))
        eval(e, globalenv())
  })
  ch <- build_call_charts(calls_text, fx_data, today)
  for (c in ch) expect_s3_class(ai_chart_plotly(c), "plotly")
})

test_that("tags are stripped for readers and the export embeds the charts", {
  expect_false(grepl("[chart:", strip_chart_tags(calls_text), fixed = TRUE))
  expect_match(strip_chart_tags(calls_text), "confidence: high.\n", fixed = TRUE)
  ch <- build_call_charts(calls_text, fx_data, today)
  html <- export_ai_html(insights = strip_chart_tags(calls_text),
                         charts = vapply(ch, ai_chart_svg, ""))
  expect_match(html, "<b>Evidence.</b>", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<figure class=\"chart\"", html))), 3)
  expect_false(grepl("<b>Evidence.</b>", export_ai_html(insights = "## x"), fixed = TRUE))
})

u <- function(...) intToUtf8(c(...), multiple = FALSE)
nbh <- u(0x2011); nnb <- u(0x202F)   # non-breaking hyphen, narrow no-break space (GPT-OSS output)

test_that("typographic hyphens and spaces don't break dates, names or thresholds", {
  call <- paste0("By 2026", nbh, "12", nbh, "09: 10Y", nbh, "2Y Treasury spread ", u(0x2265),
                 " 0.25", nnb, "pp (now 0.29", nnb, "pp)")
  s <- resolve_chart_spec(NULL, call, today)
  expect_equal(s$series, "t10y2y")
  expect_equal(s$by, as.Date("2026-12-09"))
  expect_equal(s$threshold, 0.25)
  expect_equal(.parse_call_thresholds(paste0("Job openings (JOLTS) > 7,500", nnb, "K (now 7,523", nnb, "K)")), 7500)
  expect_equal(.parse_call_thresholds("Continuing claims < 1,650K"), 1650)
  expect_equal(.parse_call_thresholds(paste0("CPI YoY between 3.8", nnb, "% and 4.0", nnb, "%")), c(3.8, 4.0))
  expect_equal(.match_series("2-year Treasury yield above 4.5%"), "t2yr")
})

test_that("tags with arrays, backticks and smart quotes still parse", {
  txt <- paste0("## Calls to Track\n",
                '- **By 2026-12-10:** CPI between 3.5% and 3.8% `[chart: {"series": "cpi_yoy", "threshold": [3.5, 3.8]}]`\n',
                "- **By 2026-12-10:** CPI above 3.5% [chart: {", u(0x201C), "series", u(0x201D), ": ",
                u(0x201C), "cpi_yoy", u(0x201D), "}]\n")
  pc <- parse_call_charts(txt)
  expect_equal(pc[[1]]$spec$threshold, c(3.5, 3.8))
  expect_equal(pc[[2]]$spec$series, "cpi_yoy")
  expect_false(grepl("chart:|`", strip_chart_tags(txt)))
})

theme_text <- paste0(
  "## Themes Gaining Momentum\n",
  '- **AI data centers** (46 articles, rising): power demand lifts electricity prices. [chart: {"series": "smh"}]\n',
  "- **Housing** (3 articles, steady): mortgage rates bite.\n\n",
  "## Calls to Track\n",
  '- **By 2026-12-10:** CPI YoY above 3.5% (now 3.66%) [chart: {"series": "cpi_yoy", "threshold": 3.5, "by": "2026-12-10"}]\n')
fx_mkt <- data.frame(symbol = "SMH", date = seq(as.Date("2024-01-02"), by = "week", length.out = 150),
                     close = seq(200, 320, length.out = 150))

test_that("theme charts use only tagged items, including market prices indexed to 100", {
  d <- c(fx_data, list(mkt = fx_mkt))
  th <- build_theme_charts(theme_text, d, today)
  expect_length(th, 1)                                   # untagged Housing theme gets no guessed chart
  expect_equal(th[[1]]$spec$series, "smh")
  expect_equal(th[[1]]$data$value[1], 100)
  table_text <- paste0("## Themes Gaining Momentum\n| Theme | Transmission |\n|---|---|\n",
                       '| **Chips** | export controls [chart: {"series": "smh"}] |\n')
  tt <- build_theme_charts(table_text, d, today)
  expect_length(tt, 1)
  expect_match(tt[[1]]$call, "Chips — export controls", fixed = TRUE)
})

test_that("evidence blocks render under their own sections", {
  skip_if_not_installed("plotly")
  source(file.path("..", "..", "R", "data_news.R"), local = FALSE)
  news <- data.frame(title = c("AI data center boom", "AI data center boom 2", "Data center power", "Tariff talk"),
                     description = "", hours_ago = c(1, 2, 60, 5), relevance_score = 1)
  news$themes <- news_themes(news$title, news$description)
  ev <- build_insight_evidence(theme_text, c(fx_data, list(mkt = fx_mkt)), news, today)
  expect_false(is.null(ev$themes$coverage))
  expect_length(ev$calls, 1)
  html <- as.character(htmltools::renderTags(insights_ui(theme_text, ev))$html)
  themes_at <- regexpr("Themes Gaining Momentum", html); calls_at <- regexpr("Calls to Track", html)
  ev_at <- gregexpr("ai-evidence", html)[[1]]
  expect_length(ev_at, 2)
  expect_true(ev_at[1] > themes_at && ev_at[1] < calls_at && ev_at[2] > calls_at)
  expect_match(theme_coverage_svg(ev$themes$coverage), "Headline coverage by theme", fixed = TRUE)

  out <- export_ai_html(insights = strip_chart_tags(theme_text),
                        charts = list(themes = theme_coverage_svg(ev$themes$coverage),
                                      calls = vapply(ev$calls, ai_chart_svg, "")))
  expect_true(regexpr("Headline coverage", out) < regexpr("Calls to Track", out))
  expect_true(regexpr("<figure class=\"chart\"><div class=\"ct\">CPI", out) > regexpr("Calls to Track", out))
})

test_that("evidence survives the project-wide %||% and untagged themes use a named series id", {
  # R/synopsis_tier3.R defines this %||%, which turns lists of lists into its fallback
  assign("%||%", function(a, b) {
    if (is.null(a)) return(b)
    ok <- tryCatch(!is.na(a[[1]]), error = function(e) TRUE)
    if (isTRUE(ok) && length(a) > 0) a else b
  }, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  txt <- paste0("## Themes Gaining Momentum\n",
                "- **Chips** (23 articles, steady): export curbs; watch the **smh** index.\n",
                "- **Housing** (3 articles, steady): mortgage rates bite.\n\n",
                "## Calls to Track\n",
                '- **By 2026-12-10:** CPI YoY above 3.5% `[chart: {"series": "cpi_yoy", "threshold": 3.5}]`\n',
                '- **By 2026-12-23:** Building permits below 1,300K `[chart: {"series": "permits", "threshold": 1300}]`\n')
  ev <- build_insight_evidence(txt, c(fx_data, list(mkt = fx_mkt)), NULL, today)
  expect_length(ev$calls, 2)
  expect_length(ev$themes$charts, 1)
  expect_equal(ev$themes$charts[[1]]$spec$series, "smh")
  expect_equal(.id_mention("the ppi_yoy series and permits"), "ppi_yoy")
  expect_null(.id_mention("no series named here"))
})
