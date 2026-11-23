source(file.path("..", "..", "R", "data_forward.R"), local = FALSE)

series <- function(dates, values) data.frame(date = as.Date(dates), value = values)

test_that("forward signals compute levels, changes and the implied policy gap", {
  fwd <- list(
    T5YIE = series(c("2026-08-20", "2026-11-20"), c(2.34, 2.18)),
    T5YIFR = series("2026-11-20", 2.12),
    SAHMREALTIME = series(c("2026-04-01", "2026-10-01"), c(0.13, 0)),
    NFCI = series(c("2026-08-14", "2026-11-13"), c(-0.55, -0.48)),
    DRTSCILM = series(c("2026-07-01", "2026-10-01"), c(0, 1.7)),
    CCSA = series(c("2025-11-08", "2026-11-07"), c(1949000, 1763000))
  )
  s <- build_forward_signals(fwd, kpis = list(t2yr = 3.55, fed_funds = 3.88))
  expect_equal(s$breakeven_5y, 2.18)
  expect_equal(s$breakeven_5y_chg_3m, -0.16)
  expect_equal(s$two_year_minus_ff, -0.33)
  expect_equal(s$sloos_ci_prev, 0)
  expect_equal(s$cont_claims_chg_yoy_k, -186)
  expect_true(is.na(s$permits))

  txt <- forward_signals_to_text(s)
  expect_match(txt, "1.3 25bp cuts priced", fixed = TRUE)
  expect_match(txt, "Building permits:           N/A", fixed = TRUE)
  expect_false(grepl("NA%|NaN", txt))
})

test_that("missing series degrade to N/A rather than erroring", {
  s <- build_forward_signals(list(), kpis = NULL)
  txt <- forward_signals_to_text(s)
  expect_match(txt, "5-yr breakeven inflation:   N/A", fixed = TRUE)
  expect_match(forward_signals_to_text(NULL), "Unavailable", fixed = TRUE)
})

test_that("release calendar merges FRED releases with FOMC decisions and minutes", {
  today <- as.Date("2026-11-23")
  fake <- function(id, from, to) {
    switch(id,
      "10"  = as.Date("2026-12-10"),
      "53"  = as.Date("2026-12-23"),
      "180" = as.Date(c("2026-12-03", "2026-11-25", "2026-12-10")),
      "50"  = stop("boom"),
      as.Date(character()))
  }
  fomc <- data.frame(date = as.Date(c("2026-10-28", "2026-12-09")), sep = c(FALSE, TRUE))
  cal <- suppressMessages(fetch_release_calendar(45, today, fake, fomc = fomc))
  expect_equal(cal$event, c("Weekly jobless claims", "FOMC rate decision + new dot plot", "CPI",
                            "GDP (Q3 2026 estimate)", "FOMC minutes (Dec 09 meeting)"))
  expect_equal(cal$date, as.Date(c("2026-11-25", "2026-12-09", "2026-12-10", "2026-12-23", "2026-12-30")))
  txt <- calendar_to_text(cal, today)
  expect_match(txt, "Thu Dec 10 (+17 days)  CPI", fixed = TRUE)
  expect_false(grepl("FOMC meeting dates unavailable", txt, fixed = TRUE))
  expect_match(calendar_to_text(NULL, today), "unavailable", fixed = TRUE)

  no_fomc <- suppressMessages(fetch_release_calendar(45, today, fake, fomc = NULL))
  expect_false(any(grepl("FOMC", no_fomc$event)))
  expect_match(calendar_to_text(no_fomc, today), "FOMC meeting dates unavailable", fixed = TRUE)
})

test_that("GDP releases are labelled with the quarter they report", {
  expect_equal(gdp_release_quarter(as.Date("2026-11-25")), "Q3 2026")
  expect_equal(gdp_release_quarter(as.Date("2026-12-23")), "Q3 2026")
  expect_equal(gdp_release_quarter(as.Date("2027-01-28")), "Q4 2026")
  expect_equal(gdp_release_quarter(as.Date("2027-04-29")), "Q1 2027")
})

test_that("FOMC calendar page parses decision days, projections and cross-month meetings", {
  cell <- function(mon, day) sprintf(paste0(
    '<div class="fomc-meeting__month col-xs-5"><strong>%s</strong></div>\n',
    '<div class="fomc-meeting__date col-xs-4">%s</div>\n<div>statement</div>\n'), mon, day)
  html <- paste0(
    '<h4><a id="1">2027 FOMC Meetings</a></h4>', cell("January", "26-27"), cell("March", "16-17*"),
    cell("Apr/May", "30-1"),
    '<h4><a id="2">2026 FOMC Meetings</a></h4>', cell("August", "22 (notation vote)"),
    cell("December", "8-9*"), cell("October", "31-1"))
  m <- parse_fomc_calendar(html)
  expect_equal(m$date, as.Date(c("2026-11-01", "2026-12-09", "2027-01-27", "2027-03-17", "2027-05-01")))
  expect_equal(m$sep, c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(nrow(parse_fomc_calendar("<html>nothing</html>")), 0)
  expect_error(fetch_fomc_meetings(function() "<html></html>"), "no meetings")
})

test_that("FOMC meetings are cached and a failed refresh serves the last copy", {
  dir <- tempfile(); dir.create(dir)
  risk_cache_clear_memory()
  good <- function() data.frame(date = as.Date("2026-12-09"), sep = TRUE)
  expect_equal(nrow(fomc_meetings(dir, good)), 1)
  risk_cache_clear_memory()
  later <- risk_cached("fomc_meetings", 7, function() stop("down"), dir,
                       now = Sys.time() + 86400 * 10, seed_dir = NULL)
  expect_equal(later$date, as.Date("2026-12-09"))
  expect_true(attr(later, "stale"))
})

test_that("calls log extracts the section and only returns calls old enough to grade", {
  text <- paste0("## Bottom Line\nx\n\n## Calls to Track\n- **By 2027-01-31:** UNRATE above 4.5%\n",
                 "- **By 2027-03-31:** CPI below 2.8%\n")
  expect_equal(extract_calls_section(text),
               "- **By 2027-01-31:** UNRATE above 4.5%\n- **By 2027-03-31:** CPI below 2.8%")
  expect_null(extract_calls_section("## Bottom Line\nno calls"))
  mid <- "## Calls to Track\n- a\n## Previous Calls Review\n- b"
  expect_equal(extract_calls_section(mid), "- a")

  path <- tempfile(fileext = ".rds")
  t0 <- as.POSIXct("2026-11-01 12:00", tz = "UTC")
  expect_true(record_llm_calls(text, path, now = t0))
  expect_false(record_llm_calls("no section", path, now = t0))
  record_llm_calls("## Calls to Track\n- newer", path, now = t0 + 86400 * 20)

  expect_null(previous_llm_calls(path, now = t0 + 86400))           # too fresh
  got <- previous_llm_calls(path, now = t0 + 86400 * 21)            # newest is 1 day old -> older one
  expect_match(got$calls, "UNRATE above 4.5%", fixed = TRUE)
  got <- previous_llm_calls(path, now = t0 + 86400 * 25)
  expect_equal(got$calls, "- newer")
  expect_null(previous_llm_calls(tempfile()))
})
