# Synthetic monthly FRED panel 2000-01..2026-10 (no network).
sector_fixture <- function(drop_temphelps = TRUE) {
  dates <- seq(as.Date("2000-01-01"), as.Date("2026-10-01"), by = "month")
  n <- length(dates)
  ser <- function(id, v) tibble::tibble(series_id = id, date = dates, value = v)

  man <- rep(100, n)
  i0 <- which(dates == as.Date("2008-01-01")); i1 <- which(dates == as.Date("2009-06-01"))
  man[i0:i1] <- seq(100, 90, length.out = i1 - i0 + 1)
  i2 <- which(dates == as.Date("2012-06-01"))
  man[(i1 + 1):i2] <- seq(90, 100, length.out = i2 - i1 + 1)[-1]
  man[(i2 + 1):n] <- 100
  tail6 <- (n - 6):n                      # 6 months back from the last obs
  man[tail6] <- seq(100, 97, length.out = 7)

  ehs <- 100 * 1.02^((seq_len(n) - 1) / 12)
  rec <- as.numeric(dates >= as.Date("2008-01-01") & dates <= as.Date("2009-06-01"))

  others <- setdiff(RISK_SECTORS$fred, c("MANEMP", "USEHS"))
  out <- dplyr::bind_rows(
    ser("MANEMP", man), ser("USEHS", ehs), ser("USREC", rec),
    lapply(others, function(s) ser(s, rep(100, n)))
  )
  if (!drop_temphelps) out <- dplyr::bind_rows(out, ser("TEMPHELPS", rep(100, n)))
  out
}

test_that("RISK_SECTORS covers 13 groups and all C24030 detail lines", {
  expect_equal(nrow(RISK_SECTORS), 13)
  expect_length(intersect(RISK_SECTORS$acs_m, RISK_SECTORS$acs_f), 0)
  expect_false(anyDuplicated(c(RISK_SECTORS$acs_m, RISK_SECTORS$acs_f)) > 0)
  expect_true(all(c("USMINE", "USEHS") %in% RISK_SECTORS$fred))
  expect_equal(names(RISK_SECTORS), c("group", "label", "fred", "acs_m", "acs_f", "cns"))
})

test_that("momentum is annualized 6-month change", {
  dates <- seq(as.Date("2026-04-01"), as.Date("2026-10-01"), by = "month")
  f <- tibble::tibble(series_id = "X", date = dates, value = seq(100, 103, length.out = 7))
  m <- sector_momentum(f)
  expect_equal(m$m_pct, 100 * (1.03^2 - 1), tolerance = 0.01)
  expect_equal(m$ref_month, as.Date("2026-10-01"))
})

test_that("momentum is NA when the base month is missing", {
  f <- tibble::tibble(series_id = "X", date = as.Date(c("2026-08-01", "2026-10-01")),
                      value = c(100, 101))
  expect_true(is.na(sector_momentum(f)$m_pct))
})

test_that("recession episodes found from USREC", {
  ep <- recession_episodes(sector_fixture())
  expect_equal(nrow(ep), 1)
  expect_equal(ep$start, as.Date("2008-01-01"))
  expect_equal(ep$end, as.Date("2009-06-01"))
})

test_that("recession episodes split on gaps and honour `since`", {
  d <- seq(as.Date("1999-01-01"), as.Date("2021-12-01"), by = "month")
  on <- d %in% c(seq(as.Date("1999-03-01"), by = "month", length.out = 3),
                 as.Date(c("2001-04-01", "2001-05-01", "2020-03-01", "2020-04-01")))
  f <- tibble::tibble(series_id = "USREC", date = d, value = as.numeric(on))
  ep <- recession_episodes(f)
  expect_equal(ep$start, as.Date(c("2001-04-01", "2020-03-01")))
  expect_equal(ep$end, as.Date(c("2001-05-01", "2020-04-01")))
})

test_that("structural decline is peak to trough", {
  w <- sector_weights(sector_fixture())
  expect_lt(abs(w$structural[w$group == "manufacturing"] - 10), 0.5)
  expect_equal(w$structural[w$group == "educ_health"], 0, tolerance = 1e-6)
})

test_that("weights in 0..1, falling cyclical sector weighs most", {
  w <- sector_weights(sector_fixture())
  expect_equal(nrow(w), 13)
  expect_true(all(w$w >= 0 & w$w <= 1))
  expect_equal(w$w[w$group == "manufacturing"], max(w$w))
  expect_true(w$w[w$group == "educ_health"] < w$w[w$group == "manufacturing"])
  expect_true(all(is.finite(w$current_z)))            # flat series -> 0, not NaN
  expect_equal(w$current_z[w$group == "retail"], 0)
  expect_equal(attr(w, "ref_month"), as.Date("2026-10-01"))
})

test_that("missing TEMPHELPS is tolerated", {
  w <- expect_no_error(sector_weights(sector_fixture(drop_temphelps = TRUE)))
  expect_true(is.na(attr(w, "temphelp_m_pct")))
  w2 <- sector_weights(sector_fixture(drop_temphelps = FALSE))
  expect_equal(attr(w2, "temphelp_m_pct"), 0)
})

test_that("fetch_fred_sectors returns NULL without an API key", {
  withr_key <- Sys.getenv("FRED_API_KEY", unset = NA)
  on.exit(if (is.na(withr_key)) Sys.unsetenv("FRED_API_KEY") else Sys.setenv(FRED_API_KEY = withr_key))
  Sys.setenv(FRED_API_KEY = "")
  old <- options(risk.cache_dir = tempfile("riskcache"), risk.seed_dir = "")
  on.exit(options(old), add = TRUE)
  dir.create(getOption("risk.cache_dir"))
  risk_cache_clear_memory()
  expect_null(suppressMessages(fetch_fred_sectors()))
  risk_cache_clear_memory()
})

# fetch_fred_sectors with a fake per-series pull (no network, throwaway cache).
fred_with <- function(pull) {
  key <- Sys.getenv("FRED_API_KEY", unset = NA)
  old <- options(risk.cache_dir = tempfile("riskcache"), risk.seed_dir = "")
  on.exit({
    options(old)
    if (is.na(key)) Sys.unsetenv("FRED_API_KEY") else Sys.setenv(FRED_API_KEY = key)
    risk_cache_clear_memory()
  })
  dir.create(getOption("risk.cache_dir"))
  Sys.setenv(FRED_API_KEY = "test-key")
  risk_cache_clear_memory()
  suppressMessages(fetch_fred_sectors(pull = pull))
}
fake_pull <- function(fail = character(0), empty = character(0)) {
  function(s, start) {
    if (s %in% fail) stop("boom")
    if (s %in% empty) return(tibble::tibble(series_id = character(), date = as.Date(character()),
                                            value = numeric()))
    tibble::tibble(series_id = s, date = as.Date(c("2026-09-01", "2026-10-01")), value = c(1, 2))
  }
}

test_that("fetch_fred_sectors binds every series when all pulls succeed", {
  out <- fred_with(fake_pull())
  expect_setequal(unique(out$series_id), c(RISK_SECTORS$fred, "USREC", "TEMPHELPS"))
})

test_that("a failed required series makes the whole pull NULL; TEMPHELPS is optional", {
  expect_null(fred_with(fake_pull(fail = "USMINE")))
  expect_null(fred_with(fake_pull(fail = "USREC")))
  out <- fred_with(fake_pull(fail = "TEMPHELPS"))
  expect_false("TEMPHELPS" %in% out$series_id)
})

test_that("a 0-row required series counts as failed, not as zero momentum", {
  expect_null(fred_with(fake_pull(empty = "USCONS")))
  expect_null(fred_with(fake_pull(empty = "USREC")))
})

test_that("a sector with no momentum has NA current_z, not 0, and a finite weight", {
  f <- sector_fixture()
  # Drop USCONS' base month (6 back from the last obs) so its momentum is NA.
  f <- f[!(f$series_id == "USCONS" & f$date == as.Date("2026-04-01")), ]
  w <- sector_weights(f)
  expect_true(is.na(w$m_pct[w$group == "construction"]))
  expect_true(is.na(w$current_z[w$group == "construction"]))
  expect_equal(w$current_z[w$group == "retail"], 0)    # real momentum, no spread: still 0
  expect_true(all(is.finite(w$w)))
})
