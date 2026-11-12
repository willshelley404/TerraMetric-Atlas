# ── Parser ───────────────────────────────────────────────────────────────────
test_that("parser keeps monthly numeric rows and extracts FIPS", {
  j <- jsonlite::read_json(test_path("fixtures", "bls_laus_sample.json"))
  x <- parse_bls_laus(j$Results$series)
  expect_equal(nrow(x), 2)
  expect_equal(x$fips, c("13095", "09110"))
  expect_equal(x$date, as.Date(c("2026-09-01", "2026-09-01")))
  expect_equal(x$rate, c(4.6, 3.7))
})

test_that("series ids are LAUCN + fips + 0000000003", {
  expect_equal(laus_series_ids(c("13295", "09110")),
               c("LAUCN132950000000003", "LAUCN091100000000003"))
})

# ── Momentum ─────────────────────────────────────────────────────────────────
months <- seq(as.Date("2025-07-01"), as.Date("2026-09-01"), by = "month")  # 15 months

county_series <- function(fips, yr_ago, now, mid = 4.3, drop = as.Date(character())) {
  r <- rep(mid, length(months))
  r[1:3] <- yr_ago
  r[13:15] <- now
  d <- data.frame(fips = fips, date = months, rate = r)
  d[!d$date %in% drop, ]
}

laus_fixture <- function() {
  filler <- do.call(rbind, lapply(sprintf("99%03d", 1:20), county_series, yr_ago = 4, now = 4))
  rbind(
    county_series("A0001", 4.0, 4.6),
    county_series("B0001", 4.0, 4.6, drop = as.Date("2025-08-01")),
    county_series("C0001", 4.0, 4.6, drop = as.Date(c("2026-08-01", "2026-09-01"))),
    county_series("D0001", 4.0, 5.0),
    filler
  )
}

test_that("rising county flagged Rising", {
  m <- laus_momentum(laus_fixture())
  a <- m[m$fips == "A0001", ]
  expect_equal(a$momentum_pp, 0.6)
  expect_equal(a$flag, "Rising")
  expect_equal(a$rate_now, 4.6)
  expect_equal(a$rate_yr_ago, 4.0)
})

test_that("one missing month still computes", {
  m <- laus_momentum(laus_fixture())
  expect_equal(m$momentum_pp[m$fips == "B0001"], 0.6)
})

test_that("two missing months gives NA", {
  m <- laus_momentum(laus_fixture())
  c1 <- m[m$fips == "C0001", ]
  expect_true(is.na(c1$momentum_pp))
  expect_true(is.na(c1$flag))
})

test_that("thin latest month is not the reference month", {
  x <- rbind(laus_fixture(), data.frame(fips = "A0001", date = as.Date("2026-10-01"), rate = 9))
  m <- laus_momentum(x)
  expect_true(all(m$ref_month == as.Date("2026-09-01")))
  expect_equal(m$momentum_pp[m$fips == "A0001"], 0.6)
})

test_that("trigger at 1.0 pp", {
  m <- laus_momentum(laus_fixture())
  expect_equal(m$flag[m$fips == "D0001"], "Trigger")
  expect_equal(m$flag[m$fips == "99001"], "None")
})
