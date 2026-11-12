test_that("probe picks the newest available year and never invents a fallback", {
  expect_equal(.probe_acs_year(2025, function(y) y == 2024), 2024L)
  expect_equal(.probe_acs_year(2025, function(y) y <= 2025), 2025L)
  expect_null(.probe_acs_year(2025, function(y) FALSE))
})

test_that("preferred ACS year follows the calendar", {
  expect_equal(.acs_preferred_year(as.Date("2026-11-12")), 2025L)
  expect_equal(.acs_preferred_year(as.Date("2027-03-01")), 2026L)
})

test_that("a failed probe is not cached and falls back to year - 2", {
  risk_cache_clear_memory()
  d <- tempfile("acsy"); dir.create(d)
  y <- risk_acs_year(probe = function(y) FALSE, cache_dir = d, today = as.Date("2026-11-12"))
  expect_equal(y, 2024L)
  expect_length(list.files(d), 0)
  y2 <- risk_acs_year(probe = function(y) y == 2025, cache_dir = d, today = as.Date("2026-11-12"))
  expect_equal(y2, 2025L)
})
