tract_wide <- function(...) {
  d <- make_acs_wide(...)
  for (e in grep("E$", names(d), value = TRUE)) d[[sub("E$", "M", e)]] <- 1
  over <- list(...)
  m_over <- over[grepl("M$", names(over))]
  d[names(m_over)] <- m_over
  d
}

test_that("noisy flag when MOE exceeds the estimate itself", {
  noisy <- tract_wide(poverty_universeE = 100, poverty_countE = 10,
                      poverty_universeM = 20, poverty_countM = 12)  # MOE ~11.8 pp on 10%
  wide  <- tract_wide(poverty_universeE = 100, poverty_countE = 10,
                      poverty_universeM = 20, poverty_countM = 8)   # MOE ~7.7 pp on 10%
  quiet <- tract_wide(poverty_universeE = 1000, poverty_countE = 100,
                      poverty_universeM = 20, poverty_countM = 15)  # MOE ~1.5 pp on 10%
  x <- tract_moe_flags(.derive_metrics(rbind(noisy, wide, quiet)))
  expect_equal(x$poverty_rate, c(10, 10, 10))
  expect_equal(round(x$poverty_rate_moe[1:2], 1), c(11.8, 7.7))
  expect_equal(x$noisy_poverty_rate, c(TRUE, FALSE, FALSE))
  expect_equal(x$noisy, c(TRUE, FALSE, FALSE))
})

test_that("noisy flags are per variable", {
  x <- tract_moe_flags(.derive_metrics(tract_wide(
    poverty_universeE = 100, poverty_countE = 10, poverty_universeM = 20, poverty_countM = 12)))
  expect_true(x$noisy_poverty_rate)
  expect_false(x$noisy_rent_burden_rate)
})

test_that("rent burden MOE pools the four burden bins", {
  x <- tract_moe_flags(.derive_metrics(tract_wide(
    rent_burden_universeE = 400, rb_not_computedE = 0,
    rb_30_34E = 40, rb_35_39E = 40, rb_40_49E = 40, rb_50_plusE = 80,
    rb_30_34M = 120, rb_35_39M = 120, rb_40_49M = 120, rb_50_plusM = 120,
    rent_burden_universeM = 40)))
  expect_equal(x$rent_burden_rate, 50)
  # pooled bin MOE = sqrt(4 * 120^2) = 240; moe_prop -> sqrt(240^2 - .25 * 40^2) / 400 = 59.8 pp
  expect_equal(round(x$rent_burden_rate_moe, 1), 59.8)
  expect_true(x$noisy_rent_burden_rate)
  expect_true(x$noisy)
})

test_that("zero denominators give NA not Inf", {
  x <- tract_moe_flags(.derive_metrics(tract_wide(
    poverty_universeE = 0, poverty_countE = 0, total_popE = 0, labor_forceE = 0,
    rent_burden_universeE = 0, rb_not_computedE = 0, rb_30_34E = 0, rb_35_39E = 0,
    rb_40_49E = 0, rb_50_plusE = 0, housing_unitsE = 0, education_total_25plusE = 0)))
  for (col in c("poverty_rate", "rent_burden_rate", "unemp_rate", "lfp_rate",
                "vacancy_rate", "bachelors_plus_rate", "poverty_rate_moe")) {
    expect_true(is.na(x[[col]]), info = col)
  }
  expect_false(x$noisy)
  expect_false(x$noisy_poverty_rate)
})
