test_that("rent burden counts all renters paying >=30% over computed renters", {
  df <- make_acs_wide(rent_burden_universeE = 1000, rb_not_computedE = 100,
                      rb_30_34E = 100, rb_35_39E = 80, rb_40_49E = 90, rb_50_plusE = 180)
  expect_equal(.derive_metrics(df)$rent_burden_rate, 50)   # 450 / 900
})

test_that("metro summary flags rent burden relative to US, not a fixed 30%", {
  us <- list(rent_burden_rate = 50, unemp_rate = 4, med_income = 70000,
             bachelors_plus_rate = 33, lfp_rate = 63, poverty_rate = 12, vacancy_rate = 7)
  row_hi <- data.frame(metro = "X", rent_burden_rate = 53.5, unemp_rate = 4, lfp_rate = 63,
                       poverty_rate = 12, med_income = 70000, bachelors_plus_rate = 33,
                       vacancy_rate = 7, med_rent = 1200, pop = 1e6, med_age = 38,
                       per_cap_income = 35000, med_home_val = 3e5)
  row_lo <- transform(row_hi, rent_burden_rate = 52)
  expect_true(any(grepl("Housing affordability", build_metro_summary(row_hi, us)$signals$risks)))
  expect_false(any(grepl("Housing affordability", build_metro_summary(row_lo, us)$signals$risks)))
})
