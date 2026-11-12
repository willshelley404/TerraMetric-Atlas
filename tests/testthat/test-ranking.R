test_that("rank labels read as top/bottom shares", {
  expect_equal(rank_label(88, "nationally"), "Top 12% nationally")
  expect_equal(rank_label(30, "in Georgia"), "Bottom 30% in Georgia")
  expect_equal(rank_label(99.6, "nationally"), "Top 1% nationally")
  expect_equal(rank_label(88, "nationally", higher_is_worse = FALSE), "Bottom 12% nationally")
  expect_true(is.na(rank_label(NA, "x")))
})

test_that("rank_label is vectorised", {
  expect_equal(rank_label(c(88, NA, 30), "nationally"),
               c("Top 12% nationally", NA, "Bottom 30% nationally"))
})

test_that("pct_rank keeps NA and spans 0-100", {
  expect_equal(pct_rank(c(1, 2, NA, 3)), c(0, 50, NA, 100))
})
