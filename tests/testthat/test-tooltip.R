drv <- function(n) data.frame(label = sprintf("Driver %d", seq_len(n)),
                              value_text = sprintf("%d%%", seq_len(n)),
                              worse = rep(TRUE, n))

test_that("title is escaped", {
  h <- as.character(risk_tooltip_html("O'Brien & <b>Co</b>", value_text = "5%", footer = "src"))
  expect_true(grepl("O'Brien &amp; &lt;b&gt;Co&lt;/b&gt;", h, fixed = TRUE))
  expect_false(grepl("<b>Co</b>", h, fixed = TRUE))
})

test_that("drivers capped at 3", {
  h <- as.character(risk_tooltip_html("X", value_text = "5%", drivers = drv(5), footer = "src"))
  expect_true(grepl("Driven by", h))
  expect_true(all(vapply(paste("Driver", 1:3), grepl, logical(1), h, fixed = TRUE)))
  expect_false(grepl("Driver 4", h, fixed = TRUE))
})

test_that("no drivers block when NULL", {
  h <- as.character(risk_tooltip_html("X", value_text = "5%", footer = "src"))
  expect_false(grepl("Driven by", h))
  h0 <- as.character(risk_tooltip_html("X", value_text = "5%", drivers = drv(0), footer = "src"))
  expect_false(grepl("Driven by", h0))
})

test_that("NA value shows Insufficient data", {
  h <- as.character(risk_tooltip_html("X", value_text = NA, footer = "src"))
  expect_true(grepl("Insufficient data", h))
})

test_that("rank pill, rank sub-line and subtitle render when given", {
  h <- as.character(risk_tooltip_html("Walker County, GA", subtitle = "Chattanooga metro",
                                      value_text = "84", rank_text = "Top 8% in Chattanooga metro",
                                      rank_sub = "Top 15% in Georgia", footer = "src"))
  for (s in c("Chattanooga metro", "Top 8% in Chattanooga metro", "Top 15% in Georgia"))
    expect_true(grepl(s, h, fixed = TRUE))
})
