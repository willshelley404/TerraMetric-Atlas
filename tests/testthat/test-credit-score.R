# Credit components, score and tax base proxy (pure)

cr_fx <- function() {
  ids <- c("01001", "01003", "13295", "47065", "09110", "48201")
  list(
    cl = tibble::tibble(GEOID = ids, job_pressure = c(0.1, 0.3, 0.5, 0.7, NA, 0.9)),
    cfpb = tibble::tibble(GEOID = ids, dq90 = c(1, 2, 3, 4, 5, 6),
                          dq3089_chg = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          cfpb_source = c("county", "county", "metro", "metro", "state non-metro", "county"),
                          cfpb_month = "2026-03"),
    urban = tibble::tibble(GEOID = ids, card_del = c(10, 12, 14, 16, NA, 20),
                           auto_del = c(2, 4, 6, 8, NA, 12)),
    hpi = tibble::tibble(GEOID = ids, hpi_yoy = c(8, 6, 4, 2, -1, -3),
                         hpi_source = "metro", hpi_q = "2025 Q4"))
}

test_that("components are national percentiles with mortgage as mean of level and change", {
  f <- cr_fx()
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_equal(r$GEOID, f$cl$GEOID)
  # level rises, change falls: the two percentiles offset exactly
  expect_equal(r$cr_mortgage, rep(50, 6))
  expect_equal(r$cr_consumer[1:4], c(0, 25, 50, 75))
  expect_equal(r$cr_consumer[6], 100)
  expect_true(is.na(r$cr_consumer[5]))
  expect_equal(r$cr_jobs, c(10, 30, 50, 70, NA, 90))
  expect_equal(r$cr_collateral, c(0, 20, 40, 60, 80, 100))
  expect_equal(r$credit_n, c(4, 4, 4, 4, 2, 4))
  expect_equal(r$credit_score[1], round(mean(c(50, 0, 10, 0)), 1))
  expect_true(all(c("dq90", "dq3089_chg", "cfpb_source", "card_del", "auto_del",
                    "hpi_yoy", "hpi_source") %in% names(r)))
})

test_that("mortgage uses whichever of level and change is available", {
  f <- cr_fx()
  f$cfpb$dq3089_chg[1] <- NA
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_equal(r$cr_mortgage[1], 0)
  f$cfpb$dq90[1] <- NA
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_true(is.na(r$cr_mortgage[1]))
  expect_match(r$credit_missing[1], "mortgage")
})

test_that("consumer delinquency averages card and auto, whichever is available", {
  f <- cr_fx()
  f$urban$auto_del[1] <- NA; f$urban$card_del[1] <- 5
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  # county 1 is card only (5), still the lowest
  expect_equal(r$cr_consumer[1], 0)
})

test_that("score runs on 3 components when Urban missing", {
  f <- cr_fx()
  r <- credit_components(f$cl, f$cfpb, NULL, f$hpi)
  ok <- !is.na(f$cl$job_pressure)
  expect_true(all(!is.na(r$credit_score[ok])))
  expect_equal(r$credit_missing[ok], rep("consumer", 5))
  expect_equal(r$credit_n[ok], rep(3, 5))
  expect_equal(r$credit_score[1], round(mean(c(50, 10, 0)), 1))
  expect_true(all(is.na(r$card_del)))
})

test_that("fewer than 3 components gives Insufficient data naming the missing", {
  f <- cr_fx()
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  ct <- r[r$GEOID == "09110", ]
  expect_equal(ct$credit_n, 2)
  expect_true(is.na(ct$credit_score))
  expect_equal(ct$credit_missing, "consumer, job pressure")
  expect_equal(r$credit_missing[1], "")
})

test_that("collateral inverted: falling prices score higher", {
  f <- cr_fx()
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_gt(r$cr_collateral[r$GEOID == "48201"], r$cr_collateral[r$GEOID == "01001"])
  expect_equal(r$cr_collateral[r$GEOID == "48201"], 100)
})

test_that("all sources NULL gives NA scores, never NaN, and still names the missing", {
  f <- cr_fx()
  r <- credit_components(f$cl, NULL, NULL, NULL)
  expect_true(all(is.na(r$credit_score)))
  expect_false(any(is.nan(unlist(r[sapply(r, is.numeric)]))))
  expect_equal(r$credit_n[1], 1)
  expect_equal(r$credit_missing[1], "mortgage, consumer, collateral")
})

test_that("works with the app-style scalar %||% override", {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
  f <- cr_fx()
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_equal(r$cr_consumer[1:4], c(0, 25, 50, 75))
  expect_equal(tax_base_proxy(dplyr::mutate(f$cl, mig_dom_rate = 1:6, pop_chg_pct = 6:1,
                                            hpi_yoy = f$hpi$hpi_yoy))[1],
               round(mean(c(100, 0, 0)), 1))
})

test_that("tax base proxy needs two inputs", {
  cl <- tibble::tibble(GEOID = c("a", "b", "c", "d", "e"),
                       mig_dom_rate = c(-3, -1, 0, 1, 3),
                       pop_chg_pct = c(-2, -1, 0, 1, 2),
                       hpi_yoy = c(-4, -2, 0, 2, NA))
  tb <- tax_base_proxy(cl)
  expect_type(tb, "double")
  expect_equal(tb[1], 100)                 # worst on all three
  expect_equal(tb[4], round(mean(c(25, 25, 0)), 1))
  expect_equal(tb[5], 0)                   # best on both available inputs
  # HPI missing for every row: migration + population still score
  tb2 <- tax_base_proxy(cl[c("GEOID", "mig_dom_rate", "pop_chg_pct")])
  expect_equal(tb2, c(100, 75, 50, 25, 0))
  # only one input -> NA
  one <- cl; one$pop_chg_pct <- NA_real_; one$hpi_yoy <- NA_real_
  expect_true(all(is.na(tax_base_proxy(one))))
  expect_false(any(is.nan(tb)))
})

test_that("consumer follows the percentile of the card/auto mean when they disagree", {
  f <- cr_fx()
  f$urban$card_del <- c(1, 2, 3, 4, 5, 6)      # ascending
  f$urban$auto_del <- c(12, 10, 8, 6, 4, 2)    # descending
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  m <- (f$urban$card_del + f$urban$auto_del) / 2
  expect_equal(r$cr_consumer, pct_rank(m))
  expect_false(isTRUE(all.equal(r$cr_consumer, pct_rank(f$urban$card_del))))
})

test_that("shuffled and extra or missing GEOIDs in the source tables change nothing", {
  f <- cr_fx()
  base <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  extra <- function(t, id) { x <- t[1, ]; x$GEOID <- id; rbind(t, x) }
  shuf <- function(t, id) extra(t, id)[c(7, 3, 1, 6, 2, 5, 4), ]
  r <- credit_components(f$cl, shuf(f$cfpb, "99999"), shuf(f$urban, "99998"), shuf(f$hpi, "99997"))
  expect_equal(r, base)
  # a county absent from a table is just missing there; the others keep their percentiles
  # because 99999 etc. are not in cl and never enter the ranking
  gone <- f$urban[f$urban$GEOID != "01003", ]
  r2 <- credit_components(f$cl, f$cfpb, gone, f$hpi)
  expect_true(is.na(r2$cr_consumer[r2$GEOID == "01003"]))
  expect_match(r2$credit_missing[r2$GEOID == "01003"], "consumer")
  expect_equal(r2$cr_mortgage, base$cr_mortgage)
  expect_equal(r2$cr_collateral, base$cr_collateral)
  expect_equal(r2$GEOID, f$cl$GEOID)
})

test_that("tied input values share a percentile and never produce NaN", {
  f <- cr_fx()
  f$cfpb$dq90 <- c(2, 2, 2, 5, 5, 5)
  f$cfpb$dq3089_chg <- c(1, 1, 1, 1, 1, 1)
  f$urban$card_del <- rep(7, 6); f$urban$auto_del <- rep(3, 6)
  f$hpi$hpi_yoy <- c(4, 4, 4, 4, 4, 4)
  r <- credit_components(f$cl, f$cfpb, f$urban, f$hpi)
  expect_equal(length(unique(r$cr_mortgage[1:3])), 1)
  expect_equal(length(unique(r$cr_mortgage[4:6])), 1)
  expect_equal(length(unique(r$cr_consumer[!is.na(r$cr_consumer)])), 1)
  expect_equal(length(unique(r$cr_collateral)), 1)
  expect_false(any(is.nan(unlist(r[sapply(r, is.numeric)]))))
  expect_false(any(is.nan(r$credit_score)))
})
