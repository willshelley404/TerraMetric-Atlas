# Assistance metrics: LODES/ACS job groups, exposure, job pressure, score.

lodes_row <- function(geoid = "13295000100", ...) {
  base <- c(list(GEOID = geoid, C000 = 0, CA01 = 0, CE01 = 0),
            stats::setNames(as.list(rep(0, 20)), sprintf("CNS%02d", 1:20)))
  over <- list(...)
  for (n in names(over)) base[[n]] <- over[[n]]
  tibble::as_tibble(base)
}

sector_weights_fx <- function(w = NULL, m_pct = NULL) {
  g <- RISK_SECTORS$group
  tibble::tibble(group = g,
                 w = if (is.null(w)) rep(0.5, length(g)) else w,
                 m_pct = if (is.null(m_pct)) rep(1, length(g)) else m_pct)
}

test_that("CNS columns map to 13 groups", {
  l <- lodes_row(C000 = 15, CNS03 = 10, CNS08 = 5)
  g <- lodes_group_jobs(l)
  expect_equal(g$jobs_transport_util, 15)

  l2 <- lodes_row(C000 = 6, CNS12 = 1, CNS13 = 2, CNS14 = 3)
  g2 <- lodes_group_jobs(l2)
  expect_equal(g2$jobs_prof_admin, 6)

  l3 <- lodes_row(C000 = 210, CNS01 = 1, CNS02 = 2, CNS03 = 3, CNS04 = 4, CNS05 = 5,
                  CNS06 = 6, CNS07 = 7, CNS08 = 8, CNS09 = 9, CNS10 = 10, CNS11 = 11,
                  CNS12 = 12, CNS13 = 13, CNS14 = 14, CNS15 = 15, CNS16 = 16,
                  CNS17 = 17, CNS18 = 18, CNS19 = 19, CNS20 = 20)
  g3 <- lodes_group_jobs(l3)
  expect_equal(sum(unlist(g3[paste0("jobs_", RISK_SECTORS$group)])), l3$C000)
  expect_equal(g3$jobs_total, 210)
})

test_that("low-wage and young shares come from CE01 and CA01", {
  g <- lodes_group_jobs(lodes_row(C000 = 200, CA01 = 50, CE01 = 80, CNS01 = 200))
  expect_equal(g$low_wage_share, 40)
  expect_equal(g$young_share, 25)
})

test_that("zero jobs gives NA exposure and shares", {
  g <- lodes_group_jobs(lodes_row(C000 = 0))
  e <- exposure_from_jobs(g, sector_weights_fx())
  expect_true(is.na(e$exposure))
  expect_true(is.na(e$exp_workers))
  expect_true(is.na(e$weak_share))
  expect_true(is.na(g$low_wage_share))
  expect_true(is.na(g$young_share))
  expect_false(any(is.nan(c(g$low_wage_share, g$young_share, e$exposure,
                            e$exp_workers, e$weak_share))))
})

test_that("ACS fallback has job groups but no wage or age bands", {
  ind <- tibble::tibble(GEOID = "13295", emp_total = 100)
  for (grp in RISK_SECTORS$group) ind[[paste0("emp_", grp)]] <- 0
  ind$emp_manufacturing <- 60
  ind$emp_retail <- 40
  g <- acs_group_jobs(ind)
  expect_equal(g$jobs_total, 100)
  expect_equal(g$jobs_manufacturing, 60)
  expect_true(is.na(g$low_wage_share))
  expect_true(is.na(g$young_share))
  expect_equal(names(g), names(lodes_group_jobs(lodes_row(C000 = 1))))
})

test_that("exposure is the job-share-weighted mean of sector weights", {
  w <- sector_weights_fx(w = ifelse(RISK_SECTORS$group == "manufacturing", 1, 0),
                         m_pct = ifelse(RISK_SECTORS$group == "manufacturing", -2, 1))
  g <- lodes_group_jobs(lodes_row(C000 = 100, CNS05 = 50, CNS15 = 50))
  e <- exposure_from_jobs(g, w)
  expect_equal(e$exposure, 0.5)
  expect_equal(e$exp_workers, 50)
  expect_equal(e$weak_share, 50)
  expect_equal(e$GEOID, g$GEOID)
})

test_that("exposure is all NA when weights are NULL", {
  g <- lodes_group_jobs(lodes_row(C000 = 100, CNS05 = 100))
  e <- exposure_from_jobs(g, NULL)
  expect_true(is.na(e$exposure) && is.na(e$exp_workers) && is.na(e$weak_share))
})

test_that("job pressure uses available signals, NA when none", {
  cl <- data.frame(pct_momentum_pp = c(80, NA, NA), pct_stress_ahead_pp = c(20, 40, NA))
  jp <- job_pressure(cl)
  expect_equal(as.numeric(jp), c(0.8, 0.6, NA))
  expect_equal(attr(jp, "n_inputs"), c(2L, 1L, 0L))
  expect_equal(attr(jp, "inputs"), c("both", "ahead", NA))
  now_only <- job_pressure(data.frame(pct_momentum_pp = 30, pct_stress_ahead_pp = NA_real_))
  expect_equal(attr(now_only, "inputs"), "now")
})

test_that("job pressure is NA when the columns are missing", {
  jp <- job_pressure(data.frame(x = 1:2))
  expect_equal(as.numeric(jp), c(NA_real_, NA_real_))
  expect_equal(attr(jp, "n_inputs"), c(0L, 0L))
})

test_that("score is the geometric mean and needs all three", {
  exposure <- c(1, 2, 3, 4, 5)
  ci <- data.frame(a = c(1, 2, 3, 4, 5), b = c(10, 20, 30, 40, 50))
  grp <- rep("A", 5)
  s <- assistance_score(exposure, ci, J = rep(1, 5), group = grp)
  expect_equal(s$score[5], 100)
  expect_equal(s$pct_exposure[5], 100)
  expect_equal(s$pct_cushion[5], 100)

  sna <- assistance_score(exposure, ci, J = c(1, 1, 1, 1, NA), group = grp)
  expect_true(is.na(sna$score[5]))

  ci1 <- ci
  ci1$b[5] <- NA
  s1 <- assistance_score(exposure, ci1, J = rep(1, 5), group = grp)
  expect_true(is.na(s1$cushion[5]))
  expect_true(is.na(s1$score[5]))
  expect_false(is.na(s1$cushion[4]))
})

test_that("percentiles are within group", {
  exposure <- c(1, 2, 3, 10, 20, 30)
  ci <- data.frame(a = c(3, 2, 1, 30, 20, 10), b = c(1, 2, 3, 10, 20, 30))
  s <- assistance_score(exposure, ci, J = rep(0.8, 6), group = rep(c("A", "B"), each = 3))
  expect_equal(s$score[1:3], s$score[4:6])
  expect_equal(s$pct_exposure[1:3], s$pct_exposure[4:6])
})

test_that("score never yields NaN for singleton groups", {
  s <- assistance_score(c(1, 2), data.frame(a = c(1, 2), b = c(1, 2)),
                        J = c(1, 1), group = c("A", "B"))
  expect_false(any(is.nan(unlist(s))))
})

test_that("score is the geometric mean of distinct factors, not arithmetic or min", {
  # 3 tracts: exposure pct 0/50/100; cushion inputs give cushion pct 100/0/50.
  ci <- data.frame(a = c(3, 1, 2), b = c(30, 10, 20))
  s <- assistance_score(c(1, 2, 3), ci, J = c(1, 1, 0.5), group = rep("A", 3))
  expect_equal(s$pct_exposure, c(0, 50, 100))
  expect_equal(s$pct_cushion, c(100, 0, 50))
  expect_equal(s$score[3], round(100 * (1 * 0.5 * 0.5)^(1 / 3), 1))
  expect_equal(s$score[3], 63)
})

test_that("a single-member group gives NA, not NaN", {
  s <- assistance_score(1, data.frame(a = 1, b = 1), J = 1, group = "A")
  expect_true(all(is.na(s$score)))
  expect_false(any(is.nan(unlist(s))))
})

test_that("cushion averages the available percentiles and honours the minimum", {
  ci <- data.frame(a = c(1, 2, 3, 4), b = c(4, 3, 2, 1), c = c(1, 2, 3, NA))
  grp <- rep("A", 4)
  s <- assistance_score(1:4, ci, J = rep(1, 4), group = grp)
  # Tract 4: pct a = 100, pct b = 0, c missing -> mean of the two = 50.
  expect_equal(s$cushion[4], 50)
  # Tract 1: pct a = 0, b = 100, c = 0 -> 100 / 3.
  expect_equal(s$cushion[1], 100 / 3)
  s3 <- assistance_score(1:4, ci, J = rep(1, 4), group = grp,
                         cfg = modifyList(RISK_CFG, list(cushion_min_inputs = 3)))
  expect_true(is.na(s3$cushion[4]))
  expect_true(is.na(s3$score[4]))
  expect_false(is.na(s3$cushion[1]))
})
