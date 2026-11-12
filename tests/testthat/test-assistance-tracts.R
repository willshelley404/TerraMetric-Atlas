# Assistance in the tract drill-down: jobs join, score, ranks, sources.

# County layers with Walker GA (13295) and a TN county (47065), both in the
# Chattanooga frame, and sector weights that differ by sector.
at_county_layers <- function() {
  base <- fx_county_base()
  tn <- base[1, ]
  tn$GEOID <- "47065"; tn$name <- "Hamilton County, TN"
  tn$state_fips <- "47"; tn$state_abbr <- "TN"; tn$state_name <- "Tennessee"
  sf::st_geometry(tn) <- sf::st_sfc(sq(-86, 35), crs = 4326)
  base <- rbind(base, tn)
  attr(base, "acs_year") <- 2024
  mom <- rbind(fx_momentum(), tibble::tibble(fips = "47065", ref_month = as.Date("2026-09-01"),
                                             rate_now = 3.0, rate_yr_ago = 3.2,
                                             momentum_pp = -0.2, flag = "None"))
  w <- fx_weights()
  w$w <- seq(0.1, 0.9, length.out = nrow(w))
  ind <- fx_industry()
  ind <- rbind(ind, transform(ind[1, ], GEOID = "47065"))
  sa <- stress_ahead(ind, w)
  attr(sa, "weights") <- w
  build_county_layers(base, mom, stress_ahead = sa)
}

at_tracts <- function() {
  mk <- function(county, pov, rb) {
    n <- length(pov)
    sf::st_sf(GEOID = paste0(county, sprintf("%06d", seq_len(n))), county_geoid = county,
              poverty_rate = pov, rent_burden_rate = rb, noisy = FALSE,
              geometry = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(c(i, i))),
                                    crs = 4326))
  }
  list("13" = mk("13295", c(30, 10, 20, 40), c(40, 50, 45, 60)),
       "47" = mk("47065", c(15, 25), c(35, 55)))
}

at_fetch <- function() {
  tr <- at_tracts()
  function(state_fips, year = NULL) tr[[state_fips]]
}

# Six tracts with different sector mixes; LODES-shaped (all four cushion inputs).
at_jobs <- function(source = "LODES 2024", wage = TRUE) {
  ids <- c(at_tracts()[["13"]]$GEOID, at_tracts()[["47"]]$GEOID)
  counts <- outer(1:6, 1:13, function(i, g) 1 + ((i * g) %% 7) * 5)
  j <- tibble::tibble(GEOID = ids, jobs_total = rowSums(counts))
  for (g in seq_len(13)) j[[.JOB_COLS[g]]] <- counts[, g]
  j$low_wage_share <- if (wage) c(20, 30, 40, 25, 35, 45) else NA_real_
  j$young_share <- if (wage) c(10, 15, 20, 12, 18, 22) else NA_real_
  attr(j, "source") <- source
  j
}

at_fetch_jobs <- function(jobs) {
  function(state_fips, acs_geoids) {
    out <- jobs[substr(jobs$GEOID, 1, 2) == state_fips, ]
    attr(out, "source") <- attr(jobs, "source")
    out
  }
}

at_run <- function(jobs = at_jobs(), weights = attr(at_county_layers(), "sector_weights"),
                   county = "13295") {
  tracts_for_county(county, at_county_layers(), fetch = at_fetch(),
                    fetch_jobs = at_fetch_jobs(jobs), weights = weights)
}

test_that("tracts get assistance columns and ranks", {
  r <- at_run()
  t <- r$tracts
  expect_equal(nrow(t), 6)
  expect_true(all(c("exposure", "exp_workers", "weak_share", "low_wage_share", "young_share",
                    "assist_source", "job_pressure", "assist_score", "assist_exposure",
                    "assist_cushion", "assist_workers", "assist_score_st",
                    "lm_pct_assist_score", "st_pct_assist_score") %in% names(t)))
  expect_false(anyNA(t$assist_score))
  expect_true(all(t$assist_score >= 0 & t$assist_score <= 100))
  expect_equal(unique(t$assist_source), "LODES 2024")
  expect_equal(unname(r$sources[c("13", "47")]), c("LODES 2024", "LODES 2024"))
  expect_equal(t$assist_exposure, round(100 * t$exposure, 1))
  expect_equal(t$assist_workers, t$exp_workers)
  # Job pressure comes from the tract's own county.
  cl <- at_county_layers()
  jp <- job_pressure(cl)
  expect_equal(t$job_pressure, as.numeric(jp)[match(t$county_geoid, cl$GEOID)])
  expect_equal(t$job_pressure_n, attr(jp, "n_inputs")[match(t$county_geoid, cl$GEOID)])
  # Frame rank is a percentile of the score across all six tracts.
  expect_equal(t$lm_pct_assist_score, pct_rank(t$assist_score))
})

test_that("ACS fallback scores with 2-input cushion", {
  src <- "ACS 2020–2024 (LODES incomplete for this state)"
  r <- at_run(jobs = at_jobs(source = src, wage = FALSE))
  t <- r$tracts
  expect_true(all(is.na(t$low_wage_share)))
  expect_true(all(is.na(t$young_share)))
  expect_false(anyNA(t$assist_score))
  expect_equal(unique(t$assist_source), src)
  expect_equal(unname(r$sources[c("13", "47")]), c(src, src))
})

test_that("no weights gives NA assistance, other columns intact", {
  r <- tracts_for_county("13295", at_county_layers(), fetch = at_fetch(),
                         fetch_jobs = at_fetch_jobs(at_jobs()), weights = NULL)
  t <- r$tracts
  for (col in c("assist_score", "assist_exposure", "assist_cushion", "assist_workers",
                "assist_score_st")) {
    expect_true(all(is.na(t[[col]])), info = col)
  }
  expect_false(anyNA(t$poverty_rate))
  expect_false(anyNA(t$lm_pct_poverty_rate))
})

test_that("a state with no job table gets NA assistance and no source", {
  jobs <- at_jobs()
  fj <- function(state_fips, acs_geoids) {
    if (state_fips == "47") NULL else at_fetch_jobs(jobs)(state_fips, acs_geoids)
  }
  r <- tracts_for_county("13295", at_county_layers(), fetch = at_fetch(), fetch_jobs = fj,
                         weights = attr(at_county_layers(), "sector_weights"))
  t <- r$tracts
  expect_true(all(is.na(t$assist_score[t$county_geoid == "47065"])))
  expect_false(anyNA(t$assist_score[t$county_geoid == "13295"]))
  expect_true(is.na(r$sources[["47"]]))
  expect_equal(r$sources[["13"]], "LODES 2024")
})

test_that("a failing jobs fetch does not break the drill-down", {
  r <- tracts_for_county("13295", at_county_layers(), fetch = at_fetch(),
                         fetch_jobs = function(...) stop("boom"),
                         weights = attr(at_county_layers(), "sector_weights"))
  expect_equal(nrow(r$tracts), 6)
  expect_true(all(is.na(r$tracts$assist_score)))
})

test_that("failed tract and jobs fetches name the state and the reason", {
  expect_message(
    tracts_for_county("13295", at_county_layers(), fetch = at_fetch(),
                      fetch_jobs = function(...) stop("boom"),
                      weights = attr(at_county_layers(), "sector_weights")),
    "[Risk] jobs 13 failed: boom", fixed = TRUE)
  tr <- at_tracts()
  bad_fetch <- function(state_fips, year = NULL) {
    if (state_fips == "47") stop("census down") else tr[[state_fips]]
  }
  expect_message(
    tracts_for_county("13295", at_county_layers(), fetch = bad_fetch,
                      fetch_jobs = function(...) NULL),
    "[Risk] tracts 47 failed: census down", fixed = TRUE)
})

test_that("state rank uses state-frame score", {
  t <- at_run()$tracts
  for (st in c("13", "47")) {
    i <- substr(t$GEOID, 1, 2) == st
    expect_equal(t$st_pct_assist_score[i], pct_rank(t$assist_score_st[i]))
  }
  # With 4 GA and 2 TN tracts the state frame differs from the metro frame.
  expect_false(isTRUE(all.equal(t$assist_score, t$assist_score_st)))
})

test_that("assistance_tracts ranks only frame tracts and tolerates NULL jobs", {
  cl <- at_county_layers()
  tr <- do.call(rbind, at_tracts())
  a <- assistance_tracts(tr, NULL, attr(cl, "sector_weights"), cl, "13295")
  expect_true(all(is.na(a$assist_score)))
  b <- assistance_tracts(tr, at_jobs(), attr(cl, "sector_weights"), cl, "13295")
  expect_false(anyNA(b$assist_score[b$county_geoid == "13295"]))
  expect_true(all(is.na(b$assist_score[b$county_geoid == "47065"])))
  expect_false(anyNA(b$assist_score_st))
  expect_false(anyNA(b$assist_exposure))
})

test_that("tracts_for_county works under the app's scalar %||%", {
  app_op <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
  assign("%||%", app_op, envir = globalenv())
  on.exit(rm("%||%", envir = globalenv()), add = TRUE)
  expect_equal(nrow(at_run()$tracts), 6)
})

test_that(".tract_jobs prefers LODES and labels the ACS fallback", {
  geoids <- at_jobs()$GEOID
  lodes <- tibble::tibble(GEOID = geoids, C000 = 10, CA01 = 2, CE01 = 3, CNS01 = 10)
  for (n in sprintf("CNS%02d", 2:20)) lodes[[n]] <- 0
  attr(lodes, "lodes_year") <- 2024L
  ind <- tibble::tibble(GEOID = geoids, emp_total = 10)
  for (g in RISK_SECTORS$group) ind[[paste0("emp_", g)]] <- 0
  attr(ind, "acs_year") <- 2024

  # Functions are sourced into the global env; swap and restore them.
  orig <- list(l = fetch_lodes_rac, i = fetch_state_tract_industry)
  on.exit({
    assign("fetch_lodes_rac", orig$l, envir = globalenv())
    assign("fetch_state_tract_industry", orig$i, envir = globalenv())
  })
  stub <- function(lodes_val, ind_val) {
    assign("fetch_lodes_rac", function(...) lodes_val, envir = globalenv())
    assign("fetch_state_tract_industry", function(...) ind_val, envir = globalenv())
  }

  stub(lodes, ind)
  j <- .tract_jobs("13", geoids)
  expect_equal(attr(j, "source"), "LODES 2024")
  expect_equal(j$low_wage_share[1], 30)

  stub(NULL, ind)
  j <- .tract_jobs("26", geoids)
  expect_equal(attr(j, "source"), "ACS 2020–2024 (LODES incomplete for this state)")
  expect_true(all(is.na(j$low_wage_share)))
  j <- .tract_jobs("13", geoids)
  expect_equal(attr(j, "source"), "ACS 2020–2024 (LODES unavailable)")

  stub(NULL, NULL)
  expect_null(.tract_jobs("13", geoids))
})
