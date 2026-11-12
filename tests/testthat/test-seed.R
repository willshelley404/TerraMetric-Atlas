# Starter copy ("seed") shipped in the repo, used when the server cache is empty.
seed_setup <- function(age_days, value = "seeded") {
  risk_cache_clear_memory()
  rm(list = ls(.RISK_BG_JOBS), envir = .RISK_BG_JOBS)
  cache <- tempfile("cache"); dir.create(cache)
  seed <- tempfile("seed"); dir.create(seed)
  saveRDS(list(value = value, fetched_at = Sys.time() - age_days * 86400),
          file.path(seed, "k.rds"))
  list(cache = cache, seed = seed)
}
bg <- list(id = "job1", fn = "fetch_something", args = list())

test_that("a seed within its TTL is served without any fetch", {
  d <- seed_setup(age_days = 1); n <- 0
  v <- risk_cached("k", 7, function() { n <<- n + 1; "fresh" }, cache_dir = d$cache,
                   seed_dir = d$seed, bg = bg, bg_start = function(...) stop("no bg expected"))
  expect_equal(as.character(v), "seeded")
  expect_equal(n, 0)
  expect_false(attr(v, "stale"))
  expect_true(attr(v, "seed"))
})

test_that("a stale seed is served at once and a background refresh starts", {
  d <- seed_setup(age_days = 10); n <- 0; starts <- character()
  start <- function(bg, key) starts <<- c(starts, paste(bg$id, key))
  for (i in 1:2) {
    v <- risk_cached("k", 7, function() { n <<- n + 1; "fresh" }, cache_dir = d$cache,
                     seed_dir = d$seed, bg = bg, bg_start = start)
    expect_equal(as.character(v), "seeded")
    expect_true(attr(v, "stale"))
  }
  expect_equal(n, 0)                      # never blocks on a foreground fetch
  expect_equal(starts[1], "job1 k")
})

test_that("finished background job promotes the fresh disk copy", {
  d <- seed_setup(age_days = 10)
  risk_cached("k", 7, function() "x", cache_dir = d$cache, seed_dir = d$seed, bg = bg,
              bg_start = function(bg, key) assign(bg$id, list(
                proc = list(is_alive = function() FALSE), keys = key), envir = .RISK_BG_JOBS))
  saveRDS(list(value = "fresh", fetched_at = Sys.time()), file.path(d$cache, "k.rds"))
  .risk_bg_poll(cache_dir = d$cache, reschedule = FALSE)
  v <- risk_cached("k", 7, function() stop("should not fetch"), cache_dir = d$cache,
                   seed_dir = d$seed, bg = bg)
  expect_equal(as.character(v), "fresh")
  expect_false(attr(v, "stale"))
  expect_length(ls(.RISK_BG_JOBS), 0)
})

test_that("a failed background job is not restarted for an hour", {
  d <- seed_setup(age_days = 10); starts <- 0
  start <- function(bg, key) {
    starts <<- starts + 1
    assign(bg$id, list(proc = list(is_alive = function() FALSE), keys = key), envir = .RISK_BG_JOBS)
  }
  risk_cached("k", 7, function() "x", cache_dir = d$cache, seed_dir = d$seed, bg = bg, bg_start = start)
  .risk_bg_poll(cache_dir = d$cache, reschedule = FALSE)   # job ended, wrote nothing
  v <- risk_cached("k", 7, function() "x", cache_dir = d$cache, seed_dir = d$seed, bg = bg,
                   bg_start = start)
  expect_equal(starts, 1)
  expect_equal(as.character(v), "seeded")
})

test_that("no seed dir means the normal foreground fetch", {
  risk_cache_clear_memory()
  cache <- tempfile("cache"); dir.create(cache)
  v <- risk_cached("k", 7, function() "fresh", cache_dir = cache, seed_dir = NULL, bg = bg)
  expect_equal(as.character(v), "fresh")
})

test_that("risk_seed_update copies seedable cache entries, largest LAUS pull only", {
  cache <- tempfile("cache"); dir.create(cache)
  seed <- tempfile("seed")
  for (f in c("acs_year_v2", "county_shapes_2024", "cbsa_xwalk_2023", "county_acs_2024",
              "laus_county_n3", "laus_county_n3144", "tracts_v4_13_2024"))
    saveRDS(list(value = f, fetched_at = Sys.time()), file.path(cache, paste0(f, ".rds")))
  got <- risk_seed_update(cache_dir = cache, seed_dir = seed)
  expect_setequal(got, c("acs_year_v2.rds", "county_shapes_2024.rds", "cbsa_xwalk_2023.rds",
                         "county_acs_2024.rds", "laus_county_n3144.rds"))
  expect_setequal(list.files(seed), got)
})

test_that("seed update copies Phase 2 keys, newest LAUS of each kind", {
  cache <- tempfile("cache"); dir.create(cache)
  seed <- tempfile("seed")
  all <- c("pep_county_2024", "pep_county_2025", "laus_metro_n393", "laus_metro_n390",
           "laus_county_n3144", "fred_sectors_v1", "county_ind_2024", "laus_areas_v1",
           "pep_vintage_v1", "tracts_v4_47_2024")
  for (f in all)
    saveRDS(list(value = f, fetched_at = Sys.time()), file.path(cache, paste0(f, ".rds")))
  got <- risk_seed_update(cache_dir = cache, seed_dir = seed)
  skipped <- c("pep_county_2024", "laus_metro_n390", "tracts_v4_47_2024")
  expect_setequal(got, paste0(setdiff(all, skipped), ".rds"))
  expect_setequal(list.files(seed), got)
})

test_that("seed update copies the LODES year probe, never LODES or tract-industry data", {
  cache <- tempfile("cache"); dir.create(cache)
  seed <- tempfile("seed")
  all <- c("lodes_year_v1", "lodes_rac_47_2022", "tract_ind_47_2024", "tracts_v4_47_2024")
  for (f in all)
    saveRDS(list(value = f, fetched_at = Sys.time()), file.path(cache, paste0(f, ".rds")))
  got <- risk_seed_update(cache_dir = cache, seed_dir = seed)
  expect_equal(got, "lodes_year_v1.rds")
  expect_equal(list.files(seed), "lodes_year_v1.rds")
})
