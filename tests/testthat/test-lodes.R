lodes_fixture <- function() {
  data.table::fread(test_path("fixtures", "lodes_rac_sample.csv"),
                    colClasses = list(character = "h_geocode"), data.table = FALSE)
}

test_that("blocks aggregate to tracts, leading zeros kept", {
  out <- aggregate_lodes_rac(lodes_fixture())
  expect_s3_class(out, "tbl_df")
  expect_setequal(out$GEOID, c("01001020100", "01001020200", "01001020300"))
  expect_equal(out$C000[out$GEOID == "01001020100"], 8 + 18)
  expect_equal(out$C000[out$GEOID == "01001020200"], 138 + 31)
  expect_equal(out$CA01[out$GEOID == "01001020100"], 2 + 5)
  expect_equal(names(out), c("GEOID", "C000", "CA01", "CE01", sprintf("CNS%02d", 1:20)))
  expect_false("createdate" %in% names(out))
})

test_that("CT LODES tracts remap to planning-region GEOIDs", {
  lodes <- tibble::tibble(GEOID = c("09003500100", "09003990100", "47065000100", "09015000200"),
                          C000 = c(10, 5, 7, 3))
  acs <- c("09110500100", "09110990100", "09120990100", "47065000100")
  out <- lodes_ct_remap(lodes, acs)
  expect_setequal(out$GEOID, c("09110500100", "47065000100"))
  expect_equal(out$C000[out$GEOID == "09110500100"], 10)
  expect_equal(out$C000[out$GEOID == "47065000100"], 7)
  expect_equal(nrow(out), 2)   # 99xx and the unmatched CT tract are dropped
})

test_that("CT remap leaves non-CT data untouched", {
  lodes <- tibble::tibble(GEOID = c("47065000100", "01001020100"), C000 = c(7, 1))
  expect_equal(lodes_ct_remap(lodes, character()), lodes)
})

test_that("AK and MI skip LODES and use ACS fallback", {
  boom <- function(...) stop("must not download")
  d <- tempfile("lodes"); dir.create(d); risk_cache_clear_memory()
  expect_null(fetch_lodes_rac("02", 2024, cache_dir = d, download = boom))
  expect_null(fetch_lodes_rac("26", 2024, cache_dir = d, download = boom))
  expect_length(list.files(d), 0)
  expect_equal(RISK_CFG$lodes_exclude_states, c("02", "26"))
})

test_that("latest year probe steps back on 404", {
  yr <- function(probe) {
    risk_cache_clear_memory()
    d <- tempfile("lodes"); dir.create(d)
    lodes_latest_year(probe = probe, today = as.Date("2026-11-12"), cache_dir = d)
  }
  expect_equal(yr(function(u) grepl("2024", u)), 2024L)
  expect_equal(yr(function(u) grepl("2021", u)), 2021L)
  expect_null(yr(function(u) grepl("2020", u)))   # beyond the 5-year window
  seen <- character()
  expect_null(yr(function(u) { seen <<- c(seen, u); FALSE }))
  expect_equal(seen, vapply(2025:2021, function(y) .lodes_url("de", y), ""))
})

test_that("a failed year probe is not cached", {
  risk_cache_clear_memory()
  d <- tempfile("lodes"); dir.create(d)
  expect_null(lodes_latest_year(probe = function(u) FALSE, today = as.Date("2026-11-12"), cache_dir = d))
  expect_length(list.files(d), 0)
  expect_equal(lodes_latest_year(probe = function(u) grepl("2024", u),
                                 today = as.Date("2026-11-12"), cache_dir = d), 2024L)
  expect_true(file.exists(file.path(d, "lodes_year_v1.rds")))
})

test_that("lodes url is lowercase postal under LODES8", {
  expect_equal(.lodes_url("TN", 2024),
    "https://lehd.ces.census.gov/data/lodes/LODES8/tn/rac/tn_rac_S000_JT00_2024.csv.gz")
})

test_that("fetch caches per state-year", {
  risk_cache_clear_memory()
  d <- tempfile("lodes"); dir.create(d)
  n <- 0; got <- NULL
  dl <- function(st, yr) { n <<- n + 1; got <<- c(st, yr); lodes_fixture() }
  a <- fetch_lodes_rac("01", 2024, cache_dir = d, download = dl)
  b <- fetch_lodes_rac("01", 2024, cache_dir = d, download = dl)
  expect_equal(n, 1)
  expect_equal(got, c("al", "2024"))
  expect_equal(nrow(a), 3)
  expect_equal(attr(a, "lodes_year"), 2024)
  expect_equal(attr(b, "lodes_year"), 2024)
  expect_true(file.exists(file.path(d, "lodes_rac_01_2024.rds")))
  fetch_lodes_rac("01", 2023, cache_dir = d, download = dl)
  expect_equal(n, 2)
})

test_that("fetch resolves the year from the probe cache when none is given", {
  risk_cache_clear_memory()
  d <- tempfile("lodes"); dir.create(d)
  saveRDS(list(value = 2024L, fetched_at = Sys.time()), file.path(d, "lodes_year_v1.rds"))
  out <- fetch_lodes_rac("01", cache_dir = d, download = function(st, yr) lodes_fixture())
  expect_equal(attr(out, "lodes_year"), 2024)
})

test_that("a failed download returns NULL", {
  risk_cache_clear_memory()
  d <- tempfile("lodes"); dir.create(d)
  expect_null(suppressMessages(
    fetch_lodes_rac("01", 2024, cache_dir = d, download = function(...) stop("offline"))))
  expect_length(list.files(d), 0)
})

test_that("lehdr tract output is normalised to the block shape", {
  tr <- tibble::tibble(state = "AL", year = 2024, h_tract = "01001020100", C000 = 5L)
  expect_equal(.lodes_as_blocks(tr)$h_geocode, "01001020100")
})

test_that("CT tracts sharing a 6-digit code fold into one row", {
  lodes <- tibble::tibble(GEOID = c("09001500100", "09003500100", "09003500200"),
                          C000 = c(10, 5, 2), CA01 = c(1, 2, 3))
  out <- lodes_ct_remap(lodes, c("09110500100", "09110500200"))
  expect_false(anyDuplicated(out$GEOID) > 0)
  expect_equal(out$C000[out$GEOID == "09110500100"], 15)
  expect_equal(out$CA01[out$GEOID == "09110500100"], 3)
  expect_equal(out$C000[out$GEOID == "09110500200"], 2)
})

test_that("gz RAC file reads with h_geocode as character", {
  f <- tempfile(fileext = ".csv.gz")
  con <- gzfile(f, "wt"); writeLines(readLines(test_path("fixtures", "lodes_rac_sample.csv")), con); close(con)
  out <- .lodes_read_gz(f)
  expect_true(is.character(out$h_geocode))
  expect_true("010010201001000" %in% out$h_geocode)
  expect_equal(nrow(out), 6)
  expect_true(is.numeric(out$C000))
  expect_equal(aggregate_lodes_rac(out), aggregate_lodes_rac(lodes_fixture()))
})

test_that("an incomplete lehdr frame falls back to the direct download", {
  direct <- 0
  dfn <- function(st, yr) { direct <<- direct + 1; lodes_fixture() }
  bad <- tibble::tibble(h_tract = "01001020100", C000 = 5L)   # no CA01, CE01, CNS
  out <- .lodes_download("al", 2024, lehdr_fn = function(st, yr) bad, direct_fn = dfn)
  expect_equal(direct, 1)
  expect_equal(nrow(out), 6)
  # an erroring lehdr also falls back
  .lodes_download("al", 2024, lehdr_fn = function(st, yr) stop("boom"), direct_fn = dfn)
  expect_equal(direct, 2)
})

test_that("a complete lehdr tract frame is used without downloading", {
  full <- aggregate_lodes_rac(lodes_fixture())
  names(full)[1] <- "h_tract"
  out <- .lodes_download("al", 2024, lehdr_fn = function(st, yr) full,
                         direct_fn = function(...) stop("must not download"))
  expect_equal(out$h_geocode, full$h_tract)
  expect_equal(aggregate_lodes_rac(out)$C000, full$C000)
})

test_that("a failed download is remembered briefly, not retried at once", {
  risk_cache_clear_memory()
  d <- tempfile("lodes"); dir.create(d)
  n <- 0
  dl <- function(...) { n <<- n + 1; stop("offline") }
  suppressMessages({
    fetch_lodes_rac("01", 2024, cache_dir = d, download = dl)
    fetch_lodes_rac("01", 2024, cache_dir = d, download = dl)
  })
  expect_equal(n, 1)
})
