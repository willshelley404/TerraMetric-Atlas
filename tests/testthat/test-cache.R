new_cache_dir <- function() {
  d <- tempfile("riskcache")
  dir.create(d)
  d
}

test_that("fresh hit does not call fetch", {
  risk_cache_clear_memory()
  d <- new_cache_dir(); n <- 0
  f <- function() { n <<- n + 1; 42 }
  risk_cached("k1", 7, f, cache_dir = d)
  v <- risk_cached("k1", 7, f, cache_dir = d)
  expect_equal(n, 1)
  expect_equal(as.numeric(v), 42)
  expect_false(attr(v, "stale"))
  expect_s3_class(attr(v, "fetched_at"), "POSIXct")
})

test_that("expired entry refetches", {
  risk_cache_clear_memory()
  d <- new_cache_dir(); n <- 0
  f <- function() { n <<- n + 1; n }
  risk_cached("k2", 7, f, cache_dir = d)
  v <- risk_cached("k2", 7, f, cache_dir = d, now = Sys.time() + 8 * 86400)
  expect_equal(n, 2)
  expect_equal(as.numeric(v), 2)
})

test_that("fetch error serves stale copy", {
  risk_cache_clear_memory()
  d <- new_cache_dir()
  risk_cached("k3", 7, function() "first", cache_dir = d)
  v <- risk_cached("k3", 7, function() stop("quota"), cache_dir = d,
                   now = Sys.time() + 8 * 86400)
  expect_equal(as.character(v), "first")
  expect_true(attr(v, "stale"))
})

test_that("no cache and fetch error returns NULL", {
  risk_cache_clear_memory()
  d <- new_cache_dir()
  expect_null(risk_cached("k4", 7, function() stop("down"), cache_dir = d))
  expect_null(risk_cached("k5", 7, function() NULL, cache_dir = d))
})

test_that("a value can shorten its own TTL (partial data)", {
  risk_cache_clear_memory()
  d <- new_cache_dir(); n <- 0
  f <- function() { n <<- n + 1; structure(n, ttl_days = 0.1) }
  risk_cached("k7", 7, f, cache_dir = d)
  risk_cached("k7", 7, f, cache_dir = d, now = Sys.time() + 0.05 * 86400)
  expect_equal(n, 1)
  risk_cached("k7", 7, f, cache_dir = d, now = Sys.time() + 0.2 * 86400)
  expect_equal(n, 2)
})

test_that("failures are remembered for fail_ttl_days", {
  risk_cache_clear_memory()
  d <- new_cache_dir(); n <- 0
  f <- function() { n <<- n + 1; NULL }
  expect_null(risk_cached("k8", 7, f, cache_dir = d, fail_ttl_days = 1 / 24))
  expect_null(risk_cached("k8", 7, f, cache_dir = d, fail_ttl_days = 1 / 24))
  expect_equal(n, 1)
  risk_cached("k8", 7, f, cache_dir = d, fail_ttl_days = 1 / 24, now = Sys.time() + 7200)
  expect_equal(n, 2)
})

test_that("disk copy survives memory clear", {
  risk_cache_clear_memory()
  d <- new_cache_dir(); n <- 0
  f <- function() { n <<- n + 1; "v" }
  risk_cached("k6", 7, f, cache_dir = d)
  risk_cache_clear_memory()
  v <- risk_cached("k6", 7, f, cache_dir = d)
  expect_equal(n, 1)
  expect_equal(as.character(v), "v")
})
