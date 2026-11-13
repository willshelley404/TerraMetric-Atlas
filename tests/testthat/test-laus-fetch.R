# fetch_laus_county with injected performers — no network.
bls_resp <- function(ids, ok = TRUE) {
  body <- list(status = if (ok) "REQUEST_SUCCEEDED" else "REQUEST_NOT_PROCESSED",
               Results = list(series = lapply(ids, function(i) list(
                 seriesID = i, data = list(list(year = "2026", period = "M09", value = "4.0"))))))
  httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
}
req_ids <- function(req) unlist(req$body$data$seriesid)
fips120 <- sprintf("%05d", 1:120)   # 3 chunks of <= 50

run_fetch <- function(first_ok, retry_ok) {
  risk_cache_clear_memory()
  d <- tempfile("laus"); dir.create(d)
  calls <- 0
  res <- fetch_laus_county(
    fips120, key = "test", cache_dir = d,
    perform = function(reqs, ...) Map(function(r, ok) bls_resp(req_ids(r), ok), reqs, first_ok),
    perform_one = function(req) { calls <<- calls + 1; bls_resp(req_ids(req), retry_ok) })
  list(res = res, retries = calls)
}

test_that("all chunks failing skips retries and returns NULL", {
  r <- run_fetch(c(FALSE, FALSE, FALSE), TRUE)
  expect_null(r$res)
  expect_equal(r$retries, 0)
})

test_that("a few failed chunks are retried once and give a complete pull", {
  r <- run_fetch(c(TRUE, FALSE, TRUE), TRUE)
  expect_equal(r$retries, 1)
  expect_equal(nrow(r$res), 120)
  expect_false(isTRUE(attr(r$res, "partial")))
})

test_that("a partial pull is marked partial with a short TTL", {
  r <- run_fetch(c(TRUE, FALSE, TRUE), FALSE)
  expect_equal(nrow(r$res), 70)
  expect_true(isTRUE(attr(r$res, "partial")))
  expect_lt(attr(r$res, "ttl_days"), 1)
})
