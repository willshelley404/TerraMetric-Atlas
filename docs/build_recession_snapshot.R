# ─────────────────────────────────────────────────────────────────────────────
# docs/build_recession_snapshot.R
#
# Builds docs/recession_model_snapshot.rds, the data behind recession_model.qmd.
# Pulls the recession model's own code out of R/synopsis_tier3.R (no copy of the
# logic), fetches FRED, and records the fit, backtest, per-January coefficient
# path and the refit-cadence comparison.
#
# Run from the repo root (needs FRED_API_KEY; see .Renviron.example):
#   Rscript docs/build_recession_snapshot.R
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(lubridate); library(glmnet); library(zoo); library(fredr)
})
stopifnot(file.exists("R/synopsis_tier3.R"))
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# ── Load only the model code from tier 3 (by name, not by line number) ───────
.model_objects <- c(
  ".GLM_PREDICTORS", ".GLM_LATE_START", ".GLM_EXPECTED_SIGN", ".GLM_LABELS",
  ".GLM_WINDOW", ".GLM_EMBARGO", ".GLM_FIRST_FORECAST", ".GLM_SERIES",
  ".GLM_NO_INTERPOLATE", ".cv_folds", ".glm_sign_limits", "build_robust_scaler",
  "apply_robust_scaler", ".fill_single_gaps", ".fetch_glm_series",
  "build_glm_panel", ".glm_training_rows", "fit_recession_glm",
  "predict_recession_glm", "backtest_recession_glm", "train_recession_glm",
  ".compute_recession_prob", ".classify_recession_tier"
)
for (e in parse("R/synopsis_tier3.R")) {
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") &&
      as.character(e[[2]])[1] %in% .model_objects) {
    eval(e, globalenv())
  }
}
stopifnot(all(vapply(.model_objects, exists, logical(1))))

# ── Data: raw vs gap-filled series ───────────────────────────────────────────
start <- as.Date("1959-01-01")
raw <- lapply(.GLM_SERIES, function(sid) {
  r <- fredr::fredr(sid, observation_start = start, frequency = "m")
  data.frame(date = as.Date(r$date), value = as.numeric(r$value))
})
names(raw) <- .GLM_SERIES
series <- lapply(.GLM_SERIES, function(sid) {
  if (sid %in% .GLM_NO_INTERPOLATE) raw[[sid]] else .fill_single_gaps(raw[[sid]])
})
names(series) <- .GLM_SERIES
series_meta <- do.call(rbind, lapply(.GLM_SERIES, function(sid) {
  r <- raw[[sid]]; f <- series[[sid]]
  data.frame(
    id = sid,
    first = min(r$date[!is.na(r$value)]),
    last = max(r$date[!is.na(r$value)]),
    n_obs = sum(!is.na(r$value)),
    n_filled = sum(!is.na(f$value)) - sum(!is.na(r$value)),
    stringsAsFactors = FALSE
  )
}))

panel <- build_glm_panel(series)
latest <- max(panel$date[panel$complete])

# ── Live model (identical call to the app) ───────────────────────────────────
live <- {
  model <- fit_recession_glm(.glm_training_rows(panel, floor_date(latest, "year")))
  now <- panel[panel$date == latest, , drop = FALSE]
  list(
    model = model, now = now, prob = predict_recession_glm(model, now),
    backtest = backtest_recession_glm(panel), trained_at = Sys.time()
  )
}
current <- .compute_recession_prob(live)
train <- .glm_training_rows(panel, floor_date(latest, "year"))

# ── Per-January coefficient path (the "refit history") ───────────────────────
jan_models <- lapply(1985:as.integer(format(latest, "%Y")), function(y) {
  fit_recession_glm(.glm_training_rows(panel, as.Date(sprintf("%d-01-01", y))))
})
names(jan_models) <- 1985:as.integer(format(latest, "%Y"))
beta_path <- do.call(rbind, lapply(names(jan_models), function(y) {
  m <- jan_models[[y]]
  if (is.null(m)) return(NULL)
  data.frame(
    year = as.integer(y), term = c("intercept", .GLM_PREDICTORS),
    beta = c(m$intercept, m$beta), n_train = m$n_train,
    base_rate = m$base_rate, stringsAsFactors = FALSE
  )
}))

# ── Refit-cadence comparison ─────────────────────────────────────────────────
tr_rows <- function(as_of, window) {
  ok <- panel$complete & !is.na(panel$rec_next12) & panel$date <= as_of %m-% months(.GLM_EMBARGO)
  tail(panel[ok, , drop = FALSE], window)
}
cache <- new.env()
get_model <- function(as_of, window = 360L) {
  key <- paste(as_of, window)
  if (!exists(key, cache)) assign(key, fit_recession_glm(tr_rows(as_of, window)), cache)
  get(key, cache)
}
rows <- panel[panel$complete & panel$date >= as.Date("1985-01-01"), ]
score_scheme <- function(as_of_fn, window = 360L) {
  p <- vapply(seq_len(nrow(rows)), function(i) {
    ps <- vapply(as_of_fn(rows$date[i]), function(a) {
      m <- get_model(a, window)
      if (is.null(m)) NA_real_ else predict_recession_glm(m, rows[i, ])
    }, numeric(1))
    mean(ps, na.rm = TRUE)
  }, numeric(1))
  data.frame(date = rows$date, prob = p, y = rows$rec_next12)
}
jan <- function(t) floor_date(t, "year")
schemes <- list(
  list(id = "A", label = "Annual January refit, 30-yr rolling (current)", fn = jan, w = 360L),
  list(id = "B", label = "Quarterly refit, 30-yr rolling", fn = function(t) floor_date(t, "quarter"), w = 360L),
  list(id = "C", label = "Monthly refit, 30-yr rolling", fn = function(t) floor_date(t, "month"), w = 360L),
  list(id = "D", label = "Annual January refit, expanding window", fn = jan, w = Inf),
  list(id = "E", label = "Monthly refit, expanding window", fn = function(t) floor_date(t, "month"), w = Inf),
  list(id = "F", label = "Average of the last 5 January fits", fn = function(t) {
    as.Date(sprintf("%d-01-01", as.integer(format(t, "%Y")) - 0:4))
  }, w = 360L),
  list(id = "G", label = "Frozen: fitted Jan 1985, never refit", fn = function(t) as.Date("1985-01-01"), w = 360L)
)
cmp <- lapply(schemes, function(s) score_scheme(s$fn, s$w))
names(cmp) <- vapply(schemes, `[[`, "", "id")
cmp_labels <- vapply(schemes, `[[`, "", "label")
names(cmp_labels) <- names(cmp)
# Fixed-coefficient benchmark: Estrella-Mishkin (1996) probit on 10Y-3M
r3 <- fredr::fredr("T10Y3M", observation_start = start, frequency = "m")
t10y3m <- r3$value[match(rows$date, as.Date(r3$date))]
cmp$K <- data.frame(date = rows$date, prob = pnorm(-0.5333 - 0.6330 * t10y3m), y = rows$rec_next12)
cmp_labels["K"] <- "Fixed Estrella-Mishkin probit on 10Y-3M (no fitting)"

# January model-swap jump on identical data
jumps <- do.call(rbind, lapply(1986:as.integer(format(latest, "%Y")), function(y) {
  t <- as.Date(sprintf("%d-01-01", y))
  old <- jan_models[[as.character(y - 1)]]; new <- jan_models[[as.character(y)]]
  r <- panel[panel$date == t & panel$complete, ]
  if (is.null(old) || is.null(new) || nrow(r) == 0) return(NULL)
  data.frame(year = y, jump = predict_recession_glm(new, r) - predict_recession_glm(old, r))
}))

snap <- list(
  built_at = Sys.time(), latest = latest, series_meta = series_meta,
  panel = panel, train = train, live = live, current = current,
  beta_path = beta_path, cmp = cmp, cmp_labels = cmp_labels, jumps = jumps,
  consts = list(
    predictors = .GLM_PREDICTORS, labels = .GLM_LABELS, sign = .GLM_EXPECTED_SIGN,
    window = .GLM_WINDOW, embargo = .GLM_EMBARGO, first_forecast = .GLM_FIRST_FORECAST,
    series = .GLM_SERIES
  )
)
saveRDS(snap, "docs/recession_model_snapshot.rds")
message(sprintf(
  "Snapshot written: latest month %s, p = %.1f%%, %d panel rows",
  format(latest, "%b %Y"), live$prob * 100, nrow(panel)
))
