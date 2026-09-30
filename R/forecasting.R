# ─────────────────────────────────────────────────────────────────────────────
# R/forecasting.R — Macro-aware forecasting (BVAR block + ensemble)
#
# The rates/inflation/labor cluster (UNRATE, CPI YoY, FEDFUNDS, MORTGAGE30US,
# nonfarm payrolls, initial jobless claims) is modeled *jointly* with a
# Bayesian VAR (Minnesota prior) so that a labor-market shock (e.g. a
# payrolls miss or a claims spike) propagates into the unemployment, rate,
# and inflation paths automatically via Okun's-law-style cross-equation
# dynamics, instead of the labor and rates blocks being forecast in
# isolation from each other. Forecast bands widen honestly with horizon.
#
# Everything else (WTI oil, retail sales, housing starts) has no strong
# two-way feedback with the macro block, so it stays on the original
# 3-model ensemble:
#   1. Prophet          (trend + seasonality)
#   2. Auto ARIMA       (short-run dynamics)
#   3. ETS              (error/trend/season smoothing)
# Ensemble weights are now cross-validated (time_series_cv) rather than
# in-sample, and prediction bands come from horizon-scaled backtest residuals
# rather than the collapsing in-sample estimate.
#
# Output structure is identical to the original single-model version so all
# Shiny UI / plotting code continues to work without changes.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  # macro block (joint model) — accessed via `::` throughout rather than
  # library()'d, because `vars` Depends on MASS, whose select() generic
  # would otherwise mask dplyr::select() for the rest of the app.
  # requireNamespace() loads them without attaching to the search path.
  stopifnot(
    requireNamespace("vars", quietly = TRUE),
    requireNamespace("BVAR", quietly = TRUE),
    requireNamespace("tseries", quietly = TRUE)
  )

  # underlying engines
  library(prophet) # engine for prophet_reg
  library(forecast) # engine for arima_reg / exp_smoothing

  # modeltime ecosystem (univariate ensemble)
  library(modeltime)
  library(modeltime.ensemble)
  library(timetk) # future_frame(), time_series_cv(), etc.
  library(parsnip)
  library(workflows)
  library(recipes)
  library(rsample)
  library(rlang) # sym() for tidy eval

  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(glue)
  library(plotly)
})

# ── Series metadata ───────────────────────────────────────────────────────────

FORECAST_SERIES <- list(
  UNRATE = list(name = "Unemployment Rate", unit = "%", color = "#00b4d8"),
  CPIAUCSL = list(
    name = "CPI Inflation (YoY %)",
    unit = "YoY %",
    color = "#e94560"
  ),
  FEDFUNDS = list(name = "Fed Funds Rate", unit = "%", color = "#f4a261"),
  MORTGAGE30US = list(
    name = "30-Yr Mortgage Rate",
    unit = "%",
    color = "#7c5cbf"
  ),
  DGS10 = list(name = "10-Yr Treasury Yield", unit = "%", color = "#ffd166"),
  HOUST = list(name = "Housing Starts", unit = "K", color = "#2dce89"),
  PAYEMS = list(name = "Nonfarm Payrolls", unit = "K", color = "#00b4d8"),
  ICSA = list(name = "Initial Jobless Claims", unit = "K", color = "#06d6a0"),
  RSAFS = list(name = "Retail Sales", unit = "M$", color = "#f4a261"),
  DCOILWTICO = list(name = "WTI Crude Oil", unit = "$/bbl", color = "#e94560")
)

# Series modeled jointly (fed funds reaction function is endogenous to these,
# and PAYEMS/ICSA give the block a live read on labor-market momentum instead
# of inferring it only from UNRATE's own — slower-moving — history)
MACRO_BLOCK_IDS <- c(
  "UNRATE", "CPIAUCSL", "FEDFUNDS", "MORTGAGE30US", "DGS10", "PAYEMS", "ICSA"
)

# Number of simulated future trajectories kept for the fan-chart overlay
# (plot_forecast_chart draws these as thin lines so the forecast visually
# wiggles the way the underlying model's own uncertainty actually looks,
# instead of only showing the smooth median path + a static band).
SAMPLE_PATH_COUNT <- 20L

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Compute RMSE for a numeric vector of residuals
.rmse <- function(residuals) sqrt(mean(residuals^2, na.rm = TRUE))

#' Ljung-Box residual white-noise test p-value (model-adequacy diagnostic).
#' A high p means the residuals are indistinguishable from white noise — the
#' model captured the structure; a low p means autocorrelation is left over.
#' Returns NA when there aren't enough finite residuals to test.
.ljung_box_p <- function(residuals, lag = 10) {
  r <- residuals[is.finite(residuals)]
  L <- min(lag, length(r) - 2L)
  if (length(r) < 5L || L < 1L) {
    return(NA_real_)
  }
  tryCatch(
    stats::Box.test(r, lag = L, type = "Ljung-Box")$p.value,
    error = function(e) NA_real_
  )
}

#' Convert weights inversely proportional to RMSE (lower error → higher weight)
.inverse_rmse_weights <- function(rmse_vec) {
  w <- 1 / rmse_vec
  w / sum(w) # normalise to sum to 1
}

#' Reshape a (paths x horizon) matrix of simulated trajectories into the long
#' tibble (ds, path_id, value) that plot_forecast_chart's fan-chart overlay
#' expects — one row per simulated point.
.sample_paths_long <- function(path_mat, dates) {
  purrr::map_dfr(seq_len(nrow(path_mat)), function(k) {
    tibble::tibble(ds = dates, path_id = k, value = path_mat[k, ])
  })
}

#' Aggregate a raw FRED tibble (daily/weekly/monthly) to true monthly frequency
#' so the "monthly" assumption baked into the ensemble/VAR actually holds
#' (matters for DCOILWTICO [daily] and MORTGAGE30US [weekly]; a no-op for
#' series that are already monthly).
.aggregate_monthly <- function(df) {
  df %>%
    mutate(month = lubridate::floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = month) %>%
    filter(!is.na(value)) %>%
    arrange(date)
}

#' Build the shared recipe: date → time-series features
.make_recipe <- function(train_tbl) {
  recipes::recipe(value ~ date, data = train_tbl) %>%
    recipes::step_mutate(date = as.Date(date))
}

# ── Model specifications (univariate ensemble) ────────────────────────────────

#' Returns a named list of parsnip/workflow model objects
.build_models <- function(train_tbl) {
  rec <- .make_recipe(train_tbl)

  # 1. Prophet ----------------------------------------------------------------
  prophet_spec <- modeltime::prophet_reg(
    seasonality_yearly = TRUE,
    seasonality_weekly = FALSE,
    seasonality_daily = FALSE,
    changepoint_range = 0.8,
    prior_scale_changepoints = 0.05
  ) %>%
    parsnip::set_engine("prophet")

  wf_prophet <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(prophet_spec) %>%
    parsnip::fit(data = train_tbl)

  # 2. Auto ARIMA --------------------------------------------------------------
  arima_spec <- modeltime::arima_reg(
    seasonal_period = 12 # monthly data
  ) %>%
    parsnip::set_engine("auto_arima")

  wf_arima <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(arima_spec) %>%
    parsnip::fit(data = train_tbl)

  # 3. ETS (Error/Trend/Season) -----------------------------------------------
  ets_spec <- modeltime::exp_smoothing(
    seasonal_period = 12
  ) %>%
    parsnip::set_engine("ets")

  wf_ets <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(ets_spec) %>%
    parsnip::fit(data = train_tbl)

  list(prophet = wf_prophet, arima = wf_arima, ets = wf_ets)
}

#' Cross-validated diagnostics for the univariate ensemble: RMSE-based weights
#' AND horizon-scaled backtest residuals for prediction bands, from the same
#' set of expanding-window CV folds (replaces in-sample modeltime_accuracy()).
#'
#' Residuals are pooled after standardising by sqrt(h) (random-walk-style
#' error growth), which lets every fold/horizon observation contribute to one
#' shared quantile estimate instead of splitting into per-horizon buckets too
#' thin to be reliable with only a handful of CV folds.
#'
#' @return list(weights, std_resid) or NULL if there isn't enough history to
#'   carve out CV folds (caller falls back to in-sample accuracy).
.cv_ensemble_diagnostics <- function(train_tbl, horizon_months, min_initial = 36) {
  n <- nrow(train_tbl)

  n_slices <- 5
  initial <- n - horizon_months * n_slices
  while (initial < min_initial && n_slices > 1) {
    n_slices <- n_slices - 1
    initial <- n - horizon_months * n_slices
  }
  if (initial < min_initial) {
    return(NULL)
  }

  splits <- timetk::time_series_cv(
    train_tbl,
    date_var = date,
    initial = initial,
    assess = horizon_months,
    skip = horizon_months,
    slice_limit = n_slices,
    cumulative = TRUE
  )

  fold_resid <- purrr::map_dfr(seq_len(nrow(splits)), function(i) {
    split <- splits$splits[[i]]
    analysis_tbl <- rsample::analysis(split)
    assessment_tbl <- rsample::assessment(split) %>% arrange(date)

    tryCatch(
      {
        models_cv <- .build_models(analysis_tbl)
        model_tbl_cv <- modeltime::modeltime_table(
          models_cv$prophet,
          models_cv$arima,
          models_cv$ets
        )

        fc_cv <- model_tbl_cv %>%
          modeltime::modeltime_forecast(
            new_data = assessment_tbl %>% dplyr::select(date),
            actual_data = analysis_tbl
          ) %>%
          filter(.key == "prediction")

        assessment_tbl %>%
          dplyr::select(date, actual = value) %>%
          mutate(h = row_number()) %>%
          inner_join(
            fc_cv %>% transmute(date = as.Date(.index), .model_id, pred = .value),
            by = "date"
          ) %>%
          mutate(resid = actual - pred, slice = i)
      },
      error = function(e) NULL
    )
  })

  if (is.null(fold_resid) || nrow(fold_resid) == 0) {
    return(NULL)
  }

  # ── CV-based RMSE weights (model order fixed: 1=prophet, 2=arima, 3=ets) ---
  rmse_by_model <- fold_resid %>%
    group_by(.model_id) %>%
    summarise(rmse = .rmse(resid), .groups = "drop")

  rmse_vec <- rmse_by_model$rmse[match(1:3, rmse_by_model$.model_id)]
  if (any(is.na(rmse_vec)) || any(!is.finite(rmse_vec))) {
    weights <- rep(1 / 3, 3)
  } else {
    weights <- .inverse_rmse_weights(rmse_vec)
  }
  names(weights) <- c("prophet", "arima", "ets")
  cv_rmse <- setNames(rmse_vec, c("prophet", "arima", "ets"))

  # ── Horizon-scaled ensemble residual pool (for prediction bands) -----------
  ensemble_resid <- fold_resid %>%
    mutate(w = weights[.model_id]) %>%
    group_by(slice, h) %>%
    summarise(resid = sum(w * resid) / sum(w), .groups = "drop") %>%
    mutate(std_resid = resid / sqrt(h))

  list(weights = weights, std_resid = ensemble_resid$std_resid, cv_rmse = cv_rmse)
}

# ── Core ensemble runner (univariate series only) ─────────────────────────────

#' Fit ensemble and produce forecast
#'
#' @param series_df  data.frame with columns `date` (Date) and `value` (numeric)
#' @param horizon_months  integer — forecast periods ahead
#' @param ci_level  confidence interval width (default 0.90)
#'
#' @return list(data, model_table, weights, horizon) or NULL on failure
run_ensemble <- function(
  series_df,
  horizon_months = 18,
  ci_level = 0.90
) {
  if (is.null(series_df) || nrow(series_df) < 24) {
    return(NULL)
  }

  # ── Prep training table (modeltime expects `date` + `value`) ----------------
  train_tbl <- series_df %>%
    dplyr::select(date, value) %>%
    filter(!is.na(value)) %>%
    arrange(date) %>%
    mutate(date = as.Date(date))

  tryCatch(
    {
      suppressMessages({
        suppressWarnings({
          # ── Fit individual models ------------------------------------------------
          models <- .build_models(train_tbl)

          # ── Build modeltime table ------------------------------------------------
          model_tbl <- modeltime::modeltime_table(
            models$prophet,
            models$arima,
            models$ets
          )

          # ── In-sample per-model accuracy + residual white-noise test ------------
          #   Computed unconditionally now: it feeds the Model Diagnostics tab,
          #   and doubles as the weighting fallback when there isn't enough
          #   history to cross-validate. Calibrating on the training set once
          #   gives us both the accuracy metrics and the residuals the
          #   Ljung-Box adequacy test needs. modeltime_table order is fixed
          #   (1=prophet, 2=arima, 3=ets) so rows line up with those names.
          calib_tbl <- model_tbl %>%
            modeltime::modeltime_calibrate(new_data = train_tbl, quiet = TRUE)
          accuracy_tbl <- calib_tbl %>%
            modeltime::modeltime_accuracy() %>%
            arrange(.model_id)
          rmse_vec <- accuracy_tbl$rmse

          wn_by_model <- calib_tbl %>%
            modeltime::modeltime_residuals() %>%
            group_by(.model_id) %>%
            summarise(wn_p = .ljung_box_p(.residuals), .groups = "drop")
          accuracy_tbl <- accuracy_tbl %>%
            left_join(wn_by_model, by = ".model_id")

          # ── Cross-validated weights + backtest residuals -------------------------
          cv_diag <- .cv_ensemble_diagnostics(train_tbl, horizon_months)

          if (!is.null(cv_diag)) {
            weights <- cv_diag$weights
            cv_rmse <- cv_diag$cv_rmse # per-model out-of-sample RMSE (weight basis)
            weight_basis <- "cv"
          } else {
            # Not enough history to carve out CV folds — fall back to in-sample
            weights <- if (any(!is.finite(rmse_vec))) {
              rep(1 / 3, 3)
            } else {
              .inverse_rmse_weights(rmse_vec)
            }
            names(weights) <- c("prophet", "arima", "ets")
            cv_rmse <- NULL
            weight_basis <- "insample"
          }

          # ── Build weighted ensemble model ----------------------------------------
          ensemble_model <- model_tbl %>%
            modeltime.ensemble::ensemble_weighted(loadings = weights)

          ensemble_tbl <- modeltime::modeltime_table(ensemble_model)

          # ── Blended-ensemble accuracy + residual white-noise test ---------------
          #   One honest row for the combined model (not just its members), so
          #   the overview table can show each indicator on a single line.
          calib_ens <- ensemble_tbl %>%
            modeltime::modeltime_calibrate(new_data = train_tbl, quiet = TRUE)
          ensemble_accuracy <- calib_ens %>% modeltime::modeltime_accuracy()
          ensemble_accuracy$wn_p <- .ljung_box_p(
            (calib_ens %>% modeltime::modeltime_residuals())$.residuals
          )

          # ── Future date frame (monthly) -----------------------------------------
          future_tbl <- timetk::future_frame(
            train_tbl,
            .date_var = date,
            .length_out = horizon_months
          )

          # ── Forecast (point path only — bands come from CV residuals below) -----
          forecast_tbl <- ensemble_tbl %>%
            modeltime::modeltime_forecast(
              new_data = future_tbl,
              actual_data = train_tbl
            )

          # ── Prediction bands from horizon-scaled backtest residuals -------------
          alpha <- (1 - ci_level) / 2
          pred_dates <- sort(unique(
            forecast_tbl$.index[forecast_tbl$.key == "prediction"]
          ))
          h_of <- setNames(seq_along(pred_dates), as.character(pred_dates))

          if (!is.null(cv_diag) && length(cv_diag$std_resid) >= 8) {
            q_lo <- unname(quantile(cv_diag$std_resid, alpha, na.rm = TRUE))
            q_hi <- unname(quantile(cv_diag$std_resid, 1 - alpha, na.rm = TRUE))
          } else {
            # Too little history to backtest — still widen with horizon, using
            # the (weighted) in-sample RMSE as the one-step-ahead error scale.
            z <- qnorm(1 - alpha)
            fallback_rmse <- sum(weights * rmse_vec)
            q_lo <- -z * fallback_rmse
            q_hi <- z * fallback_rmse
          }

          # ── Reformat to match original output contract -------------------------
          #   Columns: ds, yhat, yhat_lower, yhat_upper, y, is_forecast
          result <- forecast_tbl %>%
            filter(.key %in% c("actual", "prediction")) %>%
            mutate(
              h = if_else(
                .key == "prediction",
                h_of[as.character(as.Date(.index))],
                NA_integer_
              )
            ) %>%
            transmute(
              ds = as.Date(.index),
              yhat = .value,
              yhat_lower = if_else(
                .key == "prediction",
                .value + q_lo * sqrt(h),
                NA_real_
              ),
              yhat_upper = if_else(
                .key == "prediction",
                .value + q_hi * sqrt(h),
                NA_real_
              ),
              y = if_else(.key == "actual", .value, NA_real_),
              is_forecast = (.key == "prediction")
            )

          # ── Sample paths for the fan-chart overlay -------------------------
          #   Bootstrap trajectories by cumulatively summing resampled
          #   one-step-ahead-scale innovations onto the point forecast — the
          #   same "random-walk-style error growth" assumption already used
          #   for the CI band above, just realized as individual paths
          #   instead of collapsed into quantiles.
          fore_yhat <- result$yhat[result$is_forecast]
          fore_dates <- result$ds[result$is_forecast]
          horizon_n <- length(fore_yhat)

          noise_pool <- if (!is.null(cv_diag) && length(cv_diag$std_resid) >= 8) {
            cv_diag$std_resid
          } else if (exists("rmse_vec", inherits = FALSE)) {
            rnorm(2000, mean = 0, sd = sum(weights * rmse_vec))
          } else {
            # cv_diag existed but its residual pool was too thin — rare edge
            # case; fall back to a fresh in-sample noise estimate.
            acc_tbl <- model_tbl %>% modeltime::modeltime_accuracy(new_data = train_tbl)
            rv <- acc_tbl$rmse
            sd_est <- if (any(!is.finite(rv))) NA_real_ else sum(weights * rv)
            if (is.na(sd_est) || sd_est <= 0) {
              sd_est <- sd(train_tbl$value, na.rm = TRUE) * 0.05
            }
            rnorm(2000, mean = 0, sd = sd_est)
          }

          noise_mat <- matrix(
            sample(noise_pool, SAMPLE_PATH_COUNT * horizon_n, replace = TRUE),
            nrow = SAMPLE_PATH_COUNT,
            ncol = horizon_n
          )
          path_mat <- sweep(
            t(apply(noise_mat, 1, cumsum)),
            2,
            fore_yhat,
            "+"
          )
          sample_paths_tbl <- .sample_paths_long(path_mat, fore_dates)
        }) # end suppressWarnings
      }) # end suppressMessages

      list(
        data = result,
        model_table = ensemble_tbl,
        weights = weights,
        horizon = horizon_months,
        sample_paths = sample_paths_tbl,
        accuracy = accuracy_tbl,
        ensemble_accuracy = ensemble_accuracy,
        cv_rmse = cv_rmse,
        weight_basis = weight_basis,
        n_obs = nrow(train_tbl),
        hist_start = min(train_tbl$date),
        hist_end = max(train_tbl$date),
        series_avg = mean(train_tbl$value, na.rm = TRUE),
        series_sd_diff = stats::sd(diff(train_tbl$value), na.rm = TRUE),
        model_family = "ensemble"
      )
    },
    error = function(e) {
      message(glue("Ensemble forecast failed: {conditionMessage(e)}"))
      NULL
    }
  )
}

# ── Macro block runner (joint BVAR) ───────────────────────────────────────────

#' Jointly forecast the rates/inflation/unemployment cluster with a Bayesian
#' VAR (Minnesota prior). The prior shrinks each equation toward a random
#' walk, so persistent series don't need to be differenced to be well-behaved
#' — but a series is still differenced first when an ADF test can't reject a
#' unit root, since a flat-out random walk in levels blows up the lag
#' structure the BVAR estimates. Differenced series are re-integrated to
#' levels at the *draw* level (cumsum per posterior path) before taking
#' quantiles, so cross-horizon correlation in the forecast bands is preserved.
#'
#' @param macro_wide  tibble with `date` plus one column per id in
#'   `MACRO_BLOCK_IDS` (CPIAUCSL expected already converted to YoY %),
#'   monthly and aligned.
#' @return named list keyed by MACRO_BLOCK_IDS, each element shaped like
#'   run_ensemble()'s return value, or NULL on failure / insufficient data.
run_macro_bvar <- function(macro_wide, horizon_months = 18, ci_level = 0.90) {
  # Driven by the MACRO_BLOCK_IDS constant (not hardcoded here) so the two
  # can never drift out of sync.
  vars_order <- MACRO_BLOCK_IDS

  if (is.null(macro_wide) || !all(vars_order %in% names(macro_wide))) {
    return(NULL)
  }

  macro_wide <- macro_wide %>%
    arrange(date) %>%
    filter(if_all(all_of(vars_order), ~ !is.na(.)))

  if (nrow(macro_wide) < 36) {
    return(NULL)
  }

  tryCatch(
    {
      suppressMessages({
        suppressWarnings({
          mat <- as.matrix(macro_wide[, vars_order])

          # ── Unit-root check → difference only the series that need it -----------
          needs_diff <- vapply(vars_order, function(v) {
            p <- tryCatch(
              tseries::adf.test(mat[, v])$p.value,
              error = function(e) NA_real_
            )
            is.na(p) || p > 0.10
          }, logical(1))
          names(needs_diff) <- vars_order

          last_level <- mat[nrow(mat), ]
          mat_fit <- mat
          for (v in vars_order) {
            if (needs_diff[[v]]) {
              mat_fit[, v] <- c(NA_real_, diff(mat[, v]))
            }
          }
          mat_fit <- mat_fit[-1, , drop = FALSE]

          # ── Standardize before fitting ------------------------------------------
          #   The block now mixes percent-scale rates (O(1-10)) with raw
          #   levels like payrolls/claims (O(1e5)-O(1e5)). The Minnesota
          #   prior is scale-aware in theory, but that scale gap still leaves
          #   the sampler numerically ill-conditioned in practice — verified
          #   empirically (synthetic data at realistic magnitudes reproduced
          #   exploding, sign-flipping forecasts before this fix). Z-scoring
          #   each column removes the scale gap; draws are converted back to
          #   each series' native scale immediately after prediction, before
          #   the differencing reintegration below.
          col_mean <- colMeans(mat_fit)
          col_sd <- apply(mat_fit, 2, sd)
          col_sd[!is.finite(col_sd) | col_sd == 0] <- 1
          mat_fit_std <- sweep(sweep(mat_fit, 2, col_mean, "-"), 2, col_sd, "/")

          # ── Lag order via AIC (vars::VARselect), then Minnesota-prior BVAR -------
          lag_max <- min(12, max(2, floor(nrow(mat_fit_std) / 15)))
          p <- tryCatch(
            unname(vars::VARselect(mat_fit_std, lag.max = lag_max, type = "const")$selection["AIC(n)"]),
            error = function(e) 4
          )
          p <- max(1, min(p, 8))

          fit <- BVAR::bvar(
            mat_fit_std,
            lags = p,
            n_draw = 5000L,
            n_burn = 1000L,
            priors = BVAR::bv_priors(
              mn = BVAR::bv_mn(lambda = BVAR::bv_lambda(mode = 0.2))
            ),
            verbose = FALSE
          )

          alpha <- (1 - ci_level) / 2
          fc <- predict(fit, horizon = horizon_months, conf_bands = alpha)

          # fc$fcast: [draw, horizon, var] — var order matches vars_order,
          # still standardized at this point. Un-standardize back to each
          # series' native (differenced-or-level) scale first.
          draws <- fc$fcast
          for (j in seq_along(vars_order)) {
            draws[, , j] <- draws[, , j] * col_sd[j] + col_mean[j]
          }

          level_draws <- draws
          for (j in seq_along(vars_order)) {
            v <- vars_order[j]
            if (needs_diff[[v]]) {
              level_draws[, , j] <- last_level[[v]] + t(apply(draws[, , j], 1, cumsum))
            }
          }

          point <- apply(level_draws, c(2, 3), median)
          lo <- apply(level_draws, c(2, 3), quantile, probs = alpha)
          hi <- apply(level_draws, c(2, 3), quantile, probs = 1 - alpha)

          # ── In-sample per-equation fit (for the Model Diagnostics tab) ----------
          #   fitted()/residuals() are on the standardized (and, where applied,
          #   differenced) scale the BVAR was estimated on. R² is scale-free so
          #   it carries over directly; RMSE is rescaled back to each series'
          #   native units (× col_sd) so it reads in the series' own scale.
          eq_fit <- tryCatch(
            {
              fv <- fitted(fit, type = "mean")
              rs <- residuals(fit, type = "mean")
              act <- fv + rs
              rsq <- vapply(seq_along(vars_order), function(j) {
                sse <- sum(rs[, j]^2)
                sst <- sum((act[, j] - mean(act[, j]))^2)
                if (!is.finite(sst) || sst == 0) NA_real_ else 1 - sse / sst
              }, numeric(1))
              rmse_native <- vapply(seq_along(vars_order), function(j) {
                sqrt(mean((rs[, j] * col_sd[j])^2))
              }, numeric(1))
              mae_native <- vapply(seq_along(vars_order), function(j) {
                mean(abs(rs[, j] * col_sd[j]))
              }, numeric(1))
              wn_p <- vapply(seq_along(vars_order), function(j) {
                .ljung_box_p(rs[, j])
              }, numeric(1))
              list(rsq = rsq, rmse = rmse_native, mae = mae_native, wn_p = wn_p)
            },
            error = function(e) NULL
          )

          future_dates <- seq(
            max(macro_wide$date),
            by = "1 month",
            length.out = horizon_months + 1
          )[-1]

          weights <- c(BVAR = 1)

          # ── Sample paths for the fan-chart overlay ------------------------
          #   We already drew thousands of full posterior trajectories to get
          #   `point`/`lo`/`hi` above — keep a handful of the actual draws
          #   (already re-integrated to levels) instead of discarding them,
          #   so the chart can show real posterior paths rather than just
          #   their smoothed median.
          n_draws_total <- dim(level_draws)[1]
          path_idx <- unique(round(seq(1, n_draws_total, length.out = SAMPLE_PATH_COUNT)))

          setNames(
            lapply(seq_along(vars_order), function(j) {
              v <- vars_order[j]
              hist_df <- tibble::tibble(
                ds = macro_wide$date,
                yhat = mat[, v],
                yhat_lower = NA_real_,
                yhat_upper = NA_real_,
                y = mat[, v],
                is_forecast = FALSE
              )
              fc_df <- tibble::tibble(
                ds = future_dates,
                yhat = point[, j],
                yhat_lower = lo[, j],
                yhat_upper = hi[, j],
                y = NA_real_,
                is_forecast = TRUE
              )
              list(
                data = bind_rows(hist_df, fc_df),
                model_table = fit,
                weights = weights,
                horizon = horizon_months,
                sample_paths = .sample_paths_long(
                  {
                    m <- level_draws[path_idx, , j]
                    # Guard the length(path_idx) == 1 edge case, where
                    # indexing already dropped the path dimension too.
                    if (is.null(dim(m))) matrix(m, nrow = length(path_idx)) else m
                  },
                  future_dates
                ),
                accuracy = tibble::tibble(
                  rsq = if (is.null(eq_fit)) NA_real_ else eq_fit$rsq[j],
                  rmse = if (is.null(eq_fit)) NA_real_ else eq_fit$rmse[j],
                  mae = if (is.null(eq_fit)) NA_real_ else eq_fit$mae[j],
                  wn_p = if (is.null(eq_fit)) NA_real_ else eq_fit$wn_p[j],
                  lag_order = p,
                  differenced = unname(needs_diff[[v]])
                ),
                n_obs = nrow(macro_wide),
                hist_start = min(macro_wide$date),
                hist_end = max(macro_wide$date),
                series_avg = mean(mat[, v], na.rm = TRUE),
                series_sd_diff = stats::sd(diff(mat[, v]), na.rm = TRUE),
                model_family = "bvar"
              )
            }),
            vars_order
          )
        }) # end suppressWarnings
      }) # end suppressMessages
    },
    error = function(e) {
      message(glue("Macro BVAR failed: {conditionMessage(e)}"))
      NULL
    }
  )
}

# ── Public API (mirrors original interface) ───────────────────────────────────

#' Drop-in replacement for the original run_all_forecasts()
run_all_forecasts <- function(fred_data, horizon_months = 18) {
  out <- list()

  # ── Aggregate every series to true monthly frequency at ingestion ---------
  #   (a no-op for series already monthly; fixes the frequency assumption for
  #   DCOILWTICO [daily] and MORTGAGE30US [weekly])
  monthly_data <- list()
  for (sid in names(FORECAST_SERIES)) {
    raw_df <- fred_data[[sid]]
    if (is.null(raw_df) || nrow(raw_df) < 24) {
      next
    }
    monthly_data[[sid]] <- .aggregate_monthly(raw_df)
  }

  # CPI: convert to YoY % before forecasting (unit already "YoY %" in metadata)
  if (!is.null(monthly_data[["CPIAUCSL"]])) {
    monthly_data[["CPIAUCSL"]] <- monthly_data[["CPIAUCSL"]] %>%
      arrange(date) %>%
      mutate(value = (value / lag(value, 12) - 1) * 100) %>%
      filter(!is.na(value))
  }

  # ── Macro block: UNRATE / CPI YoY / FEDFUNDS / MORTGAGE30US, jointly ------
  have_macro <- all(vapply(
    MACRO_BLOCK_IDS,
    function(sid) !is.null(monthly_data[[sid]]),
    logical(1)
  ))

  if (have_macro) {
    macro_wide <- purrr::reduce(
      purrr::map(MACRO_BLOCK_IDS, function(sid) {
        monthly_data[[sid]] %>% dplyr::select(date, value) %>% rename(!!sid := value)
      }),
      full_join,
      by = "date"
    ) %>%
      arrange(date)

    message("Fitting macro BVAR block (UNRATE, CPI YoY, FEDFUNDS, MORTGAGE30US, 10Y Treasury, payrolls, claims)...")
    macro_fc <- run_macro_bvar(macro_wide, horizon_months = horizon_months)

    if (!is.null(macro_fc)) {
      for (sid in MACRO_BLOCK_IDS) {
        out[[sid]] <- macro_fc[[sid]]
      }
    } else {
      message(
        "Macro BVAR failed — falling back to univariate ensembles for the rate/inflation cluster."
      )
      have_macro <- FALSE
    }
  }

  # ── Remaining series: independent univariate ensembles ---------------------
  univariate_ids <- setdiff(
    names(FORECAST_SERIES),
    if (have_macro) MACRO_BLOCK_IDS else character()
  )

  for (sid in univariate_ids) {
    raw_df <- monthly_data[[sid]]
    if (is.null(raw_df)) {
      next
    }
    message(glue("Ensemble forecasting {sid}..."))
    out[[sid]] <- run_ensemble(raw_df, horizon_months = horizon_months)
  }

  out
}

# ── Plotting (unchanged contract) ────────────────────────────────────────────

#' Human-readable label for a model key in the weights vector
.pretty_model_name <- function(key) {
  switch(
    key,
    prophet = "Prophet",
    arima = "ARIMA",
    ets = "ETS",
    BVAR = "BVAR",
    toupper(key)
  )
}

plot_forecast_chart <- function(fc_result, series_id) {
  if (is.null(fc_result)) {
    return(
      plot_ly() %>%
        layout(
          title = "Insufficient data",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#cccccc")
        ) %>%
        config(displayModeBar = FALSE)
    )
  }

  cfg <- FORECAST_SERIES[[series_id]]
  df <- fc_result$data
  hist <- df %>% filter(!is_forecast)
  fore <- df %>% filter(is_forecast)
  col <- cfg$color
  cutoff <- max(hist$ds)

  # Build weight label for subtitle (generic across ensemble / BVAR weights)
  w <- fc_result$weights
  is_bvar <- identical(names(w), "BVAR")
  method_lbl <- if (is_bvar) "Joint BVAR Forecast" else "Ensemble Forecast"
  w_lbl <- if (is_bvar) {
    "Jointly modeled with unemployment, CPI, Fed funds, mortgage rate, 10-year Treasury, payrolls & jobless claims"
  } else {
    paste(
      mapply(
        function(nm, val) glue("{.pretty_model_name(nm)} {round(val * 100)}%"),
        names(w),
        w
      ),
      collapse = " · "
    )
  }

  p <- plot_ly()

  # Simulated future paths (fan chart) — drawn first so they sit behind the
  # ribbon/median line. Individual trajectories, not the smoothed median,
  # are what actually shows the model's variation, so these are the fix for
  # forecasts otherwise looking like a single straight/curved line.
  paths <- fc_result$sample_paths
  if (!is.null(paths) && nrow(paths) > 0) {
    first_id <- min(paths$path_id)
    p <- p %>%
      add_lines(
        data = paths[paths$path_id == first_id, ],
        x = ~ds,
        y = ~value,
        line = list(color = paste0(col, "22"), width = 1),
        name = "Simulated Paths",
        showlegend = TRUE,
        hoverinfo = "none"
      ) %>%
      add_lines(
        data = paths[paths$path_id != first_id, ],
        x = ~ds,
        y = ~value,
        split = ~path_id,
        line = list(color = paste0(col, "22"), width = 1),
        showlegend = FALSE,
        hoverinfo = "none"
      )
  }

  p %>%
    # Confidence ribbon (forecast only)
    add_ribbons(
      data = fore,
      x = ~ds,
      ymin = ~yhat_lower,
      ymax = ~yhat_upper,
      fillcolor = paste0(col, "30"),
      line = list(color = "transparent"),
      name = glue("{round(100 * 0.90)}% CI"),
      showlegend = TRUE,
      hoverinfo = "none"
    ) %>%
    # Actual historical values
    add_lines(
      data = hist,
      x = ~ds,
      y = ~y,
      line = list(color = col, width = 2.5),
      name = "Actual"
    ) %>%
    # Bridge: last 24 months of actuals shown as dotted to connect into forecast
    add_lines(
      data = tail(hist, 24),
      x = ~ds,
      y = ~y,
      line = list(color = paste0(col, "99"), width = 1.5, dash = "dot"),
      name = "Recent Trend"
    ) %>%
    # Forecast line
    add_lines(
      data = fore,
      x = ~ds,
      y = ~yhat,
      line = list(color = col, width = 2.5, dash = "dash"),
      name = method_lbl
    ) %>%
    # Forecast cutoff vertical
    add_segments(
      x = cutoff,
      xend = cutoff,
      y = min(c(df$yhat_lower, df$y), na.rm = TRUE),
      yend = max(c(df$yhat_upper, df$y), na.rm = TRUE),
      line = list(color = "#ffffff44", width = 1, dash = "dash"),
      name = "Forecast Start",
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
    layout(
      title = list(
        text = paste0(
          cfg$name,
          glue(" — {fc_result$horizon}-Month "),
          method_lbl,
          "<br><sup>",
          w_lbl,
          "</sup>"
        ),
        font = list(color = "#e0e0e0", size = 14)
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#cccccc"),
      xaxis = list(title = "", gridcolor = "#2a3042", color = "#9aa3b2"),
      yaxis = list(title = cfg$unit, gridcolor = "#2a3042", color = "#9aa3b2"),
      legend = list(
        font = list(color = "#cccccc"),
        bgcolor = "rgba(0,0,0,0)",
        orientation = "h",
        y = -0.2
      ),
      margin = list(t = 60, r = 20, b = 80, l = 65),
      hovermode = "x unified"
    ) %>%
    config(displayModeBar = FALSE)
}

# ── Summary table (unchanged contract) ───────────────────────────────────────

forecast_summary_table <- function(forecasts) {
  map_dfr(names(forecasts), function(sid) {
    fc <- forecasts[[sid]]
    if (is.null(fc)) {
      return(NULL)
    }

    cfg <- FORECAST_SERIES[[sid]]
    df <- fc$data
    cur <- df %>% filter(!is_forecast) %>% slice_tail(n = 1)
    f6 <- df %>% filter(is_forecast) %>% slice(min(6, n()))
    f18 <- df %>% slice_tail(n = 1)

    # Weights summary (generic across ensemble / BVAR)
    w <- fc$weights
    w_str <- if (identical(names(w), "BVAR")) {
      "BVAR (joint)"
    } else {
      glue(
        "P{round(w['prophet'] * 100)}/A{round(w['arima'] * 100)}/E{round(w['ets'] * 100)}"
      )
    }

    tibble(
      Indicator = cfg$name,
      Current = round(cur$y, 2),
      `6M Fcst` = round(f6$yhat, 2),
      `6M CI` = paste0(
        "[",
        round(f6$yhat_lower, 2),
        ", ",
        round(f6$yhat_upper, 2),
        "]"
      ),
      `18M Fcst` = round(f18$yhat, 2),
      Unit = cfg$unit,
      `Weights (P/A/E)` = w_str
    )
  })
}

# ── Model diagnostics (goodness of fit) ───────────────────────────────────────

#' Overview table: one row per forecasted indicator, directly comparable.
#'
#' For an ensemble series the row is the *blended* ensemble model's in-sample
#' fit; for a jointly-modeled series it's that series' own BVAR equation. The
#' Detail column carries the family-specific extras (ensemble blend weights, or
#' the BVAR's lag order + I(1) flag) so the shared columns stay comparable.
#'
#' @return one tibble (Indicator · Method · Obs · R² · RMSE · MAE · Resid WN (p)
#'   · Detail), or NULL/zero-row when nothing is available.
forecast_diagnostics_overview <- function(forecasts) {
  purrr::map_dfr(names(forecasts), function(sid) {
    fc <- forecasts[[sid]]
    if (is.null(fc) || is.null(fc$accuracy)) {
      return(NULL)
    }
    cfg <- FORECAST_SERIES[[sid]]
    family <- if (is.null(fc$model_family)) "ensemble" else fc$model_family
    n_obs <- if (is.null(fc$n_obs)) NA_integer_ else fc$n_obs
    is_diff <- FALSE

    if (identical(family, "bvar")) {
      acc <- fc$accuracy
      method <- "Joint BVAR"
      rsq <- acc$rsq; rmse <- acc$rmse; mae <- acc$mae; wn <- acc$wn_p
      is_diff <- isTRUE(acc$differenced)
      detail <- paste0(
        acc$lag_order, " lags · ",
        if (is_diff) "I(1)" else "levels"
      )
    } else {
      acc <- fc$ensemble_accuracy
      w <- fc$weights
      method <- "Ensemble"
      rsq <- acc$rsq; rmse <- acc$rmse; mae <- acc$mae; wn <- acc$wn_p
      detail <- sprintf(
        "P%d / A%d / E%d",
        round(100 * unname(w["prophet"])),
        round(100 * unname(w["arima"])),
        round(100 * unname(w["ets"]))
      )
    }

    # Anchor the raw error: normalize RMSE by the scale it lives on — the
    # average level for level-fit rows, the typical monthly move (SD of
    # changes) for I(1) rows whose RMSE is itself in change units.
    series_avg <- if (is.null(fc$series_avg)) NA_real_ else fc$series_avg
    denom <- if (is_diff) fc$series_sd_diff else abs(series_avg)
    err_pct <- if (is.null(denom) || !is.finite(denom) || denom == 0) {
      NA_real_
    } else {
      round(100 * rmse / denom, 1)
    }

    tibble::tibble(
      Indicator = cfg$name,
      Method = method,
      Obs = n_obs,
      `R²` = round(rsq, 3),
      RMSE = round(rmse, 3),
      MAE = round(mae, 3),
      `Series Avg` = round(series_avg, if (abs(series_avg) >= 100) 0 else 2),
      `Err %` = err_pct,
      `Resid WN (p)` = round(wn, 3),
      Detail = detail
    )
  })
}

#' Per-model detail table for one selected indicator (shown under the overview).
#'   • ensemble → one row per member (Prophet / Auto ARIMA / ETS) with in-sample
#'     RMSE/MAE/MAPE/MASE/R², the residual white-noise p, and the blend weight.
#'   • bvar     → the joint model's own equation for this series (R², native
#'     RMSE/MAE, white-noise p, lag order, I(1) flag).
#'
#' @return list(table, caption, family), or NULL if there's nothing to show.
forecast_diagnostics_detail <- function(fc_result, series_id) {
  if (is.null(fc_result) || is.null(fc_result$accuracy)) {
    return(NULL)
  }
  cfg <- FORECAST_SERIES[[series_id]]
  family <- if (is.null(fc_result$model_family)) {
    "ensemble"
  } else {
    fc_result$model_family
  }

  if (identical(family, "bvar")) {
    acc <- fc_result$accuracy
    tbl <- tibble::tibble(
      Model = "Bayesian VAR (joint)",
      `R²` = round(acc$rsq, 3),
      RMSE = round(acc$rmse, 3),
      MAE = round(acc$mae, 3),
      `Resid WN (p)` = round(acc$wn_p, 3),
      `Lag Order` = acc$lag_order,
      Differenced = ifelse(isTRUE(acc$differenced), "Yes — I(1)", "No")
    )
    caption <- glue(
      "{cfg$name} is one equation of the joint Bayesian VAR. R², RMSE and MAE ",
      "are in-sample one-step-ahead (RMSE & MAE in native units; a differenced ",
      "series shows its differenced equation). Resid WN (p) is a Ljung-Box ",
      "white-noise test on the residuals — higher is better (little structure left)."
    )
    return(list(table = tbl, caption = caption, family = family))
  }

  # ── ensemble ───────────────────────────────────────────────────────────────
  acc <- fc_result$accuracy %>% arrange(.model_id)
  w <- fc_result$weights
  key_by_id <- c("prophet", "arima", "ets") # modeltime_table order
  pretty <- c(prophet = "Prophet", arima = "Auto ARIMA", ets = "ETS")
  keys <- key_by_id[acc$.model_id]
  wn_p <- if ("wn_p" %in% names(acc)) round(acc$wn_p, 3) else NA_real_

  # CV RMSE is the actual weight basis: Weight % ∝ 1 / CV RMSE. It usually
  # reorders the models versus the (optimistic) in-sample RMSE, which is why
  # the best in-sample fit is not always the highest-weighted model.
  cv_rmse_vec <- fc_result$cv_rmse
  cv_col <- if (is.null(cv_rmse_vec)) {
    rep(NA_real_, length(keys))
  } else {
    round(unname(cv_rmse_vec[keys]), 3)
  }

  tbl <- tibble::tibble(
    Model = unname(pretty[keys]),
    `RMSE (in-samp)` = round(acc$rmse, 3),
    MAE = round(acc$mae, 3),
    MAPE = round(acc$mape, 2),
    MASE = round(acc$mase, 3),
    `R²` = round(acc$rsq, 3),
    `Resid WN (p)` = wn_p,
    `CV RMSE` = cv_col,
    `Weight %` = round(100 * unname(w[keys]))
  )
  basis_note <- if (identical(fc_result$weight_basis, "insample")) {
    glue(
      "Too little history to cross-validate here, so the weights fell back to ",
      "the in-sample RMSE (CV RMSE shown blank)."
    )
  } else {
    glue(
      "Weight % ∝ 1 / CV RMSE — the cross-validated (out-of-sample) error shown ",
      "here — so a model that fits the training data best can still get less ",
      "weight if it generalizes worse (which is why ARIMA/ETS can swap ranks)."
    )
  }
  caption <- glue(
    "Per-model fit for {cfg$name} (the overview row above blends these). ",
    "RMSE (in-samp)/MAE/MAPE/MASE/R² are in-sample (optimistic, fit on the ",
    "training data). {basis_note} Resid WN (p) is a Ljung-Box white-noise test ",
    "on each model's residuals — higher is better."
  )
  list(table = tbl, caption = caption, family = family)
}
