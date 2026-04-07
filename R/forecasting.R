# ─────────────────────────────────────────────────────────────────────────────
# R/forecasting.R — Ensemble time-series forecasting (modeltime.ensemble style)
#
# Models combined:
#   1. Prophet          (trend + seasonality)
#   2. Auto ARIMA       (short-run dynamics)
#   3. ETS              (error/trend/season smoothing)
#
# Ensemble strategy: weighted mean (weights tuned by in-sample RMSE).
# Output structure is identical to the original single-model version so all
# Shiny UI / plotting code continues to work without changes.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(glue)
  library(plotly)

  # modeltime ecosystem
  library(modeltime)
  library(modeltime.ensemble)
  library(timetk) # future_frame(), pad_by_time(), etc.
  library(parsnip)
  library(workflows)
  library(recipes)
  library(rsample)
  library(rlang) # sym() for tidy eval

  # underlying engines
  library(prophet) # engine for prophet_reg
  library(forecast) # engine for arima_reg / exp_smoothing
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
  HOUST = list(name = "Housing Starts", unit = "K", color = "#2dce89"),
  PAYEMS = list(name = "Nonfarm Payrolls", unit = "K", color = "#00b4d8"),
  RSAFS = list(name = "Retail Sales", unit = "M$", color = "#f4a261"),
  DCOILWTICO = list(name = "WTI Crude Oil", unit = "$/bbl", color = "#e94560")
)

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Compute RMSE for a numeric vector of residuals
.rmse <- function(residuals) sqrt(mean(residuals^2, na.rm = TRUE))

#' Convert weights inversely proportional to RMSE (lower error → higher weight)
.inverse_rmse_weights <- function(rmse_vec) {
  w <- 1 / rmse_vec
  w / sum(w) # normalise to sum to 1
}

#' Build the shared recipe: date → time-series features
.make_recipe <- function(train_tbl) {
  recipes::recipe(value ~ date, data = train_tbl) %>%
    recipes::step_mutate(date = as.Date(date))
}

# ── Model specifications ───────────────────────────────────────────────────────

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

# ── Core ensemble runner ───────────────────────────────────────────────────────

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
    select(date, value) %>%
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

          # ── In-sample accuracy to compute ensemble weights ----------------------
          accuracy_tbl <- model_tbl %>%
            modeltime::modeltime_accuracy(new_data = train_tbl)

          rmse_vec <- accuracy_tbl$rmse
          # Guard against NA/Inf (fallback: equal weights)
          if (any(!is.finite(rmse_vec))) {
            weights <- rep(1 / 3, 3)
          } else {
            weights <- .inverse_rmse_weights(rmse_vec)
          }
          names(weights) <- c("prophet", "arima", "ets")

          # ── Build weighted ensemble model ----------------------------------------
          ensemble_model <- model_tbl %>%
            modeltime.ensemble::ensemble_weighted(loadings = weights)

          ensemble_tbl <- modeltime::modeltime_table(ensemble_model)

          # ── Future date frame (monthly) -----------------------------------------
          future_tbl <- timetk::future_frame(
            train_tbl,
            .date_var = date,
            .length_out = horizon_months
          )

          # ── Forecast ------------------------------------------------------------
          forecast_tbl <- ensemble_tbl %>%
            modeltime::modeltime_forecast(
              new_data = future_tbl,
              actual_data = train_tbl,
              conf_interval = ci_level
            )

          # ── Detect CI column names (vary across modeltime versions) -----------
          #   Possible names: .conf_lo/.conf_hi  OR  .conf_lower/.conf_upper
          fc_cols <- names(forecast_tbl)
          ci_lo_col <- intersect(c(".conf_lo", ".conf_lower"), fc_cols)[1]
          ci_hi_col <- intersect(c(".conf_hi", ".conf_upper"), fc_cols)[1]

          if (is.na(ci_lo_col) || is.na(ci_hi_col)) {
            # Fallback: approximate 90% CI as ±1.645 SD of forecast values
            pred_sd <- sd(
              forecast_tbl$.value[forecast_tbl$.key == "prediction"],
              na.rm = TRUE
            )
            forecast_tbl <- forecast_tbl %>%
              mutate(
                .ci_lo_safe = .value - 1.645 * pred_sd,
                .ci_hi_safe = .value + 1.645 * pred_sd
              )
            ci_lo_col <- ".ci_lo_safe"
            ci_hi_col <- ".ci_hi_safe"
          }

          # ── Reformat to match original output contract -------------------------
          #   Columns: ds, yhat, yhat_lower, yhat_upper, y, is_forecast
          result <- forecast_tbl %>%
            filter(.key %in% c("actual", "prediction")) %>%
            rename(
              .ci_lo = !!rlang::sym(ci_lo_col),
              .ci_hi = !!rlang::sym(ci_hi_col)
            ) %>%
            transmute(
              ds = as.Date(.index),
              yhat = .value,
              yhat_lower = if_else(.key == "prediction", .ci_lo, NA_real_),
              yhat_upper = if_else(.key == "prediction", .ci_hi, NA_real_),
              y = if_else(.key == "actual", .value, NA_real_),
              is_forecast = (.key == "prediction")
            )
        }) # end suppressWarnings
      }) # end suppressMessages

      list(
        data = result,
        model_table = ensemble_tbl,
        weights = weights,
        horizon = horizon_months
      )
    },
    error = function(e) {
      message(glue("Ensemble forecast failed: {conditionMessage(e)}"))
      NULL
    }
  )
}


# ── Public API (mirrors original interface) ───────────────────────────────────

#' Drop-in replacement for the original run_all_forecasts()
run_all_forecasts <- function(fred_data, horizon_months = 18) {
  out <- list()

  for (sid in names(FORECAST_SERIES)) {
    raw_df <- fred_data[[sid]]
    if (is.null(raw_df) || nrow(raw_df) < 24) {
      next
    }

    # CPI: convert to YoY % before forecasting
    if (sid == "CPIAUCSL") {
      raw_df <- raw_df %>%
        arrange(date) %>%
        mutate(value = (value / lag(value, 12) - 1) * 100) %>%
        filter(!is.na(value))
      FORECAST_SERIES[["CPIAUCSL"]]$unit <<- "YoY %"
    }

    message(glue("Ensemble forecasting {sid}..."))
    out[[sid]] <- run_ensemble(raw_df, horizon_months = horizon_months)
  }

  out
}

# ── Plotting (unchanged contract) ────────────────────────────────────────────

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

  # Build weight label for subtitle
  w <- fc_result$weights
  w_lbl <- glue(
    "Prophet {round(w['prophet']*100)}% · ",
    "ARIMA {round(w['arima']*100)}% · ",
    "ETS {round(w['ets']*100)}%"
  )

  plot_ly() %>%
    # Confidence ribbon (forecast only)
    add_ribbons(
      data = fore,
      x = ~ds,
      ymin = ~yhat_lower,
      ymax = ~yhat_upper,
      fillcolor = paste0(col, "30"),
      line = list(color = "transparent"),
      name = glue("{round(100*0.90)}% CI"),
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
    # Ensemble forecast line
    add_lines(
      data = fore,
      x = ~ds,
      y = ~yhat,
      line = list(color = col, width = 2.5, dash = "dash"),
      name = "Ensemble Forecast"
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
          " — 18-Month Ensemble Forecast<br>",
          "<sup>",
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

    # Ensemble weights summary
    w <- fc$weights
    w_str <- glue(
      "P{round(w['prophet']*100)}/A{round(w['arima']*100)}/E{round(w['ets']*100)}"
    )

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
