# ─────────────────────────────────────────────────────────────────────────────
# R/forecasting.R — Prophet-based time series forecasting
# ─────────────────────────────────────────────────────────────────────────────

FORECAST_SERIES <- list(
  UNRATE       = list(name="Unemployment Rate",        unit="%",      color="#00b4d8"),
  CPIAUCSL     = list(name="CPI Inflation (YoY %)",    unit="YoY %",  color="#e94560"),
  FEDFUNDS     = list(name="Fed Funds Rate",            unit="%",      color="#f4a261"),
  MORTGAGE30US = list(name="30-Yr Mortgage Rate",       unit="%",      color="#7c5cbf"),
  HOUST        = list(name="Housing Starts",            unit="K",      color="#2dce89"),
  PAYEMS       = list(name="Nonfarm Payrolls",          unit="K",      color="#00b4d8"),
  RSAFS        = list(name="Retail Sales",              unit="M$",     color="#f4a261"),
  DCOILWTICO   = list(name="WTI Crude Oil",             unit="$/bbl",  color="#e94560")
)

run_prophet <- function(series_df, horizon_months=18, changepoint_prior=0.05) {
  if (is.null(series_df) || nrow(series_df) < 24) return(NULL)
  df <- series_df %>% select(ds=date, y=value) %>% filter(!is.na(y)) %>% arrange(ds)
  tryCatch({
    suppressMessages({
      m <- prophet(df, yearly.seasonality="auto",
                   weekly.seasonality=FALSE, daily.seasonality=FALSE,
                   changepoint.prior.scale=changepoint_prior,
                   seasonality.mode="additive", interval.width=0.9)
      future   <- make_future_dataframe(m, periods=horizon_months, freq="month")
      forecast <- predict(m, future)
    })
    result <- forecast %>%
      select(ds, yhat, yhat_lower, yhat_upper) %>%
      left_join(df, by="ds") %>%
      mutate(ds=as.Date(ds), is_forecast=is.na(y))
    list(data=result, model=m, horizon=horizon_months)
  }, error=function(e) { message(glue("Prophet: {conditionMessage(e)}")); NULL })
}

run_all_forecasts <- function(fred_data, horizon_months=18) {
  out <- list()
  for (sid in names(FORECAST_SERIES)) {
    raw_df <- fred_data[[sid]]
    if (is.null(raw_df) || nrow(raw_df) < 24) next

    # CPI is a price index — forecast its YoY % instead of the raw level
    # This is how it is universally reported and more meaningful to forecast
    if (sid == "CPIAUCSL") {
      raw_df <- raw_df %>%
        arrange(date) %>%
        mutate(value = (value / lag(value, 12) - 1) * 100) %>%
        filter(!is.na(value))
      # Update label in FORECAST_SERIES (runtime patch)
      FORECAST_SERIES[["CPIAUCSL"]]$unit  <<- "YoY %"
    }

    message(glue("Forecasting {sid}..."))
    out[[sid]] <- run_prophet(raw_df, horizon_months=horizon_months)
  }
  out
}

plot_forecast_chart <- function(fc_result, series_id) {
  if (is.null(fc_result)) {
    return(plot_ly() %>%
             layout(title="Insufficient data",
                    paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
                    font=list(color="#cccccc")) %>% config(displayModeBar=FALSE))
  }
  cfg  <- FORECAST_SERIES[[series_id]]
  df   <- fc_result$data
  hist <- df %>% filter(!is_forecast)
  fore <- df %>% filter(is_forecast)
  col  <- cfg$color
  cutoff <- max(hist$ds)

  plot_ly() %>%
    add_ribbons(data=fore, x=~ds, ymin=~yhat_lower, ymax=~yhat_upper,
                fillcolor=paste0(col,"30"), line=list(color="transparent"),
                name="90% CI", showlegend=TRUE, hoverinfo="none") %>%
    add_lines(data=hist, x=~ds, y=~y, line=list(color=col, width=2.5), name="Actual") %>%
    add_lines(data=hist, x=~ds, y=~yhat,
              line=list(color=paste0(col,"99"), width=1.5, dash="dot"),
              name="Prophet Fit") %>%
    add_lines(data=fore, x=~ds, y=~yhat,
              line=list(color=col, width=2.5, dash="dash"), name="Forecast") %>%
    add_segments(x=cutoff, xend=cutoff,
                 y=min(c(df$yhat_lower, df$y), na.rm=TRUE),
                 yend=max(c(df$yhat_upper, df$y), na.rm=TRUE),
                 line=list(color="#ffffff44", width=1, dash="dash"),
                 name="Forecast Start", showlegend=FALSE, hoverinfo="none") %>%
    layout(
      title        = list(text=paste0(cfg$name, " — 18-Month Forecast"),
                          font=list(color="#e0e0e0", size=14)),
      paper_bgcolor= "rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font         = list(color="#cccccc"),
      xaxis        = list(title="", gridcolor="#2a3042", color="#9aa3b2"),
      yaxis        = list(title=cfg$unit, gridcolor="#2a3042", color="#9aa3b2"),
      legend       = list(font=list(color="#cccccc"), bgcolor="rgba(0,0,0,0)",
                          orientation="h", y=-0.2),
      margin       = list(t=50, r=20, b=80, l=65),
      hovermode    = "x unified"
    ) %>% config(displayModeBar=FALSE)
}

forecast_summary_table <- function(forecasts) {
  map_dfr(names(forecasts), function(sid) {
    fc  <- forecasts[[sid]]
    if (is.null(fc)) return(NULL)
    cfg <- FORECAST_SERIES[[sid]]
    df  <- fc$data
    cur <- df %>% filter(!is_forecast) %>% slice_tail(n=1)
    f6  <- df %>% filter(is_forecast) %>% slice(min(6, n()))
    f18 <- df %>% slice_tail(n=1)
    tibble(
      Indicator    = cfg$name,
      Current      = round(cur$y, 2),
      `6M Fcst`    = round(f6$yhat, 2),
      `6M CI`      = paste0("[", round(f6$yhat_lower,2), ", ", round(f6$yhat_upper,2), "]"),
      `18M Fcst`   = round(f18$yhat, 2),
      Unit         = cfg$unit
    )
  })
}
