# ─────────────────────────────────────────────────────────────────────────────
# R/data_markets.R — Financial market data via tidyquant (Yahoo Finance)
# ─────────────────────────────────────────────────────────────────────────────

#' Fetch price history for a set of tickers
fetch_market_data <- function(tickers = ALL_TICKERS, lookback_days = 400) {
  from_date <- Sys.Date() - days(lookback_days)

  tryCatch({
    data <- tq_get(
      tickers,
      from = from_date,
      to   = Sys.Date(),
      get  = "stock.prices"
    ) %>%
      select(symbol, date, open, high, low, close, volume) %>%
      filter(!is.na(close)) %>%
      arrange(symbol, date)

    data
  }, error = function(e) {
    message(glue("Market data fetch failed: {conditionMessage(e)}"))
    NULL
  })
}

#' Compute rolling returns for each ticker
compute_returns <- function(mkt) {
  if (is.null(mkt)) return(NULL)
  mkt %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      ret_1d  = (close / lag(close, 1)   - 1) * 100,
      ret_5d  = (close / lag(close, 5)   - 1) * 100,
      ret_1m  = (close / lag(close, 21)  - 1) * 100,
      ret_3m  = (close / lag(close, 63)  - 1) * 100,
      ret_ytd = {
        ytd_open <- close[which.min(abs(date - ymd(paste0(year(max(date)), "-01-01"))))]
        (close / ytd_open - 1) * 100
      }
    ) %>%
    ungroup()
}

#' Performance summary table (latest snapshot)
market_summary_table <- function(mkt_returns, group = "all") {
  if (is.null(mkt_returns)) return(NULL)

  tickers <- switch(group,
    equity    = EQUITY_TICKERS,
    sector    = SECTOR_TICKERS,
    commodity = COMMODITY_TICKERS,
    bond      = BOND_TICKERS,
    ALL_TICKERS
  )

  mkt_returns %>%
    filter(symbol %in% tickers) %>%
    group_by(symbol) %>%
    filter(!is.na(close)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      name   = TICKER_LABELS[symbol] %||% symbol,
      price  = round(close, 2)
    ) %>%
    select(symbol, name, price, ret_1d, ret_5d, ret_1m, ret_3m, ret_ytd) %>%
    rename(
      Ticker = symbol, Name = name, Price = price,
      `1D %` = ret_1d, `5D %` = ret_5d,
      `1M %` = ret_1m, `3M %` = ret_3m,
      `YTD %`= ret_ytd
    ) %>%
    arrange(Name)
}

#' Sector heat-map data
sector_heatmap_data <- function(mkt_returns) {
  if (is.null(mkt_returns)) return(NULL)

  mkt_returns %>%
    filter(symbol %in% SECTOR_TICKERS) %>%
    group_by(symbol) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      name   = TICKER_LABELS[symbol] %||% symbol,
      colour = case_when(
        ret_1m >  2 ~ CLR$success,
        ret_1m >  0 ~ "#a8e6cf",
        ret_1m > -2 ~ "#ffb3b3",
        TRUE        ~ CLR$danger
      )
    )
}

#' Build an indexed price chart (rebased to 100)
indexed_price_chart <- function(mkt, tickers, base_date = NULL) {
  if (is.null(mkt)) return(NULL)

  df <- mkt %>%
    filter(symbol %in% tickers) %>%
    arrange(symbol, date)

  if (!is.null(base_date)) {
    df <- df %>% filter(date >= base_date)
  }

  df %>%
    group_by(symbol) %>%
    mutate(index = close / first(close) * 100) %>%
    ungroup()
}

#' Yield curve snapshot from FRED DGS series stored in fred_data
build_yield_curve <- function(fred_data) {
  tenors <- c("DGS1MO","DGS3MO","DGS6MO","DGS1","DGS2","DGS3","DGS5","DGS7","DGS10","DGS20","DGS30")
  labels <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

  vals <- map_dbl(tenors, function(t) {
    df <- fred_data[[t]]
    if (is.null(df) || nrow(df)==0) return(NA_real_)
    df$value[nrow(df)]
  })

  tibble(tenor = factor(labels, levels=labels), yield = vals) %>%
    filter(!is.na(yield))
}
