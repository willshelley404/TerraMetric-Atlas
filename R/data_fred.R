# ─────────────────────────────────────────────────────────────────────────────
# R/data_fred.R — FRED API data fetching
# ─────────────────────────────────────────────────────────────────────────────

#' Fetch a single FRED series with error handling
fetch_fred <- function(
  series_id,
  start = Sys.Date() - years(7),
  end = Sys.Date(),
  freq = NULL
) {
  tryCatch(
    {
      # fredr requires Date objects, not character strings
      args <- list(
        series_id = series_id,
        observation_start = as.Date(start),
        observation_end = as.Date(end)
      )
      if (!is.null(freq)) {
        args$frequency <- freq
      }

      result <- do.call(fredr, args)

      result %>%
        select(date, value) %>%
        filter(!is.na(value)) %>%
        arrange(date) %>%
        mutate(series_id = series_id)
    },
    error = function(e) {
      message(glue("FRED fetch failed [{series_id}]: {conditionMessage(e)}"))
      NULL
    }
  )
}

#' Fetch all configured FRED series
#' Daily series (DGS*, T10Y2Y, VIXCLS etc.) get extended history automatically
fetch_all_fred <- function(lookback_years = 7, progress_cb = NULL) {
  # Daily series benefit from much longer history for context
  LONG_HISTORY_IDS <- c(
    "DGS10",
    "DGS2",
    "DGS1MO",
    "DGS3MO",
    "DGS6MO",
    "DGS1",
    "DGS3",
    "DGS5",
    "DGS7",
    "DGS20",
    "DGS30",
    "T10Y2Y",
    "FEDFUNDS",
    "MORTGAGE30US",
    "VIXCLS",
    "DCOILWTICO",
    "DTWEXBGS",
    "ICSA",
    "LNS11300060"
  ) # weekly claims + prime-age LFPR need history

  start_default <- as.Date(Sys.Date() - years(lookback_years))
  start_long <- as.Date(Sys.Date() - years(max(lookback_years, 15)))
  result <- list()

  for (i in seq_along(FRED_IDS)) {
    sid <- FRED_IDS[i]
    start <- if (sid %in% LONG_HISTORY_IDS) start_long else start_default
    if (!is.null(progress_cb)) {
      progress_cb(i, length(FRED_IDS), sid)
    }
    result[[sid]] <- fetch_fred(sid, start = start)
    Sys.sleep(0.08)
  }
  result
}

#' Latest value in a series
latest_val <- function(dlist, sid) {
  df <- dlist[[sid]]
  if (is.null(df) || nrow(df) == 0) {
    return(NA_real_)
  }
  df$value[nrow(df)]
}

#' YoY percent change (approx 12 monthly periods)
yoy_val <- function(dlist, sid, lag_months = 12) {
  df <- dlist[[sid]]
  if (is.null(df) || nrow(df) < (lag_months + 1)) {
    return(NA_real_)
  }
  df <- df %>% arrange(date)
  n <- nrow(df)
  # Find observation ~12 months ago
  target <- df$date[n] - months(lag_months)
  past_idx <- which.min(abs(as.numeric(df$date - target)))
  if (abs(as.numeric(df$date[past_idx] - target)) > 60) {
    return(NA_real_)
  }
  (df$value[n] / df$value[past_idx] - 1) * 100
}

#' Month-over-month change (absolute)
mom_val <- function(dlist, sid) {
  df <- dlist[[sid]]
  if (is.null(df) || nrow(df) < 2) {
    return(NA_real_)
  }
  n <- nrow(df)
  df$value[n] - df$value[n - 1]
}

#' Patch DCOILWTICO with real-time WTI futures from Yahoo Finance (CL=F)
#'
#' FRED's daily WTI series (DCOILWTICO) typically lags 3-5 days.  This function
#' fetches the front-month WTI futures contract from Yahoo Finance via tidyquant,
#' then splices any dates *after* the last FRED observation into the series so
#' the rest of the app sees a seamless, up-to-date tibble in the same format
#' (columns: date, value, series_id).
#'
#' Call this immediately after fetch_all_fred() in server.R:
#'   fred_data <- fetch_all_fred()
#'   fred_data <- patch_wti_realtime(fred_data)
#'
#' @param dlist  Named list returned by fetch_all_fred()
#' @param ticker Yahoo Finance ticker for WTI. Default "CL=F" (front-month futures).
#'               "BZ=F" would give Brent instead.
#' @param lookback_days How many calendar days of Yahoo data to pull (default 10,
#'               enough to bridge any FRED lag with minimal API overhead).
#' @return dlist with the DCOILWTICO entry extended/replaced to the latest trading day.
patch_wti_realtime <- function(dlist, ticker = "CL=F", lookback_days = 10) {
  yahoo_raw <- tryCatch(
    tq_get(
      ticker,
      from = Sys.Date() - days(lookback_days),
      to = Sys.Date(),
      get = "stock.prices"
    ),
    error = function(e) {
      message(glue(
        "WTI real-time patch failed [{ticker}]: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  if (is.null(yahoo_raw) || nrow(yahoo_raw) == 0) {
    message(
      "WTI patch: no Yahoo data returned — keeping FRED series unchanged."
    )
    return(dlist)
  }

  # Shape Yahoo data to match the FRED tibble format exactly
  yahoo_wti <- yahoo_raw %>%
    filter(!is.na(close)) %>%
    transmute(
      date = as.Date(date),
      value = close, # settle / last price in USD/bbl
      series_id = "DCOILWTICO"
    ) %>%
    arrange(date)

  fred_wti <- dlist[["DCOILWTICO"]]

  if (is.null(fred_wti) || nrow(fred_wti) == 0) {
    # No FRED data at all — use Yahoo entirely
    dlist[["DCOILWTICO"]] <- yahoo_wti
    message(glue(
      "WTI patch: FRED series missing; using Yahoo data ({nrow(yahoo_wti)} rows)."
    ))
    return(dlist)
  }

  last_fred_date <- max(fred_wti$date, na.rm = TRUE)

  # Keep only Yahoo rows that are strictly newer than the last FRED observation
  new_rows <- yahoo_wti %>% filter(date > last_fred_date)

  if (nrow(new_rows) == 0) {
    message(glue(
      "WTI patch: FRED already current through {last_fred_date}; no update needed."
    ))
    return(dlist)
  }

  dlist[["DCOILWTICO"]] <- bind_rows(fred_wti, new_rows) %>% arrange(date)
  message(glue(
    "WTI patch: appended {nrow(new_rows)} row(s) from Yahoo Finance ",
    "(FRED through {last_fred_date}, now through {max(new_rows$date)})."
  ))

  dlist
}

#' Build dashboard KPI summary from fetched data
build_kpis <- function(dlist) {
  list(
    # Labor
    unemp_rate = latest_val(dlist, "UNRATE"),
    payrolls_chg = mom_val(dlist, "PAYEMS"),
    job_openings = latest_val(dlist, "JTSJOL"),
    avg_hrly_earn = latest_val(dlist, "CES0500000003"),
    wage_yoy = yoy_val(dlist, "CES0500000003"),
    # Inflation
    cpi_yoy = yoy_val(dlist, "CPIAUCSL"),
    core_cpi_yoy = yoy_val(dlist, "CPILFESL"),
    core_pce = yoy_val(dlist, "PCEPILFE"),
    ppi_yoy = yoy_val(dlist, "PPIACO"),
    # Monetary / Rates
    fed_funds = latest_val(dlist, "FEDFUNDS"),
    t10y2y = latest_val(dlist, "T10Y2Y"),
    t10yr = latest_val(dlist, "DGS10"),
    t2yr = latest_val(dlist, "DGS2"),
    mortgage30 = latest_val(dlist, "MORTGAGE30US"),
    # Housing
    housing_starts = latest_val(dlist, "HOUST"),
    permits = latest_val(dlist, "PERMIT"),
    # Consumer
    retail_yoy = yoy_val(dlist, "RSAFS"),
    cons_sent = latest_val(dlist, "UMCSENT"),
    indpro_yoy = yoy_val(dlist, "INDPRO"),
    # Macro
    gdp_yoy = yoy_val(dlist, "GDPC1", lag_months = 4), # quarterly
    # Markets — gold comes from GLD ETF via tidyquant (FRED gold series deprecated)
    oil_price = latest_val(dlist, "DCOILWTICO"),
    oil_yoy = yoy_val(dlist, "DCOILWTICO"), # oil price shock signal
    gold_price = NA_real_, # populated from market data in server.R after mkt fetch
    hy_spread = latest_val(dlist, "BAMLH0A0HYM2"),
    vix = latest_val(dlist, "VIXCLS"),
    usd_idx = latest_val(dlist, "DTWEXBGS"),
    usd_yoy = yoy_val(dlist, "DTWEXBGS"), # dollar surge signal
    # Enhanced recession model inputs
    prime_age_lfpr = latest_val(dlist, "LNS11300060"), # prime-age LFPR 25-54
    prime_age_lfpr_chg = {
      # 6-month change in prime-age LFPR
      df <- dlist[["LNS11300060"]]
      if (!is.null(df) && nrow(df) >= 7) {
        df <- df %>% arrange(date)
        n <- nrow(df)
        df$value[n] - df$value[n - 6]
      } else {
        NA_real_
      }
    },
    jobless_claims_4wk = {
      # 4-week MA of initial claims
      df <- dlist[["ICSA"]]
      if (!is.null(df) && nrow(df) >= 4) {
        mean(tail(df$value, 4), na.rm = TRUE) / 1000 # convert to thousands
      } else {
        NA_real_
      }
    },
    jobless_claims_trend = {
      # claims rising? (4wk vs 26wk MA)
      df <- dlist[["ICSA"]]
      if (!is.null(df) && nrow(df) >= 26) {
        ma4 <- mean(tail(df$value, 4), na.rm = TRUE)
        ma26 <- mean(tail(df$value, 26), na.rm = TRUE)
        ma4 / ma26 - 1 # positive = claims trending up = bad
      } else {
        NA_real_
      }
    },
    inventory_sales_ratio = latest_val(dlist, "ISRATIO"),
    cc_delinquency = latest_val(dlist, "DRCCLACBS"),
    quits_rate = latest_val(dlist, "JTSQUL"),
    quits_yoy = yoy_val(dlist, "JTSQUL") # quits falling YoY = workers scared
  )
}

#' Compute correlation matrix between selected FRED series
fred_correlation_matrix <- function(dlist, sids = NULL) {
  if (is.null(sids)) {
    sids <- c(
      "UNRATE",
      "CPIAUCSL",
      "FEDFUNDS",
      "DGS10",
      "DCOILWTICO",
      "VIXCLS",
      "RSAFS"
    )
  }
  sids <- sids[sids %in% names(dlist)]

  wide <- map(sids, function(sid) {
    df <- dlist[[sid]]
    if (is.null(df)) {
      return(NULL)
    }
    df %>% select(date, value) %>% rename(!!sid := value)
  }) %>%
    reduce(function(a, b) full_join(a, b, by = "date")) %>%
    arrange(date) %>%
    mutate(across(-date, ~ zoo::na.approx(., na.rm = FALSE))) %>%
    select(-date)

  # Monthly sample
  cor(wide, use = "pairwise.complete.obs")
}
