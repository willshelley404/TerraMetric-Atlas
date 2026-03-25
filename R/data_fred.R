# ─────────────────────────────────────────────────────────────────────────────
# R/data_fred.R — FRED API data fetching (with LBMA gold fallback)
# ─────────────────────────────────────────────────────────────────────────────

#' Fetch gold price from LBMA via DBnomics (free, no API key)
fetch_gold_lbma <- function(start = Sys.Date() - years(7), end = Sys.Date()) {
  tryCatch(
    {
      url <- paste0(
        "https://api.db.nomics.world/v22/series/LBMA/gold_D_USD_PM?observations=1"
      )

      json <- jsonlite::fromJSON(url)

      df <- tibble::tibble(
        date = as.Date(json$series$docs[[1]]$period),
        value = as.numeric(json$series$docs[[1]]$value)
      ) %>%
        dplyr::filter(date >= as.Date(start), date <= as.Date(end)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(series_id = "GOLD_LBMA_PM")

      df
    },
    error = function(e) {
      message(glue("LBMA gold fetch failed: {conditionMessage(e)}"))
      NULL
    }
  )
}

#' Fetch a single FRED series with error handling
fetch_fred <- function(
  series_id,
  start = Sys.Date() - years(7),
  end = Sys.Date(),
  freq = NULL
) {
  # 🔁 GOLD fallback (FRED series deprecated)
  if (series_id %in% c("GOLDPMGBD228NLBM", "GOLDAMGBD228NLBM")) {
    message(glue("Using LBMA fallback for {series_id}"))
    return(fetch_gold_lbma(start, end))
  }

  tryCatch(
    {
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
        dplyr::select(date, value) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(series_id = series_id)
    },
    error = function(e) {
      message(glue("FRED fetch failed [{series_id}]: {conditionMessage(e)}"))
      NULL
    }
  )
}

#' Fetch all configured FRED series
fetch_all_fred <- function(lookback_years = 7, progress_cb = NULL) {
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
    "DTWEXBGS"
  )

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
  df <- df %>% dplyr::arrange(date)
  n <- nrow(df)

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
    gdp_yoy = yoy_val(dlist, "GDPC1", lag_months = 4),

    # Markets
    oil_price = latest_val(dlist, "DCOILWTICO"),
    gold_price = latest_val(dlist, "GOLD_LBMA_PM"), # ✅ updated
    hy_spread = latest_val(dlist, "BAMLH0A0HYM2"),
    vix = latest_val(dlist, "VIXCLS"),
    usd_idx = latest_val(dlist, "DTWEXBGS")
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

  wide <- purrr::map(sids, function(sid) {
    df <- dlist[[sid]]
    if (is.null(df)) {
      return(NULL)
    }
    df %>% dplyr::select(date, value) %>% dplyr::rename(!!sid := value)
  }) %>%
    purrr::reduce(function(a, b) dplyr::full_join(a, b, by = "date")) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(dplyr::across(-date, ~ zoo::na.approx(., na.rm = FALSE))) %>%
    dplyr::select(-date)

  cor(wide, use = "pairwise.complete.obs")
}
