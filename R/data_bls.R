# ─────────────────────────────────────────────────────────────────────────────
# R/data_bls.R — BLS Public Data API v2 (direct HTTP, no package dependency)
# ─────────────────────────────────────────────────────────────────────────────

BLS_API_URL <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

#' Fetch BLS series via v2 API
#' @param series_ids Character vector of BLS series IDs (max 50 per call)
#' @param start_year First year of data
#' @param end_year   Last year of data
fetch_bls <- function(series_ids,
                       start_year = as.integer(format(Sys.Date(), "%Y")) - 5,
                       end_year   = as.integer(format(Sys.Date(), "%Y"))) {

  api_key <- Sys.getenv("BLS_API_KEY")

  body <- list(
    seriesid     = as.list(series_ids),
    startyear    = as.character(start_year),
    endyear      = as.character(end_year),
    catalog      = FALSE,
    calculations = FALSE,
    annualaverage= FALSE
  )
  if (nchar(api_key) > 0) body$registrationkey <- api_key

  tryCatch({
    resp <- request(BLS_API_URL) %>%
      req_headers("Content-Type" = "application/json") %>%
      req_body_json(body) %>%
      req_timeout(45) %>%
      req_perform()

    raw <- resp %>% resp_body_json()

    if (raw$status != "REQUEST_SUCCEEDED") {
      msg <- paste(unlist(raw$message), collapse="; ")
      message(glue("BLS API returned status '{raw$status}': {msg}"))
      return(NULL)
    }

    # Parse each series
    all_rows <- map_dfr(raw$Results$series, function(s) {
      sid <- s$seriesID
      map_dfr(s$data, function(obs) {
        period <- obs$period
        year   <- as.integer(obs$year)

        # Convert period to date
        date <- tryCatch({
          if (grepl("^M\\d{2}$", period)) {
            month <- as.integer(sub("M", "", period))
            if (month == 13) return(NULL)   # annual average
            ymd(paste0(year, "-", sprintf("%02d", month), "-01"))
          } else if (grepl("^Q\\d$", period)) {
            qtr <- as.integer(sub("Q", "", period))
            ymd(paste0(year, "-", sprintf("%02d", (qtr-1)*3+1), "-01"))
          } else {
            return(NULL)
          }
        }, error = function(e) return(NULL))

        if (is.null(date)) return(NULL)

        tibble(
          series_id = sid,
          date      = date,
          value     = suppressWarnings(as.numeric(obs$value))
        )
      })
    })

    all_rows %>%
      filter(!is.na(value), !is.na(date)) %>%
      arrange(series_id, date)

  }, error = function(e) {
    message(glue("BLS fetch error: {conditionMessage(e)}"))
    NULL
  })
}

#' Fetch all configured BLS series and return as named list
fetch_all_bls <- function(lookback_years = 5) {
  start_year <- as.integer(format(Sys.Date(), "%Y")) - lookback_years
  end_year   <- as.integer(format(Sys.Date(), "%Y"))

  raw <- fetch_bls(BLS_IDS, start_year, end_year)
  if (is.null(raw)) return(list())

  result <- list()
  for (sid in BLS_IDS) {
    result[[sid]] <- raw %>%
      filter(series_id == sid) %>%
      select(date, value) %>%
      arrange(date)
  }
  result
}

#' Merge BLS and FRED series for a combined inflation view
build_inflation_panel <- function(bls_data, fred_data) {
  frames <- list()

  # BLS CPI-U All Items
  if (!is.null(bls_data[["CUSR0000SA0"]]) &&
      nrow(bls_data[["CUSR0000SA0"]]) > 12) {
    frames[["CPI-U (BLS)"]] <- bls_data[["CUSR0000SA0"]] %>%
      yoy_change() %>%
      select(date, yoy) %>%
      filter(!is.na(yoy)) %>%
      rename(value = yoy) %>%
      mutate(series = "CPI-U YoY (BLS)")
  }

  # FRED CPI
  if (!is.null(fred_data[["CPIAUCSL"]]) &&
      nrow(fred_data[["CPIAUCSL"]]) > 12) {
    frames[["Core CPI (FRED)"]] <- fred_data[["CPIAUCSL"]] %>%
      yoy_change() %>%
      select(date, yoy) %>%
      filter(!is.na(yoy)) %>%
      rename(value = yoy) %>%
      mutate(series = "CPI-U YoY (FRED)")
  }

  # Core CPI
  if (!is.null(fred_data[["CPILFESL"]]) &&
      nrow(fred_data[["CPILFESL"]]) > 12) {
    frames[["Core CPI"]] <- fred_data[["CPILFESL"]] %>%
      yoy_change() %>%
      select(date, yoy) %>%
      filter(!is.na(yoy)) %>%
      rename(value = yoy) %>%
      mutate(series = "Core CPI YoY")
  }

  # Core PCE
  if (!is.null(fred_data[["PCEPI"]]) &&
      nrow(fred_data[["PCEPI"]]) > 12) {
    frames[["Core PCE"]] <- fred_data[["PCEPI"]] %>%
      yoy_change() %>%
      select(date, yoy) %>%
      filter(!is.na(yoy)) %>%
      rename(value = yoy) %>%
      mutate(series = "PCE YoY")
  }

  bind_rows(frames)
}
