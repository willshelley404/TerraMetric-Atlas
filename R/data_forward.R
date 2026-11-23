# ─────────────────────────────────────────────────────────────────────────────
# R/data_forward.R — forward-looking context for the LLM: market-implied
# expectations, leading indicators, and the upcoming release / FOMC calendar
# ─────────────────────────────────────────────────────────────────────────────

# Small set fetched on its own (not in FRED_CFG) so it adds no charts and no
# full-history pull. Three years is enough for 3-month changes and YoY.
FORWARD_SERIES <- c(
  T5YIE        = "5-yr breakeven inflation",
  T5YIFR       = "5y5y forward inflation expectation",
  SAHMREALTIME = "Sahm rule (real-time)",
  NFCI         = "Chicago Fed financial conditions (NFCI)",
  DRTSCILM     = "Banks tightening C&I loan standards (SLOOS)",
  CCSA         = "Continuing jobless claims",
  CUSR0000SEHF01 = "Electricity CPI"
)

#' Named list of series data frames (date, value); failed pulls are NULL.
fetch_forward_indicators <- function(pull = function(sid) fetch_fred(sid, start = Sys.Date() - 3 * 365)) {
  out <- lapply(names(FORWARD_SERIES), function(sid) tryCatch(pull(sid), error = function(e) NULL))
  stats::setNames(out, names(FORWARD_SERIES))
}

# Latest value and its change over roughly `days` (closest observation, within 45 days).
.fwd_latest <- function(df) if (is.null(df) || !nrow(df)) NA_real_ else df$value[nrow(df)]
.fwd_change <- function(df, days) {
  if (is.null(df) || nrow(df) < 2) return(NA_real_)
  df <- df[order(df$date), ]
  target <- df$date[nrow(df)] - days
  i <- which.min(abs(as.numeric(df$date - target)))
  if (abs(as.numeric(df$date[i] - target)) > 45 || i == nrow(df)) return(NA_real_)
  df$value[nrow(df)] - df$value[i]
}
.fwd_asof <- function(df) if (is.null(df) || !nrow(df)) NA_character_ else format(max(df$date), "%Y-%m-%d")
# YoY % change of the latest observation; NA when the year-ago month is missing.
.fwd_yoy <- function(df) {
  if (is.null(df) || nrow(df) < 13) return(NA_real_)
  df <- df[order(df$date), ]
  past <- df$value[df$date == seq(df$date[nrow(df)], by = "-1 year", length.out = 2)[2]]
  if (length(past) != 1) NA_real_ else 100 * (df$value[nrow(df)] / past - 1)
}

#' Forward signals from the forward series plus readings already in `kpis` / `fred_model`.
build_forward_signals <- function(fwd, kpis = NULL, fred_model = NULL) {
  k <- function(n) { v <- kpis[[n]]; if (is.null(v) || length(v) != 1) NA_real_ else v }
  ccsa <- fwd$CCSA
  permits <- fred_model[["PERMIT"]]
  list(
    breakeven_5y        = .fwd_latest(fwd$T5YIE),
    breakeven_5y_chg_3m = .fwd_change(fwd$T5YIE, 91),
    breakeven_5y5y      = .fwd_latest(fwd$T5YIFR),
    breakeven_asof      = .fwd_asof(fwd$T5YIE),
    # 2y yield below the funds rate = market prices cuts over ~2 years (rough: ignores term premium)
    two_year_minus_ff   = if (is.na(k("t2yr")) || is.na(k("fed_funds"))) NA_real_ else k("t2yr") - k("fed_funds"),
    sahm                = .fwd_latest(fwd$SAHMREALTIME),
    sahm_chg_6m         = .fwd_change(fwd$SAHMREALTIME, 182),
    sahm_asof           = .fwd_asof(fwd$SAHMREALTIME),
    nfci                = .fwd_latest(fwd$NFCI),
    nfci_chg_3m         = .fwd_change(fwd$NFCI, 91),
    nfci_asof           = .fwd_asof(fwd$NFCI),
    sloos_ci            = .fwd_latest(fwd$DRTSCILM),
    sloos_ci_prev       = .fwd_latest(if (!is.null(fwd$DRTSCILM) && nrow(fwd$DRTSCILM) > 1)
                                        fwd$DRTSCILM[order(fwd$DRTSCILM$date), ][-nrow(fwd$DRTSCILM), ]),
    sloos_asof          = .fwd_asof(fwd$DRTSCILM),
    cont_claims_k       = .fwd_latest(ccsa) / 1000,
    cont_claims_chg_yoy_k = .fwd_change(ccsa, 365) / 1000,
    cont_claims_asof    = .fwd_asof(ccsa),
    init_claims_4wk_k   = k("jobless_claims_4wk"),
    init_claims_trend   = k("jobless_claims_trend"),
    permits             = k("permits"),
    permits_chg_6m      = .fwd_change(permits, 182),
    elec_cpi_yoy        = .fwd_yoy(fwd$CUSR0000SEHF01),
    elec_cpi_asof       = .fwd_asof(fwd$CUSR0000SEHF01)
  )
}

.na_str <- function(x) if (is.null(x) || is.na(x)) "N/A" else x
.fmt_or_na <- function(x, fmt) if (is.null(x) || is.na(x)) "N/A" else sprintf(fmt, x)

#' Text block for prompts. Readings that are missing print as N/A (never invented).
forward_signals_to_text <- function(s) {
  if (is.null(s)) return("=== FORWARD-LOOKING SIGNALS ===\nUnavailable this session.\n")
  cuts <- if (is.na(s$two_year_minus_ff)) "N/A" else {
    n <- -s$two_year_minus_ff / 0.25
    sprintf("%+.2fpp (%s)", s$two_year_minus_ff,
            if (abs(n) < 0.5) "roughly no net change priced"
            else sprintf("≈%.1f 25bp %s priced over ~2 yrs", abs(n), if (n > 0) "cuts" else "hikes"))
  }
  paste0(
    "=== FORWARD-LOOKING SIGNALS (what markets and leading indicators expect) ===\n",
    "MARKET-IMPLIED\n",
    "  5-yr breakeven inflation:   ", .fmt_or_na(s$breakeven_5y, "%.2f%%"),
    " (3M chg ", .fmt_or_na(s$breakeven_5y_chg_3m, "%+.2fpp"), "; as of ", .na_str(s$breakeven_asof), ")\n",
    "  5y5y forward inflation:     ", .fmt_or_na(s$breakeven_5y5y, "%.2f%%"),
    " (long-run anchor; Fed target 2%)\n",
    "  2Y Treasury minus Fed Funds:", " ", cuts, "\n",
    "LEADING INDICATORS\n",
    "  Sahm rule (real-time):      ", .fmt_or_na(s$sahm, "%.2f"),
    " (6M chg ", .fmt_or_na(s$sahm_chg_6m, "%+.2f"), "; recession signal at >= 0.50; as of ", .na_str(s$sahm_asof), ")\n",
    "  Initial claims 4-wk avg:    ", .fmt_or_na(s$init_claims_4wk_k, "%.0fK"),
    " (4wk vs 26wk avg ", .fmt_or_na(100 * s$init_claims_trend, "%+.1f%%"), ")\n",
    "  Continuing claims:          ", .fmt_or_na(s$cont_claims_k, "%.0fK"),
    " (YoY chg ", .fmt_or_na(s$cont_claims_chg_yoy_k, "%+.0fK"), "; as of ", .na_str(s$cont_claims_asof), ")\n",
    "  Building permits:           ", .fmt_or_na(s$permits, "%.0fK ann."),
    " (6M chg ", .fmt_or_na(s$permits_chg_6m, "%+.0fK"), ")\n",
    "  Financial conditions NFCI:  ", .fmt_or_na(s$nfci, "%+.2f"),
    " (3M chg ", .fmt_or_na(s$nfci_chg_3m, "%+.2f"), "; <0 = looser than average; as of ", .na_str(s$nfci_asof), ")\n",
    "  Banks tightening C&I loans: ", .fmt_or_na(s$sloos_ci, "%+.1f%% net"),
    " (prior qtr ", .fmt_or_na(s$sloos_ci_prev, "%+.1f%%"), "; quarterly survey, as of ", .na_str(s$sloos_asof), ")\n",
    "  Electricity CPI YoY:        ", .fmt_or_na(s$elec_cpi_yoy, "%+.1f%%"),
    " (power-price pressure, incl. data-center demand; as of ", .na_str(s$elec_cpi_asof), ")\n"
  )
}

# ── Calendar ──────────────────────────────────────────────────────────────────
# FOMC meetings come from the Fed's own calendar page, which lists the next year
# as soon as it is published. Cached for a week; a failed refresh serves the
# last good copy, so there is no hardcoded list to maintain.
FOMC_CALENDAR_URL <- "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"

#' Decision days (second day of each meeting) from the FOMC calendar page.
#' Returns data.frame(date, sep): sep = a Summary of Economic Projections ("*").
#' Notation votes and unscheduled meetings are skipped.
parse_fomc_calendar <- function(html) {
  months <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
              jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
  pos <- gregexpr("\\d{4} FOMC Meetings", html)[[1]]
  if (pos[1] == -1) return(data.frame(date = as.Date(character()), sep = logical()))
  chunks <- substring(html, pos, c(pos[-1] - 1L, nchar(html)))
  rows <- lapply(chunks, function(ch) {
    yr <- suppressWarnings(as.integer(sub("^(\\d{4}) FOMC Meetings.*", "\\1", substr(ch, 1, 30))))
    if (is.na(yr)) return(NULL)
    pat <- "(?s)fomc-meeting__month[^>]*><strong>([^<]+)</strong>.*?fomc-meeting__date[^>]*>([^<]+)<"
    m <- regmatches(ch, gregexpr(pat, ch, perl = TRUE))[[1]]
    if (!length(m)) return(NULL)
    mon_txt <- tolower(sub(pat, "\\1", m, perl = TRUE))
    day_txt <- trimws(sub(pat, "\\2", m, perl = TRUE))
    keep <- !grepl("notation|unscheduled|cancel", day_txt, ignore.case = TRUE)
    mon_txt <- mon_txt[keep]; day_txt <- day_txt[keep]
    if (!length(day_txt)) return(NULL)
    days <- lapply(regmatches(day_txt, gregexpr("\\d+", day_txt)), as.integer)
    d1 <- vapply(days, `[`, 1L, 1L)
    d2 <- vapply(days, function(x) x[length(x)], 1L)
    mon <- unname(months[substr(mon_txt, 1, 3)])
    # "Apr/May 30-1" or "October 31-1": the decision day falls in the next month
    roll <- !is.na(d1) & !is.na(d2) & d2 < d1
    mon <- ifelse(roll, mon %% 12 + 1, mon)
    y <- ifelse(roll & mon == 1, yr + 1L, yr)
    data.frame(date = as.Date(sprintf("%04d-%02d-%02d", y, mon, d2)),
               sep = grepl("*", day_txt, fixed = TRUE))
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, TRUE)])
  if (is.null(out)) return(data.frame(date = as.Date(character()), sep = logical()))
  out <- out[!is.na(out$date) & !duplicated(out$date), ]
  out[order(out$date), , drop = FALSE]
}

.fomc_http <- function() {
  httr2::request(FOMC_CALENDAR_URL) |>
    httr2::req_user_agent("Mozilla/5.0 (TerraMetric Atlas)") |>
    httr2::req_timeout(20) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

fetch_fomc_meetings <- function(get_html = .fomc_http) {
  out <- parse_fomc_calendar(get_html())
  if (!nrow(out)) stop("no meetings parsed from the FOMC calendar page")
  out
}

#' Cached FOMC meetings (7-day TTL, stale copy on failure), or NULL.
fomc_meetings <- function(cache_dir = risk_cache_dir(), fetch = fetch_fomc_meetings) {
  risk_cached("fomc_meetings", ttl_days = 7, fetch_fn = fetch, cache_dir = cache_dir,
              fail_ttl_days = 1 / 24, seed_dir = NULL)
}

#' Which quarter a GDP release reports: the latest quarter that ended 25+ days
#' before the release (advance ~4 weeks after quarter end, revisions after that).
gdp_release_quarter <- function(d) {
  yrs <- as.integer(format(d, "%Y")) + (-1:0)
  qe <- sort(as.Date(c(outer(yrs, c("-03-31", "-06-30", "-09-30", "-12-31"), paste0))))
  last <- max(qe[as.numeric(d - qe) >= 25])
  sprintf("Q%d %s", (as.integer(format(last, "%m")) - 1) %/% 3 + 1, format(last, "%Y"))
}

# FRED release ids for the market-moving releases.
KEY_RELEASES <- c(
  "50"  = "Jobs report (Employment Situation)",
  "10"  = "CPI",
  "54"  = "PCE inflation / personal spending",
  "53"  = "GDP",
  "46"  = "PPI",
  "9"   = "Retail sales",
  "192" = "JOLTS job openings",
  "27"  = "Housing starts & permits",
  "91"  = "UMich consumer sentiment",
  "180" = "Weekly jobless claims"
)

.fred_release_dates <- function(release_id, from, to, key = Sys.getenv("FRED_API_KEY")) {
  raw <- httr2::request("https://api.stlouisfed.org/fred/release/dates") |>
    httr2::req_url_query(release_id = release_id, api_key = key, file_type = "json",
                         realtime_start = format(from), realtime_end = format(to),
                         include_release_dates_with_no_data = "true", sort_order = "asc") |>
    httr2::req_timeout(15) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  rd <- raw$release_dates
  if (is.null(rd)) return(as.Date(character()))
  d <- as.Date(vapply(rd, function(x) if (is.null(x$date)) NA_character_ else x$date, ""))
  d[!is.na(d) & d >= from & d <= to]
}

#' Upcoming key releases (date, event) over the next `days_ahead` days, with FOMC
#' decisions and minutes (released three weeks after each decision). Weekly claims
#' are reduced to the next print. Failed lookups are skipped; if FOMC dates are
#' unavailable the result carries attr "fomc_missing" so the prompt can say so.
fetch_release_calendar <- function(days_ahead = 45, today = Sys.Date(), get_dates = .fred_release_dates,
                                   fomc = tryCatch(fomc_meetings(), error = function(e) NULL)) {
  to <- today + days_ahead
  rows <- lapply(names(KEY_RELEASES), function(id) {
    d <- tryCatch(get_dates(id, today, to), error = function(e) {
      message("[Calendar] release ", id, ": ", conditionMessage(e)); NULL })
    if (!length(d)) return(NULL)
    if (id == "180") d <- min(d)
    ev <- unname(KEY_RELEASES[id])
    if (id == "53") ev <- paste0(ev, " (", vapply(d, gdp_release_quarter, ""), " estimate)")
    data.frame(date = d, event = ev, stringsAsFactors = FALSE)
  })
  if (!is.null(fomc) && nrow(fomc)) {
    dec <- fomc[fomc$date >= today & fomc$date <= to, , drop = FALSE]
    if (nrow(dec)) rows[[length(rows) + 1]] <- data.frame(
      date = dec$date,
      event = ifelse(dec$sep, "FOMC rate decision + new dot plot", "FOMC rate decision"),
      stringsAsFactors = FALSE)
    mins <- fomc$date + 21
    keep <- mins >= today & mins <= to
    if (any(keep)) rows[[length(rows) + 1]] <- data.frame(
      date = mins[keep],
      event = paste0("FOMC minutes (", format(fomc$date[keep], "%b %d"), " meeting)"),
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows[!vapply(rows, is.null, TRUE)])
  if (is.null(out) || !nrow(out)) out <- data.frame(date = as.Date(character()), event = character())
  out <- out[order(out$date, out$event), , drop = FALSE]
  rownames(out) <- NULL
  if (is.null(fomc) || !nrow(fomc)) attr(out, "fomc_missing") <- TRUE
  out
}

calendar_to_text <- function(cal, today = Sys.Date()) {
  hdr <- sprintf("=== UPCOMING CATALYSTS (today is %s) ===\n", format(today, "%a %b %d, %Y"))
  if (is.null(cal) || !nrow(cal)) return(paste0(hdr, "Calendar unavailable this session.\n"))
  note <- if (isTRUE(attr(cal, "fomc_missing"))) "  (FOMC meeting dates unavailable this session)\n" else ""
  paste0(hdr, paste(sprintf("  %s (%+d days)  %s", format(cal$date, "%a %b %d"),
                             as.integer(cal$date - today), cal$event), collapse = "\n"), "\n", note)
}

#' Everything the forward-looking prompts need; each part degrades to NULL on failure.
load_forward_context <- function(kpis = NULL, fred_model = NULL) {
  fwd <- tryCatch(fetch_forward_indicators(), error = function(e) NULL)
  list(
    signals  = if (is.null(fwd)) NULL else tryCatch(build_forward_signals(fwd, kpis, fred_model),
                                                    error = function(e) NULL),
    calendar = tryCatch(fetch_release_calendar(), error = function(e) NULL),
    series   = fwd   # raw series, for the evidence charts
  )
}

forward_context_text <- function(fwd_ctx, today = Sys.Date()) {
  paste0(forward_signals_to_text(fwd_ctx$signals), "\n", calendar_to_text(fwd_ctx$calendar, today))
}

# ── Prediction log ────────────────────────────────────────────────────────────
# Each AI Insights run's "Calls to Track" section is kept so a later run can
# grade it. Stored next to the regional-risk cache; last 10 runs.
llm_calls_path <- function(dir = risk_cache_dir()) file.path(dir, "llm_calls_log.rds")

#' Body of the markdown section whose "## " heading starts with `heading`, or NULL.
extract_section <- function(text, heading) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  m <- regmatches(text, regexpr(paste0("(?s)##\\s*\\Q", heading, "\\E.*?(?=\n##\\s|$)"), text, perl = TRUE))
  if (!length(m)) return(NULL)
  body <- trimws(sub("^##[^\n]*\n?", "", m))
  if (nzchar(body)) body else NULL
}

extract_calls_section <- function(text) extract_section(text, "Calls to Track")

# Chart tag for the evidence charts: [chart: {...json...}]. Tolerates backticks
# around it and arrays inside it (e.g. "threshold": [3.8, 4.0]).
CHART_TAG_RE <- "\\s*`?\\[chart:\\s*(\\{[^{}]*\\})\\s*\\]`?"

#' Text with chart tags removed (what readers and later prompts should see).
strip_chart_tags <- function(text) {
  if (is.null(text)) return(text)
  gsub(CHART_TAG_RE, "", text, perl = TRUE)
}

record_llm_calls <- function(text, path = llm_calls_path(), now = Sys.time(), keep = 10) {
  calls <- extract_calls_section(strip_chart_tags(text))
  if (is.null(calls)) return(invisible(FALSE))
  log <- tryCatch(if (file.exists(path)) readRDS(path) else list(), error = function(e) list())
  log <- utils::tail(c(log, list(list(made_at = now, calls = calls))), keep)
  ok <- tryCatch({ saveRDS(log, path); TRUE }, error = function(e) FALSE)
  invisible(ok)
}

#' The newest logged calls at least `min_age_days` old (so a same-day regenerate
#' does not grade calls made minutes ago), or NULL.
previous_llm_calls <- function(path = llm_calls_path(), now = Sys.time(), min_age_days = 3) {
  log <- tryCatch(if (file.exists(path)) readRDS(path) else NULL, error = function(e) NULL)
  if (!length(log)) return(NULL)
  age <- vapply(log, function(e) as.numeric(difftime(now, e$made_at, units = "days")), 0)
  ok <- which(age >= min_age_days)
  if (!length(ok)) return(NULL)
  log[[max(ok)]]
}
