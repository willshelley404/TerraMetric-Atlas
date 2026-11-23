# ─────────────────────────────────────────────────────────────────────────────
# R/ai_charts.R — evidence charts for the AI's "Calls to Track"
# The LLM only picks a chart (series ids from CHART_SERIES + a threshold); every
# plotted value comes from FRED data already loaded in the app. Rendered as
# plotly in the app and as inline SVG in the exported HTML report.
# ─────────────────────────────────────────────────────────────────────────────

# id = list(label, unit, src = FRED id, tf = transform, from = "fred" | "fwd", aliases)
# Aliases are matched (longest first) against a call's text when its tag is missing.
CHART_SERIES <- list(
  cpi_yoy        = list("CPI inflation, YoY", "%", "CPIAUCNS", "yoy", "fred", c("headline cpi", "cpi yoy", "cpi")),
  core_cpi_yoy   = list("Core CPI, YoY", "%", "CPILFENS", "yoy", "fred", c("core cpi")),
  core_pce_yoy   = list("Core PCE inflation, YoY", "%", "PCEPILFE", "yoy", "fred", c("core pce", "pce")),
  ppi_yoy        = list("PPI final demand, YoY", "%", "PPIFID", "yoy", "fred", c("ppi")),
  unemp_rate     = list("Unemployment rate", "%", "UNRATE", "level", "fred", c("unemployment rate", "unemployment", "unrate")),
  payrolls_chg   = list("Nonfarm payrolls, monthly change", "K jobs", "PAYEMS", "diff", "fred", c("nonfarm payrolls", "payrolls", "jobs report")),
  job_openings   = list("Job openings (JOLTS)", "K", "JTSJOL", "level", "fred", c("job openings", "jolts")),
  wage_yoy       = list("Avg hourly earnings, YoY", "%", "CES0500000003", "yoy", "fred", c("hourly earnings", "wage growth", "wages")),
  init_claims    = list("Initial jobless claims", "K", "ICSA", "k", "fred", c("initial claims", "jobless claims", "claims")),
  cont_claims    = list("Continuing jobless claims", "K", "CCSA", "k", "fwd", c("continuing claims")),
  sahm           = list("Sahm rule (real-time)", "pp", "SAHMREALTIME", "level", "fwd", c("sahm")),
  fed_funds      = list("Fed funds rate", "%", "FEDFUNDS", "level", "fred", c("fed funds", "policy rate")),
  t2yr           = list("2-year Treasury yield", "%", "DGS2", "level", "fred", c("2-year treasury", "2-yr treasury", "2-year yield")),
  t10yr          = list("10-year Treasury yield", "%", "DGS10", "level", "fred", c("10-year treasury", "10-yr treasury", "10-year yield")),
  t10y2y         = list("10Y-2Y Treasury spread", "pp", "T10Y2Y", "level", "fred",
                        c("10y-2y treasury spread", "10y-2y spread", "10y-2y", "10y 2y", "2s10s", "yield curve", "treasury spread")),
  mortgage30     = list("30-year mortgage rate", "%", "MORTGAGE30US", "level", "fred", c("mortgage rate", "mortgage")),
  breakeven_5y   = list("5-year breakeven inflation", "%", "T5YIE", "level", "fwd", c("5-yr breakeven", "5-year breakeven", "breakeven")),
  breakeven_5y5y = list("5y5y forward inflation", "%", "T5YIFR", "level", "fwd", c("5y5y")),
  housing_starts = list("Housing starts", "K ann.", "HOUST", "level", "fred", c("housing starts", "starts")),
  permits        = list("Building permits", "K ann.", "PERMIT", "level", "fred", c("building permits", "permits")),
  retail_yoy     = list("Retail sales, YoY", "%", "RSAFS", "yoy", "fred", c("retail sales")),
  cons_sent      = list("Consumer sentiment (UMich)", "index", "UMCSENT", "level", "fred", c("consumer sentiment", "sentiment")),
  indpro_yoy     = list("Industrial production, YoY", "%", "INDPRO", "yoy", "fred", c("industrial production")),
  gdp_saar       = list("Real GDP growth, annualized", "%", "GDPC1", "saar", "fred", c("gdp")),
  oil            = list("WTI crude oil", "$/bbl", "DCOILWTICO", "level", "fred", c("wti", "crude", "oil")),
  hy_spread      = list("High-yield credit spread", "pp", "BAMLH0A0HYM2", "level", "fred", c("high-yield", "hy spread", "credit spread")),
  vix            = list("VIX", "index", "VIXCLS", "level", "fred", c("vix")),
  nfci           = list("Chicago Fed financial conditions", "index", "NFCI", "level", "fwd", c("nfci", "financial conditions")),
  sloos_ci       = list("Banks tightening C&I standards", "% net", "DRTSCILM", "level", "fwd", c("c&i", "lending standards", "sloos")),
  elec_cpi_yoy   = list("Electricity CPI, YoY", "%", "CUSR0000SEHF01", "yoy", "fwd", c("electricity price", "electricity cpi", "power price", "utility bills")),
  # Market prices, indexed to 100 at the start of the chart window
  spy            = list("S&P 500 (SPY), indexed", "start = 100", "SPY", "index", "mkt", c("s&p 500", "spy")),
  qqq            = list("Nasdaq 100 (QQQ), indexed", "start = 100", "QQQ", "index", "mkt", c("nasdaq", "qqq")),
  iwm            = list("Small caps (IWM), indexed", "start = 100", "IWM", "index", "mkt", c("small caps", "russell 2000", "iwm")),
  smh            = list("Semiconductors (SMH), indexed", "start = 100", "SMH", "index", "mkt", c("semiconductor stocks", "chip stocks", "smh")),
  xlk            = list("Tech sector (XLK), indexed", "start = 100", "XLK", "index", "mkt", c("tech stocks", "tech sector", "xlk")),
  xlu            = list("Utilities (XLU), indexed", "start = 100", "XLU", "index", "mkt", c("utilities stocks", "utility stocks", "xlu")),
  xle            = list("Energy sector (XLE), indexed", "start = 100", "XLE", "index", "mkt", c("energy stocks", "xle")),
  xlf            = list("Financials (XLF), indexed", "start = 100", "XLF", "index", "mkt", c("bank stocks", "financials", "xlf")),
  xlre           = list("Real estate (XLRE), indexed", "start = 100", "XLRE", "index", "mkt", c("real estate stocks", "reits", "xlre"))
)
CHART_SERIES <- lapply(CHART_SERIES, function(x)
  stats::setNames(x, c("label", "unit", "src", "tf", "from", "aliases")))

#' Catalog for the prompt: one line per id.
chart_catalog_text <- function() {
  paste0("CHART SERIES (ids for chart tags; values are in these units):\n",
         paste(sprintf("  %-15s %s (%s)", names(CHART_SERIES),
                       vapply(CHART_SERIES, `[[`, "", "label"),
                       vapply(CHART_SERIES, `[[`, "", "unit")), collapse = "\n"))
}

# ── Data ──────────────────────────────────────────────────────────────────────
# Value at or before each date minus `days` (within `tol` days), else NA.
.lag_by_days <- function(dates, values, days, tol = 20) {
  idx <- findInterval(as.numeric(dates - days), as.numeric(dates))
  out <- rep(NA_real_, length(dates))
  ok <- idx > 0
  out[ok] <- values[idx[ok]]
  out[ok][as.numeric(dates[ok] - days - dates[idx[ok]]) > tol] <- NA_real_
  out
}

#' Transformed series for a catalog id over the last `years`: data.frame(date, value), or NULL.
#' `data` = list(fred = FRED list, fwd = forward series list, mkt = market prices
#' data frame with symbol/date/close).
chart_series_data <- function(id, data, years = 3, today = Sys.Date()) {
  cfg <- CHART_SERIES[[id]]
  if (is.null(cfg)) return(NULL)
  df <- if (identical(cfg$from, "mkt")) {
    m <- data$mkt
    if (is.null(m) || !all(c("symbol", "date", "close") %in% names(m))) return(NULL)
    m <- m[m$symbol == cfg$src, , drop = FALSE]
    data.frame(date = as.Date(m$date), value = m$close)
  } else (if (identical(cfg$from, "fwd")) data$fwd else data$fred)[[cfg$src]]
  if (is.null(df) || !nrow(df)) return(NULL)
  df <- df[order(df$date), c("date", "value")]
  df <- df[!is.na(df$value), ]
  v <- switch(cfg$tf,
    level = , index = df$value,
    k     = df$value / 1000,
    diff  = c(NA, diff(df$value)),
    yoy   = 100 * (df$value / .lag_by_days(df$date, df$value, 365) - 1),
    saar  = 100 * ((df$value / c(NA, utils::head(df$value, -1)))^4 - 1))
  out <- data.frame(date = df$date, value = v)
  out <- out[!is.na(out$value) & out$date >= today - round(365.25 * years), ]
  if (cfg$tf == "index" && nrow(out)) out$value <- 100 * out$value / out$value[1]
  if (nrow(out) < 3) NULL else out
}

# LLM text uses typographic hyphens (U+2011 etc.), narrow/no-break spaces before
# units, smart quotes and >=/<= signs; map them to ASCII before parsing.
.u <- function(...) intToUtf8(c(...), multiple = FALSE)
.NORM_DASH  <- paste0("[", .u(0x2010:0x2015, 0x2212), "]")
.NORM_SPACE <- paste0("[", .u(0x00A0, 0x2007, 0x2009, 0x202F), "]")
.norm_text <- function(x) {
  x <- gsub(.NORM_DASH, "-", x)
  x <- gsub(.NORM_SPACE, " ", x)
  x <- gsub(paste0("[", .u(0x201C, 0x201D), "]"), "\"", x)
  x <- gsub(paste0("[", .u(0x2018, 0x2019), "]"), "'", x)
  x <- gsub(.u(0x2265), ">=", x, fixed = TRUE)
  gsub(.u(0x2264), "<=", x, fixed = TRUE)
}

# ── Specs ─────────────────────────────────────────────────────────────────────
.parse_call_date <- function(text, today = Sys.Date()) {
  iso <- regmatches(text, regexpr("\\d{4}-\\d{2}-\\d{2}", text))
  if (length(iso)) return(as.Date(iso))
  m <- regmatches(text, regexpr("(?i)\\bby\\s+\\**\\s*([A-Z][a-z]{2,8})\\.?\\s+(\\d{1,2})(,?\\s+(\\d{4}))?", text, perl = TRUE))
  if (!length(m)) return(NA)
  parts <- regmatches(m, regexec("(?i)by\\s+\\**\\s*([A-Za-z]{3,9})\\.?\\s+(\\d{1,2})(,?\\s+(\\d{4}))?", m, perl = TRUE))[[1]]
  mon <- match(tolower(substr(parts[2], 1, 3)), tolower(month.abb))
  if (is.na(mon)) return(NA)
  yr <- if (nzchar(parts[5])) as.integer(parts[5]) else as.integer(format(today, "%Y"))
  d <- as.Date(sprintf("%04d-%02d-%02d", yr, mon, as.integer(parts[3])))
  if (!nzchar(parts[5]) && !is.na(d) && d < today - 30) d <- as.Date(sprintf("%04d-%02d-%02d", yr + 1L, mon, as.integer(parts[3])))
  d
}

.parse_call_thresholds <- function(text) {
  t <- gsub("\\*", "", .norm_text(text))
  num <- "([-+]?\\d[\\d,]*(?:\\.\\d+)?|[-+]?\\.\\d+)"
  unit <- "\\s*(?:%|pp|bp|K|M|k|m)?"
  pat <- paste0("(?i)(above|below|under|over|exceeds?|at least|at most|between|>=|<=|>|<)\\s*",
                num, unit, "(?:\\s*(?:and|-|to)\\s*", num, unit, ")?")
  m <- regmatches(t, regexec(pat, t, perl = TRUE))[[1]]
  if (!length(m)) return(numeric(0))
  v <- suppressWarnings(as.numeric(gsub(",", "", c(m[3], if (length(m) > 3 && nzchar(m[4])) m[4]))))
  v[is.finite(v)]
}

.match_series <- function(text) {
  t <- tolower(.norm_text(text))
  al <- unlist(lapply(names(CHART_SERIES), function(id)
    stats::setNames(CHART_SERIES[[id]]$aliases, rep(id, length(CHART_SERIES[[id]]$aliases)))))
  al <- al[order(-nchar(al))]
  hit <- which(vapply(al, function(a) grepl(a, t, fixed = TRUE), TRUE))
  if (length(hit)) names(al)[hit[1]] else NULL
}

.ITEM_RE <- "^([-*•]|\\d+[.)])\\s+"

#' First catalog id named verbatim in `text` (word-bounded, longest ids first), or NULL.
.id_mention <- function(text) {
  t <- tolower(text)
  ids <- names(CHART_SERIES)[order(-nchar(names(CHART_SERIES)))]
  hit <- ids[vapply(ids, function(id) grepl(paste0("(?<![a-z0-9_])", id, "(?![a-z0-9_])"), t, perl = TRUE), TRUE)]
  if (length(hit)) {
    pos <- vapply(hit, function(id) regexpr(paste0("(?<![a-z0-9_])", id, "(?![a-z0-9_])"), t, perl = TRUE), 1L)
    hit[which.min(pos)]
  } else NULL
}

#' Items of a section (bullets, numbered lines, or table rows) with their parsed
#' chart tag (NULL when absent). `call` is the item's plain text.
parse_section_charts <- function(text, heading) {
  sec <- extract_section(text, heading)
  if (is.null(sec)) return(list())
  lines <- trimws(strsplit(.norm_text(sec), "\n")[[1]])
  is_row <- grepl("^\\|", lines) & !grepl("^\\|[\\s|:-]+\\|?$", lines, perl = TRUE)
  lines <- lines[grepl(.ITEM_RE, lines) | is_row]
  if (length(lines) && grepl("^\\|", lines[1]) && !grepl(CHART_TAG_RE, lines[1], perl = TRUE))
    lines <- lines[-1]                                    # table header row
  lapply(lines, function(l) {
    tag <- regmatches(l, regexec(CHART_TAG_RE, l, perl = TRUE))[[1]]
    spec <- if (length(tag)) tryCatch(jsonlite::fromJSON(tag[2], simplifyVector = TRUE),
                                      error = function(e) NULL)
    plain <- strip_chart_tags(l)
    if (grepl("^\\|", plain)) {
      cells <- trimws(strsplit(gsub("^\\||\\|$", "", plain), "\\|")[[1]])
      plain <- paste(cells[nzchar(cells)], collapse = " — ")
    }
    call <- trimws(gsub("\\*\\*|__", "", sub(.ITEM_RE, "", plain)))
    list(call = call, spec = spec)
  })
}

parse_call_charts <- function(text) parse_section_charts(text, "Calls to Track")

#' A validated spec, falling back to what the call text names. NULL when nothing chartable.
resolve_chart_spec <- function(spec, call, today = Sys.Date(), fallback = TRUE) {
  ok_id <- function(x) is.character(x) && length(x) == 1 && x %in% names(CHART_SERIES)
  num <- function(x) { x <- suppressWarnings(as.numeric(unlist(x))); x[is.finite(x)] }
  years <- num(spec$years); years <- if (length(years)) min(max(years[1], 1), 10) else 3
  if (identical(spec$type, "scatter") && ok_id(spec$x) && ok_id(spec$y) && spec$x != spec$y)
    return(list(type = "scatter", x = spec$x, y = spec$y, years = max(years, 5), call = call))
  id <- if (ok_id(spec$series)) spec$series else if (fallback) .match_series(call)
  if (is.null(id)) return(NULL)
  th <- num(spec$threshold)
  if (!length(th) && fallback) th <- .parse_call_thresholds(call)
  by <- tryCatch(as.Date(.norm_text(as.character(spec$by))), error = function(e) NA)
  if ((length(by) != 1 || is.na(by)) && fallback) by <- .parse_call_date(.norm_text(call), today)
  if (length(by) != 1) by <- as.Date(NA)
  list(type = "line", series = id, threshold = utils::head(th, 2), by = by, years = years, call = call)
}

#' Charts for the items of section `heading` that resolve to data: list of
#' list(call, spec, data). With fallback = FALSE only tagged items get a chart
#' (theme prose names too many things to guess from).
build_section_charts <- function(text, heading, data, fallback = TRUE, today = Sys.Date()) {
  items <- parse_section_charts(text, heading)
  if (!fallback) {
    # No tag: accept a catalog id the item names verbatim (e.g. "the smh index"),
    # never a guess from looser wording
    items <- lapply(items, function(c) {
      if (is.null(c$spec)) { id <- .id_mention(c$call); if (!is.null(id)) c$spec <- list(series = id) }
      c
    })
    items <- Filter(function(c) !is.null(c$spec), items)
  }
  out <- lapply(items, function(c) {
    s <- tryCatch(resolve_chart_spec(c$spec, c$call, today, fallback), error = function(e) NULL)
    if (is.null(s)) return(NULL)
    if (s$type == "scatter") {
      x <- chart_series_data(s$x, data, s$years, today); y <- chart_series_data(s$y, data, s$years, today)
      if (is.null(x) || is.null(y)) return(NULL)
      xy <- .monthly_join(x, y)
      if (nrow(xy) < 6) return(NULL)
      return(list(call = c$call, spec = s, data = xy))
    }
    d <- chart_series_data(s$series, data, s$years, today)
    if (is.null(d)) return(NULL)
    # Drop thresholds on a different scale (e.g. "1.2M" against a series in K)
    # floor the span so a flat series still keeps a nearby threshold
    span <- max(diff(range(d$value)), 0.5 * max(abs(d$value)))
    lo <- min(d$value) - span; hi <- max(d$value) + span
    s$threshold <- s$threshold[s$threshold >= lo & s$threshold <= hi]
    list(call = c$call, spec = s, data = d)
  })
  out[!vapply(out, is.null, TRUE)]
}

build_call_charts <- function(text, data, today = Sys.Date())
  build_section_charts(text, "Calls to Track", data, fallback = TRUE, today = today)

build_theme_charts <- function(text, data, today = Sys.Date())
  build_section_charts(text, "Themes Gaining Momentum", data, fallback = FALSE, today = today)

#' Insights split at "## " headings: list of list(heading, md). Text before the
#' first heading gets heading "".
split_md_sections <- function(text) {
  if (is.null(text) || !nzchar(text)) return(list())
  lines <- strsplit(text, "\n")[[1]]
  start <- grepl("^##\\s", lines)
  grp <- cumsum(start)
  out <- lapply(split(lines, grp), function(l) {
    h <- if (grepl("^##\\s", l[1])) trimws(sub("^##\\s*", "", l[1])) else ""
    list(heading = h, md = paste(l, collapse = "\n"))
  })
  unname(out[vapply(out, function(s) nzchar(trimws(s$md)), TRUE)])
}

#' Which evidence block (if any) follows a section heading.
evidence_slot <- function(heading) {
  h <- tolower(.norm_text(heading))
  if (startsWith(h, "themes gaining momentum")) "themes"
  else if (startsWith(h, "calls to track")) "calls"
  else NA_character_
}

.monthly_join <- function(x, y) {
  m <- function(d) stats::aggregate(value ~ month, transform(d, month = as.Date(format(date, "%Y-%m-01"))), mean)
  xy <- merge(m(x), m(y), by = "month", suffixes = c("_x", "_y"))
  data.frame(date = xy$month, x = xy$value_x, y = xy$value_y)
}

.chart_title <- function(ch) {
  s <- ch$spec
  if (s$type == "scatter") sprintf("%s vs %s", CHART_SERIES[[s$y]]$label, CHART_SERIES[[s$x]]$label)
  else CHART_SERIES[[s$series]]$label
}

# ── Renderers ─────────────────────────────────────────────────────────────────
CHART_COL <- list(line = "#00b4d8", call = "#f4a261", date = "#a785e0", muted = "#9aa3b2")

#' Plotly chart for the app (dark theme).
ai_chart_plotly <- function(ch, height = 260) {
  s <- ch$spec; d <- ch$data
  if (s$type == "scatter") {
    recent <- d$date >= max(d$date) - 365
    last <- d[which.max(d$date), ]
    p <- plotly::plot_ly(height = height) |>
      plotly::add_markers(x = d$x[!recent], y = d$y[!recent], name = "earlier",
                          marker = list(color = "rgba(154,163,178,0.45)", size = 7),
                          text = format(d$date[!recent], "%b %Y"), hoverinfo = "text+x+y") |>
      plotly::add_markers(x = d$x[recent], y = d$y[recent], name = "last 12 months",
                          marker = list(color = CHART_COL$line, size = 8),
                          text = format(d$date[recent], "%b %Y"), hoverinfo = "text+x+y") |>
      plotly::add_markers(x = last$x, y = last$y, name = format(last$date, "latest (%b %Y)"),
                          marker = list(color = CHART_COL$call, size = 12, line = list(color = "#fff", width = 1)))
    return(dark_theme(p, title = .chart_title(ch), date_range = "none",
                      x = sprintf("%s (%s)", CHART_SERIES[[s$x]]$label, CHART_SERIES[[s$x]]$unit),
                      y = sprintf("%s (%s)", CHART_SERIES[[s$y]]$label, CHART_SERIES[[s$y]]$unit)) |>
             plotly::layout(hovermode = "closest", legend = list(orientation = "h", y = -0.3)))
  }
  last <- d[nrow(d), ]
  x_end <- max(c(last$date, s$by), na.rm = TRUE) + 15
  shapes <- lapply(s$threshold, function(t) list(type = "line", xref = "x", x0 = min(d$date), x1 = x_end,
                                                 y0 = t, y1 = t, line = list(color = CHART_COL$call, dash = "dash")))
  if (!is.na(s$by)) shapes[[length(shapes) + 1]] <- list(
    type = "line", xref = "x", yref = "paper", x0 = s$by, x1 = s$by, y0 = 0, y1 = 1,
    line = list(color = CHART_COL$date, dash = "dot"))
  ann <- list(list(x = last$date, y = last$value, text = sprintf("%.2f", last$value), showarrow = TRUE,
                   arrowcolor = CHART_COL$muted, ax = -30, ay = -25, font = list(color = "#e0e0e0", size = 11)))
  if (!is.na(s$by)) ann[[2]] <- list(x = s$by, y = 1, yref = "paper", text = format(s$by, "call by %b %d"),
                                     showarrow = FALSE, yanchor = "bottom", xanchor = "right",
                                     font = list(color = CHART_COL$date, size = 10))
  p <- plotly::plot_ly(d, x = ~date, y = ~value, type = "scatter", mode = "lines", height = height,
                       line = list(color = CHART_COL$line, width = 2), name = CHART_SERIES[[s$series]]$label,
                       hovertemplate = paste0("%{x|%b %d, %Y}: %{y:.2f} ", CHART_SERIES[[s$series]]$unit, "<extra></extra>"))
  dark_theme(p, title = .chart_title(ch), y = CHART_SERIES[[s$series]]$unit, date_range = "none") |>
    plotly::layout(shapes = shapes, annotations = ann, showlegend = FALSE,
                   xaxis = list(range = list(format(min(d$date)), format(x_end)), type = "date",
                                gridcolor = "#2a3042", color = "#9aa3b2"))
}

# Inline SVG for the exported report: themed with the report's CSS variables.
ai_chart_svg <- function(ch, width = 440, height = 230, label = "Call") {
  s <- ch$spec; d <- ch$data
  m <- list(l = 48, r = 14, t = 14, b = 30)
  pw <- width - m$l - m$r; ph <- height - m$t - m$b
  esc <- function(x) htmltools::htmlEscape(x)
  fmt <- function(v) formatC(v, format = "fg", digits = 3)
  ticks <- function(r) { if (diff(r) == 0) r <- r + c(-0.5, 0.5) * max(abs(r[1]) * 0.05, 0.1); pretty(r, n = 4) }
  # tick labels carry as many decimals as the tick step needs
  tfmt <- function(t) { st <- if (length(t) > 1) min(diff(t)) else 1
    formatC(t, format = "f", digits = max(0, min(4, -floor(log10(st))))) }
  scale <- function(v, r, px, flip = FALSE) { f <- (v - r[1]) / (r[2] - r[1]); if (flip) m$t + px * (1 - f) else m$l + px * f }
  ytick_svg <- function(yr, yt) paste(sprintf(
    '<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="var(--line)"/><text x="%.1f" y="%.1f" font-size="10" text-anchor="end" fill="var(--mut)">%s</text>',
    m$l, width - m$r, scale(yt, yr, ph, TRUE), scale(yt, yr, ph, TRUE), m$l - 6, scale(yt, yr, ph, TRUE) + 3, tfmt(yt)), collapse = "")

  if (s$type == "scatter") {
    xr <- range(ticks(range(d$x))); yr <- range(ticks(range(d$y)))
    recent <- d$date >= max(d$date) - 365; last <- which.max(d$date)
    xt <- ticks(range(d$x))
    body <- paste0(
      ytick_svg(yr, ticks(range(d$y))),
      paste(sprintf('<text x="%.1f" y="%.1f" font-size="10" text-anchor="middle" fill="var(--mut)">%s</text>',
                    scale(xt, xr, pw), height - m$b + 14, tfmt(xt)), collapse = ""),
      paste(sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s" fill-opacity="%s"/>',
                    scale(d$x, xr, pw), scale(d$y, yr, ph, TRUE), ifelse(recent, "3.5", "3"),
                    ifelse(recent, "var(--teal)", "var(--mut)"), ifelse(recent, "0.9", "0.35")), collapse = ""),
      sprintf('<circle cx="%.1f" cy="%.1f" r="6" fill="var(--acc)" stroke="var(--card)" stroke-width="1.5"/>',
              scale(d$x[last], xr, pw), scale(d$y[last], yr, ph, TRUE)),
      sprintf('<text x="%.1f" y="%d" font-size="10" text-anchor="middle" fill="var(--mut)">%s</text>',
              m$l + pw / 2, height - 2, esc(sprintf("%s (%s)", CHART_SERIES[[s$x]]$label, CHART_SERIES[[s$x]]$unit))))
    legend <- sprintf("Teal = last 12 months, orange = latest (%s). Y: %s (%s).",
                      format(d$date[last], "%b %Y"), CHART_SERIES[[s$y]]$label, CHART_SERIES[[s$y]]$unit)
  } else {
    x_end <- max(c(max(d$date), s$by), na.rm = TRUE) + 10
    xr <- as.numeric(c(min(d$date), x_end))
    yr <- range(ticks(range(c(d$value, s$threshold))))
    xs <- scale(as.numeric(d$date), xr, pw); ys <- scale(d$value, yr, ph, TRUE)
    yrs <- seq(as.integer(format(min(d$date), "%Y")) + 1L, as.integer(format(x_end, "%Y")))
    xt <- as.Date(paste0(yrs, "-01-01")); xt <- xt[as.numeric(xt) >= xr[1] & as.numeric(xt) <= xr[2]]
    last <- nrow(d)
    body <- paste0(
      ytick_svg(yr, ticks(range(c(d$value, s$threshold)))),
      paste(sprintf('<text x="%.1f" y="%.1f" font-size="10" text-anchor="middle" fill="var(--mut)">%s</text>',
                    scale(as.numeric(xt), xr, pw), height - m$b + 14, format(xt, "%Y")), collapse = ""),
      sprintf('<polyline points="%s" fill="none" stroke="var(--teal)" stroke-width="2"/>',
              paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")),
      paste(sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="var(--acc)" stroke-dasharray="5 4" stroke-width="1.5"/>',
                    m$l, width - m$r, scale(s$threshold, yr, ph, TRUE), scale(s$threshold, yr, ph, TRUE)), collapse = ""),
      if (!is.na(s$by)) sprintf(
        '<line x1="%.1f" x2="%.1f" y1="%d" y2="%.1f" stroke="var(--mut)" stroke-dasharray="2 3"/><text x="%.1f" y="%d" font-size="10" text-anchor="end" fill="var(--mut)">%s</text>',
        scale(as.numeric(s$by), xr, pw), scale(as.numeric(s$by), xr, pw), m$t, m$t + ph,
        scale(as.numeric(s$by), xr, pw) - 3, m$t + 9, format(s$by, "by %b %d")) else "",
      sprintf('<circle cx="%.1f" cy="%.1f" r="3.5" fill="var(--teal)"/><text x="%.1f" y="%.1f" font-size="10.5" font-weight="600" text-anchor="end" fill="var(--ink)">%s</text>',
              xs[last], ys[last], xs[last] - 6, ys[last] - 6, fmt(d$value[last])))
    legend <- sprintf("%s (%s), latest %s on %s%s.", CHART_SERIES[[s$series]]$label, CHART_SERIES[[s$series]]$unit,
                      fmt(d$value[last]), format(d$date[last], "%b %d, %Y"),
                      if (length(s$threshold)) paste0("; dashed = call threshold ", paste(fmt(s$threshold), collapse = " / ")) else "")
  }
  svg <- sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s" xmlns="http://www.w3.org/2000/svg">%s</svg>',
                 width, height, esc(.chart_title(ch)), body)
  sprintf('<figure class="chart"><div class="ct">%s</div>%s<figcaption><b>%s:</b> %s<br>%s</figcaption></figure>',
          esc(.chart_title(ch)), svg, esc(label), esc(ch$call), esc(legend))
}

# ── Theme coverage (news momentum) ────────────────────────────────────────────
#' Top themes from news_theme_summary(): recent vs older headline counts.
theme_coverage_data <- function(news_df, top = 8) {
  s <- tryCatch(news_theme_summary(news_df), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(NULL)
  s <- utils::head(s[order(-s$n), ], top)
  s[rev(seq_len(nrow(s))), ]                      # largest at the top of a horizontal bar chart
}

theme_coverage_plotly <- function(cov, height = NULL) {
  if (is.null(height)) height <- 70 + 28 * nrow(cov)
  lab <- sprintf("%s  (%s)", cov$theme, cov$momentum)
  p <- plotly::plot_ly(height = height) |>
    plotly::add_bars(y = lab, x = cov$older, orientation = "h", name = "older half of window",
                     marker = list(color = "rgba(154,163,178,0.55)"), hovertemplate = "%{x} older<extra></extra>") |>
    plotly::add_bars(y = lab, x = cov$recent, orientation = "h", name = "newer half",
                     marker = list(color = CHART_COL$line), hovertemplate = "%{x} newer<extra></extra>")
  dark_theme(p, title = "Headline coverage by theme", x = "", date_range = "none") |>
    plotly::layout(barmode = "stack", hovermode = "closest",
                   yaxis = list(categoryorder = "array", categoryarray = lab, title = "",
                                color = "#cfd5e1", gridcolor = "rgba(0,0,0,0)"),
                   legend = list(orientation = "h", y = -0.12, x = 0), margin = list(l = 10, r = 10, t = 40, b = 30))
}

theme_coverage_svg <- function(cov, width = 440) {
  esc <- function(x) htmltools::htmlEscape(x)
  cov <- cov[rev(seq_len(nrow(cov))), ]           # back to largest first, top to bottom
  row <- 24; top <- 6; lab_w <- 200; bar_w <- width - lab_w - 85
  height <- top + row * nrow(cov) + 22
  mx <- max(cov$n, 1)
  bars <- vapply(seq_len(nrow(cov)), function(i) {
    y <- top + (i - 1) * row
    wo <- bar_w * cov$older[i] / mx; wr <- bar_w * cov$recent[i] / mx
    sprintf(paste0('<text x="%d" y="%.1f" font-size="11" text-anchor="end" fill="var(--ink)">%s</text>',
                   '<rect x="%d" y="%.1f" width="%.1f" height="14" fill="var(--mut)" fill-opacity="0.4"/>',
                   '<rect x="%.1f" y="%.1f" width="%.1f" height="14" fill="var(--teal)"/>',
                   '<text x="%.1f" y="%.1f" font-size="10" fill="var(--mut)">%d · %s</text>'),
            lab_w - 8, y + 11, esc(cov$theme[i]), lab_w, y, wo, lab_w + wo, y, wr,
            lab_w + wo + wr + 4, y + 11, cov$n[i], esc(cov$momentum[i]))
  }, "")
  svg <- sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="Headline coverage by theme" xmlns="http://www.w3.org/2000/svg">%s%s</svg>',
                 width, height, paste(bars, collapse = ""),
                 sprintf('<text x="%d" y="%d" font-size="10" fill="var(--mut)">teal = newer half of the news window, grey = older half</text>',
                         lab_w, height - 6))
  sprintf('<figure class="chart"><div class="ct">Headline coverage by theme</div>%s<figcaption>%s</figcaption></figure>',
          svg, esc("Counts of recent headlines matching each theme's keywords. Rising = newer half has 1.5x the older half."))
}

# ── App view ──────────────────────────────────────────────────────────────────
#' Evidence for the insights: list(themes = list(coverage, charts), calls = charts).
#' `data` = list(fred, fwd, mkt); `news_df` feeds the coverage chart.
# Not %||%: the project-wide version (R/synopsis_tier3.R) replaces lists of lists with its fallback.
.or_empty <- function(x) if (is.null(x)) list() else x

build_insight_evidence <- function(text, data, news_df = NULL, today = Sys.Date()) {
  safe <- function(expr) tryCatch(expr, error = function(e) {
    message("[AI charts] ", conditionMessage(e)); NULL })
  list(themes = list(coverage = safe(theme_coverage_data(news_df)),
                     charts = .or_empty(safe(build_theme_charts(text, data, today)))),
       calls = .or_empty(safe(build_call_charts(text, data, today))))
}

.EVIDENCE_NOTE <- c(
  themes = "Bars count recent headlines per theme (teal = newer half of the news window). Charts are the data the AI chose as evidence for each theme, drawn from FRED and market prices.",
  calls  = "Drawn from the app's FRED and market data. Dashed orange = the call's threshold; dotted purple = its deadline (the x-axis extends to it).")

.chart_card <- function(caption, widget) htmltools::div(
  style = "background:#161b27;border:1px solid #2a3042;border-radius:8px;padding:10px 12px;",
  htmltools::div(style = "font-size:12.5px;line-height:1.45;color:#cfd5e1;margin-bottom:6px;",
                 stringr::str_trunc(caption, 260)),
  widget)

.evidence_ui <- function(slot, ev) {
  cards <- list()
  if (slot == "themes" && !is.null(ev$themes$coverage))
    cards[[length(cards) + 1]] <- .chart_card("How much each storyline is in the news, and whether it is building.",
                                              theme_coverage_plotly(ev$themes$coverage))
  charts <- if (slot == "themes") ev$themes$charts else ev$calls
  for (ch in charts) cards[[length(cards) + 1]] <- .chart_card(ch$call, ai_chart_plotly(ch))
  if (!length(cards)) return(NULL)
  htmltools::div(
    class = "ai-evidence", style = "margin:10px 0 22px;",
    htmltools::div(style = "color:#9aa3b2;font-size:12px;margin-bottom:8px;",
                   htmltools::tags$b("Evidence. "), .EVIDENCE_NOTE[[slot]]),
    htmltools::div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,420px),1fr));gap:14px;align-items:start;",
                   cards))
}

#' The AI Insights panel: each markdown section, followed by its evidence block.
insights_ui <- function(text, ev) {
  secs <- split_md_sections(strip_chart_tags(text))
  htmltools::div(id = "llm_output", lapply(secs, function(s) {
    slot <- evidence_slot(s$heading)
    htmltools::tagList(
      htmltools::HTML(markdown::markdownToHTML(text = s$md, fragment.only = TRUE)),
      if (!is.na(slot)) .evidence_ui(slot, ev))
  }))
}
