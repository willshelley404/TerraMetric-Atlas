# ─────────────────────────────────────────────────────────────────────────────
# export.R — turn what the app has computed into downloadable tables / text.
#
# Every builder takes plain data (no Shiny state) and returns a data frame, or
# NULL when there is nothing to export yet (e.g. AI text before "Generate").
# server.R wires these to downloadHandlers on the Export tab.
# ─────────────────────────────────────────────────────────────────────────────

# Keep atomic columns only: list/sf geometry columns cannot go in a CSV or workbook.
.export_flat <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(x) {
    if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    else if (is.factor(x)) as.character(x)
    else x
  })
  keep <- vapply(df, is.atomic, logical(1))
  df[, keep, drop = FALSE]
}

# Headlines with their links, most relevant first (the order the app uses).
export_news <- function(news_df) {
  if (is.null(news_df) || nrow(news_df) == 0) return(NULL)
  cols <- intersect(
    c("title", "source_name", "published", "url", "description",
      "relevance_score", "topic", "themes", "supply_chain", "days_in_cache"),
    names(news_df)
  )
  .export_flat(news_df[, cols, drop = FALSE])
}

# Every forecast point: history and forecast, with the confidence band.
export_forecast_series <- function(forecasts) {
  rows <- lapply(names(forecasts), function(sid) {
    fc <- forecasts[[sid]]
    if (is.null(fc) || is.null(fc$data) || nrow(fc$data) == 0) return(NULL)
    cfg <- FORECAST_SERIES[[sid]]
    d <- fc$data
    out <- data.frame(
      series_id = sid,
      indicator = cfg$name %||% sid,
      unit = cfg$unit %||% NA_character_,
      date = as.Date(d$ds),
      type = ifelse(d$is_forecast, "forecast", "actual"),
      actual = d$y,
      forecast = ifelse(d$is_forecast, d$yhat, NA_real_),
      lower = ifelse(d$is_forecast, d$yhat_lower, NA_real_),
      upper = ifelse(d$is_forecast, d$yhat_upper, NA_real_),
      stringsAsFactors = FALSE
    )
    out
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  .export_flat(do.call(rbind, rows))
}

# Headline numbers of the growth-outlook model (score, regime, probabilities, GDP).
export_outlook_summary <- function(outlook) {
  if (is.null(outlook) || isTRUE(outlook$.error)) return(NULL)
  num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else round(suppressWarnings(as.numeric(x[[1]])), 3)
  chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
  rp <- outlook$recession_prob
  gf <- outlook$gdp_forecast
  gn <- outlook$gdpnow
  rows <- list(
    c("Outlook score (0-10)", num(outlook$score), ""),
    c("Regime", chr(outlook$regime$label), ""),
    c("Recession probability", num(rp$prob), "%"),
    c("Recession risk tier", chr(rp$tier$label), ""),
    c("Recession model", chr(rp$model_type), ""),
    c("Recession model: base rate", num(rp$base_rate), ""),
    c("Recession model: training end", chr(rp$train_end), ""),
    c("GDP forecast (point)", num(gf$point), "% SAAR"),
    c("GDP forecast (low)", num(gf$ci_lo), "% SAAR"),
    c("GDP forecast (high)", num(gf$ci_hi), "% SAAR"),
    c("GDP forecast method", chr(gf$method), ""),
    c("GDPNow", num(gn$value), "% SAAR"),
    c("GDPNow quarter", chr(gn$quarter), ""),
    c("As of", chr(outlook$as_of), "")
  )
  df <- data.frame(
    metric = vapply(rows, `[`, "", 1),
    value = vapply(rows, `[`, "", 2),
    unit = vapply(rows, `[`, "", 3),
    stringsAsFactors = FALSE
  )
  df[!is.na(df$value) & df$value != "NA", , drop = FALSE]
}

# The composite score's components (what moved the score).
export_outlook_components <- function(outlook) {
  comps <- outlook$components
  if (is.null(comps) || length(comps) == 0) return(NULL)
  .export_flat(data.frame(
    component = vapply(comps, function(c) c$label %||% NA_character_, ""),
    weight = vapply(comps, function(c) c$weight %||% NA_real_, 0),
    score = vapply(comps, function(c) round(c$score %||% NA_real_, 3), 0),
    detail = vapply(comps, function(c) c$detail %||% NA_character_, ""),
    stringsAsFactors = FALSE
  ))
}

# What pushes the recession probability up or down.
export_recession_drivers <- function(outlook) {
  fc <- outlook$recession_prob$factor_contributions
  if (is.null(fc) || nrow(fc) == 0) return(NULL)
  df <- data.frame(
    predictor = fc$name,
    current_value = fc$raw_value,
    z_score = round(fc$z_score, 3),
    coefficient = round(fc$beta, 3),
    contribution_to_logit = round(fc$contribution, 3),
    data_missing = fc$data_missing,
    stringsAsFactors = FALSE
  )
  .export_flat(df[order(-abs(df$contribution_to_logit)), , drop = FALSE])
}

# Dashboard KPIs (named scalar list) as a long table.
export_kpis <- function(kpis) {
  if (is.null(kpis) || length(kpis) == 0) return(NULL)
  ok <- vapply(kpis, function(x) is.atomic(x) && length(x) == 1, logical(1))
  if (!any(ok)) return(NULL)
  data.frame(kpi = names(kpis)[ok],
             value = vapply(kpis[ok], function(x) as.character(x), ""),
             stringsAsFactors = FALSE)
}

# ── AI report (self-contained HTML) ──────────────────────────────────────────
# Insights, metro analysis and chat as one styled page. `snapshot` is the outlook
# summary table (metric/value/unit) shown as headline cards. NULL when there is
# no AI text yet.
export_ai_html <- function(insights = NULL, metro_text = NULL, metro_label = NULL,
                           chat = list(), provider = NULL, model = NULL,
                           snapshot = NULL, generated = Sys.time(), charts = character()) {
  has <- function(x) is.character(x) && length(x) == 1 && nzchar(x)
  if (!has(insights) && !has(metro_text) && length(chat) == 0) return(NULL)
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  md <- function(x) commonmark::markdown_html(x, extensions = TRUE)

  card <- function(label, value, sub = NA_character_) sprintf(
    "<div class=\"card\"><div class=\"k\">%s</div><div class=\"v\">%s</div>%s</div>",
    esc(label), esc(value), if (is.na(sub)) "" else sprintf("<div class=\"s\">%s</div>", esc(sub)))
  cards <- ""
  if (is.data.frame(snapshot) && nrow(snapshot) > 0) {
    val <- function(m) {
      r <- snapshot[snapshot$metric == m, , drop = FALSE]
      if (nrow(r) == 0 || is.na(r$value[1])) NA_character_ else r$value[1]
    }
    with_unit <- function(m) {
      v <- val(m); if (is.na(v)) return(NA_character_)
      u <- snapshot$unit[snapshot$metric == m][1]
      if (!is.na(u) && nzchar(u)) paste(v, u) else v
    }
    lo <- val("GDP forecast (low)"); hi <- val("GDP forecast (high)")
    tier <- val("Recession risk tier")
    gnq <- val("GDPNow quarter")
    score <- val("Outlook score (0-10)")
    items <- list(
      list("Outlook score", if (is.na(score)) NA_character_ else paste0(score, " / 10"),
           "Composite of labor, consumer, rates, housing and market signals. 5 = neutral; higher is stronger."),
      list("Regime", val("Regime"), "Label derived from the GDP forecast."),
      list("Recession probability", with_unit("Recession probability"),
           if (is.na(tier)) NA_character_ else paste0(tier, " risk · chance of an NBER recession in the next 12 months")),
      list("GDP forecast", with_unit("GDP forecast (point)"),
           if (is.na(lo) || is.na(hi)) NA_character_ else paste0("Next quarter, annualized · range ", lo, " to ", hi)),
      list("GDPNow", with_unit("GDPNow"),
           if (is.na(gnq)) NA_character_ else paste0("Atlanta Fed nowcast for ", gnq))
    )
    items <- Filter(function(x) !is.na(x[[2]]), items)
    if (length(items)) {
      cards <- paste0("<div class=\"cards\">",
                      paste(vapply(items, function(x) card(x[[1]], x[[2]], x[[3]]), ""), collapse = ""), "</div>")
    }
  }

  sections <- character()
  if (has(insights)) {
    # `charts`: list(themes = <svg figures>, calls = <svg figures>), each placed
    # right after its section; a plain character vector is appended at the end.
    block <- function(figs, note) {
      figs <- figs[nzchar(figs)]
      if (!length(figs)) return("")
      paste0("<p class=\"meta\"><b>Evidence.</b> ", esc(note), "</p><div class=\"charts\">",
             paste(figs, collapse = ""), "</div>")
    }
    notes <- c(themes = paste("Bars count recent headlines per theme (teal = newer half of the news window);",
                              "charts are the data the AI chose as evidence, drawn from FRED and market prices."),
               calls = paste("Drawn from FRED and market data loaded in the app.",
                             "Dashed orange = the call's threshold; dotted line = its deadline."))
    body <- if (is.list(charts) && length(charts) && exists("split_md_sections", mode = "function")) {
      paste(vapply(split_md_sections(insights), function(s) {
        slot <- evidence_slot(s$heading)
        paste0(md(s$md), if (!is.na(slot)) block(unlist(charts[[slot]]), notes[[slot]]) else "")
      }, ""), collapse = "")
    } else paste0(md(insights), block(unlist(charts), notes[["calls"]]))
    sections <- c(sections, sprintf("<section><h2 class=\"sec\">Economic insights</h2>%s</section>", body))
  }
  if (has(metro_text)) {
    sections <- c(sections, sprintf(
      "<section><h2 class=\"sec\">Metro analysis<span> — %s</span></h2>%s</section>",
      esc(if (has(metro_label)) metro_label else "selected area"), md(metro_text)))
  }
  if (length(chat) > 0) {
    turns <- vapply(chat, function(m) {
      if (identical(m$role, "user")) sprintf("<div class=\"q\">%s</div>", esc(m$content))
      else sprintf("<div class=\"a\">%s</div>", md(m$content))
    }, "")
    sections <- c(sections, sprintf("<section><h2 class=\"sec\">Ask the Economist</h2>%s</section>",
                                    paste(turns, collapse = "")))
  }

  meta <- paste0("Exported ", format(generated, "%b %d, %Y %H:%M"),
                 if (has(provider)) paste0(" · ", esc(provider), if (has(model)) paste0(" / ", esc(model)) else ""))
  paste0("<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
         "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
         "<title>TerraMetric Atlas — AI report</title><style>", .export_css, "</style></head><body><main>",
         "<header><h1>TerraMetric Atlas</h1><p class=\"meta\">AI report · ", meta, "</p></header>",
         cards, paste(sections, collapse = ""),
         "<footer>AI-generated analysis from live data and headlines. Verify figures before relying on them.</footer>",
         "</main></body></html>")
}

.export_css <- "
:root{--bg:#f6f7fa;--card:#fff;--ink:#1B2A4A;--mut:#6c757d;--line:#e3e6ec;--acc:#FC6600;--teal:#2EC4B6;--th:#1B2A4A}
@media (prefers-color-scheme:dark){:root{--bg:#0f1117;--card:#1a2033;--ink:#e6e9f0;--mut:#9aa3b2;--line:#2a3042;--th:#2a3350}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.65 Inter,'Segoe UI',system-ui,Arial,sans-serif}
main{max-width:920px;margin:0 auto;padding:32px 20px 48px}
header{border-bottom:3px solid var(--acc);padding-bottom:12px;margin-bottom:22px}
h1{margin:0;font-size:26px;letter-spacing:-.3px}
.meta{margin:4px 0 0;color:var(--mut);font-size:13px}
.cards{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:26px}
.card{flex:1 1 160px;background:var(--card);border:1px solid var(--line);border-left:4px solid var(--teal);border-radius:8px;padding:10px 14px}
.card .k{font-size:11px;text-transform:uppercase;letter-spacing:.8px;color:var(--mut)}
.card .v{font-size:20px;font-weight:700;margin-top:2px}
.card .s{font-size:11.5px;line-height:1.4;color:var(--mut);margin-top:4px}
section{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:8px 26px 18px;margin-bottom:22px}
h2.sec{font-size:13px;text-transform:uppercase;letter-spacing:1.2px;color:var(--acc);margin:18px 0 6px}
h2.sec span{color:var(--mut);text-transform:none;letter-spacing:0;font-weight:500}
section h2:not(.sec){font-size:18px;margin:26px 0 6px;padding-bottom:4px;border-bottom:1px solid var(--line)}
section h3{font-size:15px;margin:18px 0 4px}
p{margin:8px 0}
ul,ol{padding-left:22px}
table{border-collapse:collapse;width:100%;margin:12px 0;font-size:13.5px;display:block;overflow-x:auto}
th{background:var(--th);color:#fff;text-align:left;font-weight:600}
th,td{padding:7px 10px;border-bottom:1px solid var(--line);vertical-align:top}
tbody tr:nth-child(even){background:rgba(127,127,127,.06)}
hr{border:0;border-top:1px solid var(--line);margin:18px 0}
blockquote{margin:10px 0;padding:2px 14px;border-left:3px solid var(--teal);color:var(--mut)}
code{background:rgba(127,127,127,.15);padding:1px 5px;border-radius:4px;font-size:.9em}
.q{margin:14px 0 6px;padding:6px 12px;border-left:3px solid var(--acc);font-weight:600}
.a{margin:0 0 6px}
footer{color:var(--mut);font-size:12px;text-align:center;margin-top:8px}
.charts{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,380px),1fr));gap:14px;margin:12px 0}
figure.chart{margin:0;border:1px solid var(--line);border-radius:8px;padding:10px 12px;background:var(--card)}
figure.chart .ct{font-size:13px;font-weight:600;margin-bottom:4px}
figure.chart svg{width:100%;height:auto;display:block;font-family:inherit}
figure.chart figcaption{font-size:12px;line-height:1.45;color:var(--mut);margin-top:6px}
@media print{body{background:#fff}section,.card,figure.chart{break-inside:avoid}}
"

# ── Tables (workbook sheets) ─────────────────────────────────────────────────
# Named list of data frames; empty ones are dropped.
export_sheets <- function(forecasts = NULL, outlook = NULL, news = NULL, kpis = NULL,
                          risk_table = NULL, metro = NULL) {
  sheets <- list(
    `Outlook summary`      = export_outlook_summary(outlook),
    `Outlook components`   = export_outlook_components(outlook),
    `Recession drivers`    = export_recession_drivers(outlook),
    `Forecast summary`     = if (!is.null(forecasts)) .export_flat(forecast_summary_table(forecasts)) else NULL,
    `Forecast series`      = if (!is.null(forecasts)) export_forecast_series(forecasts) else NULL,
    `Forecast diagnostics` = if (!is.null(forecasts)) .export_flat(forecast_diagnostics_overview(forecasts)) else NULL,
    News                   = export_news(news),
    KPIs                   = export_kpis(kpis),
    `Regional risk`        = .export_flat(risk_table),
    `Metro ACS`            = .export_flat(metro)
  )
  Filter(Negate(is.null), sheets)
}

.export_sheet_notes <- c(
  `Outlook summary`      = "Growth-outlook score, regime, recession probability and GDP forecast.",
  `Outlook components`   = "The weighted components behind the outlook score.",
  `Recession drivers`    = "What pushes the recession probability up or down (logit contributions).",
  `Forecast summary`     = "Current value, 6- and 18-month forecasts and 6-month interval per indicator.",
  `Forecast series`      = "Every actual and forecast point with its confidence band.",
  `Forecast diagnostics` = "In-sample fit of each forecast model (R², RMSE, MAE).",
  News                   = "Headlines used as AI context, most relevant first; click Open for the article.",
  KPIs                   = "Dashboard KPI values at export time.",
  `Regional risk`        = "County or tract table as shown on the Regional Risk tab (selected layer).",
  `Metro ACS`            = "ACS 5-year metro / micro area indicators."
)

# Column width: wide enough for the header and typical values, capped.
.xl_width <- function(x, header, cap = 60) {
  v <- as.character(x[!is.na(x)])
  n <- if (length(v)) as.numeric(stats::quantile(nchar(v), 0.9, names = FALSE)) else 0
  max(8, min(cap, max(nchar(header) + 3, n + 2)))
}

# A formatted workbook: contents sheet, navy header row, frozen panes, filters,
# number formats, clickable article links. Needs openxlsx.
export_workbook <- function(sheets, file, generated = Sys.time()) {
  wb <- openxlsx::createWorkbook(creator = "TerraMetric Atlas")
  openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")
  navy <- "#1B2A4A"; orange <- "#FC6600"; rule <- "#E3E6EC"
  sty <- function(...) openxlsx::createStyle(valign = "top", border = "bottom", borderColour = rule, ...)
  st_head <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = navy, textDecoration = "bold",
                                   halign = "left", valign = "center", border = "bottom",
                                   borderColour = orange, borderStyle = "medium", wrapText = TRUE)
  st_body <- sty()
  st_wrap <- sty(wrapText = TRUE)
  st_int  <- sty(numFmt = "#,##0")
  st_dec  <- sty(numFmt = "#,##0.00")
  st_date <- sty(numFmt = "yyyy-mm-dd", halign = "left")
  st_num1 <- sty(numFmt = "0.0##", halign = "left")
  st_link <- sty(fontColour = "#0563C1", textDecoration = "underline")
  st_title <- openxlsx::createStyle(fontSize = 18, textDecoration = "bold", fontColour = navy)
  st_sub <- openxlsx::createStyle(fontColour = "#6c757d")

  # Contents sheet first
  openxlsx::addWorksheet(wb, "Contents", tabColour = orange, gridLines = FALSE)
  openxlsx::writeData(wb, "Contents", "TerraMetric Atlas — export", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "Contents", st_title, rows = 2, cols = 2)
  openxlsx::writeData(wb, "Contents",
                      paste("Exported", format(generated, "%b %d, %Y %H:%M"), "· AI text is in the separate HTML report"),
                      startCol = 2, startRow = 3)
  openxlsx::addStyle(wb, "Contents", st_sub, rows = 3, cols = 2)
  toc <- data.frame(Sheet = names(sheets), Rows = vapply(sheets, nrow, 1L),
                    About = unname(.export_sheet_notes[names(sheets)]), stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Contents", toc, startCol = 2, startRow = 5, headerStyle = st_head)
  trow <- 6:(5 + nrow(toc))
  openxlsx::addStyle(wb, "Contents", st_body, rows = trow, cols = 2:4, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Contents", st_int, rows = trow, cols = 3, gridExpand = TRUE)
  for (k in seq_along(sheets)) {
    openxlsx::writeFormula(wb, "Contents", openxlsx::makeHyperlinkString(sheet = names(sheets)[k], row = 1, col = 1, text = names(sheets)[k]),
                           startCol = 2, startRow = 5 + k)
  }
  openxlsx::addStyle(wb, "Contents", st_link, rows = trow, cols = 2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Contents", cols = 1:4, widths = c(2, 24, 8, 82))

  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    n <- nrow(df); rows <- 2:(n + 1)
    openxlsx::addWorksheet(wb, nm, tabColour = navy)

    # Outlook summary mixes numbers and text in one column; write numbers as numbers.
    vals <- NULL
    if (nm == "Outlook summary") {
      num <- suppressWarnings(as.numeric(df$value))
      vals <- as.list(df$value); vals[!is.na(num)] <- as.list(num[!is.na(num)])
      df$value <- NA_character_
    }
    link_col <- match("url", names(df))
    urls <- NULL
    if (!is.na(link_col)) { urls <- df$url; df$url <- NA_character_ }

    openxlsx::writeData(wb, nm, df, headerStyle = st_head, withFilter = n > 1)
    openxlsx::addStyle(wb, nm, st_body, rows = rows, cols = seq_len(ncol(df)), gridExpand = TRUE)

    widths <- numeric(ncol(df))
    for (j in seq_len(ncol(df))) {
      x <- df[[j]]
      w <- .xl_width(x, names(df)[j])
      if (is.numeric(x)) {
        whole <- all(is.na(x) | abs(x - round(x)) < 1e-9)
        openxlsx::addStyle(wb, nm, if (whole) st_int else st_dec, rows = rows, cols = j, gridExpand = TRUE)
        w <- max(w, 11)
      } else if (inherits(x, "Date")) {
        openxlsx::addStyle(wb, nm, st_date, rows = rows, cols = j, gridExpand = TRUE)
        w <- max(w, 12)
      } else if (is.character(x) && w >= 60) {
        openxlsx::addStyle(wb, nm, st_wrap, rows = rows, cols = j, gridExpand = TRUE)
      }
      widths[j] <- w
    }

    if (!is.null(vals)) {
      for (r in seq_along(vals)) {
        v <- vals[[r]]
        if (is.na(v)) next
        openxlsx::writeData(wb, nm, v, startCol = 2, startRow = r + 1, colNames = FALSE)
        if (is.numeric(v)) openxlsx::addStyle(wb, nm, st_num1, rows = r + 1, cols = 2)
      }
      widths[2] <- 34
    }
    if (!is.na(link_col)) {
      for (r in seq_along(urls)) {
        if (!is.na(urls[r]) && nzchar(urls[r])) {
          openxlsx::writeFormula(wb, nm, openxlsx::makeHyperlinkString(urls[r], "Open"),
                                 startCol = link_col, startRow = r + 1)
        }
      }
      openxlsx::addStyle(wb, nm, st_link, rows = rows, cols = link_col, gridExpand = TRUE)
      widths[link_col] <- 9
    }
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(df)), widths = widths)
    openxlsx::setRowHeights(wb, nm, rows = 1, heights = 22)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Safe file-name stem for a sheet name.
export_slug <- function(x) gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(x)))
