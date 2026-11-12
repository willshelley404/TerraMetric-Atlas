# ─────────────────────────────────────────────────────────────────────────────
# R/map_risk.R — leaflet and HTML builders for the Regional Risk map
# ─────────────────────────────────────────────────────────────────────────────

.RISK_COL <- list(panel = "#1e2640", page = "#161b27", accent = "#00b4d8",
                  muted = "#9aa3b2", text = "#e0e0e0", worse = "#ff6b6b",
                  better = "#51cf66", border = "#2a3042", warn = "#fcc419")

# ── Hover tooltip, option B layout (spec §10) ────────────────────────────────
#' drivers: data.frame(label, value_text, worse) — first 3 rows shown.
risk_tooltip_html <- function(title, subtitle = NULL, value_text, rank_text = NULL,
                              rank_sub = NULL, drivers = NULL, footer) {
  c <- .RISK_COL
  tg <- htmltools::tags
  has <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  value_text <- if (has(value_text)) value_text else "Insufficient data"

  driver_rows <- NULL
  if (!is.null(drivers) && nrow(drivers) > 0) {
    d <- utils::head(drivers, 3)
    driver_rows <- htmltools::tagList(
      tg$hr(style = sprintf("border:0;border-top:1px solid %s;margin:7px 0;", c$border)),
      tg$div(style = sprintf("color:%s;font-size:11px;", c$muted), "Driven by"),
      lapply(seq_len(nrow(d)), function(i) tg$div(
        style = "display:flex;justify-content:space-between;gap:8px;",
        tg$span(d$label[i]),
        tg$span(style = sprintf("font-weight:600;color:%s;",
                                if (isTRUE(d$worse[i])) c$worse else c$text),
                d$value_text[i])
      ))
    )
  }

  out <- tg$div(
    style = sprintf(paste0("font-family:Inter,system-ui,sans-serif;background:%s;color:%s;",
                           "border-radius:8px;padding:10px 12px;min-width:220px;max-width:280px;",
                           "font-size:12px;line-height:1.5;white-space:normal;"), c$panel, c$text),
    tg$div(style = sprintf("color:%s;font-weight:700;font-size:14px;", c$accent), title),
    if (has(subtitle)) tg$div(style = sprintf("color:%s;font-size:11px;", c$muted), subtitle),
    tg$div(style = "display:flex;align-items:baseline;justify-content:space-between;gap:8px;margin-top:4px;",
           tg$span(style = "font-size:20px;font-weight:700;", value_text),
           if (has(rank_text)) tg$span(
             style = "padding:1px 7px;border-radius:10px;font-size:11px;font-weight:600;background:#5c1f2a;color:#ff8787;",
             rank_text)),
    if (has(rank_sub)) tg$div(style = sprintf("color:%s;font-size:11px;", c$muted), rank_sub),
    driver_rows,
    tg$div(style = sprintf("color:%s;font-size:11px;margin-top:4px;", c$muted), footer)
  )
  htmltools::HTML(as.character(out))
}
