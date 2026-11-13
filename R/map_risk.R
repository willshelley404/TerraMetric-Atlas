# ─────────────────────────────────────────────────────────────────────────────
# R/map_risk.R — leaflet and HTML builders for the Regional Risk map
# ─────────────────────────────────────────────────────────────────────────────

.RISK_COL <- list(panel = "#1e2640", page = "#161b27", accent = "#00b4d8",
                  muted = "#9aa3b2", text = "#e0e0e0", worse = "#ff6b6b",
                  better = "#51cf66", border = "#2a3042", warn = "#fcc419")

# ── Hover tooltip, option B layout (spec §10) ────────────────────────────────
# String templates, vectorised: thousands of county labels are built per render,
# and htmltools tag trees cost ~1 ms each.
.tt_has <- function(x) !is.na(x) & nzchar(x)
.tt_esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))

#' One HTML label per element. All text arguments are vectors of equal length
#' (or length 1); NA means "omit" (value_text NA shows "Insufficient data").
#' drivers_html: pre-rendered "Driven by" block per element ("" for none).
#' rank_bad: TRUE where the rank sits on the bad side (red pill), FALSE neutral.
#' standing: pre-escaped HTML lines shown between the drivers block and the footer.
risk_tooltips_vec <- function(title, subtitle = NA, value_text, rank_text = NA,
                              rank_sub = NA, footer, drivers_html = "", rank_bad = TRUE,
                              standing = "", zips = NA) {
  c <- .RISK_COL
  n <- max(length(title), length(value_text), length(footer))
  rep_n <- function(x) rep_len(x, n)
  subtitle <- rep_n(subtitle); rank_text <- rep_n(rank_text); rank_sub <- rep_n(rank_sub)
  value_text <- ifelse(.tt_has(value_text), value_text, "Insufficient data")
  pill_style <- ifelse(rep_n(rank_bad) %in% TRUE, "background:#5c1f2a;color:#ff8787;",
                       "background:#1f3b4d;color:#8fd3ff;")

  sub_html <- ifelse(.tt_has(subtitle), sprintf(
    '<div style="color:%s;font-size:11px;">%s</div>', c$muted, .tt_esc(subtitle)), "")
  pill <- ifelse(.tt_has(rank_text), sprintf(paste0(
    '<span style="padding:1px 7px;border-radius:10px;font-size:11px;font-weight:600;',
    '%s">%s</span>'), pill_style, .tt_esc(rank_text)), "")
  rsub_html <- ifelse(.tt_has(rank_sub), sprintf(
    '<div style="color:%s;font-size:11px;">%s</div>', c$muted, .tt_esc(rank_sub)), "")
  zips <- rep_n(zips)
  rsub_html <- paste0(rsub_html, ifelse(.tt_has(zips), sprintf(
    '<div style="color:%s;font-size:11px;">%s</div>', c$muted, .tt_esc(zips)), ""))

  out <- paste0(
    sprintf(paste0('<div style="font-family:Inter,system-ui,sans-serif;background:%s;color:%s;',
                   'border-radius:8px;padding:10px 12px;min-width:220px;max-width:280px;',
                   'font-size:12px;line-height:1.5;white-space:normal;">'), c$panel, c$text),
    sprintf('<div style="color:%s;font-weight:700;font-size:14px;">%s</div>', c$accent,
            .tt_esc(title)),
    sub_html,
    '<div style="display:flex;align-items:baseline;justify-content:space-between;gap:8px;margin-top:4px;">',
    sprintf('<span style="font-size:20px;font-weight:700;">%s</span>', .tt_esc(value_text)),
    pill, '</div>',
    rsub_html,
    rep_n(drivers_html),
    rep_n(standing),
    sprintf('<div style="color:%s;font-size:11px;margin-top:4px;">%s</div>', c$muted,
            .tt_esc(footer)),
    '</div>')
  lapply(out, htmltools::HTML)
}

#' "Driven by" blocks, one per element. labels: character(k), or a list of k
#' character vectors when the label differs per element; values: list of k
#' character vectors (one value per element); worse: list of k logical vectors.
#' A NULL element of values/worse (a missing column) counts as all-NA/FALSE; `n` is the
#' element count (default: the longest of values). Rows with an NA value are omitted,
#' and each element shows at most its first `max_rows` present rows. Elements whose
#' values are all NA get "".
risk_drivers_vec <- function(labels, values, worse, n = max(lengths(values)), max_rows = 3) {
  c <- .RISK_COL
  fill <- function(x, na) rep_len(if (is.null(x)) na else x, n)    # NULL = missing column
  labels <- if (is.list(labels)) unlist(lapply(labels, fill, NA_character_)) else
    rep(labels, each = n)
  vm <- matrix(unlist(lapply(values, fill, NA_character_)), nrow = n)   # n x k
  wm <- matrix(unlist(lapply(worse, fill, FALSE)), nrow = n) %in% TRUE
  seen <- matrix(0L, nrow = n, ncol = ncol(vm))   # present rows so far, left to right
  run <- rep(0L, n)
  for (j in seq_len(ncol(vm))) { run <- run + !is.na(vm[, j]); seen[, j] <- run }
  hide <- is.na(vm) | seen > max_rows
  rows <- matrix(sprintf(
    paste0('<div style="display:flex;justify-content:space-between;gap:8px;">',
           '<span>%s</span><span style="font-weight:600;color:%s;">%s</span></div>'),
    .tt_esc(labels), ifelse(wm, c$worse, c$text), .tt_esc(vm)), nrow = n)
  rows[hide] <- ""
  out <- paste0(
    sprintf('<hr style="border:0;border-top:1px solid %s;margin:7px 0;"/>', c$border),
    sprintf('<div style="color:%s;font-size:11px;">Driven by</div>', c$muted),
    apply(rows, 1, paste, collapse = ""))
  ifelse(rowSums(!hide) > 0, out, "")
}

#' Hover label: name, value and rank pill on two short lines, never wrapped. Everything
#' else (drivers, metro line, sources) lives in the click panel and the data table.
risk_tooltips_compact_vec <- function(title, value_text, rank_text = NA, rank_bad = TRUE,
                                      zips = NA, note = NA) {
  c <- .RISK_COL
  n <- max(length(title), length(value_text))
  rep_n <- function(x) rep_len(x, n)
  note_html <- ifelse(.tt_has(rep_n(note)), sprintf(
    '<div style="color:%s;font-size:10px;">%s</div>', c$text, .tt_esc(rep_n(note))), "")
  zip_html <- ifelse(.tt_has(rep_n(zips)), sprintf(
    '<div style="color:%s;font-size:10px;">%s</div>', c$muted, .tt_esc(rep_n(zips))), "")
  value_text <- ifelse(.tt_has(value_text), value_text, "Insufficient data")
  rank_text <- rep_n(rank_text)
  pill_style <- ifelse(rep_n(rank_bad) %in% TRUE, "background:#5c1f2a;color:#ff8787;",
                       "background:#1f3b4d;color:#8fd3ff;")
  pill <- ifelse(.tt_has(rank_text), sprintf(paste0(
    '<span style="padding:0 6px;border-radius:9px;font-size:10px;font-weight:600;',
    '%s">%s</span>'), pill_style, .tt_esc(rank_text)), "")
  out <- paste0(
    sprintf(paste0('<div style="font-family:Inter,system-ui,sans-serif;background:%s;color:%s;',
                   'border-radius:6px;padding:5px 9px;font-size:11px;line-height:1.4;',
                   'white-space:nowrap;opacity:.96;">'), c$panel, c$text),
    sprintf('<div style="color:%s;font-weight:700;font-size:12px;">%s</div>', c$accent,
            .tt_esc(rep_n(title))),
    '<div style="display:flex;align-items:center;gap:7px;">',
    sprintf('<span style="font-size:14px;font-weight:700;">%s</span>', .tt_esc(rep_n(value_text))),
    pill, '</div>', note_html, zip_html, '</div>')
  lapply(out, htmltools::HTML)
}

#' Single tooltip. drivers: data.frame(label, value_text, worse) — first 3 shown.
risk_tooltip_html <- function(title, subtitle = NULL, value_text, rank_text = NULL,
                              rank_sub = NULL, drivers = NULL, footer) {
  na1 <- function(x) if (is.null(x) || length(x) == 0) NA else x
  drivers_html <- ""
  if (!is.null(drivers) && nrow(drivers) > 0) {
    d <- utils::head(drivers, 3)
    drivers_html <- risk_drivers_vec(d$label, as.list(as.character(d$value_text)),
                                     as.list(d$worse %in% TRUE))
  }
  risk_tooltips_vec(title, na1(subtitle), na1(value_text), na1(rank_text), na1(rank_sub),
                    footer, drivers_html)[[1]]
}

# ── Layer registry ───────────────────────────────────────────────────────────
# derived = TRUE: the column is computed inside build_county_layers from the base
# percentiles (assistance, credit, tax base), so its percentile is added after them.
RISK_LAYERS <- c(
  list(stress_now = list(
    label = "Stress now: unemployment vs. a year ago", col = "momentum_pp", fmt = "pp",
    pal = "RdBu", rev = TRUE, higher_is_worse = TRUE, source = "BLS LAUS",
    levels = "county", lens = "context",
    loading_text = "loading county unemployment from BLS…",
    bins = c(-50, -1, -0.5, -0.2, 0.2, 0.5, 1, 50),
    bin_labels = c("below −1.0 pp", "−1.0 to −0.5", "−0.5 to −0.2", "−0.2 to +0.2",
                   "+0.2 to +0.5", "+0.5 to +1.0 (rising)",
                   "above +1.0 pp (Sahm-style trigger)")),
    stress_ahead = list(
    label = "Stress ahead: projected job growth vs. US (shift-share)",
    col = "stress_ahead_pp", fmt = "pp", pal = "RdBu", rev = FALSE,
    higher_is_worse = FALSE, digits = 2, source = "BLS CES (FRED) × ACS industry mix",
    levels = "county", lens = "context",
    loading_text = "loading sector payrolls and county industry mix…",
    partial_text = "partial: some sector data missing",
    bins = c(-50, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 50)),
    # Categorical: the value is the quadrant code (see county_now_vs_ahead()), so the
    # palette is explicit and colours are never rescaled to a state.
    now_vs_ahead = list(
      label = "Now vs ahead: where stress is building or easing",
      col = "now_vs_ahead", fmt = "quadrant", pal = "RdBu", rev = FALSE,
      higher_is_worse = TRUE, source = "BLS LAUS × BLS CES (FRED) × ACS industry mix",
      levels = "county", lens = "context", derived = TRUE, categorical = TRUE,
      loading_text = "loading county unemployment and sector payrolls…",
      partial_text = "partial: some stress data missing",
      colors = c("#a9c4d9", "#2EC4B6", "#FC6600", "#b3122e"),
      bins = c(0.5, 1.5, 2.5, 3.5, 4.5),
      bin_labels = c("Healthy: neither in the worst third",
                     "Recovering: bad now, better ahead",
                     "Worsening: fine now, bad ahead",
                     "Bad now and ahead"))),
  local({
    # A tract is noisy here when either cushion input from the ACS is.
    asg <- function(label, col, fmt, source, ...) list(
      label = label, col = col, fmt = fmt, pal = "YlOrRd", rev = FALSE,
      higher_is_worse = TRUE, source = source, levels = c("county", "tract"),
      lens = "assistance", derived = TRUE, noisy_cols = c("noisy_poverty_rate", "noisy_rent_burden_rate"),
      loading_text = "loading sector payrolls and county industry mix…", ...)
    src <- "LODES / ACS × CES sector weights"
    list(
      assist_score = asg("Assistance need score (0–100)", "assist_score", "score", src,
                         bins = c(0, 20, 35, 50, 65, 80, 101),
                         bin_labels = c("0–20", "20–35", "35–50", "50–65", "65–80", "80–100")),
      assist_exposure = asg("Exposure to weakening sectors (index)", "assist_exposure",
                            "score", src),
      assist_cushion = asg("Thin financial cushion (percentile)", "assist_cushion", "score",
                           "LODES / ACS"),
      assist_workers = asg(
        "Exposure-weighted workers (index headcount, not a layoff forecast)",
        "assist_workers", "n", src))
  }),
  local({
    # County-only; every value is derived inside build_county_layers (credit_components()).
    cr <- function(label, col, fmt, source, ...) {
      base <- list(label = label, col = col, fmt = fmt, pal = "YlOrRd", rev = FALSE,
                   higher_is_worse = TRUE, source = source, levels = "county",
                   lens = "credit", derived = TRUE,
                   loading_text = "loading CFPB, Urban Institute and FHFA data…")
      utils::modifyList(base, list(...))
    }
    list(
      credit_score = cr("Household credit risk score (0–100)", "credit_score", "score",
                        "CFPB · Urban · FHFA · BLS", bins = c(0, 20, 35, 50, 65, 80, 101),
                        partial_text = "partial: some job inputs missing or incomplete",
                        bin_labels = c("0–20", "20–35", "35–50", "50–65", "65–80", "80–100")),
      credit_mortgage = cr("Mortgages 90+ days delinquent (%)", "dq90", "%",
                           "CFPB mortgage performance"),
      credit_consumer = cr("Card & auto loans delinquent (%)", "consumer_del", "%",
                           "Urban Institute (Debt in America)"),
      credit_jobs = cr("Job pressure (percentile)", "cr_jobs", "score", "BLS LAUS / CES",
                       partial_text = "partial: some job inputs missing or incomplete"),
      credit_collateral = cr("Home prices, year over year (%)", "hpi_yoy", "pct_signed",
                             "FHFA house price index", pal = "RdBu", rev = FALSE,
                             higher_is_worse = FALSE, digits = 1,
                             bins = c(-50, -5, -2, 0, 2, 5, 50)))
  }),
  stats::setNames(lapply(names(VAR_CFG), function(id) {
    v <- VAR_CFG[[id]]
    list(label = v$label, col = id, fmt = v$fmt, pal = v$pal, rev = v$rev,
         higher_is_worse = v$rev, source = "Census ACS 5-yr", levels = c("county", "tract"),
         lens = "context")
  }), names(VAR_CFG)),
  list(tax_base = list(
    label = "Tax base pressure (proxy — not a bond default model)", col = "tax_base",
    fmt = "score", pal = "YlOrBr", rev = FALSE, higher_is_worse = TRUE,
    source = "Census PEP · FHFA (proxy — not a bond default model)", levels = "county",
    lens = "context", derived = TRUE,
    loading_text = "loading CFPB, Urban Institute and FHFA data…")),
  local({
    mig <- function(label, col, bins) list(
      label = label, col = col, fmt = "per1k", pal = "PuOr", rev = FALSE,
      higher_is_worse = NA, source = "Census PEP", levels = "county", lens = "migration",
      rank_frame = "for in-migration", bins = bins,
      loading_text = "loading Census population estimates…")
    rate_bins <- c(-1000, -10, -5, -2, -0.5, 0.5, 2, 5, 10, 1000)
    list(
      mig_dom = mig("Net domestic migration (per 1,000)", "mig_dom_rate", rate_bins),
      mig_intl = mig("Net international migration (per 1,000)", "mig_intl_rate", rate_bins),
      mig_net = mig("Total net migration (per 1,000)", "mig_net_rate", rate_bins),
      mig_dom_cum = mig("Net domestic migration since 2020 (per 1,000 of 2020 pop.)",
                        "mig_dom_cum", c(-1000, -50, -25, -10, -2, 2, 10, 25, 50, 1000)))
  })
)

# Lenses in display order; only lenses that have layers.
RISK_LENSES <- c("Assistance" = "assistance", "Credit" = "credit", "Migration" = "migration",
                 "Context" = "context")

#' Layer ids (values) and labels (names) of one lens, in registry order.
risk_lens_choices <- function(lens) {
  ids <- names(RISK_LAYERS)[vapply(RISK_LAYERS, function(l) identical(l$lens, lens), TRUE)]
  stats::setNames(ids, vapply(RISK_LAYERS[ids], `[[`, "", "label"))
}
risk_lens_default <- function(lens) unname(risk_lens_choices(lens)[1])

# Display text for the Stress now flag column (values stay Trigger/Rising/None).
.flag_text <- function(flag) ifelse(flag %in% "Trigger", "Sahm-style trigger", flag)
# Only a raised flag is worth showing; "None" is the quiet default.
.flag_shown <- function(flag) flag %in% c("Rising", "Trigger")

#' digits applies to "pp" and "pct_signed" (layers may set `digits` in RISK_LAYERS; default 1). Values that
#' round to zero print as +0.0 / +0.00, never a signed zero.
.risk_fmt <- function(v, fmt, digits = 1) {
  out <- switch(fmt,
    "$"  = paste0("$", formatC(round(v), format = "d", big.mark = ",")),
    "pp" = {
      r <- round(v, digits); r[!is.na(r) & r == 0] <- 0
      sprintf(paste0("%+.", digits, "f pp"), r)
    },
    "pct_signed" = {      # a percent change (not points), signed
      r <- round(v, digits); r[!is.na(r) & r == 0] <- 0
      sprintf(paste0("%+.", digits, "f%%"), r)
    },
    "per1k" = sprintf("%+.1f per 1,000", v),
    "quadrant" = QUAD_LABELS[round(v)],
    "score" = sprintf("%.0f", v),
    "n"  = formatC(round(v), big.mark = ",", format = "d"),
    sprintf("%.1f%%", v))
  out[is.na(v)] <- NA_character_
  out
}

# "No signal" colour for the middle bin of a diverging palette: the Brewer centre is near
# white, which disappears into the light basemap. Missing values get a darker grey.
.RISK_NEUTRAL <- "#c4c9d0"
.RISK_NA <- "#8a8f99"

#' Brewer colours for `n` bins; a diverging RdBu palette with an odd bin count has its
#' pale centre swapped for a visible neutral. NULL (use the palette name) otherwise.
.palette_colors <- function(cfg, n) {
  if (!identical(cfg$pal, "RdBu") || n < 3 || n > 11 || n %% 2 == 0) return(NULL)
  cols <- RColorBrewer::brewer.pal(n, "RdBu")
  cols[(n + 1) / 2] <- .RISK_NEUTRAL
  cols
}

.risk_palette <- function(cfg, vals) {
  if (!is.null(cfg$bins)) {
    # Not %||%: it is scalar in the app, and these are vectors.
    cols <- if (!is.null(cfg$colors)) cfg$colors else .palette_colors(cfg, length(cfg$bins) - 1)
    return(leaflet::colorBin(if (is.null(cols)) cfg$pal else cols,
                             domain = range(cfg$bins), reverse = cfg$rev,
                             bins = cfg$bins, na.color = .RISK_NA))
  }
  if (all(is.na(vals))) vals <- c(0, 1)      # nothing loaded yet: a grey map, not an error
  brks <- unique(stats::quantile(vals, seq(0, 1, length.out = 8), na.rm = TRUE))
  if (length(brks) < 3) brks <- 7
  pal <- leaflet::colorBin(.palette_colors(cfg, length(brks) - 1) %||% cfg$pal,
                           domain = vals, bins = brks, reverse = cfg$rev, na.color = .RISK_NA)
  if (length(brks) >= 3) attr(pal, "brks") <- unname(brks)    # the legend draws its own rows
  pal
}

# Fill opacity: translucent enough for the basemap's roads and places to read through.
RISK_FILL_OPACITY <- 0.6
RISK_TRACT_OPACITY <- 0.7
RISK_DIM_OPACITY <- 0.2          # counties under a drilled-in tract layer

#' Legend rows, worst at the top: a layer where high is worse lists its highest values first,
#' one where low is worse (stress ahead, home prices, income) its lowest first; a neutral
#' layer (migration) lists high values first.
.risk_legend <- function(map, pal, vals, cfg, lbl_fmt, title) {
  b <- if (!is.null(cfg$bins)) cfg$bins else attr(pal, "brks")
  if (is.null(b)) {
    return(leaflet::addLegend(map, pal = pal, values = vals, opacity = 0.9,
                              layerId = "risk_legend", title = title,
                              position = "bottomright", labFormat = lbl_fmt))
  }
  labs <- if (!is.null(cfg$bin_labels)) cfg$bin_labels else
    if (!is.null(cfg$bins)) .bin_labels(b, cfg$fmt, cfg$digits %||% 1) else
      lbl_fmt(type = "bin", cuts = b)
  cols <- pal((b[-1] + b[-length(b)]) / 2)
  ord <- if (isFALSE(cfg$higher_is_worse)) seq_along(labs) else rev(seq_along(labs))
  leaflet::addLegend(map, colors = cols[ord], labels = labs[ord], opacity = 0.9,
                     layerId = "risk_legend", title = title, position = "bottomright")
}

#' Plain-text standing line per county ("" when the value or the vintage is missing).
.migration_line <- function(d, vintage) {
  n <- nrow(d)
  if (is.null(vintage) || !"mig_dom_rate" %in% names(d)) return(rep("", n))
  v <- d$mig_dom_rate
  ifelse(is.na(v), "", sprintf("Net domestic migration: %s (%s)",
                               .risk_fmt(v, "per1k"), pep_year_label(vintage)))
}

.metro_line <- function(cbsa_name, momentum_pp, ref_month) {
  ok <- !is.na(cbsa_name) & !is.na(momentum_pp) & !is.na(ref_month)
  out <- rep("", length(ok))
  out[ok] <- sprintf("%s metro: %+.1f pp unemployment vs. a year ago (%s)",
                     cbsa_name[ok], momentum_pp[ok], format(ref_month[ok], "%b %Y"))
  out
}

#' Metro line for each row of `d` (county rows, or tract rows via their county_geoid).
.metro_line_rows <- function(d, county_layers = d, key = "GEOID") {
  cl <- .plain_df(county_layers)
  if (!all(c("cbsa_name", "metro_momentum_pp", "metro_ref_month") %in% names(cl))) {
    return(rep("", nrow(d)))
  }
  i <- match(.plain_df(d)[[key]], cl$GEOID)
  .metro_line(cl$cbsa_name[i], cl$metro_momentum_pp[i], cl$metro_ref_month[i])
}

.standing_html <- function(lines) {
  ifelse(.tt_has(lines), sprintf(
    '<div style="color:%s;font-size:11px;margin-top:4px;">%s</div>', .RISK_COL$muted,
    .tt_esc(lines)), "")
}

# Assistance tooltip helpers: a missing column is NULL (risk_drivers_vec reads it as NA).
.drv_text <- function(d, cols, fmt = "%") lapply(cols, function(col)
  if (is.null(d[[col]])) NULL else .risk_fmt(as.numeric(d[[col]]), fmt))
.drv_worse <- function(d, cols) lapply(cols, function(col)
  if (is.null(d[[col]])) NULL else as.numeric(d[[col]]) >= 50)

# J is the mean of the available percentiles, so it is an index, not itself a percentile.
# n/from (job_pressure_n, job_pressure_from) say which inputs it rests on; NA = both.
.job_pressure_text <- function(j, n = NA, from = NA) {
  how <- ifelse(n %in% 1L & from %in% "ahead", "Stress ahead only — BLS unavailable",
         ifelse(n %in% 1L & from %in% "now", "Stress now only — sector data unavailable",
                "mean of Stress now and Stress ahead percentiles"))
  sprintf("%.0f/100 (%s)", 100 * j, how)
}

.job_pressure_line <- function(j, n = NA, from = NA) {
  ifelse(is.na(j), "Job pressure: missing (BLS/FRED unavailable)",
         paste0("Job pressure: ", .job_pressure_text(j, n, from)))
}

#' TRUE where a value's margin of error is flagged: any of the layer's noisy columns
#' (default: its own), so assistance layers inherit the cushion inputs' flags.
.noisy_flags <- function(d, cfg) {
  cols <- if (is.null(cfg$noisy_cols)) paste0("noisy_", cfg$col) else cfg$noisy_cols
  d <- .plain_df(d)
  out <- rep(FALSE, nrow(d))
  for (col in intersect(cols, names(d))) out <- out | (d[[col]] %in% TRUE)
  out
}

# ── Credit lens and tax base tooltip pieces ──────────────────────────────────
.ordinal <- function(x) {
  r <- round(x)
  suf <- ifelse(r %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", "th", "th", "th", "th",
                                              "th", "th")[r %% 10 + 1])
  ifelse(is.na(x), NA_character_, paste0(r, suf))
}

# Column `col` of d as a vector (all NA when the frame lacks it).
.col_or_na <- function(d, col, na = NA_real_) if (is.null(d[[col]])) rep(na, nrow(d)) else d[[col]]

# " (metro est.)" etc.: the CFPB value is not the county's own.
.cfpb_note <- function(src) ifelse(src %in% "metro", " (metro est.)",
                                   ifelse(src %in% "state non-metro", " (state non-metro est.)",
                                          ifelse(src %in% "state", " (state est.)", "")))
.cfpb_where <- function(src) ifelse(src %in% "county", "county data",
                                    ifelse(src %in% "metro", "metro estimate",
                                           ifelse(src %in% "state non-metro",
                                                  "state non-metro estimate",
                                                  ifelse(src %in% "state", "state estimate",
                                                         NA_character_))))
.month_label <- function(x) format(as.Date(x), "%b %Y")

# Home price change in natural units, marked when only the state's value was available.
.hpi_text <- function(d) {
  yoy <- as.numeric(.col_or_na(d, "hpi_yoy"))
  ifelse(is.na(yoy), NA_character_, paste0(
    .risk_fmt(yoy, "pct_signed"),
    ifelse(.col_or_na(d, "hpi_source", NA_character_) %in% "state", " (state est.)", "")))
}

#' "Driven by" for the credit score: the 3 components with the highest percentile, in
#' natural units, red from the 50th percentile on. Order differs per county.
.credit_drivers <- function(d) {
  n <- nrow(d)
  g <- function(col) as.numeric(.col_or_na(d, col))
  src <- as.character(.col_or_na(d, "cfpb_source", NA_character_))
  vals <- cbind(
    ifelse(is.na(g("dq90")), NA_character_, paste0(sprintf("%.1f%%", g("dq90")), .cfpb_note(src))),
    .risk_fmt(g("consumer_del"), "%"),
    ifelse(is.na(g("cr_jobs")), NA_character_, paste(.ordinal(g("cr_jobs")), "pct")),
    .hpi_text(d))
  pct <- cbind(g("cr_mortgage"), g("cr_consumer"), g("cr_jobs"), g("cr_collateral"))
  pct[is.na(vals)] <- NA_real_
  labels <- c("Mortgages 90+ late", "Card & auto delinquent", "Job pressure", "Home prices YoY")
  ord <- matrix(unlist(lapply(seq_len(n), function(i) order(-pct[i, ], na.last = TRUE))),
                nrow = n, byrow = TRUE)
  at <- function(m, j) m[cbind(seq_len(n), ord[, j])]
  risk_drivers_vec(
    lapply(1:3, function(j) labels[ord[, j]]),
    lapply(1:3, function(j) at(vals, j)),
    lapply(1:3, function(j) at(pct, j) >= 50), n = n)
}

#' Standing line(s) per credit-lens county: what the score rests on and where each
#' value came from (CFPB level and month, FHFA quarter and level).
.credit_standing <- function(d, id) {
  n <- nrow(d)
  g <- function(col) as.numeric(.col_or_na(d, col))
  src <- as.character(.col_or_na(d, "cfpb_source", NA_character_))
  mon <- as.character(.col_or_na(d, "cfpb_month", NA_character_))
  switch(id,
    credit_score = {
      miss <- as.character(.col_or_na(d, "credit_missing", NA_character_))
      cn <- g("credit_n")
      ifelse(is.na(g("credit_score")) & !is.na(miss), paste("Missing:", miss),
             ifelse(!is.na(g("credit_score")) & cn %in% 3L & nzchar(miss),
                    sprintf("Based on 3 of 4 components (missing: %s)", miss), ""))
    },
    credit_mortgage = ifelse(is.na(g("dq90")), "", paste0(
      "CFPB ", .month_label(mon), " · ", .cfpb_where(src))),
    credit_collateral = {
      hs <- as.character(.col_or_na(d, "hpi_source", NA_character_))
      hq <- as.character(.col_or_na(d, "hpi_q", NA_character_))
      ifelse(is.na(g("hpi_yoy")), "No FHFA home price data for this county",
             paste0("FHFA ", hq, " · ", hs))
    },
    credit_consumer = ifelse(is.na(g("consumer_del")),
                             "No Urban Institute data for this county", ""),
    credit_jobs = .job_pressure_line(g("job_pressure"), .col_or_na(d, "job_pressure_n"),
                                     .col_or_na(d, "job_pressure_from")),
    rep("", n))
}

#' Drivers for the layers that list their inputs.
.credit_layer_drivers <- function(d, id, cfg) {
  g <- function(col) as.numeric(.col_or_na(d, col))
  pct <- function(col) as.numeric(.col_or_na(d, paste0("pct_", col)))
  switch(id,
    credit_score = .credit_drivers(d),
    credit_mortgage = risk_drivers_vec(
      c("90+ days late", "30–89 days late, 12-month change"),
      list(.risk_fmt(g("dq90"), "%"), .risk_fmt(g("dq3089_chg"), "pp", 2)),
      list(pct("dq90") >= 50, g("dq3089_chg") > 0)),
    credit_consumer = risk_drivers_vec(
      c("Card delinquent", "Auto delinquent"),
      list(.risk_fmt(g("card_del"), "%"), .risk_fmt(g("auto_del"), "%")),
      list(FALSE, FALSE), n = nrow(d)),
    tax_base = {
      vint <- attr(d, "pep_vintage")
      risk_drivers_vec(
        c("Net domestic migration",
          if (is.null(vint)) "Population change since 2020" else
            sprintf("Population change 2020–%s", substr(vint, 3, 4)),
          "Home prices YoY"),
        list(.risk_fmt(g("mig_dom_rate"), "per1k"), .risk_fmt(g("pop_chg_pct"), "pct_signed"),
             .hpi_text(d)),
        # Low migration, shrinking population and falling prices all weaken the base.
        list(pct("mig_dom_rate") < 50, pct("pop_chg_pct") < 50, pct("hpi_yoy") < 50),
        n = nrow(d))
    },
    "")
}

#' "worst 9%" / "better half": where a badness percentile (high = worse) sits nationally.
.bad_rank_text <- function(b) {
  ifelse(is.na(b), NA_character_,
         ifelse(b >= 50, sprintf("worst %d%%", as.integer(pmax(1, ceiling(100 - b)))),
                "better half"))
}

#' Hover pill for Now vs ahead: "Now: worst 9% · Ahead: better half". NA when either is.
.quad_rank_text <- function(d) {
  n <- .bad_rank_text(as.numeric(.col_or_na(d, "quad_now_pct")))
  a <- .bad_rank_text(as.numeric(.col_or_na(d, "quad_ahead_pct")))
  ifelse(is.na(n) | is.na(a), NA_character_, paste0("Now: ", n, " · Ahead: ", a))
}

#' "Manufacturing (−0.31), Retail (−0.12)": the top `n` sector contributions stored under
#' `prefix` (sa_drv = drags, sa_lift = lifts); NA where a county has none. with_pp = FALSE
#' gives the names only.
.sector_list <- function(d, prefix, n = 2, with_pp = TRUE) {
  nr <- nrow(d)
  if (nr == 0) return(character(0))
  col <- function(k, what) {
    x <- d[[sprintf("%s%d_%s", prefix, k, what)]]
    if (is.null(x)) rep(NA, nr) else x
  }
  parts <- lapply(seq_len(n), function(k) {
    lab <- as.character(col(k, "label")); pp <- suppressWarnings(as.numeric(col(k, "pp")))
    txt <- if (with_pp) sprintf("%s (%s)", lab, sub("-", "−", sprintf("%+.2f", pp), fixed = TRUE)) else lab
    ifelse(is.na(lab) | is.na(pp), NA_character_, txt)
  })
  m <- matrix(unlist(parts), nrow = nr)
  vapply(seq_len(nr), function(i) {
    r <- m[i, ][!is.na(m[i, ])]
    if (length(r)) paste(r, collapse = ", ") else NA_character_
  }, character(1))
}

#' Why each county sits in its Now vs ahead quadrant, in words: `headline` (what the
#' quadrant means for this county), `now` and `ahead` (the numbers behind each half, with
#' the sectors dragging or lifting the outlook) and `short` (one hover line).
#' NA where the quadrant is missing.
.quad_why <- function(d) {
  nr <- nrow(d)
  code <- as.numeric(.col_or_na(d, "now_vs_ahead"))
  mom <- as.numeric(.col_or_na(d, "momentum_pp"))
  rate <- as.numeric(.col_or_na(d, "rate_now"))
  gap <- as.numeric(.col_or_na(d, "stress_ahead_pp"))
  rank_now <- .bad_rank_text(as.numeric(.col_or_na(d, "quad_now_pct")))
  rank_ahead <- .bad_rank_text(as.numeric(.col_or_na(d, "quad_ahead_pct")))
  drag <- .sector_list(d, "sa_drv"); lift <- .sector_list(d, "sa_lift")
  drag_n <- .sector_list(d, "sa_drv", with_pp = FALSE)
  lift_n <- .sector_list(d, "sa_lift", with_pp = FALSE)
  pick <- function(a, b, pre_a, pre_b) ifelse(!is.na(a), paste0(pre_a, a),
                                               ifelse(!is.na(b), paste0(pre_b, b), ""))

  now <- ifelse(is.na(mom), NA_character_, sprintf(
    "unemployment %s vs a year ago%s (%s)", .risk_fmt(mom, "pp"),
    ifelse(is.na(rate), "", sprintf(", now %.1f%%", rate)),
    ifelse(is.na(rank_now), "", ifelse(rank_now == "better half", "better half of US counties",
                                       paste(rank_now, "of US counties")))))
  ahead <- ifelse(is.na(gap), NA_character_, paste0(
    sprintf("projected job growth %s vs the US (%s)", .risk_fmt(gap, "pp", 2),
            ifelse(is.na(rank_ahead), "", ifelse(rank_ahead == "better half", "better half of US counties",
                                                 paste(rank_ahead, "of US counties")))),
    pick(drag, lift, "; dragged down by ", "; lifted by ")))
  headline <- c(
    "Unemployment is holding up and the industry mix does not point to weak job growth.",
    paste("Unemployment is trending worse than in most counties, but the industry mix does not",
          "point to weak job growth, so this county may be recovering."),
    paste("Unemployment is holding up, but the industry mix points to weaker job growth than in",
          "most counties, so stress may build."),
    paste("Unemployment is trending worse than in most counties and the industry mix points to",
          "weak job growth."))[code]
  short <- ifelse(is.na(code), NA_character_, paste0(
    ifelse(is.na(mom), "", paste0("Unemp. ", .risk_fmt(mom, "pp"))),
    ifelse(is.na(mom), "", " · "),
    ifelse(code >= 3, pick(drag_n, NA, "drag: ", ""), pick(lift_n, NA, "lift: ", ""))))
  short <- sub(" · $", "", short)
  short[!nzchar(short)] <- NA_character_
  data.frame(headline = headline, now = now, ahead = ahead, short = short,
             stringsAsFactors = FALSE)[seq_len(nr), , drop = FALSE]
}

#' detail = FALSE gives the compact hover label (the map's default); TRUE the full block.
.county_tooltips <- function(d, id, cfg, asof, status = "ok", detail = TRUE) {
  if (any(status %in% c("loading", "not_loaded"))) {
    if (!detail) return(risk_tooltips_compact_vec(d$name, "Loading…"))
    return(risk_tooltips_vec(title = d$name, value_text = "Loading…",
                             footer = paste(cfg$source, "— loading")))
  }
  vtxt <- .risk_fmt(d[[cfg$col]], cfg$fmt, cfg$digits %||% 1)
  if (id == "stress_now") {
    has_flag <- !is.na(vtxt) & .flag_shown(d$flag)
    vtxt[has_flag] <- paste0(vtxt[has_flag], " · ", .flag_text(d$flag[has_flag]))
  }
  pct <- d[[paste0("pct_", cfg$col)]]
  zips <- if ("zip_list" %in% names(d)) zip_label(d$zip_list, d$zip_n) else NA_character_
  # The quadrant has no rank of its own: the pill gives where the county sits on each measure.
  rk <- if (id == "now_vs_ahead") .quad_rank_text(d) else
    rank_label(pct, cfg$rank_frame %||% "nationally")
  rk_bad <- if (id == "now_vs_ahead") as.numeric(d[[cfg$col]]) >= 2 & !is.na(d[[cfg$col]]) else
    .rank_bad(pct, cfg, d[[cfg$col]])
  why <- if (id == "now_vs_ahead") .quad_why(d) else NULL
  if (!detail) return(risk_tooltips_compact_vec(d$name, vtxt, rk, rk_bad, zips = zips,
                                                note = if (is.null(why)) NA else why$short))
  subs <- ifelse(is.na(d$cbsa_name), NA_character_, paste(d$cbsa_name, d$cbsa_type))
  footer <- if (is.na(asof)) paste(cfg$source, "— unavailable") else
    paste0(cfg$source, ", ", asof)
  drivers <- if (identical(cfg$lens, "migration")) {
    # The cumulative layer's headline spans 2020 on, but these are the latest year only.
    yr <- if (id == "mig_dom_cum" && !is.null(attr(d, "pep_vintage")))
      sprintf(" (%s)", pep_year_label(attr(d, "pep_vintage"))) else NULL
    risk_drivers_vec(
      if (is.null(yr)) c("Domestic", "International", "Natural change (births − deaths)") else
        paste0(c("Domestic", "International", "Natural change"), yr),
      lapply(c("mig_dom_rate", "mig_intl_rate", "nat_chg_rate"),
             function(col) .risk_fmt(d[[col]], "per1k")),
      rep(list(FALSE), 3))
  } else if (id == "stress_ahead") {
    # Drags when a county has any (red); otherwise its biggest lifts (neutral colour).
    has_drag <- !is.na(d$sa_drv1_label)
    pick <- function(k, what) ifelse(has_drag, d[[sprintf("sa_drv%d_%s", k, what)]],
                                     d[[sprintf("sa_lift%d_%s", k, what)]])
    risk_drivers_vec(
      lapply(1:3, function(k) pick(k, "label")),
      lapply(1:3, function(k) {
        v <- pick(k, "pp")
        ifelse(is.na(v), NA_character_, sprintf("%+.2f pp", v))
      }),
      lapply(1:3, function(k) has_drag & pick(k, "pp") < 0))
  } else if (id == "now_vs_ahead") {
    cut <- RISK_CFG$quad_bad_pct
    risk_drivers_vec(
      c("Stress now (unemployment vs a year ago)", "Stress ahead (job growth vs US)"),
      list(.risk_fmt(.col_or_na(d, "momentum_pp"), "pp"),
           .risk_fmt(.col_or_na(d, "stress_ahead_pp"), "pp", 2)),
      list(as.numeric(d$quad_now_pct) >= cut, as.numeric(d$quad_ahead_pct) >= cut),
      n = nrow(d))
  } else if (identical(cfg$lens, "assistance")) {
    risk_drivers_vec(
      c("Workers in weakening sectors", "Renters cost-burdened", "Poverty rate"),
      .drv_text(d, c("weak_share", "rent_burden_rate", "poverty_rate")),
      .drv_worse(d, c("pct_weak_share", "pct_rent_burden_rate", "pct_poverty_rate")))
  } else if (identical(cfg$lens, "credit") || id == "tax_base") {
    .credit_layer_drivers(d, id, cfg)
  } else ""
  na_col <- function(col) if (is.null(d[[col]])) rep(NA, nrow(d)) else d[[col]]
  job_line <- if (identical(cfg$lens, "assistance")) .standing_html(.job_pressure_line(
    as.numeric(na_col("job_pressure")), na_col("job_pressure_n"),
    na_col("job_pressure_from"))) else ""
  credit_line <- if (identical(cfg$lens, "credit")) .standing_html(.credit_standing(d, id)) else ""
  risk_tooltips_vec(title = d$name, subtitle = subs, value_text = vtxt,
                    rank_text = rk,
                    footer = footer, drivers_html = drivers, zips = zips,
                    rank_bad = rk_bad,
                    standing = paste0(job_line, credit_line,
                                      if (!is.null(why)) .standing_html(why$headline) else "",
                                      .standing_html(.migration_line(d, attr(d, "pep_vintage"))),
                                      .standing_html(.metro_line_rows(d))))
}

#' Ranks always read in value terms ("Top" = highest value). The pill is red only
#' on the bad side: sign for pp layers, the median otherwise; never for a neutral
#' (NA higher_is_worse) layer.
.rank_bad <- function(pct, cfg, vals = NULL) {
  hiw <- cfg$higher_is_worse %||% NA        # missing = neutral
  if (is.na(hiw)) return(rep(FALSE, length(pct)))
  bad <- if (cfg$fmt %in% c("pp", "pct_signed") && !is.null(vals)) {
    vals <- round(vals, cfg$digits %||% 1)       # what is displayed decides the colour
    if (isTRUE(hiw)) vals > 0 else vals < 0
  } else if (isTRUE(hiw)) pct >= 50 else pct < 50
  bad %in% TRUE
}

#' Open-ended bin labels from breaks; numbers only (no unit), signed, as
#' "below −10", "−10 to −5", ..., "above +10".
.bin_labels <- function(bins, fmt, digits = 1) {
  num <- function(x) {
    out <- if (fmt %in% c("pp", "pct_signed")) sprintf(paste0("%+.", digits, "f"), x) else
      ifelse(x == round(x), sprintf("%+d", as.integer(round(x))), sprintf("%+.1f", x))
    out <- ifelse(x == 0, "0", out)
    sub("-", "−", out, fixed = TRUE)
  }
  inner <- num(bins[-c(1, length(bins))])
  k <- length(inner)
  c(paste("below", inner[1]),
    if (k > 1) paste(inner[-k], "to", inner[-1]),
    paste("above", inner[k]))
}

#' The layer's config with the loading text that fits why it is loading: the tax base also
#' waits on Census population estimates, which are not the credit sources.
risk_layer_cfg <- function(layer_id, county_layers) {
  cfg <- RISK_LAYERS[[layer_id]]
  ld <- attr(county_layers, "loading")
  if (identical(layer_id, "tax_base") && !"credit" %in% ld && "migration" %in% ld)
    cfg$loading_text <- RISK_LAYERS$mig_dom$loading_text
  cfg
}

#' Credit lens layers, and the tax base (which needs the credit sources for home prices).
.is_credit_layer <- function(layer_id) {
  identical(RISK_LAYERS[[layer_id]]$lens, "credit") || identical(layer_id, "tax_base")
}

#' Swap only the legend for a "loading" one; the polygons already on screen stay. Used for
#' the first Credit open, where resending every county polygon would delay the fetch.
#' The old polygons stay, so dim them and hide their tooltips until the repaint
#' (.add_county_polygons removes the style control).
risk_loading_legend <- function(map, layer_id, county_layers = NULL) {
  cfg <- if (is.null(county_layers)) RISK_LAYERS[[layer_id]] else
    risk_layer_cfg(layer_id, county_layers)
  cfg$loading_text <- paste(cfg$loading_text %||% "loading…", "— showing previous layer until loaded")
  map |>
    leaflet::removeControl("risk_legend") |>
    leaflet::addControl(html = .legend_title(cfg, "loading", NA), layerId = "risk_legend",
                        position = "bottomright", className = "info legend") |>
    leaflet::addControl(html = paste0("<style>#risk_map .leaflet-overlay-pane{opacity:.35}",
                                      "#risk_map .leaflet-tooltip-pane{display:none}</style>"),
                        layerId = "risk_loading_dim", className = "")
}

.legend_title <- function(cfg, status, asof, scope = NULL) {
  head <- htmltools::htmlEscape(cfg$label)
  sub <- if (identical(status, "unavailable")) "unavailable" else
    if (any(status %in% c("loading", "not_loaded"))) {
      cfg$loading_text %||% "loading…"
    } else
      paste0(if (!is.null(scope)) paste0(scope, " · "), cfg$source, ", ", asof,
             if (identical(status, "stale")) " (stale)",
             if (identical(status, "partial"))
               paste0(" (", cfg$partial_text %||% "partial: some counties missing, retrying later", ")"))
  # Long titles (credit score, tax base) wrap instead of running ~600 px wide.
  paste0("<div style='max-width:260px;white-space:normal;'>", head,
         "<br><small style='font-weight:400;'>", htmltools::htmlEscape(sub), "</small></div>")
}

.legend_format <- function(cfg) {
  switch(cfg$fmt,
    "$"  = leaflet::labelFormat(prefix = "$", big.mark = ",", digits = 0),
    "pp" = leaflet::labelFormat(suffix = " pp", digits = cfg$digits %||% 1),
    "per1k" = leaflet::labelFormat(suffix = " per 1,000", digits = 1),
    "score" = leaflet::labelFormat(digits = 0),
    "n"  = leaflet::labelFormat(big.mark = ",", digits = 0),
    leaflet::labelFormat(suffix = "%", digits = 1))
}

#' Hover labels are compact; options(risk.hover_detail = TRUE) restores the full block
#' (the tests read their drivers and sources from it).
.hover_detail <- function() isTRUE(getOption("risk.hover_detail", FALSE))

#' Hover labels sit beside the cursor, not under it, and carry no box of their own.
.hover_label_options <- function() {
  leaflet::labelOptions(
    style = list(background = "transparent", border = "none", padding = "0",
                 "box-shadow" = "none"),
    direction = "auto", offset = c(14, 0), sticky = TRUE)
}

#' The state view of the map rides on the layers object: attr "view_state" (a state FIPS or
#' "" for the whole US) and attr "view_rescale" (colours span the state's own range).
risk_view <- function(county_layers, state = NULL, rescale = FALSE) {
  attr(county_layers, "view_state") <- if (is.null(state) || is.na(state)) "" else as.character(state)
  attr(county_layers, "view_rescale") <- isTRUE(rescale)
  county_layers
}

.view_state <- function(county_layers) {
  st <- attr(county_layers, "view_state")
  if (is.null(st) || length(st) != 1 || is.na(st) || !nzchar(st)) NULL else st
}

# Values outside `ref`'s range take its nearest end (the backdrop of a rescaled state view).
.clamp_to <- function(x, ref) {
  r <- range(ref, na.rm = TRUE)
  if (!all(is.finite(r))) return(x)
  pmin(pmax(x, r[1]), r[2])
}

#' Counties are drawn from the layer's values; with a state selected, only that state's
#' counties are interactive (ranked within the state), the rest are a faint backdrop.
.add_county_polygons <- function(map, county_layers, layer_id, fill_opacity = RISK_FILL_OPACITY) {
  cfg <- RISK_LAYERS[[layer_id]]
  vals <- as.numeric(county_layers[[cfg$col]])
  st <- .view_state(county_layers)
  in_st <- if (is.null(st)) rep(TRUE, length(vals)) else
    as.character(county_layers$state_fips) == st
  st_name <- if (is.null(st)) NULL else county_layers$state_name[which(in_st)[1]]
  rescale <- !is.null(st) && isTRUE(attr(county_layers, "view_rescale")) &&
    !isTRUE(cfg$categorical) && any(!is.na(vals[in_st]))

  # Colours: national scale, unless asked to span only the state's own values.
  pal_cfg <- cfg
  if (rescale) { pal_cfg$bins <- NULL; pal_cfg$bin_labels <- NULL }
  pal_vals <- if (rescale) vals[in_st] else vals
  pal <- .risk_palette(pal_cfg, pal_vals)

  asof <- attr(county_layers, "asof")[[layer_id]]
  status <- attr(county_layers, "layer_status")[[layer_id]]
  lbl_fmt <- .legend_format(cfg)
  scope <- if (is.null(st)) NULL else if (rescale) paste0(st_name, ", colours scaled to the state") else st_name
  title <- .legend_title(risk_layer_cfg(layer_id, county_layers), status, asof, scope)
  add_legend <- function(m) .risk_legend(m, pal, pal_vals, pal_cfg, lbl_fmt, title)

  # Hover ranks are within the state when one is selected.
  tip_d <- county_layers
  tip_cfg <- cfg
  if (!is.null(st)) {
    pcol <- paste0("pct_", cfg$col)
    tip_d[[pcol]][in_st] <- pct_rank(vals[in_st])
    tip_cfg$rank_frame <- paste("in", st_name)
  }
  tips <- .county_tooltips(tip_d, layer_id, tip_cfg, asof, status, detail = .hover_detail())

  map <- map |>
    leaflet::clearGroup("counties") |>
    leaflet::clearGroup("counties_bg") |>
    leaflet::removeControl("risk_legend") |>
    leaflet::removeControl("risk_loading_dim")
  if (!is.null(st) && any(!in_st)) {
    bg <- county_layers[!in_st, ]
    map <- leaflet::addPolygons(
      map, data = bg, group = "counties_bg", fillColor = pal(.clamp_to(vals[!in_st], pal_vals)),
      fillOpacity = 0.12, color = "#0f1117", weight = 0.2, smoothFactor = 0.5,
      options = leaflet::pathOptions(interactive = FALSE))
  }
  fg <- county_layers[in_st, ]
  leaflet::addPolygons(
    map, data = fg, layerId = fg$GEOID, group = "counties",
    fillColor = pal(vals[in_st]), fillOpacity = fill_opacity,
    color = "#0f1117", weight = 0.4, smoothFactor = 0.5,
    label = tips[in_st], labelOptions = .hover_label_options(),
    highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2,
                                                 bringToFront = FALSE)
  ) |>
    add_legend()
}

# ── Tract drill-down layer ───────────────────────────────────────────────────
# Frames smaller than this rank too coarsely to read as percentiles.
.SMALL_FRAME <- 5

.small_frame_text <- function(n) sprintf("Small comparison group (n = %d) — ranks are coarse.", as.integer(n))

#' Standing lines for an assistance tract whose value is NA: what is missing. The score
#' needs the industry mix and job pressure; the other layers only the industry mix.
.assist_missing_html <- function(t, cfg) {
  n <- nrow(t)
  if (!identical(cfg$lens, "assistance")) return(rep("", n))
  na_col <- function(col) if (is.null(t[[col]])) rep(NA_real_, n) else as.numeric(t[[col]])
  no_val <- is.na(as.numeric(t[[cfg$col]]))
  mix <- no_val & is.na(na_col("assist_exposure"))
  jp <- if (identical(cfg$col, "assist_score")) no_val & is.na(na_col("job_pressure")) else
    rep(FALSE, n)
  paste0(.standing_html(ifelse(jp, "Job pressure: missing (BLS/FRED unavailable)", "")),
         .standing_html(ifelse(mix, "Industry mix: unavailable", "")))
}

.tract_tooltips <- function(t, cfg, asof, noisy, county_layers, status = "ok", detail = TRUE) {
  vtxt <- .risk_fmt(as.numeric(t[[cfg$col]]), cfg$fmt, cfg$digits %||% 1)
  lm_pct <- t[[paste0("lm_pct_", cfg$col)]]
  lm <- rank_label(lm_pct, t$frame_label)
  zips <- if ("zip_list" %in% names(t)) zip_label(t$zip_list, t$zip_n, t$zip_nearest) else
    NA_character_
  if (!detail) {
    return(risk_tooltips_compact_vec(
      paste0(t$tract_name, ifelse(noisy, " · high margin of error", "")), vtxt, lm,
      .rank_bad(lm_pct, cfg, as.numeric(t[[cfg$col]])), zips = zips))
  }
  st <- rank_label(t[[paste0("st_pct_", cfg$col)]], t$state_label)
  footer <- paste0(
    if (identical(status, "unavailable") || is.na(asof)) paste(cfg$source, "— unavailable") else
      paste0(cfg$source, ", ", asof),
    ifelse(noisy, " · High margin of error", ""))
  small <- if (nrow(t) < .SMALL_FRAME) .standing_html(rep(.small_frame_text(nrow(t)), nrow(t))) else ""
  drivers <- if (identical(cfg$lens, "assistance")) {
    ins <- c("weak_share", "low_wage_share", "rent_burden_rate")
    risk_drivers_vec(
      c("Workers in weakening sectors", "Low-wage workers (≤$1,250/mo)",
        "Renters cost-burdened", "≈ Exposure-weighted workers"),
      c(.drv_text(t, ins), .drv_text(t, "assist_workers", "n")),
      c(.drv_worse(t, paste0("lm_pct_", ins)), list(FALSE)),   # above the median is red
      n = nrow(t), max_rows = 4)
  } else ""
  risk_tooltips_vec(title = t$tract_name, value_text = vtxt, rank_text = lm,
                    rank_sub = st, footer = footer, drivers_html = drivers,
                    rank_bad = .rank_bad(lm_pct, cfg, as.numeric(t[[cfg$col]])), zips = zips,
                    standing = paste0(
                      .assist_missing_html(t, cfg),
                      .standing_html(.metro_line_rows(t, county_layers, key = "county_geoid")),
                      small))
}

#' Tract-layer status and as-of. Assistance tracts do not depend on county assistance:
#' the as-of is built from the tracts' own job sources, the ACS span and the CES month,
#' and is "unavailable" only without sector weights. Other layers use the county as-of.
.tract_status_asof <- function(tracts, cfg, layer_id, county_layers) {
  if (!identical(cfg$lens, "assistance")) {
    return(list(status = "ok", asof = attr(county_layers, "asof")[[layer_id]]))
  }
  weights <- attr(county_layers, "sector_weights")
  if (is.null(weights)) return(list(status = "unavailable", asof = NA_character_))
  src <- if ("assist_source" %in% names(tracts)) as.character(tracts$assist_source) else character()
  src <- unique(src[!is.na(src)])
  lodes <- sub("^LODES\\s*", "", src[startsWith(src, "LODES")])
  lodes <- lodes[nzchar(lodes)]
  yr <- attr(county_layers, "acs_year")
  ces <- attr(weights, "ref_month")
  parts <- c(if (any(startsWith(src, "LODES")))
               paste0("LODES", if (length(lodes)) paste0(" ", paste(sort(lodes), collapse = "/"))),
             if (!is.null(yr)) sprintf("ACS %d–%d", yr - 4, yr))
  if (!length(parts)) return(list(status = "unavailable", asof = NA_character_))
  asof <- paste(parts, collapse = " / ")
  if (length(ces) == 1 && !is.na(ces)) asof <- paste0(asof, format(ces, " · CES %b %Y"))
  st <- if (isTRUE(attr(weights, "stale"))) "stale" else
    if (isTRUE(attr(weights, "partial"))) "partial" else "ok"
  list(status = st, asof = asof)
}

add_tract_legend <- function(map, pal, vals, cfg, lbl_fmt, title) {
  .risk_legend(map, pal, vals, cfg, lbl_fmt, title)
}

#' Tract layer drawn when the selected layer is county-only (Stress now, Credit, Migration,
#' Tax base): the assistance score where tracts have it, else ACS poverty, which always loads.
.tract_fallback_id <- function(tracts) {
  if ("assist_score" %in% names(tracts) && any(!is.na(tracts$assist_score))) "assist_score"
  else "poverty_rate"
}

#' Dim counties and draw the frame's tracts. A county-only layer has no tract values, so the
#' tracts are coloured by a tract-level fallback layer (named in the legend) while the
#' counties underneath stay on the selected layer.
add_tract_layer <- function(map, tracts, layer_id, county_layers, fit = TRUE) {
  cfg <- RISK_LAYERS[[layer_id]]
  map <- leaflet::clearGroup(map, "tracts")
  have <- !is.null(tracts) && nrow(tracts) > 0
  fit_to <- function(m) {
    if (!fit || !have) return(m)
    bb <- as.numeric(sf::st_bbox(tracts))
    leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
  }
  if (!have) {
    return(fit_to(.add_county_polygons(map, county_layers, layer_id, RISK_FILL_OPACITY)))
  }
  county_only <- !"tract" %in% cfg$levels
  tract_id <- if (county_only) .tract_fallback_id(tracts) else layer_id
  if (county_only) cfg <- RISK_LAYERS[[tract_id]]
  sa <- .tract_status_asof(tracts, cfg, tract_id, county_layers)
  vals <- as.numeric(tracts[[cfg$col]])
  pal <- .risk_palette(cfg, vals)
  noisy <- .noisy_flags(tracts, cfg)
  lbl_fmt <- .legend_format(cfg)

  .add_county_polygons(map, county_layers, layer_id, RISK_DIM_OPACITY) |>
    leaflet::addPolygons(
      data = tracts, layerId = tracts$GEOID, group = "tracts",
      fillColor = pal(vals), fillOpacity = RISK_TRACT_OPACITY,
      color = ifelse(noisy, .RISK_COL$warn, "#0f1117"),
      weight = ifelse(noisy, 1.5, 0.4), dashArray = ifelse(noisy, "4", NA),
      label = .tract_tooltips(tracts, cfg, sa$asof, noisy, county_layers, sa$status,
                              detail = .hover_detail()),
      labelOptions = .hover_label_options(),
      highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2)
    ) |>
    leaflet::removeControl("risk_legend") |>
    add_tract_legend(pal, vals, cfg, lbl_fmt, .legend_title(
      cfg, sa$status, sa$asof,
      scope = paste0(sprintf("tracts %s (n = %d)", tracts$frame_label[1], nrow(tracts)),
                     if (county_only)
                       " · county-only layer, so tracts show this one instead" else ""))) |>
    fit_to()
}

#' Zoom to the given counties (the clicked county, or a CBSA's counties), so the
#' view does not depend on the tract frame or on tracts loading at all.
fit_to_counties <- function(map, county_layers, geoids) {
  sel <- county_layers[county_layers$GEOID %in% geoids, ]
  if (nrow(sel) == 0) return(map)
  bb <- as.numeric(sf::st_bbox(sel))
  leaflet::fitBounds(map, bb[1], bb[2], bb[3], bb[4])
}

#' Zoom to a state (FIPS) or back to the US when `fips` is empty. Alaska's Aleutians cross
#' the antimeridian, which would make its bounding box span the globe, so it is fixed.
fit_to_state <- function(map, county_layers, fips) {
  if (is.null(fips) || is.na(fips) || !nzchar(fips)) {
    return(leaflet::setView(map, lng = -96, lat = 38, zoom = 4))
  }
  if (identical(fips, "02")) return(leaflet::setView(map, lng = -150, lat = 63.5, zoom = 4))
  fit_to_counties(map, county_layers, county_layers$GEOID[county_layers$state_fips == fips])
}

reset_to_us <- function(map, county_layers, layer_id) {
  leaflet::clearGroup(map, "tracts") |>
    .add_county_polygons(county_layers, layer_id, RISK_FILL_OPACITY) |>
    fit_to_state(county_layers, .view_state(county_layers))
}

#' Migration blocks for the county side panel: one bar per year (divs, not rows) and
#' the population change since 2020. NULL when the area has no PEP values.
.migration_panel <- function(a, vintage) {
  tg <- htmltools::tags; c <- .RISK_COL
  yrs <- 2021:vintage
  v <- vapply(yrs, function(y) {
    col <- paste0("mig_dom_rate_", y)
    if (col %in% names(a)) as.numeric(a[[col]]) else NA_real_
  }, numeric(1))
  if (all(is.na(v))) return(NULL)
  top <- max(abs(v), na.rm = TRUE)
  bars <- lapply(seq_along(yrs), function(i) {
    w <- if (is.na(v[i]) || top == 0) 0 else max(2, 100 * abs(v[i]) / top)
    tg$div(
      style = "display:flex;align-items:center;gap:8px;margin:2px 0;",
      tg$span(style = sprintf("width:52px;color:%s;", c$muted), pep_year_label(yrs[i])),
      tg$div(style = "flex:1;", tg$div(style = sprintf(
        "height:10px;border-radius:2px;width:%.0f%%;background:%s;", w,
        if (!is.na(v[i]) && v[i] < 0) "#f1a340" else "#998ec3"))),
      tg$span(style = "width:90px;text-align:right;font-weight:600;",
              if (is.na(v[i])) "—" else .risk_fmt(v[i], "per1k")))
  })
  pc <- as.numeric(a$pop_chg_pct)
  tg$div(
    style = sprintf("margin-top:10px;border-top:1px solid %s;padding-top:8px;", c$border),
    tg$div(style = sprintf("color:%s;margin-bottom:4px;", c$muted),
           sprintf("Net domestic migration per 1,000, %s to %s",
                   pep_year_label(min(yrs)), pep_year_label(vintage))),
    bars,
    if (length(pc) == 1 && !is.na(pc)) tg$div(
      style = "margin-top:6px;",
      sprintf("Population change 2020–%s: %+.1f%%", substr(vintage, 3, 4), pc)),
    tg$div(style = sprintf("color:%s;", c$muted), sprintf("Census PEP V%d", vintage)))
}

#' Top 5 resident industries (share of employed residents, with the national payroll
#' momentum of each sector) and the national temp-help note. Divs, not rows. NULL when
#' there is nothing to show.
.industry_panel <- function(a, weights, acs_year = NULL) {
  if (is.null(weights)) return(NULL)
  tg <- htmltools::tags; c <- .RISK_COL
  sh <- vapply(paste0("share_", RISK_SECTORS$group), function(col)
    if (col %in% names(a)) as.numeric(a[[col]]) else NA_real_, numeric(1))
  top <- utils::head(order(sh, decreasing = TRUE), 5)
  top <- top[!is.na(sh[top])]
  m <- weights$m_pct[match(RISK_SECTORS$group, weights$group)]
  signed <- function(x) sub("-", "−", sprintf("%+.1f", x), fixed = TRUE)
  tp <- attr(weights, "temphelp_m_pct")
  if (!length(top) && (is.null(tp) || is.na(tp))) return(NULL)
  tg$div(
    style = sprintf("margin-top:10px;border-top:1px solid %s;padding-top:8px;", c$border),
    if (length(top)) tg$div(
      tg$div(style = sprintf("color:%s;margin-bottom:4px;", c$muted), "Top resident industries"),
      lapply(top, function(i) tg$div(
        style = "margin:2px 0;",
        tg$b(RISK_SECTORS$label[i]), sprintf(" %.1f%%", 100 * sh[i]),
        if (!is.na(m[i])) tg$span(style = sprintf("color:%s;", c$muted),
                                  sprintf(" · US payrolls %s%%/yr", signed(m[i]))))),
      if (!is.null(acs_year)) tg$div(
        style = sprintf("color:%s;margin-top:4px;", c$muted),
        sprintf(paste0("Industry mix: ACS %d–%d 5-year (includes pandemic years; ",
                       "mix moves slowly)."), acs_year - 4, acs_year))),
    if (!is.null(tp) && !is.na(tp)) tg$div(
      style = sprintf("color:%s;margin-top:6px;", c$muted),
      sprintf(paste0("Temporary help payrolls (US): %+.1f%% annualized over 6 months — ",
                     "a leading indicator, not in the score."), tp)))
}

#' "Why this quadrant" block for the county side panel: what the quadrant means for this
#' county, then the numbers behind the Now and Ahead halves. NULL without a quadrant.
.quad_panel <- function(a) {
  a <- .plain_df(a)[1, , drop = FALSE]
  if (is.na(suppressWarnings(as.numeric(.col_or_na(a, "now_vs_ahead"))))) return(NULL)
  w <- .quad_why(a)
  tg <- htmltools::tags; c <- .RISK_COL
  line <- function(label, txt) if (is.na(txt)) NULL else tg$div(
    style = "margin:3px 0;", tg$b(label), " ", txt)
  tg$div(
    style = sprintf("margin-top:10px;border-top:1px solid %s;padding-top:8px;", c$border),
    tg$div(style = sprintf("color:%s;margin-bottom:4px;", c$muted),
           paste("Why:", QUAD_LABELS[as.numeric(a$now_vs_ahead)])),
    tg$div(style = "margin:3px 0;", w$headline),
    line("Now:", w$now), line("Ahead:", w$ahead),
    tg$div(style = sprintf("color:%s;margin-top:4px;", c$muted), sprintf(paste0(
      "A county is \"bad\" on a measure when it is in the worst third of US counties. ",
      "Sector drags and lifts are its industry mix times each sector's recent US payroll ",
      "momentum, against the US mix; they describe exposure, not a forecast."))))
}

#' Assistance notes for the side panel (divs, not rows): industry-mix source, the
#' exposure-weighted headcount with its caveat, rent-burdened households (tract only)
#' and county job pressure. NULL when the area has no assistance values.
#' `sources`: state FIPS -> source text of the loaded tract frame (fallback for a tract
#' missing from the job table; per-state list when LODES and ACS are mixed).
.assist_panel <- function(a, level, county_layers, st_names, sources = character(0)) {
  get <- function(col) if (col %in% names(a)) a[[col]][1] else NA
  workers <- as.numeric(get("assist_workers"))
  no_mix <- if (level == "tract") unique(names(sources)[is.na(sources)]) else character(0)
  if (is.na(workers) && is.na(as.numeric(get("assist_score"))) && !length(no_mix)) return(NULL)
  tg <- htmltools::tags; c <- .RISK_COL
  line <- function(txt, color = c$text) tg$div(style = sprintf("color:%s;margin:2px 0;", color), txt)
  src <- if (level == "tract") {
    s <- as.character(get("assist_source"))
    if (is.na(s)) s <- unname(sources[substr(get("GEOID"), 1, 2)])
    if (length(s) == 0) NA_character_ else s
  } else {
    y <- attr(county_layers, "acs_year")
    if (is.null(y)) NA_character_ else sprintf("ACS %d–%d (county)", y - 4, y)
  }
  have <- sources[!is.na(sources)]
  mixed <- level == "tract" && length(unique(have)) > 1
  hh <- as.numeric(get("rent_burdened_hh"))
  j <- as.numeric(get("job_pressure"))
  jn <- get("job_pressure_n"); jf <- get("job_pressure_from")
  tg$div(
    style = sprintf("margin-top:10px;border-top:1px solid %s;padding-top:8px;", c$border),
    tg$div(style = sprintf("color:%s;margin-bottom:4px;", c$muted), "Assistance score inputs"),
    if (!is.na(src)) line(paste("Industry mix source:", src)),
    if (length(no_mix)) line(color = c$warn, paste(
      "Industry mix unavailable for:", paste(unname(st_names[no_mix]), collapse = ", "))),
    if (mixed) line(color = c$warn, paste0(
      "Industry mix source by state: ",
      paste(sprintf("%s — %s%s", unname(st_names[names(have)]), have,
                    ifelse(startsWith(have, "ACS"), " (cushion uses 2 inputs there)", "")),
            collapse = "; "))),
    if (!is.na(workers)) line(sprintf(
      "Exposure-weighted workers: %s (index headcount, not a layoff forecast)",
      .risk_fmt(workers, "n"))),
    if (level == "tract" && !is.na(hh)) line(sprintf(
      "Rent-burdened renter households: %s (ACS estimate)", .risk_fmt(hh, "n"))),
    line(if (is.na(j)) "County job pressure: missing (BLS/FRED unavailable)" else
      paste0("County job pressure: ", .job_pressure_text(j, jn, jf))))
}

#' Credit components block for the county side panel (divs, not rows): each component
#' with its value, national risk percentile, source (incl. the CFPB and FHFA level the
#' county's value came from) and as-of, then the score. NULL until credit data has loaded.
.credit_panel <- function(a, county_layers) {
  info <- attr(county_layers, "credit_info")
  if (is.null(info) || !isTRUE(info$loaded) || !any(info$has)) return(NULL)
  tg <- htmltools::tags; c <- .RISK_COL
  get <- function(col) if (col %in% names(a)) a[[col]][1] else NA
  num <- function(col) suppressWarnings(as.numeric(get(col)))
  asof <- attr(county_layers, "asof")
  pctl <- function(p) if (is.na(p)) "no risk percentile" else
    sprintf("risk percentile %.0f", p)
  line <- function(name, value, p, source) tg$div(
    style = "margin:3px 0;",
    tg$b(name), " ", value,
    tg$span(style = sprintf("color:%s;", c$muted), " · ", pctl(p), " · ", source))

  src <- as.character(get("cfpb_source"))
  dq <- num("dq90"); chg <- num("dq3089_chg")
  mortgage <- line("Mortgage delinquency",
    if (is.na(dq)) "—" else sprintf("%.1f%% 90+ days late%s", dq, if (is.na(chg)) "" else
      sprintf(" (30–89 days: %+.2f pp over 12 months)", chg)),
    num("cr_mortgage"),
    if (is.na(src)) "no CFPB data for this county" else
      sprintf("CFPB %s, %s", .cfpb_where(src), .month_label(as.character(get("cfpb_month")))))

  card <- num("card_del"); auto <- num("auto_del")
  consumer <- line("Card & auto delinquency",
    if (is.na(card) && is.na(auto)) "—" else paste(c(
      if (!is.na(card)) sprintf("card %.1f%%", card),
      if (!is.na(auto)) sprintf("auto %.1f%%", auto)), collapse = ", "),
    num("cr_consumer"),
    if (!info$has[["urban"]]) "Urban Institute data unavailable" else
      if (is.na(card) && is.na(auto)) "no Urban Institute data for this county" else
        sprintf("Urban Institute (Debt in America), %s", info$urban_release))

  jobs <- line("Job pressure", if (is.na(num("job_pressure"))) "—" else
                 sprintf("%.0f/100", 100 * num("job_pressure")), num("cr_jobs"),
               if (is.na(asof[["credit_jobs"]])) "BLS unemployment and payrolls unavailable" else
                 paste("BLS", asof[["credit_jobs"]]))

  hs <- as.character(get("hpi_source")); hq <- as.character(get("hpi_q"))
  hpi <- num("hpi_yoy")
  collateral <- line("Home prices (collateral)",
    if (is.na(hpi)) "—" else sprintf("%s year over year", .risk_fmt(hpi, "pct_signed")),
    num("cr_collateral"),
    if (is.na(hs)) "no FHFA data for this county" else sprintf("FHFA %s, %s", hs, hq))

  score <- num("credit_score"); n <- num("credit_n"); miss <- as.character(get("credit_missing"))
  min_n <- RISK_CFG$credit_min_components
  tg$div(
    style = sprintf("margin-top:10px;border-top:1px solid %s;padding-top:8px;", c$border),
    tg$div(style = sprintf("color:%s;margin-bottom:4px;", c$muted), "Credit components"),
    tg$div(style = "margin:2px 0;", tg$b("Score: "),
           if (is.na(score)) sprintf("Insufficient data (%s of 4 components; missing: %s)",
                                     if (is.na(n)) 0 else n, if (is.na(miss)) "all" else miss) else
             sprintf("%.0f/100, mean of %.0f of 4 component risk percentiles%s", score, n,
                     if (nzchar(miss)) paste0(" (missing: ", miss, ")") else "")),
    mortgage, consumer, jobs, collateral,
    if (!info$has[["urban"]]) tg$div(
      style = sprintf("color:%s;margin-top:4px;", c$warn),
      if (!is.na(score)) sprintf("Urban Institute data unavailable — score uses %.0f components.", n) else
        "Urban Institute data unavailable."),
    tg$div(style = sprintf("color:%s;margin-top:4px;", c$muted), sprintf(paste0(
      "Risk percentiles compare US counties (high = more credit risk); the score needs ",
      "at least %d of 4 components."), min_n)))
}

# Dashed-outline notes, one sentence per group: assistance layers are only noisy through
# their inputs (rent burden, poverty); ACS layers are noisy in themselves.
noisy_notes <- function(labels, is_assist, color) {
  tg <- htmltools::tags
  note <- function(txt) tg$div(style = sprintf("color:%s;margin-top:8px;", color), txt)
  list(
    if (any(!is_assist)) note(sprintf(
      "Dashed outline: the margin of error is larger than the value for %s.",
      paste(labels[!is_assist], collapse = ", "))),
    if (any(is_assist)) note(sprintf(
      "Dashed outline: the margin of error is larger than the value for an input (rent burden or poverty) to: %s.",
      paste(labels[is_assist], collapse = ", "))))
}

# ── Side panel: full component breakdown for the selected county or tract ────
risk_side_panel <- function(area, level = c("county", "tract"), county_layers,
                            missing_states = character(0), sources = character(0)) {
  level <- match.arg(level)
  tg <- htmltools::tags; c <- .RISK_COL
  a <- .plain_df(area)[1, , drop = FALSE]
  asof <- attr(county_layers, "asof"); status <- attr(county_layers, "layer_status")
  ids <- names(RISK_LAYERS)
  if (level == "tract") ids <- ids[vapply(RISK_LAYERS[ids], function(l) "tract" %in% l$levels, logical(1))]
  get <- function(col) if (col %in% names(a)) a[[col]] else NA
  rk <- function(p, hiw) { r <- rank_label(p, ""); if (is.na(r)) "—" else trimws(r) }  # value terms
  td <- function(...) tg$td(style = "padding:3px 10px 3px 0;", ...)

  head <- c("Component", "Value",
            if (level == "county") "National rank" else c("Labor market", "State"),
            "As of", "Source")
  # An assistance tract's industry-mix source: its own, else its state's in the loaded frame.
  tract_src <- if (level == "tract") {
    s <- as.character(get("assist_source"))
    if (is.na(s)) s <- unname(sources[substr(get("GEOID"), 1, 2)])
    if (length(s) == 0) NA_character_ else s
  }

  noisy_labels <- character(0); noisy_assist <- logical(0)
  row_for <- function(id) {
    cfg <- RISK_LAYERS[[id]]
    v <- as.numeric(get(cfg$col))
    vt <- .risk_fmt(v, cfg$fmt, cfg$digits %||% 1)
    if (id == "stress_now" && !is.na(v)) {
      fl <- get("flag")
      vt <- sprintf("%s%s (%.1f%% now vs %.1f%% a year ago)", vt,
                    if (.flag_shown(fl)) paste0(" · ", .flag_text(fl)) else "",
                    get("rate_now"), get("rate_yr_ago"))
    }
    moe <- get(paste0(cfg$col, "_moe"))
    if (level == "tract" && !is.na(v) && !is.na(moe)) vt <- sprintf("%s ± %.1f", vt, moe)
    if (level == "tract" && .noisy_flags(a, cfg)) {
      noisy_labels <<- c(noisy_labels, cfg$label)
      noisy_assist <<- c(noisy_assist, identical(cfg$lens, "assistance"))
    }
    # Assistance tracts have their own status and as-of (not the county's).
    st_id <- status[[id]]; asof_id <- asof[[id]]
    if (level == "tract" && identical(cfg$lens, "assistance")) {
      sa <- .tract_status_asof(data.frame(assist_source = if (length(tract_src)) tract_src else
                                            NA_character_), cfg, id, county_layers)
      st_id <- sa$status; asof_id <- sa$asof
    }
    when <- if (identical(st_id, "unavailable")) "unavailable" else
      if (identical(st_id, "not_loaded")) "not loaded — open the Credit lens" else
      if (is.na(asof_id)) "loading" else
        paste0(asof_id, if (st_id %in% c("stale", "partial")) paste0(" (", st_id, ")") else "")
    quad_rk <- if (id == "now_vs_ahead") { q <- .quad_rank_text(a); if (is.na(q)) "—" else q }
    pcts <- if (level == "county" && id == "now_vs_ahead") list(td(quad_rk)) else
      if (level == "county") list(td(rk(get(paste0("pct_", cfg$col)), cfg$higher_is_worse))) else
      list(td(rk(get(paste0("lm_pct_", cfg$col)), cfg$higher_is_worse)),
           td(rk(get(paste0("st_pct_", cfg$col)), cfg$higher_is_worse)))
    tg$tr(td(cfg$label), td(tg$b(if (is.na(vt)) "—" else vt)), pcts, td(when), td(cfg$source))
  }
  # One header row per lens, in lens order, then that lens's layers in registry order.
  lens_of <- vapply(RISK_LAYERS[ids], function(l) l$lens, "")
  rows <- unlist(lapply(seq_along(RISK_LENSES), function(k) {
    in_lens <- ids[lens_of == RISK_LENSES[[k]]]
    if (!length(in_lens)) return(NULL)
    c(list(tg$tr(tg$td(colspan = length(head), style = sprintf(
      "padding:8px 0 2px;color:%s;font-weight:700;letter-spacing:.04em;text-transform:uppercase;font-size:11px;",
      c$accent), names(RISK_LENSES)[k]))), lapply(in_lens, row_for))
  }), recursive = FALSE)

  title <- if (level == "county") get("name") else get("tract_name")
  sub <- if (level == "county") {
    if (!is.na(get("cbsa_name"))) paste(get("cbsa_name"), get("cbsa_type")) else "Outside any metro or micro area"
  } else paste("Ranked", get("frame_label"), "and", get("state_label"))
  st_names <- stats::setNames(.plain_df(county_layers)$state_name, .plain_df(county_layers)$state_fips)

  vintage <- attr(county_layers, "pep_vintage")
  standing <- c(
    if (level == "county" && !is.null(vintage)) .migration_line(a, vintage),
    if (level == "county") .metro_line_rows(a, county_layers)
    else .metro_line_rows(a, county_layers, key = "county_geoid"))
  fn <- suppressWarnings(as.integer(get("frame_n")))
  if (level == "tract" && length(fn) == 1 && !is.na(fn) && fn < .SMALL_FRAME)
    standing <- c(standing, .small_frame_text(fn))
  standing <- standing[nzchar(standing)]
  tg$div(
    style = sprintf("background:%s;color:%s;border-radius:8px;padding:14px 16px;margin-top:12px;font-size:12px;",
                    c$page, c$text),
    tg$div(style = sprintf("color:%s;font-weight:700;font-size:16px;", c$accent), title),
    tg$div(style = sprintf("color:%s;margin-bottom:%s;", c$muted,
                           if (length(standing)) "2px" else "8px"), sub),
    lapply(seq_along(standing), function(k) tg$div(
      style = sprintf("color:%s;margin-bottom:%s;", c$muted,
                      if (k == length(standing)) "8px" else "2px"), standing[k])),
    tg$div(style = "overflow-x:auto;", tg$table(
      style = "border-collapse:collapse;width:100%;",
      tg$tr(style = sprintf("color:%s;text-align:left;", c$muted), lapply(head, tg$th)),
      rows)),
    if (level == "county") .quad_panel(a),
    .assist_panel(a, level, county_layers, st_names, sources),
    if (level == "county" && !is.null(vintage)) .migration_panel(a, vintage),
    if (level == "county") .industry_panel(a, attr(county_layers, "sector_weights"),
                                          attr(county_layers, "acs_year")),
    if (level == "county") .credit_panel(a, county_layers),
    noisy_notes(noisy_labels, noisy_assist, c$warn),
    if (length(missing_states)) tg$div(
      style = sprintf("color:%s;margin-top:8px;", c$warn),
      paste("Tract data unavailable for:",
            paste(unique(unname(st_names[missing_states])), collapse = ", ")))
  )
}

#' US county choropleth for one layer. Polygons carry GEOID layerIds for clicks.
build_risk_county_map <- function(county_layers, layer_id, fill_opacity = RISK_FILL_OPACITY) {
  leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
    leaflet::addTiles(options = leaflet::tileOptions(opacity = 0.95)) |>  # CARTO now needs a key
    .add_county_polygons(county_layers, layer_id, fill_opacity) |>
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
}

# ── Data table under the map ──────────────────────────────────────────────────
#' One row per county (in `state` when given), worst first for the selected layer.
#' Values are numeric; the app formats them. `Value` is the selected layer's column,
#' `US pct` / `State pct` its percentile rank (high = high value) nationally / in the state.
risk_county_table <- function(county_layers, layer_id, state = NULL) {
  cfg <- RISK_LAYERS[[layer_id]]
  d <- .plain_df(county_layers)
  n <- nrow(d)
  num <- function(col) as.numeric(.col_or_na(d, col))
  keep <- if (is.null(state) || !nzchar(state)) rep(TRUE, n) else
    as.character(d$state_fips) == state
  v <- num(cfg$col)
  pct_st <- rep(NA_real_, n)
  pct_st[keep] <- pct_rank(v[keep])
  pct_st[is.nan(pct_st)] <- NA_real_      # a lone county has no rank
  out <- data.frame(
    GEOID = d$GEOID,
    County = sub(", [A-Z]{2}$", "", d$name),
    State = d$state_name,
    Metro = ifelse(is.na(d$cbsa_name), "—", d$cbsa_name),
    Value = v,
    `US pct` = round(num(paste0("pct_", cfg$col))),
    `State pct` = round(pct_st),
    `Unemp. now (%)` = num("rate_now"),
    `Unemp. vs yr ago (pp)` = num("momentum_pp"),
    `Stress ahead (pp)` = num("stress_ahead_pp"),
    `Assistance score` = num("assist_score"),
    `Credit score` = num("credit_score"),
    `Net dom. migration /1k` = num("mig_dom_rate"),
    Population = num("pop"),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- out[keep, , drop = FALSE]
  worst_first <- if (isFALSE(cfg$higher_is_worse)) out$Value else -out$Value
  out <- out[order(worst_first, na.last = TRUE), , drop = FALSE]
  if (identical(cfg$fmt, "quadrant")) {
    # The code only orders the rows; show the name and drop the meaningless percentiles.
    out$Value <- .risk_fmt(out$Value, "quadrant")
    out[["US pct"]] <- NULL
    out[["State pct"]] <- NULL
  }
  out
}

#' One row per tract of the drilled-in frame, ranked within the metro and the state.
risk_tract_table <- function(tracts, layer_id) {
  cfg <- RISK_LAYERS[[layer_id]]
  t <- .plain_df(tracts)
  v <- as.numeric(t[[cfg$col]])
  out <- data.frame(
    GEOID = t$GEOID, Tract = t$tract_name, Value = v,
    `Metro pct` = round(as.numeric(.col_or_na(t, paste0("lm_pct_", cfg$col)))),
    `State pct` = round(as.numeric(.col_or_na(t, paste0("st_pct_", cfg$col)))),
    `High margin of error` = ifelse(.noisy_flags(t, cfg), "yes", ""),
    check.names = FALSE, stringsAsFactors = FALSE)
  worst_first <- if (isFALSE(cfg$higher_is_worse)) out$Value else -out$Value
  out[order(worst_first, na.last = TRUE), , drop = FALSE]
}

# ── Layer descriptions ────────────────────────────────────────────────────────
# One short line for the selected layer; the full guide is a collapsed panel.
.LENS_HELP <- c(
  assistance = paste("Where help may be needed soonest: exposure of the local industry mix",
                     "to weakening sectors, thin financial cushion (rent burden, poverty,",
                     "low-wage and young workers) and county job pressure. Index headcounts,",
                     "not layoff forecasts."),
  credit = paste("Household credit risk score (0–100, needs 3 of 4 components): CFPB",
                 "mortgage delinquency, Urban Institute card and auto delinquency, county",
                 "job pressure and FHFA home prices. Loads on first use."),
  migration = "Census PEP county components of change, per 1,000 residents.",
  context = "Census ACS 5-year estimates.")

.LAYER_HELP <- c(
  stress_now = paste("BLS county unemployment, latest 3 months vs. the same months a year",
                     "earlier. +1.0 pp or more is a Sahm-style trigger."),
  stress_ahead = paste("National sector payroll momentum (6-mo.) applied to each county's",
                       "resident industry mix; the gap vs. the US."),
  tax_base = paste("A proxy from migration, population change and home prices — not a",
                   "bond default model."),
  now_vs_ahead = paste("Stress now and stress ahead on one scale: a county is bad on a measure",
                       "if it is in the worst third of US counties. Orange = fine now, bad",
                       "ahead; teal = bad now, better ahead."))

risk_layer_help <- function(layer_id) {
  if (layer_id %in% names(.LAYER_HELP)) return(unname(.LAYER_HELP[[layer_id]]))
  lens <- RISK_LAYERS[[layer_id]]$lens
  if (!is.null(lens) && lens %in% names(.LENS_HELP)) unname(.LENS_HELP[[lens]]) else ""
}

#' The full guide: every layer family, plus how to read the map. Shown collapsed.
risk_guide_items <- function() c(
  paste("Stress now:", .LAYER_HELP[["stress_now"]]),
  paste("Stress ahead:", .LAYER_HELP[["stress_ahead"]]),
  paste("Now vs ahead:", .LAYER_HELP[["now_vs_ahead"]]),
  paste("Assistance:", .LENS_HELP[["assistance"]]),
  paste("Credit:", .LENS_HELP[["credit"]],
        "CFPB mortgage delinquency is the county's own, else a metro or state non-metro",
        "estimate; Urban Institute shows the release date, not the data month; FHFA home",
        "prices are for the metro division, metro or state."),
  paste("Context layers:", .LENS_HELP[["context"]], "Tax base pressure:", .LAYER_HELP[["tax_base"]]),
  paste("Migration:", .LENS_HELP[["migration"]]),
  "State filter: zooms to a state, fades the rest, and ranks counties within the state. Colours stay on the national scale unless you tick \"Rescale colours\".",
  "Click a county (or a row in the table) to load its tracts, ranked within the metro and the state. Layers that only exist per county (Stress now, Credit, Migration) show tracts coloured by assistance need instead; the legend says so.",
  "Dashed outline: margin of error larger than the value.")
