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
                              standing = "") {
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
# percentiles (assistance now, credit later), so its percentile is added after them.
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
    bins = c(-50, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 50))),
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
  stats::setNames(lapply(names(VAR_CFG), function(id) {
    v <- VAR_CFG[[id]]
    list(label = v$label, col = id, fmt = v$fmt, pal = v$pal, rev = v$rev,
         higher_is_worse = v$rev, source = "Census ACS 5-yr", levels = c("county", "tract"),
         lens = "context")
  }), names(VAR_CFG)),
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
RISK_LENSES <- c("Assistance" = "assistance", "Migration" = "migration", "Context" = "context")

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

#' digits applies to "pp" (layers may set `digits` in RISK_LAYERS; default 1). Values that
#' round to zero print as +0.0 / +0.00, never a signed zero.
.risk_fmt <- function(v, fmt, digits = 1) {
  out <- switch(fmt,
    "$"  = paste0("$", formatC(round(v), format = "d", big.mark = ",")),
    "pp" = {
      r <- round(v, digits); r[!is.na(r) & r == 0] <- 0
      sprintf(paste0("%+.", digits, "f pp"), r)
    },
    "per1k" = sprintf("%+.1f per 1,000", v),
    "score" = sprintf("%.0f", v),
    "n"  = formatC(round(v), big.mark = ",", format = "d"),
    sprintf("%.1f%%", v))
  out[is.na(v)] <- NA_character_
  out
}

.risk_palette <- function(cfg, vals) {
  if (!is.null(cfg$bins)) {
    return(leaflet::colorBin(cfg$pal, domain = range(cfg$bins), reverse = cfg$rev,
                             bins = cfg$bins, na.color = "#555566"))
  }
  if (all(is.na(vals))) vals <- c(0, 1)      # nothing loaded yet: a grey map, not an error
  brks <- unique(stats::quantile(vals, seq(0, 1, length.out = 8), na.rm = TRUE))
  if (length(brks) < 3) brks <- 7
  leaflet::colorBin(cfg$pal, domain = vals, bins = brks, reverse = cfg$rev,
                    na.color = "#555566")
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

.county_tooltips <- function(d, id, cfg, asof, status = "ok") {
  if (identical(status, "loading")) {
    return(risk_tooltips_vec(title = d$name, value_text = "Loading…",
                             footer = paste(cfg$source, "— loading")))
  }
  vtxt <- .risk_fmt(d[[cfg$col]], cfg$fmt, cfg$digits %||% 1)
  if (id == "stress_now") {
    has_flag <- !is.na(vtxt) & .flag_shown(d$flag)
    vtxt[has_flag] <- paste0(vtxt[has_flag], " · ", .flag_text(d$flag[has_flag]))
  }
  pct <- d[[paste0("pct_", cfg$col)]]
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
  } else if (identical(cfg$lens, "assistance")) {
    risk_drivers_vec(
      c("Workers in weakening sectors", "Renters cost-burdened", "Poverty rate"),
      .drv_text(d, c("weak_share", "rent_burden_rate", "poverty_rate")),
      .drv_worse(d, c("pct_weak_share", "pct_rent_burden_rate", "pct_poverty_rate")))
  } else ""
  na_col <- function(col) if (is.null(d[[col]])) rep(NA, nrow(d)) else d[[col]]
  job_line <- if (identical(cfg$lens, "assistance")) .standing_html(.job_pressure_line(
    as.numeric(na_col("job_pressure")), na_col("job_pressure_n"),
    na_col("job_pressure_from"))) else ""
  risk_tooltips_vec(title = d$name, subtitle = subs, value_text = vtxt,
                    rank_text = rank_label(pct, cfg$rank_frame %||% "nationally"),
                    footer = footer, drivers_html = drivers,
                    rank_bad = .rank_bad(pct, cfg, d[[cfg$col]]),
                    standing = paste0(job_line,
                                      .standing_html(.migration_line(d, attr(d, "pep_vintage"))),
                                      .standing_html(.metro_line_rows(d))))
}

#' Ranks always read in value terms ("Top" = highest value). The pill is red only
#' on the bad side: sign for pp layers, the median otherwise; never for a neutral
#' (NA higher_is_worse) layer.
.rank_bad <- function(pct, cfg, vals = NULL) {
  hiw <- cfg$higher_is_worse %||% NA        # missing = neutral
  if (is.na(hiw)) return(rep(FALSE, length(pct)))
  bad <- if (identical(cfg$fmt, "pp") && !is.null(vals)) {
    vals <- round(vals, cfg$digits %||% 1)       # what is displayed decides the colour
    if (isTRUE(hiw)) vals > 0 else vals < 0
  } else if (isTRUE(hiw)) pct >= 50 else pct < 50
  bad %in% TRUE
}

#' Open-ended bin labels from breaks; numbers only (no unit), signed, as
#' "below −10", "−10 to −5", ..., "above +10".
.bin_labels <- function(bins, fmt, digits = 1) {
  num <- function(x) {
    out <- if (fmt == "pp") sprintf(paste0("%+.", digits, "f"), x) else
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

.legend_title <- function(cfg, status, asof, scope = NULL) {
  head <- htmltools::htmlEscape(cfg$label)
  sub <- if (identical(status, "unavailable")) "unavailable" else
    if (identical(status, "loading")) {
      cfg$loading_text %||% "loading…"
    } else
      paste0(if (!is.null(scope)) paste0(scope, " · "), cfg$source, ", ", asof,
             if (identical(status, "stale")) " (stale)",
             if (identical(status, "partial"))
               paste0(" (", cfg$partial_text %||% "partial: some counties missing, retrying later", ")"))
  paste0(head, "<br><small style='font-weight:400;'>", htmltools::htmlEscape(sub), "</small>")
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

.add_county_polygons <- function(map, county_layers, layer_id, fill_opacity = 0.82) {
  cfg <- RISK_LAYERS[[layer_id]]
  vals <- as.numeric(county_layers[[cfg$col]])
  pal <- .risk_palette(cfg, vals)
  asof <- attr(county_layers, "asof")[[layer_id]]
  status <- attr(county_layers, "layer_status")[[layer_id]]
  lbl_fmt <- .legend_format(cfg)
  title <- .legend_title(cfg, status, asof)
  add_legend <- function(m) {
    if (is.null(cfg$bins)) {
      return(leaflet::addLegend(m, pal = pal, values = vals, opacity = 0.9,
                                layerId = "risk_legend", title = title,
                                position = "bottomright", labFormat = lbl_fmt))
    }
    b <- cfg$bins
    mids <- (b[-1] + b[-length(b)]) / 2
    leaflet::addLegend(m, colors = pal(mids), opacity = 0.9, layerId = "risk_legend",
                       title = title, position = "bottomright",
                       labels = cfg$bin_labels %||% .bin_labels(b, cfg$fmt, cfg$digits %||% 1))
  }
  map |>
    leaflet::clearGroup("counties") |>
    leaflet::removeControl("risk_legend") |>
    leaflet::addPolygons(
      data = county_layers, layerId = county_layers$GEOID, group = "counties",
      fillColor = pal(vals), fillOpacity = fill_opacity,
      color = "#0f1117", weight = 0.4, smoothFactor = 0.5,
      label = .county_tooltips(county_layers, layer_id, cfg, asof, status),
      labelOptions = leaflet::labelOptions(
        style = list(background = "transparent", border = "none", padding = "0",
                     "box-shadow" = "none"), direction = "auto"),
      highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2,
                                                   bringToFront = FALSE)
    ) |>
    add_legend()
}

# ── Tract drill-down layer ───────────────────────────────────────────────────
.tract_tooltips <- function(t, cfg, asof, noisy, county_layers) {
  vtxt <- .risk_fmt(as.numeric(t[[cfg$col]]), cfg$fmt, cfg$digits %||% 1)
  lm_pct <- t[[paste0("lm_pct_", cfg$col)]]
  lm <- rank_label(lm_pct, t$frame_label)
  st <- rank_label(t[[paste0("st_pct_", cfg$col)]], t$state_label)
  footer <- paste0(cfg$source, ", ", asof, ifelse(noisy, " · High margin of error", ""))
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
                    rank_bad = .rank_bad(lm_pct, cfg, as.numeric(t[[cfg$col]])),
                    standing = .standing_html(
                      .metro_line_rows(t, county_layers, key = "county_geoid")))
}

# A binned layer keeps its own bin labels (no "80 – 101"); others use the palette's.
add_tract_legend <- function(map, pal, vals, cfg, lbl_fmt, title) {
  if (is.null(cfg$bin_labels)) {
    return(leaflet::addLegend(map, pal = pal, values = vals, opacity = 0.9,
                              layerId = "risk_legend", position = "bottomright",
                              labFormat = lbl_fmt, title = title))
  }
  b <- cfg$bins
  leaflet::addLegend(map, colors = pal((b[-1] + b[-length(b)]) / 2), labels = cfg$bin_labels,
                     opacity = 0.9, layerId = "risk_legend", position = "bottomright",
                     title = title)
}

#' Dim counties and draw the frame's tracts for a tract-level layer. For a
#' county-only layer (Stress now) the counties are restored and no tracts drawn.
add_tract_layer <- function(map, tracts, layer_id, county_layers, fit = TRUE) {
  cfg <- RISK_LAYERS[[layer_id]]
  map <- leaflet::clearGroup(map, "tracts")
  have <- !is.null(tracts) && nrow(tracts) > 0
  fit_to <- function(m) {
    if (!fit || !have) return(m)
    bb <- as.numeric(sf::st_bbox(tracts))
    leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
  }
  if (!"tract" %in% cfg$levels || !have) {
    return(fit_to(.add_county_polygons(map, county_layers, layer_id, 0.82)))
  }
  asof <- attr(county_layers, "asof")[[layer_id]]
  vals <- as.numeric(tracts[[cfg$col]])
  pal <- .risk_palette(cfg, vals)
  noisy <- .noisy_flags(tracts, cfg)
  lbl_fmt <- .legend_format(cfg)

  .add_county_polygons(map, county_layers, layer_id, 0.25) |>
    leaflet::addPolygons(
      data = tracts, layerId = tracts$GEOID, group = "tracts",
      fillColor = pal(vals), fillOpacity = 0.85,
      color = ifelse(noisy, .RISK_COL$warn, "#0f1117"),
      weight = ifelse(noisy, 1.5, 0.4), dashArray = ifelse(noisy, "4", NA),
      label = .tract_tooltips(tracts, cfg, asof, noisy, county_layers),
      labelOptions = leaflet::labelOptions(
        style = list(background = "transparent", border = "none", padding = "0",
                     "box-shadow" = "none"), direction = "auto"),
      highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2)
    ) |>
    leaflet::removeControl("risk_legend") |>
    add_tract_legend(pal, vals, cfg, lbl_fmt,
                     .legend_title(cfg, "ok", asof, scope = paste("tracts", tracts$frame_label[1]))) |>
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

reset_to_us <- function(map, county_layers, layer_id) {
  leaflet::clearGroup(map, "tracts") |>
    .add_county_polygons(county_layers, layer_id, 0.82) |>
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
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

#' Assistance notes for the side panel (divs, not rows): industry-mix source, the
#' exposure-weighted headcount with its caveat, rent-burdened households (tract only)
#' and county job pressure. NULL when the area has no assistance values.
#' `sources`: state FIPS -> source text of the loaded tract frame (fallback for a tract
#' missing from the job table; per-state list when LODES and ACS are mixed).
.assist_panel <- function(a, level, county_layers, st_names, sources = character(0)) {
  get <- function(col) if (col %in% names(a)) a[[col]][1] else NA
  workers <- as.numeric(get("assist_workers"))
  if (is.na(workers) && is.na(as.numeric(get("assist_score")))) return(NULL)
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

  noisy_labels <- character(0); noisy_assist <- logical(0)
  rows <- lapply(ids, function(id) {
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
    when <- if (identical(status[[id]], "unavailable")) "unavailable" else
      if (is.na(asof[[id]])) "loading" else asof[[id]]
    pcts <- if (level == "county") list(td(rk(get(paste0("pct_", cfg$col)), cfg$higher_is_worse))) else
      list(td(rk(get(paste0("lm_pct_", cfg$col)), cfg$higher_is_worse)),
           td(rk(get(paste0("st_pct_", cfg$col)), cfg$higher_is_worse)))
    tg$tr(td(cfg$label), td(tg$b(if (is.na(vt)) "—" else vt)), pcts, td(when), td(cfg$source))
  })
  head <- c("Component", "Value",
            if (level == "county") "National rank" else c("Labor market", "State"),
            "As of", "Source")

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
    .assist_panel(a, level, county_layers, st_names, sources),
    if (level == "county" && !is.null(vintage)) .migration_panel(a, vintage),
    if (level == "county") .industry_panel(a, attr(county_layers, "sector_weights"),
                                          attr(county_layers, "acs_year")),
    noisy_notes(noisy_labels, noisy_assist, c$warn),
    if (length(missing_states)) tg$div(
      style = sprintf("color:%s;margin-top:8px;", c$warn),
      paste("Tract data unavailable for:",
            paste(unique(unname(st_names[missing_states])), collapse = ", ")))
  )
}

#' US county choropleth for one layer. Polygons carry GEOID layerIds for clicks.
build_risk_county_map <- function(county_layers, layer_id, fill_opacity = 0.82) {
  leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
    leaflet::addTiles(options = leaflet::tileOptions(opacity = 0.95)) |>  # CARTO now needs a key
    .add_county_polygons(county_layers, layer_id, fill_opacity) |>
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
}
