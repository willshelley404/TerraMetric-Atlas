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
#' Elements whose values are all NA get "".
risk_drivers_vec <- function(labels, values, worse) {
  c <- .RISK_COL
  n <- length(values[[1]])
  labels <- if (is.list(labels)) unlist(lapply(labels, rep_len, n)) else rep(labels, each = n)
  vm <- matrix(unlist(lapply(values, rep_len, n)), nrow = n)          # n x k
  wm <- matrix(unlist(lapply(worse, function(w) rep_len(w, n))), nrow = n) %in% TRUE
  rows <- matrix(sprintf(
    paste0('<div style="display:flex;justify-content:space-between;gap:8px;">',
           '<span>%s</span><span style="font-weight:600;color:%s;">%s</span></div>'),
    .tt_esc(labels), ifelse(wm, c$worse, c$text), .tt_esc(vm)), nrow = n)
  rows[is.na(vm)] <- ""
  out <- paste0(
    sprintf('<hr style="border:0;border-top:1px solid %s;margin:7px 0;"/>', c$border),
    sprintf('<div style="color:%s;font-size:11px;">Driven by</div>', c$muted),
    apply(rows, 1, paste, collapse = ""))
  ifelse(rowSums(!is.na(vm)) > 0, out, "")
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
RISK_LAYERS <- c(
  list(stress_now = list(
    label = "Stress now: unemployment vs. a year ago", col = "momentum_pp", fmt = "pp",
    pal = "RdBu", rev = TRUE, higher_is_worse = TRUE, source = "BLS LAUS",
    levels = "county", lens = "context",
    bins = c(-50, -1, -0.5, -0.2, 0.2, 0.5, 1, 50),
    bin_labels = c("below −1.0 pp", "−1.0 to −0.5", "−0.5 to −0.2", "−0.2 to +0.2",
                   "+0.2 to +0.5", "+0.5 to +1.0 (rising)",
                   "above +1.0 pp (Sahm-style trigger)")),
    stress_ahead = list(
    label = "Stress ahead: projected job growth vs. US (shift-share)",
    col = "stress_ahead_pp", fmt = "pp", pal = "RdBu", rev = FALSE,
    higher_is_worse = FALSE, source = "BLS CES (FRED) × ACS industry mix",
    levels = "county", lens = "context",
    loading_text = "loading sector payrolls and county industry mix…",
    bins = c(-50, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 50))),
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
      rank_frame = "for in-migration", bins = bins)
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
RISK_LENSES <- c("Migration" = "migration", "Context" = "context")

#' Layer ids (values) and labels (names) of one lens, in registry order.
risk_lens_choices <- function(lens) {
  ids <- names(RISK_LAYERS)[vapply(RISK_LAYERS, function(l) identical(l$lens, lens), TRUE)]
  stats::setNames(ids, vapply(RISK_LAYERS[ids], `[[`, "", "label"))
}
risk_lens_default <- function(lens) unname(risk_lens_choices(lens)[1])

# Display text for the Stress now flag column (values stay Trigger/Rising/None).
.flag_text <- function(flag) ifelse(flag %in% "Trigger", "Sahm-style trigger", flag)

.risk_fmt <- function(v, fmt) {
  out <- switch(fmt,
    "$"  = paste0("$", formatC(round(v), format = "d", big.mark = ",")),
    "pp" = sprintf("%+.1f pp", v),
    "per1k" = sprintf("%+.1f per 1,000", v),
    sprintf("%.1f%%", v))
  out[is.na(v)] <- NA_character_
  out
}

.risk_palette <- function(cfg, vals) {
  if (!is.null(cfg$bins)) {
    return(leaflet::colorBin(cfg$pal, domain = range(cfg$bins), reverse = cfg$rev,
                             bins = cfg$bins, na.color = "#555566"))
  }
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

.standing_html <- function(lines) {
  ifelse(.tt_has(lines), sprintf(
    '<div style="color:%s;font-size:11px;margin-top:4px;">%s</div>', .RISK_COL$muted,
    .tt_esc(lines)), "")
}

.county_tooltips <- function(d, id, cfg, asof, status = "ok") {
  if (identical(status, "loading")) {
    return(risk_tooltips_vec(title = d$name, value_text = "Loading…",
                             footer = paste(cfg$source, "— loading")))
  }
  vtxt <- .risk_fmt(d[[cfg$col]], cfg$fmt)
  if (id == "stress_now") {
    has_flag <- !is.na(vtxt) & !is.na(d$flag)
    vtxt[has_flag] <- paste0(vtxt[has_flag], " · ", .flag_text(d$flag[has_flag]))
  }
  pct <- d[[paste0("pct_", cfg$col)]]
  subs <- ifelse(is.na(d$cbsa_name), NA_character_, paste(d$cbsa_name, d$cbsa_type))
  footer <- if (is.na(asof)) paste(cfg$source, "— unavailable") else
    paste0(cfg$source, ", ", asof)
  drivers <- if (identical(cfg$lens, "migration")) {
    risk_drivers_vec(
      c("Domestic", "International", "Natural change (births − deaths)"),
      lapply(c("mig_dom_rate", "mig_intl_rate", "nat_chg_rate"),
             function(col) .risk_fmt(d[[col]], "per1k")),
      rep(list(FALSE), 3))
  } else if (id == "stress_ahead") {
    risk_drivers_vec(
      lapply(1:3, function(k) d[[sprintf("sa_drv%d_label", k)]]),
      lapply(1:3, function(k) {
        v <- d[[sprintf("sa_drv%d_pp", k)]]
        ifelse(is.na(v), NA_character_, sprintf("%+.2f pp", v))
      }),
      lapply(1:3, function(k) d[[sprintf("sa_drv%d_pp", k)]] < 0))
  } else ""
  risk_tooltips_vec(title = d$name, subtitle = subs, value_text = vtxt,
                    rank_text = rank_label(pct, cfg$rank_frame %||% "nationally"),
                    footer = footer, drivers_html = drivers,
                    rank_bad = .rank_bad(pct, cfg, d[[cfg$col]]),
                    standing = .standing_html(.migration_line(d, attr(d, "pep_vintage"))))
}

#' Ranks always read in value terms ("Top" = highest value). The pill is red only
#' on the bad side: sign for pp layers, the median otherwise; never for a neutral
#' (NA higher_is_worse) layer.
.rank_bad <- function(pct, cfg, vals = NULL) {
  if (is.na(cfg$higher_is_worse)) return(rep(FALSE, length(pct)))
  bad <- if (identical(cfg$fmt, "pp") && !is.null(vals)) {
    if (isTRUE(cfg$higher_is_worse)) vals > 0 else vals < 0
  } else if (isTRUE(cfg$higher_is_worse)) pct >= 50 else pct < 50
  bad %in% TRUE
}

#' Open-ended bin labels from breaks; numbers only (no unit), signed, as
#' "below −10", "−10 to −5", ..., "above +10".
.bin_labels <- function(bins, fmt) {
  num <- function(x) {
    out <- if (fmt == "pp") sprintf("%+.1f", x) else
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
      if (!is.null(cfg$loading_text)) cfg$loading_text else
      if (identical(cfg$source, "Census PEP")) "loading Census population estimates…" else
        "loading county unemployment from BLS…"
    } else
      paste0(if (!is.null(scope)) paste0(scope, " · "), cfg$source, ", ", asof,
             if (identical(status, "stale")) " (stale)",
             if (identical(status, "partial")) " (partial: some counties missing, retrying later)")
  paste0(head, "<br><small style='font-weight:400;'>", htmltools::htmlEscape(sub), "</small>")
}

.legend_format <- function(cfg) {
  switch(cfg$fmt,
    "$"  = leaflet::labelFormat(prefix = "$", big.mark = ",", digits = 0),
    "pp" = leaflet::labelFormat(suffix = " pp", digits = 1),
    "per1k" = leaflet::labelFormat(suffix = " per 1,000", digits = 1),
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
                       labels = cfg$bin_labels %||% .bin_labels(b, cfg$fmt))
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
.tract_tooltips <- function(t, cfg, asof, noisy) {
  vtxt <- .risk_fmt(as.numeric(t[[cfg$col]]), cfg$fmt)
  lm_pct <- t[[paste0("lm_pct_", cfg$col)]]
  lm <- rank_label(lm_pct, t$frame_label)
  st <- rank_label(t[[paste0("st_pct_", cfg$col)]], t$state_label)
  footer <- paste0(cfg$source, ", ", asof, ifelse(noisy, " · High margin of error", ""))
  risk_tooltips_vec(title = t$tract_name, value_text = vtxt, rank_text = lm,
                    rank_sub = st, footer = footer,
                    rank_bad = .rank_bad(lm_pct, cfg, as.numeric(t[[cfg$col]])))
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
  noisy_col <- paste0("noisy_", cfg$col)
  noisy <- if (noisy_col %in% names(tracts)) tracts[[noisy_col]] %in% TRUE else
    rep(FALSE, nrow(tracts))
  lbl_fmt <- .legend_format(cfg)

  .add_county_polygons(map, county_layers, layer_id, 0.25) |>
    leaflet::addPolygons(
      data = tracts, layerId = tracts$GEOID, group = "tracts",
      fillColor = pal(vals), fillOpacity = 0.85,
      color = ifelse(noisy, .RISK_COL$warn, "#0f1117"),
      weight = ifelse(noisy, 1.5, 0.4), dashArray = ifelse(noisy, "4", NA),
      label = .tract_tooltips(tracts, cfg, asof, noisy),
      labelOptions = leaflet::labelOptions(
        style = list(background = "transparent", border = "none", padding = "0",
                     "box-shadow" = "none"), direction = "auto"),
      highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2)
    ) |>
    leaflet::removeControl("risk_legend") |>
    leaflet::addLegend(pal = pal, values = vals, opacity = 0.9, layerId = "risk_legend",
                       position = "bottomright", labFormat = lbl_fmt,
                       title = .legend_title(cfg, "ok", asof,
                                             scope = paste("tracts", tracts$frame_label[1]))) |>
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
.industry_panel <- function(a, weights) {
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
                                  sprintf(" · US payrolls %s%%/yr", signed(m[i])))))),
    if (!is.null(tp) && !is.na(tp)) tg$div(
      style = sprintf("color:%s;margin-top:6px;", c$muted),
      sprintf(paste0("Temporary help payrolls (US): %+.1f%% annualized over 6 months — ",
                     "a leading indicator, not in the score."), tp)))
}

# ── Side panel: full component breakdown for the selected county or tract ────
risk_side_panel <- function(area, level = c("county", "tract"), county_layers,
                            missing_states = character(0)) {
  level <- match.arg(level)
  tg <- htmltools::tags; c <- .RISK_COL
  a <- .plain_df(area)[1, , drop = FALSE]
  asof <- attr(county_layers, "asof"); status <- attr(county_layers, "layer_status")
  ids <- names(RISK_LAYERS)
  if (level == "tract") ids <- ids[vapply(RISK_LAYERS[ids], function(l) "tract" %in% l$levels, logical(1))]
  get <- function(col) if (col %in% names(a)) a[[col]] else NA
  rk <- function(p, hiw) { r <- rank_label(p, ""); if (is.na(r)) "—" else trimws(r) }  # value terms
  td <- function(...) tg$td(style = "padding:3px 10px 3px 0;", ...)

  noisy_labels <- character(0)
  rows <- lapply(ids, function(id) {
    cfg <- RISK_LAYERS[[id]]
    v <- as.numeric(get(cfg$col))
    vt <- .risk_fmt(v, cfg$fmt)
    if (id == "stress_now" && !is.na(v)) {
      vt <- sprintf("%s · %s (%.1f%% now vs %.1f%% a year ago)", vt, .flag_text(get("flag")),
                    get("rate_now"), get("rate_yr_ago"))
    }
    moe <- get(paste0(cfg$col, "_moe"))
    if (level == "tract" && !is.na(v) && !is.na(moe)) vt <- sprintf("%s ± %.1f", vt, moe)
    if (level == "tract" && isTRUE(get(paste0("noisy_", cfg$col)))) {
      noisy_labels <<- c(noisy_labels, cfg$label)
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
  standing <- if (level == "county" && !is.null(vintage)) .migration_line(a, vintage) else ""
  tg$div(
    style = sprintf("background:%s;color:%s;border-radius:8px;padding:14px 16px;margin-top:12px;font-size:12px;",
                    c$page, c$text),
    tg$div(style = sprintf("color:%s;font-weight:700;font-size:16px;", c$accent), title),
    tg$div(style = sprintf("color:%s;margin-bottom:%s;", c$muted,
                           if (nzchar(standing)) "2px" else "8px"), sub),
    if (nzchar(standing)) tg$div(style = sprintf("color:%s;margin-bottom:8px;", c$muted),
                                 standing),
    tg$div(style = "overflow-x:auto;", tg$table(
      style = "border-collapse:collapse;width:100%;",
      tg$tr(style = sprintf("color:%s;text-align:left;", c$muted), lapply(head, tg$th)),
      rows)),
    if (level == "county" && !is.null(vintage)) .migration_panel(a, vintage),
    if (level == "county") .industry_panel(a, attr(county_layers, "sector_weights")),
    if (length(noisy_labels)) tg$div(
      style = sprintf("color:%s;margin-top:8px;", c$warn),
      sprintf("Dashed outline: the margin of error is larger than the value for %s.",
              paste(noisy_labels, collapse = ", "))),
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
