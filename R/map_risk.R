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
risk_tooltips_vec <- function(title, subtitle = NA, value_text, rank_text = NA,
                              rank_sub = NA, footer, drivers_html = "", rank_bad = TRUE) {
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
    sprintf('<div style="color:%s;font-size:11px;margin-top:4px;">%s</div>', c$muted,
            .tt_esc(footer)),
    '</div>')
  lapply(out, htmltools::HTML)
}

#' Single tooltip. drivers: data.frame(label, value_text, worse) — first 3 shown.
risk_tooltip_html <- function(title, subtitle = NULL, value_text, rank_text = NULL,
                              rank_sub = NULL, drivers = NULL, footer) {
  c <- .RISK_COL
  na1 <- function(x) if (is.null(x) || length(x) == 0) NA else x
  drivers_html <- ""
  if (!is.null(drivers) && nrow(drivers) > 0) {
    d <- utils::head(drivers, 3)
    rows <- sprintf(paste0('<div style="display:flex;justify-content:space-between;gap:8px;">',
                           '<span>%s</span><span style="font-weight:600;color:%s;">%s</span></div>'),
                    .tt_esc(d$label), ifelse(d$worse %in% TRUE, c$worse, c$text),
                    .tt_esc(d$value_text))
    drivers_html <- paste0(
      sprintf('<hr style="border:0;border-top:1px solid %s;margin:7px 0;"/>', c$border),
      sprintf('<div style="color:%s;font-size:11px;">Driven by</div>', c$muted),
      paste(rows, collapse = ""))
  }
  risk_tooltips_vec(title, na1(subtitle), na1(value_text), na1(rank_text), na1(rank_sub),
                    footer, drivers_html)[[1]]
}

# ── Layer registry ───────────────────────────────────────────────────────────
RISK_LAYERS <- c(
  list(stress_now = list(
    label = "Stress now: unemployment vs. a year ago", col = "momentum_pp", fmt = "pp",
    pal = "RdBu", rev = TRUE, higher_is_worse = TRUE, source = "BLS LAUS",
    levels = "county")),
  stats::setNames(lapply(names(VAR_CFG), function(id) {
    v <- VAR_CFG[[id]]
    list(label = v$label, col = id, fmt = v$fmt, pal = v$pal, rev = v$rev,
         higher_is_worse = v$rev, source = "Census ACS 5-yr", levels = c("county", "tract"))
  }), names(VAR_CFG))
)

.risk_fmt <- function(v, fmt) {
  out <- switch(fmt,
    "$"  = paste0("$", formatC(round(v), format = "d", big.mark = ",")),
    "pp" = sprintf("%+.1f pp", v),
    sprintf("%.1f%%", v))
  out[is.na(v)] <- NA_character_
  out
}

.risk_palette <- function(cfg, vals) {
  if (cfg$fmt == "pp") {
    return(leaflet::colorBin(cfg$pal, domain = c(-50, 50), reverse = cfg$rev,
                             bins = c(-50, -1, -0.5, -0.2, 0.2, 0.5, 1, 50),
                             na.color = "#555566"))
  }
  brks <- unique(stats::quantile(vals, seq(0, 1, length.out = 8), na.rm = TRUE))
  if (length(brks) < 3) brks <- 7
  leaflet::colorBin(cfg$pal, domain = vals, bins = brks, reverse = cfg$rev,
                    na.color = "#555566")
}

.county_tooltips <- function(d, id, cfg, asof, status = "ok") {
  if (identical(status, "loading")) {
    return(risk_tooltips_vec(title = d$name, value_text = "Loading…",
                             footer = paste(cfg$source, "— loading")))
  }
  vtxt <- .risk_fmt(d[[cfg$col]], cfg$fmt)
  if (id == "stress_now") {
    has_flag <- !is.na(vtxt) & !is.na(d$flag)
    vtxt[has_flag] <- paste0(vtxt[has_flag], " · ", d$flag[has_flag])
  }
  pct <- d[[paste0("pct_", cfg$col)]]
  subs <- ifelse(is.na(d$cbsa_name), NA_character_, paste(d$cbsa_name, d$cbsa_type))
  footer <- if (is.na(asof)) paste(cfg$source, "— unavailable") else
    paste0(cfg$source, ", ", asof)
  risk_tooltips_vec(title = d$name, subtitle = subs, value_text = vtxt,
                    rank_text = rank_label(pct, "nationally"), footer = footer,
                    rank_bad = .rank_bad(pct, cfg))
}

#' Ranks always read in value terms ("Top" = highest value); the pill turns red
#' only on the bad side of the median for that variable.
.rank_bad <- function(pct, cfg) {
  bad <- if (isTRUE(cfg$higher_is_worse)) pct >= 50 else pct < 50
  bad %in% TRUE
}

.legend_title <- function(cfg, status, asof, scope = NULL) {
  head <- htmltools::htmlEscape(cfg$label)
  sub <- if (identical(status, "unavailable")) "unavailable" else
    if (identical(status, "loading")) "loading county unemployment from BLS…" else
      paste0(if (!is.null(scope)) paste0(scope, " · "), cfg$source, ", ", asof,
             if (identical(status, "stale")) " (stale)",
             if (identical(status, "partial")) " (partial: some counties missing, retrying later)")
  paste0(head, "<br><small style='font-weight:400;'>", htmltools::htmlEscape(sub), "</small>")
}

.legend_format <- function(cfg) {
  switch(cfg$fmt,
    "$"  = leaflet::labelFormat(prefix = "$", big.mark = ",", digits = 0),
    "pp" = leaflet::labelFormat(suffix = " pp", digits = 1),
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
    if (cfg$fmt != "pp") {
      return(leaflet::addLegend(m, pal = pal, values = vals, opacity = 0.9,
                                layerId = "risk_legend", title = title,
                                position = "bottomright", labFormat = lbl_fmt))
    }
    mids <- c(-2, -0.75, -0.35, 0, 0.35, 0.75, 2)
    leaflet::addLegend(m, colors = pal(mids), opacity = 0.9, layerId = "risk_legend",
                       title = title, position = "bottomright",
                       labels = c("below −1.0 pp", "−1.0 to −0.5", "−0.5 to −0.2",
                                  "−0.2 to +0.2", "+0.2 to +0.5", "+0.5 to +1.0 (rising)",
                                  "above +1.0 pp (trigger)"))
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
                    rank_sub = st, footer = footer, rank_bad = .rank_bad(lm_pct, cfg))
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
      vt <- sprintf("%s · %s (%.1f%% now vs %.1f%% a year ago)", vt, get("flag"),
                    get("rate_now"), get("rate_yr_ago"))
    }
    moe <- get(paste0(cfg$col, "_moe"))
    if (level == "tract" && !is.na(v) && !is.na(moe)) vt <- sprintf("%s ± %.1f", vt, moe)
    if (level == "tract" && isTRUE(get(paste0("noisy_", cfg$col)))) {
      noisy_labels <<- c(noisy_labels, cfg$label)
    }
    when <- if (identical(status[[id]], "unavailable")) "unavailable" else asof[[id]]
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

  tg$div(
    style = sprintf("background:%s;color:%s;border-radius:8px;padding:14px 16px;margin-top:12px;font-size:12px;",
                    c$page, c$text),
    tg$div(style = sprintf("color:%s;font-weight:700;font-size:16px;", c$accent), title),
    tg$div(style = sprintf("color:%s;margin-bottom:8px;", c$muted), sub),
    tg$div(style = "overflow-x:auto;", tg$table(
      style = "border-collapse:collapse;width:100%;",
      tg$tr(style = sprintf("color:%s;text-align:left;", c$muted), lapply(head, tg$th)),
      rows)),
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
