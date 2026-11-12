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

.county_tooltips <- function(d, id, cfg, asof) {
  vtxt <- .risk_fmt(d[[cfg$col]], cfg$fmt)
  if (id == "stress_now") {
    has_flag <- !is.na(vtxt) & !is.na(d$flag)
    vtxt[has_flag] <- paste0(vtxt[has_flag], " · ", d$flag[has_flag])
  }
  ranks <- rank_label(d[[paste0("pct_", cfg$col)]], "nationally", cfg$higher_is_worse)
  subs <- ifelse(is.na(d$cbsa_name), NA_character_, paste(d$cbsa_name, d$cbsa_type))
  footer <- if (is.na(asof)) paste(cfg$source, "— unavailable") else
    paste0(cfg$source, ", ", asof)
  lapply(seq_len(nrow(d)), function(i) risk_tooltip_html(
    title = d$name[i], subtitle = subs[i], value_text = vtxt[i],
    rank_text = ranks[i], footer = footer))
}

.legend_title <- function(cfg, status, asof) {
  if (identical(status, "unavailable")) return(paste(cfg$label, "— unavailable"))
  t <- paste0(cfg$label, " — ", cfg$source, ", ", asof)
  if (identical(status, "stale")) t <- paste0(t, " (stale)")
  t
}

.add_county_polygons <- function(map, county_layers, layer_id, fill_opacity = 0.82) {
  cfg <- RISK_LAYERS[[layer_id]]
  vals <- as.numeric(county_layers[[cfg$col]])
  pal <- .risk_palette(cfg, vals)
  asof <- attr(county_layers, "asof")[[layer_id]]
  status <- attr(county_layers, "layer_status")[[layer_id]]
  lbl_fmt <- switch(cfg$fmt,
    "$"  = leaflet::labelFormat(prefix = "$", big.mark = ","),
    "pp" = leaflet::labelFormat(suffix = " pp"),
    leaflet::labelFormat(suffix = "%"))
  map |>
    leaflet::clearGroup("counties") |>
    leaflet::removeControl("risk_legend") |>
    leaflet::addPolygons(
      data = county_layers, layerId = county_layers$GEOID, group = "counties",
      fillColor = pal(vals), fillOpacity = fill_opacity,
      color = "#0f1117", weight = 0.4, smoothFactor = 0.5,
      label = .county_tooltips(county_layers, layer_id, cfg, asof),
      labelOptions = leaflet::labelOptions(
        style = list(background = "transparent", border = "none", padding = "0",
                     "box-shadow" = "none"), direction = "auto"),
      highlightOptions = leaflet::highlightOptions(color = .RISK_COL$accent, weight = 2,
                                                   bringToFront = FALSE)
    ) |>
    leaflet::addLegend(pal = pal, values = vals, opacity = 0.9, layerId = "risk_legend",
                       title = .legend_title(cfg, status, asof), position = "bottomright",
                       labFormat = lbl_fmt)
}

# ── Tract drill-down layer ───────────────────────────────────────────────────
.tract_tooltips <- function(t, cfg, asof, noisy) {
  vtxt <- .risk_fmt(as.numeric(t[[cfg$col]]), cfg$fmt)
  lm <- rank_label(t[[paste0("lm_pct_", cfg$col)]], t$frame_label, cfg$higher_is_worse)
  st <- rank_label(t[[paste0("st_pct_", cfg$col)]], t$state_label, cfg$higher_is_worse)
  footer <- paste0(cfg$source, ", ", asof, ifelse(noisy, " · High margin of error", ""))
  lapply(seq_len(nrow(t)), function(i) risk_tooltip_html(
    title = t$tract_name[i], value_text = vtxt[i], rank_text = lm[i],
    rank_sub = st[i], footer = footer[i]))
}

#' Dim counties and draw the frame's tracts for a tract-level layer. For a
#' county-only layer (Stress now) the counties are restored and no tracts drawn.
add_tract_layer <- function(map, tracts, layer_id, county_layers) {
  cfg <- RISK_LAYERS[[layer_id]]
  map <- leaflet::clearGroup(map, "tracts")
  if (!"tract" %in% cfg$levels || is.null(tracts) || nrow(tracts) == 0) {
    return(.add_county_polygons(map, county_layers, layer_id, 0.82))
  }
  asof <- attr(county_layers, "asof")[[layer_id]]
  vals <- as.numeric(tracts[[cfg$col]])
  pal <- .risk_palette(cfg, vals)
  noisy_col <- paste0("noisy_", cfg$col)
  noisy <- if (noisy_col %in% names(tracts)) tracts[[noisy_col]] %in% TRUE else
    rep(FALSE, nrow(tracts))
  bb <- as.numeric(sf::st_bbox(tracts))
  lbl_fmt <- if (cfg$fmt == "$") leaflet::labelFormat(prefix = "$", big.mark = ",") else
    leaflet::labelFormat(suffix = "%")

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
                       title = sprintf("%s — tracts %s — %s, %s", cfg$label,
                                       tracts$frame_label[1], cfg$source, asof)) |>
    leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
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
  rk <- function(p, hiw) { r <- rank_label(p, "", hiw); if (is.na(r)) "—" else trimws(r) }
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
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
    .add_county_polygons(county_layers, layer_id, fill_opacity) |>
    leaflet::setView(lng = -96, lat = 38, zoom = 4)
}
