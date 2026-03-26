# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis.R — Data-driven Overview synopsis panel
# Computes correlations, lags, trend stats from live FRED data
# ─────────────────────────────────────────────────────────────────────────────

# ── Helpers ───────────────────────────────────────────────────────────────────

# Rolling correlation between two numeric vectors (same length)
roll_cor <- function(x, y, n = 36) {
  if (length(x) < n || length(y) < n) return(NA_real_)
  x <- tail(x, n); y <- tail(y, n)
  if (sd(x, na.rm=TRUE) == 0 || sd(y, na.rm=TRUE) == 0) return(NA_real_)
  cor(x, y, use = "pairwise.complete.obs")
}

# Lag-correlation: correlate x with y shifted by `lag` periods
lag_cor <- function(x, y, lag = 0) {
  n <- length(x)
  if (lag >= n || lag < 0) return(NA_real_)
  cor(x[(lag+1):n], y[1:(n-lag)], use = "pairwise.complete.obs")
}

# Find lag (in months) that maximises |correlation| between two series
best_lag <- function(x, y, max_lag = 18) {
  cors <- vapply(0:max_lag, function(l) lag_cor(x, y, l), numeric(1))
  best <- which.max(abs(cors))
  list(lag = best - 1L, r = cors[best])
}

# Simple linear trend over last n observations: positive/negative/flat
trend_dir <- function(vals, n = 12) {
  v <- tail(na.omit(vals), n)
  if (length(v) < 4) return("unclear")
  fit <- lm(v ~ seq_along(v))
  slope <- coef(fit)[2]
  se    <- summary(fit)$coefficients[2, 2]
  if (abs(slope) < se) return("flat")
  if (slope > 0)       return("rising")
  return("falling")
}

# Format correlation with strength label
fmt_cor <- function(r) {
  if (is.na(r)) return("N/A")
  lbl <- if (abs(r) > 0.75) "strong" else if (abs(r) > 0.5) "moderate" else "weak"
  dir <- if (r > 0) "positive" else "negative"
  sprintf("r \u2248 %.2f (%s %s)", r, lbl, dir)
}

# Arrow indicator
arrow <- function(dir) switch(dir,
  "rising"  = "\u2191",
  "falling" = "\u2193",
  "flat"    = "\u2192",
  "\u2014"
)

# Colour for trend direction
trend_col <- function(dir, higher_good = TRUE) {
  if (dir == "rising")  return(if (higher_good) "#2dce89" else "#e94560")
  if (dir == "falling") return(if (higher_good) "#e94560" else "#2dce89")
  "#9aa3b2"
}

# Format distance from target
pct_from_target <- function(val, target) {
  if (is.na(val) || is.na(target)) return(NULL)
  diff <- val - target
  list(
    diff  = diff,
    above = diff > 0,
    str   = sprintf("%+.1f pp %s 2%% target", diff, if (diff > 0) "above" else "below")
  )
}

# ── Main synopsis builder ──────────────────────────────────────────────────────

build_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  # ── Pull aligned series ──────────────────────────────────────────────────────
  get_vals <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 12) return(NULL)
    df %>% arrange(date) %>% pull(value)
  }
  get_df <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 12) return(NULL)
    df %>% arrange(date)
  }

  unemp    <- get_vals("UNRATE")
  payrolls <- get_vals("PAYEMS")
  cpi      <- get_vals("CPIAUCSL")
  fed      <- get_vals("FEDFUNDS")
  t10      <- get_vals("DGS10")
  t2       <- get_vals("DGS2")
  mort     <- get_vals("MORTGAGE30US")
  houst    <- get_vals("HOUST")
  retail   <- get_vals("RSAFS")
  vix      <- get_vals("VIXCLS")
  oil      <- get_vals("DCOILWTICO")

  blocks <- list()

  # ── 1. Labor Market ──────────────────────────────────────────────────────────
  unemp_trend  <- if (!is.null(unemp))    trend_dir(unemp,    12) else "unclear"
  payroll_trend<- if (!is.null(payrolls)) trend_dir(diff(payrolls), 12) else "unclear"

  unemp_6m_ago <- if (!is.null(unemp) && length(unemp) >= 6) unemp[length(unemp)-6] else NA
  unemp_chg_6m <- if (!is.na(unemp_6m_ago)) round(kpis$unemp_rate - unemp_6m_ago, 1) else NA

  unemp_yr_ago <- if (!is.null(unemp) && length(unemp) >= 12) unemp[length(unemp)-12] else NA
  unemp_chg_yr <- if (!is.na(unemp_yr_ago)) round(kpis$unemp_rate - unemp_yr_ago, 1) else NA

  avg_payrolls_3m <- if (!is.null(payrolls) && length(payrolls) >= 4)
    round(mean(diff(tail(payrolls, 4)), na.rm=TRUE), 0) else NA

  blocks[["labor"]] <- list(
    title  = "Labor Market",
    color  = "#00b4d8",
    icon   = "users",
    stats  = list(
      list(label="Unemployment trend (12M)",
           value=sprintf("%s %.1f%%", arrow(unemp_trend), kpis$unemp_rate %||% NA),
           col=trend_col(unemp_trend, higher_good=FALSE)),
      list(label="Change vs. 6 months ago",
           value=if (!is.na(unemp_chg_6m)) sprintf("%+.1f pp", unemp_chg_6m) else "N/A",
           col=if (!is.na(unemp_chg_6m) && unemp_chg_6m > 0) "#e94560" else "#2dce89"),
      list(label="Change vs. 1 year ago",
           value=if (!is.na(unemp_chg_yr)) sprintf("%+.1f pp", unemp_chg_yr) else "N/A",
           col=if (!is.na(unemp_chg_yr) && unemp_chg_yr > 0) "#e94560" else "#2dce89"),
      list(label="Avg. payrolls (last 3M)",
           value=if (!is.na(avg_payrolls_3m)) sprintf("%+.0fK/mo", avg_payrolls_3m) else "N/A",
           col=if (!is.na(avg_payrolls_3m) && avg_payrolls_3m > 0) "#2dce89" else "#e94560")
    ),
    note = {
      note_parts <- c()
      if (!is.na(avg_payrolls_3m))
        note_parts <- c(note_parts, sprintf(
          "3-month average payroll growth of %+.0fK/mo suggests a labor market that is %s.",
          avg_payrolls_3m,
          if (avg_payrolls_3m > 200) "running hot" else if (avg_payrolls_3m > 100) "cooling but solid" else "slowing materially"
        ))
      if (!is.null(unemp) && !is.null(payrolls)) {
        rc <- roll_cor(unemp[1:min(length(unemp),length(payrolls))],
                       diff(c(NA, payrolls))[1:min(length(unemp),length(payrolls))], 36)
        if (!is.na(rc))
          note_parts <- c(note_parts, sprintf(
            "36-month rolling correlation between unemployment level and payroll growth: %s — historically inverse as expected.",
            fmt_cor(rc)
          ))
      }
      paste(note_parts, collapse=" ")
    }
  )

  # ── 2. Inflation ─────────────────────────────────────────────────────────────
  cpi_yoy_now  <- kpis$cpi_yoy  %||% NA
  core_yoy_now <- kpis$core_cpi_yoy %||% NA
  cpi_target   <- 2.0

  cpi_trend <- if (!is.null(cpi)) {
    yoy_series <- (cpi / lag(cpi, 12) - 1) * 100
    yoy_series <- yoy_series[!is.na(yoy_series)]
    trend_dir(yoy_series, 6)
  } else "unclear"

  # CPI peak (last 3 years)
  cpi_peak <- if (!is.null(cpi) && length(cpi) >= 36) {
    recent_yoy <- zoo::rollapply(cpi, 13, function(x) (x[13]/x[1]-1)*100, fill=NA, align="right")
    recent_yoy <- recent_yoy[!is.na(recent_yoy)]
    if (length(recent_yoy) >= 36) round(max(tail(recent_yoy, 36)), 1) else NA
  } else NA

  dist <- pct_from_target(cpi_yoy_now, cpi_target)

  blocks[["inflation"]] <- list(
    title  = "Inflation",
    color  = "#e94560",
    icon   = "fire",
    stats  = list(
      list(label="CPI YoY trend (6M)",
           value=sprintf("%s %.1f%%", arrow(cpi_trend), cpi_yoy_now),
           col=trend_col(cpi_trend, higher_good=FALSE)),
      list(label="vs. Fed 2% target",
           value=if (!is.null(dist)) dist$str else "N/A",
           col=if (!is.null(dist) && dist$above) "#e94560" else "#2dce89"),
      list(label="Core CPI YoY",
           value=if (!is.na(core_yoy_now)) sprintf("%.1f%%", core_yoy_now) else "N/A",
           col=if (!is.na(core_yoy_now) && core_yoy_now > 3) "#e94560" else "#f4a261"),
      list(label="3-yr CPI peak",
           value=if (!is.na(cpi_peak)) sprintf("%.1f%%", cpi_peak) else "N/A",
           col="#9aa3b2")
    ),
    note = {
      note_parts <- c()
      if (!is.na(cpi_yoy_now) && !is.na(core_yoy_now)) {
        gap <- round(cpi_yoy_now - core_yoy_now, 1)
        note_parts <- c(note_parts, sprintf(
          "Headline CPI runs %s pp %s core, indicating food & energy %s are %s the overall print.",
          abs(gap),
          if (gap > 0) "above" else "below",
          if (gap > 0) "pressures" else "relief",
          if (gap > 0) "elevating" else "tempering"
        ))
      }
      if (!is.null(fed) && !is.na(cpi_yoy_now)) {
        real_rate <- round(tail(fed, 1) - cpi_yoy_now, 1)
        note_parts <- c(note_parts, sprintf(
          "Real Fed Funds rate (nominal \u2212 CPI): %.1f%% — %s.",
          real_rate,
          if (real_rate > 1.5) "restrictive territory, historically associated with demand cooling"
          else if (real_rate > 0) "mildly positive but below historical neutral"
          else "still negative in real terms, accommodative bias remains"
        ))
      }
      paste(note_parts, collapse=" ")
    }
  )

  # ── 3. Rates & Yield Curve ───────────────────────────────────────────────────
  spread_now  <- kpis$t10y2y   %||% NA
  t10_now     <- kpis$t10yr    %||% NA
  mort_now    <- kpis$mortgage30 %||% NA

  # Count months inverted in last 24
  months_inverted <- if (!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 24)
    sum((tail(t10, n) - tail(t2, n)) < 0, na.rm=TRUE)
  } else NA

  # Mort-Housing lag correlation
  mort_houst_lag <- if (!is.null(mort) && !is.null(houst)) {
    # Align lengths
    n <- min(length(mort), length(houst))
    best_lag(tail(mort, n), tail(houst, n), max_lag = 12)
  } else NULL

  mort_trend <- if (!is.null(mort)) trend_dir(mort, 12) else "unclear"

  blocks[["rates"]] <- list(
    title  = "Rates & Yield Curve",
    color  = "#f4a261",
    icon   = "percent",
    stats  = list(
      list(label="10Y-2Y spread",
           value=if (!is.na(spread_now)) sprintf("%.2f pp (%s)", spread_now,
                 if (spread_now < 0) "INVERTED" else "normal") else "N/A",
           col=if (!is.na(spread_now) && spread_now < 0) "#e94560" else "#2dce89"),
      list(label="Months inverted (last 24M)",
           value=if (!is.na(months_inverted)) sprintf("%d of 24 months", months_inverted) else "N/A",
           col=if (!is.na(months_inverted) && months_inverted > 12) "#e94560"
               else if (!is.na(months_inverted) && months_inverted > 6) "#f4a261"
               else "#2dce89"),
      list(label="30-yr mortgage trend",
           value=sprintf("%s %.2f%%", arrow(mort_trend), mort_now %||% NA),
           col=trend_col(mort_trend, higher_good=FALSE)),
      list(label="Mortgage–Housing starts lag",
           value=if (!is.null(mort_houst_lag))
             sprintf("%d-mo lag, %s", mort_houst_lag$lag, fmt_cor(mort_houst_lag$r)) else "N/A",
           col="#9aa3b2")
    ),
    note = {
      note_parts <- c()
      if (!is.na(months_inverted) && months_inverted > 0)
        note_parts <- c(note_parts, sprintf(
          "Yield curve has been inverted for %d of the past 24 months — a historically reliable (if lagged) recession signal, though timing varies widely.",
          months_inverted
        ))
      if (!is.null(mort_houst_lag) && !is.na(mort_houst_lag$r))
        note_parts <- c(note_parts, sprintf(
          "Mortgage rate leads housing starts by ~%d months (lag-adjusted %s, R\u00B2 \u2248 %.0f%%). Rate moves today are a forward indicator for construction activity.",
          mort_houst_lag$lag,
          fmt_cor(mort_houst_lag$r),
          mort_houst_lag$r^2 * 100
        ))
      paste(note_parts, collapse=" ")
    }
  )

  # ── 4. Markets & Risk ────────────────────────────────────────────────────────
  vix_now   <- kpis$vix   %||% NA
  oil_now   <- kpis$oil_price %||% NA

  vix_trend  <- if (!is.null(vix)) trend_dir(vix, 12) else "unclear"
  vix_6m_avg <- if (!is.null(vix) && length(vix) >= 6)
    round(mean(tail(vix, 6), na.rm=TRUE), 1) else NA
  vix_1y_avg <- if (!is.null(vix) && length(vix) >= 12)
    round(mean(tail(vix, 12), na.rm=TRUE), 1) else NA

  oil_trend <- if (!is.null(oil)) trend_dir(oil, 12) else "unclear"
  oil_6m_ago <- if (!is.null(oil) && length(oil) >= 130)
    oil[length(oil)-130] else NA  # daily ~6 months
  oil_chg_pct <- if (!is.na(oil_6m_ago) && oil_6m_ago > 0)
    round((oil_now / oil_6m_ago - 1) * 100, 1) else NA

  # VIX–CPI correlation
  vix_cpi_cor <- if (!is.null(vix) && !is.null(cpi)) {
    n   <- min(length(vix), length(cpi))
    yoy <- (tail(cpi, n) / lag(tail(cpi, n), 12) - 1) * 100
    roll_cor(tail(vix, n), yoy, 36)
  } else NA

  blocks[["markets"]] <- list(
    title  = "Markets & Risk",
    color  = "#7c5cbf",
    icon   = "chart-bar",
    stats  = list(
      list(label="VIX regime",
           value=sprintf("%.1f (%s)", vix_now,
                 if (!is.na(vix_now) && vix_now > 30) "elevated fear"
                 else if (!is.na(vix_now) && vix_now > 20) "cautious"
                 else "complacent"),
           col=if (!is.na(vix_now) && vix_now > 25) "#e94560"
               else if (!is.na(vix_now) && vix_now > 18) "#f4a261" else "#2dce89"),
      list(label="VIX: 6M avg vs 1Y avg",
           value=if (!is.na(vix_6m_avg) && !is.na(vix_1y_avg))
             sprintf("%.1f vs %.1f (%s)", vix_6m_avg, vix_1y_avg,
                     if (vix_6m_avg > vix_1y_avg) "stress rising" else "stress falling") else "N/A",
           col=if (!is.na(vix_6m_avg) && !is.na(vix_1y_avg) && vix_6m_avg > vix_1y_avg)
             "#e94560" else "#2dce89"),
      list(label="WTI crude trend",
           value=sprintf("%s $%.0f/bbl", arrow(oil_trend), oil_now %||% NA),
           col=trend_col(oil_trend, higher_good=FALSE)),
      list(label="VIX\u2013Inflation correlation (36M)",
           value=if (!is.na(vix_cpi_cor)) fmt_cor(vix_cpi_cor) else "N/A",
           col="#9aa3b2")
    ),
    note = {
      note_parts <- c()
      if (!is.na(vix_now))
        note_parts <- c(note_parts, sprintf(
          "VIX at %.1f places equity vol in %s — %s.",
          vix_now,
          if (vix_now > 30) "the upper quartile historically (>30 = stress event)" else if (vix_now > 20) "a cautious zone (20–30)" else "a low-vol regime (<20)",
          if (vix_now > 30) "historically associated with risk-off positioning and tighter credit conditions"
          else if (vix_now > 20) "watch for vol regime shift"
          else "market is pricing low near-term risk; watch for mean-reversion"
        ))
      if (!is.na(oil_chg_pct))
        note_parts <- c(note_parts, sprintf(
          "WTI crude %s%.0f%% over the past 6 months — a %s for headline CPI via energy component.",
          if (oil_chg_pct > 0) "+" else "", oil_chg_pct,
          if (abs(oil_chg_pct) > 10) "meaningful impulse" else "modest move"
        ))
      paste(note_parts, collapse=" ")
    }
  )

  blocks
}

# ── HTML renderer ──────────────────────────────────────────────────────────────

render_synopsis_html <- function(blocks) {
  if (is.null(blocks) || length(blocks) == 0) {
    return(HTML("<p style='color:#6b7585;padding:16px;'>Synopsis not yet available — data loading.</p>"))
  }

  # Render one block
  render_block <- function(b) {
    stats_html <- paste(vapply(b$stats, function(s) {
      sprintf(
        "<div style='display:flex;justify-content:space-between;align-items:baseline;
                     padding:4px 0;border-bottom:1px solid #2a3042;'>
           <span style='color:#9aa3b2;font-size:11px;'>%s</span>
           <span style='font-size:13px;font-weight:600;color:%s;'>%s</span>
         </div>",
        htmltools::htmlEscape(s$label),
        s$col,
        htmltools::htmlEscape(as.character(s$value))
      )
    }, character(1)), collapse="")

    note_html <- if (nchar(b$note) > 0) sprintf(
      "<div style='margin-top:10px;padding:10px 12px;background:#0f1117;border-radius:6px;
                   border-left:3px solid %s;'>
         <span style='color:#9aa3b2;font-size:11px;line-height:1.7;'>%s</span>
       </div>",
      b$color,
      htmltools::htmlEscape(b$note)
    ) else ""

    sprintf(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                   border-top:3px solid %s;padding:14px 16px;height:100%%;'>
         <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                     letter-spacing:1px;margin-bottom:10px;'>
           <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>%s
         </div>
         %s
         %s
       </div>",
      b$color, b$color, b$icon, b$title,
      stats_html, note_html
    )
  }

  block_htmls <- vapply(names(blocks), function(nm) render_block(blocks[[nm]]), character(1))

  # Two per row
  rows_html <- paste(vapply(seq(1, length(block_htmls), by=2), function(i) {
    left  <- block_htmls[i]
    right <- if (i+1 <= length(block_htmls)) block_htmls[i+1] else
      "<div style='flex:1;'></div>"
    sprintf(
      "<div style='display:flex;gap:14px;margin-bottom:14px;'>
         <div style='flex:1;'>%s</div>
         <div style='flex:1;'>%s</div>
       </div>",
      left, right
    )
  }, character(1)), collapse="")

  ts <- format(Sys.time(), "%b %d, %Y %H:%M")
  header <- sprintf(
    "<div style='display:flex;justify-content:space-between;align-items:center;
                 margin-bottom:14px;'>
       <span style='color:#e0e0e0;font-size:13px;font-weight:700;text-transform:uppercase;
                    letter-spacing:1px;'>
         <i class=\"fa fa-chart-line\" style=\"color:#00b4d8;margin-right:8px;\"></i>
         Live Data Synopsis
       </span>
       <span style='color:#6b7585;font-size:11px;'>Computed %s</span>
     </div>",
    ts
  )

  HTML(paste0(
    "<div style='padding:4px 2px;'>",
    header,
    rows_html,
    "<div style='color:#6b7585;font-size:10px;margin-top:4px;'>",
    "All statistics computed from live FRED data. Correlations use trailing windows as noted.",
    "</div></div>"
  ))
}
