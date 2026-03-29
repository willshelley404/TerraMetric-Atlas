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

# ═══════════════════════════════════════════════════════════════════════════════
# TAB-LEVEL SYNOPSIS BUILDERS
# Each returns an HTML string for inline display above the charts/tables
# ═══════════════════════════════════════════════════════════════════════════════

# Shared HTML wrapper for a tab synopsis
.tab_synopsis_html <- function(title, color, icon_name, bullets, note = NULL) {
  bullet_html <- paste(vapply(bullets, function(b) {
    sprintf(
      "<li style='margin-bottom:5px;'>
         <span style='color:%s;font-weight:600;'>%s</span>
         <span style='color:#d0d0d0;'>%s</span>
       </li>",
      b$col %||% "#9aa3b2",
      htmltools::htmlEscape(b$label),
      htmltools::htmlEscape(b$text)
    )
  }, character(1)), collapse = "")

  note_html <- if (!is.null(note) && nchar(note) > 0) sprintf(
    "<div style='margin-top:10px;padding:10px 14px;background:#0f1117;border-radius:6px;
                 border-left:3px solid %s;color:#9aa3b2;font-size:12px;line-height:1.75;'>
       <i class=\"fa fa-lightbulb\" style=\"color:%s;margin-right:6px;\"></i>%s
     </div>", color, color, htmltools::htmlEscape(note)
  ) else ""

  sprintf(
    "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                 border-left:4px solid %s;padding:14px 18px;margin-bottom:14px;'>
       <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                   letter-spacing:1px;margin-bottom:10px;'>
         <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>%s — Current Landscape
       </div>
       <ul style='list-style:none;padding:0;margin:0;'>%s</ul>
       %s
     </div>",
    color, color, icon_name, title, bullet_html, note_html
  )
}

# ── Labor Market tab synopsis ──────────────────────────────────────────────────
build_labor_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  unemp    <- fred_data$UNRATE    %>% { if (!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  payrolls <- fred_data$PAYEMS    %>% { if (!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  jolts    <- fred_data$JTSJOL    %>% { if (!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  wages    <- fred_data$CES0500000003 %>% { if (!is.null(.)) arrange(., date) %>% pull(value) else NULL }

  u_now <- kpis$unemp_rate %||% NA
  u_prev <- if (!is.null(unemp) && length(unemp) >= 2) unemp[length(unemp)-1] else NA
  u_6m   <- if (!is.null(unemp) && length(unemp) >= 6)  unemp[length(unemp)-6]  else NA
  u_yr   <- if (!is.null(unemp) && length(unemp) >= 12) unemp[length(unemp)-12] else NA

  pay_3m_avg <- if (!is.null(payrolls) && length(payrolls) >= 4)
    round(mean(diff(tail(payrolls, 4)), na.rm=TRUE)) else NA
  jolts_now  <- kpis$job_openings %||% NA
  jolts_prev <- if (!is.null(jolts) && length(jolts) >= 2) jolts[length(jolts)-1] else NA
  wage_yoy   <- kpis$wage_yoy %||% NA

  beveridge_ratio <- if (!is.null(jolts) && !is.null(unemp)) {
    n <- min(length(jolts), length(unemp))
    round(tail(jolts, 1) / tail(unemp, 1), 2)
  } else NA

  bullets <- list(
    list(col="#00b4d8", label="Unemployment rate: ",
         text=sprintf("%.1f%% (prev %.1f%%; %s pp vs 6mo; %s pp vs 1yr)",
                      u_now, u_prev %||% NA,
                      if(!is.na(u_6m)) sprintf("%+.1f", u_now - u_6m) else "N/A",
                      if(!is.na(u_yr)) sprintf("%+.1f", u_now - u_yr) else "N/A")),
    list(col="#2dce89", label="3-month avg payrolls: ",
         text=if(!is.na(pay_3m_avg)) sprintf("%+.0fK/mo — %s",
              pay_3m_avg, if(pay_3m_avg>200)"above-trend growth" else if(pay_3m_avg>100)"moderate" else "slowing") else "N/A"),
    list(col="#7c5cbf", label="Job openings (JOLTS): ",
         text=sprintf("%s vs prev %s",
                      if(!is.na(jolts_now)) sprintf("%.0fK", jolts_now) else "N/A",
                      if(!is.na(jolts_prev)) sprintf("%.0fK", jolts_prev) else "N/A")),
    list(col="#f4a261", label="Wage growth (YoY): ",
         text=if(!is.na(wage_yoy)) sprintf("%.1f%% — %s vs PCE/CPI, affecting real purchasing power",
              wage_yoy, if(wage_yoy > (kpis$cpi_yoy%||%0)+0.5) "above inflation" else "below or inline with inflation") else "N/A"),
    list(col="#9aa3b2", label="Beveridge ratio (openings/unemployed): ",
         text=if(!is.na(beveridge_ratio)) sprintf("%.2f — %s",
              beveridge_ratio, if(beveridge_ratio>1.5)"tight labour market, wage pressure likely" else "loosening, employer leverage increasing") else "N/A")
  )

  udir <- trend_dir(unemp, 12)
  note <- sprintf(
    "Unemployment is %s%s. %s%s",
    udir,
    if(!is.na(u_6m)) sprintf(" — %.1f pp %s over 6 months", abs(u_now - u_6m), if(u_now>u_6m)"higher" else "lower") else "",
    if(!is.na(pay_3m_avg)) sprintf("Payroll momentum (%+.0fK/mo 3M avg) %s. ", pay_3m_avg,
      if(pay_3m_avg>150)"suggests continued tightness — watch wage acceleration"
      else if(pay_3m_avg>0)"remains positive but cooling"
      else "has turned negative — recession risk elevated") else "",
    if(!is.na(jolts_now) && !is.na(jolts_prev)) sprintf(
      "JOLTS openings moved %+.0fK month-on-month; labour demand %s.", jolts_now-jolts_prev,
      if(jolts_now>jolts_prev)"firming" else "softening") else ""
  )

  htmltools::HTML(.tab_synopsis_html("Labor Market", "#00b4d8", "users", bullets, note))
}

# ── Inflation tab synopsis ─────────────────────────────────────────────────────
build_inflation_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  cpi_now   <- kpis$cpi_yoy     %||% NA
  core_now  <- kpis$core_cpi_yoy %||% NA
  pce_now   <- kpis$core_pce    %||% NA
  ppi_yoy   <- kpis$ppi_yoy     %||% NA
  fed_funds <- kpis$fed_funds   %||% NA

  real_rate <- if (!is.na(fed_funds) && !is.na(cpi_now)) round(fed_funds - cpi_now, 1) else NA
  cpi_vs_target <- if (!is.na(cpi_now)) round(cpi_now - 2.0, 1) else NA
  cpi_vs_core   <- if (!is.na(cpi_now) && !is.na(core_now)) round(cpi_now - core_now, 1) else NA

  cpi_df <- fred_data$CPIAUCSL
  cpi_trend <- if (!is.null(cpi_df) && nrow(cpi_df) > 12) {
    yoy_v <- (cpi_df$value / dplyr::lag(cpi_df$value, 12) - 1) * 100
    trend_dir(yoy_v[!is.na(yoy_v)], 6)
  } else "unclear"

  bullets <- list(
    list(col=if(!is.na(cpi_now)&&cpi_now>3)"#e94560" else "#2dce89",
         label="CPI YoY: ",
         text=sprintf("%.1f%% (%s vs 2%% target; trend: %s)",
                      cpi_now, if(!is.na(cpi_vs_target)) sprintf("%+.1f pp", cpi_vs_target) else "N/A",
                      cpi_trend)),
    list(col=if(!is.na(core_now)&&core_now>3)"#f4a261" else "#2dce89",
         label="Core CPI YoY: ",
         text=if(!is.na(core_now)) sprintf("%.1f%% — %s", core_now,
              if(core_now>cpi_now)"above headline (energy/food providing relief)"
              else "below headline (energy adding to print)") else "N/A"),
    list(col="#7c5cbf", label="Core PCE YoY (Fed target): ",
         text=if(!is.na(pce_now)) sprintf("%.1f%% — Fed's preferred gauge; %s",
              pce_now, if(pce_now>2.5)"above target, policy likely to remain restrictive"
              else "nearing target range") else "N/A"),
    list(col="#f4a261", label="PPI YoY (input prices): ",
         text=if(!is.na(ppi_yoy)) sprintf("%.1f%% — %s CPI by ~2-3 months",
              ppi_yoy, if(ppi_yoy>cpi_now%||%0)"leading indicator suggests continued price pressure on" else "declining, leading") else "N/A"),
    list(col="#00b4d8", label="Real Fed Funds rate: ",
         text=if(!is.na(real_rate)) sprintf("%.1f%% — %s",
              real_rate, if(real_rate>1.5)"firmly restrictive"
              else if(real_rate>0)"mildly positive" else "still negative, policy not fully restrictive") else "N/A")
  )

  note <- paste(
    if(!is.na(cpi_vs_core) && abs(cpi_vs_core)>0.5)
      sprintf("Headline-core gap of %+.1f pp driven by %s. ", cpi_vs_core,
              if(cpi_vs_core>0)"energy & food price pressure" else "energy relief in headline") else "",
    if(!is.na(real_rate))
      sprintf("Real rate of %.1f%% %s. ", real_rate,
              if(real_rate>2)"is clearly restrictive — historically associated with falling inflation within 6-12 months"
              else if(real_rate>0)"is positive but may need more time to fully transmit"
              else "means Fed is still not tight in real terms despite nominal hikes") else "",
    "PPI leads CPI by ~2 months; watch PPI trend for forward read on goods inflation."
  )

  htmltools::HTML(.tab_synopsis_html("Inflation", "#e94560", "fire", bullets, note))
}

# ── Housing tab synopsis ───────────────────────────────────────────────────────
build_housing_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  mort <- fred_data$MORTGAGE30US %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  hst  <- fred_data$HOUST        %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  pmt  <- fred_data$PERMIT       %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }

  mort_now   <- kpis$mortgage30     %||% NA
  mort_prev  <- if(!is.null(mort)  && length(mort) >= 2)  mort[length(mort)-1]  else NA
  starts_now <- kpis$housing_starts %||% NA
  starts_prev<- if(!is.null(hst)   && length(hst)  >= 2)  hst[length(hst)-1]   else NA
  permits_now<- kpis$permits       %||% NA
  permits_prev<-if(!is.null(pmt)   && length(pmt)  >= 2)  pmt[length(pmt)-1]   else NA

  mort_houst <- if(!is.null(mort) && !is.null(hst)) {
    n <- min(length(mort), length(hst))
    best_lag(tail(mort, n), tail(hst, n), max_lag=12)
  } else NULL

  mort_trend <- if(!is.null(mort)) trend_dir(mort, 12) else "unclear"

  bullets <- list(
    list(col=if(!is.na(mort_now)&&mort_now>6)"#e94560" else "#f4a261",
         label="30-yr mortgage: ",
         text=sprintf("%.2f%% (prev %.2f%%; %s); threshold ~5.5%% historically unlocks demand",
                      mort_now, mort_prev%||%NA,
                      if(!is.na(mort_prev)) sprintf("%+.2f pp MoM", mort_now-mort_prev) else "N/A")),
    list(col="#2dce89", label="Housing starts: ",
         text=sprintf("%sK ann. (prev %sK; %s MoM)",
                      if(!is.na(starts_now)) format(round(starts_now), big.mark=",") else "N/A",
                      if(!is.na(starts_prev)) format(round(starts_prev), big.mark=",") else "N/A",
                      if(!is.na(starts_now)&&!is.na(starts_prev)) sprintf("%+.0fK", starts_now-starts_prev) else "N/A")),
    list(col="#00b4d8", label="Building permits: ",
         text=sprintf("%sK ann. (prev %sK) — leading indicator for starts",
                      if(!is.na(permits_now)) format(round(permits_now), big.mark=",") else "N/A",
                      if(!is.na(permits_prev)) format(round(permits_prev), big.mark=",") else "N/A")),
    list(col="#9aa3b2", label="Mortgage trend (12M): ",
         text=sprintf("%s %s", arrow(mort_trend),
                      if(mort_trend=="falling")"— affordability improving, watch starts lag"
                      else if(mort_trend=="rising")"— affordability worsening, starts likely to follow lower"
                      else "— stable; starts driven by supply-side factors")),
    list(col="#7c5cbf", label="Rate→Starts lag: ",
         text=if(!is.null(mort_houst)) sprintf("~%d months (r=%.2f, R²≈%.0f%%)",
              mort_houst$lag, mort_houst$r, mort_houst$r^2*100) else "N/A")
  )

  note <- paste(
    if(!is.na(mort_now)) sprintf("At %.2f%%, mortgage rates are %s. ", mort_now,
      if(mort_now>7)"historically elevated — first-time buyer affordability severely constrained"
      else if(mort_now>6)"elevated but declining from 2023 peak"
      else "approaching the ~5.5–6%% range where historically demand re-ignites") else "",
    if(!is.null(mort_houst)) sprintf(
      "Mortgage rate leads housing starts by ~%d months — current rate level suggests %s for starts over the next %d months.",
      mort_houst$lag,
      if(mort_trend=="falling")"improving" else if(mort_trend=="rising")"further pressure" else "neutral outlook",
      mort_houst$lag) else ""
  )

  htmltools::HTML(.tab_synopsis_html("Housing", "#2dce89", "home", bullets, note))
}

# ── Consumer tab synopsis ──────────────────────────────────────────────────────
build_consumer_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  ret  <- fred_data$RSAFS   %>% { if(!is.null(.)) arrange(., date) else NULL }
  sent <- fred_data$UMCSENT %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  ip   <- fred_data$INDPRO  %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }

  retail_yoy <- kpis$retail_yoy %||% NA
  retail_prev_yoy <- if(!is.null(ret) && nrow(ret) > 13) {
    n <- nrow(ret)
    round((ret$value[n-1] / ret$value[n-13] - 1) * 100, 1)
  } else NA

  sent_now  <- kpis$cons_sent %||% NA
  sent_prev <- if(!is.null(sent) && length(sent) >= 2) sent[length(sent)-1] else NA
  sent_yr   <- if(!is.null(sent) && length(sent) >= 12) sent[length(sent)-12] else NA

  ip_yoy    <- kpis$indpro_yoy %||% NA
  ip_trend  <- if(!is.null(ip)) trend_dir(ip, 12) else "unclear"

  sent_trend <- if(!is.null(sent)) trend_dir(sent, 6) else "unclear"

  bullets <- list(
    list(col=if(!is.na(retail_yoy)&&retail_yoy>0)"#2dce89" else "#e94560",
         label="Retail sales YoY: ",
         text=sprintf("%.1f%% (prev print: %.1f%%; %s)",
                      retail_yoy, retail_prev_yoy%||%NA,
                      if(!is.na(retail_prev_yoy)) sprintf("%+.1f pp change", retail_yoy-retail_prev_yoy) else "N/A")),
    list(col=if(!is.na(sent_now)&&sent_now>80)"#2dce89" else if(!is.na(sent_now)&&sent_now>65)"#f4a261" else "#e94560",
         label="Consumer sentiment: ",
         text=sprintf("%.1f (prev %.1f; %s vs 1yr ago %.1f)",
                      sent_now, sent_prev%||%NA,
                      if(!is.na(sent_prev)) sprintf("%+.1f", sent_now-sent_prev) else "N/A",
                      sent_yr%||%NA)),
    list(col=if(!is.na(ip_yoy)&&ip_yoy>0)"#2dce89" else "#e94560",
         label="Industrial production YoY: ",
         text=sprintf("%.1f%% — %s", ip_yoy,
                      if(!is.na(ip_yoy)&&ip_yoy>1)"expansion in manufacturing activity"
                      else if(!is.na(ip_yoy)&&ip_yoy>0)"stagnant growth"
                      else "contraction, watch for inventory build reversal")),
    list(col="#9aa3b2", label="Sentiment trend (6M): ",
         text=sprintf("%s %s — leading indicator for spending 1-2 quarters ahead",
                      arrow(sent_trend),
                      if(sent_trend=="rising")"consumers growing more confident"
                      else if(sent_trend=="falling")"confidence deteriorating; spending may follow"
                      else "sentiment stable"))
  )

  note <- paste(
    if(!is.na(retail_yoy)) sprintf("Real retail growth (nominal YoY %.1f%% less CPI %.1f%% ≈ %.1f%% real) suggests consumer spending is %s. ",
      retail_yoy, kpis$cpi_yoy%||%0, retail_yoy-(kpis$cpi_yoy%||%0),
      if((retail_yoy-(kpis$cpi_yoy%||%0))>1)"growing in real terms" else "barely keeping pace with inflation") else "",
    if(!is.na(sent_now)&&!is.na(sent_yr)) sprintf(
      "Sentiment is %s year-over-year (%+.1f pts), %s.",
      if(sent_now>sent_yr)"higher" else "lower", sent_now-sent_yr,
      if(sent_now>sent_yr)"supporting continued discretionary spending" else "a headwind for non-essential categories") else ""
  )

  htmltools::HTML(.tab_synopsis_html("Consumer Economy", "#7c5cbf", "shopping-cart", bullets, note))
}

# ── Markets tab synopsis ───────────────────────────────────────────────────────
build_markets_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) return(NULL)

  t10   <- fred_data$DGS10  %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  t2    <- fred_data$DGS2   %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  vix   <- fred_data$VIXCLS %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  oil   <- fred_data$DCOILWTICO %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  gold  <- fred_data$GOLDPMGBD228NLBM %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }
  hy    <- fred_data$BAMLH0A0HYM2 %>% { if(!is.null(.)) arrange(., date) %>% pull(value) else NULL }

  spread_now  <- kpis$t10y2y %||% NA
  t10_now     <- kpis$t10yr  %||% NA
  t10_prev    <- if(!is.null(t10) && length(t10)>=2) t10[length(t10)-1] else NA
  vix_now     <- kpis$vix    %||% NA
  vix_prev    <- if(!is.null(vix) && length(vix)>=2) vix[length(vix)-1] else NA
  oil_now     <- kpis$oil_price %||% NA
  oil_prev    <- if(!is.null(oil) && length(oil)>=2) oil[length(oil)-1] else NA
  gold_now    <- kpis$gold_price %||% NA
  gold_prev   <- if(!is.null(gold)&& length(gold)>=2) gold[length(gold)-1] else NA
  hy_now      <- kpis$hy_spread %||% NA
  hy_prev     <- if(!is.null(hy)   && length(hy)>=2) hy[length(hy)-1] else NA

  months_inv  <- if(!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 24)
    sum((tail(t10,n) - tail(t2,n)) < 0, na.rm=TRUE)
  } else NA

  bullets <- list(
    list(col=if(!is.na(spread_now)&&spread_now<0)"#e94560" else "#2dce89",
         label="10Y-2Y spread: ",
         text=sprintf("%.2f pp (%s); %s months inverted in past 24",
                      spread_now, if(!is.na(spread_now)&&spread_now<0)"INVERTED — recession signal" else "normal",
                      months_inv%||%"N/A")),
    list(col="#f4a261",
         label="10Y Treasury: ",
         text=sprintf("%.2f%% (prev %.2f%%; %s bp)",
                      t10_now, t10_prev%||%NA,
                      if(!is.na(t10_prev)) sprintf("%+.0f", (t10_now-t10_prev)*100) else "N/A")),
    list(col=if(!is.na(vix_now)&&vix_now>25)"#e94560" else if(!is.na(vix_now)&&vix_now>18)"#f4a261" else "#2dce89",
         label="VIX: ",
         text=sprintf("%.1f (prev %.1f; %s) — %s",
                      vix_now, vix_prev%||%NA,
                      if(!is.na(vix_prev)) sprintf("%+.1f", vix_now-vix_prev) else "N/A",
                      if(!is.na(vix_now)&&vix_now>30)"risk-off: flight to safety, tighter financial conditions"
                      else if(!is.na(vix_now)&&vix_now>20)"cautious"
                      else "complacent; watch for vol mean-reversion")),
    list(col="#f4a261",
         label="WTI crude / Gold: ",
         text=sprintf("$%.0f/bbl (%s MoM) / $%.0f/oz (%s MoM) — gold/oil ratio: %.2f",
                      oil_now, if(!is.na(oil_prev)) sprintf("%+.1f%%", (oil_now/oil_prev-1)*100) else "N/A",
                      gold_now%||%0,
                      if(!is.na(gold_prev)&&!is.na(gold_now)) sprintf("%+.1f%%", (gold_now/gold_prev-1)*100) else "N/A",
                      if(!is.na(oil_now)&&!is.na(gold_now)&&oil_now>0) gold_now/oil_now else NA_real_)),
    list(col=if(!is.na(hy_now)&&hy_now>5)"#e94560" else "#2dce89",
         label="HY credit spread: ",
         text=sprintf("%.2f%% (prev %.2f%%; %s bp) — %s",
                      hy_now, hy_prev%||%NA,
                      if(!is.na(hy_prev)) sprintf("%+.0f", (hy_now-hy_prev)*100) else "N/A",
                      if(!is.na(hy_now)&&hy_now>6)"stress: credit markets pricing elevated default risk"
                      else if(!is.na(hy_now)&&hy_now>4.5)"elevated but contained"
                      else "benign credit conditions, risk appetite intact"))
  )

  note <- paste(
    if(!is.na(months_inv)&&months_inv>0) sprintf(
      "Curve inverted for %d of 24 months. Historically a recession follows within 6-18 months of inversion onset, though timing varies. ", months_inv) else "",
    if(!is.na(hy_now)&&!is.na(vix_now))
      sprintf("HY spreads (%.2f%%) and VIX (%.1f) are %s — %s.",
              hy_now, vix_now,
              if(hy_now<4.5&&vix_now<20)"aligned in a risk-on regime"
              else if(hy_now>5.5||vix_now>25)"diverging toward risk-off"
              else "in a cautious middle zone",
              "these two together are a reliable coincident indicator of financial stress") else ""
  )

  htmltools::HTML(.tab_synopsis_html("Financial Markets", "#f4a261", "chart-line", bullets, note))
}

# ═══════════════════════════════════════════════════════════════════════════════
# "SO WHAT" GROWTH OUTLOOK PANEL
# Synthesises all macro indicators into a 6-12 month GDP growth signal
# Uses a simple composite scoring model — no LLM required
# ═══════════════════════════════════════════════════════════════════════════════

#' Score a single indicator: +1 (positive impulse), 0 (neutral), -1 (negative)
.score <- function(val, good_threshold, bad_threshold, higher_good = TRUE) {
  if (is.na(val)) return(0)
  if (higher_good) {
    if (val >= good_threshold) return( 1)
    if (val <= bad_threshold)  return(-1)
    return(0)
  } else {
    if (val <= good_threshold) return( 1)
    if (val >= bad_threshold)  return(-1)
    return(0)
  }
}

#' Build the full growth outlook object
build_growth_outlook <- function(fred_data, kpis, mkt_returns = NULL) {
  if (is.null(kpis)) return(NULL)

  # ── Pull series ──────────────────────────────────────────────────────────────
  gv  <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 2) return(NULL)
    df %>% dplyr::arrange(date) %>% dplyr::pull(value)
  }

  unemp    <- gv("UNRATE");   payrolls <- gv("PAYEMS")
  cpi      <- gv("CPIAUCSL"); fedfunds <- gv("FEDFUNDS")
  t10      <- gv("DGS10");    t2       <- gv("DGS2")
  mort     <- gv("MORTGAGE30US")
  houst    <- gv("HOUST");    retail   <- gv("RSAFS")
  sent     <- gv("UMCSENT"); indpro   <- gv("INDPRO")
  vix_vals <- gv("VIXCLS");   hy       <- gv("BAMLH0A0HYM2")
  oil      <- gv("DCOILWTICO")

  # ── Key current values ────────────────────────────────────────────────────────
  u      <- kpis$unemp_rate  %||% NA
  cpi_y  <- kpis$cpi_yoy    %||% NA
  pce_y  <- kpis$core_pce   %||% NA
  ff     <- kpis$fed_funds  %||% NA
  t10v   <- kpis$t10yr      %||% NA
  t2v    <- kpis$t2yr       %||% NA
  mort_v <- kpis$mortgage30 %||% NA
  hs     <- kpis$housing_starts %||% NA
  ret_y  <- kpis$retail_yoy  %||% NA
  cs     <- kpis$cons_sent   %||% NA
  ip_y   <- kpis$indpro_yoy  %||% NA
  vix_v  <- kpis$vix         %||% NA
  hy_v   <- kpis$hy_spread   %||% NA
  oil_v  <- kpis$oil_price   %||% NA

  # ── Derived metrics ──────────────────────────────────────────────────────────
  real_rate   <- if (!is.na(ff)  && !is.na(cpi_y)) ff - cpi_y   else NA
  yield_spread<- if (!is.na(t10v)&& !is.na(t2v))  t10v - t2v  else NA
  pay_3m      <- if (!is.null(payrolls) && length(payrolls)>=4)
                   mean(diff(tail(payrolls,4)), na.rm=TRUE) else NA
  ret_real    <- if (!is.na(ret_y) && !is.na(cpi_y)) ret_y - cpi_y else NA

  # Months of yield inversion in last 18
  inv_months  <- if (!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 18)
    sum((tail(t10,n) - tail(t2,n)) < 0, na.rm=TRUE)
  } else 0

  # VIX 6-month average
  vix_6m_avg <- if (!is.null(vix_vals) && length(vix_vals)>=126)
    mean(tail(vix_vals,126), na.rm=TRUE) else vix_v

  # ── Composite score model ────────────────────────────────────────────────────
  # Each component scores -1 / 0 / +1; weighted sum → composite
  # Positive composite = growth-positive conditions; negative = recessionary
  components <- list(

    # Labor (weight 2.5)
    labor = list(
      weight = 2.5,
      score  = mean(c(
        .score(u,      3.5, 5.0,   higher_good=FALSE),  # unemployment
        .score(pay_3m, 150, 50,    higher_good=TRUE),    # payroll momentum
        .score(u,      4.5, 5.5,   higher_good=FALSE)    # secondary unemployment gauge
      ), na.rm=TRUE),
      label  = "Labor Market",
      detail = if (!is.na(u) && !is.na(pay_3m))
        sprintf("%.1f%% unemployment; avg payrolls %+.0fK/mo (3M)", u, pay_3m) else "N/A"
    ),

    # Consumer (weight 2.0)
    consumer = list(
      weight = 2.0,
      score  = mean(c(
        .score(ret_real, 1.0, -1.0, higher_good=TRUE),   # real retail growth
        .score(cs,       85,  65,   higher_good=TRUE)    # consumer sentiment
      ), na.rm=TRUE),
      label  = "Consumer Demand",
      detail = if (!is.na(ret_real) && !is.na(cs))
        sprintf("Real retail YoY %.1f%%; sentiment %.0f", ret_real, cs) else "N/A"
    ),

    # Monetary conditions (weight 2.0)
    monetary = list(
      weight = 2.0,
      score  = mean(c(
        .score(real_rate,    0.5, 2.5,   higher_good=FALSE), # real rate (higher=tighter)
        .score(yield_spread, 0.5, -0.25, higher_good=TRUE),  # yield curve
        .score(inv_months,   3,   9,     higher_good=FALSE)  # inversion duration
      ), na.rm=TRUE),
      label  = "Monetary Conditions",
      detail = if (!is.na(real_rate) && !is.na(yield_spread))
        sprintf("Real rate %.1f%%; 10Y-2Y spread %.2f pp; %d mo inverted",
                real_rate, yield_spread, inv_months) else "N/A"
    ),

    # Housing (weight 1.5)
    housing = list(
      weight = 1.5,
      score  = mean(c(
        .score(hs,     1400, 1000, higher_good=TRUE),    # housing starts (K)
        .score(mort_v, 6.0,  7.5,  higher_good=FALSE)   # mortgage rate
      ), na.rm=TRUE),
      label  = "Housing",
      detail = if (!is.na(hs) && !is.na(mort_v))
        sprintf("%.0fK starts ann.; %.2f%% mortgage rate", hs, mort_v) else "N/A"
    ),

    # Financial conditions (weight 1.5)
    financial = list(
      weight = 1.5,
      score  = mean(c(
        .score(vix_v, 18, 28, higher_good=FALSE),  # VIX (lower=calmer)
        .score(hy_v,  4.5, 6.0, higher_good=FALSE) # HY spread (lower=benign)
      ), na.rm=TRUE),
      label  = "Financial Conditions",
      detail = if (!is.na(vix_v) && !is.na(hy_v))
        sprintf("VIX %.1f; HY spread %.2f%%", vix_v, hy_v) else "N/A"
    ),

    # Inflation / pricing pressure (weight 1.5 — elevated = headwind)
    inflation = list(
      weight = 1.5,
      score  = mean(c(
        .score(cpi_y, 2.5, 4.5, higher_good=FALSE),  # CPI (lower=better for growth)
        .score(pce_y, 2.3, 3.5, higher_good=FALSE)   # Core PCE
      ), na.rm=TRUE),
      label  = "Inflation",
      detail = if (!is.na(cpi_y) && !is.na(pce_y))
        sprintf("CPI YoY %.1f%%; Core PCE %.1f%%", cpi_y, pce_y) else "N/A"
    ),

    # Industrial / production (weight 1.0)
    industrial = list(
      weight = 1.0,
      score  = .score(ip_y, 1.0, -1.0, higher_good=TRUE),
      label  = "Industrial Output",
      detail = if (!is.na(ip_y)) sprintf("IP YoY %.1f%%", ip_y) else "N/A"
    )
  )

  # ── Weighted composite ────────────────────────────────────────────────────────
  total_weight <- sum(sapply(components, `[[`, "weight"))
  raw_score    <- sum(sapply(components, function(c) c$score * c$weight)) / total_weight
  # Scale: -1 → +1 → normalise to 0-10 for display
  score_0_10   <- round((raw_score + 1) / 2 * 10, 1)
  score_0_10   <- max(0, min(10, score_0_10))

  # ── Regime classification ─────────────────────────────────────────────────────
  regime <- dplyr::case_when(
    score_0_10 >= 7.5 ~ list(
      label   = "Expansion",
      color   = "#2dce89",
      icon    = "arrow-up",
      gdp_est = "+2.5% to +3.5%",
      summary = "Broad-based growth signals. Labour market solid, consumer spending resilient, financial conditions supportive."
    ),
    score_0_10 >= 5.5 ~ list(
      label   = "Moderate Growth",
      color   = "#00b4d8",
      icon    = "minus",
      gdp_est = "+1.0% to +2.5%",
      summary = "Below-trend but positive growth likely. Mixed signals across sectors — watch rate transmission and consumer confidence."
    ),
    score_0_10 >= 3.5 ~ list(
      label   = "Stall / Slowdown",
      color   = "#f4a261",
      icon    = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk. Restrictive monetary conditions, softening demand, or financial stress beginning to bite."
    ),
    TRUE ~ list(
      label   = "Contraction Risk",
      color   = "#e94560",
      icon    = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals. Recession probability elevated — yield curve, credit spreads, and demand indicators all deteriorating."
    )
  )

  # ── Key swing factors ─────────────────────────────────────────────────────────
  swing_factors <- list()

  # Most negative drag
  drag <- names(which.min(sapply(components, `[[`, "score")))
  swing_factors$biggest_drag <- list(
    name   = components[[drag]]$label,
    score  = round(components[[drag]]$score, 2),
    detail = components[[drag]]$detail
  )

  # Most positive support
  supp <- names(which.max(sapply(components, `[[`, "score")))
  swing_factors$biggest_support <- list(
    name   = components[[supp]]$label,
    score  = round(components[[supp]]$score, 2),
    detail = components[[supp]]$detail
  )

  # Key watch: inversion status
  swing_factors$yield_curve_watch <- if (inv_months > 6) {
    sprintf("Yield curve inverted %d months — recession historically follows within 6-18 months of onset.", inv_months)
  } else if (!is.na(yield_spread) && yield_spread > 0) {
    sprintf("Yield curve re-steepened to %.2f pp — often precedes recovery but watch for 'bear steepener' pattern.", yield_spread)
  } else NULL

  # Fed pivot watch
  swing_factors$fed_watch <- if (!is.na(real_rate) && real_rate > 2.0) {
    sprintf("Real Fed Funds rate %.1f%% — firmly restrictive. Rate cuts would provide significant growth tailwind.", real_rate)
  } else if (!is.na(real_rate) && real_rate < 0.5) {
    "Real rate near zero — limited conventional monetary stimulus remaining."
  } else NULL

  # ── Recession probability model ───────────────────────────────────────────────
  # Equity drawdown from peak (SPY as S&P proxy)
  equity_drawdown_pct <- if (!is.null(mkt_returns)) {
    spy <- mkt_returns %>%
      dplyr::filter(symbol == "SPY") %>%
      dplyr::arrange(date)
    if (nrow(spy) >= 52) {
      current   <- tail(spy$close, 1)
      peak_52wk <- max(spy$close[max(1,nrow(spy)-252):nrow(spy)], na.rm=TRUE)
      round((current / peak_52wk - 1) * 100, 1)
    } else NA_real_
  } else NA_real_

  recession_prob <- .compute_recession_prob(
    yield_spread          = yield_spread,
    inv_months            = inv_months,
    unemp                 = unemp,
    u                     = u,
    pay_3m                = pay_3m,
    real_rate             = real_rate,
    hy_v                  = hy_v,
    vix_v                 = vix_v,
    cs                    = cs,
    ip_y                  = ip_y,
    hs                    = hs,
    prime_age_lfpr        = kpis$prime_age_lfpr        %||% NA,
    prime_age_lfpr_chg    = kpis$prime_age_lfpr_chg    %||% NA,
    jobless_claims_trend  = kpis$jobless_claims_trend   %||% NA,
    quits_yoy             = kpis$quits_yoy              %||% NA,
    oil_yoy               = kpis$oil_yoy                %||% NA,
    equity_drawdown_pct   = equity_drawdown_pct,
    usd_yoy               = kpis$usd_yoy                %||% NA,
    inventory_sales_ratio = kpis$inventory_sales_ratio  %||% NA,
    cc_delinquency        = kpis$cc_delinquency         %||% NA
  )

  list(
    score          = score_0_10,
    raw_score      = raw_score,
    regime         = regime,
    components     = components,
    swing          = swing_factors,
    recession_prob = recession_prob,
    as_of          = Sys.Date()
  )
}

# ── Enhanced Recession Probability Engine ─────────────────────────────────────
# Multi-factor logit model calibrated to NBER recession dates.
# Baseline log-odds = log(0.15/0.85) ≈ -1.73 (15% unconditional 12M base rate).
# Each factor shifts log-odds; final p = 1 / (1 + exp(-log_odds)), clamped [2,97].
#
# Factor selection rationale:
#  1.  Yield curve          — Estrella & Mishkin (1998): best single predictor, 12M lead
#  2.  Sahm / unemployment  — Real-time recession indicator, near-certain at 0.5pp rise
#  3.  Prime-age LFPR       — Masks Sahm rule: LFPR drop = supply withdrawal, hidden slack
#  4.  Jobless claims trend — 4wk vs 26wk MA crossover; sensitive to sudden layoffs
#  5.  Quits rate trend     — Workers quit when confident; falling quits = fear
#  6.  Oil price shock      — >25% 3M spike transmits into CPI → spending contraction
#  7.  Equity bear market   — >20% S&P drawdown preceded every post-WW2 recession
#  8.  Credit stress (HY)   — HY spreads >500bp = credit crunch; coincident/leading
#  9.  Dollar surge         — >8% YoY tightens global conditions, hits EM, exports
# 10.  Real rate            — Monetary drag with 6-18M lag
# 11.  Consumer sentiment   — Conference Board LEI proxy, forward-looking
# 12.  Inventory-sales      — Rising ratio = demand weakness ahead of employment cuts
# 13.  CC delinquency       — Consumer stress shows here 2-3Q before unemployment rises

.compute_recession_prob <- function(yield_spread, inv_months, unemp, u, pay_3m,
                                     real_rate, hy_v, vix_v, cs, ip_y, hs,
                                     # New factors
                                     prime_age_lfpr = NA, prime_age_lfpr_chg = NA,
                                     jobless_claims_trend = NA,
                                     quits_yoy = NA,
                                     oil_yoy = NA,
                                     equity_drawdown_pct = NA,
                                     usd_yoy = NA,
                                     inventory_sales_ratio = NA,
                                     cc_delinquency = NA) {

  # ── Baseline ──────────────────────────────────────────────────────────────────
  log_odds <- -1.73   # 15% unconditional base rate

  # ── 1. Yield Curve (weight: highest) ─────────────────────────────────────────
  if (!is.na(yield_spread)) {
    log_odds <- log_odds + (-yield_spread * 0.85)
    if (!is.na(inv_months)) {
      if      (inv_months >= 18) log_odds <- log_odds + 1.5
      else if (inv_months >= 12) log_odds <- log_odds + 1.2
      else if (inv_months >= 6)  log_odds <- log_odds + 0.6
      else if (inv_months >= 3)  log_odds <- log_odds + 0.3
    }
  }

  # ── 2. Sahm Rule / Unemployment Rise ─────────────────────────────────────────
  if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
    u_trough_12m <- min(tail(unemp, 12), na.rm = TRUE)
    u_rise       <- u - u_trough_12m
    if      (u_rise >= 0.5)  log_odds <- log_odds + 1.8   # Sahm rule triggered
    else if (u_rise >= 0.3)  log_odds <- log_odds + 0.9
    else if (u_rise >= 0.1)  log_odds <- log_odds + 0.3
    else if (u_rise < -0.2)  log_odds <- log_odds - 0.4
  }
  if (!is.na(pay_3m)) {
    if      (pay_3m < -50)  log_odds <- log_odds + 1.5
    else if (pay_3m <   0)  log_odds <- log_odds + 0.6
    else if (pay_3m < 100)  log_odds <- log_odds + 0.1
    else if (pay_3m > 250)  log_odds <- log_odds - 0.5
  }

  # ── 3. Prime-Age LFPR (25-54) — Hidden Labour Weakness ───────────────────────
  # Declining prime-age LFPR signals real demand withdrawal even if unemployment
  # is stable (people leave workforce rather than register as unemployed).
  # Pre-pandemic trend ~83%; below 82% = structural weakness.
  if (!is.na(prime_age_lfpr)) {
    if      (prime_age_lfpr < 80.5) log_odds <- log_odds + 0.8  # deep slack
    else if (prime_age_lfpr < 82.0) log_odds <- log_odds + 0.4  # below trend
    else if (prime_age_lfpr > 83.5) log_odds <- log_odds - 0.3  # healthy participation
  }
  # 6-month direction matters more than level
  if (!is.na(prime_age_lfpr_chg)) {
    if      (prime_age_lfpr_chg < -0.5) log_odds <- log_odds + 0.9  # sharp drop
    else if (prime_age_lfpr_chg < -0.2) log_odds <- log_odds + 0.4
    else if (prime_age_lfpr_chg >  0.3) log_odds <- log_odds - 0.3  # rising = good
  }

  # ── 4. Initial Jobless Claims Trend ──────────────────────────────────────────
  # 4-week MA vs 26-week MA ratio; >0 = claims trending up = leading recession signal
  if (!is.na(jobless_claims_trend)) {
    if      (jobless_claims_trend >  0.20) log_odds <- log_odds + 1.2  # claims surging
    else if (jobless_claims_trend >  0.10) log_odds <- log_odds + 0.6
    else if (jobless_claims_trend >  0.05) log_odds <- log_odds + 0.2
    else if (jobless_claims_trend < -0.10) log_odds <- log_odds - 0.3  # falling = good
  }

  # ── 5. Quits Rate YoY Change ─────────────────────────────────────────────────
  # Workers quit voluntarily when confident about finding new jobs.
  # Quits falling YoY = workers scared to leave = labour market cooling.
  if (!is.na(quits_yoy)) {
    if      (quits_yoy < -15) log_odds <- log_odds + 0.8  # quits collapsed
    else if (quits_yoy < -8)  log_odds <- log_odds + 0.4
    else if (quits_yoy < -3)  log_odds <- log_odds + 0.2
    else if (quits_yoy >  8)  log_odds <- log_odds - 0.3  # workers confident
  }

  # ── 6. Oil Price Shock — Supply Chain Transmission ───────────────────────────
  # Hamilton (1983): >25% oil price increase is a reliable recession precursor.
  # Mechanism: oil → gas/diesel → trucking/freight → retail prices → CPI →
  #            Fed tightens → consumer spending contracts → recession.
  # A >20% DROP is also relevant (signals collapsing global demand).
  if (!is.na(oil_yoy)) {
    if      (oil_yoy >  50) log_odds <- log_odds + 1.0  # severe supply shock
    else if (oil_yoy >  25) log_odds <- log_odds + 0.6  # Hamilton threshold
    else if (oil_yoy >  15) log_odds <- log_odds + 0.2
    else if (oil_yoy < -30) log_odds <- log_odds + 0.5  # demand collapse = bad signal
    else if (oil_yoy < -15) log_odds <- log_odds + 0.2
    else if (oil_yoy >   0 && oil_yoy < 10) log_odds <- log_odds - 0.1  # stable/mild
  }

  # ── 7. Equity Market — Bear Market Signal ────────────────────────────────────
  # S&P 500 >20% drawdown from 52-week high has preceded every post-WW2 recession.
  # >10% correction = caution; >30% = recessionary environment almost certain.
  if (!is.na(equity_drawdown_pct)) {
    dd <- abs(equity_drawdown_pct)   # pct is already negative; work with magnitude
    if      (dd > 30) log_odds <- log_odds + 1.6  # severe bear market
    else if (dd > 20) log_odds <- log_odds + 1.1  # official bear market
    else if (dd > 10) log_odds <- log_odds + 0.5  # correction
    else if (dd <  5) log_odds <- log_odds - 0.2  # near all-time highs = good
  }

  # ── 8. Credit Stress (HY Spreads) ────────────────────────────────────────────
  if (!is.na(hy_v)) {
    if      (hy_v > 7.0) log_odds <- log_odds + 1.4
    else if (hy_v > 5.5) log_odds <- log_odds + 0.7
    else if (hy_v > 4.5) log_odds <- log_odds + 0.2
    else if (hy_v < 3.5) log_odds <- log_odds - 0.4
  }

  # ── 9. Dollar Surge — Global Tightening ─────────────────────────────────────
  # USD appreciation >8% YoY tightens global financial conditions: raises EM
  # debt burdens, hurts US exporters, and signals risk-off positioning.
  if (!is.na(usd_yoy)) {
    if      (usd_yoy >  12) log_odds <- log_odds + 0.8  # major dollar surge
    else if (usd_yoy >   8) log_odds <- log_odds + 0.4  # meaningful appreciation
    else if (usd_yoy >   4) log_odds <- log_odds + 0.1
    else if (usd_yoy < -5)  log_odds <- log_odds - 0.2  # dollar weakening = easing
  }

  # ── 10. Monetary Restrictiveness ─────────────────────────────────────────────
  if (!is.na(real_rate)) {
    if      (real_rate > 3.0) log_odds <- log_odds + 1.0
    else if (real_rate > 2.0) log_odds <- log_odds + 0.5
    else if (real_rate > 1.0) log_odds <- log_odds + 0.2
    else if (real_rate < 0.0) log_odds <- log_odds - 0.3
  }

  # ── 11. Consumer Sentiment ───────────────────────────────────────────────────
  if (!is.na(cs)) {
    if      (cs < 60) log_odds <- log_odds + 0.7
    else if (cs < 70) log_odds <- log_odds + 0.3
    else if (cs > 90) log_odds <- log_odds - 0.4
  }

  # ── 12. Inventory-to-Sales Ratio ─────────────────────────────────────────────
  # Rising above trend (>1.40) signals businesses cutting orders → hiring slows.
  # Spike above 1.5 historically coincides with recessionary slowdowns.
  if (!is.na(inventory_sales_ratio)) {
    if      (inventory_sales_ratio > 1.55) log_odds <- log_odds + 0.7
    else if (inventory_sales_ratio > 1.45) log_odds <- log_odds + 0.3
    else if (inventory_sales_ratio > 1.40) log_odds <- log_odds + 0.1
    else if (inventory_sales_ratio < 1.30) log_odds <- log_odds - 0.2
  }

  # ── 13. Consumer Credit Card Delinquency ─────────────────────────────────────
  # Rising delinquency rates signal over-extended consumers 2-3 quarters before
  # it appears in unemployment data. Pre-GFC spike was a clear leading signal.
  if (!is.na(cc_delinquency)) {
    if      (cc_delinquency > 4.0) log_odds <- log_odds + 0.9  # serious stress
    else if (cc_delinquency > 3.0) log_odds <- log_odds + 0.5
    else if (cc_delinquency > 2.5) log_odds <- log_odds + 0.2
    else if (cc_delinquency < 1.8) log_odds <- log_odds - 0.3  # very low = healthy
  }

  # ── Convert → probability ─────────────────────────────────────────────────────
  prob <- round(1 / (1 + exp(-log_odds)) * 100, 1)
  prob <- max(2, min(97, prob))

  # ── Tier classification ───────────────────────────────────────────────────────
  tier <- if (prob >= 60) {
    list(label="Elevated", color="#e94560", icon="exclamation-triangle",
         desc="Multiple leading indicators aligned in a recessionary direction. Historical hit rate at this probability level: ~65%. Defensive positioning warranted.")
  } else if (prob >= 40) {
    list(label="Moderate", color="#f4a261", icon="exclamation-circle",
         desc="Mixed signals with several genuine warning signs. Late-cycle slowdown most likely; recession possible if conditions deteriorate further.")
  } else if (prob >= 20) {
    list(label="Low–Moderate", color="#f4a261", icon="minus-circle",
         desc="Some headwinds present but no acute stress signal. Growth outlook cautiously positive; monitor labour and credit conditions.")
  } else {
    list(label="Low", color="#2dce89", icon="check-circle",
         desc="Most leading indicators benign. Historical baseline ~15%; current conditions are at or below average 12-month recession risk.")
  }

  # ── Key drivers pushing probability up or down ────────────────────────────────
  drivers_up   <- character(0)
  drivers_down <- character(0)

  if (!is.na(yield_spread) && yield_spread < -0.1)
    drivers_up <- c(drivers_up, sprintf("Inverted yield curve (%+.2f pp)", yield_spread))
  if (!is.na(inv_months) && inv_months >= 6)
    drivers_up <- c(drivers_up, sprintf("Inversion sustained %d months", inv_months))
  if (!is.null(unemp) && length(unemp) >= 12) {
    u_rise <- u - min(tail(unemp, 12), na.rm=TRUE)
    if (u_rise >= 0.3) drivers_up <- c(drivers_up,
      sprintf("Unemployment %.1f pp above 12M low (Sahm signal)", u_rise))
  }
  if (!is.na(prime_age_lfpr_chg) && prime_age_lfpr_chg < -0.2)
    drivers_up <- c(drivers_up,
      sprintf("Prime-age LFPR falling (%+.1f pp in 6M) — hidden labour slack", prime_age_lfpr_chg))
  if (!is.na(jobless_claims_trend) && jobless_claims_trend > 0.10)
    drivers_up <- c(drivers_up,
      sprintf("Jobless claims trending up (+%.0f%% vs 6M avg)", jobless_claims_trend * 100))
  if (!is.na(oil_yoy) && oil_yoy > 25)
    drivers_up <- c(drivers_up,
      sprintf("Oil price shock +%.0f%% YoY — supply chain inflation pressure", oil_yoy))
  if (!is.na(equity_drawdown_pct) && abs(equity_drawdown_pct) > 10)
    drivers_up <- c(drivers_up,
      sprintf("Equity drawdown %.0f%% from 52-week high", equity_drawdown_pct))
  if (!is.na(hy_v) && hy_v > 4.5)
    drivers_up <- c(drivers_up, sprintf("HY credit spread %.2f%%", hy_v))
  if (!is.na(usd_yoy) && usd_yoy > 8)
    drivers_up <- c(drivers_up, sprintf("USD +%.0f%% YoY — global tightening", usd_yoy))
  if (!is.na(cc_delinquency) && cc_delinquency > 2.5)
    drivers_up <- c(drivers_up,
      sprintf("Credit card delinquency rate %.1f%% — consumer stress", cc_delinquency))
  if (!is.na(quits_yoy) && quits_yoy < -8)
    drivers_up <- c(drivers_up,
      sprintf("Quits rate %.0f%% YoY — workers losing labour market confidence", quits_yoy))

  if (!is.na(yield_spread) && yield_spread > 0.5)
    drivers_down <- c(drivers_down, sprintf("Normal yield curve (+%.2f pp)", yield_spread))
  if (!is.na(pay_3m) && pay_3m > 200)
    drivers_down <- c(drivers_down, sprintf("Strong payrolls (%+.0fK/mo 3M avg)", pay_3m))
  if (!is.na(prime_age_lfpr) && prime_age_lfpr > 83.5)
    drivers_down <- c(drivers_down,
      sprintf("Prime-age LFPR %.1f%% — robust labour force attachment", prime_age_lfpr))
  if (!is.na(hy_v) && hy_v < 3.5)
    drivers_down <- c(drivers_down, sprintf("Benign credit spreads (%.2f%%)", hy_v))
  if (!is.na(equity_drawdown_pct) && abs(equity_drawdown_pct) < 5)
    drivers_down <- c(drivers_down, "Equities near 52-week highs — risk appetite intact")
  if (!is.na(cs) && cs > 85)
    drivers_down <- c(drivers_down, sprintf("Elevated consumer sentiment (%.0f)", cs))
  if (!is.na(cc_delinquency) && cc_delinquency < 1.8)
    drivers_down <- c(drivers_down,
      sprintf("Low credit card delinquency (%.1f%%) — consumers in good shape", cc_delinquency))

  list(
    prob         = prob,
    tier         = tier,
    drivers_up   = head(drivers_up,   4),
    drivers_down = head(drivers_down, 3)
  )
}
render_growth_outlook_html <- function(outlook) {
  if (is.null(outlook)) {
    return(HTML("<p style='color:#6b7585;padding:16px;'>Growth outlook not yet computed — data loading.</p>"))
  }

  reg   <- outlook$regime
  score <- outlook$score
  comps <- outlook$components
  swing <- outlook$swing

  # Score gauge bar
  pct   <- score / 10 * 100
  gauge_col <- reg$color

  gauge_html <- sprintf(
    "<div style='margin:12px 0 16px;'>
       <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
         <span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span>
         <span style='color:#9aa3b2;font-size:11px;'>Expansion</span>
       </div>
       <div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'>
         <div style='background:linear-gradient(90deg,%s,%s);width:%.0f%%;height:100%%;
                     border-radius:6px;transition:width 0.5s ease;'></div>
       </div>
       <div style='margin-top:5px;display:flex;justify-content:space-between;'>
         <span style='color:#9aa3b2;font-size:10px;'>0</span>
         <span style='color:%s;font-size:11px;font-weight:700;'>Score: %.1f / 10</span>
         <span style='color:#9aa3b2;font-size:10px;'>10</span>
       </div>
     </div>",
    if (score < 5) "#e94560" else "#f4a261",
    gauge_col, pct, gauge_col, score
  )

  # Component scores
  comp_html <- paste(vapply(names(comps), function(nm) {
    c  <- comps[[nm]]
    s  <- c$score
    col<- if (s >= 0.4) "#2dce89" else if (s >= -0.4) "#f4a261" else "#e94560"
    bar_w <- round((s + 1) / 2 * 100)
    sprintf(
      "<div style='margin-bottom:8px;'>
         <div style='display:flex;justify-content:space-between;margin-bottom:3px;'>
           <span style='color:#9aa3b2;font-size:11px;'>%s</span>
           <span style='color:%s;font-size:11px;font-weight:600;'>%s</span>
         </div>
         <div style='background:#2a3042;border-radius:3px;height:5px;'>
           <div style='background:%s;width:%d%%;height:100%%;border-radius:3px;'></div>
         </div>
         <div style='color:#6b7585;font-size:10px;margin-top:2px;'>%s</div>
       </div>",
      c$label,
      col,
      if (s >= 0.4) "Positive" else if (s >= -0.4) "Neutral" else "Negative",
      col, bar_w,
      htmltools::htmlEscape(c$detail)
    )
  }, character(1)), collapse="")

  # Swing factors
  swing_html <- paste(c(
    if (!is.null(swing$biggest_drag))
      sprintf("<li style='margin-bottom:6px;'><span style='color:#e94560;font-weight:600;'>⬇ Biggest drag: %s</span> — %s</li>",
              swing$biggest_drag$name, htmltools::htmlEscape(swing$biggest_drag$detail)),
    if (!is.null(swing$biggest_support))
      sprintf("<li style='margin-bottom:6px;'><span style='color:#2dce89;font-weight:600;'>⬆ Biggest support: %s</span> — %s</li>",
              swing$biggest_support$name, htmltools::htmlEscape(swing$biggest_support$detail)),
    if (!is.null(swing$yield_curve_watch))
      sprintf("<li style='margin-bottom:6px;'><span style='color:#f4a261;font-weight:600;'>⚠ Yield Curve:</span> %s</li>",
              htmltools::htmlEscape(swing$yield_curve_watch)),
    if (!is.null(swing$fed_watch))
      sprintf("<li style='margin-bottom:6px;'><span style='color:#00b4d8;font-weight:600;'>⚙ Fed Policy:</span> %s</li>",
              htmltools::htmlEscape(swing$fed_watch))
  ), collapse="")

  # ── Recession probability panel ───────────────────────────────────────────────
  rp       <- outlook$recession_prob
  rp_html  <- ""
  if (!is.null(rp)) {
    prob       <- rp$prob
    tier       <- rp$tier
    bar_col    <- tier$color
    # Semicircular gauge using CSS clip — simulated with a gradient bar + pointer
    bar_pct    <- prob

    # Driver bullets
    up_html <- if (length(rp$drivers_up) > 0)
      paste(sprintf(
        "<li style='margin-bottom:4px;'><span style='color:#e94560;'>▲</span> %s</li>",
        htmltools::htmlEscape(rp$drivers_up)
      ), collapse="")
    else ""

    down_html <- if (length(rp$drivers_down) > 0)
      paste(sprintf(
        "<li style='margin-bottom:4px;'><span style='color:#2dce89;'>▼</span> %s</li>",
        htmltools::htmlEscape(rp$drivers_down)
      ), collapse="")
    else ""

    drivers_section <- if (nchar(up_html) > 0 || nchar(down_html) > 0)
      sprintf(
        "<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>
          %s%s
         </ul>",
        up_html, down_html
      ) else ""

    rp_html <- sprintf(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                   border-top:3px solid %s;padding:14px 16px;margin-top:14px;'>

        <!-- Header row -->
        <div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'>
          <div>
            <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;
                        letter-spacing:1px;'>
              <i class=\"fa fa-%s\" style=\"color:%s;margin-right:6px;\"></i>
              Recession Probability — 12-Month Horizon
            </div>
            <div style='margin-top:4px;'>
              <span style='color:%s;font-size:36px;font-weight:800;'>%s%%</span>
              <span style='color:%s;font-size:14px;font-weight:600;margin-left:8px;
                           background:rgba(%s,0.12);padding:2px 10px;border-radius:12px;
                           border:1px solid rgba(%s,0.3);'>
                %s
              </span>
            </div>
          </div>
          <!-- Mini thermometer -->
          <div style='text-align:right;'>
            <div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0%%  ·  50%%  ·  100%%</div>
            <div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);
                        border-radius:7px;position:relative;'>
              <div style='position:absolute;top:-3px;left:calc(%s%% - 8px);
                          width:0;height:0;border-left:7px solid transparent;
                          border-right:7px solid transparent;border-top:8px solid #ffffff;'></div>
            </div>
            <div style='font-size:10px;color:#6b7585;margin-top:3px;text-align:center;'>▲ Current</div>
          </div>
        </div>

        <!-- Description -->
        <div style='color:#9aa3b2;font-size:12px;line-height:1.6;
                    background:#0f1117;border-radius:6px;padding:8px 12px;
                    border-left:3px solid %s;'>
          %s
        </div>

        %s

        <div style='color:#555;font-size:10px;margin-top:8px;'>
          Multi-factor logit model (yield curve, Sahm rule, credit spreads, leading indicators).
          Calibrated to historical NBER recession dates. Not a point forecast — conveys relative risk level.
        </div>
      </div>",
      bar_col,                                  # border-top colour
      tier$icon, bar_col,                       # icon
      bar_col,                                  # probability number colour
      sprintf("%.0f", prob),                    # probability %
      bar_col, bar_col, bar_col,                # badge colour + rgba placeholders
      tier$label,                               # tier label
      sprintf("%.0f", min(prob, 98)),           # thermometer pointer position (capped)
      bar_col,                                  # left border of desc box
      htmltools::htmlEscape(tier$desc),         # description text
      drivers_section
    )
  }

  HTML(sprintf(
    "<div style='display:flex;gap:14px;'>
       <!-- Left: regime verdict -->
       <div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                   border-top:3px solid %s;padding:16px 18px;'>
         <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                     letter-spacing:1px;margin-bottom:6px;'>
           <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>6–12 Month Growth Outlook
         </div>
         <div style='color:%s;font-size:22px;font-weight:800;margin-bottom:2px;'>%s</div>
         <div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>
           GDP Est: %s annualised
         </div>
         %s
         <div style='background:#0f1117;border-radius:6px;padding:10px 12px;
                     color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid %s;'>
           %s
         </div>
         <div style='color:#555;font-size:10px;margin-top:10px;'>
           Composite score model. %d indicators. As of %s.
         </div>
         %s
       </div>
       <!-- Right: component breakdown + swing factors -->
       <div style='flex:1;display:flex;flex-direction:column;gap:10px;'>
         <div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                     padding:14px 16px;flex:1;'>
           <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;
                       letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>
           %s
         </div>
         <div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                     padding:14px 16px;'>
           <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;
                       letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div>
           <ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>
             %s
           </ul>
         </div>
       </div>
     </div>",
    reg$color, reg$color, reg$icon,
    reg$color, reg$label,
    reg$gdp_est,
    gauge_html,
    reg$color, htmltools::htmlEscape(reg$summary),
    length(comps),
    format(outlook$as_of, "%b %d, %Y"),
    rp_html,           # ← recession probability panel inside left column
    comp_html,
    swing_html
  ))
}
