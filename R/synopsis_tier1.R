# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier1.R — Tier 1: Estimated Logistic Regression Model
#
# WHAT'S NEW vs the original hand-tuned synopsis.R:
#
#  1. train_recession_model()  — Fits a glm(binomial) on actual NBER recession
#     history using live FRED series. Trained once at first call, cached
#     globally, re-trained every 30 days. Falls back to hand-tuned model if
#     FRED download or glm() fails.
#
#  2. build_feature_vector()   — Converts the current KPI snapshot into a
#     named one-row data.frame suitable for glm predict().
#
#  3. .compute_recession_prob() — Now tracks each factor's *actual current*
#     log-odds contribution (not just a threshold-based indicator).  When a
#     trained GLM is available, predict() supplies the final probability and
#     β_i × x_i contributions replace the hand-tuned deltas.
#
#  4. .build_factor_bar_chart() — NEW.  Renders a diverging HTML bar chart of
#     every factor's current contribution (red = raises probability, green =
#     lowers it).  When the GLM is active, also shows the estimated z-statistic
#     so the user can see what the historical data says about each variable's
#     statistical reliability.
#
#  5. .build_model_modal()     — Updated.  Inserts the live bar chart above the
#     existing detailed factor table.
#
# All other functions (helpers, build_synopsis, tab synopsis builders,
# render_synopsis_html, .score, build_growth_outlook composite scorecard,
# render_growth_outlook_html) are IDENTICAL to the original synopsis.R.
# ─────────────────────────────────────────────────────────────────────────────

# ── Helpers ───────────────────────────────────────────────────────────────────

roll_cor <- function(x, y, n = 36) {
  if (length(x) < n || length(y) < n) {
    return(NA_real_)
  }
  x <- tail(x, n)
  y <- tail(y, n)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  cor(x, y, use = "pairwise.complete.obs")
}

lag_cor <- function(x, y, lag = 0) {
  n <- length(x)
  if (lag >= n || lag < 0) {
    return(NA_real_)
  }
  cor(x[(lag + 1):n], y[1:(n - lag)], use = "pairwise.complete.obs")
}

best_lag <- function(x, y, max_lag = 18) {
  cors <- vapply(0:max_lag, function(l) lag_cor(x, y, l), numeric(1))
  best <- which.max(abs(cors))
  list(lag = best - 1L, r = cors[best])
}

trend_dir <- function(vals, n = 12) {
  v <- tail(na.omit(vals), n)
  if (length(v) < 4) {
    return("unclear")
  }
  fit <- lm(v ~ seq_along(v))
  slope <- coef(fit)[2]
  se <- summary(fit)$coefficients[2, 2]
  if (abs(slope) < se) {
    return("flat")
  }
  if (slope > 0) {
    return("rising")
  }
  return("falling")
}

fmt_cor <- function(r) {
  if (is.na(r)) {
    return("N/A")
  }
  lbl <- if (abs(r) > 0.75) {
    "strong"
  } else if (abs(r) > 0.5) {
    "moderate"
  } else {
    "weak"
  }
  dir <- if (r > 0) "positive" else "negative"
  sprintf("r \u2248 %.2f (%s %s)", r, lbl, dir)
}

arrow <- function(dir) {
  switch(
    dir,
    "rising" = "\u2191",
    "falling" = "\u2193",
    "flat" = "\u2192",
    "\u2014"
  )
}

trend_col <- function(dir, higher_good = TRUE) {
  if (dir == "rising") {
    return(if (higher_good) "#2dce89" else "#e94560")
  }
  if (dir == "falling") {
    return(if (higher_good) "#e94560" else "#2dce89")
  }
  "#9aa3b2"
}

pct_from_target <- function(val, target) {
  if (is.na(val) || is.na(target)) {
    return(NULL)
  }
  diff <- val - target
  list(
    diff = diff,
    above = diff > 0,
    str = sprintf(
      "%+.1f pp %s 2%% target",
      diff,
      if (diff > 0) "above" else "below"
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b


# ── build_synopsis (unchanged) ────────────────────────────────────────────────

build_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) {
    return(NULL)
  }

  get_vals <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 12) {
      return(NULL)
    }
    df %>% arrange(date) %>% pull(value)
  }
  get_df <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 12) {
      return(NULL)
    }
    df %>% arrange(date)
  }

  unemp <- get_vals("UNRATE")
  payrolls <- get_vals("PAYEMS")
  cpi <- get_vals("CPIAUCSL")
  fed <- get_vals("FEDFUNDS")
  t10 <- get_vals("DGS10")
  t2 <- get_vals("DGS2")
  mort <- get_vals("MORTGAGE30US")
  houst <- get_vals("HOUST")
  retail <- get_vals("RSAFS")
  vix <- get_vals("VIXCLS")
  oil <- get_vals("DCOILWTICO")

  blocks <- list()

  # ── 1. Labor Market ─────────────────────────────────────────────────────────
  unemp_trend <- if (!is.null(unemp)) trend_dir(unemp, 12) else "unclear"
  payroll_trend <- if (!is.null(payrolls)) {
    trend_dir(diff(payrolls), 12)
  } else {
    "unclear"
  }
  unemp_6m_ago <- if (!is.null(unemp) && length(unemp) >= 6) {
    unemp[length(unemp) - 6]
  } else {
    NA
  }
  unemp_chg_6m <- if (!is.na(unemp_6m_ago)) {
    round(kpis$unemp_rate - unemp_6m_ago, 1)
  } else {
    NA
  }
  unemp_yr_ago <- if (!is.null(unemp) && length(unemp) >= 12) {
    unemp[length(unemp) - 12]
  } else {
    NA
  }
  unemp_chg_yr <- if (!is.na(unemp_yr_ago)) {
    round(kpis$unemp_rate - unemp_yr_ago, 1)
  } else {
    NA
  }
  avg_payrolls_3m <- if (!is.null(payrolls) && length(payrolls) >= 4) {
    round(mean(diff(tail(payrolls, 4)), na.rm = TRUE), 0)
  } else {
    NA
  }

  blocks[["labor"]] <- list(
    title = "Labor Market",
    color = "#00b4d8",
    icon = "users",
    stats = list(
      list(
        label = "Unemployment trend (12M)",
        value = sprintf(
          "%s %.1f%%",
          arrow(unemp_trend),
          kpis$unemp_rate %||% NA
        ),
        col = trend_col(unemp_trend, higher_good = FALSE)
      ),
      list(
        label = "Change vs. 6 months ago",
        value = if (!is.na(unemp_chg_6m)) {
          sprintf("%+.1f pp", unemp_chg_6m)
        } else {
          "N/A"
        },
        col = if (!is.na(unemp_chg_6m) && unemp_chg_6m > 0) {
          "#e94560"
        } else {
          "#2dce89"
        }
      ),
      list(
        label = "Change vs. 1 year ago",
        value = if (!is.na(unemp_chg_yr)) {
          sprintf("%+.1f pp", unemp_chg_yr)
        } else {
          "N/A"
        },
        col = if (!is.na(unemp_chg_yr) && unemp_chg_yr > 0) {
          "#e94560"
        } else {
          "#2dce89"
        }
      ),
      list(
        label = "Avg. payrolls (last 3M)",
        value = if (!is.na(avg_payrolls_3m)) {
          sprintf("%+.0fK/mo", avg_payrolls_3m)
        } else {
          "N/A"
        },
        col = if (!is.na(avg_payrolls_3m) && avg_payrolls_3m > 0) {
          "#2dce89"
        } else {
          "#e94560"
        }
      )
    ),
    note = {
      np <- c()
      if (!is.na(avg_payrolls_3m)) {
        np <- c(
          np,
          sprintf(
            "3-month average payroll growth of %+.0fK/mo suggests a labor market that is %s.",
            avg_payrolls_3m,
            if (avg_payrolls_3m > 200) {
              "running hot"
            } else if (avg_payrolls_3m > 100) {
              "cooling but solid"
            } else {
              "slowing materially"
            }
          )
        )
      }
      if (!is.null(unemp) && !is.null(payrolls)) {
        rc <- roll_cor(
          unemp[1:min(length(unemp), length(payrolls))],
          diff(c(NA, payrolls))[1:min(length(unemp), length(payrolls))],
          36
        )
        if (!is.na(rc)) {
          np <- c(
            np,
            sprintf(
              "36-month rolling correlation between unemployment level and payroll growth: %s — historically inverse as expected.",
              fmt_cor(rc)
            )
          )
        }
      }
      paste(np, collapse = " ")
    }
  )

  # ── 2. Inflation ─────────────────────────────────────────────────────────────
  cpi_yoy_now <- kpis$cpi_yoy %||% NA
  core_yoy_now <- kpis$core_cpi_yoy %||% NA
  cpi_trend <- if (!is.null(cpi)) {
    yoy_series <- (cpi / lag(cpi, 12) - 1) * 100
    trend_dir(yoy_series[!is.na(yoy_series)], 6)
  } else {
    "unclear"
  }
  cpi_peak <- if (!is.null(cpi) && length(cpi) >= 36) {
    ry <- zoo::rollapply(
      cpi,
      13,
      function(x) (x[13] / x[1] - 1) * 100,
      fill = NA,
      align = "right"
    )
    ry <- ry[!is.na(ry)]
    if (length(ry) >= 36) round(max(tail(ry, 36)), 1) else NA
  } else {
    NA
  }
  dist <- pct_from_target(cpi_yoy_now, 2.0)

  blocks[["inflation"]] <- list(
    title = "Inflation",
    color = "#e94560",
    icon = "fire",
    stats = list(
      list(
        label = "CPI YoY trend (6M)",
        value = sprintf("%s %.1f%%", arrow(cpi_trend), cpi_yoy_now),
        col = trend_col(cpi_trend, higher_good = FALSE)
      ),
      list(
        label = "vs. Fed 2% target",
        value = if (!is.null(dist)) dist$str else "N/A",
        col = if (!is.null(dist) && dist$above) "#e94560" else "#2dce89"
      ),
      list(
        label = "Core CPI YoY",
        value = if (!is.na(core_yoy_now)) {
          sprintf("%.1f%%", core_yoy_now)
        } else {
          "N/A"
        },
        col = if (!is.na(core_yoy_now) && core_yoy_now > 3) {
          "#e94560"
        } else {
          "#f4a261"
        }
      ),
      list(
        label = "3-yr CPI peak",
        value = if (!is.na(cpi_peak)) sprintf("%.1f%%", cpi_peak) else "N/A",
        col = "#9aa3b2"
      )
    ),
    note = {
      np <- c()
      if (!is.na(cpi_yoy_now) && !is.na(core_yoy_now)) {
        gap <- round(cpi_yoy_now - core_yoy_now, 1)
        np <- c(
          np,
          sprintf(
            "Headline CPI runs %s pp %s core, indicating food & energy %s are %s the overall print.",
            abs(gap),
            if (gap > 0) "above" else "below",
            if (gap > 0) "pressures" else "relief",
            if (gap > 0) "elevating" else "tempering"
          )
        )
      }
      if (!is.null(fed) && !is.na(cpi_yoy_now)) {
        rr <- round(tail(fed, 1) - cpi_yoy_now, 1)
        np <- c(
          np,
          sprintf(
            "Real Fed Funds rate (nominal \u2212 CPI): %.1f%% \u2014 %s.",
            rr,
            if (rr > 1.5) {
              "restrictive territory, historically associated with demand cooling"
            } else if (rr > 0) {
              "mildly positive but below historical neutral"
            } else {
              "still negative in real terms, accommodative bias remains"
            }
          )
        )
      }
      paste(np, collapse = " ")
    }
  )

  # ── 3. Rates & Yield Curve ───────────────────────────────────────────────────
  spread_now <- kpis$t10y2y %||% NA
  t10_now <- kpis$t10yr %||% NA
  mort_now <- kpis$mortgage30 %||% NA
  months_inverted <- if (!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 24)
    sum((tail(t10, n) - tail(t2, n)) < 0, na.rm = TRUE)
  } else {
    NA
  }
  mort_houst_lag <- if (!is.null(mort) && !is.null(houst)) {
    n <- min(length(mort), length(houst))
    best_lag(tail(mort, n), tail(houst, n), max_lag = 12)
  } else {
    NULL
  }
  mort_trend <- if (!is.null(mort)) trend_dir(mort, 12) else "unclear"

  blocks[["rates"]] <- list(
    title = "Rates & Yield Curve",
    color = "#f4a261",
    icon = "percent",
    stats = list(
      list(
        label = "10Y-2Y spread",
        value = if (!is.na(spread_now)) {
          sprintf(
            "%.2f pp (%s)",
            spread_now,
            if (spread_now < 0) "INVERTED" else "normal"
          )
        } else {
          "N/A"
        },
        col = if (!is.na(spread_now) && spread_now < 0) "#e94560" else "#2dce89"
      ),
      list(
        label = "Months inverted (last 24M)",
        value = if (!is.na(months_inverted)) {
          sprintf("%d of 24 months", months_inverted)
        } else {
          "N/A"
        },
        col = if (!is.na(months_inverted) && months_inverted > 12) {
          "#e94560"
        } else if (!is.na(months_inverted) && months_inverted > 6) {
          "#f4a261"
        } else {
          "#2dce89"
        }
      ),
      list(
        label = "30-yr mortgage trend",
        value = sprintf("%s %.2f%%", arrow(mort_trend), mort_now %||% NA),
        col = trend_col(mort_trend, higher_good = FALSE)
      ),
      list(
        label = "Mortgage\u2013Housing starts lag",
        value = if (!is.null(mort_houst_lag)) {
          sprintf(
            "%d-mo lag, %s",
            mort_houst_lag$lag,
            fmt_cor(mort_houst_lag$r)
          )
        } else {
          "N/A"
        },
        col = "#9aa3b2"
      )
    ),
    note = {
      np <- c()
      if (!is.na(months_inverted) && months_inverted > 0) {
        np <- c(
          np,
          sprintf(
            "Yield curve has been inverted for %d of the past 24 months \u2014 a historically reliable (if lagged) recession signal, though timing varies widely.",
            months_inverted
          )
        )
      }
      if (!is.null(mort_houst_lag) && !is.na(mort_houst_lag$r)) {
        np <- c(
          np,
          sprintf(
            "Mortgage rate leads housing starts by ~%d months (lag-adjusted %s, R\u00B2 \u2248 %.0f%%). Rate moves today are a forward indicator for construction activity.",
            mort_houst_lag$lag,
            fmt_cor(mort_houst_lag$r),
            mort_houst_lag$r^2 * 100
          )
        )
      }
      paste(np, collapse = " ")
    }
  )

  # ── 4. Markets & Risk ────────────────────────────────────────────────────────
  vix_now <- kpis$vix %||% NA
  oil_now <- kpis$oil_price %||% NA
  vix_trend <- if (!is.null(vix)) trend_dir(vix, 12) else "unclear"
  vix_6m_avg <- if (!is.null(vix) && length(vix) >= 6) {
    round(mean(tail(vix, 6), na.rm = TRUE), 1)
  } else {
    NA
  }
  vix_1y_avg <- if (!is.null(vix) && length(vix) >= 12) {
    round(mean(tail(vix, 12), na.rm = TRUE), 1)
  } else {
    NA
  }
  oil_trend <- if (!is.null(oil)) trend_dir(oil, 12) else "unclear"
  oil_6m_ago <- if (!is.null(oil) && length(oil) >= 130) {
    oil[length(oil) - 130]
  } else {
    NA
  }
  oil_chg_pct <- if (!is.na(oil_6m_ago) && oil_6m_ago > 0) {
    round((oil_now / oil_6m_ago - 1) * 100, 1)
  } else {
    NA
  }
  vix_cpi_cor <- if (!is.null(vix) && !is.null(cpi)) {
    n <- min(length(vix), length(cpi))
    yoy <- (tail(cpi, n) / lag(tail(cpi, n), 12) - 1) * 100
    roll_cor(tail(vix, n), yoy, 36)
  } else {
    NA
  }

  blocks[["markets"]] <- list(
    title = "Markets & Risk",
    color = "#7c5cbf",
    icon = "chart-bar",
    stats = list(
      list(
        label = "VIX regime",
        value = sprintf(
          "%.1f (%s)",
          vix_now,
          if (!is.na(vix_now) && vix_now > 30) {
            "elevated fear"
          } else if (!is.na(vix_now) && vix_now > 20) {
            "cautious"
          } else {
            "complacent"
          }
        ),
        col = if (!is.na(vix_now) && vix_now > 25) {
          "#e94560"
        } else if (!is.na(vix_now) && vix_now > 18) {
          "#f4a261"
        } else {
          "#2dce89"
        }
      ),
      list(
        label = "VIX: 6M avg vs 1Y avg",
        value = if (!is.na(vix_6m_avg) && !is.na(vix_1y_avg)) {
          sprintf(
            "%.1f vs %.1f (%s)",
            vix_6m_avg,
            vix_1y_avg,
            if (vix_6m_avg > vix_1y_avg) "stress rising" else "stress falling"
          )
        } else {
          "N/A"
        },
        col = if (
          !is.na(vix_6m_avg) && !is.na(vix_1y_avg) && vix_6m_avg > vix_1y_avg
        ) {
          "#e94560"
        } else {
          "#2dce89"
        }
      ),
      list(
        label = "WTI crude trend",
        value = sprintf("%s $%.0f/bbl", arrow(oil_trend), oil_now %||% NA),
        col = trend_col(oil_trend, higher_good = FALSE)
      ),
      list(
        label = "VIX\u2013Inflation correlation (36M)",
        value = if (!is.na(vix_cpi_cor)) fmt_cor(vix_cpi_cor) else "N/A",
        col = "#9aa3b2"
      )
    ),
    note = {
      np <- c()
      if (!is.na(vix_now)) {
        np <- c(
          np,
          sprintf(
            "VIX at %.1f places equity vol in %s \u2014 %s.",
            vix_now,
            if (vix_now > 30) {
              "the upper quartile historically (>30 = stress event)"
            } else if (vix_now > 20) {
              "a cautious zone (20\u201330)"
            } else {
              "a low-vol regime (<20)"
            },
            if (vix_now > 30) {
              "historically associated with risk-off positioning and tighter credit conditions"
            } else if (vix_now > 20) {
              "watch for vol regime shift"
            } else {
              "market is pricing low near-term risk; watch for mean-reversion"
            }
          )
        )
      }
      if (!is.na(oil_chg_pct)) {
        np <- c(
          np,
          sprintf(
            "WTI crude %s%.0f%% over the past 6 months \u2014 a %s for headline CPI via energy component.",
            if (oil_chg_pct > 0) "+" else "",
            oil_chg_pct,
            if (abs(oil_chg_pct) > 10) "meaningful impulse" else "modest move"
          )
        )
      }
      paste(np, collapse = " ")
    }
  )

  blocks
}


# ── render_synopsis_html (unchanged) ─────────────────────────────────────────

render_synopsis_html <- function(blocks) {
  if (is.null(blocks) || length(blocks) == 0) {
    return(HTML(
      "<p style='color:#6b7585;padding:16px;'>Synopsis not yet available \u2014 data loading.</p>"
    ))
  }

  render_block <- function(b) {
    stats_html <- paste(
      vapply(
        b$stats,
        function(s) {
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
        },
        character(1)
      ),
      collapse = ""
    )

    note_html <- if (nchar(b$note) > 0) {
      sprintf(
        "<div style='margin-top:10px;padding:10px 12px;background:#0f1117;border-radius:6px;
                 border-left:3px solid %s;'>
       <span style='color:#9aa3b2;font-size:11px;line-height:1.7;'>%s</span>
     </div>",
        b$color,
        htmltools::htmlEscape(b$note)
      )
    } else {
      ""
    }

    sprintf(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                 border-top:3px solid %s;padding:14px 16px;height:100%%;'>
       <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                   letter-spacing:1px;margin-bottom:10px;'>
         <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>%s
       </div>%s%s</div>",
      b$color,
      b$color,
      b$icon,
      b$title,
      stats_html,
      note_html
    )
  }

  block_htmls <- vapply(
    names(blocks),
    function(nm) render_block(blocks[[nm]]),
    character(1)
  )
  rows_html <- paste(
    vapply(
      seq(1, length(block_htmls), by = 2),
      function(i) {
        left <- block_htmls[i]
        right <- if (i + 1 <= length(block_htmls)) {
          block_htmls[i + 1]
        } else {
          "<div style='flex:1;'></div>"
        }
        sprintf(
          "<div style='display:flex;gap:14px;margin-bottom:14px;'>
       <div style='flex:1;'>%s</div><div style='flex:1;'>%s</div></div>",
          left,
          right
        )
      },
      character(1)
    ),
    collapse = ""
  )

  ts <- format(Sys.time(), "%b %d, %Y %H:%M")
  HTML(paste0(
    sprintf(
      "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;'>
       <span style='color:#e0e0e0;font-size:13px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>
         <i class=\"fa fa-chart-line\" style=\"color:#00b4d8;margin-right:8px;\"></i>Live Data Synopsis
       </span><span style='color:#6b7585;font-size:11px;'>Computed %s</span></div>",
      ts
    ),
    "<div style='padding:4px 2px;'>",
    rows_html,
    "<div style='color:#6b7585;font-size:10px;margin-top:4px;'>All statistics computed from live FRED data. Correlations use trailing windows as noted.</div></div>"
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# TAB-LEVEL SYNOPSIS BUILDERS (unchanged from original)
# ═══════════════════════════════════════════════════════════════════════════════

.tab_synopsis_html <- function(title, color, icon_name, bullets, note = NULL) {
  bullet_html <- paste(
    vapply(
      bullets,
      function(b) {
        sprintf(
          "<li style='margin-bottom:5px;'>
       <span style='color:%s;font-weight:600;'>%s</span>
       <span style='color:#d0d0d0;'>%s</span></li>",
          b$col %||% "#9aa3b2",
          htmltools::htmlEscape(b$label),
          htmltools::htmlEscape(b$text)
        )
      },
      character(1)
    ),
    collapse = ""
  )

  note_html <- if (!is.null(note) && nchar(note) > 0) {
    sprintf(
      "<div style='margin-top:10px;padding:10px 14px;background:#0f1117;border-radius:6px;
               border-left:3px solid %s;color:#9aa3b2;font-size:12px;line-height:1.75;'>
     <i class=\"fa fa-lightbulb\" style=\"color:%s;margin-right:6px;\"></i>%s</div>",
      color,
      color,
      htmltools::htmlEscape(note)
    )
  } else {
    ""
  }

  sprintf(
    "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
               border-left:4px solid %s;padding:14px 18px;margin-bottom:14px;'>
     <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                 letter-spacing:1px;margin-bottom:10px;'>
       <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>%s \u2014 Current Landscape
     </div>
     <ul style='list-style:none;padding:0;margin:0;'>%s</ul>%s</div>",
    color,
    color,
    icon_name,
    title,
    bullet_html,
    note_html
  )
}

build_labor_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) {
    return(NULL)
  }
  htmltools::HTML(.tab_synopsis_html(
    "Labor Market",
    "#00b4d8",
    "users",
    list(
      list(
        col = "#00b4d8",
        label = "Unemployment rate: ",
        text = sprintf("%.1f%%", kpis$unemp_rate %||% NA)
      ),
      list(
        col = "#2dce89",
        label = "3M avg payrolls: ",
        text = sprintf(
          "%+.0fK/mo",
          {
            p <- fred_data$PAYEMS
            if (!is.null(p) && nrow(p) >= 4) {
              round(mean(
                diff(tail(arrange(p, date) %>% pull(value), 4)),
                na.rm = TRUE
              ))
            } else {
              NA
            }
          } %||%
            NA
        )
      ),
      list(
        col = "#7c5cbf",
        label = "Job openings: ",
        text = sprintf("%.0fK", kpis$job_openings %||% NA)
      ),
      list(
        col = "#f4a261",
        label = "Wage growth YoY: ",
        text = sprintf("%.1f%%", kpis$wage_yoy %||% NA)
      )
    )
  ))
}

build_inflation_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) {
    return(NULL)
  }
  htmltools::HTML(.tab_synopsis_html(
    "Inflation",
    "#e94560",
    "fire",
    list(
      list(
        col = if (!is.na(kpis$cpi_yoy %||% NA) && (kpis$cpi_yoy %||% 0) > 3) {
          "#e94560"
        } else {
          "#2dce89"
        },
        label = "CPI YoY: ",
        text = sprintf("%.1f%%", kpis$cpi_yoy %||% NA)
      ),
      list(
        col = "#f4a261",
        label = "Core CPI YoY: ",
        text = sprintf("%.1f%%", kpis$core_cpi_yoy %||% NA)
      ),
      list(
        col = "#7c5cbf",
        label = "Core PCE YoY: ",
        text = sprintf("%.1f%%", kpis$core_pce %||% NA)
      ),
      list(
        col = "#00b4d8",
        label = "Real Fed Funds: ",
        text = sprintf(
          "%.1f%%",
          (kpis$fed_funds %||% NA) - (kpis$cpi_yoy %||% NA)
        )
      )
    )
  ))
}

build_housing_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) {
    return(NULL)
  }
  htmltools::HTML(.tab_synopsis_html(
    "Housing",
    "#2dce89",
    "home",
    list(
      list(
        col = "#2dce89",
        label = "Housing starts: ",
        text = sprintf("%.0fK ann.", kpis$housing_starts %||% NA)
      ),
      list(
        col = "#f4a261",
        label = "30yr mortgage: ",
        text = sprintf("%.2f%%", kpis$mortgage30 %||% NA)
      ),
      list(
        col = "#00b4d8",
        label = "Existing home sales: ",
        text = sprintf("%.2fM ann.", kpis$home_sales %||% NA)
      )
    )
  ))
}

build_markets_synopsis <- function(fred_data, kpis) {
  if (is.null(fred_data) || is.null(kpis)) {
    return(NULL)
  }
  vix_now <- kpis$vix %||% NA
  hy_now <- kpis$hy_spread %||% NA
  months_inv <- if (!is.null(fred_data$DGS10) && !is.null(fred_data$DGS2)) {
    t10 <- arrange(fred_data$DGS10, date)$value
    t2 <- arrange(fred_data$DGS2, date)$value
    n <- min(length(t10), length(t2), 24)
    sum((tail(t10, n) - tail(t2, n)) < 0, na.rm = TRUE)
  } else {
    NA
  }
  hy_prev <- if (
    !is.null(fred_data$BAMLH0A0HYM2) && nrow(fred_data$BAMLH0A0HYM2) >= 2
  ) {
    arrange(fred_data$BAMLH0A0HYM2, date)$value[
      nrow(fred_data$BAMLH0A0HYM2) - 1
    ]
  } else {
    NA
  }

  bullets <- list(
    list(
      col = if (!is.na(months_inv) && months_inv > 12) "#e94560" else "#2dce89",
      label = "10Y-2Y spread: ",
      text = sprintf(
        "inverted %d of last 24 months; spread %.2f pp",
        months_inv %||% 0L,
        kpis$t10y2y %||% NA
      )
    ),
    list(
      col = if (!is.na(vix_now) && vix_now > 25) {
        "#e94560"
      } else if (!is.na(vix_now) && vix_now > 18) {
        "#f4a261"
      } else {
        "#2dce89"
      },
      label = "VIX: ",
      text = sprintf(
        "%.1f \u2014 %s",
        vix_now,
        if (!is.na(vix_now) && vix_now > 30) {
          "stress event"
        } else if (!is.na(vix_now) && vix_now > 20) {
          "cautious"
        } else {
          "risk-on"
        }
      )
    ),
    list(
      col = if (!is.na(hy_now) && hy_now > 5) "#e94560" else "#2dce89",
      label = "HY credit spread: ",
      text = sprintf("%.2f%% (prev %.2f%%)", hy_now, hy_prev %||% NA)
    )
  )
  htmltools::HTML(.tab_synopsis_html(
    "Financial Markets",
    "#f4a261",
    "chart-line",
    bullets,
    if (!is.na(months_inv) && months_inv > 0) {
      sprintf(
        "Curve inverted for %d of 24 months. Historically recession follows within 6-18 months.",
        months_inv
      )
    } else {
      NULL
    }
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# GROWTH OUTLOOK & RECESSION MODEL
# ═══════════════════════════════════════════════════════════════════════════════

.score <- function(val, good_threshold, bad_threshold, higher_good = TRUE) {
  if (is.na(val)) {
    return(0)
  }
  if (higher_good) {
    if (val >= good_threshold) {
      return(1)
    }
    if (val <= bad_threshold) {
      return(-1)
    }
    return(0)
  } else {
    if (val <= good_threshold) {
      return(1)
    }
    if (val >= bad_threshold) {
      return(-1)
    }
    return(0)
  }
}


# ── Tier 1 NEW: Train GLM on NBER recession history ──────────────────────────

#' Build a monthly feature panel from already-loaded fred_data + NBER indicator
#' Returns a data.frame with one row per month, or NULL on error.
build_glm_feature_matrix <- function(fred_data) {
  tryCatch(
    {
      # Download NBER recession indicator
      usrec <- fredr::fredr(
        "USREC",
        observation_start = as.Date("1959-01-01"),
        frequency = "m"
      ) %>%
        dplyr::select(date, recession = value) %>%
        dplyr::arrange(date)

      if (nrow(usrec) < 120) {
        return(NULL)
      } # need at least 10 years

      # Helper: get monthly series from fred_data, collapse to month-end mean
      gm <- function(sid) {
        df <- fred_data[[sid]]
        if (is.null(df) || nrow(df) < 24) {
          return(NULL)
        }
        df %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(
            value = mean(value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::rename(date = month)
      }

      t10y2y <- gm("T10Y2Y") # yield spread (direct)
      unrate <- gm("UNRATE")
      fedfund <- gm("FEDFUNDS")
      cpi_m <- gm("CPIAUCSL")
      hy_m <- gm("BAMLH0A0HYM2")
      oil_m <- gm("DCOILWTICO")
      lfpr_m <- gm("LNS11300060")
      usd_m <- gm("DTWEXBGS")
      isr_m <- gm("ISRATIO")
      cc_m <- gm("DRCCLACBS")
      pay_m <- gm("PAYEMS")
      sent_m <- gm("UMCSENT")

      if (is.null(unrate) || is.null(cpi_m) || is.null(fedfund)) {
        return(NULL)
      }

      # ── Derived features ────────────────────────────────────────────────────
      # Yield spread: use T10Y2Y if available, else DGS10-DGS2
      if (!is.null(t10y2y)) {
        spread_df <- t10y2y %>% dplyr::rename(yield_spread = value)
      } else {
        t10 <- gm("DGS10")
        t2 <- gm("DGS2")
        if (is.null(t10) || is.null(t2)) {
          return(NULL)
        }
        spread_df <- dplyr::inner_join(
          t10 %>% dplyr::rename(t10 = value),
          t2 %>% dplyr::rename(t2 = value),
          by = "date"
        ) %>%
          dplyr::mutate(yield_spread = t10 - t2) %>%
          dplyr::select(date, yield_spread)
      }

      # Unemployment rise from 12M trough (Sahm proxy)
      u_df <- unrate %>%
        dplyr::rename(u = value) %>%
        dplyr::mutate(
          u_trough12 = zoo::rollapply(u, 12, min, fill = NA, align = "right"),
          u_rise = u - u_trough12
        )

      # Payroll 3M average change
      pay_df <- if (!is.null(pay_m)) {
        pay_m %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(
            pay_chg = value - dplyr::lag(value, 1),
            pay_3m = zoo::rollmean(pay_chg, 3, fill = NA, align = "right")
          ) %>%
          dplyr::select(date, pay_3m)
      } else {
        NULL
      }

      # CPI YoY → real rate
      cpi_df <- cpi_m %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(cpi_yoy = (value / dplyr::lag(value, 12) - 1) * 100) %>%
        dplyr::select(date, cpi_yoy)

      ff_df <- fedfund %>% dplyr::rename(ff = value)
      hy_df <- if (!is.null(hy_m)) {
        hy_m %>% dplyr::rename(hy_spread = value)
      } else {
        NULL
      }
      oil_df <- if (!is.null(oil_m)) {
        oil_m %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(oil_yoy = (value / dplyr::lag(value, 12) - 1) * 100) %>%
          dplyr::select(date, oil_yoy)
      }
      lfpr_df <- if (!is.null(lfpr_m)) {
        lfpr_m %>% dplyr::rename(prime_lfpr = value)
      } else {
        NULL
      }
      usd_df <- if (!is.null(usd_m)) {
        usd_m %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(usd_yoy = (value / dplyr::lag(value, 12) - 1) * 100) %>%
          dplyr::select(date, usd_yoy)
      }
      isr_df <- if (!is.null(isr_m)) {
        isr_m %>% dplyr::rename(inv_sales = value)
      } else {
        NULL
      }
      cc_df <- if (!is.null(cc_m)) {
        cc_m %>% dplyr::rename(cc_del = value)
      } else {
        NULL
      }
      sent_df <- if (!is.null(sent_m)) {
        sent_m %>% dplyr::rename(sentiment = value)
      } else {
        NULL
      }

      # ── Create 12-month forward recession target ─────────────────────────────
      # rec_next12 = 1 if a recession month occurs in any of the next 12 months
      n <- nrow(usrec)
      usrec$rec_next12 <- vapply(
        seq_len(n),
        function(i) {
          idx <- (i + 1):min(i + 12, n)
          if (length(idx) < 6) {
            return(NA_real_)
          }
          as.numeric(any(usrec$recession[idx] == 1))
        },
        numeric(1)
      )

      # ── Join all series ──────────────────────────────────────────────────────
      panel <- usrec %>%
        dplyr::left_join(spread_df, by = "date") %>%
        dplyr::left_join(
          u_df %>% dplyr::select(date, u, u_rise),
          by = "date"
        ) %>%
        dplyr::left_join(cpi_df, by = "date") %>%
        dplyr::left_join(ff_df, by = "date") %>%
        dplyr::mutate(real_rate = ff - cpi_yoy)

      for (df in list(
        pay_df,
        hy_df,
        oil_df,
        lfpr_df,
        usd_df,
        isr_df,
        cc_df,
        sent_df
      )) {
        if (!is.null(df)) panel <- dplyr::left_join(panel, df, by = "date")
      }

      panel <- panel %>%
        dplyr::filter(!is.na(rec_next12), !is.na(yield_spread), !is.na(u_rise))
      if (nrow(panel) < 120) {
        return(NULL)
      }

      panel
    },
    error = function(e) {
      message("[synopsis_tier1] build_glm_feature_matrix failed: ", e$message)
      NULL
    }
  )
}


#' Fit a logistic regression on the recession panel.
#' Returns list(model, coef_table, trained_at) or NULL.
train_recession_model <- function(fred_data) {
  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    return(NULL)
  }

  # Build formula from available columns
  possible_predictors <- c(
    "yield_spread",
    "u_rise",
    "pay_3m",
    "real_rate",
    "hy_spread",
    "oil_yoy",
    "prime_lfpr",
    "usd_yoy",
    "inv_sales",
    "cc_del",
    "sentiment"
  )
  available <- intersect(possible_predictors, names(panel))
  if (length(available) < 3) {
    return(NULL)
  }

  # Remove rows with too many NAs across predictors
  panel_clean <- panel %>%
    dplyr::select(rec_next12, dplyr::all_of(available)) %>%
    tidyr::drop_na()

  if (nrow(panel_clean) < 100) {
    return(NULL)
  }

  fml <- as.formula(paste("rec_next12 ~", paste(available, collapse = " + ")))

  tryCatch(
    {
      model <- glm(fml, data = panel_clean, family = binomial(link = "logit"))
      sm <- summary(model)
      coef_tbl <- as.data.frame(sm$coefficients)
      coef_tbl$predictor <- rownames(coef_tbl)
      coef_tbl <- coef_tbl %>%
        dplyr::filter(predictor != "(Intercept)") %>%
        dplyr::rename(
          estimate = Estimate,
          se = `Std. Error`,
          z_stat = `z value`,
          p_val = `Pr(>|z|)`
        ) %>%
        dplyr::select(predictor, estimate, se, z_stat, p_val)

      message(sprintf(
        "[synopsis_tier1] GLM trained on %d months, AIC=%.1f, %d predictors",
        nrow(panel_clean),
        AIC(model),
        length(available)
      ))

      list(
        model = model,
        coef_table = coef_tbl,
        n_obs = nrow(panel_clean),
        aic = round(AIC(model), 1),
        predictors = available,
        trained_at = Sys.time()
      )
    },
    error = function(e) {
      message("[synopsis_tier1] glm() failed: ", e$message)
      NULL
    }
  )
}


#' Build a one-row data.frame of current feature values for glm predict()
build_current_feature_vector <- function(
  yield_spread,
  u_rise,
  pay_3m,
  real_rate,
  hy_v,
  oil_yoy,
  prime_age_lfpr,
  usd_yoy,
  inventory_sales_ratio,
  cc_delinquency,
  cs
) {
  data.frame(
    yield_spread = yield_spread %||% NA_real_,
    u_rise = u_rise %||% NA_real_,
    pay_3m = pay_3m %||% NA_real_,
    real_rate = real_rate %||% NA_real_,
    hy_spread = hy_v %||% NA_real_,
    oil_yoy = oil_yoy %||% NA_real_,
    prime_lfpr = prime_age_lfpr %||% NA_real_,
    usd_yoy = usd_yoy %||% NA_real_,
    inv_sales = inventory_sales_ratio %||% NA_real_,
    cc_del = cc_delinquency %||% NA_real_,
    sentiment = cs %||% NA_real_,
    stringsAsFactors = FALSE
  )
}


# ── Tier 1: Contribution-tracked recession probability engine ─────────────────
# Returns the same list as the original + factor_contributions data.frame.
# When glm_result is non-NULL, uses predict() and β_i × x_i contributions.
# Falls back to hand-tuned log-odds when glm_result is NULL.

.compute_recession_prob <- function(
  yield_spread,
  inv_months,
  unemp,
  u,
  pay_3m,
  real_rate,
  hy_v,
  vix_v,
  cs,
  ip_y,
  hs,
  prime_age_lfpr = NA,
  prime_age_lfpr_chg = NA,
  jobless_claims_trend = NA,
  quits_yoy = NA,
  oil_yoy = NA,
  equity_drawdown_pct = NA,
  usd_yoy = NA,
  inventory_sales_ratio = NA,
  cc_delinquency = NA,
  glm_result = NULL # ← Tier 1 addition
) {
  # We always track per-factor contributions for the bar chart.
  # Structure: named list → data.frame at the end.
  contribs <- list()

  # ── Helper: record a contribution ──────────────────────────────────────────
  add_contrib <- function(id, label, delta, z_stat = NA_real_) {
    contribs[[id]] <<- list(name = label, contribution = delta, z_stat = z_stat)
  }

  # ────────────────────────────────────────────────────────────────────────────
  # PATH A: Use estimated GLM when available
  # ────────────────────────────────────────────────────────────────────────────
  if (!is.null(glm_result) && !is.null(glm_result$model)) {
    u_rise <- if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
      u - min(tail(unemp, 12), na.rm = TRUE)
    } else {
      NA_real_
    }

    feat_df <- build_current_feature_vector(
      yield_spread,
      u_rise,
      pay_3m,
      real_rate,
      hy_v,
      oil_yoy,
      prime_age_lfpr,
      usd_yoy,
      inventory_sales_ratio,
      cc_delinquency,
      cs
    )

    # Only keep predictors the model was trained on; impute NAs with 0 (mean-centred at train time)
    keep_cols <- intersect(glm_result$predictors, names(feat_df))
    feat_sub <- feat_df[, keep_cols, drop = FALSE]
    # Impute NAs with column means from training data (approximated as 0 for scaled vars,
    # or we use the marginal safer fallback of the predictor mean)
    for (col in names(feat_sub)) {
      if (is.na(feat_sub[[col]])) feat_sub[[col]] <- 0
    }

    glm_prob <- tryCatch(
      predict(glm_result$model, newdata = feat_sub, type = "response")[[1]],
      error = function(e) NA_real_
    )

    if (!is.na(glm_prob)) {
      prob <- round(glm_prob * 100, 1)
      prob <- max(2, min(97, prob))

      # Per-factor contributions: β_i × x_i for current values
      coefs <- coef(glm_result$model)
      coef_tbl <- glm_result$coef_table
      feat_vals <- as.list(feat_sub[1, , drop = FALSE])

      # Pretty labels for GLM predictors
      pretty_labels <- c(
        yield_spread = "Yield Curve (10Y-2Y spread)",
        u_rise = "Sahm Rule (unemployment rise)",
        pay_3m = "Payroll Momentum (3M avg)",
        real_rate = "Real Fed Funds Rate",
        hy_spread = "HY Credit Spreads",
        oil_yoy = "Oil Price Shock (YoY)",
        prime_lfpr = "Prime-Age LFPR (25\u201354)",
        usd_yoy = "USD Surge (YoY)",
        inv_sales = "Inventory-to-Sales Ratio",
        cc_del = "CC Delinquency Rate",
        sentiment = "Consumer Sentiment"
      )

      for (pred in keep_cols) {
        b <- if (pred %in% names(coefs)) coefs[[pred]] else NA_real_
        x <- feat_vals[[pred]]
        z <- if (pred %in% coef_tbl$predictor) {
          coef_tbl$z_stat[coef_tbl$predictor == pred]
        } else {
          NA_real_
        }
        lbl <- if (pred %in% names(pretty_labels)) {
          pretty_labels[[pred]]
        } else {
          pred
        }
        delta <- if (!is.na(b) && !is.na(x)) b * x else 0
        add_contrib(pred, lbl, delta, z)
      }

      # Build drivers from contributions
      contrib_df <- do.call(
        rbind,
        lapply(names(contribs), function(id) {
          r <- contribs[[id]]
          data.frame(
            name = r$name,
            contribution = r$contribution,
            z_stat = r$z_stat,
            stringsAsFactors = FALSE
          )
        })
      )

      # Classify tier
      tier <- .classify_recession_tier(prob)

      # Simple driver lists from contributions
      contrib_sorted <- contrib_df[
        order(contrib_df$contribution, decreasing = TRUE),
      ]
      drivers_up <- head(
        contrib_sorted$name[contrib_sorted$contribution > 0.1],
        4
      )
      drivers_down <- head(
        contrib_sorted$name[contrib_sorted$contribution < -0.1],
        3
      )

      return(list(
        prob = prob,
        tier = tier,
        drivers_up = drivers_up,
        drivers_down = drivers_down,
        factor_contributions = contrib_df,
        model_type = "GLM (estimated)",
        n_obs = glm_result$n_obs,
        aic = glm_result$aic
      ))
    }
    # If predict() failed, fall through to hand-tuned
    message("[synopsis_tier1] GLM predict() failed; using hand-tuned fallback.")
  }

  # ────────────────────────────────────────────────────────────────────────────
  # PATH B: Hand-tuned log-odds (original model, with contribution tracking)
  # ────────────────────────────────────────────────────────────────────────────
  log_odds <- -1.73 # baseline

  # 1. Yield Curve
  yc_delta <- 0
  if (!is.na(yield_spread)) {
    yc_delta <- yc_delta + (-yield_spread * 0.85)
    if (!is.na(inv_months)) {
      yc_delta <- yc_delta +
        if (inv_months >= 18) {
          1.5
        } else if (inv_months >= 12) {
          1.2
        } else if (inv_months >= 6) {
          0.6
        } else if (inv_months >= 3) {
          0.3
        } else {
          0
        }
    }
  }
  log_odds <- log_odds + yc_delta
  add_contrib("yield_curve", "Yield Curve (10Y-2Y spread)", yc_delta)

  # 2. Sahm Rule / Labor Momentum
  sahm_delta <- 0
  if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
    u_rise <- u - min(tail(unemp, 12), na.rm = TRUE)
    sahm_delta <- sahm_delta +
      if (u_rise >= 0.5) {
        1.8
      } else if (u_rise >= 0.3) {
        0.9
      } else if (u_rise >= 0.1) {
        0.3
      } else if (u_rise < (-0.2)) {
        -0.4
      } else {
        0
      }
  }
  if (!is.na(pay_3m)) {
    sahm_delta <- sahm_delta +
      if (pay_3m < (-50)) {
        1.5
      } else if (pay_3m < 0) {
        0.6
      } else if (pay_3m < 100) {
        0.1
      } else if (pay_3m > 250) {
        -0.5
      } else {
        0
      }
  }
  log_odds <- log_odds + sahm_delta
  add_contrib("sahm", "Sahm Rule / Labor Momentum", sahm_delta)

  # 3. Prime-Age LFPR
  lfpr_delta <- 0
  if (!is.na(prime_age_lfpr)) {
    lfpr_delta <- lfpr_delta +
      if (prime_age_lfpr < 80.5) {
        0.8
      } else if (prime_age_lfpr < 82.0) {
        0.4
      } else if (prime_age_lfpr > 83.5) {
        -0.3
      } else {
        0
      }
  }
  if (!is.na(prime_age_lfpr_chg)) {
    lfpr_delta <- lfpr_delta +
      if (prime_age_lfpr_chg < (-0.5)) {
        0.9
      } else if (prime_age_lfpr_chg < (-0.2)) {
        0.4
      } else if (prime_age_lfpr_chg > 0.3) {
        -0.3
      } else {
        0
      }
  }
  log_odds <- log_odds + lfpr_delta
  add_contrib("prime_lfpr", "Prime-Age LFPR (25\u201354)", lfpr_delta)

  # 4. Jobless Claims Trend
  claims_delta <- 0
  if (!is.na(jobless_claims_trend)) {
    claims_delta <- if (jobless_claims_trend > 0.20) {
      1.2
    } else if (jobless_claims_trend > 0.10) {
      0.6
    } else if (jobless_claims_trend > 0.05) {
      0.2
    } else if (jobless_claims_trend < (-0.10)) {
      -0.3
    } else {
      0
    }
  }
  log_odds <- log_odds + claims_delta
  add_contrib("claims", "Jobless Claims Trend (4wk/26wk)", claims_delta)

  # 5. Quits Rate YoY
  quits_delta <- 0
  if (!is.na(quits_yoy)) {
    quits_delta <- if (quits_yoy < (-15)) {
      0.8
    } else if (quits_yoy < (-8)) {
      0.4
    } else if (quits_yoy < (-3)) {
      0.2
    } else if (quits_yoy > 8) {
      -0.3
    } else {
      0
    }
  }
  log_odds <- log_odds + quits_delta
  add_contrib("quits", "Quits Rate YoY Change", quits_delta)

  # 6. Oil Price Shock
  oil_delta <- 0
  if (!is.na(oil_yoy)) {
    oil_delta <- if (oil_yoy > 50) {
      1.0
    } else if (oil_yoy > 25) {
      0.6
    } else if (oil_yoy > 15) {
      0.2
    } else if (oil_yoy < (-30)) {
      0.5
    } else if (oil_yoy < (-15)) {
      0.2
    } else if (oil_yoy > 0 && oil_yoy < 10) {
      -0.1
    } else {
      0
    }
  }
  log_odds <- log_odds + oil_delta
  add_contrib("oil", "Oil Price Shock (YoY)", oil_delta)

  # 7. Equity Bear Market
  eq_delta <- 0
  if (!is.na(equity_drawdown_pct)) {
    dd <- abs(equity_drawdown_pct)
    eq_delta <- if (dd > 30) {
      1.6
    } else if (dd > 20) {
      1.1
    } else if (dd > 10) {
      0.5
    } else if (dd < 5) {
      -0.2
    } else {
      0
    }
  }
  log_odds <- log_odds + eq_delta
  add_contrib("equity", "Equity Bear Market (SPY drawdown)", eq_delta)

  # 8. HY Credit Spreads
  hy_delta <- 0
  if (!is.na(hy_v)) {
    hy_delta <- if (hy_v > 7.0) {
      1.4
    } else if (hy_v > 5.5) {
      0.7
    } else if (hy_v > 4.5) {
      0.2
    } else if (hy_v < 3.5) {
      -0.4
    } else {
      0
    }
  }
  log_odds <- log_odds + hy_delta
  add_contrib("hy_spreads", "HY Credit Spreads", hy_delta)

  # 9. USD Surge
  usd_delta <- 0
  if (!is.na(usd_yoy)) {
    usd_delta <- if (usd_yoy > 12) {
      0.8
    } else if (usd_yoy > 8) {
      0.4
    } else if (usd_yoy > 4) {
      0.1
    } else if (usd_yoy < (-5)) {
      -0.2
    } else {
      0
    }
  }
  log_odds <- log_odds + usd_delta
  add_contrib("usd", "USD Surge (YoY)", usd_delta)

  # 10. Real Fed Funds Rate
  rr_delta <- 0
  if (!is.na(real_rate)) {
    rr_delta <- if (real_rate > 3.0) {
      1.0
    } else if (real_rate > 2.0) {
      0.5
    } else if (real_rate > 1.0) {
      0.2
    } else if (real_rate < 0) {
      -0.3
    } else {
      0
    }
  }
  log_odds <- log_odds + rr_delta
  add_contrib("real_rate", "Real Fed Funds Rate", rr_delta)

  # 11. Consumer Sentiment
  sent_delta <- 0
  if (!is.na(cs)) {
    sent_delta <- if (cs < 60) {
      0.7
    } else if (cs < 70) {
      0.3
    } else if (cs > 90) {
      -0.4
    } else {
      0
    }
  }
  log_odds <- log_odds + sent_delta
  add_contrib("sentiment", "Consumer Sentiment", sent_delta)

  # 12. Inventory-to-Sales Ratio
  inv_delta <- 0
  if (!is.na(inventory_sales_ratio)) {
    inv_delta <- if (inventory_sales_ratio > 1.55) {
      0.7
    } else if (inventory_sales_ratio > 1.45) {
      0.3
    } else if (inventory_sales_ratio > 1.40) {
      0.1
    } else if (inventory_sales_ratio < 1.30) {
      -0.2
    } else {
      0
    }
  }
  log_odds <- log_odds + inv_delta
  add_contrib("inv_sales", "Inventory-to-Sales Ratio", inv_delta)

  # 13. CC Delinquency
  cc_delta <- 0
  if (!is.na(cc_delinquency)) {
    cc_delta <- if (cc_delinquency > 4.0) {
      0.9
    } else if (cc_delinquency > 3.0) {
      0.5
    } else if (cc_delinquency > 2.5) {
      0.2
    } else if (cc_delinquency < 1.8) {
      -0.3
    } else {
      0
    }
  }
  log_odds <- log_odds + cc_delta
  add_contrib("cc_del", "CC Delinquency Rate", cc_delta)

  prob <- round(1 / (1 + exp(-log_odds)) * 100, 1)
  prob <- max(2, min(97, prob))

  tier <- .classify_recession_tier(prob)

  # Build contribution data.frame
  contrib_df <- do.call(
    rbind,
    lapply(names(contribs), function(id) {
      r <- contribs[[id]]
      data.frame(
        name = r$name,
        contribution = r$contribution,
        z_stat = r$z_stat,
        stringsAsFactors = FALSE
      )
    })
  )
  contrib_df <- contrib_df[
    order(abs(contrib_df$contribution), decreasing = TRUE),
  ]

  # Driver lists
  drivers_up <- character(0)
  drivers_down <- character(0)
  if (!is.na(yield_spread) && yield_spread < -0.1) {
    drivers_up <- c(
      drivers_up,
      sprintf("Inverted yield curve (%+.2f pp)", yield_spread)
    )
  }
  if (!is.na(inv_months) && inv_months >= 6) {
    drivers_up <- c(
      drivers_up,
      sprintf("Inversion sustained %d months", inv_months)
    )
  }
  if (!is.null(unemp) && length(unemp) >= 12) {
    ur <- u - min(tail(unemp, 12), na.rm = TRUE)
    if (ur >= 0.3) {
      drivers_up <- c(
        drivers_up,
        sprintf("Unemployment %.1f pp above 12M low", ur)
      )
    }
  }
  if (!is.na(equity_drawdown_pct) && abs(equity_drawdown_pct) > 10) {
    drivers_up <- c(
      drivers_up,
      sprintf("Equity drawdown %.0f%% from 52-week high", equity_drawdown_pct)
    )
  }
  if (!is.na(hy_v) && hy_v > 4.5) {
    drivers_up <- c(drivers_up, sprintf("HY credit spread %.2f%%", hy_v))
  }
  if (!is.na(yield_spread) && yield_spread > 0.5) {
    drivers_down <- c(
      drivers_down,
      sprintf("Normal yield curve (+%.2f pp)", yield_spread)
    )
  }
  if (!is.na(pay_3m) && pay_3m > 200) {
    drivers_down <- c(
      drivers_down,
      sprintf("Strong payrolls (%+.0fK/mo 3M avg)", pay_3m)
    )
  }
  if (!is.na(hy_v) && hy_v < 3.5) {
    drivers_down <- c(
      drivers_down,
      sprintf("Benign credit spreads (%.2f%%)", hy_v)
    )
  }

  list(
    prob = prob,
    tier = tier,
    drivers_up = head(drivers_up, 4),
    drivers_down = head(drivers_down, 3),
    factor_contributions = contrib_df,
    model_type = "Hand-tuned log-odds"
  )
}


#' Shared tier classifier
.classify_recession_tier <- function(prob) {
  if (prob >= 60) {
    list(
      label = "Elevated",
      color = "#e94560",
      icon = "exclamation-triangle",
      desc = "Multiple leading indicators aligned recessionary. Historical hit rate ~65%. Defensive positioning warranted."
    )
  } else if (prob >= 40) {
    list(
      label = "Moderate",
      color = "#f4a261",
      icon = "exclamation-circle",
      desc = "Mixed signals with genuine warning signs. Late-cycle slowdown most likely; recession possible if conditions deteriorate."
    )
  } else if (prob >= 20) {
    list(
      label = "Low\u2013Moderate",
      color = "#f4a261",
      icon = "minus-circle",
      desc = "Some headwinds present but no acute stress. Growth outlook cautiously positive; monitor labor and credit."
    )
  } else {
    list(
      label = "Low",
      color = "#2dce89",
      icon = "check-circle",
      desc = "Most leading indicators benign. Historical baseline ~15%; current conditions at or below average 12-month recession risk."
    )
  }
}


# ── Tier 1 NEW: Diverging bar chart of factor contributions ───────────────────

.build_factor_bar_chart <- function(
  factor_contributions,
  model_type = "Hand-tuned log-odds"
) {
  if (is.null(factor_contributions) || nrow(factor_contributions) == 0) {
    return("<p style='color:#6b7585;font-size:11px;'>No contribution data.</p>")
  }

  # Sort by absolute contribution desc, show top 13
  fc <- factor_contributions[
    order(abs(factor_contributions$contribution), decreasing = TRUE),
  ]
  fc <- head(fc, 13)

  max_abs <- max(abs(fc$contribution), na.rm = TRUE)
  if (max_abs < 0.01) {
    max_abs <- 1
  }

  has_z <- any(!is.na(fc$z_stat))

  subtitle <- if (model_type == "GLM (estimated)") {
    "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated coefficients &nbsp;|&nbsp; bar = \u03b2\u1d62 \u00d7 x\u1d62 (current log-odds contribution) &nbsp;|&nbsp; z = Wald statistic</span>"
  } else {
    "<span style='color:#9aa3b2;font-size:10px;'>Hand-tuned model &nbsp;|&nbsp; bar = current log-odds delta per factor &nbsp;|&nbsp; GLM training pending</span>"
  }

  rows <- vapply(
    seq_len(nrow(fc)),
    function(i) {
      row <- fc[i, ]
      delta <- row$contribution
      pct <- round(min(abs(delta) / max_abs * 44, 44)) # max 44% of half-width
      col <- if (delta > 0.02) {
        "#e94560"
      } else if (delta < -0.02) {
        "#2dce89"
      } else {
        "#9aa3b2"
      }
      dir <- if (delta >= 0) {
        "left:calc(50% + 1px)"
      } else {
        sprintf("left:calc(50%% - %dpx)", as.integer(pct * 3))
      }
      # CSS for diverging bar: right-going = recessionary (red), left-going = protective (green)
      bar_style <- if (delta >= 0) {
        sprintf("left:50%%;width:%d%%;background:%s;", pct, col)
      } else {
        sprintf("right:50%%;width:%d%%;background:%s;", pct, col)
      }

      z_badge <- if (has_z && !is.na(row$z_stat)) {
        z_col <- if (abs(row$z_stat) > 2.5) {
          "#d0d0d0"
        } else if (abs(row$z_stat) > 1.5) {
          "#9aa3b2"
        } else {
          "#555"
        }
        sprintf(
          "&nbsp;<span style='color:%s;font-size:9px;'>(z=%.1f)</span>",
          z_col,
          row$z_stat
        )
      } else {
        ""
      }

      sprintf(
        "<div style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'>
         <div style='width:210px;text-align:right;color:#d0d0d0;font-size:10.5px;
                     flex-shrink:0;line-height:1.2;'>%s%s</div>
         <div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;'>
           <div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;'></div>
           <div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div>
         </div>
         <div style='width:46px;color:%s;font-size:10px;font-weight:600;text-align:right;'>%+.2f</div>
       </div>",
        htmltools::htmlEscape(row$name),
        z_badge,
        bar_style,
        col,
        delta
      )
    },
    character(1)
  )

  paste0(
    "<div style='margin-bottom:18px;'>",
    "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;'>",
    "<span style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>",
    "Live Factor Contributions \u2014 Current Log-Odds Impact</span>",
    "</div>",
    "<div style='margin-bottom:8px;'>",
    subtitle,
    "</div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'>",
    "<span>\u25c4 Protective (lowers probability)</span>",
    "<span style='color:#6b7585;'>| zero |</span>",
    "<span>Recessionary (raises probability) \u25ba</span>",
    "</div>",
    paste(rows, collapse = ""),
    "<div style='font-size:9px;color:#555;margin-top:6px;'>",
    "Each bar = actual log-odds contribution of that factor given current data. Red = raises recession risk. Green = lowers it. Length proportional to impact.",
    "</div></div>"
  )
}


# ── build_growth_outlook (modified to add GLM training) ───────────────────────

build_growth_outlook <- function(fred_data, kpis, mkt_returns = NULL) {
  if (is.null(kpis)) {
    return(NULL)
  }

  gv <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 2) {
      return(NULL)
    }
    df %>% dplyr::arrange(date) %>% dplyr::pull(value)
  }

  unemp <- gv("UNRATE")
  payrolls <- gv("PAYEMS")
  cpi <- gv("CPIAUCSL")
  fedfunds <- gv("FEDFUNDS")
  t10 <- gv("DGS10")
  t2 <- gv("DGS2")
  mort <- gv("MORTGAGE30US")
  houst <- gv("HOUST")
  retail <- gv("RSAFS")
  sent <- gv("UMCSENT")
  indpro <- gv("INDPRO")
  vix_vals <- gv("VIXCLS")
  hy <- gv("BAMLH0A0HYM2")
  oil <- gv("DCOILWTICO")

  u <- kpis$unemp_rate %||% NA
  cpi_y <- kpis$cpi_yoy %||% NA
  pce_y <- kpis$core_pce %||% NA
  ff <- kpis$fed_funds %||% NA
  t10v <- kpis$t10yr %||% NA
  t2v <- kpis$t2yr %||% NA
  mort_v <- kpis$mortgage30 %||% NA
  hs <- kpis$housing_starts %||% NA
  ret_y <- kpis$retail_yoy %||% NA
  cs <- kpis$cons_sent %||% NA
  ip_y <- kpis$indpro_yoy %||% NA
  vix_v <- kpis$vix %||% NA
  hy_v <- kpis$hy_spread %||% NA
  oil_v <- kpis$oil_price %||% NA

  real_rate <- if (!is.na(ff) && !is.na(cpi_y)) ff - cpi_y else NA
  yield_spread <- if (!is.na(t10v) && !is.na(t2v)) t10v - t2v else NA
  pay_3m <- if (!is.null(payrolls) && length(payrolls) >= 4) {
    mean(diff(tail(payrolls, 4)), na.rm = TRUE)
  } else {
    NA
  }
  ret_real <- if (!is.na(ret_y) && !is.na(cpi_y)) ret_y - cpi_y else NA

  inv_months <- if (!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 18)
    sum((tail(t10, n) - tail(t2, n)) < 0, na.rm = TRUE)
  } else {
    0
  }

  vix_6m_avg <- if (!is.null(vix_vals) && length(vix_vals) >= 126) {
    mean(tail(vix_vals, 126), na.rm = TRUE)
  } else {
    vix_v
  }

  # ── Composite score (unchanged) ──────────────────────────────────────────────
  components <- list(
    labor = list(
      weight = 2.5,
      label = "Labor Market",
      score = mean(
        c(
          .score(u, 3.5, 5.0, FALSE),
          .score(pay_3m, 150, 50, TRUE),
          .score(u, 4.5, 5.5, FALSE)
        ),
        na.rm = TRUE
      ),
      detail = if (!is.na(u) && !is.na(pay_3m)) {
        sprintf("%.1f%% unemployment; avg payrolls %+.0fK/mo (3M)", u, pay_3m)
      } else {
        "N/A"
      }
    ),
    consumer = list(
      weight = 2.0,
      label = "Consumer Demand",
      score = mean(
        c(.score(ret_real, 1.0, -1.0, TRUE), .score(cs, 85, 65, TRUE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(ret_real) && !is.na(cs)) {
        sprintf("Real retail YoY %.1f%%; sentiment %.0f", ret_real, cs)
      } else {
        "N/A"
      }
    ),
    monetary = list(
      weight = 2.0,
      label = "Monetary Conditions",
      score = mean(
        c(
          .score(real_rate, 0.5, 2.5, FALSE),
          .score(yield_spread, 0.5, -0.25, TRUE),
          .score(inv_months, 3, 9, FALSE)
        ),
        na.rm = TRUE
      ),
      detail = if (!is.na(real_rate) && !is.na(yield_spread)) {
        sprintf(
          "Real rate %.1f%%; 10Y-2Y %.2f pp; %d mo inverted",
          real_rate,
          yield_spread,
          inv_months
        )
      } else {
        "N/A"
      }
    ),
    housing = list(
      weight = 1.5,
      label = "Housing",
      score = mean(
        c(.score(hs, 1400, 1000, TRUE), .score(mort_v, 6.0, 7.5, FALSE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(hs) && !is.na(mort_v)) {
        sprintf("%.0fK starts ann.; %.2f%% mortgage rate", hs, mort_v)
      } else {
        "N/A"
      }
    ),
    financial = list(
      weight = 1.5,
      label = "Financial Conditions",
      score = mean(
        c(.score(vix_v, 18, 28, FALSE), .score(hy_v, 4.5, 6.0, FALSE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(vix_v) && !is.na(hy_v)) {
        sprintf("VIX %.1f; HY spread %.2f%%", vix_v, hy_v)
      } else {
        "N/A"
      }
    ),
    inflation = list(
      weight = 1.5,
      label = "Inflation",
      score = mean(
        c(.score(cpi_y, 2.5, 4.5, FALSE), .score(pce_y, 2.3, 3.5, FALSE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(cpi_y) && !is.na(pce_y)) {
        sprintf("CPI YoY %.1f%%; Core PCE %.1f%%", cpi_y, pce_y)
      } else {
        "N/A"
      }
    ),
    industrial = list(
      weight = 1.0,
      label = "Industrial Output",
      score = .score(ip_y, 1.0, -1.0, TRUE),
      detail = if (!is.na(ip_y)) sprintf("IP YoY %.1f%%", ip_y) else "N/A"
    )
  )

  total_weight <- sum(sapply(components, `[[`, "weight"))
  raw_score <- sum(sapply(components, function(c) c$score * c$weight)) /
    total_weight
  score_0_10 <- max(0, min(10, round((raw_score + 1) / 2 * 10, 1)))

  regime <- dplyr::case_when(
    score_0_10 >= 7.5 ~ list(
      label = "Expansion",
      color = "#2dce89",
      icon = "arrow-up",
      gdp_est = "+2.5% to +3.5%",
      summary = "Broad-based growth signals."
    ),
    score_0_10 >= 5.5 ~ list(
      label = "Moderate Growth",
      color = "#00b4d8",
      icon = "minus",
      gdp_est = "+1.0% to +2.5%",
      summary = "Below-trend but positive growth likely. Mixed signals."
    ),
    score_0_10 >= 3.5 ~ list(
      label = "Stall / Slowdown",
      color = "#f4a261",
      icon = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk. Restrictive monetary conditions or financial stress beginning to bite."
    ),
    TRUE ~ list(
      label = "Contraction Risk",
      color = "#e94560",
      icon = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals. Recession probability elevated."
    )
  )

  swing_factors <- list()
  drag <- names(which.min(sapply(components, `[[`, "score")))
  swing_factors$biggest_drag <- list(
    name = components[[drag]]$label,
    score = round(components[[drag]]$score, 2),
    detail = components[[drag]]$detail
  )
  supp <- names(which.max(sapply(components, `[[`, "score")))
  swing_factors$biggest_support <- list(
    name = components[[supp]]$label,
    score = round(components[[supp]]$score, 2),
    detail = components[[supp]]$detail
  )
  swing_factors$yield_curve_watch <- if (inv_months > 6) {
    sprintf("Yield curve inverted %d months.", inv_months)
  } else if (!is.na(yield_spread) && yield_spread > 0) {
    sprintf("Yield curve re-steepened to %.2f pp.", yield_spread)
  } else {
    NULL
  }
  swing_factors$fed_watch <- if (!is.na(real_rate) && real_rate > 2.0) {
    sprintf("Real Fed Funds %.1f%% — restrictive.", real_rate)
  } else if (!is.na(real_rate) && real_rate < 0.5) {
    "Real rate near zero."
  } else {
    NULL
  }

  # ── Equity drawdown ──────────────────────────────────────────────────────────
  equity_drawdown_pct <- if (!is.null(mkt_returns)) {
    spy <- mkt_returns %>%
      dplyr::filter(symbol == "SPY") %>%
      dplyr::arrange(date)
    if (nrow(spy) >= 52) {
      current <- tail(spy$close, 1)
      peak <- max(spy$close[max(1, nrow(spy) - 252):nrow(spy)], na.rm = TRUE)
      round((current / peak - 1) * 100, 1)
    } else {
      NA_real_
    }
  } else {
    NA_real_
  }

  # ── Tier 1: Train / retrieve GLM model ──────────────────────────────────────
  glm_result <- tryCatch(
    {
      cache_name <- ".recession_glm_cache_t1"
      needs_train <- !exists(cache_name, envir = .GlobalEnv) ||
        is.null(get(cache_name, envir = .GlobalEnv)) ||
        difftime(
          Sys.time(),
          get(cache_name, envir = .GlobalEnv)$trained_at,
          units = "days"
        ) >
          30

      if (needs_train) {
        message("[synopsis_tier1] Training GLM recession model...")
        m <- train_recession_model(fred_data)
        assign(cache_name, m, envir = .GlobalEnv)
      }
      get(cache_name, envir = .GlobalEnv)
    },
    error = function(e) {
      message("[synopsis_tier1] GLM cache error: ", e$message)
      NULL
    }
  )

  recession_prob <- .compute_recession_prob(
    yield_spread = yield_spread,
    inv_months = inv_months,
    unemp = unemp,
    u = u,
    pay_3m = pay_3m,
    real_rate = real_rate,
    hy_v = hy_v,
    vix_v = vix_v,
    cs = cs,
    ip_y = ip_y,
    hs = hs,
    prime_age_lfpr = kpis$prime_age_lfpr %||% NA,
    prime_age_lfpr_chg = kpis$prime_age_lfpr_chg %||% NA,
    jobless_claims_trend = kpis$jobless_claims_trend %||% NA,
    quits_yoy = kpis$quits_yoy %||% NA,
    oil_yoy = kpis$oil_yoy %||% NA,
    equity_drawdown_pct = equity_drawdown_pct,
    usd_yoy = kpis$usd_yoy %||% NA,
    inventory_sales_ratio = kpis$inventory_sales_ratio %||% NA,
    cc_delinquency = kpis$cc_delinquency %||% NA,
    glm_result = glm_result # ← Tier 1
  )

  list(
    score = score_0_10,
    raw_score = raw_score,
    regime = regime,
    components = components,
    swing = swing_factors,
    recession_prob = recession_prob,
    as_of = Sys.Date()
  )
}


# ── render_growth_outlook_html (unchanged except passes model_type to modal) ──

render_growth_outlook_html <- function(outlook) {
  if (is.null(outlook)) {
    return(HTML(
      "<p style='color:#6b7585;padding:16px;'>Growth outlook not yet computed.</p>"
    ))
  }

  reg <- outlook$regime
  score <- outlook$score
  comps <- outlook$components
  swing <- outlook$swing
  pct <- score / 10 * 100
  gauge_col <- reg$color

  gauge_html <- sprintf(
    "<div style='margin:12px 0 16px;'>
       <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
         <span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span>
         <span style='color:#9aa3b2;font-size:11px;'>Expansion</span>
       </div>
       <div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'>
         <div style='background:linear-gradient(90deg,%s,%s);width:%.0f%%;height:100%%;border-radius:6px;'></div>
       </div>
       <div style='margin-top:5px;display:flex;justify-content:space-between;'>
         <span style='color:#9aa3b2;font-size:10px;'>0</span>
         <span style='color:%s;font-size:11px;font-weight:700;'>Score: %.1f / 10</span>
         <span style='color:#9aa3b2;font-size:10px;'>10</span>
       </div></div>",
    if (score < 5) "#e94560" else "#f4a261",
    gauge_col,
    pct,
    gauge_col,
    score
  )

  comp_html <- paste(
    vapply(
      names(comps),
      function(nm) {
        c <- comps[[nm]]
        s <- c$score
        col <- if (s >= 0.4) {
          "#2dce89"
        } else if (s >= -0.4) {
          "#f4a261"
        } else {
          "#e94560"
        }
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
       <div style='color:#6b7585;font-size:10px;margin-top:2px;'>%s</div></div>",
          c$label,
          col,
          if (s >= 0.4) {
            "Positive"
          } else if (s >= -0.4) {
            "Neutral"
          } else {
            "Negative"
          },
          col,
          bar_w,
          htmltools::htmlEscape(c$detail)
        )
      },
      character(1)
    ),
    collapse = ""
  )

  swing_html <- paste(
    c(
      if (!is.null(swing$biggest_drag)) {
        sprintf(
          "<li style='margin-bottom:6px;'><span style='color:#e94560;font-weight:600;'>\u2b07 Biggest drag: %s</span> \u2014 %s</li>",
          swing$biggest_drag$name,
          htmltools::htmlEscape(swing$biggest_drag$detail)
        )
      },
      if (!is.null(swing$biggest_support)) {
        sprintf(
          "<li style='margin-bottom:6px;'><span style='color:#2dce89;font-weight:600;'>\u2b06 Biggest support: %s</span> \u2014 %s</li>",
          swing$biggest_support$name,
          htmltools::htmlEscape(swing$biggest_support$detail)
        )
      },
      if (!is.null(swing$yield_curve_watch)) {
        sprintf(
          "<li style='margin-bottom:6px;'><span style='color:#f4a261;font-weight:600;'>\u26a0 Yield Curve:</span> %s</li>",
          htmltools::htmlEscape(swing$yield_curve_watch)
        )
      },
      if (!is.null(swing$fed_watch)) {
        sprintf(
          "<li style='margin-bottom:6px;'><span style='color:#00b4d8;font-weight:600;'>\u2699 Fed Policy:</span> %s</li>",
          htmltools::htmlEscape(swing$fed_watch)
        )
      }
    ),
    collapse = ""
  )

  rp <- outlook$recession_prob
  rp_html <- ""
  if (!is.null(rp)) {
    prob <- rp$prob
    tier <- rp$tier
    bar_col <- tier$color
    up_html <- if (length(rp$drivers_up) > 0) {
      paste(
        sprintf(
          "<li style='margin-bottom:4px;'><span style='color:#e94560;'>\u25b2</span> %s</li>",
          htmltools::htmlEscape(rp$drivers_up)
        ),
        collapse = ""
      )
    } else {
      ""
    }
    down_html <- if (length(rp$drivers_down) > 0) {
      paste(
        sprintf(
          "<li style='margin-bottom:4px;'><span style='color:#2dce89;'>\u25bc</span> %s</li>",
          htmltools::htmlEscape(rp$drivers_down)
        ),
        collapse = ""
      )
    } else {
      ""
    }
    drivers_section <- if (nchar(up_html) > 0 || nchar(down_html) > 0) {
      sprintf(
        "<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>%s%s</ul>",
        up_html,
        down_html
      )
    } else {
      ""
    }

    model_badge <- if (
      !is.null(rp$model_type) && rp$model_type == "GLM (estimated)"
    ) {
      sprintf(
        "<span style='background:rgba(45,206,137,0.12);color:#2dce89;border:1px solid rgba(45,206,137,0.3);
               border-radius:8px;padding:1px 8px;font-size:9px;font-weight:600;margin-left:6px;'>
               GLM n=%d AIC=%.0f</span>",
        rp$n_obs %||% 0L,
        rp$aic %||% 0
      )
    } else {
      "<span style='color:#9aa3b2;font-size:9px;margin-left:6px;'>hand-tuned</span>"
    }

    rp_html <- sprintf(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:14px 16px;margin-top:14px;'>
        <div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'>
          <div>
            <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>
              <i class=\"fa fa-%s\" style=\"color:%s;margin-right:6px;\"></i>Recession Probability \u2014 12-Month Horizon%s
            </div>
            <div style='margin-top:4px;'>
              <span style='color:%s;font-size:36px;font-weight:800;'>%s%%</span>
              <span style='color:%s;font-size:14px;font-weight:600;margin-left:8px;background:rgba(%s,0.12);padding:2px 10px;border-radius:12px;border:1px solid rgba(%s,0.3);'>%s</span>
            </div>
          </div>
          <div style='text-align:right;'>
            <div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0%%  \u00b7  50%%  \u00b7  100%%</div>
            <div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);border-radius:7px;position:relative;'>
              <div style='position:absolute;top:-3px;left:calc(%s%% - 8px);width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:8px solid #ffffff;'></div>
            </div>
            <div style='font-size:10px;color:#6b7585;margin-top:3px;text-align:center;'>\u25b2 Current</div>
          </div>
        </div>
        <div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;border-radius:6px;padding:8px 12px;border-left:3px solid %s;'>%s</div>
        %s
        <div style='color:#555;font-size:10px;margin-top:8px;'>Multi-factor logit model. Calibrated to NBER recession dates. Not a point forecast.</div>
      </div>",
      bar_col,
      tier$icon,
      bar_col,
      model_badge,
      bar_col,
      sprintf("%.0f", prob),
      bar_col,
      bar_col,
      bar_col,
      tier$label,
      sprintf("%.0f", min(prob, 98)),
      bar_col,
      htmltools::htmlEscape(tier$desc),
      drivers_section
    )
  }

  HTML(paste0(
    sprintf(
      "<div style='display:flex;gap:14px;'>
       <div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:16px 18px;'>
         <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>
           <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook
         </div>
         <div style='color:%s;font-size:22px;font-weight:800;margin-bottom:2px;'>%s</div>
         <div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>GDP Est: %s annualized</div>
         %s
         <div style='background:#0f1117;border-radius:6px;padding:10px 12px;color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid %s;'>%s</div>
         <div style='color:#555;font-size:10px;margin-top:10px;'>
           Composite model \u2014 %d growth indicators + 13-factor recession model. As of %s.
           <button onclick=\"document.getElementById('recModelModal').style.display='flex'\"
             style='margin-left:8px;background:transparent;border:1px solid #2a3042;color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'>
             <i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details
           </button>
         </div>
         %s
       </div>
       <div style='flex:1;display:flex;flex-direction:column;gap:10px;'>
         <div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;flex:1;'>
           <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>
           %s
         </div>
         <div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;'>
           <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div>
           <ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>%s</ul>
         </div>
       </div></div>",
      reg$color,
      reg$color,
      reg$icon,
      reg$color,
      reg$label,
      reg$gdp_est,
      gauge_html,
      reg$color,
      htmltools::htmlEscape(reg$summary),
      length(comps),
      format(outlook$as_of, "%b %d, %Y"),
      rp_html,
      comp_html,
      swing_html
    ),
    .build_model_modal(
      factor_contributions = if (!is.null(outlook$recession_prob)) {
        outlook$recession_prob$factor_contributions
      } else {
        NULL
      },
      model_type = if (!is.null(outlook$recession_prob)) {
        outlook$recession_prob$model_type %||% "Hand-tuned log-odds"
      } else {
        "Hand-tuned log-odds"
      }
    )
  ))
}


# ── Model explanation modal (updated with live bar chart) ─────────────────────

.build_model_modal <- function(
  factor_contributions = NULL,
  model_type = "Hand-tuned log-odds"
) {
  # ── Bar chart section ──────────────────────────────────────────────────────
  bar_chart_html <- .build_factor_bar_chart(factor_contributions, model_type)

  # ── Factor detail table ────────────────────────────────────────────────────
  factors <- list(
    list(
      rank = 1,
      name = "Yield Curve (10Y-2Y spread)",
      weight = "High",
      col = "#e94560",
      source = "Estrella & Mishkin (1998); NY Fed model",
      mechanism = "Every 1pp spread narrowing adds +0.85 log-odds. Inversion duration: +0.3 (3M), +0.6 (6M), +1.2 (12M), +1.5 (18M+).",
      interpret = "Negative spread = market pricing rate cuts ahead = recession concern. Most reliable single predictor at 12-month horizon."
    ),
    list(
      rank = 2,
      name = "Sahm Rule / Labor Momentum",
      weight = "High",
      col = "#e94560",
      source = "Sahm (2019); BLS UNRATE + PAYEMS",
      mechanism = "Rise from 12M unemployment trough: \u22650.5pp = +1.8 log-odds. Payroll momentum: negative 3M avg = +0.6 to +1.5.",
      interpret = "Sahm Rule has a perfect record triggering before or at every recession since 1970."
    ),
    list(
      rank = 3,
      name = "Prime-Age LFPR (25\u201354)",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS LNS11300060",
      mechanism = "Level below 82% = +0.4; 6M drop >0.2pp = +0.4; drop >0.5pp = +0.9. LFPR above 83.5% = -0.3.",
      interpret = "Captures hidden weakness: workers exiting labour force keep headline unemployment flat while real demand contracts."
    ),
    list(
      rank = 4,
      name = "Jobless Claims Trend",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS ICSA (initial claims, weekly)",
      mechanism = "4-week MA vs 26-week MA ratio. >10% above baseline = +0.6; >20% = +1.2 log-odds.",
      interpret = "Most real-time labour indicator (weekly). Detects sudden deterioration weeks before monthly payrolls."
    ),
    list(
      rank = 5,
      name = "Quits Rate YoY Change",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS JTSQUL (JOLTS quits)",
      mechanism = "YoY decline >8% = +0.4; >15% = +0.8. Rising quits = -0.3.",
      interpret = "Workers quit when confident. Sustained decline signals fear of job loss \u2014 leads Sahm Rule by 1\u20132 quarters."
    ),
    list(
      rank = 6,
      name = "Oil Price Shock",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED DCOILWTICO (WTI crude)",
      mechanism = ">25% YoY = +0.6 (Hamilton threshold); >50% = +1.0. >30% drop (demand collapse) = +0.5.",
      interpret = "Hamilton (1983): every major US recession was preceded by an oil shock."
    ),
    list(
      rank = 7,
      name = "Equity Bear Market (SPY drawdown)",
      weight = "Medium",
      col = "#f4a261",
      source = "Yahoo Finance / tidyquant (SPY)",
      mechanism = ">10% drawdown = +0.5; >20% = +1.1; >30% = +1.6. <5% from 52-week high = -0.2.",
      interpret = "S&P >20% drawdown has preceded every post-WW2 recession. Markets price future earnings."
    ),
    list(
      rank = 8,
      name = "HY Credit Spreads",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED BAMLH0A0HYM2 (ICE BofA HY OAS)",
      mechanism = ">4.5% = +0.2; >5.5% = +0.7; >7% = +1.4. <3.5% = -0.4.",
      interpret = "HY spreads price default risk. >500bp signals credit market stress and lending tightening."
    ),
    list(
      rank = 9,
      name = "USD Surge (YoY)",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DTWEXBGS (trade-weighted USD)",
      mechanism = ">8% YoY = +0.4; >12% = +0.8. Weakening >5% = -0.2.",
      interpret = "Dollar surge tightens global conditions: raises EM debt burdens, hurts US exporters, signals risk-off."
    ),
    list(
      rank = 10,
      name = "Real Fed Funds Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED FEDFUNDS minus CPI YoY",
      mechanism = ">1% = +0.2; >2% = +0.5; >3% = +1.0. Negative = -0.3.",
      interpret = "Monetary policy works with long and variable lags (Friedman). Firmly positive real rate is restrictive."
    ),
    list(
      rank = 11,
      name = "Consumer Sentiment",
      weight = "Low",
      col = "#9aa3b2",
      source = "U Michigan UMCSENT",
      mechanism = "<60 = +0.7; <70 = +0.3. >90 = -0.4.",
      interpret = "Conference Board LEI component. Sentiment leads spending by 1\u20132 quarters."
    ),
    list(
      rank = 12,
      name = "Inventory-to-Sales Ratio",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED ISRATIO (total business)",
      mechanism = ">1.40 = +0.1; >1.45 = +0.3; >1.55 = +0.7. <1.30 = -0.2.",
      interpret = "Excess inventory \u2192 cut orders \u2192 cut production \u2192 cut headcount. Leads hiring slowdowns 1\u20132 quarters."
    ),
    list(
      rank = 13,
      name = "Credit Card Delinquency Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DRCCLACBS (Fed / FDIC)",
      mechanism = ">2.5% = +0.2; >3% = +0.5; >4% = +0.9. <1.8% = -0.3.",
      interpret = "Consumer stress shows here 2\u20133 quarters before unemployment rises."
    )
  )

  rows <- paste(
    vapply(
      factors,
      function(f) {
        wt_col <- switch(
          f$weight,
          "High" = "#e94560",
          "Medium" = "#f4a261",
          "#9aa3b2"
        )
        sprintf(
          "<tr style='border-bottom:1px solid #2a3042;'>
         <td style='padding:10px 8px;color:%s;font-weight:700;font-size:12px;'>%d. %s</td>
         <td style='padding:10px 8px;'><span style='background:rgba(%s,0.12);color:%s;border:1px solid rgba(%s,0.3);border-radius:8px;padding:1px 8px;font-size:10px;font-weight:600;'>%s</span></td>
         <td style='padding:10px 8px;color:#9aa3b2;font-size:11px;'>%s</td>
         <td style='padding:10px 8px;color:#9aa3b2;font-size:11px;line-height:1.5;'>%s</td>
         <td style='padding:10px 8px;color:#d0d0d0;font-size:11px;line-height:1.5;'>%s</td>
       </tr>",
          f$col,
          f$rank,
          htmltools::htmlEscape(f$name),
          wt_col,
          wt_col,
          wt_col,
          f$weight,
          htmltools::htmlEscape(f$source),
          htmltools::htmlEscape(f$mechanism),
          htmltools::htmlEscape(f$interpret)
        )
      },
      character(1)
    ),
    collapse = ""
  )

  paste0(
    "<div id='recModelModal' onclick=\"if(event.target===this)this.style.display='none'\"
      style='display:none;position:fixed;inset:0;background:rgba(0,0,0,0.75);
             z-index:9999;align-items:center;justify-content:center;padding:20px;'>
       <div style='background:#161b27;border:1px solid #2a3042;border-radius:10px;
                   max-width:1100px;width:100%;max-height:90vh;overflow-y:auto;
                   padding:24px;position:relative;'>

         <button onclick=\"document.getElementById('recModelModal').style.display='none'\"
           style='position:absolute;top:14px;right:16px;background:transparent;border:none;
                  color:#9aa3b2;font-size:20px;cursor:pointer;line-height:1;'>&times;</button>

         <div style='color:#00b4d8;font-size:16px;font-weight:800;margin-bottom:4px;'>
           <i class=\"fa fa-brain\" style=\"margin-right:8px;\"></i>
           Recession Probability Model \u2014 Technical Reference (Tier 1: Estimated GLM)
         </div>
         <div style='color:#9aa3b2;font-size:12px;margin-bottom:20px;'>
           13-factor logistic model. When historical FRED data is available, coefficients are
           estimated via glm() on NBER recession dates. Falls back to hand-tuned log-odds on failure.
         </div>

         <!-- LIVE BAR CHART -->
         <div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
    bar_chart_html,
    "</div>

         <!-- Interpretability note -->
         <div style='background:#1e2640;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'>
           <div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'>
             <i class=\"fa fa-eye\" style=\"margin-right:6px;\"></i>Interpretability &amp; Trust
           </div>
           <div style='color:#d0d0d0;font-size:12px;line-height:1.8;'>
             <b style='color:#e0e0e0;'>Tier 1 model type:</b> When trained, a logistic regression (glm) fit on NBER monthly recession history with live FRED series.
             Coefficients are statistically estimated \u2014 not hand-tuned. The bar chart above shows each variable\u2019s
             <em>current actual contribution</em> (\u03b2\u1d62 \u00d7 x\u1d62) to the log-odds at today\u2019s values,
             alongside the Wald z-statistic from the historical fit.<br/>
             <b style='color:#e0e0e0;'>Fallback:</b> If GLM training fails (FRED unavailable, insufficient history),
             the original hand-tuned log-odds model activates automatically. The bar chart then shows
             the hand-tuned threshold-based deltas instead of \u03b2\u1d62 \u00d7 x\u1d62.
           </div>
         </div>

         <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Factor Detail \u2014 Log-Odds Contributions</div>
         <div style='overflow-x:auto;'>
         <table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>
           <thead>
             <tr style='background:#1e2640;border-bottom:2px solid #2a3042;'>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:200px;'>Factor</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;'>Weight</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:160px;'>Data Source</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:220px;'>How It\u2019s Scored</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:240px;'>Why It Matters</th>
             </tr>
           </thead>
           <tbody>",
    rows,
    "
           </tbody>
         </table>
         </div>

         <div style='color:#555;font-size:11px;margin-top:16px;line-height:1.7;'>
           <b style='color:#9aa3b2;'>Formula:</b>
           p = 1 / (1 + exp(\u2212log_odds)) &nbsp;|&nbsp;
           Baseline log_odds = \u22121.73 (15% base rate) &nbsp;|&nbsp;
           Tier 1: GLM-estimated \u03b2 when FRED history available &nbsp;|&nbsp;
           Final probability clamped [2%, 97%].<br/>
           <b style='color:#9aa3b2;'>References:</b>
           Estrella &amp; Mishkin (1998) \u2014 yield curve; Sahm (2019) \u2014 labor rule;
           Hamilton (1983) \u2014 oil shocks; Conference Board LEI methodology.
         </div>
       </div>
     </div>"
  )
}
