# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3.R — Tier 3: Markov-Switching + Online Rolling Retrain
#
# SOURCE STRATEGY: sources synopsis_tier2.R, then overrides only Tier-3
# additions.  synopsis_tier2.R must be in the same directory (or source path).
#
# WHAT'S NEW vs Tier 2:
#
#  1. train_markov_model()       — Fits a 2-state Hamilton (1989) Markov-
#     switching model on GDP growth using MSwM::msmFit().  State 0 = expansion,
#     State 1 = recession.  Returns smoothed regime probabilities for the full
#     series and regime-conditional mean / variance for each predictor.
#     Falls back gracefully to Tier 2 path if MSwM is unavailable or fitting
#     fails.
#
#  2. rolling_retrain()          — Wrapper that retrains the GLM model on a
#     rolling 30-year window of the most recent data so the model adapts as new
#     observations arrive, rather than being fixed at one training snapshot.
#     Called by build_growth_outlook() on a 30-day cycle.
#
#  3. .compute_recession_prob()  — Tier 3 path: blends three probability
#     estimates:
#         p = w_glm × p_GLM  +  w_ms × p_MS  +  anomaly_blend
#     where p_MS = current smoothed Markov recession state probability,
#     w_glm = 0.55, w_ms = 0.45 (down-weighted when MS model unavailable).
#     Per-factor contributions now include a "Markov Regime State" row showing
#     the current probability from the regime model alone.
#
#  4. .build_factor_bar_chart()  — Tier 3 version:
#     • Adds a fourth "regime" colour (teal #00b4d8) for the Markov state row.
#     • When the Markov model is available, shows a secondary "Regime-conditional
#       means" mini-table below the chart showing each variable's mean in
#       State 0 (expansion) vs State 1 (recession) from the fitted model.
#       This lets the user see which variables most strongly differentiate the
#       two regimes historically.
#
#  5. build_growth_outlook()     — Passes markov_result to prob engine;
#     rolling retraining logic uses .recession_t3_cache.
#
#  6. .build_model_modal()       — Tier 3 version:
#     • Title: "Tier 3: Markov-Switching + Online Retraining"
#     • New "Regime Model Status" callout showing current Markov state
#       probability, regime-conditional means table, retrain schedule.
#     • Bar chart shows four-colour legend (red/green/purple/teal).
#     • Footer formula includes three-way blend equation.
#     • Factor table row 15 added: "Markov Regime State" as a blend factor.
# ─────────────────────────────────────────────────────────────────────────────

# Inherit Tier 1 + Tier 2
source(
  file.path(dirname(sys.frame(1)$ofile %||% "."), "R/synopsis_tier2.R"),
  local = FALSE
)

if (!exists("build_anomaly_detector")) {
  source("R/synopsis_tier2.R", local = FALSE)
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 NEW: Markov-switching model
# ═══════════════════════════════════════════════════════════════════════════════

#' Fit a 2-state Markov-switching model (Hamilton 1989) using MSwM.
#'
#' Uses GDP / industrial production YoY growth as the switching variable.
#' State 1 historically corresponds to recession (lower mean, higher variance).
#'
#' Returns list or NULL on failure.
#'
#' @param fred_data   Named list of FRED data frames (from data_fred.R).
#' @param n_states    Integer, default 2.  Change to 3 for a "stall" middle regime.
train_markov_model <- function(fred_data, n_states = 2L) {
  if (!requireNamespace("MSwM", quietly = TRUE)) {
    message(
      "[synopsis_tier3] MSwM not available. install.packages('MSwM') for Markov model."
    )
    return(NULL)
  }

  tryCatch(
    {
      # ── Build monthly panel ────────────────────────────────────────────────────
      gm <- function(sid, val_name) {
        df <- fred_data[[sid]]
        if (is.null(df) || nrow(df) < 24) {
          return(NULL)
        }
        df %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(
            !!val_name := mean(value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::rename(date = month)
      }

      # Industrial production YoY as proxy for GDP growth (monthly, long history)
      indpro <- gm("INDPRO", "indpro")
      if (is.null(indpro) || nrow(indpro) < 120) {
        message("[synopsis_tier3] Insufficient INDPRO data for Markov model.")
        return(NULL)
      }

      indpro <- indpro %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(ip_yoy = (indpro / dplyr::lag(indpro, 12) - 1) * 100) %>%
        dplyr::filter(!is.na(ip_yoy))

      # Optional predictors for the switching equation (regime indicators)
      spread_df <- gm("T10Y2Y", "yield_spread") %||%
        {
          t10 <- gm("DGS10", "t10")
          t2 <- gm("DGS2", "t2")
          if (!is.null(t10) && !is.null(t2)) {
            dplyr::inner_join(t10, t2, by = "date") %>%
              dplyr::mutate(yield_spread = t10 - t2) %>%
              dplyr::select(date, yield_spread)
          } else {
            NULL
          }
        }
      hy_df <- gm("BAMLH0A0HYM2", "hy_spread")
      unem_df <- gm("UNRATE", "unemp")

      panel <- indpro %>% dplyr::select(date, ip_yoy)
      for (df in list(spread_df, hy_df, unem_df)) {
        if (!is.null(df)) panel <- dplyr::left_join(panel, df, by = "date")
      }
      panel <- panel %>% tidyr::drop_na()
      if (nrow(panel) < 120) {
        return(NULL)
      }

      # ── Fit base linear model for msmFit ──────────────────────────────────────
      # MSwM switches coefficients + variance; we switch on the intercept + slope
      avail_preds <- intersect(
        c("yield_spread", "hy_spread", "unemp"),
        names(panel)
      )
      base_fml <- if (length(avail_preds) > 0) {
        as.formula(paste("ip_yoy ~", paste(avail_preds, collapse = " + ")))
      } else {
        as.formula("ip_yoy ~ 1")
      }

      base_lm <- lm(base_fml, data = panel)

      # sw = vector of TRUE/FALSE for which parameters are allowed to switch state
      n_coef <- length(coef(base_lm))
      sw_vec <- rep(TRUE, n_coef) # all coefficients + variance switch

      ms_model <- MSwM::msmFit(
        base_lm,
        k = n_states,
        sw = sw_vec,
        control = list(parallel = FALSE, maxiter = 500)
      )

      # ── Extract regime probabilities ─────────────────────────────────────────
      # Smoothed state probabilities: rows = obs, cols = states
      smooth_probs <- ms_model@Fit@smoProb # matrix [n_obs × k]
      if (is.null(smooth_probs) || nrow(smooth_probs) == 0) {
        return(NULL)
      }

      # Identify which state = recession (lower mean ip_yoy)
      state_means <- sapply(seq_len(n_states), function(s) ms_model@Coef[1, s])
      recession_state <- which.min(state_means) # typically state 1

      # Current (most recent) smoothed recession probability
      current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]

      # Regime-conditional means for each predictor (for bar chart annotation)
      regime_means <- if (length(avail_preds) > 0) {
        lapply(avail_preds, function(v) {
          # Simple: group by argmax(smoothed state) and compute mean
          state_assign <- apply(smooth_probs, 1, which.max)
          panel_trimmed <- panel[seq_len(nrow(smooth_probs)), ]
          list(
            variable = v,
            expansion = round(
              mean(
                panel_trimmed[[v]][state_assign != recession_state],
                na.rm = TRUE
              ),
              2
            ),
            recession = round(
              mean(
                panel_trimmed[[v]][state_assign == recession_state],
                na.rm = TRUE
              ),
              2
            )
          )
        })
      } else {
        NULL
      }

      message(sprintf(
        "[synopsis_tier3] Markov model fitted: n=%d, recession state=%d, current p_rec=%.1f%%",
        nrow(panel),
        recession_state,
        current_rec_prob * 100
      ))

      list(
        model = ms_model,
        smooth_probs = smooth_probs,
        recession_state = recession_state,
        current_rec_prob = current_rec_prob,
        state_means_ip = state_means,
        regime_means = regime_means,
        n_states = n_states,
        n_obs = nrow(panel),
        trained_at = Sys.time()
      )
    },
    error = function(e) {
      message("[synopsis_tier3] Markov model failed: ", e$message)
      NULL
    }
  )
}


#' Rolling retrain: refit the GLM on the most recent `window` months of data.
#'
#' Called by build_growth_outlook() to update the model as new data arrives.
#' @param fred_data  Current fred_data list.
#' @param window     Integer. Rolling window in months, default 360 (30 years).
rolling_retrain <- function(fred_data, window = 360L) {
  message(sprintf(
    "[synopsis_tier3] Rolling retrain (window=%d months)...",
    window
  ))
  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    return(NULL)
  }

  # Keep only the most recent `window` observations
  panel_rolling <- tail(panel, window)
  if (nrow(panel_rolling) < 100) {
    panel_rolling <- panel
  } # fall back to full panel

  possible <- c(
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
  available <- intersect(possible, names(panel_rolling))
  if (length(available) < 3) {
    return(NULL)
  }

  panel_clean <- panel_rolling %>%
    dplyr::select(rec_next12, dplyr::all_of(available)) %>%
    tidyr::drop_na()
  if (nrow(panel_clean) < 60) {
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

      feature_only <- panel_clean[, available, drop = FALSE]
      ad <- build_anomaly_detector(feature_only)

      message(sprintf(
        "[synopsis_tier3] Rolling GLM: n=%d (rolling %dM), AIC=%.1f",
        nrow(panel_clean),
        window,
        AIC(model)
      ))

      list(
        model = model,
        coef_table = coef_tbl,
        n_obs = nrow(panel_clean),
        aic = round(AIC(model), 1),
        predictors = available,
        trained_at = Sys.time(),
        anomaly_detector = ad,
        window_months = window
      )
    },
    error = function(e) {
      message("[synopsis_tier3] rolling_retrain glm failed: ", e$message)
      NULL
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: .compute_recession_prob — 3-way blend
# ═══════════════════════════════════════════════════════════════════════════════

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
  glm_result = NULL,
  anomaly_detector = NULL,
  markov_result = NULL # ← Tier 3 addition
) {
  # ── Weights ──────────────────────────────────────────────────────────────────
  W_GLM <- if (!is.null(markov_result)) 0.55 else 1.00
  W_MS <- if (!is.null(markov_result)) 0.45 else 0.00

  # Anomaly blend parameters (inherited from Tier 2)
  ANOMALY_WEIGHT <- 0.35
  ANOMALY_FLOOR <- 0.25
  COMPRESS_CENTER <- 0.50

  contribs <- list()
  add_contrib <- function(id, label, delta, z_stat = NA_real_) {
    contribs[[id]] <<- list(name = label, contribution = delta, z_stat = z_stat)
  }

  u_rise_val <- if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
    u - min(tail(unemp, 12), na.rm = TRUE)
  } else {
    NA_real_
  }

  feat_df <- build_current_feature_vector(
    yield_spread,
    u_rise_val,
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

  # ── Anomaly score (Tier 2 function) ──────────────────────────────────────────
  anomaly_result <- compute_anomaly_score(anomaly_detector, feat_df)
  A <- anomaly_result$score

  # ── GLM probability + per-factor contributions ───────────────────────────────
  glm_prob_raw <- NA_real_

  if (!is.null(glm_result) && !is.null(glm_result$model)) {
    keep_cols <- intersect(glm_result$predictors, names(feat_df))
    feat_sub <- feat_df[, keep_cols, drop = FALSE]
    for (col in names(feat_sub)) {
      if (is.na(feat_sub[[col]])) feat_sub[[col]] <- 0
    }

    glm_prob_raw <- tryCatch(
      predict(glm_result$model, newdata = feat_sub, type = "response")[[1]],
      error = function(e) NA_real_
    )

    if (!is.na(glm_prob_raw)) {
      coefs <- coef(glm_result$model)
      ct <- glm_result$coef_table
      fv <- as.list(feat_sub[1, , drop = FALSE])
      plabels <- c(
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
        x <- fv[[pred]]
        z <- if (pred %in% ct$predictor) {
          ct$z_stat[ct$predictor == pred]
        } else {
          NA_real_
        }
        lbl <- if (pred %in% names(plabels)) plabels[[pred]] else pred
        add_contrib(pred, lbl, if (!is.na(b) && !is.na(x)) b * x else 0, z)
      }
    }
  }

  # ── Hand-tuned fallback (Tier 1 logic) ───────────────────────────────────────
  if (is.na(glm_prob_raw)) {
    lo <- -1.73
    yc <- 0
    if (!is.na(yield_spread)) {
      yc <- (-yield_spread * 0.85)
      if (!is.na(inv_months)) {
        yc <- yc +
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
    lo <- lo + yc
    add_contrib("yield_curve", "Yield Curve (10Y-2Y spread)", yc)
    sh <- 0
    if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
      ur <- u - min(tail(unemp, 12), na.rm = TRUE)
      sh <- sh +
        if (ur >= 0.5) {
          1.8
        } else if (ur >= 0.3) {
          0.9
        } else if (ur >= 0.1) {
          0.3
        } else if (ur < (-0.2)) {
          -0.4
        } else {
          0
        }
    }
    if (!is.na(pay_3m)) {
      sh <- sh +
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
    lo <- lo + sh
    add_contrib("sahm", "Sahm Rule / Labor Momentum", sh)
    lf <- 0
    if (!is.na(prime_age_lfpr)) {
      lf <- lf +
        if (prime_age_lfpr < 80.5) {
          0.8
        } else if (prime_age_lfpr < 82) {
          0.4
        } else if (prime_age_lfpr > 83.5) {
          -0.3
        } else {
          0
        }
    }
    if (!is.na(prime_age_lfpr_chg)) {
      lf <- lf +
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
    lo <- lo + lf
    add_contrib("prime_lfpr", "Prime-Age LFPR", lf)
    cl <- 0
    if (!is.na(jobless_claims_trend)) {
      cl <- if (jobless_claims_trend > 0.20) {
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
    lo <- lo + cl
    add_contrib("claims", "Jobless Claims Trend", cl)
    qt <- 0
    if (!is.na(quits_yoy)) {
      qt <- if (quits_yoy < (-15)) {
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
    lo <- lo + qt
    add_contrib("quits", "Quits Rate YoY", qt)
    ol <- 0
    if (!is.na(oil_yoy)) {
      ol <- if (oil_yoy > 50) {
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
    lo <- lo + ol
    add_contrib("oil", "Oil Price Shock", ol)
    eq <- 0
    if (!is.na(equity_drawdown_pct)) {
      dd <- abs(equity_drawdown_pct)
      eq <- if (dd > 30) {
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
    lo <- lo + eq
    add_contrib("equity", "Equity Bear Market", eq)
    hy <- 0
    if (!is.na(hy_v)) {
      hy <- if (hy_v > 7) {
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
    lo <- lo + hy
    add_contrib("hy_spreads", "HY Credit Spreads", hy)
    ud <- 0
    if (!is.na(usd_yoy)) {
      ud <- if (usd_yoy > 12) {
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
    lo <- lo + ud
    add_contrib("usd", "USD Surge", ud)
    rr <- 0
    if (!is.na(real_rate)) {
      rr <- if (real_rate > 3) {
        1.0
      } else if (real_rate > 2) {
        0.5
      } else if (real_rate > 1) {
        0.2
      } else if (real_rate < 0) {
        -0.3
      } else {
        0
      }
    }
    lo <- lo + rr
    add_contrib("real_rate", "Real Fed Funds Rate", rr)
    se <- 0
    if (!is.na(cs)) {
      se <- if (cs < 60) {
        0.7
      } else if (cs < 70) {
        0.3
      } else if (cs > 90) {
        -0.4
      } else {
        0
      }
    }
    lo <- lo + se
    add_contrib("sentiment", "Consumer Sentiment", se)
    iv <- 0
    if (!is.na(inventory_sales_ratio)) {
      iv <- if (inventory_sales_ratio > 1.55) {
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
    lo <- lo + iv
    add_contrib("inv_sales", "Inventory-to-Sales Ratio", iv)
    cc <- 0
    if (!is.na(cc_delinquency)) {
      cc <- if (cc_delinquency > 4) {
        0.9
      } else if (cc_delinquency > 3) {
        0.5
      } else if (cc_delinquency > 2.5) {
        0.2
      } else if (cc_delinquency < 1.8) {
        -0.3
      } else {
        0
      }
    }
    lo <- lo + cc
    add_contrib("cc_del", "CC Delinquency Rate", cc)
    glm_prob_raw <- 1 / (1 + exp(-lo))
  }

  # ── Tier 3: Markov state probability ─────────────────────────────────────────
  ms_prob_raw <- NA_real_
  if (!is.null(markov_result) && !is.na(markov_result$current_rec_prob)) {
    ms_prob_raw <- markov_result$current_rec_prob

    # Contribution in percentage-point terms relative to GLM baseline
    ms_delta <- (ms_prob_raw - glm_prob_raw) * 100 * W_MS
    add_contrib(
      "markov_regime",
      sprintf(
        "Markov Regime State (p_rec=%.0f%%; expansion mean IP=%.1f%%, recession=%.1f%%)",
        ms_prob_raw * 100,
        markov_result$state_means_ip[setdiff(
          1:markov_result$n_states,
          markov_result$recession_state
        )[1]] %||%
          NA,
        markov_result$state_means_ip[markov_result$recession_state]
      ),
      ms_delta
    )
  }

  # ── 3-way blend ──────────────────────────────────────────────────────────────
  blended_pre_anomaly <- if (!is.na(ms_prob_raw)) {
    W_GLM * glm_prob_raw + W_MS * ms_prob_raw
  } else {
    glm_prob_raw
  }

  # Tier 2 anomaly blend on top
  blended_raw <- if (A > 0.05) {
    compressed <- blended_pre_anomaly +
      ANOMALY_WEIGHT * A * (COMPRESS_CENTER - blended_pre_anomaly)
    floor_level <- ANOMALY_FLOOR * A
    max(compressed, floor_level)
  } else {
    blended_pre_anomaly
  }

  prob <- max(2, min(97, round(blended_raw * 100, 1)))

  if (A > 0.05) {
    anom_delta <- (blended_raw - blended_pre_anomaly) * 100
    add_contrib(
      "anomaly_signal",
      sprintf(
        "Anomaly Signal (MD\u2248%.1f; %s)",
        anomaly_result$md %||% 0,
        anomaly_result$label
      ),
      anom_delta
    )
  }

  tier <- .classify_recession_tier(prob)

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

  du <- character(0)
  dd2 <- character(0)
  for (i in seq_len(min(8, nrow(contrib_df)))) {
    r <- contrib_df[i, ]
    if (r$contribution > 0.1) {
      du <- c(du, r$name)
    }
    if (r$contribution < -0.1) dd2 <- c(dd2, r$name)
  }
  if (A > 0.3) {
    du <- c(
      sprintf(
        "\u26a0 Anomaly: %s (MD=%.1f)",
        anomaly_result$label,
        anomaly_result$md %||% 0
      ),
      du
    )
  }
  if (!is.na(ms_prob_raw) && ms_prob_raw > 0.4) {
    du <- c(
      sprintf(
        "Markov regime: %.0f%% recession state probability",
        ms_prob_raw * 100
      ),
      du
    )
  }

  mt <- if (!is.null(glm_result) && !is.na(ms_prob_raw)) {
    "GLM + Markov + Anomaly"
  } else if (!is.null(glm_result)) {
    "GLM + Anomaly Blend"
  } else {
    "Hand-tuned + Anomaly Blend"
  }

  list(
    prob = prob,
    tier = tier,
    drivers_up = head(du, 4),
    drivers_down = head(dd2, 3),
    factor_contributions = contrib_df,
    anomaly = anomaly_result,
    markov_prob = ms_prob_raw,
    model_type = mt,
    n_obs = glm_result$n_obs %||% NULL,
    aic = glm_result$aic %||% NULL
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: .build_factor_bar_chart — 4-colour + regime-conditional table
# ═══════════════════════════════════════════════════════════════════════════════

.build_factor_bar_chart <- function(
  factor_contributions,
  model_type = "Hand-tuned log-odds",
  markov_result = NULL
) {
  if (is.null(factor_contributions) || nrow(factor_contributions) == 0) {
    return("<p style='color:#6b7585;font-size:11px;'>No contribution data.</p>")
  }

  fc <- factor_contributions[
    order(abs(factor_contributions$contribution), decreasing = TRUE),
  ]
  fc <- head(fc, 15)
  max_abs <- max(abs(fc$contribution), na.rm = TRUE)
  if (max_abs < 0.01) {
    max_abs <- 1
  }
  has_z <- any(!is.na(fc$z_stat))

  subtitle <- switch(
    model_type,
    "GLM + Markov + Anomaly" = "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated \u03b2s + Markov regime state (teal) + Anomaly blend (purple)</span>",
    "GLM + Anomaly Blend" = "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated \u03b2s + Anomaly blend &nbsp;|\u00a0Markov fitting pending</span>",
    "<span style='color:#9aa3b2;font-size:10px;'>Hand-tuned fallback active.</span>"
  )

  rows <- vapply(
    seq_len(nrow(fc)),
    function(i) {
      row <- fc[i, ]
      delta <- row$contribution
      is_anomaly <- grepl("Anomaly Signal", row$name)
      is_markov <- grepl("Markov Regime", row$name)
      col <- if (is_markov) {
        "#00b4d8"
      } else if (is_anomaly) {
        "#7c5cbf"
      } else if (delta > 0.02) {
        "#e94560"
      } else if (delta < -0.02) {
        "#2dce89"
      } else {
        "#9aa3b2"
      }
      pct <- round(min(abs(delta) / max_abs * 44, 44))
      bar_style <- if (delta >= 0) {
        sprintf("left:50%%;width:%dpx;background:%s;", pct * 3, col)
      } else {
        sprintf("right:50%%;width:%dpx;background:%s;", pct * 3, col)
      }
      z_badge <- if (has_z && !is.na(row$z_stat)) {
        zc <- if (abs(row$z_stat) > 2.5) {
          "#d0d0d0"
        } else if (abs(row$z_stat) > 1.5) {
          "#9aa3b2"
        } else {
          "#555"
        }
        sprintf(
          "&nbsp;<span style='color:%s;font-size:9px;'>(z=%.1f)</span>",
          zc,
          row$z_stat
        )
      } else {
        ""
      }
      tag <- if (is_markov) {
        "&nbsp;<span style='color:#00b4d8;font-size:9px;'>\u25c6 REGIME</span>"
      } else if (is_anomaly) {
        "&nbsp;<span style='color:#a785e0;font-size:9px;'>\u26a0 ANOMALY</span>"
      } else {
        ""
      }
      nc <- if (is_markov) {
        "#7dd8f0"
      } else if (is_anomaly) {
        "#c8a8f0"
      } else {
        "#d0d0d0"
      }

      sprintf(
        "<div style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'>
         <div style='width:220px;text-align:right;color:%s;font-size:10.5px;flex-shrink:0;line-height:1.3;'>%s%s%s</div>
         <div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;overflow:hidden;'>
           <div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;z-index:1;'></div>
           <div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div>
         </div>
         <div style='width:48px;color:%s;font-size:10px;font-weight:600;text-align:right;'>%+.2f</div>
       </div>",
        nc,
        htmltools::htmlEscape(row$name),
        z_badge,
        tag,
        bar_style,
        col,
        delta
      )
    },
    character(1)
  )

  # ── Regime-conditional means mini-table ──────────────────────────────────────
  regime_table_html <- if (
    !is.null(markov_result) && !is.null(markov_result$regime_means)
  ) {
    header_row <- "<tr style='background:#1e2640;'>
      <th style='padding:5px 8px;color:#9aa3b2;font-size:10px;text-align:left;'>Variable</th>
      <th style='padding:5px 8px;color:#2dce89;font-size:10px;text-align:right;'>Expansion mean</th>
      <th style='padding:5px 8px;color:#e94560;font-size:10px;text-align:right;'>Recession mean</th>
      <th style='padding:5px 8px;color:#9aa3b2;font-size:10px;text-align:right;'>Diff (signal strength)</th>
    </tr>"
    data_rows <- paste(
      vapply(
        markov_result$regime_means,
        function(rm) {
          diff_val <- rm$recession - rm$expansion
          diff_col <- if (abs(diff_val) > 1) "#f4a261" else "#9aa3b2"
          sprintf(
            "<tr style='border-bottom:1px solid #2a3042;'>
         <td style='padding:5px 8px;color:#d0d0d0;font-size:10px;'>%s</td>
         <td style='padding:5px 8px;color:#2dce89;font-size:10px;text-align:right;'>%.2f</td>
         <td style='padding:5px 8px;color:#e94560;font-size:10px;text-align:right;'>%.2f</td>
         <td style='padding:5px 8px;color:%s;font-size:10px;text-align:right;font-weight:600;'>%+.2f</td>
       </tr>",
            htmltools::htmlEscape(rm$variable),
            rm$expansion,
            rm$recession,
            diff_col,
            diff_val
          )
        },
        character(1)
      ),
      collapse = ""
    )

    sprintf(
      "<div style='margin-top:14px;'>
       <div style='color:#9aa3b2;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>
         Regime-Conditional Means (Markov State Assignment)
         <span style='color:#555;font-weight:400;margin-left:8px;'>how each variable behaves in each regime historically</span>
       </div>
       <table style='width:100%%;border-collapse:collapse;'>%s%s</table>
     </div>",
      header_row,
      data_rows
    )
  } else {
    ""
  }

  paste0(
    "<div style='margin-bottom:18px;'>",
    "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;'>",
    "<span style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>",
    "Live Factor Contributions \u2014 Tier\u00a03: GLM + Markov + Anomaly</span></div>",
    "<div style='margin-bottom:8px;'>",
    subtitle,
    "</div>",
    # Four-colour legend
    "<div style='display:flex;flex-wrap:wrap;gap:14px;font-size:10px;color:#9aa3b2;margin-bottom:9px;'>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#e94560;border-radius:2px;margin-right:4px;'></span>Raises recession risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#2dce89;border-radius:2px;margin-right:4px;'></span>Lowers recession risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#7c5cbf;border-radius:2px;margin-right:4px;'></span>Anomaly (uncertainty)</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#00b4d8;border-radius:2px;margin-right:4px;'></span>Markov regime state</span>",
    "<span style='color:#555;'>| zero |</span></div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'>",
    "<span>\u25c4 Protective</span><span>Recessionary \u25ba</span></div>",
    paste(rows, collapse = ""),
    regime_table_html,
    "<div style='font-size:9px;color:#555;margin-top:8px;'>",
    "Teal bar = Markov regime pull. Purple = anomaly uncertainty. Red/green = GLM factor contributions. ",
    "Regime table shows historical mean values of each variable in expansion vs recession states as identified by the Markov model.",
    "</div></div>"
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: build_growth_outlook — trains Markov + rolling retrain
# ═══════════════════════════════════════════════════════════════════════════════

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
  sent <- gv("UMCSENT")
  indpro <- gv("INDPRO")
  vix_vals <- gv("VIXCLS")
  hy <- gv("BAMLH0A0HYM2")
  oil <- gv("DCOILWTICO")
  retail <- gv("RSAFS")

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
        sprintf("%.1f%% unemployment; %+.0fK/mo payrolls", u, pay_3m)
      } else {
        "N/A"
      }
    ),
    consumer = list(
      weight = 2.0,
      label = "Consumer Demand",
      score = mean(
        c(.score(ret_real, 1, -1, TRUE), .score(cs, 85, 65, TRUE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(ret_real) && !is.na(cs)) {
        sprintf("Real retail %.1f%%; sentiment %.0f", ret_real, cs)
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
          "Real rate %.1f%%; 10Y-2Y %.2f pp; %d mo inv.",
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
        c(.score(hs, 1400, 1000, TRUE), .score(mort_v, 6, 7.5, FALSE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(hs) && !is.na(mort_v)) {
        sprintf("%.0fK starts; %.2f%% mortgage", hs, mort_v)
      } else {
        "N/A"
      }
    ),
    financial = list(
      weight = 1.5,
      label = "Financial Conditions",
      score = mean(
        c(.score(vix_v, 18, 28, FALSE), .score(hy_v, 4.5, 6, FALSE)),
        na.rm = TRUE
      ),
      detail = if (!is.na(vix_v) && !is.na(hy_v)) {
        sprintf("VIX %.1f; HY %.2f%%", vix_v, hy_v)
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
        sprintf("CPI %.1f%%; Core PCE %.1f%%", cpi_y, pce_y)
      } else {
        "N/A"
      }
    ),
    industrial = list(
      weight = 1.0,
      label = "Industrial Output",
      score = .score(ip_y, 1, -1, TRUE),
      detail = if (!is.na(ip_y)) sprintf("IP YoY %.1f%%", ip_y) else "N/A"
    )
  )
  tw <- sum(sapply(components, `[[`, "weight"))
  raw_score <- sum(sapply(components, function(c) c$score * c$weight)) / tw
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
      summary = "Below-trend but positive."
    ),
    score_0_10 >= 3.5 ~ list(
      label = "Stall / Slowdown",
      color = "#f4a261",
      icon = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk."
    ),
    TRUE ~ list(
      label = "Contraction Risk",
      color = "#e94560",
      icon = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals."
    )
  )
  drag <- names(which.min(sapply(components, `[[`, "score")))
  supp <- names(which.max(sapply(components, `[[`, "score")))
  swing_factors <- list(
    biggest_drag = list(
      name = components[[drag]]$label,
      score = round(components[[drag]]$score, 2),
      detail = components[[drag]]$detail
    ),
    biggest_support = list(
      name = components[[supp]]$label,
      score = round(components[[supp]]$score, 2),
      detail = components[[supp]]$detail
    ),
    yield_curve_watch = if (inv_months > 6) {
      sprintf("Inverted %d months.", inv_months)
    } else if (!is.na(yield_spread) && yield_spread > 0) {
      sprintf("Re-steepened to %.2f pp.", yield_spread)
    } else {
      NULL
    },
    fed_watch = if (!is.na(real_rate) && real_rate > 2) {
      sprintf("Real rate %.1f%% — restrictive.", real_rate)
    } else {
      NULL
    }
  )

  equity_drawdown_pct <- if (!is.null(mkt_returns)) {
    spy <- mkt_returns %>%
      dplyr::filter(symbol == "SPY") %>%
      dplyr::arrange(date)
    if (nrow(spy) >= 52) {
      cur <- tail(spy$close, 1)
      pk <- max(spy$close[max(1, nrow(spy) - 252):nrow(spy)], na.rm = TRUE)
      round((cur / pk - 1) * 100, 1)
    } else {
      NA_real_
    }
  } else {
    NA_real_
  }

  # ── Train / retrieve models — Tier 3 rolling retrain + Markov ────────────────
  cache_name <- ".recession_t3_cache"
  glm_result <- tryCatch(
    {
      cache <- if (exists(cache_name, envir = .GlobalEnv)) {
        get(cache_name, envir = .GlobalEnv)
      } else {
        NULL
      }
      needs_train <- is.null(cache) ||
        difftime(
          Sys.time(),
          cache$glm$trained_at %||% (Sys.time() - 1e6),
          units = "days"
        ) >
          30
      if (needs_train) {
        message("[synopsis_tier3] Rolling retrain + Markov fit...")
        glm_obj <- rolling_retrain(fred_data, window = 360L) # ← Tier 3: rolling
        ms_obj <- train_markov_model(fred_data)
        assign(
          cache_name,
          list(glm = glm_obj, markov = ms_obj),
          envir = .GlobalEnv
        )
      }
      get(cache_name, envir = .GlobalEnv)$glm
    },
    error = function(e) {
      message("[synopsis_tier3] cache error: ", e$message)
      NULL
    }
  )

  markov_result <- tryCatch(
    get(cache_name, envir = .GlobalEnv)$markov,
    error = function(e) NULL
  )

  anomaly_detector <- if (!is.null(glm_result)) {
    glm_result$anomaly_detector
  } else {
    NULL
  }

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
    glm_result = glm_result,
    anomaly_detector = anomaly_detector,
    markov_result = markov_result
  ) # ← Tier 3

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


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: render_growth_outlook_html — passes markov_result to modal
# ═══════════════════════════════════════════════════════════════════════════════
# (Minimal override: we only need to pass the right arguments to .build_model_modal)
# The original render_growth_outlook_html calls .build_model_modal() at the end.
# We override here to thread markov_result through.

render_growth_outlook_html <- function(outlook) {
  if (is.null(outlook)) {
    return(HTML(
      "<p style='color:#6b7585;padding:16px;'>Growth outlook loading.</p>"
    ))
  }

  rp <- outlook$recession_prob
  reg <- outlook$regime
  score <- outlook$score
  comps <- outlook$components
  swing <- outlook$swing
  gc <- reg$color

  gauge_html <- sprintf(
    "<div style='margin:12px 0 16px;'><div style='display:flex;justify-content:space-between;margin-bottom:5px;'><span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span><span style='color:#9aa3b2;font-size:11px;'>Expansion</span></div><div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'><div style='background:linear-gradient(90deg,%s,%s);width:%.0f%%;height:100%%;border-radius:6px;'></div></div><div style='margin-top:5px;display:flex;justify-content:space-between;'><span style='color:#9aa3b2;font-size:10px;'>0</span><span style='color:%s;font-size:11px;font-weight:700;'>Score: %.1f / 10</span><span style='color:#9aa3b2;font-size:10px;'>10</span></div></div>",
    if (score < 5) "#e94560" else "#f4a261",
    gc,
    score / 10 * 100,
    gc,
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
        bw <- round((s + 1) / 2 * 100)
        sprintf(
          "<div style='margin-bottom:8px;'><div style='display:flex;justify-content:space-between;margin-bottom:3px;'><span style='color:#9aa3b2;font-size:11px;'>%s</span><span style='color:%s;font-size:11px;font-weight:600;'>%s</span></div><div style='background:#2a3042;border-radius:3px;height:5px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:3px;'></div></div><div style='color:#6b7585;font-size:10px;margin-top:2px;'>%s</div></div>",
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
          bw,
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
          "<li style='margin-bottom:6px;'><span style='color:#00b4d8;font-weight:600;'>\u2699 Fed:</span> %s</li>",
          htmltools::htmlEscape(swing$fed_watch)
        )
      }
    ),
    collapse = ""
  )

  rp_html <- ""
  if (!is.null(rp)) {
    prob <- rp$prob
    tier <- rp$tier
    bc <- tier$color
    markov_badge <- if (!is.null(rp$markov_prob) && !is.na(rp$markov_prob)) {
      sprintf(
        "&nbsp;<span style='background:rgba(0,180,216,0.12);color:#00b4d8;border:1px solid rgba(0,180,216,0.3);border-radius:8px;padding:1px 8px;font-size:9px;font-weight:600;'>MS: %.0f%%</span>",
        rp$markov_prob * 100
      )
    } else {
      ""
    }
    anom_badge <- if (!is.null(rp$anomaly) && rp$anomaly$score > 0.1) {
      sprintf(
        "&nbsp;<span style='background:rgba(124,92,191,0.12);color:#a785e0;border:1px solid rgba(124,92,191,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>\u26a0 Anomaly %.0f%%</span>",
        rp$anomaly$score * 100
      )
    } else {
      ""
    }
    uh <- if (length(rp$drivers_up) > 0) {
      paste(
        sprintf(
          "<li><span style='color:#e94560;'>\u25b2</span> %s</li>",
          htmltools::htmlEscape(rp$drivers_up)
        ),
        collapse = ""
      )
    } else {
      ""
    }
    dh <- if (length(rp$drivers_down) > 0) {
      paste(
        sprintf(
          "<li><span style='color:#2dce89;'>\u25bc</span> %s</li>",
          htmltools::htmlEscape(rp$drivers_down)
        ),
        collapse = ""
      )
    } else {
      ""
    }
    ds <- if (nchar(uh) > 0 || nchar(dh) > 0) {
      sprintf(
        "<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>%s%s</ul>",
        uh,
        dh
      )
    } else {
      ""
    }
    rp_html <- sprintf(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:14px 16px;margin-top:14px;'><div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'><div><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'><i class=\"fa fa-%s\" style=\"color:%s;margin-right:6px;\"></i>Recession Probability \u2014 12-Month Horizon%s%s</div><div style='margin-top:4px;'><span style='color:%s;font-size:36px;font-weight:800;'>%s%%</span><span style='color:%s;font-size:14px;font-weight:600;margin-left:8px;background:rgba(%s,0.12);padding:2px 10px;border-radius:12px;border:1px solid rgba(%s,0.3);'>%s</span></div></div><div style='text-align:right;'><div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0%% \u00b7 50%% \u00b7 100%%</div><div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);border-radius:7px;position:relative;'><div style='position:absolute;top:-3px;left:calc(%s%% - 8px);width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:8px solid #fff;'></div></div><div style='font-size:10px;color:#6b7585;margin-top:3px;'>\u25b2 Current</div></div></div><div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;border-radius:6px;padding:8px 12px;border-left:3px solid %s;'>%s</div>%s<div style='color:#555;font-size:10px;margin-top:8px;'>Tier 3: Rolling GLM (30yr window) + Hamilton Markov-switching + anomaly detection. NBER-calibrated.</div></div>",
      bc,
      tier$icon,
      bc,
      markov_badge,
      anom_badge,
      bc,
      sprintf("%.0f", prob),
      bc,
      bc,
      bc,
      tier$label,
      sprintf("%.0f", min(prob, 98)),
      bc,
      htmltools::htmlEscape(tier$desc),
      ds
    )
  }

  mr <- tryCatch(
    get(".recession_t3_cache", envir = .GlobalEnv)$markov,
    error = function(e) NULL
  )

  HTML(paste0(
    sprintf(
      "<div style='display:flex;gap:14px;'><div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:16px 18px;'><div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'><i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook</div><div style='color:%s;font-size:22px;font-weight:800;margin-bottom:2px;'>%s</div><div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>GDP Est: %s annualized</div>%s<div style='background:#0f1117;border-radius:6px;padding:10px 12px;color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid %s;'>%s</div><div style='color:#555;font-size:10px;margin-top:10px;'>%d indicators + Markov-switching + rolling GLM + anomaly detection. As of %s.<button onclick=\"document.getElementById('recModelModal').style.display='flex'\" style='margin-left:8px;background:transparent;border:1px solid #2a3042;color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'><i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details</button></div>%s</div><div style='flex:1;display:flex;flex-direction:column;gap:10px;'><div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;flex:1;'><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>%s</div><div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;'><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div><ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>%s</ul></div></div></div>",
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
      factor_contributions = if (!is.null(rp)) {
        rp$factor_contributions
      } else {
        NULL
      },
      model_type = if (!is.null(rp)) {
        rp$model_type %||% "Hand-tuned"
      } else {
        "Hand-tuned"
      },
      anomaly = if (!is.null(rp)) rp$anomaly else NULL,
      markov_result = mr
    )
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: .build_model_modal
# ═══════════════════════════════════════════════════════════════════════════════

.build_model_modal <- function(
  factor_contributions = NULL,
  model_type = "Hand-tuned log-odds",
  anomaly = NULL,
  markov_result = NULL
) {
  # ← Tier 3

  bar_chart_html <- .build_factor_bar_chart(
    factor_contributions,
    model_type,
    markov_result
  )

  # ── Markov regime status panel ────────────────────────────────────────────────
  markov_html <- if (!is.null(markov_result)) {
    ms_pct <- round(markov_result$current_rec_prob * 100)
    ms_col <- if (ms_pct > 60) {
      "#e94560"
    } else if (ms_pct > 35) {
      "#f4a261"
    } else {
      "#2dce89"
    }
    wnd_lbl <- if (!is.null(markov_result$window_months)) {
      sprintf(" (rolling %d-month window)", markov_result$window_months)
    } else {
      ""
    }
    sprintf(
      "<div style='background:#0f1a27;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>
         <div style='color:#00b4d8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           <i class=\"fa fa-random\" style=\"margin-right:6px;\"></i>Markov Regime Model Status%s
         </div>
         <div style='display:flex;align-items:center;gap:10px;margin-bottom:8px;'>
           <span style='color:#9aa3b2;font-size:11px;width:160px;'>Recession state p (smoothed)</span>
           <div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'>
             <div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div>
           </div>
           <span style='color:%s;font-size:14px;font-weight:700;width:40px;text-align:right;'>%d%%</span>
         </div>
         <div style='color:#9aa3b2;font-size:11px;margin-bottom:8px;'>
           <b style='color:#7dd8f0;'>States:</b> %d &nbsp;|\u00a0
           <b style='color:#7dd8f0;'>Training obs:</b> %d months &nbsp;|\u00a0
           <b style='color:#7dd8f0;'>Expansion IP mean:</b> %.1f%% YoY &nbsp;|\u00a0
           <b style='color:#7dd8f0;'>Recession IP mean:</b> %.1f%% YoY
         </div>
         <div style='color:#d0d0d0;font-size:11px;line-height:1.75;'>
           Hamilton (1989) 2-state Markov-switching model identifies expansion and recession regimes
           directly from the data without requiring NBER dates as labels. The smoothed probability
           is the posterior probability of being in the recession state given all available information.
           Unlike the GLM, it can identify regime shifts even when individual factor thresholds aren\u2019t triggered,
           because it looks at the overall pattern of mean + variance changes across multiple series simultaneously.
         </div>
       </div>",
      wnd_lbl,
      ms_col,
      ms_pct,
      ms_col,
      ms_pct,
      markov_result$n_states %||% 2L,
      markov_result$n_obs %||% 0L,
      markov_result$state_means_ip[setdiff(
        1:markov_result$n_states,
        markov_result$recession_state
      )[1]] %||%
        0,
      markov_result$state_means_ip[markov_result$recession_state] %||% 0
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'>
       <i class=\"fa fa-info-circle\" style=\"color:#00b4d8;margin-right:6px;\"></i>
       Markov regime model not yet fitted. Install <code>MSwM</code> and ensure INDPRO / T10Y2Y / BAMLH0A0HYM2 data is available.
     </div>"
  }

  # ── Anomaly status (same as Tier 2) ──────────────────────────────────────────
  anomaly_html <- if (!is.null(anomaly) && anomaly$score > 0.05) {
    score_pct <- round(anomaly$score * 100)
    bar_col <- if (anomaly$score > 0.6) {
      "#e94560"
    } else if (anomaly$score > 0.3) {
      "#f4a261"
    } else {
      "#7c5cbf"
    }
    sprintf(
      "<div style='background:#1e1030;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'><div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'><i class=\"fa fa-exclamation-triangle\" style=\"margin-right:6px;\"></i>Anomaly Detection Active</div><div style='display:flex;align-items:center;gap:10px;margin-bottom:6px;'><span style='color:#9aa3b2;font-size:11px;width:90px;'>Score</span><div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div></div><span style='color:%s;font-size:13px;font-weight:700;width:40px;'>%d%%</span></div><div style='color:#d0d0d0;font-size:11px;line-height:1.75;'><b>MD=%.2f</b> &nbsp;\u2014 %s &nbsp;|\u00a0 Blend: p_final = (0.55\u00d7p_GLM + 0.45\u00d7p_Markov)\u00d7(1\u22120.35A) + 0.25A</div></div>",
      bar_col,
      score_pct,
      bar_col,
      score_pct,
      anomaly$md %||% 0,
      anomaly$label
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-check-circle\" style=\"color:#2dce89;margin-right:6px;\"></i>Anomaly score normal. No uncertainty adjustment applied.</div>"
  }

  # ── Factor table ─────────────────────────────────────────────────────────────
  factors <- list(
    list(
      rank = 1,
      name = "Yield Curve (10Y-2Y spread)",
      weight = "High",
      col = "#e94560",
      source = "Estrella & Mishkin (1998)",
      mechanism = "Every 1pp narrowing: +0.85. Duration: +0.3–+1.5.",
      interpret = "Most reliable single 12-month predictor."
    ),
    list(
      rank = 2,
      name = "Sahm Rule / Labor Momentum",
      weight = "High",
      col = "#e94560",
      source = "Sahm (2019); BLS",
      mechanism = "\u22650.5pp rise: +1.8. Neg payrolls: +0.6–+1.5.",
      interpret = "Perfect recession-trigger record since 1970."
    ),
    list(
      rank = 3,
      name = "Prime-Age LFPR (25\u201354)",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS LNS11300060",
      mechanism = "<82%: +0.4. 6M drop >0.2pp: +0.4–+0.9.",
      interpret = "Captures hidden labour weakness."
    ),
    list(
      rank = 4,
      name = "Jobless Claims Trend",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS ICSA (weekly)",
      mechanism = "4wk/26wk MA ratio: >10%: +0.6; >20%: +1.2.",
      interpret = "Most real-time labour signal."
    ),
    list(
      rank = 5,
      name = "Quits Rate YoY",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS JTSQUL",
      mechanism = "YoY decline >8%: +0.4; >15%: +0.8.",
      interpret = "Leads Sahm by 1\u20132 quarters."
    ),
    list(
      rank = 6,
      name = "Oil Price Shock (YoY)",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED DCOILWTICO",
      mechanism = ">25%: +0.6; >50%: +1.0; <\u221230%: +0.5.",
      interpret = "Hamilton (1983): precedes every major US recession."
    ),
    list(
      rank = 7,
      name = "Equity Bear Market",
      weight = "Medium",
      col = "#f4a261",
      source = "SPY / tidyquant",
      mechanism = ">10%: +0.5; >20%: +1.1; >30%: +1.6.",
      interpret = ">20% drawdown preceded every post-WW2 recession."
    ),
    list(
      rank = 8,
      name = "HY Credit Spreads",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED BAMLH0A0HYM2",
      mechanism = ">4.5%: +0.2; >5.5%: +0.7; >7%: +1.4.",
      interpret = ">500bp = credit market stress."
    ),
    list(
      rank = 9,
      name = "USD Surge (YoY)",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DTWEXBGS",
      mechanism = ">8%: +0.4; >12%: +0.8.",
      interpret = "Tightens global financial conditions."
    ),
    list(
      rank = 10,
      name = "Real Fed Funds Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FEDFUNDS \u2212 CPI YoY",
      mechanism = ">1%: +0.2; >2%: +0.5; >3%: +1.0.",
      interpret = "Monetary restrictiveness with 6\u201318M lag."
    ),
    list(
      rank = 11,
      name = "Consumer Sentiment",
      weight = "Low",
      col = "#9aa3b2",
      source = "U Michigan UMCSENT",
      mechanism = "<60: +0.7; <70: +0.3; >90: \u22120.4.",
      interpret = "Leads spending by 1\u20132 quarters."
    ),
    list(
      rank = 12,
      name = "Inventory-to-Sales Ratio",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED ISRATIO",
      mechanism = ">1.40–>1.55: +0.1–+0.7.",
      interpret = "Excess inventory \u2192 cut orders \u2192 hiring slows."
    ),
    list(
      rank = 13,
      name = "CC Delinquency Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DRCCLACBS",
      mechanism = ">2.5–>4%: +0.2–+0.9.",
      interpret = "Consumer stress 2\u20133Q before unemployment rises."
    ),
    list(
      rank = 14,
      name = "Anomaly Signal (Mahalanobis)",
      weight = "Blend",
      col = "#7c5cbf",
      source = "Training feature distribution",
      mechanism = "p = p_base\u00d7(1\u22120.35A) + 0.25A.",
      interpret = "Widens uncertainty for historically unprecedented conditions."
    ),
    list(
      rank = 15,
      name = "Markov Regime State",
      weight = "Regime",
      col = "#00b4d8",
      source = "MSwM: Hamilton (1989) HMM",
      mechanism = "p_final = 0.55\u00d7p_GLM + 0.45\u00d7p_Markov.",
      interpret = "Identifies structural regime shifts without needing labeled data. Complements GLM which assumes constant coefficients."
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
          "Blend" = "#7c5cbf",
          "Regime" = "#00b4d8",
          "#9aa3b2"
        )
        sprintf(
          "<tr style='border-bottom:1px solid #2a3042;'><td style='padding:10px 8px;color:%s;font-weight:700;font-size:12px;'>%d. %s</td><td style='padding:10px 8px;'><span style='background:rgba(%s,0.12);color:%s;border:1px solid rgba(%s,0.3);border-radius:8px;padding:1px 8px;font-size:10px;font-weight:600;'>%s</span></td><td style='padding:10px 8px;color:#9aa3b2;font-size:11px;'>%s</td><td style='padding:10px 8px;color:#9aa3b2;font-size:11px;line-height:1.5;'>%s</td><td style='padding:10px 8px;color:#d0d0d0;font-size:11px;line-height:1.5;'>%s</td></tr>",
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
                  color:#9aa3b2;font-size:20px;cursor:pointer;'>&times;</button>

         <div style='color:#00b4d8;font-size:16px;font-weight:800;margin-bottom:4px;'>
           <i class=\"fa fa-brain\" style=\"margin-right:8px;\"></i>
           Recession Probability Model \u2014 Technical Reference
           <span style='font-size:11px;font-weight:400;color:#00b4d8;margin-left:10px;border:1px solid rgba(0,180,216,0.3);border-radius:6px;padding:1px 8px;'>
             Tier\u00a03: Rolling GLM + Markov-Switching + Anomaly Detection
           </span>
         </div>
         <div style='color:#9aa3b2;font-size:12px;margin-bottom:20px;'>
           Three-model ensemble: (1) logistic regression retrained on a rolling 30-year window,
           (2) Hamilton (1989) 2-state Markov-switching regime model, and (3) Mahalanobis anomaly
           detector that widens uncertainty for historically unprecedented conditions.
           Calibrated to NBER recession dates. Not a point forecast.
         </div>

         <!-- Bar chart -->
         <div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
    bar_chart_html,
    "</div>

         <!-- Markov status -->",
    markov_html,

    "<!-- Anomaly status -->",
    anomaly_html,

    "<!-- Interpretability note -->
         <div style='background:#1e2640;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'>
           <div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'>
             <i class=\"fa fa-eye\" style=\"margin-right:6px;\"></i>Interpretability, Trust &amp; Architecture
           </div>
           <div style='color:#d0d0d0;font-size:12px;line-height:1.8;'>
             <b style='color:#e0e0e0;'>Tier 3 vs Tier 2:</b> Adds a Markov-switching component that operates without pre-specified thresholds.
             It identifies latent regimes from the joint distribution of macro variables, capturing
             structural breaks (e.g., the 2020 shock) as regime transitions rather than
             parameter-space extrapolation. The rolling retrain ensures the GLM\u2019s coefficient estimates
             adapt to structural changes over time rather than being frozen to a 1960\u20132020 average.<br/>
             <b style='color:#e0e0e0;'>Three-way blend:</b> p_final = 0.55\u00d7p_GLM + 0.45\u00d7p_Markov, then anomaly-adjusted.
             The Markov weight (0.45) reflects genuine uncertainty about which model is better-calibrated
             in the current data regime. When MSwM is unavailable, falls back to Tier 2 GLM-only path.
           </div>
         </div>

         <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           Factor Detail \u2014 15 Factors (13 GLM + Anomaly Blend + Markov Regime)
         </div>
         <div style='overflow-x:auto;'>
         <table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>
           <thead>
             <tr style='background:#1e2640;border-bottom:2px solid #2a3042;'>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:200px;'>Factor</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;'>Weight / Type</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:160px;'>Source</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:220px;'>Mechanism</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:240px;'>Why It Matters</th>
             </tr>
           </thead>
           <tbody>",
    rows,
    "</tbody>
         </table></div>

         <div style='color:#555;font-size:11px;margin-top:16px;line-height:1.7;'>
           <b style='color:#9aa3b2;'>Tier 3 formula:</b>
           p_blend = 0.55\u00d7p_GLM + 0.45\u00d7p_Markov &nbsp;|\u00a0
           A = min(1, MD/(d95\u00d71.5)) &nbsp;|\u00a0
           p_final = p_blend\u00d7(1\u22120.35A) + 0.25A &nbsp;|\u00a0
           Clamped [2%, 97%].<br/>
           GLM retrained on rolling 30-year window; Markov model refitted monthly.<br/>
           <b style='color:#9aa3b2;'>References:</b>
           Estrella &amp; Mishkin (1998) \u2014 yield curve; Sahm (2019) \u2014 labour;
           Hamilton (1989) \u2014 Markov-switching; Mahalanobis (1936) \u2014 distance metric.
           MSwM package (Sanchez-Espigares &amp; Lopez-Moreno).
         </div>
       </div>
     </div>"
  )
}
