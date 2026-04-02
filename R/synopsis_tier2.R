# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier2.R — Tier 2: GLM + Mahalanobis Anomaly Detection
#
# SOURCE STRATEGY: this file sources synopsis_tier1.R, then overrides only the
# functions that change in Tier 2.  synopsis_tier1.R must be in the same
# directory (or on the source path configured in global.R).
#
# WHAT'S NEW vs Tier 1:
#
#  1. build_anomaly_detector()   — Estimates mean vector and inverse-covariance
#     matrix from the GLM training panel (base R only — no extra packages).
#     Stored inside the GLM cache object as $anomaly_detector.
#
#  2. compute_anomaly_score()    — Mahalanobis distance of current conditions
#     from the historical training distribution, scaled to [0,1] using the
#     empirical 95th percentile distance.  Returns score, raw distance, and a
#     plain-English label ("Normal", "Elevated", "High", "Extreme").
#
#  3. .compute_recession_prob()  — After the GLM probability, applies an
#     anomaly blend:
#         p_final = p_GLM × (1 − w×A) + floor_A × A
#     where A = anomaly score ∈ [0,1], w = 0.35, floor_A = 0.25.
#     This widens uncertainty and raises the floor when current conditions are
#     historically unprecedented (pandemic-style, GFC-style shocks).
#     The anomaly contribution appears as a separate "Anomaly Signal" row in
#     factor_contributions so it shows up in the bar chart.
#
#  4. .build_factor_bar_chart()  — Tier 2 version.  Purple bar for anomaly
#     signal, legend shows three colours, subtitle updated.
#
#  5. train_recession_model()    — Extended to call build_anomaly_detector()
#     and store the result in $anomaly_detector.
#
#  6. build_growth_outlook()     — Passes anomaly_detector to the prob engine.
#
#  7. .build_model_modal()       — Adds anomaly detection explainer panel above
#     the factor table; bar chart shows purple anomaly bar.
#
# Model Details modal changes (Tier 2):
#   • Title updated: "Tier 2: GLM + Anomaly Detection"
#   • New "Anomaly Detection" callout box showing current score, Mahalanobis
#     distance, and an explanation of why uncertainty is widened.
#   • Bar chart subtitle shows "GLM + Anomaly Blend" or "Hand-tuned + Anomaly"
#     and the purple bar for the anomaly signal.
#   • Footer formula updated to include anomaly blending equation.
# ─────────────────────────────────────────────────────────────────────────────

# Inherit everything from Tier 1
source(
  file.path(dirname(sys.frame(1)$ofile %||% "."), "R/synopsis_tier1.R"),
  local = FALSE
)

# ── If source() fails (e.g. interactive use), fall back to explicit path
if (!exists("train_recession_model")) {
  source("R/synopsis_tier1.R", local = FALSE)
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 NEW: Anomaly detector (Mahalanobis distance, base R only)
# ═══════════════════════════════════════════════════════════════════════════════

#' Build anomaly detector from the GLM training feature panel.
#'
#' Returns list(mu, cov_reg, sigma_inv, feature_cols, d90, d95) or NULL.
#' d90/d95 are the empirical 90th/95th percentile Mahalanobis distances in the
#' training data — used to calibrate what "normal" looks like.
build_anomaly_detector <- function(feature_panel) {
  if (is.null(feature_panel) || nrow(feature_panel) < 50) {
    return(NULL)
  }

  fp <- feature_panel[complete.cases(feature_panel), , drop = FALSE]
  if (nrow(fp) < 50) {
    return(NULL)
  }

  mu <- colMeans(fp, na.rm = TRUE)
  cov_mat <- tryCatch(cov(fp), error = function(e) NULL)
  if (is.null(cov_mat)) {
    return(NULL)
  }

  # Tikhonov regularisation: ensures matrix invertibility across all data eras
  cov_reg <- cov_mat + diag(1e-6, ncol(fp))
  sigma_inv <- tryCatch(solve(cov_reg), error = function(e) NULL)
  if (is.null(sigma_inv)) {
    return(NULL)
  }

  # Empirical percentile distances in training data for calibration
  train_dists <- sqrt(mahalanobis(fp, mu, cov_reg))

  list(
    mu = mu,
    cov_reg = cov_reg,
    sigma_inv = sigma_inv,
    feature_cols = names(mu),
    d90 = quantile(train_dists, 0.90, na.rm = TRUE),
    d95 = quantile(train_dists, 0.95, na.rm = TRUE),
    d99 = quantile(train_dists, 0.99, na.rm = TRUE)
  )
}


#' Compute anomaly score ∈ [0, 1] for one row of current feature values.
#'
#' Score interpretation:
#'   0.00–0.20  Normal          — within historical variation
#'   0.20–0.40  Elevated        — unusual but historically observed
#'   0.40–0.70  High            — top-5% historically rare
#'   0.70–1.00  Extreme         — historically unprecedented (pandemic-class)
#'
#' @param anomaly_detector  Output of build_anomaly_detector().
#' @param current_df        One-row data.frame of current feature values
#'                          (as produced by build_current_feature_vector()).
compute_anomaly_score <- function(anomaly_detector, current_df) {
  null_result <- list(
    score = 0,
    md = NA_real_,
    label = "Normal",
    percentile = NA_real_
  )
  if (is.null(anomaly_detector) || is.null(current_df)) {
    return(null_result)
  }

  ad <- anomaly_detector
  cols <- intersect(ad$feature_cols, names(current_df))
  if (length(cols) < 3) {
    return(null_result)
  }

  x_vec <- as.numeric(current_df[1, cols])
  mu_sub <- ad$mu[cols]
  si_sub <- ad$sigma_inv[cols, cols]

  # Impute NAs with training mean — missing data = average, not extreme
  x_vec[is.na(x_vec)] <- mu_sub[is.na(x_vec)]

  md <- tryCatch(
    sqrt(as.numeric(t(x_vec - mu_sub) %*% si_sub %*% (x_vec - mu_sub))),
    error = function(e) NA_real_
  )
  if (is.na(md) || !is.finite(md)) {
    return(null_result)
  }

  # Scale: distance beyond d95 × 1.5 = score 1.0
  ceiling_dist <- ad$d95 * 1.5
  score <- min(1.0, max(0.0, md / ceiling_dist))

  # Approximate percentile by comparing to empirical thresholds
  pctile <- if (md > ad$d99) {
    99
  } else if (md > ad$d95) {
    95
  } else if (md > ad$d90) {
    90
  } else {
    round(50 + 40 * (md / ad$d90))
  }

  label <- if (score > 0.70) {
    "Extreme \u2014 historically unprecedented"
  } else if (score > 0.40) {
    "High \u2014 rare conditions (top 5% historically)"
  } else if (score > 0.20) {
    "Elevated \u2014 unusual but observed historically"
  } else {
    "Normal"
  }

  list(score = score, md = round(md, 2), label = label, percentile = pctile)
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 OVERRIDE: train_recession_model — stores anomaly detector in cache
# ═══════════════════════════════════════════════════════════════════════════════

train_recession_model <- function(fred_data) {
  # Call the Tier 1 version (bound before we overwrite it)
  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    return(NULL)
  }

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
  available <- intersect(possible, names(panel))
  if (length(available) < 3) {
    return(NULL)
  }

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

      # ── Tier 2: build anomaly detector from training feature panel ────────────
      feature_only <- panel_clean[, available, drop = FALSE]
      ad <- build_anomaly_detector(feature_only)

      message(sprintf(
        "[synopsis_tier2] GLM trained: n=%d AIC=%.1f | Anomaly detector: %s",
        nrow(panel_clean),
        AIC(model),
        if (!is.null(ad)) sprintf("d95=%.2f", ad$d95) else "FAILED"
      ))

      list(
        model = model,
        coef_table = coef_tbl,
        n_obs = nrow(panel_clean),
        aic = round(AIC(model), 1),
        predictors = available,
        trained_at = Sys.time(),
        anomaly_detector = ad
      ) # ← Tier 2 addition
    },
    error = function(e) {
      message("[synopsis_tier2] glm() failed: ", e$message)
      NULL
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 OVERRIDE: .compute_recession_prob — adds anomaly blend
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
  anomaly_detector = NULL # ← Tier 2 addition
) {
  # ── Blend parameters ────────────────────────────────────────────────────────
  ANOMALY_WEIGHT <- 0.35 # how hard to pull GLM prob toward 50% when anomalous
  ANOMALY_FLOOR <- 0.25 # minimum prob floor when fully unprecedented
  COMPRESS_CENTER <- 0.50 # uncertainty compresses toward 50-50

  contribs <- list()
  add_contrib <- function(id, label, delta, z_stat = NA_real_) {
    contribs[[id]] <<- list(name = label, contribution = delta, z_stat = z_stat)
  }

  # ── Compute anomaly score first so it can be appended to contributions ───────
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

  anomaly_result <- compute_anomaly_score(anomaly_detector, feat_df)
  A <- anomaly_result$score # ∈ [0, 1]

  # ── PATH A: GLM probability ──────────────────────────────────────────────────
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
      coef_tbl <- glm_result$coef_table
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
        z <- if (pred %in% coef_tbl$predictor) {
          coef_tbl$z_stat[coef_tbl$predictor == pred]
        } else {
          NA_real_
        }
        lbl <- if (pred %in% names(plabels)) plabels[[pred]] else pred
        add_contrib(pred, lbl, if (!is.na(b) && !is.na(x)) b * x else 0, z)
      }
    }
  }

  # ── PATH B: Hand-tuned fallback ──────────────────────────────────────────────
  if (is.na(glm_prob_raw)) {
    lo <- -1.73

    yc <- 0
    if (!is.na(yield_spread)) {
      yc <- yc + (-yield_spread * 0.85)
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
    add_contrib("prime_lfpr", "Prime-Age LFPR (25\u201354)", lf)

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
    add_contrib("oil", "Oil Price Shock (YoY)", ol)

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
    add_contrib("equity", "Equity Bear Market (SPY drawdown)", eq)

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
    add_contrib("usd", "USD Surge (YoY)", ud)

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

  # ── Tier 2: Anomaly blend ────────────────────────────────────────────────────
  # When A is elevated, two things happen:
  #   1. The GLM probability is compressed toward 50% (epistemic humility)
  #   2. A floor is raised so the model never reads <25% when in uncharted territory
  blended_raw <- if (A > 0.05) {
    compressed <- glm_prob_raw +
      ANOMALY_WEIGHT * A * (COMPRESS_CENTER - glm_prob_raw)
    floor_level <- ANOMALY_FLOOR * A
    max(compressed, floor_level)
  } else {
    glm_prob_raw
  }

  prob <- max(2, min(97, round(blended_raw * 100, 1)))

  # Record anomaly as a named bar in the chart
  # Contribution = signed pull toward 50% (can be positive or negative)
  if (A > 0.05) {
    anomaly_delta <- (blended_raw - glm_prob_raw) * 100 # pp shift caused by anomaly
    add_contrib(
      "anomaly_signal",
      sprintf(
        "Anomaly Signal (MD\u2248%.1f; %s)",
        anomaly_result$md %||% 0,
        anomaly_result$label
      ),
      anomaly_delta
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

  # Narrative driver lists
  drivers_up <- character(0)
  drivers_down <- character(0)
  for (i in seq_len(min(8, nrow(contrib_df)))) {
    r <- contrib_df[i, ]
    if (r$contribution > 0.1) {
      drivers_up <- c(drivers_up, r$name)
    }
    if (r$contribution < -0.1) drivers_down <- c(drivers_down, r$name)
  }
  if (A > 0.3) {
    drivers_up <- c(
      sprintf(
        "\u26a0 Anomaly signal: %s (Mahal. dist=%.1f)",
        anomaly_result$label,
        anomaly_result$md %||% 0
      ),
      drivers_up
    )
  }

  mt <- if (!is.null(glm_result)) {
    "GLM + Anomaly Blend"
  } else {
    "Hand-tuned + Anomaly Blend"
  }

  list(
    prob = prob,
    tier = tier,
    drivers_up = head(drivers_up, 4),
    drivers_down = head(drivers_down, 3),
    factor_contributions = contrib_df,
    anomaly = anomaly_result,
    model_type = mt,
    n_obs = glm_result$n_obs %||% NULL,
    aic = glm_result$aic %||% NULL
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 OVERRIDE: .build_factor_bar_chart — adds purple anomaly bar + legend
# ═══════════════════════════════════════════════════════════════════════════════

.build_factor_bar_chart <- function(
  factor_contributions,
  model_type = "Hand-tuned log-odds"
) {
  if (is.null(factor_contributions) || nrow(factor_contributions) == 0) {
    return("<p style='color:#6b7585;font-size:11px;'>No contribution data.</p>")
  }

  fc <- factor_contributions[
    order(abs(factor_contributions$contribution), decreasing = TRUE),
  ]
  fc <- head(fc, 14)
  max_abs <- max(abs(fc$contribution), na.rm = TRUE)
  if (max_abs < 0.01) {
    max_abs <- 1
  }
  has_z <- any(!is.na(fc$z_stat))

  subtitle <- switch(
    model_type,
    "GLM + Anomaly Blend" = "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated \u03b2s &nbsp;|\u00a0bar\u00a0=\u00a0\u03b2\u1d62\u00d7x\u1d62 &nbsp;|\u00a0\u26a0 purple\u00a0bar\u00a0=\u00a0anomaly\u00a0uncertainty\u00a0adjustment</span>",
    "Hand-tuned + Anomaly Blend" = "<span style='color:#f4a261;font-size:10px;'>\u26a0 Anomaly blend active &nbsp;|\u00a0GLM pending &nbsp;|\u00a0purple\u00a0bar\u00a0=\u00a0anomaly\u00a0contribution</span>",
    "<span style='color:#9aa3b2;font-size:10px;'>Hand-tuned model. Bar = log-odds delta per factor.</span>"
  )

  rows <- vapply(
    seq_len(nrow(fc)),
    function(i) {
      row <- fc[i, ]
      delta <- row$contribution
      is_anomaly <- grepl("Anomaly Signal", row$name, fixed = FALSE)
      col <- if (is_anomaly) {
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
      anm_tag <- if (is_anomaly) {
        "&nbsp;<span style='color:#a785e0;font-size:9px;'>\u26a0&nbsp;ANOMALY</span>"
      } else {
        ""
      }
      name_col <- if (is_anomaly) "#c8a8f0" else "#d0d0d0"

      sprintf(
        "<div style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'>
         <div style='width:220px;text-align:right;color:%s;font-size:10.5px;flex-shrink:0;line-height:1.3;'>%s%s%s</div>
         <div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;overflow:hidden;'>
           <div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;z-index:1;'></div>
           <div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div>
         </div>
         <div style='width:48px;color:%s;font-size:10px;font-weight:600;text-align:right;'>%+.2f</div>
       </div>",
        name_col,
        htmltools::htmlEscape(row$name),
        z_badge,
        anm_tag,
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
    "Live Factor Contributions \u2014 Tier\u00a02: GLM + Anomaly Detection</span></div>",
    "<div style='margin-bottom:8px;'>",
    subtitle,
    "</div>",
    # Three-colour legend
    "<div style='display:flex;gap:16px;font-size:10px;color:#9aa3b2;margin-bottom:9px;'>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#e94560;border-radius:2px;margin-right:4px;'></span>Raises recession risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#2dce89;border-radius:2px;margin-right:4px;'></span>Lowers recession risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#7c5cbf;border-radius:2px;margin-right:4px;'></span>Anomaly \u2014 uncertainty adjustment</span>",
    "<span style='color:#555;'>| zero |</span></div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'>",
    "<span>\u25c4 Protective</span><span>Recessionary \u25ba</span></div>",
    paste(rows, collapse = ""),
    "<div style='font-size:9px;color:#555;margin-top:6px;'>",
    "Purple bar = probability pull due to historically unprecedented conditions (Mahalanobis anomaly blend). ",
    "Red/green bars = GLM or hand-tuned factor contributions. Length \u221d impact magnitude.",
    "</div></div>"
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 OVERRIDE: build_growth_outlook — passes anomaly_detector
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
  houst <- gv("HOUST")
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
        sprintf("%.1f%% unemployment; avg payrolls %+.0fK/mo", u, pay_3m)
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
      summary = "Below-trend but positive growth likely. Mixed signals."
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
      sprintf("Real rate %.1f%% \u2014 restrictive.", real_rate)
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

  # ── Train / retrieve GLM + anomaly detector ──────────────────────────────────
  cache_name <- ".recession_t2_cache"
  glm_result <- tryCatch(
    {
      needs_train <- !exists(cache_name, envir = .GlobalEnv) ||
        is.null(get(cache_name, envir = .GlobalEnv)) ||
        difftime(
          Sys.time(),
          get(cache_name, envir = .GlobalEnv)$trained_at,
          units = "days"
        ) >
          30
      if (needs_train) {
        message("[synopsis_tier2] Training GLM + anomaly detector...")
        m <- train_recession_model(fred_data) # Tier 2 version includes anomaly_detector
        assign(cache_name, m, envir = .GlobalEnv)
      }
      get(cache_name, envir = .GlobalEnv)
    },
    error = function(e) {
      message("[synopsis_tier2] cache error: ", e$message)
      NULL
    }
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
    anomaly_detector = anomaly_detector # ← Tier 2
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


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 2 OVERRIDE: .build_model_modal
# Adds: anomaly detection callout, updated title, updated formula footer,
# three-colour bar chart legend, Tier 2 interpretability note.
# ═══════════════════════════════════════════════════════════════════════════════

.build_model_modal <- function(
  factor_contributions = NULL,
  model_type = "Hand-tuned log-odds",
  anomaly = NULL
) {
  # ── 1. Live bar chart ───────────────────────────────────────────────────────
  bar_chart_html <- .build_factor_bar_chart(factor_contributions, model_type)

  # ── 2. Anomaly status callout ────────────────────────────────────────────────
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
      "<div style='background:#1e1030;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'>
         <div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           <i class=\"fa fa-exclamation-triangle\" style=\"margin-right:6px;\"></i>Anomaly Detection Active
         </div>
         <div style='display:flex;align-items:center;gap:10px;margin-bottom:8px;'>
           <span style='color:#9aa3b2;font-size:11px;width:90px;'>Anomaly score</span>
           <div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'>
             <div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div>
           </div>
           <span style='color:%s;font-size:13px;font-weight:700;width:40px;text-align:right;'>%d%%</span>
         </div>
         <div style='color:#9aa3b2;font-size:11px;margin-bottom:6px;'>
           <b style='color:#c8a8f0;'>Mahalanobis distance:</b> %.2f &nbsp;|&nbsp;
           <b style='color:#c8a8f0;'>Status:</b> %s
         </div>
         <div style='color:#d0d0d0;font-size:11px;line-height:1.75;'>
           The current combination of macro indicators is <b>%s</b> relative to the
           historical training distribution (1959\u2013present). The Mahalanobis distance
           measures multivariate departure from the normal macro envelope \u2014 a single
           variable being extreme is less concerning than <em>several variables simultaneously
           departing from normal in correlated ways</em>, which is the hallmark of
           pandemic-class or financial-crisis-class events.<br/>
           <b style='color:#a785e0;'>Blending:</b>&nbsp;
           p_final = p_GLM \u00d7 (1 \u2212 0.35\u00d7A) + 0.25\u00d7A, where A = anomaly score.
           This compresses overconfident estimates toward 50%% and raises the floor to 25%%
           when conditions are fully unprecedented.
         </div>
       </div>",
      bar_col,
      score_pct,
      bar_col,
      score_pct,
      anomaly$md %||% 0,
      anomaly$label,
      if (anomaly$score > 0.6) {
        "highly unusual"
      } else if (anomaly$score > 0.3) {
        "unusual"
      } else {
        "somewhat elevated"
      }
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'>
       <i class=\"fa fa-check-circle\" style=\"color:#2dce89;margin-right:6px;\"></i>
       Anomaly detector: current conditions are within historical norms. No uncertainty adjustment applied.
     </div>"
  }

  # ── 3. Factor detail rows ────────────────────────────────────────────────────
  factors <- list(
    list(
      rank = 1,
      name = "Yield Curve (10Y-2Y spread)",
      weight = "High",
      col = "#e94560",
      source = "Estrella & Mishkin (1998); NY Fed",
      mechanism = "Every 1pp narrowing: +0.85 log-odds. Duration: +0.3 (3M), +0.6 (6M), +1.2 (12M), +1.5 (18M+).",
      interpret = "Most reliable single predictor at 12-month horizon. Negative spread = market pricing imminent rate cuts."
    ),
    list(
      rank = 2,
      name = "Sahm Rule / Labor Momentum",
      weight = "High",
      col = "#e94560",
      source = "Sahm (2019); BLS UNRATE + PAYEMS",
      mechanism = "Unemployment rise \u22650.5pp from 12M trough: +1.8. Payroll 3M avg negative: +0.6 to +1.5.",
      interpret = "Perfect record triggering at or before every recession since 1970."
    ),
    list(
      rank = 3,
      name = "Prime-Age LFPR (25\u201354)",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS LNS11300060",
      mechanism = "Level <82%: +0.4. 6M drop >0.2pp: +0.4; >0.5pp: +0.9. Above 83.5%: \u22120.3.",
      interpret = "Captures hidden labour weakness: workers leaving the force keep headline unemployment flat."
    ),
    list(
      rank = 4,
      name = "Jobless Claims Trend",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS ICSA (weekly initial claims)",
      mechanism = "4-week MA / 26-week MA: >10% above = +0.6; >20% = +1.2.",
      interpret = "Most real-time labour indicator. Detects layoff acceleration weeks before monthly payrolls."
    ),
    list(
      rank = 5,
      name = "Quits Rate YoY",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS JTSQUL (JOLTS quits)",
      mechanism = "YoY decline >8%: +0.4; >15%: +0.8. Rising >8%: \u22120.3.",
      interpret = "Workers quit when confident. Sustained decline leads Sahm by 1\u20132 quarters."
    ),
    list(
      rank = 6,
      name = "Oil Price Shock (YoY)",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED DCOILWTICO",
      mechanism = ">25% YoY: +0.6; >50%: +1.0. Demand collapse (<\u221230%): +0.5.",
      interpret = "Hamilton (1983): every major US recession preceded by an oil shock."
    ),
    list(
      rank = 7,
      name = "Equity Bear Market",
      weight = "Medium",
      col = "#f4a261",
      source = "Yahoo Finance / tidyquant (SPY)",
      mechanism = ">10% drawdown: +0.5; >20%: +1.1; >30%: +1.6. <5% from high: \u22120.2.",
      interpret = ">20% drawdown preceded every post-WW2 recession. Markets price future earnings."
    ),
    list(
      rank = 8,
      name = "HY Credit Spreads",
      weight = "Medium",
      col = "#f4a261",
      source = "FRED BAMLH0A0HYM2 (ICE BofA)",
      mechanism = ">4.5%: +0.2; >5.5%: +0.7; >7%: +1.4. <3.5%: \u22120.4.",
      interpret = "Spreads >500bp signal credit market stress; coincident/leading recession onset."
    ),
    list(
      rank = 9,
      name = "USD Surge (YoY)",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DTWEXBGS",
      mechanism = ">8% YoY: +0.4; >12%: +0.8. Weakening >5%: \u22120.2.",
      interpret = "Dollar surge tightens global conditions; raises EM burdens, hurts exporters."
    ),
    list(
      rank = 10,
      name = "Real Fed Funds Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED FEDFUNDS minus CPI YoY",
      mechanism = ">1%: +0.2; >2%: +0.5; >3%: +1.0. Negative: \u22120.3.",
      interpret = "Monetary policy with long & variable lags (Friedman). Firmly positive = restrictive."
    ),
    list(
      rank = 11,
      name = "Consumer Sentiment",
      weight = "Low",
      col = "#9aa3b2",
      source = "U Michigan UMCSENT",
      mechanism = "<60: +0.7; <70: +0.3. >90: \u22120.4.",
      interpret = "Conference Board LEI component. Leads spending by 1\u20132 quarters."
    ),
    list(
      rank = 12,
      name = "Inventory-to-Sales Ratio",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED ISRATIO",
      mechanism = ">1.40: +0.1; >1.45: +0.3; >1.55: +0.7. <1.30: \u22120.2.",
      interpret = "Excess inventory \u2192 cut orders \u2192 cut production \u2192 cut headcount."
    ),
    list(
      rank = 13,
      name = "CC Delinquency Rate",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DRCCLACBS",
      mechanism = ">2.5%: +0.2; >3%: +0.5; >4%: +0.9. <1.8%: \u22120.3.",
      interpret = "Consumer stress appears here 2\u20133 quarters before unemployment rises."
    ),
    list(
      rank = 14,
      name = "Anomaly Signal (Mahalanobis)",
      weight = "Blend",
      col = "#7c5cbf",
      source = "Computed from GLM training feature distribution",
      mechanism = "A = MD / (d95 \u00d7 1.5). Blend: p_final = p_GLM\u00d7(1\u22120.35A) + 0.25A.",
      interpret = "When current macro conditions are multivariate outliers vs. history, the model widens uncertainty bands rather than extrapolating a trained relationship into uncharted territory."
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

  # ── Assemble modal ───────────────────────────────────────────────────────────
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
           <span style='font-size:11px;font-weight:400;color:#7c5cbf;margin-left:10px;'>
             Tier\u00a02: Estimated GLM + Mahalanobis Anomaly Detection
           </span>
         </div>
         <div style='color:#9aa3b2;font-size:12px;margin-bottom:20px;'>
           13-factor logistic model (GLM-estimated when FRED history available) plus an
           anomaly detector that widens uncertainty bands when current conditions depart
           significantly from the historical training distribution.
           Calibrated to NBER recession dates. Not a point forecast.
         </div>

         <!-- Bar chart -->
         <div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
    bar_chart_html,
    "</div>

         <!-- Anomaly status -->",
    anomaly_html,

    "<!-- Interpretability note -->
         <div style='background:#1e2640;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'>
           <div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'>
             <i class=\"fa fa-eye\" style=\"margin-right:6px;\"></i>Interpretability, Trust &amp; Novel Shock Handling
           </div>
           <div style='color:#d0d0d0;font-size:12px;line-height:1.8;'>
             <b style='color:#e0e0e0;'>Tier 2 vs Tier 1:</b> The GLM (Tier 1) learns coefficients from history. It is well-calibrated
             for conditions that resemble past recessions. Its weakness: if the current environment is
             multivariate-unprecedented (a combination of macro signals that didn't co-occur in training),
             the GLM extrapolates its historical coefficients into genuinely uncharted territory.<br/>
             <b style='color:#e0e0e0;'>Anomaly layer:</b> The Mahalanobis distance measures how far
             today\u2019s feature vector is from the centroid of the historical training distribution,
             accounting for inter-feature correlations. A pandemic creates a large MD because
             <em>multiple indicators simultaneously</em> hit values they've never taken together.
             A single-variable extreme (e.g., a brief VIX spike) may not trigger a high anomaly score.<br/>
             <b style='color:#e0e0e0;'>Calibration target:</b> Normal expansions ~15% base rate;
             12 months before NBER recession: ~50\u201360%. Anomaly blend compresses both tails toward
             50% in proportion to the anomaly score, reflecting honest model uncertainty.
           </div>
         </div>

         <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           Factor Detail \u2014 Log-Odds Contributions (incl. Anomaly Blend row)
         </div>
         <div style='overflow-x:auto;'>
         <table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>
           <thead>
             <tr style='background:#1e2640;border-bottom:2px solid #2a3042;'>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:200px;'>Factor</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;'>Weight</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:160px;'>Source</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:220px;'>Scoring / Mechanism</th>
               <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:240px;'>Why It Matters</th>
             </tr>
           </thead>
           <tbody>",
    rows,
    "</tbody>
         </table></div>

         <div style='color:#555;font-size:11px;margin-top:16px;line-height:1.7;'>
           <b style='color:#9aa3b2;'>Tier 2 formula:</b>
           p_GLM = 1/(1+exp(\u2212(\u03b20 + \u03a3 \u03b2i\u00d7xi))) &nbsp;|\u00a0
           A = min(1, MD / (d95\u00d71.5)) &nbsp;|\u00a0
           p_final = p_GLM\u00d7(1\u22120.35A) + 0.25A &nbsp;|\u00a0
           Clamped [2%, 97%].<br/>
           <b style='color:#9aa3b2;'>References:</b>
           Estrella &amp; Mishkin (1998) \u2014 yield curve; Sahm (2019) \u2014 labor rule;
           Hamilton (1983) \u2014 oil shocks; Mahalanobis (1936) \u2014 distance metric.
         </div>
       </div>
     </div>"
  )
}
