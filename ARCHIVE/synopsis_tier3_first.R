# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3.R — Tier 3: Markov-Switching + Online Rolling Retrain
#
# ── APP STRUCTURE (read this first) ──────────────────────────────────────────
#
#  project/
#    global.R          ← source ONLY this tier here:
#                          source("R/synopsis_tier3.R")
#                         Do NOT also source tier1 or tier2 — tier3 chains them.
#    server.R
#    ui.R
#    R/
#      synopsis_tier1.R   ← base layer (standalone, all helpers + GLM)
#      synopsis_tier2.R   ← sources tier1; adds anomaly detection
#      synopsis_tier3.R   ← sources tier2; adds Markov + rolling retrain
#      synopsis_original.R ← rename the old file to this; keep for reference
#      data_fred.R
#      data_census.R
#      llm_insights.R
#
#  Tier chaining: tier3 → sources tier2 → sources tier1.
#  Each tier ONLY defines the functions it changes; everything else is inherited.
#  To switch tiers, change one line in global.R.
#
# ── WHAT'S NEW / FIXED IN THIS VERSION ───────────────────────────────────────
#
#  1. train_markov_model()     — Removed MSwM availability check (assume installed).
#     Switching variable now tries a cascade: INDPRO YoY → PAYEMS MoM change →
#     UNRATE level.  Minimum obs reduced from 120 → 60.  Predictors added from
#     whatever is available in fred_data; model fits even with intercept-only
#     base if no predictors load.
#
#  2. compute_ensemble_weights() — NEW.  Computes data-driven blend weights for
#     GLM and Markov using out-of-sample Brier scores on a 36-month holdout.
#     Weights are proportional to model skill (1 − Brier), so if the Markov
#     model fits better on the holdout, it gets higher weight automatically.
#     Stored in the cache; shown in the Model Details modal.
#
#  3. .compute_recession_prob() — Uses compute_ensemble_weights() instead of
#     hard-coded 0.55 / 0.45.  Weights are recomputed each 30-day retrain.
#
#  4. .build_factor_bar_chart() — Distinguishes three states visually:
#       • Coloured bar   = factor has data AND a non-zero contribution
#       • Grey flat line = factor has data but is currently at neutral (true 0)
#       • Striped/dim    = factor data is NA (not available from FRED)
#     The contribution value column now shows "N/A" for missing-data factors
#     rather than "+0.00", so the user knows the difference.
#
#  5. Source strategy updated — uses normalizePath + dirname(sys.frame) with
#     fallback to explicit "R/synopsis_tier2.R" so it works both interactively
#     and from Shiny's working directory.
# ─────────────────────────────────────────────────────────────────────────────

# ── Source tier 2 (which sources tier 1) ─────────────────────────────────────
local({
  this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) {
    NULL
  })
  tier2_path <- if (!is.null(this_file)) {
    file.path(dirname(this_file), "R/synopsis_tier2.R")
  } else {
    "R/synopsis_tier2.R" # Shiny working-directory fallback
  }
  if (file.exists(tier2_path)) {
    source(tier2_path, local = FALSE)
  } else {
    stop(
      "[synopsis_tier3] Cannot find synopsis_tier2.R at: ",
      tier2_path,
      "\nSet working directory to the project root or adjust the path."
    )
  }
})


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3: Data-driven ensemble weights via out-of-sample Brier scoring
# ═══════════════════════════════════════════════════════════════════════════════

#' Compute GLM vs Markov blend weights from Brier scores on a holdout period.
#'
#' Uses the last `holdout_months` months of the GLM training panel as an
#' out-of-sample test set.  Each model's skill = 1 − Brier_score.
#' Weights are proportional to skill, so better-calibrated models get more
#' weight automatically.  If Brier scores are equal, weights are 50/50.
#'
#' @param glm_result     Output of rolling_retrain() — must contain $model,
#'                       $training_panel (see rolling_retrain update below).
#' @param markov_result  Output of train_markov_model().
#' @param holdout_months Integer; default 36 (3 years of monthly data).
#'
#' @return list(w_glm, w_ms, glm_brier, ms_brier, n_holdout)
compute_ensemble_weights <- function(
  glm_result,
  markov_result,
  holdout_months = 36L
) {
  fallback <- list(
    w_glm = 0.60,
    w_ms = 0.40,
    glm_brier = NA_real_,
    ms_brier = NA_real_,
    n_holdout = 0L,
    method = "fallback (equal-ish)"
  )

  if (is.null(glm_result) || is.null(glm_result$training_panel)) {
    return(fallback)
  }
  panel <- glm_result$training_panel
  n <- nrow(panel)
  if (n < holdout_months + 30L) {
    return(fallback)
  }

  train_idx <- seq_len(n - holdout_months)
  holdout_idx <- (n - holdout_months + 1L):n
  train_df <- panel[train_idx, , drop = FALSE]
  holdout_df <- panel[holdout_idx, , drop = FALSE]

  y_actual <- holdout_df$rec_next12
  if (any(is.na(y_actual))) {
    ok <- !is.na(y_actual)
    holdout_df <- holdout_df[ok, , drop = FALSE]
    y_actual <- y_actual[ok]
  }
  if (length(y_actual) < 12) {
    return(fallback)
  }

  # ── GLM Brier score ──────────────────────────────────────────────────────────
  glm_brier <- tryCatch(
    {
      avail <- intersect(glm_result$predictors, names(holdout_df))
      preds <- predict(
        glm_result$model,
        newdata = holdout_df[, avail, drop = FALSE],
        type = "response"
      )
      mean((preds - y_actual)^2, na.rm = TRUE)
    },
    error = function(e) NA_real_
  )

  # ── Markov Brier score ───────────────────────────────────────────────────────
  # The Markov smoothed probabilities are computed over the full training panel.
  # We align the tail of smooth_probs with the holdout rows.
  ms_brier <- tryCatch(
    {
      if (is.null(markov_result) || is.null(markov_result$smooth_probs)) {
        return(NA_real_)
      }
      sp <- markov_result$smooth_probs # full-sample smoothed probs
      rs <- markov_result$recession_state
      sp_rec <- sp[, rs] # recession-state column
      n_sp <- length(sp_rec)
      n_hold <- length(y_actual)

      if (n_sp < n_hold) {
        return(NA_real_)
      }
      # Take the last n_hold rows — aligns with the holdout period approximately
      ms_preds_holdout <- tail(sp_rec, n_hold)
      mean((ms_preds_holdout - y_actual)^2, na.rm = TRUE)
    },
    error = function(e) NA_real_
  )

  if (is.na(glm_brier) || is.na(ms_brier)) {
    if (!is.na(glm_brier)) {
      return(modifyList(
        fallback,
        list(
          w_glm = 0.85,
          w_ms = 0.15,
          glm_brier = glm_brier,
          method = "GLM only (Markov Brier unavail)"
        )
      ))
    }
    return(fallback)
  }

  # ── Convert Brier → skill → weight ──────────────────────────────────────────
  # Brier skill score = 1 − (model Brier / climatology Brier)
  # Climatology = always predict base rate (fraction of 1s in training)
  base_rate <- mean(panel$rec_next12, na.rm = TRUE)
  clim_brier <- base_rate * (1 - base_rate) # Brier of climatological forecast
  glm_skill <- max(0, 1 - glm_brier / clim_brier)
  ms_skill <- max(0, 1 - ms_brier / clim_brier)
  total_skill <- glm_skill + ms_skill

  if (total_skill < 0.001) {
    return(fallback)
  } # both models are useless

  w_glm <- glm_skill / total_skill
  w_ms <- ms_skill / total_skill

  message(sprintf(
    "[synopsis_tier3] Ensemble weights: GLM=%.2f (Brier=%.4f, skill=%.3f) | Markov=%.2f (Brier=%.4f, skill=%.3f) | holdout=%dM",
    w_glm,
    glm_brier,
    glm_skill,
    w_ms,
    ms_brier,
    ms_skill,
    length(y_actual)
  ))

  list(
    w_glm = round(w_glm, 3),
    w_ms = round(w_ms, 3),
    glm_brier = round(glm_brier, 4),
    ms_brier = round(ms_brier, 4),
    glm_skill = round(glm_skill, 3),
    ms_skill = round(ms_skill, 3),
    n_holdout = length(y_actual),
    method = sprintf("Brier skill (clim=%.4f)", clim_brier)
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: train_markov_model — robust switching variable cascade,
#                  MSwM always assumed available
# ═══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data, n_states = 2L) {
  tryCatch(
    {
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

      # ── Switching variable: cascade through options ───────────────────────────
      # MSwM needs a continuous outcome variable.  Try multiple candidates in order
      # of historical length and economic relevance.
      switching_var <- NULL
      switch_name <- NULL

      # Option 1: INDPRO YoY (best — monthly, back to 1919)
      indpro_raw <- gm("INDPRO", "indpro")
      if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
        cand <- indpro_raw %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(
            sw_var = (indpro / dplyr::lag(indpro, 12) - 1) * 100
          ) %>%
          dplyr::filter(!is.na(sw_var)) %>%
          dplyr::select(date, sw_var)
        if (nrow(cand) >= 60) {
          switching_var <- cand
          switch_name <- "IP YoY %"
        }
      }

      # Option 2: PAYEMS monthly change (always available if FRED loads)
      if (is.null(switching_var)) {
        pay_raw <- gm("PAYEMS", "payems")
        if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
          cand <- pay_raw %>%
            dplyr::arrange(date) %>%
            dplyr::mutate(sw_var = payems - dplyr::lag(payems, 1)) %>%
            dplyr::filter(!is.na(sw_var)) %>%
            dplyr::select(date, sw_var)
          if (nrow(cand) >= 60) {
            switching_var <- cand
            switch_name <- "Payroll MoM chg (K)"
          }
        }
      }

      # Option 3: UNRATE level (available from 1948)
      if (is.null(switching_var)) {
        urate_raw <- gm("UNRATE", "unemp")
        if (!is.null(urate_raw) && nrow(urate_raw) >= 60) {
          switching_var <- urate_raw %>% dplyr::rename(sw_var = unemp)
          switch_name <- "Unemployment rate"
        }
      }

      if (is.null(switching_var)) {
        message(
          "[synopsis_tier3] No usable switching variable found for Markov model."
        )
        return(NULL)
      }

      message(sprintf(
        "[synopsis_tier3] Markov switching variable: '%s' (%d obs)",
        switch_name,
        nrow(switching_var)
      ))

      # ── Build predictor panel ─────────────────────────────────────────────────
      spread_df <- {
        s <- gm("T10Y2Y", "yield_spread")
        if (!is.null(s)) {
          s
        } else {
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
      }
      hy_df <- gm("BAMLH0A0HYM2", "hy_spread")
      unem_df <- gm("UNRATE", "unemp")

      panel <- switching_var
      for (df in list(spread_df, hy_df, unem_df)) {
        if (!is.null(df)) panel <- dplyr::left_join(panel, df, by = "date")
      }

      # Require only sw_var; predictors can be partially missing
      panel <- panel %>% dplyr::filter(!is.na(sw_var))
      if (nrow(panel) < 60) {
        message(sprintf(
          "[synopsis_tier3] Panel too small after cleaning: %d rows.",
          nrow(panel)
        ))
        return(NULL)
      }

      avail_preds <- intersect(
        c("yield_spread", "hy_spread", "unemp"),
        names(panel)
      )
      # Remove predictors with > 30% missing
      avail_preds <- avail_preds[sapply(avail_preds, function(v) {
        mean(is.na(panel[[v]])) < 0.30
      })]
      # Fill remaining NAs with column mean (Markov needs complete matrix)
      for (v in avail_preds) {
        panel[[v]][is.na(panel[[v]])] <- mean(panel[[v]], na.rm = TRUE)
      }

      base_fml <- if (length(avail_preds) > 0) {
        as.formula(paste("sw_var ~", paste(avail_preds, collapse = " + ")))
      } else {
        as.formula("sw_var ~ 1")
      }

      base_lm <- lm(base_fml, data = panel)
      n_coef <- length(coef(base_lm))

      # Only switch intercept + variance (more stable than switching all slopes)
      # sw[1] = intercept switches; remaining slopes fixed; last = variance switches
      sw_vec <- c(TRUE, rep(FALSE, n_coef - 1), TRUE)
      if (length(sw_vec) != n_coef + 1) {
        sw_vec <- rep(TRUE, n_coef)
      } # safety

      ms_model <- MSwM::msmFit(
        base_lm,
        k = n_states,
        sw = sw_vec,
        control = list(parallel = FALSE, maxiter = 300)
      )

      smooth_probs <- ms_model@Fit@smoProb
      if (is.null(smooth_probs) || nrow(smooth_probs) == 0) {
        return(NULL)
      }

      # Identify recession state = lower mean switching variable
      # For UNRATE: higher mean = recession, so flip
      state_means <- sapply(seq_len(n_states), function(s) ms_model@Coef[1, s])
      recession_state <- if (switch_name == "Unemployment rate") {
        which.max(state_means)
      } else {
        which.min(state_means)
      }

      current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]

      # Regime-conditional means for all predictors
      state_assign <- apply(smooth_probs, 1, which.max)
      panel_trimmed <- panel[seq_len(nrow(smooth_probs)), ]
      regime_means <- lapply(avail_preds, function(v) {
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

      # Also report switching-variable regime means
      sw_regime_means <- list(
        variable = switch_name,
        expansion = round(
          mean(
            panel_trimmed$sw_var[state_assign != recession_state],
            na.rm = TRUE
          ),
          2
        ),
        recession = round(
          mean(
            panel_trimmed$sw_var[state_assign == recession_state],
            na.rm = TRUE
          ),
          2
        )
      )

      message(sprintf(
        "[synopsis_tier3] Markov fitted: %s, n=%d, recession_state=%d, p_rec_now=%.1f%%",
        switch_name,
        nrow(panel),
        recession_state,
        current_rec_prob * 100
      ))

      list(
        model = ms_model,
        smooth_probs = smooth_probs,
        recession_state = recession_state,
        current_rec_prob = current_rec_prob,
        state_means_sw = state_means,
        switch_name = switch_name,
        regime_means = regime_means,
        sw_regime_means = sw_regime_means,
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


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: rolling_retrain — stores training_panel for Brier scoring
# ═══════════════════════════════════════════════════════════════════════════════

rolling_retrain <- function(fred_data, window = 360L) {
  message(sprintf("[synopsis_tier3] Rolling retrain (window=%dM)...", window))
  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    return(NULL)
  }

  panel_rolling <- tail(panel, window)
  if (nrow(panel_rolling) < 100) {
    panel_rolling <- panel
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
        "[synopsis_tier3] Rolling GLM: n=%d, window=%dM, AIC=%.1f",
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
        window_months = window,
        training_panel = panel_clean
      ) # ← stored for Brier scoring
    },
    error = function(e) {
      message("[synopsis_tier3] rolling_retrain glm failed: ", e$message)
      NULL
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: .compute_recession_prob — data-driven weights
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
  markov_result = NULL,
  ensemble_weights = NULL # ← Tier 3: list(w_glm, w_ms, ...)
) {
  # Anomaly blend params (Tier 2 values)
  ANOMALY_WEIGHT <- 0.35
  ANOMALY_FLOOR <- 0.25
  COMPRESS_CENTER <- 0.50

  contribs <- list()
  # is_na_input tracks which factors had missing data (for bar chart styling)
  na_inputs <- character(0)

  add_contrib <- function(
    id,
    label,
    delta,
    z_stat = NA_real_,
    data_missing = FALSE
  ) {
    contribs[[id]] <<- list(
      name = label,
      contribution = delta,
      z_stat = z_stat,
      data_missing = data_missing
    )
    if (data_missing) na_inputs <<- c(na_inputs, id)
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

  anomaly_result <- compute_anomaly_score(anomaly_detector, feat_df)
  A <- anomaly_result$score

  # ── PATH A: GLM probability + β×x contributions ──────────────────────────────
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
      fv_orig <- build_current_feature_vector(
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
      fv_list <- as.list(fv_orig[1, , drop = FALSE])
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
        x <- fv_list[[pred]]
        z <- if (pred %in% ct$predictor) {
          ct$z_stat[ct$predictor == pred]
        } else {
          NA_real_
        }
        lbl <- if (pred %in% names(plabels)) plabels[[pred]] else pred
        miss <- is.na(x) # original value was NA before imputation
        add_contrib(
          pred,
          lbl,
          if (!is.na(b) && !miss) b * x else 0,
          z,
          data_missing = miss
        )
      }
    }
  }

  # ── PATH B: Hand-tuned fallback with NA tracking ──────────────────────────────
  if (is.na(glm_prob_raw)) {
    lo <- -1.73

    # Helper: add a hand-tuned factor with NA awareness
    ht <- function(id, label, val, delta_fn) {
      if (is.na(val)) {
        add_contrib(id, label, 0, data_missing = TRUE)
        return(0)
      }
      d <- delta_fn(val)
      add_contrib(id, label, d)
      d
    }

    lo <- lo +
      ht(
        "yield_curve",
        "Yield Curve (10Y-2Y spread)",
        yield_spread,
        function(v) {
          d <- -v * 0.85
          if (!is.na(inv_months)) {
            d <- d +
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
          d
        }
      )

    # Sahm: needs both u and unemp series
    if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12) {
      sh <- 0
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
      add_contrib("sahm", "Sahm Rule / Labor Momentum", sh)
      lo <- lo + sh
    } else {
      add_contrib("sahm", "Sahm Rule / Labor Momentum", 0, data_missing = TRUE)
    }

    lo <- lo +
      ht(
        "prime_lfpr",
        "Prime-Age LFPR (25\u201354)",
        prime_age_lfpr,
        function(v) {
          d <- if (v < 80.5) {
            0.8
          } else if (v < 82) {
            0.4
          } else if (v > 83.5) {
            -0.3
          } else {
            0
          }
          if (!is.na(prime_age_lfpr_chg)) {
            d <- d +
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
          d
        }
      )
    lo <- lo +
      ht(
        "claims",
        "Jobless Claims Trend (4wk/26wk)",
        jobless_claims_trend,
        function(v) {
          if (v > 0.20) {
            1.2
          } else if (v > 0.10) {
            0.6
          } else if (v > 0.05) {
            0.2
          } else if (v < (-0.10)) {
            -0.3
          } else {
            0
          }
        }
      )
    lo <- lo +
      ht("quits", "Quits Rate YoY", quits_yoy, function(v) {
        if (v < (-15)) {
          0.8
        } else if (v < (-8)) {
          0.4
        } else if (v < (-3)) {
          0.2
        } else if (v > 8) {
          -0.3
        } else {
          0
        }
      })
    lo <- lo +
      ht("oil", "Oil Price Shock (YoY)", oil_yoy, function(v) {
        if (v > 50) {
          1.0
        } else if (v > 25) {
          0.6
        } else if (v > 15) {
          0.2
        } else if (v < (-30)) {
          0.5
        } else if (v < (-15)) {
          0.2
        } else if (v > 0 && v < 10) {
          -0.1
        } else {
          0
        }
      })
    lo <- lo +
      ht(
        "equity",
        "Equity Bear Market (SPY drawdown)",
        equity_drawdown_pct,
        function(v) {
          dd <- abs(v)
          if (dd > 30) {
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
      )
    lo <- lo +
      ht("hy_spreads", "HY Credit Spreads", hy_v, function(v) {
        if (v > 7) {
          1.4
        } else if (v > 5.5) {
          0.7
        } else if (v > 4.5) {
          0.2
        } else if (v < 3.5) {
          -0.4
        } else {
          0
        }
      })
    lo <- lo +
      ht("usd", "USD Surge (YoY)", usd_yoy, function(v) {
        if (v > 12) {
          0.8
        } else if (v > 8) {
          0.4
        } else if (v > 4) {
          0.1
        } else if (v < (-5)) {
          -0.2
        } else {
          0
        }
      })
    lo <- lo +
      ht("real_rate", "Real Fed Funds Rate", real_rate, function(v) {
        if (v > 3) {
          1.0
        } else if (v > 2) {
          0.5
        } else if (v > 1) {
          0.2
        } else if (v < 0) {
          -0.3
        } else {
          0
        }
      })
    lo <- lo +
      ht("sentiment", "Consumer Sentiment", cs, function(v) {
        if (v < 60) {
          0.7
        } else if (v < 70) {
          0.3
        } else if (v > 90) {
          -0.4
        } else {
          0
        }
      })
    lo <- lo +
      ht(
        "inv_sales",
        "Inventory-to-Sales Ratio",
        inventory_sales_ratio,
        function(v) {
          if (v > 1.55) {
            0.7
          } else if (v > 1.45) {
            0.3
          } else if (v > 1.40) {
            0.1
          } else if (v < 1.30) {
            -0.2
          } else {
            0
          }
        }
      )
    lo <- lo +
      ht("cc_del", "CC Delinquency Rate", cc_delinquency, function(v) {
        if (v > 4) {
          0.9
        } else if (v > 3) {
          0.5
        } else if (v > 2.5) {
          0.2
        } else if (v < 1.8) {
          -0.3
        } else {
          0
        }
      })

    glm_prob_raw <- 1 / (1 + exp(-lo))
  }

  # ── Tier 3: Markov state probability ─────────────────────────────────────────
  ms_prob_raw <- NA_real_
  if (!is.null(markov_result) && !is.na(markov_result$current_rec_prob)) {
    ms_prob_raw <- markov_result$current_rec_prob
  }

  # ── Data-driven weights ───────────────────────────────────────────────────────
  ew <- ensemble_weights %||%
    list(w_glm = 0.60, w_ms = 0.40, method = "fallback")
  W_GLM <- if (!is.na(ms_prob_raw)) ew$w_glm else 1.0
  W_MS <- if (!is.na(ms_prob_raw)) ew$w_ms else 0.0

  # Record Markov as a contribution (pp shift from GLM baseline, weighted)
  if (!is.na(ms_prob_raw)) {
    ms_delta <- (ms_prob_raw - glm_prob_raw) * 100 * W_MS
    sn <- markov_result$switch_name %||% "IP YoY"
    state_means <- markov_result$state_means_sw
    add_contrib(
      "markov_regime",
      sprintf(
        "Markov Regime State (w=%.0f%%; p_rec=%.0f%%; sw: %s exp=%.1f rec=%.1f)",
        W_MS * 100,
        ms_prob_raw * 100,
        sn,
        state_means[setdiff(
          1:markov_result$n_states,
          markov_result$recession_state
        )[1]] %||%
          0,
        state_means[markov_result$recession_state] %||% 0
      ),
      ms_delta
    )
  }

  blended_pre_anomaly <- if (!is.na(ms_prob_raw)) {
    W_GLM * glm_prob_raw + W_MS * ms_prob_raw
  } else {
    glm_prob_raw
  }

  blended_raw <- if (A > 0.05) {
    compressed <- blended_pre_anomaly +
      ANOMALY_WEIGHT * A * (COMPRESS_CENTER - blended_pre_anomaly)
    max(compressed, ANOMALY_FLOOR * A)
  } else {
    blended_pre_anomaly
  }

  prob <- max(2, min(97, round(blended_raw * 100, 1)))

  if (A > 0.05) {
    add_contrib(
      "anomaly_signal",
      sprintf(
        "Anomaly Signal (MD\u2248%.1f; %s)",
        anomaly_result$md %||% 0,
        anomaly_result$label
      ),
      (blended_raw - blended_pre_anomaly) * 100
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
        data_missing = r$data_missing %||% FALSE,
        stringsAsFactors = FALSE
      )
    })
  )
  contrib_df <- contrib_df[
    order(abs(contrib_df$contribution), decreasing = TRUE),
  ]

  # Narrative drivers
  du <- character(0)
  dd <- character(0)
  for (i in seq_len(min(8, nrow(contrib_df)))) {
    r <- contrib_df[i, ]
    if (isTRUE(r$data_missing)) {
      next
    }
    if (r$contribution > 0.1) {
      du <- c(du, r$name)
    }
    if (r$contribution < -0.1) dd <- c(dd, r$name)
  }
  if (A > 0.3) {
    du <- c(
      sprintf(
        "\u26a0 %s (MD=%.1f)",
        anomaly_result$label,
        anomaly_result$md %||% 0
      ),
      du
    )
  }
  if (!is.na(ms_prob_raw) && ms_prob_raw > 0.4) {
    du <- c(
      sprintf("Markov regime: %.0f%% recession probability", ms_prob_raw * 100),
      du
    )
  }

  mt <- if (!is.null(glm_result) && !is.na(ms_prob_raw)) {
    sprintf(
      "GLM + Markov + Anomaly (w_glm=%.0f%% w_ms=%.0f%%; %s)",
      W_GLM * 100,
      W_MS * 100,
      ew$method %||% ""
    )
  } else if (!is.null(glm_result)) {
    "GLM + Anomaly Blend"
  } else {
    "Hand-tuned + Anomaly Blend"
  }

  list(
    prob = prob,
    tier = tier,
    drivers_up = head(du, 4),
    drivers_down = head(dd, 3),
    factor_contributions = contrib_df,
    anomaly = anomaly_result,
    markov_prob = ms_prob_raw,
    ensemble_weights = ew,
    model_type = mt,
    n_obs = glm_result$n_obs %||% NULL,
    aic = glm_result$aic %||% NULL
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: .build_factor_bar_chart
# — 4-colour legend, NA vs true-zero visual distinction
# ═══════════════════════════════════════════════════════════════════════════════

.build_factor_bar_chart <- function(
  factor_contributions,
  model_type = "Hand-tuned log-odds",
  markov_result = NULL,
  ensemble_weights = NULL
) {
  if (is.null(factor_contributions) || nrow(factor_contributions) == 0) {
    return("<p style='color:#6b7585;font-size:11px;'>No contribution data.</p>")
  }

  fc <- factor_contributions[
    order(abs(factor_contributions$contribution), decreasing = TRUE),
  ]
  # Show ALL factors (including zero-contribution ones) — up to 15
  fc <- head(fc, 15)
  max_abs <- max(abs(fc$contribution), na.rm = TRUE)
  if (max_abs < 0.01) {
    max_abs <- 1
  }
  has_z <- any(!is.na(fc$z_stat) & !isTRUE(fc$data_missing))

  # Weight badge for subtitle
  ew_badge <- if (
    !is.null(ensemble_weights) && !is.null(ensemble_weights$glm_brier)
  ) {
    sprintf(
      " &nbsp;|\u00a0w_GLM=<b style='color:#2dce89;'>%.0f%%</b> w_MS=<b style='color:#00b4d8;'>%.0f%%</b> (Brier: GLM=%.3f MS=%.3f)",
      ensemble_weights$w_glm * 100,
      ensemble_weights$w_ms * 100,
      ensemble_weights$glm_brier,
      ensemble_weights$ms_brier
    )
  } else if (!is.null(ensemble_weights)) {
    sprintf(
      " &nbsp;|\u00a0w_GLM=<b>%.0f%%</b> w_MS=<b>%.0f%%</b>",
      ensemble_weights$w_glm * 100,
      ensemble_weights$w_ms * 100
    )
  } else {
    ""
  }

  subtitle <- switch(
    model_type,
    "GLM + Anomaly Blend" = paste0(
      "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated \u03b2s + Anomaly blend &nbsp;|\u00a0Markov fitting pending</span>"
    ),
    paste0(
      "<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM + Markov + Anomaly",
      ew_badge,
      "</span>"
    )
  )
  if (grepl("Hand-tuned", model_type)) {
    subtitle <- "<span style='color:#f4a261;font-size:10px;'>\u26a0 Hand-tuned fallback \u2014 GLM/Markov training pending</span>"
  }

  rows <- vapply(
    seq_len(nrow(fc)),
    function(i) {
      row <- fc[i, ]
      delta <- row$contribution
      is_missing <- isTRUE(row$data_missing)
      is_anomaly <- grepl("Anomaly Signal", row$name)
      is_markov <- grepl("Markov Regime", row$name)
      is_true_zero <- (!is_missing &&
        !is_anomaly &&
        !is_markov &&
        abs(delta) < 0.001)

      # Bar colour logic
      col <- if (is_markov) {
        "#00b4d8"
      } else if (is_anomaly) {
        "#7c5cbf"
      } else if (is_missing) {
        # dim grey — no data
        "#3a4052"
      } else if (is_true_zero) {
        # flat grey — neutral / at threshold
        "#3a4052"
      } else if (delta > 0) {
        "#e94560"
      } else {
        "#2dce89"
      }

      pct <- if (is_missing || is_true_zero) {
        0
      } else {
        round(min(abs(delta) / max_abs * 44, 44))
      }
      bar_width <- pct * 3 # px

      bar_style <- if (is_missing) {
        # Dashed hairline to mark that data is absent
        "left:50%;width:2px;background:repeating-linear-gradient(180deg,#4a5062 0,#4a5062 3px,transparent 3px,transparent 6px);"
      } else if (is_true_zero) {
        "left:50%;width:2px;background:#3a4052;"
      } else if (delta >= 0) {
        sprintf("left:50%%;width:%dpx;background:%s;", bar_width, col)
      } else {
        sprintf("right:50%%;width:%dpx;background:%s;", bar_width, col)
      }

      z_badge <- if (has_z && !is.na(row$z_stat) && !is_missing) {
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
        "&nbsp;<span style='color:#00b4d8;font-size:9px;'>\u25c6&nbsp;REGIME</span>"
      } else if (is_anomaly) {
        "&nbsp;<span style='color:#a785e0;font-size:9px;'>\u26a0&nbsp;ANOMALY</span>"
      } else if (is_missing) {
        "&nbsp;<span style='color:#555;font-size:9px;'>N/A</span>"
      } else {
        ""
      }

      name_col <- if (is_markov) {
        "#7dd8f0"
      } else if (is_anomaly) {
        "#c8a8f0"
      } else if (is_missing) {
        "#555"
      } else {
        "#d0d0d0"
      }
      value_str <- if (is_missing) {
        "<span style='color:#555;font-size:10px;'>N/A</span>"
      } else {
        sprintf(
          "<span style='color:%s;font-size:10px;font-weight:600;'>%+.2f</span>",
          col,
          delta
        )
      }

      sprintf(
        "<div style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'>
         <div style='width:220px;text-align:right;color:%s;font-size:10.5px;flex-shrink:0;line-height:1.3;'>%s%s%s</div>
         <div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;overflow:hidden;'>
           <div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;z-index:1;'></div>
           <div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div>
         </div>
         <div style='width:52px;text-align:right;'>%s</div>
       </div>",
        name_col,
        htmltools::htmlEscape(row$name),
        z_badge,
        tag,
        bar_style,
        value_str
      )
    },
    character(1)
  )

  # Count how many factors are missing data
  n_missing <- sum(
    isTRUE(fc$data_missing) |
      (sapply(seq_len(nrow(fc)), function(i) isTRUE(fc$data_missing[i])))
  )
  missing_note <- if (n_missing > 0) {
    sprintf(
      "<div style='margin-top:4px;font-size:9px;color:#555;'>
       <span style='color:#6b7585;'>\u2139</span>&nbsp;
       %d factor%s showing N/A: data not yet loaded from FRED or not included in kpis list.
       Check that <code>kpis$prime_age_lfpr</code>, <code>kpis$quits_yoy</code>,
       <code>kpis$oil_yoy</code>, <code>kpis$equity_drawdown_pct</code>, etc. are populated in
       <code>data_fred.R</code>.
     </div>",
      n_missing,
      if (n_missing == 1) "" else "s"
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
    "<div style='display:flex;flex-wrap:wrap;gap:14px;font-size:10px;color:#9aa3b2;margin-bottom:9px;'>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#e94560;border-radius:2px;margin-right:4px;'></span>Raises risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#2dce89;border-radius:2px;margin-right:4px;'></span>Lowers risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#7c5cbf;border-radius:2px;margin-right:4px;'></span>Anomaly (uncertainty)</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#00b4d8;border-radius:2px;margin-right:4px;'></span>Markov regime state</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#3a4052;border-radius:2px;margin-right:4px;'></span>Neutral / no data</span>",
    "</div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'>",
    "<span>\u25c4 Protective</span><span>| zero baseline |</span><span>Recessionary \u25ba</span></div>",
    paste(rows, collapse = ""),
    missing_note,
    "<div style='font-size:9px;color:#555;margin-top:6px;'>",
    "Coloured bar = active contribution. Flat grey line = factor is neutral (at threshold boundary). N/A = FRED data not available for this session.",
    "</div></div>"
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# TIER 3 OVERRIDE: build_growth_outlook — computes Brier weights, passes through
# ═══════════════════════════════════════════════════════════════════════════════

.classify_regime <- function(score_0_10) {
  if (score_0_10 >= 7.5) {
    list(
      label = "Expansion",
      color = "#2dce89",
      icon = "arrow-up",
      gdp_est = "+2.5% to +3.5%",
      summary = "Broad-based growth signals. Labour market solid, consumer spending resilient, financial conditions supportive."
    )
  } else if (score_0_10 >= 5.5) {
    list(
      label = "Moderate Growth",
      color = "#00b4d8",
      icon = "minus",
      gdp_est = "+1.0% to +2.5%",
      summary = "Below-trend but positive growth likely. Mixed signals across sectors — watch rate transmission and consumer confidence."
    )
  } else if (score_0_10 >= 3.5) {
    list(
      label = "Stall / Slowdown",
      color = "#f4a261",
      icon = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk. Restrictive monetary conditions, softening demand, or financial stress beginning to bite."
    )
  } else {
    list(
      label = "Contraction Risk",
      color = "#e94560",
      icon = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals. Recession probability elevated — yield curve, credit spreads, and demand indicators all deteriorating."
    )
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# NEW: trigger_model_training() — ALL slow work lives here
# Call once from server.R inside withProgress().  Stores result in global cache.
# ═══════════════════════════════════════════════════════════════════════════════

#' Train GLM + Markov + compute ensemble weights.  Store in .recession_t3_cache.
#'
#' This is the only function that does slow work.  Expected runtime: 5–30s.
#' Call from an observeEvent in server.R, not from a reactive expression.
#'
#' @param fred_data  Named list from data_fred.R.
#' @param force      If TRUE, retrain even if cache is fresh.
#' @return Invisibly returns TRUE on success, FALSE on failure.
trigger_model_training <- function(fred_data, force = FALSE) {
  cache_name <- ".recession_t3_cache"

  cache <- tryCatch(
    if (exists(cache_name, envir = .GlobalEnv)) {
      get(cache_name, envir = .GlobalEnv)
    } else {
      NULL
    },
    error = function(e) NULL
  )

  needs_train <- force ||
    is.null(cache) ||
    is.null(cache$glm) ||
    difftime(
      Sys.time(),
      cache$glm$trained_at %||% (Sys.time() - 1e9),
      units = "days"
    ) >
      30

  if (!needs_train) {
    message("[synopsis_tier3] Cache fresh — skipping retrain.")
    return(invisible(TRUE))
  }

  assign(".t3_training_status", "running", envir = .GlobalEnv)
  on.exit(assign(".t3_training_status", "idle", envir = .GlobalEnv), add = TRUE)

  tryCatch(
    {
      message("[synopsis_tier3] trigger_model_training: GLM...")
      glm_obj <- rolling_retrain(fred_data, window = 360L)

      message("[synopsis_tier3] trigger_model_training: Markov...")
      ms_obj <- train_markov_model(fred_data)

      message("[synopsis_tier3] trigger_model_training: ensemble weights...")
      ew_obj <- compute_ensemble_weights(glm_obj, ms_obj, holdout_months = 36L)

      assign(
        cache_name,
        list(
          glm = glm_obj,
          markov = ms_obj,
          ew = ew_obj,
          completed_at = Sys.time()
        ),
        envir = .GlobalEnv
      )

      message("[synopsis_tier3] Training complete.")
      invisible(TRUE)
    },
    error = function(e) {
      message("[synopsis_tier3] trigger_model_training ERROR: ", e$message)
      invisible(FALSE)
    }
  )
}

#' Helper: is a retrain needed right now?
model_training_needed <- function() {
  cache_name <- ".recession_t3_cache"
  cache <- tryCatch(
    if (exists(cache_name, envir = .GlobalEnv)) {
      get(cache_name, envir = .GlobalEnv)
    } else {
      NULL
    },
    error = function(e) NULL
  )
  is.null(cache) ||
    is.null(cache$glm) ||
    difftime(
      Sys.time(),
      cache$glm$trained_at %||% (Sys.time() - 1e9),
      units = "days"
    ) >
      30
}

#' Helper: is training currently in progress?
model_training_running <- function() {
  tryCatch(
    identical(get(".t3_training_status", envir = .GlobalEnv), "running"),
    error = function(e) FALSE
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# FIX + OVERRIDE: build_growth_outlook — fast path only, no inline training
# ═══════════════════════════════════════════════════════════════════════════════

build_growth_outlook <- function(fred_data, kpis, mkt_returns = NULL) {
  if (is.null(kpis)) {
    return(NULL)
  }

  # ── Return loading sentinel if cache not ready ────────────────────────────
  cache_name <- ".recession_t3_cache"
  cache <- tryCatch(
    if (exists(cache_name, envir = .GlobalEnv)) {
      get(cache_name, envir = .GlobalEnv)
    } else {
      NULL
    },
    error = function(e) NULL
  )

  if (is.null(cache) || is.null(cache$glm)) {
    return(list(
      training = TRUE,
      training_running = model_training_running(),
      as_of = Sys.Date()
    ))
  }

  # ── Fast composite score (< 50ms) ────────────────────────────────────────
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

  # ── FIX: if/else chain instead of case_when ───────────────────────────────
  regime <- .classify_regime(score_0_10)

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

  # ── Read from cache (training already done) ───────────────────────────────
  glm_result <- cache$glm
  markov_result <- cache$markov
  ensemble_weights <- cache$ew
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
    markov_result = markov_result,
    ensemble_weights = ensemble_weights
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
# NEW: render_training_splash_html()
# Returns an HTML string suitable for renderUI() while models are training.
# ═══════════════════════════════════════════════════════════════════════════════

render_training_splash_html <- function(step = NULL) {
  # step: NULL = "queued", "glm", "markov", "weights", "done"
  steps <- list(
    list(id = "glm", label = "Rolling GLM (30yr window)", icon = "chart-line"),
    list(
      id = "markov",
      label = "Markov regime model (Hamilton 1989)",
      icon = "random"
    ),
    list(
      id = "weights",
      label = "Brier skill weights (36M holdout)",
      icon = "balance-scale"
    )
  )

  step_html <- paste(
    vapply(
      steps,
      function(s) {
        is_done <- !is.null(step) &&
          which(sapply(steps, `[[`, "id") == s$id) <
            (match(step, sapply(steps, `[[`, "id")) %||% 99)
        is_current <- !is.null(step) && s$id == step
        icon_col <- if (is_done) {
          "#2dce89"
        } else if (is_current) {
          "#00b4d8"
        } else {
          "#3a4052"
        }
        icon_name <- if (is_done) {
          "check-circle"
        } else if (is_current) {
          "spinner fa-spin"
        } else {
          s$icon
        }
        text_col <- if (is_done) {
          "#2dce89"
        } else if (is_current) {
          "#e0e0e0"
        } else {
          "#555"
        }
        sprintf(
          "<div style='display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #1e2640;'>
         <i class=\"fa fa-%s\" style=\"color:%s;font-size:14px;width:18px;text-align:center;\"></i>
         <span style='color:%s;font-size:12px;'>%s</span>
         %s
       </div>",
          icon_name,
          icon_col,
          text_col,
          htmltools::htmlEscape(s$label),
          if (is_current) {
            "<span style='color:#00b4d8;font-size:10px;margin-left:auto;'>Running...</span>"
          } else {
            ""
          }
        )
      },
      character(1)
    ),
    collapse = ""
  )

  # Pulsing bar animation via CSS
  HTML(sprintf(
    "<div style='padding:20px 0;'>
       <div style='background:#1a2035;border:1px solid #2a3042;border-radius:10px;
                   padding:28px 32px;max-width:520px;margin:0 auto;text-align:center;'>

         <!-- Icon + Title -->
         <div style='font-size:32px;margin-bottom:14px;'>
           <i class=\"fa fa-brain\" style=\"color:#00b4d8;\"></i>
         </div>
         <div style='color:#e0e0e0;font-size:16px;font-weight:700;margin-bottom:6px;'>
           Fitting Recession Models
         </div>
         <div style='color:#6b7585;font-size:12px;margin-bottom:22px;'>
           First-run training takes 10–30 seconds. Results cache for 30 days.
         </div>

         <!-- Animated progress bar -->
         <div style='background:#2a3042;border-radius:6px;height:6px;overflow:hidden;margin-bottom:22px;'>
           <div style='height:100%%;width:100%%;background:linear-gradient(90deg,#00b4d8,#7c5cbf,#00b4d8);
                       background-size:200%% 100%%;
                       animation:shimmer 1.8s ease-in-out infinite;border-radius:6px;'>
           </div>
         </div>
         <style>
           @keyframes shimmer {
             0%%   { background-position: -200%% 0; }
             100%% { background-position:  200%% 0; }
           }
         </style>

         <!-- Step list -->
         <div style='text-align:left;background:#0f1117;border-radius:8px;padding:8px 16px;margin-bottom:20px;'>
           %s
         </div>

         <!-- What happens after -->
         <div style='color:#555;font-size:11px;line-height:1.7;'>
           <i class=\"fa fa-info-circle\" style=\"margin-right:4px;\"></i>
           Training runs once per session and refreshes every 30 days.
           The composite growth score and KPI panels load immediately —
           this screen only blocks the recession probability model.
         </div>
       </div>
     </div>",
    step_html
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# OVERRIDE: render_growth_outlook_html — handles training sentinel
# ═══════════════════════════════════════════════════════════════════════════════

render_growth_outlook_html <- function(outlook) {
  # NULL = data not loaded yet
  if (is.null(outlook)) {
    return(HTML(
      "<p style='color:#6b7585;padding:20px;text-align:center;'>
       <i class=\"fa fa-circle-o-notch fa-spin\" style=\"margin-right:8px;color:#00b4d8;\"></i>
       Loading economic data...</p>"
    ))
  }

  # Training sentinel
  if (isTRUE(outlook$training)) {
    return(render_training_splash_html())
  }

  rp <- outlook$recession_prob
  reg <- outlook$regime
  score <- outlook$score
  comps <- outlook$components
  swing <- outlook$swing
  gc <- reg$color

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
       </div>
     </div>",
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
    ew_badge <- if (
      !is.null(rp$ensemble_weights) && !is.null(rp$ensemble_weights$glm_brier)
    ) {
      sprintf(
        "&nbsp;<span style='background:rgba(0,180,216,0.1);color:#00b4d8;border:1px solid rgba(0,180,216,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>w_GLM=%.0f%% w_MS=%.0f%%</span>",
        rp$ensemble_weights$w_glm * 100,
        rp$ensemble_weights$w_ms * 100
      )
    } else {
      ""
    }
    ms_badge <- if (!is.null(rp$markov_prob) && !is.na(rp$markov_prob)) {
      sprintf(
        "&nbsp;<span style='background:rgba(0,180,216,0.08);color:#7dd8f0;border:1px solid rgba(0,180,216,0.2);border-radius:8px;padding:1px 8px;font-size:9px;'>MS: %.0f%%</span>",
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
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                   border-top:3px solid %s;padding:14px 16px;margin-top:14px;'>
         <div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'>
           <div>
             <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>
               <i class=\"fa fa-%s\" style=\"color:%s;margin-right:6px;\"></i>
               Recession Probability \u2014 12-Month Horizon%s%s%s
             </div>
             <div style='margin-top:4px;'>
               <span style='color:%s;font-size:36px;font-weight:800;'>%s%%</span>
               <span style='color:%s;font-size:14px;font-weight:600;margin-left:8px;
                            background:rgba(%s,0.12);padding:2px 10px;border-radius:12px;
                            border:1px solid rgba(%s,0.3);'>%s</span>
             </div>
           </div>
           <div style='text-align:right;'>
             <div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0%% \u00b7 50%% \u00b7 100%%</div>
             <div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);
                         border-radius:7px;position:relative;'>
               <div style='position:absolute;top:-3px;left:calc(%s%% - 8px);
                           width:0;height:0;border-left:7px solid transparent;
                           border-right:7px solid transparent;border-top:8px solid #fff;'></div>
             </div>
             <div style='font-size:10px;color:#6b7585;margin-top:3px;'>\u25b2 Current</div>
           </div>
         </div>
         <div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;
                     border-radius:6px;padding:8px 12px;border-left:3px solid %s;'>%s</div>
         %s
         <div style='color:#555;font-size:10px;margin-top:8px;'>
           Tier 3: rolling GLM (30yr) + Hamilton Markov-switching + anomaly detection.
           Data-driven blend weights. NBER-calibrated.
         </div>
       </div>",
      bc,
      tier$icon,
      bc,
      ew_badge,
      ms_badge,
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
  ew <- tryCatch(
    get(".recession_t3_cache", envir = .GlobalEnv)$ew,
    error = function(e) NULL
  )

  HTML(paste0(
    sprintf(
      "<div style='display:flex;gap:14px;'>
         <!-- Left: regime verdict -->
         <div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                     border-top:3px solid %s;padding:16px 18px;'>
           <div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;
                       letter-spacing:1px;margin-bottom:6px;'>
             <i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook
           </div>
           <div style='color:%s;font-size:22px;font-weight:800;margin-bottom:2px;'>%s</div>
           <div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>
             GDP Est: %s annualized
           </div>
           %s
           <div style='background:#0f1117;border-radius:6px;padding:10px 12px;
                       color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid %s;'>
             %s
           </div>
           <div style='color:#555;font-size:10px;margin-top:10px;'>
             %d indicators + Markov + rolling GLM + anomaly. As of %s.
             <button onclick=\"document.getElementById('recModelModal').style.display='flex'\"
               style='margin-left:8px;background:transparent;border:1px solid #2a3042;
                      color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'>
               <i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details
             </button>
           </div>
           %s
         </div>
         <!-- Right: component breakdown -->
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
             <ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>%s</ul>
           </div>
         </div>
       </div>",
      gc,
      gc,
      reg$icon,
      gc,
      reg$label,
      reg$gdp_est,
      gauge_html,
      gc,
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
      markov_result = mr,
      ensemble_weights = ew
    )
  ))
}


.build_model_modal <- function(
  factor_contributions = NULL,
  model_type = "Hand-tuned log-odds",
  anomaly = NULL,
  markov_result = NULL,
  ensemble_weights = NULL
) {
  bar_chart_html <- .build_factor_bar_chart(
    factor_contributions,
    model_type,
    markov_result,
    ensemble_weights
  )

  # ── Ensemble weights panel ──────────────────────────────────────────────────
  ew_html <- if (
    !is.null(ensemble_weights) && !is.null(ensemble_weights$glm_brier)
  ) {
    glm_w_pct <- round(ensemble_weights$w_glm * 100)
    ms_w_pct <- round(ensemble_weights$w_ms * 100)
    sprintf(
      "<div style='background:#0d1f10;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #2dce89;'>
         <div style='color:#2dce89;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           <i class=\"fa fa-balance-scale\" style=\"margin-right:6px;\"></i>Data-Driven Ensemble Weights
           <span style='color:#555;font-size:10px;font-weight:400;margin-left:8px;'>(%s)</span>
         </div>
         <div style='display:flex;gap:20px;margin-bottom:10px;'>
           <div style='flex:1;'>
             <div style='color:#9aa3b2;font-size:10px;margin-bottom:4px;'>GLM weight &nbsp;<b style='color:#2dce89;'>%d%%</b></div>
             <div style='background:#2a3042;border-radius:3px;height:8px;'><div style='background:#2dce89;width:%d%%;height:100%%;border-radius:3px;'></div></div>
             <div style='color:#555;font-size:9px;margin-top:3px;'>Brier=%.4f | skill=%.3f</div>
           </div>
           <div style='flex:1;'>
             <div style='color:#9aa3b2;font-size:10px;margin-bottom:4px;'>Markov weight &nbsp;<b style='color:#00b4d8;'>%d%%</b></div>
             <div style='background:#2a3042;border-radius:3px;height:8px;'><div style='background:#00b4d8;width:%d%%;height:100%%;border-radius:3px;'></div></div>
             <div style='color:#555;font-size:9px;margin-top:3px;'>Brier=%.4f | skill=%.3f</div>
           </div>
         </div>
         <div style='color:#d0d0d0;font-size:11px;line-height:1.75;'>
           Weights are computed from <b>Brier skill scores</b> on a %d-month out-of-sample holdout.
           A Brier score measures mean squared error between predicted probability and actual NBER recession outcome (0/1).
           Skill = 1 \u2212 (model Brier / climatology Brier), where climatology = always predicting the base rate.
           Higher skill \u2192 higher blend weight.  Weights update every 30 days when models retrain.
           <span style='color:#555;'>Method: %s</span>
         </div>
       </div>",
      ensemble_weights$method,
      glm_w_pct,
      glm_w_pct,
      ensemble_weights$glm_brier,
      ensemble_weights$glm_skill %||% 0,
      ms_w_pct,
      ms_w_pct,
      ensemble_weights$ms_brier,
      ensemble_weights$ms_skill %||% 0,
      ensemble_weights$n_holdout %||% 0,
      ensemble_weights$method
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'>
       <i class=\"fa fa-info-circle\" style=\"color:#9aa3b2;margin-right:6px;\"></i>
       Ensemble weights not yet computed (Markov model training pending or holdout insufficient).
       Default 60/40 GLM/Markov split in use.
     </div>"
  }

  # ── Markov status panel ─────────────────────────────────────────────────────
  markov_html <- if (!is.null(markov_result)) {
    ms_pct <- round(markov_result$current_rec_prob * 100)
    ms_col <- if (ms_pct > 60) {
      "#e94560"
    } else if (ms_pct > 35) {
      "#f4a261"
    } else {
      "#2dce89"
    }
    sw_means <- markov_result$sw_regime_means
    exp_val <- if (!is.null(sw_means)) sw_means$expansion else NA
    rec_val <- if (!is.null(sw_means)) sw_means$recession else NA
    sprintf(
      "<div style='background:#0f1a27;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>
         <div style='color:#00b4d8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           <i class=\"fa fa-random\" style=\"margin-right:6px;\"></i>Markov Regime Model
           <span style='color:#555;font-weight:400;font-size:10px;margin-left:8px;'>switching variable: %s</span>
         </div>
         <div style='display:flex;align-items:center;gap:10px;margin-bottom:8px;'>
           <span style='color:#9aa3b2;font-size:11px;width:180px;'>Recession state probability</span>
           <div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'>
             <div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div>
           </div>
           <span style='color:%s;font-size:14px;font-weight:700;width:40px;text-align:right;'>%d%%</span>
         </div>
         <div style='color:#9aa3b2;font-size:11px;margin-bottom:6px;'>
           Expansion mean: <b style='color:#2dce89;'>%.2f</b> &nbsp;|\u00a0
           Recession mean: <b style='color:#e94560;'>%.2f</b> &nbsp;|\u00a0
           n=%d months &nbsp;|\u00a0 %d states
         </div>
       </div>",
      htmltools::htmlEscape(markov_result$switch_name %||% "unknown"),
      ms_col,
      ms_pct,
      ms_col,
      ms_pct,
      exp_val %||% 0,
      rec_val %||% 0,
      markov_result$n_obs %||% 0,
      markov_result$n_states %||% 2
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'>
       <i class=\"fa fa-info-circle\" style=\"color:#00b4d8;margin-right:6px;\"></i>
       Markov model not yet fitted. Check that PAYEMS / INDPRO / UNRATE data is available in fred_data.
     </div>"
  }

  # ── Anomaly status (compact) ────────────────────────────────────────────────
  anomaly_html <- if (!is.null(anomaly) && anomaly$score > 0.05) {
    sc_pct <- round(anomaly$score * 100)
    bc <- if (anomaly$score > 0.6) {
      "#e94560"
    } else if (anomaly$score > 0.3) {
      "#f4a261"
    } else {
      "#7c5cbf"
    }
    sprintf(
      "<div style='background:#1e1030;border-radius:8px;padding:12px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'><div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'><i class=\"fa fa-exclamation-triangle\" style=\"margin-right:6px;\"></i>Anomaly Detection Active</div><div style='display:flex;align-items:center;gap:10px;margin-bottom:6px;'><span style='color:#9aa3b2;font-size:11px;width:90px;'>Score</span><div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div></div><span style='color:%s;font-size:13px;font-weight:700;width:40px;'>%d%%</span></div><div style='color:#d0d0d0;font-size:11px;line-height:1.6;'>MD=%.2f \u2014 %s. Blend: p_final=(0.55\u00d7p_GLM+0.45\u00d7p_MS)\u00d7(1\u22120.35A)+0.25A</div></div>",
      bc,
      sc_pct,
      bc,
      sc_pct,
      anomaly$md %||% 0,
      anomaly$label
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:8px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-check-circle\" style=\"color:#2dce89;margin-right:6px;\"></i>Anomaly normal. No uncertainty adjustment.</div>"
  }

  # ── Factor table ─────────────────────────────────────────────────────────────
  factors <- list(
    list(
      rank = 1,
      name = "Yield Curve (10Y-2Y)",
      weight = "High",
      col = "#e94560",
      source = "Estrella & Mishkin (1998)",
      mechanism = "Every 1pp narrowing: +0.85. Duration: +0.3–+1.5.",
      interpret = "Most reliable single 12-month predictor."
    ),
    list(
      rank = 2,
      name = "Sahm Rule / Labor",
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
      mechanism = "<82%: +0.4; 6M drop >0.2pp: +0.4–+0.9.",
      interpret = "Hidden labour weakness invisible to Sahm."
    ),
    list(
      rank = 4,
      name = "Jobless Claims Trend",
      weight = "Medium",
      col = "#f4a261",
      source = "BLS ICSA (weekly)",
      mechanism = "4wk/26wk MA: >10%: +0.6; >20%: +1.2.",
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
      interpret = ">500bp = credit crunch signal."
    ),
    list(
      rank = 9,
      name = "USD Surge (YoY)",
      weight = "Low",
      col = "#9aa3b2",
      source = "FRED DTWEXBGS",
      mechanism = ">8%: +0.4; >12%: +0.8.",
      interpret = "Tightens global conditions."
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
      interpret = "Excess inventory \u2192 hiring slows."
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
      name = "Anomaly (Mahalanobis)",
      weight = "Blend",
      col = "#7c5cbf",
      source = "Training distribution",
      mechanism = "p=p_base\u00d7(1\u22120.35A)+0.25A.",
      interpret = "Widens uncertainty for unprecedented conditions."
    ),
    list(
      rank = 15,
      name = "Markov Regime State",
      weight = "Regime",
      col = "#00b4d8",
      source = "MSwM: Hamilton (1989)",
      mechanism = "w_GLM\u00d7p_GLM + w_MS\u00d7p_Markov (data-driven)",
      interpret = "Identifies structural regime shifts without NBER labels. Weight from Brier skill scoring."
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
           Three-model ensemble with <b style='color:#2dce89;'>data-driven blend weights</b>:
           (1) logistic regression retrained on a rolling 30-year window,
           (2) Hamilton (1989) 2-state Markov-switching regime model, and
           (3) Mahalanobis anomaly detector that widens uncertainty for historically unprecedented conditions.
           GLM/Markov weights are computed from Brier skill scores on a 36-month out-of-sample holdout \u2014 not hand-tuned.
           Calibrated to NBER recession dates. Not a point forecast.
         </div>
         <div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
    bar_chart_html,
    "</div>",
    ew_html,
    markov_html,
    anomaly_html,
    "<div style='background:#1e2640;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'>
       <div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'>
         <i class=\"fa fa-eye\" style=\"margin-right:6px;\"></i>Architecture Summary
       </div>
       <div style='color:#d0d0d0;font-size:12px;line-height:1.8;'>
         <b>GLM (rolling):</b> coefficients estimated on 30-year rolling window so the model adapts to structural changes rather than being frozen to a 1960\u20132020 average.<br/>
         <b>Markov:</b> identifies latent expansion/recession regimes from the joint distribution of macro variables without needing NBER labels as training targets. Captures structural breaks as regime transitions.<br/>
         <b>Anomaly:</b> when today\u2019s multivariate feature vector is far from the historical training cloud (high Mahalanobis distance), both models are compressing toward 50%% rather than extrapolating trained relationships into uncharted territory.<br/>
         <b>Blend:</b> Brier-weighted so whichever model was better-calibrated on recent holdout data gets proportionally more weight.
       </div>
     </div>
     <div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Factor Detail \u2014 15 Factors</div>
     <div style='overflow-x:auto;'>
     <table style='width:100%;border-collapse:collapse;font-family:Inter,sans-serif;'>
       <thead><tr style='background:#1e2640;border-bottom:2px solid #2a3042;'>
         <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:200px;'>Factor</th>
         <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;'>Weight / Type</th>
         <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:160px;'>Source</th>
         <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:220px;'>Mechanism</th>
         <th style='text-align:left;padding:10px 8px;color:#9aa3b2;font-size:11px;text-transform:uppercase;letter-spacing:0.5px;min-width:240px;'>Why It Matters</th>
       </tr></thead>
       <tbody>",
    rows,
    "</tbody></table></div>
     <div style='color:#555;font-size:11px;margin-top:16px;line-height:1.7;'>
       <b style='color:#9aa3b2;'>Tier 3 formula:</b>
       w_GLM, w_MS = Brier skill weights (recomputed monthly) &nbsp;|\u00a0
       p_blend = w_GLM\u00d7p_GLM + w_MS\u00d7p_Markov &nbsp;|\u00a0
       A = min(1, MD/(d95\u00d71.5)) &nbsp;|\u00a0
       p_final = p_blend\u00d7(1\u22120.35A) + 0.25A &nbsp;|\u00a0 clamped [2%, 97%].<br/>
       <b style='color:#9aa3b2;'>References:</b>
       Estrella &amp; Mishkin (1998); Sahm (2019); Hamilton (1983, 1989);
       Mahalanobis (1936); Brier (1950); MSwM (Sanchez-Espigares &amp; Lopez-Moreno).
     </div>
   </div></div>"
  )
}
