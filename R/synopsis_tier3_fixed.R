# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_fixed.R
#
# PURPOSE: Drop-in replacement patch for synopsis_tier3.R.
#          Source this file AFTER synopsis_tier2.R in global.R.  It
#          overrides exactly the functions that have critical statistical bugs.
#          All other Tier-3 functions (UI helpers, HTML builders, caching)
#          remain unchanged from the original file.
#
# FIXES APPLIED (7 critical issues):
#   1. Robust feature scaling  — median + MAD, clamp to [-3, 3]
#   2. Regularised GLM         — glmnet Ridge/ElasticNet replaces plain glm()
#   3. Correct contributions   — β_i × z_i on standardised scale
#   4. Consistent pipeline     — same scaler object used in training AND scoring
#   5. Likelihood-based Markov — best restart by log-likelihood, not freq match
#   6. Shrunk anomaly floor    — 0.7 × empirical + 0.3 × base-rate
#   7. Stable Mahalanobis      — scaled inputs + corpcor shrinkage covariance
#
# DEPENDENCIES: glmnet, corpcor, dplyr, tidyr, zoo, slider
# ─────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# FIX 1 + 7 — ROBUST FEATURE SCALER
# A reusable, stateful scaler built on the training distribution.
# *Only* the training rows define the median/MAD; both train and score rows
# are transformed using the SAME stored parameters.
# ══════════════════════════════════════════════════════════════════════════════

#' Build a robust scaler from a training data.frame.
#'
#' @param df       data.frame of numeric predictor columns (training rows only)
#' @param cols     character vector of column names to scale
#' @param mad_min  minimum MAD floor — prevents division-by-zero on constant
#'                 series (default 1e-6)
#'
#' @return A list with:
#'   \item{center}{named numeric — rolling median per column}
#'   \item{scale}{named numeric  — rolling MAD per column  (floored)}
#'   \item{cols}{character — columns included in the scaler}
#'
#' @details
#'   z = clamp((x - median) / MAD, -3, 3)
#'   After clamping, 0 always equals the training median, so imputing NAs
#'   as 0 is statistically correct (= "average, no signal").
build_robust_scaler <- function(df, cols, mad_min = 1e-6) {
  stopifnot(is.data.frame(df), length(cols) >= 1)
  cols <- intersect(cols, names(df))
  center <- vapply(cols, function(v) median(df[[v]], na.rm = TRUE), numeric(1))
  scale  <- vapply(cols, function(v) {
    m <- mad(df[[v]], na.rm = TRUE)
    max(m, mad_min)
  }, numeric(1))
  names(center) <- names(scale) <- cols
  list(center = center, scale = scale, cols = cols)
}


#' Apply a pre-built robust scaler to a data.frame and clamp to [-3, 3].
#'
#' @param df      data.frame (train or score)
#' @param scaler  output of build_robust_scaler()
#'
#' @return data.frame with the same columns, now z-scored and clamped
apply_robust_scaler <- function(df, scaler) {
  out <- df
  for (v in scaler$cols) {
    if (!v %in% names(df)) next
    z <- (df[[v]] - scaler$center[v]) / scaler$scale[v]
    z[is.na(z)] <- 0          # impute AFTER scaling: 0 = median
    out[[v]] <- pmax(-3, pmin(3, z))
  }
  out
}


#' Sanity-check a scaled matrix and emit warnings if bounds are violated.
validate_scaled <- function(mat, label = "") {
  rng <- range(mat, na.rm = TRUE)
  if (rng[1] < -3.01 || rng[2] > 3.01)
    warning(sprintf("[sanity %s] z-scores outside [-3,3]: min=%.2f max=%.2f",
                    label, rng[1], rng[2]))
  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 2 + 3 + 4 — REGULARISED GLM VIA glmnet (Ridge → ElasticNet fallback)
# Replaces rolling_retrain() from synopsis_tier3.R.
# ══════════════════════════════════════════════════════════════════════════════

rolling_retrain <- function(fred_data, window = 360L) {
  message(sprintf("[t3] Rolling GLM retrain (window=%dM, regularised)...", window))

  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    message("[t3] build_glm_feature_matrix returned NULL — cannot train GLM.")
    return(NULL)
  }

  panel_rolling <- tail(panel, window)
  if (nrow(panel_rolling) < 100) panel_rolling <- panel

  possible  <- c("yield_spread", "u_rise", "pay_3m", "real_rate", "hy_spread",
                 "oil_yoy", "prime_lfpr", "usd_yoy", "inv_sales", "cc_del", "sentiment")
  available <- intersect(possible, names(panel_rolling))
  if (length(available) < 3) {
    message("[t3] Too few predictors for GLM."); return(NULL)
  }

  # Drop predictors with >30% NA in the rolling window
  coverage  <- sapply(available, function(v) mean(!is.na(panel_rolling[[v]])))
  good_preds <- available[coverage >= 0.70]
  dropped    <- setdiff(available, good_preds)
  if (length(dropped) > 0)
    message(sprintf("[t3] Dropping low-coverage predictors: %s", paste(dropped, collapse = ", ")))
  if (length(good_preds) < 3) {
    message(sprintf("[t3] Too few predictors after coverage filter (%d).", length(good_preds)))
    return(NULL)
  }

  panel_clean <- panel_rolling %>%
    dplyr::select(rec_next12, dplyr::all_of(good_preds)) %>%
    tidyr::drop_na()

  if (nrow(panel_clean) < 60) {
    message(sprintf("[t3] Clean panel too small: %d rows.", nrow(panel_clean)))
    return(NULL)
  }

  # ── FIX 1: Build robust scaler on TRAINING rows only ───────────────────────
  scaler <- build_robust_scaler(panel_clean, good_preds)

  # ── FIX 4: Scale training data with the stored scaler ──────────────────────
  train_scaled <- apply_robust_scaler(panel_clean, scaler)
  validate_scaled(as.matrix(train_scaled[, good_preds]), "GLM train")

  X_train <- as.matrix(train_scaled[, good_preds, drop = FALSE])
  y_train <- train_scaled$rec_next12

  # ── FIX 2: glmnet Ridge (alpha=0) with CV lambda selection ─────────────────
  #   Falls back to ElasticNet (alpha=0.5) if Ridge CV fails.
  glm_result <- tryCatch({
    if (!requireNamespace("glmnet", quietly = TRUE))
      stop("glmnet not installed")

    cv_fit <- tryCatch(
      glmnet::cv.glmnet(X_train, y_train, family = "binomial",
                        alpha = 0, nfolds = 5, type.measure = "deviance"),
      error = function(e) {
        message(sprintf("[t3] Ridge CV failed (%s) — trying ElasticNet", e$message))
        glmnet::cv.glmnet(X_train, y_train, family = "binomial",
                          alpha = 0.5, nfolds = 5, type.measure = "deviance")
      }
    )
    lam   <- cv_fit$lambda.1se   # more regularised: better OOS stability
    coefs <- as.numeric(coef(cv_fit, s = lam))
    # coefs[1] = intercept, coefs[2:] = predictors
    coef_names <- c("(Intercept)", good_preds)
    coef_tbl <- data.frame(
      predictor = good_preds,
      estimate  = coefs[-1],
      stringsAsFactors = FALSE
    )

    # ── FIX 3: Sanity-check coefficient magnitudes ────────────────────────────
    large_coefs <- coef_tbl$predictor[abs(coef_tbl$estimate) > 4]
    if (length(large_coefs) > 0)
      warning(sprintf("[t3] Large glmnet coefficients (|β|>4): %s",
                      paste(large_coefs, collapse = ", ")))

    message(sprintf("[t3] glmnet Ridge: n=%d, window=%dM, lambda=%.5f, %d predictors",
                    nrow(panel_clean), window, lam, length(good_preds)))

    # ── Build anomaly detector on SCALED features ────────────────────────────
    ad <- build_anomaly_detector_scaled(train_scaled[, good_preds, drop = FALSE],
                                        panel_clean$rec_next12,
                                        scaler)

    list(
      model          = cv_fit,
      model_type     = "glmnet_ridge",
      coef_table     = coef_tbl,
      lambda         = lam,
      n_obs          = nrow(panel_clean),
      predictors     = good_preds,
      scaler         = scaler,          # ← stored for scoring
      trained_at     = Sys.time(),
      anomaly_detector = ad,
      window_months  = window,
      training_panel = panel_clean,     # unscaled, for Brier scoring
      training_panel_scaled = train_scaled  # scaled, for anomaly scoring
    )
  }, error = function(e) {
    # ── Graceful fallback: plain glm on scaled data ──────────────────────────
    message(sprintf("[t3] glmnet failed (%s) — falling back to scaled glm()", e$message))
    fml <- as.formula(paste("rec_next12 ~", paste(good_preds, collapse = " + ")))
    m   <- tryCatch(
      glm(fml, data = train_scaled, family = binomial(link = "logit")),
      error = function(e2) { message("[t3] Fallback glm() also failed: ", e2$message); NULL }
    )
    if (is.null(m)) return(NULL)
    sm  <- summary(m)
    cdf <- as.data.frame(sm$coefficients)
    cdf$predictor <- rownames(cdf)
    coef_tbl <- cdf %>%
      dplyr::filter(predictor != "(Intercept)") %>%
      dplyr::rename(estimate = Estimate, se = `Std. Error`,
                    z_stat = `z value`, p_val = `Pr(>|z|)`) %>%
      dplyr::select(predictor, estimate, dplyr::everything())

    large_coefs <- coef_tbl$predictor[abs(coef_tbl$estimate) > 4]
    if (length(large_coefs) > 0)
      warning(sprintf("[t3] Large glm coefficients (|β|>4) even after scaling: %s",
                      paste(large_coefs, collapse = ", ")))

    ad <- build_anomaly_detector_scaled(train_scaled[, good_preds, drop = FALSE],
                                        panel_clean$rec_next12,
                                        scaler)
    list(
      model          = m,
      model_type     = "glm_scaled_fallback",
      coef_table     = coef_tbl,
      lambda         = NA_real_,
      n_obs          = nrow(panel_clean),
      predictors     = good_preds,
      scaler         = scaler,
      trained_at     = Sys.time(),
      anomaly_detector = ad,
      window_months  = window,
      training_panel = panel_clean,
      training_panel_scaled = train_scaled
    )
  })

  glm_result
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 7 — MAHALANOBIS ANOMALY DETECTOR ON SCALED FEATURES
# Replaces build_anomaly_detector() from synopsis_tier2.R.
# Uses corpcor shrinkage covariance; operates entirely on the already-scaled
# feature matrix so distances are dimensionless and well-conditioned.
# ══════════════════════════════════════════════════════════════════════════════

#' Build anomaly detector from SCALED training features.
#'
#' @param scaled_df   data.frame of z-scored, clamped predictor columns
#'                    (output of apply_robust_scaler on the training panel)
#' @param rec_labels  numeric 0/1 vector — rec_next12 for training rows
#' @param scaler      the scaler object used to produce scaled_df
#'
#' @return Named list ready to pass to compute_anomaly_score_scaled().
build_anomaly_detector_scaled <- function(scaled_df, rec_labels = NULL, scaler = NULL) {
  null_result <- NULL

  if (is.null(scaled_df) || nrow(scaled_df) < 50) return(null_result)

  fp <- scaled_df[complete.cases(scaled_df), , drop = FALSE]
  if (nrow(fp) < 50) return(null_result)

  # ── FIX 7a: shrinkage covariance via corpcor ────────────────────────────────
  cov_mat <- tryCatch({
    if (requireNamespace("corpcor", quietly = TRUE)) {
      as.matrix(corpcor::cov.shrink(fp, verbose = FALSE))
    } else {
      message("[t3] corpcor unavailable — using regularised sample covariance")
      cov(fp) + diag(1e-4, ncol(fp))
    }
  }, error = function(e) {
    message(sprintf("[t3] Shrinkage cov failed (%s) — Tikhonov fallback", e$message))
    cov(fp) + diag(1e-4, ncol(fp))
  })

  sigma_inv <- tryCatch(solve(cov_mat), error = function(e) NULL)
  if (is.null(sigma_inv)) return(null_result)

  mu <- colMeans(fp)
  train_dists <- tryCatch(
    sqrt(mahalanobis(fp, mu, cov_mat)),
    error = function(e) NULL
  )
  if (is.null(train_dists)) return(null_result)

  d90 <- quantile(train_dists, 0.90, na.rm = TRUE)
  d95 <- quantile(train_dists, 0.95, na.rm = TRUE)
  d99 <- quantile(train_dists, 0.99, na.rm = TRUE)

  # ── Sanity check: MD should be near sqrt(p) in normal conditions ─────────
  p <- ncol(fp)
  expected_md_normal <- sqrt(p)
  med_dist <- median(train_dists, na.rm = TRUE)
  if (med_dist > expected_md_normal * 2.5)
    warning(sprintf(
      "[t3] Median Mahalanobis distance (%.2f) >> expected (%.2f). ",
      "Check that scaled_df is truly standardised.", med_dist, expected_md_normal))

  # ── FIX 6: shrinkage-smoothed anomaly recession floor ──────────────────────
  p_anom_rec <- NULL
  if (!is.null(rec_labels) && length(rec_labels) == nrow(scaled_df)) {
    rec_full <- rec_labels[complete.cases(scaled_df)]
    n_anom   <- sum(train_dists > d90, na.rm = TRUE)
    if (n_anom >= 12) {
      base_rate   <- mean(rec_full, na.rm = TRUE)
      p_tail_raw  <- mean(rec_full[train_dists > d90], na.rm = TRUE)
      # Shrink toward base rate — reduces small-sample instability
      p_anom_rec <- 0.7 * p_tail_raw + 0.3 * base_rate
      message(sprintf(
        "[t3] Anomaly recession floor (MD>d90, n=%d): raw=%.1f%% shrunk=%.1f%% base=%.1f%%",
        n_anom, p_tail_raw * 100, p_anom_rec * 100, base_rate * 100))
    }
  }

  list(
    mu                   = mu,
    cov_mat              = cov_mat,
    sigma_inv            = sigma_inv,
    feature_cols         = names(mu),
    d90                  = d90,
    d95                  = d95,
    d99                  = d99,
    p_anomalous_recession = p_anom_rec,
    scaler               = scaler,    # stored so scoring can re-scale on the fly
    n_train              = nrow(fp)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 4 (continued) — ANOMALY SCORING ON CONSISTENTLY SCALED INPUTS
# Replaces compute_anomaly_score() from synopsis_tier2.R.
# ══════════════════════════════════════════════════════════════════════════════

#' Compute anomaly score for a one-row current feature vector.
#'
#' @param anomaly_detector  output of build_anomaly_detector_scaled()
#' @param current_df        one-row data.frame in the ORIGINAL (unscaled) units
#'
#' @details
#'   The function scales current_df using the stored scaler BEFORE computing
#'   Mahalanobis distance, guaranteeing the same transformation as training.
compute_anomaly_score <- function(anomaly_detector, current_df) {
  null_result <- list(score = 0, md = NA_real_, label = "Normal", percentile = NA_real_)

  if (is.null(anomaly_detector) || is.null(current_df)) return(null_result)

  ad   <- anomaly_detector
  cols <- intersect(ad$feature_cols, names(current_df))
  if (length(cols) < 3) return(null_result)

  # ── Apply same scaler used during training ────────────────────────────────
  if (!is.null(ad$scaler)) {
    current_scaled <- apply_robust_scaler(current_df[, cols, drop = FALSE], ad$scaler)
  } else {
    # Legacy path: if no scaler stored, use training distribution parameters
    current_scaled <- current_df[, cols, drop = FALSE]
  }

  x_vec  <- as.numeric(current_scaled[1, cols])
  mu_sub <- ad$mu[cols]
  si_sub <- ad$sigma_inv[cols, cols]

  # Impute remaining NAs as 0 = training median on the scaled space
  x_vec[is.na(x_vec)] <- 0

  # ── Sanity check ──────────────────────────────────────────────────────────
  if (any(abs(x_vec) > 3.1))
    warning("[t3] compute_anomaly_score: some z-scores exceed 3 after scaling — check scaler")

  md <- tryCatch(
    sqrt(as.numeric(t(x_vec - mu_sub) %*% si_sub %*% (x_vec - mu_sub))),
    error = function(e) NA_real_
  )
  if (is.na(md) || !is.finite(md)) return(null_result)

  # Scale: d95 × 1.5 = score 1.0
  ceiling_dist <- ad$d95 * 1.5
  score <- min(1.0, max(0.0, md / ceiling_dist))

  pctile <- if (md > ad$d99) 99L
  else if (md > ad$d95) 95L
  else if (md > ad$d90) 90L
  else round(50 + 40 * (md / ad$d90))

  label <- if (score > 0.70) "Extreme \u2014 historically unprecedented"
  else if (score > 0.40)     "High \u2014 rare conditions (top 5% historically)"
  else if (score > 0.20)     "Elevated \u2014 unusual but observed historically"
  else                       "Normal"

  list(score = score, md = round(md, 2), label = label, percentile = pctile)
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 5 — LIKELIHOOD-BASED MARKOV MODEL SELECTION
# Replaces train_markov_model() from synopsis_tier3.R.
# Best restart is now chosen by maximum log-likelihood.
# Frequency constraint is enforced as a hard cap ONLY (not the objective).
# ══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data, n_states = 3L, n_restarts = 8L) {
  tryCatch({
    gm_local <- function(sid, val_name) {
      df <- fred_data[[sid]]; if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(date = month)
    }
    gf <- function(sid, val_name, start = as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid, observation_start = start, frequency = "m") %>%
          dplyr::select(date, value) %>%
          dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(!!val_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::rename(date = month),
        error = function(e) {
          message(sprintf("[t3] fredr('%s') failed: %s", sid, e$message))
          gm_local(sid, val_name)
        })
    }

    # Variable cascade: IP YoY → Payroll MoM → Unemployment
    switching_var <- NULL; switch_name <- NULL; higher_is_recession <- FALSE
    indpro_raw <- gf("INDPRO", "indpro")
    if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
      cand <- indpro_raw %>% dplyr::arrange(date) %>%
        dplyr::mutate(sw_var = (indpro / dplyr::lag(indpro, 12) - 1) * 100) %>%
        dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date, sw_var)
      if (nrow(cand) >= 60) { switching_var <- cand; switch_name <- "IP YoY %"; higher_is_recession <- FALSE }
    }
    if (is.null(switching_var)) {
      pay_raw <- gf("PAYEMS", "payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var = payems - dplyr::lag(payems, 1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date, sw_var)
        if (nrow(cand) >= 60) { switching_var <- cand; switch_name <- "Payroll MoM (K)"; higher_is_recession <- FALSE }
      }
    }
    if (is.null(switching_var)) {
      urate_raw <- gf("UNRATE", "unemp")
      if (!is.null(urate_raw) && nrow(urate_raw) >= 60)
        { switching_var <- urate_raw %>% dplyr::rename(sw_var = unemp)
          switch_name <- "Unemployment rate"; higher_is_recession <- TRUE }
    }
    if (is.null(switching_var)) { message("[t3] No switching variable."); return(NULL) }

    y          <- switching_var$sw_var[!is.na(switching_var$sw_var)]
    dates_used <- switching_var$date[!is.na(switching_var$sw_var)]
    message(sprintf("[t3] Markov EM: '%s' (%d obs), %d states, %d restarts",
                    switch_name, length(y), n_states, n_restarts))

    fit_once <- function(r) {
      tryCatch({
        y_fit <- if (r > 1) y + rnorm(length(y), 0, sd(y) * 0.04 * (r - 1)) else y
        fit   <- .fit_hamilton_em(y_fit, k = n_states, max_iter = 500L, tol = 1e-6, seed = r * 37L)
        sp    <- fit$smooth_probs
        sa    <- apply(sp, 1, which.max)
        emp   <- sapply(seq_len(n_states), function(s) mean(y[sa == s], na.rm = TRUE))
        rs    <- if (higher_is_recession) which.max(emp) else which.min(emp)
        freq  <- mean(sa == rs)
        list(fit = fit, smooth_probs = sp, recession_state = rs,
             emp_means = emp, freq_rec = freq, state_assign = sa,
             loglik = fit$loglik)
      }, error = function(e) {
        message(sprintf("[t3]   restart %d failed: %s", r, e$message)); NULL
      })
    }

    cands <- Filter(Negate(is.null), lapply(seq_len(n_restarts), fit_once))
    if (length(cands) == 0) { message("[t3] All EM restarts failed."); return(NULL) }

    # ── FIX 5: Select by MAXIMUM LOG-LIKELIHOOD ───────────────────────────────
    # Previous code: which.min(abs(freq_rec - target))  ← WRONG
    # Correct: max log-likelihood; only apply frequency as a hard cap.
    MAX_FREQ <- .MARKOV_FREQ_MAX  # 0.25 — set in original constants block

    # Prefer candidates whose recession freq is within a reasonable range first;
    # if none qualify, relax to all candidates.
    freq_ok   <- Filter(function(c) c$freq_rec <= MAX_FREQ, cands)
    pool      <- if (length(freq_ok) > 0) freq_ok else cands
    best      <- pool[[which.max(sapply(pool, function(c) c$loglik))]]

    # Log out all candidates for diagnostics
    for (i in seq_along(cands)) {
      c <- cands[[i]]
      message(sprintf("[t3]   restart %d: ll=%.1f conv=%s | means=[%s] | rec=%d freq=%.0f%% %s",
                      i, c$loglik, c$fit$converged,
                      paste(round(c$emp_means, 1), collapse = ","),
                      c$recession_state, c$freq_rec * 100,
                      if (c$loglik == best$loglik) "<-- SELECTED (max LL)" else ""))
    }

    sp       <- best$smooth_probs
    rs       <- best$recession_state
    emp      <- best$emp_means
    sa       <- best$state_assign
    freq_rec <- best$freq_rec
    cur_p    <- sp[length(y), rs]
    reliable <- freq_rec <= MAX_FREQ

    ord <- order(emp, decreasing = !higher_is_recession)
    lbl <- character(n_states)
    if (n_states == 3L) {
      lbl[ord[1]] <- "Expansion"; lbl[ord[2]] <- "Stall"; lbl[ord[3]] <- "Contraction"
    } else {
      lbl[rs] <- "Contraction"; lbl[setdiff(seq_len(n_states), rs)[1]] <- "Expansion"
    }

    exp_st   <- ord[1]
    sw_means <- list(variable = switch_name,
                     expansion = round(emp[exp_st], 2),
                     recession = round(emp[rs], 2))

    if (!reliable)
      message(sprintf("[t3] WARNING: rec state = %.0f%% of history (max %.0f%%). Weight capped.",
                      freq_rec * 100, MAX_FREQ * 100))

    message(sprintf("[t3] Markov final (LL=%.1f): [%s] | rec='%s' mean=%.2f (%.0f%%) | p_rec=%.1f%% | reliable=%s",
                    best$loglik,
                    paste(sprintf("%s=%.1f", lbl, emp), collapse = " / "),
                    lbl[rs], emp[rs], freq_rec * 100, cur_p * 100, reliable))

    list(model = best$fit, smooth_probs = sp, recession_state = rs,
         current_rec_prob = cur_p, state_means_sw = emp, state_labels = lbl,
         switch_name = switch_name, higher_is_recession = higher_is_recession,
         regime_means = list(), sw_regime_means = sw_means,
         state_freq_recession = freq_rec, reliable = reliable,
         n_states = n_states, n_obs = length(y), trained_at = Sys.time())

  }, error = function(e) { message("[t3] Markov failed: ", e$message); NULL })
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 4 (continued) — CONSISTENT BRIER SCORING PIPELINE
# Replaces compute_ensemble_weights() from synopsis_tier3.R.
# The scoring GLM uses the same robust scaler logic as rolling_retrain().
# NAs in the holdout are NOT filled with raw 0 — they are scaled to 0
# (= median on the standardised scale) to maintain consistency.
# ══════════════════════════════════════════════════════════════════════════════

compute_ensemble_weights <- function(glm_result, markov_result, holdout_months = 36L) {
  fallback <- list(w_glm = 0.60, w_ms = 0.40,
                   glm_brier = NA_real_, ms_brier = NA_real_,
                   n_holdout = 0L, method = "fallback (equal-ish)")

  if (is.null(glm_result) || is.null(glm_result$training_panel)) {
    message("[t3] compute_ensemble_weights: training_panel missing, using fallback.")
    return(fallback)
  }

  panel <- glm_result$training_panel   # UNSCALED (raw values)
  n     <- nrow(panel)

  if (n < holdout_months + 30L) {
    message(sprintf("[t3] Panel too small for holdout (n=%d). Fallback.", n))
    return(fallback)
  }

  hold_idx  <- (n - holdout_months + 1L):n
  train_idx <- seq_len(n - holdout_months)
  hold_df   <- panel[hold_idx,  , drop = FALSE]
  train_df  <- panel[train_idx, , drop = FALSE]
  y_act     <- hold_df$rec_next12
  ok        <- !is.na(y_act)

  if (sum(ok) < 12L) {
    message(sprintf("[t3] Only %d valid holdout outcomes. Fallback.", sum(ok)))
    return(fallback)
  }

  hold_df <- hold_df[ok, , drop = FALSE]
  y_act   <- y_act[ok]
  avail   <- intersect(glm_result$predictors, names(panel))

  # ── FIX 4: Build scaler on training split; apply identically to holdout ─────
  glm_brier <- tryCatch({
    sc_train <- train_df %>%
      dplyr::select(rec_next12, dplyr::all_of(avail)) %>%
      tidyr::drop_na()
    if (nrow(sc_train) < 60) stop("scoring train set too small")

    # Build fresh scaler from the scoring train partition
    sc_scaler  <- build_robust_scaler(sc_train, avail)
    sc_scaled  <- apply_robust_scaler(sc_train, sc_scaler)

    # Holdout: scale with the SAME scaler (no re-fitting on holdout rows)
    ho_scaled  <- apply_robust_scaler(hold_df[, avail, drop = FALSE], sc_scaler)
    # NAs after scaling become 0 (= median) — already handled by apply_robust_scaler

    X_sc   <- as.matrix(sc_scaled[, avail, drop = FALSE])
    X_ho   <- as.matrix(ho_scaled[, avail, drop = FALSE])
    y_sc   <- sc_scaled$rec_next12

    # Use glmnet Ridge for the scoring model too
    if (requireNamespace("glmnet", quietly = TRUE)) {
      cv_sc <- glmnet::cv.glmnet(X_sc, y_sc, family = "binomial",
                                 alpha = 0, nfolds = 5, type.measure = "deviance")
      preds <- as.numeric(predict(cv_sc, newx = X_ho, s = "lambda.1se", type = "response"))
    } else {
      # Plain glm fallback
      sc_df <- as.data.frame(cbind(rec_next12 = y_sc, sc_scaled[, avail]))
      tmp   <- glm(as.formula(paste("rec_next12 ~", paste(avail, collapse = "+"))),
                   data = sc_df, family = binomial)
      ho_df <- as.data.frame(X_ho); names(ho_df) <- avail
      preds <- predict(tmp, newdata = ho_df, type = "response")
    }
    mean((preds - y_act)^2, na.rm = TRUE)
  }, error = function(e) {
    message("[t3] GLM Brier failed: ", e$message); NA_real_
  })

  ms_brier <- tryCatch({
    if (is.null(markov_result) || is.null(markov_result$smooth_probs))
      stop("no smooth_probs")
    sp_rec <- markov_result$smooth_probs[, markov_result$recession_state]
    n_h    <- length(y_act)
    if (length(sp_rec) < n_h) stop("smooth_probs too short")
    mean((tail(sp_rec, n_h) - y_act)^2, na.rm = TRUE)
  }, error = function(e) {
    message("[t3] Markov Brier failed: ", e$message); NA_real_
  })

  message(sprintf("[t3] Brier — GLM:%s Markov:%s holdout:%d months",
                  if (is.na(glm_brier)) "NA" else sprintf("%.4f", glm_brier),
                  if (is.na(ms_brier)) "NA" else sprintf("%.4f", ms_brier),
                  length(y_act)))

  if (is.na(glm_brier) && is.na(ms_brier)) return(fallback)

  base_rate <- mean(panel$rec_next12, na.rm = TRUE)
  clim      <- base_rate * (1 - base_rate)

  # Sanity: if Markov Brier > climatology it has zero skill
  if (!is.na(ms_brier) && ms_brier > clim) {
    message(sprintf("[t3] Markov Brier (%.4f) > climatology (%.4f) — zero skill, capping at 20%%.",
                    ms_brier, clim))
    return(modifyList(fallback,
                      list(w_glm = 0.80, w_ms = 0.20,
                           glm_brier = glm_brier, ms_brier = ms_brier,
                           n_holdout = length(y_act),
                           method = "GLM dominant (Markov zero skill)")))
  }

  if (is.na(ms_brier))
    return(modifyList(fallback,
                      list(w_glm = 0.70, w_ms = 0.30,
                           glm_brier = glm_brier, n_holdout = length(y_act),
                           method = "GLM only (Markov NA)")))
  if (is.na(glm_brier))
    return(modifyList(fallback,
                      list(w_glm = 0.30, w_ms = 0.70,
                           ms_brier = ms_brier, n_holdout = length(y_act),
                           method = "Markov only (GLM NA)")))

  gs  <- max(0, 1 - glm_brier / clim)
  ms  <- max(0, 1 - ms_brier / clim)
  tot <- gs + ms
  if (tot < 1e-4) { message("[t3] Both models near-zero skill. Fallback."); return(fallback) }

  w_glm <- gs / tot; w_ms <- ms / tot

  # Reliability penalty
  if (!is.null(markov_result) && isFALSE(markov_result$reliable)) {
    w_ms  <- min(w_ms, 0.10); w_glm <- 1 - w_ms
    message(sprintf("[t3] Markov weight capped at 10%% (state freq=%.0f%%)",
                    markov_result$state_freq_recession * 100))
  }

  method <- sprintf("OOS Brier skill [consistent scaling] (clim=%.4f, n=%d months)", clim, length(y_act))

  message(sprintf("[t3] Weights → GLM=%.0f%% (skill=%.3f) | Markov=%.0f%% (skill=%.3f)",
                  w_glm * 100, gs, w_ms * 100, ms))

  list(w_glm = round(w_glm, 3), w_ms = round(w_ms, 3),
       glm_brier = round(glm_brier, 4), ms_brier = round(ms_brier, 4),
       glm_skill = round(gs, 3), ms_skill = round(ms, 3),
       n_holdout = length(y_act), method = method)
}


# ══════════════════════════════════════════════════════════════════════════════
# FIX 3 + 4 (continued) — SCORING / CONTRIBUTION CALCULATION
#
# The original build_current_feature_vector() is unchanged (it creates the
# raw one-row data.frame).  This helper wraps it with scaling so that the
# GLM predict() call and the β_i × z_i contributions are always on the
# standardised scale, matching what glmnet was trained on.
# ══════════════════════════════════════════════════════════════════════════════

#' Score a single observation and return recession probability + contributions.
#'
#' @param feat_df      one-row data.frame from build_current_feature_vector()
#' @param glm_result   output of rolling_retrain() — must contain $scaler
#'
#' @return list(prob, logit, contributions, model_type, scaled_df)
score_glm_observation <- function(feat_df, glm_result) {
  if (is.null(glm_result) || is.null(feat_df)) return(NULL)

  avail  <- intersect(glm_result$predictors, names(feat_df))
  if (length(avail) < 2) return(NULL)

  # ── FIX 4: use the training scaler stored inside glm_result ─────────────
  scaler <- glm_result$scaler
  if (is.null(scaler)) {
    warning("[t3] score_glm_observation: no scaler in glm_result — contributions unreliable")
    scaled_df <- feat_df
  } else {
    scaled_df <- apply_robust_scaler(feat_df[, avail, drop = FALSE], scaler)
  }

  validate_scaled(as.matrix(scaled_df[, avail, drop = FALSE]), "scoring")

  model      <- glm_result$model
  model_type <- glm_result$model_type %||% "unknown"

  # Predict probability
  prob <- tryCatch({
    if (inherits(model, "cv.glmnet")) {
      X_new <- as.matrix(scaled_df[, avail, drop = FALSE])
      as.numeric(predict(model, newx = X_new, s = "lambda.1se", type = "response"))
    } else {
      predict(model, newdata = scaled_df, type = "response")
    }
  }, error = function(e) { message("[t3] GLM predict failed: ", e$message); NA_real_ })

  if (is.na(prob)) return(NULL)

  # ── FIX 3: Contributions on the logit scale β_i × z_i ───────────────────
  coef_tbl <- glm_result$coef_table
  contribs <- if (!is.null(coef_tbl)) {
    lapply(seq_len(nrow(coef_tbl)), function(i) {
      v   <- coef_tbl$predictor[i]
      b   <- coef_tbl$estimate[i]
      z   <- if (v %in% names(scaled_df)) as.numeric(scaled_df[[v]]) else 0
      z[is.na(z)] <- 0
      delta <- b * z
      # Sanity check: individual contributions should stay within [-4, 4]
      if (!is.na(delta) && abs(delta) > 4)
        warning(sprintf("[t3] Large contribution for '%s': β=%.3f × z=%.3f = %.3f",
                        v, b, z, delta))
      list(name         = v,
           contribution = round(delta, 4),
           beta         = round(b, 4),
           z_score      = round(z, 4),
           z_stat       = coef_tbl$z_stat[i] %||% NA_real_,
           data_missing = FALSE)
    })
  } else list()

  intercept <- if (inherits(model, "cv.glmnet")) {
    as.numeric(coef(model, s = "lambda.1se")[1, ])
  } else {
    coef(model)["(Intercept)"]
  }
  logit <- as.numeric(intercept) + sum(sapply(contribs, `[[`, "contribution"))

  list(prob = prob, logit = logit, contributions = contribs,
       model_type = model_type, scaled_df = scaled_df)
}


# ══════════════════════════════════════════════════════════════════════════════
# SANITY CHECK RUNNER — call after training to verify all invariants
# ══════════════════════════════════════════════════════════════════════════════

#' Run post-training sanity checks on a glm_result object.
#'
#' Emits warnings for any violation; returns a named logical vector.
sanity_check_glm_result <- function(glm_result) {
  ok <- c(
    has_scaler   = !is.null(glm_result$scaler),
    has_coeftbl  = !is.null(glm_result$coef_table),
    has_anomaly  = !is.null(glm_result$anomaly_detector)
  )

  if (!is.null(glm_result$coef_table)) {
    betas <- glm_result$coef_table$estimate
    ok["coefs_bounded"] <- all(abs(betas) <= 4, na.rm = TRUE)
    if (!ok["coefs_bounded"])
      warning(sprintf("[sanity] %d coefficient(s) |β|>4: %s",
                      sum(abs(betas) > 4, na.rm = TRUE),
                      paste(glm_result$coef_table$predictor[abs(betas) > 4], collapse = ", ")))
  }

  if (!is.null(glm_result$scaler)) {
    tp <- glm_result$training_panel
    if (!is.null(tp)) {
      sp <- apply_robust_scaler(tp[, glm_result$scaler$cols, drop = FALSE], glm_result$scaler)
      rng <- range(as.matrix(sp), na.rm = TRUE)
      ok["zscores_clamped"] <- rng[1] >= -3.01 && rng[2] <= 3.01
      if (!ok["zscores_clamped"])
        warning(sprintf("[sanity] Training z-scores outside [-3,3]: [%.2f, %.2f]", rng[1], rng[2]))
    }
  }

  if (!is.null(glm_result$anomaly_detector)) {
    ad <- glm_result$anomaly_detector
    ok["md_d95_reasonable"] <- !is.na(ad$d95) && ad$d95 < 6
    if (!ok["md_d95_reasonable"])
      warning(sprintf("[sanity] d95 Mahalanobis distance is %.2f (expected <6) — check scaling", ad$d95))
  }

  message(sprintf("[sanity] %s", paste(names(ok), ifelse(ok, "✓", "✗"), sep = "=", collapse = "  ")))
  invisible(ok)
}

# ── End of synopsis_tier3_fixed.R ─────────────────────────────────────────────
# Usage in global.R:
#
#   source("R/synopsis_tier1.R")
#   source("R/synopsis_tier2.R")
#   source("R/synopsis_tier3.R")        # original file (UI helpers etc.)
#   source("R/synopsis_tier3_fixed.R")  # this file — overrides the buggy fns
#
# After train:
#   result <- rolling_retrain(fred_data)
#   sanity_check_glm_result(result)
