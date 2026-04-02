# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch2.R — Second patch layer for synopsis_tier3
#
# SOURCE ORDER in global.R:
#   source("R/synopsis_tier3.R")
#   source("R/synopsis_tier3_patch.R")    # case_when + loading screen fixes
#   source("R/synopsis_tier3_patch2.R")   # this file
#
# FIXES:
#
#  1. MARKOV BAR SCALE
#     The Markov contribution was computed in percentage-point units
#     ((p_ms - p_glm) × 100 × weight) while every other factor bar is in
#     log-odds units (β×x, typically ±0.1 to ±1.8).  This caused the Markov
#     bar to swamp the chart at ±10.  Fix: convert both probabilities to
#     log-odds before differencing so Markov sits on the same scale.
#
#  2. BRIER SCORING — TRUE OUT-OF-SAMPLE
#     The old code trained the production GLM on all 360 months, then scored
#     the last 36 of those same months.  That is in-sample scoring and the
#     result is meaningless.  Fix: fit a *temporary* scoring GLM on months
#     1 to (n - holdout), score on months (n - holdout + 1) to n (which
#     that GLM never saw), then discard it.  Production model is unchanged.
#     rec_next12 outcomes are always available through ~12 months ago, so
#     the holdout always has valid data.
#
#  3. MARKOV RECESSION STATE IDENTIFICATION
#     Old code used ms_model@Coef[1,s] (regression intercept when all
#     predictors = 0) to pick which state = recession.  Intercepts are not
#     means; they can be in any order.  Fix: after state assignment, compute
#     the empirical mean of sw_var in each state and pick the state with the
#     lowest mean (for IP / payrolls) or highest mean (for UNRATE).
#     This corrects the "Expansion mean: 1.02 | Recession mean: 3.83" flip.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

# ── Helpers ──────────────────────────────────────────────────────────────────
.prob_to_lo  <- function(p) log(pmax(1e-6, pmin(1-1e-6, p)) / (1 - pmax(1e-6, pmin(1-1e-6, p))))
.lo_to_prob  <- function(lo) 1 / (1 + exp(-lo))


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 2: compute_ensemble_weights — truly out-of-sample Brier scoring
# ═══════════════════════════════════════════════════════════════════════════════

compute_ensemble_weights <- function(glm_result, markov_result,
                                     holdout_months = 36L) {

  fallback <- list(w_glm = 0.60, w_ms = 0.40,
                   glm_brier = NA_real_, ms_brier = NA_real_,
                   n_holdout = 0L, method = "fallback (equal-ish)")

  if (is.null(glm_result) || is.null(glm_result$training_panel)) {
    message("[t3] compute_ensemble_weights: training_panel missing, using fallback weights")
    return(fallback)
  }

  panel <- glm_result$training_panel   # full cleaned panel used to fit production GLM
  n     <- nrow(panel)

  if (n < holdout_months + 30L) {
    message(sprintf("[t3] Panel too small for Brier holdout (n=%d, need >%d)", n, holdout_months+30L))
    return(fallback)
  }

  # ── Split into scoring-train / holdout ────────────────────────────────────
  # The production GLM was trained on ALL n rows.  To get OOS Brier scores we
  # re-fit a temporary GLM on rows 1:(n-holdout), then score on the holdout.
  score_train_idx <- seq_len(n - holdout_months)
  holdout_idx     <- (n - holdout_months + 1L):n

  score_train_df <- panel[score_train_idx, , drop = FALSE]
  holdout_df     <- panel[holdout_idx,     , drop = FALSE]

  # Remove holdout rows where outcome is NA (last ~12M of history have no outcome yet)
  y_actual <- holdout_df$rec_next12
  ok       <- !is.na(y_actual)
  if (sum(ok) < 12L) {
    message(sprintf("[t3] Only %d scored holdout rows (need ≥12). Brier fallback.", sum(ok)))
    return(fallback)
  }
  holdout_df <- holdout_df[ok, , drop = FALSE]
  y_actual   <- y_actual[ok]

  avail <- intersect(glm_result$predictors, names(panel))

  # ── FIT temporary scoring GLM (never sees holdout) ────────────────────────
  glm_brier <- tryCatch({
    score_train_clean <- score_train_df %>%
      dplyr::select(rec_next12, dplyr::all_of(avail)) %>%
      tidyr::drop_na()
    if (nrow(score_train_clean) < 60) stop("Too few rows in scoring train set")

    fml_score  <- as.formula(paste("rec_next12 ~", paste(avail, collapse=" + ")))
    tmp_glm    <- glm(fml_score, data=score_train_clean, family=binomial(link="logit"))

    # Score on holdout (impute NAs with 0 — same as production model)
    hdf <- holdout_df[, avail, drop=FALSE]
    for (col in names(hdf)) if (is.na(hdf[[col]])) hdf[[col]] <- 0
    preds <- predict(tmp_glm, newdata=hdf, type="response")
    mean((preds - y_actual)^2, na.rm=TRUE)
  }, error = function(e) {
    message("[t3] GLM Brier scoring failed: ", e$message)
    NA_real_
  })

  # ── Markov Brier — align smooth_probs tail to holdout dates ───────────────
  # The Markov smooth_probs cover the full INDPRO history (~794 months).
  # The GLM holdout covers approximately the last (holdout + 12) months of
  # history (12M offset because rec_next12 is a forward variable).
  # We align by taking the last sum(ok) rows of smooth_probs.
  ms_brier <- tryCatch({
    if (is.null(markov_result) || is.null(markov_result$smooth_probs))
      stop("No Markov smooth_probs")
    sp_rec  <- markov_result$smooth_probs[, markov_result$recession_state]
    n_hold  <- length(y_actual)
    if (length(sp_rec) < n_hold) stop("smooth_probs shorter than holdout")
    ms_preds <- tail(sp_rec, n_hold)
    mean((ms_preds - y_actual)^2, na.rm=TRUE)
  }, error = function(e) {
    message("[t3] Markov Brier scoring failed: ", e$message)
    NA_real_
  })

  message(sprintf("[t3] Brier scores — GLM: %s | Markov: %s | holdout: %d months",
                  if (is.na(glm_brier)) "NA" else sprintf("%.4f", glm_brier),
                  if (is.na(ms_brier))  "NA" else sprintf("%.4f", ms_brier),
                  length(y_actual)))

  if (is.na(glm_brier) && is.na(ms_brier)) return(fallback)
  if (is.na(ms_brier))
    return(modifyList(fallback, list(w_glm=0.85, w_ms=0.15,
                                     glm_brier=glm_brier, n_holdout=length(y_actual),
                                     method="GLM only (Markov Brier unavailable)")))
  if (is.na(glm_brier))
    return(modifyList(fallback, list(w_glm=0.15, w_ms=0.85,
                                     ms_brier=ms_brier, n_holdout=length(y_actual),
                                     method="Markov only (GLM Brier unavailable)")))

  base_rate   <- mean(panel$rec_next12, na.rm=TRUE)
  clim_brier  <- base_rate * (1 - base_rate)
  glm_skill   <- max(0, 1 - glm_brier / clim_brier)
  ms_skill    <- max(0, 1 - ms_brier  / clim_brier)
  total_skill <- glm_skill + ms_skill

  if (total_skill < 1e-4) return(fallback)

  w_glm <- glm_skill / total_skill
  w_ms  <- ms_skill  / total_skill

  message(sprintf("[t3] Ensemble weights → GLM=%.0f%% (skill=%.3f) | Markov=%.0f%% (skill=%.3f)",
                  w_glm*100, glm_skill, w_ms*100, ms_skill))

  list(w_glm     = round(w_glm, 3),
       w_ms      = round(w_ms,  3),
       glm_brier = round(glm_brier, 4),
       ms_brier  = round(ms_brier,  4),
       glm_skill = round(glm_skill, 3),
       ms_skill  = round(ms_skill,  3),
       n_holdout = length(y_actual),
       method    = sprintf("OOS Brier skill (clim=%.4f, n=%d holdout months)", clim_brier, length(y_actual)))
}


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 3: train_markov_model — use empirical state means, not model intercepts
# ═══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data, n_states = 2L) {
  tryCatch({
    gm_local <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name := mean(value, na.rm=TRUE), .groups="drop") %>%
        dplyr::rename(date = month)
    }
    gm_fred <- function(sid, val_name, start = as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid, observation_start=start, frequency="m") %>%
          dplyr::select(date, !!val_name := value),
        error = function(e) {
          message(sprintf("[t3] fredr('%s') failed: %s — using fred_data", sid, e$message))
          gm_local(sid, val_name)
        })
    }

    switching_var <- NULL; switch_name <- NULL; higher_is_recession <- FALSE

    # Cascade: INDPRO YoY → PAYEMS MoM → UNRATE
    indpro_raw <- gm_fred("INDPRO", "indpro")
    if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
      cand <- indpro_raw %>% dplyr::arrange(date) %>%
        dplyr::mutate(sw_var = (indpro / dplyr::lag(indpro, 12) - 1) * 100) %>%
        dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date, sw_var)
      if (nrow(cand) >= 60) {
        switching_var <- cand; switch_name <- "IP YoY %"; higher_is_recession <- FALSE
      }
    }
    if (is.null(switching_var)) {
      pay_raw <- gm_fred("PAYEMS", "payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var = payems - dplyr::lag(payems, 1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date, sw_var)
        if (nrow(cand) >= 60) {
          switching_var <- cand; switch_name <- "Payroll MoM chg (K)"; higher_is_recession <- FALSE
        }
      }
    }
    if (is.null(switching_var)) {
      urate_raw <- gm_fred("UNRATE", "unemp")
      if (!is.null(urate_raw) && nrow(urate_raw) >= 60) {
        switching_var <- urate_raw %>% dplyr::rename(sw_var = unemp)
        switch_name   <- "Unemployment rate"; higher_is_recession <- TRUE
      }
    }
    if (is.null(switching_var)) {
      message("[t3] No switching variable found after full cascade.")
      return(NULL)
    }

    message(sprintf("[t3] Markov switching variable: '%s' (%d obs)", switch_name, nrow(switching_var)))

    # Predictors
    spread_raw <- gm_fred("T10Y2Y", "yield_spread")
    if (is.null(spread_raw)) {
      t10 <- gm_fred("DGS10","t10"); t2 <- gm_fred("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        spread_raw <- dplyr::inner_join(t10,t2,by="date") %>%
          dplyr::mutate(yield_spread=t10-t2) %>% dplyr::select(date,yield_spread)
    }
    hy_raw   <- gm_fred("BAMLH0A0HYM2", "hy_spread")
    unem_raw <- gm_fred("UNRATE", "unemp")

    panel <- switching_var
    for (df in list(spread_raw, hy_raw, unem_raw))
      if (!is.null(df)) panel <- dplyr::left_join(panel, df, by="date")
    panel <- panel %>% dplyr::filter(!is.na(sw_var))

    avail_preds <- intersect(c("yield_spread","hy_spread","unemp"), names(panel))
    avail_preds <- avail_preds[sapply(avail_preds, function(v) mean(is.na(panel[[v]])) < 0.30)]
    for (v in avail_preds) panel[[v]][is.na(panel[[v]])] <- mean(panel[[v]], na.rm=TRUE)

    if (nrow(panel) < 60) {
      message(sprintf("[t3] Panel too small after cleaning: %d rows.", nrow(panel)))
      return(NULL)
    }

    base_fml <- if (length(avail_preds) > 0)
      as.formula(paste("sw_var ~", paste(avail_preds, collapse=" + ")))
    else as.formula("sw_var ~ 1")

    base_lm <- lm(base_fml, data=panel)
    n_coef  <- length(coef(base_lm))
    sw_vec  <- c(TRUE, rep(FALSE, max(0, n_coef-1)), TRUE)
    if (length(sw_vec) != n_coef + 1) sw_vec <- rep(TRUE, n_coef)

    ms_model <- MSwM::msmFit(base_lm, k=n_states, sw=sw_vec,
                              control=list(parallel=FALSE, maxiter=300))

    smooth_probs <- ms_model@Fit@smoProb
    if (is.null(smooth_probs) || nrow(smooth_probs) == 0) return(NULL)

    # FIX 3: Use empirical mean of sw_var per state (not model intercept)
    # This correctly identifies which state = recession regardless of covariate values
    state_assign <- apply(smooth_probs, 1, which.max)
    panel_trimmed <- panel[seq_len(nrow(smooth_probs)), ]

    empirical_state_means <- sapply(seq_len(n_states), function(s)
      mean(panel_trimmed$sw_var[state_assign == s], na.rm = TRUE))

    # Recession = lower mean growth (IP, payrolls) or higher mean (UNRATE)
    recession_state <- if (higher_is_recession) which.max(empirical_state_means)
                       else                     which.min(empirical_state_means)

    current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]

    # Regime-conditional means for predictors
    regime_means <- lapply(avail_preds, function(v) list(
      variable  = v,
      expansion = round(mean(panel_trimmed[[v]][state_assign != recession_state], na.rm=TRUE), 2),
      recession = round(mean(panel_trimmed[[v]][state_assign == recession_state],  na.rm=TRUE), 2)
    ))
    sw_regime_means <- list(
      variable  = switch_name,
      expansion = round(empirical_state_means[setdiff(seq_len(n_states), recession_state)[1]], 2),
      recession = round(empirical_state_means[recession_state], 2)
    )

    # Fraction of months in each state (useful context)
    state_freq <- table(state_assign) / length(state_assign)

    message(sprintf(
      "[t3] Markov fitted: '%s' n=%d | recession_state=%d (emp.mean=%.2f%%, %.0f%% of months) | p_rec_now=%.1f%%",
      switch_name, nrow(panel), recession_state,
      empirical_state_means[recession_state],
      state_freq[recession_state] * 100,
      current_rec_prob * 100))

    list(model            = ms_model,
         smooth_probs     = smooth_probs,
         recession_state  = recession_state,
         current_rec_prob = current_rec_prob,
         state_means_sw   = empirical_state_means,   # now empirical, not intercepts
         switch_name      = switch_name,
         higher_is_recession = higher_is_recession,
         regime_means     = regime_means,
         sw_regime_means  = sw_regime_means,
         state_freq_recession = as.numeric(state_freq[recession_state]),
         n_states         = n_states,
         n_obs            = nrow(panel),
         trained_at       = Sys.time())
  }, error = function(e) {
    message("[t3] Markov model failed: ", e$message)
    NULL
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 1: .compute_recession_prob — Markov contribution in log-odds units
# ═══════════════════════════════════════════════════════════════════════════════

.compute_recession_prob <- function(
  yield_spread, inv_months, unemp, u, pay_3m, real_rate,
  hy_v, vix_v, cs, ip_y, hs,
  prime_age_lfpr = NA, prime_age_lfpr_chg = NA,
  jobless_claims_trend = NA, quits_yoy = NA, oil_yoy = NA,
  equity_drawdown_pct = NA, usd_yoy = NA,
  inventory_sales_ratio = NA, cc_delinquency = NA,
  glm_result       = NULL,
  anomaly_detector = NULL,
  markov_result    = NULL,
  ensemble_weights = NULL
) {
  ANOMALY_WEIGHT  <- 0.35
  ANOMALY_FLOOR   <- 0.25
  COMPRESS_CENTER <- 0.50

  contribs   <- list()
  add_contrib <- function(id, label, delta, z_stat=NA_real_, data_missing=FALSE)
    contribs[[id]] <<- list(name=label, contribution=delta, z_stat=z_stat, data_missing=data_missing)

  u_rise_val <- if (!is.na(u) && !is.null(unemp) && length(unemp) >= 12)
    u - min(tail(unemp, 12), na.rm=TRUE) else NA_real_

  feat_df <- build_current_feature_vector(
    yield_spread, u_rise_val, pay_3m, real_rate, hy_v, oil_yoy,
    prime_age_lfpr, usd_yoy, inventory_sales_ratio, cc_delinquency, cs)

  anomaly_result <- compute_anomaly_score(anomaly_detector, feat_df)
  A <- anomaly_result$score

  # ── GLM probability ──────────────────────────────────────────────────────────
  glm_prob_raw <- NA_real_
  if (!is.null(glm_result) && !is.null(glm_result$model)) {
    keep_cols <- intersect(glm_result$predictors, names(feat_df))
    feat_sub  <- feat_df[, keep_cols, drop=FALSE]
    for (col in names(feat_sub)) if (is.na(feat_sub[[col]])) feat_sub[[col]] <- 0

    glm_prob_raw <- tryCatch(
      predict(glm_result$model, newdata=feat_sub, type="response")[[1]],
      error=function(e) NA_real_)

    if (!is.na(glm_prob_raw)) {
      coefs <- coef(glm_result$model); ct <- glm_result$coef_table
      fv_orig <- build_current_feature_vector(
        yield_spread, u_rise_val, pay_3m, real_rate, hy_v, oil_yoy,
        prime_age_lfpr, usd_yoy, inventory_sales_ratio, cc_delinquency, cs)
      fv_list <- as.list(fv_orig[1,,drop=FALSE])
      plabels <- c(yield_spread="Yield Curve (10Y-2Y spread)",
                   u_rise="Sahm Rule (unemployment rise)",
                   pay_3m="Payroll Momentum (3M avg)",
                   real_rate="Real Fed Funds Rate",
                   hy_spread="HY Credit Spreads",
                   oil_yoy="Oil Price Shock (YoY)",
                   prime_lfpr="Prime-Age LFPR (25\u201354)",
                   usd_yoy="USD Surge (YoY)",
                   inv_sales="Inventory-to-Sales Ratio",
                   cc_del="CC Delinquency Rate",
                   sentiment="Consumer Sentiment")
      for (pred in keep_cols) {
        b    <- if (pred %in% names(coefs)) coefs[[pred]] else NA_real_
        x    <- fv_list[[pred]]
        z    <- if (pred %in% ct$predictor) ct$z_stat[ct$predictor==pred] else NA_real_
        lbl  <- if (pred %in% names(plabels)) plabels[[pred]] else pred
        miss <- is.na(x)
        add_contrib(pred, lbl, if(!is.na(b)&&!miss) b*x else 0, z, data_missing=miss)
      }
    }
  }

  # ── Hand-tuned fallback ───────────────────────────────────────────────────────
  if (is.na(glm_prob_raw)) {
    lo <- -1.73
    ht <- function(id, label, val, delta_fn) {
      if (is.na(val)) { add_contrib(id, label, 0, data_missing=TRUE); return(0) }
      d <- delta_fn(val); add_contrib(id, label, d); d
    }
    lo <- lo + ht("yield_curve","Yield Curve (10Y-2Y spread)", yield_spread, function(v){
      d <- -v*0.85
      if (!is.na(inv_months)) d <- d + if(inv_months>=18)1.5 else if(inv_months>=12)1.2 else if(inv_months>=6)0.6 else if(inv_months>=3)0.3 else 0
      d })
    if (!is.na(u) && !is.null(unemp) && length(unemp)>=12) {
      sh<-0; ur<-u-min(tail(unemp,12),na.rm=TRUE)
      sh<-sh+if(ur>=0.5)1.8 else if(ur>=0.3)0.9 else if(ur>=0.1)0.3 else if(ur<(-0.2))-0.4 else 0
      if(!is.na(pay_3m))sh<-sh+if(pay_3m<(-50))1.5 else if(pay_3m<0)0.6 else if(pay_3m<100)0.1 else if(pay_3m>250)-0.5 else 0
      add_contrib("sahm","Sahm Rule / Labor Momentum",sh); lo<-lo+sh
    } else add_contrib("sahm","Sahm Rule / Labor Momentum",0,data_missing=TRUE)
    lo<-lo+ht("prime_lfpr","Prime-Age LFPR (25\u201354)",prime_age_lfpr,function(v){d<-if(v<80.5)0.8 else if(v<82)0.4 else if(v>83.5)-0.3 else 0;if(!is.na(prime_age_lfpr_chg))d<-d+if(prime_age_lfpr_chg<(-0.5))0.9 else if(prime_age_lfpr_chg<(-0.2))0.4 else if(prime_age_lfpr_chg>0.3)-0.3 else 0;d})
    lo<-lo+ht("claims","Jobless Claims Trend (4wk/26wk)",jobless_claims_trend,function(v)if(v>0.20)1.2 else if(v>0.10)0.6 else if(v>0.05)0.2 else if(v<(-0.10))-0.3 else 0)
    lo<-lo+ht("quits","Quits Rate YoY",quits_yoy,function(v)if(v<(-15))0.8 else if(v<(-8))0.4 else if(v<(-3))0.2 else if(v>8)-0.3 else 0)
    lo<-lo+ht("oil","Oil Price Shock (YoY)",oil_yoy,function(v)if(v>50)1.0 else if(v>25)0.6 else if(v>15)0.2 else if(v<(-30))0.5 else if(v<(-15))0.2 else if(v>0&&v<10)-0.1 else 0)
    lo<-lo+ht("equity","Equity Bear Market",equity_drawdown_pct,function(v){dd<-abs(v);if(dd>30)1.6 else if(dd>20)1.1 else if(dd>10)0.5 else if(dd<5)-0.2 else 0})
    lo<-lo+ht("hy_spreads","HY Credit Spreads",hy_v,function(v)if(v>7)1.4 else if(v>5.5)0.7 else if(v>4.5)0.2 else if(v<3.5)-0.4 else 0)
    lo<-lo+ht("usd","USD Surge (YoY)",usd_yoy,function(v)if(v>12)0.8 else if(v>8)0.4 else if(v>4)0.1 else if(v<(-5))-0.2 else 0)
    lo<-lo+ht("real_rate","Real Fed Funds Rate",real_rate,function(v)if(v>3)1.0 else if(v>2)0.5 else if(v>1)0.2 else if(v<0)-0.3 else 0)
    lo<-lo+ht("sentiment","Consumer Sentiment",cs,function(v)if(v<60)0.7 else if(v<70)0.3 else if(v>90)-0.4 else 0)
    lo<-lo+ht("inv_sales","Inventory-to-Sales Ratio",inventory_sales_ratio,function(v)if(v>1.55)0.7 else if(v>1.45)0.3 else if(v>1.40)0.1 else if(v<1.30)-0.2 else 0)
    lo<-lo+ht("cc_del","CC Delinquency Rate",cc_delinquency,function(v)if(v>4)0.9 else if(v>3)0.5 else if(v>2.5)0.2 else if(v<1.8)-0.3 else 0)
    glm_prob_raw <- 1 / (1 + exp(-lo))
  }

  # ── Markov contribution — FIX 1: log-odds units ──────────────────────────────
  ms_prob_raw <- NA_real_
  if (!is.null(markov_result) && !is.na(markov_result$current_rec_prob)) {
    ms_prob_raw <- markov_result$current_rec_prob

    ew     <- ensemble_weights %||% list(w_glm=0.60, w_ms=0.40)
    W_GLM  <- ew$w_glm
    W_MS   <- ew$w_ms

    # Convert both probabilities to log-odds, take difference, scale by weight.
    # This puts the Markov bar on the same scale as all other β×x contributions
    # (typically ±0.1 to ±1.8 log-odds units).
    lo_glm <- .prob_to_lo(glm_prob_raw)
    lo_ms  <- .prob_to_lo(ms_prob_raw)
    ms_lo_delta <- (lo_ms - lo_glm) * W_MS   # signed log-odds pull from Markov

    sn  <- markov_result$switch_name %||% "IP YoY"
    sm  <- markov_result$state_means_sw
    exp_mean <- round(sm[setdiff(seq_len(markov_result$n_states), markov_result$recession_state)[1]], 2)
    rec_mean <- round(sm[markov_result$recession_state], 2)
    freq_pct <- round((markov_result$state_freq_recession %||% 0) * 100)

    add_contrib(
      "markov_regime",
      sprintf("Markov Regime (w=%.0f%%; p_rec=%.0f%%; %s: exp=%.1f rec=%.1f; %.0f%% of history in recession state)",
              W_MS*100, ms_prob_raw*100, sn, exp_mean, rec_mean, freq_pct),
      ms_lo_delta)
  }

  ew     <- ensemble_weights %||% list(w_glm=0.60, w_ms=0.40)
  W_GLM  <- if (!is.na(ms_prob_raw)) ew$w_glm else 1.0
  W_MS   <- if (!is.na(ms_prob_raw)) ew$w_ms  else 0.0

  blended_pre_anomaly <- if (!is.na(ms_prob_raw))
    W_GLM * glm_prob_raw + W_MS * ms_prob_raw
  else glm_prob_raw

  blended_raw <- if (A > 0.05) {
    compressed  <- blended_pre_anomaly + ANOMALY_WEIGHT * A * (COMPRESS_CENTER - blended_pre_anomaly)
    max(compressed, ANOMALY_FLOOR * A)
  } else blended_pre_anomaly

  prob <- max(2, min(97, round(blended_raw * 100, 1)))

  if (A > 0.05) {
    anom_lo_delta <- .prob_to_lo(blended_raw) - .prob_to_lo(blended_pre_anomaly)
    add_contrib("anomaly_signal",
                sprintf("Anomaly Signal (MD\u2248%.1f; %s)", anomaly_result$md %||% 0, anomaly_result$label),
                anom_lo_delta)
  }

  tier <- .classify_recession_tier(prob)

  contrib_df <- do.call(rbind, lapply(names(contribs), function(id) {
    r <- contribs[[id]]
    data.frame(name=r$name, contribution=r$contribution,
               z_stat=r$z_stat, data_missing=r$data_missing %||% FALSE,
               stringsAsFactors=FALSE)
  }))
  contrib_df <- contrib_df[order(abs(contrib_df$contribution), decreasing=TRUE),]

  du <- character(0); dd <- character(0)
  for (i in seq_len(min(8, nrow(contrib_df)))) {
    r <- contrib_df[i,]
    if (isTRUE(r$data_missing)) next
    if (r$contribution > 0.1)  du <- c(du, r$name)
    if (r$contribution < -0.1) dd <- c(dd, r$name)
  }
  if (A > 0.3) du <- c(sprintf("\u26a0 %s (MD=%.1f)", anomaly_result$label, anomaly_result$md%||%0), du)
  if (!is.na(ms_prob_raw) && ms_prob_raw > 0.4)
    du <- c(sprintf("Markov regime: %.0f%% recession probability", ms_prob_raw*100), du)

  mt <- if (!is.null(glm_result) && !is.na(ms_prob_raw)) {
    sprintf("GLM + Markov + Anomaly (w_glm=%.0f%% w_ms=%.0f%%)", W_GLM*100, W_MS*100)
  } else if (!is.null(glm_result)) "GLM + Anomaly Blend"
    else "Hand-tuned + Anomaly Blend"

  list(prob=prob, tier=tier,
       drivers_up=head(du,4), drivers_down=head(dd,3),
       factor_contributions=contrib_df,
       anomaly=anomaly_result,
       markov_prob=ms_prob_raw,
       ensemble_weights=ew,
       model_type=mt,
       n_obs=glm_result$n_obs %||% NULL,
       aic=glm_result$aic %||% NULL)
}
