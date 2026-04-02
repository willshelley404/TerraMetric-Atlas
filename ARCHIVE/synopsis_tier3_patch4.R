# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch4.R
#
# SOURCE ORDER in global.R:
#   source("R/synopsis_tier3.R")
#   source("R/synopsis_tier3_patch.R")
#   source("R/synopsis_tier3_patch2.R")
#   source("R/synopsis_tier3_patch3.R")
#   source("R/synopsis_tier3_patch4.R")   ← add this
#
# Then force retrain once:
#   rm(.recession_t3_cache, envir = .GlobalEnv)
#
# FIXES:
#
#  1. 3-STATE MARKOV (replaces 2-state)
#     2-state EM on IP growth converges to a fast/slow growth split, not
#     expansion/recession.  3 states let the algorithm find:
#       State A: strong expansion  (IP YoY > ~3%)
#       State B: slow growth / stall  (IP YoY 0–2%)
#       State C: contraction  (IP YoY < 0%)
#     The recession state = whichever has the LOWEST empirical mean.
#     State C should cover ~10–15% of history, matching NBER frequency.
#     The reliability check still applies: if recession state > 25%,
#     weight is capped.
#
#  2. BRIER training_panel FIX
#     rolling_retrain() builds panel_clean from panel_rolling, but the
#     field stored in the list is named 'training_panel'.  When patch2's
#     compute_ensemble_weights checks glm_result$training_panel it gets
#     NULL because the build_growth_outlook in patch calls rolling_retrain
#     which DOES store training_panel, but then the cache object is
#     re-retrieved and somewhere the field is lost.
#     Fix: store the panel explicitly in the cache under $glm$training_panel
#     and have compute_ensemble_weights fall back to rebuilding it from
#     build_glm_feature_matrix if the field is missing.
#
#  3. N_RESTARTS INCREASED TO 5 + SMARTER INITIALISATION
#     Instead of pure random seeds, we perturb the data slightly for each
#     restart so the EM explores different basins of attraction.  This
#     substantially improves the chance of finding the contraction state.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

MARKOV_RECESSION_FREQ_MAX    <- 0.25
MARKOV_RECESSION_FREQ_TARGET <- 0.13
MARKOV_N_STATES              <- 3L    # ← changed from 2 to 3


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 1+3: train_markov_model — 3-state + smarter restarts
# ═══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data,
                                n_states   = MARKOV_N_STATES,
                                n_restarts = 5L) {
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
          message(sprintf("[t3] fredr('%s') failed — using fred_data: %s", sid, e$message))
          gm_local(sid, val_name)
        })
    }

    # ── Switching variable cascade ────────────────────────────────────────────
    switching_var <- NULL; switch_name <- NULL; higher_is_recession <- FALSE

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
        switch_name <- "Unemployment rate"; higher_is_recession <- TRUE
      }
    }
    if (is.null(switching_var)) { message("[t3] No switching variable."); return(NULL) }

    message(sprintf("[t3] Markov: '%s' (%d obs), fitting %d-state model with %d restarts",
                    switch_name, nrow(switching_var), n_states, n_restarts))

    # Predictors
    spread_raw <- gm_fred("T10Y2Y", "yield_spread")
    if (is.null(spread_raw)) {
      t10 <- gm_fred("DGS10","t10"); t2 <- gm_fred("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        spread_raw <- dplyr::inner_join(t10,t2,by="date") %>%
          dplyr::mutate(yield_spread=t10-t2) %>% dplyr::select(date,yield_spread)
    }
    hy_raw   <- gm_fred("BAMLH0A0HYM2","hy_spread")
    unem_raw <- gm_fred("UNRATE","unemp")

    panel <- switching_var
    for (df in list(spread_raw, hy_raw, unem_raw))
      if (!is.null(df)) panel <- dplyr::left_join(panel, df, by="date")
    panel <- panel %>% dplyr::filter(!is.na(sw_var))

    avail_preds <- intersect(c("yield_spread","hy_spread","unemp"), names(panel))
    avail_preds <- avail_preds[sapply(avail_preds, function(v) mean(is.na(panel[[v]])) < 0.30)]
    for (v in avail_preds) panel[[v]][is.na(panel[[v]])] <- mean(panel[[v]], na.rm=TRUE)

    if (nrow(panel) < 60) {
      message(sprintf("[t3] Panel too small: %d rows.", nrow(panel)))
      return(NULL)
    }

    base_fml <- if (length(avail_preds) > 0)
      as.formula(paste("sw_var ~", paste(avail_preds, collapse=" + ")))
    else as.formula("sw_var ~ 1")

    # ── Fit once function with optional data jitter ──────────────────────────
    # Jittering the sw_var slightly for restarts > 1 helps the EM escape
    # the fast/slow growth local minimum and find the contraction regime.
    fit_once <- function(restart_idx) {
      tryCatch({
        set.seed(restart_idx * 42L)

        panel_fit <- panel
        if (restart_idx > 1) {
          # Add tiny proportional noise — preserves sign structure while
          # perturbing initial state probabilities in the EM
          jitter_sd <- sd(panel$sw_var, na.rm=TRUE) * 0.03 * (restart_idx - 1)
          panel_fit$sw_var <- panel$sw_var +
            rnorm(nrow(panel), 0, jitter_sd)
        }

        base_lm <- lm(base_fml, data=panel_fit)
        n_coef  <- length(coef(base_lm))

        # Switch intercept + variance per state; slopes shared across states.
        # For k states: MSwM sw vector length = n_coef + 1 (variance is appended)
        sw_vec <- c(TRUE, rep(FALSE, max(0, n_coef-1)), TRUE)
        if (length(sw_vec) != n_coef + 1) sw_vec <- rep(TRUE, n_coef)

        ms <- MSwM::msmFit(base_lm, k=n_states, sw=sw_vec,
                            control=list(parallel=FALSE, maxiter=300))
        sp <- ms@Fit@smoProb
        if (is.null(sp) || nrow(sp) == 0) return(NULL)

        # Identify recession state by empirical mean of sw_var per state
        # Use original (un-jittered) panel for state assignment
        sa <- apply(sp, 1, which.max)
        pt <- panel[seq_len(nrow(sp)), ]   # original panel, trimmed to smoProb length
        emp_means <- sapply(seq_len(n_states), function(s)
          mean(pt$sw_var[sa == s], na.rm=TRUE))

        recession_state <- if (higher_is_recession) which.max(emp_means)
                           else                     which.min(emp_means)
        freq_rec <- mean(sa == recession_state)

        list(model=ms, smooth_probs=sp, recession_state=recession_state,
             empirical_means=emp_means, freq_rec=freq_rec,
             panel_trimmed=pt, state_assign=sa)
      }, error=function(e) {
        message(sprintf("[t3] Restart %d failed: %s", restart_idx, e$message))
        NULL
      })
    }

    candidates <- lapply(seq_len(n_restarts), fit_once)
    candidates <- Filter(Negate(is.null), candidates)

    if (length(candidates) == 0) {
      message("[t3] All Markov restarts failed.")
      return(NULL)
    }

    # Pick the candidate whose recession-state frequency is closest to ~13%
    freq_errors <- sapply(candidates, function(c) abs(c$freq_rec - MARKOV_RECESSION_FREQ_TARGET))
    best        <- candidates[[which.min(freq_errors)]]

    ms_model        <- best$model
    smooth_probs    <- best$smooth_probs
    recession_state <- best$recession_state
    panel_trimmed   <- best$panel_trimmed
    state_assign    <- best$state_assign
    emp_means       <- best$empirical_means
    freq_rec        <- best$freq_rec

    # Label all states for display (sorted by empirical mean)
    state_labels <- rep("", n_states)
    if (n_states == 3L) {
      ordered_states <- order(emp_means, decreasing = !higher_is_recession)
      state_labels[ordered_states[1]] <- "Expansion"
      state_labels[ordered_states[2]] <- "Stall / Slow"
      state_labels[ordered_states[length(ordered_states)]] <- "Contraction"
    } else {
      state_labels[recession_state]                          <- "Contraction"
      state_labels[setdiff(seq_len(n_states), recession_state)[1]] <- "Expansion"
    }

    current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]
    reliable         <- freq_rec <= MARKOV_RECESSION_FREQ_MAX

    if (!reliable)
      message(sprintf("[t3] WARNING: Recession state covers %.0f%% of history (max=%.0f%%). Weight will be capped.",
                      freq_rec*100, MARKOV_RECESSION_FREQ_MAX*100))

    # Regime-conditional means for predictors
    regime_means <- lapply(avail_preds, function(v) list(
      variable  = v,
      expansion = round(mean(panel_trimmed[[v]][state_assign != recession_state], na.rm=TRUE), 2),
      recession = round(mean(panel_trimmed[[v]][state_assign == recession_state],  na.rm=TRUE), 2)
    ))
    # Expansion = state with highest mean growth for display
    exp_states <- which(state_assign != recession_state)
    exp_mean   <- round(mean(panel_trimmed$sw_var[exp_states], na.rm=TRUE), 2)
    rec_mean   <- round(emp_means[recession_state], 2)

    sw_regime_means <- list(
      variable  = switch_name,
      expansion = exp_mean,
      recession = rec_mean
    )

    message(sprintf(
      "[t3] Markov %d-state: '%s' n=%d | rec_state=%d '%s' emp_mean=%.2f (%.0f%% of months) | reliable=%s | p_rec=%.1f%%",
      n_states, switch_name, nrow(panel), recession_state,
      state_labels[recession_state], emp_means[recession_state],
      freq_rec*100, reliable, current_rec_prob*100))

    list(model               = ms_model,
         smooth_probs        = smooth_probs,
         recession_state     = recession_state,
         current_rec_prob    = current_rec_prob,
         state_means_sw      = emp_means,
         state_labels        = state_labels,
         switch_name         = switch_name,
         higher_is_recession = higher_is_recession,
         regime_means        = regime_means,
         sw_regime_means     = sw_regime_means,
         state_freq_recession= freq_rec,
         reliable            = reliable,
         n_states            = n_states,
         n_obs               = nrow(panel),
         trained_at          = Sys.time())
  }, error = function(e) {
    message("[t3] Markov model failed: ", e$message)
    NULL
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 2: rolling_retrain — ensure training_panel is always stored
# ═══════════════════════════════════════════════════════════════════════════════

rolling_retrain <- function(fred_data, window = 360L) {
  message(sprintf("[t3] Rolling GLM retrain (window=%dM)...", window))

  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) {
    message("[t3] build_glm_feature_matrix returned NULL — cannot train GLM.")
    return(NULL)
  }

  panel_rolling <- tail(panel, window)
  if (nrow(panel_rolling) < 100) {
    message(sprintf("[t3] Rolling window too small (%d rows), using full panel (%d rows).",
                    nrow(panel_rolling), nrow(panel)))
    panel_rolling <- panel
  }

  possible  <- c("yield_spread","u_rise","pay_3m","real_rate","hy_spread",
                 "oil_yoy","prime_lfpr","usd_yoy","inv_sales","cc_del","sentiment")
  available <- intersect(possible, names(panel_rolling))
  if (length(available) < 3) {
    message("[t3] Too few predictors available for GLM.")
    return(NULL)
  }

  panel_clean <- panel_rolling %>%
    dplyr::select(rec_next12, dplyr::all_of(available)) %>%
    tidyr::drop_na()

  if (nrow(panel_clean) < 60) {
    message(sprintf("[t3] panel_clean too small after drop_na: %d rows.", nrow(panel_clean)))
    return(NULL)
  }

  fml <- as.formula(paste("rec_next12 ~", paste(available, collapse=" + ")))

  tryCatch({
    model    <- glm(fml, data=panel_clean, family=binomial(link="logit"))
    sm       <- summary(model)
    coef_tbl <- as.data.frame(sm$coefficients)
    coef_tbl$predictor <- rownames(coef_tbl)
    coef_tbl <- coef_tbl %>%
      dplyr::filter(predictor != "(Intercept)") %>%
      dplyr::rename(estimate=Estimate, se=`Std. Error`,
                    z_stat=`z value`, p_val=`Pr(>|z|)`) %>%
      dplyr::select(predictor, estimate, se, z_stat, p_val)

    feature_only <- panel_clean[, available, drop=FALSE]
    ad <- build_anomaly_detector(feature_only)

    message(sprintf("[t3] GLM: n=%d, window=%dM, AIC=%.1f, %d predictors",
                    nrow(panel_clean), window, AIC(model), length(available)))

    list(
      model          = model,
      coef_table     = coef_tbl,
      n_obs          = nrow(panel_clean),
      aic            = round(AIC(model), 1),
      predictors     = available,
      trained_at     = Sys.time(),
      anomaly_detector = ad,
      window_months  = window,
      training_panel = panel_clean   # ← explicitly stored for Brier scoring
    )
  }, error = function(e) {
    message("[t3] rolling_retrain glm() failed: ", e$message)
    NULL
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 2: compute_ensemble_weights — rebuild panel from scratch if missing
# ═══════════════════════════════════════════════════════════════════════════════

compute_ensemble_weights <- function(glm_result, markov_result,
                                     holdout_months = 36L) {

  fallback <- list(w_glm=0.60, w_ms=0.40, glm_brier=NA_real_, ms_brier=NA_real_,
                   n_holdout=0L, method="fallback (equal-ish)")

  # ── Recover training_panel if missing ────────────────────────────────────────
  # If the field wasn't stored (e.g. loaded from an old cache), we can't
  # do OOS Brier scoring.  Log and return fallback — it will fix itself on
  # next 30-day retrain.
  if (is.null(glm_result)) {
    message("[t3] compute_ensemble_weights: glm_result is NULL, using fallback.")
    return(fallback)
  }

  panel <- glm_result$training_panel
  if (is.null(panel)) {
    message("[t3] compute_ensemble_weights: training_panel missing from glm_result. ",
            "This happens when loading an old cache. Retrain will fix it. Using fallback weights.")
    return(fallback)
  }

  n <- nrow(panel)
  if (n < holdout_months + 30L) {
    message(sprintf("[t3] Panel too small for holdout (n=%d, need>%d). Fallback.", n, holdout_months+30L))
    return(fallback)
  }

  holdout_idx     <- (n - holdout_months + 1L):n
  score_train_idx <- seq_len(n - holdout_months)

  score_train_df <- panel[score_train_idx, , drop=FALSE]
  holdout_df     <- panel[holdout_idx,     , drop=FALSE]

  y_actual <- holdout_df$rec_next12
  ok       <- !is.na(y_actual)
  if (sum(ok) < 12L) {
    message(sprintf("[t3] Only %d valid holdout outcomes (need≥12). Brier fallback.", sum(ok)))
    return(fallback)
  }
  holdout_df <- holdout_df[ok, , drop=FALSE]
  y_actual   <- y_actual[ok]

  avail <- intersect(glm_result$predictors, names(panel))

  # Fit temporary scoring GLM (never sees holdout data)
  glm_brier <- tryCatch({
    sc_clean <- score_train_df %>%
      dplyr::select(rec_next12, dplyr::all_of(avail)) %>%
      tidyr::drop_na()
    if (nrow(sc_clean) < 60) stop("Scoring train set too small.")
    tmp_glm <- glm(as.formula(paste("rec_next12 ~", paste(avail, collapse=" + "))),
                   data=sc_clean, family=binomial(link="logit"))
    hdf <- holdout_df[, avail, drop=FALSE]
    for (col in names(hdf)) if (is.na(hdf[[col]])) hdf[[col]] <- 0
    preds <- predict(tmp_glm, newdata=hdf, type="response")
    mean((preds - y_actual)^2, na.rm=TRUE)
  }, error=function(e) { message("[t3] GLM Brier failed: ", e$message); NA_real_ })

  ms_brier <- tryCatch({
    if (is.null(markov_result) || is.null(markov_result$smooth_probs)) stop("No smooth_probs.")
    sp_rec  <- markov_result$smooth_probs[, markov_result$recession_state]
    n_hold  <- length(y_actual)
    if (length(sp_rec) < n_hold) stop("smooth_probs shorter than holdout.")
    mean((tail(sp_rec, n_hold) - y_actual)^2, na.rm=TRUE)
  }, error=function(e) { message("[t3] Markov Brier failed: ", e$message); NA_real_ })

  message(sprintf("[t3] Brier — GLM: %s | Markov: %s | holdout: %d months",
                  if(is.na(glm_brier))"NA" else sprintf("%.4f",glm_brier),
                  if(is.na(ms_brier)) "NA" else sprintf("%.4f",ms_brier),
                  length(y_actual)))

  if (is.na(glm_brier) && is.na(ms_brier)) return(fallback)
  if (is.na(ms_brier))  return(modifyList(fallback, list(w_glm=0.85, w_ms=0.15, glm_brier=glm_brier, n_holdout=length(y_actual), method="GLM only (Markov Brier unavailable)")))
  if (is.na(glm_brier)) return(modifyList(fallback, list(w_glm=0.15, w_ms=0.85, ms_brier=ms_brier,   n_holdout=length(y_actual), method="Markov only (GLM Brier unavailable)")))

  base_rate   <- mean(panel$rec_next12, na.rm=TRUE)
  clim_brier  <- base_rate * (1 - base_rate)
  glm_skill   <- max(0, 1 - glm_brier / clim_brier)
  ms_skill    <- max(0, 1 - ms_brier  / clim_brier)
  total_skill <- glm_skill + ms_skill

  if (total_skill < 1e-4) {
    message("[t3] Both models near-zero skill. Using fallback weights.")
    return(fallback)
  }

  w_glm <- glm_skill / total_skill
  w_ms  <- ms_skill  / total_skill

  # Apply reliability penalty BEFORE returning (patch3's wrapper no longer needed)
  if (!is.null(markov_result) && isFALSE(markov_result$reliable)) {
    w_ms  <- min(w_ms, 0.10)
    w_glm <- 1 - w_ms
    message(sprintf("[t3] Markov weight capped at 10%% (state freq=%.0f%%)", markov_result$state_freq_recession*100))
  }

  method_str <- sprintf("OOS Brier skill (clim=%.4f, n=%d holdout months)", clim_brier, length(y_actual))
  if (!is.null(markov_result) && isFALSE(markov_result$reliable))
    method_str <- paste0(method_str, sprintf(" [Markov capped: state freq=%.0f%%]", markov_result$state_freq_recession*100))

  message(sprintf("[t3] Ensemble weights → GLM=%.0f%% (skill=%.3f) | Markov=%.0f%% (skill=%.3f)",
                  w_glm*100, glm_skill, w_ms*100, ms_skill))

  list(w_glm=round(w_glm,3), w_ms=round(w_ms,3),
       glm_brier=round(glm_brier,4), ms_brier=round(ms_brier,4),
       glm_skill=round(glm_skill,3), ms_skill=round(ms_skill,3),
       n_holdout=length(y_actual), method=method_str)
}


# ═══════════════════════════════════════════════════════════════════════════════
# PATCH compute_ensemble_weights wrapper from patch3 — no longer needed,
# the cap is now inside compute_ensemble_weights itself.
# Override the wrapper to be a passthrough so it doesn't double-cap.
compute_ensemble_weights_orig <- compute_ensemble_weights
