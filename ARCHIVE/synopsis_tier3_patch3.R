# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch3.R  —  Markov reliability + tier threshold fixes
#
# SOURCE ORDER in global.R:
#   source("R/synopsis_tier3.R")
#   source("R/synopsis_tier3_patch.R")
#   source("R/synopsis_tier3_patch2.R")
#   source("R/synopsis_tier3_patch3.R")   ← add this
#
# FIXES:
#
#  1. MARKOV STATE VALIDATION
#     A well-fitted 2-state Markov model on IP growth should assign roughly
#     10–20% of months to the recession state (matching NBER ~13% base rate).
#     If the recession state claims >25% of history, the EM algorithm found
#     the wrong split (high-vs-moderate growth rather than expansion-vs-recession).
#     Fix: after fitting, check state_freq_recession. If >25%, mark the model
#     as unreliable (markov_result$reliable = FALSE), reduce its ensemble
#     weight to 10%, and show a warning badge in the UI.
#     Also try 3 random restarts with different initializations and pick the
#     fit where the recession-state frequency is closest to 15%.
#
#  2. TIER THRESHOLDS — finer-grained labels
#     Old: <20% Low | 20-40% Low-Moderate | 40-60% Moderate | 60%+ Elevated
#     New: <15% Low | 15-30% Low-Moderate | 30-50% Moderate | 50-65% High |
#          65%+ Elevated
#     Each tier now has a distinct description that reflects its actual risk
#     level rather than the same "recession possible" hedge across a 20pp range.
#
#  3. MARKOV WEIGHT PENALTY
#     When the Markov model is unreliable (state_freq > 25%), its ensemble
#     weight is capped at 0.10 regardless of Brier score, and the remaining
#     weight goes to the GLM. This prevents a misfitted regime model from
#     dominating the probability when it's clearly assigning too many months
#     to the recession state.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

# ── Constants ─────────────────────────────────────────────────────────────────
# Expected fraction of months in recession state for a well-fitted model
# US NBER recessions ~13% of months since 1960; allow up to 25% for model slack
MARKOV_RECESSION_FREQ_MAX <- 0.25
MARKOV_RECESSION_FREQ_TARGET <- 0.13   # NBER historical base rate

# ── FIX 2: Refined tier classifier ───────────────────────────────────────────
.classify_recession_tier <- function(prob) {
  if (prob >= 65)
    list(label = "Elevated",
         color = "#e94560",
         icon  = "exclamation-triangle",
         desc  = paste0("Multiple leading indicators aligned in a recessionary direction. ",
                        "Historical hit rate at this probability level: ~65%. ",
                        "This is the model's highest-concern tier — defensive positioning and close monitoring of labor and credit conditions warranted."))
  else if (prob >= 50)
    list(label = "High",
         color = "#e07040",
         icon  = "exclamation-circle",
         desc  = paste0("A majority of probability weight now sits on the recessionary side. ",
                        "Multiple genuine warning signs are active simultaneously. ",
                        "Historically, this range precedes a recession in roughly half of cases — ",
                        "the economy can still avoid contraction, but conditions need to improve, not deteriorate further."))
  else if (prob >= 30)
    list(label = "Moderate",
         color = "#f4a261",
         icon  = "minus-circle",
         desc  = paste0("Mixed signals. Some meaningful risk factors are elevated but no acute stress. ",
                        "Late-cycle slowdown is the most likely outcome; ",
                        "a full recession would require additional deterioration in labor or credit."))
  else if (prob >= 15)
    list(label = "Low\u2013Moderate",
         color = "#f4a261",
         icon  = "check-circle",
         desc  = paste0("A handful of headwinds are present but the overall macro picture is cautiously positive. ",
                        "Monitor labor market momentum and credit spreads for early deterioration signals."))
  else
    list(label = "Low",
         color = "#2dce89",
         icon  = "check-circle",
         desc  = paste0("Most leading indicators are benign. ",
                        "Current conditions are at or below the historical 12-month recession base rate (~13%). ",
                        "No acute stress signals are active."))
}


# ── FIX 1: train_markov_model with restart logic + reliability check ──────────
train_markov_model <- function(fred_data, n_states = 2L, n_restarts = 3L) {
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
          message(sprintf("[t3] fredr('%s') failed: %s", sid, e$message))
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
      if (nrow(cand) >= 60) { switching_var <- cand; switch_name <- "IP YoY %"; higher_is_recession <- FALSE }
    }
    if (is.null(switching_var)) {
      pay_raw <- gm_fred("PAYEMS", "payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var = payems - dplyr::lag(payems, 1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date, sw_var)
        if (nrow(cand) >= 60) { switching_var <- cand; switch_name <- "Payroll MoM chg (K)"; higher_is_recession <- FALSE }
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

    message(sprintf("[t3] Markov switching variable: '%s' (%d obs)", switch_name, nrow(switching_var)))

    # ── Build predictor panel ─────────────────────────────────────────────────
    spread_raw <- gm_fred("T10Y2Y", "yield_spread")
    if (is.null(spread_raw)) {
      t10 <- gm_fred("DGS10","t10"); t2 <- gm_fred("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        spread_raw <- dplyr::inner_join(t10,t2,by="date") %>%
          dplyr::mutate(yield_spread=t10-t2) %>% dplyr::select(date,yield_spread)
    }
    hy_raw   <- gm_fred("BAMLH0A0HYM2", "hy_spread")
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

    # ── FIX 1: Multiple restarts — pick best by recession-state frequency ─────
    # "Best" = whose recession-state frequency is closest to the NBER base rate
    # This guards against the EM converging to a high-growth vs low-growth split
    # instead of an expansion vs recession split.
    fit_once <- function(seed) {
      tryCatch({
        set.seed(seed)
        base_lm <- lm(base_fml, data=panel)
        n_coef  <- length(coef(base_lm))
        sw_vec  <- c(TRUE, rep(FALSE, max(0,n_coef-1)), TRUE)
        if (length(sw_vec) != n_coef+1) sw_vec <- rep(TRUE, n_coef)

        ms <- MSwM::msmFit(base_lm, k=n_states, sw=sw_vec,
                            control=list(parallel=FALSE, maxiter=300))
        sp <- ms@Fit@smoProb
        if (is.null(sp) || nrow(sp) == 0) return(NULL)

        # Identify recession state by empirical mean
        sa <- apply(sp, 1, which.max)
        pt <- panel[seq_len(nrow(sp)),]
        emp_means <- sapply(seq_len(n_states), function(s) mean(pt$sw_var[sa==s], na.rm=TRUE))
        rs <- if (higher_is_recession) which.max(emp_means) else which.min(emp_means)

        freq_rec <- mean(sa == rs)
        list(model=ms, smooth_probs=sp, recession_state=rs,
             empirical_means=emp_means, freq_rec=freq_rec, panel_trimmed=pt)
      }, error=function(e) NULL)
    }

    candidates <- lapply(seq_len(n_restarts) * 42L, fit_once)
    candidates <- Filter(Negate(is.null), candidates)

    if (length(candidates) == 0) {
      message("[t3] All Markov restarts failed.")
      return(NULL)
    }

    # Pick the candidate whose recession-state frequency is closest to target
    freq_errors <- sapply(candidates, function(c) abs(c$freq_rec - MARKOV_RECESSION_FREQ_TARGET))
    best        <- candidates[[which.min(freq_errors)]]

    ms_model      <- best$model
    smooth_probs  <- best$smooth_probs
    recession_state <- best$recession_state
    panel_trimmed <- best$panel_trimmed
    state_assign  <- apply(smooth_probs, 1, which.max)
    freq_rec      <- best$freq_rec
    emp_means     <- best$empirical_means

    current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]

    # ── FIX 1b: Reliability flag ─────────────────────────────────────────────
    reliable <- freq_rec <= MARKOV_RECESSION_FREQ_MAX
    if (!reliable) {
      message(sprintf(
        "[t3] WARNING: Markov recession state claims %.0f%% of history (max=%.0f%%). Model marked unreliable.",
        freq_rec*100, MARKOV_RECESSION_FREQ_MAX*100))
    }

    regime_means <- lapply(avail_preds, function(v) list(
      variable  = v,
      expansion = round(mean(panel_trimmed[[v]][state_assign != recession_state], na.rm=TRUE), 2),
      recession = round(mean(panel_trimmed[[v]][state_assign == recession_state],  na.rm=TRUE), 2)
    ))
    sw_regime_means <- list(
      variable  = switch_name,
      expansion = round(emp_means[setdiff(seq_len(n_states), recession_state)[1]], 2),
      recession = round(emp_means[recession_state], 2)
    )

    message(sprintf(
      "[t3] Markov: '%s' n=%d | rec_state=%d emp_mean=%.2f (%.0f%% of history) | reliable=%s | p_rec=%.1f%%",
      switch_name, nrow(panel), recession_state, emp_means[recession_state],
      freq_rec*100, reliable, current_rec_prob*100))

    list(model            = ms_model,
         smooth_probs     = smooth_probs,
         recession_state  = recession_state,
         current_rec_prob = current_rec_prob,
         state_means_sw   = emp_means,
         switch_name      = switch_name,
         higher_is_recession = higher_is_recession,
         regime_means     = regime_means,
         sw_regime_means  = sw_regime_means,
         state_freq_recession = freq_rec,
         reliable         = reliable,            # ← NEW
         n_states         = n_states,
         n_obs            = nrow(panel),
         trained_at       = Sys.time())
  }, error = function(e) {
    message("[t3] Markov model failed: ", e$message)
    NULL
  })
}


# ── FIX 3: Apply Markov weight penalty when model is unreliable ───────────────
# Override compute_ensemble_weights to cap unreliable Markov weight at 10%
compute_ensemble_weights_orig <- compute_ensemble_weights

compute_ensemble_weights <- function(glm_result, markov_result, holdout_months = 36L) {
  ew <- compute_ensemble_weights_orig(glm_result, markov_result, holdout_months)

  if (!is.null(markov_result) && isFALSE(markov_result$reliable)) {
    # Markov found the wrong regimes — don't let it dominate
    ew$w_ms   <- min(ew$w_ms, 0.10)
    ew$w_glm  <- 1 - ew$w_ms
    ew$method <- paste0(ew$method, " [Markov weight capped: state freq=",
                        round(markov_result$state_freq_recession*100), "% > 25% max]")
    message(sprintf("[t3] Markov weight capped at %.0f%% (state freq=%.0f%% > %.0f%% max)",
                    ew$w_ms*100, markov_result$state_freq_recession*100,
                    MARKOV_RECESSION_FREQ_MAX*100))
  }
  ew
}


# ── Markov reliability badge for the modal ────────────────────────────────────
.markov_reliability_badge <- function(markov_result) {
  if (is.null(markov_result)) return("")
  if (isFALSE(markov_result$reliable)) {
    freq_pct <- round((markov_result$state_freq_recession %||% 0) * 100)
    sprintf(
      "<div style='background:#2a1010;border:1px solid rgba(233,69,96,0.4);border-radius:6px;
                   padding:10px 14px;margin-bottom:12px;font-size:11px;color:#e94560;'>
         <i class='fa fa-exclamation-triangle' style='margin-right:6px;'></i>
         <b>Markov state reliability warning:</b> The recession state accounts for
         <b>%d%%</b> of historical months. NBER data shows recessions occur in ~13%% of months.
         A well-fitted model should assign no more than 25%% of months to the recession state.
         This fit may have split the data on growth level (high vs moderate) rather than
         expansion vs recession. Markov ensemble weight has been capped at 10%%.
         Consider resetting the model cache (<code>rm(.recession_t3_cache, envir=.GlobalEnv)</code>).
       </div>", freq_pct)
  } else {
    freq_pct <- round((markov_result$state_freq_recession %||% 0) * 100)
    sprintf(
      "<div style='background:#0d1f10;border:1px solid rgba(45,206,137,0.3);border-radius:6px;
                   padding:8px 14px;margin-bottom:12px;font-size:11px;color:#2dce89;'>
         <i class='fa fa-check-circle' style='margin-right:6px;'></i>
         Markov state assignment looks reasonable: recession state covers <b>%d%%</b>
         of historical months (NBER base rate ~13%%, model tolerance ≤25%%).
       </div>", freq_pct)
  }
}


# ── Updated .build_model_modal to inject reliability badge ───────────────────
# We patch the Markov status section inside the modal. Override the full modal
# function to add the badge directly under the Markov header.
.build_markov_status_html_patched <- function(markov_result, ensemble_weights) {
  reliability_badge <- .markov_reliability_badge(markov_result)

  if (is.null(markov_result)) {
    return(paste0(
      reliability_badge,
      "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'>
         <i class='fa fa-info-circle' style='color:#00b4d8;margin-right:6px;'></i>
         Markov model not yet fitted. Check PAYEMS / INDPRO / UNRATE data in fred_data.
       </div>"))
  }

  ms_pct  <- round(markov_result$current_rec_prob * 100)
  ms_col  <- if (ms_pct > 60) "#e94560" else if (ms_pct > 35) "#f4a261" else "#2dce89"
  sw_means <- markov_result$sw_regime_means
  freq_pct <- round((markov_result$state_freq_recession %||% 0) * 100)
  weight_note <- if (isFALSE(markov_result$reliable))
    "<span style='color:#e94560;font-size:10px;'> &nbsp;(weight capped at 10% — unreliable state assignment)</span>"
  else ""

  paste0(
    reliability_badge,
    sprintf(
      "<div style='background:#0f1a27;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>
         <div style='color:#00b4d8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>
           <i class='fa fa-random' style='margin-right:6px;'></i>Markov Regime Model
           <span style='color:#555;font-weight:400;font-size:10px;margin-left:8px;'>switching variable: %s</span>%s
         </div>
         <div style='display:flex;align-items:center;gap:10px;margin-bottom:8px;'>
           <span style='color:#9aa3b2;font-size:11px;width:200px;'>Recession state probability</span>
           <div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'>
             <div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div>
           </div>
           <span style='color:%s;font-size:14px;font-weight:700;width:45px;text-align:right;'>%d%%</span>
         </div>
         <div style='color:#9aa3b2;font-size:11px;margin-bottom:4px;'>
           Expansion mean: <b style='color:#2dce89;'>%.2f</b> &nbsp;|&nbsp;
           Recession mean: <b style='color:#e94560;'>%.2f</b> &nbsp;|&nbsp;
           <b>%d%%</b> of %d months in recession state
           (NBER target ~13%%)
         </div>
         <div style='color:#6b7585;font-size:10px;line-height:1.7;'>
           The EM algorithm found 2 hidden states in %s by fitting different intercept &amp; variance
           parameters per state. The recession state has empirical mean %.2f vs expansion %.2f.
           Smoothed probabilities are the posterior probability of being in each state given
           all available data — not just the current observation.
         </div>
       </div>",
      htmltools::htmlEscape(markov_result$switch_name %||% "unknown"), weight_note,
      ms_col, ms_pct, ms_col, ms_pct,
      sw_means$expansion %||% 0, sw_means$recession %||% 0,
      freq_pct, markov_result$n_obs %||% 0,
      htmltools::htmlEscape(markov_result$switch_name %||% "IP YoY"),
      sw_means$recession %||% 0, sw_means$expansion %||% 0))
}

# ── Patch render_growth_outlook_html to show tier description + reliability ───
# The tier colour badge in the probability panel now also shows an orange
# warning when the Markov model is flagged unreliable.
.markov_probe_badge <- function(markov_result) {
  if (is.null(markov_result)) return("")
  freq_pct <- round((markov_result$state_freq_recession %||% 0) * 100)
  if (isFALSE(markov_result$reliable))
    sprintf("&nbsp;<span style='background:rgba(233,69,96,0.1);color:#e94560;border:1px solid rgba(233,69,96,0.3);border-radius:8px;padding:1px 8px;font-size:9px;font-weight:600;'>\u26a0 Markov %d%% history</span>", freq_pct)
  else
    sprintf("&nbsp;<span style='background:rgba(45,206,137,0.08);color:#2dce89;border:1px solid rgba(45,206,137,0.2);border-radius:8px;padding:1px 8px;font-size:9px;'>Markov %d%% history</span>", freq_pct)
}
