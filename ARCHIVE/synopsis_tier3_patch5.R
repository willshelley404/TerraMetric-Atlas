# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch5.R
#
# SOURCE ORDER in global.R (add this last):
#   source("R/synopsis_tier3_patch5.R")
#
# FIXES:
#
#  1. MARKOV SINGULARITY — drop all covariates, use intercept-only model.
#     "Lapack dgesv: system is exactly singular" happens because with 3 states
#     and 3+ predictors, some states get too few observations during EM
#     iteration and their within-state design matrix becomes rank-deficient.
#     The solution is a pure Hamilton (1989) regime-switching model:
#       lm(sw_var ~ 1)  +  sw = c(TRUE, TRUE)
#     Each state estimates only its own mean (intercept) and variance.
#     That is 2 parameters × 3 states = 6, estimated from 794 obs.
#     No matrix inversion can fail. The 3 state means are exactly what we
#     need: expansion (high IP growth), stall (low-positive), contraction
#     (negative). Covariates are not needed to identify the regimes —
#     the switching variable itself carries all the signal.
#
#  2. GLM NULL / build_glm_feature_matrix USREC FALLBACK
#     build_glm_feature_matrix calls fredr::fredr("USREC") to get the NBER
#     recession indicator. If fredr is rate-limited, the API key is missing,
#     or the call simply times out, the function returns NULL and the entire
#     GLM training chain fails silently.
#     Fix: wrap the USREC call in a robust fallback chain:
#       a) fredr::fredr("USREC")                   — primary
#       b) Construct from INDPRO + UNRATE in fred_data if fredr fails
#          (Sahm-like rule: UNRATE rise > 0.5pp from 12M trough → recession)
#     If both fail, log clearly and return NULL so the hand-tuned path
#     activates gracefully rather than the app appearing to hang.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b


# ═══════════════════════════════════════════════════════════════════════════════
# FIX 1: train_markov_model — intercept-only, numerically bulletproof
# ═══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data,
                                n_states   = 3L,
                                n_restarts = 5L) {
  tryCatch({
    gm_local <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(date = month)
    }
    gm_fred <- function(sid, val_name, start = as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid, observation_start = start, frequency = "m") %>%
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
    if (is.null(switching_var)) {
      message("[t3] No switching variable available for Markov model.")
      return(NULL)
    }

    panel <- switching_var %>% dplyr::filter(!is.na(sw_var))
    if (nrow(panel) < 60) {
      message(sprintf("[t3] Markov panel too small: %d rows.", nrow(panel)))
      return(NULL)
    }

    message(sprintf("[t3] Markov: '%s' (%d obs), fitting %d-state intercept-only model, %d restarts",
                    switch_name, nrow(panel), n_states, n_restarts))

    # ── FIX 1: Intercept-only base model ─────────────────────────────────────
    # sw_var ~ 1   →  n_coef = 1 (just the intercept)
    # sw = c(TRUE, TRUE)  →  intercept switches + variance switches per state
    # Each state estimates: mean of sw_var + variance of sw_var
    # Parameters: n_states × 2 + transition_matrix  ≈ 12 free params for k=3
    # No within-state design matrix inversion → singularity impossible.
    base_lm <- lm(sw_var ~ 1, data = panel)

    # MSwM sw vector: one entry per coefficient + one for variance
    # length must equal n_coef + 1 = 2
    sw_vec <- c(TRUE, TRUE)   # intercept switches, variance switches

    fit_once <- function(restart_idx) {
      tryCatch({
        set.seed(restart_idx * 37L)

        # Perturb sw_var slightly for restarts > 1 to escape local optima
        panel_fit <- panel
        if (restart_idx > 1) {
          jitter_sd <- sd(panel$sw_var, na.rm = TRUE) * 0.04 * (restart_idx - 1)
          panel_fit$sw_var <- panel$sw_var + rnorm(nrow(panel), 0, jitter_sd)
        }

        base_lm_fit <- lm(sw_var ~ 1, data = panel_fit)
        ms <- MSwM::msmFit(base_lm_fit, k = n_states, sw = sw_vec,
                            control = list(parallel = FALSE, maxiter = 500))

        sp <- ms@Fit@smoProb
        if (is.null(sp) || nrow(sp) == 0) return(NULL)

        # Assign each month to its most probable state
        sa <- apply(sp, 1, which.max)
        # Use original (un-jittered) panel for state means
        pt <- panel[seq_len(nrow(sp)), ]
        emp_means <- sapply(seq_len(n_states), function(s)
          mean(pt$sw_var[sa == s], na.rm = TRUE))

        recession_state <- if (higher_is_recession) which.max(emp_means)
                           else                     which.min(emp_means)
        freq_rec <- mean(sa == recession_state)

        list(model = ms, smooth_probs = sp, recession_state = recession_state,
             empirical_means = emp_means, freq_rec = freq_rec,
             panel_trimmed = pt, state_assign = sa)
      }, error = function(e) {
        message(sprintf("[t3] Restart %d failed: %s", restart_idx, e$message))
        NULL
      })
    }

    candidates <- Filter(Negate(is.null), lapply(seq_len(n_restarts), fit_once))

    if (length(candidates) == 0) {
      message("[t3] All Markov restarts failed even with intercept-only model.")
      return(NULL)
    }

    # Best = recession-state frequency closest to ~13% (NBER base rate)
    freq_errors <- sapply(candidates, function(c) abs(c$freq_rec - 0.13))
    best        <- candidates[[which.min(freq_errors)]]

    ms_model        <- best$model
    smooth_probs    <- best$smooth_probs
    recession_state <- best$recession_state
    panel_trimmed   <- best$panel_trimmed
    state_assign    <- best$state_assign
    emp_means       <- best$empirical_means
    freq_rec        <- best$freq_rec
    current_rec_prob <- smooth_probs[nrow(smooth_probs), recession_state]
    reliable         <- freq_rec <= 0.25

    # Label states by growth level for display
    state_order  <- order(emp_means, decreasing = !higher_is_recession)
    state_labels <- character(n_states)
    if (n_states == 3L) {
      state_labels[state_order[1]] <- "Expansion"
      state_labels[state_order[2]] <- "Stall"
      state_labels[state_order[3]] <- "Contraction"
    } else {
      state_labels[recession_state] <- "Contraction"
      state_labels[setdiff(seq_len(n_states), recession_state)[1]] <- "Expansion"
    }

    # Regime-conditional means are just the empirical state means of sw_var
    exp_state <- state_order[1]
    sw_regime_means <- list(
      variable  = switch_name,
      expansion = round(emp_means[exp_state], 2),
      recession = round(emp_means[recession_state], 2)
    )

    if (!reliable)
      message(sprintf("[t3] WARNING: Recession state = %.0f%% of history (max 25%%). Will cap weight.",
                      freq_rec * 100))

    message(sprintf(
      "[t3] Markov %d-state fitted: '%s' | States: %s | rec_state=%d '%s' mean=%.2f (%.0f%% of months) | p_rec=%.1f%% | reliable=%s",
      n_states, switch_name,
      paste(sprintf("%d=%.1f%%(%s)", seq_len(n_states), emp_means, state_labels), collapse=" / "),
      recession_state, state_labels[recession_state], emp_means[recession_state],
      freq_rec * 100, current_rec_prob * 100, reliable))

    list(model               = ms_model,
         smooth_probs        = smooth_probs,
         recession_state     = recession_state,
         current_rec_prob    = current_rec_prob,
         state_means_sw      = emp_means,
         state_labels        = state_labels,
         switch_name         = switch_name,
         higher_is_recession = higher_is_recession,
         regime_means        = list(),       # no predictor-conditional means for intercept-only
         sw_regime_means     = sw_regime_means,
         state_freq_recession = freq_rec,
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
# FIX 2: build_glm_feature_matrix — robust USREC with fred_data fallback
# ═══════════════════════════════════════════════════════════════════════════════

build_glm_feature_matrix <- function(fred_data) {
  tryCatch({

    # ── Step 1: Get NBER recession indicator ─────────────────────────────────
    # Primary: fredr. Fallback: construct from UNRATE using Sahm-like rule.
    usrec <- tryCatch({
      r <- fredr::fredr("USREC", observation_start = as.Date("1959-01-01"),
                        frequency = "m")
      if (is.null(r) || nrow(r) < 120) stop("USREC too short")
      message(sprintf("[t3] USREC loaded from fredr: %d months", nrow(r)))
      r %>% dplyr::select(date, recession = value) %>% dplyr::arrange(date)
    }, error = function(e) {
      message(sprintf("[t3] fredr('USREC') failed (%s) — constructing recession proxy from UNRATE.", e$message))

      # Sahm rule proxy: recession = UNRATE rose >= 0.5pp from 12M trailing min
      ur_df <- fred_data$UNRATE
      if (is.null(ur_df) || nrow(ur_df) < 24) {
        message("[t3] UNRATE not available in fred_data either. Cannot build feature matrix.")
        return(NULL)
      }
      ur_df %>% dplyr::arrange(date) %>%
        dplyr::mutate(
          month     = lubridate::floor_date(date, "month"),
          u         = value) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(u = mean(u, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(date = month) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
          u_min12   = zoo::rollapply(u, 12, min, fill = NA, align = "right"),
          recession = as.numeric((u - u_min12) >= 0.5)) %>%
        dplyr::select(date, recession) %>%
        dplyr::filter(!is.na(recession))
    })

    if (is.null(usrec) || nrow(usrec) < 120) {
      message("[t3] build_glm_feature_matrix: recession indicator unavailable. Returning NULL.")
      return(NULL)
    }

    # ── Step 2: Build feature series ─────────────────────────────────────────
    gm <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(date = month)
    }
    gm_fred_full <- function(sid, val_name, start = as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid, observation_start = start, frequency = "m") %>%
          dplyr::select(date, !!val_name := value),
        error = function(e) gm(sid, val_name))
    }

    t10y2y  <- gm_fred_full("T10Y2Y","yield_spread")
    unrate  <- gm("UNRATE","u")
    fedfund <- gm("FEDFUNDS","ff")
    cpi_m   <- gm("CPIAUCSL","cpi")
    hy_m    <- gm("BAMLH0A0HYM2","hy_spread")
    oil_m   <- gm("DCOILWTICO","oil")
    lfpr_m  <- gm("LNS11300060","prime_lfpr")
    usd_m   <- gm_fred_full("DTWEXBGS","usd")
    isr_m   <- gm("ISRATIO","inv_sales")
    cc_m    <- gm("DRCCLACBS","cc_del")
    pay_m   <- gm("PAYEMS","payems")
    sent_m  <- gm("UMCSENT","sentiment")

    if (is.null(unrate) || is.null(cpi_m) || is.null(fedfund)) {
      message("[t3] build_glm_feature_matrix: missing core series (UNRATE/CPIAUCSL/FEDFUNDS). Returning NULL.")
      return(NULL)
    }

    # Yield spread: use T10Y2Y if available, else DGS10-DGS2
    if (is.null(t10y2y)) {
      t10 <- gm("DGS10","t10"); t2 <- gm("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        t10y2y <- dplyr::inner_join(t10, t2, by="date") %>%
          dplyr::mutate(yield_spread = t10 - t2) %>%
          dplyr::select(date, yield_spread)
    }

    # Derived series
    u_df <- unrate %>% dplyr::rename(u = value) %>%
      dplyr::mutate(u_trough12 = zoo::rollapply(u, 12, min, fill=NA, align="right"),
                    u_rise = u - u_trough12)
    pay_df <- if (!is.null(pay_m))
      pay_m %>% dplyr::arrange(date) %>%
        dplyr::mutate(pay_chg = payems - dplyr::lag(payems,1),
                      pay_3m  = zoo::rollmean(pay_chg, 3, fill=NA, align="right")) %>%
        dplyr::select(date, pay_3m) else NULL
    cpi_df <- cpi_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(cpi_yoy = (cpi / dplyr::lag(cpi,12) - 1) * 100) %>%
      dplyr::select(date, cpi_yoy)
    ff_df   <- fedfund %>% dplyr::rename(ff = ff)
    hy_df   <- if (!is.null(hy_m))   hy_m   %>% dplyr::rename(hy_spread  = hy_spread) else NULL
    oil_df  <- if (!is.null(oil_m))  oil_m  %>% dplyr::arrange(date) %>%
      dplyr::mutate(oil_yoy = (oil / dplyr::lag(oil,12) - 1) * 100) %>%
      dplyr::select(date, oil_yoy) else NULL
    lfpr_df <- if (!is.null(lfpr_m)) lfpr_m %>% dplyr::rename(prime_lfpr = prime_lfpr) else NULL
    usd_df  <- if (!is.null(usd_m))  usd_m  %>% dplyr::arrange(date) %>%
      dplyr::mutate(usd_yoy = (usd / dplyr::lag(usd,12) - 1) * 100) %>%
      dplyr::select(date, usd_yoy) else NULL
    isr_df  <- if (!is.null(isr_m))  isr_m  %>% dplyr::rename(inv_sales = inv_sales) else NULL
    cc_df   <- if (!is.null(cc_m))   cc_m   %>% dplyr::rename(cc_del    = cc_del)    else NULL
    sent_df <- if (!is.null(sent_m)) sent_m %>% dplyr::rename(sentiment = sentiment) else NULL

    # Forward recession indicator: did any recession start in next 12 months?
    n_usrec <- nrow(usrec)
    usrec$rec_next12 <- vapply(seq_len(n_usrec), function(i) {
      idx <- (i + 1):min(i + 12, n_usrec)
      if (length(idx) < 6) return(NA_real_)
      as.numeric(any(usrec$recession[idx] == 1))
    }, numeric(1))

    # Assemble panel
    panel <- usrec %>%
      dplyr::left_join(if (!is.null(t10y2y)) t10y2y else data.frame(date=as.Date(character())), by="date") %>%
      dplyr::left_join(u_df %>% dplyr::select(date, u, u_rise), by="date") %>%
      dplyr::left_join(cpi_df, by="date") %>%
      dplyr::left_join(ff_df, by="date") %>%
      dplyr::mutate(real_rate = ff - cpi_yoy)

    for (df in list(pay_df, hy_df, oil_df, lfpr_df, usd_df, isr_df, cc_df, sent_df))
      if (!is.null(df)) panel <- dplyr::left_join(panel, df, by="date")

    # Rename to match predictor names expected by train/score GLM
    if ("u" %in% names(panel))           panel <- dplyr::rename(panel, u_rise_raw = u)
    if ("yield_spread" %in% names(panel)) {}   # already correct
    if ("u_rise" %in% names(panel))      {}    # already correct

    panel <- panel %>% dplyr::filter(!is.na(rec_next12), !is.na(u_rise))

    n_before <- nrow(panel)
    if (n_before < 120) {
      message(sprintf("[t3] Panel too small after filtering: %d rows (need ≥120).", n_before))
      return(NULL)
    }

    message(sprintf("[t3] GLM feature matrix: %d months, recession_base_rate=%.1f%%",
                    nrow(panel), mean(panel$rec_next12, na.rm=TRUE)*100))
    panel
  }, error = function(e) {
    message("[t3] build_glm_feature_matrix failed: ", e$message)
    NULL
  })
}
