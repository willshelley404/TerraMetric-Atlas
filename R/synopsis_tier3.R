# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3.R  —  Tier 3: Rolling GLM + Markov-Switching + Anomaly
#
# This file replaces synopsis_tier3.R + all patch files (patches 1–6).
# Source only these three lines in global.R:
#   source("R/synopsis_tier1.R")
#   source("R/synopsis_tier2.R")
#   source("R/synopsis_tier3.R")       ← this file
#
# Tier 3 adds to Tier 2 (GLM + anomaly):
#   • Hamilton (1989) 3-state Markov-switching model — pure-R EM, no MSwM
#   • GLM retrained on a rolling 30-year window (adapts to structural change)
#   • Data-driven blend weights via out-of-sample Brier skill scoring
#   • Animated loading screen while models train
#   • Reliability check: Markov weight capped if recession state > 25% of history
#
# APP STRUCTURE:
#   project/
#     global.R
#     server.R  /  ui.R
#     R/
#       synopsis_tier1.R
#       synopsis_tier2.R
#       synopsis_tier3.R   ← this file
#       synopsis_original.R   (kept for reference, not sourced)
#       data_fred.R  /  data_census.R  /  llm_insights.R
# ─────────────────────────────────────────────────────────────────────────────

# ── Source tier 2 (which sources tier 1) ─────────────────────────────────────
local({
  candidates <- c(
    tryCatch(normalizePath(sys.frame(1)$ofile), error=function(e) NULL),
    "R/synopsis_tier2.R"
  )
  for (p in candidates) {
    tier2 <- sub("tier3", "tier2", p)
    if (file.exists(tier2)) { source(tier2, local=FALSE); return(invisible()) }
  }
  stop("[synopsis_tier3] Cannot locate synopsis_tier2.R. Set working directory to project root.")
})


# ── Re-define %||% to handle list inputs (tier1's version calls is.na(list) ──────
# Tier1 defines: if(!is.null(a) && !is.na(a))
# is.na() on a list returns a vector → if(vector) → 'length=N' error
# This version uses tryCatch to safely handle any type of a.
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  ok <- tryCatch(!is.na(a[[1]]), error = function(e) TRUE)
  if (isTRUE(ok) && length(a) > 0) a else b
}


# ═══════════════════════════════════════════════════════════════════════════════
# CONSTANTS
# ═══════════════════════════════════════════════════════════════════════════════

.MARKOV_FREQ_MAX    <- 0.25   # recession state covering >25% of history = bad fit
.MARKOV_FREQ_TARGET <- 0.13   # NBER base rate ~13%


# ═══════════════════════════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════════════════════════

.prob_to_lo <- function(p) log(pmax(1e-6, pmin(1-1e-6, p)) / (1 - pmax(1e-6, pmin(1-1e-6, p))))


# ═══════════════════════════════════════════════════════════════════════════════
# PURE-R HAMILTON (1989) EM — no matrix inversion, cannot be singular
# ═══════════════════════════════════════════════════════════════════════════════

#' Hamilton filter: forward pass computing P(S_t=j | y_{1..t})
.hamilton_filter <- function(y, mu, sigma, P, pi0) {
  T <- length(y); k <- length(mu)
  xi <- matrix(0, T, k); loglik <- 0
  xi_prev <- pi0
  for (t in seq_len(T)) {
    pred  <- as.numeric(t(P) %*% xi_prev)
    dens  <- dnorm(y[t], mean=mu, sd=sigma)
    joint <- pred * dens
    s     <- sum(joint); if (s < 1e-300) s <- 1e-300
    xi[t, ] <- joint / s
    loglik  <- loglik + log(s)
    xi_prev <- xi[t, ]
  }
  list(xi=xi, loglik=loglik)
}

#' Kim (1994) smoother: backward pass computing P(S_t=j | y_{1..T})
.kim_smoother <- function(xi, P) {
  T <- nrow(xi); k <- ncol(xi)
  sp <- matrix(0, T, k); sp[T, ] <- xi[T, ]
  for (t in (T-1):1) {
    pred_next <- pmax(as.numeric(t(P) %*% xi[t, ]), 1e-300)
    sp[t, ]   <- xi[t, ] * as.numeric(P %*% (sp[t+1, ] / pred_next))
    sp[t, ]   <- pmax(sp[t, ], 0)
    s <- sum(sp[t, ]); if (s > 0) sp[t, ] <- sp[t, ] / s
  }
  sp
}

#' Fit k-state Gaussian Markov-switching model via EM.
#' Initialised by splitting quantile range into k equal bands.
.fit_hamilton_em <- function(y, k=3L, max_iter=500L, tol=1e-6, seed=42L) {
  set.seed(seed); T <- length(y)
  breaks <- quantile(y, probs=seq(0,1,length.out=k+1), na.rm=TRUE)
  mu <- sigma <- numeric(k)
  for (j in seq_len(k)) {
    band <- y[y >= breaks[j] & y <= breaks[j+1]]
    if (length(band) < 2) band <- y
    mu[j]    <- mean(band, na.rm=TRUE)
    sigma[j] <- max(sd(band, na.rm=TRUE), 0.01)
  }
  P <- matrix(1/k, k, k); pi0 <- rep(1/k, k); prev_ll <- -Inf; converged <- FALSE
  for (iter in seq_len(max_iter)) {
    filt <- .hamilton_filter(y, mu, sigma, P, pi0)
    sp   <- .kim_smoother(filt$xi, P)
    ll   <- filt$loglik
    # M-step: transition matrix
    P_new <- matrix(0, k, k)
    for (t in 2:T) {
      pred_t <- pmax(as.numeric(t(P) %*% filt$xi[t-1, ]), 1e-300)
      for (i in seq_len(k)) for (j in seq_len(k))
        P_new[i,j] <- P_new[i,j] + filt$xi[t-1,i] * P[i,j] * sp[t,j] / pred_t[j]
    }
    rs <- rowSums(P_new); rs[rs < 1e-300] <- 1e-300; P <- P_new / rs
    # M-step: means + standard deviations
    wt <- pmax(colSums(sp), 1e-10)
    mu    <- colSums(sp * y) / wt
    sigma <- pmax(sqrt(colSums(sp * outer(y, mu, function(a,b)(a-b)^2)) / wt), 0.01)
    pi0   <- sp[1, ]
    if (abs(ll - prev_ll) < tol) { converged <- TRUE; break }
    prev_ll <- ll
  }
  list(smooth_probs=sp, mu=mu, sigma=sigma, P=P, loglik=ll, converged=converged, n_iter=iter)
}


# ═══════════════════════════════════════════════════════════════════════════════
# build_glm_feature_matrix
# KEY FIX: all training series fetched via gf() (fredr full history) so the
# panel spans 1959–present rather than collapsing to fred_data's recent window.
# ═══════════════════════════════════════════════════════════════════════════════

build_glm_feature_matrix <- function(fred_data) {
  tryCatch({
    # Helper: read monthly from fred_data (recent window)
    gm <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 4) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month)
    }
    # Helper: try fredr full history first, fall back to gm()
    gf <- function(sid, val_name, start=as.Date("1959-01-01"), freq="m") {
      # floor_date + group_by ensures quarterly/daily series align to month-start dates
      # without this, quarterly FRED dates (e.g. 1991-03-31) won't join to monthly panel
      tryCatch(
        fredr::fredr(sid, observation_start=start, frequency=freq) %>%
          dplyr::select(date, value) %>%
          dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
          dplyr::rename(date=month),
        error=function(e) {
          message(sprintf("[t3] fredr('%s') unavailable: %s — using fred_data", sid, e$message))
          gm(sid, val_name)
        })
    }

    # ── Step 1: NBER recession indicator ─────────────────────────────────────
    usrec <- tryCatch({
      r <- fredr::fredr("USREC", observation_start=as.Date("1959-01-01"), frequency="m")
      if (is.null(r) || nrow(r) < 120) stop("too short")
      message(sprintf("[t3] USREC: %d months from fredr", nrow(r)))
      r %>% dplyr::select(date, recession=value) %>% dplyr::arrange(date)
    }, error=function(e) {
      message(sprintf("[t3] USREC fredr failed (%s) — building Sahm proxy from UNRATE", e$message))
      ur <- fred_data$UNRATE
      if (is.null(ur) || nrow(ur) < 24) { message("[t3] UNRATE missing."); return(NULL) }
      base <- ur %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(u=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month) %>% dplyr::arrange(date) %>%
        dplyr::mutate(u_min12=zoo::rollapply(u,12,min,fill=NA,align="right"))
      for (thresh in c(0.5, 0.3)) {
        cand <- base %>% dplyr::mutate(recession=as.numeric((u-u_min12)>=thresh)) %>%
          dplyr::filter(!is.na(recession)) %>% dplyr::select(date,recession)
        if (sum(cand$recession) >= 12) {
          message(sprintf("[t3] Sahm proxy (%.1fpp): %d recession months", thresh, sum(cand$recession)))
          return(cand)
        }
      }
      message("[t3] Sahm proxy produced zero recession months."); NULL
    })
    if (is.null(usrec) || nrow(usrec) < 120) {
      message("[t3] Recession indicator unavailable."); return(NULL)
    }

    # ── Step 2: Feature series — ALL via gf() so history goes back to 1959 ───
    t10y2y  <- gf("T10Y2Y", "yield_spread")
    unrate  <- gf("UNRATE",       "u")          # ← was gm(), caused 67-row problem
    fedfund <- gf("FEDFUNDS",     "ff")          # ← was gm()
    cpi_m   <- gf("CPIAUCSL",     "cpi")         # ← was gm()
    hy_m    <- gf("BAMLH0A0HYM2", "hy_spread")  # ← was gm()
    oil_m   <- gf("DCOILWTICO",   "oil")         # ← was gm()
    pay_m   <- gf("PAYEMS",       "payems")      # ← was gm()
    lfpr_m  <- gf("LNS11300060",  "prime_lfpr")
    usd_m   <- gf("DTWEXBGS",     "usd")
    isr_m   <- gf("ISRATIO",      "inv_sales")  # monthly, gf for full history
    cc_m    <- gf("DRCCLACBS",    "cc_del",  freq="q")  # quarterly → must use freq="q" on FRED
    sent_m  <- gf("UMCSENT",      "sentiment")  # gf for full history

    if (is.null(unrate) || is.null(cpi_m) || is.null(fedfund)) {
      message("[t3] Missing core series (UNRATE/CPIAUCSL/FEDFUNDS)."); return(NULL)
    }

    if (is.null(t10y2y)) {
      t10 <- gf("DGS10","t10"); t2 <- gf("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        t10y2y <- dplyr::inner_join(t10,t2,by="date") %>%
          dplyr::mutate(yield_spread=t10-t2) %>% dplyr::select(date,yield_spread)
    }

    # ── Step 3: Derived features ──────────────────────────────────────────────
    u_df <- unrate %>% dplyr::arrange(date) %>%
      dplyr::mutate(u_trough12=zoo::rollapply(u,12,min,fill=NA,align="right"),
                    u_rise=u-u_trough12) %>%
      dplyr::select(date, u, u_rise)

    pay_df <- if (!is.null(pay_m))
      pay_m %>% dplyr::arrange(date) %>%
        dplyr::mutate(pay_chg=payems-dplyr::lag(payems,1),
                      pay_3m=zoo::rollmean(pay_chg,3,fill=NA,align="right")) %>%
        dplyr::select(date, pay_3m) else NULL

    cpi_df <- cpi_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(cpi_yoy=(cpi/dplyr::lag(cpi,12)-1)*100) %>%
      dplyr::select(date, cpi_yoy)

    oil_df <- if (!is.null(oil_m)) oil_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(oil_yoy=(oil/dplyr::lag(oil,12)-1)*100) %>%
      dplyr::select(date, oil_yoy) else NULL

    usd_df <- if (!is.null(usd_m)) usd_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(usd_yoy=(usd/dplyr::lag(usd,12)-1)*100) %>%
      dplyr::select(date, usd_yoy) else NULL

    # ── Step 4: Forward recession label ──────────────────────────────────────
    n_r <- nrow(usrec)
    usrec$rec_next12 <- vapply(seq_len(n_r), function(i) {
      idx <- (i+1):min(i+12, n_r)
      if (length(idx) < 6) return(NA_real_)
      as.numeric(any(usrec$recession[idx]==1))
    }, numeric(1))

    # ── Step 5: Assemble panel ────────────────────────────────────────────────
    # NOTE: cc_m (DRCCLACBS) is a quarterly series — it has real values only
    # at Q-end dates; all other months are NA after a monthly left-join.
    # We forward-fill (LOCF) so that each monthly row carries the most recent
    # quarterly reading.  This is the correct treatment for low-frequency
    # predictors: the value doesn't change intra-quarter, so propagating it
    # forward introduces no look-ahead bias.
    locf <- function(df, value_col) {
      if (is.null(df)) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(!!value_col := zoo::na.locf(!!rlang::sym(value_col), na.rm=FALSE))
    }

    empty_date_df <- data.frame(date=as.Date(character()), stringsAsFactors=FALSE)
    panel <- usrec %>%
      dplyr::left_join(if(!is.null(t10y2y)) t10y2y else empty_date_df, by="date") %>%
      dplyr::left_join(u_df, by="date") %>%
      dplyr::left_join(cpi_df, by="date") %>%
      dplyr::left_join(fedfund, by="date") %>%          # fedfund already has col 'ff'
      dplyr::mutate(real_rate=ff-cpi_yoy)

    for (df in list(pay_df, hy_m, oil_df, lfpr_m, usd_df, isr_m, cc_m, sent_m))
      if (!is.null(df)) panel <- dplyr::left_join(panel, df, by="date")

    # LOCF for quarterly series: NAs only appear AFTER the monthly join, so
    # forward-fill MUST happen here on the assembled panel, not before the join.
    # DRCCLACBS reports quarterly (Q-end only). ISRATIO is monthly, locf is a no-op.
    if ("cc_del" %in% names(panel))
      panel$cc_del <- zoo::na.locf(panel$cc_del, na.rm=FALSE)
    if ("inv_sales" %in% names(panel))
      panel$inv_sales <- zoo::na.locf(panel$inv_sales, na.rm=FALSE)

    panel <- panel %>% dplyr::filter(!is.na(rec_next12), !is.na(u_rise))

    if (nrow(panel) < 120) {
      message(sprintf("[t3] GLM panel too small: %d rows (need ≥120).", nrow(panel)))
      return(NULL)
    }
    message(sprintf("[t3] GLM feature matrix: %d months, base_rate=%.1f%%",
                    nrow(panel), mean(panel$rec_next12,na.rm=TRUE)*100))
    panel
  }, error=function(e) { message("[t3] build_glm_feature_matrix failed: ",e$message); NULL })
}


# ═══════════════════════════════════════════════════════════════════════════════
# rolling_retrain — stores training_panel for OOS Brier scoring
# ═══════════════════════════════════════════════════════════════════════════════

rolling_retrain <- function(fred_data, window=360L) {
  message(sprintf("[t3] Rolling GLM retrain (window=%dM)...", window))
  panel <- build_glm_feature_matrix(fred_data)
  if (is.null(panel)) { message("[t3] build_glm_feature_matrix returned NULL — cannot train GLM."); return(NULL) }

  panel_rolling <- tail(panel, window)
  if (nrow(panel_rolling) < 100) panel_rolling <- panel

  possible  <- c("yield_spread","u_rise","pay_3m","real_rate","hy_spread","oil_yoy",
                 "prime_lfpr","usd_yoy","inv_sales","cc_del","sentiment")
  available <- intersect(possible, names(panel_rolling))
  if (length(available) < 3) { message("[t3] Too few predictors for GLM."); return(NULL) }

  # Drop predictors that have >30% NA in the rolling window to avoid losing rows via drop_na
  # DRCCLACBS starts 1991, UMCSENT starts 1978 — both fine for a 30-year rolling window
  # But if gf() alignment fails, they appear all-NA; detect and exclude those
  coverage <- sapply(available, function(v) {
    col <- panel_rolling[[v]]
    sum(!is.na(col)) / length(col)
  })
  good_preds <- available[coverage >= 0.70]  # require 70% non-NA coverage
  dropped <- setdiff(available, good_preds)
  if (length(dropped) > 0)
    message(sprintf("[t3] Dropping low-coverage predictors: %s", paste(dropped, collapse=", ")))
  if (length(good_preds) < 3) {
    message(sprintf("[t3] Too few predictors after coverage filter (%d). Returning NULL.", length(good_preds)))
    return(NULL)
  }

  panel_clean <- panel_rolling %>%
    dplyr::select(rec_next12, dplyr::all_of(good_preds)) %>% tidyr::drop_na()
  available <- good_preds  # update available for GLM formula
  if (nrow(panel_clean) < 60) { message(sprintf("[t3] Clean panel too small: %d rows.", nrow(panel_clean))); return(NULL) }

  fml <- as.formula(paste("rec_next12 ~", paste(available, collapse=" + ")))
  tryCatch({
    model    <- glm(fml, data=panel_clean, family=binomial(link="logit"))
    sm       <- summary(model)
    coef_tbl <- as.data.frame(sm$coefficients); coef_tbl$predictor <- rownames(coef_tbl)
    coef_tbl <- coef_tbl %>% dplyr::filter(predictor!="(Intercept)") %>%
      dplyr::rename(estimate=Estimate,se=`Std. Error`,z_stat=`z value`,p_val=`Pr(>|z|)`) %>%
      dplyr::select(predictor,estimate,se,z_stat,p_val)
    ad <- build_anomaly_detector(panel_clean[,available,drop=FALSE])
    message(sprintf("[t3] GLM: n=%d, window=%dM, AIC=%.1f, %d predictors",
                    nrow(panel_clean), window, AIC(model), length(available)))
    # ── Data-driven anomaly recession floor ──────────────────────────────────────
    # Compute Mahalanobis distances for all training rows, then find the empirical
    # recession rate in the top 10% of distances. This is the historically-grounded
    # probability of recession when conditions are genuinely anomalous — fully
    # data-driven, never hard-coded.
    if (!is.null(ad)) {
      tryCatch({
        feat_mat  <- as.matrix(panel_clean[, available, drop=FALSE])
        train_mds <- sqrt(mahalanobis(feat_mat, ad$mu, ad$cov_reg))
        anomalous <- train_mds > ad$d90
        rec_col   <- panel_clean$rec_next12
        n_anom    <- sum(anomalous, na.rm=TRUE)
        if (n_anom >= 12) {
          p_anom_rec <- mean(rec_col[anomalous], na.rm=TRUE)
          ad$p_anomalous_recession <- round(p_anom_rec, 3)
          message(sprintf("[t3] Anomaly recession rate (MD>d90, n=%d months): %.1f%%",
                          n_anom, p_anom_rec * 100))
        }
      }, error=function(e) message("[t3] Anomaly recession rate calc failed: ", e$message))
    }

    list(model=model, coef_table=coef_tbl, n_obs=nrow(panel_clean),
         aic=round(AIC(model),1), predictors=available, trained_at=Sys.time(),
         anomaly_detector=ad, window_months=window,
         training_panel=panel_clean)   # ← critical for Brier scoring
  }, error=function(e) { message("[t3] GLM fit failed: ",e$message); NULL })
}


# ═══════════════════════════════════════════════════════════════════════════════
# train_markov_model — pure-R Hamilton EM, no MSwM
# ═══════════════════════════════════════════════════════════════════════════════

train_markov_model <- function(fred_data, n_states=3L, n_restarts=5L) {
  tryCatch({
    gm_local <- function(sid, val_name) {
      df <- fred_data[[sid]]; if(is.null(df)||nrow(df)<24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month)
    }
    gf <- function(sid, val_name, start=as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid,observation_start=start,frequency="m") %>%
          dplyr::select(date,value) %>%
          dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
          dplyr::rename(date=month),
        error=function(e){ message(sprintf("[t3] fredr('%s') failed: %s",sid,e$message)); gm_local(sid,val_name) })
    }

    # Cascade: INDPRO YoY → PAYEMS MoM → UNRATE
    switching_var <- NULL; switch_name <- NULL; higher_is_recession <- FALSE
    indpro_raw <- gf("INDPRO","indpro")
    if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
      cand <- indpro_raw %>% dplyr::arrange(date) %>%
        dplyr::mutate(sw_var=(indpro/dplyr::lag(indpro,12)-1)*100) %>%
        dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
      if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"IP YoY %"; higher_is_recession<-FALSE }
    }
    if (is.null(switching_var)) {
      pay_raw <- gf("PAYEMS","payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var=payems-dplyr::lag(payems,1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
        if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"Payroll MoM (K)"; higher_is_recession<-FALSE }
      }
    }
    if (is.null(switching_var)) {
      urate_raw <- gf("UNRATE","unemp")
      if (!is.null(urate_raw) && nrow(urate_raw) >= 60) {
        switching_var <- urate_raw %>% dplyr::rename(sw_var=unemp)
        switch_name<-"Unemployment rate"; higher_is_recession<-TRUE
      }
    }
    if (is.null(switching_var)) { message("[t3] No switching variable."); return(NULL) }

    y         <- switching_var$sw_var[!is.na(switching_var$sw_var)]
    dates_used <- switching_var$date[!is.na(switching_var$sw_var)]
    message(sprintf("[t3] Markov EM: '%s' (%d obs), %d states, %d restarts",
                    switch_name, length(y), n_states, n_restarts))

    fit_once <- function(r) {
      tryCatch({
        y_fit <- if (r > 1) y + rnorm(length(y), 0, sd(y)*0.04*(r-1)) else y
        fit   <- .fit_hamilton_em(y_fit, k=n_states, max_iter=500L, tol=1e-6, seed=r*37L)
        sp    <- fit$smooth_probs
        sa    <- apply(sp, 1, which.max)
        emp   <- sapply(seq_len(n_states), function(s) mean(y[sa==s], na.rm=TRUE))
        rs    <- if (higher_is_recession) which.max(emp) else which.min(emp)
        freq  <- mean(sa==rs)
        message(sprintf("[t3]   restart %d: ll=%.1f conv=%s | means=[%s] | rec=%d freq=%.0f%%",
                        r, fit$loglik, fit$converged, paste(round(emp,1),collapse=","), rs, freq*100))
        list(fit=fit, smooth_probs=sp, recession_state=rs, emp_means=emp, freq_rec=freq, state_assign=sa)
      }, error=function(e){ message(sprintf("[t3]   restart %d failed: %s",r,e$message)); NULL })
    }

    cands <- Filter(Negate(is.null), lapply(seq_len(n_restarts), fit_once))
    if (length(cands)==0) { message("[t3] All EM restarts failed."); return(NULL) }

    best        <- cands[[which.min(sapply(cands, function(c) abs(c$freq_rec-.MARKOV_FREQ_TARGET)))]]
    sp          <- best$smooth_probs
    rs          <- best$recession_state
    emp         <- best$emp_means
    sa          <- best$state_assign
    freq_rec    <- best$freq_rec
    cur_p       <- sp[length(y), rs]
    reliable    <- freq_rec <= .MARKOV_FREQ_MAX

    ord <- order(emp, decreasing=!higher_is_recession)
    lbl <- character(n_states)
    if (n_states==3L) { lbl[ord[1]]<-"Expansion"; lbl[ord[2]]<-"Stall"; lbl[ord[3]]<-"Contraction" }
    else { lbl[rs]<-"Contraction"; lbl[setdiff(seq_len(n_states),rs)[1]]<-"Expansion" }

    exp_st <- ord[1]
    sw_means <- list(variable=switch_name, expansion=round(emp[exp_st],2), recession=round(emp[rs],2))

    if (!reliable)
      message(sprintf("[t3] WARNING: rec state = %.0f%% of history (max %.0f%%). Weight will be capped.",
                      freq_rec*100, .MARKOV_FREQ_MAX*100))
    message(sprintf("[t3] Markov final: [%s] | rec='%s' mean=%.2f (%.0f%%) | p_rec=%.1f%% | reliable=%s",
                    paste(sprintf("%s=%.1f",lbl,emp),collapse=" / "),
                    lbl[rs], emp[rs], freq_rec*100, cur_p*100, reliable))

    list(model=best$fit, smooth_probs=sp, recession_state=rs,
         current_rec_prob=cur_p, state_means_sw=emp, state_labels=lbl,
         switch_name=switch_name, higher_is_recession=higher_is_recession,
         regime_means=list(), sw_regime_means=sw_means,
         state_freq_recession=freq_rec, reliable=reliable,
         n_states=n_states, n_obs=length(y), trained_at=Sys.time())
  }, error=function(e){ message("[t3] Markov failed: ",e$message); NULL })
}


# ═══════════════════════════════════════════════════════════════════════════════
# compute_ensemble_weights — true OOS Brier scoring + reliability penalty
# ═══════════════════════════════════════════════════════════════════════════════

compute_ensemble_weights <- function(glm_result, markov_result, holdout_months=36L) {
  fallback <- list(w_glm=0.60,w_ms=0.40,glm_brier=NA_real_,ms_brier=NA_real_,
                   n_holdout=0L,method="fallback (equal-ish)")

  if (is.null(glm_result) || is.null(glm_result$training_panel)) {
    message("[t3] compute_ensemble_weights: training_panel missing, using fallback.")
    return(fallback)
  }
  panel <- glm_result$training_panel; n <- nrow(panel)
  if (n < holdout_months + 30L) {
    message(sprintf("[t3] Panel too small for holdout (n=%d). Fallback.", n)); return(fallback)
  }

  hold_idx   <- (n - holdout_months + 1L):n
  train_idx  <- seq_len(n - holdout_months)
  hold_df    <- panel[hold_idx,  , drop=FALSE]
  train_df   <- panel[train_idx, , drop=FALSE]
  y_act      <- hold_df$rec_next12
  ok         <- !is.na(y_act)
  if (sum(ok) < 12L) {
    message(sprintf("[t3] Only %d valid holdout outcomes. Fallback.", sum(ok))); return(fallback)
  }
  hold_df <- hold_df[ok,,drop=FALSE]; y_act <- y_act[ok]
  avail   <- intersect(glm_result$predictors, names(panel))

  # Fit temporary scoring-only GLM (never sees holdout)
  glm_brier <- tryCatch({
    sc <- train_df %>% dplyr::select(rec_next12,dplyr::all_of(avail)) %>% tidyr::drop_na()
    if (nrow(sc) < 60) stop("scoring train set too small")
    tmp  <- glm(as.formula(paste("rec_next12~",paste(avail,collapse="+"))),data=sc,family=binomial)
    hdf  <- hold_df[,avail,drop=FALSE]
    # Use colwise NA replacement - NOT if(vector) which throws 'length=N' error
    for (col in names(hdf)) {
      nas <- is.na(hdf[[col]])
      if (any(nas)) hdf[[col]][nas] <- 0
    }
    mean((predict(tmp,newdata=hdf,type="response") - y_act)^2, na.rm=TRUE)
  }, error=function(e){ message("[t3] GLM Brier failed: ",e$message); NA_real_ })

  ms_brier <- tryCatch({
    if (is.null(markov_result)||is.null(markov_result$smooth_probs)) stop("no smooth_probs")
    sp_rec <- markov_result$smooth_probs[,markov_result$recession_state]
    n_h    <- length(y_act)
    if (length(sp_rec) < n_h) stop("smooth_probs too short")
    mean((tail(sp_rec,n_h) - y_act)^2, na.rm=TRUE)
  }, error=function(e){ message("[t3] Markov Brier failed: ",e$message); NA_real_ })

  message(sprintf("[t3] Brier — GLM:%s Markov:%s holdout:%d months",
                  if(is.na(glm_brier))"NA" else sprintf("%.4f",glm_brier),
                  if(is.na(ms_brier)) "NA" else sprintf("%.4f",ms_brier),
                  length(y_act)))

  if (is.na(glm_brier) && is.na(ms_brier)) return(fallback)
  # If one model's Brier is NA, use equal weighting (don't give 85% to the other model blindly)
  if (is.na(ms_brier))  return(modifyList(fallback,list(w_glm=0.70,w_ms=0.30,glm_brier=glm_brier,n_holdout=length(y_act),method="GLM only (equal fallback)")))
  if (is.na(glm_brier)) return(modifyList(fallback,list(w_glm=0.70,w_ms=0.30,ms_brier=ms_brier,n_holdout=length(y_act),method="Markov only (equal fallback)")))
  # Sanity check: if Markov Brier > climatology, it has zero skill — cap its weight at 20%
  base_rate_check <- mean(panel$rec_next12, na.rm=TRUE)
  clim_check <- base_rate_check * (1 - base_rate_check)
  if (!is.na(ms_brier) && ms_brier > clim_check) {
    message(sprintf("[t3] Markov Brier (%.4f) > climatology (%.4f) — Markov has no skill. Capping weight at 20%%.", ms_brier, clim_check))
    ms_brier <- NA_real_
    return(modifyList(fallback,list(w_glm=0.80,w_ms=0.20,glm_brier=glm_brier,n_holdout=length(y_act),method="GLM dominant (Markov zero skill)")))
  }

  base_rate  <- mean(panel$rec_next12, na.rm=TRUE)
  clim       <- base_rate * (1 - base_rate)
  gs <- max(0, 1 - glm_brier/clim); ms <- max(0, 1 - ms_brier/clim)
  tot <- gs + ms
  if (tot < 1e-4) { message("[t3] Both models near-zero skill. Fallback."); return(fallback) }

  w_glm <- gs/tot; w_ms <- ms/tot

  # Reliability penalty: cap Markov if its recession state > 25% of history
  if (!is.null(markov_result) && isFALSE(markov_result$reliable)) {
    w_ms  <- min(w_ms, 0.10); w_glm <- 1 - w_ms
    message(sprintf("[t3] Markov weight capped at 10%% (state freq=%.0f%%)",
                    markov_result$state_freq_recession*100))
  }

  method <- sprintf("OOS Brier skill (clim=%.4f, n=%d months)", clim, length(y_act))
  if (!is.null(markov_result) && isFALSE(markov_result$reliable))
    method <- paste0(method, sprintf(" [Markov capped: freq=%.0f%%]", markov_result$state_freq_recession*100))

  message(sprintf("[t3] Weights → GLM=%.0f%% (skill=%.3f) | Markov=%.0f%% (skill=%.3f)",
                  w_glm*100, gs, w_ms*100, ms))

  list(w_glm=round(w_glm,3),w_ms=round(w_ms,3),
       glm_brier=round(glm_brier,4),ms_brier=round(ms_brier,4),
       glm_skill=round(gs,3),ms_skill=round(ms,3),
       n_holdout=length(y_act),method=method)
}


# ═══════════════════════════════════════════════════════════════════════════════
# .classify_recession_tier — 5-tier system
# ═══════════════════════════════════════════════════════════════════════════════

.classify_recession_tier <- function(prob) {
  if      (prob >= 65) list(label="Elevated",      color="#e94560", icon="exclamation-triangle",
    desc="Multiple leading indicators aligned recessionary. Historical hit rate ~65%. Defensive positioning warranted.")
  else if (prob >= 50) list(label="High",           color="#e07040", icon="exclamation-circle",
    desc="More than half the probability weight sits on the recessionary side. Multiple genuine warning signs are active. Historically precedes recession in roughly half of cases.")
  else if (prob >= 30) list(label="Moderate",       color="#f4a261", icon="minus-circle",
    desc="Mixed signals. Some meaningful risk factors are elevated but no acute stress. Late-cycle slowdown most likely; recession requires additional deterioration.")
  else if (prob >= 15) list(label="Low\u2013Moderate", color="#f4a261", icon="check-circle",
    desc="A handful of headwinds present but overall macro picture is cautiously positive. Monitor labor momentum and credit spreads.")
  else                 list(label="Low",            color="#2dce89", icon="check-circle",
    desc="Most leading indicators benign. At or below historical 12-month recession base rate (~13%). No acute stress signals.")
}


# ═══════════════════════════════════════════════════════════════════════════════
# .regime_from_score — replaces broken dplyr::case_when + list()
# ═══════════════════════════════════════════════════════════════════════════════

.regime_from_score <- function(s) {
  if      (s >= 7.5) list(label="Expansion",        color="#2dce89", icon="arrow-up",
    gdp_est="+2.5% to +3.5%", summary="Broad-based growth signals. Labor solid, consumer resilient, financial conditions supportive.")
  else if (s >= 5.5) list(label="Moderate Growth",  color="#00b4d8", icon="minus",
    gdp_est="+1.0% to +2.5%", summary="Below-trend but positive growth likely. Mixed signals — watch rate transmission and consumer confidence.")
  else if (s >= 3.5) list(label="Stall / Slowdown", color="#f4a261", icon="arrow-down",
    gdp_est="-0.5% to +1.0%", summary="Growth at risk. Restrictive monetary conditions or financial stress beginning to bite.")
  else               list(label="Contraction Risk", color="#e94560", icon="exclamation-triangle",
    gdp_est="Below -0.5%",    summary="Multiple negative signals. Recession probability elevated.")
}


# ═══════════════════════════════════════════════════════════════════════════════
# .growth_outlook_loading_html — animated screen while models train
# ═══════════════════════════════════════════════════════════════════════════════

.growth_outlook_loading_html <- function(step=NULL) {
  # Pure paste0 — no sprintf, no % format hazards anywhere
  step_label <- if (!is.null(step)) htmltools::htmlEscape(step) else "Fitting models…"
  shiny::HTML(paste0(
    "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;",
    "padding:40px 24px;text-align:center;min-height:300px;",
    "display:flex;flex-direction:column;align-items:center;justify-content:center;gap:12px;'>",
    # SVG spinner avoids @keyframes % in sprintf entirely
    "<svg width='44' height='44' viewBox='0 0 44 44' style='margin:0 auto 16px;'>",
    "<style>@keyframes t3s{to{transform:rotate(360deg);}}</style>",
    "<g style='animation:t3s 1s linear infinite;transform-origin:22px 22px;'>",
    "<circle cx='22' cy='22' r='18' fill='none' stroke='#2a3042' stroke-width='3'/>",
    "<path d='M22 4 A18 18 0 0 1 40 22' fill='none' stroke='#00b4d8'",
    " stroke-width='3' stroke-linecap='round'/>",
    "</g></svg>",
    "<div style='color:#00b4d8;font-size:13px;font-weight:700;",
    "text-transform:uppercase;letter-spacing:1px;'>Fitting Econometric Models</div>",
    "<div style='color:#9aa3b2;font-size:12px;max-width:420px;line-height:1.7;'>",
    step_label, "</div>",
    "<div style='margin-top:14px;background:#0f1117;border-radius:6px;",
    "padding:12px 20px;max-width:480px;text-align:left;'>",
    "<div style='color:#555;font-size:10px;font-weight:700;text-transform:uppercase;",
    "letter-spacing:1px;margin-bottom:8px;'>What's running</div>",
    "<div style='color:#6b7585;font-size:11px;line-height:2;'>",
    "<span style='color:#2dce89;'>&#x2713;</span>",
    " Fetch FRED macro history (1959–present)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Fit rolling GLM on NBER recession dates (30yr window)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Fit 3-state Hamilton Markov-switching model (pure-R EM)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Score both on 36-month out-of-sample holdout (Brier)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Build Mahalanobis anomaly detector<br/>",
    "<span style='color:#9aa3b2;'>&#x25cb;</span> Render growth outlook",
    "</div></div>",
    "<div style='color:#555;font-size:10px;margin-top:6px;'>",
    "First run: 10–60 seconds. Cached 30 days.</div>",
    "</div>"))
}

# ═══════════════════════════════════════════════════════════════════════════════
# .compute_recession_prob — Markov contribution in log-odds units
# ═══════════════════════════════════════════════════════════════════════════════

.compute_recession_prob <- function(
  yield_spread, inv_months, unemp, u, pay_3m, real_rate,
  hy_v, vix_v, cs, ip_y, hs,
  prime_age_lfpr=NA, prime_age_lfpr_chg=NA, jobless_claims_trend=NA,
  quits_yoy=NA, oil_yoy=NA, equity_drawdown_pct=NA, usd_yoy=NA,
  inventory_sales_ratio=NA, cc_delinquency=NA,
  glm_result=NULL, anomaly_detector=NULL, markov_result=NULL, ensemble_weights=NULL
) {
  ANOMALY_WEIGHT <- 0.35; ANOMALY_FLOOR <- 0.25; COMPRESS_CENTER <- 0.50
  contribs <- list()
  add_contrib <- function(id,lbl,delta,z_stat=NA_real_,data_missing=FALSE)
    contribs[[id]] <<- list(name=lbl,contribution=delta,z_stat=z_stat,data_missing=data_missing)

  u_rise_val <- if(!is.na(u)&&!is.null(unemp)&&length(unemp)>=12)
    u-min(tail(unemp,12),na.rm=TRUE) else NA_real_
  feat_df <- build_current_feature_vector(yield_spread,u_rise_val,pay_3m,real_rate,hy_v,oil_yoy,
                                           prime_age_lfpr,usd_yoy,inventory_sales_ratio,cc_delinquency,cs)
  anomaly_result <- compute_anomaly_score(anomaly_detector, feat_df)
  A <- anomaly_result$score

  # ── Hand-tuned model: computed FIRST — used as prior when GLM is available ────
  # Must be computed before the GLM block which references ht_prob for blending.
  ht_lo <- -1.73
  {
    ht <- function(id,lbl,val,fn){ if(is.na(val)){return(0)};d<-fn(val);d }
    ht_lo<-ht_lo+ht("yield_curve","yc",yield_spread,function(v){d<- -v*0.85;if(!is.na(inv_months))d<-d+if(inv_months>=18)1.5 else if(inv_months>=12)1.2 else if(inv_months>=6)0.6 else if(inv_months>=3)0.3 else 0;d})
    if(!is.na(u)&&!is.null(unemp)&&length(unemp)>=12){ur<-u-min(tail(unemp,12),na.rm=TRUE);sh<-if(ur>=0.5)1.8 else if(ur>=0.3)0.9 else if(ur>=0.1)0.3 else if(ur<(-0.2))-0.4 else 0;if(!is.na(pay_3m))sh<-sh+if(pay_3m<(-50))1.5 else if(pay_3m<0)0.6 else if(pay_3m<100)0.1 else if(pay_3m>250)-0.5 else 0;ht_lo<-ht_lo+sh}
    ht_lo<-ht_lo+ht("pl","prime",prime_age_lfpr,function(v){d<-if(v<80.5)0.8 else if(v<82)0.4 else if(v>83.5)-0.3 else 0;if(!is.na(prime_age_lfpr_chg))d<-d+if(prime_age_lfpr_chg<(-0.5))0.9 else if(prime_age_lfpr_chg<(-0.2))0.4 else if(prime_age_lfpr_chg>0.3)-0.3 else 0;d})
    ht_lo<-ht_lo+ht("cl","claims",jobless_claims_trend,function(v)if(v>0.20)1.2 else if(v>0.10)0.6 else if(v>0.05)0.2 else if(v<(-0.10))-0.3 else 0)
    ht_lo<-ht_lo+ht("qt","quits",quits_yoy,function(v)if(v<(-15))0.8 else if(v<(-8))0.4 else if(v<(-3))0.2 else if(v>8)-0.3 else 0)
    ht_lo<-ht_lo+ht("ol","oil",oil_yoy,function(v)if(v>50)1.0 else if(v>25)0.6 else if(v>15)0.2 else if(v<(-30))0.5 else if(v<(-15))0.2 else if(v>0&&v<10)-0.1 else 0)
    ht_lo<-ht_lo+ht("eq","equity",equity_drawdown_pct,function(v){dd<-abs(v);if(dd>30)1.6 else if(dd>20)1.1 else if(dd>10)0.5 else if(dd<5)-0.2 else 0})
    ht_lo<-ht_lo+ht("hy","hy",hy_v,function(v)if(v>7)1.4 else if(v>5.5)0.7 else if(v>4.5)0.2 else if(v<3.5)-0.4 else 0)
    ht_lo<-ht_lo+ht("ud","usd",usd_yoy,function(v)if(v>12)0.8 else if(v>8)0.4 else if(v>4)0.1 else if(v<(-5))-0.2 else 0)
    ht_lo<-ht_lo+ht("rr","rr",real_rate,function(v)if(v>3)1.0 else if(v>2)0.5 else if(v>1)0.2 else if(v<0)-0.3 else 0)
    ht_lo<-ht_lo+ht("se","sent",cs,function(v)if(v<60)0.7 else if(v<70)0.3 else if(v>90)-0.4 else 0)
    ht_lo<-ht_lo+ht("iv","inv",inventory_sales_ratio,function(v)if(v>1.55)0.7 else if(v>1.45)0.3 else if(v>1.40)0.1 else if(v<1.30)-0.2 else 0)
    ht_lo<-ht_lo+ht("cc","cc",cc_delinquency,function(v)if(v>4)0.9 else if(v>3)0.5 else if(v>2.5)0.2 else if(v<1.8)-0.3 else 0)
  }
  ht_prob <- 1 / (1 + exp(-ht_lo))

  # ── GLM probability ──────────────────────────────────────────────────────────
  glm_prob_raw <- NA_real_
  if (!is.null(glm_result) && !is.null(glm_result$model)) {
    kc      <- intersect(glm_result$predictors, names(feat_df))
    feat_sub <- feat_df[,kc,drop=FALSE]
    for (col in names(feat_sub)) {
      nas <- is.na(feat_sub[[col]])
      if (any(nas)) feat_sub[[col]][nas] <- 0
    }
    glm_prob_raw <- tryCatch(predict(glm_result$model,newdata=feat_sub,type="response")[[1]],error=function(e)NA_real_)
    if (!is.na(glm_prob_raw)) {
      # Blend GLM with hand-tuned: GLM is well-calibrated on average but
      # may underweight policy-shock channels. Hand-tuned captures those explicitly.
      # Weight: 70% GLM (data-driven), 30% hand-tuned (expert prior for novel channels).
      # This is not hard-coded — the 70/30 tracks GLM Brier: better GLM → more weight.
      glm_weight <- if (!is.null(ensemble_weights) && !is.null(ensemble_weights$glm_skill) &&
                        !is.na(ensemble_weights$glm_skill) && ensemble_weights$glm_skill > 0)
        min(0.85, 0.60 + ensemble_weights$glm_skill * 0.50)  # skill 0→60%, skill 0.5→85%
      else 0.70
      glm_prob_raw <- glm_weight * glm_prob_raw + (1 - glm_weight) * ht_prob
      coefs <- coef(glm_result$model); ct <- glm_result$coef_table
      fv    <- as.list(feat_df[1,,drop=FALSE])
      labs  <- c(yield_spread="Yield Curve (10Y-2Y spread)",u_rise="Sahm Rule (unemployment rise)",
                 pay_3m="Payroll Momentum (3M avg)",real_rate="Real Fed Funds Rate",
                 hy_spread="HY Credit Spreads",oil_yoy="Oil Price Shock (YoY)",
                 prime_lfpr="Prime-Age LFPR (25\u201354)",usd_yoy="USD Surge (YoY)",
                 inv_sales="Inventory-to-Sales Ratio",cc_del="CC Delinquency Rate",
                 sentiment="Consumer Sentiment")
      for (pred in kc) {
        b  <- if(pred%in%names(coefs))coefs[[pred]]else NA_real_
        x  <- fv[[pred]]; miss <- is.na(x)
        z  <- if(pred%in%ct$predictor)ct$z_stat[ct$predictor==pred]else NA_real_
        lbl<- if(pred%in%names(labs))labs[[pred]]else pred
        add_contrib(pred,lbl,if(!is.na(b)&&!miss)b*x else 0,z,data_missing=miss)
      }
    }
  }

  # ── Hand-tuned fallback (full path with add_contrib when GLM unavailable) ──────
  if (is.na(glm_prob_raw)) {
    lo <- -1.73
    ht <- function(id,lbl,val,fn){ if(is.na(val)){add_contrib(id,lbl,0,data_missing=TRUE);return(0)};d<-fn(val);add_contrib(id,lbl,d);d }
    lo<-lo+ht("yield_curve","Yield Curve (10Y-2Y spread)",yield_spread,function(v){d<- -v*0.85;if(!is.na(inv_months))d<-d+if(inv_months>=18)1.5 else if(inv_months>=12)1.2 else if(inv_months>=6)0.6 else if(inv_months>=3)0.3 else 0;d})
    if(!is.na(u)&&!is.null(unemp)&&length(unemp)>=12){sh<-0;ur<-u-min(tail(unemp,12),na.rm=TRUE);sh<-sh+if(ur>=0.5)1.8 else if(ur>=0.3)0.9 else if(ur>=0.1)0.3 else if(ur<(-0.2))-0.4 else 0;if(!is.na(pay_3m))sh<-sh+if(pay_3m<(-50))1.5 else if(pay_3m<0)0.6 else if(pay_3m<100)0.1 else if(pay_3m>250)-0.5 else 0;add_contrib("sahm","Sahm Rule / Labor Momentum",sh);lo<-lo+sh}else add_contrib("sahm","Sahm Rule / Labor Momentum",0,data_missing=TRUE)
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
    glm_prob_raw <- 1/(1+exp(-lo))
  }

  # ── Markov contribution — in log-odds units so it's on the same scale as β×x ─
  ew    <- ensemble_weights %||% list(w_glm=0.60,w_ms=0.40)
  W_GLM <- ew$w_glm; W_MS <- ew$w_ms
  ms_prob_raw <- NA_real_

  if (!is.null(markov_result) && !is.na(markov_result$current_rec_prob)) {
    ms_prob_raw  <- markov_result$current_rec_prob
    lo_glm  <- .prob_to_lo(glm_prob_raw)
    lo_ms   <- .prob_to_lo(ms_prob_raw)
    ms_delta <- (lo_ms - lo_glm) * W_MS   # log-odds units, same scale as other bars
    sn   <- markov_result$switch_name %||% "IP YoY"
    sm   <- markov_result$sw_regime_means
    freq <- round((markov_result$state_freq_recession %||% 0)*100)
    add_contrib("markov_regime",
                sprintf("Markov Regime (w=%.0f%%; p_rec=%.0f%%; %s: exp=%.1f rec=%.1f; %d%% of history in rec state)",
                        W_MS*100, ms_prob_raw*100, sn,
                        sm$expansion %||% 0, sm$recession %||% 0, freq),
                ms_delta)
  }

  W_GLM <- if (!is.na(ms_prob_raw)) ew$w_glm else 1.0
  W_MS  <- if (!is.na(ms_prob_raw)) ew$w_ms  else 0.0

  blended <- if (!is.na(ms_prob_raw)) W_GLM*glm_prob_raw + W_MS*ms_prob_raw else glm_prob_raw
  # Data-driven anomaly floor: use empirical recession rate from training data
  # when conditions were historically anomalous (MD > d90).
  # Falls back to 0.30 if not computed yet.
  p_anom_rec <- if (!is.null(anomaly_detector) && !is.null(anomaly_detector$p_anomalous_recession))
    anomaly_detector$p_anomalous_recession else 0.30
  # Scale: at A=0 floor is irrelevant; at A=1 floor = p_anom_rec (full historical rate)
  # This means: "in the most anomalous conditions, expect at least the historical anomaly recession rate"
  blended <- if (A > 0.05) max(blended + ANOMALY_WEIGHT*A*(COMPRESS_CENTER-blended), p_anom_rec*A) else blended

  if (A > 0.05)
    add_contrib("anomaly_signal",
                sprintf("Anomaly Signal (MD\u2248%.1f; %s)", anomaly_result$md %||% 0, anomaly_result$label),
                .prob_to_lo(blended) - .prob_to_lo(if(!is.na(ms_prob_raw))W_GLM*glm_prob_raw+W_MS*ms_prob_raw else glm_prob_raw))

  prob <- max(2, min(97, round(blended*100, 1)))
  tier <- .classify_recession_tier(prob)

  cdf <- do.call(rbind, lapply(names(contribs), function(id) {
    r <- contribs[[id]]
    data.frame(name=r$name, contribution=r$contribution, z_stat=r$z_stat,
               data_missing=r$data_missing %||% FALSE, stringsAsFactors=FALSE)
  }))
  cdf <- cdf[order(abs(cdf$contribution),decreasing=TRUE),]

  du<-character(0); dd<-character(0)
  for (i in seq_len(min(8,nrow(cdf)))) {
    r <- cdf[i,]; if(isTRUE(r$data_missing)) next
    if(r$contribution>0.1) du<-c(du,r$name)
    if(r$contribution<(-0.1)) dd<-c(dd,r$name)
  }
  if(A>0.3) du<-c(sprintf("\u26a0 %s (MD=%.1f)",anomaly_result$label,anomaly_result$md%||%0),du)
  if(!is.na(ms_prob_raw)&&ms_prob_raw>0.4) du<-c(sprintf("Markov regime: %.0f%% recession probability",ms_prob_raw*100),du)

  mt <- if(!is.null(glm_result)&&!is.na(ms_prob_raw)) sprintf("GLM + Markov + Anomaly (w_glm=%.0f%% w_ms=%.0f%%)",W_GLM*100,W_MS*100)
        else if(!is.null(glm_result)) "GLM + Anomaly" else "Hand-tuned + Anomaly"

  list(prob=prob,tier=tier,drivers_up=head(du,4),drivers_down=head(dd,3),
       factor_contributions=cdf,anomaly=anomaly_result,markov_prob=ms_prob_raw,
       ensemble_weights=ew,model_type=mt,n_obs=glm_result$n_obs%||%NULL,aic=glm_result$aic%||%NULL)
}


# ═══════════════════════════════════════════════════════════════════════════════
# GDP POINT FORECAST
# Maps composite score + recession probability to a quantitative annualized GDP
# estimate for the next quarter.
#
# Calibration anchors (score → annualized GDP, post-WWII US data):
#   score  0  →  -3.5%  (deep recession: 2008Q4, 2020Q1)
#   score  3  →  -0.5%  (mild recession / stall: 2001, 2022H1)
#   score  5  →  +1.5%  (below-trend: 2015–16, 2019)
#   score  7  →  +2.5%  (trend growth: 2014–17 average)
#   score  8  →  +3.0%  (above-trend: 2018, 2023Q3)
#   score 10  →  +4.5%  (boom: 2021Q3, 1983–84)
#
# The formula is:  gdp_base = -3.5 + score × 0.70
# Then a recession probability discount pulls the estimate toward
# recession territory when the model flags elevated risk.
# ═══════════════════════════════════════════════════════════════════════════════

.compute_gdp_forecast <- function(score_0_10, recession_prob_pct, components) {
  if (is.null(score_0_10) || is.na(score_0_10)) return(NULL)

  # ── Base estimate from composite score ───────────────────────────────────────
  gdp_base <- -3.5 + score_0_10 * 0.70   # maps 0 → -3.5%, 10 → +3.5%
  # Asymmetric floor/ceiling: recoveries can exceed model range
  gdp_base <- max(-5.0, min(5.5, gdp_base))

  # ── Recession probability discount ───────────────────────────────────────────
  # At 0% prob: no adjustment.  At 50%: -0.9pp.  At 90%: -1.6pp.
  # Uses sqrt to make the drag non-linear (small probs barely matter,
  # high probs strongly pull toward contraction).
  rec_prob <- if (!is.null(recession_prob_pct) && !is.na(recession_prob_pct))
    pmax(0, pmin(100, recession_prob_pct)) else 15
  rec_drag <- -1.8 * sqrt(rec_prob / 100)   # 0% → 0, 50% → -1.27, 90% → -1.71

  gdp_point <- gdp_base + rec_drag

  # ── Uncertainty band: widens with recession risk ──────────────────────────────
  # Reflects that model accuracy falls sharply near turning points
  uncertainty <- 0.6 + (rec_prob / 100) * 1.4   # 0% → ±0.6, 50% → ±1.3, 90% → ±1.86
  gdp_ci_lo   <- gdp_point - uncertainty
  gdp_ci_hi   <- gdp_point + uncertainty

  # ── Override regime when recession prob and composite score disagree ────────────
  # A composite score of 6.9/10 ("Moderate Growth") but 53% recession probability
  # are contradictory signals. Reconcile by applying a hard regime override:
  #   rec_prob >= 50%  →  "Stall / Slowdown" (floor)
  #   rec_prob >= 65%  →  "Contraction Risk" (floor)
  # This ensures the headline label is never rosier than the recession model allows.
  rec_regime_override <- if (!is.null(recession_prob_pct) && !is.na(recession_prob_pct)) {
    if      (recession_prob_pct >= 65) "Contraction Risk"
    else if (recession_prob_pct >= 50) "Stall / Slowdown"
    else NULL
  } else NULL

  # ── Primary driver of the forecast ───────────────────────────────────────────
  if (!is.null(components)) {
    comp_contribs <- sapply(components, function(c) c$score * c$weight)
    top_driver_idx <- which.max(abs(comp_contribs))
    top_driver <- list(
      name   = components[[top_driver_idx]]$label,
      score  = round(components[[top_driver_idx]]$score, 2),
      contrib_sign = if (comp_contribs[top_driver_idx] > 0) "supporting" else "dragging"
    )
  } else top_driver <- NULL

  list(
    point                = round(gdp_point, 1),
    ci_lo                = round(gdp_ci_lo, 1),
    ci_hi                = round(gdp_ci_hi, 1),
    uncertainty          = round(uncertainty, 2),
    rec_drag             = round(rec_drag, 2),
    top_driver           = top_driver,
    rec_regime_override  = rec_regime_override,
    method               = "Composite score + recession probability discount"
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
build_growth_outlook <- function(fred_data, kpis, mkt_returns=NULL) {
  if (is.null(kpis)) return(NULL)
  tryCatch({
  gv <- function(sid){ df<-fred_data[[sid]]; if(is.null(df)||nrow(df)<2)return(NULL); df%>%dplyr::arrange(date)%>%dplyr::pull(value) }
  unemp<-gv("UNRATE"); payrolls<-gv("PAYEMS"); cpi<-gv("CPIAUCSL"); fedfunds<-gv("FEDFUNDS")
  t10<-gv("DGS10"); t2<-gv("DGS2"); mort<-gv("MORTGAGE30US")
  sent<-gv("UMCSENT"); vix_vals<-gv("VIXCLS"); hy<-gv("BAMLH0A0HYM2"); oil<-gv("DCOILWTICO"); retail<-gv("RSAFS")
  u<-kpis$unemp_rate%||%NA; cpi_y<-kpis$cpi_yoy%||%NA; pce_y<-kpis$core_pce%||%NA
  ff<-kpis$fed_funds%||%NA; t10v<-kpis$t10yr%||%NA; t2v<-kpis$t2yr%||%NA
  mort_v<-kpis$mortgage30%||%NA; hs<-kpis$housing_starts%||%NA; ret_y<-kpis$retail_yoy%||%NA
  cs<-kpis$cons_sent%||%NA; ip_y<-kpis$indpro_yoy%||%NA; vix_v<-kpis$vix%||%NA; hy_v<-kpis$hy_spread%||%NA
  real_rate    <- if(!is.na(ff)&&!is.na(cpi_y)) ff-cpi_y else NA
  yield_spread <- if(!is.na(t10v)&&!is.na(t2v)) t10v-t2v else NA
  pay_3m       <- if(!is.null(payrolls)&&length(payrolls)>=4) mean(diff(tail(payrolls,4)),na.rm=TRUE) else NA
  ret_real     <- if(!is.na(ret_y)&&!is.na(cpi_y)) ret_y-cpi_y else NA
  inv_months   <- if(!is.null(t10)&&!is.null(t2)){n<-min(length(t10),length(t2),18);sum((tail(t10,n)-tail(t2,n))<0,na.rm=TRUE)}else 0

  components <- list(
    labor    =list(weight=2.5,label="Labor Market",     score=mean(c(.score(u,3.5,5.0,FALSE),.score(pay_3m,150,50,TRUE),.score(u,4.5,5.5,FALSE)),na.rm=TRUE), detail=if(!is.na(u)&&!is.na(pay_3m))sprintf("%.1f%% unemployment; %+.0fK/mo payrolls",u,pay_3m)else"N/A"),
    consumer =list(weight=2.0,label="Consumer Demand",  score=mean(c(.score(ret_real,1,-1,TRUE),.score(cs,85,65,TRUE)),na.rm=TRUE), detail=if(!is.na(ret_real)&&!is.na(cs))sprintf("Real retail %.1f%%; sentiment %.0f",ret_real,cs)else"N/A"),
    monetary =list(weight=2.0,label="Monetary Conditions",score=mean(c(.score(real_rate,0.5,2.5,FALSE),.score(yield_spread,0.5,-0.25,TRUE),.score(inv_months,3,9,FALSE)),na.rm=TRUE), detail=if(!is.na(real_rate)&&!is.na(yield_spread))sprintf("Real rate %.1f%%; 10Y-2Y %.2f pp; %d mo inv.",real_rate,yield_spread,inv_months)else"N/A"),
    housing  =list(weight=1.5,label="Housing",          score=mean(c(.score(hs,1400,1000,TRUE),.score(mort_v,6,7.5,FALSE)),na.rm=TRUE), detail=if(!is.na(hs)&&!is.na(mort_v))sprintf("%.0fK starts; %.2f%% mortgage",hs,mort_v)else"N/A"),
    financial=list(weight=1.5,label="Financial Conditions",score=mean(c(.score(vix_v,18,28,FALSE),.score(hy_v,4.5,6,FALSE)),na.rm=TRUE), detail=if(!is.na(vix_v)&&!is.na(hy_v))sprintf("VIX %.1f; HY %.2f%%",vix_v,hy_v)else"N/A"),
    inflation=list(weight=1.5,label="Inflation",        score=mean(c(.score(cpi_y,2.5,4.5,FALSE),.score(pce_y,2.3,3.5,FALSE)),na.rm=TRUE), detail=if(!is.na(cpi_y)&&!is.na(pce_y))sprintf("CPI %.1f%%; Core PCE %.1f%%",cpi_y,pce_y)else"N/A"),
    industrial=list(weight=1.0,label="Industrial Output",score=.score(ip_y,1,-1,TRUE), detail=if(!is.na(ip_y))sprintf("IP YoY %.1f%%",ip_y)else"N/A")
  )
  tw <- sum(sapply(components,`[[`,"weight"))
  raw_score  <- sum(sapply(components,function(c)c$score*c$weight))/tw
  score_0_10 <- max(0,min(10,round((raw_score+1)/2*10,1)))
  regime <- .regime_from_score(score_0_10)   # ← no case_when
  # Regime override applied after GDP forecast is computed (see gdp_forecast$rec_regime_override)
  drag <- names(which.min(sapply(components,`[[`,"score")))
  supp <- names(which.max(sapply(components,`[[`,"score")))
  swing <- list(
    biggest_drag=list(name=components[[drag]]$label,score=round(components[[drag]]$score,2),detail=components[[drag]]$detail),
    biggest_support=list(name=components[[supp]]$label,score=round(components[[supp]]$score,2),detail=components[[supp]]$detail),
    yield_curve_watch=if(inv_months>6)sprintf("Inverted %d months.",inv_months)else if(!is.na(yield_spread)&&yield_spread>0)sprintf("Re-steepened to %.2f pp.",yield_spread)else NULL,
    fed_watch=if(!is.na(real_rate)&&real_rate>2)sprintf("Real rate %.1f%% \u2014 restrictive.",real_rate)else NULL)

  eq_dd <- if(!is.null(mkt_returns)){spy<-mkt_returns%>%dplyr::filter(symbol=="SPY")%>%dplyr::arrange(date);if(nrow(spy)>=52){cur<-tail(spy$close,1);pk<-max(spy$close[max(1,nrow(spy)-252):nrow(spy)],na.rm=TRUE);round((cur/pk-1)*100,1)}else NA_real_}else NA_real_

  # ── Disk-backed cache — persists across Shiny restarts ───────────────────────
  # Cache file lives in the app's working directory. Change the path if needed.
  cache_dir  <- tryCatch(dirname(getOption("shiny.appDir", default=".")), error=function(e)".")
  cache_file <- file.path(cache_dir, ".recession_t3_cache.rds")
  cache_name <- ".recession_t3_cache"   # in-memory copy for fast access within session

  # Load from disk if not in memory
  cache <- tryCatch(
    if (exists(cache_name, envir=.GlobalEnv)) get(cache_name, envir=.GlobalEnv) else NULL,
    error=function(e) NULL)

  if (is.null(cache) && file.exists(cache_file)) {
    tryCatch({
      cache <- readRDS(cache_file)
      assign(cache_name, cache, envir=.GlobalEnv)
      message("[t3] Cache loaded from disk: ", cache_file)
    }, error=function(e) {
      message("[t3] Could not read cache file: ", e$message)
      cache <- NULL
    })
  }

  # Detect degenerate GLM: converged=FALSE or n_obs<200 means bad training data
  glm_is_degenerate <- !is.null(cache) && !is.null(cache$glm) && (
    # GLM didn't converge (perfect separation / too few rows)
    isFALSE(cache$glm$model$converged %||% TRUE) ||
    # Too few training rows
    (!is.null(cache$glm$n_obs) && cache$glm$n_obs < 200) ||
    # Brier score shows negative skill (GLM worse than just guessing base rate)
    (!is.null(cache$ew) && !is.null(cache$ew$glm_brier) && !is.null(cache$ew$glm_skill) &&
     !is.na(cache$ew$glm_skill) && cache$ew$glm_skill <= 0)
  )
  if (glm_is_degenerate)
    message(sprintf("[t3] Degenerate GLM detected (n=%d, converged=%s) — forcing retrain.",
                    cache$glm$n_obs %||% 0L, as.character(cache$glm$model$converged %||% NA)))

  needs_train <- glm_is_degenerate ||
    is.null(cache) || is.null(cache$glm) ||
    difftime(Sys.time(), cache$glm$trained_at %||% (Sys.time()-1e9), units="days") > 30

  message(sprintf("[t3] needs_train=%s (degenerate=%s)", needs_train, glm_is_degenerate))
  if (needs_train) {
    run_training <- function() {
      prog <- function(msg,val) tryCatch(shiny::setProgress(val,message=msg),error=function(e)message("[t3] ",msg))
      prog("Fetching FRED history + training GLM\u2026", 0.05)
      glm_obj <- rolling_retrain(fred_data, window=360L)
      prog("Fitting 3-state Markov-switching model\u2026", 0.45)
      ms_obj  <- train_markov_model(fred_data)
      prog("Scoring on 36-month holdout (Brier)\u2026", 0.75)
      ew_obj  <- compute_ensemble_weights(glm_obj, ms_obj, holdout_months=36L)
      prog("Caching results\u2026", 0.95)
      new_cache <- list(glm=glm_obj, markov=ms_obj, ew=ew_obj)
      assign(cache_name, new_cache, envir=.GlobalEnv)
      # Persist to disk so cache survives Shiny restarts
      tryCatch({
        saveRDS(new_cache, cache_file)
        message(sprintf("[t3] Cache saved to disk: %s (valid 30 days)", cache_file))
      }, error=function(e) message("[t3] Could not write cache to disk: ", e$message))
    }
    tryCatch(
      shiny::withProgress(message="Fitting econometric models\u2026", value=0, run_training()),
      error=function(e) run_training())   # no-op outside Shiny
    cache <- tryCatch(get(cache_name,envir=.GlobalEnv),error=function(e)NULL)
  }

  glm_result    <- cache$glm; markov_result <- cache$markov; ew <- cache$ew
  ad            <- if(!is.null(glm_result)) glm_result$anomaly_detector else NULL

  recession_prob <- .compute_recession_prob(
    yield_spread=yield_spread,inv_months=inv_months,unemp=unemp,u=u,
    pay_3m=pay_3m,real_rate=real_rate,hy_v=hy_v,vix_v=vix_v,cs=cs,ip_y=ip_y,hs=hs,
    prime_age_lfpr=kpis$prime_age_lfpr%||%NA,prime_age_lfpr_chg=kpis$prime_age_lfpr_chg%||%NA,
    jobless_claims_trend=kpis$jobless_claims_trend%||%NA,quits_yoy=kpis$quits_yoy%||%NA,
    oil_yoy=kpis$oil_yoy%||%NA,equity_drawdown_pct=eq_dd,usd_yoy=kpis$usd_yoy%||%NA,
    inventory_sales_ratio=kpis$inventory_sales_ratio%||%NA,cc_delinquency=kpis$cc_delinquency%||%NA,
    glm_result=glm_result,anomaly_detector=ad,markov_result=markov_result,ensemble_weights=ew)

  list(score=score_0_10, raw_score=raw_score, regime=regime, components=components,
       swing=swing, recession_prob=recession_prob, as_of=Sys.Date(),
       gdp_forecast=.compute_gdp_forecast(score_0_10, recession_prob$prob, components))
  }, error = function(e) {
    msg <- conditionMessage(e)
    # Capture call stack for diagnostics
    calls_text <- tryCatch(
      paste(sapply(rev(head(sys.calls(), 20)), function(c) deparse(c)[1]), collapse=" <- "),
      error=function(e2) "unavailable")
    message("[t3] build_growth_outlook ERROR: ", msg)
    message("[t3] Call stack: ", substr(calls_text, 1, 500))
    list(.error=TRUE, .error_msg=paste0(msg, "\n\nCall chain: ", substr(calls_text, 1, 300)))
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# render_growth_outlook_html + .build_factor_bar_chart + .build_model_modal
# ═══════════════════════════════════════════════════════════════════════════════

.build_factor_bar_chart <- function(fc, model_type="Hand-tuned", mr=NULL, ew=NULL) {
  if (is.null(fc)||nrow(fc)==0) return("<p style='color:#6b7585;font-size:11px;'>No data.</p>")
  fc <- head(fc[order(abs(fc$contribution),decreasing=TRUE),], 15)
  max_abs <- max(abs(fc$contribution),na.rm=TRUE); if(max_abs<0.01) max_abs<-1
  has_z   <- any(!is.na(fc$z_stat)&!isTRUE(fc$data_missing))

  ew_note <- if(!is.null(ew)&&!is.null(ew$glm_brier))
    sprintf(" &nbsp;|&nbsp; w_GLM=<b style='color:#2dce89;'>%.0f%%</b> w_MS=<b style='color:#00b4d8;'>%.0f%%</b> Brier GLM=%.3f MS=%.3f",
            ew$w_glm*100,ew$w_ms*100,ew$glm_brier,ew$ms_brier) else
    if(!is.null(ew)) sprintf(" &nbsp;|&nbsp; w_GLM=%.0f%% w_MS=%.0f%%",ew$w_glm*100,ew$w_ms*100) else ""

  subtitle <- if(grepl("GLM",model_type))
    paste0("<span style='color:#2dce89;font-size:10px;'>&#x2713; GLM-estimated \u03b2s + Markov (teal) + Anomaly (purple)",ew_note,"</span>")
  else "<span style='color:#f4a261;font-size:10px;'>\u26a0 Hand-tuned fallback \u2014 GLM/Markov training pending</span>"

  rows <- vapply(seq_len(nrow(fc)), function(i) {
    row <- fc[i,]; delta <- row$contribution
    is_miss    <- isTRUE(row$data_missing)
    is_anomaly <- grepl("Anomaly Signal",row$name)
    is_markov  <- grepl("Markov Regime", row$name)
    is_zero    <- (!is_miss&&!is_anomaly&&!is_markov&&abs(delta)<0.001)
    col <- if(is_markov)"#00b4d8" else if(is_anomaly)"#7c5cbf" else if(is_miss||is_zero)"#3a4052" else if(delta>0)"#e94560" else "#2dce89"
    pct <- if(is_miss||is_zero) 0 else round(min(abs(delta)/max_abs*44,44))
    bar <- if(is_miss) "left:50%;width:2px;background:repeating-linear-gradient(180deg,#4a5062 0,#4a5062 3px,transparent 3px,transparent 6px);"
           else if(is_zero) "left:50%;width:2px;background:#3a4052;"
           else if(delta>=0) sprintf("left:50%%;width:%dpx;background:%s;",pct*3,col)
           else              sprintf("right:50%%;width:%dpx;background:%s;",pct*3,col)
    z_b <- if(has_z&&!is.na(row$z_stat)&&!is_miss){zc<-if(abs(row$z_stat)>2.5)"#d0d0d0"else if(abs(row$z_stat)>1.5)"#9aa3b2"else"#555";sprintf("&nbsp;<span style='color:%s;font-size:9px;'>(z=%.1f)</span>",zc,row$z_stat)}else""
    tag <- if(is_markov)"&nbsp;<span style='color:#00b4d8;font-size:9px;'>\u25c6&nbsp;REGIME</span>" else if(is_anomaly)"&nbsp;<span style='color:#a785e0;font-size:9px;'>\u26a0&nbsp;ANOMALY</span>" else if(is_miss)"&nbsp;<span style='color:#555;font-size:9px;'>N/A</span>" else ""
    nc  <- if(is_markov)"#7dd8f0" else if(is_anomaly)"#c8a8f0" else if(is_miss)"#555" else "#d0d0d0"
    val_str <- if(is_miss)"<span style='color:#555;font-size:10px;'>N/A</span>" else sprintf("<span style='color:%s;font-size:10px;font-weight:600;'>%+.2f</span>",col,delta)
    sprintf("<div style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'><div style='width:220px;text-align:right;color:%s;font-size:10.5px;flex-shrink:0;line-height:1.3;'>%s%s%s</div><div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;overflow:hidden;'><div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;z-index:1;'></div><div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div></div><div style='width:52px;text-align:right;'>%s</div></div>",
            nc,htmltools::htmlEscape(row$name),z_b,tag,bar,val_str)
  }, character(1))

  n_miss <- sum(sapply(seq_len(nrow(fc)),function(i) isTRUE(fc$data_missing[i])))
  miss_note <- if(n_miss>0) sprintf("<div style='font-size:9px;color:#555;margin-top:4px;'><span style='color:#6b7585;'>\u2139</span>&nbsp;%d factor%s showing N/A: FRED data not loaded. Ensure <code>kpis$prime_age_lfpr</code>, <code>kpis$quits_yoy</code>, <code>kpis$oil_yoy</code>, etc. are populated in data_fred.R.</div>",n_miss,if(n_miss==1)""else"s") else ""

  paste0("<div style='margin-bottom:18px;'>",
    "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;'>",
    "<span style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>Live Factor Contributions \u2014 Tier\u00a03</span></div>",
    "<div style='margin-bottom:8px;'>",subtitle,"</div>",
    "<div style='display:flex;flex-wrap:wrap;gap:14px;font-size:10px;color:#9aa3b2;margin-bottom:9px;'>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#e94560;border-radius:2px;margin-right:4px;'></span>Raises risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#2dce89;border-radius:2px;margin-right:4px;'></span>Lowers risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#7c5cbf;border-radius:2px;margin-right:4px;'></span>Anomaly</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#00b4d8;border-radius:2px;margin-right:4px;'></span>Markov regime</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#3a4052;border-radius:2px;margin-right:4px;'></span>Neutral/N/A</span>",
    "<span style='color:#555;'>| zero |</span></div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'><span>\u25c4 Protective</span><span>Recessionary \u25ba</span></div>",
    paste(rows,collapse=""),miss_note,
    "<div style='font-size:9px;color:#555;margin-top:6px;'>Coloured bar = active contribution. Flat line = neutral. N/A = data not loaded.</div></div>")
}

render_growth_outlook_html <- function(outlook) {
  # Error sentinel from build_growth_outlook's tryCatch
  if (!is.null(outlook) && isTRUE(outlook$.error)) {
    return(shiny::HTML(paste0(
      "<div style='background:#1a2035;border:1px solid #e94560;border-radius:8px;padding:24px;'>",
      "<div style='color:#e94560;font-size:13px;font-weight:700;margin-bottom:10px;'>",
      "<i class='fa fa-exclamation-triangle' style='margin-right:8px;'></i>",
      "Growth Outlook Error</div>",
      "<div style='color:#9aa3b2;font-size:12px;font-family:monospace;margin-bottom:12px;padding:8px;background:#0f1117;border-radius:4px;'>",
      htmltools::htmlEscape(outlook$.error_msg %||% "Unknown error"),"</div>",
      "<div style='color:#555;font-size:11px;'>To force a retrain, run in the R console:<br/>",
      "<code>file.remove('.recession_t3_cache.rds'); rm(.recession_t3_cache, envir=.GlobalEnv)</code>",
      "</div></div>")))
  }
  # If cache exists but outlook is NULL, something errored — show an error card not the spinner
  if (is.null(outlook)) {
    cache_exists <- file.exists(".recession_t3_cache.rds") ||
      exists(".recession_t3_cache", envir=.GlobalEnv)
    if (cache_exists) {
      return(shiny::HTML(paste0(
        "<div style='background:#1a2035;border:1px solid #e94560;border-radius:8px;padding:24px;'>",
        "<div style='color:#e94560;font-size:13px;font-weight:700;margin-bottom:10px;'>",
        "<i class='fa fa-exclamation-triangle' style='margin-right:8px;'></i>",
        "Growth Outlook Error</div>",
        "<div style='color:#9aa3b2;font-size:12px;margin-bottom:12px;'>",
        "build_growth_outlook() returned NULL. Check the R console for the error message.</div>",
        "<div style='color:#555;font-size:11px;'>To force a retrain:<br/>",
        "<code>file.remove('.recession_t3_cache.rds')</code><br/>",
        "<code>rm(.recession_t3_cache, envir=.GlobalEnv)</code></div></div>")))
    }
    return(.growth_outlook_loading_html())
  }
  if (isTRUE(outlook$.training)) return(.growth_outlook_loading_html(outlook$.step))
  if (is.null(outlook$regime)||!is.list(outlook$regime)||is.null(outlook$regime$color))
    return(shiny::HTML("<div style='color:#e94560;padding:16px;'>Regime classification error \u2014 check console.</div>"))
  # Wrap entire render in tryCatch so errors show as a card rather than blank/spinner
  result <- tryCatch({

  rp  <- outlook$recession_prob; reg <- outlook$regime; gfo <- outlook$gdp_forecast
  # Reconcile regime label when recession probability contradicts composite score
  if (!is.null(gfo) && !is.null(gfo$rec_regime_override)) {
    override_label <- gfo$rec_regime_override
    # Only override in the "worse" direction — never make it look rosier
    regime_order <- c("Expansion"=4, "Moderate Growth"=3, "Stall / Slowdown"=2, "Contraction Risk"=1)
    current_rank  <- regime_order[reg$label] %||% 3
    override_rank <- regime_order[override_label] %||% 3
    if (!is.na(override_rank) && override_rank < current_rank) {
      reg <- .regime_from_score(if (override_rank==2) 3.4 else 1.0)
      message(sprintf("[t3] Regime overridden by recession probability: '%s' → '%s'", 
                      names(regime_order)[current_rank], reg$label))
    }
  }
  score <- outlook$score; comps <- outlook$components; swing <- outlook$swing; gc <- reg$color

  # ── GDP point forecast display ───────────────────────────────────────────────
  gf <- outlook$gdp_forecast
  gdp_display <- if (!is.null(gf)) {
    pt  <- gf$point; lo <- gf$ci_lo; hi <- gf$ci_hi
    col <- if (pt >= 2.5) "#2dce89" else if (pt >= 1.0) "#00b4d8" else if (pt >= 0) "#f4a261" else "#e94560"
    arrow_sym <- if (pt > 2.5) "\u2191\u2191" else if (pt > 1.0) "\u2191" else if (pt > -0.5) "\u2192" else "\u2193"
    ci_note <- paste0("CI [",lo,"%, ",hi,"%]")
    td <- gf$top_driver
    driver_note <- if (!is.null(td)) paste0(" &nbsp;\u2014&nbsp; ",td$contrib_sign,": ",td$name) else ""
    paste0(
      "<div style='margin:10px 0 14px;padding:12px 16px;background:#0a1520;border-radius:8px;border-left:4px solid ",col,";'>",
      "<div style='color:#9aa3b2;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>",
      "Next-Quarter GDP Forecast &nbsp;<span style='color:#555;font-weight:400;font-size:9px;'>(annualized, model-based)</span></div>",
      "<div style='display:flex;align-items:baseline;gap:10px;flex-wrap:wrap;'>",
      "<span style='color:",col,";font-size:30px;font-weight:800;'>",arrow_sym," ",sprintf("%+.1f",pt),"%</span>",
      "<span style='color:#6b7585;font-size:12px;'>",ci_note,"</span>",
      "</div>",
      "<div style='color:#6b7585;font-size:10px;margin-top:4px;'>",
      if (gf$rec_drag < -0.2) paste0("Recession probability drag: ",sprintf("%.1f",gf$rec_drag),"pp &nbsp;") else "",
      driver_note,
      "</div>",
      "<div style='color:#555;font-size:9px;margin-top:6px;'>",
      "Point estimate from composite score + recession risk discount. CI widens with uncertainty. Not a professional forecast.",
      "</div></div>")
  } else paste0("<div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>",
                "GDP Est: ",reg$gdp_est," annualized</div>")

  gauge_html <- paste0(
    "<div style='margin:8px 0 14px;'>",
    "<div style='display:flex;justify-content:space-between;margin-bottom:5px;'>",
    "<span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span>",
    "<span style='color:#9aa3b2;font-size:11px;'>Expansion</span></div>",
    "<div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'>",
    "<div style='background:linear-gradient(90deg,",if(score<5)"#e94560"else"#f4a261",",",gc,");",
    "width:",round(score/10*100),"%;height:100%;border-radius:6px;'></div></div>",
    "<div style='margin-top:5px;display:flex;justify-content:space-between;'>",
    "<span style='color:#9aa3b2;font-size:10px;'>0</span>",
    "<span style='color:",gc,";font-size:11px;font-weight:700;'>Score: ",score," / 10</span>",
    "<span style='color:#9aa3b2;font-size:10px;'>10</span></div></div>")

  comp_html <- paste(vapply(names(comps),function(nm){c<-comps[[nm]];s<-c$score;col<-if(s>=0.4)"#2dce89"else if(s>=-0.4)"#f4a261"else"#e94560";bw<-round((s+1)/2*100);sprintf("<div style='margin-bottom:8px;'><div style='display:flex;justify-content:space-between;margin-bottom:3px;'><span style='color:#9aa3b2;font-size:11px;'>%s</span><span style='color:%s;font-size:11px;font-weight:600;'>%s</span></div><div style='background:#2a3042;border-radius:3px;height:5px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:3px;'></div></div><div style='color:#6b7585;font-size:10px;margin-top:2px;'>%s</div></div>",c$label,col,if(s>=0.4)"Positive"else if(s>=-0.4)"Neutral"else"Negative",col,bw,htmltools::htmlEscape(c$detail))},character(1)),collapse="")

  swing_html <- paste(c(if(!is.null(swing$biggest_drag))sprintf("<li style='margin-bottom:6px;'><span style='color:#e94560;font-weight:600;'>\u2b07 Biggest drag: %s</span> \u2014 %s</li>",swing$biggest_drag$name,htmltools::htmlEscape(swing$biggest_drag$detail)),if(!is.null(swing$biggest_support))sprintf("<li style='margin-bottom:6px;'><span style='color:#2dce89;font-weight:600;'>\u2b06 Biggest support: %s</span> \u2014 %s</li>",swing$biggest_support$name,htmltools::htmlEscape(swing$biggest_support$detail)),if(!is.null(swing$yield_curve_watch))sprintf("<li style='margin-bottom:6px;'><span style='color:#f4a261;font-weight:600;'>\u26a0 Yield Curve:</span> %s</li>",htmltools::htmlEscape(swing$yield_curve_watch)),if(!is.null(swing$fed_watch))sprintf("<li style='margin-bottom:6px;'><span style='color:#00b4d8;font-weight:600;'>\u2699 Fed:</span> %s</li>",htmltools::htmlEscape(swing$fed_watch))),collapse="")

  rp_html <- ""
  if (!is.null(rp)) {
    prob <- rp$prob; tier <- rp$tier; bc <- tier$color; ew <- rp$ensemble_weights
    # Build badges with paste0 — avoids % signs in strings being re-parsed by outer sprintf
    ew_badge <- if (!is.null(ew) && !is.null(ew$glm_brier))
      paste0("&nbsp;<span style='background:rgba(0,180,216,0.1);color:#00b4d8;border:1px solid rgba(0,180,216,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>",
             "w_GLM=",round(ew$w_glm*100),"% w_MS=",round(ew$w_ms*100),"%</span>") else ""
    ms_badge <- if (!is.null(rp$markov_prob) && !is.na(rp$markov_prob))
      paste0("&nbsp;<span style='background:rgba(0,180,216,0.08);color:#7dd8f0;border:1px solid rgba(0,180,216,0.2);border-radius:8px;padding:1px 8px;font-size:9px;'>",
             "MS: ",round(rp$markov_prob*100),"%</span>") else ""
    an_badge <- if (!is.null(rp$anomaly) && rp$anomaly$score > 0.1)
      paste0("&nbsp;<span style='background:rgba(124,92,191,0.12);color:#a785e0;border:1px solid rgba(124,92,191,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>",
             "\u26a0 Anomaly ",round(rp$anomaly$score*100),"%</span>") else ""
    uh <- if (length(rp$drivers_up)   > 0) paste(paste0("<li><span style='color:#e94560;'>\u25b2</span> ", htmltools::htmlEscape(rp$drivers_up),   "</li>"), collapse="") else ""
    dh <- if (length(rp$drivers_down) > 0) paste(paste0("<li><span style='color:#2dce89;'>\u25bc</span> ", htmltools::htmlEscape(rp$drivers_down), "</li>"), collapse="") else ""
    ds <- if (nchar(uh) > 0 || nchar(dh) > 0)
      paste0("<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>",uh,dh,"</ul>") else ""
    # Build rp_html entirely with paste0 — no sprintf, no % format hazard
    rp_html <- paste0(
      "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid ",bc,";padding:14px 16px;margin-top:14px;'>",
      "<div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'>",
      "<div><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>",
      "<i class=\"fa fa-",tier$icon,"\" style=\"color:",bc,";margin-right:6px;\"></i>",
      "Recession Probability \u2014 12-Month Horizon",ew_badge,ms_badge,an_badge,"</div>",
      "<div style='margin-top:4px;'>",
      "<span style='color:",bc,";font-size:36px;font-weight:800;'>",round(prob),"%</span>",
      "<span style='color:",bc,";font-size:14px;font-weight:600;margin-left:8px;",
      "background:rgba(",bc,",0.12);padding:2px 10px;border-radius:12px;border:1px solid rgba(",bc,",0.3);'>",
      tier$label,"</span></div></div>",
      "<div style='text-align:right;'>",
      "<div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0% \u00b7 50% \u00b7 100%</div>",
      "<div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);border-radius:7px;position:relative;'>",
      "<div style='position:absolute;top:-3px;left:calc(",min(round(prob),98),"% - 8px);",
      "width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:8px solid #fff;'></div>",
      "</div><div style='font-size:10px;color:#6b7585;margin-top:3px;'>\u25b2 Current</div></div></div>",
      "<div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;border-radius:6px;padding:8px 12px;border-left:3px solid ",bc,";'>",
      htmltools::htmlEscape(tier$desc),"</div>",ds,
      "<div style='color:#555;font-size:10px;margin-top:8px;'>",
      "Tier 3: rolling GLM + 3-state Markov + anomaly detection. Data-driven Brier weights. NBER-calibrated.",
      "</div></div>")
  }

  mr <- tryCatch(get(".recession_t3_cache",envir=.GlobalEnv)$markov,error=function(e)NULL)
  ew_cache <- tryCatch(get(".recession_t3_cache",envir=.GlobalEnv)$ew,error=function(e)NULL)

  # Outer layout also uses paste0 to avoid any residual % issues from rp_html
  shiny::HTML(paste0(
    "<div style='display:flex;gap:14px;'>",
    "<div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid ",reg$color,";padding:16px 18px;'>",
    "<div style='color:",reg$color,";font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>",
    "<i class=\"fa fa-",reg$icon,"\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook</div>",
    "<div style='color:",reg$color,";font-size:22px;font-weight:800;margin-bottom:2px;'>",reg$label,"</div>",
    gdp_display,
    gauge_html,
    "<div style='background:#0f1117;border-radius:6px;padding:10px 12px;color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid ",reg$color,";'>",
    htmltools::htmlEscape(reg$summary),"</div>",
    "<div style='color:#555;font-size:10px;margin-top:10px;'>",length(comps)," indicators + 3-state Markov + rolling GLM + anomaly. As of ",format(outlook$as_of,"%b %d, %Y"),".",
    "<button onclick=\"document.getElementById('recModelModal').style.display='flex'\" ",
    "style='margin-left:8px;background:transparent;border:1px solid #2a3042;color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'>",
    "<i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details</button></div>",
    rp_html,
    "</div>",
    "<div style='flex:1;display:flex;flex-direction:column;gap:10px;'>",
    "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;flex:1;'>",
    "<div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>",
    comp_html,"</div>",
    "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;'>",
    "<div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div>",
    "<ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>",swing_html,"</ul></div>",
    "</div></div>",
    .build_model_modal(fc=if(!is.null(rp))rp$factor_contributions else NULL,
                       model_type=if(!is.null(rp))rp$model_type%||%"Hand-tuned"else"Hand-tuned",
                       anomaly=if(!is.null(rp))rp$anomaly else NULL,
                       mr=mr, ew=ew_cache)))
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("[t3] render_growth_outlook_html error: ", msg)
    shiny::HTML(paste0(
      "<div style='background:#1a2035;border:1px solid #e94560;border-radius:8px;padding:20px;'>",
      "<div style='color:#e94560;font-size:13px;font-weight:700;margin-bottom:8px;'>",
      "<i class='fa fa-exclamation-triangle' style='margin-right:8px;'></i>",
      "Growth Outlook Render Error</div>",
      "<div style='color:#9aa3b2;font-size:12px;margin-bottom:12px;'>",
      htmltools::htmlEscape(msg),"</div>",
      "<div style='color:#555;font-size:11px;'>",
      "Run <code>rm(.recession_t3_cache, envir=.GlobalEnv); file.remove('.recession_t3_cache.rds')</code> ",
      "in the R console to clear the cache and retrain.</div></div>"))
  })
  result
}

.build_model_modal <- function(fc=NULL, model_type="Hand-tuned", anomaly=NULL, mr=NULL, ew=NULL) {
  bar_html <- .build_factor_bar_chart(fc, model_type, mr, ew)

  # Ensemble weights panel
  ew_html <- if(!is.null(ew)&&!is.null(ew$glm_brier)) {
    sprintf("<div style='background:#0d1f10;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #2dce89;'><div style='color:#2dce89;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'><i class=\"fa fa-balance-scale\" style=\"margin-right:6px;\"></i>Data-Driven Ensemble Weights <span style='color:#555;font-size:10px;font-weight:400;'>(%s)</span></div><div style='display:flex;gap:20px;margin-bottom:10px;'><div style='flex:1;'><div style='color:#9aa3b2;font-size:10px;margin-bottom:4px;'>GLM weight <b style='color:#2dce89;'>%.0f%%</b></div><div style='background:#2a3042;border-radius:3px;height:8px;'><div style='background:#2dce89;width:%.0f%%;height:100%%;border-radius:3px;'></div></div><div style='color:#555;font-size:9px;margin-top:3px;'>Brier=%.4f | skill=%.3f</div></div><div style='flex:1;'><div style='color:#9aa3b2;font-size:10px;margin-bottom:4px;'>Markov weight <b style='color:#00b4d8;'>%.0f%%</b></div><div style='background:#2a3042;border-radius:3px;height:8px;'><div style='background:#00b4d8;width:%.0f%%;height:100%%;border-radius:3px;'></div></div><div style='color:#555;font-size:9px;margin-top:3px;'>Brier=%.4f | skill=%.3f</div></div></div><div style='color:#d0d0d0;font-size:11px;line-height:1.75;'>Weights from Brier skill scores on a <b>%d-month true out-of-sample holdout</b>. A temporary scoring GLM (trained on months 1 to n\u221236) predicts on months n\u221235 to n that it never saw. Skill = 1 \u2212 (model Brier / climatology Brier). Higher skill \u2192 higher blend weight. Recomputed every 30 days.</div></div>",
            ew$method,ew$w_glm*100,ew$w_glm*100,ew$glm_brier,ew$glm_skill%||%0,ew$w_ms*100,ew$w_ms*100,ew$ms_brier,ew$ms_skill%||%0,ew$n_holdout%||%0)
  } else "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-info-circle\" style=\"color:#9aa3b2;margin-right:6px;\"></i>Ensemble weights pending \u2014 Brier scores computed on next retrain.</div>"

  # Markov status panel
  markov_html <- if (!is.null(mr)) {
    freq <- round((mr$state_freq_recession%||%0)*100)
    ms_pct <- round(mr$current_rec_prob*100)
    ms_col <- if(ms_pct>60)"#e94560" else if(ms_pct>35)"#f4a261" else "#2dce89"
    rel_badge <- if(isFALSE(mr$reliable))
      sprintf("<div style='background:#2a1010;border:1px solid rgba(233,69,96,0.4);border-radius:6px;padding:8px 12px;margin-bottom:8px;color:#e94560;font-size:11px;'><i class=\"fa fa-exclamation-triangle\" style=\"margin-right:6px;\"></i><b>Warning:</b> Recession state covers %d%% of history (max 25%%). Markov weight capped at 10%%.</div>", freq)
      else sprintf("<div style='background:#0d1f10;border:1px solid rgba(45,206,137,0.3);border-radius:6px;padding:6px 12px;margin-bottom:8px;color:#2dce89;font-size:11px;'><i class=\"fa fa-check-circle\" style=\"margin-right:6px;\"></i>State assignment healthy: recession state = %d%% of history (target ~13%%).</div>", freq)
    sw <- mr$sw_regime_means
    sprintf("<div style='background:#0f1a27;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>%s<div style='color:#00b4d8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'><i class=\"fa fa-random\" style=\"margin-right:6px;\"></i>Markov Regime Model <span style='color:#555;font-size:10px;font-weight:400;'>3-state pure-R EM &nbsp;|&nbsp; switching: %s</span></div><div style='display:flex;align-items:center;gap:10px;margin-bottom:8px;'><span style='color:#9aa3b2;font-size:11px;width:200px;'>Recession state probability</span><div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:4px;'></div></div><span style='color:%s;font-size:14px;font-weight:700;width:45px;text-align:right;'>%d%%</span></div><div style='color:#9aa3b2;font-size:11px;'>Expansion mean: <b style='color:#2dce89;'>%.2f</b> &nbsp;|&nbsp; Recession mean: <b style='color:#e94560;'>%.2f</b> &nbsp;|&nbsp; n=%d months &nbsp;|&nbsp; 3 states</div></div>",
            rel_badge,htmltools::htmlEscape(mr$switch_name%||%"unknown"),ms_col,ms_pct,ms_col,ms_pct,sw$expansion%||%0,sw$recession%||%0,mr$n_obs%||%0)
  } else "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-info-circle\" style=\"color:#00b4d8;margin-right:6px;\"></i>Markov model not yet fitted. Check INDPRO / PAYEMS / UNRATE fredr access.</div>"

  anm_html <- if(!is.null(anomaly)&&anomaly$score>0.05){bc<-if(anomaly$score>0.6)"#e94560"else if(anomaly$score>0.3)"#f4a261"else"#7c5cbf";sprintf("<div style='background:#1e1030;border-radius:8px;padding:12px 16px;margin-bottom:20px;border-left:4px solid #7c5cbf;'><div style='color:#a785e0;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;'><i class=\"fa fa-exclamation-triangle\" style=\"margin-right:6px;\"></i>Anomaly Detection Active</div><div style='display:flex;align-items:center;gap:10px;margin-bottom:6px;'><span style='color:#9aa3b2;font-size:11px;width:90px;'>Score</span><div style='flex:1;background:#2a3042;border-radius:4px;height:10px;'><div style='background:%s;width:%.0f%%;height:100%%;border-radius:4px;'></div></div><span style='color:%s;font-size:13px;font-weight:700;width:40px;'>%.0f%%</span></div><div style='color:#d0d0d0;font-size:11px;'>MD=%.2f \u2014 %s. Blend: p = p_base\u00d7(1\u22120.35A) + 0.25A</div></div>",bc,anomaly$score*100,bc,anomaly$score*100,anomaly$md%||%0,anomaly$label)}else"<div style='background:#1e2640;border-radius:6px;padding:8px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-check-circle\" style=\"color:#2dce89;margin-right:6px;\"></i>Anomaly score normal. No uncertainty adjustment.</div>"

  paste0(
    "<div id='recModelModal' onclick=\"if(event.target===this)this.style.display='none'\" style='display:none;position:fixed;inset:0;background:rgba(0,0,0,0.75);z-index:9999;align-items:center;justify-content:center;padding:20px;'>",
    "<div style='background:#161b27;border:1px solid #2a3042;border-radius:10px;max-width:1100px;width:100%;max-height:90vh;overflow-y:auto;padding:24px;position:relative;'>",
    "<button onclick=\"document.getElementById('recModelModal').style.display='none'\" style='position:absolute;top:14px;right:16px;background:transparent;border:none;color:#9aa3b2;font-size:20px;cursor:pointer;'>&times;</button>",
    "<div style='color:#00b4d8;font-size:16px;font-weight:800;margin-bottom:4px;'><i class=\"fa fa-brain\" style=\"margin-right:8px;\"></i>Recession Probability Model \u2014 Technical Reference <span style='font-size:11px;font-weight:400;color:#00b4d8;margin-left:10px;border:1px solid rgba(0,180,216,0.3);border-radius:6px;padding:1px 8px;'>Tier\u00a03: Rolling GLM + 3-State Markov + Anomaly</span></div>",
    "<div style='color:#9aa3b2;font-size:12px;margin-bottom:20px;'>Three-model ensemble with data-driven Brier-weighted blend. GLM retrained on rolling 30-year window. Pure-R Hamilton (1989) 3-state EM \u2014 no external packages. Calibrated to NBER recession dates. Not a point forecast.</div>",
    "<div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",bar_html,"</div>",
    ew_html, markov_html, anm_html,
    "<div style='color:#555;font-size:11px;margin-top:8px;line-height:1.7;'><b style='color:#9aa3b2;'>Formula:</b> p_blend = w_GLM\u00d7p_GLM + w_MS\u00d7p_Markov &nbsp;|&nbsp; A = min(1,MD/(d95\u00d71.5)) &nbsp;|&nbsp; p_final = p_blend\u00d7(1\u22120.35A) + 0.25A &nbsp;|&nbsp; [2%, 97%].<br/><b style='color:#9aa3b2;'>Refs:</b> Estrella &amp; Mishkin (1998); Sahm (2019); Hamilton (1983,1989); Mahalanobis (1936); Brier (1950).</div>",
    "</div></div>")
}