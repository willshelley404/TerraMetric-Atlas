# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch6.R
#
# SOURCE ORDER in global.R (add last, replaces all previous Markov patches):
#   source("R/synopsis_tier3_patch6.R")
#
# Then clear the cache once:
#   rm(.recession_t3_cache, envir = .GlobalEnv)
#
# WHAT THIS DOES:
#   Replaces the MSwM-based train_markov_model() with a self-contained
#   Hamilton (1989) EM implementation in base R.  No package dependencies.
#   No matrix inversions larger than k×k (k=3 states).  Cannot be singular.
#
# THE ALGORITHM (Hamilton 1989 / Kim 1994):
#   Each state j has Gaussian emissions: y_t | S_t=j ~ N(mu_j, sigma_j^2)
#   States evolve via a Markov chain with k×k transition matrix P.
#
#   EM loop:
#     E-step: Hamilton filter  → filtered probs  xi[t, j] = P(S_t=j | y_1..t)
#             Kim smoother     → smoothed probs  sp[t, j] = P(S_t=j | y_1..T)
#     M-step: update mu, sigma, P from weighted sufficient statistics
#   No matrix inversion — only element-wise operations and row-normalisations.
#
# ALSO FIXES:
#   The glm_result NULL issue was caused by build_glm_feature_matrix failing
#   when USREC download fails.  Patch5's USREC fallback handles that, but
#   we add one more guard: if the fallback Sahm-rule recession series has
#   zero recession months, we widen the threshold to 0.3pp so at least some
#   months are labeled, preventing a degenerate training set.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b


# ═══════════════════════════════════════════════════════════════════════════════
# Pure-R Hamilton EM for Gaussian Markov-switching model
# ═══════════════════════════════════════════════════════════════════════════════

#' Hamilton filter: compute filtered state probabilities.
#' @param y       numeric vector (the switching variable, no NAs)
#' @param mu      numeric[k] state means
#' @param sigma   numeric[k] state standard deviations
#' @param P       k×k transition matrix (rows sum to 1)
#' @param pi0     numeric[k] initial state distribution
#' @return        list(xi = T×k filtered probs, loglik = scalar)
.hamilton_filter <- function(y, mu, sigma, P, pi0) {
  T <- length(y); k <- length(mu)
  xi      <- matrix(0, T, k)    # filtered
  xi_pred <- matrix(0, T, k)    # one-step-ahead predicted
  loglik  <- 0

  xi_prev <- pi0
  for (t in seq_len(T)) {
    # Predicted probability P(S_t=j | y_{1..t-1})
    pred <- as.numeric(t(P) %*% xi_prev)   # k-vector
    # Emission density f(y_t | S_t=j)
    dens <- dnorm(y[t], mean=mu, sd=sigma)
    # Joint: P(S_t=j, y_t | y_{1..t-1})
    joint <- pred * dens
    sum_j <- sum(joint)
    if (sum_j < 1e-300) sum_j <- 1e-300   # numerical floor
    xi[t, ]      <- joint / sum_j
    xi_pred[t, ] <- pred
    loglik       <- loglik + log(sum_j)
    xi_prev      <- xi[t, ]
  }
  list(xi=xi, xi_pred=xi_pred, loglik=loglik)
}

#' Kim (1994) smoother: compute smoothed state probabilities.
#' @param xi      T×k filtered probs (from hamilton_filter)
#' @param P       k×k transition matrix
#' @return        T×k smoothed probs
.kim_smoother <- function(xi, P) {
  T <- nrow(xi); k <- ncol(xi)
  sp <- matrix(0, T, k)
  sp[T, ] <- xi[T, ]
  for (t in (T-1):1) {
    # P(S_t=i | y_{1..T}) = xi[t,i] * sum_j [ P[i,j] * sp[t+1,j] / pred[t+1,j] ]
    # where pred[t+1,j] = sum_i P[i,j] * xi[t,i]
    pred_next <- as.numeric(t(P) %*% xi[t, ])
    pred_next <- pmax(pred_next, 1e-300)
    ratio     <- sp[t+1, ] / pred_next          # k-vector
    sp[t, ]   <- xi[t, ] * as.numeric(P %*% ratio)
    sp[t, ]   <- pmax(sp[t, ], 0)
    s         <- sum(sp[t, ])
    if (s > 0) sp[t, ] <- sp[t, ] / s
  }
  sp
}

#' Fit a k-state Gaussian Markov-switching model via EM.
#' @param y         numeric vector (no NAs)
#' @param k         integer number of states
#' @param max_iter  integer EM iteration limit
#' @param tol       convergence tolerance on log-likelihood
#' @param seed      random seed for initialisation
#' @return list with smooth_probs, mu, sigma, P, loglik, converged
.fit_hamilton_em <- function(y, k=3L, max_iter=500L, tol=1e-6, seed=42L) {
  set.seed(seed)
  T <- length(y)

  # ── Initialise by splitting quantile range into k equal bands ────────────
  # This gives the EM a head start close to the true solution for economic data
  breaks  <- quantile(y, probs=seq(0, 1, length.out=k+1), na.rm=TRUE)
  mu      <- numeric(k)
  sigma   <- numeric(k)
  for (j in seq_len(k)) {
    band    <- y[y >= breaks[j] & y <= breaks[j+1]]
    if (length(band) < 2) band <- y   # fallback
    mu[j]    <- mean(band, na.rm=TRUE)
    sigma[j] <- sd(band, na.rm=TRUE)
    if (is.na(sigma[j]) || sigma[j] < 0.01) sigma[j] <- sd(y, na.rm=TRUE) / k
  }

  # Uniform transition matrix
  P   <- matrix(1/k, k, k)
  pi0 <- rep(1/k, k)

  prev_ll <- -Inf
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    # ── E-step ───────────────────────────────────────────────────────────────
    filt  <- .hamilton_filter(y, mu, sigma, P, pi0)
    xi    <- filt$xi
    ll    <- filt$loglik
    sp    <- .kim_smoother(xi, P)

    # Two-slice smoothed probs: xi2[t, i, j] = P(S_{t-1}=i, S_t=j | y_{1..T})
    # Computed on-the-fly below for M-step; store as k×k matrix of column sums

    # ── M-step ───────────────────────────────────────────────────────────────
    # Transition matrix: expected counts of i→j transitions
    P_new <- matrix(0, k, k)
    for (t in 2:T) {
      pred_t <- as.numeric(t(P) %*% xi[t-1, ])
      pred_t <- pmax(pred_t, 1e-300)
      for (i in seq_len(k)) {
        for (j in seq_len(k)) {
          # xi2[t, i, j] ≈ xi[t-1,i] * P[i,j] * dens[t,j] * sp[t,j] / pred_t[j]
          # Proportional update (avoids full 3-slice computation)
          P_new[i, j] <- P_new[i, j] + xi[t-1, i] * P[i, j] * sp[t, j] / pred_t[j]
        }
      }
    }
    # Row-normalise
    row_sums <- rowSums(P_new)
    row_sums[row_sums < 1e-300] <- 1e-300
    P <- P_new / row_sums

    # State means and standard deviations
    wt_sum <- colSums(sp)   # effective observations per state
    wt_sum[wt_sum < 1e-10] <- 1e-10
    mu    <- colSums(sp * y)      / wt_sum
    sigma <- sqrt(colSums(sp * outer(y, mu, function(a,b)(a-b)^2)) / wt_sum)
    sigma <- pmax(sigma, 0.01)    # numerical floor

    # Initial distribution
    pi0 <- sp[1, ]

    # ── Convergence ──────────────────────────────────────────────────────────
    if (abs(ll - prev_ll) < tol) { converged <- TRUE; break }
    prev_ll <- ll
  }

  list(smooth_probs=sp, mu=mu, sigma=sigma, P=P, loglik=ll,
       converged=converged, n_iter=iter)
}


# ═══════════════════════════════════════════════════════════════════════════════
# train_markov_model — uses pure-R EM, no MSwM
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
        dplyr::summarise(!!val_name := mean(value, na.rm=TRUE), .groups="drop") %>%
        dplyr::rename(date = month)
    }
    gm_fred <- function(sid, val_name, start=as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid, observation_start=start, frequency="m") %>%
          dplyr::select(date, !!val_name := value),
        error=function(e) { message(sprintf("[t3] fredr('%s') failed: %s", sid, e$message)); gm_local(sid, val_name) })
    }

    # ── Switching variable cascade ────────────────────────────────────────────
    switching_var <- NULL; switch_name <- NULL; higher_is_recession <- FALSE

    indpro_raw <- gm_fred("INDPRO","indpro")
    if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
      cand <- indpro_raw %>% dplyr::arrange(date) %>%
        dplyr::mutate(sw_var=(indpro/dplyr::lag(indpro,12)-1)*100) %>%
        dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
      if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"IP YoY %"; higher_is_recession<-FALSE }
    }
    if (is.null(switching_var)) {
      pay_raw <- gm_fred("PAYEMS","payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var=payems-dplyr::lag(payems,1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
        if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"Payroll MoM chg (K)"; higher_is_recession<-FALSE }
      }
    }
    if (is.null(switching_var)) {
      urate_raw <- gm_fred("UNRATE","unemp")
      if (!is.null(urate_raw) && nrow(urate_raw) >= 60) {
        switching_var <- urate_raw %>% dplyr::rename(sw_var=unemp); switch_name<-"Unemployment rate"; higher_is_recession<-TRUE
      }
    }
    if (is.null(switching_var)) { message("[t3] No switching variable."); return(NULL) }

    y <- switching_var$sw_var[!is.na(switching_var$sw_var)]
    dates_used <- switching_var$date[!is.na(switching_var$sw_var)]

    message(sprintf("[t3] Markov EM: '%s' (%d obs), %d states, %d restarts",
                    switch_name, length(y), n_states, n_restarts))

    # ── Multiple restarts — pick best by recession-state frequency ────────────
    candidates <- lapply(seq_len(n_restarts), function(r) {
      tryCatch({
        seed <- r * 37L
        # Perturb y slightly for restarts > 1
        y_fit <- if (r > 1) y + rnorm(length(y), 0, sd(y)*0.03*(r-1)) else y
        fit <- .fit_hamilton_em(y_fit, k=n_states, max_iter=500L, tol=1e-6, seed=seed)

        # Always evaluate on original y
        sp <- fit$smooth_probs
        sa <- apply(sp, 1, which.max)

        # Empirical means from original y
        emp_means <- sapply(seq_len(n_states), function(s) mean(y[sa==s], na.rm=TRUE))
        recession_state <- if (higher_is_recession) which.max(emp_means) else which.min(emp_means)
        freq_rec <- mean(sa == recession_state)

        message(sprintf("[t3]   restart %d: ll=%.1f conv=%s | state means=[%s] | rec_state=%d freq=%.0f%%",
                        r, fit$loglik, fit$converged,
                        paste(round(emp_means,1), collapse=","),
                        recession_state, freq_rec*100))

        list(fit=fit, smooth_probs=sp, recession_state=recession_state,
             emp_means=emp_means, freq_rec=freq_rec, state_assign=sa)
      }, error=function(e) {
        message(sprintf("[t3]   restart %d failed: %s", r, e$message)); NULL
      })
    })

    candidates <- Filter(Negate(is.null), candidates)
    if (length(candidates) == 0) { message("[t3] All EM restarts failed."); return(NULL) }

    # Best = recession-state frequency closest to NBER base rate (~13%)
    freq_errors <- sapply(candidates, function(c) abs(c$freq_rec - 0.13))
    best        <- candidates[[which.min(freq_errors)]]

    smooth_probs    <- best$smooth_probs
    recession_state <- best$recession_state
    emp_means       <- best$emp_means
    state_assign    <- best$state_assign
    freq_rec        <- best$freq_rec
    current_rec_prob <- smooth_probs[length(y), recession_state]
    reliable         <- freq_rec <= 0.25

    # State labels for 3-state model: order by mean growth
    state_order  <- order(emp_means, decreasing=!higher_is_recession)
    state_labels <- character(n_states)
    if (n_states == 3L) {
      state_labels[state_order[1]] <- "Expansion"
      state_labels[state_order[2]] <- "Stall"
      state_labels[state_order[3]] <- "Contraction"
    } else {
      state_labels[recession_state] <- "Contraction"
      state_labels[setdiff(seq_len(n_states), recession_state)[1]] <- "Expansion"
    }

    exp_state <- if (n_states==3L) state_order[1] else setdiff(seq_len(n_states),recession_state)[1]
    sw_regime_means <- list(
      variable  = switch_name,
      expansion = round(emp_means[exp_state], 2),
      recession = round(emp_means[recession_state], 2)
    )

    if (!reliable)
      message(sprintf("[t3] WARNING: recession state = %.0f%% of history (max 25%%). Weight will be capped.", freq_rec*100))

    message(sprintf("[t3] Markov final: states [%s] | rec='%s' mean=%.2f (%.0f%% of months) | p_rec_now=%.1f%% | reliable=%s",
                    paste(sprintf("%s=%.1f",state_labels,emp_means),collapse=" / "),
                    state_labels[recession_state], emp_means[recession_state],
                    freq_rec*100, current_rec_prob*100, reliable))

    list(model               = best$fit,     # the EM fit object (mu/sigma/P)
         smooth_probs        = smooth_probs,
         recession_state     = recession_state,
         current_rec_prob    = current_rec_prob,
         state_means_sw      = emp_means,
         state_labels        = state_labels,
         switch_name         = switch_name,
         higher_is_recession = higher_is_recession,
         regime_means        = list(),
         sw_regime_means     = sw_regime_means,
         state_freq_recession = freq_rec,
         reliable            = reliable,
         n_states            = n_states,
         n_obs               = length(y),
         trained_at          = Sys.time())
  }, error=function(e) { message("[t3] Markov model failed: ",e$message); NULL })
}


# ═══════════════════════════════════════════════════════════════════════════════
# USREC fallback: widen Sahm threshold if zero recession months
# ═══════════════════════════════════════════════════════════════════════════════

build_glm_feature_matrix <- function(fred_data) {
  tryCatch({
    # ── Step 1: NBER recession indicator ─────────────────────────────────────
    usrec <- tryCatch({
      r <- fredr::fredr("USREC", observation_start=as.Date("1959-01-01"), frequency="m")
      if (is.null(r) || nrow(r) < 120) stop("USREC too short")
      message(sprintf("[t3] USREC: %d months from fredr", nrow(r)))
      r %>% dplyr::select(date, recession=value) %>% dplyr::arrange(date)
    }, error=function(e) {
      message(sprintf("[t3] fredr('USREC') failed: %s — building Sahm proxy", e$message))
      ur_df <- fred_data$UNRATE
      if (is.null(ur_df) || nrow(ur_df) < 24) {
        message("[t3] UNRATE missing. Cannot build feature matrix."); return(NULL)
      }
      base <- ur_df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(u=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month) %>% dplyr::arrange(date) %>%
        dplyr::mutate(u_min12=zoo::rollapply(u,12,min,fill=NA,align="right"))

      # Try 0.5pp first, then widen to 0.3pp if zero recession months
      for (thresh in c(0.5, 0.3)) {
        cand <- base %>% dplyr::mutate(recession=as.numeric((u-u_min12)>=thresh)) %>%
          dplyr::filter(!is.na(recession)) %>% dplyr::select(date,recession)
        if (sum(cand$recession) >= 12) {
          message(sprintf("[t3] Sahm proxy (threshold=%.1fpp): %d recession months", thresh, sum(cand$recession)))
          return(cand)
        }
      }
      message("[t3] Sahm proxy produced zero recession months even at 0.3pp threshold.")
      NULL
    })

    if (is.null(usrec) || nrow(usrec) < 120) {
      message("[t3] Recession indicator unavailable or too short. Returning NULL."); return(NULL)
    }

    # ── Step 2: Feature series ────────────────────────────────────────────────
    gm <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month)
    }
    gf <- function(sid, val_name, start=as.Date("1959-01-01")) {
      tryCatch(
        fredr::fredr(sid,observation_start=start,frequency="m") %>% dplyr::select(date,!!val_name:=value),
        error=function(e) gm(sid,val_name))
    }

    t10y2y  <- gf("T10Y2Y","yield_spread")
    unrate  <- gm("UNRATE","u")
    fedfund <- gm("FEDFUNDS","ff")
    cpi_m   <- gm("CPIAUCSL","cpi")
    hy_m    <- gm("BAMLH0A0HYM2","hy_spread")
    oil_m   <- gm("DCOILWTICO","oil")
    lfpr_m  <- gm("LNS11300060","prime_lfpr")
    usd_m   <- gf("DTWEXBGS","usd")
    isr_m   <- gm("ISRATIO","inv_sales")
    cc_m    <- gm("DRCCLACBS","cc_del")
    pay_m   <- gm("PAYEMS","payems")
    sent_m  <- gm("UMCSENT","sentiment")

    if (is.null(unrate) || is.null(cpi_m) || is.null(fedfund)) {
      message("[t3] Missing UNRATE/CPIAUCSL/FEDFUNDS. Cannot build feature matrix."); return(NULL)
    }
    if (is.null(t10y2y)) {
      t10 <- gm("DGS10","t10"); t2 <- gm("DGS2","t2")
      if (!is.null(t10) && !is.null(t2))
        t10y2y <- dplyr::inner_join(t10,t2,by="date") %>%
          dplyr::mutate(yield_spread=t10-t2) %>% dplyr::select(date,yield_spread)
    }

    u_df   <- unrate %>% dplyr::rename(u=u) %>%
      dplyr::mutate(u_trough12=zoo::rollapply(u,12,min,fill=NA,align="right"), u_rise=u-u_trough12)
    pay_df <- if (!is.null(pay_m))
      pay_m %>% dplyr::arrange(date) %>%
        dplyr::mutate(pay_chg=payems-dplyr::lag(payems,1),
                      pay_3m=zoo::rollmean(pay_chg,3,fill=NA,align="right")) %>%
        dplyr::select(date,pay_3m) else NULL
    cpi_df <- cpi_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(cpi_yoy=(cpi/dplyr::lag(cpi,12)-1)*100) %>% dplyr::select(date,cpi_yoy)
    ff_df  <- fedfund %>% dplyr::rename(ff=ff)
    hy_df  <- if(!is.null(hy_m))  hy_m  else NULL
    oil_df <- if(!is.null(oil_m)) oil_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(oil_yoy=(oil/dplyr::lag(oil,12)-1)*100) %>% dplyr::select(date,oil_yoy) else NULL
    lfpr_df<- if(!is.null(lfpr_m))lfpr_m else NULL
    usd_df <- if(!is.null(usd_m)) usd_m %>% dplyr::arrange(date) %>%
      dplyr::mutate(usd_yoy=(usd/dplyr::lag(usd,12)-1)*100) %>% dplyr::select(date,usd_yoy) else NULL
    isr_df <- if(!is.null(isr_m)) isr_m else NULL
    cc_df  <- if(!is.null(cc_m))  cc_m  else NULL
    sent_df<- if(!is.null(sent_m))sent_m else NULL

    # Forward recession indicator
    n_r <- nrow(usrec)
    usrec$rec_next12 <- vapply(seq_len(n_r), function(i) {
      idx <- (i+1):min(i+12,n_r)
      if (length(idx) < 6) return(NA_real_)
      as.numeric(any(usrec$recession[idx]==1))
    }, numeric(1))

    panel <- usrec %>%
      dplyr::left_join(if(!is.null(t10y2y))t10y2y else data.frame(date=as.Date(character()),yield_spread=numeric()), by="date") %>%
      dplyr::left_join(u_df %>% dplyr::select(date,u,u_rise), by="date") %>%
      dplyr::left_join(cpi_df, by="date") %>%
      dplyr::left_join(ff_df, by="date") %>%
      dplyr::mutate(real_rate=ff-cpi_yoy)

    for (df in list(pay_df,hy_df,oil_df,lfpr_df,usd_df,isr_df,cc_df,sent_df))
      if (!is.null(df)) panel <- dplyr::left_join(panel,df,by="date")

    panel <- panel %>% dplyr::filter(!is.na(rec_next12), !is.na(u_rise))
    if (nrow(panel) < 120) {
      message(sprintf("[t3] Panel too small: %d rows.", nrow(panel))); return(NULL)
    }

    message(sprintf("[t3] GLM feature matrix: %d months, base_rate=%.1f%%",
                    nrow(panel), mean(panel$rec_next12,na.rm=TRUE)*100))
    panel
  }, error=function(e) { message("[t3] build_glm_feature_matrix failed: ",e$message); NULL })
}
