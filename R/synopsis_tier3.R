# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3.R  —  Tier 3: Growth outlook + Ridge recession GLM
#
# Source only these three lines in global.R:
#   source("R/synopsis_tier1.R")
#   source("R/synopsis_tier2.R")
#   source("R/synopsis_tier3.R")       ← this file
#
# The 12-month recession probability is a sign-constrained Ridge logistic
# regression on nine predictors, refit on a rolling 30-year window of
# resolved NBER labels. This file also holds the composite growth score, the
# score → GDP mapping, and the growth-outlook card.
#
# APP STRUCTURE:
#   project/
#     global.R
#     server.R  /  ui.R
#     R/
#       synopsis_tier1.R
#       synopsis_tier2.R
#       synopsis_tier3.R   ← this file
#       data_fred.R  /  data_census.R  /  llm_insights.R
# ─────────────────────────────────────────────────────────────────────────────
library(glmnet)

# ── Source tier 2 (which sources tier 1) ─────────────────────────────────────
local({
  candidates <- c(
    tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL),
    "R/synopsis_tier2.R"
  )
  for (p in candidates) {
    tier2 <- sub("tier3", "tier2", p)
    if (file.exists(tier2)) {
      source(tier2, local = FALSE)
      return(invisible())
    }
  }
  stop(
    "[synopsis_tier3] Cannot locate synopsis_tier2.R. Set working directory to project root."
  )
})


# ── Re-define %||% to handle list inputs (tier1's version calls is.na(list) ──────
# Tier1 defines: if(!is.null(a) && !is.na(a))
# is.na() on a list returns a vector → if(vector) → 'length=N' error
# This version uses tryCatch to safely handle any type of a.
`%||%` <- function(a, b) {
  if (is.null(a)) {
    return(b)
  }
  ok <- tryCatch(!is.na(a[[1]]), error = function(e) TRUE)
  if (isTRUE(ok) && length(a) > 0) a else b
}



# ═══════════════════════════════════════════════════════════════════════════════
# RECESSION MODEL — sign-constrained Ridge GLM
# p(NBER recession in any of the next 12 months) from nine predictors. A
# month is scored by a glmnet Ridge logit fit on the latest 360 months whose
# 12-month outcome had resolved by then (12-month embargo), each β pinned to
# its theory sign. Same method as the recession tracker; keep the two in step.
# ═══════════════════════════════════════════════════════════════════════════════

.GLM_PREDICTORS <- c(
  "yield_spread", "u_rise", "gap_rise", "pay_3m", "real_rate", "prime_lfpr", "sent_chg12",
  "baa_spread", "claims_rise"
)

# U6RATE starts in 1994. Before its first value gap_rise is NA and scores as
# the training median (z = 0); after it, an NA blocks the month.
.GLM_LATE_START <- "gap_rise"

# Sign of β (effect of a higher value on recession risk) that macro theory
# expects; each fitted β is constrained to it (0 allowed). Backtested: beat
# the unconstrained fit and stops late-cycle artefacts like "tighter policy →
# less risk".
.GLM_EXPECTED_SIGN <- c(
  yield_spread = -1, # steeper curve → less risk
  u_rise       = +1,
  gap_rise     = +1, # more involuntary part-time / marginally attached → more risk
  pay_3m       = -1,
  real_rate    = +1, # tighter policy → more risk
  prime_lfpr   = -1,
  sent_chg12   = -1, # falling sentiment → more risk
  baa_spread   = +1, # wider corporate credit spread → more risk
  claims_rise  = +1  # layoffs picking up → more risk
)

.GLM_LABELS <- c(
  yield_spread = "Yield Curve (10Y-2Y spread)",
  u_rise       = "Sahm Rule (unemployment rise)",
  gap_rise     = "Underemployment Gap Rise (U-6 − U-3)",
  pay_3m       = "Payroll Momentum (3M avg)",
  real_rate    = "Real Fed Funds Rate",
  prime_lfpr   = "Prime-Age LFPR (25–54)",
  sent_chg12   = "Consumer Sentiment (12m change)",
  baa_spread   = "Baa Credit Spread (vs 10Y Treasury)",
  claims_rise  = "Jobless Claims Rise (vs 12m low)"
)

.GLM_WINDOW <- 360L # rolling window of labelled months
.GLM_EMBARGO <- 12L # rec_next12 at month t resolves at t + 12
.GLM_FIRST_FORECAST <- as.Date("1985-01-01")

.GLM_SERIES <- c(
  "USREC", "T10Y2Y", "DGS10", "DGS2", "UNRATE", "U6RATE", "PAYEMS", "FEDFUNDS",
  "CPIAUCSL", "LNS11300060", "UMCSENT", "BAA10YM", "ICSA"
)
# Indicators and rate series aren't gap-filled; levels with a single missing
# month (e.g. the Oct-2025 shutdown) are
.GLM_NO_INTERPOLATE <- c("USREC", "T10Y2Y", "DGS10", "DGS2", "BAA10YM")

# Fixed, class-stratified CV folds for cv.glmnet. Its default random folds
# moved lambda.1se enough to swing the probability between restarts on
# identical data. Contiguous time blocks were tried and fail: with rare
# recessions a block can leave a training split with no positives.
# Restores the caller's RNG state.
.cv_folds <- function(y, k = 5L) {
  had_seed <- exists(".Random.seed", envir = globalenv())
  if (had_seed) old <- get(".Random.seed", envir = globalenv())
  on.exit(
    if (had_seed) assign(".Random.seed", old, envir = globalenv())
    else rm(".Random.seed", envir = globalenv())
  )
  set.seed(20260101L)
  f <- integer(length(y))
  for (cl in unique(y)) {
    idx <- which(y == cl)
    f[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  f
}

.glm_sign_limits <- function(preds) {
  s <- .GLM_EXPECTED_SIGN[preds]
  s[is.na(s)] <- 0
  list(
    lower = unname(ifelse(s > 0, 0, -Inf)),
    upper = unname(ifelse(s < 0, 0, Inf))
  )
}

#' Robust scaler from the training rows only: median and MAD (floored, so a
#' constant series can't divide by zero).
build_robust_scaler <- function(df, cols, mad_min = 1e-6) {
  center <- vapply(cols, function(v) median(df[[v]], na.rm = TRUE), numeric(1))
  scale <- vapply(cols, function(v) max(mad(df[[v]], na.rm = TRUE), mad_min), numeric(1))
  list(center = center, scale = scale, cols = cols)
}

#' z = clamp((x - median) / MAD, -3, 3). NA → 0, i.e. the training median.
apply_robust_scaler <- function(df, scaler) {
  for (v in scaler$cols) {
    z <- (df[[v]] - scaler$center[[v]]) / scaler$scale[[v]]
    z[is.na(z)] <- 0
    df[[v]] <- pmax(-3, pmin(3, z))
  }
  df
}

#' Fill an NA only when both neighbouring months are present: log-linear for a
#' strictly positive series, linear otherwise. Reindexed onto the monthly grid
#' first, so an absent row counts as a missing month.
.fill_single_gaps <- function(df) {
  df <- df[order(df$date), , drop = FALSE]
  grid <- seq(min(df$date), max(df$date), by = "1 month")
  v <- df$value[match(grid, df$date)]
  n <- length(v)
  if (n >= 3) {
    positive <- all(v > 0, na.rm = TRUE)
    for (i in 2:(n - 1)) {
      if (is.na(v[i]) && !is.na(v[i - 1]) && !is.na(v[i + 1])) {
        v[i] <- if (positive) sqrt(v[i - 1] * v[i + 1]) else (v[i - 1] + v[i + 1]) / 2
      }
    }
  }
  data.frame(date = grid, value = v)
}

#' Full monthly history (1959–) of every model input. fred_data only holds
#' the dashboard's lookback, too short for a 30-year window. Errors propagate.
.fetch_glm_series <- function(start = as.Date("1959-01-01")) {
  out <- lapply(.GLM_SERIES, function(sid) {
    raw <- fredr::fredr(sid, observation_start = start, frequency = "m")
    df <- data.frame(date = as.Date(raw$date), value = as.numeric(raw$value))
    if (sid %in% .GLM_NO_INTERPOLATE) df else .fill_single_gaps(df)
  })
  stats::setNames(out, .GLM_SERIES)
}

#' One row per month: NBER label + the nine predictors. Every series is
#' matched onto the contiguous USREC month grid, so every lag is by calendar
#' month, never by row offset.
build_glm_panel <- function(series) {
  usrec <- series$USREC[order(series$USREC$date), ]
  grid <- seq(min(usrec$date), max(usrec$date), by = "1 month")
  n <- length(grid)
  at <- function(sid) series[[sid]]$value[match(grid, series[[sid]]$date)]
  lag_m <- function(x, k) x[match(grid %m-% months(k), grid)]
  # Rise above the 12-month low
  rise12 <- function(x) {
    x - zoo::rollapply(
      x, 12, function(w) if (all(is.na(w))) NA_real_ else min(w, na.rm = TRUE),
      fill = NA, align = "right"
    )
  }

  rec <- usrec$value[match(grid, usrec$date)]
  # Recession in any of t+1..t+12; NA with fewer than 6 future months
  rec_next12 <- vapply(seq_len(n), function(i) {
    if (n - i < 6) return(NA_real_)
    as.numeric(any(rec[(i + 1):min(i + 12, n)] == 1, na.rm = TRUE))
  }, numeric(1))

  t10y2y <- at("T10Y2Y")
  u <- at("UNRATE")
  pay <- at("PAYEMS")
  cpi <- at("CPIAUCSL")
  sent <- at("UMCSENT")
  # Jobless claims: % above their 12-month low (monthly average of weekly claims)
  claims <- at("ICSA")
  claims_low <- claims - rise12(claims)
  panel <- data.frame(
    date = grid,
    recession = rec,
    rec_next12 = rec_next12,
    yield_spread = ifelse(is.na(t10y2y), at("DGS10") - at("DGS2"), t10y2y),
    u_rise = rise12(u),
    gap_rise = rise12(at("U6RATE") - u),
    pay_3m = zoo::rollmean(pay - lag_m(pay, 1), 3, fill = NA, align = "right"),
    real_rate = at("FEDFUNDS") - (cpi / lag_m(cpi, 12) - 1) * 100,
    prime_lfpr = at("LNS11300060"),
    sent_chg12 = sent - lag_m(sent, 12),
    baa_spread = at("BAA10YM"),
    claims_rise = (claims / claims_low - 1) * 100
  )

  panel$complete <- stats::complete.cases(panel[, setdiff(.GLM_PREDICTORS, .GLM_LATE_START)])
  for (v in .GLM_LATE_START) {
    # A series that never arrives blocks every month rather than going neutral
    has <- !is.na(panel[[v]])
    first <- if (any(has)) min(grid[has]) else min(grid)
    panel$complete <- panel$complete & (has | grid < first)
  }
  panel
}

# Labelled months whose outcome had resolved by `as_of`, latest window only
.glm_training_rows <- function(panel, as_of) {
  ok <- panel$complete & !is.na(panel$rec_next12) &
    panel$date <= as_of %m-% months(.GLM_EMBARGO)
  tail(panel[ok, , drop = FALSE], .GLM_WINDOW)
}

#' Ridge logit (lambda.1se, fixed folds) with sign-constrained βs. NULL when
#' the window has too few rows or recessions to fit.
fit_recession_glm <- function(train) {
  if (nrow(train) < 60 || sum(train$rec_next12) < 6) {
    return(NULL)
  }
  scaler <- build_robust_scaler(train, .GLM_PREDICTORS)
  X <- as.matrix(apply_robust_scaler(train, scaler)[, .GLM_PREDICTORS, drop = FALSE])
  y <- train$rec_next12
  lims <- .glm_sign_limits(.GLM_PREDICTORS)
  fit <- glmnet::cv.glmnet(
    X, y,
    family = "binomial", alpha = 0, type.measure = "deviance",
    foldid = .cv_folds(y), lower.limits = lims$lower, upper.limits = lims$upper
  )
  b <- as.numeric(coef(fit, s = "lambda.1se"))
  list(
    fit = fit,
    scaler = scaler,
    intercept = b[1],
    beta = stats::setNames(b[-1], .GLM_PREDICTORS),
    base_rate = mean(y),
    n_train = nrow(train),
    train_end = max(train$date)
  )
}

predict_recession_glm <- function(model, rows) {
  X <- as.matrix(apply_robust_scaler(rows, model$scaler)[, .GLM_PREDICTORS, drop = FALSE])
  as.numeric(predict(model$fit, newx = X, s = "lambda.1se", type = "response"))
}

#' Walk-forward backtest: refit each January on labels known then, score
#' every complete month of that year. Scores use months whose outcome is
#' known; climatology = each month's training base rate.
backtest_recession_glm <- function(panel, first = .GLM_FIRST_FORECAST) {
  rows <- panel[panel$complete & panel$date >= first, , drop = FALSE]
  yr <- as.integer(format(rows$date, "%Y"))
  series <- do.call(rbind, lapply(sort(unique(yr)), function(y) {
    model <- fit_recession_glm(.glm_training_rows(panel, as.Date(sprintf("%d-01-01", y))))
    if (is.null(model)) return(NULL)
    r <- rows[yr == y, , drop = FALSE]
    data.frame(
      date = r$date, prob = predict_recession_glm(model, r),
      rec_next12 = r$rec_next12, base_rate = model$base_rate
    )
  }))
  if (is.null(series)) {
    return(NULL)
  }

  d <- series[!is.na(series$rec_next12), ]
  y <- d$rec_next12
  p <- d$prob
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  brier <- mean((p - y)^2)
  brier_clim <- mean((d$base_rate - y)^2)

  # Per recession since `first`: peak probability over the 12 months before
  # it starts, and over the whole rec_next12 = 1 spell (through the month
  # before it ends)
  r <- rle(panel$recession == 1)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  k <- which(r$values)
  sp <- data.frame(start = panel$date[starts[k]], end = panel$date[ends[k]])
  sp <- sp[sp$start >= first, , drop = FALSE]
  peak <- function(from, to) {
    v <- series$prob[series$date >= from & series$date <= to]
    if (length(v)) max(v) else NA_real_
  }
  episodes <- data.frame(
    recession = format(sp$start, "%Y"),
    runup_peak = vapply(seq_len(nrow(sp)), function(i) {
      peak(sp$start[i] %m-% months(12), sp$start[i] %m-% months(1))
    }, numeric(1)),
    spell_peak = vapply(seq_len(nrow(sp)), function(i) {
      peak(sp$start[i] %m-% months(12), sp$end[i] %m-% months(1))
    }, numeric(1)),
    stringsAsFactors = FALSE
  )

  list(
    from = min(series$date),
    to = max(series$date),
    n = nrow(d),
    brier = brier,
    brier_clim = brier_clim,
    skill = 1 - brier / brier_clim,
    auc = if (n1 > 0 && n0 > 0) (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_,
    hit_rate = if (n1 > 0) mean(p[y == 1] > 0.5) else NA_real_,
    false_alarm_rate = if (n0 > 0) mean(p[y == 0] > 0.5) else NA_real_,
    episodes = episodes,
    series = series[, c("date", "prob", "rec_next12")]
  )
}

#' Fetch, fit on the latest window, score the latest complete month and run
#' the backtest. NULL if the fetch or fit fails.
train_recession_glm <- function() {
  tryCatch(
    {
      panel <- build_glm_panel(.fetch_glm_series())
      latest <- max(panel$date[panel$complete])
      # Refit each January, as the backtest does, so the latest reading is the
      # last point of the history it's charted against
      model <- fit_recession_glm(.glm_training_rows(panel, lubridate::floor_date(latest, "year")))
      if (is.null(model)) stop("too few labelled months to fit")
      now <- panel[panel$date == latest, , drop = FALSE]
      prob <- predict_recession_glm(model, now)
      message(sprintf(
        "[t3] Recession GLM: %s = %.1f%% (trained on %d months through %s)",
        format(latest, "%b %Y"), prob * 100, model$n_train, format(model$train_end, "%b %Y")
      ))
      list(
        model = model,
        now = now,
        prob = prob,
        backtest = backtest_recession_glm(panel),
        trained_at = Sys.time()
      )
    },
    error = function(e) {
      message("[t3] Recession GLM training failed: ", conditionMessage(e))
      NULL
    }
  )
}

#' The latest reading in the shape the card, the GDP forecast and the LLM
#' context read: prob in %, tier, and β·z per predictor (log-odds).
.compute_recession_prob <- function(glm) {
  if (is.null(glm)) {
    return(NULL)
  }
  m <- glm$model
  z <- apply_robust_scaler(glm$now, m$scaler)
  cdf <- do.call(rbind, lapply(.GLM_PREDICTORS, function(p) {
    x <- glm$now[[p]]
    data.frame(
      name = .GLM_LABELS[[p]],
      contribution = m$beta[[p]] * z[[p]],
      z_stat = NA_real_,
      data_missing = is.na(x),
      beta = m$beta[[p]],
      z_score = z[[p]],
      raw_value = x,
      sign_flag = FALSE,
      center = m$scaler$center[[p]],
      scale = m$scaler$scale[[p]],
      stringsAsFactors = FALSE
    )
  }))
  cdf <- cdf[order(abs(cdf$contribution), decreasing = TRUE), ]
  live <- !cdf$data_missing
  prob <- round(glm$prob * 100, 1)

  list(
    prob = prob,
    tier = .classify_recession_tier(prob),
    drivers_up = head(cdf$name[live & cdf$contribution > 0.1], 4),
    drivers_down = head(cdf$name[live & cdf$contribution < -0.1], 3),
    factor_contributions = cdf,
    model_type = "Ridge GLM (9 predictors, sign-constrained)",
    intercept = m$intercept,
    month = glm$now$date,
    train_end = m$train_end,
    n_train = m$n_train,
    base_rate = m$base_rate,
    trained_at = glm$trained_at
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# .classify_recession_tier — 5-tier system
# ═══════════════════════════════════════════════════════════════════════════════

.classify_recession_tier <- function(prob) {
  if (prob >= 65) {
    list(
      label = "Elevated",
      color = "#e94560",
      icon = "exclamation-triangle",
      desc = "Multiple leading indicators aligned recessionary. Historical hit rate ~65%. Defensive positioning warranted."
    )
  } else if (prob >= 50) {
    list(
      label = "High",
      color = "#e07040",
      icon = "exclamation-circle",
      desc = "More than half the probability weight sits on the recessionary side. Multiple genuine warning signs are active. Historically precedes recession in roughly half of cases."
    )
  } else if (prob >= 30) {
    list(
      label = "Moderate",
      color = "#f4a261",
      icon = "minus-circle",
      desc = "Mixed signals. Some meaningful risk factors are elevated but no acute stress. Late-cycle slowdown most likely; recession requires additional deterioration."
    )
  } else if (prob >= 15) {
    list(
      label = "Low\u2013Moderate",
      color = "#f4a261",
      icon = "check-circle",
      desc = "A handful of headwinds present but overall macro picture is cautiously positive. Monitor labor momentum and credit spreads."
    )
  } else {
    list(
      label = "Low",
      color = "#2dce89",
      icon = "check-circle",
      desc = "Most leading indicators benign. At or below historical 12-month recession base rate (~13%). No acute stress signals."
    )
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# .regime_from_score — replaces broken dplyr::case_when + list()
# ═══════════════════════════════════════════════════════════════════════════════

.regime_from_score <- function(s) {
  if (s >= 7.5) {
    list(
      label = "Expansion",
      color = "#2dce89",
      icon = "arrow-up",
      gdp_est = "+2.5% to +3.5%",
      summary = "Broad-based growth signals. Labor solid, consumer resilient, financial conditions supportive."
    )
  } else if (s >= 5.5) {
    list(
      label = "Moderate Growth",
      color = "#00b4d8",
      icon = "minus",
      gdp_est = "+1.0% to +2.5%",
      summary = "Below-trend but positive growth likely. Mixed signals — watch rate transmission and consumer confidence."
    )
  } else if (s >= 3.5) {
    list(
      label = "Stall / Slowdown",
      color = "#f4a261",
      icon = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk. Restrictive monetary conditions or financial stress beginning to bite."
    )
  } else {
    list(
      label = "Contraction Risk",
      color = "#e94560",
      icon = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals. Recession probability elevated."
    )
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# .regime_from_gdp_point — headline regime label, keyed off the FINAL GDP point
# estimate (post recession-probability drag) rather than the raw composite
# score. The composite score alone can say "Moderate Growth" while the point
# forecast — after the recession-probability discount — has already fallen
# into "Stall / Slowdown" territory; deriving the label from the same number
# shown on the card guarantees the two can never contradict each other.
# Bands match the gdp_est ranges quoted by .regime_from_score() above.
# ═══════════════════════════════════════════════════════════════════════════════

.regime_from_gdp_point <- function(pt) {
  if (is.null(pt) || is.na(pt)) {
    return(.regime_from_score(5)) # neutral fallback if forecast unavailable
  }
  if (pt >= 2.5) {
    list(
      label = "Expansion",
      color = "#2dce89",
      icon = "arrow-up",
      gdp_est = "+2.5% to +3.5%",
      summary = "Broad-based growth signals. Labor solid, consumer resilient, financial conditions supportive."
    )
  } else if (pt >= 1.0) {
    list(
      label = "Moderate Growth",
      color = "#00b4d8",
      icon = "minus",
      gdp_est = "+1.0% to +2.5%",
      summary = "Below-trend but positive growth likely. Mixed signals — watch rate transmission and consumer confidence."
    )
  } else if (pt >= -0.5) {
    list(
      label = "Stall / Slowdown",
      color = "#f4a261",
      icon = "arrow-down",
      gdp_est = "-0.5% to +1.0%",
      summary = "Growth at risk. Restrictive monetary conditions or financial stress beginning to bite."
    )
  } else {
    list(
      label = "Contraction Risk",
      color = "#e94560",
      icon = "exclamation-triangle",
      gdp_est = "Below -0.5%",
      summary = "Multiple negative signals. Recession probability elevated."
    )
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# .growth_outlook_loading_html — animated screen while models train
# ═══════════════════════════════════════════════════════════════════════════════

.growth_outlook_loading_html <- function(step = NULL) {
  # Pure paste0 — no sprintf, no % format hazards anywhere
  step_label <- if (!is.null(step)) {
    htmltools::htmlEscape(step)
  } else {
    "Fitting models…"
  }
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
    step_label,
    "</div>",
    "<div style='margin-top:14px;background:#0f1117;border-radius:6px;",
    "padding:12px 20px;max-width:480px;text-align:left;'>",
    "<div style='color:#555;font-size:10px;font-weight:700;text-transform:uppercase;",
    "letter-spacing:1px;margin-bottom:8px;'>What's running</div>",
    "<div style='color:#6b7585;font-size:11px;line-height:2;'>",
    "<span style='color:#2dce89;'>&#x2713;</span>",
    " Fetch FRED macro history (1959–present)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Fit sign-constrained Ridge GLM on NBER recession dates (30yr window)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Walk-forward backtest (1985–present)<br/>",
    "<span style='color:#f4a261;'>&#x25cb;</span>",
    " Calibrate score → GDP mapping<br/>",
    "<span style='color:#9aa3b2;'>&#x25cb;</span> Render growth outlook",
    "</div></div>",
    "<div style='color:#555;font-size:10px;margin-top:6px;'>",
    "First run: 10–60 seconds. Data refreshed daily.</div>",
    "</div>"
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# COMPOSITE SCORE — shared by the live card and the historical rebuild used to
# calibrate the GDP mapping, so both are scored with identical rules.
# `x` is a named list of scalar inputs (NA allowed → .score() treats as neutral).
# ═══════════════════════════════════════════════════════════════════════════════

.COMPOSITE_INPUTS <- c(
  "u", "pay_3m", "ret_real", "cs", "real_rate", "yield_spread", "inv_months",
  "hs", "mort", "vix", "hy", "cpi_y", "pce_y", "ip_y"
)

.composite_components <- function(x) {
  g <- function(k) {
    v <- x[[k]]
    if (is.null(v) || length(v) == 0) NA_real_ else v
  }
  u <- g("u"); pay_3m <- g("pay_3m"); ret_real <- g("ret_real"); cs <- g("cs")
  real_rate <- g("real_rate"); yield_spread <- g("yield_spread")
  inv_months <- g("inv_months"); hs <- g("hs"); mort_v <- g("mort")
  vix_v <- g("vix"); hy_v <- g("hy"); cpi_y <- g("cpi_y"); pce_y <- g("pce_y")
  ip_y <- g("ip_y")

  list(
    labor = list(
      weight = 2.5,
      label = "Labor Market",
      score = mean(c(
        .score(u, 3.5, 5.0, FALSE),
        .score(pay_3m, 150, 50, TRUE),
        .score(u, 4.5, 5.5, FALSE)
      )),
      detail = if (!is.na(u) && !is.na(pay_3m)) {
        sprintf("%.1f%% unemployment; %+.0fK/mo payrolls", u, pay_3m)
      } else {
        "N/A"
      }
    ),
    consumer = list(
      weight = 2.0,
      label = "Consumer Demand",
      score = mean(c(.score(ret_real, 1, -1, TRUE), .score(cs, 85, 65, TRUE))),
      detail = if (!is.na(ret_real) && !is.na(cs)) {
        sprintf("Real retail %.1f%%; sentiment %.0f", ret_real, cs)
      } else {
        "N/A"
      }
    ),
    monetary = list(
      weight = 2.0,
      label = "Monetary Conditions",
      score = mean(c(
        .score(real_rate, 0.5, 2.5, FALSE),
        .score(yield_spread, 0.5, -0.25, TRUE),
        .score(inv_months, 3, 9, FALSE)
      )),
      detail = if (!is.na(real_rate) && !is.na(yield_spread)) {
        sprintf(
          "Real rate %.1f%%; 10Y-2Y %.2f pp; %d mo inv.",
          real_rate, yield_spread, as.integer(inv_months %||% 0)
        )
      } else {
        "N/A"
      }
    ),
    housing = list(
      weight = 1.5,
      label = "Housing",
      score = mean(c(.score(hs, 1400, 1000, TRUE), .score(mort_v, 6, 7.5, FALSE))),
      detail = if (!is.na(hs) && !is.na(mort_v)) {
        sprintf("%.0fK starts; %.2f%% mortgage", hs, mort_v)
      } else {
        "N/A"
      }
    ),
    financial = list(
      weight = 1.5,
      label = "Financial Conditions",
      score = mean(c(.score(vix_v, 18, 28, FALSE), .score(hy_v, 4.5, 6, FALSE))),
      detail = if (!is.na(vix_v) && !is.na(hy_v)) {
        sprintf("VIX %.1f; HY %.2f%%", vix_v, hy_v)
      } else {
        "N/A"
      }
    ),
    inflation = list(
      weight = 1.5,
      label = "Inflation",
      score = mean(c(.score(cpi_y, 2.5, 4.5, FALSE), .score(pce_y, 2.3, 3.5, FALSE))),
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
}

.composite_score <- function(components) {
  tw <- sum(sapply(components, `[[`, "weight"))
  raw <- sum(sapply(components, function(c) c$score * c$weight)) / tw
  list(raw = raw, score_0_10 = max(0, min(10, round((raw + 1) / 2 * 10, 1))))
}

# Months (of the last n) with a negative monthly-average 10Y–2Y spread.
# DGS10/DGS2 are daily, so counting raw observations would count trading days.
.inv_months_monthly <- function(fred_data, n = 18L) {
  m <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 2) return(NULL)
    df %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(v = mean(value), .groups = "drop")
  }
  a <- m("DGS10")
  b <- m("DGS2")
  if (is.null(a) || is.null(b)) return(0L)
  s <- dplyr::inner_join(a, b, by = "date") %>% dplyr::arrange(date)
  as.integer(sum(tail(s$v.x - s$v.y, n) < 0, na.rm = TRUE))
}

# Atlanta Fed GDPNow — latest nowcast (SAAR %) for the most recent quarter.
# Fetched live rather than cached with the models: it updates several times a week.
.fetch_gdpnow <- function(fred_data = NULL) {
  tryCatch(
    {
      df <- tryCatch(
        fredr::fredr("GDPNOW", observation_start = Sys.Date() - 730),
        error = function(e) fred_data$GDPNOW
      )
      if (is.null(df)) return(NULL)
      df <- df[!is.na(df$value), ]
      if (nrow(df) == 0) return(NULL)
      last <- df[which.max(df$date), ]
      updated <- tryCatch(
        as.Date(substr(fredr::fredr_series("GDPNOW")$last_updated, 1, 10)),
        error = function(e) last$realtime_start %||% NA
      )
      list(
        value = round(last$value, 1),
        quarter = paste0(lubridate::year(last$date), "Q", lubridate::quarter(last$date)),
        updated = updated
      )
    },
    error = function(e) {
      message("[t3] GDPNow fetch failed: ", e$message)
      NULL
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# GDP MAPPING CALIBRATION
# Rebuilds the composite score monthly from FRED history, then regresses
# realised next-quarter real GDP growth (SAAR, A191RL1Q225SBEA) on the
# end-of-quarter score. The band comes from the regression residuals, and an
# expanding-window pseudo-out-of-sample RMSE is reported against a naive
# historical-mean forecast. Recession probability enters only if it carries
# significant information beyond the score (the score already contains the
# yield curve, labour and credit signals the recession model uses).
# Caveat: uses final-vintage data and ignores publication lags.
# ═══════════════════════════════════════════════════════════════════════════════

.fred_hist_monthly <- function(fred_data, sid, start = as.Date("1959-01-01")) {
  df <- tryCatch(
    fredr::fredr(sid, observation_start = start, frequency = "m"),
    error = function(e) {
      message(sprintf("[t3] score history: fredr('%s') failed (%s) — using fred_data", sid, e$message))
      fred_data[[sid]]
    }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    dplyr::arrange(date)
}

build_score_history <- function(fred_data) {
  ids <- c(
    "UNRATE", "PAYEMS", "RSAFS", "UMCSENT", "FEDFUNDS", "CPIAUCNS", "DGS10",
    "DGS2", "HOUST", "MORTGAGE30US", "VIXCLS", "BAMLH0A0HYM2", "PCEPILFE", "INDPRO"
  )
  raw <- lapply(setNames(ids, ids), function(s) .fred_hist_monthly(fred_data, s))
  if (is.null(raw$UNRATE) || is.null(raw$CPIAUCNS)) {
    message("[t3] score history: core series missing.")
    return(NULL)
  }

  months <- seq(min(raw$UNRATE$date), max(raw$UNRATE$date), by = "month")
  col <- function(s) {
    df <- raw[[s]]
    if (is.null(df)) return(rep(NA_real_, length(months)))
    df$value[match(months, df$date)]
  }
  yoy <- function(v) (v / dplyr::lag(v, 12) - 1) * 100

  p <- data.frame(date = months)
  p$u <- col("UNRATE")
  pay <- col("PAYEMS")
  p$pay_3m <- (pay - dplyr::lag(pay, 3)) / 3
  p$cpi_y <- yoy(col("CPIAUCNS"))
  p$pce_y <- yoy(col("PCEPILFE"))
  p$real_rate <- col("FEDFUNDS") - p$cpi_y
  p$yield_spread <- col("DGS10") - col("DGS2")
  p$inv_months <- zoo::rollapply(
    p$yield_spread < 0, 18, function(z) sum(z, na.rm = TRUE),
    fill = NA, align = "right", partial = TRUE
  )
  p$hs <- col("HOUST")
  p$mort <- col("MORTGAGE30US")
  p$ret_real <- yoy(col("RSAFS")) - p$cpi_y
  p$cs <- col("UMCSENT")
  p$vix <- col("VIXCLS")
  p$hy <- col("BAMLH0A0HYM2")
  p$ip_y <- yoy(col("INDPRO"))
  p$inv_months[is.na(p$yield_spread)] <- NA

  p$n_inputs <- rowSums(!is.na(p[, .COMPOSITE_INPUTS]))
  p$score <- vapply(seq_len(nrow(p)), function(i) {
    .composite_score(.composite_components(as.list(p[i, .COMPOSITE_INPUTS])))$score_0_10
  }, numeric(1))
  p
}

#' @param rec_series optional data.frame(date, prob) — monthly OOS recession
#'   probability in [0,1] (backtest_recession_glm()$series)
fit_gdp_mapping <- function(fred_data, rec_series = NULL, min_inputs = 12L) {
  tryCatch(
    {
      sh <- build_score_history(fred_data)
      if (is.null(sh)) return(NULL)
      gdp <- fredr::fredr(
        "A191RL1Q225SBEA",
        observation_start = as.Date("1959-01-01"),
        frequency = "q"
      ) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::transmute(q = lubridate::floor_date(date, "quarter"), gdp = value)

      # Score in the last month of quarter q → GDP growth in quarter q+1
      d <- sh %>%
        dplyr::filter(lubridate::month(date) %in% c(3, 6, 9, 12)) %>%
        dplyr::mutate(q = lubridate::floor_date(date, "quarter"), q_next = q %m+% months(3)) %>%
        dplyr::inner_join(gdp, by = c(q_next = "q")) %>%
        dplyr::rename(gdp_next = gdp)

      if (!is.null(rec_series)) {
        d <- d %>% dplyr::left_join(
          rec_series %>% dplyr::transmute(date = lubridate::floor_date(date, "month"), rec = prob),
          by = "date"
        )
      } else {
        d$rec <- NA_real_
      }

      # COVID lockdown/rebound quarters (-28% / +35% SAAR) are not forecastable
      # from macro indicators and would dominate an OLS fit
      covid <- d$q_next %in% as.Date(c("2020-04-01", "2020-07-01"))
      d <- d[d$n_inputs >= min_inputs & !covid, ]
      if (nrow(d) < 40) {
        message(sprintf("[t3] GDP mapping: only %d usable quarters — skipping fit.", nrow(d)))
        return(NULL)
      }

      # Keep recession probability only if it adds significant (negative) info
      rec_coef <- 0
      rec_t <- NA_real_
      dr <- d[!is.na(d$rec), ]
      if (nrow(dr) >= 40) {
        m2 <- lm(gdp_next ~ score + rec, data = dr)
        cf <- summary(m2)$coefficients
        rec_t <- cf["rec", "t value"]
        if (cf["rec", "Estimate"] < 0 && rec_t < -1.96) {
          d <- dr
          rec_coef <- cf["rec", "Estimate"]
        }
      }
      fml <- if (rec_coef < 0) gdp_next ~ score + rec else gdp_next ~ score
      m <- lm(fml, data = d)
      cf <- summary(m)$coefficients
      res <- residuals(m)

      # Expanding-window pseudo-OOS from 2005 vs naive historical mean
      oos_idx <- which(d$q_next >= as.Date("2005-01-01"))
      oos <- t(vapply(oos_idx, function(i) {
        tr <- d[d$q_next < d$q[i], ]
        if (nrow(tr) < 30) return(c(NA_real_, NA_real_))
        mi <- lm(fml, data = tr)
        c(d$gdp_next[i] - predict(mi, newdata = d[i, ]), d$gdp_next[i] - mean(tr$gdp_next))
      }, numeric(2)))
      ok <- stats::complete.cases(oos)

      out <- list(
        intercept = unname(coef(m)[1]),
        slope = unname(coef(m)["score"]),
        slope_se = cf["score", "Std. Error"],
        rec_coef = rec_coef,
        rec_t = rec_t,
        r2 = summary(m)$r.squared,
        n = nrow(d),
        from = min(d$q_next),
        to = max(d$q_next),
        resid_sd = stats::sd(res),
        resid_q10 = unname(stats::quantile(res, 0.10)),
        resid_q90 = unname(stats::quantile(res, 0.90)),
        rmse_oos = sqrt(mean(oos[ok, 1]^2)),
        rmse_naive = sqrt(mean(oos[ok, 2]^2)),
        n_oos = sum(ok),
        last_gdp = tail(gdp, 1),
        fitted_at = Sys.time()
      )
      message(sprintf(
        "[t3] GDP mapping: gdp = %.2f + %.2f×score%s (R²=%.2f, n=%d, %s→%s); 80%% band [%+.1f, %+.1f]; OOS RMSE %.2f vs naive %.2f; rec t=%.2f",
        out$intercept, out$slope,
        if (rec_coef < 0) sprintf(" %+.2f×rec", rec_coef) else "",
        out$r2, out$n, format(out$from, "%Y-%m"), format(out$to, "%Y-%m"),
        out$resid_q10, out$resid_q90, out$rmse_oos, out$rmse_naive, rec_t
      ))
      out
    },
    error = function(e) {
      message("[t3] fit_gdp_mapping failed: ", e$message)
      NULL
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# GDP POINT FORECAST
# Primary: the fitted mapping from fit_gdp_mapping() (score → next-quarter
# GDP, residual-based 80% band).
# Fallback (fit unavailable): piecewise-linear judgemental anchors below, with
# no recession-probability drag (the score already carries those signals) and
# a wide ±2pp band.
#   score  0  →  -3.5%  (deep recession: 2008Q4, 2020Q1)
#   score  3  →  -0.5%  (mild recession / stall: 2001, 2022H1)
#   score  5  →  +1.5%  (below-trend: 2015–16, 2019)
#   score  7  →  +2.5%  (trend growth: 2014–17 average)
#   score  8  →  +3.0%  (above-trend: 2018, 2023Q3)
#   score 10  →  +4.5%  (boom: 2021Q3, 1983–84)
# ═══════════════════════════════════════════════════════════════════════════════

.GDP_SCORE_ANCHORS <- data.frame(
  score = c(0, 3, 5, 7, 8, 10),
  gdp = c(-3.5, -0.5, 1.5, 2.5, 3.0, 4.5)
)

.compute_gdp_forecast <- function(score_0_10, recession_prob_pct, components,
                                  mapping = NULL) {
  if (is.null(score_0_10) || is.na(score_0_10)) {
    return(NULL)
  }
  rec_prob <- if (!is.null(recession_prob_pct) && !is.na(recession_prob_pct)) {
    pmax(0, pmin(100, recession_prob_pct))
  } else {
    NA_real_
  }

  if (!is.null(mapping)) {
    rec_drag <- if (mapping$rec_coef < 0 && !is.na(rec_prob)) {
      mapping$rec_coef * rec_prob / 100
    } else {
      0
    }
    gdp_point <- mapping$intercept + mapping$slope * score_0_10 + rec_drag
    gdp_ci_lo <- gdp_point + mapping$resid_q10
    gdp_ci_hi <- gdp_point + mapping$resid_q90
    method <- "fitted"
  } else {
    rec_drag <- 0
    gdp_point <- stats::approx(
      .GDP_SCORE_ANCHORS$score, .GDP_SCORE_ANCHORS$gdp,
      xout = score_0_10, rule = 2
    )$y
    gdp_ci_lo <- gdp_point - 2.0
    gdp_ci_hi <- gdp_point + 2.0
    method <- "anchors"
  }
  uncertainty <- (gdp_ci_hi - gdp_ci_lo) / 2

  # NOTE: the regime label shown alongside this forecast is derived directly
  # from `gdp_point` via .regime_from_gdp_point(), so label and number can't
  # disagree. See build_growth_outlook().

  # ── Primary driver of the forecast ───────────────────────────────────────────
  if (!is.null(components)) {
    comp_contribs <- sapply(components, function(c) c$score * c$weight)
    top_driver_idx <- which.max(abs(comp_contribs))
    top_driver <- list(
      name = components[[top_driver_idx]]$label,
      score = round(components[[top_driver_idx]]$score, 2),
      contrib_sign = if (comp_contribs[top_driver_idx] > 0) {
        "supporting"
      } else {
        "dragging"
      }
    )
  } else {
    top_driver <- NULL
  }

  list(
    point = round(gdp_point, 1),
    ci_lo = round(gdp_ci_lo, 1),
    ci_hi = round(gdp_ci_hi, 1),
    uncertainty = round(uncertainty, 2),
    rec_drag = round(rec_drag, 2),
    top_driver = top_driver,
    method = method,
    mapping = mapping
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
build_growth_outlook <- function(fred_data, kpis, mkt_returns = NULL) {
  if (is.null(kpis)) {
    return(NULL)
  }
  tryCatch(
    {
      gv <- function(sid) {
        df <- fred_data[[sid]]
        if (is.null(df) || nrow(df) < 2) {
          return(NULL)
        }
        df %>% dplyr::arrange(date) %>% dplyr::pull(value)
      }
      payrolls <- gv("PAYEMS")
      cpi <- gv("CPIAUCSL")
      fedfunds <- gv("FEDFUNDS")
      t10 <- gv("DGS10")
      t2 <- gv("DGS2")
      mort <- gv("MORTGAGE30US")
      sent <- gv("UMCSENT")
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
      inv_months <- .inv_months_monthly(fred_data, 18L)

      components <- .composite_components(list(
        u = u, pay_3m = pay_3m, ret_real = ret_real, cs = cs,
        real_rate = real_rate, yield_spread = yield_spread,
        inv_months = inv_months, hs = hs, mort = mort_v, vix = vix_v,
        hy = hy_v, cpi_y = cpi_y, pce_y = pce_y, ip_y = ip_y
      ))
      cs_out <- .composite_score(components)
      raw_score <- cs_out$raw
      score_0_10 <- cs_out$score_0_10
      drag <- names(which.min(sapply(components, `[[`, "score")))
      supp <- names(which.max(sapply(components, `[[`, "score")))
      swing <- list(
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

      # ── Disk-backed cache — persists across Shiny restarts ───────────────────────
      # Cache file lives in the app's working directory. Change the path if needed.
      cache_dir <- tryCatch(
        dirname(getOption("shiny.appDir", default = ".")),
        error = function(e) "."
      )
      cache_file <- file.path(cache_dir, ".recession_t3_cache.rds")
      cache_name <- ".recession_t3_cache" # in-memory copy for fast access within session

      # Load from disk if not in memory
      cache <- tryCatch(
        if (exists(cache_name, envir = .GlobalEnv)) {
          get(cache_name, envir = .GlobalEnv)
        } else {
          NULL
        },
        error = function(e) NULL
      )

      if (is.null(cache) && file.exists(cache_file)) {
        tryCatch(
          {
            cache <- readRDS(cache_file)
            assign(cache_name, cache, envir = .GlobalEnv)
            message("[t3] Cache loaded from disk: ", cache_file)
          },
          error = function(e) {
            message("[t3] Could not read cache file: ", e$message)
            cache <- NULL
          }
        )
      }

      # Re-fetch daily: FRED adds a month at a time and revises recent ones
      needs_train <- is.null(cache) ||
        is.null(cache$glm) ||
        difftime(
          Sys.time(),
          cache$trained_at %||% (Sys.time() - 1e9),
          units = "days"
        ) >
          1

      message(sprintf("[t3] needs_train=%s", needs_train))
      if (needs_train) {
        run_training <- function() {
          prog <- function(msg, val) {
            tryCatch(
              shiny::setProgress(val, message = msg),
              error = function(e) message("[t3] ", msg)
            )
          }
          prog("Fetching FRED history + fitting recession GLM\u2026", 0.05)
          glm_obj <- train_recession_glm()
          prog("Calibrating score \u2192 GDP mapping\u2026", 0.85)
          gdp_map <- fit_gdp_mapping(fred_data, glm_obj$backtest$series)
          prog("Caching results\u2026", 0.95)
          new_cache <- list(glm = glm_obj, gdp_map = gdp_map, trained_at = Sys.time())
          assign(cache_name, new_cache, envir = .GlobalEnv)
          # Persist to disk so cache survives Shiny restarts
          tryCatch(
            {
              # saveRDS(new_cache, cache_file)
              # message(sprintf(
              #   "[t3] Cache saved to disk: %s (valid 1 day)",
              #   cache_file
              # ))
            },
            error = function(e) {
              message("[t3] Could not write cache to disk: ", e$message)
            }
          )
        }
        tryCatch(
          shiny::withProgress(
            message = "Fitting econometric models\u2026",
            value = 0,
            run_training()
          ),
          error = function(e) run_training()
        ) # no-op outside Shiny
        cache <- tryCatch(
          get(cache_name, envir = .GlobalEnv),
          error = function(e) NULL
        )
      }

      recession_prob <- .compute_recession_prob(cache$glm)

      gdp_forecast <- .compute_gdp_forecast(
        score_0_10,
        recession_prob$prob,
        components,
        mapping = cache$gdp_map
      )
      # Regime label is derived from the final GDP point estimate (post
      # recession-probability drag), not the raw composite score, so the
      # headline label can never contradict the number shown beneath it.
      regime <- .regime_from_gdp_point(gdp_forecast$point)

      list(
        score = score_0_10,
        raw_score = raw_score,
        regime = regime,
        components = components,
        swing = swing,
        recession_prob = recession_prob,
        as_of = Sys.Date(),
        gdp_forecast = gdp_forecast,
        gdpnow = .fetch_gdpnow(fred_data)
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      # Capture call stack for diagnostics
      calls_text <- tryCatch(
        paste(
          sapply(rev(head(sys.calls(), 20)), function(c) deparse(c)[1]),
          collapse = " <- "
        ),
        error = function(e2) "unavailable"
      )
      message("[t3] build_growth_outlook ERROR: ", msg)
      message("[t3] Call stack: ", substr(calls_text, 1, 500))
      list(
        .error = TRUE,
        .error_msg = paste0(msg, "\n\nCall chain: ", substr(calls_text, 1, 300))
      )
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# render_growth_outlook_html + .build_factor_bar_chart + .build_model_modal
# ═══════════════════════════════════════════════════════════════════════════════

.build_factor_bar_chart <- function(fc, model_type = NULL) {
  if (is.null(fc) || nrow(fc) == 0) {
    return("<p style='color:#6b7585;font-size:11px;'>No data — the recession model could not be fitted (check the FRED connection).</p>")
  }
  fc <- fc[order(abs(fc$contribution), decreasing = TRUE), ]
  max_abs <- max(abs(fc$contribution), na.rm = TRUE)
  if (max_abs < 0.01) {
    max_abs <- 1
  }

  subtitle <- paste0(
    "<span style='color:#2dce89;font-size:10px;'>&#x2713; β×z per predictor (robust z-score, log-odds). ",
    htmltools::htmlEscape(model_type %||% ""),
    "</span>"
  )

  rows <- vapply(
    seq_len(nrow(fc)),
    function(i) {
      row <- fc[i, ]
      delta <- row$contribution
      is_miss <- isTRUE(row$data_missing)
      is_zero <- !is_miss && abs(delta) < 0.001
      col <- if (is_miss || is_zero) {
        "#3a4052"
      } else if (delta > 0) {
        "#e94560"
      } else {
        "#2dce89"
      }
      pct <- if (is_miss || is_zero) {
        0
      } else {
        round(min(abs(delta) / max_abs * 44, 44))
      }
      bar <- if (is_miss) {
        "left:50%;width:2px;background:repeating-linear-gradient(180deg,#4a5062 0,#4a5062 3px,transparent 3px,transparent 6px);"
      } else if (is_zero) {
        "left:50%;width:2px;background:#3a4052;"
      } else if (delta >= 0) {
        sprintf("left:50%%;width:%dpx;background:%s;", pct * 3, col)
      } else {
        sprintf("right:50%%;width:%dpx;background:%s;", pct * 3, col)
      }
      beta <- row$beta
      zsc <- row$z_score
      tag <- if (is_miss) {
        "&nbsp;<span style='color:#555;font-size:9px;'>N/A</span>"
      } else {
        ""
      }
      # Hover breakdown: why the bar points the way it does
      tip <- if (!is.na(beta) && !is.na(zsc)) {
        paste0(
          sprintf("β = %+.2f × z = %+.2f → %+.2f log-odds", beta, zsc, delta),
          if (!is.na(row$raw_value)) sprintf(" | today: %.2f", row$raw_value) else "",
          sprintf(" (%s). ", if (abs(zsc) < 0.05) "at usual level" else if (zsc < 0) "below usual" else "above usual"),
          if (beta == 0) {
            "Ridge shrank this β to 0, so it has no effect at present."
          } else {
            sprintf("Model: a higher value means %s recession risk.", if (beta > 0) "more" else "less")
          }
        )
      } else {
        "No data for this month — scored at the training median (no effect)."
      }
      nc <- if (is_miss) "#555" else "#d0d0d0"
      val_str <- if (is_miss) {
        "<span style='color:#555;font-size:10px;'>N/A</span>"
      } else {
        sprintf(
          "<span style='color:%s;font-size:10px;font-weight:600;'>%+.2f</span>",
          col,
          delta
        )
      }
      sprintf(
        "<div class='tm-fc-row' style='display:flex;align-items:center;gap:6px;margin-bottom:7px;'><div style='width:220px;text-align:right;color:%s;font-size:10.5px;flex-shrink:0;line-height:1.3;'>%s%s</div><div style='flex:1;position:relative;height:13px;background:#2a3042;border-radius:3px;overflow:hidden;'><div style='position:absolute;top:0;left:50%%;width:1px;height:100%%;background:#3a4052;z-index:1;'></div><div style='position:absolute;top:0;height:100%%;border-radius:2px;%s'></div></div><div style='width:52px;text-align:right;'>%s</div><div class='tm-fc-tip'>%s</div></div>",
        nc,
        htmltools::htmlEscape(row$name),
        tag,
        bar,
        val_str,
        htmltools::htmlEscape(tip)
      )
    },
    character(1)
  )

  # CSS tooltip: native title= tooltips are delayed and don't show in some
  # embedded viewers (e.g. RStudio)
  tip_css <- paste0(
    "<style>",
    ".tm-fc-row{position:relative;}",
    ".tm-fc-row:hover{background:rgba(255,255,255,0.03);}",
    ".tm-fc-tip{display:none;position:absolute;left:226px;top:100%;z-index:50;margin-top:2px;",
    "max-width:560px;padding:6px 10px;background:#0f1117;border:1px solid #3a4052;border-radius:6px;",
    "color:#d0d0d0;font-size:11px;line-height:1.5;box-shadow:0 4px 14px rgba(0,0,0,0.5);pointer-events:none;}",
    ".tm-fc-row:hover .tm-fc-tip{display:block;}",
    "</style>"
  )

  paste0(
    tip_css,
    "<div style='margin-bottom:18px;'>",
    "<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;'>",
    "<span style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>Live Factor Contributions</span></div>",
    "<div style='margin-bottom:8px;'>",
    subtitle,
    "</div>",
    "<div style='display:flex;flex-wrap:wrap;gap:14px;font-size:10px;color:#9aa3b2;margin-bottom:9px;'>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#e94560;border-radius:2px;margin-right:4px;'></span>Raises risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#2dce89;border-radius:2px;margin-right:4px;'></span>Lowers risk</span>",
    "<span><span style='display:inline-block;width:10px;height:10px;background:#3a4052;border-radius:2px;margin-right:4px;'></span>Neutral/N/A</span>",
    "<span style='color:#555;'>| zero |</span></div>",
    "<div style='display:flex;justify-content:space-between;font-size:10px;color:#555;margin-bottom:6px;'><span>◄ Protective</span><span>Recessionary ►</span></div>",
    paste(rows, collapse = ""),
    "<div style='font-size:9px;color:#555;margin-top:6px;'>Coloured bar = active contribution. Flat line = neutral. Hover any bar for β × z.</div></div>"
  )
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
      htmltools::htmlEscape(outlook$.error_msg %||% "Unknown error"),
      "</div>",
      "<div style='color:#555;font-size:11px;'>To force a retrain, run in the R console:<br/>",
      "<code>file.remove('.recession_t3_cache.rds'); rm(.recession_t3_cache, envir=.GlobalEnv)</code>",
      "</div></div>"
    )))
  }
  # If cache exists but outlook is NULL, something errored — show an error card not the spinner
  if (is.null(outlook)) {
    cache_exists <- file.exists(".recession_t3_cache.rds") ||
      exists(".recession_t3_cache", envir = .GlobalEnv)
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
        "<code>rm(.recession_t3_cache, envir=.GlobalEnv)</code></div></div>"
      )))
    }
    return(.growth_outlook_loading_html())
  }
  if (isTRUE(outlook$.training)) {
    return(.growth_outlook_loading_html(outlook$.step))
  }
  if (
    is.null(outlook$regime) ||
      !is.list(outlook$regime) ||
      is.null(outlook$regime$color)
  ) {
    return(shiny::HTML(
      "<div style='color:#e94560;padding:16px;'>Regime classification error \u2014 check console.</div>"
    ))
  }
  # Wrap entire render in tryCatch so errors show as a card rather than blank/spinner
  result <- tryCatch(
    {
      rp <- outlook$recession_prob
      # regime is already derived from gdp_forecast$point in build_growth_outlook(),
      # so it can't contradict the point estimate rendered below.
      reg <- outlook$regime
      gfo <- outlook$gdp_forecast
      score <- outlook$score
      comps <- outlook$components
      swing <- outlook$swing
      gc <- reg$color

      # ── GDP point forecast display ───────────────────────────────────────────────
      gf <- outlook$gdp_forecast
      gdp_display <- if (!is.null(gf)) {
        pt <- gf$point
        lo <- gf$ci_lo
        hi <- gf$ci_hi
        col <- if (pt >= 2.5) {
          "#2dce89"
        } else if (pt >= 1.0) {
          "#00b4d8"
        } else if (pt >= 0) {
          "#f4a261"
        } else {
          "#e94560"
        }
        mp <- gf$mapping
        ci_note <- paste0(
          if (identical(gf$method, "fitted")) "80% band [" else "±2pp band [",
          lo, "%, ", hi, "%]"
        )
        method_note <- if (identical(gf$method, "fitted")) {
          sprintf(
            paste0(
              "Regression of next-quarter real GDP on the composite score, %s–%s ",
              "(n=%d, excl. 2020Q2–Q3). Out-of-sample RMSE %.1fpp vs %.1fpp for a ",
              "naive average. Band from historical residuals. Not a professional forecast."
            ),
            format(mp$from, "%Y"), format(mp$to, "%Y"), mp$n, mp$rmse_oos, mp$rmse_naive
          )
        } else {
          paste0(
            "Calibration unavailable — judgemental score→GDP anchors, uncalibrated. ",
            "Not a professional forecast."
          )
        }
        gn <- outlook$gdpnow
        gdpnow_html <- if (!is.null(gn) && !is.na(gn$value)) {
          gn_col <- if (gn$value >= 2.5) {
            "#2dce89"
          } else if (gn$value >= 1.0) {
            "#00b4d8"
          } else if (gn$value >= 0) {
            "#f4a261"
          } else {
            "#e94560"
          }
          paste0(
            "<div style='display:flex;align-items:baseline;gap:10px;flex-wrap:wrap;",
            "margin-top:8px;padding-top:8px;border-top:1px solid #22304a;'>",
            "<span style='color:#9aa3b2;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>",
            "Atlanta Fed GDPNow &middot; ", gn$quarter, "</span>",
            "<span style='color:", gn_col, ";font-size:20px;font-weight:800;'>",
            sprintf("%+.1f", gn$value), "%</span>",
            "<span style='color:#6b7585;font-size:11px;'>",
            sprintf("model %+.1fpp vs nowcast", pt - gn$value),
            if (!is.na(gn$updated)) {
              paste0(" &middot; updated ", format(gn$updated, "%b %d"))
            } else {
              ""
            },
            "</span></div>"
          )
        } else {
          ""
        }
        td <- gf$top_driver
        driver_note <- if (!is.null(td)) {
          paste0(" &nbsp;\u2014&nbsp; ", td$contrib_sign, ": ", td$name)
        } else {
          ""
        }
        paste0(
          "<div style='margin:10px 0 14px;padding:12px 16px;background:#0a1520;border-radius:8px;border-left:4px solid ",
          col,
          ";'>",
          "<div style='color:#9aa3b2;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>",
          "Next-Quarter GDP Forecast &nbsp;<span style='color:#555;font-weight:400;font-size:9px;'>(annualized, model-based)</span></div>",
          "<div style='display:flex;align-items:baseline;gap:10px;flex-wrap:wrap;'>",
          "<span style='color:",
          col,
          ";font-size:30px;font-weight:800;'>",
          sprintf("%+.1f", pt),
          "%</span>",
          "<span style='color:#6b7585;font-size:12px;'>",
          ci_note,
          "</span>",
          "</div>",
          gdpnow_html,
          "<div style='color:#6b7585;font-size:10px;margin-top:4px;'>",
          if (gf$rec_drag < -0.2) {
            paste0(
              "Recession probability drag: ",
              sprintf("%.1f", gf$rec_drag),
              "pp &nbsp;"
            )
          } else {
            ""
          },
          driver_note,
          "</div>",
          "<div style='color:#555;font-size:9px;margin-top:6px;'>",
          method_note,
          "</div></div>"
        )
      } else {
        paste0(
          "<div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>",
          "GDP Est: ",
          reg$gdp_est,
          " annualized</div>"
        )
      }

      gauge_html <- paste0(
        "<div style='margin:8px 0 14px;'>",
        "<div style='display:flex;justify-content:space-between;margin-bottom:5px;'>",
        "<span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span>",
        "<span style='color:#9aa3b2;font-size:11px;'>Expansion</span></div>",
        "<div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'>",
        "<div style='background:linear-gradient(90deg,",
        if (score < 5) "#e94560" else "#f4a261",
        ",",
        gc,
        ");",
        "width:",
        round(score / 10 * 100),
        "%;height:100%;border-radius:6px;'></div></div>",
        "<div style='margin-top:5px;display:flex;justify-content:space-between;'>",
        "<span style='color:#9aa3b2;font-size:10px;'>0</span>",
        "<span style='color:",
        gc,
        ";font-size:11px;font-weight:700;'>Score: ",
        score,
        " / 10</span>",
        "<span style='color:#9aa3b2;font-size:10px;'>10</span></div></div>"
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
        # Build badges with paste0 — avoids % signs in strings being re-parsed by outer sprintf
        data_badge <- if (!is.null(rp$month)) {
          paste0(
            "&nbsp;<span style='display:inline-block;white-space:nowrap;background:rgba(0,180,216,0.1);color:#00b4d8;border:1px solid rgba(0,180,216,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>",
            "data through ",
            format(rp$month, "%b %Y"),
            "</span>"
          )
        } else {
          ""
        }
        uh <- if (length(rp$drivers_up) > 0) {
          paste(
            paste0(
              "<li><span style='color:#e94560;'>\u25b2</span> ",
              htmltools::htmlEscape(rp$drivers_up),
              "</li>"
            ),
            collapse = ""
          )
        } else {
          ""
        }
        dh <- if (length(rp$drivers_down) > 0) {
          paste(
            paste0(
              "<li><span style='color:#2dce89;'>\u25bc</span> ",
              htmltools::htmlEscape(rp$drivers_down),
              "</li>"
            ),
            collapse = ""
          )
        } else {
          ""
        }
        ds <- if (nchar(uh) > 0 || nchar(dh) > 0) {
          paste0(
            "<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>",
            uh,
            dh,
            "</ul>"
          )
        } else {
          ""
        }
        # Build rp_html entirely with paste0 — no sprintf, no % format hazard
        rp_html <- paste0(
          "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid ",
          bc,
          ";padding:14px 16px;margin-top:14px;'>",
          "<div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'>",
          "<div><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'>",
          "<i class=\"fa fa-",
          tier$icon,
          "\" style=\"color:",
          bc,
          ";margin-right:6px;\"></i>",
          "Recession Probability \u2014 12-Month Horizon",
          data_badge,
          "</div>",
          "<div style='margin-top:4px;'>",
          "<span style='color:",
          bc,
          ";font-size:36px;font-weight:800;'>",
          round(prob),
          "%</span>",
          "<span style='color:",
          bc,
          ";font-size:14px;font-weight:600;margin-left:8px;",
          "background:rgba(",
          bc,
          ",0.12);padding:2px 10px;border-radius:12px;border:1px solid rgba(",
          bc,
          ",0.3);'>",
          tier$label,
          "</span></div></div>",
          "<div style='text-align:right;'>",
          "<div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0% \u00b7 50% \u00b7 100%</div>",
          "<div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);border-radius:7px;position:relative;'>",
          "<div style='position:absolute;top:-3px;left:calc(",
          min(round(prob), 98),
          "% - 8px);",
          "width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:8px solid #fff;'></div>",
          "</div><div style='font-size:10px;color:#6b7585;margin-top:3px;'>\u25b2 Current</div></div></div>",
          "<div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;border-radius:6px;padding:8px 12px;border-left:3px solid ",
          bc,
          ";'>",
          htmltools::htmlEscape(tier$desc),
          "</div>",
          ds,
          "<div style='color:#555;font-size:10px;margin-top:8px;'>",
          "Ridge GLM on 9 predictors (each β sign-constrained), refit each January on a rolling 30-year window — see Model details for the backtest. ",
          "<a href='https://willshelley404.github.io/recession-tracker/' target='_blank' rel='noopener' ",
          "style='color:#00b4d8;text-decoration:none;'>Latest recession tracker \u2197</a>",
          "</div></div>"
        )
      }

      bt_cache <- tryCatch(
        get(".recession_t3_cache", envir = .GlobalEnv)$glm$backtest,
        error = function(e) NULL
      )

      # Outer layout also uses paste0 to avoid any residual % issues from rp_html
      shiny::HTML(paste0(
        "<div style='display:flex;gap:14px;'>",
        "<div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid ",
        reg$color,
        ";padding:16px 18px;'>",
        "<div style='color:",
        reg$color,
        ";font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'>",
        "<i class=\"fa fa-",
        reg$icon,
        "\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook</div>",
        "<div style='color:",
        reg$color,
        ";font-size:22px;font-weight:800;margin-bottom:2px;'>",
        reg$label,
        "</div>",
        gdp_display,
        gauge_html,
        "<div style='background:#0f1117;border-radius:6px;padding:10px 12px;color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid ",
        reg$color,
        ";'>",
        htmltools::htmlEscape(reg$summary),
        "</div>",
        "<div style='color:#555;font-size:10px;margin-top:10px;'>",
        length(comps),
        " indicators + Ridge recession GLM. As of ",
        format(outlook$as_of, "%b %d, %Y"),
        ".",
        "<button onclick=\"document.getElementById('recModelModal').style.display='flex'\" ",
        "style='margin-left:8px;background:transparent;border:1px solid #2a3042;color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'>",
        "<i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details</button></div>",
        rp_html,
        "</div>",
        "<div style='flex:1;display:flex;flex-direction:column;gap:10px;'>",
        "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;flex:1;'>",
        "<div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>",
        comp_html,
        "</div>",
        "<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;'>",
        "<div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div>",
        "<ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>",
        swing_html,
        "</ul></div>",
        "</div></div>",
        .build_model_modal(rp = rp, bt = bt_cache)
      ))
    },
    error = function(e) {
      msg <- conditionMessage(e)
      message("[t3] render_growth_outlook_html error: ", msg)
      shiny::HTML(paste0(
        "<div style='background:#1a2035;border:1px solid #e94560;border-radius:8px;padding:20px;'>",
        "<div style='color:#e94560;font-size:13px;font-weight:700;margin-bottom:8px;'>",
        "<i class='fa fa-exclamation-triangle' style='margin-right:8px;'></i>",
        "Growth Outlook Render Error</div>",
        "<div style='color:#9aa3b2;font-size:12px;margin-bottom:12px;'>",
        htmltools::htmlEscape(msg),
        "</div>",
        "<div style='color:#555;font-size:11px;'>",
        "Run <code>rm(.recession_t3_cache, envir=.GlobalEnv); file.remove('.recession_t3_cache.rds')</code> ",
        "in the R console to clear the cache and retrain.</div></div>"
      ))
    }
  )
  result
}

.build_model_modal <- function(rp = NULL, bt = NULL) {
  fc <- rp$factor_contributions
  bar_html <- .build_factor_bar_chart(fc, rp$model_type)
  pct <- function(x) if (is.null(x) || is.na(x)) "N/A" else sprintf("%.0f%%", x * 100)

  # Walk-forward backtest: overall scores, then one line per recession
  bt_html <- if (!is.null(bt)) {
    eps <- if (nrow(bt$episodes) > 0) {
      paste(
        sprintf(
          "<span style='margin-right:14px;'>%s: 12 months before <b style='color:%s;'>%s</b> peak, through the recession <b style='color:%s;'>%s</b></span>",
          bt$episodes$recession,
          ifelse(bt$episodes$runup_peak > 0.5, "#2dce89", "#e94560"),
          vapply(bt$episodes$runup_peak, pct, character(1)),
          ifelse(bt$episodes$spell_peak > 0.5, "#2dce89", "#e94560"),
          vapply(bt$episodes$spell_peak, pct, character(1))
        ),
        collapse = ""
      )
    } else {
      "no recession in window"
    }
    paste0(
      "<div style='background:#1a1a2e;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #f4a261;'>",
      "<div style='color:#f4a261;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'><i class=\"fa fa-history\" style=\"margin-right:6px;\"></i>Walk-Forward Backtest <span style='color:#555;font-size:10px;font-weight:400;'>refit each January on labels known at the time</span></div>",
      sprintf(
        "<div style='color:#d0d0d0;font-size:11px;margin-bottom:3px;'><span style='color:#555;'>%s–%s, n=%d months with a known outcome</span></div>",
        format(bt$from, "%Y"), format(bt$to, "%Y"), bt$n
      ),
      sprintf(
        "<div style='color:#9aa3b2;font-size:11px;margin-bottom:3px;'>Brier skill <b style='color:%s;'>%.2f</b> &nbsp;|&nbsp; AUC %.2f &nbsp;|&nbsp; &gt;50%% in %s of months with a recession ahead &nbsp;|&nbsp; &gt;50%% in %s of months without (false alarms)</div>",
        if (isTRUE(bt$skill > 0)) "#2dce89" else "#e94560",
        bt$skill, bt$auc, pct(bt$hit_rate), pct(bt$false_alarm_rate)
      ),
      "<div style='color:#9aa3b2;font-size:11px;margin-bottom:6px;'>", eps, "</div>",
      "<div style='color:#555;font-size:10px;line-height:1.6;'>Each month's value comes from a model trained only on months whose 12-month outcome had resolved (12-month embargo). Brier skill &lt; 0 = worse than forecasting the training base rate. Not backtested: NBER's ~6–12 month dating lag.</div>",
      "</div>"
    )
  } else {
    "<div style='background:#1e2640;border-radius:6px;padding:10px 14px;margin-bottom:20px;color:#555;font-size:11px;'><i class=\"fa fa-info-circle\" style=\"color:#9aa3b2;margin-right:6px;\"></i>Backtest unavailable — the recession model could not be fitted.</div>"
  }

  # Fitted parameters for the latest month
  coef_html <- if (!is.null(fc) && nrow(fc) > 0) {
    rows <- vapply(.GLM_PREDICTORS, function(p) {
      r <- fc[fc$name == .GLM_LABELS[[p]], ]
      f2 <- function(x) if (is.na(x)) "N/A" else sprintf("%.2f", x)
      sprintf(
        "<tr><td style='padding:3px 8px;color:#d0d0d0;'>%s</td><td style='padding:3px 8px;text-align:right;'>%s</td><td style='padding:3px 8px;text-align:right;'>%s</td><td style='padding:3px 8px;text-align:right;'>%s</td><td style='padding:3px 8px;text-align:right;'>%+.2f</td><td style='padding:3px 8px;text-align:right;'>%+.2f</td><td style='padding:3px 8px;text-align:right;color:%s;font-weight:600;'>%+.2f</td></tr>",
        htmltools::htmlEscape(.GLM_LABELS[[p]]),
        f2(r$raw_value),
        f2(r$center),
        f2(r$scale),
        r$z_score, r$beta,
        if (r$contribution > 0) "#e94560" else if (r$contribution < 0) "#2dce89" else "#9aa3b2",
        r$contribution
      )
    }, character(1))
    th <- "padding:3px 8px;text-align:right;color:#6b7585;font-weight:600;"
    paste0(
      "<div style='background:#0f1a27;border-radius:8px;padding:14px 16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
      "<div style='color:#00b4d8;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'><i class=\"fa fa-table\" style=\"margin-right:6px;\"></i>Fitted Model ",
      sprintf(
        "<span style='color:#555;font-size:10px;font-weight:400;'>%s data &nbsp;|&nbsp; trained on %d months through %s &nbsp;|&nbsp; base rate %s &nbsp;|&nbsp; intercept %+.2f &nbsp;|&nbsp; fitted %s</span>",
        format(rp$month, "%b %Y"), rp$n_train, format(rp$train_end, "%b %Y"),
        pct(rp$base_rate), rp$intercept, format(rp$trained_at, "%b %d, %Y")
      ),
      "</div>",
      "<div style='overflow-x:auto;'><table style='border-collapse:collapse;font-size:11px;color:#9aa3b2;width:100%;'>",
      "<tr><th style='padding:3px 8px;text-align:left;color:#6b7585;font-weight:600;'>Predictor</th>",
      "<th style='", th, "'>Today</th><th style='", th, "'>Median</th><th style='", th, "'>MAD</th>",
      "<th style='", th, "'>z</th><th style='", th, "'>β</th><th style='", th, "'>β×z</th></tr>",
      paste(rows, collapse = ""),
      "</table></div></div>"
    )
  } else {
    ""
  }

  paste0(
    "<div id='recModelModal' onclick=\"if(event.target===this)this.style.display='none'\" style='display:none;position:fixed;inset:0;background:rgba(0,0,0,0.75);z-index:9999;align-items:center;justify-content:center;padding:20px;'>",
    "<div style='background:#161b27;border:1px solid #2a3042;border-radius:10px;max-width:1100px;width:100%;max-height:90vh;overflow-y:auto;padding:24px;position:relative;'>",
    "<button onclick=\"document.getElementById('recModelModal').style.display='none'\" style='position:absolute;top:14px;right:16px;background:transparent;border:none;color:#9aa3b2;font-size:20px;cursor:pointer;'>&times;</button>",
    "<div style='color:#00b4d8;font-size:16px;font-weight:800;margin-bottom:4px;'><i class=\"fa fa-brain\" style=\"margin-right:8px;\"></i>Recession Probability Model — Technical Reference <span style='font-size:11px;font-weight:400;color:#00b4d8;margin-left:10px;border:1px solid rgba(0,180,216,0.3);border-radius:6px;padding:1px 8px;'>Sign-constrained Ridge GLM</span></div>",
    "<div style='color:#9aa3b2;font-size:12px;margin-bottom:20px;'>Ridge logistic regression on nine predictors — yield curve, unemployment rise, underemployment-gap rise (U-6 − U-3), payroll momentum, real fed funds rate, prime-age participation, the 12-month change in consumer sentiment, the Baa corporate spread over 10-year Treasuries and the rise in jobless claims — with each β constrained to its theory sign. Refit each January on the latest 30 years of months whose 12-month NBER outcome was known by then; the months of that year are scored with that fit. Not a point forecast.</div>",
    "<div style='background:#1e2640;border-radius:8px;padding:16px;margin-bottom:20px;border-left:4px solid #00b4d8;'>",
    bar_html,
    "</div>",
    coef_html,
    bt_html,
    "<div style='color:#555;font-size:11px;margin-top:8px;line-height:1.7;'><b style='color:#9aa3b2;'>Formula:</b> p = logistic(β₀ + Σ βₖ·zₖ), zₖ = clamp((xₖ − median) / MAD, −3, 3) using the training window's median and MAD; a missing value scores as z = 0. λ chosen by 5-fold CV (lambda.1se). Target: an NBER recession month in any of the next 12 months.<br/><b style='color:#9aa3b2;'>Refs:</b> Estrella &amp; Mishkin (1998); Sahm (2019); Brier (1950).</div>",
    "</div></div>"
  )
}
