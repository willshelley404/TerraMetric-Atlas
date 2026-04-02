# ─────────────────────────────────────────────────────────────────────────────
# R/synopsis_tier3_patch.R  —  Drop-in fixes for synopsis_tier3.R
#
# In global.R source BOTH files in order:
#   source("R/synopsis_tier3.R")
#   source("R/synopsis_tier3_patch.R")
#
# WHAT THIS FIXES:
#  1. CRASH  dplyr::case_when() cannot return list() on RHS in standard dplyr.
#            The error "'length = 6' in coercion to 'logical(1)'" is exactly
#            this.  Replaced with plain if/else if via .regime_from_score().
#
#  2. BLANK  render_growth_outlook_html(NULL) returned a one-liner grey text.
#            Now returns a full animated loading screen so the user knows
#            training is in progress rather than seeing nothing.
#
#  3. OBS    train_markov_model() pulled INDPRO from fred_data which often
#            only holds recent history (6-12 months).  Now fetches full series
#            from fredr() first (back to 1959), falls back to fred_data.
#            Cascade: INDPRO YoY → PAYEMS MoM → UNRATE.
#
#  4. UX     withProgress() wraps each training step so the Shiny progress bar
#            fills while GLM + Markov + Brier weights are being computed.
#            Graceful fallback when called outside a Shiny session.
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

# ── FIX 1: regime classifier that does not use case_when + list ──────────────
.regime_from_score <- function(score_0_10) {
  if (score_0_10 >= 7.5)
    list(label="Expansion",        color="#2dce89", icon="arrow-up",
         gdp_est="+2.5% to +3.5%",
         summary="Broad-based growth signals. Labor market solid, consumer spending resilient, financial conditions supportive.")
  else if (score_0_10 >= 5.5)
    list(label="Moderate Growth",  color="#00b4d8", icon="minus",
         gdp_est="+1.0% to +2.5%",
         summary="Below-trend but positive growth likely. Mixed signals — watch rate transmission and consumer confidence.")
  else if (score_0_10 >= 3.5)
    list(label="Stall / Slowdown", color="#f4a261", icon="arrow-down",
         gdp_est="-0.5% to +1.0%",
         summary="Growth at risk. Restrictive monetary conditions, softening demand, or financial stress beginning to bite.")
  else
    list(label="Contraction Risk", color="#e94560", icon="exclamation-triangle",
         gdp_est="Below -0.5%",
         summary="Multiple negative signals. Recession probability elevated — yield curve, credit spreads, and demand deteriorating.")
}

# ── FIX 2: animated loading screen ───────────────────────────────────────────
.growth_outlook_loading_html <- function(step = NULL) {
  step_label <- if (!is.null(step)) htmltools::htmlEscape(step) else "Fitting models\u2026"
  shiny::HTML(sprintf(
    "<style>
       @keyframes t3spin  { to { transform:rotate(360deg); } }
       @keyframes t3pulse { 0%%,100%% { opacity:.35; } 50%% { opacity:1; } }
       .t3-spinner { width:40px;height:40px;border:3px solid #2a3042;
                     border-top-color:#00b4d8;border-radius:50%;
                     animation:t3spin .9s linear infinite;margin:0 auto 16px; }
       .t3-dot { display:inline-block;width:6px;height:6px;border-radius:50%;
                 background:#00b4d8;margin:0 2px;
                 animation:t3pulse 1.2s ease-in-out infinite; }
       .t3-dot:nth-child(2){animation-delay:.2s;} .t3-dot:nth-child(3){animation-delay:.4s;}
     </style>
     <div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;
                 padding:40px 24px;text-align:center;min-height:300px;
                 display:flex;flex-direction:column;align-items:center;
                 justify-content:center;gap:12px;'>
       <div class='t3-spinner'></div>
       <div style='color:#00b4d8;font-size:13px;font-weight:700;
                   text-transform:uppercase;letter-spacing:1px;'>
         Fitting Econometric Models
       </div>
       <div style='color:#9aa3b2;font-size:12px;max-width:420px;line-height:1.7;'>%s</div>
       <div style='display:flex;gap:4px;'><span class='t3-dot'></span>
         <span class='t3-dot'></span><span class='t3-dot'></span></div>
       <div style='margin-top:14px;background:#0f1117;border-radius:6px;
                   padding:12px 20px;max-width:480px;text-align:left;'>
         <div style='color:#555;font-size:10px;font-weight:700;text-transform:uppercase;
                     letter-spacing:1px;margin-bottom:8px;'>What&apos;s running</div>
         <div style='color:#6b7585;font-size:11px;line-height:2;'>
           <span style='color:#2dce89;'>&#x2713;</span> Load FRED macro history<br/>
           <span style='color:#f4a261;'>&#x25cb;</span> Fit rolling GLM on NBER recession dates (30yr window)<br/>
           <span style='color:#f4a261;'>&#x25cb;</span> Fit Hamilton Markov-switching regime model<br/>
           <span style='color:#f4a261;'>&#x25cb;</span> Score models on 36-month holdout (Brier skill)<br/>
           <span style='color:#f4a261;'>&#x25cb;</span> Build Mahalanobis anomaly detector<br/>
           <span style='color:#9aa3b2;'>&#x25cb;</span> Render growth outlook panel
         </div>
       </div>
       <div style='color:#555;font-size:10px;margin-top:6px;'>
         First run: 10\u201360 seconds. Cached for 30 days.
       </div>
     </div>", step_label))
}

# ── FIX 3 + 4: build_growth_outlook with regime fix + withProgress ────────────
build_growth_outlook <- function(fred_data, kpis, mkt_returns = NULL) {
  if (is.null(kpis)) return(NULL)

  gv <- function(sid) {
    df <- fred_data[[sid]]
    if (is.null(df) || nrow(df) < 2) return(NULL)
    df %>% dplyr::arrange(date) %>% dplyr::pull(value)
  }

  unemp<-gv("UNRATE"); payrolls<-gv("PAYEMS"); cpi<-gv("CPIAUCSL")
  fedfunds<-gv("FEDFUNDS"); t10<-gv("DGS10"); t2<-gv("DGS2")
  mort<-gv("MORTGAGE30US"); sent<-gv("UMCSENT"); indpro<-gv("INDPRO")
  vix_vals<-gv("VIXCLS"); hy<-gv("BAMLH0A0HYM2"); oil<-gv("DCOILWTICO")
  retail<-gv("RSAFS")

  u    <- kpis$unemp_rate   %||% NA;  cpi_y <- kpis$cpi_yoy     %||% NA
  pce_y<- kpis$core_pce     %||% NA;  ff    <- kpis$fed_funds    %||% NA
  t10v <- kpis$t10yr         %||% NA;  t2v   <- kpis$t2yr         %||% NA
  mort_v<-kpis$mortgage30   %||% NA;  hs    <- kpis$housing_starts%||% NA
  ret_y<- kpis$retail_yoy   %||% NA;  cs    <- kpis$cons_sent    %||% NA
  ip_y <- kpis$indpro_yoy   %||% NA;  vix_v <- kpis$vix          %||% NA
  hy_v <- kpis$hy_spread     %||% NA

  real_rate    <- if (!is.na(ff)   && !is.na(cpi_y)) ff - cpi_y       else NA
  yield_spread <- if (!is.na(t10v) && !is.na(t2v))   t10v - t2v       else NA
  pay_3m       <- if (!is.null(payrolls) && length(payrolls) >= 4)
                    mean(diff(tail(payrolls,4)), na.rm=TRUE)            else NA
  ret_real     <- if (!is.na(ret_y) && !is.na(cpi_y)) ret_y - cpi_y   else NA
  inv_months   <- if (!is.null(t10) && !is.null(t2)) {
    n <- min(length(t10), length(t2), 18)
    sum((tail(t10,n) - tail(t2,n)) < 0, na.rm=TRUE)
  } else 0

  components <- list(
    labor     = list(weight=2.5, label="Labor Market",
                     score=mean(c(.score(u,3.5,5.0,FALSE),.score(pay_3m,150,50,TRUE),.score(u,4.5,5.5,FALSE)), na.rm=TRUE),
                     detail=if(!is.na(u)&&!is.na(pay_3m)) sprintf("%.1f%% unemployment; %+.0fK/mo payrolls",u,pay_3m) else "N/A"),
    consumer  = list(weight=2.0, label="Consumer Demand",
                     score=mean(c(.score(ret_real,1,-1,TRUE),.score(cs,85,65,TRUE)), na.rm=TRUE),
                     detail=if(!is.na(ret_real)&&!is.na(cs)) sprintf("Real retail %.1f%%; sentiment %.0f",ret_real,cs) else "N/A"),
    monetary  = list(weight=2.0, label="Monetary Conditions",
                     score=mean(c(.score(real_rate,0.5,2.5,FALSE),.score(yield_spread,0.5,-0.25,TRUE),.score(inv_months,3,9,FALSE)), na.rm=TRUE),
                     detail=if(!is.na(real_rate)&&!is.na(yield_spread)) sprintf("Real rate %.1f%%; 10Y-2Y %.2f pp; %d mo inv.",real_rate,yield_spread,inv_months) else "N/A"),
    housing   = list(weight=1.5, label="Housing",
                     score=mean(c(.score(hs,1400,1000,TRUE),.score(mort_v,6,7.5,FALSE)), na.rm=TRUE),
                     detail=if(!is.na(hs)&&!is.na(mort_v)) sprintf("%.0fK starts; %.2f%% mortgage",hs,mort_v) else "N/A"),
    financial = list(weight=1.5, label="Financial Conditions",
                     score=mean(c(.score(vix_v,18,28,FALSE),.score(hy_v,4.5,6,FALSE)), na.rm=TRUE),
                     detail=if(!is.na(vix_v)&&!is.na(hy_v)) sprintf("VIX %.1f; HY %.2f%%",vix_v,hy_v) else "N/A"),
    inflation = list(weight=1.5, label="Inflation",
                     score=mean(c(.score(cpi_y,2.5,4.5,FALSE),.score(pce_y,2.3,3.5,FALSE)), na.rm=TRUE),
                     detail=if(!is.na(cpi_y)&&!is.na(pce_y)) sprintf("CPI %.1f%%; Core PCE %.1f%%",cpi_y,pce_y) else "N/A"),
    industrial= list(weight=1.0, label="Industrial Output",
                     score=.score(ip_y,1,-1,TRUE),
                     detail=if(!is.na(ip_y)) sprintf("IP YoY %.1f%%",ip_y) else "N/A")
  )

  tw         <- sum(sapply(components, `[[`, "weight"))
  raw_score  <- sum(sapply(components, function(c) c$score * c$weight)) / tw
  score_0_10 <- max(0, min(10, round((raw_score + 1) / 2 * 10, 1)))

  # ── FIX 1: .regime_from_score replaces broken dplyr::case_when + list ─────
  regime <- .regime_from_score(score_0_10)

  drag <- names(which.min(sapply(components, `[[`, "score")))
  supp <- names(which.max(sapply(components, `[[`, "score")))
  swing_factors <- list(
    biggest_drag      = list(name=components[[drag]]$label, score=round(components[[drag]]$score,2), detail=components[[drag]]$detail),
    biggest_support   = list(name=components[[supp]]$label, score=round(components[[supp]]$score,2), detail=components[[supp]]$detail),
    yield_curve_watch = if (inv_months > 6) sprintf("Inverted %d months.", inv_months)
                        else if (!is.na(yield_spread) && yield_spread > 0) sprintf("Re-steepened to %.2f pp.", yield_spread)
                        else NULL,
    fed_watch = if (!is.na(real_rate) && real_rate > 2) sprintf("Real rate %.1f%% \u2014 restrictive.", real_rate) else NULL
  )

  equity_drawdown_pct <- if (!is.null(mkt_returns)) {
    spy <- mkt_returns %>% dplyr::filter(symbol=="SPY") %>% dplyr::arrange(date)
    if (nrow(spy) >= 52) {
      cur <- tail(spy$close, 1)
      pk  <- max(spy$close[max(1,nrow(spy)-252):nrow(spy)], na.rm=TRUE)
      round((cur/pk - 1)*100, 1)
    } else NA_real_
  } else NA_real_

  # ── FIX 4: withProgress around training, graceful outside-Shiny fallback ────
  cache_name <- ".recession_t3_cache"
  cache <- tryCatch(
    if (exists(cache_name, envir=.GlobalEnv)) get(cache_name, envir=.GlobalEnv) else NULL,
    error = function(e) NULL)

  needs_train <- is.null(cache) || is.null(cache$glm) ||
    difftime(Sys.time(), cache$glm$trained_at %||% (Sys.time()-1e9), units="days") > 30

  if (needs_train) {

    run_training <- function() {
      prog <- function(msg, val)
        tryCatch(shiny::setProgress(val, message=msg), error=function(e) message("[t3] ", msg))

      prog("Training GLM on NBER recession history\u2026", 0.05)
      glm_obj <- rolling_retrain(fred_data, window=360L)

      prog("Fitting Hamilton Markov-switching regime model\u2026", 0.45)
      ms_obj  <- train_markov_model(fred_data)

      prog("Scoring models on out-of-sample holdout (Brier)\u2026", 0.75)
      ew_obj  <- compute_ensemble_weights(glm_obj, ms_obj, holdout_months=36L)

      prog("Caching results\u2026", 0.95)
      assign(cache_name, list(glm=glm_obj, markov=ms_obj, ew=ew_obj), envir=.GlobalEnv)
    }

    tryCatch(
      shiny::withProgress(message="Fitting econometric models\u2026", value=0, run_training()),
      error = function(e) {
        # Outside a Shiny session — run without progress UI
        run_training()
      })

    cache <- tryCatch(get(cache_name, envir=.GlobalEnv), error=function(e) NULL)
  }

  glm_result       <- cache$glm
  markov_result    <- cache$markov
  ensemble_weights <- cache$ew
  anomaly_detector <- if (!is.null(glm_result)) glm_result$anomaly_detector else NULL

  recession_prob <- .compute_recession_prob(
    yield_spread=yield_spread, inv_months=inv_months, unemp=unemp, u=u,
    pay_3m=pay_3m, real_rate=real_rate, hy_v=hy_v, vix_v=vix_v, cs=cs, ip_y=ip_y, hs=hs,
    prime_age_lfpr       = kpis$prime_age_lfpr       %||% NA,
    prime_age_lfpr_chg   = kpis$prime_age_lfpr_chg   %||% NA,
    jobless_claims_trend = kpis$jobless_claims_trend  %||% NA,
    quits_yoy            = kpis$quits_yoy             %||% NA,
    oil_yoy              = kpis$oil_yoy               %||% NA,
    equity_drawdown_pct  = equity_drawdown_pct,
    usd_yoy              = kpis$usd_yoy               %||% NA,
    inventory_sales_ratio= kpis$inventory_sales_ratio %||% NA,
    cc_delinquency       = kpis$cc_delinquency        %||% NA,
    glm_result=glm_result, anomaly_detector=anomaly_detector,
    markov_result=markov_result, ensemble_weights=ensemble_weights)

  list(score=score_0_10, raw_score=raw_score, regime=regime, components=components,
       swing=swing_factors, recession_prob=recession_prob, as_of=Sys.Date())
}

# ── FIX 2: render with loading screen ────────────────────────────────────────
render_growth_outlook_html <- function(outlook) {
  if (is.null(outlook))          return(.growth_outlook_loading_html())
  if (isTRUE(outlook$.training)) return(.growth_outlook_loading_html(outlook$.step))

  if (is.null(outlook$regime) || !is.list(outlook$regime) || is.null(outlook$regime$color))
    return(shiny::HTML("<div style='color:#e94560;padding:16px;'>Regime classification error \u2014 check R console. Ensure synopsis_tier3_patch.R is sourced after synopsis_tier3.R in global.R.</div>"))

  rp  <- outlook$recession_prob
  reg <- outlook$regime
  score <- outlook$score
  comps <- outlook$components
  swing <- outlook$swing
  gc    <- reg$color

  gauge_html <- sprintf(
    "<div style='margin:12px 0 16px;'>
       <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
         <span style='color:#9aa3b2;font-size:11px;'>Contraction Risk</span>
         <span style='color:#9aa3b2;font-size:11px;'>Expansion</span></div>
       <div style='background:#2a3042;border-radius:6px;height:12px;overflow:hidden;'>
         <div style='background:linear-gradient(90deg,%s,%s);width:%.0f%%;height:100%%;border-radius:6px;'></div></div>
       <div style='margin-top:5px;display:flex;justify-content:space-between;'>
         <span style='color:#9aa3b2;font-size:10px;'>0</span>
         <span style='color:%s;font-size:11px;font-weight:700;'>Score: %.1f / 10</span>
         <span style='color:#9aa3b2;font-size:10px;'>10</span></div></div>",
    if(score<5)"#e94560"else"#f4a261", gc, score/10*100, gc, score)

  comp_html <- paste(vapply(names(comps), function(nm) {
    c<-comps[[nm]];s<-c$score
    col<-if(s>=0.4)"#2dce89"else if(s>=-0.4)"#f4a261"else"#e94560"
    bw<-round((s+1)/2*100)
    sprintf("<div style='margin-bottom:8px;'><div style='display:flex;justify-content:space-between;margin-bottom:3px;'><span style='color:#9aa3b2;font-size:11px;'>%s</span><span style='color:%s;font-size:11px;font-weight:600;'>%s</span></div><div style='background:#2a3042;border-radius:3px;height:5px;'><div style='background:%s;width:%d%%;height:100%%;border-radius:3px;'></div></div><div style='color:#6b7585;font-size:10px;margin-top:2px;'>%s</div></div>",
            c$label,col,if(s>=0.4)"Positive"else if(s>=-0.4)"Neutral"else"Negative",col,bw,htmltools::htmlEscape(c$detail))
  }, character(1)), collapse="")

  swing_html <- paste(c(
    if(!is.null(swing$biggest_drag))    sprintf("<li style='margin-bottom:6px;'><span style='color:#e94560;font-weight:600;'>\u2b07 Biggest drag: %s</span> \u2014 %s</li>",swing$biggest_drag$name,htmltools::htmlEscape(swing$biggest_drag$detail)),
    if(!is.null(swing$biggest_support)) sprintf("<li style='margin-bottom:6px;'><span style='color:#2dce89;font-weight:600;'>\u2b06 Biggest support: %s</span> \u2014 %s</li>",swing$biggest_support$name,htmltools::htmlEscape(swing$biggest_support$detail)),
    if(!is.null(swing$yield_curve_watch))sprintf("<li style='margin-bottom:6px;'><span style='color:#f4a261;font-weight:600;'>\u26a0 Yield Curve:</span> %s</li>",htmltools::htmlEscape(swing$yield_curve_watch)),
    if(!is.null(swing$fed_watch))       sprintf("<li style='margin-bottom:6px;'><span style='color:#00b4d8;font-weight:600;'>\u2699 Fed:</span> %s</li>",htmltools::htmlEscape(swing$fed_watch))
  ), collapse="")

  rp_html <- ""
  if (!is.null(rp)) {
    prob<-rp$prob; tier<-rp$tier; bc<-tier$color
    ew_badge<-if(!is.null(rp$ensemble_weights)&&!is.null(rp$ensemble_weights$glm_brier))
      sprintf("&nbsp;<span style='background:rgba(0,180,216,0.1);color:#00b4d8;border:1px solid rgba(0,180,216,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>w_GLM=%.0f%% w_MS=%.0f%%</span>",rp$ensemble_weights$w_glm*100,rp$ensemble_weights$w_ms*100)else""
    anom_badge<-if(!is.null(rp$anomaly)&&rp$anomaly$score>0.1)
      sprintf("&nbsp;<span style='background:rgba(124,92,191,0.12);color:#a785e0;border:1px solid rgba(124,92,191,0.3);border-radius:8px;padding:1px 8px;font-size:9px;'>\u26a0 Anomaly %.0f%%</span>",rp$anomaly$score*100)else""
    ms_badge<-if(!is.null(rp$markov_prob)&&!is.na(rp$markov_prob))
      sprintf("&nbsp;<span style='background:rgba(0,180,216,0.08);color:#7dd8f0;border:1px solid rgba(0,180,216,0.2);border-radius:8px;padding:1px 8px;font-size:9px;'>MS: %.0f%%</span>",rp$markov_prob*100)else""
    uh<-if(length(rp$drivers_up)>0)   paste(sprintf("<li><span style='color:#e94560;'>\u25b2</span> %s</li>",htmltools::htmlEscape(rp$drivers_up)),collapse="")else""
    dh<-if(length(rp$drivers_down)>0) paste(sprintf("<li><span style='color:#2dce89;'>\u25bc</span> %s</li>",htmltools::htmlEscape(rp$drivers_down)),collapse="")else""
    ds<-if(nchar(uh)>0||nchar(dh)>0) sprintf("<ul style='list-style:none;padding:0;margin:6px 0 0;font-size:11px;color:#d0d0d0;'>%s%s</ul>",uh,dh)else""
    rp_html<-sprintf("<div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:14px 16px;margin-top:14px;'><div style='display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;'><div><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;'><i class=\"fa fa-%s\" style=\"color:%s;margin-right:6px;\"></i>Recession Probability \u2014 12-Month Horizon%s%s%s</div><div style='margin-top:4px;'><span style='color:%s;font-size:36px;font-weight:800;'>%s%%</span><span style='color:%s;font-size:14px;font-weight:600;margin-left:8px;background:rgba(%s,0.12);padding:2px 10px;border-radius:12px;border:1px solid rgba(%s,0.3);'>%s</span></div></div><div style='text-align:right;'><div style='font-size:10px;color:#6b7585;margin-bottom:4px;'>0%% \u00b7 50%% \u00b7 100%%</div><div style='width:160px;height:14px;background:linear-gradient(90deg,#2dce89,#f4a261,#e94560);border-radius:7px;position:relative;'><div style='position:absolute;top:-3px;left:calc(%s%% - 8px);width:0;height:0;border-left:7px solid transparent;border-right:7px solid transparent;border-top:8px solid #fff;'></div></div><div style='font-size:10px;color:#6b7585;margin-top:3px;'>\u25b2 Current</div></div></div><div style='color:#9aa3b2;font-size:12px;line-height:1.6;background:#0f1117;border-radius:6px;padding:8px 12px;border-left:3px solid %s;'>%s</div>%s<div style='color:#555;font-size:10px;margin-top:8px;'>Tier 3: rolling GLM + Markov + anomaly. Data-driven Brier weights. NBER-calibrated.</div></div>",
            bc,tier$icon,bc,ew_badge,ms_badge,anom_badge,bc,sprintf("%.0f",prob),bc,bc,bc,tier$label,sprintf("%.0f",min(prob,98)),bc,htmltools::htmlEscape(tier$desc),ds)
  }

  mr <- tryCatch(get(".recession_t3_cache",envir=.GlobalEnv)$markov,error=function(e)NULL)
  ew <- tryCatch(get(".recession_t3_cache",envir=.GlobalEnv)$ew,    error=function(e)NULL)

  shiny::HTML(paste0(
    sprintf("<div style='display:flex;gap:14px;'><div style='flex:1;background:#1a2035;border:1px solid #2a3042;border-radius:8px;border-top:3px solid %s;padding:16px 18px;'><div style='color:%s;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;'><i class=\"fa fa-%s\" style=\"margin-right:6px;\"></i>6\u201312 Month Growth Outlook</div><div style='color:%s;font-size:22px;font-weight:800;margin-bottom:2px;'>%s</div><div style='color:#f4a261;font-size:16px;font-weight:700;margin-bottom:10px;'>GDP Est: %s annualized</div>%s<div style='background:#0f1117;border-radius:6px;padding:10px 12px;color:#9aa3b2;font-size:12px;line-height:1.7;border-left:3px solid %s;'>%s</div><div style='color:#555;font-size:10px;margin-top:10px;'>%d indicators + Markov + rolling GLM + anomaly. As of %s.<button onclick=\"document.getElementById('recModelModal').style.display='flex'\" style='margin-left:8px;background:transparent;border:1px solid #2a3042;color:#9aa3b2;border-radius:10px;padding:1px 8px;font-size:10px;cursor:pointer;'><i class=\"fa fa-info-circle\" style=\"margin-right:4px;color:#00b4d8;\"></i>Model details</button></div>%s</div><div style='flex:1;display:flex;flex-direction:column;gap:10px;'><div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;flex:1;'><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;'>Component Scorecard</div>%s</div><div style='background:#1a2035;border:1px solid #2a3042;border-radius:8px;padding:14px 16px;'><div style='color:#9aa3b2;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;'>Key Swing Factors</div><ul style='list-style:none;padding:0;margin:0;font-size:12px;color:#d0d0d0;'>%s</ul></div></div></div>",
           reg$color,reg$color,reg$icon,reg$color,reg$label,reg$gdp_est,gauge_html,reg$color,htmltools::htmlEscape(reg$summary),length(comps),format(outlook$as_of,"%b %d, %Y"),rp_html,comp_html,swing_html),
    .build_model_modal(factor_contributions=if(!is.null(rp))rp$factor_contributions else NULL,
                       model_type=if(!is.null(rp))rp$model_type%||%"Hand-tuned"else"Hand-tuned",
                       anomaly=if(!is.null(rp))rp$anomaly else NULL,
                       markov_result=mr, ensemble_weights=ew)))
}

# ── FIX 3: train_markov_model with fredr full-history fetch ──────────────────
train_markov_model <- function(fred_data, n_states = 2L) {
  tryCatch({
    gm_local <- function(sid, val_name) {
      df <- fred_data[[sid]]
      if (is.null(df) || nrow(df) < 24) return(NULL)
      df %>% dplyr::arrange(date) %>%
        dplyr::mutate(month=lubridate::floor_date(date,"month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(!!val_name:=mean(value,na.rm=TRUE),.groups="drop") %>%
        dplyr::rename(date=month)
    }
    gm_fred <- function(sid, val_name, start=as.Date("1959-01-01")) {
      tryCatch(fredr::fredr(sid,observation_start=start,frequency="m") %>%
                 dplyr::select(date,!!val_name:=value),
               error=function(e){message(sprintf("[t3] fredr('%s') failed: %s",sid,e$message));gm_local(sid,val_name)})
    }

    switching_var <- NULL; switch_name <- NULL

    indpro_raw <- gm_fred("INDPRO","indpro")
    if (!is.null(indpro_raw) && nrow(indpro_raw) >= 24) {
      cand <- indpro_raw %>% dplyr::arrange(date) %>%
        dplyr::mutate(sw_var=(indpro/dplyr::lag(indpro,12)-1)*100) %>%
        dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
      if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"IP YoY %" }
    }
    if (is.null(switching_var)) {
      pay_raw <- gm_fred("PAYEMS","payems")
      if (!is.null(pay_raw) && nrow(pay_raw) >= 24) {
        cand <- pay_raw %>% dplyr::arrange(date) %>%
          dplyr::mutate(sw_var=payems-dplyr::lag(payems,1)) %>%
          dplyr::filter(!is.na(sw_var)) %>% dplyr::select(date,sw_var)
        if (nrow(cand) >= 60) { switching_var<-cand; switch_name<-"Payroll MoM chg (K)" }
      }
    }
    if (is.null(switching_var)) {
      urate_raw <- gm_fred("UNRATE","unemp")
      if (!is.null(urate_raw) && nrow(urate_raw) >= 60) {
        switching_var<-urate_raw %>% dplyr::rename(sw_var=unemp); switch_name<-"Unemployment rate"
      }
    }
    if (is.null(switching_var)) { message("[t3] No switching variable found."); return(NULL) }

    message(sprintf("[t3] Markov switching var: '%s' (%d obs)", switch_name, nrow(switching_var)))

    spread_raw <- gm_fred("T10Y2Y","yield_spread")
    if (is.null(spread_raw)) {
      t10<-gm_fred("DGS10","t10"); t2<-gm_fred("DGS2","t2")
      if (!is.null(t10)&&!is.null(t2))
        spread_raw<-dplyr::inner_join(t10,t2,by="date")%>%dplyr::mutate(yield_spread=t10-t2)%>%dplyr::select(date,yield_spread)
    }
    hy_raw   <- gm_fred("BAMLH0A0HYM2","hy_spread")
    unem_raw <- gm_fred("UNRATE","unemp")

    panel <- switching_var
    for (df in list(spread_raw,hy_raw,unem_raw))
      if (!is.null(df)) panel<-dplyr::left_join(panel,df,by="date")
    panel <- panel %>% dplyr::filter(!is.na(sw_var))

    avail_preds <- intersect(c("yield_spread","hy_spread","unemp"),names(panel))
    avail_preds <- avail_preds[sapply(avail_preds,function(v)mean(is.na(panel[[v]]))<0.30)]
    for (v in avail_preds) panel[[v]][is.na(panel[[v]])]<-mean(panel[[v]],na.rm=TRUE)

    if (nrow(panel)<60){message(sprintf("[t3] Panel too small: %d rows.",nrow(panel)));return(NULL)}

    base_fml <- if (length(avail_preds)>0) as.formula(paste("sw_var~",paste(avail_preds,collapse="+"))) else as.formula("sw_var~1")
    base_lm  <- lm(base_fml,data=panel)
    n_coef   <- length(coef(base_lm))
    sw_vec   <- c(TRUE,rep(FALSE,max(0,n_coef-1)),TRUE)
    if (length(sw_vec)!=n_coef+1) sw_vec<-rep(TRUE,n_coef)

    ms_model <- MSwM::msmFit(base_lm,k=n_states,sw=sw_vec,control=list(parallel=FALSE,maxiter=300))
    smooth_probs <- ms_model@Fit@smoProb
    if (is.null(smooth_probs)||nrow(smooth_probs)==0) return(NULL)

    state_means     <- sapply(seq_len(n_states),function(s)ms_model@Coef[1,s])
    recession_state <- if(switch_name=="Unemployment rate")which.max(state_means)else which.min(state_means)
    current_rec_prob<- smooth_probs[nrow(smooth_probs),recession_state]

    state_assign  <- apply(smooth_probs,1,which.max)
    panel_trimmed <- panel[seq_len(nrow(smooth_probs)),]
    regime_means  <- lapply(avail_preds,function(v)list(variable=v,expansion=round(mean(panel_trimmed[[v]][state_assign!=recession_state],na.rm=TRUE),2),recession=round(mean(panel_trimmed[[v]][state_assign==recession_state],na.rm=TRUE),2)))
    sw_regime_means<-list(variable=switch_name,expansion=round(mean(panel_trimmed$sw_var[state_assign!=recession_state],na.rm=TRUE),2),recession=round(mean(panel_trimmed$sw_var[state_assign==recession_state],na.rm=TRUE),2))

    message(sprintf("[t3] Markov: %s n=%d recession_state=%d p_rec=%.1f%%",switch_name,nrow(panel),recession_state,current_rec_prob*100))

    list(model=ms_model,smooth_probs=smooth_probs,recession_state=recession_state,
         current_rec_prob=current_rec_prob,state_means_sw=state_means,switch_name=switch_name,
         regime_means=regime_means,sw_regime_means=sw_regime_means,
         n_states=n_states,n_obs=nrow(panel),trained_at=Sys.time())
  },error=function(e){message("[t3] Markov failed: ",e$message);NULL})
}
