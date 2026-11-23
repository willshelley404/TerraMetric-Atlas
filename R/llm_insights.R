# ─────────────────────────────────────────────────────────────────────────────
# R/llm_insights.R — Provider-agnostic LLM integration (OpenAI-compatible)
# ─────────────────────────────────────────────────────────────────────────────

#' Core LLM call — works with any OpenAI-compatible endpoint
call_llm <- function(messages, provider=NULL, model=NULL,
                      max_tokens=2500, temperature=0.65, system=NULL) {
  pname <- if (!is.null(provider)) provider else ACTIVE_PROVIDER
  if (is.null(pname) || !pname %in% names(LLM_PROVIDERS)) {
    return(paste0(
      "**LLM not configured.** Set one of these in your `.Renviron` and restart:\n\n",
      "- `GROQ_API_KEY` → https://console.groq.com (recommended, fastest free tier)\n",
      "- `GEMINI_API_KEY` → https://aistudio.google.com/apikey (free, strong)"
    ))
  }

  cfg     <- LLM_PROVIDERS[[pname]]
  api_key <- Sys.getenv(cfg$key_env)
  if (nchar(api_key) == 0) {
    return(glue(
      "**{cfg$display_name} API key not set.**\n\n",
      "Add `{cfg$key_env}=your_key_here` to `.Renviron` and restart the app.\n",
      "Get a free key at: {
        switch(pname,
          groq   = 'https://console.groq.com',
          gemini = 'https://aistudio.google.com/apikey',
          'see provider docs')
      }"
    ))
  }

  if (is.null(model)) model <- cfg$default_model

  full_msgs <- if (!is.null(system)) {
    c(list(list(role="system", content=system)), messages)
  } else messages

  # A model that just failed with 429/5xx is skipped for a few minutes, so the
  # next call (e.g. chat right after insights) goes straight to the fallback.
  if (llm_is_down(pname, model)) {
    message(sprintf("[LLM] %s/%s failed recently — skipping to fallback", pname, model))
    res <- list(ok = FALSE, transient = TRUE,
                text = paste0("**", cfg$display_name, " is temporarily unavailable.**"))
  } else {
    res <- llm_request(pname, model, full_msgs, max_tokens, temperature)
    if (res$ok || !res$transient) return(res$text)
    llm_mark_down(pname, model)
  }

  # Transient failure (429 / 5xx) persisted through retries — fall back to the
  # provider's other models, then to any other provider with a key configured.
  failed <- sprintf("%s/%s", pname, model)
  for (cand in llm_fallbacks(pname, model)) {
    if (llm_is_down(cand$provider, cand$model)) next
    message(sprintf("[LLM] %s unavailable — trying %s/%s", failed, cand$provider, cand$model))
    alt <- llm_request(cand$provider, cand$model, full_msgs, max_tokens, temperature, tries = 2L)
    if (!alt$ok && alt$transient) llm_mark_down(cand$provider, cand$model)
    failed <- sprintf("%s/%s", cand$provider, cand$model)
    if (alt$ok) {
      return(paste0(alt$text, "

*Note: ", cfg$display_name, " (", model,
                    ") was temporarily unavailable; answered by ",
                    LLM_PROVIDERS[[cand$provider]]$display_name, " (", cand$model, ").*"))
    }
  }
  res$text
}

.LLM_DOWN <- new.env(parent = emptyenv())
LLM_DOWN_MINUTES <- 5

llm_mark_down <- function(pname, model, now = Sys.time())
  assign(paste0(pname, "/", model), now, envir = .LLM_DOWN)

llm_is_down <- function(pname, model, now = Sys.time()) {
  t <- .LLM_DOWN[[paste0(pname, "/", model)]]
  !is.null(t) && as.numeric(difftime(now, t, units = "mins")) < LLM_DOWN_MINUTES
}

#' Alternative (provider, model) pairs to try when the requested one is down
llm_fallbacks <- function(pname, model) {
  out <- list()
  for (m in setdiff(LLM_PROVIDERS[[pname]]$models, model))
    out[[length(out) + 1]] <- list(provider = pname, model = m)
  for (p in setdiff(names(LLM_PROVIDERS), pname)) {
    if (nchar(Sys.getenv(LLM_PROVIDERS[[p]]$key_env)) > 0)
      out[[length(out) + 1]] <- list(provider = p, model = LLM_PROVIDERS[[p]]$default_model)
  }
  head(out, 3)
}

#' One provider/model request with retry on transient errors (429, 500-504).
#' Returns list(ok, text, transient).
llm_request <- function(pname, model, full_msgs, max_tokens, temperature, tries = 3L) {
  cfg     <- LLM_PROVIDERS[[pname]]
  api_key <- Sys.getenv(cfg$key_env)
  fail <- function(text, transient = FALSE) list(ok = FALSE, text = text, transient = transient)

  for (attempt in seq_len(tries)) {
    result <- tryCatch({
      resp <- request(paste0(cfg$base_url, "/chat/completions")) %>%
        req_headers(
          "Authorization" = paste("Bearer", api_key),
          "Content-Type"  = "application/json",
          "HTTP-Referer"  = "https://econpulse.app",
          "X-Title"       = "EconPulse AI"
        ) %>%
        req_body_json(list(
          model       = model,
          messages    = full_msgs,
          max_tokens  = max_tokens,
          temperature = temperature
        )) %>%
        req_timeout(90) %>%
        req_perform()

      body <- resp %>% resp_body_json()

      if (!is.null(body$choices) && length(body$choices) > 0) {
        content <- body$choices[[1]]$message$content
        # DeepSeek R1 sometimes puts answer in reasoning_content
        if (is.null(content) || nchar(trimws(content)) == 0)
          content <- body$choices[[1]]$message$reasoning_content %||%
                     "Model returned an empty response."
        # Reasoning models spend part of max_tokens thinking; say so when the answer was cut
        if (identical(body$choices[[1]]$finish_reason, "length"))
          content <- paste0(content, "\n\n*(Answer hit the length limit and was cut off. ",
                            "Ask to continue, or try a shorter question.)*")
        list(ok = TRUE, text = content, transient = FALSE)
      } else if (!is.null(body$error)) {
        fail(paste0("**API error (", cfg$display_name, "):** ", body$error$message))
      } else {
        fail(paste0("Unexpected response from ", cfg$display_name, ". Try Groq."))
      }
    }, error = function(e) {
      # httr2 colours messages with ANSI escapes, which render as junk in the UI
      msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(e))
      transient <- grepl("\\b(429|500|502|503|504)\\b", msg)
      if (transient && attempt < tries) {
        wait <- if (grepl("429", msg)) 5 * 2^(attempt - 1) else 2 * 2^(attempt - 1)
        message(sprintf("[LLM] transient error on attempt %d (%s) — waiting %ds", attempt, msg, wait))
        Sys.sleep(wait)
        return("__RETRY__")
      }
      guidance <- if (grepl("resolve|Could not resolve|resolve hostname", msg, ignore.case=TRUE)) {
        paste0(" — **DNS/network error.** The server cannot reach `", cfg$base_url, "`.",
               " On Posit Connect, enable outbound connections in App Settings,",
               " or ask your admin to allowlist the LLM hostname.")
      } else if (grepl("429", msg)) {
        " — **Rate limited.** Wait 60 seconds and retry, or switch provider."
      } else if (grepl("\\b(500|502|503|504)\\b", msg)) {
        " — **Provider temporarily unavailable/overloaded.** Retry in a minute or switch provider."
      } else if (grepl("404", msg)) {
        " — **Model not found.** Select a different model from the dropdown."
      } else if (grepl("400", msg)) {
        " — **Bad request.** Try switching to Groq GPT-OSS 120B."
      } else if (grepl("401|403", msg)) {
        " — **Auth failed.** Check your API key is correct and active."
      } else if (grepl("timeout|timed out", msg, ignore.case=TRUE)) {
        " — **Request timed out.** Try a smaller/faster model."
      } else if (grepl("network|connect|connection", msg, ignore.case=TRUE)) {
        " — **Network error.** Check outbound internet access from this server."
      } else {
        ""
      }
      fail(paste0("**LLM call failed (", cfg$display_name, "):** ", msg, guidance), transient)
    })

    if (!identical(result, "__RETRY__")) return(result)
  }
  fail(paste0("**LLM call failed (", cfg$display_name, "):** retries exhausted."), TRUE)
}

# Reference month for a YoY KPI, e.g. " (Sep 2026)"; empty when unknown.
.kpi_month <- function(d) if (is.null(d) || length(d) != 1 || is.na(d)) "" else format(as.Date(d), " (%b %Y)")

# ── Context builders ────────────────────────────────────────────────────────────
kpis_to_text <- function(kpis) {
  glue(
    "=== U.S. ECONOMIC SNAPSHOT ({format(Sys.Date(),'%B %d, %Y')}) ===\n\n",
    "LABOR MARKET\n",
    "  Unemployment Rate:          {fmt_pct(kpis$unemp_rate)}\n",
    "  Nonfarm Payrolls (MoM chg): {fmt_num(kpis$payrolls_chg, 0, 'K jobs')}\n",
    "  Job Openings:               {fmt_num(kpis$job_openings, 0, 'K')}\n",
    "  Avg Hourly Earnings (YoY):  {fmt_pct(kpis$wage_yoy)}{.kpi_month(kpis$wage_yoy_asof)}\n\n",
    "INFLATION\n",
    "  CPI YoY:                    {fmt_pct(kpis$cpi_yoy)}{.kpi_month(kpis$cpi_yoy_asof)}\n",
    "  Core CPI YoY:               {fmt_pct(kpis$core_cpi_yoy)}{.kpi_month(kpis$core_cpi_yoy_asof)}\n",
    "  Core PCE YoY:               {fmt_pct(kpis$core_pce)}{.kpi_month(kpis$core_pce_asof)}\n",
    "  PPI YoY:                    {fmt_pct(kpis$ppi_yoy)}{.kpi_month(kpis$ppi_yoy_asof)}\n\n",
    "MONETARY POLICY\n",
    "  Fed Funds Rate:             {fmt_pct(kpis$fed_funds)}\n",
    "  10-Year Treasury:           {fmt_pct(kpis$t10yr)}\n",
    "  2-Year Treasury:            {fmt_pct(kpis$t2yr)}\n",
    "  10Y-2Y Spread:              {fmt_pct(kpis$t10y2y)}\n",
    "  30-Yr Mortgage Rate:        {fmt_pct(kpis$mortgage30)}\n\n",
    "HOUSING & CONSUMER\n",
    "  Housing Starts:             {fmt_num(kpis$housing_starts, 0, 'K ann.')}\n",
    "  Retail Sales YoY:           {fmt_pct(kpis$retail_yoy)}{.kpi_month(kpis$retail_yoy_asof)}\n",
    "  Consumer Sentiment:         {fmt_num(kpis$cons_sent)}\n",
    "  Industrial Production YoY:  {fmt_pct(kpis$indpro_yoy)}{.kpi_month(kpis$indpro_yoy_asof)}\n\n",
    "MARKETS & COMMODITIES\n",
    "  WTI Crude Oil:              {fmt_dollar(kpis$oil_price)}/bbl\n",
    "  Gold:                       {fmt_dollar(kpis$gold_price)}/oz\n",
    "  VIX:                        {fmt_num(kpis$vix)}\n",
    "  HY Credit Spread:           {fmt_pct(kpis$hy_spread)}\n",
    "  USD Index:                  {fmt_num(kpis$usd_idx)}\n"
  )
}

#' 1-year price return (%) from the close history: latest close vs the last
#' close on or before the same date a year earlier (within a week), else NA.
ret_1y <- function(mkt_returns, symbol) {
  if (is.null(mkt_returns) || !all(c("symbol", "date", "close") %in% names(mkt_returns))) return(NA_real_)
  d <- mkt_returns[mkt_returns$symbol == symbol & !is.na(mkt_returns$close), c("date", "close")]
  if (nrow(d) < 2) return(NA_real_)
  d <- d[order(d$date), ]
  last <- d[nrow(d), ]
  target <- as.Date(last$date) - 365
  prior <- d[as.Date(d$date) <= target, ]
  if (!nrow(prior) || target - as.Date(prior$date[nrow(prior)]) > 7) return(NA_real_)
  100 * (last$close / prior$close[nrow(prior)] - 1)
}

# SMH/XLK/XLU track the AI cycle (chips, tech, power); IWM vs SPY is a breadth/rates read.
market_returns_text <- function(mkt_returns,
                                tickers=c("SPY","QQQ","IWM","SMH","XLK","XLU","XLF","GLD","USO","TLT","HYG")) {
  if (is.null(mkt_returns)) return("")
  df <- mkt_returns %>% filter(symbol %in% tickers) %>%
    group_by(symbol) %>% slice_tail(n=1) %>% ungroup()
  if (nrow(df)==0) return("")
  lines <- map_chr(seq_len(nrow(df)), function(i) {
    r <- df[i,]
    nm <- if (!is.na(TICKER_LABELS[r$symbol])) TICKER_LABELS[r$symbol] else r$symbol
    r3 <- if ("ret_3m" %in% names(r)) paste0("  3M=", fmt_pct(r$ret_3m,1,TRUE)) else ""
    r1y <- paste0("  1Y=", fmt_pct(ret_1y(mkt_returns, r$symbol), 1, TRUE))
    glue("  {r$symbol} ({nm}): 1D={fmt_pct(r$ret_1d,1,TRUE)}  1M={fmt_pct(r$ret_1m,1,TRUE)}{r3}{r1y}")
  })
  paste0("MARKET RETURNS\n", paste(lines, collapse="\n"))
}

#' The app's own model outputs (growth outlook, recession model, forecasts)
#' as text, so the LLM quotes these numbers instead of inventing its own.
model_outputs_to_text <- function(outlook = NULL, forecasts = NULL) {
  out <- "=== APP MODEL OUTPUTS (AUTHORITATIVE — quote these, do not re-derive) ===\n"

  if (is.null(outlook) || isTRUE(outlook$.error)) {
    out <- paste0(out, "Growth outlook / recession model: unavailable.\n")
  } else {
    gf <- outlook$gdp_forecast
    rp <- outlook$recession_prob
    out <- paste0(out,
      "GROWTH OUTLOOK (6-12 month)\n",
      sprintf("  Regime:                     %s\n", outlook$regime$label %||% "N/A"),
      sprintf("  Composite score:            %.1f / 10\n", outlook$score %||% NA),
      if (!is.null(gf)) sprintf(
        "  Next-qtr GDP forecast:      %+.1f%% annualized (%s %.1f%% to %.1f%%; recession drag %+.1fpp; %s)\n",
        gf$point,
        if (identical(gf$method, "fitted")) "80% band" else "rough band",
        gf$ci_lo, gf$ci_hi, gf$rec_drag,
        if (identical(gf$method, "fitted")) sprintf(
          "regression on score, OOS RMSE %.1fpp", gf$mapping$rmse_oos)
        else "uncalibrated anchors") else "",
      if (!is.null(outlook$gdpnow)) sprintf(
        "  Atlanta Fed GDPNow (%s):   %+.1f%% annualized (updated %s) — external nowcast, more accurate than the model for the current quarter\n",
        outlook$gdpnow$quarter, outlook$gdpnow$value,
        format(outlook$gdpnow$updated, "%Y-%m-%d")) else ""
    )
    comps <- outlook$components
    if (length(comps)) {
      out <- paste0(out, "  Component scores (-1 contractionary … +1 expansionary):\n",
        paste0(vapply(comps, function(c)
          sprintf("    %-22s %+.2f  (%s)", c$label, c$score, c$detail %||% ""),
          character(1)), collapse = "\n"), "\n")
    }

    if (!is.null(rp)) {
      out <- paste0(out, "\nRECESSION PROBABILITY MODEL (12-month horizon)\n",
        sprintf("  Probability:                %.0f%% (%s)\n", rp$prob, rp$tier$label %||% ""),
        sprintf("  Model:                      %s\n", rp$model_type %||% "N/A"),
        if (!is.null(rp$month))
          sprintf("  Data month / trained on:    %s / %d months through %s (base rate %.0f%%)\n",
                  format(rp$month, "%b %Y"), rp$n_train, format(rp$train_end, "%b %Y"),
                  rp$base_rate * 100) else ""
      )
      fc <- rp$factor_contributions
      if (!is.null(fc) && nrow(fc) > 0) {
        fc <- head(fc[!fc$data_missing & abs(fc$contribution) > 0.01, ], 10)
        if (nrow(fc) > 0) out <- paste0(out,
          "  Top factor contributions (+ raises risk, - lowers):\n",
          paste(sprintf("    %-32s %+.2f", fc$name, fc$contribution), collapse = "\n"), "\n")
      }
    }
  }

  if (is.null(forecasts)) {
    out <- paste0(out, "\nINDICATOR FORECASTS: not yet run this session ",
                  "(user has not opened the Forecasting tab).\n")
  } else {
    tbl <- tryCatch(forecast_summary_table(forecasts), error = function(e) NULL)
    if (!is.null(tbl) && nrow(tbl) > 0) {
      out <- paste0(out, "\nINDICATOR FORECASTS (Prophet/ARIMA/ETS ensemble or BVAR)\n",
        paste(sprintf("  %-28s current %s (as of %s) → 6M %s %s → end-horizon %s (%s)",
                      tbl$Indicator, tbl$Current, tbl$`As of`, tbl$`6M Fcst`, tbl$`6M CI`,
                      tbl$`18M Fcst`, tbl$Unit), collapse = "\n"), "\n")
    }
  }
  out
}

MODEL_GROUNDING_RULES <- paste0(
  "When discussing recession odds, GDP growth, or indicator paths, use the APP MODEL OUTPUTS ",
  "as the authoritative figures and cite them. Do not produce a different recession probability ",
  "or derive one by summing scenario weights. If you disagree with a model output, say so ",
  "explicitly and explain why, but still state the model's number first."
)

# ── Insight generators ─────────────────────────────────────────────────────────
FORECASTING_RULES <- paste0(
  "Your job is to anticipate, not to recap. Spend most words on what happens next and why. ",
  "Lean on the FORWARD-LOOKING SIGNALS (market-implied expectations and leading indicators) and ",
  "the UPCOMING CATALYSTS calendar; treat backward-looking KPIs as the starting point only. ",
  "Every forward claim needs a mechanism (A → B → C, with rough lags) and a signpost that would ",
  "confirm or refute it. Where markets already price something (breakevens, the 2Y-vs-Fed-Funds ",
  "gap), say whether you agree and why. Treat the AI investment cycle as a macro driver: its ",
  "support to business investment and GDP, power demand, chip supply, equity concentration, and ",
  "white-collar labor displacement, and what would end it. Rising NEWS THEMES matter more than ",
  "any single headline. Calendar GDP entries name the quarter they report; do not treat a revision ",
  "of an earlier quarter as the current quarter. Use only figures present in the data; never invent ",
  "a number or a date."
)

#' The AI Insights prompt. `forward_ctx` = load_forward_context() result;
#' `prev_calls` = previous_llm_calls() result (graded when present).
build_insights_prompt <- function(kpis, mkt_returns = NULL, news_df = NULL, outlook = NULL,
                                  forecasts = NULL, forward_ctx = NULL, prev_calls = NULL,
                                  today = Sys.Date()) {
  prev_block <- if (!is.null(prev_calls)) paste0(
    "\n=== YOUR PREVIOUS CALLS (made ", format(prev_calls$made_at, "%Y-%m-%d"), ") ===\n",
    prev_calls$calls, "\n") else ""
  review_section <- if (!is.null(prev_calls)) paste0(
    "## Previous Calls Review\n",
    "Grade each previous call against the current data: on track / off track / too early, ",
    "one line each with the evidence. Say what the misses teach about the current view.\n\n") else ""

  paste0(
    kpis_to_text(kpis), "\n", market_returns_text(mkt_returns), "\n\n",
    forward_context_text(forward_ctx, today), "\n",
    model_outputs_to_text(outlook, forecasts), "\n",
    "RECENT NEWS (macro + tech/AI):\n", headlines_for_llm(news_df, n = 24), "\n",
    prev_block,
    "\n---\nWrite a forward-looking outlook in EXACTLY these sections:\n\n",
    "## Bottom Line\n",
    "3 sentences: where the economy is heading over the next 6-12 months, the single biggest ",
    "swing factor, and the one data point that would change your view.\n\n",
    "## What Markets Price vs. Our View\n",
    "Inflation expectations, the implied policy path, credit and financial conditions. ",
    "Agree or disagree with each, with reasons.\n\n",
    "## Leading Signals\n",
    "What claims, the Sahm rule, permits, financial conditions and bank lending standards say about ",
    "the next 1-2 quarters. Flag any that are turning.\n\n",
    "## Themes Gaining Momentum\n",
    "The 3-4 storylines from NEWS THEMES that matter most (always assess the AI/tech investment cycle), ",
    "as bullets (not a table), each in the form:\n",
    '- **<Theme>** (<n> articles, <rising/steady/fading>): <transmission to growth, inflation, jobs or markets>; ',
    '<second-order effects>; <timing>. [chart: {"series": "<id>"}]\n',
    "The chart tag names the ONE series from CHART SERIES that best shows whether the transmission is ",
    'actually happening (e.g. elec_cpi_yoy for power demand, smh for the chip cycle, oil for energy). ',
    "Say in the bullet what that series currently shows.\n\n",
    "## Catalyst Calendar\n",
    "The 4-6 most important upcoming events from UPCOMING CATALYSTS, with dates. For each: the ",
    "result that would confirm the base case vs. challenge it, as numeric thresholds.\n\n",
    "## Scenarios (6-12 months)\n",
    "**Bear (X%) / Base (Y%) / Bull (Z%)**: for each, the trigger, the path, and the leading ",
    "indicator that would move first. Choose X/Y/Z yourself (summing to 100%) so the Bear weight ",
    "is consistent with the model's recession probability and the Base case with its GDP forecast.\n\n",
    review_section,
    "## Calls to Track\n",
    "3-5 falsifiable calls as bullets, each in the form:\n",
    '- **By <YYYY-MM-DD>:** <metric> <above/below/between> <value> (now <current value>) — confidence ',
    '<medium/high>. [chart: {"series": "<id>", "threshold": <value>, "by": "<YYYY-MM-DD>"}]\n',
    "Rules: a call is what you EXPECT to happen ",
    "under your base case, not a bear/bull trigger. Confidence is the chance you are right (high > 70%, ",
    "medium 50-70%); drop anything you think is less likely than not. Size thresholds to what the metric can ",
    "plausibly move by that date (e.g. YoY CPI rarely moves more than 0.3pp in a month). Use metrics present ",
    "in the data so they can be checked later, and prefer dates just after a release in UPCOMING CATALYSTS.\n",
    "Every call bullet MUST end with its chart tag; the app draws it from real data as evidence. ",
    "Thresholds are in the series' units (e.g. K for claims). When a relationship is the better evidence, ",
    'use a scatter instead: [chart: {"type": "scatter", "x": "init_claims", "y": "unemp_rate"}]\n',
    "Use only ids from CHART SERIES, plain ASCII hyphens in dates, and no other numbers in tags.\n\n",
    chart_catalog_text(), "\n"
  )
}

generate_insights <- function(kpis, fred_data, mkt_returns=NULL,
                               news_df=NULL, outlook=NULL, forecasts=NULL,
                               forward_ctx=NULL, prev_calls=NULL,
                               provider=NULL, model=NULL) {
  user_msg <- build_insights_prompt(kpis, mkt_returns, news_df, outlook, forecasts,
                                    forward_ctx, prev_calls)
  call_llm(
    messages   = list(list(role="user", content=user_msg)),
    provider   = provider, model=model,
    max_tokens = 4000, temperature=0.45,
    system     = paste0(
      "You are the chief economist and forward strategist at a global investment bank. ",
      "Deep expertise in monetary policy, labor economics, financial markets, technology investment ",
      "cycles, and geopolitical risk. Be direct, quantitative, and take clear positions. ",
      "Use markdown formatting. ", FORECASTING_RULES, " ", MODEL_GROUNDING_RULES
    )
  )
}

generate_sector_insight <- function(sector, kpi_text, news_headlines=NULL,
                                     provider=NULL, model=NULL) {
  news_ctx <- if (!is.null(news_headlines)) paste0("News:\n", news_headlines) else ""
  msg <- glue(
    "Analyze U.S. {sector} sector.\n\nIndicators:\n{kpi_text}\n\n{news_ctx}\n\n",
    "Provide: (1) current conditions, (2) key drivers, (3) 6-month outlook. ",
    "Under 300 words. Be specific and data-driven."
  )
  call_llm(messages=list(list(role="user", content=msg)),
            provider=provider, model=model, max_tokens=600, temperature=0.6,
            system="You are a concise sector economist. Use numbers. No filler.")
}

#' Multi-turn Q&A. `history` is a list of prior list(role, content) turns;
#' `insights` is the generated AI Insights text (optional) so follow-ups can
#' reference it. Data context goes in the system prompt so it isn't repeated
#' every turn.
economic_chat <- function(question, kpis, news_df=NULL, history=list(),
                          insights=NULL, outlook=NULL, forecasts=NULL, forward_ctx=NULL,
                          provider=NULL, model=NULL, max_turns=10) {
  ctx      <- paste0(kpis_to_text(kpis), "\n\n", forward_context_text(forward_ctx), "\n",
                     model_outputs_to_text(outlook, forecasts))
  news_ctx <- if (!is.null(news_df)) headlines_for_llm(news_df, 12) else "N/A"
  ins_ctx  <- if (!is.null(insights) && nzchar(insights))
    paste0("\n\n=== PRIOR AI ECONOMIC ANALYSIS SHOWN TO USER ===\n", insights) else ""

  system <- paste0(
    "You are a precise macroeconomic analyst. Give direct, data-grounded answers. ",
    "Answer concisely using the data below. The user may ask follow-up questions; ",
    "use the conversation so far for context. When the question is about the future, answer with ",
    "a clear call, the mechanism, the signpost to watch (with its date from UPCOMING CATALYSTS when ",
    "one fits), and what would change your mind. ", MODEL_GROUNDING_RULES, "\n\n",
    ctx, "\n\nNews:\n", news_ctx, ins_ctx
  )

  # Keep only the most recent turns to bound prompt size
  if (length(history) > 2 * max_turns)
    history <- tail(history, 2 * max_turns)

  msgs <- c(history, list(list(role="user", content=question)))
  call_llm(messages=msgs, provider=provider, model=model,
            max_tokens=1800, temperature=0.6, system=system)
}

available_models <- function(provider=NULL) {
  p <- if (!is.null(provider)) provider else ACTIVE_PROVIDER
  if (is.null(p)) return(character(0))
  LLM_PROVIDERS[[p]]$models
}
