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

  res <- llm_request(pname, model, full_msgs, max_tokens, temperature)
  if (res$ok || !res$transient) return(res$text)

  # Transient failure (429 / 5xx) persisted through retries — fall back to the
  # provider's other models, then to any other provider with a key configured.
  for (cand in llm_fallbacks(pname, model)) {
    message(sprintf("[LLM] %s/%s unavailable — falling back to %s/%s",
                    pname, model, cand$provider, cand$model))
    alt <- llm_request(cand$provider, cand$model, full_msgs, max_tokens, temperature, tries = 2L)
    if (alt$ok) {
      return(paste0(alt$text, "

*Note: ", cfg$display_name, " (", model,
                    ") was temporarily unavailable; answered by ",
                    LLM_PROVIDERS[[cand$provider]]$display_name, " (", cand$model, ").*"))
    }
  }
  res$text
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

# ── Context builders ────────────────────────────────────────────────────────────
kpis_to_text <- function(kpis) {
  glue(
    "=== U.S. ECONOMIC SNAPSHOT ({format(Sys.Date(),'%B %d, %Y')}) ===\n\n",
    "LABOR MARKET\n",
    "  Unemployment Rate:          {fmt_pct(kpis$unemp_rate)}\n",
    "  Nonfarm Payrolls (MoM chg): {fmt_num(kpis$payrolls_chg, 0, 'K jobs')}\n",
    "  Job Openings:               {fmt_num(kpis$job_openings, 0, 'K')}\n",
    "  Avg Hourly Earnings (YoY):  {fmt_pct(kpis$wage_yoy)}\n\n",
    "INFLATION\n",
    "  CPI YoY:                    {fmt_pct(kpis$cpi_yoy)}\n",
    "  Core CPI YoY:               {fmt_pct(kpis$core_cpi_yoy)}\n",
    "  Core PCE:                   {fmt_pct(kpis$core_pce)}\n",
    "  PPI YoY:                    {fmt_pct(kpis$ppi_yoy)}\n\n",
    "MONETARY POLICY\n",
    "  Fed Funds Rate:             {fmt_pct(kpis$fed_funds)}\n",
    "  10-Year Treasury:           {fmt_pct(kpis$t10yr)}\n",
    "  2-Year Treasury:            {fmt_pct(kpis$t2yr)}\n",
    "  10Y-2Y Spread:              {fmt_pct(kpis$t10y2y)}\n",
    "  30-Yr Mortgage Rate:        {fmt_pct(kpis$mortgage30)}\n\n",
    "HOUSING & CONSUMER\n",
    "  Housing Starts:             {fmt_num(kpis$housing_starts, 0, 'K ann.')}\n",
    "  Retail Sales YoY:           {fmt_pct(kpis$retail_yoy)}\n",
    "  Consumer Sentiment:         {fmt_num(kpis$cons_sent)}\n",
    "  Industrial Production YoY:  {fmt_pct(kpis$indpro_yoy)}\n\n",
    "MARKETS & COMMODITIES\n",
    "  WTI Crude Oil:              {fmt_dollar(kpis$oil_price)}/bbl\n",
    "  Gold:                       {fmt_dollar(kpis$gold_price)}/oz\n",
    "  VIX:                        {fmt_num(kpis$vix)}\n",
    "  HY Credit Spread:           {fmt_pct(kpis$hy_spread)}\n",
    "  USD Index:                  {fmt_num(kpis$usd_idx)}\n"
  )
}

market_returns_text <- function(mkt_returns, tickers=c("SPY","QQQ","GLD","USO","TLT","HYG")) {
  if (is.null(mkt_returns)) return("")
  df <- mkt_returns %>% filter(symbol %in% tickers) %>%
    group_by(symbol) %>% slice_tail(n=1) %>% ungroup()
  if (nrow(df)==0) return("")
  lines <- map_chr(seq_len(nrow(df)), function(i) {
    r <- df[i,]
    nm <- if (!is.na(TICKER_LABELS[r$symbol])) TICKER_LABELS[r$symbol] else r$symbol
    glue("  {r$symbol} ({nm}): 1D={fmt_pct(r$ret_1d,1,TRUE)}  1M={fmt_pct(r$ret_1m,1,TRUE)}")
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
generate_insights <- function(kpis, fred_data, mkt_returns=NULL,
                               news_df=NULL, outlook=NULL, forecasts=NULL,
                               provider=NULL, model=NULL) {
  eco_ctx   <- kpis_to_text(kpis)
  mkt_ctx   <- market_returns_text(mkt_returns)
  news_ctx  <- headlines_for_llm(news_df, n=20)
  model_ctx <- model_outputs_to_text(outlook, forecasts)

  user_msg <- glue(
    "{eco_ctx}\n{mkt_ctx}\n\n{model_ctx}\n\n",
    "RECENT ECONOMIC NEWS:\n{news_ctx}\n\n---\n",
    "Provide a structured economic analysis:\n\n",
    "## Current Economic Conditions\n",
    "Overall health assessment with specific data references.\n\n",
    "## Key Relationships & Dynamics\n",
    "Most important causal chains (Fed policy, inflation, labor, housing, markets).\n\n",
    "## News Impact Assessment\n",
    "Top 5 headlines: economic impact, mechanisms, duration.\n\n",
    "## Risk Matrix\n",
    "3 downside risks (probability + severity) and 2 upside scenarios.\n\n",
    "## 6-12 Month Scenarios\n",
    "**Bear (X%):** ... **Base (Y%):** ... **Bull (Z%):** ... ",
    "Choose X/Y/Z yourself (summing to 100%) so the Bear weight is consistent with the ",
    "model's recession probability and the Base case with its GDP forecast.\n\n",
    "## Executive Summary\n",
    "3 crisp sentences for a portfolio manager.\n\nUse data throughout."
  )

  call_llm(
    messages   = list(list(role="user", content=user_msg)),
    provider   = provider, model=model,
    max_tokens = 3500, temperature=0.6,
    system     = paste0(
      "You are a senior macroeconomic strategist at a global investment bank. ",
      "Deep expertise in monetary policy, labor economics, financial markets, and geopolitical risk. ",
      "Be direct, quantitative, and take clear analytical positions. Use markdown formatting. ",
      MODEL_GROUNDING_RULES
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
                          insights=NULL, outlook=NULL, forecasts=NULL,
                          provider=NULL, model=NULL, max_turns=10) {
  ctx      <- paste0(kpis_to_text(kpis), "\n\n", model_outputs_to_text(outlook, forecasts))
  news_ctx <- if (!is.null(news_df)) headlines_for_llm(news_df, 10) else "N/A"
  ins_ctx  <- if (!is.null(insights) && nzchar(insights))
    paste0("\n\n=== PRIOR AI ECONOMIC ANALYSIS SHOWN TO USER ===\n", insights) else ""

  system <- paste0(
    "You are a precise macroeconomic analyst. Give direct, data-grounded answers. ",
    "Answer concisely using the data below. The user may ask follow-up questions; ",
    "use the conversation so far for context. ", MODEL_GROUNDING_RULES, "\n\n",
    ctx, "\n\nNews:\n", news_ctx, ins_ctx
  )

  # Keep only the most recent turns to bound prompt size
  if (length(history) > 2 * max_turns)
    history <- tail(history, 2 * max_turns)

  msgs <- c(history, list(list(role="user", content=question)))
  call_llm(messages=msgs, provider=provider, model=model,
            max_tokens=800, temperature=0.7, system=system)
}

available_models <- function(provider=NULL) {
  p <- if (!is.null(provider)) provider else ACTIVE_PROVIDER
  if (is.null(p)) return(character(0))
  LLM_PROVIDERS[[p]]$models
}
