# ─────────────────────────────────────────────────────────────────────────────
# R/llm_insights.R — Provider-agnostic LLM integration (OpenAI-compatible)
# ─────────────────────────────────────────────────────────────────────────────

#' Core LLM call — works with any OpenAI-compatible endpoint
call_llm <- function(messages, provider=NULL, model=NULL,
                      max_tokens=2500, temperature=0.65, system=NULL) {
  pname <- if (!is.null(provider)) provider else ACTIVE_PROVIDER
  if (is.null(pname)) {
    return(paste0("LLM provider not configured.\n",
                  "Set GROQ_API_KEY, OPENROUTER_API_KEY, or TOGETHER_API_KEY in .Renviron."))
  }

  cfg     <- LLM_PROVIDERS[[pname]]
  api_key <- Sys.getenv(cfg$key_env)
  if (nchar(api_key) == 0)
    return(glue("{cfg$display_name} API key ({cfg$key_env}) is not set."))

  if (is.null(model)) model <- cfg$default_model

  full_msgs <- if (!is.null(system)) {
    c(list(list(role="system", content=system)), messages)
  } else messages

  tryCatch({
    resp <- request(paste0(cfg$base_url, "/chat/completions")) %>%
      req_headers("Authorization"=paste("Bearer", api_key),
                  "Content-Type"="application/json") %>%
      req_body_json(list(model=model, messages=full_msgs,
                         max_tokens=max_tokens, temperature=temperature)) %>%
      req_timeout(90) %>%
      req_retry(max_tries=2, backoff=~2) %>%
      req_perform()

    result <- resp %>% resp_body_json()
    if (!is.null(result$choices) && length(result$choices)>0) {
      result$choices[[1]]$message$content
    } else if (!is.null(result$error)) {
      paste0("API Error: ", result$error$message)
    } else {
      paste0("Unexpected response from ", cfg$display_name)
    }
  }, error=function(e) paste0("LLM call failed: ", conditionMessage(e)))
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

# ── Insight generators ─────────────────────────────────────────────────────────
generate_insights <- function(kpis, fred_data, mkt_returns=NULL,
                               news_df=NULL, provider=NULL, model=NULL) {
  eco_ctx  <- kpis_to_text(kpis)
  mkt_ctx  <- market_returns_text(mkt_returns)
  news_ctx <- headlines_for_llm(news_df, n=20)

  user_msg <- glue(
    "{eco_ctx}\n{mkt_ctx}\n\n",
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
    "**Bear (25%):** ... **Base (55%):** ... **Bull (20%):** ...\n\n",
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
      "Be direct, quantitative, and take clear analytical positions. Use markdown formatting."
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

economic_chat <- function(question, kpis, news_df=NULL, provider=NULL, model=NULL) {
  ctx      <- kpis_to_text(kpis)
  news_ctx <- if (!is.null(news_df)) headlines_for_llm(news_df, 10) else "N/A"
  msg      <- glue("{ctx}\n\nNews:\n{news_ctx}\n\nQuestion: {question}\n\n",
                   "Answer concisely using the data above.")
  call_llm(messages=list(list(role="user", content=msg)),
            provider=provider, model=model, max_tokens=800, temperature=0.7,
            system="You are a precise macroeconomic analyst. Give direct, data-grounded answers.")
}

available_models <- function(provider=NULL) {
  p <- if (!is.null(provider)) provider else ACTIVE_PROVIDER
  if (is.null(p)) return(character(0))
  LLM_PROVIDERS[[p]]$models
}
