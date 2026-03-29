# ─────────────────────────────────────────────────────────────────────────────
# R/data_news.R — Multi-source news: NewsAPI → MediaStack → GDELT
# No filesystem cache — always fetches fresh on load (works on any server)
# ─────────────────────────────────────────────────────────────────────────────

# ── Expanded query covers supply chain + geopolitical triggers ────────────────
NEWS_QUERY <- paste(
  # Monetary policy
  '"federal reserve" OR "FOMC" OR "rate cut" OR "rate hike" OR "interest rates" OR',
  '"jerome powell" OR "fed funds" OR "quantitative tightening" OR',
  # Inflation / prices
  'inflation OR "cost of living" OR "affordability" OR "unaffordable" OR',
  # Labor / employment
  'unemployment OR "jobs report" OR "labor market" OR "job ladder" OR',
  '"payrolls" OR "job openings" OR "wage growth" OR "healthcare jobs" OR',
  # Housing
  '"housing market" OR "housing starts" OR "mortgage demand" OR',
  '"housing supply" OR "home prices" OR "rent" OR "housing bills" OR',
  # Trade / supply chain / geopolitics
  '"trade war" OR tariff OR "supply chain" OR "strait of hormuz" OR',
  '"suez canal" OR "red sea" OR sanctions OR "shipping rates" OR',
  # Energy / commodities
  '"oil price" OR OPEC OR "natural gas" OR "energy prices" OR',
  # Consumer / spending
  '"consumer spending" OR "retail sales" OR "consumer confidence" OR',
  # Financial markets
  '"stock market" OR "earnings" OR "credit spreads" OR "yield curve" OR',
  # Macro
  'recession OR GDP OR "economic growth" OR "fiscal policy"',
  collapse = " "
)

# Keyword weights for relevance scoring
# Higher weight = more likely to surface in feed and LLM context
KW_WEIGHTS <- c(
  # ── Monetary policy (highest signal) ─────────────────────────────────────────
  "federal reserve"       = 5,
  "fomc"                  = 5,
  "rate cut"              = 5,
  "rate hike"             = 5,
  "jerome powell"         = 4,
  "fed funds"             = 4,
  "interest rate"         = 4,
  "quantitative tightening"= 4,
  "dot plot"              = 4,

  # ── Inflation / prices ────────────────────────────────────────────────────────
  "inflation"             = 4,
  "cost of living"        = 4,
  "affordability"         = 4,
  "unaffordable"          = 3,
  "price pressure"        = 3,
  "cpi"                   = 3,
  "pce"                   = 3,

  # ── Labor / employment ────────────────────────────────────────────────────────
  "unemployment"          = 4,
  "jobs report"           = 4,
  "nonfarm payrolls"      = 4,
  "payrolls"              = 3,
  "labor market"          = 4,
  "job openings"          = 3,
  "job ladder"            = 3,
  "wage growth"           = 3,
  "wages"                 = 2,
  "healthcare jobs"       = 3,
  "labor force"           = 3,
  "workers"               = 1,

  # ── Housing ───────────────────────────────────────────────────────────────────
  "housing market"        = 4,
  "housing starts"        = 4,
  "mortgage demand"       = 4,
  "mortgage rate"         = 4,
  "mortgage"              = 3,
  "housing supply"        = 4,
  "home prices"           = 3,
  "housing bills"         = 3,
  "rent"                  = 2,
  "vacancy"               = 2,
  "affordability"         = 3,   # also in housing context

  # ── Supply chain / geopolitics ────────────────────────────────────────────────
  "strait of hormuz"      = 5,
  "suez canal"            = 5,
  "red sea"               = 5,
  "supply chain"          = 4,
  "port strike"           = 4,
  "shipping rates"        = 4,
  "freight"               = 3,
  "container"             = 3,
  "sanctions"             = 4,
  "tariff"                = 4,
  "trade war"             = 4,
  "geopolitical"          = 3,
  "conflict"              = 2,
  "embargo"               = 4,

  # ── Energy / commodities ──────────────────────────────────────────────────────
  "oil price"             = 4,
  "opec"                  = 4,
  "crude"                 = 3,
  "natural gas"           = 3,
  "energy prices"         = 3,
  "gasoline"              = 3,
  "diesel"                = 2,

  # ── Consumer / spending ───────────────────────────────────────────────────────
  "consumer spending"     = 3,
  "retail sales"          = 3,
  "consumer confidence"   = 3,
  "consumer sentiment"    = 3,
  "spending"              = 1,

  # ── Financial markets ─────────────────────────────────────────────────────────
  "yield curve"           = 3,
  "credit spreads"        = 3,
  "treasury"              = 2,
  "earnings"              = 2,
  "stock market"          = 2,
  "vix"                   = 2,
  "dollar"                = 2,

  # ── Macro / fiscal ────────────────────────────────────────────────────────────
  "recession"             = 4,
  "gdp"                   = 3,
  "economic growth"       = 3,
  "debt ceiling"          = 3,
  "fiscal policy"         = 3,
  "deficit"               = 2,
  "manufacturing"         = 2,
  "industrial"            = 2,

  # ── General (low weight) ──────────────────────────────────────────────────────
  "economy"               = 1,
  "market"                = 1,
  "trade"                 = 1,
  "growth"                = 1,
  "jobs"                  = 1
)

# Supply chain causal chain descriptions (used in LLM context + synopsis)
SUPPLY_CHAIN_CHAINS <- list(
  "strait of hormuz" = "~20% of global oil + LNG transits here → oil/gas price spike → diesel/petrol → trucking costs → retail prices, CPI energy component",
  "suez canal"       = "~12% of global trade + major container route → shipping delays → inventory shortages → retail price pressure, PPI input costs",
  "red sea"          = "Houthi attacks re-routing ships around Cape of Good Hope → +10-14 day transit → shipping rates → import prices → core goods CPI",
  "port strike"      = "US port disruptions → supply shock → retail inventory drawdown → price pressure on goods, GDP drag via net exports",
  "supply chain"     = "Disruption → input cost pass-through → PPI → CPI goods with 1-3 month lag",
  "sanctions"        = "Export restrictions on targeted country → commodity supply shock → price spike in affected market (oil, wheat, metals)",
  "oil price"        = "Oil → diesel/petrol → trucking/shipping costs → food + retail prices → CPI headline; also airline costs, plastics, chemicals",
  "freight"          = "Shipping rates (Baltic Dry, Freightos) → import costs → retail goods prices → core CPI with ~2-month lag",
  "tariff"           = "Import duties → domestic producer price relief, consumer price increase → CPI goods, retaliation risk to exports",
  "trade war"        = "Broad tariffs → supply chain restructuring → near-term inflation, medium-term efficiency losses → GDP drag"
)

# ── Score articles ─────────────────────────────────────────────────────────────
score_relevance <- function(titles, descs) {
  text   <- tolower(paste(titles, descs))
  scores <- numeric(length(text))
  for (kw in names(KW_WEIGHTS)) {
    scores <- scores + stringr::str_count(text, stringr::fixed(kw)) * KW_WEIGHTS[[kw]]
  }
  scores
}

# Detect which supply chain chains are triggered
detect_chains <- function(title, desc) {
  text    <- tolower(paste(title, desc))
  matches <- names(SUPPLY_CHAIN_CHAINS)[
    vapply(names(SUPPLY_CHAIN_CHAINS), function(k) grepl(k, text, fixed=TRUE), logical(1))
  ]
  if (length(matches) == 0) return(NA_character_)
  paste(matches, collapse="; ")
}

# ── Source 1: NewsAPI ─────────────────────────────────────────────────────────
# Free dev plan = localhost only. Paid plan required for server deployments.
# Short query avoids HTTP 400 (URL length limit).
NEWSAPI_SHORT_QUERY <- paste(
  '"federal reserve" OR inflation OR recession OR unemployment OR',
  '"trade war" OR tariff OR "housing market" OR "oil price" OR',
  '"interest rates" OR "supply chain" OR "jobs report" OR earnings',
  collapse = " "
)

# Domains to exclude from news results — highly partisan sources that
# introduce political framing rather than economic analysis.
# Focus: centrist (Reuters, AP, WSJ, Bloomberg) and centre-left (NYT, WaPo,
# FT, Economist) outlets known for factual economic reporting.
NEWS_EXCLUDED_DOMAINS <- paste(
  c("breitbart.com", "dailywire.com", "newsmax.com", "thefederalist.com",
    "washingtonexaminer.com", "nypost.com", "oann.com", "westernjournal.com",
    "townhall.com", "redstate.com", "dailycaller.com", "theblaze.com",
    "pjmedia.com", "zerohedge.com", "naturalnews.com", "infowars.com"),
  collapse = ","
)

# Source pattern filter applied to ALL sources (NewsAPI, MediaStack, GDELT)
.is_excluded_source <- function(source_names) {
  excluded_patterns <- c(
    "breitbart", "daily wire", "newsmax", "federalist", "washington examiner",
    "new york post", "one america", "western journal", "townhall", "red state",
    "daily caller", "the blaze", "pj media", "zero hedge", "natural news",
    "infowars", "epoch times", "gateway pundit", "american thinker",
    "daily mail"  # known for sensationalism
  )
  tolower(source_names) %in% tolower(excluded_patterns) |
    sapply(tolower(source_names), function(s) {
      any(sapply(excluded_patterns, function(p) grepl(p, s, fixed=TRUE)))
    })
}

fetch_newsapi <- function(days_back = 4, max_articles = 100) {
  key <- Sys.getenv("NEWS_API_KEY")
  if (nchar(key) == 0) return(list(data=NULL, error="key_not_set"))

  tryCatch({
    resp <- httr2::request("https://newsapi.org/v2/everything") %>%
      httr2::req_url_query(
        q              = NEWSAPI_SHORT_QUERY,
        excludeDomains = NEWS_EXCLUDED_DOMAINS,   # exclude partisan sources
        from           = format(Sys.Date() - days_back, "%Y-%m-%d"),
        sortBy         = "relevancy",
        language       = "en",
        pageSize       = as.character(min(max_articles, 100)),
        apiKey         = key
      ) %>%
      httr2::req_timeout(25) %>%
      httr2::req_perform()

    raw <- resp %>% httr2::resp_body_json()
    if ((raw$status %||% "error") != "ok")
      return(list(data=NULL, error=raw$message %||% "unknown"))

    df <- .parse_newsapi_articles(raw$articles, days_back)
    list(data=df, error=NULL)
  }, error=function(e) list(data=NULL, error=e$message))
}

.parse_newsapi_articles <- function(arts, days_back) {
  now_utc <- as.POSIXct(Sys.time(), tz="UTC")
  purrr::map_dfr(arts, function(a) {
    dplyr::tibble(
      title       = a$title       %||% "",
      description = a$description %||% "",
      source_name = a$source$name %||% "Unknown",
      url         = a$url         %||% "",
      published   = suppressWarnings(
        as.POSIXct(a$publishedAt %||% "", format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")),
      content     = a$content     %||% ""
    )
  }) %>%
    dplyr::filter(nchar(title) > 5, title != "[Removed]") %>%
    .add_news_meta(now_utc, days_back)
}

# ── Source 2: MediaStack ──────────────────────────────────────────────────────
# Free tier: 500 req/month, works from any server, no localhost restriction.
# Get key: https://mediastack.com/signup/free
# Free plan uses HTTP only (HTTPS requires paid). Keywords are comma-separated,
# max ~200 chars. Use `date` param for recency filter.
fetch_mediastack <- function(days_back = 4, max_articles = 100) {
  key <- Sys.getenv("MEDIASTACK_API_KEY")
  if (nchar(key) == 0) return(list(data=NULL, error="key_not_set"))

  # Comma-separated keywords (MediaStack format — NOT OR syntax)
  ms_keywords <- paste(
    c("economy","inflation","federal reserve","unemployment","housing",
      "mortgage","tariff","recession","oil price","interest rates"),
    collapse=","
  )

  # Date range: MediaStack uses YYYY-MM-DD,YYYY-MM-DD
  date_from  <- format(Sys.Date() - days_back, "%Y-%m-%d")
  date_range <- paste0(date_from, ",", format(Sys.Date(), "%Y-%m-%d"))

  tryCatch({
    # MediaStack free plan: HTTP only (not HTTPS)
    resp <- httr2::request("http://api.mediastack.com/v1/news") %>%
      httr2::req_url_query(
        access_key = key,
        keywords   = ms_keywords,
        languages  = "en",
        date       = date_range,
        sort       = "published_desc",
        limit      = as.character(min(max_articles, 100))
      ) %>%
      httr2::req_timeout(25) %>%
      httr2::req_perform()

    raw  <- resp %>% httr2::resp_body_json()

    # Check for API-level error
    if (!is.null(raw$error)) {
      err <- raw$error$message %||% raw$error$code %||% "MediaStack API error"
      return(list(data=NULL, error=as.character(err)))
    }

    arts <- raw$data
    if (is.null(arts) || length(arts) == 0)
      return(list(data=NULL, error="no_articles"))

    now_utc <- as.POSIXct(Sys.time(), tz="UTC")
    df <- purrr::map_dfr(arts, function(a) {
      # MediaStack published_at: "2024-03-25T12:34:00+0000"
      pub <- suppressWarnings(tryCatch(
        as.POSIXct(a$published_at %||% "", format="%Y-%m-%dT%H:%M:%S+0000", tz="UTC"),
        error=function(e) as.POSIXct(NA)
      ))
      dplyr::tibble(
        title       = a$title       %||% "",
        description = a$description %||% "",
        source_name = a$source      %||% "Unknown",
        url         = a$url         %||% "",
        published   = pub,
        content     = ""
      )
    }) %>%
      dplyr::filter(nchar(title) > 5) %>%
      .add_news_meta(now_utc, days_back)

    list(data=df, error=NULL)
  }, error=function(e) list(data=NULL, error=e$message))
}

# ── Source 3: GDELT ───────────────────────────────────────────────────────────
# No API key required. Uses simple keyword query — NOT boolean OR syntax.
# Rate limit: ~1 req/sec sustained. On 429 or HTML response, skip gracefully.
fetch_gdelt <- function(days_back = 4, max_articles = 50) {
  tryCatch({
    # Simple space-separated theme query — GDELT handles it well
    gdelt_q  <- "economy inflation unemployment housing interest rates tariff oil recession jobs"
    from_str <- format(Sys.Date() - days_back, "%Y%m%d%H%M%S")
    now_utc  <- as.POSIXct(Sys.time(), tz="UTC")

    resp <- httr2::request("https://api.gdeltproject.org/api/v2/doc/doc") %>%
      httr2::req_url_query(
        query         = gdelt_q,
        mode          = "artlist",
        maxrecords    = max_articles,
        startdatetime = from_str,
        sort          = "hybridrel",
        format        = "json"
      ) %>%
      httr2::req_timeout(30) %>%
      httr2::req_perform()

    # Check content type — rate limit returns HTML error page
    ct <- httr2::resp_content_type(resp)
    if (!grepl("json", ct, ignore.case=TRUE)) {
      return(list(data=NULL, error=paste0("Unexpected content type: ", ct,
                                           " (possibly rate-limited)")))
    }

    raw  <- resp %>% httr2::resp_body_json()
    arts <- raw$articles
    if (is.null(arts) || length(arts) == 0) return(list(data=NULL, error="no_articles"))

    df <- purrr::map_dfr(arts, function(a) {
      pub <- tryCatch(
        as.POSIXct(a$seendate %||% "", format="%Y%m%dT%H%M%SZ", tz="UTC"),
        error=function(e) as.POSIXct(NA)
      )
      dplyr::tibble(
        title       = a$title  %||% "",
        description = "",
        source_name = a$domain %||% "Unknown",
        url         = a$url    %||% "",
        published   = pub,
        content     = ""
      )
    }) %>%
      dplyr::filter(nchar(title) > 5) %>%
      .add_news_meta(now_utc, days_back)

    list(data=df, error=NULL)
  }, error=function(e) {
    message("[GDELT] ", e$message)
    list(data=NULL, error=e$message)
  })
}

# ── Shared post-processing ────────────────────────────────────────────────────
.add_news_meta <- function(df, now_utc, days_back) {
  df %>%
    dplyr::mutate(
      hours_ago       = as.numeric(difftime(now_utc, published, units="hours")),
      days_in_cache   = 1L,
      relevance_score = score_relevance(title, description),
      supply_chain    = mapply(detect_chains, title, description, SIMPLIFY=TRUE)
    ) %>%
    dplyr::filter(
      !is.na(published),
      hours_ago >= 0,
      hours_ago < days_back * 24,
      !.is_excluded_source(source_name)   # remove partisan sources
    ) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))
}

# ── Main fetch: try all sources in order ──────────────────────────────────────
fetch_news <- function(days_back = 4, max_articles = 80) {
  sources <- list(
    list(name="NewsAPI",    fn=fetch_newsapi,    args=list(days_back=days_back, max_articles=max_articles)),
    list(name="MediaStack", fn=fetch_mediastack, args=list(days_back=days_back, max_articles=max_articles)),
    list(name="GDELT",      fn=fetch_gdelt,      args=list(days_back=days_back, max_articles=max_articles))
  )

  for (src in sources) {
    result <- tryCatch(
      do.call(src$fn, src$args),
      error=function(e) list(data=NULL, error=e$message)
    )
    if (!is.null(result$data) && nrow(result$data) > 0) {
      message(sprintf("[News] %s: %d articles", src$name, nrow(result$data)))
      return(result$data)
    }
    if (!is.null(result$error) && result$error != "key_not_set") {
      message(sprintf("[News] %s failed: %s", src$name, result$error))
    }
  }

  message("[News] All sources returned no data")
  NULL
}

# Keep news_full_history as a no-op passthrough (cache removed — df IS the full result)
news_full_history <- function(news_df) news_df

# ── Context for LLM — always uses full 10-day history ─────────────────────────
headlines_for_llm <- function(news_df, n = 25) {
  if (is.null(news_df)) return("No recent headlines available.")
  # Use full 10-day history for LLM context (not just the display window)
  df <- news_full_history(news_df)
  if (nrow(df) == 0) return("No recent headlines available.")

  rows <- df %>% dplyr::slice_head(n = n) %>%
    dplyr::mutate(
      age_str = dplyr::case_when(
        hours_ago < 2  ~ "just now",
        hours_ago < 24 ~ paste0(round(hours_ago), "h ago"),
        TRUE           ~ paste0(round(hours_ago / 24), "d ago")
      )
    )

  lines <- vapply(seq_len(nrow(rows)), function(i) {
    r          <- rows[i, ]
    desc       <- if (!is.na(r$description) && nchar(r$description) > 0)
                    paste0(" — ", stringr::str_trunc(r$description, 110)) else ""
    cache_note <- if (!is.na(r$days_in_cache) && r$days_in_cache > 1)
                    sprintf(" [ongoing: %d days]", r$days_in_cache) else ""
    chain_note <- if (!is.na(r$supply_chain) && nchar(r$supply_chain) > 0)
                    sprintf("\n   SUPPLY CHAIN IMPACT: %s",
                            paste(sapply(strsplit(r$supply_chain, "; ")[[1]],
                                         function(k) SUPPLY_CHAIN_CHAINS[[k]] %||% k),
                                  collapse = " | "))
                  else ""
    paste0(i, ". [", r$source_name, " | ", r$age_str, cache_note, "] ",
           r$title, desc, chain_note)
  }, character(1))

  paste(lines, collapse = "\n")
}

# ── HTML renderer ─────────────────────────────────────────────────────────────
render_news_html <- function(news_df, n = 15) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    newsapi_set    <- nchar(Sys.getenv("NEWS_API_KEY")) > 0
    mediastack_set <- nchar(Sys.getenv("MEDIASTACK_API_KEY")) > 0
    msg <- paste0(
      "<div style='color:#9aa3b2;font-size:12px;padding:10px;background:#1e2640;",
      "border-radius:6px;border-left:3px solid #f4a261;'>",
      "<b style='color:#f4a261;'>News feed unavailable.</b><br/>",
      "Sources tried in order: ",
      if (newsapi_set)    "<b>NewsAPI</b> (key set)" else "<span style='color:#555;'>NewsAPI (no key)</span>",
      " &rarr; ",
      if (mediastack_set) "<b>MediaStack</b> (key set)" else "<span style='color:#555;'>MediaStack (no key)</span>",
      " &rarr; GDELT (always tried)<br/>",
      "<span style='color:#6b7585;'>Check the R console for the specific error. ",
      "If the server cannot reach external hosts, contact your hosting provider about outbound network access.</span>",
      "</div>"
    )
    return(htmltools::HTML(msg))
  }

  items <- news_df %>% dplyr::slice_head(n = n) %>%
    dplyr::mutate(
      age_str = dplyr::case_when(
        hours_ago < 1  ~ "just now",
        hours_ago < 24 ~ paste0(round(hours_ago, 0), "h ago"),
        TRUE           ~ paste0(round(hours_ago / 24, 0), "d ago")
      )
    )

  html_items <- vapply(seq_len(nrow(items)), function(i) {
    row <- items[i, ]
    url_str  <- if (!is.na(row$url) && nchar(row$url) > 0) row$url else ""
    url_tag  <- if (nchar(url_str) > 0)
      paste0(' <a href="', url_str, '" target="_blank" style="color:#00b4d8;font-size:11px;">&#8594; Read</a>')
    else ""
    desc_raw <- if (!is.na(row$description) && nchar(row$description) > 0) row$description else ""
    desc_str <- if (nchar(desc_raw) > 0)
      htmltools::htmlEscape(stringr::str_trunc(desc_raw, 140)) else ""

    # Supply chain badge
    chain_badge <- if (!is.na(row$supply_chain) && nchar(row$supply_chain) > 0) {
      chain_keys <- trimws(strsplit(row$supply_chain, ";")[[1]])
      badges <- paste(vapply(chain_keys, function(k) {
        sprintf('<span style="background:rgba(244,162,97,0.15);color:#f4a261;border:1px solid rgba(244,162,97,0.3);
                 border-radius:4px;padding:1px 7px;font-size:10px;margin-right:4px;">%s</span>',
                htmltools::htmlEscape(k))
      }, character(1)), collapse = "")
      paste0('<div style="margin-top:4px;">', badges, '</div>')
    } else ""

    # Ongoing story badge
    ongoing_badge <- if (!is.na(row$days_in_cache) && row$days_in_cache > 1)
      sprintf('<span style="background:rgba(124,92,191,0.15);color:#a785e0;border:1px solid rgba(124,92,191,0.3);
               border-radius:4px;padding:1px 7px;font-size:10px;margin-left:6px;">ongoing %dd</span>',
              row$days_in_cache)
    else ""

    paste0(
      '<div class="news-item">',
      '<div class="news-headline">',
        htmltools::htmlEscape(row$title), url_tag, ongoing_badge,
      '</div>',
      if (nchar(desc_str) > 0) paste0('<div class="news-desc">', desc_str, '</div>') else '',
      chain_badge,
      '<div class="news-meta">',
        '<span class="news-source">', htmltools::htmlEscape(row$source_name), '</span>',
        '<span>', row$age_str, '</span>',
      '</div>',
      '</div>'
    )
  }, character(1))

  htmltools::HTML(paste(html_items, collapse = "\n"))
}
