# ─────────────────────────────────────────────────────────────────────────────
# R/data_news.R — NewsAPI + supply-chain scoring + 10-day rolling cache
# ─────────────────────────────────────────────────────────────────────────────

# ── Persistent cache path ──────────────────────────────────────────────────────
# Priority order:
#   1. ECONPULSE_CACHE_DIR env var (set this in .Renviron for Shiny Server /
#      shinyapps.io / Posit Connect deployments)
#   2. ./cache/ relative to the app directory (works for local dev and any
#      server where the app directory is writable)
#   3. Fall back to tempdir() with a warning (cache won't persist across restarts)

.news_cache_dir <- local({
  env_dir <- Sys.getenv("ECONPULSE_CACHE_DIR")
  if (nchar(env_dir) > 0) {
    dir.create(env_dir, recursive = TRUE, showWarnings = FALSE)
    env_dir
  } else {
    app_cache <- file.path(getwd(), "cache")
    created   <- tryCatch({
      dir.create(app_cache, recursive = TRUE, showWarnings = FALSE)
      file.access(app_cache, 2) == 0   # writable?
    }, error = function(e) FALSE)

    if (created) {
      app_cache
    } else {
      warning(
        "[EconPulse] cache/ directory not writable. News cache will use tempdir() ",
        "and will NOT persist across app restarts. ",
        "Set ECONPULSE_CACHE_DIR in .Renviron to a writable path to enable persistence."
      )
      tempdir()
    }
  }
})

NEWS_CACHE_FILE <- file.path(.news_cache_dir, "news_cache.rds")

# ── Expanded query covers supply chain + geopolitical triggers ────────────────
NEWS_QUERY <- paste(
  '"federal reserve" OR inflation OR recession OR "interest rates" OR',
  '"trade war" OR tariff OR "supply chain" OR "strait of hormuz" OR',
  '"oil price" OR OPEC OR sanctions OR "shipping" OR "freight" OR',
  '"port strike" OR "suez canal" OR "red sea" OR geopolitical OR',
  'unemployment OR "jobs report" OR "housing market" OR "earnings"',
  collapse = " "
)

# ── Keyword weights — supply chain + geopolitical added ──────────────────────
KW_WEIGHTS <- c(
  # Monetary / macro (highest signal)
  "federal reserve" = 5, "rate hike" = 5,    "rate cut" = 5,
  "inflation" = 4,        "interest rate" = 4, "recession" = 4,
  "opec" = 4,             "tariff" = 4,        "trade war" = 4,
  # Supply chain / geopolitical (new high-weight tier)
  "strait of hormuz" = 5, "suez canal" = 5,    "red sea" = 5,
  "supply chain" = 4,     "port strike" = 4,   "shipping" = 3,
  "freight" = 3,          "container" = 3,     "logistics" = 3,
  "sanctions" = 4,        "geopolitical" = 3,  "conflict" = 3,
  "war" = 3,              "embargo" = 4,       "blockade" = 4,
  # Energy / commodities
  "oil price" = 4,        "crude" = 3,         "gasoline" = 3,
  "diesel" = 3,           "natural gas" = 3,   "energy crisis" = 4,
  "pipeline" = 3,         "refinery" = 3,
  # Labor
  "unemployment" = 3,     "jobs" = 2,          "wages" = 2,
  "payrolls" = 3,         "labor market" = 3,
  # Housing / consumer
  "housing" = 2,          "mortgage" = 2,      "retail" = 2,
  "consumer spending" = 3,
  # Financial
  "gdp" = 3,              "treasury" = 2,      "yield" = 2,
  "dollar" = 2,           "debt ceiling" = 3,  "credit" = 2,
  # General
  "economy" = 1,          "market" = 1,        "trade" = 1,
  "growth" = 1,           "manufacturing" = 2, "industrial" = 2
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

# ── Fetch from NewsAPI ─────────────────────────────────────────────────────────
fetch_news_raw <- function(days_back = 4, max_articles = 100) {
  key <- Sys.getenv("NEWS_API_KEY")
  if (nchar(key) == 0) { message("NEWS_API_KEY not set"); return(NULL) }

  from_date <- format(Sys.Date() - lubridate::days(days_back), "%Y-%m-%dT00:00:00")

  tryCatch({
    resp <- httr2::request("https://newsapi.org/v2/everything") %>%
      httr2::req_url_query(
        q        = NEWS_QUERY,
        from     = from_date,
        sortBy   = "relevancy",
        language = "en",
        pageSize = as.character(min(max_articles, 100)),
        apiKey   = key
      ) %>%
      httr2::req_timeout(25) %>%
      httr2::req_perform()

    raw <- resp %>% httr2::resp_body_json()
    if (raw$status != "ok") { message("NewsAPI: ", raw$message); return(NULL) }

    purrr::map_dfr(raw$articles, function(a) {
      dplyr::tibble(
        title       = a$title       %||% "",
        description = a$description %||% "",
        source_name = a$source$name %||% "Unknown",
        url         = a$url         %||% "",
        published   = a$publishedAt %||% NA_character_,
        content     = a$content     %||% ""
      )
    }) %>%
      dplyr::filter(nchar(title) > 5, title != "[Removed]") %>%
      dplyr::mutate(
        published       = suppressWarnings(lubridate::ymd_hms(published)),
        hours_ago       = as.numeric(difftime(Sys.time(), published, units = "hours")),
        relevance_score = score_relevance(title, description),
        supply_chain    = mapply(detect_chains, title, description, SIMPLIFY = TRUE)
      ) %>%
      dplyr::filter(!is.na(published), hours_ago < days_back * 24) %>%
      dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))

  }, error = function(e) { message("News fetch error: ", e$message); NULL })
}

# ── 10-day rolling cache ───────────────────────────────────────────────────────
# The cache always accumulates 10 days of history regardless of the UI slider.
# days_back controls: (a) how far back the fresh API fetch goes, and (b) which
# articles are shown in the news feed UI. The full 10-day history is always
# available to the LLM for context (ongoing story detection).

load_news_cache <- function() {
  tryCatch({
    if (file.exists(NEWS_CACHE_FILE)) readRDS(NEWS_CACHE_FILE) else NULL
  }, error = function(e) NULL)
}

save_news_cache <- function(df) {
  tryCatch(saveRDS(df, NEWS_CACHE_FILE), error = function(e) invisible(NULL))
}

fetch_news <- function(days_back = 4, max_articles = 80, cache_days = 10) {
  # 1. Fetch fresh articles (controlled by days_back slider)
  fresh <- fetch_news_raw(days_back = days_back, max_articles = max_articles)

  # 2. Load full 10-day cache
  cached <- load_news_cache()

  # 3. Merge all history (cache + fresh), deduplicate
  all_history <- dplyr::bind_rows(cached, fresh) %>%
    dplyr::filter(!is.na(published)) %>%
    dplyr::group_by(title) %>%
    dplyr::slice_min(hours_ago, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    # Cache window: always keep 10 days for LLM context
    dplyr::filter(
      as.numeric(difftime(Sys.time(), published, units = "hours")) < cache_days * 24
    ) %>%
    dplyr::mutate(
      hours_ago     = as.numeric(difftime(Sys.time(), published, units = "hours")),
      days_in_cache = pmax(1L,
        as.integer(difftime(Sys.Date(), as.Date(published), units = "days")) + 1L)
    ) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))

  # 4. Save updated full cache
  if (nrow(all_history) > 0) save_news_cache(all_history)

  # 5. Return two views as attributes:
  #    $display  = articles within days_back window (what the news feed shows)
  #    $full     = all 10 days of cache (what the LLM uses for context)
  display_view <- all_history %>%
    dplyr::filter(hours_ago <= days_back * 24)

  structure(
    display_view,
    full_history = all_history
  )
}

# Convenience accessor
news_full_history <- function(news_df) {
  attr(news_df, "full_history") %||% news_df
}

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
    return(htmltools::HTML(
      "<p class='text-muted-custom'>No recent news available. Check NEWS_API_KEY.</p>"
    ))
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
