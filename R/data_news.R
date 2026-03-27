# ─────────────────────────────────────────────────────────────────────────────
# R/data_news.R — NewsAPI + supply-chain scoring + 10-day rolling cache
# ─────────────────────────────────────────────────────────────────────────────

# ── Persistent cache path ──────────────────────────────────────────────────────
.news_cache_dir <- local({
  env_dir <- Sys.getenv("ECONPULSE_CACHE_DIR")
  if (nchar(env_dir) > 0) {
    dir.create(env_dir, recursive = TRUE, showWarnings = FALSE)
    if (file.access(env_dir, 2) == 0) {
      return(env_dir)
    }
    message("[News cache] ECONPULSE_CACHE_DIR not writable, skipping cache")
    return(NULL)
  }
  app_cache <- file.path(getwd(), "cache")
  tryCatch(
    {
      dir.create(app_cache, recursive = TRUE, showWarnings = FALSE)
      if (file.access(app_cache, 2) == 0) {
        return(app_cache)
      }
      NULL # read-only (e.g. Posit Connect bundle) — cache disabled, fresh fetch only
    },
    error = function(e) NULL
  )
})

NEWS_CACHE_FILE <- if (!is.null(.news_cache_dir)) {
  file.path(.news_cache_dir, "news_cache.rds")
} else {
  NULL
}

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
  "federal reserve" = 5,
  "fomc" = 5,
  "rate cut" = 5,
  "rate hike" = 5,
  "jerome powell" = 4,
  "fed funds" = 4,
  "interest rate" = 4,
  "quantitative tightening" = 4,
  "dot plot" = 4,

  # ── Inflation / prices ────────────────────────────────────────────────────────
  "inflation" = 4,
  "cost of living" = 4,
  "affordability" = 4,
  "unaffordable" = 3,
  "price pressure" = 3,
  "cpi" = 3,
  "pce" = 3,

  # ── Labor / employment ────────────────────────────────────────────────────────
  "unemployment" = 4,
  "jobs report" = 4,
  "nonfarm payrolls" = 4,
  "payrolls" = 3,
  "labor market" = 4,
  "job openings" = 3,
  "job ladder" = 3,
  "wage growth" = 3,
  "wages" = 2,
  "healthcare jobs" = 3,
  "labor force" = 3,
  "workers" = 1,

  # ── Housing ───────────────────────────────────────────────────────────────────
  "housing market" = 4,
  "housing starts" = 4,
  "mortgage demand" = 4,
  "mortgage rate" = 4,
  "mortgage" = 3,
  "housing supply" = 4,
  "home prices" = 3,
  "housing bills" = 3,
  "rent" = 2,
  "vacancy" = 2,
  "affordability" = 3, # also in housing context

  # ── Supply chain / geopolitics ────────────────────────────────────────────────
  "strait of hormuz" = 5,
  "suez canal" = 5,
  "red sea" = 5,
  "supply chain" = 4,
  "port strike" = 4,
  "shipping rates" = 4,
  "freight" = 3,
  "container" = 3,
  "sanctions" = 4,
  "tariff" = 4,
  "trade war" = 4,
  "geopolitical" = 3,
  "conflict" = 2,
  "embargo" = 4,

  # ── Energy / commodities ──────────────────────────────────────────────────────
  "oil price" = 4,
  "opec" = 4,
  "crude" = 3,
  "natural gas" = 3,
  "energy prices" = 3,
  "gasoline" = 3,
  "diesel" = 2,

  # ── Consumer / spending ───────────────────────────────────────────────────────
  "consumer spending" = 3,
  "retail sales" = 3,
  "consumer confidence" = 3,
  "consumer sentiment" = 3,
  "spending" = 1,

  # ── Financial markets ─────────────────────────────────────────────────────────
  "yield curve" = 3,
  "credit spreads" = 3,
  "treasury" = 2,
  "earnings" = 2,
  "stock market" = 2,
  "vix" = 2,
  "dollar" = 2,

  # ── Macro / fiscal ────────────────────────────────────────────────────────────
  "recession" = 4,
  "gdp" = 3,
  "economic growth" = 3,
  "debt ceiling" = 3,
  "fiscal policy" = 3,
  "deficit" = 2,
  "manufacturing" = 2,
  "industrial" = 2,

  # ── General (low weight) ──────────────────────────────────────────────────────
  "economy" = 1,
  "market" = 1,
  "trade" = 1,
  "growth" = 1,
  "jobs" = 1
)

# Supply chain causal chain descriptions (used in LLM context + synopsis)
SUPPLY_CHAIN_CHAINS <- list(
  "strait of hormuz" = "~20% of global oil + LNG transits here → oil/gas price spike → diesel/petrol → trucking costs → retail prices, CPI energy component",
  "suez canal" = "~12% of global trade + major container route → shipping delays → inventory shortages → retail price pressure, PPI input costs",
  "red sea" = "Houthi attacks re-routing ships around Cape of Good Hope → +10-14 day transit → shipping rates → import prices → core goods CPI",
  "port strike" = "US port disruptions → supply shock → retail inventory drawdown → price pressure on goods, GDP drag via net exports",
  "supply chain" = "Disruption → input cost pass-through → PPI → CPI goods with 1-3 month lag",
  "sanctions" = "Export restrictions on targeted country → commodity supply shock → price spike in affected market (oil, wheat, metals)",
  "oil price" = "Oil → diesel/petrol → trucking/shipping costs → food + retail prices → CPI headline; also airline costs, plastics, chemicals",
  "freight" = "Shipping rates (Baltic Dry, Freightos) → import costs → retail goods prices → core CPI with ~2-month lag",
  "tariff" = "Import duties → domestic producer price relief, consumer price increase → CPI goods, retaliation risk to exports",
  "trade war" = "Broad tariffs → supply chain restructuring → near-term inflation, medium-term efficiency losses → GDP drag"
)

# ── Score articles ─────────────────────────────────────────────────────────────
score_relevance <- function(titles, descs) {
  text <- tolower(paste(titles, descs))
  scores <- numeric(length(text))
  for (kw in names(KW_WEIGHTS)) {
    scores <- scores +
      stringr::str_count(text, stringr::fixed(kw)) * KW_WEIGHTS[[kw]]
  }
  scores
}

# Detect which supply chain chains are triggered
detect_chains <- function(title, desc) {
  text <- tolower(paste(title, desc))
  matches <- names(SUPPLY_CHAIN_CHAINS)[
    vapply(
      names(SUPPLY_CHAIN_CHAINS),
      function(k) grepl(k, text, fixed = TRUE),
      logical(1)
    )
  ]
  if (length(matches) == 0) {
    return(NA_character_)
  }
  paste(matches, collapse = "; ")
}

# ── GDELT fetch (free, no API key, works from any server) ─────────────────────
fetch_gdelt <- function(days_back = 4, max_articles = 60) {
  tryCatch(
    {
      gdelt_q <- paste(
        c(
          "Federal Reserve",
          "inflation",
          "unemployment",
          "recession",
          "housing",
          "mortgage",
          "tariff",
          "oil price",
          "supply chain",
          "interest rates",
          "GDP",
          "consumer spending",
          "trade war",
          "earnings",
          "yield curve",
          "FOMC",
          "jobs"
        ),
        collapse = " OR "
      )
      from_str <- format(Sys.Date() - days_back, "%Y%m%d%H%M%S")
      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

      resp <- httr2::request("https://api.gdeltproject.org/api/v2/doc/doc") %>%
        httr2::req_url_query(
          query = gdelt_q,
          mode = "artlist",
          maxrecords = max_articles,
          startdatetime = from_str,
          sort = "hybridrel",
          format = "json"
        ) %>%
        httr2::req_timeout(30) %>%
        httr2::req_perform()

      raw <- resp %>% httr2::resp_body_json()
      arts <- raw$articles
      if (is.null(arts) || length(arts) == 0) {
        return(NULL)
      }

      purrr::map_dfr(arts, function(a) {
        pub <- tryCatch(
          as.POSIXct(a$seendate %||% "", format = "%Y%m%dT%H%M%SZ", tz = "UTC"),
          error = function(e) as.POSIXct(NA)
        )
        dplyr::tibble(
          title = a$title %||% "",
          description = "",
          source_name = a$domain %||% "Unknown",
          url = a$url %||% "",
          published = pub,
          content = ""
        )
      }) %>%
        dplyr::filter(nchar(title) > 5) %>%
        dplyr::mutate(
          hours_ago = as.numeric(difftime(now_utc, published, units = "hours")),
          relevance_score = score_relevance(title, description),
          supply_chain = mapply(
            detect_chains,
            title,
            description,
            SIMPLIFY = TRUE
          )
        ) %>%
        dplyr::filter(
          !is.na(published),
          hours_ago >= 0,
          hours_ago < days_back * 24
        ) %>%
        dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))
    },
    error = function(e) {
      message("[GDELT] fetch failed: ", e$message)
      NULL
    }
  )
}

# ── NewsAPI fetch (better quality; free dev plan blocked on servers) ───────────
fetch_newsapi <- function(days_back = 4, max_articles = 100) {
  key <- Sys.getenv("NEWS_API_KEY")
  if (nchar(key) == 0) {
    message("[NewsAPI] NEWS_API_KEY not set — skipping")
    return(list(data = NULL, error = "key_not_set"))
  }

  from_date <- format(Sys.Date() - days_back, "%Y-%m-%dT00:00:00")
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

  tryCatch(
    {
      resp <- httr2::request("https://newsapi.org/v2/everything") %>%
        httr2::req_url_query(
          q = NEWS_QUERY,
          from = from_date,
          sortBy = "relevancy",
          language = "en",
          pageSize = as.character(min(max_articles, 100)),
          apiKey = key
        ) %>%
        httr2::req_timeout(25) %>%
        httr2::req_perform()

      raw <- resp %>% httr2::resp_body_json()
      status <- raw$status %||% "error"

      if (status != "ok") {
        err_msg <- raw$message %||% "Unknown NewsAPI error"
        message("[NewsAPI] ", status, ": ", err_msg)
        return(list(data = NULL, error = err_msg))
      }

      df <- purrr::map_dfr(raw$articles, function(a) {
        dplyr::tibble(
          title = a$title %||% "",
          description = a$description %||% "",
          source_name = a$source$name %||% "Unknown",
          url = a$url %||% "",
          published = a$publishedAt %||% NA_character_,
          content = a$content %||% ""
        )
      }) %>%
        dplyr::filter(nchar(title) > 5, title != "[Removed]") %>%
        dplyr::mutate(
          published = suppressWarnings(
            as.POSIXct(published, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          ),
          hours_ago = as.numeric(difftime(now_utc, published, units = "hours")),
          relevance_score = score_relevance(title, description),
          supply_chain = mapply(
            detect_chains,
            title,
            description,
            SIMPLIFY = TRUE
          )
        ) %>%
        dplyr::filter(
          !is.na(published),
          hours_ago >= 0,
          hours_ago < days_back * 24
        ) %>%
        dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))

      list(data = df, error = NULL)
    },
    error = function(e) {
      message("[NewsAPI] request failed: ", e$message)
      list(data = NULL, error = e$message)
    }
  )
}

# ── Main fetch: NewsAPI → GDELT fallback ──────────────────────────────────────
fetch_news_raw <- function(days_back = 4, max_articles = 100) {
  # 1. Try NewsAPI
  result <- fetch_newsapi(days_back = days_back, max_articles = max_articles)

  if (!is.null(result$data) && nrow(result$data) > 0) {
    message(sprintf("[News] NewsAPI: %d articles fetched", nrow(result$data)))
    return(result$data)
  }

  # 2. Log why NewsAPI failed, fall back to GDELT
  if (!is.null(result$error)) {
    if (
      grepl(
        "426|upgrade|plan|cors|developer|produc|server",
        result$error,
        ignore.case = TRUE
      )
    ) {
      message(
        "[News] NewsAPI free plan requires upgrade for server deployments. Switching to GDELT."
      )
    } else if (result$error != "key_not_set") {
      message("[News] NewsAPI error '", result$error, "' — trying GDELT")
    }
  }

  gdelt_df <- fetch_gdelt(days_back = days_back, max_articles = max_articles)
  if (!is.null(gdelt_df) && nrow(gdelt_df) > 0) {
    message(sprintf("[News] GDELT: %d articles fetched", nrow(gdelt_df)))
    return(gdelt_df)
  }

  message("[News] All sources returned no data")
  NULL
}


# ── 10-day rolling cache ───────────────────────────────────────────────────────
# The cache always accumulates 10 days of history regardless of the UI slider.
# days_back controls: (a) how far back the fresh API fetch goes, and (b) which
# articles are shown in the news feed UI. The full 10-day history is always
# available to the LLM for context (ongoing story detection).

load_news_cache <- function() {
  if (is.null(NEWS_CACHE_FILE)) {
    return(NULL)
  }
  tryCatch(
    {
      if (file.exists(NEWS_CACHE_FILE)) readRDS(NEWS_CACHE_FILE) else NULL
    },
    error = function(e) NULL
  )
}

save_news_cache <- function(df) {
  if (is.null(NEWS_CACHE_FILE)) {
    return(invisible(NULL))
  }
  tryCatch(saveRDS(df, NEWS_CACHE_FILE), error = function(e) invisible(NULL))
}

fetch_news <- function(days_back = 4, max_articles = 80, cache_days = 10) {
  # Always use UTC for time comparisons — Posit Connect servers run UTC
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

  # 1. Fetch fresh articles
  fresh <- fetch_news_raw(days_back = days_back, max_articles = max_articles)

  # 2. Load full cache (NULL if cache disabled/unavailable)
  cached <- load_news_cache()

  # 3. Merge, deduplicate, recompute hours_ago in UTC
  all_rows <- dplyr::bind_rows(cached, fresh)
  if (is.null(all_rows) || nrow(all_rows) == 0) {
    if (!is.null(fresh) && nrow(fresh) > 0) {
      return(structure(fresh, full_history = fresh))
    }
    return(structure(dplyr::tibble(), full_history = dplyr::tibble()))
  }

  all_history <- all_rows %>%
    dplyr::filter(!is.na(published)) %>%
    dplyr::mutate(
      # Force UTC for comparison — prevents timezone mismatch on cloud servers
      published_utc = as.POSIXct(published, tz = "UTC"),
      hours_ago = as.numeric(difftime(now_utc, published_utc, units = "hours"))
    ) %>%
    dplyr::filter(hours_ago >= 0, hours_ago < cache_days * 24) %>%
    dplyr::group_by(title) %>%
    dplyr::slice_min(hours_ago, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      days_in_cache = pmax(
        1L,
        as.integer(difftime(
          as.Date(now_utc),
          as.Date(published_utc),
          units = "days"
        )) +
          1L
      )
    ) %>%
    dplyr::select(-published_utc) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))

  # 4. Save updated cache
  if (nrow(all_history) > 0) {
    save_news_cache(all_history)
  }

  # 5. Display view = articles within days_back window
  display_view <- all_history %>%
    dplyr::filter(hours_ago <= days_back * 24)

  # Fallback: if display_view is empty (e.g. timezone edge case or short lookback)
  # show the most recent articles from full history rather than nothing
  if (nrow(display_view) == 0 && nrow(all_history) > 0) {
    message(
      "[News] display_view empty after filter — showing most recent from cache"
    )
    display_view <- all_history %>% dplyr::slice_head(n = 20)
  }

  structure(display_view, full_history = all_history)
}

# Convenience accessor
news_full_history <- function(news_df) {
  attr(news_df, "full_history") %||% news_df
}

# ── Context for LLM — always uses full 10-day history ─────────────────────────
headlines_for_llm <- function(news_df, n = 25) {
  if (is.null(news_df)) {
    return("No recent headlines available.")
  }
  # Use full 10-day history for LLM context (not just the display window)
  df <- news_full_history(news_df)
  if (nrow(df) == 0) {
    return("No recent headlines available.")
  }

  rows <- df %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(
      age_str = dplyr::case_when(
        hours_ago < 2 ~ "just now",
        hours_ago < 24 ~ paste0(round(hours_ago), "h ago"),
        TRUE ~ paste0(round(hours_ago / 24), "d ago")
      )
    )

  lines <- vapply(
    seq_len(nrow(rows)),
    function(i) {
      r <- rows[i, ]
      desc <- if (!is.na(r$description) && nchar(r$description) > 0) {
        paste0(" — ", stringr::str_trunc(r$description, 110))
      } else {
        ""
      }
      cache_note <- if (!is.na(r$days_in_cache) && r$days_in_cache > 1) {
        sprintf(" [ongoing: %d days]", r$days_in_cache)
      } else {
        ""
      }
      chain_note <- if (!is.na(r$supply_chain) && nchar(r$supply_chain) > 0) {
        sprintf(
          "\n   SUPPLY CHAIN IMPACT: %s",
          paste(
            sapply(strsplit(r$supply_chain, "; ")[[1]], function(k) {
              SUPPLY_CHAIN_CHAINS[[k]] %||% k
            }),
            collapse = " | "
          )
        )
      } else {
        ""
      }
      paste0(
        i,
        ". [",
        r$source_name,
        " | ",
        r$age_str,
        cache_note,
        "] ",
        r$title,
        desc,
        chain_note
      )
    },
    character(1)
  )

  paste(lines, collapse = "\n")
}

# ── HTML renderer ─────────────────────────────────────────────────────────────
render_news_html <- function(news_df, n = 15) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    key_set <- nchar(Sys.getenv("NEWS_API_KEY")) > 0
    msg <- if (key_set) {
      paste0(
        "<p style='color:#f4a261;font-size:12px;padding:8px 0;'>",
        "<b>News unavailable.</b> NewsAPI free plan may be blocked on this server. ",
        "GDELT fallback also returned no results. Check R console for details. ",
        "Consider upgrading to NewsAPI Developer plan for server deployments.</p>"
      )
    } else {
      paste0(
        "<p style='color:#9aa3b2;font-size:12px;padding:8px 0;'>",
        "No <code>NEWS_API_KEY</code> set &mdash; using GDELT (free, no key needed). ",
        "If GDELT returns no results, check network connectivity from the server.</p>"
      )
    }
    return(htmltools::HTML(msg))
  }

  items <- news_df %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(
      age_str = dplyr::case_when(
        hours_ago < 1 ~ "just now",
        hours_ago < 24 ~ paste0(round(hours_ago, 0), "h ago"),
        TRUE ~ paste0(round(hours_ago / 24, 0), "d ago")
      )
    )

  html_items <- vapply(
    seq_len(nrow(items)),
    function(i) {
      row <- items[i, ]
      url_str <- if (!is.na(row$url) && nchar(row$url) > 0) row$url else ""
      url_tag <- if (nchar(url_str) > 0) {
        paste0(
          ' <a href="',
          url_str,
          '" target="_blank" style="color:#00b4d8;font-size:11px;">&#8594; Read</a>'
        )
      } else {
        ""
      }
      desc_raw <- if (!is.na(row$description) && nchar(row$description) > 0) {
        row$description
      } else {
        ""
      }
      desc_str <- if (nchar(desc_raw) > 0) {
        htmltools::htmlEscape(stringr::str_trunc(desc_raw, 140))
      } else {
        ""
      }

      # Supply chain badge
      chain_badge <- if (
        !is.na(row$supply_chain) && nchar(row$supply_chain) > 0
      ) {
        chain_keys <- trimws(strsplit(row$supply_chain, ";")[[1]])
        badges <- paste(
          vapply(
            chain_keys,
            function(k) {
              sprintf(
                '<span style="background:rgba(244,162,97,0.15);color:#f4a261;border:1px solid rgba(244,162,97,0.3);
                 border-radius:4px;padding:1px 7px;font-size:10px;margin-right:4px;">%s</span>',
                htmltools::htmlEscape(k)
              )
            },
            character(1)
          ),
          collapse = ""
        )
        paste0('<div style="margin-top:4px;">', badges, '</div>')
      } else {
        ""
      }

      # Ongoing story badge
      ongoing_badge <- if (!is.na(row$days_in_cache) && row$days_in_cache > 1) {
        sprintf(
          '<span style="background:rgba(124,92,191,0.15);color:#a785e0;border:1px solid rgba(124,92,191,0.3);
               border-radius:4px;padding:1px 7px;font-size:10px;margin-left:6px;">ongoing %dd</span>',
          row$days_in_cache
        )
      } else {
        ""
      }

      paste0(
        '<div class="news-item">',
        '<div class="news-headline">',
        htmltools::htmlEscape(row$title),
        url_tag,
        ongoing_badge,
        '</div>',
        if (nchar(desc_str) > 0) {
          paste0('<div class="news-desc">', desc_str, '</div>')
        } else {
          ''
        },
        chain_badge,
        '<div class="news-meta">',
        '<span class="news-source">',
        htmltools::htmlEscape(row$source_name),
        '</span>',
        '<span>',
        row$age_str,
        '</span>',
        '</div>',
        '</div>'
      )
    },
    character(1)
  )

  htmltools::HTML(paste(html_items, collapse = "\n"))
}
