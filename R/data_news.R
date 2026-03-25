# ─────────────────────────────────────────────────────────────────────────────
# R/data_news.R — NewsAPI integration + relevance scoring
# ─────────────────────────────────────────────────────────────────────────────

NEWS_QUERY_TERMS <- paste(
  c(
    "inflation",
    "federal reserve",
    "interest rates",
    "unemployment",
    "recession",
    "GDP",
    "economy",
    "tariff",
    "trade war",
    "oil price",
    "housing market",
    "mortgage",
    "stock market",
    "earnings",
    "jobs report",
    "OPEC",
    "geopolitical",
    "sanctions",
    "energy crisis",
    "debt ceiling",
    "consumer spending",
    "industrial production",
    "dollar",
    "yield curve"
  ),
  collapse = " OR "
)

# Keyword weights for relevance scoring
KW_WEIGHTS <- c(
  "federal reserve" = 4,
  "inflation" = 4,
  "interest rate" = 4,
  "recession" = 4,
  "opec" = 4,
  "tariff" = 3,
  "unemployment" = 3,
  "gdp" = 3,
  "rate hike" = 3,
  "rate cut" = 3,
  "trade war" = 3,
  "oil" = 3,
  "housing" = 2,
  "mortgage" = 2,
  "jobs" = 2,
  "wages" = 2,
  "treasury" = 2,
  "yield" = 2,
  "dollar" = 2,
  "geopolitical" = 2,
  "sanctions" = 2,
  "debt" = 2,
  "retail" = 2,
  "manufacturing" = 2,
  "economy" = 1,
  "market" = 1,
  "trade" = 1,
  "energy" = 1,
  "growth" = 1,
  "consumer" = 1
)

#' Fetch economic news from NewsAPI
fetch_news <- function(days_back = 4, max_articles = 60) {
  key <- Sys.getenv("NEWS_API_KEY")
  if (nchar(key) == 0) {
    message("NEWS_API_KEY not set – news feed disabled")
    return(NULL)
  }

  from_date <- format(Sys.Date() - days(days_back), "%Y-%m-%dT00:00:00")

  tryCatch(
    {
      resp <- request("https://newsapi.org/v2/everything") %>%
        req_url_query(
          q = "inflation OR \"federal reserve\" OR economy OR recession OR \"interest rates\" OR \"trade war\"",
          from = from_date,
          sortBy = "relevancy",
          language = "en",
          pageSize = as.character(min(max_articles, 100)),
          apiKey = key
        ) %>%
        req_timeout(25) %>%
        req_perform()

      raw <- resp %>% resp_body_json()

      if (raw$status != "ok") {
        message(glue("NewsAPI error: {raw$message}"))
        return(NULL)
      }

      arts <- raw$articles
      if (length(arts) == 0) {
        return(NULL)
      }

      df <- map_dfr(arts, function(a) {
        tibble(
          title = a$title %||% "",
          description = a$description %||% "",
          source_name = a$source$name %||% "Unknown",
          url = a$url %||% "",
          published = a$publishedAt %||% NA_character_,
          content = a$content %||% ""
        )
      }) %>%
        filter(nchar(title) > 5, title != "[Removed]") %>%
        mutate(
          published = suppressWarnings(ymd_hms(published)),
          relevance_score = score_relevance(title, description),
          hours_ago = as.numeric(difftime(
            Sys.time(),
            published,
            units = "hours"
          ))
        ) %>%
        filter(!is.na(published), hours_ago < days_back * 24) %>%
        arrange(desc(relevance_score), desc(published)) %>%
        slice_head(n = max_articles)

      df
    },
    error = function(e) {
      message(glue("News fetch error: {conditionMessage(e)}"))
      NULL
    }
  )
}

#' Score articles by keyword relevance
score_relevance <- function(titles, descs) {
  text <- tolower(paste(titles, descs))
  scores <- numeric(length(text))
  for (kw in names(KW_WEIGHTS)) {
    hits <- str_count(text, fixed(kw))
    scores <- scores + hits * KW_WEIGHTS[[kw]]
  }
  scores
}

#' Format top headlines as plain text for LLM context
headlines_for_llm <- function(news_df, n = 20) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    return("No recent headlines available.")
  }

  rows <- news_df %>%
    slice_head(n = n) %>%
    mutate(
      age_str = case_when(
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
        paste0(" — ", stringr::str_trunc(r$description, 120))
      } else {
        ""
      }
      paste0(i, ". [", r$source_name, " | ", r$age_str, "] ", r$title, desc)
    },
    character(1)
  )

  paste(lines, collapse = "\n")
}

#' Render news items as HTML for the UI
render_news_html <- function(news_df, n = 15) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    return(HTML(
      "<p class='text-muted-custom'>No recent news available. Check NEWS_API_KEY.</p>"
    ))
  }

  items <- news_df %>%
    slice_head(n = n) %>%
    mutate(
      age_str = case_when(
        hours_ago < 1 ~ "just now",
        hours_ago < 24 ~ paste0(round(hours_ago, 0), "h ago"),
        TRUE ~ paste0(round(hours_ago / 24, 0), "d ago")
      )
    )

  html_items <- map_chr(seq_len(nrow(items)), function(i) {
    row <- items[i, ]

    url_str <- if (!is.na(row$url) && nchar(row$url) > 0) row$url else ""
    url_tag <- if (nchar(url_str) > 0) {
      paste0(
        ' <a href="',
        url_str,
        '" target="_blank" style="color:#00b4d8;font-size:11px;">→ Read</a>'
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
      htmltools::htmlEscape(stringr::str_trunc(desc_raw, 160))
    } else {
      ""
    }

    paste0(
      '<div class="news-item">',
      '<div class="news-headline">',
      htmltools::htmlEscape(row$title),
      url_tag,
      '</div>',
      if (nchar(desc_str) > 0) {
        paste0('<div class="news-desc">', desc_str, '</div>')
      } else {
        ''
      },
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
  })

  HTML(paste(html_items, collapse = "\n"))
}
