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
  " rent " = 2,
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

  # ── Tech / AI (investment cycle, power, chips, labor displacement) ───────────
  "artificial intelligence" = 4,
  " ai " = 3,
  "generative ai" = 3,
  "ai spending" = 4,
  "ai investment" = 4,
  "ai bubble" = 4,
  "data center" = 4,
  "datacenter" = 4,
  "hyperscaler" = 4,
  "capex" = 3,
  "capital spending" = 3,
  "nvidia" = 3,
  "semiconductor" = 3,
  "chip" = 2,
  "export control" = 4,
  "power demand" = 4,
  "electricity" = 3,
  "power grid" = 3,
  "layoffs" = 3,
  "job cuts" = 3,
  "white collar" = 3,
  "automation" = 2,
  "productivity" = 3,

  # ── Forward-looking language (expectations beat recaps) ──────────────────────
  "outlook" = 2,
  "forecast" = 2,
  "expects" = 2,
  "expected to" = 2,
  "guidance" = 3,
  "plans to" = 2,
  "signals" = 2,
  "warns" = 2,
  "next year" = 2,
  "2027" = 2,

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
  "trade war" = "Broad tariffs → supply chain restructuring → near-term inflation, medium-term efficiency losses → GDP drag",
  "data center" = "AI data-center buildout → business investment (equipment + structures) props up GDP → power demand → utility capex, electricity prices (CPI energy) → regional construction jobs",
  "hyperscaler" = "Hyperscaler capex guidance → AI investment cycle → chip/equipment orders, GDP investment component; a guidance cut is an early read on an AI-spending slowdown",
  "export control" = "Chip export restrictions → semiconductor supply/revenue shock → tech earnings, retaliation risk on critical minerals",
  "power demand" = "AI and electrification load growth → grid strain → electricity rates (CPI) → utility investment, siting of data centers",
  "layoffs" = "Announced job cuts → initial claims with ~1-2 month lag → unemployment; white-collar/tech cuts hit high-income spending first"
)

# ── Score articles ─────────────────────────────────────────────────────────────
score_relevance <- function(titles, descs) {
  # Padded so word-bounded keys like " ai " match at the start/end of a title
  text <- paste0(" ", gsub("[[:punct:]]", " ", tolower(paste(titles, descs))), " ")
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

# ── Themes ────────────────────────────────────────────────────────────────────
# Keyword groups for counting how much coverage each storyline gets. Matched on
# padded, punctuation-stripped lowercase text (same as score_relevance).
NEWS_THEMES <- list(
  "Fed & rates"                 = c("federal reserve", "fomc", "rate cut", "rate hike", "powell", "interest rate", "fed "),
  "Inflation & prices"          = c("inflation", "cpi", "pce", "cost of living", "price pressure", "affordability"),
  "Labor market"                = c("jobs report", "payrolls", "unemployment", "labor market", "job openings", "hiring", "jobless"),
  "Layoffs & job cuts"          = c("layoffs", "job cuts", "laid off", "white collar", "hiring freeze", "workforce reduction"),
  "AI investment & data centers"= c("artificial intelligence", " ai ", "data center", "datacenter", "hyperscaler",
                                    "ai spending", "ai investment", "capex", "ai bubble"),
  "Chips & export controls"     = c("semiconductor", "chip", "nvidia", "tsmc", "export control"),
  "Power & grid"                = c("power demand", "electricity", "power grid", "utility", "utilities", "nuclear"),
  "Trade & tariffs"             = c("tariff", "trade war", "trade deal", "sanctions", "embargo"),
  "Energy & geopolitics"        = c("oil price", "opec", "crude", "natural gas", "strait of hormuz", "red sea", "geopolitical"),
  "Housing"                     = c("housing", "mortgage", "home prices", " rent ", " rents "),
  "Consumer"                    = c("consumer spending", "retail sales", "consumer confidence", "consumer sentiment", "holiday sales"),
  "Credit & financial stress"   = c("credit spread", "default", "private credit", "delinquen", "bank failure", "bankruptcy"),
  "Fiscal & government"         = c("shutdown", "deficit", "debt ceiling", "fiscal", "budget", "treasury auction")
)
NEWS_TECH_THEMES <- c("AI investment & data centers", "Chips & export controls", "Power & grid")

.news_text <- function(title, desc) {
  if (!length(title)) return(character(0))
  desc[is.na(desc)] <- ""
  paste0(" ", gsub("[[:punct:]]", " ", tolower(paste(title, desc))), " ")
}

#' "; "-joined theme names per article (NA when none match).
news_themes <- function(title, desc) {
  if (!length(title)) return(character(0))
  text <- .news_text(title, desc)
  hits <- vapply(NEWS_THEMES, function(kws)
    Reduce(`|`, lapply(kws, function(k) grepl(k, text, fixed = TRUE))), logical(length(text)))
  if (length(text) == 1) hits <- matrix(hits, nrow = 1, dimnames = list(NULL, names(NEWS_THEMES)))
  out <- apply(hits, 1, function(r) if (any(r)) paste(names(NEWS_THEMES)[r], collapse = "; ") else NA_character_)
  unname(out)
}

#' "tech" when an article touches a tech/AI theme, else "macro".
news_topic <- function(title, desc) {
  th <- news_themes(title, desc)
  ifelse(!is.na(th) & grepl(paste(NEWS_TECH_THEMES, collapse = "|"), th), "tech", "macro")
}

#' Collapse near-duplicate headlines (same first 8 words) across sources and days.
#' Keeps the most relevant copy; days_in_cache = distinct days the story ran.
dedupe_news <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  key <- vapply(strsplit(trimws(gsub("[^a-z0-9 ]", " ", tolower(df$title))), " +"),
                function(w) paste(utils::head(w, 8), collapse = " "), "")
  df$.key <- key
  df %>%
    dplyr::group_by(.key) %>%
    dplyr::mutate(days_in_cache = dplyr::n_distinct(as.Date(published))) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published), .by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.key) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))
}

#' Coverage per theme with momentum: articles in the newer half of the observed
#' window vs the older half. The split is the midpoint of the articles' own ages,
#' not of `window_hours`, because NewsAPI's free plan delays articles ~24h and a
#' fixed split would make every theme look like it is fading.
news_theme_summary <- function(news_df, window_hours = 96) {
  if (is.null(news_df) || !nrow(news_df) || !"themes" %in% names(news_df)) return(NULL)
  h <- news_df$hours_ago[!is.na(news_df$hours_ago)]
  half <- if (length(h)) (min(h) + max(h)) / 2 else window_hours / 2
  rows <- lapply(names(NEWS_THEMES), function(th) {
    hit <- !is.na(news_df$themes) & grepl(th, news_df$themes, fixed = TRUE)
    if (!any(hit)) return(NULL)
    d <- news_df[hit, ]
    recent <- sum(d$hours_ago < half, na.rm = TRUE)
    older <- sum(d$hours_ago >= half, na.rm = TRUE)
    top <- d$title[order(-d$relevance_score)][seq_len(min(2, nrow(d)))]
    data.frame(theme = th, n = nrow(d), recent = recent, older = older,
               momentum = if (recent >= older * 1.5 && recent >= 3) "rising"
                          else if (older >= recent * 1.5 && older >= 3) "fading" else "steady",
               top = paste(stringr::str_trunc(top, 90), collapse = " | "), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, TRUE)])
  if (is.null(out)) return(NULL)
  out[order(-out$n), ]
}

news_themes_text <- function(news_df, window_hours = 96) {
  s <- news_theme_summary(news_df, window_hours)
  if (is.null(s)) return("NEWS THEMES: unavailable.")
  paste0(
    sprintf("NEWS THEMES (articles over the last %dh; momentum = newer half of the coverage window vs older half)\n",
            window_hours),
    paste(sprintf("  %-30s %3d articles  %-7s (%d new / %d older)  e.g. %s",
                  s$theme, s$n, s$momentum, s$recent, s$older, s$top), collapse = "\n"))
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

# Second NewsAPI call: the tech/AI investment cycle and its labor-market side.
NEWSAPI_TECH_QUERY <- paste(
  '"artificial intelligence" OR "AI spending" OR "data center" OR hyperscaler OR',
  'semiconductor OR nvidia OR "export controls" OR "power demand" OR',
  '"AI layoffs" OR "job cuts" OR "white-collar" OR "capex"',
  collapse = " "
)

# GDELT allows ~1 request per 5 s, so it gets one combined query (OR terms must
# be parenthesised; bare space-separated words are ANDed).
GDELT_QUERY <- paste0(
  '("federal reserve" OR inflation OR recession OR unemployment OR tariff OR ',
  '"interest rates" OR "jobs report" OR "housing market" OR "oil price" OR ',
  '"artificial intelligence" OR "data center" OR semiconductor OR "AI spending" OR layoffs) ',
  'sourcelang:english'
)

# Domains to exclude from news results — highly partisan sources that
# introduce political framing rather than economic analysis.
# Focus: centrist (Reuters, AP, WSJ, Bloomberg) and centre-left (NYT, WaPo,
# FT, Economist) outlets known for factual economic reporting.
NEWS_EXCLUDED_DOMAINS <- paste(
  c(
    "breitbart.com",
    "dailywire.com",
    "newsmax.com",
    "thefederalist.com",
    "washingtonexaminer.com",
    "nypost.com",
    "oann.com",
    "westernjournal.com",
    "townhall.com",
    "redstate.com",
    "dailycaller.com",
    "theblaze.com",
    "pjmedia.com",
    "zerohedge.com",
    "naturalnews.com",
    "infowars.com"
  ),
  collapse = ","
)

# Source pattern filter applied to ALL sources (NewsAPI, MediaStack, GDELT)
.is_excluded_source <- function(source_names) {
  excluded_patterns <- c(
    "breitbart",
    "daily wire",
    "newsmax",
    "federalist",
    "washington examiner",
    "new york post",
    "one america",
    "western journal",
    "townhall",
    "red state",
    "daily caller",
    "the blaze",
    "pj media",
    "zero hedge",
    "natural news",
    "infowars",
    "epoch times",
    "gateway pundit",
    "american thinker",
    "daily mail" # known for sensationalism
  )
  tolower(source_names) %in%
    tolower(excluded_patterns) |
    sapply(tolower(source_names), function(s) {
      any(sapply(excluded_patterns, function(p) grepl(p, s, fixed = TRUE)))
    })
}

#' Macro and tech/AI queries, merged. One failing query does not drop the other.
fetch_newsapi <- function(days_back = 4, max_articles = 100) {
  macro <- fetch_newsapi_query(NEWSAPI_SHORT_QUERY, days_back, max_articles)
  if (identical(macro$error, "key_not_set")) return(macro)
  tech <- fetch_newsapi_query(NEWSAPI_TECH_QUERY, days_back, max_articles)
  df <- dplyr::bind_rows(macro$data, tech$data)
  if (!nrow(df)) return(list(data = NULL, error = macro$error %||% tech$error %||% "no_articles"))
  list(data = df, error = NULL)
}

fetch_newsapi_query <- function(query, days_back = 4, max_articles = 100) {
  key <- Sys.getenv("NEWS_API_KEY")
  if (nchar(key) == 0) {
    return(list(data = NULL, error = "key_not_set"))
  }

  tryCatch(
    {
      resp <- httr2::request("https://newsapi.org/v2/everything") %>%
        httr2::req_url_query(
          q = query,
          excludeDomains = NEWS_EXCLUDED_DOMAINS, # exclude partisan sources
          from = format(Sys.Date() - days_back, "%Y-%m-%d"),
          sortBy = "relevancy",
          language = "en",
          pageSize = as.character(min(max_articles, 100)),
          apiKey = key
        ) %>%
        httr2::req_timeout(25) %>%
        httr2::req_perform()

      raw <- resp %>% httr2::resp_body_json()
      if ((raw$status %||% "error") != "ok") {
        return(list(data = NULL, error = raw$message %||% "unknown"))
      }

      df <- .parse_newsapi_articles(raw$articles, days_back)
      list(data = df, error = NULL)
    },
    error = function(e) list(data = NULL, error = e$message)
  )
}

.parse_newsapi_articles <- function(arts, days_back) {
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  purrr::map_dfr(arts, function(a) {
    dplyr::tibble(
      title = a$title %||% "",
      description = a$description %||% "",
      source_name = a$source$name %||% "Unknown",
      url = a$url %||% "",
      published = suppressWarnings(
        as.POSIXct(
          a$publishedAt %||% "",
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      ),
      content = a$content %||% ""
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
  if (nchar(key) == 0) {
    return(list(data = NULL, error = "key_not_set"))
  }

  # Comma-separated keywords (MediaStack format — NOT OR syntax)
  ms_keywords <- paste(
    c(
      "economy",
      "inflation",
      "federal reserve",
      "unemployment",
      "housing",
      "mortgage",
      "tariff",
      "recession",
      "oil price",
      "interest rates",
      "artificial intelligence",
      "data center",
      "layoffs"
    ),
    collapse = ","
  )

  # Date range: MediaStack uses YYYY-MM-DD,YYYY-MM-DD
  date_from <- format(Sys.Date() - days_back, "%Y-%m-%d")
  date_range <- paste0(date_from, ",", format(Sys.Date(), "%Y-%m-%d"))

  tryCatch(
    {
      # MediaStack free plan: HTTP only (not HTTPS)
      resp <- httr2::request("http://api.mediastack.com/v1/news") %>%
        httr2::req_url_query(
          access_key = key,
          keywords = ms_keywords,
          languages = "en",
          date = date_range,
          sort = "published_desc",
          limit = as.character(min(max_articles, 100))
        ) %>%
        httr2::req_timeout(25) %>%
        httr2::req_perform()

      raw <- resp %>% httr2::resp_body_json()

      # Check for API-level error
      if (!is.null(raw$error)) {
        err <- raw$error$message %||% raw$error$code %||% "MediaStack API error"
        return(list(data = NULL, error = as.character(err)))
      }

      arts <- raw$data
      if (is.null(arts) || length(arts) == 0) {
        return(list(data = NULL, error = "no_articles"))
      }

      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
      df <- purrr::map_dfr(arts, function(a) {
        # MediaStack published_at: "2024-03-25T12:34:00+0000"
        pub <- suppressWarnings(tryCatch(
          as.POSIXct(
            a$published_at %||% "",
            format = "%Y-%m-%dT%H:%M:%S+0000",
            tz = "UTC"
          ),
          error = function(e) as.POSIXct(NA)
        ))
        dplyr::tibble(
          title = a$title %||% "",
          description = a$description %||% "",
          source_name = a$source %||% "Unknown",
          url = a$url %||% "",
          published = pub,
          content = ""
        )
      }) %>%
        dplyr::filter(nchar(title) > 5) %>%
        .add_news_meta(now_utc, days_back)

      list(data = df, error = NULL)
    },
    error = function(e) list(data = NULL, error = e$message)
  )
}

# ── Source 3: GDELT ───────────────────────────────────────────────────────────
# No API key required. Uses simple keyword query — NOT boolean OR syntax.
# Rate limit: ~1 req/sec sustained. On 429 or HTML response, skip gracefully.
fetch_gdelt <- function(days_back = 4, max_articles = 150) {
  tryCatch(
    {
      from_str <- format(Sys.Date() - days_back, "%Y%m%d%H%M%S")
      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

      resp <- httr2::request("https://api.gdeltproject.org/api/v2/doc/doc") %>%
        httr2::req_url_query(
          query = GDELT_QUERY,
          mode = "artlist",
          maxrecords = min(max_articles, 250),
          startdatetime = from_str,
          sort = "hybridrel",
          format = "json"
        ) %>%
        httr2::req_timeout(30) %>%
        httr2::req_perform()

      # Check content type — rate limit returns HTML error page
      ct <- httr2::resp_content_type(resp)
      if (!grepl("json", ct, ignore.case = TRUE)) {
        return(list(
          data = NULL,
          error = paste0(
            "Unexpected content type: ",
            ct,
            " (possibly rate-limited)"
          )
        ))
      }

      raw <- resp %>% httr2::resp_body_json()
      arts <- raw$articles
      if (is.null(arts) || length(arts) == 0) {
        return(list(data = NULL, error = "no_articles"))
      }

      df <- purrr::map_dfr(arts, function(a) {
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
        .add_news_meta(now_utc, days_back)

      list(data = df, error = NULL)
    },
    error = function(e) {
      message("[GDELT] ", e$message)
      list(data = NULL, error = e$message)
    }
  )
}

# ── Shared post-processing ────────────────────────────────────────────────────
.add_news_meta <- function(df, now_utc, days_back) {
  df %>%
    dplyr::mutate(
      hours_ago = as.numeric(difftime(now_utc, published, units = "hours")),
      days_in_cache = 1L,
      relevance_score = score_relevance(title, description),
      supply_chain = mapply(detect_chains, title, description, SIMPLIFY = TRUE),
      topic = news_topic(title, description),
      themes = news_themes(title, description)
    ) %>%
    dplyr::filter(
      !is.na(published),
      hours_ago >= 0,
      hours_ago < days_back * 24,
      !.is_excluded_source(source_name) # remove partisan sources
    ) %>%
    dplyr::arrange(dplyr::desc(relevance_score), dplyr::desc(published))
}

# ── Main fetch: try all sources in order ──────────────────────────────────────
fetch_news <- function(days_back = 4, max_articles = 80) {
  sources <- list(
    list(
      name = "NewsAPI",
      fn = fetch_newsapi,
      args = list(days_back = days_back, max_articles = max_articles)
    ),
    list(
      name = "MediaStack",
      fn = fetch_mediastack,
      args = list(days_back = days_back, max_articles = max_articles)
    ),
    list(
      name = "GDELT",
      fn = fetch_gdelt,
      args = list(days_back = days_back, max_articles = max(max_articles, 150))
    )
  )

  for (src in sources) {
    result <- tryCatch(
      do.call(src$fn, src$args),
      error = function(e) list(data = NULL, error = e$message)
    )
    if (!is.null(result$data) && nrow(result$data) > 0) {
      df <- dedupe_news(result$data)
      message(sprintf("[News] %s: %d articles (%d tech/AI)", src$name, nrow(df), sum(df$topic == "tech")))
      return(df)
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
headlines_for_llm <- function(news_df, n = 25, themes = TRUE) {
  if (is.null(news_df)) {
    return("No recent headlines available.")
  }
  # Use full 10-day history for LLM context (not just the display window)
  df <- news_full_history(news_df)
  if (nrow(df) == 0) {
    return("No recent headlines available.")
  }

  # Reserve ~1/3 of the slots for tech/AI so macro keywords don't crowd it out;
  # unused slots go to the next most relevant articles. Relevance order is kept.
  if ("topic" %in% names(df)) {
    tech <- utils::head(which(df$topic == "tech"), max(1, floor(n / 3)))
    rest <- setdiff(seq_len(nrow(df)), tech)
    df <- df[sort(c(tech, utils::head(rest, n - length(tech)))), ]
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
          "\n   TRANSMISSION: %s",
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
      theme_note <- if (!is.null(r$themes) && !is.na(r$themes)) paste0(" {", r$themes, "}") else ""
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
        theme_note,
        chain_note
      )
    },
    character(1)
  )
  if (isTRUE(themes)) lines <- c(news_themes_text(news_df), "", "TOP HEADLINES", lines)

  paste(lines, collapse = "\n")
}

# ── HTML renderer ─────────────────────────────────────────────────────────────
render_news_html <- function(news_df, n = 15) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    newsapi_set <- nchar(Sys.getenv("NEWS_API_KEY")) > 0
    mediastack_set <- nchar(Sys.getenv("MEDIASTACK_API_KEY")) > 0
    msg <- paste0(
      "<div style='color:#9aa3b2;font-size:12px;padding:10px;background:#1e2640;",
      "border-radius:6px;border-left:3px solid #f4a261;'>",
      "<b style='color:#f4a261;'>News feed unavailable.</b><br/>",
      "Sources tried in order: ",
      if (newsapi_set) {
        "<b>NewsAPI</b> (key set)"
      } else {
        "<span style='color:#555;'>NewsAPI (no key)</span>"
      },
      " &rarr; ",
      if (mediastack_set) {
        "<b>MediaStack</b> (key set)"
      } else {
        "<span style='color:#555;'>MediaStack (no key)</span>"
      },
      " &rarr; GDELT (always tried)<br/>",
      "<span style='color:#6b7585;'>Check the R console for the specific error. ",
      "If the server cannot reach external hosts, contact your hosting provider about outbound network access.</span>",
      "</div>"
    )
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
