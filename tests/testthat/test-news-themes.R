source(file.path("..", "..", "R", "data_news.R"), local = FALSE)

news_fx <- function(titles, hours_ago, desc = "") {
  now <- as.POSIXct("2026-11-23 12:00", tz = "UTC")
  df <- data.frame(title = titles, description = desc, source_name = "Reuters", url = "",
                   published = now - hours_ago * 3600, content = "", stringsAsFactors = FALSE)
  df$hours_ago <- hours_ago
  df$days_in_cache <- 1L
  df$relevance_score <- score_relevance(df$title, df$description)
  df$supply_chain <- mapply(detect_chains, df$title, df$description, SIMPLIFY = TRUE)
  df$topic <- news_topic(df$title, df$description)
  df$themes <- news_themes(df$title, df$description)
  df
}

test_that("themes and topic tag AI/tech stories without false hits", {
  th <- news_themes(c("Hyperscalers lift AI data center capex again",
                      "Fed's Powell signals rate cut in December",
                      "Current account deficit widens"), c("", "", ""))
  expect_match(th[1], "AI investment & data centers", fixed = TRUE)
  expect_match(th[2], "Fed & rates", fixed = TRUE)
  expect_false(grepl("Housing", th[3]))   # "current" must not match "rent"
  expect_equal(news_topic(c("Nvidia export controls tighten", "Jobs report beats"), c("", "")),
               c("tech", "macro"))
  expect_equal(news_themes(character(0), character(0)), character(0))
})

test_that("word-bounded AI keyword matches AI but not words containing 'ai'", {
  expect_gt(score_relevance("AI stocks rally", ""), 0)
  expect_equal(score_relevance("Officials said again", ""), 0)
})

test_that("near-duplicates collapse and count the days a story ran", {
  df <- news_fx(c("Nvidia shares plunge as AI spending fears grow - Reuters",
                  "Nvidia shares plunge as AI spending fears grow",
                  "Housing starts fall"), c(2, 30, 5))
  out <- dedupe_news(df)
  expect_equal(nrow(out), 2)
  expect_equal(out$days_in_cache[grepl("Nvidia", out$title)], 2L)
})

test_that("theme summary flags rising storylines", {
  df <- news_fx(c(rep("AI data center boom", 4), "Data center power demand", "Tariff talks stall"),
                c(1, 5, 10, 20, 70, 60))
  s <- news_theme_summary(df, 96)
  ai <- s[s$theme == "AI investment & data centers", ]
  expect_equal(ai$n, 5)
  expect_equal(ai$momentum, "rising")
  expect_match(news_themes_text(df), "NEWS THEMES", fixed = TRUE)
  expect_null(news_theme_summary(NULL))
})

test_that("headlines for the LLM reserve slots for tech stories", {
  macro <- paste("Fed rate cut inflation recession unemployment story", 1:10)
  df <- news_fx(c(macro, "Chipmaker earnings"), rep(3, 11))
  df <- df[order(-df$relevance_score), ]
  txt <- headlines_for_llm(df, n = 6)
  expect_match(txt, "Chipmaker earnings", fixed = TRUE)
  expect_match(txt, "TOP HEADLINES", fixed = TRUE)
  expect_false(grepl("NEWS THEMES", headlines_for_llm(df, n = 6, themes = FALSE)))
})
