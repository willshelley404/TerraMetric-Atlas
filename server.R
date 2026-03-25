# ─────────────────────────────────────────────────────────────────────────────
# server.R — EconPulse AI Shiny Server
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # ── Waiter (full-screen loading overlay) ────────────────────────────────────
  w <- Waiter$new(
    html = tagList(
      spin_dots(),
      br(),
      div(
        style = "color:#e0e0e0;font-size:14px;margin-top:16px;",
        "Loading EconPulse AI…"
      ),
      div(
        style = "color:#9aa3b2;font-size:12px;margin-top:6px;",
        "Fetching FRED · BLS · Markets · News"
      )
    ),
    color = "rgba(15,17,23,0.95)"
  )

  # ── Reactive State ────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    fred_data = NULL,
    bls_data = NULL,
    mkt_data = NULL,
    mkt_returns = NULL,
    news_data = NULL,
    census_data = NULL,
    metro_data = NULL,
    kpis = NULL,
    forecasts = NULL,
    infl_panel = NULL,
    llm_text = NULL,
    chat_text = NULL,
    data_loaded = FALSE,
    loading_msg = "",
    last_updated = NULL
  )

  # ── Initial data load on app start ───────────────────────────────────────────
  observe({
    req(!rv$data_loaded)
    w$show()
    load_all_data()
    w$hide()
  })

  # ── Refresh button ────────────────────────────────────────────────────────────
  observeEvent(input$refresh_data, {
    w$show()
    load_all_data()
    w$hide()
    showNotification(
      "Data refreshed successfully!",
      type = "message",
      duration = 4
    )
  })

  # ── Core data loader function ─────────────────────────────────────────────────
  load_all_data <- function() {
    lookback <- isolate(input$cfg_lookback) %||% 7
    news_days <- isolate(input$cfg_news_days) %||% 4

    # FRED
    tryCatch(
      {
        rv$fred_data <- fetch_all_fred(lookback_years = lookback)
        rv$kpis <- build_kpis(rv$fred_data)
      },
      error = function(e) message("FRED load error: ", e$message)
    )

    # BLS
    tryCatch(
      {
        rv$bls_data <- fetch_all_bls(lookback_years = min(lookback, 5))
      },
      error = function(e) message("BLS load error: ", e$message)
    )

    # Inflation panel (BLS + FRED combined)
    tryCatch(
      {
        if (!is.null(rv$bls_data) && !is.null(rv$fred_data)) {
          rv$infl_panel <- build_inflation_panel(rv$bls_data, rv$fred_data)
        }
      },
      error = function(e) message("Inflation panel error: ", e$message)
    )

    # Markets
    tryCatch(
      {
        raw_mkt <- fetch_market_data(lookback_days = max(lookback * 365, 400))
        rv$mkt_data <- raw_mkt
        rv$mkt_returns <- compute_returns(raw_mkt)
      },
      error = function(e) message("Market load error: ", e$message)
    )

    # News
    tryCatch(
      {
        rv$news_data <- fetch_news(days_back = news_days)
      },
      error = function(e) message("News load error: ", e$message)
    )

    # Forecasts (lazy – only when Forecasting tab is visited)
    # Census (lazy – only when Metro Map tab is visited)
    rv$data_loaded <- TRUE
    rv$last_updated <- Sys.time()
  }

  # ── Lazy-load forecasts when tab is opened ─────────────────────────────────
  observeEvent(input$tabs, {
    if (
      input$tabs == "forecasting" &&
        is.null(rv$forecasts) &&
        !is.null(rv$fred_data)
    ) {
      withProgress(message = "Running Prophet forecasts…", value = 0, {
        rv$forecasts <- run_all_forecasts(rv$fred_data, horizon_months = 18)
        incProgress(1)
      })
    }
  })

  # ── Helper: safe plotly empty state ───────────────────────────────────────────
  empty_plot <- function(msg = "Data not available") {
    plot_ly() %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#9aa3b2"),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = msg,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(color = "#9aa3b2", size = 14)
        ))
      ) %>%
      config(displayModeBar = FALSE)
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # KPI OUTPUTS
  # ═══════════════════════════════════════════════════════════════════════════

  make_kpi_output <- function(id, val_fn, chg_fn = NULL, fmt_fn = NULL) {
    output[[id]] <- renderText({
      req(rv$kpis)
      v <- val_fn(rv$kpis)
      if (!is.null(fmt_fn)) fmt_fn(v) else as.character(round(v, 2))
    })
    if (!is.null(chg_fn)) {
      chg_id <- paste0(id, "_chg")
      output[[chg_id]] <- renderUI({
        req(rv$kpis)
        result <- chg_fn(rv$kpis)
        HTML(result)
      })
    }
  }

  pct_badge <- function(val, label = "", higher_good = TRUE) {
    if (is.na(val)) {
      return("")
    }
    cls <- if (val > 0) {
      if (higher_good) "positive" else "negative"
    } else if (val < 0) {
      if (higher_good) "negative" else "positive"
    } else {
      "neutral"
    }
    arrow <- if (val > 0) {
      "▲"
    } else if (val < 0) {
      "▼"
    } else {
      "—"
    }
    paste0(
      '<span class="metric-change ',
      cls,
      '">',
      arrow,
      " ",
      label,
      round(abs(val), 2),
      "</span>"
    )
  }

  output$kpi_unemp <- renderText({
    req(rv$kpis)
    paste0(round(rv$kpis$unemp_rate %||% NA, 1), "%")
  })
  output$kpi_unemp_chg <- renderUI({
    req(rv$kpis)
    HTML(pct_badge(rv$kpis$unemp_rate %||% NA - 3.5, "vs 3.5%", FALSE))
  })

  output$kpi_cpi <- renderText({
    req(rv$kpis)
    fmt_pct(rv$kpis$cpi_yoy)
  })
  output$kpi_cpi_chg <- renderUI({
    req(rv$kpis)
    HTML(pct_badge(rv$kpis$cpi_yoy %||% NA - 2, "vs 2% target", FALSE))
  })

  output$kpi_fedfunds <- renderText({
    req(rv$kpis)
    fmt_pct(rv$kpis$fed_funds)
  })
  output$kpi_fedfunds_chg <- renderUI({
    HTML("")
  })

  output$kpi_payrolls <- renderText({
    req(rv$kpis)
    fmt_num(rv$kpis$payrolls_chg, 0, "K")
  })
  output$kpi_payrolls_chg <- renderUI({
    req(rv$kpis)
    v <- rv$kpis$payrolls_chg %||% NA
    HTML(pct_badge(v, "K jobs ", TRUE))
  })

  output$kpi_oil <- renderText({
    req(rv$kpis)
    fmt_dollar(rv$kpis$oil_price)
  })
  output$kpi_oil_chg <- renderUI({
    HTML("")
  })

  output$kpi_vix <- renderText({
    req(rv$kpis)
    fmt_num(rv$kpis$vix)
  })
  output$kpi_vix_chg <- renderUI({
    req(rv$kpis)
    v <- rv$kpis$vix %||% NA
    cls <- if (!is.na(v) && v > 20) "negative" else "positive"
    HTML(paste0(
      '<span class="metric-change ',
      cls,
      '">',
      if (!is.na(v) && v > 20) "Elevated" else "Low",
      '</span>'
    ))
  })

  output$kpi_t10yr <- renderText({
    req(rv$kpis)
    fmt_pct(rv$kpis$t10yr)
  })
  output$kpi_t10yr_chg <- renderUI({
    HTML("")
  })

  output$kpi_t10y2y <- renderText({
    req(rv$kpis)
    fmt_pct(rv$kpis$t10y2y)
  })
  output$kpi_t10y2y_chg <- renderUI({
    req(rv$kpis)
    v <- rv$kpis$t10y2y %||% NA
    if (is.na(v)) {
      return(HTML(""))
    }
    cls <- if (v < 0) "negative" else "positive"
    HTML(paste0(
      '<span class="metric-change ',
      cls,
      '">',
      if (v < 0) "⚠ Inverted" else "Normal",
      '</span>'
    ))
  })

  output$kpi_mortgage <- renderText({
    req(rv$kpis)
    fmt_pct(rv$kpis$mortgage30)
  })
  output$kpi_mortgage_chg <- renderUI({
    HTML("")
  })

  output$kpi_housing <- renderText({
    req(rv$kpis)
    fmt_num(rv$kpis$housing_starts, 0, "K")
  })
  output$kpi_housing_chg <- renderUI({
    HTML("")
  })

  output$kpi_consump <- renderText({
    req(rv$kpis)
    fmt_num(rv$kpis$cons_sent)
  })
  output$kpi_consump_chg <- renderUI({
    req(rv$kpis)
    v <- rv$kpis$cons_sent %||% NA
    HTML(paste0(
      '<span class="metric-change ',
      if (!is.na(v) && v > 80) "positive" else "negative",
      '">',
      if (!is.na(v) && v > 80) "Optimistic" else "Pessimistic",
      '</span>'
    ))
  })

  output$kpi_jolts <- renderText({
    req(rv$kpis)
    fmt_num(rv$kpis$job_openings, 0, "K")
  })
  output$kpi_jolts_chg <- renderUI({
    HTML("")
  })

  output$last_updated <- renderText({
    if (is.null(rv$last_updated)) {
      return("—")
    }
    format(rv$last_updated, "%b %d, %Y %H:%M")
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # OVERVIEW CHARTS
  # ═══════════════════════════════════════════════════════════════════════════

  output$ov_unemp <- renderPlotly({
    req(rv$fred_data, rv$fred_data$UNRATE)
    df <- rv$fred_data$UNRATE %>% tail(84)
    plot_ly(
      df,
      x = ~date,
      y = ~value,
      type = "scatter",
      mode = "lines",
      line = list(color = "#00b4d8", width = 2.5),
      name = "Unemployment Rate",
      fill = "tozeroy",
      fillcolor = "rgba(0,180,216,0.1)",
      hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_theme(y = "%")
  })

  output$ov_cpi <- renderPlotly({
    req(rv$fred_data)
    plots <- list()
    if (!is.null(rv$fred_data$CPIAUCSL) && nrow(rv$fred_data$CPIAUCSL) > 12) {
      cpi_yoy <- rv$fred_data$CPIAUCSL %>%
        yoy_change() %>%
        filter(!is.na(yoy)) %>%
        tail(84)
      plots[["CPI YoY"]] <- list(
        x = cpi_yoy$date,
        y = cpi_yoy$yoy,
        color = "#e94560"
      )
    }
    if (!is.null(rv$fred_data$CPILFESL) && nrow(rv$fred_data$CPILFESL) > 12) {
      core_yoy <- rv$fred_data$CPILFESL %>%
        yoy_change() %>%
        filter(!is.na(yoy)) %>%
        tail(84)
      plots[["Core CPI YoY"]] <- list(
        x = core_yoy$date,
        y = core_yoy$yoy,
        color = "#f4a261"
      )
    }
    p <- plot_ly()
    for (nm in names(plots)) {
      p <- p %>%
        add_lines(
          x = plots[[nm]]$x,
          y = plots[[nm]]$y,
          line = list(color = plots[[nm]]$color, width = 2),
          name = nm,
          hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
        )
    }
    p %>%
      add_segments(
        x = min(do.call(c, lapply(plots, `[[`, "x"))),
        xend = max(do.call(c, lapply(plots, `[[`, "x"))),
        y = 2,
        yend = 2,
        line = list(color = "#ffffff44", width = 1, dash = "dot"),
        name = "2% Target",
        showlegend = TRUE,
        hoverinfo = "none"
      ) %>%
      dark_theme(y = "%")
  })

  output$ov_rates <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    series_map <- list(
      "Fed Funds" = list(id = "FEDFUNDS", color = "#f4a261"),
      "10Y Tsy" = list(id = "DGS10", color = "#00b4d8"),
      "2Y Tsy" = list(id = "DGS2", color = "#7c5cbf"),
      "Mortgage" = list(id = "MORTGAGE30US", color = "#e94560")
    )
    for (nm in names(series_map)) {
      cfg <- series_map[[nm]]
      df <- rv$fred_data[[cfg$id]]
      if (!is.null(df) && nrow(df) > 0) {
        # Use full available history (daily series have 15 yrs via LONG_HISTORY_IDS)
        p <- p %>%
          add_lines(
            x = df$date,
            y = df$value,
            line = list(color = cfg$color, width = 1.8),
            name = nm,
            hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
          )
      }
    }
    p %>% dark_theme(y = "%")
  })

  output$ov_market <- renderPlotly({
    req(rv$mkt_data)
    tickers <- c("SPY", "QQQ", "GLD", "TLT")
    avail <- tickers[tickers %in% unique(rv$mkt_data$symbol)]
    if (length(avail) == 0) {
      return(empty_plot("Market data unavailable"))
    }
    cutoff <- Sys.Date() - 365
    df <- rv$mkt_data %>% filter(symbol %in% avail, date >= cutoff)
    df <- df %>%
      group_by(symbol) %>%
      mutate(idx = close / first(close) * 100) %>%
      ungroup()
    cols <- c(
      SPY = "#00b4d8",
      QQQ = "#7c5cbf",
      GLD = "#f4a261",
      TLT = "#2dce89"
    )
    p <- plot_ly()
    for (t in avail) {
      sub <- df %>% filter(symbol == t)
      p <- p %>%
        add_lines(
          data = sub,
          x = ~date,
          y = ~idx,
          line = list(color = cols[t] %||% "#aaaaaa", width = 2),
          name = TICKER_LABELS[t] %||% t,
          hovertemplate = "%{x|%b %d}: %{y:.1f}<extra></extra>"
        )
    }
    p %>%
      add_segments(
        x = min(df$date),
        xend = max(df$date),
        y = 100,
        yend = 100,
        line = list(color = "#ffffff33", width = 1, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      dark_theme(y = "Indexed (100 = 1Y ago)")
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # LABOR CHARTS
  # ═══════════════════════════════════════════════════════════════════════════

  output$lab_unemp <- renderPlotly({
    req(rv$fred_data$UNRATE)
    df <- rv$fred_data$UNRATE %>% tail(120)
    plot_ly(
      df,
      x = ~date,
      y = ~value,
      type = "scatter",
      mode = "lines",
      line = list(color = "#00b4d8", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(0,180,216,0.08)",
      name = "Unemployment Rate",
      hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_theme(y = "%")
  })

  output$lab_payrolls <- renderPlotly({
    req(rv$fred_data$PAYEMS)
    df <- rv$fred_data$PAYEMS %>%
      tail(120) %>%
      mutate(
        mom = c(NA, diff(value)),
        col = if_else(is.na(mom) | mom >= 0, "#2dce89", "#e94560")
      ) %>%
      filter(!is.na(mom))
    # Pass colour vector directly — formula (~col) inside marker=list() is NOT
    # evaluated per-bar by plotly and causes wrong colours
    plot_ly(
      df,
      x = ~date,
      y = ~mom,
      type = "bar",
      marker = list(color = df$col),
      name = "MoM Change (K)",
      hovertemplate = "%{x|%b %Y}: %{y:+,.0f}K<extra></extra>"
    ) %>%
      dark_theme(y = "K Jobs")
  })

  output$lab_jolts <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    if (!is.null(rv$fred_data$JTSJOL)) {
      df <- rv$fred_data$JTSJOL %>% tail(84)
      p <- p %>%
        add_lines(
          data = df,
          x = ~date,
          y = ~value,
          line = list(color = "#00b4d8", width = 2),
          name = "Job Openings",
          hovertemplate = "%{x|%b %Y}: %{y:,.0f}K<extra></extra>"
        )
    }
    if (!is.null(rv$fred_data$JTSQUL)) {
      df <- rv$fred_data$JTSQUL %>% tail(84)
      p <- p %>%
        add_lines(
          data = df,
          x = ~date,
          y = ~value,
          line = list(color = "#f4a261", width = 2),
          name = "Quits",
          hovertemplate = "%{x|%b %Y}: %{y:,.0f}K<extra></extra>"
        )
    }
    p %>% dark_theme(y = "Thousands")
  })

  output$lab_wages <- renderPlotly({
    req(rv$fred_data$CES0500000003)
    df <- rv$fred_data$CES0500000003 %>%
      yoy_change() %>%
      filter(!is.na(yoy)) %>%
      tail(84)
    plot_ly(
      df,
      x = ~date,
      y = ~yoy,
      type = "scatter",
      mode = "lines",
      line = list(color = "#f4a261", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(244,162,97,0.1)",
      name = "Wage Growth YoY",
      hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
    ) %>%
      dark_theme(y = "YoY %")
  })

  output$tbl_labor <- renderDT({
    req(rv$fred_data)
    rows <- map_dfr(
      c("UNRATE", "PAYEMS", "JTSJOL", "JTSQUL", "CES0500000003"),
      function(sid) {
        df <- rv$fred_data[[sid]]
        if (is.null(df) || nrow(df) == 0) {
          return(NULL)
        }
        latest <- tail(df, 1)
        tibble(
          Series = FRED_CFG[[sid]]$label %||% sid,
          Date = format(latest$date, "%b %Y"),
          Value = round(latest$value, 2),
          Unit = FRED_CFG[[sid]]$unit %||% ""
        )
      }
    )
    datatable(
      rows,
      options = list(pageLength = 10, dom = "t"),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatStyle(columns = 1:4, backgroundColor = "#1e2640", color = "#d0d0d0")
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # INFLATION CHARTS
  # ═══════════════════════════════════════════════════════════════════════════

  output$inf_cpi <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    for (pair in list(
      list("CPIAUCSL", "CPI YoY", "#e94560"),
      list("CPILFESL", "Core CPI YoY", "#f4a261")
    )) {
      df <- rv$fred_data[[pair[[1]]]]
      if (!is.null(df) && nrow(df) > 12) {
        d <- df %>% yoy_change() %>% filter(!is.na(yoy)) %>% tail(84)
        p <- p %>%
          add_lines(
            data = d,
            x = ~date,
            y = ~yoy,
            line = list(color = pair[[3]], width = 2.5),
            name = pair[[2]],
            hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
          )
      }
    }
    p %>%
      add_segments(
        x = as.Date("2018-01-01"),
        xend = Sys.Date(),
        y = 2,
        yend = 2,
        line = list(color = "#ffffff44", width = 1, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      dark_theme(y = "YoY %")
  })

  output$inf_pce <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    # Both are price INDICES — compute YoY % for each
    for (pair in list(
      list("PCEPI", "PCE YoY", "#7c5cbf"),
      list("PCEPILFE", "Core PCE YoY", "#00b4d8")
    )) {
      df <- rv$fred_data[[pair[[1]]]]
      if (!is.null(df) && nrow(df) > 12) {
        d <- df %>% yoy_change() %>% filter(!is.na(yoy)) %>% tail(84)
        p <- p %>%
          add_lines(
            data = d,
            x = ~date,
            y = ~yoy,
            line = list(color = pair[[3]], width = 2.5),
            name = pair[[2]],
            hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
          )
      }
    }
    # Fed 2% target line
    p %>%
      add_segments(
        x = as.Date("2018-01-01"),
        xend = Sys.Date(),
        y = 2,
        yend = 2,
        line = list(color = "#ffffff44", width = 1, dash = "dot"),
        name = "2% Target",
        showlegend = TRUE,
        hoverinfo = "none"
      ) %>%
      dark_theme(y = "YoY %")
  })

  output$inf_ppi <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    for (pair in list(
      list("PPIACO", "PPI YoY", "#e94560"),
      list("CPIAUCSL", "CPI YoY", "#f4a261")
    )) {
      df <- rv$fred_data[[pair[[1]]]]
      if (!is.null(df) && nrow(df) > 12) {
        d <- df %>% yoy_change() %>% filter(!is.na(yoy)) %>% tail(84)
        p <- p %>%
          add_lines(
            data = d,
            x = ~date,
            y = ~yoy,
            line = list(color = pair[[3]], width = 2.5),
            name = pair[[2]],
            hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
          )
      }
    }
    p %>% dark_theme(y = "YoY %")
  })

  output$inf_compare <- renderPlotly({
    req(rv$infl_panel)
    if (nrow(rv$infl_panel) == 0) {
      return(empty_plot("BLS/FRED data unavailable"))
    }
    cols <- c(
      "CPI-U YoY (BLS)" = "#00b4d8",
      "CPI-U YoY (FRED)" = "#e94560",
      "Core CPI YoY" = "#f4a261",
      "PCE YoY" = "#7c5cbf"
    )
    p <- plot_ly()
    for (s in unique(rv$infl_panel$series)) {
      d <- rv$infl_panel %>% filter(series == s) %>% tail(84)
      p <- p %>%
        add_lines(
          data = d,
          x = ~date,
          y = ~value,
          line = list(color = cols[s] %||% "#aaaaaa", width = 2),
          name = s,
          hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
        )
    }
    p %>% dark_theme(y = "YoY %")
  })

  output$tbl_inflation <- renderDT({
    req(rv$fred_data)
    rows <- map_dfr(
      c("CPIAUCSL", "CPILFESL", "PCEPI", "PCEPILFE", "PPIACO"),
      function(sid) {
        df <- rv$fred_data[[sid]]
        if (is.null(df) || nrow(df) < 13) {
          return(NULL)
        }
        # Show YoY % (not raw index level) for all inflation series
        d_yoy <- df %>% yoy_change() %>% filter(!is.na(yoy))
        if (nrow(d_yoy) == 0) {
          return(NULL)
        }
        latest_yoy <- tail(d_yoy, 1)
        tibble(
          Series = FRED_CFG[[sid]]$label %||% sid,
          Date = format(latest_yoy$date, "%b %Y"),
          `YoY %` = round(latest_yoy$yoy, 2),
          Note = "Year-over-Year Change"
        )
      }
    )
    datatable(
      rows,
      options = list(dom = "t"),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatStyle(
        "YoY %",
        color = styleInterval(c(2, 4), c("#2dce89", "#f4a261", "#e94560"))
      ) %>%
      formatStyle(
        columns = seq_len(ncol(rows)),
        backgroundColor = "#1e2640",
        color = "#d0d0d0"
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # HOUSING CHARTS
  # ═══════════════════════════════════════════════════════════════════════════

  output$hous_starts <- renderPlotly({
    req(rv$fred_data)
    p <- plot_ly()
    if (!is.null(rv$fred_data$HOUST)) {
      df <- rv$fred_data$HOUST %>% tail(120)
      p <- p %>%
        add_lines(
          data = df,
          x = ~date,
          y = ~value,
          line = list(color = "#2dce89", width = 2.5),
          name = "Housing Starts",
          hovertemplate = "%{x|%b %Y}: %{y:,.0f}K<extra></extra>"
        )
    }
    if (!is.null(rv$fred_data$PERMIT)) {
      df <- rv$fred_data$PERMIT %>% tail(120)
      p <- p %>%
        add_lines(
          data = df,
          x = ~date,
          y = ~value,
          line = list(color = "#00b4d8", width = 2, dash = "dash"),
          name = "Building Permits",
          hovertemplate = "%{x|%b %Y}: %{y:,.0f}K<extra></extra>"
        )
    }
    p %>% dark_theme(y = "K (annualized)")
  })

  output$hous_mortgage <- renderPlotly({
    req(rv$fred_data$MORTGAGE30US)
    df <- rv$fred_data$MORTGAGE30US %>% tail(240)
    plot_ly(
      df,
      x = ~date,
      y = ~value,
      type = "scatter",
      mode = "lines",
      line = list(color = "#e94560", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(233,69,96,0.08)",
      name = "30-Yr Fixed",
      hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
    ) %>%
      dark_theme(y = "%")
  })

  output$hous_nexus <- renderPlotly({
    req(rv$fred_data)
    if (is.null(rv$fred_data$MORTGAGE30US) || is.null(rv$fred_data$HOUST)) {
      return(empty_plot("Data unavailable"))
    }
    mort <- rv$fred_data$MORTGAGE30US %>%
      tail(120) %>%
      select(date, mort = value)
    start <- rv$fred_data$HOUST %>% tail(120) %>% select(date, starts = value)
    df <- inner_join(mort, start, by = "date") %>%
      filter(!is.na(mort), !is.na(starts))

    plot_ly(df) %>%
      add_lines(
        x = ~date,
        y = ~mort,
        name = "Mortgage Rate (%)",
        line = list(color = "#e94560", width = 2.5),
        hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
      ) %>%
      add_lines(
        x = ~date,
        y = ~starts,
        name = "Housing Starts (K)",
        line = list(color = "#2dce89", width = 2.5),
        yaxis = "y2",
        hovertemplate = "%{x|%b %Y}: %{y:,.0f}K<extra></extra>"
      ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#cccccc"),
        yaxis = list(
          title = "Mortgage Rate (%)",
          gridcolor = "#2a3042",
          color = "#e94560"
        ),
        yaxis2 = list(
          title = "Housing Starts (K)",
          overlaying = "y",
          side = "right",
          gridcolor = "transparent",
          color = "#2dce89"
        ),
        legend = list(
          font = list(color = "#cccccc"),
          bgcolor = "rgba(0,0,0,0)"
        ),
        margin = list(t = 40, r = 80, b = 50, l = 70),
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # CONSUMER CHARTS
  # ═══════════════════════════════════════════════════════════════════════════

  output$con_retail <- renderPlotly({
    req(rv$fred_data$RSAFS)
    df <- rv$fred_data$RSAFS %>%
      yoy_change() %>%
      filter(!is.na(yoy)) %>%
      tail(84)
    plot_ly(
      df,
      x = ~date,
      y = ~yoy,
      type = "bar",
      marker = list(color = if_else(df$yoy >= 0, "#00b4d8", "#e94560")),
      name = "Retail Sales YoY",
      hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>"
    ) %>%
      dark_theme(y = "YoY %")
  })

  output$con_sent <- renderPlotly({
    req(rv$fred_data$UMCSENT)
    df <- rv$fred_data$UMCSENT %>% tail(120)
    plot_ly(
      df,
      x = ~date,
      y = ~value,
      type = "scatter",
      mode = "lines",
      line = list(color = "#7c5cbf", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(124,92,191,0.08)",
      name = "Consumer Sentiment",
      hovertemplate = "%{x|%b %Y}: %{y:.1f}<extra></extra>"
    ) %>%
      dark_theme(y = "Index")
  })

  output$con_indpro <- renderPlotly({
    req(rv$fred_data$INDPRO)
    df <- rv$fred_data$INDPRO %>%
      yoy_change() %>%
      filter(!is.na(yoy)) %>%
      tail(84)
    plot_ly(
      df,
      x = ~date,
      y = ~yoy,
      type = "scatter",
      mode = "lines",
      line = list(color = "#2dce89", width = 2.5),
      fill = "tozeroy",
      fillcolor = "rgba(45,206,137,0.08)",
      name = "Industrial Production YoY",
      hovertemplate = "%{x|%b %Y}: %{y:.2f}%<extra></extra>"
    ) %>%
      dark_theme(y = "YoY %")
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # MARKETS
  # ═══════════════════════════════════════════════════════════════════════════

  output$tbl_mkt <- renderDT({
    req(rv$mkt_returns)
    tbl <- market_summary_table(rv$mkt_returns, input$mkt_grp %||% "equity")
    if (is.null(tbl)) {
      return(datatable(data.frame()))
    }
    datatable(
      tbl,
      options = list(pageLength = 15, dom = "ft"),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatRound(c("Price"), 2) %>%
      formatRound(c("1D %", "5D %", "1M %", "3M %", "YTD %"), 2) %>%
      formatStyle("1D %", color = styleInterval(0, c("#e94560", "#2dce89"))) %>%
      formatStyle("1M %", color = styleInterval(0, c("#e94560", "#2dce89"))) %>%
      formatStyle(
        "YTD %",
        color = styleInterval(0, c("#e94560", "#2dce89"))
      ) %>%
      formatStyle(
        columns = names(tbl),
        backgroundColor = "#1e2640",
        color = "#d0d0d0"
      )
  })

  output$mkt_price_chart <- renderPlotly({
    req(rv$mkt_data, input$mkt_tickers)
    tickers <- input$mkt_tickers
    cutoff <- Sys.Date() - input$mkt_lookback
    df <- rv$mkt_data %>%
      filter(symbol %in% tickers, date >= cutoff) %>%
      group_by(symbol) %>%
      mutate(idx = close / first(close) * 100) %>%
      ungroup()
    if (nrow(df) == 0) {
      return(empty_plot("No data for selected tickers"))
    }
    pal <- c(
      "#00b4d8",
      "#e94560",
      "#2dce89",
      "#f4a261",
      "#7c5cbf",
      "#a8dadc",
      "#ff6b6b",
      "#ffd93d"
    )
    p <- plot_ly()
    for (i in seq_along(tickers)) {
      t <- tickers[i]
      sub <- df %>% filter(symbol == t)
      p <- p %>%
        add_lines(
          data = sub,
          x = ~date,
          y = ~idx,
          line = list(color = pal[((i - 1) %% 8) + 1], width = 2.2),
          name = TICKER_LABELS[t] %||% t,
          hovertemplate = paste0(t, ": %{y:.1f}<extra></extra>")
        )
    }
    p %>%
      add_segments(
        x = min(df$date),
        xend = max(df$date),
        y = 100,
        yend = 100,
        line = list(color = "#ffffff33", width = 1, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      dark_theme(y = "Indexed (100 = Start)")
  })

  output$mkt_sector_heat <- renderPlotly({
    req(rv$mkt_returns)
    df <- sector_heatmap_data(rv$mkt_returns)
    if (is.null(df) || nrow(df) == 0) {
      return(empty_plot("Sector data unavailable"))
    }
    plot_ly(
      df,
      x = ~name,
      y = ~ret_1m,
      type = "bar",
      marker = list(color = ~colour),
      text = ~ paste0(round(ret_1m, 1), "%"),
      textposition = "outside",
      hovertemplate = "%{x}: %{y:.2f}%<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#cccccc"),
        xaxis = list(title = "", gridcolor = "#2a3042", color = "#9aa3b2"),
        yaxis = list(
          title = "1-Month Return %",
          gridcolor = "#2a3042",
          color = "#9aa3b2",
          zeroline = TRUE,
          zerolinecolor = "#ffffff55"
        ),
        margin = list(t = 30, r = 20, b = 80, l = 60),
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$mkt_commodities <- renderPlotly({
    req(rv$mkt_data)
    comm_tickers <- c("GLD", "USO", "SLV")
    avail <- comm_tickers[comm_tickers %in% unique(rv$mkt_data$symbol)]
    if (length(avail) == 0) {
      return(empty_plot("Commodity ETF data unavailable"))
    }
    df <- rv$mkt_data %>%
      filter(symbol %in% avail, date >= Sys.Date() - 365) %>%
      group_by(symbol) %>%
      mutate(idx = close / first(close) * 100) %>%
      ungroup()
    cols <- c(GLD = "#f4a261", USO = "#e94560", SLV = "#9aa3b2")
    p <- plot_ly()
    for (t in avail) {
      sub <- df %>% filter(symbol == t)
      p <- p %>%
        add_lines(
          data = sub,
          x = ~date,
          y = ~idx,
          line = list(color = cols[t] %||% "#aaa", width = 2.5),
          name = TICKER_LABELS[t] %||% t,
          hovertemplate = "%{x|%b %d}: %{y:.1f}<extra></extra>"
        )
    }
    p %>% dark_theme(y = "Indexed (100=1Y ago)")
  })

  output$mkt_yield_curve <- renderPlotly({
    req(rv$fred_data)
    # Fetch yield curve tenors directly
    tenors <- c(
      "DGS1MO",
      "DGS3MO",
      "DGS6MO",
      "DGS1",
      "DGS2",
      "DGS3",
      "DGS5",
      "DGS7",
      "DGS10",
      "DGS20",
      "DGS30"
    )
    labels <- c(
      "1M",
      "3M",
      "6M",
      "1Y",
      "2Y",
      "3Y",
      "5Y",
      "7Y",
      "10Y",
      "20Y",
      "30Y"
    )

    vals <- vapply(
      tenors,
      function(t) {
        df <- rv$fred_data[[t]]
        if (!is.null(df) && nrow(df) > 0) tail(df$value, 1) else NA_real_
      },
      numeric(1)
    )

    df_yc <- tibble(tenor = factor(labels, levels = labels), yield = vals) %>%
      filter(!is.na(yield))
    if (nrow(df_yc) < 3) {
      return(empty_plot(
        "Yield curve data incomplete (add DGS tenor series to FRED_CFG)"
      ))
    }

    plot_ly(
      df_yc,
      x = ~tenor,
      y = ~yield,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#2dce89", width = 2.5),
      marker = list(color = "#2dce89", size = 8),
      hovertemplate = "%{x}: %{y:.2f}%<extra></extra>",
      name = "Yield Curve"
    ) %>%
      dark_theme(y = "%")
  })

  output$mkt_corr <- renderPlotly({
    req(rv$fred_data)
    sids <- intersect(
      c(
        "UNRATE",
        "CPIAUCSL",
        "FEDFUNDS",
        "DGS10",
        "DCOILWTICO",
        "VIXCLS",
        "RSAFS",
        "MORTGAGE30US"
      ),
      names(rv$fred_data)
    )
    sids <- sids[!sapply(sids, function(s) is.null(rv$fred_data[[s]]))]
    if (length(sids) < 3) {
      return(empty_plot("Insufficient data for correlation"))
    }

    cor_mat <- fred_correlation_matrix(rv$fred_data, sids)
    labs <- sapply(sids, function(s) FRED_CFG[[s]]$label %||% s)

    plot_ly(
      x = labs,
      y = labs,
      z = cor_mat,
      type = "heatmap",
      colorscale = list(c(0, "#e94560"), c(0.5, "#1e2640"), c(1, "#00b4d8")),
      zmin = -1,
      zmax = 1,
      hovertemplate = "%{x} × %{y}: %{z:.2f}<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#cccccc", size = 11),
        xaxis = list(tickangle = -30, gridcolor = "#2a3042"),
        yaxis = list(gridcolor = "#2a3042"),
        margin = list(t = 40, r = 40, b = 100, l = 130)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # METRO MAP
  # ═══════════════════════════════════════════════════════════════════════════

  observeEvent(input$tabs, {
    if (input$tabs == "metro") {
      # Load state choropleth data (geometry-bearing sf)
      if (is.null(rv$census_data)) {
        withProgress(
          message = "Loading state ACS data from Census API…",
          value = 0,
          {
            incProgress(0.1, detail = "Connecting to Census API…")
            rv$census_data <- fetch_state_acs()
            incProgress(0.5, detail = "State data loaded. Fetching metro data…")
            rv$metro_data <- fetch_metro_acs()
            incProgress(1.0)
          }
        )
      }
    }
  })

  output$metro_map <- renderLeaflet({
    # Show a loading placeholder until census data is ready
    if (is.null(rv$census_data)) {
      return(
        leaflet() %>%
          addProviderTiles(
            "CartoDB.DarkMatter",
            options = providerTileOptions(opacity = 0.95)
          ) %>%
          setView(lng = -96, lat = 38, zoom = 4) %>%
          addControl(
            html = "<div style='background:#1e2640;color:#9aa3b2;padding:10px 14px;
                    border-radius:6px;font-size:13px;'>
                    &#9203; Loading Census data&hellip; (first load ~20s)</div>",
            position = "topright"
          )
      )
    }
    req(input$map_var)
    map_out <- tryCatch(
      build_state_map(rv$census_data, variable = input$map_var),
      error = function(e) {
        message("[Map error] ", e$message)
        NULL
      }
    )
    if (is.null(map_out)) {
      leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        setView(lng = -96, lat = 38, zoom = 4) %>%
        addControl(
          html = "<div style='background:#1e2640;color:#e94560;padding:10px 14px;
                  border-radius:6px;font-size:13px;'>
                  &#9888; Map failed &mdash; check R console</div>",
          position = "topright"
        )
    } else {
      map_out
    }
  })

  # State rankings table
  output$tbl_metro <- renderDT({
    req(rv$census_data)
    df <- rv$census_data %>%
      sf::st_drop_geometry() %>%
      transmute(
        State = state,
        `Unemp Rate %` = round(unemp_rate, 1),
        `Poverty Rate %` = round(poverty_rate, 1),
        `LFP Rate %` = round(lfp_rate, 1),
        `Median Income` = scales::dollar(med_income, accuracy = 1),
        `Median Home` = scales::dollar(med_home_val, accuracy = 1),
        Population = scales::comma(pop)
      ) %>%
      arrange(`Unemp Rate %`)

    datatable(
      df,
      caption = "State Rankings (ACS 5-Year Estimates)",
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatStyle(
        "Unemp Rate %",
        color = styleInterval(c(4, 6), c("#2dce89", "#f4a261", "#e94560"))
      ) %>%
      formatStyle(
        columns = seq_len(7),
        backgroundColor = "#1e2640",
        color = "#d0d0d0"
      )
  })

  # Metro CBSA rankings table
  output$tbl_metro_cbsa <- renderDT({
    if (is.null(rv$metro_data) || nrow(rv$metro_data) == 0) {
      return(datatable(
        data.frame(Message = "Metro data not yet loaded or unavailable."),
        rownames = FALSE
      ))
    }
    df <- rv$metro_data %>%
      transmute(
        Metro = metro,
        `Unemp Rate %` = round(unemp_rate, 1),
        `Poverty Rate %` = round(poverty_rate, 1),
        `LFP Rate %` = round(lfp_rate, 1),
        `Median Income` = scales::dollar(med_income, accuracy = 1),
        `Median Home` = scales::dollar(med_home_val, accuracy = 1),
        Population = scales::comma(pop)
      ) %>%
      arrange(desc(as.numeric(gsub("[^0-9]", "", Population))))

    datatable(
      df,
      caption = "Major Metro Areas (250k+ pop.) — ACS 5-Year Estimates",
      options = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatStyle(
        "Unemp Rate %",
        color = styleInterval(c(4, 6), c("#2dce89", "#f4a261", "#e94560"))
      ) %>%
      formatStyle(
        columns = seq_len(7),
        backgroundColor = "#1e2640",
        color = "#d0d0d0"
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # FORECASTING
  # ═══════════════════════════════════════════════════════════════════════════

  observeEvent(
    input$fcst_horizon,
    {
      req(rv$fred_data)
      withProgress(message = "Re-running forecasts…", value = 0, {
        rv$forecasts <- run_all_forecasts(
          rv$fred_data,
          horizon_months = input$fcst_horizon
        )
        incProgress(1)
      })
    },
    ignoreInit = TRUE
  )

  output$fcst_chart <- renderPlotly({
    req(rv$forecasts, input$fcst_series)
    fc <- rv$forecasts[[input$fcst_series]]
    plot_forecast_chart(fc, input$fcst_series)
  })

  output$tbl_fcst_summary <- renderDT({
    req(rv$forecasts)
    tbl <- forecast_summary_table(rv$forecasts)
    if (is.null(tbl)) {
      return(datatable(data.frame()))
    }
    datatable(
      tbl,
      options = list(dom = "t", pageLength = 20),
      rownames = FALSE,
      class = "table-dark compact stripe"
    ) %>%
      formatStyle(
        columns = names(tbl),
        backgroundColor = "#1e2640",
        color = "#d0d0d0"
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # AI INSIGHTS
  # ═══════════════════════════════════════════════════════════════════════════

  # Update model list when provider changes
  observeEvent(
    input$llm_provider,
    {
      prov <- input$llm_provider %||% ACTIVE_PROVIDER
      models <- if (!is.null(prov) && prov %in% names(LLM_PROVIDERS)) {
        LLM_PROVIDERS[[prov]]$models
      } else {
        character(0)
      }
      updateSelectInput(
        session,
        "llm_model",
        choices = models,
        selected = models[1]
      )
    },
    ignoreNULL = FALSE
  )

  output$llm_status_badge <- renderUI({
    prov <- input$llm_provider %||% ACTIVE_PROVIDER
    if (is.null(prov)) {
      span(class = "status-badge badge-red", icon("times"), " No LLM")
    } else {
      key_ok <- nchar(Sys.getenv(LLM_PROVIDERS[[prov]]$key_env)) > 0
      if (key_ok) {
        span(class = "status-badge badge-green", icon("check"), " Ready")
      } else {
        span(class = "status-badge badge-red", icon("key"), " No Key")
      }
    }
  })

  output$provider_badge_sidebar <- renderUI({
    prov <- ACTIVE_PROVIDER
    if (is.null(prov)) {
      div(
        style = "margin-top:8px;",
        span(
          style = "color:#e94560;font-size:11px;",
          icon("exclamation-triangle"),
          " LLM not configured"
        )
      )
    } else {
      div(
        style = "margin-top:8px;",
        span(
          class = "provider-badge",
          icon("robot"),
          " ",
          LLM_PROVIDERS[[prov]]$display_name
        )
      )
    }
  })

  # Generate full AI insights
  observeEvent(input$btn_generate, {
    req(rv$kpis)

    rv$llm_text <- NULL

    withProgress(message = "Generating AI insights…", value = 0.1, {
      incProgress(0.3, detail = "Preparing economic context…")
      result <- tryCatch(
        generate_insights(
          kpis = rv$kpis,
          fred_data = rv$fred_data,
          mkt_returns = rv$mkt_returns,
          news_df = rv$news_data,
          provider = input$llm_provider,
          model = input$llm_model
        ),
        error = function(e) paste0("Error: ", e$message)
      )
      incProgress(0.6, detail = "Rendering…")
      rv$llm_text <- result
      incProgress(1)
    })
  })

  output$llm_output <- renderUI({
    if (is.null(rv$llm_text)) {
      div(
        style = "color:#6b7585;font-size:13px;padding:20px;text-align:center;",
        icon(
          "robot",
          style = "font-size:32px;color:#2a3042;margin-bottom:12px;display:block;"
        ),
        "Click \"Generate AI Insights\" to synthesise live economic data and headlines.",
        br(),
        "The AI will provide macro analysis, scenario modelling, and risk assessment."
      )
    } else {
      div(
        id = "llm_output",
        HTML(markdown::markdownToHTML(text = rv$llm_text, fragment.only = TRUE))
      )
    }
  })

  # Quick Q&A
  observeEvent(input$btn_chat, {
    req(nchar(input$chat_q) > 3, rv$kpis)
    rv$chat_text <- NULL
    withProgress(message = "Thinking…", value = 0.5, {
      rv$chat_text <- tryCatch(
        economic_chat(
          question = input$chat_q,
          kpis = rv$kpis,
          news_df = rv$news_data,
          provider = input$llm_provider,
          model = input$llm_model
        ),
        error = function(e) paste0("Error: ", e$message)
      )
    })
  })

  output$chat_answer <- renderUI({
    if (is.null(rv$chat_text)) {
      return(NULL)
    }
    div(
      style = "background:#1e2640;border-radius:8px;padding:14px;margin-top:10px;font-size:13px;line-height:1.7;",
      HTML(markdown::markdownToHTML(text = rv$chat_text, fragment.only = TRUE))
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # NEWS
  # ═══════════════════════════════════════════════════════════════════════════

  output$news_brief <- renderUI({
    # render_news_html handles NULL gracefully — don't use req() here
    render_news_html(rv$news_data, n = 8)
  })

  output$news_full <- renderUI({
    render_news_html(rv$news_data, n = 20)
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # SETTINGS: API STATUS TABLE
  # ═══════════════════════════════════════════════════════════════════════════

  output$tbl_api_status <- renderTable(
    {
      apis <- list(
        "FRED" = "FRED_API_KEY",
        "BLS" = "BLS_API_KEY",
        "Census" = "CENSUS_API_KEY",
        "Groq (LLM)" = "GROQ_API_KEY",
        "OpenRouter" = "OPENROUTER_API_KEY",
        "Together AI" = "TOGETHER_API_KEY",
        "NewsAPI" = "NEWS_API_KEY"
      )
      rows <- map_dfr(names(apis), function(name) {
        key_val <- Sys.getenv(apis[[name]])
        configured <- nchar(key_val) > 0
        tibble(
          Service = name,
          Variable = apis[[name]],
          Status = if (configured) "✅ Configured" else "❌ Not Set",
          Required = if (name %in% c("FRED", "BLS")) "Yes" else "Recommended"
        )
      })
      rows
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    rownames = FALSE,
    na = "—"
  )
}
