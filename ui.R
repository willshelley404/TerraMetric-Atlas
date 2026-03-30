# ─────────────────────────────────────────────────────────────────────────────
# ui.R — EconPulse AI Dashboard UI
# ─────────────────────────────────────────────────────────────────────────────

kpi_card <- function(value_id, change_id = NULL, title, icon_name, col = "#00b4d8") {
  div(class = "metric-card",
    div(class = "metric-label",
        tags$i(class = paste0("fa fa-", icon_name), style = paste0("color:", col, "; margin-right:6px;")),
        title),
    div(class = "metric-value", textOutput(value_id, inline = TRUE)),
    if (!is.null(change_id))
      div(class = "metric-change", htmlOutput(change_id, inline = TRUE))
  )
}

ui <- dashboardPage(
  skin = "black",

  # ── HEADER ────────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = span(span(class = "live-indicator"), "EconPulse AI"),
    tags$li(class = "dropdown", style = "padding:10px 14px;",
      span(style = "color:#9aa3b2; font-size:12px;",
           "Updated: ", textOutput("last_updated", inline = TRUE))
    ),
    tags$li(class = "dropdown", style = "padding:7px 10px;",
      actionButton("refresh_data", NULL, icon = icon("sync-alt"),
                   class = "btn-sm btn-primary-custom", title = "Refresh all data")
    )
  ),

  # ── SIDEBAR ───────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 215,
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap"),
      useShinyjs(),
      useWaiter()
    ),
    sidebarMenu(id = "tabs",
      menuItem("Overview",       tabName = "overview",    icon = icon("tachometer-alt")),
      menuItem("Labor Market",   tabName = "labor",       icon = icon("briefcase")),
      menuItem("Inflation",      tabName = "inflation",   icon = icon("percent")),
      menuItem("Housing",        tabName = "housing",     icon = icon("home")),
      menuItem("Consumer",       tabName = "consumer",    icon = icon("shopping-cart")),
      menuItem("Markets",        tabName = "markets",     icon = icon("chart-line")),
      menuItem("Metro Map",      tabName = "metro",       icon = icon("map-marked-alt")),
      menuItem("Forecasting",    tabName = "forecasting", icon = icon("magic")),
      menuItem("AI Insights",    tabName = "ai",          icon = icon("robot")),
      menuItem("Settings",       tabName = "settings",    icon = icon("cog"))
    ),
    div(style = "padding:14px 16px; border-top:1px solid #2a3042; margin-top:10px;",
      div(style = "font-size:10px;color:#6b7585;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;",
          "Data Sources"),
      div(style = "font-size:11px;color:#9aa3b2;line-height:2;",
          "FRED · BLS · Yahoo Finance", tags$br(),
          "Census ACS · NewsAPI"
      ),
      uiOutput("provider_badge_sidebar")
    )
  ),

  # ── BODY ──────────────────────────────────────────────────────────────────────
  dashboardBody(
    tabItems(

      # ══════════════════════════════════════════════════════
      # OVERVIEW TAB
      # ══════════════════════════════════════════════════════
      tabItem("overview",
        fluidRow(
          column(2, kpi_card("kpi_unemp",    "kpi_unemp_chg",    "Unemployment",     "users",       "#00b4d8")),
          column(2, kpi_card("kpi_cpi",      "kpi_cpi_chg",      "CPI YoY",          "fire",        "#e94560")),
          column(2, kpi_card("kpi_fedfunds", "kpi_fedfunds_chg", "Fed Funds",        "university",  "#f4a261")),
          column(2, kpi_card("kpi_payrolls", "kpi_payrolls_chg", "Payrolls Chg",     "plus-circle", "#2dce89")),
          column(2, kpi_card("kpi_oil",      "kpi_oil_chg",      "WTI Oil",          "oil-can",     "#f4a261")),
          column(2, kpi_card("kpi_vix",      "kpi_vix_chg",      "VIX",              "chart-bar",   "#7c5cbf"))
        ),
        fluidRow(
          column(2, kpi_card("kpi_t10yr",    "kpi_t10yr_chg",    "10Y Treasury",     "percent",     "#00b4d8")),
          column(2, kpi_card("kpi_t10y2y",   "kpi_t10y2y_chg",   "10Y–2Y Spread",    "arrows-h",    "#e94560")),
          column(2, kpi_card("kpi_mortgage", "kpi_mortgage_chg", "Mortgage 30Y",     "home",        "#f4a261")),
          column(2, kpi_card("kpi_housing",  "kpi_housing_chg",  "Housing Starts",   "building",    "#2dce89")),
          column(2, kpi_card("kpi_consump",  "kpi_consump_chg",  "Consumer Sent.",   "smile",       "#00b4d8")),
          column(2, kpi_card("kpi_jolts",    "kpi_jolts_chg",    "Job Openings",     "search",      "#7c5cbf"))
        ),
        fluidRow(
          box(title = "Unemployment Rate", status = "primary", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("ov_unemp",  height = "240px"), type=4, color="#00b4d8", color.background="#161b27", size=0.7)),
          box(title = "Inflation (CPI & Core, YoY %)", status = "danger", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("ov_cpi",    height = "240px"), type=4, color="#e94560", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "Interest Rates", status = "warning", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("ov_rates",  height = "240px"), type=4, color="#f4a261", color.background="#161b27", size=0.7)),
          box(title = "Market Returns — Indexed (1Y)", status = "info", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("ov_market", height = "240px"), type=4, color="#7c5cbf", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(
            title        = tagList(
              tags$i(class="fa fa-chart-line", style="color:#00b4d8;margin-right:8px;"),
              "Live Data Synopsis"
            ),
            status       = "primary",
            solidHeader  = TRUE,
            width        = 12,
            collapsible  = TRUE,
            collapsed    = FALSE,
            withSpinner(
              uiOutput("synopsis_panel"),
              type  = 4,
              color = "#00b4d8",
              color.background = "#161b27",
              size  = 0.7
            )
          )
        ),
        fluidRow(
          box(
            title = tagList(
              tags$i(class="fa fa-brain", style="color:#f4a261;margin-right:8px;"),
              "So What? — 6–12 Month GDP Growth Outlook"
            ),
            status      = "warning",
            solidHeader = TRUE,
            width       = 12,
            collapsible = TRUE,
            collapsed   = FALSE,
            div(style = "color:#9aa3b2;font-size:11px;margin-bottom:12px;",
                "Composite scoring model across 6 macro dimensions. ",
                "Each dimension scores –1 (negative impulse) to +1 (positive). ",
                "Weighted sum → growth regime + GDP range estimate. No LLM — computed from live data."
            ),
            withSpinner(
              uiOutput("growth_outlook_panel"),
              type  = 4,
              color = "#f4a261",
              color.background = "#161b27",
              size  = 0.7
            )
          )
        ),
        fluidRow(
          box(title = "Latest Economic Headlines", status = "primary", solidHeader = TRUE, width = 12,
              htmlOutput("news_brief"))
        )
      ),

      # ══════════════════════════════════════════════════════
      # LABOR TAB
      # ══════════════════════════════════════════════════════
      tabItem("labor",
        uiOutput("labor_synopsis"),
        fluidRow(
          box(title = "Unemployment Rate", status = "primary", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("lab_unemp",    height = "280px"), type=4, color="#00b4d8", color.background="#161b27", size=0.7)),
          box(title = "Nonfarm Payrolls — MoM Change", status = "success", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("lab_payrolls", height = "280px"), type=4, color="#2dce89", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "JOLTS: Job Openings & Quits", status = "info", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("lab_jolts",    height = "280px"), type=4, color="#7c5cbf", color.background="#161b27", size=0.7)),
          box(title = "Average Hourly Earnings (YoY %)", status = "warning", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("lab_wages",    height = "280px"), type=4, color="#f4a261", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "Labor Market Data Table", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("tbl_labor"))
        )
      ),

      # ══════════════════════════════════════════════════════
      # INFLATION TAB
      # ══════════════════════════════════════════════════════
      tabItem("inflation",
        uiOutput("inflation_synopsis"),
        fluidRow(
          box(title = "CPI & Core CPI (YoY %)", status = "danger", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("inf_cpi",     height = "280px"), type=4, color="#e94560", color.background="#161b27", size=0.7)),
          box(title = "PCE & Core PCE (Fed's Preferred)", status = "warning", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("inf_pce",     height = "280px"), type=4, color="#f4a261", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "PPI vs CPI — Input vs Output Prices", status = "info", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("inf_ppi",     height = "280px"), type=4, color="#7c5cbf", color.background="#161b27", size=0.7)),
          box(title = "Inflation Sources: BLS & FRED Combined", status = "primary", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("inf_compare", height = "280px"), type=4, color="#00b4d8", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "Inflation Data Table", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("tbl_inflation"))
        )
      ),

      # ══════════════════════════════════════════════════════
      # HOUSING TAB
      # ══════════════════════════════════════════════════════
      tabItem("housing",
        uiOutput("housing_synopsis"),
        fluidRow(
          box(title = "Housing Starts & Building Permits", status = "success", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("hous_starts",  height = "300px"), type=4, color="#2dce89", color.background="#161b27", size=0.7)),
          box(title = "30-Year Mortgage Rate", status = "danger", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("hous_mortgage",height = "300px"), type=4, color="#e94560", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "Mortgage Rate vs Housing Starts (Dual Axis)", status = "warning", solidHeader = TRUE, width = 12,
              withSpinner(plotlyOutput("hous_nexus",   height = "300px"), type=4, color="#f4a261", color.background="#161b27", size=0.7))
        )
      ),

      # ══════════════════════════════════════════════════════
      # CONSUMER TAB
      # ══════════════════════════════════════════════════════
      tabItem("consumer",
        uiOutput("consumer_synopsis"),
        fluidRow(
          box(title = "Retail & Food Services Sales (YoY %)", status = "primary", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("con_retail",  height = "280px"), type=4, color="#00b4d8", color.background="#161b27", size=0.7)),
          box(title = "University of Michigan Consumer Sentiment", status = "info", solidHeader = TRUE, width = 6,
              withSpinner(plotlyOutput("con_sent",    height = "280px"), type=4, color="#7c5cbf", color.background="#161b27", size=0.7))
        ),
        fluidRow(
          box(title = "Industrial Production Index", status = "success", solidHeader = TRUE, width = 12,
              withSpinner(plotlyOutput("con_indpro",  height = "280px"), type=4, color="#2dce89", color.background="#161b27", size=0.7))
        )
      ),

      # ══════════════════════════════════════════════════════
      # MARKETS TAB
      # ══════════════════════════════════════════════════════
      tabItem("markets",
        uiOutput("markets_synopsis"),
        box(title = "Financial Market Data", status = "primary", solidHeader = TRUE, width = 12,
          tabsetPanel(
            tabPanel("Performance Table",
              br(),
              fluidRow(
                column(3, selectInput("mkt_grp", "Category:",
                  choices = c("Equity Indices"="equity","Sector ETFs"="sector",
                              "Commodities"="commodity","Bonds"="bond","All"="all"),
                  selected = "equity"))
              ),
              DTOutput("tbl_mkt")
            ),
            tabPanel("Price Chart",
              br(),
              fluidRow(
                column(6, selectInput("mkt_tickers", "Tickers (multi-select):", multiple = TRUE,
                  choices = setNames(ALL_TICKERS,
                                     paste0(ALL_TICKERS, " — ", ifelse(is.na(TICKER_LABELS[ALL_TICKERS]), ALL_TICKERS, TICKER_LABELS[ALL_TICKERS]))),
                  selected = c("SPY","QQQ","GLD","TLT"))),
                column(3, sliderInput("mkt_lookback", "Lookback (days):",
                  min=30, max=365, value=252, step=10))
              ),
              withSpinner(plotlyOutput("mkt_price_chart", height = "360px"),
                          type=4, color="#00b4d8", color.background="#161b27", size=0.7)
            ),
            tabPanel("Sector Heatmap",
              br(),
              withSpinner(plotlyOutput("mkt_sector_heat", height = "380px"),
                          type=4, color="#00b4d8", color.background="#161b27", size=0.7)
            ),
            tabPanel("Commodities",
              br(),
              withSpinner(plotlyOutput("mkt_commodities", height = "360px"),
                          type=4, color="#f4a261", color.background="#161b27", size=0.7)
            ),
            tabPanel("Yield Curve",
              br(),
              withSpinner(plotlyOutput("mkt_yield_curve", height = "360px"),
                          type=4, color="#2dce89", color.background="#161b27", size=0.7)
            ),
            tabPanel("Correlations",
              br(),
              withSpinner(plotlyOutput("mkt_corr", height = "420px"),
                          type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # METRO MAP TAB
      # ══════════════════════════════════════════════════════
      tabItem("metro",
        box(title = "Regional Economic Analysis — ACS Data", status = "primary",
            solidHeader = TRUE, width = 12,
          tabsetPanel(id = "metro_subtab",
            # ── Sub-tab 1: State choropleth ──────────────────────────────────
            tabPanel("State Choropleth",
              br(),
              fluidRow(
                column(3,
                  selectInput("map_var", "Map Variable:",
                    choices = c(
                      "Unemployment Rate"     = "unemp_rate",
                      "Poverty Rate"          = "poverty_rate",
                      "Labor Force Part. Rate"= "lfp_rate",
                      "Median HH Income"      = "med_income",
                      "Median Home Value"     = "med_home_val",
                      "Housing Vacancy Rate"  = "vacancy_rate"
                    ), selected = "unemp_rate"),
                  div(style = "background:#1e2640;border-radius:6px;padding:12px;margin-top:10px;",
                    div(style = "color:#9aa3b2;font-size:11px;line-height:1.9;",
                      icon("info-circle", style="color:#00b4d8;"),
                      " ACS 5-Year Estimates.", tags$br(),
                      "Click any state for details.", tags$br(),
                      "Hover for quick tooltip.", tags$br(),
                      "Scroll to zoom."
                    )
                  )
                ),
                column(9,
                  withSpinner(leafletOutput("metro_map", height = "480px"),
                              type=4, color="#00b4d8", color.background="#161b27", size=0.7)
                )
              ),
              hr(style = "border-color:#2a3042; margin-top:16px;"),
              div(style = "color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;",
                  "State Rankings"),
              DTOutput("tbl_metro")
            ),

            # ── Sub-tab 2: Metro bubble map ──────────────────────────────────
            tabPanel("Metro Bubble Map",
              br(),
              fluidRow(
                column(3,
                  selectInput("metro_map_var", "Map Variable:",
                    choices = c(
                      "Unemployment Rate"     = "unemp_rate",
                      "Poverty Rate"          = "poverty_rate",
                      "Labor Force Part. Rate"= "lfp_rate",
                      "Median HH Income"      = "med_income",
                      "Median Home Value"     = "med_home_val",
                      "Rent Burden Rate"      = "rent_burden_rate",
                      "Bachelor's+ Rate"      = "bachelors_plus_rate"
                    ), selected = "unemp_rate"),
                  sliderInput("metro_min_pop", "Min. Population (M):",
                              min = 0.1, max = 3, value = 0.5, step = 0.1),
                  div(style = "background:#1e2640;border-radius:6px;padding:12px;margin-top:10px;",
                    div(style = "color:#9aa3b2;font-size:11px;line-height:1.9;",
                      icon("circle", style="color:#00b4d8;"),
                      " Bubble size = population.", tags$br(),
                      " Colour = selected variable.", tags$br(),
                      " Click for full detail.", tags$br(),
                      " ACS 5-Year Estimates."
                    )
                  )
                ),
                column(9,
                  withSpinner(leafletOutput("metro_bubble_map", height = "480px"),
                              type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
                )
              ),
              hr(style = "border-color:#2a3042; margin-top:16px;"),
              # Metro AI Analysis
              box(title = tagList(icon("robot", style="color:#7c5cbf;"), " Metro AI Analysis"),
                  status = "info", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(4,
                    selectInput("metro_ai_select", "Select Metro for Analysis:",
                                choices = character(0)),
                    actionButton("btn_metro_ai", "Generate Metro Analysis",
                                 class="btn-ai", style="margin-top:8px;width:100%;"),
                    div(style="color:#9aa3b2;font-size:11px;margin-top:8px;",
                        "Synthesises ACS data + macro environment + news headlines into a structured regional economic assessment.")
                  ),
                  column(8,
                    withSpinner(uiOutput("metro_ai_output"),
                                type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
                  )
                )
              ),
              hr(style = "border-color:#2a3042; margin-top:16px;"),
              div(style = "color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;",
                  "Major Metro Areas (CBSAs, 250k+ population)"),
              withSpinner(DTOutput("tbl_metro_cbsa"),
                          type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
            )
          ) # end tabsetPanel
        )
      ),

      # ══════════════════════════════════════════════════════
      # FORECASTING TAB
      # ══════════════════════════════════════════════════════
      tabItem("forecasting",
        box(title = "18-Month Economic Forecasts — Meta Prophet", status = "info",
            solidHeader = TRUE, width = 12,
          fluidRow(
            column(4,
              selectInput("fcst_series", "Indicator:",
                choices = setNames(names(FORECAST_SERIES),
                                   vapply(names(FORECAST_SERIES), function(x) FORECAST_SERIES[[x]]$name, character(1))),
                selected = "UNRATE"),
              sliderInput("fcst_horizon", "Forecast Horizon (months):",
                          min = 6, max = 24, value = 18, step = 3),
              div(style = "background:#1e2640;border-radius:6px;padding:12px;margin-top:10px;",
                div(style = "color:#9aa3b2;font-size:11px;line-height:1.8;",
                  icon("info-circle", style="color:#00b4d8;"), " Prophet model with:", tags$br(),
                  "· Annual seasonality", tags$br(),
                  "· 90% confidence intervals", tags$br(),
                  "· Automatic changepoint detection", tags$br(),
                  "· Historical FRED data"
                )
              )
            ),
            column(8,
              withSpinner(plotlyOutput("fcst_chart", height = "380px"),
                          type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
            )
          ),
          hr(style = "border-color:#2a3042;"),
          div(style = "color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;margin-bottom:10px;",
              "All Forecast Endpoints"),
          withSpinner(DTOutput("tbl_fcst_summary"),
                      type=4, color="#7c5cbf", color.background="#161b27", size=0.7)
        )
      ),

      # ══════════════════════════════════════════════════════
      # AI INSIGHTS TAB
      # ══════════════════════════════════════════════════════
      tabItem("ai",
        fluidRow(
          # News panel (left)
          column(4,
            box(title = "Real-Time Economic News", status = "primary",
                solidHeader = TRUE, width = 12,
              div(style = "max-height:560px;overflow-y:auto;",
                htmlOutput("news_full")
              ),
              hr(style = "border-color:#2a3042;"),
              div(style = "color:#9aa3b2;font-size:10px;",
                  icon("info-circle"), " Top headlines by economic relevance score are passed to the AI as context.")
            )
          ),
          # AI panel (right)
          column(8,
            box(title = "AI Economic Intelligence", status = "info",
                solidHeader = TRUE, width = 12,
              # Provider & model selector row
              fluidRow(
                column(5,
                  div(style = "color:#9aa3b2;font-size:11px;margin-bottom:4px;", "LLM Provider"),
                  selectInput("llm_provider", NULL,
                    choices = setNames(names(LLM_PROVIDERS),
                                       vapply(names(LLM_PROVIDERS), function(x) LLM_PROVIDERS[[x]]$display_name, character(1))),
                    selected = ACTIVE_PROVIDER %||% "groq", width = "100%")
                ),
                column(5,
                  div(style = "color:#9aa3b2;font-size:11px;margin-bottom:4px;", "Model"),
                  selectInput("llm_model", NULL, choices = character(0), width = "100%")
                ),
                column(2,
                  div(style = "margin-top:22px;",
                    uiOutput("llm_status_badge"))
                )
              ),
              # Generate button
              div(style = "margin:12px 0;",
                actionButton("btn_generate", "🤖  Generate AI Insights",
                             class = "btn-ai")
              ),
              div(style = "color:#9aa3b2;font-size:11px;margin-bottom:10px;",
                  "Synthesises live FRED data + market returns + top news headlines → structured macro analysis + scenario modelling"),
              # Output area
              div(id = "llm_output_area",
                withSpinner(
                  uiOutput("llm_output"),
                  type = 4, color = "#7c5cbf", color.background = "#161b27", size = 0.7
                )
              ),
              hr(style = "border-color:#2a3042; margin:16px 0;"),
              # Quick Q&A
              div(style = "color:#e0e0e0;font-size:13px;font-weight:600;margin-bottom:10px;",
                  icon("comments", style="color:#00b4d8;"), " Ask the Economist"),
              fluidRow(
                column(9,
                  textInput("chat_q", NULL, placeholder = "E.g. 'How will tariffs affect inflation?' or 'Is a recession likely?'",
                            width = "100%")
                ),
                column(3,
                  actionButton("btn_chat", "Ask", class = "btn-primary-custom",
                               style = "width:100%;margin-top:2px;")
                )
              ),
              div(style = "min-height:40px;",
                withSpinner(uiOutput("chat_answer"),
                            type=4, color="#00b4d8", color.background="#161b27", size=0.5)
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════
      # SETTINGS TAB
      # ══════════════════════════════════════════════════════
      tabItem("settings",
        box(title = "Configuration", status = "primary", solidHeader = TRUE, width = 12,
          h4(style = "color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;",
             "Data Settings"),
          fluidRow(
            column(4, sliderInput("cfg_lookback", "Data Lookback (years):",
                                  min=2, max=10, value=7, step=1)),
            column(4, sliderInput("cfg_news_days", "News Display Window (days):",
                                  min=1, max=7, value=4, step=1)),
            column(4, sliderInput("cfg_forecast_horizon", "Forecast Horizon (months):",
                                  min=6, max=24, value=18, step=3))
          ),
          hr(style="border-color:#2a3042;"),
          h4(style="color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;",
             "LLM Settings"),
          fluidRow(
            column(4, selectInput("cfg_llm_provider", "Default Provider:",
              choices = setNames(names(LLM_PROVIDERS),
                                 vapply(names(LLM_PROVIDERS),
                                        function(x) LLM_PROVIDERS[[x]]$display_name, character(1))),
              selected = ACTIVE_PROVIDER %||% "groq")),
            column(4, uiOutput("cfg_llm_model_ui")),
            column(4,
              div(style="margin-top:28px;",
                actionButton("cfg_apply", "Apply & Refresh Data",
                             class="btn-primary-custom", icon=icon("sync-alt"))))
          ),
          hr(style="border-color:#2a3042;"),
          h4(style="color:#9aa3b2;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;",
             "News Sources"),
          div(style="background:#1e2640;border-radius:6px;padding:12px;color:#9aa3b2;font-size:12px;line-height:2;",
            icon("globe", style="color:#00b4d8;"), " Sources tried in order: ",
            tags$b("NewsAPI"), " → ", tags$b("MediaStack"), " → ", tags$b("GDELT")
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
)
