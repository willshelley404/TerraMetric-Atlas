# ─────────────────────────────────────────────────────────────────────────────
# global.R — Libraries, API config, constants, shared utilities
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny);         library(shinydashboard); library(shinyWidgets)
  library(shinycssloaders); library(shinyjs);      library(DT)
  library(plotly);        library(leaflet.extras)
  library(tidyverse);     library(lubridate);      library(scales)
  library(glue);          library(fredr);           library(tidyquant)
  library(tidycensus);    library(prophet);         library(httr2)
  library(jsonlite);      library(sf);              library(waiter)
  library(markdown)
})

# Load leaflet AFTER tidyquant/xts to avoid .xts_chob namespace collision.
# All leaflet calls in the app use leaflet:: prefix to be safe.
if (!requireNamespace("leaflet", quietly = TRUE)) stop("leaflet package required")
library(leaflet)

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ── API Keys ───────────────────────────────────────────────────────────────────
FRED_API_KEY   <- Sys.getenv("FRED_API_KEY")
BLS_API_KEY    <- Sys.getenv("BLS_API_KEY")
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")
NEWS_API_KEY   <- Sys.getenv("NEWS_API_KEY")

if (nchar(FRED_API_KEY) > 0) fredr_set_key(FRED_API_KEY)

# ── FRED Series Master ─────────────────────────────────────────────────────────
FRED_CFG <- list(
  GDPC1              = list(label="Real GDP",              unit="Bil.$",   tab="macro"),
  UNRATE             = list(label="Unemployment Rate",      unit="%",       tab="labor"),
  PAYEMS             = list(label="Nonfarm Payrolls",       unit="K",       tab="labor"),
  JTSJOL             = list(label="Job Openings",           unit="K",       tab="labor"),
  JTSQUL             = list(label="Quits Rate",             unit="K",       tab="labor"),
  CES0500000003      = list(label="Avg Hourly Earnings",    unit="$/hr",    tab="labor"),
  CPIAUCSL           = list(label="CPI All Items",          unit="Index",   tab="inflation"),
  CPILFESL           = list(label="Core CPI",               unit="Index",   tab="inflation"),
  PCEPI              = list(label="PCE Price Index",        unit="Index",   tab="inflation"),
  PCEPILFE           = list(label="Core PCE YoY",           unit="%",       tab="inflation"),
  PPIACO             = list(label="PPI All Commodities",    unit="Index",   tab="inflation"),
  FEDFUNDS           = list(label="Fed Funds Rate",         unit="%",       tab="monetary"),
  T10Y2Y             = list(label="10Y-2Y Yield Spread",    unit="%",       tab="monetary"),
  DGS10              = list(label="10-Year Treasury",       unit="%",       tab="monetary"),
  DGS2               = list(label="2-Year Treasury",        unit="%",       tab="monetary"),
  MORTGAGE30US       = list(label="30-Yr Mortgage Rate",    unit="%",       tab="housing"),
  HOUST              = list(label="Housing Starts",         unit="K ann.",  tab="housing"),
  PERMIT             = list(label="Building Permits",       unit="K ann.",  tab="housing"),
  RSAFS              = list(label="Retail Sales",           unit="M$",      tab="consumer"),
  UMCSENT            = list(label="Consumer Sentiment",     unit="Index",   tab="consumer"),
  INDPRO             = list(label="Industrial Production",  unit="Index",   tab="consumer"),
  DCOILWTICO         = list(label="WTI Crude Oil",          unit="$/bbl",   tab="markets"),
  BAMLH0A0HYM2       = list(label="HY Credit Spread",       unit="%",       tab="markets"),
  VIXCLS             = list(label="VIX",                    unit="Index",   tab="markets"),
  DTWEXBGS           = list(label="USD Index",              unit="Index",   tab="markets"),
  # Gold: London PM Fix (USD/troy oz) — use GOLDPMGBD228NLBM; AM fix deprecated on FRED
  GOLDPMGBD228NLBM   = list(label="Gold Price (London PM)",  unit="$/oz",    tab="markets"),
  # Yield curve tenors (for yield curve chart)
  DGS1MO             = list(label="1-Month Treasury",       unit="%",       tab="monetary"),
  DGS3MO             = list(label="3-Month Treasury",       unit="%",       tab="monetary"),
  DGS6MO             = list(label="6-Month Treasury",       unit="%",       tab="monetary"),
  DGS1               = list(label="1-Year Treasury",        unit="%",       tab="monetary"),
  DGS3               = list(label="3-Year Treasury",        unit="%",       tab="monetary"),
  DGS5               = list(label="5-Year Treasury",        unit="%",       tab="monetary"),
  DGS7               = list(label="7-Year Treasury",        unit="%",       tab="monetary"),
  DGS20              = list(label="20-Year Treasury",       unit="%",       tab="monetary"),
  DGS30              = list(label="30-Year Treasury",       unit="%",       tab="monetary")
)

FRED_IDS <- names(FRED_CFG)

# ── BLS Series ─────────────────────────────────────────────────────────────────
BLS_IDS <- c(
  "CUSR0000SA0", "CUSR0000SA0L1E", "WPSFD49207",
  "CES0500000003", "CES0000000001", "LNS14000000"
)
BLS_LABELS <- c(
  CUSR0000SA0    = "CPI-U All Items",
  CUSR0000SA0L1E = "CPI-U Core",
  WPSFD49207     = "PPI Finished Goods",
  CES0500000003  = "Avg Hourly Earnings",
  CES0000000001  = "Nonfarm Employment",
  LNS14000000    = "Unemployment Rate"
)

# ── Market Tickers ─────────────────────────────────────────────────────────────
EQUITY_TICKERS   <- c("SPY","QQQ","IWM","DIA")
SECTOR_TICKERS   <- c("XLF","XLE","XLK","XLV","XLI","XLY","XLP","XLB","XLRE","XLC")
COMMODITY_TICKERS<- c("GLD","USO","SLV")
BOND_TICKERS     <- c("TLT","IEF","HYG","LQD")
ALL_TICKERS      <- c(EQUITY_TICKERS, SECTOR_TICKERS, COMMODITY_TICKERS, BOND_TICKERS)

TICKER_LABELS <- c(
  SPY="S&P 500", QQQ="Nasdaq 100", IWM="Russell 2000", DIA="Dow Jones",
  XLF="Financials", XLE="Energy", XLK="Technology", XLV="Healthcare",
  XLI="Industrials", XLY="Cons. Disc.", XLP="Cons. Staples", XLB="Materials",
  XLRE="Real Estate", XLC="Comm. Svcs.",
  GLD="Gold ETF", USO="Oil ETF", SLV="Silver ETF",
  TLT="LT Treasury", IEF="7-10Y Tsy.", HYG="High Yield", LQD="Inv. Grade"
)

# ── LLM Providers ─────────────────────────────────────────────────────────────
LLM_PROVIDERS <- list(
  groq = list(
    base_url      = "https://api.groq.com/openai/v1",
    key_env       = "GROQ_API_KEY",
    # llama-3.3-70b trained on data through Dec 2024; fast & free
    default_model = "llama-3.3-70b-versatile",
    models        = c(
      "llama-3.3-70b-versatile",   # best quality, ~Dec 2024 knowledge
      "llama-3.1-8b-instant"        # fast, lower latency
    ),
    display_name  = "Groq"
  ),
  openrouter = list(
    base_url      = "https://openrouter.ai/api/v1",
    key_env       = "OPENROUTER_API_KEY",
    # Free-tier models as of early 2026; DeepSeek R1 has Jan 2025 knowledge
    default_model = "deepseek/deepseek-r1:free",
    models        = c(
      "deepseek/deepseek-r1:free",            # strong reasoning, ~Jan 2025
      "deepseek/deepseek-chat:free",           # faster DeepSeek V3
      "meta-llama/llama-3.3-70b-instruct:free" # fallback
    ),
    display_name  = "OpenRouter (Free)"
  ),
  gemini = list(
    base_url      = "https://generativelanguage.googleapis.com/v1beta/openai",
    key_env       = "GEMINI_API_KEY",
    # Gemini 2.0 Flash — free tier via Google AI Studio, ~early 2025 knowledge
    default_model = "gemini-2.0-flash",
    models        = c(
      "gemini-2.0-flash",     # fast, strong, free tier
      "gemini-1.5-flash"      # fallback
    ),
    display_name  = "Google Gemini"
  )
)

detect_llm_provider <- function() {
  forced <- Sys.getenv("LLM_PROVIDER")
  if (nchar(forced) > 0 && forced %in% names(LLM_PROVIDERS)) return(forced)
  # Auto-detect: prefer groq → openrouter → gemini
  for (p in c("groq","openrouter","gemini")) {
    if (p %in% names(LLM_PROVIDERS) &&
        nchar(Sys.getenv(LLM_PROVIDERS[[p]]$key_env)) > 0) return(p)
  }
  NULL
}

ACTIVE_PROVIDER <- detect_llm_provider()

# ── Colour Palette ─────────────────────────────────────────────────────────────
CLR <- list(
  accent="#00b4d8", success="#2dce89", danger="#e94560",
  warning="#f4a261", purple="#7c5cbf",
  bg="#0f1117", card="#161b27", border="#2a3042",
  text="#e0e0e0", muted="#9aa3b2"
)

# ── Plotly Dark Theme Helper ───────────────────────────────────────────────────
dark_theme <- function(p, title=NULL, x="", y="") {
  p %>%
    layout(
      title         = if (!is.null(title)) list(text=title, font=list(color="#e0e0e0",size=14)) else NULL,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color="#cccccc", family="Inter,Segoe UI,system-ui"),
      xaxis         = list(title=x, gridcolor="#2a3042", zerolinecolor="#3a4052", color="#9aa3b2"),
      yaxis         = list(title=y, gridcolor="#2a3042", zerolinecolor="#3a4052", color="#9aa3b2"),
      legend        = list(font=list(color="#cccccc"), bgcolor="rgba(0,0,0,0)"),
      margin        = list(t=45, r=20, b=50, l=60),
      hovermode     = "x unified"
    ) %>%
    config(displayModeBar=FALSE)
}

# ── Formatters ─────────────────────────────────────────────────────────────────
fmt_pct    <- function(x, d=2, sign=FALSE) {
  if (is.null(x)||is.na(x)) return("N/A")
  v <- round(x, d)
  paste0(if(sign&&v>0) "+" else "", v, "%")
}
fmt_num    <- function(x, d=1, sfx="", pfx="") {
  if (is.null(x)||is.na(x)) return("N/A")
  paste0(pfx, format(round(x,d), big.mark=","), sfx)
}
fmt_dollar <- function(x, d=2) {
  if (is.null(x)||is.na(x)) return("N/A")
  paste0("$", format(round(x,d), big.mark=","))
}
pct_change <- function(v) {
  n <- length(v)
  if (n < 2 || any(is.na(v[c(n-1,n)]))) return(NA_real_)
  (v[n]/v[n-1] - 1) * 100
}
yoy_change <- function(df, lag_n=12) {
  df %>% arrange(date) %>%
    mutate(yoy = (value/lag(value, lag_n) - 1)*100)
}

# ── Source modules ─────────────────────────────────────────────────────────────
for (f in list.files("R", pattern="\\.R$", full.names=TRUE)) source(f)

message("global.R loaded \u2713")
