# EconPulse AI — Real-Time Economic Intelligence Dashboard

A fully dynamic, AI-powered economic intelligence dashboard built with R Shiny.
Combines live FRED data, BLS statistics, Yahoo Finance markets, U.S. Census demographics,
real-time news, and free-tier LLM analysis into a single interactive platform.

---

## ✨ Features

| Module | Data Sources | Description |
|---|---|---|
| **Overview** | FRED, Yahoo Finance | 12 live KPI cards + 4 summary charts |
| **Labor Market** | FRED (UNRATE, PAYEMS, JOLTS, wages) | Unemployment, payrolls, openings, wage growth |
| **Inflation** | FRED + BLS | CPI, Core CPI, PCE, PPI — YoY comparisons |
| **Housing** | FRED | Starts, permits, mortgage rate nexus |
| **Consumer** | FRED | Retail sales, consumer sentiment, industrial production |
| **Markets** | Yahoo Finance (tidyquant) | Equity/sector ETFs, commodities, bonds, yield curve, correlations |
| **Metro Map** | U.S. Census ACS | Interactive choropleth map of state-level economic variables |
| **Forecasting** | FRED + Prophet | 18-month probabilistic forecasts with 90% CI |
| **AI Insights** | Groq / OpenRouter / Together AI + NewsAPI | LLM synthesis of data + current events |

---

## 🚀 Quick Start

### 1. Install dependencies

```r
source("install_packages.R")
```

### 2. Set up API keys

```bash
cp .Renviron.example .Renviron
# Edit .Renviron and fill in your keys
```

### 3. Launch the app

```r
shiny::runApp()
# or
shiny::runApp("app.R")
```

---

## 🔑 API Keys

All free to obtain:

| Service | URL | Required? |
|---|---|---|
| **FRED** | https://fred.stlouisfed.org/docs/api/api_key.html | ✅ Yes |
| **BLS** | https://www.bls.gov/developers/home.htm | ✅ Yes |
| **Census** | https://api.census.gov/data/key_signup.html | For Metro Map |
| **Groq** | https://console.groq.com | For AI (recommended) |
| **OpenRouter** | https://openrouter.ai | For AI (alternative) |
| **Together AI** | https://api.together.xyz | For AI (alternative) |
| **NewsAPI** | https://newsapi.org | For news feed |

> **Tip:** You only need ONE LLM key. Groq is fastest and most generous on free tier.

---

## 🏗️ Architecture

```
econ_dashboard/
├── app.R               # Entry point
├── global.R            # Libraries, constants, shared config
├── ui.R                # Shiny UI (10 tabs)
├── server.R            # Reactive server logic
├── R/
│   ├── data_fred.R     # FRED API integration
│   ├── data_bls.R      # BLS API v2 (direct HTTP)
│   ├── data_markets.R  # tidyquant / Yahoo Finance
│   ├── data_news.R     # NewsAPI + relevance scoring
│   ├── data_census.R   # tidycensus ACS + Leaflet maps
│   ├── llm_insights.R  # Provider-agnostic LLM calls
│   └── forecasting.R   # Prophet models + visualisation
├── www/
│   └── custom.css      # Dark theme stylesheet
└── install_packages.R  # One-time package installer
```

---

## 🤖 LLM Integration

The AI Insights tab is **provider-agnostic** — all providers use the same
OpenAI-compatible `/v1/chat/completions` endpoint:

```r
# Switching providers is a one-line change in .Renviron:
LLM_PROVIDER=groq       # fastest, best free tier
LLM_PROVIDER=openrouter # access to many free models
LLM_PROVIDER=together   # alternative free credits
```

Auto-detection: if `LLM_PROVIDER` is not set, the app detects whichever key is present.

**What the AI generates:**
- Current economic conditions assessment
- Key causal relationships (Fed → rates → housing, etc.)
- News impact analysis with duration estimates
- Risk matrix (3 downside, 2 upside scenarios)
- Bear / Base / Bull 6–12 month scenarios
- Executive summary for immediate action

**Q&A mode:** Ask any economic question and get a data-grounded answer using live indicators.

---

## 📊 FRED Series Pulled

| Category | Series IDs |
|---|---|
| Labor | UNRATE, PAYEMS, JTSJOL, JTSQUL, CES0500000003 |
| Inflation | CPIAUCSL, CPILFESL, PCEPI, PCEPILFE, PPIACO |
| Monetary | FEDFUNDS, DGS2, DGS10, T10Y2Y |
| Yield Curve | DGS1MO, DGS3MO, DGS6MO, DGS1, DGS3, DGS5, DGS7, DGS20, DGS30 |
| Housing | MORTGAGE30US, HOUST, PERMIT |
| Consumer | RSAFS, UMCSENT, INDPRO |
| Markets | DCOILWTICO, GOLDAMGBD228NLBM, BAMLH0A0HYM2, VIXCLS, DTWEXBGS |
| Growth | GDPC1 |

---

## 📰 News Relevance Scoring

Headlines are scored by keyword weight before being passed to the LLM:

| Weight | Keywords |
|---|---|
| 4 | federal reserve, inflation, interest rate, recession, opec |
| 3 | unemployment, GDP, tariff, trade war, oil, rate hike/cut |
| 2 | housing, mortgage, wages, treasury, yield, geopolitical |
| 1 | economy, market, trade, energy, consumer |

Top 20 scored headlines are injected as context into every LLM call.

---

## 🔮 Forecasting

Each series is modelled with [Meta Prophet](https://facebook.github.io/prophet/):
- **Annual seasonality** detected automatically
- **Changepoint detection** for structural breaks
- **90% confidence intervals** shown as shaded bands
- **18-month horizon** (configurable 6–24 months)

Forecasted indicators: Unemployment, CPI, Fed Funds, Mortgage Rate, Housing Starts, Payrolls, Retail Sales, WTI Oil

---

## ⚙️ Configuration

Settings are adjustable in-app (Settings tab) or via `.Renviron`:

| Setting | Default | Description |
|---|---|---|
| Data lookback | 7 years | Historical window for all FRED/BLS series |
| News lookback | 4 days | How far back to fetch headlines |
| Forecast horizon | 18 months | Prophet prediction window |

---

## 📝 Notes

- **Data freshness:** All data is fetched live on app start and on "Refresh" button click
- **Rate limits:** BLS API has 25 req/day (unregistered) / 500 req/day (with key). FRED is generous (120/min with key)
- **Census map:** ACS data loads once per session (slow first load ~10–20s)
- **Forecasts:** Computed lazily on first visit to Forecasting tab
- **No hardcoded data:** All outputs are dynamically generated from live APIs

---

## 🛠️ Extending

To add a new FRED series:
```r
# In global.R, add to FRED_CFG:
MY_SERIES = list(label="My Series", unit="$", tab="macro")
```

To add a new LLM provider:
```r
# In global.R, add to LLM_PROVIDERS:
myprovider = list(
  base_url      = "https://api.myprovider.com/v1",
  key_env       = "MY_PROVIDER_API_KEY",
  default_model = "my-model",
  models        = c("my-model"),
  display_name  = "My Provider"
)
```

---

*Built with R · Shiny · FRED · BLS · tidyquant · tidycensus · Prophet · Leaflet · Plotly*
