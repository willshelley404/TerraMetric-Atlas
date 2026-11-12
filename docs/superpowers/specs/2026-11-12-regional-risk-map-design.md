# Regional Risk Map — Design

**Date:** 2026-11-12
**Status:** Draft for review
**Replaces:** the "Metro Bubble Map" sub-tab of the Metro Map tab (`ui.R` / `server.R` / `R/data_census.R`)

---

## 1. Intent

The app's recession model answers *"is a recession coming?"* at the national level. This map answers the follow-on questions an investor or policymaker actually acts on:

- **Who will need assistance?** Which neighborhoods have many workers in weakening industries and little financial cushion?
- **Where is default risk building?** Which counties show rising household delinquency, job pressure and weakening collateral?

The current bubble map (ACS 5-year values drawn as circles at metro centroids) shows neither. It is lagging, it overlaps visually, and it says nothing about direction.

The economic framing is **risk = stress now + stress ahead + cushion**:

- **Stress now:** local unemployment is already rising.
- **Stress ahead:** the local industry mix is tilted toward sectors that are weakening nationally (a shift-share projection).
- **Cushion:** whether the households exposed can absorb a shock (low earnings, rent burden, poverty).

## 2. Audience

Investors and policymakers. They are analytical and will distrust a score they can't decompose. Every score must break down into named components with as-of dates.

## 3. Goals

1. A county choropleth of the US that replaces the bubble map, with no gaps.
2. Drill-down: clicking a county (or choosing a metro) zooms in and loads census tracts for that area.
3. Two **lenses** on the same map:
   - **Assistance lens** (tract-level score, plus estimated at-risk worker counts).
   - **Credit lens** (county-level score of household default risk).
4. Two county **signals** shared by both lenses: *Stress now* (local unemployment momentum) and *Stress ahead* (shift-share projected job change).
5. A separately labeled **Tax base pressure (proxy)** layer for municipal-credit context.
6. A prominent **Net migration** view (requested by the user: "that is interesting to people"). It is a lens of its own, not buried in the tax-base proxy (§9.7).
7. Every layer shows its data as-of date. Every score shows its components.
8. Everything runs **inside the Shiny app**. No separate build job, no GitHub Action.
9. Picks up newer data automatically (new LODES year, new ACS year, new CFPB file) without code changes.

## 4. Non-goals

- **Municipal bond default modeling.** Issuer finances (revenues, pensions, debt service) are out of scope. The tax-base layer is a warning sign only and is labeled as such.
- **QCEW.** About 60% of county × industry cells are suppressed. Industry mix comes from ACS (county) and LODES (tract) instead.
- **A forecast of local unemployment or GDP.** The shift-share projection is an exposure measure, not a point forecast.
- **Changing other tabs or the national recession model.**

## 5. Anti-goals (would make this a failure even if it works)

- **Stale or mixed dates presented as current.** Sources span roughly one month to 22 months of lag. Hiding that misleads the audience.
- **Opaque scores.** A 0–100 number with no breakdown.
- **First load much slower than today.** The Metro tab's first load is about 20 seconds now; the target is about the same (measured below).
- **Tract-level noise presented as precision.** Tracts with high ACS margins of error must be visibly flagged.

## 6. Constraints

- **Stack:** R / Shiny / shinydashboard / leaflet, deployed to Posit (`manifest.json`). All leaflet calls use the `leaflet::` prefix (see `global.R`).
- **Hosting:** disk is ephemeral on Posit. A restart wipes any disk cache, and the next visitor rebuilds it.
- **Keys** come from the existing `.Renviron`: `CENSUS_API_KEY`, `BLS_API_KEY`, `FRED_API_KEY`. No new keys.
- **BLS API quota:** 500 queries per day per key. One full county unemployment refresh uses 64.
- **Territories excluded** via the existing `.TERRITORY_PAT`.
- **Connecticut** uses 9 planning regions (FIPS 09110–09190) in LAUS, ACS 2022+, tigris 2024, CFPB, PEP and LODES. Urban Institute still uses the old 8 counties, so those components are NA for Connecticut.

## 7. Measured load times (spike, 2026-11-12)

| Pull | Time | Notes |
| --- | --- | --- |
| County unemployment, all counties (BLS API v2, rate series `LAUCN{fips}0000000003`, 64 calls, 8 in parallel, 3 years) | 13.5 s | 3,143 counties with data; latest month Sep 2026; BLS throttled briefly but all 64 calls succeeded |
| County industry mix (ACS 5-yr 2024, table C24030, one call) | 4.1 s | 3,222 counties |
| County shapes (tigris cb 20m, 2024) | 0.5 s | 4.4 MB; may have been cached locally |
| LODES RAC one state (NJ 2024, direct download) | 3.2 s | 3.4 MB, 2,175 tracts. 2024 exists; 2025 returns 404 |
| Tract shapes, one county | 0.3 s | may have been cached locally |
| FRED v2 bulk county release | ruled out | about 400 MB with no date filter |

`lehdr` was not installed on the dev machine, so the direct-download path was the one tested.

## 8. Data sources

| Source | Used for | Geography | Access | Latest (as of 2026-11-12) | Cache TTL |
| --- | --- | --- | --- | --- | --- |
| BLS LAUS | Stress now | County | BLS API v2, `LAUCN{5-digit FIPS}0000000003`, 50 series per call, parallel | Sep 2026 | 7 days |
| FRED CES sector payrolls | Stress ahead, sector weights | National | `fredr` (already used) | Oct 2026 | 1 day |
| ACS 5-yr C24030 (industry by sex) | County industry mix; tract fallback | County, tract | `tidycensus` | 2020–2024 | 30 days |
| ACS 5-yr (existing `ACS_VARS`) | Context layers; tract cushion (rent burden, poverty) | County, tract | `tidycensus` | 2020–2024 | 30 days |
| LEHD LODES8 RAC | Tract exposure and cushion (earnings, age, sector) | Block, aggregated to tract | `lehdr::grab_lodes()` if installed and it accepts the year; otherwise direct download `https://lehd.ces.census.gov/data/lodes/LODES8/{st}/rac/{st}_rac_S000_JT00_{year}.csv.gz` | 2024 | 90 days, per state |
| CFPB Mortgage Performance Trends | Credit: mortgage delinquency | County (≥1,000 mortgages in sample) | CSV; the file name changes each release, so scrape the download page for the current `CountyMortgagesPercent-90-plusDaysLate-thru-*.csv` (and 30–89) | Mar 2026 | 30 days |
| Urban Institute Debt in America | Credit: credit card and auto delinquency | County | Download files (format to confirm at build time, see §14) | Aug 2026 credit data | 30 days |
| FHFA HPI, all-transactions | Credit: collateral | Metro (quarterly); state for non-metro counties | CSV `hpi_at_metro.csv`; state file | 2026 Q2 | 30 days |
| Census PEP Vintage 2025 components | Migration lens; tax base proxy | County | `tidycensus::get_estimates(product="components")` (2.0+) or flat CSV `co-est2025-alldata.csv` | V2025 (Jul 2024–Jun 2025) | 30 days |
| tigris | County, tract, CBSA shapes | — | `tigris` (already used) | 2024 | 90 days |

**Latest-year detection** generalizes the existing `.best_acs_year()` pattern. For LODES, send a HEAD request for the current year minus 1, then step back up to 4 years. For CFPB, scrape the download page. The app never hard-codes the year.

**LODES gaps:** Alaska and Michigan have incomplete 2022–2024 files (their RAC files only contain residents who work in other states). For those states, and any state where LODES fails, the tract exposure falls back to the ACS tract industry mix (C24030), and the cushion falls back to ACS-only inputs. The panel says which source was used.

## 9. Metrics

All formulas below are **my calls**. Constants live in one named config list so they can be tuned.

### 9.1 Stress now: local unemployment momentum (county)

- Inputs are not seasonally adjusted, so compare against the same months a year earlier: `momentum = avg(rate[t, t-1, t-2]) − avg(rate[t-12, t-13, t-14])`, in percentage points.
- The month `t` is the latest month for which at least 90% of counties report.
- **Missing months** (October 2025 was never collected): average the months that exist; require at least 2 of 3 in each window, otherwise NA.
- **Flags:** ≥ +0.5 pp = "Rising", ≥ +1.0 pp = "Sahm-style trigger".
- The legend calls it "Sahm-style" and notes that it compares year over year.

### 9.2 Stress ahead: shift-share projected job change (county)

- **Sector momentum** `m_s` = 6-month annualized % change in national payrolls for sector `s`.
- **County mix** `share_cs` = residents employed in sector `s` / total employed residents (ACS C24030, summing male and female per industry). ACS mix is residence-based, which matches LAUS (also residence-based).
- **Projection** `P_c = Σ_s share_cs × m_s`. Display it as the gap from the national value `P_US` (percentage points), so the map shows relative exposure.
- **Sector to FRED series mapping** (the builder must verify the IDs marked † against FRED before relying on them):

  | ACS C24030 industry group | FRED series |
  | --- | --- |
  | Agriculture, forestry, fishing, hunting, mining | `USMINE` † (mining & logging; agriculture is not in CES, so this is acknowledged as imperfect) |
  | Construction | `USCONS` |
  | Manufacturing | `MANEMP` |
  | Wholesale trade | `USWTRADE` † |
  | Retail trade | `USTRADE` |
  | Transportation, warehousing, utilities | `CES4300000001` |
  | Information | `USINFO` |
  | Finance, insurance, real estate | `USFIRE` |
  | Professional, scientific, management, admin & waste | `USPBS` |
  | Educational services, health care, social assistance | `USEHS` † |
  | Arts, entertainment, recreation, accommodation, food | `USLAH` |
  | Other services | `USSERV` † |
  | Public administration | `USGOVT` |

- `TEMPHELPS` (temporary help) is shown as a national leading-indicator note in the side panel. It is not an ACS industry, so it is not part of the formula.

### 9.3 Sector risk weight `w_s` (shared by both lenses)

- **Structural cyclicality:** the average peak-to-trough % decline in sector payrolls across the 2001, 2007–09 and 2020 recessions (NBER dates from FRED `USREC`), computed in-app from the same FRED series.
- **Current weakness:** `−m_s`, expressed as a z-score against the sector's own history.
- `w_s = 0.5 × rescale01(structural) + 0.5 × rescale01(current)`.
- The blend weights are tunable constants.

### 9.4 Assistance lens (tract)

- **Exposure** `E_t = Σ_s (resident jobs_ts / total resident jobs_t) × w_s`, using LODES RAC sector columns CNS01–CNS20 mapped to the 13 groups above (ACS tract C24030 as fallback).
- **Ranking frame (decided with the user):** tract percentiles `pct(·)` are computed within the tract's **labor market**, not nationally.
  - **Metro tracts:** a tract in a CBSA is ranked against all tracts in that CBSA, across state lines. For example, Chattanooga ranks its Tennessee and Georgia tracts together. Opening a multi-state metro loads every state file it touches.
  - **Non-metro tracts:** ranked against the other non-metro tracts in the same state.
  - **What the map shows:** colors use the labor-market rank. The popup also shows the **state rank** ("Top 8% in Chattanooga metro · Top 15% in Tennessee"), computed from the same whole-state files at no extra download cost.
  - **Why not national:** national tract ranking would need all 51 state files on every cold start. It is ruled out by the in-app constraint.
  - **US-view comparability:** the county Assistance value on the US view (from county ACS industry mix and cushion) *is* ranked nationally.
- **Cushion** (higher means a thinner cushion). Take the mean of percentile ranks of:
  - LODES CE01 share (≤ $1,250/month earnings);
  - LODES CA01 share (age ≤ 29);
  - ACS rent-burden rate;
  - ACS poverty rate.
- **Local job pressure** `J_c`: the county's job-pressure percentile (same definition as Credit component 3: the mean of the Stress now and Stress ahead percentiles), applied to every tract in that county. A tract in a county where unemployment is already rising needs more help than an identical tract where it isn't.
- **Score** = `100 × (pct(E_t) × pct(cushion_t) × J_c)^(1/3)`. The geometric mean means a tract scores high only when it is exposed, thin-cushioned, *and* in a county under pressure. *(My call, settled after review.)*
- **Counts** for budgeting:
  - **Exposure-weighted workers** `Σ_s jobs_ts × w_s`, labeled as an index-like headcount, not a layoff forecast;
  - rent-burdened households (ACS).
- **Noise flag (revised during build, 2026-11-12):** flags are **per layer**. A tract is outlined on the poverty layer when its poverty MOE exceeds the estimate itself (90% MOE / estimate > 1.0, i.e. CV above about 61%, "very unreliable"), and likewise for rent burden on its own layer. Other layers carry no flag. The popup says so, and the side panel always shows ± MOE.
  - **Why the original rule changed:** the original "above 50% of the estimate" rule flagged 67% of Chattanooga-metro tracts, which carries no information. The median tract rent-burden MOE in TN+GA is ±22 pp. The revised rule flags about 6% of tracts on poverty and about 16% on rent burden.

### 9.5 Credit lens (county)

Components, each converted to a national percentile:

1. **Mortgage delinquency:** the mean of two percentiles. *(My call, settled after review.)*
   - The **level** of the CFPB 90+ day rate (serious delinquency) in the latest month.
   - The **12-month change** in the CFPB 30–89 day rate. Early-stage delinquency is the leading signal.
2. **Consumer delinquency:** Urban credit card 60+ and auto 60+ rates (averaged).
3. **Job pressure:** the average of the Stress now and Stress ahead percentiles.
4. **Collateral:** FHFA HPI year-over-year change for the county's CBSA (inverted, so falling prices score as higher risk). Non-metro counties use their state's HPI.

- **Score** = mean of the available component percentiles × 100, requiring at least 3 of 4; otherwise NA ("insufficient data").
- **Drill-down:** the credit lens is county-only, so clicking a county opens its component breakdown. Tracts still load, but they show the Assistance or context layers.

### 9.6 Tax base pressure (proxy), its own layer, not in any score

- Mean percentile of:
  - net domestic migration rate (PEP, inverted, so out-migration means more pressure);
  - population change (inverted);
  - HPI year-over-year change (inverted).
- Labeled "proxy — not a bond default model".

### 9.7 Net migration (county), its own lens

Source: Census PEP Vintage 2025 county components of change. The annual columns cover each July–June year from 2020–21 to 2024–25.

**Layers:**

- **Net domestic migration rate:** per 1,000 residents, latest year (2024–25). This is the default layer of the lens.
- **Net international migration rate:** per 1,000 residents, latest year.
- **Total net migration rate:** per 1,000 residents, latest year (domestic + international).
- **Net domestic migration since 2020:** cumulative 2020–21 through 2024–25, per 1,000 of the 2020 base population. This shows the pandemic-era shift.

**Display rules:**

- **Palette:** diverging and centered on 0. In-migration and out-migration are shown as directions, not as "good" or "bad", so the palette is not red/green. *(My call.)*
- **Hover tooltip (option B layout):** the value, a rank pill ("Top 5% for in-migration"), and "Driven by" lines for domestic, international and natural change (births − deaths), so a viewer can see whether growth comes from moves or births.
- **Side panel:**
  - a small bar chart of annual net domestic migration from 2020–21 to 2024–25;
  - population change over the same period;
  - the as-of date.
- **On every county tooltip and side panel, whatever the lens:** one standing line, e.g. "Net domestic migration: +8.2 per 1,000 (2024–25)". Migration then stays visible even while someone is looking at credit or assistance.
- **Metro rankings table:** gains a net domestic migration rate column, aggregated from the CBSA's counties.
- **Tracts:** PEP has no tract data. When drilled in, the Migration lens keeps the county shading underneath and does not shade the tracts.

## 10. User experience

Placement (decided with the user): **Risk Map becomes the tab's only map.** The bubble map is replaced, and the State Choropleth sub-tab is **retired**: a state map of 5-year averages repeats the county map at a coarser grain. The State Rankings table goes with it. The tab has no sub-tabs any more. It shows the Risk Map, then the Metro AI Analysis box, then the metro (CBSA) rankings table. The menu item is renamed from "Metro Map" to "Regional Risk".

**Controls (left column):**

- **Lens:** Assistance | Credit | Migration | Context.
- **Layer**, which depends on the lens:
  - *Assistance:* score, exposure, cushion, exposure-weighted workers.
  - *Credit:* score plus its four components.
  - *Migration:* the four layers in §9.7.
  - *Context:* Stress now, Stress ahead, Tax base proxy, and the existing ACS variables at county/tract.
- **Jump to metro:** a select box that zooms to the metro's counties and loads their tracts (multi-state metros load each state's LODES file).
- **Back to US** button and a breadcrumb (US › State › County).

**Map behavior:**

- **US view:** counties shaded by the selected layer. Clicking a county zooms to it and adds its tracts on top. The other counties stay visible but dimmed.
- **Legend** includes the source and as-of date of the layer, e.g. "Stress now — BLS LAUS, Sep 2026".

**Hover tooltip ("score + why", chosen by the user from mockups):**

- **County tooltip:**
  - area name, with the metro name underneath if it is in a CBSA;
  - the current layer's value in large type, with a rank pill ("Top 12% nationally");
  - a "Driven by" list: the **top 3 components by percentile**, each with its value in natural units and colored by direction (worse = red);
  - a footer with the date span of the inputs ("Data Mar–Sep 2026 · click for all").
- **Tract tooltip:** the same layout, but the rank pill shows the labor-market rank ("Top 8% Chattanooga metro"), with the state rank on the line below ("Top 15% in Georgia"). The drivers are the exposure and cushion inputs (share of workers in weakening sectors, low-wage share, renters cost-burdened), plus "≈ Exposure-weighted workers" as the last line.
- **Single-value layers** (e.g. a raw ACS context variable, or Stress now on its own): show the value, rank pill and source/as-of line. The "Driven by" list is omitted when the layer has no components.
- **Missing data:** the value shows "Insufficient data" and the drivers list says which components are missing.
- **Click → side panel:** full component table (component, value, national or labor-market percentile, as-of date, source). Below it, the area's top resident industries and the national temp-help note (§9.2).
- **Visual reference:** `.superpowers/brainstorm/1971-1794501828/content/tooltip-options.html`, option B.
- **Popups/side panel** for the selected area show:
  - the score;
  - each component with its value, national percentile and as-of date;
  - which fallbacks were used.

**Loading:**

- The county base (shapes, LAUS, ACS county mix, ACS county context variables, FRED sectors) loads on the first visit to the tab, alongside `fetch_metro_acs()` for the metro table and Metro AI. The state ACS load is dropped with the State Choropleth. The CBSA centroid fetch (`fetch_metro_sf()`) is removed with the bubble map, which saves a little time. Expected cost is about 25 s cold for the sub-tab and near-instant warm. No async code is needed. *(My call, settled after review: the "first load" target in §5 is measured per sub-tab.)*
- Credit sources load the first time the Credit lens is chosen.
- Tract data loads per county on click (about 3–10 s cold for that state).

**Metro AI Analysis:** extend `build_metro_summary()` to include the metro's population-weighted Stress now, Stress ahead, Credit score and components. The rest of the prompt stays the same.

## 11. Architecture

**New files:**

- `R/data_risk.R`: source fetchers, latest-year detection, `lehdr`-or-direct LODES loader, cache helpers.
- `R/risk_metrics.R`: pure functions for §9 (no I/O), so they can be tested with fixtures.
- `R/map_risk.R`: leaflet builders for the county and tract layers.

`R/data_census.R`: once Risk Map ships, remove `fetch_metro_sf()`, `build_metro_bubble_map()`, `METRO_VAR_CFG`, `fetch_state_acs()` and `build_state_map()`. Remove their server outputs too: `metro_map`, `metro_bubble_map`, `tbl_metro` and the `rv$census_data` / `rv$metro_sf` loads. Keep `fetch_metro_acs()`, `build_metro_summary()`, `build_metro_prompt()` and `VAR_CFG`, which the Risk Map's ACS context layers reuse. The tab-entry load now runs only `fetch_metro_acs()` (metro table and Metro AI) plus the county base.

**Cache:**

- Two tiers:
  - a process-wide environment shared across Shiny sessions in the same R process;
  - plus `.rds` files on disk. The directory is `tools::R_user_dir("TerraMetricAtlas", "cache")`, falling back to `tempdir()`.
- One file per source (per state for LODES and tracts), each with its own TTL (§8).
- **Stale-on-error:** if a refresh fails (network, or the BLS quota is exhausted), serve the stale cache and mark the layer "stale" in the legend.

**Other rules:**

- **FRED sector series** are fetched inside the risk module, *not* added to `FRED_CFG`, so the app's startup fetch and other tabs are unaffected.
- **`lehdr`:**
  - `requireNamespace("lehdr")` and a `tryCatch` around `grab_lodes(..., agg_geo = "tract")`;
  - on error or missing package, use direct download plus aggregation by `substr(h_geocode, 1, 11)`;
  - `lehdr` is *not* added as a hard dependency.

## 12. Phases

Each phase ships on its own and leaves the app working.

0. **Fix the existing rent-burden definition (separate commit, first).** *(My call, settled after review.)* The current definition is wrong in two ways:
   - `ACS_VARS` counts only 30–34.9% (`B25070_007`) and 35–39.9% (`B25070_008`). It misses 40–49.9% (`_009`) and 50%+ (`_010`).
   - It divides by all occupied units, including owners, instead of renter households.

   Correct formula: `(_007 + _008 + _009 + _010) / (B25070_001 − B25070_011)`, i.e. the share of renters paying ≥30% of income, excluding "not computed". The fix lands in `ACS_VARS` / `.derive_metrics()`, so the metro table and Metro AI show the corrected number (as does the State Choropleth until Phase 1 retires it). The corrected national rate is roughly half of renters, so the Metro AI flag in `build_metro_summary()` changes from "rent burden > 30%" to "rent burden more than 3 pp above the US rate". This is a bug fix, not a redesign, so it does not conflict with §4.
1. **County base + drill-down.**
   - County choropleth replacing the bubble map. Retire the State Choropleth sub-tab and State Rankings table.
   - Stress now layer.
   - Existing ACS variables at county and tract.
   - Click-to-tract, Jump to metro, Back to US.
   - Cache layer.
2. **Migration lens + Stress ahead.**
   - Migration lens (PEP: one small CSV, cheap, high interest) and the standing migration line on county tooltips.
   - Stress ahead: FRED sector payrolls, ACS county mix, shift-share, sector risk weights.
   - **Metro-level Stress now** (added 2026-11-12 at the user's request):
     - Source: BLS's published metro LAUS unemployment rates (about 8 API calls per refresh, 50 series per call), seeded in `data/risk_seed/` like the county pull.
     - Computed with the same pairwise year-over-year momentum as counties.
     - When you jump to or drill into a metro, the side panel header and the tooltips show e.g. "Chattanooga metro: +0.x pp vs a year ago".
     - Today the side panel shows only the metro's largest county.
3. **Assistance lens.** LODES loader (`lehdr` with direct fallback), tract exposure, cushion, score, counts, MOE flags, AK/MI fallback.
4. **Credit lens + tax base.** CFPB, Urban, FHFA; credit score; tax base proxy layer (reuses the PEP data from Phase 2).
5. **Metro AI.** Feed the new components into the metro summary.

## 13. Left to the builder

- Color palettes and legend binning (follow the existing dark theme and `VAR_CFG` conventions).
- Exact layout of the side panel and popups.
- Parallelism level and retry/backoff for BLS calls (8 concurrent worked in the spike).
- How to draw the MOE flag (hatching vs. outline).
- Test approach for `R/risk_metrics.R` (the repo has no test suite today).

## 14. Open risks

- **Urban Institute 2026 file format** could not be confirmed (the catalog page returned 403 to an automated fetch). If it is per-county Excel like 2025, parsing is straightforward. If it is unavailable, the credit score uses 3 components (mortgage, job pressure, collateral) and says so.
- **Rural coverage holes:**
  - CFPB covers only counties with ≥1,000 sampled mortgages;
  - Urban suppresses areas with fewer than 50 people;
  - FHFA uses state HPI for non-metro counties.

  The ≥3-of-4 rule and the NA styling handle this, but many rural counties may show "insufficient data" on the Credit lens.
- **BLS quota on frequent restarts:** more than 7 cold starts in one day would exhaust the 500-query limit. Stale-on-error covers this only if a disk cache survived. If it didn't, Stress now is unavailable until the quota resets.
- **LODES earnings bands are nominal** ($1,250 has not been indexed), so CE01 is used only cross-sectionally.
- **ACS 5-year mix** averages 2020–2024, which includes the pandemic years. Acceptable for industry mix (it moves slowly) but noted in the panel.
