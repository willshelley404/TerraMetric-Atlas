# Regional Risk Map — Phase 4 Implementation Plan (Credit lens + Tax base proxy)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the county Credit lens and a separately labelled "Tax base pressure (proxy)" layer on the Context lens.
- **Credit score:** mortgage delinquency (CFPB), consumer delinquency (Urban Institute), job pressure (Phases 2–3) and collateral (FHFA HPI), reduced to national percentiles. It needs at least 3 of the 4 components.
- **Tax base proxy:** out-migration + population decline + falling home prices.

**Architecture:**
- **Fetchers** (`R/data_risk.R`): three fetchers behind `risk_cached()`, each seeded, plus a county → metro-division crosswalk for FHFA.
- **Credit sources load lazily:** the first time the Credit lens (or the Tax base layer) is chosen. They run through the same `risk_set_parts()` / `risk_rebuild()` path, with component `"credit"` in `loading` meanwhile.
- **Metrics** (`R/risk_metrics.R`): pure functions for component percentiles, the score and the tax-base proxy.
- **Layers** go in `RISK_LAYERS` on lens `"credit"`.

**Tech Stack:** R 4.6, Shiny, leaflet, sf, tigris, httr2, data.table (`fread`), testthat. No rvest: links are pulled from the HTML with a regex, so no new package is needed.

**Spec:** `docs/superpowers/specs/2026-11-12-regional-risk-map-design.md`. Read §8 (CFPB, Urban, FHFA rows), §9.5, §9.6, §10 (Credit lens, drill-down note), §12 (Phase 4) and §14 (risks).

**Builds on:**
- Phase 2: PEP columns `mig_dom_rate`, `pop_chg_pct`; `risk_set_parts` / `risk_rebuild`; `loading_text`; `digits`.
- Phase 3: `job_pressure(county_layers)` returns 0..1.

## Global Constraints

- All earlier-phase constraints still hold: `leaflet::` prefix, no new packages, `.RISK_UA` on every download, everything in-app, seeding with `bg` refresh.
- **TTLs (days):** CFPB 30; Urban 30; FHFA 30; MSAD crosswalk 90. All four are seeded.
- **Credit score:** the mean of the available component percentiles × 1, on a 0–100 scale, requiring at least 3 of 4. Otherwise NA, shown as "Insufficient data" with the missing components named.
- **Tax base layer copy:** the label must contain `"proxy"`, and the legend/tooltip footer must say `"proxy — not a bond default model"`.
- **Credit lens is county-only** (`levels = "county"`). When drilled in on the Credit lens, the counties stay shaded and no tracts are drawn. This is the existing county-only behaviour; tracts still load for other lenses.
- **Connecticut:**
  - Urban uses the old 8 counties, so the consumer component is NA for 09110–09190. Do not crosswalk.
  - CFPB and FHFA state data cover CT planning regions.

## Verified source facts (spike 2026-11-12)

- **CFPB:**
  - **Page:** `https://www.consumerfinance.gov/data-research/mortgage-performance-trends/download-the-data/` (200 with UA).
  - **Links:** hrefs matching `(County|MetroArea)MortgagesPercent-(30-89|90-plus)DaysLate-thru-(\d{4}-\d{2})\.csv`. Take the maximum `thru` date per file type. Some links use `http://files.consumerfinance.gov.s3.amazonaws.com/…`, so rewrite them to `https://files.consumerfinance.gov/…`.
  - **Latest:** thru 2026-03.
  - **County file:** wide, with header `RegionType,State,Name,FIPSCode,2008-01,…`. Row 1 is National, with FIPS `-----`. `FIPSCode` is wrapped in single quotes (`'01003'`). Values are percents with 1 dp. Only **509 counties** are present; absent = suppressed. CT uses planning regions.
  - **Metro file:** `RegionType,Name,CBSACode,…`. It has 310 `MetroArea` rows (CBSA code) and 33 `NonMetroArea` rows coded `{2-digit state FIPS}-non`.
- **Urban "Debt in America":**
  - Read `https://apps.urban.org/features/debt-interactive-map/js/minified-main.js` and regex-match `pathFiles="([^"]+)"` (currently `data/20261022-update/`).
  - The data is at `https://apps.urban.org/features/debt-interactive-map/{pathFiles}county_overall.csv`.
  - **Columns:** `id` (5-char FIPS), `carddelrate`, `autoretdelrate` (fractions such as `".0544"`; suppressed values are the string `"NA"`). There are 3,143 rows; credit data is from Aug 2026; CT uses the old counties.
  - The data catalog returns 403 (Cloudflare), so don't use it.
  - **Delinquency threshold:** unconfirmed. Label it "delinquent" without a day count.
- **FHFA:**
  - **Metro file:** `https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_metro.csv`, headerless, with columns name, code, year, quarter, index, se. Missing values are `-`. The latest quarter is 2026 Q2.
    - The 13 large CBSAs are present **only as their 37 metro divisions (MSAD codes)**: Boston, Chicago, Dallas, Detroit, Los Angeles, Miami, New York, Philadelphia, San Francisco, Seattle, Washington, Atlanta, Tampa.
  - **State file:** `https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_state.csv`, headerless, with columns abbr, year, quarter, index.
  - **YoY** = `100 * (idx[t] / idx[t - 4 quarters] - 1)`. Example: Chattanooga 16860 is +1.92%.

## Review Focus

1. **A rural county with no CFPB row and in no metro.** The mortgage component falls back to the state's `NonMetroArea` row, and the side panel says "CFPB: state non-metro estimate". It is not NA, and it is not silently presented as a county value. Test: `test-credit-sources.R::"CFPB falls back county → metro → state non-metro with source"` (Task 1).
2. **A county in a divided metro** (e.g. Cook County IL in Chicago, which FHFA reports only by division). Collateral comes from its MSAD, not NA. Test: `test-credit-sources.R::"FHFA uses metro division for divided CBSAs, state for micro/rural"` (Task 2).
3. **Urban's undocumented route changes or fails.** The consumer component is NA everywhere, the score runs on 3 components, and the side panel and legend say "Urban Institute data unavailable — score uses 3 components". Test: `test-credit-score.R::"score runs on 3 components when Urban missing"` (Task 3).
4. **Connecticut.** The consumer component is NA, as is any score with only 2 other components. The tooltip names the missing components. Test: `test-credit-score.R::"fewer than 3 components gives Insufficient data naming the missing"` (Task 3).
5. **Choosing the Credit lens while its sources load** (or when they all fail). The legend says loading/unavailable, other lenses still work, and switching back and forth does not refetch. Test: `test-credit-layers.R::"credit status loading then ok; unavailable when all sources fail"` (Task 4).

---

### Task 1: CFPB mortgage delinquency fetch with geographic fallback

**Files:**
- Modify: `R/data_risk.R` (new "CFPB" section), `R/risk_metrics.R` (`parse_cfpb_wide`, `cfpb_county_components`)
- Create: `tests/testthat/test-credit-sources.R`, `tests/testthat/fixtures/cfpb_page.html` (a trimmed snippet with 6 hrefs: two `thru` dates, county and metro, http and https), `fixtures/cfpb_county_90.csv`, `cfpb_county_3089.csv`, `cfpb_metro_90.csv`, `cfpb_metro_3089.csv` (a few rows each, real headers, 14 month columns 2025-02..2026-03)

**Interfaces:**
- Produces:
  - **`cfpb_links(html) -> list(county_90, county_3089, metro_90, metro_3089, thru)`:** the newest `thru` date per type, rewritten to https.
  - **`parse_cfpb_wide(df, id_col) -> tibble(id, date, pct)`:**
    - `id_col` is `"FIPSCode"` (strip the `'` characters) or `"CBSACode"`;
    - drops the National row and rows with no id;
    - `date` comes from the `YYYY-MM` column names.
  - **`cfpb_county_components(county_ids, cbsa_of, state_of, county90, county3089, metro90, metro3089) -> tibble(GEOID, dq90, dq3089_chg, cfpb_source, cfpb_month)`:**
    - `dq90` is the latest month;
    - `dq3089_chg` = 30–89 latest − 30–89 12 months earlier (pp);
    - **lookup order:** the county row (`cfpb_source = "county"`), then the county's CBSA row if `cbsa_of` is a metro code present in the metro file (`"metro"`), then the `{state}-non` row (`"state non-metro"`), else NA.
  - **`fetch_cfpb_mortgage() -> list(county90, county3089, metro90, metro3089, thru) or NULL`:**
    - downloads the page and the 4 CSVs with `.RISK_UA` and parses each with `parse_cfpb_wide`;
    - keeps only the last 13 months, so the seed stays small;
    - cached under key `"cfpb_mortgage_v1"` for 30 days, with `bg = list(id = "cfpb", fn = "fetch_cfpb_mortgage", args = list())`;
    - NULL if any of the 4 files fails.

- [ ] **Step 1: Write failing tests:**
  - `"links pick newest thru and https"`: `thru == "2026-03"`; every URL starts `https://files.consumerfinance.gov/`.
  - `"FIPS quotes stripped, national row dropped"`: `parse_cfpb_wide(read county fixture, "FIPSCode")$id` contains `"01003"` and not `"-----"`.
  - `"CFPB falls back county → metro → state non-metro with source"`, with three fixture counties:
    - one present in the county file → `"county"`;
    - one absent but in CBSA `16860`, which is in the metro file → `"metro"`;
    - one absent and non-CBSA in state `"13"` → `"state non-metro"`;
    - `dq3089_chg` equals the hand-computed difference.
- [ ] **Step 2: Run** with `filter='credit-sources'`. Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS. Live check: `fetch_cfpb_mortgage()$thru` is `"2026-03"`.
- [ ] **Step 5: Commit** `feat: CFPB mortgage delinquency with county → metro → state non-metro fallback`

---

### Task 2: Urban consumer delinquency and FHFA HPI (with metro divisions)

**Files:**
- Modify: `R/data_risk.R` (new "Urban" and "FHFA" sections, `fetch_msad_xwalk`), `R/risk_metrics.R` (`parse_urban_county`, `hpi_yoy`, `county_hpi`)
- Modify: `tests/testthat/test-credit-sources.R`; create fixtures `urban_main_snippet.js`, `urban_county_overall.csv` (5 rows incl. one `"NA"` and CT `09001`), `fhfa_metro.csv` (Chattanooga 16860 plus Chicago division 16984, 5 quarters each, one `-`), `fhfa_state.csv` (TN, GA, IL, CT, 5 quarters)

**Interfaces:**
- Produces:
  - **`urban_path(js) -> character or NULL`:** the `pathFiles` regex.
  - **`parse_urban_county(df) -> tibble(GEOID, card_del, auto_del)`:** percent = `100 × as.numeric(fraction)`; `"NA"` → NA.
  - **`fetch_urban_debt() -> tibble or NULL`:**
    - JS → path → `county_overall.csv`;
    - cached under `"urban_debt_v1"` for 30 days, with `bg = list(id = "urban", fn = "fetch_urban_debt", args = list())`;
    - `attr(, "asof") <- "Aug 2026"`, taken as the date in the path folder (`YYYYMMDD-update` → `format(%b %Y)`) if parseable, else `NA`.
  - **`hpi_yoy(df) -> tibble(code, hpi_yoy, hpi_q)`:** works for metro (code = column 2) and state (code = column 1). It uses the latest (year, quarter) with a non-NA index and the same quarter one year earlier. `hpi_q` is e.g. `"2026 Q2"`.
  - **`fetch_msad_xwalk(county_base) -> tibble(GEOID, msad_code)`:**
    - point-on-surface join of counties into `tigris::metro_divisions(cb = TRUE, year = 2023)`, mirroring `county_cbsa_xwalk`;
    - cached under `"msad_xwalk_2023"` for 90 days, with `bg = list(id = "msad", fn = "fetch_msad_xwalk", args = list(county_base = NULL))`. When `county_base` is NULL, the function loads it via `fetch_county_base()`, so the bg process can call it.
    - If `tigris::metro_divisions` is unavailable, return NULL: collateral then uses the CBSA code, so divided metros fall to state HPI. Log it.
  - **`fetch_fhfa_hpi() -> list(metro = hpi_yoy tibble, state = hpi_yoy tibble) or NULL`:** cached under `"fhfa_hpi_v1"` for 30 days, with `bg = list(id = "fhfa", fn = "fetch_fhfa_hpi", args = list())`.
  - **`county_hpi(cb, hpi, msad_xwalk) -> tibble(GEOID, hpi_yoy, hpi_source, hpi_q)`:**
    - **metro counties:** the MSAD value if the county has one present in the metro file, else the CBSA value;
    - **micro, non-CBSA, or a metro absent from the file:** the state value via `state_abbr`;
    - `hpi_source` is `"metro"`, `"metro division"` or `"state"`.

- [ ] **Step 1: Write failing tests:**
  - `"Urban path parsed and NA strings handled"`: path is `"data/20261022-update/"`; `card_del` for a `".0544018"` row ≈ 5.44; the `"NA"` row gives NA.
  - `"HPI yoy uses same quarter a year earlier, skips dashes"`: Chattanooga yoy equals the hand-computed value from the fixture.
  - `"FHFA uses metro division for divided CBSAs, state for micro/rural"`:
    - Cook-like county `17031` with `msad_code = "16984"` → `"metro division"`;
    - Walker `13295` (CBSA 16860) → `"metro"`;
    - a micro county → `"state"`.
- [ ] **Step 2: Run.** Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS. Live check: Urban has about 3,143 rows, FHFA metro yoy has 410 codes, the MSAD crosswalk has about 140 counties.
- [ ] **Step 5: Commit** `feat: Urban consumer delinquency and FHFA HPI with metro-division crosswalk`

---

### Task 3: Credit components, score, tax base proxy (pure)

**Files:**
- Modify: `R/risk_metrics.R` (new "Credit" section; `RISK_CFG$credit_min_components = 3`)
- Create: `tests/testthat/test-credit-score.R`

**Interfaces:**
- Consumes: `job_pressure()` (Phase 3), `cfpb_county_components`, `parse_urban_county` output, `county_hpi`.
- Produces:
  - **`credit_components(cl, cfpb, urban, hpi) -> tibble`**, keyed by GEOID with national percentiles:
    - `cr_mortgage` = mean(`pct_rank(dq90)`, `pct_rank(dq3089_chg)`), the available ones;
    - `consumer_del` = mean(`card_del`, `auto_del`), the available ones; `cr_consumer` = `pct_rank(consumer_del)`;
    - `cr_jobs` = `100 × job_pressure(cl)`;
    - `cr_collateral` = `100 − pct_rank(hpi_yoy)`;
    - `credit_n` = the count of non-NA components;
    - `credit_score` = `round(mean(available), 1)` if `credit_n >= RISK_CFG$credit_min_components`, else NA;
    - `credit_missing` = a comma-separated list of missing component names (`"mortgage"`, `"consumer"`, `"job pressure"`, `"collateral"`);
    - the raw inputs are kept: `dq90`, `dq3089_chg`, `cfpb_source`, `card_del`, `auto_del`, `hpi_yoy`, `hpi_source`.
  - **`tax_base_proxy(cl) -> numeric`:**
    - mean of `100 − pct_rank(mig_dom_rate)`, `100 − pct_rank(pop_chg_pct)` and `100 − pct_rank(hpi_yoy)`, the available ones;
    - requires at least 2 inputs, else NA;
    - rounded to 1 dp.

- [ ] **Step 1: Write failing tests** with a 6-county synthetic `cl`:
  - `"components are national percentiles with mortgage as mean of level and change"`.
  - `"score runs on 3 components when Urban missing"`: `urban = NULL` gives a non-NA score for complete rows, and `credit_missing == "consumer"`.
  - `"fewer than 3 components gives Insufficient data naming the missing"`: a CT row with only mortgage and collateral gives NA score and `credit_missing == "consumer, job pressure"`.
  - `"collateral inverted: falling prices score higher"`.
  - `"tax base proxy needs two inputs"`: the HPI missing for all rows still gives values from migration + population; only one input gives NA.
- [ ] **Step 2: Run.** **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: credit components, score (>=3 of 4) and tax base proxy`

---

### Task 4: Credit lens and Tax base layer — layers, tooltips, side panel, lazy load

**Files:**
- Modify: `R/risk_metrics.R` (`build_county_layers` gains `credit = NULL`), `R/map_risk.R` (`RISK_LAYERS`, `RISK_LENSES`, tooltips, side panel), `server.R` (lazy credit load), `ui.R` (info text)
- Create: `tests/testthat/test-credit-layers.R`

**Interfaces:**
- Produces:
  - **`build_county_layers(..., credit = NULL)`:**
    - `credit` is `list(cfpb, urban, hpi, msad)`, the raw fetch results, any of which may be NULL;
    - when non-NULL it computes `credit_components()` and `tax_base`;
    - **status for every credit-lens layer:** `"loading"` if `"credit"` is in `loading`, `"unavailable"` if `credit` is NULL or all three sources are NULL, else `"ok"`;
    - **`tax_base`** follows its inputs: it is `"ok"` when PEP is present even without credit (HPI then simply missing).
  - **As-of labels:**
    - `credit_score`: `"CFPB {Mon YYYY} · Urban {asof} · FHFA {YYYY Qn} · BLS/CES"`;
    - `credit_mortgage`: `"CFPB thru {Mon YYYY}"`;
    - `credit_consumer`: `"Urban Institute, {asof}"`;
    - `credit_collateral`: `"FHFA {YYYY Qn}"`;
    - `credit_jobs`: the Stress now / Stress ahead as-ofs joined with `" · "`;
    - `tax_base`: `"PEP V{V} · FHFA {YYYY Qn}"`.
  - **`RISK_LAYERS` entries:**
    - **Credit lens** (`lens = "credit"`, `levels = "county"`, `pal = "YlOrRd"`, `higher_is_worse = TRUE` unless noted):

      | id | col | label | fmt | bins |
      |---|---|---|---|---|
      | `credit_score` | `credit_score` | "Household credit risk score (0–100)" | `"score"` | `c(0, 20, 35, 50, 65, 80, 101)` |
      | `credit_mortgage` | `dq90` | "Mortgages 90+ days delinquent (%)" | `"%"` | — |
      | `credit_consumer` | `consumer_del` | "Card & auto loans delinquent (%)" | `"%"` | — |
      | `credit_jobs` | `cr_jobs` | "Job pressure (percentile)" | `"score"` | — |
      | `credit_collateral` | `hpi_yoy` | "Home prices, year over year (%)" | `"pp"`-style signed | `c(-50, -5, -2, 0, 2, 5, 50)` |

      - `credit_collateral` overrides: `pal = "RdBu"`, `rev = FALSE`, `higher_is_worse = FALSE`, `digits = 1`.
      - **Sources:** "CFPB · Urban · FHFA · BLS" for the score, and each component's own source.
    - **`tax_base`** goes on the Context lens: `col = "tax_base"`, `label = "Tax base pressure (proxy — not a bond default model)"`, `fmt = "score"`, `pal = "YlOrBr"`, `higher_is_worse = TRUE`, `source = "Census PEP · FHFA (proxy — not a bond default model)"`, `levels = "county"`.
    - **`RISK_LENSES`** = `c("Assistance" = "assistance", "Credit" = "credit", "Migration" = "migration", "Context" = "context")`.
  - **Tooltips:**
    - **`credit_score`:** "Driven by" shows the top 3 components by percentile, in natural units:
      - `"Mortgages 90+ late"` (`dq90` %, plus `" (metro est.)"` / `" (state non-metro est.)"` when `cfpb_source` is not county);
      - `"Card & auto delinquent"` (`consumer_del` %);
      - `"Job pressure"` (`cr_jobs` th pct);
      - `"Home prices YoY"` (`hpi_yoy` %);
      - each is red when its percentile is ≥ 50.
      - When the score is NA, the value shows "Insufficient data" and the standing line is `"Missing: {credit_missing}"`.
    - **`tax_base`:** drivers `"Net domestic migration"`, `"Population change 2020–{yy}"` and `"Home prices YoY"`. The footer carries the proxy wording through `source`.
  - **Side panel (county):** a "Credit components" block (divs, not rows) listing each component with its value, national percentile, source (incl. `cfpb_source` / `hpi_source`) and as-of. If Urban is NULL it adds `"Urban Institute data unavailable — score uses 3 components."`.
  - **Server lazy load:**
    - `observeEvent(list(input$risk_lens, input$risk_layer))`: when the lens is `"credit"` or the layer is `"tax_base"`, and `rv$credit_requested` is not TRUE, set it to TRUE;
    - `risk_set_parts(loading = union(loading, "credit"))` + `risk_rebuild()`;
    - then, after `session$onFlushed`, fetch `cfpb` / `urban` / `hpi` / `msad`, each in `tryCatch` → NULL;
    - `risk_set_parts(credit = list(...), loading = setdiff(loading, "credit"))` + `risk_rebuild()`.
    - Within a session this never refetches. Other sessions get the cache.

- [ ] **Step 1: Write failing tests:**
  - `"credit status loading then ok; unavailable when all sources fail"`.
  - `"credit lens layers are county-only and in the credit lens"`.
  - `"score tooltip names the CFPB fallback and the missing components"`: `"(state non-metro est.)"` and `"Missing: consumer, job pressure"` appear for the relevant fixture rows.
  - `"tax base layer is labelled proxy everywhere"`: its legend title and tooltip footer both contain `"proxy — not a bond default model"`.
  - `"side panel lists credit components with sources"`: contains `"Credit components"`, `"CFPB"` and `"FHFA"`; with `urban = NULL` it contains `"score uses 3 components"`.
  - **Existing tests:** the side-panel `<tr` count stays `length(RISK_LAYERS) + 1`.
- [ ] **Step 2: Run.** **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Live check** (port 8123, Playwright):
  - Choose the Credit lens: the legend says loading, then the score shades counties.
  - A rural county tooltip shows the fallback note; a CT region shows "Insufficient data" and the missing components.
  - Tax base on Context shows the proxy wording.
  - Drill into Chicago (Cook): collateral says "metro division".
- [ ] **Step 6: Commit** `feat: Credit lens (CFPB, Urban, FHFA, job pressure) and tax base proxy layer`

---

### Task 5: Seed, manifest, end-to-end

**Files:**
- Modify: `R/data_risk.R` (`risk_seed_update` allow-list adds `cfpb_mortgage_v1`, `urban_debt_v1`, `fhfa_hpi_v1`, `msad_xwalk_2023`), `tests/testthat/test-seed.R`, `data/risk_seed/`, `manifest.json`

- [ ] **Step 1: Failing test:** the seed update copies the four new keys.
- [ ] **Step 2: Run.** **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Refresh the seed live, then check size:** `du -sh data/risk_seed` stays under about 5 MB.
- [ ] **Step 6: Regenerate `manifest.json`** (explicit file list, keeping the existing entries and the package count).
- [ ] **Step 7: End-to-end check:** all four lenses; drill on Assistance; switch to Credit while drilled in; Back to US; no console errors. Note the idle tab load time.
- [ ] **Step 8: Commit** `chore: seed credit sources and regenerate manifest`
