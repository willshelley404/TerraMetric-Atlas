# Regional Risk Map — Phases 0–1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the rent-burden definition, then replace the Metro Map tab's state choropleth and bubble map with a county choropleth. The new map has a "Stress now" layer and ACS context layers, and it drills down to census tracts ranked within their labor market.

**Architecture:**
- **Data:** fetched in-app through a two-tier cache (process memory + `.rds` on disk, per-source TTL, stale-on-error).
  - County base: tigris shapes, ACS county variables, a county→CBSA crosswalk and BLS LAUS county unemployment. It loads on tab entry.
  - Tract data: loaded per state when someone clicks a county.
- **Code split:**
  - `R/data_risk.R`: I/O.
  - `R/risk_metrics.R`: pure metric functions.
  - `R/map_risk.R`: leaflet and HTML builders.
  - `ui.R` / `server.R`: wiring.

**Tech Stack:** R 4.6, Shiny + shinydashboard, leaflet 2.2.3, sf 1.1.1, tidycensus 1.8.1, tigris 2.2.1, httr2 1.3.0, htmltools, testthat 3.3.2 (tests only).

**Spec:** `docs/superpowers/specs/2026-11-12-regional-risk-map-design.md`. Read §6, §8, §9.1, §9.4 ("Ranking frame"), §10, §11 and §12 (Phases 0–1).

**Deviation from spec:** the tract MOE "noisy" flag moves from Phase 3 into Phase 1. Tract ACS values first appear in Phase 1, and spec §5 forbids showing noisy tract values without a flag.

**Scope:** spec Phases 0 and 1 only. Phases 2–5 (Migration + Stress ahead, Assistance, Credit + tax base, Metro AI) each get their own plan once this ships. Lens choices for those phases are not added to the UI yet.

## Global Constraints

- **leaflet:** every call uses the `leaflet::` prefix (`global.R` loads leaflet after tidyquant/xts to avoid a namespace collision).
- **No new hard dependencies.** `testthat` is used only under `tests/`. `lehdr` is not used in these phases.
- **Keys** come from `Sys.getenv()`: `CENSUS_API_KEY`, `BLS_API_KEY`. To run anything that needs live keys from a shell: `export R_ENVIRON_USER="C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron"`.
- **BLS API v2:** 50 series per call, at most 8 calls in flight, rate series `LAUCN{5-digit FIPS}0000000003`, 3 years per pull (64 calls total, about 13.5 s measured). Quota is 500 queries per day.
- **Territories** are excluded via the existing `.TERRITORY_PAT`, keeping only state FIPS ≤ 56.
- **Connecticut planning regions** (FIPS 09110–09190) must join across 2024 county shapes, ACS 2024 and LAUS.
- **Cache:** directory `tools::R_user_dir("TerraMetricAtlas", "cache")`, falling back to `tempdir()`. TTLs in days:

  | Source | TTL (days) |
  |---|---|
  | LAUS | 7 |
  | ACS | 30 |
  | Shapes and CBSA crosswalk | 90 |
  | Tract bundle (ACS + shapes, per state) | 30 |
- **Tunable constants** all live in one list, `RISK_CFG`, in `R/risk_metrics.R`:
  `momentum_rising_pp = 0.5, momentum_trigger_pp = 1.0, laus_min_coverage = 0.9, laus_min_months = 2, moe_flag_ratio = 0.5`.
- **Theme colors:** panel `#1e2640`, page `#161b27`, accent `#00b4d8`, muted `#9aa3b2`, text `#e0e0e0`, worse `#ff6b6b`, border `#2a3042`.
- **As-of labels:** every layer's legend title includes its source and as-of date, e.g. `"Stress now — BLS LAUS, Sep 2026"`.
- **Labor-market frame:** a tract in a CBSA (metro *or* micro) ranks against all tracts in that CBSA, across states. Tracts outside any CBSA rank against the non-CBSA tracts of their state. Every tract also carries a state rank.
- **Tooltip layout** is option B (spec §10): title, subtitle, large value, rank pill, optional "Driven by" (max 3), footer.

## Review Focus

1. **Cold start with BLS failing** (quota exhausted or network down). The map still renders the ACS layers. Stress now shows "unavailable" in its legend and "Insufficient data" in tooltips, and nothing crashes. Test: `test-county-layers.R::"missing LAUS leaves Stress now NA and marks layer unavailable"` (Task 5).
2. **Connecticut planning regions.** `09110` must join shapes, ACS and LAUS. Test: `test-county-layers.R::"Connecticut planning region joins"` (Task 5).
3. **Clicking a rural county outside any CBSA.** The frame label must read e.g. "in non-metro Georgia", with no NA. Test: `test-tract-ranking.R::"non-CBSA tracts rank within state non-CBSA frame"` (Task 6).
4. **A multi-state metro where one state's tract fetch fails.** The available states still display, and the side panel lists the missing state. Test: `test-tract-ranking.R::"missing state is reported, others still ranked"` (Task 6).
5. **A tract with zero population or a zero denominator.** Rates are NA, never `Inf`/`NaN`, and the tooltip says "Insufficient data". Tests: `test-tract-moe.R::"zero denominators give NA not Inf"` (Task 6) and `test-tooltip.R::"NA value shows Insufficient data"` (Task 4).

---

### Task 1: Test harness + rent-burden fix (spec Phase 0)

**Files:**
- Create: `tests/testthat/helper-source.R`, `tests/testthat/test-rent-burden.R`
- Modify: `R/data_census.R` — `ACS_VARS` (lines 29–31), `.derive_metrics()` (lines 63–64), `build_metro_summary()` (lines 227 and 278–279)

**Interfaces:**
- Produces:
  - **`helper-source.R`:**
    - loads `dplyr`, `sf`, `htmltools`;
    - sources every existing file among `../../R/data_census.R`, `../../R/data_risk.R`, `../../R/risk_metrics.R`, `../../R/map_risk.R` (skip any that don't exist yet);
    - defines `make_acs_wide(...)`, which returns a one-row data frame holding every `*E` column `.derive_metrics()` reads, with defaults overridable by name.
  - **`ACS_VARS`** names replacing `rent_burden_30_34` and `rent_burden_35_plus`: `rent_burden_universe = "B25070_001"`, `rb_30_34 = "B25070_007"`, `rb_35_39 = "B25070_008"`, `rb_40_49 = "B25070_009"`, `rb_50_plus = "B25070_010"`, `rb_not_computed = "B25070_011"`.
  - **`rent_burden_rate`** = `round((rb_30_34E + rb_35_39E + rb_40_49E + rb_50_plusE) / (rent_burden_universeE - rb_not_computedE) * 100, 2)`.

- [ ] **Step 1: Write the failing tests** in `tests/testthat/test-rent-burden.R`

```r
test_that("rent burden counts all renters paying >=30% over computed renters", {
  df <- make_acs_wide(rent_burden_universeE = 1000, rb_not_computedE = 100,
                      rb_30_34E = 100, rb_35_39E = 80, rb_40_49E = 90, rb_50_plusE = 180)
  expect_equal(.derive_metrics(df)$rent_burden_rate, 50)   # 450 / 900
})
test_that("metro summary flags rent burden relative to US, not a fixed 30%", {
  us <- list(rent_burden_rate = 50, unemp_rate = 4, med_income = 70000,
             bachelors_plus_rate = 33, lfp_rate = 63, poverty_rate = 12, vacancy_rate = 7)
  row_hi <- data.frame(metro = "X", rent_burden_rate = 53.5, unemp_rate = 4, lfp_rate = 63,
                       poverty_rate = 12, med_income = 70000, bachelors_plus_rate = 33,
                       vacancy_rate = 7, med_rent = 1200, pop = 1e6, med_age = 38,
                       per_cap_income = 35000, med_home_val = 3e5)
  row_lo <- transform(row_hi, rent_burden_rate = 52)
  expect_true(any(grepl("Housing affordability", build_metro_summary(row_hi, us)$signals$risks)))
  expect_false(any(grepl("Housing affordability", build_metro_summary(row_lo, us)$signals$risks)))
})
```

- [ ] **Step 2: Run** `Rscript -e "testthat::test_dir('tests/testthat')"`. Expected: FAIL, because the old columns are missing and the flag still uses the fixed 30% threshold.
- [ ] **Step 3: Implement the fix.**
  - Update `ACS_VARS` and `.derive_metrics()` as listed in the Interfaces block.
  - In `build_metro_summary()`, the risk flag becomes `rb > (us_avg$rent_burden_rate %||% 50) + 3`, with the text `"Housing affordability pressure (rent burden >3 pp above US)"`.
  - The housing trend becomes `get_trend(rb, us_avg$rent_burden_rate %||% 50, FALSE)`.
- [ ] **Step 4: Run tests.** Expected: PASS. Then run `Rscript -e "source('global.R')"` with keys set. Expected: no error.
- [ ] **Step 5: Commit** `fix: rent burden counts 40%+ renters and uses renter denominator`

---

### Task 2: Two-tier cache

**Files:**
- Create: `R/data_risk.R` (cache section), `tests/testthat/test-cache.R`

**Interfaces:**
- Produces:
  - **`risk_cache_dir() -> character`:** creates the directory if needed.
  - **`risk_cached(key, ttl_days, fetch_fn, cache_dir = risk_cache_dir(), now = Sys.time())`:**
    - Returns the cached value with `attr(, "fetched_at")` (POSIXct) and `attr(, "stale")` (logical), or `NULL`.
    - Lookup order: fresh memory entry, then fresh disk entry `file.path(cache_dir, paste0(key, ".rds"))` (promoted to memory), then `fetch_fn()`.
    - A fetch that returns non-NULL without error is stored in both tiers.
    - A fetch that errors or returns NULL falls back to any expired copy, with `stale = TRUE`. With no copy at all, it returns `NULL`.
  - **`risk_cache_clear_memory()`:** empties the process-wide `.RISK_CACHE_ENV`.

- [ ] **Step 1: Write failing tests:**
  - `"fresh hit does not call fetch"`: a counter stays at 1 after two calls.
  - `"expired entry refetches"`: pass `now = Sys.time() + 8*86400` with `ttl_days = 7`; the counter reaches 2.
  - `"fetch error serves stale copy"`: the value is equal to the first value, and `attr(, "stale")` is `TRUE`.
  - `"no cache and fetch error returns NULL"`.
  - `"disk copy survives memory clear"`: after `risk_cache_clear_memory()`, the counter stays at 1.

  All tests use `withr::local_tempdir()`, or `tempfile()` if withr is missing.
- [ ] **Step 2: Run** `Rscript -e "testthat::test_file('tests/testthat/test-cache.R')"`. Expected: FAIL (function not found).
- [ ] **Step 3: Implement** the cache in `R/data_risk.R`. Disk writes go through `tryCatch`, so an unwritable directory only loses disk caching.
- [ ] **Step 4: Run tests.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: two-tier cache for regional risk data`

---

### Task 3: BLS LAUS county fetch + Stress now metric

**Files:**
- Modify: `R/data_risk.R` (LAUS section)
- Create: `R/risk_metrics.R`, `tests/testthat/fixtures/bls_laus_sample.json`, `tests/testthat/test-laus.R`

**Interfaces:**
- Consumes: `risk_cached()` (Task 2).
- Produces:
  - **`laus_series_ids(fips) -> character`** returns `paste0("LAUCN", fips, "0000000003")`.
  - **`parse_bls_laus(results) -> tibble(fips chr, date Date, rate dbl)`**
    - `results` is the parsed BLS `Results$series` list.
    - `fips = substr(seriesID, 6, 10)`, `date = as.Date(paste0(year, "-", substr(period, 2, 3), "-01"))`.
    - Drops `period == "M13"` and non-numeric values (`"-"`).
  - **`fetch_laus_county(fips, years = 3, key = Sys.getenv("BLS_API_KEY"), max_active = 8)`**
    - Returns a tibble as above, or `NULL`.
    - Cached through `risk_cached("laus_county", 7, ...)`.
    - Request: POST to `https://api.bls.gov/publicAPI/v2/timeseries/data/` with JSON `{seriesid, startyear, endyear, registrationkey}`, one request per 50 ids, sent with `httr2::req_perform_parallel(max_active = 8, on_error = "continue")`.
    - Failed chunks (HTTP error or `status != "REQUEST_SUCCEEDED"`) are retried once, one at a time.
    - Returns `NULL` if fewer than 50% of chunks succeed.
  - **`RISK_CFG`:** the list in Global Constraints.
  - **`laus_momentum(laus, cfg = RISK_CFG) -> tibble(fips, ref_month Date, rate_now, rate_yr_ago, momentum_pp, flag)`**
    - `ref_month` is the latest month in which the number of reporting counties is at least `cfg$laus_min_coverage × max(counties reporting in any month)`.
    - `rate_now` is the mean of the months `ref_month`, −1 and −2 that exist; `rate_yr_ago` is the same for −12, −13 and −14.
    - Each window needs at least `cfg$laus_min_months` months, otherwise its value is NA.
    - `momentum_pp = round(rate_now - rate_yr_ago, 2)`.
    - `flag`: `"Trigger"` if momentum ≥ 1.0, `"Rising"` if ≥ 0.5, `"None"` otherwise, NA if momentum is NA.

- [ ] **Step 1: Write the fixture.** It is a hand-written JSON copy of a BLS response, `{"status":"REQUEST_SUCCEEDED","Results":{"series":[...]}}`. It holds two series, `LAUCN130950000000003` (Dougherty GA) and `LAUCN091100000000003` (CT planning region), each with three entries:
  - one `M13` entry;
  - one entry with value `"-"`;
  - one normal month.
- [ ] **Step 2: Write failing tests in `test-laus.R`.**
  - **Parser:**
    - `"parser keeps monthly numeric rows and extracts FIPS"`: `nrow == 2` and `fips == c("13095", "09110")`.
    - `"series ids are LAUCN + fips + 0000000003"`. `laus_series_ids(fips)` is a small exported helper inside `fetch_laus_county`'s file.
  - **Momentum** (synthetic monthly data built in-test for 3 counties over 15 months ending 2026-09-01):
    - `"rising county flagged Rising"`: county A is 4.0 a year ago and 4.6 now, giving `momentum_pp == 0.6` and `flag == "Rising"`.
    - `"one missing month still computes"`: county B has no 2025-10 row (the October 2025 shutdown gap, inside its year-ago window). It still gets a non-NA momentum.
    - `"two missing months gives NA"`: county C is missing two of the three now-months, so its momentum and flag are NA.
    - `"thin latest month is not the reference month"`: add a 2026-10 row for county A only, so 1 of 3 report. Expect `ref_month == as.Date("2026-09-01")`.
    - `"trigger at 1.0 pp"`.
- [ ] **Step 3: Run tests.** Expected: FAIL.
- [ ] **Step 4: Implement** `laus_series_ids()`, `parse_bls_laus()` and `fetch_laus_county()` in `R/data_risk.R`, and `RISK_CFG` and `laus_momentum()` in `R/risk_metrics.R`.
- [ ] **Step 5: Run tests.** Expected: PASS.
- [ ] **Step 6: Live check** (keys set):

  ```bash
  Rscript -e "source('global.R'); x <- fetch_laus_county(c('13295','47065','09110')); print(laus_momentum(x))"
  ```

  Expected: 3 rows, `ref_month` 2026-09-01 or later, numeric momentum.
- [ ] **Step 7: Commit** `feat: county LAUS fetch and Stress now momentum`

---

### Task 4: Ranking helpers + tooltip HTML

**Files:**
- Modify: `R/risk_metrics.R`
- Create: `R/map_risk.R` (tooltip section), `tests/testthat/test-ranking.R`, `tests/testthat/test-tooltip.R`

**Interfaces:**
- Produces:
  - **`pct_rank(x) -> numeric`** returns `100 * dplyr::percent_rank(x)`. NA stays NA.
  - **`rank_label(pct, frame_label, higher_is_worse = TRUE) -> character`**
    - Let `w` = `pct` if higher is worse, else `100 - pct`.
    - If `w >= 50`: `"Top {max(1, ceiling(100 - w))}% {frame_label}"`. Otherwise: `"Bottom {max(1, ceiling(w))}% {frame_label}"`.
    - NA gives `NA_character_`.
    - `frame_label` examples: `"nationally"`, `"in Chattanooga metro"`, `"in Georgia"`, `"in non-metro Georgia"`.
  - **`risk_tooltip_html(title, subtitle = NULL, value_text, rank_text = NULL, rank_sub = NULL, drivers = NULL, footer) -> htmltools::HTML`**
    - `drivers` is a `data.frame(label, value_text, worse = logical)`; only the first 3 rows are shown.
    - The "Driven by" block is omitted when `drivers` is NULL or has 0 rows.
    - An NA `value_text` renders as `"Insufficient data"`.
    - All text is HTML-escaped.
    - Styling follows the spec §10 option B mockup (`.superpowers/brainstorm/1971-1794501828/content/tooltip-options.html`) with the theme colors from Global Constraints.

- [ ] **Step 1: Write failing tests.**
  - **Ranking:**
    - `rank_label(88, "nationally") == "Top 12% nationally"`
    - `rank_label(30, "in Georgia") == "Bottom 30% in Georgia"`
    - `rank_label(99.6, "nationally") == "Top 1% nationally"`
    - `rank_label(88, "nationally", higher_is_worse = FALSE) == "Bottom 12% nationally"`
    - `is.na(rank_label(NA, "x"))`
    - `pct_rank(c(1, 2, NA, 3)) == c(0, 50, NA, 100)`
  - **Tooltip:**
    - `"title is escaped"`: `"O'Brien & Co"` appears as `O&#39;Brien &amp; Co`.
    - `"drivers capped at 3"`: given 5 driver rows, only the first 3 labels appear.
    - `"no drivers block when NULL"`: the output has no `"Driven by"`.
    - `"NA value shows Insufficient data"`.
- [ ] **Step 2: Run.** Expected: FAIL.
- [ ] **Step 3: Implement** the four functions.
- [ ] **Step 4: Run.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: percentile rank labels and option-B tooltip builder`

---

### Task 5: County base data + county layers + county map

**Files:**
- Modify: `R/data_risk.R` (county section), `R/risk_metrics.R`, `R/map_risk.R`
- Create: `tests/testthat/test-county-layers.R`, `tests/testthat/test-county-map.R`

**Interfaces:**
- Consumes:
  - `risk_cached()`, `fetch_laus_county()`, `laus_momentum()`, `pct_rank()`, `rank_label()`, `risk_tooltip_html()`;
  - the existing `ACS_VARS`, `.derive_metrics()`, `.best_acs_year()`, `VAR_CFG`, `.TERRITORY_PAT`.
- Produces:
  - **`county_cbsa_xwalk(counties_sf, cbsa_sf) -> tibble(GEOID, cbsa_geoid, cbsa_name, cbsa_type)`**
    - Joins each county's `sf::st_point_on_surface()` to the CBSA polygons with `sf::st_join(join = sf::st_within)`.
    - `cbsa_name` keeps the text before the first comma (same as `fetch_metro_acs()`).
    - `cbsa_type` is `"metro"` when LSAD is `M1` and `"micro area"` when it is `M2`.
    - Counties outside every CBSA get NA.
    - Wherever this plan writes `"{cbsa_name} metro"` (tooltip subtitle, `frame_label`), use `"{cbsa_name} {cbsa_type}"`. Omit the subtitle when `cbsa_name` is NA.
  - **`fetch_county_base(year = NULL) -> sf`**
    - Columns: `GEOID, name` (e.g. `"Walker County, GA"`), `state_fips, state_abbr, state_name, cbsa_geoid, cbsa_name`, plus the same ACS metric columns `fetch_metro_acs()` produces (`pop, unemp_rate, lfp_rate, poverty_rate, vacancy_rate, rent_burden_rate, bachelors_plus_rate, med_income, per_cap_income, med_home_val, med_rent, med_age`).
    - Attributes: `acs_year`, `fetched_at`.
    - Sources:
      - shapes: `tigris::counties(cb = TRUE, resolution = "20m", year = 2024)`, transformed to EPSG 4326, cache key `"county_shapes_2024"`;
      - CBSAs: `tigris::core_based_statistical_areas(cb = TRUE, resolution = "20m", year = 2023)`, keeping both metro and micro, cache key `"cbsa_xwalk_2023"`;
      - ACS: `tidycensus::get_acs(geography = "county", variables = ACS_VARS, output = "wide")`, cache key `"county_acs_{year}"`.
  - **`build_county_layers(county_base, momentum, laus_stale = FALSE) -> sf`**
    - Left-joins `momentum` by `GEOID == fips`. `momentum` may be NULL.
    - Adds `pct_<col>` via `pct_rank()` for each layer column in `RISK_LAYERS`.
    - Sets `attr(, "layer_status")`, a named character vector per layer:
      - `"ok"` by default;
      - `"unavailable"` for Stress now when `momentum` is NULL;
      - `"stale"` for Stress now when `laus_stale` is TRUE (the caller passes `isTRUE(attr(laus, "stale"))`).
    - A stale layer's legend title gets the suffix `" (stale)"`.
    - Sets `attr(, "asof")`, a named character vector per layer: Stress now gets `format(ref_month, "%b %Y")` and ACS layers get `"ACS {year-4}–{year}"`.
  - **`RISK_LAYERS`** (in `R/map_risk.R`): a named list. Each entry has `label`, `col`, `fmt` (`"%"`, `"$"` or `"pp"`), `pal`, `rev`, `higher_is_worse`, `source` and `levels` (`"county"` or `c("county", "tract")`).
    - `stress_now`: `col = "momentum_pp"`, `fmt = "pp"`, diverging `"RdBu"` with `rev = TRUE` and its domain centered on 0, `higher_is_worse = TRUE`, `source = "BLS LAUS"`, county only.
    - Plus one entry per `VAR_CFG` variable, reusing its label, format and palette, `source = "Census ACS 5-yr"`, county and tract.
  - **`build_risk_county_map(county_layers, layer_id, fill_opacity = 0.82) -> leaflet`**
    - Polygons: `layerId = GEOID`, `group = "counties"`, one label per county from `risk_tooltip_html()`.
      - Title: `name`. Subtitle: `paste(cbsa_name, "metro")`.
      - Value: formatted. Rank: `rank_label(pct, "nationally")`.
      - Footer: `"{source}, {asof}"`. For Stress now, the value also carries the flag, e.g. `"+0.7 pp · Rising"`.
    - Legend title: `"{label} — {source}, {asof}"`, or `"{label} — unavailable"`.
    - Initial view: `setView(-96, 38, 4)`.

- [ ] **Step 1: Write failing tests in `test-county-layers.R`.** They use two synthetic 1×1 square counties, `"13295"` and `"09110"`, and one CBSA square covering only the first.
  - `"xwalk assigns CBSA by point on surface"`: county 13295 has `cbsa_geoid == "16860"` and county 09110 has NA.
  - `"Connecticut planning region joins"`: `build_county_layers()` gives `09110` a non-NA `momentum_pp` when the momentum fixture contains `fips = "09110"`.
  - `"missing LAUS leaves Stress now NA and marks layer unavailable"`: `momentum = NULL` gives all-NA `momentum_pp` and `attr(, "layer_status")[["stress_now"]] == "unavailable"`.
  - `"pct columns exist for every layer"`.
- [ ] **Step 2: Write failing tests in `test-county-map.R`.**
  - `"county map has one polygon per county with GEOID layerIds"`: inspect `m$x$calls` for `addPolygons`; its `layerId` equals the GEOIDs.
  - `"legend title carries source and as-of"`: the `addLegend` title contains `"BLS LAUS, Sep 2026"`.
  - `"unavailable layer legend says unavailable"`.
  - `"stale LAUS legend says (stale)"`.
- [ ] **Step 3: Run.** Expected: FAIL.
- [ ] **Step 4: Implement** the five items above.
- [ ] **Step 5: Run.** Expected: PASS.
- [ ] **Step 6: Live check:**

  ```bash
  Rscript -e "source('global.R'); b <- fetch_county_base(); print(nrow(b)); print(table(is.na(b\$cbsa_geoid)))"
  ```

  Expected: about 3,140 rows (50 states + DC), and about 1,900 counties in a CBSA. Record the exact counts in the commit message.
- [ ] **Step 7: Commit** `feat: county base, Stress now and context layers, county risk map`

---

### Task 6: Tract drill-down data + labor-market ranking

**Files:**
- Modify: `R/data_risk.R` (tract section), `R/risk_metrics.R`
- Create: `tests/testthat/test-tract-moe.R`, `tests/testthat/test-tract-ranking.R`

**Interfaces:**
- Consumes: `risk_cached()`, `ACS_VARS`, `.derive_metrics()`, `pct_rank()`, `fetch_county_base()` output columns.
- Produces:
  - **`tract_moe_flags(df, cfg = RISK_CFG) -> df`**
    - Input is wide ACS with both `*E` and `*M` columns.
    - Adds `poverty_rate_moe` (`tidycensus::moe_prop` on poverty count / universe) and `rent_burden_rate_moe` (`moe_sum` of the four burden bins, then `moe_prop` against universe − not computed, with the universe MOE used as the denominator MOE).
    - Adds `noisy = (poverty_rate_moe / poverty_rate > cfg$moe_flag_ratio) | (rent_burden_rate_moe / rent_burden_rate > cfg$moe_flag_ratio)`. NA is treated as FALSE.
    - Any zero or NA denominator gives an NA rate (replace non-finite values with NA).
  - **`fetch_state_tracts(state_fips, year) -> sf`**
    - Covers every tract in the state: `GEOID` (11 characters), `county_geoid = substr(GEOID, 1, 5)`, the ACS metric columns, `noisy`.
    - Source: `tidycensus::get_acs(geography = "tract", state = state_fips, variables = ACS_VARS, output = "wide", geometry = TRUE, cb = TRUE)`, transformed to EPSG 4326.
    - Cache key `"tracts_{state_fips}_{year}"`, TTL 30. Returns NULL on failure.
  - **`tracts_for_county(county_geoid, county_base, fetch = fetch_state_tracts) -> list(tracts = sf, frame_label = chr, missing_states = chr)`**
    - The frame is the county's CBSA if it has one, otherwise "non-CBSA counties of the county's state".
    - It fetches every state touched by the frame, plus the clicked county's state.
    - Ranking runs on the whole-state data; the returned `tracts` keep only the frame's counties.
  - **`rank_tracts(tracts_all, county_base, frame_counties, vars) -> sf`**
    - Adds `lm_pct_<var>` (within the tracts of `frame_counties`) and `st_pct_<var>` (within each tract's own state, over all of that state's tracts).
    - Also adds `frame_label` and `state_label` (`"in {state_name}"`).
    - `frame_label` is `"in {cbsa_name} metro"`, or `"in non-metro {state_name}"`.

- [ ] **Step 1: Write failing tests in `test-tract-moe.R`.**
  - `"noisy flag when MOE exceeds half the estimate"`.
  - `"zero denominators give NA not Inf"`: a tract with `poverty_universeE = 0` gets NA `poverty_rate`, and `noisy` is FALSE.
- [ ] **Step 2: Write failing tests in `test-tract-ranking.R`.** They use synthetic data and inject a fake `fetch`:
  - `"metro tracts rank across state lines"`: a CBSA has counties in TN (`47065`) and GA (`13295`). `lm_pct` is computed over both states' frame tracts together, and `frame_label == "in Chattanooga metro"`.
  - `"state rank uses whole state"`: a GA tract outside the frame still affects GA `st_pct`.
  - `"non-CBSA tracts rank within state non-CBSA frame"`: `frame_label == "in non-metro Georgia"` and no NA in `lm_pct_poverty_rate` where `poverty_rate` is present.
  - `"missing state is reported, others still ranked"`: the fake fetch returns NULL for `"13"`. `missing_states == "13"` and the TN tracts are still ranked.
- [ ] **Step 3: Run.** Expected: FAIL.
- [ ] **Step 4: Implement** the four functions.
- [ ] **Step 5: Run.** Expected: PASS.
- [ ] **Step 6: Live check:**

  ```bash
  Rscript -e "source('global.R'); b <- fetch_county_base(); r <- tracts_for_county('47065', b); print(r\$frame_label); print(table(substr(r\$tracts\$GEOID,1,2)))"
  ```

  Expected: `"in Chattanooga metro"`, with tracts from both state `47` and state `13`.
- [ ] **Step 7: Commit** `feat: tract drill-down data with labor-market and state ranks`

---

### Task 7: Tract map layer + side panel

**Files:**
- Modify: `R/map_risk.R`
- Create: `tests/testthat/test-tract-map.R`, `tests/testthat/test-side-panel.R`

**Interfaces:**
- Consumes: `RISK_LAYERS`, `risk_tooltip_html()`, `rank_label()`, `tracts_for_county()` output, `build_county_layers()` output.
- Produces:
  - **`add_tract_layer(map, tracts, layer_id, county_layers) -> map`**
    - Works on either a `leaflet` object or a `leafletProxy`.
    - Steps:
      1. `clearGroup("tracts")`;
      2. redraw the counties at `fill_opacity = 0.25` (reuse the polygon-drawing internals of `build_risk_county_map()`; factor them into `.add_county_polygons(map, county_layers, layer_id, fill_opacity)`);
      3. `addPolygons(group = "tracts", layerId = GEOID)`;
      4. `fitBounds` to the tracts' bounding box.
    - Noisy tracts get a dashed `#fcc419` outline (`dashArray = "4"`).
    - Tooltip:
      - Rank: `rank_label(lm_pct, frame_label)`. Rank sub-line: `rank_label(st_pct, state_label)`.
      - For noisy tracts, the footer adds `"High margin of error"`.
    - A county-only layer (Stress now) draws no tracts and keeps the counties at full opacity.
  - **`reset_to_us(map, county_layers, layer_id) -> map`:** `clearGroup("tracts")`, redraw the counties at 0.82, `setView(-96, 38, 4)`.
  - **`risk_side_panel(area, level = c("county", "tract"), county_layers, missing_states = character(0)) -> htmltools tag`**
    - A table with columns Component | Value | Percentile | As of | Source, one row per layer that applies to the level.
    - County: national percentile. Stress now rows also show the rate now, the rate a year earlier and the flag.
    - Tract: two percentile columns, Labor market and State. Noisy tracts get a note.
    - Missing states are listed as `"Tract data unavailable for: {state names}"`.

- [ ] **Step 1: Write failing tests:**
  - `"tract layer uses tract GEOID layerIds and dims counties"`: the county `addPolygons` call has `fillOpacity` 0.25.
  - `"noisy tracts get dashed outline"`.
  - `"county-only layer adds no tracts"`.
  - `"tract tooltip shows labor-market rank and state rank"`: it contains both `"in Chattanooga metro"` and `"in Georgia"`.
  - `"side panel lists as-of and source per row"`.
  - `"side panel reports missing states"`.
- [ ] **Step 2: Run.** Expected: FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: tract layer, reset, and side panel`

---

### Task 8: Wire the Regional Risk tab; retire the state choropleth and bubble map

**Files:**
- Modify:
  - `ui.R`: line 50 (menu item); lines 289–386 (`tabItem("metro", …)`).
  - `server.R`: lines 30–33 (`rv` fields); lines 1249–1431 (tab-entry observer, `metro_map`, `tbl_metro`, `metro_bubble_map`).
  - `R/data_census.R`: delete `fetch_state_acs()`, `build_state_map()`, `METRO_VAR_CFG`, `fetch_metro_sf()`, `build_metro_bubble_map()` (lines 76–128 and 331–485). Keep `VAR_CFG`.

**Interfaces:**
- Consumes: `fetch_metro_acs()`, `fetch_county_base()`, `fetch_laus_county()`, `laus_momentum()`, `build_county_layers()`, `build_risk_county_map()`, `tracts_for_county()`, `add_tract_layer()`, `reset_to_us()`, `risk_side_panel()`.
- Produces:
  - **UI ids:** `risk_lens` (only choice for now: `"Context" = "context"`), `risk_layer`, `risk_jump_metro`, `risk_back_us`, `risk_breadcrumb`, `risk_map`, `risk_side_panel`.
  - **Existing ids kept:** `metro_ai_select`, `btn_metro_ai`, `metro_ai_output`, `tbl_metro_cbsa`.
  - **`rv` fields:** `county_layers`, `risk_tracts` (the `tracts_for_county()` list), `risk_sel` (`list(level, geoid)`). `census_data` and `metro_sf` are removed.

- [ ] **Step 1: Update `ui.R`.**
  - Menu item: `menuItem("Regional Risk", tabName = "metro", icon = icon("map-marked-alt"))`. The tabName stays `"metro"` so nothing else breaks.
  - Tab body, with no `tabsetPanel`:
    1. One box titled `"Regional Risk"`:
       - `column(3)`: lens select, layer select (choices from `RISK_LAYERS` labels, default `"stress_now"`), `selectizeInput("risk_jump_metro", "Jump to metro:", choices = NULL)`, `actionButton("risk_back_us", "Back to US")`, `uiOutput("risk_breadcrumb")`, and the existing info-note style describing data sources.
       - `column(9)`: `leafletOutput("risk_map", height = "560px")` wrapped in the existing `withSpinner` style.
    2. `uiOutput("risk_side_panel")`.
    3. The existing Metro AI box.
    4. The existing metro rankings heading and `tbl_metro_cbsa`.
- [ ] **Step 2: Update `server.R`.**
  - **Tab-entry observer** (`input$tabs == "metro"` and `rv$county_layers` NULL), inside `withProgress` with these detail strings:
    1. "Metro ACS…" → `fetch_metro_acs()`;
    2. "County shapes and ACS…" → `fetch_county_base()`;
    3. "County unemployment (BLS)…" → `fetch_laus_county(county_base$GEOID)` then `laus_momentum()`;
    4. `build_county_layers()`.
  - **`output$risk_map`:** a loading placeholder (reuse the current placeholder control HTML) until `rv$county_layers` exists, then `build_risk_county_map(rv$county_layers, input$risk_layer)`.
  - **Layer change while drilled in:** use `leafletProxy` with `add_tract_layer()` so the view isn't lost.
  - **`observeEvent(input$risk_map_shape_click)`:**
    - A 5-character id: `rv$risk_tracts <- withProgress(tracts_for_county(id, county_base))`, then `add_tract_layer()` and `rv$risk_sel <- list(level = "county", geoid = id)`.
    - An 11-character id: `rv$risk_sel <- list(level = "tract", geoid = id)`.
  - **`risk_jump_metro`:**
    - Choices are the sorted unique `cbsa_name` values, filled with `updateSelectizeInput(server = TRUE)` once layers load.
    - Selecting one drills into its most populous county.
  - **`risk_back_us`:** `reset_to_us()`, and clear `rv$risk_tracts` and `rv$risk_sel`.
  - **`risk_breadcrumb`:** `"US › {state_name} › {county name}"`, plus `" › Tract {GEOID}"` when a tract is selected.
  - **`risk_side_panel`:** renders `risk_side_panel()` for `rv$risk_sel`. Before anything is selected, it shows a muted hint: "Click a county to see its breakdown and load its tracts."
  - **Delete** the `metro_map`, `tbl_metro` and `metro_bubble_map` outputs and the old observer. The Metro AI and `tbl_metro_cbsa` code stays as is.
- [ ] **Step 3: Delete the dead functions** from `R/data_census.R`. Then run `grep -rn "fetch_state_acs\|build_state_map\|fetch_metro_sf\|build_metro_bubble_map\|METRO_VAR_CFG\|metro_map_var\|metro_min_pop\|map_var" --include=*.R .` (excluding `ARCHIVE/`). Expected: no matches.
- [ ] **Step 4: Run the full test suite** with `Rscript -e "testthat::test_dir('tests/testthat')"`. Expected: all PASS.
- [ ] **Step 5: Run the app and verify in a browser** (keys set).
  - Start it with `Rscript -e "shiny::runApp('.', port = 4321, launch.browser = FALSE)"` as a background process. Drive it with the Playwright browser tools at `http://127.0.0.1:4321`.
  - Check each of these:
    1. The "Regional Risk" menu item opens the tab, which has no sub-tabs. A cold load finishes in about 30 s or less. Record the actual time.
    2. The counties fill the US with no gaps. The Stress now legend reads `"Stress now — BLS LAUS, {Mon YYYY}"`.
    3. Hovering a county shows the option-B tooltip with a `"Top N% nationally"` pill.
    4. Clicking Hamilton County, TN loads tracts in both TN and GA. A Georgia tract's tooltip shows `"in Chattanooga metro"` and `"in Georgia"`.
    5. Switching the layer to "Poverty Rate" while drilled in keeps the view.
    6. "Back to US" resets the view.
    7. The Metro AI box and the metro table still work.
    8. A rural county outside any CBSA shows `"in non-metro {State}"`.
  - Save screenshots to the scratchpad and do not commit them.
- [ ] **Step 6: Commit** `feat: Regional Risk tab replaces state choropleth and bubble map`
