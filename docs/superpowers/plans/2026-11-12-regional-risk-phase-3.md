# Regional Risk Map — Phase 3 Implementation Plan (Assistance lens)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the Assistance lens. Score, exposure, cushion and exposure-weighted workers are shown for tracts (labor-market ranked, plus a state rank) and for counties (nationally ranked). It is built from LEHD LODES RAC with an ACS C24030 fallback, the Phase 2 sector weights and county job pressure.

**Architecture:**
- **I/O** (`R/data_risk.R`): a LODES year probe and a per-state RAC loader (lehdr if installed, else direct download), aggregated to tracts. The ACS tract industry fallback loads only when LODES is unusable.
- **Metrics** (`R/risk_metrics.R`): pure functions for exposure, cushion, job pressure and the geometric-mean score. Both the tract path (`tracts_for_county`) and the county path (`build_county_layers`) call them.
- **Display:** four new `RISK_LAYERS` entries on lens `"assistance"`, with drivers in tract and county tooltips and source/count lines in the side panel.

**Tech Stack:** R 4.6, Shiny, leaflet, sf, tidycensus 1.8.1, httr2, data.table (`fread`, already in manifest), testthat.

**Spec:** `docs/superpowers/specs/2026-11-12-regional-risk-map-design.md`. Read §8 (LODES row, "LODES gaps"), §9.3, §9.4, §10 (tract tooltip), §11 (`lehdr` rules) and §12 (Phase 3).

**Builds on:** Phase 2 (`docs/superpowers/plans/2026-11-12-regional-risk-phase-2.md`, as amended by its final-review fix wave). Interfaces as of commit `3f76bd5`:
- **`RISK_SECTORS`:** column `cns` holds comma-separated CNS codes per group.
- **`sector_weights()`:** its output is a server part of its own (`sector_weights`, computed from FRED alone). `build_county_layers(..., sector_weights =)` always puts it on `attr(county_layers, "sector_weights")`.
  - **Never use `%||%` on data frames.** The app redefines `%||%` with scalar semantics (global.R, R/synopsis_tier1.R), so use explicit `if (is.null(x))`.
- **`stress_ahead()`:**
  - columns `share_<group>`, an unrounded `stress_ahead_pp`, `sa_drv*`/`sa_lift*`;
  - `attr(, "partial")` when a sector's momentum is missing.
- **`build_county_layers` signature:** `(county_base, momentum, laus_stale, laus_partial, migration, loading, stress_ahead, metro_momentum, sector_weights)`.
  - There is one loading mechanism: `loading` is a character vector of component names (`"stress_now"`, `"migration"`, `"stress_ahead"`).
  - `laus_loading` no longer exists.
- **Display helpers:** `risk_drivers_vec(labels, values, worse, n)` (NULL elements are treated as NA), the `standing` lines, the `digits` field, and `loading_text` / `partial_text` on layer configs.
- **Server parts:**
  - `risk_set_parts()`, `risk_try_part(name, ...)` (sets parts and rebuilds without redrawing, restoring the parts on error), `risk_redraw_safe()` and `risk_rebuild()`;
  - the deferred `rv$laus_go` step does one redraw at the end.
  - A new server part must go through `risk_try_part`.

## Global Constraints

- All Phase 2 constraints still hold: `leaflet::` prefix, no new hard dependencies, the `.RISK_UA` User-Agent on census.gov downloads, everything in-app.
- **`lehdr`** is optional: `requireNamespace("lehdr", quietly = TRUE)` plus `tryCatch` around `lehdr::grab_lodes(state = st, year = yr, lodes_type = "rac", job_type = "JT00", segment = "S000", agg_geo = "tract")`. On error, or when the package is missing, use the direct download. Never add it to `manifest.json`.
- **LODES URL:** `https://lehd.ces.census.gov/data/lodes/LODES8/{st}/rac/{st}_rac_S000_JT00_{year}.csv.gz`, where `st` is the lowercase postal code.
- **Read `h_geocode` as character.** `data.table::fread(colClasses = list(character = "h_geocode"))`. The default integer64 read drops Alabama's leading zero.
- **Alaska (`"02"`) and Michigan (`"26"`) are hard-excluded from LODES** (`RISK_CFG$lodes_exclude_states`). Their files exist but hold only out-of-state workers (about 1–3% of real jobs). They always use the ACS fallback.
- **Connecticut LODES tracts use the old counties (09001–09015).** ACS 2024 uses planning regions. Remap CT LODES tracts to ACS GEOIDs by the 6-digit tract code (`substr(GEOID, 6, 11)`), skipping codes that start with `"99"` (water tracts, the only duplicates).
- **TTLs (days):** LODES year probe 30; LODES RAC per state 90; ACS tract industry per state 30.
  - LODES and tract-industry files are **not** seeded (one per state, like tracts).
  - The year probe **is** seeded.
- **Ranking frame (spec §9.4, unchanged):** the score's component percentiles are computed within the tract's labor-market frame (CBSA across states; non-CBSA tracts within the state). The state rank uses the same formula computed within the state.
- **Score:** `100 × (pct(E)/100 × pct(cushion)/100 × J)^(1/3)`. Here `J` is the county job-pressure fraction in 0..1.
- **"Exposure-weighted workers"** is always labelled as an index-like headcount, not a layoff forecast.

## Verified source facts (spike 2026-11-12)

- **LODES 2024 is the latest year;** 2025 returns 404. `HEAD` returns 200/404 reliably.
- **TN 2024:** 3.2 MB, 128,544 blocks → 1,701 tracts. Download takes about 2 s, `fread` about 2 s, aggregation is instant. LODES tracts and ACS 2024 tracts match 100%.
- **RAC columns used:** `h_geocode` (15-char block), `C000` (total jobs), `CA01` (age ≤ 29), `CE01` (≤ $1,250/month), `CNS01`–`CNS20`. `C000` equals the sum of the `CNS` columns.
- **ACS tract C24030:** the 13 group sums (male + female, per `RISK_SECTORS$acs_m`/`acs_f`) add up exactly to `C24030_001`.

## Review Focus

1. **Drilling into an Alaska or Michigan county.**
   - Tracts still get exposure from ACS C24030 and a cushion from ACS-only inputs (rent burden + poverty).
   - The side panel says "Industry mix: ACS (LODES incomplete for this state)".
   - No LODES download is attempted.
   - Test: `test-lodes.R::"AK and MI skip LODES and use ACS fallback"` (Task 1) and `test-assistance-tracts.R::"ACS fallback scores with 2-input cushion"` (Task 3).
2. **A Connecticut drill.** LODES tracts must join ACS planning-region tracts. Test: `test-lodes.R::"CT LODES tracts remap to planning-region GEOIDs"` (Task 1).
3. **Drilling in before Stress ahead / sector weights have loaded** (deferred step still running), or after FRED failed.
   - Assistance values are NA ("Insufficient data").
   - The legend/status says loading or unavailable.
   - Context layers still draw.
   - Test: `test-assistance-tracts.R::"no weights gives NA assistance, other columns intact"` (Task 3).
4. **A tract with zero resident jobs in LODES** (e.g. a park, airport or water tract) or zero ACS employment. Exposure and shares are NA, never NaN, and the score is NA. Test: `test-assistance-metrics.R::"zero jobs gives NA exposure and shares"` (Task 2).
5. **A county where both Stress now and Stress ahead are NA** (BLS down and FRED down). `J` is NA, so the score is NA, and the driver list says job pressure is missing. Test: `test-assistance-metrics.R::"job pressure uses available signals, NA when none"` (Task 2).

---

### Task 1: LODES loader with lehdr/direct fallback, AK/MI exclusion, CT remap

**Files:**
- Modify: `R/data_risk.R` (new "LODES" section), `R/risk_metrics.R` (`RISK_CFG` additions, `aggregate_lodes_rac`, `lodes_ct_remap`)
- Create: `tests/testthat/test-lodes.R`, `tests/testthat/fixtures/lodes_rac_sample.csv` (about 6 blocks over 3 tracts, real column header)

**Interfaces:**
- Produces:
  - **`RISK_CFG` additions:** `lodes_exclude_states = c("02", "26")`.
  - **`aggregate_lodes_rac(blocks) -> tibble(GEOID, C000, CA01, CE01, CNS01..CNS20)`:**
    - `GEOID = substr(h_geocode, 1, 11)`;
    - sums by GEOID;
    - drops `createdate` and the other columns.
  - **`lodes_ct_remap(lodes, acs_geoids) -> tibble`:**
    - for rows with state `"09"`, replaces GEOID with the ACS GEOID whose `substr(, 6, 11)` matches, among `acs_geoids` starting `"09"` and not having a tract code starting `"99"`;
    - unmatched CT rows are dropped;
    - non-CT rows pass through unchanged.
  - **`.lodes_url(st_abbr, year)`.**
  - **`lodes_latest_year(probe = .lodes_head_ok, today = Sys.Date(), cache_dir = risk_cache_dir()) -> integer or NULL`:**
    - probes `"de"` for `year(today) - 1` down to `year(today) - 5`;
    - cached under key `"lodes_year_v1"` for 30 days, with `bg = list(id = "lodes_year", fn = "lodes_latest_year", args = list())`;
    - a failed probe is not cached (same rule as `risk_acs_year`).
  - **`.lodes_head_ok(url) -> logical`:** a HEAD request with `.RISK_UA` and `req_error(is_error = \(r) FALSE)`; TRUE when the status is 200.
  - **`fetch_lodes_rac(state_fips, year = NULL, cache_dir = risk_cache_dir(), download = .lodes_download) -> tibble or NULL`:**
    - returns NULL at once (no fetch, no cache entry) when `state_fips %in% RISK_CFG$lodes_exclude_states` or `year` resolves to NULL;
    - otherwise cached under `sprintf("lodes_rac_%s_%d", state_fips, year)` for 90 days, with no `bg`;
    - `download(st_abbr, year)` returns a block-level data.frame. The default tries `lehdr` (if installed) and falls back to `httr2` + `.RISK_UA` → tempfile → `data.table::fread(colClasses = list(character = "h_geocode"))`;
    - the result is `aggregate_lodes_rac(...)` with `attr(, "lodes_year") <- year`.
  - **State postal codes:** come from `unique(tigris::fips_codes[, c("state_code", "state")])`. tigris is already a dependency.

- [ ] **Step 1: Write failing tests** in `test-lodes.R`:
  - `"blocks aggregate to tracts, leading zeros kept"`: the fixture includes an Alabama block `"010010201001000"`; the output GEOID `"01001020100"` exists and `C000` sums are correct.
  - `"CT LODES tracts remap to planning-region GEOIDs"`:
    - with LODES GEOIDs `c("09003500100", "09003990100", "47065000100")` and `acs_geoids = c("09110500100", "09110990100", "09120990100")`;
    - the output has `"09110500100"` and `"47065000100"`;
    - the 99xx row is dropped.
  - `"AK and MI skip LODES and use ACS fallback"`: `fetch_lodes_rac("02", 2024, download = function(...) stop("must not download"))` returns NULL; the same for `"26"`.
  - `"latest year probe steps back on 404"`: `lodes_latest_year(probe = function(u) grepl("2024", u), today = as.Date("2026-11-12"), cache_dir = tempfile())` returns `2024`. All-FALSE returns NULL.
  - `"fetch caches per state-year"`: using a counting `download` stub that returns the fixture, two calls → counter 1. Use `risk_cache_clear_memory()` and a temp cache dir.
- [ ] **Step 2: Run** `Rscript -e "testthat::test_dir('tests/testthat', filter='lodes')"`. Expected: FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run the full suite.** Expected: PASS. Live check (keys not needed): `Rscript -e "try(source('global.R')); y <- lodes_latest_year(); x <- fetch_lodes_rac('47', y); print(y); print(dim(x))"`. Expected: 2024, and about 1701 × 24.
- [ ] **Step 5: Commit** `feat: LODES RAC loader (lehdr or direct), AK/MI exclusion, CT tract remap`

---

### Task 2: Assistance metrics (pure)

**Files:**
- Modify: `R/risk_metrics.R` (new "Assistance" section; `RISK_CFG` additions)
- Create: `tests/testthat/test-assistance-metrics.R`

**Interfaces:**
- Consumes: `RISK_SECTORS`, the `sector_weights()` tibble (columns `group`, `w`, `m_pct`).
- Produces:
  - **`lodes_group_jobs(lodes) -> tibble(GEOID, jobs_total, jobs_<group> ×13, low_wage_share, young_share)`:**
    - `jobs_<group>` sums the CNS columns listed in `RISK_SECTORS$cns`;
    - `low_wage_share = 100 * CE01 / C000`, `young_share = 100 * CA01 / C000`;
    - NA when `C000` is 0.
  - **`acs_group_jobs(ind) -> same shape`:** from `fetch_county_industry`-style `emp_total`/`emp_<group>` columns. `low_wage_share` and `young_share` are NA (the ACS fallback has no wage or age bands).
  - **`exposure_from_jobs(jobs, weights) -> tibble(GEOID, exposure, exp_workers, weak_share)`:**
    - `exposure = Σ_s (jobs_s / jobs_total) × w_s`;
    - `exp_workers = round(Σ_s jobs_s × w_s)`;
    - `weak_share = 100 × Σ_{s: m_pct < 0} jobs_s / jobs_total`;
    - all NA when `jobs_total` is 0 or NA, or `weights` is NULL.
  - **`job_pressure(county_layers) -> numeric (0..1)`:**
    - the row-wise mean of the available values of `pct_momentum_pp / 100` and `(100 - pct_stress_ahead_pp) / 100` (lower Stress ahead = more pressure);
    - NA when both are NA.
    - Phase 4's Credit component 3 reuses this.
  - **`assistance_score(exposure, cushion_inputs, J, group) -> tibble(pct_exposure, cushion, pct_cushion, score)`:**
    - **`group`** is a vector the same length as `exposure`, the frame id;
    - **`cushion_inputs`** is a data.frame of columns where higher = thinner cushion;
    - **`pct_*`** = `pct_rank()` within `group`;
    - **`cushion`** = row-wise mean of the available within-group percentiles of `cushion_inputs`, requiring at least `RISK_CFG$cushion_min_inputs = 2`;
    - **`pct_cushion`** = within-group `pct_rank(cushion)`;
    - **`score`** = `round(100 * ((pct_exposure/100) * (pct_cushion/100) * J)^(1/3), 1)`, NA if any factor is NA.

- [ ] **Step 1: Write failing tests:**
  - `"CNS columns map to 13 groups"`: a single-tract LODES row with `CNS03 = 10` and `CNS08 = 5` gives `jobs_transport_util = 15`. `CNS12 + CNS13 + CNS14` all land in `prof_admin`. The 13 groups sum to `C000`.
  - `"zero jobs gives NA exposure and shares"`: `C000 = 0` gives NA `exposure`, `low_wage_share` and `exp_workers`, and `!any(is.nan(...))`.
  - `"exposure is the job-share-weighted mean of sector weights"`:
    - 50 jobs in manufacturing (w = 1) and 50 in educ_health (w = 0) give exposure 0.5 and `exp_workers` 50;
    - with `m_pct` < 0 only for manufacturing, `weak_share` is 50.
  - `"job pressure uses available signals, NA when none"`: `pct_momentum_pp = c(80, NA, NA)` and `pct_stress_ahead_pp = c(20, 40, NA)` give `c(0.8, 0.6, NA)`.
  - `"score is the geometric mean and needs all three"`:
    - within one group of 5 tracts, the tract with max exposure and max cushion and `J = 1` scores 100;
    - `J = NA` gives NA;
    - a tract with only 1 cushion input gives NA `cushion`.
  - `"percentiles are within group"`: two groups with identical values give identical scores per position.
- [ ] **Step 2: Run** with `filter='assistance-metrics'`. Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: assistance exposure, cushion, job pressure and score functions`

---

### Task 3: Tract assistance in the drill-down

**Files:**
- Modify: `R/data_risk.R` (`fetch_state_tracts` adds rent-burdened household counts → key `tracts_v4_…`; new `fetch_state_tract_industry`), `R/risk_metrics.R` (`tracts_for_county`, new `assistance_tracts`)
- Create: `tests/testthat/test-assistance-tracts.R`

**Interfaces:**
- Consumes: Task 1 (`fetch_lodes_rac`, `lodes_ct_remap`, `lodes_latest_year`), Task 2 (all functions).
- Produces:
  - **`fetch_state_tracts`** keeps every existing column and adds `rent_burdened_hh = rb_30_34E + rb_35_39E + rb_40_49E + rb_50_plusE`. The cache key becomes `sprintf("tracts_v4_%s_%d", st, yr)`.
  - **`fetch_state_tract_industry(state_fips, year = NULL) -> tibble(GEOID, emp_total, emp_<group>)`:** tract-level `tidycensus::get_acs` of `C24030_001` plus every `acs_m`/`acs_f`, cached under `sprintf("tract_ind_%s_%d", st, yr)` for 30 days.
  - **`assistance_tracts(tracts, jobs, weights, county_layers, frame_counties) -> tracts`:** adds these columns:
    - `exposure`, `exp_workers`, `weak_share`, `low_wage_share`, `young_share`;
    - `assist_source` (`"LODES {year}"` or `"ACS {y-4}–{y} (LODES incomplete for this state)"` / `"ACS {y-4}–{y} (LODES unavailable)"`);
    - `job_pressure` (from the tract's county via `job_pressure(county_layers)`);
    - `assist_score`, `assist_exposure` (= `round(100 * exposure, 1)`), `assist_cushion` (= `pct_cushion` within frame), `assist_workers` (= `exp_workers`);
    - `assist_score_st` (the same formula with `group = state`).

    Rules:
    - **Cushion inputs:** `low_wage_share`, `young_share`, `rent_burden_rate`, `poverty_rate`. ACS-fallback tracts have only the last two, which satisfies `cushion_min_inputs = 2`.
    - **Frame group:** tracts in `frame_counties` get group `"frame"`. Others get NA lm values (they are dropped after ranking anyway).
  - **`tracts_for_county(county_geoid, county_base, fetch = fetch_state_tracts, fetch_jobs = .tract_jobs, weights = attr(county_base, "sector_weights"), vars = .tract_vars())`:**
    - **`.tract_jobs(state_fips, acs_geoids)`:**
      - returns `lodes_group_jobs(lodes_ct_remap(fetch_lodes_rac(st), acs_geoids))` with `attr(, "source")`;
      - when that is NULL (excluded state or failure) it returns `acs_group_jobs(fetch_state_tract_industry(st))` with the fallback source text;
      - NULL if both fail.
    - It is called per state after the ACS tracts load.
    - Join the jobs, call `assistance_tracts`, then `rank_tracts`.
    - **Ranking override:** for `assist_score`, `lm_pct_` is pct within the frame and `st_pct_assist_score` is `pct_rank(assist_score_st)` within the state. Special-case these two in `rank_tracts`.
    - The return value gains `sources` (a named character vector state_fips → source text).

- [ ] **Step 1: Write failing tests** with fixture tracts. Make 4 tracts in GA `13295` (frame) and 2 in TN `47065` (frame); use a stub `fetch` and a stub `fetch_jobs`; build `county_base` via `build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = <fixture with weights attr>)`. Tests:
  - `"tracts get assistance columns and ranks"`: `assist_score` in [0, 100] for complete rows; `lm_pct_assist_score` and `st_pct_assist_score` exist; `assist_source` is `"LODES 2024"`.
  - `"ACS fallback scores with 2-input cushion"`: a `fetch_jobs` stub returning ACS-shaped jobs with source `"ACS 2020–2024 (LODES incomplete for this state)"` gives non-NA scores, NA `low_wage_share`, and the source text propagates.
  - `"no weights gives NA assistance, other columns intact"`: `weights = NULL` gives all-NA `assist_*`, while `poverty_rate` and `lm_pct_poverty_rate` are still present.
  - `"state rank uses state-frame score"`: `st_pct_assist_score` equals `pct_rank(assist_score_st)` within each state.
  - **Existing test-tract-ranking.R tests still pass.** Update their stubs only for the new `fetch_jobs` argument (default it to a stub returning NULL in those tests).
- [ ] **Step 2: Run.** Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Commit** `feat: tract assistance score from LODES (ACS fallback) in the drill-down`

---

### Task 4: County assistance values, Assistance lens layers, tooltips, side panel, wiring

**Files:**
- Modify: `R/risk_metrics.R` (`build_county_layers`), `R/map_risk.R` (`RISK_LAYERS`, `RISK_LENSES`, tract + county tooltips, side panel), `server.R` (drill passes weights; lens default), `ui.R` (info text)
- Create: `tests/testthat/test-assistance-layers.R`

**Interfaces:**
- Consumes: Tasks 2–3 and the Phase 2 `stress_ahead` columns `share_<group>` plus `emp_total`. If `stress_ahead()` does not keep `emp_total`, add it there.
- Produces:
  - **`build_county_layers`:** when `stress_ahead` (for the county shares) and `sector_weights` are both present it computes county `exposure`, `exp_workers`, `weak_share`, `assist_score` / `assist_exposure` / `assist_cushion` / `assist_workers`, with these rules:
    - **national group;** the cushion inputs are `rent_burden_rate` and `poverty_rate`; `J = job_pressure(out)`;
    - **jobs** = `emp_<group>`, recovered as `share_<group> × emp_total`;
    - **status:** component name `"assistance"` is `"loading"` whenever `"stress_ahead"` is in `loading`; `"unavailable"` if weights are missing; else `"ok"`;
    - **as-of:** `"ACS {y-4}–{y} · CES {Mon YYYY}"`;
    - `attr(out, "sector_weights")` is kept for the drill.
  - **`RISK_LAYERS` entries** with `lens = "assistance"` and `levels = c("county", "tract")`:

    | id | col | label | fmt | palette | rev | higher_is_worse | source | extra |
    |---|---|---|---|---|---|---|---|---|
    | `assist_score` | `assist_score` | "Assistance need score (0–100)" | `"score"` (`sprintf("%.0f", v)`) | `"YlOrRd"` | FALSE | TRUE | "LODES / ACS × CES sector weights" | `bins = c(0, 20, 35, 50, 65, 80, 101)` |
    | `assist_exposure` | `assist_exposure` | "Exposure to weakening sectors (index)" | `"score"` | `"YlOrRd"` | | | same | — |
    | `assist_cushion` | `assist_cushion` | "Thin financial cushion (percentile)" | `"score"` | `"YlOrRd"` | | | "LODES / ACS" | — |
    | `assist_workers` | `assist_workers` | "Exposure-weighted workers (index headcount, not a layoff forecast)" | `"n"` (`formatC(round(v), big.mark = ",", format = "d")`) | `"YlOrRd"` | | | same as score | — |

    - **`RISK_LENSES`** = `c("Assistance" = "assistance", "Migration" = "migration", "Context" = "context")`, the spec §10 order. The UI default lens stays `"context"`.
    - **Noise flag:** for assistance layers, a tract is noisy when `noisy_poverty_rate | noisy_rent_burden_rate`. `add_tract_layer` reads a new optional cfg field `noisy_cols` (character); default `paste0("noisy_", cfg$col)`.
  - **Tract tooltips** for assistance layers have "Driven by" (`risk_drivers_vec`), in this order:
    - `"Workers in weakening sectors"` (`weak_share` %);
    - `"Low-wage workers (≤$1,250/mo)"` (`low_wage_share` %; omitted when NA);
    - `"Renters cost-burdened"` (`rent_burden_rate` %);
    - then a 4th line, always shown: `"≈ Exposure-weighted workers"` (`assist_workers`).

    To get this, `risk_drivers_vec` gains `max_rows = 3`, and the tract call passes 4. Values above the median are red: worse = `lm_pct_<input> >= 50`.
  - **County tooltips** for assistance layers: drivers `"Workers in weakening sectors"`, `"Renters cost-burdened"` and `"Poverty rate"`. Missing job pressure shows `"Job pressure: missing (BLS/FRED unavailable)"` in `standing`.
  - **Side panel** (tract and county), as divs:
    - `"Industry mix source: {assist_source}"` (tract) or `"Industry mix source: ACS {y-4}–{y} (county)"`;
    - `"Exposure-weighted workers: N (index headcount, not a layoff forecast)"`;
    - `"Rent-burdened renter households: N"` (tract only; ACS ± is not computed for this count, so label it "ACS estimate");
    - `"County job pressure: NN th percentile"`.
  - **Server:** `risk_drill` passes `weights = attr(rv$county_layers, "sector_weights")` (already the default via `county_base`). No other server change is needed.

- [ ] **Step 1: Write failing tests:**
  - `"county assistance computed when weights present, loading otherwise"`: with a `stress_ahead` fixture carrying weights, `assist_score` is non-NA for Walker and Capitol; with `loading = "stress_ahead"` the status `assist_score == "loading"`.
  - `"every assistance layer is in the assistance lens at both levels"`.
  - `"tract tooltip shows 4 drivers with workers last"`: the label contains `"Workers in weakening sectors"`, `"Renters cost-burdened"` and `"≈ Exposure-weighted workers"`, in that order.
  - `"assistance noise flag uses cushion inputs"`: a tract with `noisy_poverty_rate = TRUE` gets a dashed outline on `assist_score` (`dashArray` "4" in the `addPolygons` call), and its tooltip footer contains "High margin of error".
  - `"side panel names the industry source and count caveat"`: contains `"Industry mix source: LODES 2024"` and `"not a layoff forecast"`.
  - `"workers format with thousands separator"`: `.risk_fmt(12345.4, "n") == "12,345"`.
- [ ] **Step 2: Run.** Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Live check** in the app (port 8123, Playwright):
  - The Assistance lens at the US view shows county scores after the deferred step.
  - Jump to Chattanooga metro: tracts shade for the score with labor-market + state ranks, and tooltips have 4 drivers.
  - Click an Anchorage AK county: the side panel says the ACS source.
  - Record the cold drill time for a 2-state metro (it should be about 3–10 s plus about 2 s per state of LODES).
- [ ] **Step 6: Commit** `feat: Assistance lens (county + tract) with drivers, MOE flags and source notes`

---

### Task 5: Seed, manifest, end-to-end

**Files:**
- Modify: `R/data_risk.R` (`risk_seed_update` allow-list adds `lodes_year_v1`), `tests/testthat/test-seed.R`, `data/risk_seed/`, `manifest.json`

- [ ] **Step 1: Failing test:** the seed update copies `lodes_year_v1.rds` and never copies `lodes_rac_*`, `tract_ind_*` or `tracts_v4_*`.
- [ ] **Step 2: Run.** **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Refresh the seed** (`risk_seed_update()` after `lodes_latest_year()`). Regenerate `manifest.json` with the same explicit-file approach as Phase 2 Task 8 (keep all existing entries, add the new seed file). The package count is unchanged.
- [ ] **Step 6: End-to-end check:**
  - Assistance, Migration and Context lenses at the US view;
  - drill into Chattanooga (2 states), a CT county and an AK county;
  - Back to US.
  - No errors in the R console.
- [ ] **Step 7: Commit** `chore: seed LODES year and regenerate manifest`
