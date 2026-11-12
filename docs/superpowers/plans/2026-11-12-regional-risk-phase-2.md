# Regional Risk Map — Phase 2 Implementation Plan (Migration lens, Stress ahead, metro Stress now)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the Migration lens (Census PEP), the Stress ahead layer (FRED sector payrolls × ACS county industry mix, shift-share) with sector risk weights, and metro-level Stress now (BLS metro LAUS), each with as-of dates and starter-copy seeding.

**Architecture:**
- New fetchers go in `R/data_risk.R` behind the existing `risk_cached()` (memory → disk → seed → fetch, `bg` refresh for stale seeds).
- New pure metrics go in `R/risk_metrics.R`; new constants go in `RISK_CFG`; the sector table goes in `RISK_SECTORS`.
- New layers are entries in `RISK_LAYERS` (`R/map_risk.R`), each with a `lens` field. `build_county_layers()` joins each new component onto the county base.
- The server keeps the progressive load: the county base draws first, then one deferred step fetches LAUS county + LAUS metro + FRED + ACS industry mix and redraws.

**Tech Stack:** R 4.6, Shiny + shinydashboard, leaflet 2.2.3, sf, tidycensus 1.8.1, tigris, httr2 1.3.0, fredr 2.1.0, readr, htmltools, testthat (tests only).

**Spec:** `docs/superpowers/specs/2026-11-12-regional-risk-map-design.md`. Read §8, §9.1–§9.3, §9.7, §10 and §12 (Phase 2, including the "Metro-level Stress now" bullet).

**Prior plan (built):** `docs/superpowers/plans/2026-11-12-regional-risk-map-phase-0-1.md`.

**Run tests:** `Rscript -e "testthat::test_dir('tests/testthat')"` (230 passing at the start of this plan).

## Global Constraints

- **leaflet:** every call uses the `leaflet::` prefix.
- **No new hard dependencies.** `fredr`, `readr`, `httr2` and `tidycensus` are already installed and in `manifest.json`.
- **Keys:** `Sys.getenv("FRED_API_KEY")`, `"BLS_API_KEY"`, `"CENSUS_API_KEY"`. fredr reads `FRED_API_KEY` from the environment by itself. For live shell runs: `export R_ENVIRON_USER="C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron"`.
- **Everything in-app:** no build jobs and no GitHub Actions. Every new source is cached via `risk_cached()` with a `bg = list(id, fn, args)` so a stale seed refreshes in the background, and it is added to the seed allow-list in `risk_seed_update()`.
- **FRED sector series are fetched in the risk module.** Do not add them to `FRED_CFG`.
- **User-Agent:** `"TerraMetricAtlas/1.0 (research)"` on every request to `download.bls.gov` and `www2.census.gov`. BLS returns 403 to httr2's default UA. Never put an email in it.
- **TTLs (days):**

  | Source | TTL |
  |---|---|
  | PEP | 30 |
  | FRED | 1 |
  | ACS county industry mix | 30 |
  | LAUS metro | 7 |
  | LAUS area list | 90 |
- **Connecticut:** planning regions 09110–09190 must join across shapes, PEP and ACS C24030.
- **Migration palette:** diverging, centred on 0, **not** red/green (use `"PuOr"`). The rank pill is never red on migration layers.
- **BLS quota:** metro LAUS adds 8 calls per refresh. Each call covers 50 series and 3 years. The metro pull uses the same >50%-failed → no-retry, 1 h failure memory rule as the county pull.

## Verified source facts (spike 2026-11-12)

- **PEP:**
  - URL: `https://www2.census.gov/programs-surveys/popest/datasets/2020-{V}/counties/totals/co-est{V}-alldata.csv`.
  - The file is **latin1** ("Doña Ana"). `SUMLEV` `"050"` marks counties. GEOID = `STATE` (2 chars) + `COUNTY` (3 chars).
  - Columns used for vintage `V`: `ESTIMATESBASE2020`, `POPESTIMATE{y}`, `DOMESTICMIG{y}`, `INTERNATIONALMIG{y}`, `NATURALCHG{y}`, and the per-1,000 rates `RDOMESTICMIG{y}`, `RINTERNATIONALMIG{y}`, `RNETMIG{y}`, `RNATURALCHG{y}`. The annual columns run y = 2020..V; the rates exist for y = 2021..V only.
  - Column y = July (y−1) → June y, so 2025 = "2024–25".
  - **A missing vintage returns HTTP 200 `text/html`** (a firewall page), not 404. A vintage exists only when `content-type` starts with `text/csv` *and* the first line contains `SUMLEV`.
- **FRED:** all of these are monthly, SA, in thousands, with the latest observation 2026-10-01: `USMINE`, `USCONS`, `MANEMP`, `USWTRADE`, `USTRADE` (retail trade), `CES4300000001`, `USINFO`, `USFIRE`, `USPBS`, `USEHS`, `USLAH`, `USSERV`, `USGOVT`, `TEMPHELPS`. `USREC` is a 0/1 NBER flag. All of them go back before 2000.
- **BLS LAUS metro:**
  - Area list: `https://download.bls.gov/pub/time.series/la/la.area` (tab-delimited, CRLF, 440 KB). Columns: `area_type_code, area_code, area_text, display_level, selectable, sort_sequence`.
  - `area_type_code` `B` = metro (393 rows); `D` = micro.
  - CBSA = `substr(area_code, 5, 9)`. Series ID = `paste0("LAU", area_code, "03")`, e.g. `LAUMT471686000000003` for Chattanooga.
  - The state digits in `area_code` cannot be derived from the name (Davenport IA-IL uses 17), so always use the area list.
  - In the series ID, the CBSA sits at `substr(series_id, 8, 12)`. County series keep FIPS at `substr(id, 6, 10)`.

## Review Focus

1. **A PEP vintage that isn't published yet** returns a 200 HTML page. The probe must skip it and use the newest real CSV. If no vintage is found, the Migration layers are "unavailable" and nothing errors. Test: `test-pep.R::"HTML 200 is not a vintage"` (Task 2).
2. **FRED or the ACS industry pull fails on a cold start with no cache.** Stress ahead shows "unavailable" in the legend and "Insufficient data" in tooltips. Every other layer still works. Test: `test-stress-ahead.R::"missing Stress ahead marks layer unavailable, others ok"` (Task 6).
3. **A county with zero employed residents or missing C24030** (e.g. Kalawao HI `15005`). Shares and Stress ahead are NA, never `NaN`/`Inf`. Test: `test-stress-ahead.R::"zero employment gives NA projection"` (Task 6).
4. **A county whose CBSA has no metro LAUS row** (a micro area, or a metro series that came back empty). No metro line appears: no "NA pp", no empty line. Test: `test-metro-laus.R::"no metro line without metro data"` (Task 7).
5. **Switching lens while drilled into a metro.** The layer choices change to the new lens, the selected layer is valid for that lens, and county-only layers (Migration, Stress ahead) shade the counties with no tracts drawn. Test: `test-lens.R::"every lens default is one of its own layers and county-only layers draw no tracts"` (Task 1).

---

### Task 1: Lens registry, lens-driven layer list, pp-layer conventions

**Files:**
- Modify: `R/map_risk.R` (`RISK_LAYERS`, `.risk_palette`, `.rank_bad`, `.county_tooltips`, `.add_county_polygons` legend, `risk_tooltips_vec`, `risk_side_panel`)
- Modify: `ui.R` (`tabItem("metro")` lens/layer selects), `server.R` (REGIONAL RISK MAP section)
- Test: create `tests/testthat/test-lens.R`; update `tests/testthat/test-side-panel.R` and `test-county-map.R` where the wording changes

**Interfaces:**
- Produces:
  - **Every `RISK_LAYERS` entry** gains `lens` (one of `"context"`, `"migration"`; later phases add `"assistance"` and `"credit"`). It may also gain `bins` (numeric breaks) and `bin_labels` (character, `length(bins) - 1`).
    - `stress_now` gets `lens = "context"`, `bins = c(-50, -1, -0.5, -0.2, 0.2, 0.5, 1, 50)`, and `bin_labels` equal to today's labels, with the last two reading `"+0.5 to +1.0 (rising)"` and `"above +1.0 pp (Sahm-style trigger)"`.
    - All the ACS entries get `lens = "context"`.
  - **`higher_is_worse`** may be `NA` (neutral: the pill is never red).
  - **`RISK_LENSES`** is a named character vector in display order (spec §10 order, restricted to lenses that have layers): `c("Migration" = "migration", "Context" = "context")` after Task 3. In this task it is just `c("Context" = "context")`.
  - **`risk_lens_choices(lens) -> named character`:** values are layer ids in registry order, names are labels.
  - **`risk_lens_default(lens) -> character(1)`:** the first id of `risk_lens_choices(lens)`.
  - **`risk_tooltips_vec(..., standing = "")`:** a new argument holding a character vector of pre-escaped HTML lines, shown between the drivers block and the footer (`""` = none). Later tasks put the migration and metro lines here.
  - **`.risk_palette(cfg, vals)`:** uses `cfg$bins` whenever present (any fmt). Otherwise it keeps today's quantile bins.
  - **`.add_county_polygons`:** when `cfg$bins` is set, the legend uses `colors = pal(bin midpoints)` and `labels = cfg$bin_labels %||% .bin_labels(cfg$bins, cfg$fmt)`.
  - **`.bin_labels(bins, fmt) -> character`:** e.g. `c("below −10", "−10 to −5", …, "above +10")`. It uses the `.risk_fmt` number style without units for the inner bins.
  - **`.rank_bad(pct, cfg, vals)`:**
    - `NA` `higher_is_worse` → all FALSE;
    - fmt `"pp"` → `vals > 0` when higher is worse, `vals < 0` otherwise (sign, not median);
    - else today's median rule.
  - **Display text for a Stress now flag:** `"Trigger"` → `"Sahm-style trigger"` (tooltip and side panel). The `flag` column values stay `"Trigger"` / `"Rising"` / `"None"`.

- [ ] **Step 1: Write failing tests** in `tests/testthat/test-lens.R`:
  - `"every layer has a lens and every lens has layers"`:
    - `all(vapply(RISK_LAYERS, function(l) l$lens %in% RISK_LENSES, TRUE))`;
    - `all(vapply(RISK_LENSES, function(z) length(risk_lens_choices(z)) > 0, TRUE))`.
  - `"every lens default is one of its own layers and county-only layers draw no tracts"`:
    - for each lens, `risk_lens_default(z) %in% risk_lens_choices(z)`;
    - then `add_tract_layer(leaflet::leaflet(), <1-tract sf fixture>, "stress_now", build_county_layers(fx_county_base(), fx_momentum()))` has no `addPolygons` call with `group == "tracts"`.
  - `"pp pill is keyed to sign"`:
    - `.rank_bad(c(90, 10), RISK_LAYERS$stress_now, c(-0.1, 0.2))` is `c(FALSE, TRUE)`;
    - a cfg with `higher_is_worse = NA` gives all FALSE.
  - `"binned legend labels"`: `.bin_labels(c(-1000, -10, -5, 5, 10, 1000), "per1k")` equals `c("below −10", "−10 to −5", "−5 to +5", "+5 to +10", "above +10")`.
  - `"Sahm-style wording"`: the Stress now legend title or labels of `build_risk_county_map(l, "stress_now")` contain `"Sahm-style"`, and a Trigger county tooltip contains `"Sahm-style trigger"`.
  - `"standing lines render above footer"`: `risk_tooltips_vec("X", value_text = "1", footer = "F", standing = "<div>S1</div>")` contains `S1` before `F`.
- [ ] **Step 2: Run** `Rscript -e "testthat::test_file('tests/testthat/test-lens.R')"`. Expected: FAIL (`RISK_LENSES` not found).
- [ ] **Step 3: Implement** the Interfaces above.
  - **UI:** `selectInput("risk_lens", choices = RISK_LENSES, selected = "context")`. The `risk_layer` choices come from `risk_lens_choices("context")`.
  - **Server:** `observeEvent(input$risk_lens, ignoreInit = TRUE)` → `updateSelectInput(session, "risk_layer", choices = risk_lens_choices(input$risk_lens), selected = risk_lens_default(input$risk_lens))`. The existing `input$risk_layer` observer redraws.
  - **Existing tests:** update the side-panel test's `"Trigger"` expectation to `"Sahm-style trigger"`.
- [ ] **Step 4: Run the full suite.** Expected: all PASS.
- [ ] **Step 5: Commit** `feat: lens registry drives the layer list; sign-keyed pp pill, Sahm-style wording`

---

### Task 2: PEP fetch and migration metrics

**Files:**
- Modify: `R/data_risk.R` (new "PEP" section), `R/risk_metrics.R` (new "Migration" section)
- Create: `tests/testthat/fixtures/pep_sample.csv`, `tests/testthat/test-pep.R`

**Interfaces:**
- Produces:
  - **`.pep_url(vintage) -> character`** (pattern in Verified facts).
  - **`.pep_probe(vintage) -> logical`:** a GET with the UA and `httr2::req_error(is_error = \(r) FALSE)`. TRUE only if status is 200, `resp_content_type` starts with `"text/csv"`, and the first line contains `"SUMLEV"`. Read only the first chunk if practical; otherwise the whole 2 MB file is acceptable.
  - **`risk_pep_vintage(probe = .pep_probe, today = Sys.Date(), cache_dir = risk_cache_dir()) -> integer or NULL`:**
    - cached under key `"pep_vintage_v1"` for 30 days, with `bg = list(id = "pep_vintage", fn = "risk_pep_vintage", args = list())`;
    - tries `year(today)` down to `year(today) - 3`;
    - a failed probe is not cached (same rule as `risk_acs_year`).
  - **`parse_pep_county(raw, vintage) -> tibble`:**
    - `raw` is the CSV read with every column as character (`readr::read_csv(..., col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = "latin1"))`);
    - keeps `SUMLEV == "050"` and state FIPS ≤ 56;
    - returns one row per county with these columns:
      - `GEOID`;
      - `mig_dom_rate` = `RDOMESTICMIG{V}`;
      - `mig_intl_rate` = `RINTERNATIONALMIG{V}`;
      - `mig_net_rate` = `RNETMIG{V}`;
      - `nat_chg_rate` = `RNATURALCHG{V}`;
      - `mig_dom_cum` = `1000 * sum(DOMESTICMIG2021..V) / ESTIMATESBASE2020`;
      - `pop_chg_pct` = `100 * (POPESTIMATE{V} / ESTIMATESBASE2020 - 1)`;
      - `mig_dom_n` = `DOMESTICMIG{V}`;
      - `pop_avg` = `mean(POPESTIMATE{V-1}, POPESTIMATE{V})`;
      - `mig_dom_rate_{y}` for y = 2021..V.
    - All values are numeric and rounded to 2 dp. `attr(, "vintage") <- V`.
  - **`fetch_pep_county(vintage = NULL) -> tibble or NULL`:**
    - when `vintage` is NULL it uses `risk_pep_vintage()`, and returns NULL if that is NULL;
    - cached under key `sprintf("pep_county_%d", V)` for 30 days, with `bg = list(id = "pep", fn = "fetch_pep_county", args = list(vintage = V))`;
    - downloads the CSV with the UA into a tempfile, then runs `parse_pep_county`.
  - **`pep_year_label(y) -> character`:** `2025` → `"2024–25"` (en dash).
- Consumes: `risk_cached()`.

- [ ] **Step 1: Build the fixture.**
  - `pep_sample.csv` holds the header row of the real file restricted to the columns above plus `SUMLEV, STATE, COUNTY, STNAME, CTYNAME`, and 4 rows:
    - a state row (SUMLEV 040, Tennessee);
    - Hamilton TN 47065 with the real V2025 values (POPESTIMATE2025 390833, RDOMESTICMIG2025 5.50, RINTERNATIONALMIG2025 2.38, RNETMIG2025 7.88, RNATURALCHG2025 1.22);
    - Capitol CT 09110 (RDOMESTICMIG2025 −0.11);
    - Doña Ana NM 35013, with the `ñ` stored in latin1.
  - Other values may be invented, but keep `ESTIMATESBASE2020` and the DOMESTICMIG columns consistent so the cumulative value is checkable by hand.
- [ ] **Step 2: Write failing tests** in `test-pep.R`:
  - `"parses counties only, keeps CT planning regions"`: 3 rows; `"09110" %in% GEOID`; `"47" %in% substr(GEOID, 1, 2)`.
  - `"latest-year rates and cumulative since 2020"`:
    - Hamilton: `mig_dom_rate == 5.5`, `mig_net_rate == 7.88`;
    - `mig_dom_cum` equals your hand-computed value;
    - `mig_dom_rate_2021` through `mig_dom_rate_2025` are present.
  - `"HTML 200 is not a vintage"`: `risk_pep_vintage(probe = function(v) v == 2025, today = as.Date("2026-11-12"), cache_dir = tempfile())` returns `2025`. Also test `.pep_probe` itself through a mocked response: given `httr2::response(200, headers = list("content-type" = "text/html"), body = charToRaw("<html>"))`, the content check returns FALSE. To make this testable, factor the check into `.pep_resp_ok(resp)`.
  - `"no vintage found returns NULL"`: `probe = function(v) FALSE` gives NULL.
  - `"year label"`: `pep_year_label(2025) == "2024–25"`.
- [ ] **Step 3: Run** `Rscript -e "testthat::test_file('tests/testthat/test-pep.R')"`. Expected: FAIL.
- [ ] **Step 4: Implement** the Interfaces. Fix the encoding at read time (`latin1` locale) so county names never fail later.
- [ ] **Step 5: Run the full suite.** Expected: PASS. Then make a live check with keys set: `Rscript -e "source('global.R'); x <- fetch_pep_county(); print(dim(x)); print(attr(x,'vintage'))"`. Expected: 3144 rows, vintage 2025.
- [ ] **Step 6: Commit** `feat: Census PEP county migration fetch with vintage probe`

---

### Task 3: Migration lens layers, standing migration line, side-panel chart

**Files:**
- Modify: `R/map_risk.R` (`RISK_LAYERS`, `RISK_LENSES`, `.county_tooltips`, `risk_side_panel`), `R/risk_metrics.R` (`build_county_layers`)
- Test: create `tests/testthat/test-migration-layers.R`; add `fx_migration()` to `helper-fixtures.R`

**Interfaces:**
- Consumes: `parse_pep_county()` output (Task 2), `pep_year_label()`, `standing` (Task 1).
- Produces:
  - **`build_county_layers(county_base, momentum, laus_stale = FALSE, laus_loading = FALSE, laus_partial = FALSE, migration = NULL, loading = character(0))`:**
    - left-joins `migration` by GEOID;
    - if `migration` is NULL, adds every migration column as NA;
    - each new layer's status is:
      - `"loading"` if its component name is in `loading` (component name `"migration"`);
      - `"unavailable"` if its data is NULL;
      - `"stale"` if `attr(migration, "stale")`;
      - else `"ok"`.
  - **As-of labels:**
    - annual migration layers: `sprintf("V%d, %s", V, pep_year_label(V))`, e.g. `"V2025, 2024–25"`;
    - cumulative layer: `sprintf("V%d, 2020–%s", V, substr(V, 3, 4))`.
    - `attr(out, "pep_vintage") <- V`.
  - **Four `RISK_LAYERS` entries** with `lens = "migration"`, `levels = "county"`, `fmt = "per1k"`, `pal = "PuOr"`, `rev = FALSE`, `higher_is_worse = NA`, `source = "Census PEP"`, `rank_frame = "for in-migration"`:

    | id | col | label | bins |
    |---|---|---|---|
    | `mig_dom` | `mig_dom_rate` | "Net domestic migration (per 1,000)" | `c(-1000, -10, -5, -2, -0.5, 0.5, 2, 5, 10, 1000)` |
    | `mig_intl` | `mig_intl_rate` | "Net international migration (per 1,000)" | same |
    | `mig_net` | `mig_net_rate` | "Total net migration (per 1,000)" | same |
    | `mig_dom_cum` | `mig_dom_cum` | "Net domestic migration since 2020 (per 1,000 of 2020 pop.)" | `c(-1000, -50, -25, -10, -2, 2, 10, 25, 50, 1000)` |

    `mig_dom` is listed first, so it is the lens default. `RISK_LENSES` becomes `c("Migration" = "migration", "Context" = "context")`.
  - **`.risk_fmt(v, "per1k")`:** `sprintf("%+.1f per 1,000", v)`.
  - **`.legend_format`:** for `per1k`, the binned legend from Task 1.
  - **Migration-layer tooltips:**
    - the rank pill reads `rank_label(pct, cfg$rank_frame %||% "nationally")` and is always neutral;
    - "Driven by" has 3 lines — `"Domestic"` (`mig_dom_rate`), `"International"` (`mig_intl_rate`) and `"Natural change (births − deaths)"` (`nat_chg_rate`) — each formatted per1k and never red.
    - The vectorised drivers block is built by a new `risk_drivers_vec(labels, values, worse) -> character(n)`: `labels` is character(k), `values` is a list of k character vectors, and `worse` is a list of k logical vectors. `risk_tooltip_html` must produce identical HTML for one row (the existing equivalence test still passes).
  - **Standing migration line on every county tooltip, any lens:** `.migration_line(d, vintage) -> character(n)` gives `"Net domestic migration: +5.5 per 1,000 (2024–25)"`, or `""` when the value is NA or the vintage is NULL. It is passed via `standing`.
  - **Side panel (county level):**
    - under the title, the same standing line;
    - after the table, a block titled `"Net domestic migration per 1,000, 2020–21 to 2024–25"`, with one horizontal bar per `mig_dom_rate_{y}`: a plain `div` with width ∝ |value|, coloured `#b35806` when negative and `#542788` when positive (PuOr ends), with the value text;
    - then `"Population change 2020–{yy}: +x.x%"` and `"Census PEP V2025"`.

- [ ] **Step 1: Add `fx_migration()`** to `helper-fixtures.R`. It is a tibble for GEOIDs `"13295"` and `"09110"`:
  - `mig_dom_rate = c(8.2, -0.11)`;
  - `mig_intl_rate = c(1.0, 4.78)`;
  - `mig_net_rate = c(9.2, 4.67)`;
  - `nat_chg_rate = c(-1.5, 0.23)`;
  - `mig_dom_cum = c(40.1, -12.0)`;
  - `pop_chg_pct = c(4.0, 1.2)`;
  - `mig_dom_n = c(560, -110)`;
  - `pop_avg = c(69000, 990000)`;
  - `mig_dom_rate_2021`..`_2025` (any values);
  - `attr(, "vintage") = 2025`.
- [ ] **Step 2: Write failing tests** in `test-migration-layers.R`:
  - `"migration joins and has as-of"`:
    - with `l <- build_county_layers(fx_county_base(), fx_momentum(), migration = fx_migration())`, `l$mig_dom_rate[l$GEOID == "09110"] == -0.11`;
    - `attr(l, "asof")[["mig_dom"]] == "V2025, 2024–25"`;
    - `attr(l, "asof")[["mig_dom_cum"]] == "V2025, 2020–25"`.
  - `"missing PEP marks migration unavailable, loading when asked"`: `migration = NULL` gives status `"unavailable"`; with `loading = "migration"` it is `"loading"`.
  - `"standing line on every lens"`: the label HTML of `build_risk_county_map(l, "poverty_rate")` contains `"Net domestic migration: +8.2 per 1,000 (2024–25)"`. Labels are found at `Filter(function(cl) cl$method == "addPolygons", m$x$calls)[[1]]$args[[...]]`; reuse the lookup the existing `test-county-map.R` uses.
  - `"no standing line when PEP missing"`: with `migration = NULL`, no label contains `"Net domestic migration:"`.
  - `"migration pill never red and drivers listed"`:
    - the `mig_dom` labels contain `"for in-migration"`, `"Natural change (births − deaths)"` and `"International"`;
    - they do not contain the red pill style `#5c1f2a`.
  - `"diverging palette centred on zero"`: `.risk_palette(RISK_LAYERS$mig_dom, c(-3, 3))(c(-0.1, 0.1))` gives the same colour (both fall in the −0.5..0.5 bin), and that colour differs from `pal(-6)` and `pal(6)`.
  - `"side panel shows migration chart and population change"`: the county panel HTML contains `"2020–21 to 2024–25"`, `"Population change 2020–25: +4.0%"` and `"Census PEP V2025"`.
- [ ] **Step 3: Run** the file. Expected: FAIL.
- [ ] **Step 4: Implement.** Check that `test-side-panel.R` still passes, and update its row-count expectation only if your rows change by design. All layers are county-level, so the count stays `length(RISK_LAYERS) + 1`.
- [ ] **Step 5: Run the full suite.** Expected: PASS.
- [ ] **Step 6: Commit** `feat: Migration lens (4 PEP layers), standing migration line, side-panel chart`

---

### Task 4: Migration wiring — tab entry, metro table column

**Files:**
- Modify: `server.R` (REGIONAL RISK MAP section), `R/risk_metrics.R` (new `cbsa_migration`), `ui.R` (info text)
- Test: add to `tests/testthat/test-migration-layers.R`

**Interfaces:**
- Consumes: `fetch_pep_county()`, `build_county_layers(..., migration =)`.
- Produces:
  - **`cbsa_migration(county_layers) -> tibble(cbsa_geoid, mig_dom_rate)`:** `round(1000 * sum(mig_dom_n) / sum(pop_avg), 1)` over each CBSA's counties, with `na.rm = TRUE` on both sums. A CBSA whose counties all lack data gets NA.
  - **Server state:**
    - `rv$risk_parts` is a list holding everything `build_county_layers` takes besides the base: `momentum`, `laus_stale`, `laus_partial`, `migration`, `loading`;
    - **`risk_rebuild()`** is one local helper that sets `rv$county_layers <- do.call(build_county_layers, c(list(rv$risk_base), rv$risk_parts))` and then redraws via the same tract-or-county branch the LAUS observer uses today;
    - later tasks add their components to `rv$risk_parts` and call `risk_rebuild()`.

- [ ] **Step 1: Write the failing test** `"CBSA migration is population-weighted"`. Give two counties in CBSA `"X"` `mig_dom_n = c(100, -50)` and `pop_avg = c(10000, 40000)`; the result is `1.0`. A CBSA with all-NA inputs gives NA.
- [ ] **Step 2: Run.** Expected: FAIL.
- [ ] **Step 3: Implement.**
  - **Tab entry:** after `fetch_county_base()`, call `fetch_pep_county()` in a `tryCatch` (it is seeded, so it is fast) and store it in `rv$risk_parts$migration`. Its NULL result is fine.
  - **Refactor** the existing LAUS observer to fill `rv$risk_parts` and call `risk_rebuild()`.
  - **`tbl_metro_cbsa`:** left-join `cbsa_migration(rv$county_layers)` onto `rv$metro_data` by `GEOID == cbsa_geoid`, and add the column `` `Net Dom. Migration /1k` `` after `Population`, formatted `sprintf("%+.1f", x)`. Extend the `formatStyle(columns = …)` range to the new column count.
  - **UI info text:** add `" Migration: Census PEP county components of change (per 1,000 residents)."`.
- [ ] **Step 4: Run the full suite.** Expected: PASS. Then start the app with keys set:
  ```
  Rscript -e "shiny::runApp('.', port = 8123, launch.browser = FALSE)"
  ```
  Open the Regional Risk tab (Playwright MCP or a browser) and check:
  - the Lens dropdown shows Migration and Context;
  - choosing Migration shades counties purple/orange;
  - county tooltips show the standing line on the Context lens too;
  - the metro table has the new column.
- [ ] **Step 5: Commit** `feat: wire Migration lens into the tab load and metro table`

---

### Task 5: FRED sector payrolls, sector momentum and risk weights

**Files:**
- Modify: `R/risk_metrics.R` (`RISK_CFG`, new `RISK_SECTORS`, new "Sectors" section), `R/data_risk.R` (new "FRED sectors" section)
- Create: `tests/testthat/test-sectors.R`

**Interfaces:**
- Produces:
  - **`RISK_SECTORS`:** a data.frame with columns `group, label, fred, acs_m, acs_f, cns`, one row per spec §9.2 group in this order. `cns` is a comma-separated string used by Phase 3.

    | group | label | fred | acs_m | acs_f | cns |
    |---|---|---|---|---|---|
    | `ag_mining` | Agriculture & mining | USMINE | C24030_003 | C24030_030 | 01,02 |
    | `construction` | Construction | USCONS | C24030_006 | C24030_033 | 04 |
    | `manufacturing` | Manufacturing | MANEMP | C24030_007 | C24030_034 | 05 |
    | `wholesale` | Wholesale trade | USWTRADE | C24030_008 | C24030_035 | 06 |
    | `retail` | Retail trade | USTRADE | C24030_009 | C24030_036 | 07 |
    | `transport_util` | Transportation & utilities | CES4300000001 | C24030_010 | C24030_037 | 03,08 |
    | `information` | Information | USINFO | C24030_013 | C24030_040 | 09 |
    | `finance_re` | Finance & real estate | USFIRE | C24030_014 | C24030_041 | 10,11 |
    | `prof_admin` | Professional & admin services | USPBS | C24030_017 | C24030_044 | 12,13,14 |
    | `educ_health` | Education & health | USEHS | C24030_021 | C24030_048 | 15,16 |
    | `arts_food` | Leisure & hospitality | USLAH | C24030_024 | C24030_051 | 17,18 |
    | `other_services` | Other services | USSERV | C24030_027 | C24030_054 | 19 |
    | `public_admin` | Public administration | USGOVT | C24030_028 | C24030_055 | 20 |
  - **`RISK_CFG` additions:** `sector_momentum_months = 6`, `recession_since = as.Date("2000-01-01")`, `peak_lead_months = 6`, `trough_lag_months = 24`, `w_structural = 0.5`, `w_current = 0.5`.
  - **`fetch_fred_sectors(start = as.Date("2000-01-01")) -> tibble(series_id, date, value) or NULL`:**
    - one `fredr::fredr(series_id = s, observation_start = start)` per series in `c(RISK_SECTORS$fred, "TEMPHELPS", "USREC")`;
    - cached under key `"fred_sectors_v1"` for 1 day, with `bg = list(id = "fred_sectors", fn = "fetch_fred_sectors", args = list())` and `fail_ttl_days = 1/24`;
    - returns NULL if `FRED_API_KEY` is empty or if any of the 13 sector series or USREC fails (`TEMPHELPS` is optional).
  - **`sector_momentum(fred, months = RISK_CFG$sector_momentum_months) -> tibble(series_id, m_pct, ref_month)`:** at each series' latest date `t`, `m_pct = 100 * ((v[t] / v[t - months])^(12 / months) - 1)`, where `t - months` means the observation dated `months` calendar months earlier; NA if it is missing.
  - **`recession_episodes(fred, since = RISK_CFG$recession_since) -> data.frame(start, end)`:** runs of consecutive `USREC == 1` months starting on/after `since` (2001-04..2001-11, 2008-01..2009-06, 2020-03..2020-04 in real data).
  - **`sector_weights(fred, cfg = RISK_CFG) -> tibble(group, label, fred, m_pct, structural, current_z, w)`:**
    - **structural** = the mean over episodes of `100 * (1 - trough / peak)`, where:
      - `peak` = the max value in `[start − peak_lead_months, end]`;
      - `trough` = the min value in `[date of that peak, end + trough_lag_months]`.
    - **current_z** = `(−m_now − mean(−m_hist)) / sd(−m_hist)`. `m_hist` is the series of 6-month annualized changes at every month since `recession_since`.
    - **w** = `cfg$w_structural * rescale01(structural) + cfg$w_current * rescale01(current_z)`, where `rescale01(x) = (x - min) / (max - min)` across the 13 sectors (all 0.5 if the range is 0).
    - Attributes: `attr(, "ref_month")` and `attr(, "temphelp_m_pct")` (NA if TEMPHELPS is absent).

- [ ] **Step 1: Write failing tests** in `test-sectors.R`. Build a synthetic `fred` tibble with monthly dates 2000-01..2026-10:
  - `USREC` = 1 in 2008-01..2009-06, else 0;
  - two sector series: `"MANEMP"` falls 10% during the episode and recovers, then falls 3% over the last 6 months; `"USEHS"` grows 2%/yr steadily;
  - fill the other 11 sector IDs with a flat 100.

  Tests:
  - `"RISK_SECTORS covers 13 groups and all C24030 detail lines"`: 13 rows; the `acs_m`/`acs_f` sets are disjoint; `RISK_SECTORS$fred` contains `"USMINE"` and `"USEHS"`.
  - `"momentum is annualized 6-month change"`: for a series going 100 → 103 over 6 months, `m_pct` is about `100 * (1.03^2 - 1)` (6.09) within 0.01.
  - `"recession episodes found from USREC"`: one row, start `2008-01-01`, end `2009-06-01`.
  - `"structural decline is peak to trough"`: MANEMP `structural` ≈ 10 (±0.5); USEHS `structural` ≈ 0.
  - `"weights in 0..1, falling cyclical sector weighs most"`: `all(w >= 0 & w <= 1)`; `w[group == "manufacturing"] == max(w)`; `w[group == "educ_health"] < w[group == "manufacturing"]`.
  - `"missing TEMPHELPS is tolerated"`: `attr(sector_weights(fred_without_temphelps), "temphelp_m_pct")` is NA, with no error.
- [ ] **Step 2: Run** the file. Expected: FAIL.
- [ ] **Step 3: Implement.** Pure functions take the tibble; only `fetch_fred_sectors` does I/O.
- [ ] **Step 4: Run the full suite.** Expected: PASS. Live check with keys set: `Rscript -e "source('global.R'); f <- fetch_fred_sectors(); print(sector_weights(f))"`. Expected: 13 rows, finite `w`, and a plausible ordering (construction and manufacturing high; education & health low).
- [ ] **Step 5: Commit** `feat: FRED sector payroll momentum and recession-based sector risk weights`

---

### Task 6: County industry mix and the Stress ahead layer

**Files:**
- Modify: `R/data_risk.R` (`fetch_county_industry`), `R/risk_metrics.R` (`stress_ahead`, `build_county_layers`), `R/map_risk.R` (`RISK_LAYERS`, tooltips, side panel), `server.R` (deferred load step), `ui.R` (info text)
- Create: `tests/testthat/test-stress-ahead.R`; add `fx_industry()` and `fx_weights()` to `helper-fixtures.R`

**Interfaces:**
- Consumes: `RISK_SECTORS`, `sector_weights()` (Task 5), `risk_rebuild()` / `rv$risk_parts` (Task 4).
- Produces:
  - **`fetch_county_industry(year = NULL) -> tibble(GEOID, emp_total, emp_<group> ×13) or NULL`:**
    - variables `C24030_001` plus every `acs_m`/`acs_f`; `emp_<group>` = the sum of the male and female estimates;
    - cached under key `sprintf("county_ind_%d", year)` for 30 days, with `bg = list(id = "county_ind", fn = "fetch_county_industry", args = list(year = year))`;
    - one `tidycensus::get_acs(geography = "county", …, output = "wide")` call.
  - **`stress_ahead(ind, weights) -> tibble`:**
    - `share_<group>` = `emp_<group> / emp_total` (NA when `emp_total` is 0 or NA);
    - `proj_pct` = `Σ share × m_pct`;
    - `us_proj_pct` = the same with national shares (column sums of `emp_*`), identical on every row;
    - `stress_ahead_pp` = `round(proj_pct − us_proj_pct, 2)`;
    - top-3 drag drivers: the sectors with the most negative `(share − us_share) × m_pct`, stored as `sa_drv{1,2,3}_label` (from `RISK_SECTORS$label`) and `sa_drv{1,2,3}_pp` (rounded to 2 dp);
    - keeps `share_<group>` for the side panel;
    - `attr(, "ref_month")`, `attr(, "temphelp_m_pct")` and `attr(, "acs_year")` are carried over.
  - **`build_county_layers(..., stress_ahead = NULL)`:**
    - left-joins by GEOID; component name `"stress_ahead"` follows the same `loading` / `unavailable` / `stale` status rules as `migration`;
    - as-of = `sprintf("CES %s · ACS %d–%d", format(ref_month, "%b %Y"), acs_year - 4, acs_year)`;
    - the stale state comes from `attr(stress_ahead, "stale")`, which the caller sets when either FRED or ACS was stale.
  - **`RISK_LAYERS$stress_ahead`:** placed right after `stress_now`, with:
    - `label = "Stress ahead: projected job growth vs. US (shift-share)"`;
    - `col = "stress_ahead_pp"`, `fmt = "pp"`, `pal = "RdBu"`, `rev = FALSE`;
    - `higher_is_worse = FALSE`, `lens = "context"`, `levels = "county"`;
    - `source = "BLS CES (FRED) × ACS industry mix"`;
    - `bins = c(-50, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 50)`.
  - **Stress ahead tooltip:** "Driven by" shows the 3 drag sectors, `"<label>"` and `sprintf("%+.2f pp", sa_drv_pp)`, red when < 0.
  - **Side panel (county), below the table:**
    - "Top resident industries": the 5 largest `share_<group>` values, each with its share % and the national `m_pct` of that sector (`"Manufacturing 18.2% · US payrolls −2.1%/yr"`);
    - then the national temp-help note: `sprintf("Temporary help payrolls (US): %+.1f%% annualized over 6 months — a leading indicator, not in the score.", temphelp_m_pct)`, omitted when NA.
    - **The side panel needs the weights:** `build_county_layers` stores the `sector_weights` tibble as `attr(out, "sector_weights")` (passed through from `attr(stress_ahead, "weights")`).

- [ ] **Step 1: Add fixtures.**
  - `fx_industry()`: GEOIDs `"13295"`, `"09110"` and `"15005"`. `"15005"` has `emp_total = 0` and every `emp_* = 0`. The other two have invented counts, with Walker heavy in manufacturing.
  - `fx_weights()`: a 13-row tibble shaped like `sector_weights()` output, with `m_pct` = −3 for manufacturing, +2 for educ_health, 0 elsewhere, plus `attr(, "ref_month") = as.Date("2026-10-01")` and `attr(, "temphelp_m_pct") = -4.2`.
- [ ] **Step 2: Write failing tests** in `test-stress-ahead.R`:
  - `"shift-share gap is relative to US"`:
    - `s <- stress_ahead(fx_industry(), fx_weights())`;
    - Walker's `stress_ahead_pp < 0`;
    - the population-weighted mean of `proj_pct` over the fixture equals `us_proj_pct` (within 1e-6), using `emp_total` as weights and excluding NA.
  - `"zero employment gives NA projection"`: the `15005` values `stress_ahead_pp`, `share_manufacturing` and `sa_drv1_label` are all NA, and `!any(is.nan(s$stress_ahead_pp))`.
  - `"drag drivers are most negative contributions"`: Walker's `sa_drv1_label == "Manufacturing"` and `sa_drv1_pp < 0`.
  - `"missing Stress ahead marks layer unavailable, others ok"`: `build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = NULL)` gives status `stress_ahead == "unavailable"`, `stress_now == "ok"` and `poverty_rate == "ok"`; the `stress_ahead` tooltips show `"Insufficient data"`.
  - `"Stress ahead as-of and drivers in tooltip"`:
    - as-of is `"CES Oct 2026 · ACS 2020–2024"` (the ACS year comes from `fx_county_base`);
    - the label HTML for Walker contains `"Manufacturing"` and the red colour `#ff6b6b`.
  - `"side panel lists top industries and temp-help note"`: it contains `"Top resident industries"`, `"Temporary help payrolls (US): -4.2%"` and `"Manufacturing"`.
- [ ] **Step 3: Run** the file. Expected: FAIL.
- [ ] **Step 4: Implement.**
  - **Server, in the deferred step** (the `rv$laus_go` observer), after county LAUS:
    - `fred <- fetch_fred_sectors()`, `ind <- fetch_county_industry(attr(rv$risk_base, "acs_year"))`;
    - if both are non-NULL: `w <- sector_weights(fred)`, `sa <- stress_ahead(ind, w)`, `attr(sa, "weights") <- w`, and `attr(sa, "stale") <- isTRUE(attr(fred, "stale")) || isTRUE(attr(ind, "stale"))`;
    - store it in `rv$risk_parts$stress_ahead`, each fetch wrapped in `tryCatch` → NULL;
    - at tab entry set `rv$risk_parts$loading <- c("stress_ahead")` (plus migration if you choose to defer it), and clear it after this step.
  - **UI info text:** `" Stress ahead: national sector payroll momentum (6-mo.) applied to each county's resident industry mix; gap vs. the US."`.
- [ ] **Step 5: Run the full suite.** Expected: PASS. Then in the live app check that Stress ahead shades counties, tooltips list drag sectors, and the side panel shows industries and the temp-help note.
- [ ] **Step 6: Commit** `feat: Stress ahead layer (shift-share of sector payroll momentum on county industry mix)`

---

### Task 7: Metro LAUS and metro-level Stress now

**Files:**
- Modify: `R/data_risk.R` (`parse_bls_laus`, new `parse_laus_areas`, `fetch_laus_areas`, `fetch_laus_metro`; factor out the chunked pull), `R/risk_metrics.R` (`build_county_layers`), `R/map_risk.R` (county + tract tooltips, side panel header), `server.R` (deferred step)
- Create: `tests/testthat/test-metro-laus.R`, `tests/testthat/fixtures/la_area_sample.txt`

**Interfaces:**
- Produces:
  - **`parse_bls_laus(results, id_start = 6L)`:** the area id is `substr(seriesID, id_start, id_start + 4)` and is still named `fips` in the output. Existing calls keep the default 6. Metro calls pass 8.
  - **`.laus_pull(ids, years, key, max_active, perform, perform_one, id_start)`:** the chunk → perform → retry/no-retry → partial logic now in `fetch_laus_county`'s `fetch_fn`, moved verbatim and reused by both fetchers. Existing `test-laus-fetch.R` tests must pass unchanged.
  - **`parse_laus_areas(lines) -> tibble(cbsa_geoid, cbsa_title, series_id)`:** input is a character vector of lines (header first, tab-delimited). It keeps `area_type_code == "B"` (metro) only, with `series_id = paste0("LAU", area_code, "03")` and `cbsa_geoid = substr(area_code, 5, 9)`.
  - **`fetch_laus_areas() -> tibble or NULL`:** a GET of la.area with the UA, cached under key `"laus_areas_v1"` for 90 days with `bg = list(id = "laus_areas", fn = "fetch_laus_areas", args = list())`.
  - **`fetch_laus_metro(series_ids, years = 3, key = Sys.getenv("BLS_API_KEY"), max_active = 8, cache_dir = risk_cache_dir(), perform = .default_perform, perform_one = httr2::req_perform)`:**
    - cached under key `sprintf("laus_metro_n%d", length(series_ids))` for 7 days, `fail_ttl_days = 1/24`, `bg = list(id = "laus_metro", fn = "fetch_laus_metro", args = list(series_ids = series_ids))`;
    - returns the same shape as the county pull (`fips` = CBSA code).
  - **`build_county_layers(..., metro_momentum = NULL)`:** `metro_momentum` is `laus_momentum()` output on the metro pull. It joins `metro_momentum_pp`, `metro_rate_now` and `metro_ref_month` onto counties by `cbsa_geoid == fips`. It is not a layer and gets no status entry.
  - **`.metro_line(cbsa_name, momentum_pp, ref_month) -> character(n)`:**
    - text: `sprintf("%s metro: %+.1f pp unemployment vs. a year ago (%s)", cbsa_name, momentum_pp, format(ref_month, "%b %Y"))`, HTML-escaped and wrapped like the migration line;
    - `""` when any input is NA.
  - **Display:**
    - county tooltips put `.metro_line` in `standing` (after the migration line) on every lens;
    - tract tooltips get the metro line of their county: `add_tract_layer` looks up `cbsa_name` / `metro_momentum_pp` / `metro_ref_month` from `county_layers` by `county_geoid`;
    - the side panel header (county and tract) shows the line under the subtitle. The same line therefore appears whether you jump to a metro (largest county selected) or click any county in it.

- [ ] **Step 1: Build the fixture.** `la_area_sample.txt` is the real header line plus 5 real rows:
  - `B	MT4716860000000	Chattanooga, TN-GA Metropolitan Statistical Area	…`;
  - `B	MT1719340000000	Davenport-Moline-Rock Island, IA-IL …`;
  - one `D` micro row;
  - one `C` metro-division row;
  - one `A` state row.

  Copy the exact format from `https://download.bls.gov/pub/time.series/la/la.area`, fetched with the UA.
- [ ] **Step 2: Write failing tests** in `test-metro-laus.R`:
  - `"area list keeps metros, builds series ids"`: 2 rows; Chattanooga `series_id == "LAUMT471686000000003"` and `cbsa_geoid == "16860"`; Davenport `cbsa_geoid == "19340"`.
  - `"metro series parse CBSA at position 8"`: `parse_bls_laus(list(list(seriesID = "LAUMT471686000000003", data = list(list(year = "2026", period = "M09", value = "3.9")))), id_start = 8L)$fips == "16860"`.
  - `"metro pull reuses county retry rules"`: with a fake `perform` returning failures for 5 of 8 chunks, `fetch_laus_metro(<400 ids>, perform = …, cache_dir = tempfile())` returns NULL. Mirror the existing county test in `test-laus-fetch.R`.
  - `"metro line on CBSA counties"`:
    - with `l <- build_county_layers(fx_county_base(), fx_momentum(), metro_momentum = tibble::tibble(fips = "16860", ref_month = as.Date("2026-09-01"), rate_now = 3.9, rate_yr_ago = 3.6, momentum_pp = 0.3, flag = "None"))`;
    - the Walker label contains `"Chattanooga metro: +0.3 pp unemployment vs. a year ago (Sep 2026)"`;
    - the risk side panel for Walker contains the same text.
  - `"no metro line without metro data"`:
    - with `metro_momentum = NULL`, no label contains `"pp unemployment vs. a year ago"`;
    - a CBSA county whose CBSA is missing from `metro_momentum` gets no line, and nothing contains `"NA pp"`.
- [ ] **Step 3: Run** the file. Expected: FAIL.
- [ ] **Step 4: Implement.**
  - **Server, in the deferred step after county LAUS:** `areas <- fetch_laus_areas()`. If it is non-NULL, `ml <- fetch_laus_metro(areas$series_id)` and `rv$risk_parts$metro_momentum <- laus_momentum(ml)`, each wrapped in `tryCatch` → NULL.
  - Keep the side panel's "Click a county…" placeholder unchanged.
- [ ] **Step 5: Run the full suite.** Expected: PASS. Then make a live check with keys set:
  ```
  Rscript -e "source('global.R'); a <- fetch_laus_areas(); m <- fetch_laus_metro(a\$series_id); print(nrow(a)); print(subset(laus_momentum(m), fips=='16860'))"
  ```
  Expected: 393 areas and a Chattanooga row with a finite `momentum_pp`. If BLS returns 503 "busy", wait and retry. Do not loop; the quota is 500/day.
- [ ] **Step 6: Commit** `feat: metro-level Stress now from BLS metro LAUS in tooltips and side panel`

---

### Task 8: Seed, manifest, end-to-end check

**Files:**
- Modify: `R/data_risk.R` (`risk_seed_update` allow-list), `tests/testthat/test-seed.R`, `data/risk_seed/` (new .rds files), `manifest.json`

**Interfaces:**
- Produces: `risk_seed_update()` also copies:
  - `pep_vintage_v1`;
  - the newest `pep_county_\d+`;
  - `fred_sectors_v1`;
  - the newest `county_ind_\d+`;
  - `laus_areas_v1`;
  - the newest `laus_metro_n\d+`.

  It never copies `tracts_*`.

- [ ] **Step 1: Write the failing test** in `test-seed.R`: `"seed update copies Phase 2 keys, newest LAUS of each kind"`.
  - Put files in a temp cache dir: `pep_county_2024.rds`, `pep_county_2025.rds`, `laus_metro_n393.rds`, `laus_metro_n390.rds`, `laus_county_n3144.rds`, `fred_sectors_v1.rds`, `county_ind_2024.rds`, `laus_areas_v1.rds`, `pep_vintage_v1.rds`, `tracts_v3_47_2024.rds`.
  - The returned set equals all of these except `pep_county_2024`, `laus_metro_n390` and `tracts_v3_47_2024`.
  - Generalise the "newest by number" rule into one helper used for `laus_county_n`, `laus_metro_n`, `pep_county_` and `county_ind_`.
- [ ] **Step 2: Run.** Expected: FAIL. **Step 3: Implement.** **Step 4: Run the full suite.** Expected: PASS.
- [ ] **Step 5: Refresh the seed live** (keys set; this spends about 8 BLS queries if the metro cache is cold):
  ```
  Rscript -e "source('global.R'); b <- fetch_county_base(); fetch_laus_county(b\$GEOID); fetch_pep_county(); fetch_fred_sectors(); fetch_county_industry(attr(b,'acs_year')); a <- fetch_laus_areas(); fetch_laus_metro(a\$series_id); print(risk_seed_update())"
  ```
  Check that `du -sh data/risk_seed` stays under about 3 MB.
- [ ] **Step 6: Regenerate `manifest.json`.** Use the same explicit file list approach as before, adding the new seed files:
  ```r
  files <- c(list.files(".", pattern = "[.]R$"), list.files("R", full.names = TRUE),
             list.files("data/risk_seed", full.names = TRUE), "www" if it exists …)
  rsconnect::writeManifest(appDir = ".", appFiles = files, appMode = "shiny")
  ```
  First read the current `manifest.json` `files` keys and keep every non-seed entry it already lists. Confirm the package count is unchanged (259).
- [ ] **Step 7: End-to-end check** in the running app (Playwright MCP):
  - **Context lens:** Stress now, Stress ahead and an ACS layer all render with legends carrying source + as-of.
  - **Migration lens:** 4 layers.
  - Jump to "Chattanooga metro": the side panel header shows the metro line, and tracts load on ACS layers.
  - Back to US.
  - Note the idle tab load time (it was about 8.6 s). It must not be much slower: the new sources are seeded.
- [ ] **Step 8: Commit** `chore: seed Phase 2 sources and regenerate manifest`
