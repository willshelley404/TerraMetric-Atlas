# Conversation Handoff: Regional Risk Map — start Phase 2

> Handoff summary generated 2026-11-12 from a previous conversation, not a full transcript. Open a new chat in `C:\code_repos\TerraMetric-Atlas` and paste this, or point Claude at this file.

## Context

TerraMetric-Atlas ("EconPulse AI") is an R/Shiny economic dashboard deployed to Posit via `manifest.json`. A Regional Risk Map replaced the old Metro Map (state choropleth + metro bubble map). It is aimed at investors and policymakers, and answers two questions:
- who will need assistance;
- where default risk is building.

Phases 0–1 are built, reviewed and merged. The next job is **Phase 2**.

## Where things stand

- **Branch:** `recession-glm-tracker-method` @ `8e62643` contains everything. It was fast-forward merged from `regional-risk-map`. The suite has 14 test files and 230 tests, all passing.
- **Run the tests:** `Rscript -e "testthat::test_dir('tests/testthat')"`
- **Spec (authoritative):** `docs/superpowers/specs/2026-11-12-regional-risk-map-design.md`.
  - §12 Phase 2 now also includes **metro-level Stress now**. That edit is not yet committed.
- **Phase 0–1 plan (done):** `docs/superpowers/plans/2026-11-12-regional-risk-map-phase-0-1.md`.
- **Built so far:**
  - Rent-burden fix.
  - County choropleth with the "Stress now" layer: BLS LAUS county unemployment, pairwise year-over-year 3-month momentum.
  - ACS context layers at county and tract level.
  - Click-to-tract drill-down. Tracts are ranked within their CBSA across state lines, and within their state. Non-CBSA counties use "non-metro {State}".
  - Per-layer MOE noise flags.
  - Option-B tooltips (score + top drivers), side panel, Jump to metro, Back to US, breadcrumb.
  - Progressive load: counties render first, then BLS data fills in.
  - Two-tier cache, plus a starter copy in the repo that refreshes in the background.
- **Lens dropdown** currently offers only "Context". New lenses are added as each phase ships: Migration (2), Assistance (3), Credit (4).

## Key details to preserve

- **Code layout:**
  - `R/data_risk.R`: I/O, cache, seed, BLS and ACS fetchers.
  - `R/risk_metrics.R`: pure metric functions, plus `RISK_CFG`.
  - `R/map_risk.R`: leaflet/HTML builders, plus the `RISK_LAYERS` registry.
  - `server.R`: "REGIONAL RISK MAP" section, around line 1245.
  - `ui.R`: `tabItem("metro")`. The tab name stayed `metro`; the menu label is "Regional Risk".
- **Cache:**
  - Location: `tools::R_user_dir("TerraMetricAtlas","cache")`, overridable with `options(risk.cache_dir=)`.
  - `risk_cached(key, ttl_days, fetch_fn, ..., fail_ttl_days, seed_dir, bg)`.
  - A value can shorten its own lifetime with `attr(,"ttl_days")`.
- **Starter copy:**
  - Location: `data/risk_seed/` (~1 MB): `acs_year_v2`, `county_shapes_2024`, `cbsa_xwalk_2023`, `county_acs_2024`, `laus_county_n3144`.
  - A stale seed is served at once, and a `callr` background job (`.risk_bg_start` / `.risk_bg_poll`) refreshes it. Disable with `options(risk.seed_dir="")`.
  - Update before a deploy:
    ```
    Rscript -e "source('global.R'); risk_seed_update()"
    ```
- **Cache key versions:** `tracts_v3_{st}_{yr}` (bump the version when tract columns change) and `laus_county_n{count}`.
- **BLS:**
  - API v2; series `LAUCN{5-digit FIPS}0000000003`; 50 series per call; `max_active = 8`; 30 s timeout.
  - Quota is 500 queries/day; a full county pull is 64 calls.
  - When more than 50% of calls fail there are no retries, and the failure is remembered for 1 h. A partial pull is flagged "partial" with a 6 h TTL.
  - The local cache is good until **Nov 19**.
  - FRED monthly county rate IDs are not built from FIPS.
- **ACS year:** `risk_acs_year()` probes from current year − 1 downward and never caches a failed probe. ACS 2021–2025 is due Dec 2026.
- **Keys:** `export R_ENVIRON_USER="C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron"` is needed for Rscript live checks.
- **Verified sources for Phase 2:**
  - **Census PEP V2025 components:** `https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/totals/co-est2025-alldata.csv` (fields include `DOMESTICMIG2025`, `INTERNATIONALMIG2025`, `NETMIG`, `RESIDUAL`).
    - The PEP API stopped at V2019.
    - `tidycensus` 2.0 reads PEP via `get_estimates(product="components")`, but **1.8.1 is installed**, so read the flat CSV directly.
  - **FRED CES sector series:** `USCONS`, `MANEMP`, `USTRADE`, `USLAH`, `TEMPHELPS`, `CES4300000001`, `USPBS`, `USFIRE`, `USINFO`, `CES6562000001`, `USGOVT`. Spec §9.2 also needs `USMINE`, `USWTRADE`, `USEHS` and `USSERV`, which are not yet verified.
  - **Metro LAUS:** the BLS publishes metro unemployment rates. The exact series-ID pattern still needs confirming before building.
- **Deploy:** `manifest.json` was regenerated with an explicit file list: same 259 packages, plus the new R files and the seed. Regenerate it whenever new runtime files are added; the script was `rsconnect::writeManifest(appDir=".", appFiles=files, appMode="shiny")`.
- **Observed facts:**
  - Alabama is uniformly +0.65 pp. This is real: BLS controls county estimates to the state total.
  - The idle tab load takes about 8.6 s.
  - A 56 s load seen once was contention with app startup (pre-existing).
  - 1,844 of 3,144 counties are in a CBSA (393 metro + 542 micro CBSAs).

## Decisions made

- Everything stays in the app: no separate build job or GitHub Action. The user explicitly dislikes extra jobs. The starter copy is the deploy-cold-start answer.
- Tracts are ranked within the labor market (CBSA) with the state rank shown as well, not nationally. National ranking would need all 51 LODES files.
- Ranks read in value terms ("Top" = highest value). The pill turns red only on the bad side.
- Tiles are default OSM; CARTO now needs an API key.
- Migration gets its own lens and also a standing line on every county tooltip. The user called it "interesting to people".
- Municipal bonds are covered only by a labeled "tax base pressure (proxy)" layer.

## Ruled out

- QCEW (about 60% of cells suppressed).
- FRED bulk county endpoint (about 400 MB, no date filter).
- The BLS 322 MB flat file in-app.
- National tract ranking.
- Within-state-only ranking: it creates seams in tri-state metros such as Chattanooga.

## Open items / next steps

1. **Write the Phase 2 plan** (superpowers:writing-plans) from spec §9.2, §9.3, §9.7, §12 Phase 2:
   - Migration lens (4 PEP layers, diverging palette, a standing line on county tooltips, a metro-table column);
   - Stress ahead (shift-share);
   - **metro-level Stress now**.
2. **User still to confirm:** the MOE noise-flag threshold change. Spec said MOE > 50% of the estimate; the build uses MOE > estimate (`moe_flag_ratio = 1.0`) because 0.5 flagged 67% of tracts.
3. **Deferred minor review items:**
   - pooled MOE uses root-sum-of-squares, not `moe_sum`'s zero-estimate rule;
   - the side panel shows a literal NA as-of date while loading, and stale/partial isn't shown there;
   - an ACS county-fetch failure isn't marked "unavailable";
   - the labor-market pill isn't qualified when a frame state failed to load;
   - `rv$risk_failed` is permanent per session;
   - Jump-to-metro re-select doesn't fire / `which.max` on all-NA population;
   - no outer `tryCatch` with `showNotification` on the drill/layer/LAUS observers;
   - dead `higher_is_worse` parameters;
   - the Stress now pill is keyed to the median rather than `momentum > 0`;
   - "Sahm-style" wording is missing;
   - every layer switch re-sends 3,144 polygons;
   - no `shiny::testServer` tests.
4. **Cleanup:** the old worktree `.claude/worktrees/regional-risk-map` and branch `regional-risk-map` are still on disk (untracked screenshots in its `.playwright-mcp/`). Delete them if not needed.
5. **Uncommitted:** the spec §12 edit and this handoff file.
