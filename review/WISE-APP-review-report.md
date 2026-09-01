# WISE-APP Comprehensive Review Report

**Scope:** Whole repository (`R/`, `batch/`, `dev/`, `tests/`, `inst/`, docs), plus live Chrome desktop verification
**Date:** 2026-08-28 · **Reviewed Repo State:** `main` @ `cf1cb48` · **Current Remediation Base:** `golem`
**Target Context:** Wider release beyond core team; primary performance bottleneck is Step 2 simulation.
**Deployment Model:** Posit Connect (git-backed, 1 session/process, Databricks backend, auto-connect — see §10.1) & Local R package (single user, custom data connection). Synchronous execution, desktop browser.

> **Remediation status (through 2026-09-01):** Five fix waves applied; completed rows are compressed under their sections, open rows keep full detail. Wave 0 determinism (DET-01..09) complete 2026-08-31, plus two low-risk batches and scoped parts of REACT-01/13, TEST-03/05, RED-09. On 2026-09-01: a blocker batch (DEP-01, INT-02/09, DEP-02/03 incl. the REACT-01 remainder, REACT-05, SEC-02, TEST-06/09 gradient bugs), a test batch (TEST-07 raw-weights contract, TEST-08 packaging), four output-neutral, equivalence-verified perf batches — **Batch A** (PERF-23b/08/25/10), **Batch B** (DUP-01, PERF-30/REACT-07), **Batch C** (PERF-22/34, RNG-stream-invariant, bit-identical), **Batch D** (PERF-32) — and a quick-win batch (INT-04, INT-07, UI-04, UI-38, RED-05; REACT-04/06 closed as no-action). Full edition-3 suite: **473/473 pass** (module testServer harnesses for mod_2_02/mod_2_03 added); built-source `R CMD check` clean at the code level (remaining findings environment-only — no `pdflatex`); `R CMD build` succeeds. Deployment commits `0b73a4c`/`459d3f7`/`fdd3960` — see §10.1.

---

## 1. Executive Summary

The codebase has a sound modular architecture (Golem structure, pluggable engine registry in `fct_fit_model.R`, Bootstrap 5 / bslib UI). Three gaps still block wide release:

1. **Result Integrity & State Synchronization:** Upstream changes trigger `renderUI` rebuilds that wipe selections, and completed downstream results remain presented after their inputs change (provenance-signature work INT-08 outstanding). (Reproducibility ✅ Wave 0 complete.)
2. **Release Readiness & UX:** Export is limited to two threshold-table CSV buttons, configuration cannot be saved/restored, long computations lack double-click guards, and uncaught errors are hidden by CSS (UI-01).
3. **Packaging:** `R CMD check` is clean at the code level (TEST-08 ✅, TEST-07 ✅); remaining check findings are environment-only (no `pdflatex` for the PDF manual on the check machine).

---

## 2. Priority Implementation Roadmap

```
┌──────────────────────────────────────────────────────────────────────────────────┐
│ Wave 0: Reproducibility & Determinism (DET-01..09) [COMPLETE 2026-08-31]         │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 0.5: Integrity/deployment blockers (INT-01..09, DEP-01..03, SEC-01..02)     │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 1: Reactivity & Correctness Quick Wins (REACT-01..14, DUP-01..02, PERF-07)  │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 2: Safe speedups (PERF-03/06/08/10/11/13/17/19..32/34/36/37)                │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 3: Release readiness - UI-01..04/10/23..26/28..36/38..41 + PERF-16          │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 4: Packaging, tests & repository hygiene (TEST-01..09, RED-01..09)          │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 5: Validated Numerical & Structural Refactors (PERF-02/05/09/15/26/33)      │
└──────────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Reproducibility & Determinism (`DET-*`) — ✅ COMPLETE (2026-08-31)

All fixed with isolated `withr::with_seed` streams, deterministic ordering, and regression coverage in `tests/testthat/test-determinism.R` (repeatability, seed sensitivity, exact caller RNG restoration); weather end-to-end tests in `test-fct_get_weather.R`. DET-01 policy seeding · DET-02 residual draws + run-time mode snapshot · DET-03/04 no global `set.seed`/RNG-consuming names · DET-05 dormant engines seeded · DET-06 app/batch Lasso aligned · DET-07 DuckDB single-thread invariant · DET-08 `collect_deterministic()` canonical ordering · DET-09 committed determinism tests replace the OneDrive-only script.

---

## 4. Result Integrity & State Synchronization (`INT-*`)

| ID | Issue & Location | Impact | Actionable Fix |
|---|---|---|---|
| **INT-01** | Dynamic inputs are rebuilt with defaults (`R/mod_1_06_model.R:277-478`, `R/mod_1_04_weather.R:43-241`, `R/mod_1_01_sample.R:98-120`, `R/mod_2_01_weathersim.R:310-323`, `R/fct_policy_sim_compare.R:968-1015`) | **High** (Changing model type, interactions, fixed effects, weather variables, economies, baseline data, or re-running Step 3 wipes adjacent selections and filters) | Snapshot compatible current values with `isolate(input$...)` and restore them as `selected`; explicitly reset only values no longer valid. |
| **INT-05** | Results use live labels/settings after fitting or simulation (`R/mod_1_07_results.R:108-185`, `R/mod_1_08_modelfit.R:73-94`, `R/fct_policy_sim_compare.R:428-431`); engine-conditional headings created only on first fit | **Medium** (Old results can be relabeled with new variables or historical years, while headings describe the first engine) | Snapshot labels, historical range, and engine specification into result objects; bind renderers and headings strictly to those snapshots. |
| **INT-06** | Survey reload and inner map/panel failures (`R/mod_1_02_surveystats.R:139-147, 181-223`) update microdata but can retain old `map_data()` / `cell_data()` | **Medium** (Map can show previous geography; failed panel join silently removes `loc_id_panel` and changes the VCV fallback) | Clear map/cell state at load start and on every failure; notify when `loc_id_panel` is unavailable because it changes inference. |
| **INT-08** | No provenance signature or stale-state invalidation links Step 1 fits, Step 2 simulations, and Step 3 policy results (`R/mod_1_07_results.R:87-88`, `R/mod_2_01_weathersim.R:603-604`, `R/mod_3_06_policy_sim.R:122-130`) | **High** (A refit or upstream re-run leaves completed downstream tabs presenting results from superseded data/model/weather inputs as current) | Store an immutable run signature in each result; on upstream changes clear dependent results or mark them stale and disable interpretation/export until re-run. |

**Done, compressed:** INT-02 (2026-09-01 — manual connect resets metadata up front, loads each file into a failure ledger, publishes the connection only when every load succeeds, persistent failure card; auto-connect shares the channel) · INT-03 (2026-08-31 — aggregators prefer the run-time residual snapshot over the live radio) · INT-04 (2026-09-01 — Step 3 scenario-aggregation failures collected and surfaced as a persistent warning toast naming the dropped scenarios, deduped per failure set) · INT-07 (2026-09-01 — Results/Diagnostics tabs track the `hist_sim` lifecycle: appended on run, `removeTab()` + flag reset on clear, re-appended on re-run; testServer-tested) · INT-09 (2026-09-01 — Step 3 computes in locals and publishes reactives + `sim_run_id` only after simulation and decomposition both succeed).

---

## 5. Backend Performance & Safe Speedups (`PERF-*`)

### 5.1 Open Bottlenecks (Wave 2 — output-neutral unless noted)

| ID | Bottleneck & Location | Expected Win | Actionable Fix |
|---|---|---|---|
| **PERF-23** | `R/mod_1_02_surveystats.R:153-213` & `R/fct_loc_panel.R:96-158` execute ~6 separate remote DuckDB parquet scans over `h3_df` | **Medium-Large** | Materialize remote table once into a local DuckDB temporary table via `dplyr::compute(name = ..., temporary = TRUE)`. |
| **PERF-31** | Step 3 re-aggregates baseline and every scenario/member whenever aggregation method or deviation label changes (`R/fct_policy_sim_compare.R:495-622`) | **Medium-Large** | Add a per-method workspace cache keyed only by value-affecting inputs; move `cmp_deviation`-dependent label assembly downstream because deviation is applied after aggregation. |
| **PERF-36** | Map pipeline retains raw GeoJSON strings beside string-only features, and continuous colours can create nearly one `addGeoJSON()` layer per distinct value (`R/mod_1_02_surveystats.R:153-179`, `R/fct_surveystats.R:573-602, 859-871`, `R/fct_outcome.R:432-453`, `R/fct_weatherstats.R:1156-1221`) | **Medium-Large for many polygons** | Compute bounds beside geometry in DuckDB, keep only raw JSON, emit one per-feature-style GeoJSON layer, enable Canvas on the survey map, and cache cell features by data/wave signature. These Leaflet edits are output-neutral; an optional `deckglgeoarrow` migration (CRAN v0.0.2, needs `mapgl` host + `geoarrow`/`nanoarrow`) is a gated prototype — see §5.3. (Parsed-copy elimination already shipped under PERF-10.) |
| **PERF-13** | `R/fct_get_weather.R:439-444, 644-713` re-fetches identical ERA5/CMIP6 parquet files; CMIP6 history opened twice | **Medium** | Bounded disk cache keyed by source/version, codes, variables, date range, and transformation; reuse loaded historical relation for the resolution probe. Do not use an unversioned key. |
| **PERF-16** | Existing `dev_mode` limits the ensemble to one model (`R/mod_2_01_weathersim.R:212-219, 576`) but leaves coefficient uncertainty enabled and is exposed as a development control | **Medium, explicitly output-changing** | Replace with a clearly labeled Preview mode (one member + point estimates, reported in provenance, never final output). Not an output-neutral speedup. |

**Done (2026-08-31), compressed:** PERF-03 (KDE/bandwidth hoisted out of the 9-tau loop, `fct_fit_model.R:234-243` + optional `dens` param in `compute_rif`); PERF-06 (dead `.feature_centroid()` deleted; `.geojson_bounds()` running min/max still open); PERF-07 (duplicate categorical diagnostics append deleted, `fct_sim_diag.R:140-165`); PERF-17 (`do.call(rbind, 1-row dfs)` → `dplyr::bind_rows`, 3 sites); PERF-19 (`resimulate_with_svy()` precomputes `train_aug`/prepared svy once — dormant-path win only, original "Large" estimate stale); PERF-20 (append `.svy_row_id` once before the key loop — `$<-` shallow-copies, not two full copies as first reported); PERF-21 (hoisted per-year `format()` in Step 3); PERF-24 (hoisted per-variable weather filters in density panel); PERF-27 (per-key training `ecdf()` threaded through `precomputed_ecdf_train`); PERF-28 (`installed.packages()` → `requireNamespace()`); PERF-29 (dead duplicate `bins_df` build removed); PERF-37 (stale custom-encoder comments removed).

**Done (2026-09-01), compressed (Batches A-D, all output-neutral, equivalence/bit-identical verified):** PERF-08 (single grouped `rowsum()` completeness pass, ~3x at 200k×72) · PERF-10 (parsed GeoJSON copies dropped from feature builders; `.geojson_bounds()` parses string-only features transiently) · PERF-22 (policy deltas + training ecdf precomputed once per run and threaded through the per-(scenario × year) decomposition loop; 1.5x at 50k HH, RNG-free, bit-identical) · PERF-23b (hash-cached `CREATE OR REPLACE SECRET` extended to S3/GCS/Azure) · PERF-25 (grouping hoisted out of the per-variable weather loop via `.summarise_loc_prep(prep =)`) · PERF-30 (display-only `band_q` dropped from `agg_workspace()`; poverty-line/bandwidth folded into method-specific cache keys, so moving those controls no longer destroys unrelated cached methods) · PERF-32 (prediction frame released before the N×K matrix build; process peak now bounded by the join phase, byte-identical) · PERF-34 (ID→residual lookup + residual variance built once per pipeline/member; bit-identical, RNG unchanged). Regression coverage: `test-mod_2_02_results.R` (first testServer harness for the module).

### 5.2 Numerical Refactoring (Wave 5 — Requires Golden-File Validation)

| ID | Location | Potential Risk | Actionable Fix |
|---|---|---|---|
| **PERF-02** | `R/fct_get_weather.R:138-175, 532-533, 829-831` | Restructures SQL query plan; altered float sum order | Collapse V separate scans/joins into single wide `summarise(across(...))` + single join. |
| **PERF-05** | `R/fct_weatherstats.R:749-771` & `R/fct_surveystats.R:784-815` | Replaces custom loop with dplyr grouped aggregation | Rewrite custom `split/lapply` math as one grouped `summarise()` pass. |
| **PERF-09** | `R/fct_weatherstats.R:1346-1352` & `R/fct_surveystats.R:1065-1073` | Per-variable loop over full frame | Replace with `summarise(across(all_of(vars), ~mean(is.na(.x))))`. |
| **PERF-15** | `R/fct_simulations.R:664-665` vs `R/fct_predict_outcomes.R:154-157` | Modifies `fixest` row-dropping & offset handling | Reuse design matrix between prediction and uncertainty factor loading. |
| **PERF-26** | `R/fct_simulations.R:493-498` | Date parsing edge cases | Replace `as.integer(format(timestamp, "%m"))` with `as.POSIXlt(timestamp)$mon + 1L`. |
| **PERF-33** | `R/utils_mod_1_helpers.R:32-78` full-frame `split()` + nested `rbind`; six Step 1 survey tables + weather table call it; `batch/R/batch_utils.R:246-714` already has grouped `collapse` engines | **Not output-neutral naively**: app uses reliability-weights unbiased denominator $\sum w-\sum w^2/\sum w$; `collapse::fsd(w=)` uses $\sum w-1$ and produced materially different SDs; batch path also lacks the app's `is.finite(x) & is.finite(w) & w > 0` mask; `GRP()` uses C-locale ordering and retains an `NA` group; `collapse` undeclared in `DESCRIPTION`/`renv.lock` | Reuse `GRP`/`fmean`/`fsum`/`fmin`/`fmax`/`fnobs` but preserve the app mask and compute variance from grouped $\sum w$, $\sum w^2$, $\sum w(x-\bar{x})^2$; compute missingness on unmasked rows; drop `NA` group keys. Cache one grouping across all tables. Synthetic validation: ~1.5-2.2x weighted, ~5x missingness; add a committed production-scale benchmark with exact N/min/max/missingness and two-decimal mean/SD parity (invalid weights, all-missing/single-row groups, constants, `NA` keys, non-ASCII ordering). |

### 5.3 Map Rendering Decision Gate

1. Benchmark current Leaflet on representative 1k, 10k, and maximum-production polygon sets: server build time, serialized payload bytes, browser first paint, pan FPS, hover/popup latency, peak browser memory.
2. Apply the remaining output-neutral fixes (PERF-36; PERF-10 already shipped) and repeat the benchmark.
3. Prototype `deckglgeoarrow` only if optimized Leaflet misses agreed targets: identical classification/legend values, tooltips, map view, Shiny proxy updates, no runtime CDN, fresh-clone Connect deploy, non-WebGL fallback. Keep geometry server-mediated; never expose storage credentials/tokens or direct browser URLs to restricted GeoParquet.
4. Provisional targets at 10k polygons: exactly one data layer, server widget build <=300 ms, time-to-interactive <=1.5 s, sustained pan/zoom >=45 FPS. Pursue WebGL/GeoArrow only if optimized Leaflet fails the maximum-production or 50k-polygon probe.

---

## 6. Reactivity & Pipeline Correctness (`REACT-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **REACT-02** | No busy guards on loads, fit, or simulation actions (`R/mod_1_02_surveystats.R:101`, `R/mod_1_05_weatherstats.R:88`, `R/mod_1_07_results.R:63-92`, `R/mod_1_06_model.R:702-794`, `R/mod_2_01_weathersim.R:527-631`, `R/mod_3_scenario.R:307-309`) | **High** | Add observer-side `running <- reactiveVal(FALSE)` guards with `on.exit(running(FALSE))`; disable the triggering control while running. |
| **REACT-03** | `R/mod_1_02_surveystats.R:101-224` & `R/mod_1_05_weatherstats.R:88-163` re-execute full I/O on repeated clicks | **Medium** | Cache hash signature (`digest::digest()`) of inputs; short-circuit identical load requests. |
| **REACT-08** | Term extraction duplicated across `mod_3_02..mod_3_06` (`R/mod_3_02_infra.R:49-89` et al.) | **Medium** | Extract shared helper into `utils_mod_1_helpers.R`. |
| **REACT-09** | `R/mod_3_scenario.R:307-309` calls child module's exported `run()` closure directly | **Low** | Pass reactive trigger parameter into child module server. |
| **REACT-11** | Parallel aggregation engines in `R/mod_2_02_results.R:287-517` vs `R/fct_policy_sim_compare.R:412-527` | **Low** | Consolidate Step 2 and Step 3 aggregation into unified `fct_aggregation.R` engine. |
| **REACT-14** | Model fitting can silently fall back from logistic to linear or clustered to unclustered VCV (`R/fct_fit_model.R:789-798, 926-933`) while the observer reports only generic success (`R/mod_1_07_results.R:73-90`) | **High** (Fitted estimand/inference can differ from the user's selection without persistent disclosure) | Capture structured warnings/fallback metadata in `model_fit`, show in the settings/provenance banner, require explicit acknowledgement for model-family fallback. |

**Done, compressed:** REACT-01 (2026-09-01 — `return()` bug fixed; local-path validation closed by DEP-03) · REACT-05 (2026-09-01 — decomposition failures fail the run with a persistent alert + toast; partial per-scenario-year failures publish results with a warning naming the dropped count) · REACT-07 (2026-09-01 — closed via PERF-30: display-only dependencies removed, method-specific cache keys, eager warming only for workspace rebuilds; no further scope identified) · REACT-10 (2026-08-31) · REACT-12 (2026-09-01 — `fct_run_simulation()` collects a per-key failure ledger (key/group/error/is_hist); the run throws when the historical key or *all* members of a requested (SSP × period) group fail, so no unusable results are published and previous results survive; partial member failures publish with a persistent warning naming the failed keys, a partial-results completion toast, and per-scenario `n_models`/`n_models_requested` provenance; weather/pipeline functions injectable for tests — 5 direct `fct_run_simulation()` ledger tests, the TEST-01 remainder) · REACT-13 (2026-09-01 — hero point-range scenario filter fixed; first module testServer harness added in `test-mod_2_02_results.R`; broader output-family assertions tracked as ongoing test coverage). **Closed, no action:** REACT-04 (Info — replacement semantics are by design; merging would resurrect removed scenarios) · REACT-06 (Low — extraction declined per the review's own guidance).

---

## 7. UI, UX & Accessibility Findings (`UI-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **UI-01** | `custom.css:120-129` sets `.shiny-output-error { visibility: hidden; }` while zero `validate(need())` calls exist | **High** | Remove blanket CSS hide; add explicit `validate(need(...))` to long renderers; style error states as visible warning cards. |
| **UI-02** | Config flyouts (`mod_1_04:87-239`, `mod_1_06:28-48`, `mod_2_01:43-57`) share fixed `left: 400px; top: 90px`, can remain open across accordion switches, lack focus/Escape handling | **High** | Anchor one flyout to its sidebar, enforce one-open state, synchronize `aria-expanded`, move focus on open/close, close on Escape. |
| **UI-03** | Visual headings/`tags$label` not associated with controls whose Shiny `label = NULL` (`R/mod_2_01_weathersim.R:69-157, 310-323`, `mod_2_02:813-821`, `mod_3_01_sp:117-194, 325-345`, Step 3 helpers incl. `mod_3_04_labor:126-206`) | **High** | Use each input's `label`, or `<label for=...>` / `aria-labelledby`; preserve visible grouped headings. |
| **UI-10** | Collapsing the Lasso forced-covariates panel (`R/mod_1_06_model.R:456-474, 511-617`) removes inputs; live `selected_model()` changes the apparent model contract while `model_fit` stays unchanged | **High** | Keep inputs registered via CSS visibility, or snapshot forced-in/out + advanced settings at fit time; all downstream consumers use the snapshot. |
| **UI-23** | Export incomplete: only Step 2/3 threshold DTs expose CSV (`R/mod_2_02_results.R:1276-1284`, `R/fct_policy_sim_compare.R:1047-1055`); other tables, plots, configuration, results bundle have no export | **High** | Phase 1: CSV for aggregated plot/table data, server-rendered PNG from the same ggplot builder (fixed dims/DPI), config JSON, provenance sidecar. Prefer base PNG or declare `ragg` + lock entry. No widget screenshots; no raw household-row export without a disclosure policy. |
| **UI-24** | Configurations cannot be saved/shared/restored (`enableBookmarking` inactive) | **High** | JSON export/import of complete analysis configuration state (inputs, model specs, random seed). |
| **UI-25** | Steps 1-3 openable in any order without prerequisite indicators | **Med-High** | Completion badges (✓) in nav header; "Prerequisites: ..." notices in overview empty states. |
| **UI-26** | Summaries/headers omit source identity, exact model specification, seed, run signature (`mod_1_04:292-338`, `mod_1_06:107-147`, `R/mod_2_01_weathersim.R:340-393`, `mod_2_02:759-783`, `R/fct_policy_sim_compare.R:944-966`); Step 2 hardcodes "All models" even in dev mode (`:363`) | **Medium** | Standardize provenance banners from immutable result metadata; show ensemble count/dev mode accurately. |
| **UI-28** | Core Step 2 settings (baseline survey, periods) hidden inside flyout (`R/mod_2_01_weathersim.R:59-121`) | **Medium** | Surface core controls in sidebar; reserve flyout for advanced parameters. |
| **UI-29** | Steps 1/2 run buttons still silently no-op when prerequisites are absent (`mod_1_06:49`, `R/mod_2_01_weathersim.R:527-529`). ⚠️ **PARTIALLY DONE** (2026-08-31): Step 3's checks render via a persistent `alert-danger` output (`R/mod_3_06_policy_sim.R:105-140`) | **High** | Disable run buttons and add prerequisite explanations before click across all steps. |
| **UI-32** | Social protection targeting inputs (`mod_3_01_sp.R:197-216`) lack pre-run feedback on eligible population size | **Medium** | Dynamic text output with estimated eligible household count based on active cutoff. |
| **UI-36** | All 21 `plotOutput()` sites omit alt text (e.g. `R/mod_2_02_results.R:196, 242`; `R/mod_2_03_diagnostics.R:51, 87, 114`); three Leaflet maps lack text equivalents | **Medium** | Reactive `alt` text for plots; adjacent text/table summaries for maps. |
| **UI-41** | All charts are static ggplots; global widget conversion would serialize household-level values and can misrender complex geoms (21 sites; aggregated candidates at `R/mod_2_02_results.R:1246-1344`, `R/fct_policy_sim_compare.R:1017-1115`, `R/mod_3_09_decomposition.R:134-201`) | **Medium, confidentiality/rendering risk** | Keep static ggplot canonical. After UI-23, prototype interactivity only for aggregated charts; compare `ggiraph` vs `plotly` empirically (neither declared/locked). Restore `suspendWhenHidden = TRUE` for Step 3 chart widgets (`R/fct_policy_sim_compare.R:1031, 1102, 1115`) unless measured necessary. Household-level plots stay server-rendered with CSV/text equivalents. |

**Done (2026-08-31), compressed:** UI-30 (inverted future-period warning UI); UI-31 (deleted ~650 dead lines: `pol_*` trio, `.plot_beta_curves`, plus stale Rd/NAMESPACE entries); UI-33 (relabeled "Share of unserved gaining access/attainment (%)" with help lines); UI-34 (visible warning when manufacturing+services >100% and agriculture is clamped); UI-35 (updated obsolete repo URLs in DESCRIPTION/README/CLAUDE.md — note: actual `git remote -v` still points at `welfare-weather-app`; the canonical remote is now `worldbank/wise-app` after the rename); UI-39 (nav links: `aria-label` + `rel="noopener noreferrer"`); UI-40 (corrected coefficient-uncertainty help to state delta-method-only).

**Done (2026-09-01), compressed:** UI-04 (colorblind-safe palettes: Okabe-Ito centralized in `utils_plot_theme.R` with `wise_scale_colour/fill_okabe_ito()` replacing ColorBrewer Set1 in all result/decomposition plots; SSP colours now bluish green/blue/vermillion; coverage map ramp now vermillion/orange/bluish green) · UI-38 (unchecking the final Step 2 scenario checkbox re-checks the held scenarios instead of silently re-displaying the first one; the grid can never drop its last selection).

### 7.1 Recommended Output Delivery Sequence

1. **Exports first:** CSV of the tidy aggregated frame behind each result plot + server-rendered PNG from the same ggplot object. Include source/model/seed/run signature in filenames or a sidecar; raw household-row export requires a disclosure policy and is out of scope for the default UI.
2. **Selective interactivity second:** only aggregated decision charts; preserve the static ggplot path for publication PNG and as fallback.
3. **No global conversion:** household-level distributions, residual diagnostics, binscatters, and policy histograms remain server-rendered; Leaflet maps are already interactive.

---

## 8. Redundancy & Dead Code (`RED-*` & `DUP-*`)

- **RED-01** `inst/mathjax/` no longer exists; the prior ~100 MB deletion recommendation is stale. No action.
- **RED-06** Six near-identical simulation scripts (`batch/04_run_sim_*.R`, ~11,500 lines) → single canonical script parameterized via `Sys.getenv("WISEAPP_COUNTRY")` + YAML config.
- **RED-08** Committed one-off scratch scripts in `dev/` and `batch/` → move historical scripts to `dev/archive/` (`dev/` still holds several one-off `test_*.R` scripts).
- **RED-09** ⚠️ **PARTIALLY DONE**: `.Rbuildignore` anchoring fixes applied and the file is now tracked in git (commit `5550efc`). README re-render still open (`covr` unavailable locally).
- **DUP-03** `fct_policy_sim_compare.R` mirrors plotting/aggregation internals of `fct_sim_compare.R` → extract shared series assembly, threshold tables, exceedance rendering.

**Done (2026-08-31), compressed:** RED-02 (deleted `golem_utils_ui.R`/`golem_utils_server.R`, regenerated NAMESPACE); RED-03 (deleted `dev/archived_fct/`); RED-04 (byte-identical `dev/` vs `batch/R/` scripts repointed and deduped); RED-07 (deleted `fct_h3_check.R` + export + Rd); DUP-02 (deleted weaker `.normalise_ssp()` duplicate; the robust implementation was already winning).

**Done (2026-09-01), compressed:** DUP-01 (shadowed `resolve_band_q` duplicate deleted from `fct_aggregation.R`; `fct_sim_compare.R` is the single authoritative definition whose `minmax = 0/1` semantics every caller already saw — output-neutral; key-contract tests added) · RED-05 (never-executed `spec_curve.R` moved out of the auto-sourced `batch/R/` to `dev/archive/`).

---

## 9. Testing & Packaging (`TEST-*`)

| ID | Issue & Location | Actionable Fix |
|---|---|---|
| **TEST-03** | ⚠️ **PARTIALLY DONE**: `Suggests` complete (incl. `covr`, 2026-09-01); edition 3 set; renv.lock already had `testthat`/`spelling`/`covr`. | `covr` used in `README.Rmd` still unavailable locally for re-render. |
| **TEST-05** | ⚠️ **PARTIALLY DONE**: edition-3 suite fully passes; `R CMD build` succeeds; built-source `R CMD check` clean on every code-level section. | Remaining findings are environment-only on the check machine: PDF-manual ERROR/WARNING (no `pdflatex`), tidy HTML validator, clock skew. Re-verify on a machine with TeX. |

**Done, compressed:** TEST-02/TEST-04 (2026-08-31 — unused imports removed; CLAUDE.md inventory updated) · TEST-01 (2026-09-01 — direct `fct_run_simulation()` failure-ledger tests added under REACT-12, completing the direct-coverage list: `fit_model()`, `run_sim_pipeline()`, `apply_policy_to_svy()`, `apply_policy_delta_to_baseline()`, `fct_run_simulation()`) · TEST-06 (2026-09-01 — `prosperity_gap`/`avg_poverty` delta-method gradients fixed (dropped chain-rule factors restored); stale tests + `.claude/method_uncertainty.md` §3.6/§3.7 rewritten; finite-difference validation for every smooth method) · TEST-07 (2026-09-01 — statistical contract decided: **raw survey weights**; stale normalization test replaced with a raw-weight preservation assertion; `merge_survey_weather()` roxygen corrected) · TEST-08 (2026-09-01 — runtime deps declared (arrow/brand.yml/pkgload → Suggests, bit64 added), `stats::vcov`, 16 Rd usage fixes, 24 R sources ASCII-fied (`\uXXXX` escapes preserve UI glyphs), `R/globals.R` for visible bindings, `.Rbuildignore` additions) · TEST-09 (2026-09-01 — delta gradient now $-w_i/W \cdot 28/\mu_i$ below \$28; exact-formula + finite-difference tests; point estimates were unaffected, coefficient bands were).

---

## 10. Deployment, Connections & Security (`DEP-*` / `SEC-*`)

| ID | Issue & Location | Severity / Impact | Actionable Fix |
|---|---|---|---|
| **DEP-01** | ✅ Bundled `inst/duckdb_extensions/` binaries + fail-fast `.duck_load_ext()` (missing bundle → loud error; extension recorded only after a successful `LOAD`) (2026-09-01) | **Critical** | Remaining: verify a fresh-clone Connect deployment end to end. |
| **DEP-02** | ✅ Dead GCS-keyfile/HF-token controls removed (GCS exposes HMAC keys; HF documented public-repos-only); Azure help corrected to Account Key; blank S3/GCS inputs fall through to env credentials (2026-09-01) | **High** | Remaining: integration-test each supported auth contract against real backends. |
| **SEC-03** | Process-wide DuckDB connection, views, tokens, secrets have no session-end cleanup (`R/fct_load_data.R:21-43, 128-179`) | **Low on one-session Connect; Medium for repeated local `run_app()` sessions** | Disconnect and clear caches in `session$onSessionEnded`; store token hashes rather than plaintext; prefer temporary views. |

**Done, compressed:** DEP-03 (2026-09-01 — local connects require `dir.exists()`; badge distinguishes "configured" from "metadata-verified") · SEC-01 (2026-08-31 — `.sql_literal()` applied to every dynamic credential with adversarial quote tests) · SEC-02 (2026-09-01 — every materialised weather temp table registered at creation in a cleanup ledger with one best-effort `on.exit` drop on all paths) · SEC-04/SEC-05 (2026-08-31 — data-derived Leaflet popups and DT labels escaped).

### 10.1 Git-backed Posit Connect deployment (config as of 2026-09-01; commits `0b73a4c`, `459d3f7`, `fdd3960`)

- Connect pulls straight from this GitHub repo (poll or "Update Now") instead of a push-button `rsconnect::deployApp()` bundle. This requires a committed root `manifest.json`; `app.R` runs via `pkgload::load_all()` because `wiseapp` is never installed into the Connect library.
- **Regenerating the manifest:** whenever dependencies or the shipped file set change, run in `dev/03_deploy.R`: `rsconnect::writeManifest(appDir = ".", appFiles = c("app.R", "R", "inst", "man", "DESCRIPTION", "NAMESPACE"), appPrimaryDoc = "app.R")`, then **re-strip `sf`** from `manifest.json$packages` before committing — `{leaflet}` hard-Imports sf, which the Connect host cannot build, but the app never loads it at runtime (only excluded `batch/` uses it). Verify with `grep -c '"sf":' manifest.json` → `0`, then commit the diff.
- **DuckDB extensions:** `httpfs`, `h3`, `spatial` are committed under `inst/duckdb_extensions/` as gzipped **v1.5.5 binaries matching the Connect library's DuckDB version** (version drift breaks `INSTALL`); `.duck_load_ext()` prefers the `.gz` (DuckDB decompresses on INSTALL) and falls back to uncompressed. `renv.lock` is pinned to the matching DuckDB.
- Renew this manifest/binary pairing on every DuckDB or dependency upgrade; a stale binary version breaks extension `INSTALL` on Connect with a loud error (fail-fast since 2026-09-01), not silent breakage.
