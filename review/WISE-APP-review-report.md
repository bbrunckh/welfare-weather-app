# WISE-APP Comprehensive Review Report

**Scope:** Whole repository (`R/`, `batch/`, `dev/`, `tests/`, `inst/`, docs), plus live Chrome desktop verification
**Date:** 2026-08-28 · **Reviewed Repo State:** `main` @ `cf1cb48` · **Current Remediation Base:** `golem`
**Target Context:** Wider release beyond core team; primary performance bottleneck is Step 2 simulation.
**Deployment Model:** Posit Connect (git-backed, 1 session/process, Databricks backend, auto-connect — see §10.1) & Local R package (single user, custom data connection). Synchronous execution, desktop browser.

> **Remediation status (through 2026-09-02):** Waves 0 and 0.5 are complete; most Wave 1-4 items are fixed. Completed rows are compressed under their sections, open rows keep full detail. On 2026-09-01, 17 batches landed: deployment/test blockers (DEP-01..03, INT-02/09, REACT-05, SEC-02, TEST-06..09) · output-neutral perf batches A-D (PERF-08/10/13/22/23/23b/25/30/32/34, DUP-01 — all equivalence- or bit-identical-verified) · quick wins (INT-04/07, UI-04/38, RED-05) · three High-severity fixes (REACT-02 busy guards, REACT-12 failure ledger, UI-01 visible errors, PERF-31 aggregation cache) · standalone perf wins (PERF-13/16/23) · the Wave 0.5 completion batch (INT-01/05/06/08) · a Step 1 responsiveness batch (button-snapshot rendering in Outcome/Weather stats + survey-reload staleness banners) · a threshold-table matrix fix (transposed per-model RP matrix with a single admissible RP). On 2026-09-02, the Wave 5 grouped-aggregation items PERF-05/09/33 landed as collapse-based rewrites (see §5.2). Remaining: the open rows below, Wave 5 golden-file refactors (PERF-02/15/26), and environment-only verifications (DEP-01/02 live backends, PDF manual). Suite: **639/639 pass**, no warnings; `R CMD build` clean. Deployment commits `0b73a4c`/`459d3f7`/`fdd3960` — see §10.1.

---

## 1. Executive Summary

The codebase has a sound modular architecture (Golem structure, pluggable engine registry in `fct_fit_model.R`, Bootstrap 5 / bslib UI). Three gaps still block wide release:

1. **Result Integrity & State Synchronization:** Upstream changes trigger `renderUI` rebuilds that wipe selections, and completed downstream results remain presented after their inputs change. ✅ Closed 2026-09-01: dynamic inputs restore selections (INT-01), results bind to fit-time snapshots (INT-05), map state clears on reload/failure (INT-06), and run signatures with stale-marking now link Step 1 fits, Step 2 simulations, and Step 3 policy results (INT-08). (Reproducibility ✅ Wave 0 complete.)
2. **Release Readiness & UX:** Export is limited to two threshold-table CSV buttons (UI-23), configuration cannot be saved/restored (UI-24), flyout/a11y gaps (UI-02/03/10), and run buttons can still no-op without prerequisites (UI-29).
3. **Packaging:** `R CMD check` is clean at the code level (TEST-08 ✅, TEST-07 ✅); remaining check findings are environment-only (no `pdflatex` for the PDF manual on the check machine).

---

## 2. Priority Implementation Roadmap

```
┌──────────────────────────────────────────────────────────────────────────────────┐
│ Wave 0: Reproducibility & Determinism (DET-01..09) [COMPLETE 2026-08-31]         │
└────────────────────────┬─────────────────────────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────────────────────────┐
│ Wave 0.5: Integrity/deployment blockers [COMPLETE 2026-09-01]                    │
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
│ Wave 5: Validated Numerical & Structural Refactors [PERF-05/09/33 ✅ 2026-09-02] │
│ (remaining open: PERF-02/15/26)                                                  │
└──────────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Reproducibility & Determinism (`DET-*`) — ✅ COMPLETE (2026-08-31)

All fixed with isolated `withr::with_seed` streams, deterministic ordering, and regression coverage in `tests/testthat/test-determinism.R` (repeatability, seed sensitivity, exact caller RNG restoration); weather end-to-end tests in `test-fct_get_weather.R`. DET-01 policy seeding · DET-02 residual draws + run-time mode snapshot · DET-03/04 no global `set.seed`/RNG-consuming names · DET-05 dormant engines seeded · DET-06 app/batch Lasso aligned · DET-07 DuckDB single-thread invariant · DET-08 `collect_deterministic()` canonical ordering · DET-09 committed determinism tests replace the OneDrive-only script.

---

## 4. Result Integrity & State Synchronization (`INT-*`) — ✅ COMPLETE (2026-09-01)

**Done, compressed:** INT-01 — `.restore_selection()`/`.restore_numeric()` helpers; every dynamic input (Step 1 year selectors, weather selector, model panel incl. Lasso force/advanced, Step 2 baseline survey, Step 3 poverty line + scenario filters) restores its prior selection clipped to the new choices, falling back to the old defaults only when nothing survives. · INT-08 — immutable run signatures on fit/sim/policy results (survey version + config digests via `.sig_plain()`), staleness observers with a Step-2→3 cascade, `.stale_banner()` on all result surfaces, CSV exports dropped while stale (stale-mark semantics per user decision). · INT-05 — fit results carry a fit-time `.snap` (outcome/weather/variable labels + survey-weather frame); all Step 1 renderers bind to it; engine-conditional headings are reactive; Step 3's historical label is bound to the run. · INT-06 — map/cell state cleared on survey reload and every failure; the `loc_id_panel` warning states the inference implication. · INT-02 — connect failure ledger; the connection publishes only when every file loads. · INT-03 — aggregators prefer the run-time residual snapshot. · INT-04 — Step 3 aggregation failures surface as a persistent toast naming dropped scenarios. · INT-07 — Results/Diagnostics tabs track the `hist_sim` lifecycle. · INT-09 — Step 3 publishes atomically after simulation + decomposition both succeed.

---

## 5. Backend Performance & Safe Speedups (`PERF-*`)

### 5.1 Open Bottlenecks (Wave 2 — output-neutral unless noted)

| ID | Bottleneck & Location | Expected Win | Actionable Fix |
|---|---|---|---|
| **PERF-36** | Map pipeline retains raw GeoJSON strings beside string-only features, and continuous colours can create nearly one `addGeoJSON()` layer per distinct value (`R/mod_1_02_surveystats.R:153-179`, `R/fct_surveystats.R:573-602, 859-871`, `R/fct_outcome.R:432-453`, `R/fct_weatherstats.R:1156-1221`) | **Medium-Large for many polygons** | Compute bounds beside geometry in DuckDB, keep only raw JSON, emit one per-feature-style GeoJSON layer, enable Canvas on the survey map, and cache cell features by data/wave signature. These Leaflet edits are output-neutral; an optional `deckglgeoarrow` migration (CRAN v0.0.2, needs `mapgl` host + `geoarrow`/`nanoarrow`) is a gated prototype — see §5.3. (Parsed-copy elimination already shipped under PERF-10.) |

**Done (2026-08-31), compressed:** small verified wins — PERF-03 (KDE hoisted out of the 9-tau loop) · PERF-06 (dead `.feature_centroid()` deleted) · PERF-07 (duplicate diagnostics append) · PERF-17 (`rbind` → `bind_rows`) · PERF-19 (dormant-path precompute) · PERF-20 (shallow-copy fix) · PERF-21/24 (hoisted format/filter loops) · PERF-27 (training `ecdf()` cached per key) · PERF-28 (`requireNamespace()`) · PERF-29 (dead `bins_df` build) · PERF-37 (stale comments).

**Done (2026-09-01), compressed (Batches A-D + standalone; all output-neutral, equivalence/bit-identical verified):** PERF-08 (single grouped `rowsum()` completeness pass, ~3x at 200k×72) · PERF-10 (parsed GeoJSON copies dropped from feature builders) · PERF-13 (bounded versioned disk cache for every remote weather parquet: digest-keyed local slices, one fetch per SSP file shared across baseline overlap + all periods, 2 GB LRU cap, `WISEAPP_WEATHER_CACHE_DISABLE`/`_FORCE` switches; bit-identical cold+warm) · PERF-22 (policy deltas + training ecdf precomputed once per run; 1.5x at 50k HH, bit-identical) · PERF-23 (remote `h3_df` view materialised into a local DuckDB temp table for the six downstream scans) · PERF-23b (hash-cached secrets for S3/GCS/Azure) · PERF-25 (grouping hoisted out of the per-variable weather loop) · PERF-30/REACT-07 (display-only `band_q` dropped from the aggregation workspace; poverty-line/bandwidth folded into method-specific cache keys) · PERF-31 (Step 3 aggregation cache keyed by source/method/poverty line, deviation applied downstream; cached ≡ uncached bit-identical) · PERF-32 (prediction frame released before the N×K build; byte-identical) · PERF-34 (ID→residual lookup + variance built once per pipeline; bit-identical) · PERF-16 (dev mode removed entirely per user decision). Regression coverage: `test-mod_2_02_results.R`, `test-policy-sim-compare-agg-cache.R`, `test-fct_wx_cache.R`, `test-fct_get_weather.R`.

### 5.2 Numerical Refactoring (Wave 5 — Requires Golden-File Validation)

| ID | Location | Potential Risk | Actionable Fix |
|---|---|---|---|
| **PERF-02** | `R/fct_get_weather.R:138-175, 532-533, 829-831` | Restructures SQL query plan; altered float sum order | Collapse V separate scans/joins into single wide `summarise(across(...))` + single join. |
| **PERF-15** | `R/fct_simulations.R:664-665` vs `R/fct_predict_outcomes.R:154-157` | Modifies `fixest` row-dropping & offset handling | Reuse design matrix between prediction and uncertainty factor loading. |
| **PERF-26** | `R/fct_simulations.R:493-498` | Date parsing edge cases | Replace `as.integer(format(timestamp, "%m"))` with `as.POSIXlt(timestamp)$mon + 1L`. |

**Done (2026-09-02), compressed (collapse throughout; old-vs-new parity harness run on adversarial synthetic data):** PERF-05 (`summarise_weather_by_loc()` and `merge_loc_values_to_cells()` rebuilt as grouped `collapse` passes over one shared `GRP()` — `.summarise_loc_prep()` now returns the grouping instead of split indices; continuous means, weighted/unweighted modal bins with their tie-break orders, `n_hh`/`n_months`/`n_locs` semantics and row order all preserved exactly; 69x on the Step 1 weather-map collapse at 50k rows x 10 vars, 5.9x on cell merge) · PERF-09 (wave-specific `% Missing` for all variables in one grouped pass via a new shared `survey_missingness_long()` helper used by both Step 1 stats tables; exact parity; ~25x) · PERF-33 (`weighted_summary_long()` rebuilt as six grouped `collapse` matrix passes — `fmean`/`fsd`/`fmin`/`fmax`/`fnobs` — with the app's `is.finite(x) & is.finite(w) & w > 0` mask folded in, NA-key rows dropped as `split()` did, and all-masked (countryyear, variable) cells still emitted with N = 0; weighted SD now uses `fsd(w=)` with the $\sum w-1$ denominator, accepted per user decision; 6.5x at 50k x 24, 2.2x at the 200k x 72 production scale). `collapse` 2.1.7 declared in `DESCRIPTION` Imports and `renv.lock` (`Rcpp` already locked). Follow-up collapse wins in the same pass: `.compute_hazard_values()` (`R/fct_policy_decompose.R`) and `summarise_weather_anomaly_by_loc()` (anomaly/percentile historical view — bit-exact parity incl. the old `weighted.mean` NA-weight poisoning semantics) now grouped passes (35x at 400k rows x 40k locations), the Step 3 decomposition channel aggregation (`mod_3_09_decomposition.R`) shares one grouping across channels instead of re-splitting per channel, and the binned-weather DT hoists its per-variable missingness into the shared helper. The Step 1 stats-table `Variable` column change is recorded under §7.

### 5.3 Map Rendering Decision Gate

1. Benchmark current Leaflet on representative 1k, 10k, and maximum-production polygon sets: server build time, serialized payload bytes, browser first paint, pan FPS, hover/popup latency, peak browser memory.
2. Apply the remaining output-neutral fixes (PERF-36; PERF-10 already shipped) and repeat the benchmark.
3. Prototype `deckglgeoarrow` only if optimized Leaflet misses agreed targets: identical classification/legend values, tooltips, map view, Shiny proxy updates, no runtime CDN, fresh-clone Connect deploy, non-WebGL fallback. Keep geometry server-mediated; never expose storage credentials/tokens or direct browser URLs to restricted GeoParquet.
4. Provisional targets at 10k polygons: exactly one data layer, server widget build <=300 ms, time-to-interactive <=1.5 s, sustained pan/zoom >=45 FPS. Pursue WebGL/GeoArrow only if optimized Leaflet fails the maximum-production or 50k-polygon probe.

---

## 6. Reactivity & Pipeline Correctness (`REACT-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **REACT-03** | `R/mod_1_02_surveystats.R:101-224` & `R/mod_1_05_weatherstats.R:88-163` re-execute full I/O on repeated clicks | **Medium** | Cache hash signature (`digest::digest()`) of inputs; short-circuit identical load requests. |
| **REACT-08** | Term extraction duplicated across `mod_3_02..mod_3_06` (`R/mod_3_02_infra.R:49-89` et al.) | **Medium** | Extract shared helper into `utils_mod_1_helpers.R`. |
| **REACT-09** | `R/mod_3_scenario.R:307-309` calls child module's exported `run()` closure directly | **Low** | Pass reactive trigger parameter into child module server. |
| **REACT-11** | Parallel aggregation engines in `R/mod_2_02_results.R:287-517` vs `R/fct_policy_sim_compare.R:412-527` | **Low** | Consolidate Step 2 and Step 3 aggregation into unified `fct_aggregation.R` engine. |
| **REACT-14** | Model fitting can silently fall back from logistic to linear or clustered to unclustered VCV (`R/fct_fit_model.R:789-798, 926-933`) while the observer reports only generic success (`R/mod_1_07_results.R:73-90`) | **High** (Fitted estimand/inference can differ from the user's selection without persistent disclosure) | Capture structured warnings/fallback metadata in `model_fit`, show in the settings/provenance banner, require explicit acknowledgement for model-family fallback. |

**Done, compressed:** REACT-01 (`return()` bug fixed; local-path validation closed by DEP-03) · REACT-02 (`.busy_guard()` refuses re-entry and disables buttons at all six load/run sites; error-path release tested) · REACT-05 (decomposition failures fail the run with a persistent alert; partial per-scenario-year failures publish with a warning naming the dropped count) · REACT-07 (closed via PERF-30) · REACT-10 (2026-08-31) · REACT-12 (per-key failure ledger in `fct_run_simulation()`: throws on historical or all-member group failure so unusable results never publish; partial failures publish with a persistent warning + `n_models`/`n_models_requested` provenance; TEST-01 remainder) · REACT-13 (hero point-range scenario filter fixed; module harness added). **Closed, no action:** REACT-04 (replacement semantics are by design) · REACT-06 (declined per review guidance).

---

## 7. UI, UX & Accessibility Findings (`UI-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **UI-02** | Config flyouts (`mod_1_04:87-239`, `mod_1_06:28-48`, `mod_2_01:43-57`) share fixed `left: 400px; top: 90px`, can remain open across accordion switches, lack focus/Escape handling | **High** | Anchor one flyout to its sidebar, enforce one-open state, synchronize `aria-expanded`, move focus on open/close, close on Escape. |
| **UI-03** | Visual headings/`tags$label` not associated with controls whose Shiny `label = NULL` (`R/mod_2_01_weathersim.R:69-157, 310-323`, `mod_2_02:813-821`, `mod_3_01_sp:117-194, 325-345`, Step 3 helpers incl. `mod_3_04_labor:126-206`) | **High** | Use each input's `label`, or `<label for=...>` / `aria-labelledby`; preserve visible grouped headings. |
| **UI-10** | Collapsing the Lasso forced-covariates panel (`R/mod_1_06_model.R:456-474, 511-617`) removes inputs; live `selected_model()` changes the apparent model contract while `model_fit` stays unchanged | **High** | Keep inputs registered via CSS visibility, or snapshot forced-in/out + advanced settings at fit time; all downstream consumers use the snapshot. |
| **UI-23** | Export incomplete: only Step 2/3 threshold DTs expose CSV (`R/mod_2_02_results.R:1276-1284`, `R/fct_policy_sim_compare.R:1047-1055`); other tables, plots, configuration, results bundle have no export | **High** | Phase 1: CSV for aggregated plot/table data, server-rendered PNG from the same ggplot builder (fixed dims/DPI), config JSON, provenance sidecar. Prefer base PNG or declare `ragg` + lock entry. No widget screenshots; no raw household-row export without a disclosure policy. |
| **UI-24** | Configurations cannot be saved/shared/restored (`enableBookmarking` inactive) | **High** | JSON export/import of complete analysis configuration state (inputs, model specs, random seed). |
| **UI-25** | Steps 1-3 openable in any order without prerequisite indicators | **Med-High** | Completion badges (✓) in nav header; "Prerequisites: ..." notices in overview empty states. |
| **UI-26** | Summaries/headers omit source identity, exact model specification, seed, run signature (`mod_1_04:292-338`, `mod_1_06:107-147`, `R/mod_2_01_weathersim.R:340-393`, `mod_2_02:759-783`, `R/fct_policy_sim_compare.R:944-966`) | **Medium** | Standardize provenance banners from immutable result metadata. |
| **UI-28** | Core Step 2 settings (baseline survey, periods) hidden inside flyout (`R/mod_2_01_weathersim.R:59-121`) | **Medium** | Surface core controls in sidebar; reserve flyout for advanced parameters. |
| **UI-29** | Steps 1/2 run buttons still silently no-op when prerequisites are absent (`mod_1_06:49`, `R/mod_2_01_weathersim.R:527-529`). ⚠️ **PARTIALLY DONE** (2026-08-31): Step 3's checks render via a persistent `alert-danger` output (`R/mod_3_06_policy_sim.R:105-140`) | **High** | Disable run buttons and add prerequisite explanations before click across all steps. |
| **UI-32** | Social protection targeting inputs (`mod_3_01_sp.R:197-216`) lack pre-run feedback on eligible population size | **Medium** | Dynamic text output with estimated eligible household count based on active cutoff. |
| **UI-36** | All 21 `plotOutput()` sites omit alt text (e.g. `R/mod_2_02_results.R:196, 242`; `R/mod_2_03_diagnostics.R:51, 87, 114`); three Leaflet maps lack text equivalents | **Medium** | Reactive `alt` text for plots; adjacent text/table summaries for maps. |
| **UI-41** | All charts are static ggplots; global widget conversion would serialize household-level values and can misrender complex geoms (21 sites; aggregated candidates at `R/mod_2_02_results.R:1246-1344`, `R/fct_policy_sim_compare.R:1017-1115`, `R/mod_3_09_decomposition.R:134-201`) | **Medium, confidentiality/rendering risk** | Keep static ggplot canonical. After UI-23, prototype interactivity only for aggregated charts; compare `ggiraph` vs `plotly` empirically (neither declared/locked). Restore `suspendWhenHidden = TRUE` for Step 3 chart widgets (`R/fct_policy_sim_compare.R:1031, 1102, 1115`) unless measured necessary. Household-level plots stay server-rendered with CSV/text equivalents. |

**Done (2026-08-31), compressed:** UI-30 (inverted future-period warning) · UI-31 (~650 dead lines deleted) · UI-33 (access-attainment relabel) · UI-34 (manufacturing+services >100% warning) · UI-35 (obsolete repo URLs; note the remote is now `worldbank/wise-app` after the rename) · UI-39 (nav link `aria-label` + `rel="noopener"`) · UI-40 (coefficient-uncertainty help corrected).

**Done (2026-09-01), compressed:** UI-01 (output errors visible as warning cards; all long renderers audited — `req()`-guarded or NULL-safe on the pre-run path) · UI-04 (Okabe-Ito palettes centralized in `utils_plot_theme.R`; SSP colours bluish green/blue/vermillion) · UI-38 (Step 2 scenario grid can never drop its last selection).

**Done (2026-09-02), compressed:** Step 1 survey/weather stats tables (incl. the binned-weather table) now show a single `Variable` column — the readable variable label, falling back to the raw name when no label exists — instead of a raw name column plus a separate `Variable Label` column.

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

**Done (2026-08-31), compressed:** RED-02 (golem utils trio deleted, NAMESPACE regenerated) · RED-03 (`dev/archived_fct/` deleted) · RED-04 (byte-identical `dev/` vs `batch/R/` scripts deduped) · RED-07 (`fct_h3_check.R` deleted) · DUP-02 (weaker `.normalise_ssp()` duplicate deleted).

**Done (2026-09-01), compressed:** DUP-01 (shadowed `resolve_band_q` duplicate deleted; `fct_sim_compare.R` authoritative, key-contract tests added) · RED-05 (never-executed `spec_curve.R` moved to `dev/archive/`).

---

## 9. Testing & Packaging (`TEST-*`)

| ID | Issue & Location | Actionable Fix |
|---|---|---|
| **TEST-03** | ⚠️ **PARTIALLY DONE**: `Suggests` complete (incl. `covr`, 2026-09-01); edition 3 set; renv.lock already had `testthat`/`spelling`/`covr`. | `covr` used in `README.Rmd` still unavailable locally for re-render. |
| **TEST-05** | ⚠️ **PARTIALLY DONE**: edition-3 suite fully passes; `R CMD build` succeeds; built-source `R CMD check` clean on every code-level section. | Remaining findings are environment-only on the check machine: PDF-manual ERROR/WARNING (no `pdflatex`), tidy HTML validator, clock skew. Re-verify on a machine with TeX. |

**Done, compressed:** TEST-02/TEST-04 (unused imports removed; CLAUDE.md inventory updated) · TEST-01 (direct failure-ledger tests for every pipeline stage, completing the direct-coverage list) · TEST-06 (prosperity_gap/avg_poverty delta-method gradients fixed; finite-difference validation for every smooth method) · TEST-07 (statistical contract: **raw survey weights**, asserted) · TEST-08 (runtime deps declared, Rd fixes, ASCII-fication, `R/globals.R`, `.Rbuildignore`) · TEST-09 (delta gradient corrected below \$28; point estimates unaffected, coefficient bands were).

---

## 10. Deployment, Connections & Security (`DEP-*` / `SEC-*`)

| ID | Issue & Location | Severity / Impact | Actionable Fix |
|---|---|---|---|
| **DEP-01** | ✅ Bundled `inst/duckdb_extensions/` binaries + fail-fast `.duck_load_ext()` (missing bundle → loud error; extension recorded only after a successful `LOAD`) (2026-09-01) | **Critical** | Remaining: verify a fresh-clone Connect deployment end to end. |
| **DEP-02** | ✅ Dead GCS-keyfile/HF-token controls removed (GCS exposes HMAC keys; HF documented public-repos-only); Azure help corrected to Account Key; blank S3/GCS inputs fall through to env credentials (2026-09-01) | **High** | Remaining: integration-test each supported auth contract against real backends. |
| **SEC-03** | Process-wide DuckDB connection, views, tokens, secrets have no session-end cleanup (`R/fct_load_data.R:21-43, 128-179`) | **Low on one-session Connect; Medium for repeated local `run_app()` sessions** | Disconnect and clear caches in `session$onSessionEnded`; store token hashes rather than plaintext; prefer temporary views. |

**Done, compressed:** DEP-03 (local connects require `dir.exists()`; badge distinguishes configured from metadata-verified) · SEC-01 (`.sql_literal()` on every dynamic credential, adversarial quote tests) · SEC-02 (weather temp tables registered in a cleanup ledger with `on.exit` drop on all paths) · SEC-04/SEC-05 (data-derived popups and DT labels escaped).

### 10.1 Git-backed Posit Connect deployment (config as of 2026-09-02; commits `0b73a4c`, `459d3f7`, `fdd3960`)

- Connect pulls straight from this GitHub repo (poll or "Update Now") instead of a push-button `rsconnect::deployApp()` bundle. This requires a committed root `manifest.json`; `app.R` runs via `pkgload::load_all()` because `wiseapp` is never installed into the Connect library.
- **Regenerating the manifest:** whenever dependencies or the shipped file set change, run in `dev/03_deploy.R`: `rsconnect::writeManifest(appDir = ".", appFiles = c("app.R", "R", "inst", "man", "DESCRIPTION", "NAMESPACE"), appPrimaryDoc = "app.R")`, then **re-strip `sf`** from `manifest.json$packages` before committing — `{leaflet}` hard-Imports sf, which the Connect host cannot build, but the app never loads it at runtime (only excluded `batch/` uses it). Verify with `grep -c '"sf":' manifest.json` → `0`, then commit the diff.
- **`brand.yml` must stay in `Imports`:** `app_ui.R` passes `bs_theme(brand = app_sys("app/_brand.yml"))` on every launch, and `bslib:::brand_resolve.character()` hard-checks the `brand.yml` package via `rlang::check_installed()` — a missing package aborts the app on Connect. The TEST-08 dependency audit (2026-09-01) demoted it to `Suggests`, which the static scan cannot see and would silently drop from a regenerated manifest; it was moved back to `Imports` on 2026-09-02 before regenerating the manifest for `collapse`.
- **DuckDB extensions:** `httpfs`, `h3`, `spatial` are committed under `inst/duckdb_extensions/` as gzipped **v1.5.5 binaries matching the Connect library's DuckDB version** (version drift breaks `INSTALL`); `.duck_load_ext()` prefers the `.gz` (DuckDB decompresses on INSTALL) and falls back to uncompressed. `renv.lock` is pinned to the matching DuckDB.
- Renew this manifest/binary pairing on every DuckDB or dependency upgrade; a stale binary version breaks extension `INSTALL` on Connect with a loud error (fail-fast since 2026-09-01), not silent breakage.
