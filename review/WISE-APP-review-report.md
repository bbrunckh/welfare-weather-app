# WISE-APP Comprehensive Review Report

**Scope:** Whole repository (`R/`, `batch/`, `dev/`, `tests/`, `inst/`, docs), plus live Chrome desktop verification
**Date:** 2026-08-28 · **Reviewed Repo State:** `main` @ `cf1cb48` · **Current Remediation Base:** `golem` @ `489372c`
**Target Context:** Wider release beyond core team; primary performance bottleneck is Step 2 simulation.
**Deployment Model:** Posit Connect (git-backed, 1 session/process, Databricks backend, auto-connect — see §10.1) & Local R package (single user, custom data connection). Synchronous execution, desktop browser.

> **Remediation status (through 2026-08-31, plus commits of 2026-09-01):** Five fix waves applied. Wave 0 determinism (DET-01..09) is complete, plus two low-risk batches (dead code, dedup, perf hoists, REACT-10, UI-29..31/33..35/39/40, DUP-02, RED-02/03/04/07, SEC-01/04/05, TEST-02/04) and scoped parts of REACT-01/13, TEST-03/05, RED-09. A 2026-09-01 blocker batch fixed DEP-01, INT-02/INT-09, DEP-02/DEP-03 (incl. the REACT-01 remainder), REACT-05, SEC-02, and the TEST-06/TEST-09 gradient bugs. A 2026-09-01 test batch decided the TEST-07 statistical contract (raw weights) and completed TEST-08. Verified with `devtools::document()`, `pkgload::load_all()`, and the full edition-3 suite: **all 437 tests pass**; built-source `R CMD check` is clean at the code level (remaining findings are environment-only — no `pdflatex`). `R CMD build` succeeds. Completed rows are struck through and compressed below; open rows keep full detail. Deployment commits `0b73a4c`/`459d3f7`/`fdd3960` — see §10.1.

---

## 1. Executive Summary

The codebase has a sound modular architecture (Golem structure, pluggable engine registry in `fct_fit_model.R`, Bootstrap 5 / bslib UI). Three gaps still block wide release:

1. **Result Integrity & State Synchronization:** Upstream changes trigger `renderUI` rebuilds that wipe selections, and completed downstream results remain presented after their inputs change (provenance-signature work INT-08 outstanding; connection mispairing, Step 3 non-atomic publication, and silent reinterpreting of results are fixed). (Reproducibility ✅ Wave 0 complete.)
2. **Release Readiness & UX:** Export is limited to two threshold-table CSV buttons, configuration cannot be saved/restored, long computations lack double-click guards, and uncaught errors are hidden by CSS.
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

All fixed with isolated `withr::with_seed` streams, deterministic ordering, and regression coverage in `tests/testthat/test-determinism.R` (repeatability, seed sensitivity, exact caller RNG restoration); weather end-to-end tests in `test-fct_get_weather.R`.

- **DET-01** Policy assignment: `apply_policy_to_svy()` seeds every stochastic lever via `wise_seed(seed, "policy")` (`R/fct_policy_sim.R`); all Module 3 and batch call sites pass the fixed seed.
- **DET-02** Residuals: `draw_residuals_vec()` seeds `normal`/`resample` draws; unmatched IDs use hash-indexed fallback; run-time residual mode snapshotted into Step 2 results (`R/fct_run_simulation.R`, `R/fct_aggregation.R`).
- **DET-03** Global `set.seed(123)` in `.compute_breaks` replaced with `withr::with_seed()` (`R/fct_get_weather.R`).
- **DET-04** RNG-consuming DuckDB temp-table names replaced with `tempfile()` (3 sites, `R/fct_get_weather.R`); verified `tempfile()` does not advance the RNG.
- **DET-05** Dormant `ranger`/`xgboost` engines seeded (`123L`, one worker) and dependencies declared; choices stay hidden from `model_type_choices()`.
- **DET-06** App/batch Lasso aligned: seed `123L`, explicit folds, tolerance `1e-4`, isolated per-imputation streams (`R/fct_fit_model.R`; `LASSO_PARALLEL_SEED <- 123L` in all batch configs).
- **DET-07** DuckDB weather single-thread invariant retained with `on.exit` restoration; regression-tested.
- **DET-08** `collect_deterministic()` added (`R/fct_load_data.R`); all non-weather collection paths, H3/map/panel sites, and `loc_panel()` ID normalization use canonical ordering/tie-breakers.
- **DET-09** OneDrive-only script deleted; replaced by committed package-level Step 1/2/3, RNG, engine, ordering, and weather determinism tests.

---

## 4. Result Integrity & State Synchronization (`INT-*`)

| ID | Issue & Location | Impact | Actionable Fix |
|---|---|---|---|
| **INT-01** | Dynamic inputs are rebuilt with defaults (`R/mod_1_06_model.R:277-478`, `R/mod_1_04_weather.R:43-241`, `R/mod_1_01_sample.R:98-120`, `R/mod_2_01_weathersim.R:310-323`, `R/fct_policy_sim_compare.R:968-1015`) | **High** (Changing model type, interactions, fixed effects, weather variables, economies, baseline data, or re-running Step 3 wipes adjacent selections and filters) | Snapshot compatible current values with `isolate(input$...)` and restore them as `selected`; explicitly reset only values no longer valid. |
| **INT-02** | ~~`applied_connection` published before metadata loads; file errors swallowed; stale metadata retained across re-connects~~ ✅ **DONE** (2026-09-01) | **High** | Manual connect now resets all metadata + `applied_connection` up front, loads each metadata file into a failure ledger, and publishes the connection + verified status only when all loads succeed; failures render in a persistent status card. Auto-connect uses the same status channel. |
| **INT-03** | ~~Step 2/3 aggregators preferred the live residual radio over the run-time setting~~ ✅ **DONE** (2026-08-31) | **High** | Results now prefer the run-time residual snapshot stored in Step 2 (`R/fct_run_simulation.R:341-350, 400-408`; consumers in `mod_2_02_results.R`, `fct_policy_sim_compare.R`), with live reactive retained only for older objects. |
| **INT-04** | `R/fct_policy_sim_compare.R:548-602` wraps scenario aggregations in `tryCatch(..., error = function(e) NULL)` | **Medium** (Failed scenarios silently vanish from results charts and threshold tables) | Collect scenario aggregation errors; display warning toast identifying dropped scenarios. |
| **INT-05** | Results use live labels/settings after fitting or simulation (`R/mod_1_07_results.R:108-185`, `R/mod_1_08_modelfit.R:73-94`, `R/fct_policy_sim_compare.R:428-431`); engine-conditional headings created only on first fit | **Medium** (Old results can be relabeled with new variables or historical years, while headings describe the first engine) | Snapshot labels, historical range, and engine specification into result objects; bind renderers and headings strictly to those snapshots. |
| **INT-06** | Survey reload and inner map/panel failures (`R/mod_1_02_surveystats.R:139-147, 181-223`) update microdata but can retain old `map_data()` / `cell_data()` | **Medium** (Map can show previous geography; failed panel join silently removes `loc_id_panel` and changes the VCV fallback) | Clear map/cell state at load start and on every failure; notify when `loc_id_panel` is unavailable because it changes inference. |
| **INT-07** | "Clear simulation results" (`R/mod_2_simulation.R:132-139`) nulls reactive data but does not call `removeTab()` | **Medium** (Results/Diagnostics tabs remain rendered with blank/error panes; empty state never returns) | Call `removeTab()` for each appended tab and reset `*_tab_added` reactive flags to `FALSE`. |
| **INT-08** | No provenance signature or stale-state invalidation links Step 1 fits, Step 2 simulations, and Step 3 policy results (`R/mod_1_07_results.R:87-88`, `R/mod_2_01_weathersim.R:603-604`, `R/mod_3_06_policy_sim.R:122-130`) | **High** (A refit or upstream re-run leaves completed downstream tabs presenting results from superseded data/model/weather inputs as current) | Store an immutable run signature in each result; on upstream changes clear dependent results or mark them stale and disable interpretation/export until re-run. |
| **INT-09** | ~~Step 3 mutates survey diagnostic state before simulation succeeds; prior outputs preserved on failure~~ ✅ **DONE** (2026-09-01) | **High** | `run()` computes everything in locals and publishes all reactive values + increments `sim_run_id` only after the complete run (simulation + decomposition) succeeds; a failure anywhere leaves the previous results, diagnostics, and run ID intact. |

---

## 5. Backend Performance & Safe Speedups (`PERF-*`)

### 5.1 Open Bottlenecks (Wave 2 — output-neutral unless noted)

| ID | Bottleneck & Location | Expected Win | Actionable Fix |
|---|---|---|---|
| **PERF-23** | `R/mod_1_02_surveystats.R:153-213` & `R/fct_loc_panel.R:96-158` execute ~6 separate remote DuckDB parquet scans over `h3_df` | **Medium-Large** | Materialize remote table once into a local DuckDB temporary table via `dplyr::compute(name = ..., temporary = TRUE)`. |
| **PERF-31** | Step 3 re-aggregates baseline and every scenario/member whenever aggregation method or deviation label changes (`R/fct_policy_sim_compare.R:495-622`) | **Medium-Large** | Add a per-method workspace cache keyed only by value-affecting inputs; move `cmp_deviation`-dependent label assembly downstream because deviation is applied after aggregation. |
| **PERF-36** | Map pipeline parses every geometry into nested R lists solely for bounds, retains both parsed and raw GeoJSON, and continuous colours can create nearly one `addGeoJSON()` layer per distinct value (`R/mod_1_02_surveystats.R:153-179`, `R/fct_surveystats.R:573-602, 859-871`, `R/fct_outcome.R:432-453`, `R/fct_weatherstats.R:1156-1221`) | **Medium-Large for many polygons** | Compute bounds beside geometry in DuckDB, keep only raw JSON, emit one per-feature-style GeoJSON layer, enable Canvas on the survey map, and cache cell features by data/wave signature. These Leaflet edits are output-neutral; an optional `deckglgeoarrow` migration (CRAN v0.0.2, needs `mapgl` host + `geoarrow`/`nanoarrow`) is a gated prototype — see §5.3. |
| **PERF-13** | `R/fct_get_weather.R:439-444, 644-713` re-fetches identical ERA5/CMIP6 parquet files; CMIP6 history opened twice | **Medium** | Bounded disk cache keyed by source/version, codes, variables, date range, and transformation; reuse loaded historical relation for the resolution probe. Do not use an unversioned key. |
| **PERF-30** | ~~`R/mod_2_02_results.R:310-376, 500-506` includes display-only `band_q` in `agg_workspace()`, so changing the coefficient-band dropdown destroys the full aggregation cache~~ ✅ **DONE** (2026-09-01): `band_q` removed from the workspace (it only touches `value_lo/hi`, which no builder consumes — the band is re-derived from cached SDs at render time); poverty-line and bandwidth are folded into per-method cache keys only for methods that consume them (`headcount_ratio`/`gap`/`fgt2`; bandwidth: `headcount_ratio`), so moving those controls no longer destroys unrelated cached methods. Regression-tested via `test-mod_2_02_results.R` (Shiny testServer harness — first coverage for this module, part of REACT-13's gap). | Complete. |
| **PERF-22** | `R/fct_policy_decompose.R:449, 119` recomputes policy deltas and `stats::ecdf()` per scenario-year | **Medium** | Precompute weather-independent `.compute_policy_deltas()` and empirical CDF once per scenario; pass down to year loop. |
| **PERF-23b** | ~~`R/fct_load_data.R:365-437` executes `CREATE OR REPLACE SECRET` on every `load_data()` call~~ ✅ **DONE** (2026-09-01): `.register_cached_secret()` applies the Databricks hash-cached check to S3, GCS, and Azure (all three auth branches); SQL runs only when the resolved credential tuple changes, and the cache resets with the connection. | Complete. |
| **PERF-16** | Existing `dev_mode` limits the ensemble to one model (`R/mod_2_01_weathersim.R:212-219, 576`) but leaves coefficient uncertainty enabled and is exposed as a development control | **Medium, explicitly output-changing** | Replace with a clearly labeled Preview mode (one member + point estimates, reported in provenance, never final output). Not an output-neutral speedup. |
| **PERF-32** | ~~`run_sim_pipeline()` retains a full prediction frame while allocating the $N \times K$ design/factor matrices (`R/fct_predict_outcomes.R:162-163`, `R/fct_simulations.R:572-710`)~~ ✅ **DONE** (2026-09-01): `out` (verified a full deep copy of the joined prediction frame, ~1 byte per value) is released immediately after the RIF `F_loading` attribute is captured / required vectors are extracted, before `X_nonFE` and `F_loading` are built; `X_nonFE` is dropped as soon as `F_loading` exists and `survey_wd_sim` right after the block (train_aug never reads it). A/B measurement (`gc(reset=TRUE)` peak, 300k rows): the process peak is now set by the join phase rather than the matrix phase — at production N and column counts the saving is one full prediction frame. Output byte-identical (determinism suite passes). | Complete. |
| **PERF-10** | ~~`R/fct_surveystats.R:859-869, 918-929` parses GeoJSON to R lists and retains raw strings~~ ✅ **DONE** (2026-09-01): parsed `geometry` copies dropped from all three feature builders (`build_cell_features`, density map, `mod_1_02` survey features); `.geojson_bounds()` now parses `geom_json` transiently when no parsed copy exists, so bounds still work with string-only features. | Complete. (DuckDB-side extents remain available under PERF-36.) |
| **PERF-25** | ~~`R/fct_weatherstats.R:740-749` re-computes `interaction()` and `split()` per weather variable~~ ✅ **DONE** (2026-09-01): `.summarise_loc_prep()` builds the grouping once per survey frame and the Step 1 weather map passes `prep =` through `summarise_weather_by_loc()` per variable; output-equivalence verified with and without prep. | Complete. |
| **PERF-08** | ~~`R/fct_model_select.R:230-240` scans the full frame once per wave to count non-missing values~~ ✅ **DONE** (2026-09-01): single grouped pass via C-level `rowsum()` with first-appearance group order preserved; zero-row edge replicated exactly. Measured ~3x on 200k rows x 72 groups (output-equivalence test vs the old algorithm). | Complete. |
| **PERF-34** | Default residual aggregation rebuilds the same ID-to-residual named vector and character keys per year, member, method, and weighting (`R/fct_aggregation.R:215-221`) | **Small-Medium** | Cache the deterministic matched-ID lookup and residual variance with each pipeline; leave unmatched-ID sampling in the call path so RNG consumption is not shifted. |

**Done (2026-08-31), compressed:** PERF-03 (KDE/bandwidth hoisted out of the 9-tau loop, `fct_fit_model.R:234-243` + optional `dens` param in `compute_rif`); PERF-06 (dead `.feature_centroid()` deleted; `.geojson_bounds()` running min/max still open); PERF-07 (duplicate categorical diagnostics append deleted, `fct_sim_diag.R:140-165`); PERF-17 (`do.call(rbind, 1-row dfs)` → `dplyr::bind_rows`, 3 sites); PERF-19 (`resimulate_with_svy()` precomputes `train_aug`/prepared svy once — dormant-path win only, original "Large" estimate stale); PERF-20 (append `.svy_row_id` once before the key loop — `$<-` shallow-copies, not two full copies as first reported); PERF-21 (hoisted per-year `format()` in Step 3); PERF-24 (hoisted per-variable weather filters in density panel); PERF-27 (per-key training `ecdf()` threaded through `precomputed_ecdf_train`); PERF-28 (`installed.packages()` → `requireNamespace()`); PERF-29 (dead duplicate `bins_df` build removed); PERF-37 (stale custom-encoder comments removed).

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
2. Apply the output-neutral fixes in PERF-10/PERF-36 and repeat the benchmark.
3. Prototype `deckglgeoarrow` only if optimized Leaflet misses agreed targets: identical classification/legend values, tooltips, map view, Shiny proxy updates, no runtime CDN, fresh-clone Connect deploy, non-WebGL fallback. Keep geometry server-mediated; never expose storage credentials/tokens or direct browser URLs to restricted GeoParquet.
4. Provisional targets at 10k polygons: exactly one data layer, server widget build <=300 ms, time-to-interactive <=1.5 s, sustained pan/zoom >=45 FPS. Pursue WebGL/GeoArrow only if optimized Leaflet fails the maximum-production or 50k-polygon probe.

---

## 6. Reactivity & Pipeline Correctness (`REACT-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **REACT-01** | ~~`tryCatch` handler `return()` bug~~ ✅ fixed; ~~local paths accepted without existence check~~ ✅ closed by DEP-03 (2026-09-01) | **Medium** | Complete. |
| **REACT-02** | No busy guards on loads, fit, or simulation actions (`R/mod_1_02_surveystats.R:101`, `R/mod_1_05_weatherstats.R:88`, `R/mod_1_07_results.R:63-92`, `R/mod_1_06_model.R:702-794`, `R/mod_2_01_weathersim.R:527-631`, `R/mod_3_scenario.R:307-309`) | **High** | Add observer-side `running <- reactiveVal(FALSE)` guards with `on.exit(running(FALSE))`; disable the triggering control while running. |
| **REACT-03** | `R/mod_1_02_surveystats.R:101-224` & `R/mod_1_05_weatherstats.R:88-163` re-execute full I/O on repeated clicks | **Medium** | Cache hash signature (`digest::digest()`) of inputs; short-circuit identical load requests. |
| **REACT-04** | Each Step 2 run regenerates the full configured scenario set (`R/fct_run_simulation.R:394-408`; `R/mod_2_01_weathersim.R:603-604`) | **Info** (No result loss; merging would retain scenarios the user removed) | Keep replacement semantics; preserve compatible scenario-filter selections separately if desired. |
| **REACT-05** | ~~`sim_run_id` incremented and success shown after policy simulation/decomposition failure~~ ✅ **DONE** (2026-09-01) | **High** | Decomposition failures now fail the run (persistent `alert-danger` output + error toast). Per-scenario-year decomposition failures: all-failing fails the run; partial failures publish results with a warning toast naming the dropped count. |
| **REACT-06** | `R/mod_2_02_results.R:871-1244` repeats `one_scenario()` band-assembly scaffolding across five reactives | **Low** | Parameterize only if it improves testability; do not re-extract `.apply_contrast_sd()` or `by_model_matrix()`. |
| **REACT-07** | ⚠️ **PARTIALLY DONE** (2026-09-01, via PERF-30): display-only dependencies removed and cached values keyed by the method-specific inputs they consume; eager warming retained only for workspace rebuilds (hist_sim/scenarios/residuals/skip). | Remaining: nothing further identified — close. |
| **REACT-08** | Term extraction duplicated across `mod_3_02..mod_3_06` (`R/mod_3_02_infra.R:49-89` et al.) | **Medium** | Extract shared helper into `utils_mod_1_helpers.R`. |
| **REACT-09** | `R/mod_3_scenario.R:307-309` calls child module's exported `run()` closure directly | **Low** | Pass reactive trigger parameter into child module server. |
| **REACT-11** | Parallel aggregation engines in `R/mod_2_02_results.R:287-517` vs `R/fct_policy_sim_compare.R:412-527` | **Low** | Consolidate Step 2 and Step 3 aggregation into unified `fct_aggregation.R` engine. |
| **REACT-12** | Per-key Step 2 errors become `NULL` (`R/fct_run_simulation.R:288-338`); no failure ledger; `R/mod_2_01_weathersim.R:609-629` announces unconditional completion | **High** (Historical failure can yield no baseline; future-member failures silently narrow or remove ensemble groups) | Collect key/error pairs; fail the run if historical or all members of a requested group fail, otherwise publish a prominent partial-result warning and provenance counts. |
| **REACT-13** | ~~Scenario filters not applied to the Step 2 hero point-range reactive~~ ✅ fixed (`R/mod_2_02_results.R:970`, matching siblings). ⚠️ **PARTIALLY DONE** | **High** | A reactive module regression test covering all output families remains open; no test harness exists for `mod_2_02_results`. |
| **REACT-14** | Model fitting can silently fall back from logistic to linear or clustered to unclustered VCV (`R/fct_fit_model.R:789-798, 926-933`) while the observer reports only generic success (`R/mod_1_07_results.R:73-90`) | **High** (Fitted estimand/inference can differ from the user's selection without persistent disclosure) | Capture structured warnings/fallback metadata in `model_fit`, show in the settings/provenance banner, require explicit acknowledgement for model-family fallback. |

---

## 7. UI, UX & Accessibility Findings (`UI-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **UI-01** | `custom.css:120-129` sets `.shiny-output-error { visibility: hidden; }` while zero `validate(need())` calls exist | **High** | Remove blanket CSS hide; add explicit `validate(need(...))` to long renderers; style error states as visible warning cards. |
| **UI-02** | Config flyouts (`mod_1_04:87-239`, `mod_1_06:28-48`, `mod_2_01:43-57`) share fixed `left: 400px; top: 90px`, can remain open across accordion switches, lack focus/Escape handling | **High** | Anchor one flyout to its sidebar, enforce one-open state, synchronize `aria-expanded`, move focus on open/close, close on Escape. |
| **UI-03** | Visual headings/`tags$label` not associated with controls whose Shiny `label = NULL` (`R/mod_2_01_weathersim.R:69-157, 310-323`, `mod_2_02:813-821`, `mod_3_01_sp:117-194, 325-345`, Step 3 helpers incl. `mod_3_04_labor:126-206`) | **High** | Use each input's `label`, or `<label for=...>` / `aria-labelledby`; preserve visible grouped headings. |
| **UI-04** | Colorblind-unsafe palettes (`fct_simulations.R:23-25` green/red SSP, `fct_outcome.R:418-421` RYG, `fct_results.R` Set1) | **High** | Standardize on Okabe-Ito / Viridis / Mako centralized in `utils_plot_theme.R`. |
| **UI-10** | Collapsing the Lasso forced-covariates panel (`R/mod_1_06_model.R:456-474, 511-617`) removes inputs; live `selected_model()` changes the apparent model contract while `model_fit` stays unchanged | **High** | Keep inputs registered via CSS visibility, or snapshot forced-in/out + advanced settings at fit time; all downstream consumers use the snapshot. |
| **UI-23** | Export incomplete: only Step 2/3 threshold DTs expose CSV (`R/mod_2_02_results.R:1276-1284`, `R/fct_policy_sim_compare.R:1047-1055`); other tables, plots, configuration, results bundle have no export | **High** | Phase 1: CSV for aggregated plot/table data, server-rendered PNG from the same ggplot builder (fixed dims/DPI), config JSON, provenance sidecar. Prefer base PNG or declare `ragg` + lock entry. No widget screenshots; no raw household-row export without a disclosure policy. |
| **UI-24** | Configurations cannot be saved/shared/restored (`enableBookmarking` inactive) | **High** | JSON export/import of complete analysis configuration state (inputs, model specs, random seed). |
| **UI-25** | Steps 1-3 openable in any order without prerequisite indicators | **Med-High** | Completion badges (✓) in nav header; "Prerequisites: ..." notices in overview empty states. |
| **UI-26** | Summaries/headers omit source identity, exact model specification, seed, run signature (`mod_1_04:292-338`, `mod_1_06:107-147`, `R/mod_2_01_weathersim.R:340-393`, `mod_2_02:759-783`, `R/fct_policy_sim_compare.R:944-966`); Step 2 hardcodes "All models" even in dev mode (`:363`) | **Medium** | Standardize provenance banners from immutable result metadata; show ensemble count/dev mode accurately. |
| **UI-28** | Core Step 2 settings (baseline survey, periods) hidden inside flyout (`R/mod_2_01_weathersim.R:59-121`) | **Medium** | Surface core controls in sidebar; reserve flyout for advanced parameters. |
| **UI-29** | Step 1/2 run buttons still silently no-op when prerequisites are absent (`mod_1_06:49`, `R/mod_2_01_weathersim.R:527-529`). ⚠️ **PARTIALLY DONE** (2026-08-31): Step 3's checks now render via a persistent `alert-danger` output (`R/mod_3_06_policy_sim.R:105-140`) | **High** | Disable run buttons and add prerequisite explanations before click across all steps. |
| **UI-32** | Social protection targeting inputs (`mod_3_01_sp.R:197-216`) lack pre-run feedback on eligible population size | **Medium** | Dynamic text output with estimated eligible household count based on active cutoff. |
| **UI-36** | All 21 `plotOutput()` sites omit alt text (e.g. `R/mod_2_02_results.R:196, 242`; `R/mod_2_03_diagnostics.R:51, 87, 114`); three Leaflet maps lack text equivalents | **Medium** | Reactive `alt` text for plots; adjacent text/table summaries for maps. |
| **UI-38** | Unchecking every Step 2 scenario checkbox silently re-adds the first scenario (`R/mod_2_02_results.R:667-681`) | **Medium** | Prevent clearing the final choice or render a true no-scenario empty state. |
| **UI-41** | All charts are static ggplots; global widget conversion would serialize household-level values and can misrender complex geoms (21 sites; aggregated candidates at `R/mod_2_02_results.R:1246-1344`, `R/fct_policy_sim_compare.R:1017-1115`, `R/mod_3_09_decomposition.R:134-201`) | **Medium, confidentiality/rendering risk** | Keep static ggplot canonical. After UI-23, prototype interactivity only for aggregated charts; compare `ggiraph` vs `plotly` empirically (neither declared/locked). Restore `suspendWhenHidden = TRUE` for Step 3 chart widgets (`R/fct_policy_sim_compare.R:1031, 1102, 1115`) unless measured necessary. Household-level plots stay server-rendered with CSV/text equivalents. |

**Done (2026-08-31), compressed:** UI-30 (inverted future-period warning UI); UI-31 (deleted ~650 dead lines: `pol_*` trio, `.plot_beta_curves`, plus stale Rd/NAMESPACE entries); UI-33 (relabeled "Share of unserved gaining access/attainment (%)" with help lines); UI-34 (visible warning when manufacturing+services >100% and agriculture is clamped); UI-35 (updated obsolete repo URLs in DESCRIPTION/README/CLAUDE.md — note: actual `git remote -v` still points at `welfare-weather-app`; confirm the intended canonical remote before relying on the links); UI-39 (nav links: `aria-label` + `rel="noopener noreferrer"`); UI-40 (corrected coefficient-uncertainty help to state delta-method-only, no Monte Carlo fallback — TEST-06's gradient bug itself unfixed).

### 7.1 Recommended Output Delivery Sequence

1. **Exports first:** CSV of the tidy aggregated frame behind each result plot + server-rendered PNG from the same ggplot object. Include source/model/seed/run signature in filenames or a sidecar; raw household-row export requires a disclosure policy and is out of scope for the default UI.
2. **Selective interactivity second:** only aggregated decision charts; preserve the static ggplot path for publication PNG and as fallback.
3. **No global conversion:** household-level distributions, residual diagnostics, binscatters, and policy histograms remain server-rendered; Leaflet maps are already interactive.

---

## 8. Redundancy & Dead Code (`RED-*` & `DUP-*`)

- **RED-01** `inst/mathjax/` no longer exists; the prior ~100 MB deletion recommendation is stale. No action.
- **RED-05** `batch/R/spec_curve.R` (608 lines) auto-sourced by all batch scripts but never executed → move to `dev/archive/`.
- **RED-06** Six near-identical simulation scripts (`batch/04_run_sim_*.R`, ~11,500 lines) → single canonical script parameterized via `Sys.getenv("WISEAPP_COUNTRY")` + YAML config.
- **RED-08** Committed one-off scratch scripts in `dev/` and `batch/` → move historical scripts to `dev/archive/`.
- **RED-09** ⚠️ **PARTIALLY DONE**: `.Rbuildignore` anchoring fixes applied (`^dev_history\.R$`, `^run_dev\.R$`) and the file is now **tracked in git** (commit `5550efc` added `!.Rbuildignore` to `.gitignore`). README re-render still open.
- **DUP-01** ~~`resolve_band_q()` defined twice with divergent `minmax` semantics (`R/fct_aggregation.R:157` winsorizes to 0.001/0.999; later-sourced `R/fct_sim_compare.R:76` overwrites with 0/1)~~ ✅ **DONE** (2026-09-01): the shadowed duplicate in `fct_aggregation.R` deleted; `fct_sim_compare.R:76` is the single authoritative definition (its `minmax = 0/1` semantics are what every caller already saw at runtime, so the change is output-neutral). Key-contract regression tests added in `test-mod_2_02_results.R`.
- **DUP-03** `fct_policy_sim_compare.R` mirrors plotting/aggregation internals of `fct_sim_compare.R` → extract shared series assembly, threshold tables, exceedance rendering.

**Done (2026-08-31), compressed:** RED-02 (deleted `golem_utils_ui.R`/`golem_utils_server.R`, regenerated NAMESPACE); RED-03 (deleted `dev/archived_fct/`); RED-04 (byte-identical `dev/` vs `batch/R/` scripts repointed and deduped); RED-07 (deleted `fct_h3_check.R` + export + Rd); DUP-02 (deleted weaker `.normalise_ssp()` duplicate; load-order correction noted — the robust implementation was already winning).

---

## 9. Testing & Packaging (`TEST-*`)

| ID | Issue & Location | Actionable Fix |
|---|---|---|
| **TEST-01** | ⚠️ **PARTIALLY DONE**: `test-determinism.R` directly covers `fit_model()`, `run_sim_pipeline()`, `apply_policy_to_svy()`, `apply_policy_delta_to_baseline()`. | Direct `fct_run_simulation()` partial-key failure-ledger and provenance-signature tests remain open under REACT-12/INT-08. |
| **TEST-03** | ⚠️ **PARTIALLY DONE**: `Suggests` added; edition 3 set. | `covr` declared in `Suggests` (2026-09-01); the renv.lock claim was stale — `testthat`/`spelling`/`covr` were already locked. `covr` used in `README.Rmd` still unavailable locally for re-render. |
| **TEST-05** | ⚠️ **PARTIALLY DONE**: edition-3 suite fully passes (437/437, 2026-09-01); `R CMD build` succeeds; built-source `R CMD check` is clean on every code-level section (dependencies, R-code problems, Rd usage, non-ASCII, test deps). | Remaining findings are environment-only on the check machine: PDF-manual ERROR/WARNING (no `pdflatex` installed), tidy HTML validator, clock skew. Re-verify on a machine with TeX. |
| **TEST-06** | ~~Four failures: stale tests/method docs described the old "average welfare among the poor" functional while the code computes `mean(1 / welfare)` with a dropped $1/\mu_i$ chain-rule factor~~ ✅ **DONE** (2026-09-01): derivative fixed to $-1/(n_{ok} \mu_i)$ (weighted $-w_i/(W_{ok} \mu_i)$); stale tests rewritten and `.claude/method_uncertainty.md` §3.6/§3.7 corrected for the `prosperity_gap`/`avg_poverty` metrics; finite-difference validation added for every smooth method (weighted + unweighted; median excluded as piecewise-constant, headcount validated on its kernel-smoothed surrogate) | Complete. |
| **TEST-07** | ~~`tests/testthat/test-fct_weatherstats.R:73-89` expects per-wave weight normalization; `R/fct_weatherstats.R:37-42` explicitly disables it and returns raw weights~~ ✅ **DONE** (2026-09-01): statistical contract decided — **raw weights are the contract**. Stale test replaced with a raw-weight preservation assertion; `merge_survey_weather()` roxygen rewritten (the "OUTDATED" marker and normalisation claim removed) and the dead commented-out normalise line deleted. | Complete. Multi-wave weight semantics documented in the function docs. |
| **TEST-08** | ~~Built-package check: undeclared runtime use (`config`, `Hmisc`, `htmlwidgets`, `jsonlite`, `matrixStats`), non-exported `fixest::vcov` calls (`R/fct_results.R:83-91`), unused declared imports, undocumented Rd arguments, non-ASCII R files, visible-binding notes~~ ✅ **DONE** (2026-09-01): runtime deps declared (arrow/brand.yml/pkgload moved to Suggests, bit64 added), `stats::vcov` used, all 16 Rd usage mismatches fixed, 24 non-ASCII R sources ASCII-fied (UI glyphs preserved via `\uXXXX` escapes), visible bindings resolved via `R/globals.R` (`@importFrom` + `globalVariables`), hidden/top-level files excluded via `.Rbuildignore`. | Complete. |
| **TEST-09** | ~~`prosperity_gap` delta gradient wrong in both branches: dropped the $1/\mu_i$ factor and the unweighted branch divided by $N$ twice~~ ✅ **DONE** (2026-09-01): gradient is now $-w_i/W \cdot 28/\mu_i$ below \$28 (unweighted $-28/(N\mu_i)$), zero above the threshold and for non-positive $\mu_i$; exact-formula + weighted/unweighted finite-difference tests added (`tests/testthat/test-fct_aggregation_delta.R`) | Complete. Point estimates were unaffected; coefficient bands were. |

**Done (2026-08-31), compressed:** TEST-02 (removed unused imports `vip`/`relaimpo`/`markdown` + dead `@importFrom vip` tag); TEST-04 (CLAUDE.md inventory updated).

---

## 10. Deployment, Connections & Security (`DEP-*` / `SEC-*`)

| ID | Issue & Location | Severity / Impact | Actionable Fix |
|---|---|---|---|
| **DEP-01** | ~~`inst/duckdb_extensions/` untracked while manifest.json referenced files there~~ ✅ *binaries bundled and tracked (§10.1)*; ~~extensions marked loaded without a successful `LOAD`; missing bundles silently skipped~~ ✅ **DONE** (2026-09-01): `.duck_load_ext()` fails fast when a required bundle is absent on Connect and records the extension only after a successful `LOAD` | **Critical** | Remaining: verify a fresh-clone Connect deployment end to end. |
| **DEP-02** | ~~Blank S3 inputs shadowed env credentials; GCS `keyfile` and HF `token` never consumed; Azure UI advertised SAS while backend builds an `AccountKey` string~~ ✅ **DONE** (2026-09-01): removed the dead GCS-keyfile/HF-token controls (GCS now exposes HMAC keys matching the backend; HF is documented public-repos-only); Azure help text corrected to Account Key; S3/GCS blank inputs fall through to env credentials via empty-aware `%|||%` | **High** | Remaining: integration-test each supported auth contract against real backends. |
| **DEP-03** | ~~Local validation accepted nonexistent paths; green badge validated fields, not reachability~~ ✅ **DONE** (2026-09-01): local connects require `dir.exists()`; the status badge distinguishes "configured" from "metadata-verified", and verified status is granted only after metadata actually loads | **Medium** | Complete. |
| **SEC-02** | ~~Weather temp tables cleaned only on the happy path; lazy queries could outlive dropped tables~~ ✅ **DONE** (2026-09-01): every materialised table in `get_weather()` is registered in a cleanup ledger the moment it is created, with one best-effort `on.exit` drop covering happy and error paths; early per-SSP releases are removed from the ledger, and `on.exit` runs only after all relations are collected | **Medium** | Complete. |
| **SEC-03** | Process-wide DuckDB connection, views, tokens, secrets have no session-end cleanup (`R/fct_load_data.R:21-43, 128-179`) | **Low on one-session Connect; Medium for repeated local `run_app()` sessions** | Disconnect and clear caches in `session$onSessionEnded`; store token hashes rather than plaintext; prefer temporary views. |

**Done (2026-08-31), compressed:** SEC-01 (added `.sql_literal()` and applied to every dynamic credential across Databricks/S3/GCS/Azure with adversarial quote tests — unblocks PERF-11); SEC-04 (data-derived Leaflet popup interpolations wrapped in `htmltools::htmlEscape()`); SEC-05 (`make_stats_dt()` escapes data-derived labels before joining with the app-generated `<br>` markup).

### 10.1 Git-backed Posit Connect deployment (config as of 2026-09-01; commits `0b73a4c`, `459d3f7`, `fdd3960`)

- Connect pulls straight from this GitHub repo (poll or "Update Now") instead of a push-button `rsconnect::deployApp()` bundle. This requires a committed root `manifest.json`; `app.R` runs via `pkgload::load_all()` because `wiseapp` is never installed into the Connect library.
- **Regenerating the manifest:** whenever dependencies or the shipped file set change, run in `dev/03_deploy.R`: `rsconnect::writeManifest(appDir = ".", appFiles = c("app.R", "R", "inst", "man", "DESCRIPTION", "NAMESPACE"), appPrimaryDoc = "app.R")`, then **re-strip `sf`** from `manifest.json$packages` before committing — `{leaflet}` hard-Imports sf, which the Connect host cannot build, but the app never loads it at runtime (only excluded `batch/` uses it). Verify with `grep -c '"sf":' manifest.json` → `0`, then commit the diff.
- **DuckDB extensions:** `httpfs`, `h3`, `spatial` are committed under `inst/duckdb_extensions/` as gzipped **v1.5.5 binaries matching the Connect library's DuckDB version** (version drift breaks `INSTALL`); `.duck_load_ext()` prefers the `.gz` (DuckDB decompresses on INSTALL) and falls back to uncompressed. `renv.lock` is pinned to the matching DuckDB.
- Renew this manifest/binary pairing on every DuckDB or dependency upgrade; a stale binary version breaks extension `INSTALL` on Connect with a loud error (fail-fast since 2026-09-01), not silent breakage.
