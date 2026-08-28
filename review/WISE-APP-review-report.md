# WISE-APP Comprehensive Review Report

**Scope:** Whole repository (`R/`, `batch/`, `dev/`, `tests/`, `inst/`, docs), plus live Chrome desktop verification  
**Date:** 2026-08-28 · **Repo State:** `main` @ `cf1cb48`  
**Target Context:** Wider release beyond core team; primary performance bottleneck is Step 2 simulation.  
**Deployment Model:** Posit Connect (1 session/process, Databricks backend, auto-connect) & Local R package (single user, custom data connection). Synchronous execution.
**Form Factor:** Desktop browser; mobile use and mobile-specific remediation are out of scope.

---

## 1. Executive Summary

The codebase has a sound modular architecture (Golem structure, pluggable engine registry in `fct_fit_model.R`, Bootstrap 5 / bslib UI). However, four critical gaps block wide release:

1. **Reproducibility & Determinism:** Step 2 (climate scenarios), Step 3 (policy scenarios), and batch Lasso use unseeded random number generation and unpinned database scan ordering. Identical inputs yield different outputs.
2. **Result Integrity & State Synchronization:** Upstream changes trigger `renderUI` rebuilds that wipe selections, completed downstream results remain presented after their inputs change, and live controls can decouple displayed labels/aggregations from run-time settings.
3. **Release Readiness & UX:** Export is limited to two threshold-table CSV buttons, configuration cannot be saved/restored, long computations lack double-click guards, uncaught errors are hidden by CSS, and the built package currently fails its test suite.
4. **Deployment & Connection Safety:** Connect extension loading can silently mark missing binaries as loaded; custom connection forms contain broken credential paths and unescaped SQL literals; weather temp tables are not failure-safe.

---

## 2. Priority Implementation Roadmap

```
┌──────────────────────────────────────────────────────────────────────────────────┐
│ Wave 0: Reproducibility & Determinism (DET-01..09) [BLOCKING CORE]               │
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

## 3. Reproducibility & Determinism (`DET-*`)

| ID | Issue & Location | Impact | Actionable Fix |
|---|---|---|---|
| **DET-01** | Unseeded `sample()` calls in `R/fct_policy_sim.R:108, 115, 174, 179, 395, 415, 474, 499, 526` | **Critical** (Step 3 policy assignment non-reproducible) | Thread explicit `seed` argument through `apply_policy_to_svy()`; wrap execution in `withr::with_seed(seed, ...)`. |
| **DET-02** | Unseeded `rnorm` / `sample` in `R/fct_aggregation.R:196-239` & `R/fct_predict_outcomes.R:330, 348, 356` | **Critical** (Step 2/3 residual draws & unmatched-ID fallbacks shift between runs) | Plumb explicit `seed` into `draw_residuals_vec()`; replace random unmatched ID fallback with deterministic hash-indexed lookup. |
| **DET-03** | Global `set.seed(123)` in `R/fct_get_weather.R:225-229` (`.compute_breaks`) | **High** (Clobbers caller RNG state; resets stream conditionally) | Replace with `withr::with_seed(123, kmeans(...))`. |
| **DET-04** | RNG consumed for DuckDB temp table names in `R/fct_get_weather.R:519, 766, 807` (`sample(letters, 8L)`) | **High** (Advances RNG stream by variable steps depending on var count) | Replace with `basename(tempfile(pattern = "lw_base_"))` or deterministic session counter. |
| **DET-05** | Dormant `ranger` and `xgboost` registry engines are unseeded (`R/fct_fit_model.R:124-126, 164-172`); current `model_type_choices()` exposes only fixest/RIF (`R/fct_model_select.R:294-305`) | **Medium now; High before enabling tree models** | Set explicit engine seeds and thread counts, and add determinism tests before exposing either choice. |
| **DET-06** | Batch sets `LASSO_PARALLEL_SEED <- NULL` (`batch/03_run_mod1.R:131`, `04_run_sim.R:134`) while the app uses `123L` (`R/mod_1_06_model.R:765`) | **High** (App vs batch covariate selection can diverge; batch currently runs this path sequentially but still calls unseeded `mice`/random folds) | Set a fixed integer in all batch configs and the helper default; retain deterministic per-imputation seeds. If parallel mode is enabled, test the existing `future.seed`/`parallelseed` path rather than adding an unscoped global `RNGkind()` change. |
| **DET-07** | DuckDB single-thread pin in `R/fct_get_weather.R:373-379` (`SET threads TO 1`) | **Info** (Preserves FP determinism against multi-threaded summation drift) | **Retain thread pin invariant**. Pursue weather performance via caching and query restructuring only. |
| **DET-08** | Non-weather DuckDB paths (`load_data()`, `mod_1_02_surveystats.R:156`, `fct_surveystats.R`) lack thread pin and explicit sort | **High** (Multi-threaded DuckDB table scans do not guarantee row order) | Add deterministic `ORDER BY` on stable primary keys at all `collect()` boundaries. |
| **DET-09** | Determinism test (`dev/test_determinism.R`) is manual, hardcoded to OneDrive, and covers only `get_weather()` | **Medium** (Core simulation & policy pipelines lack regression guards) | Promote to `tests/testthat/test-determinism.R` using bundled test fixtures; add end-to-end `identical()` checks for Step 1, 2, and 3 pipelines. |

---

## 4. Result Integrity & State Synchronization (`INT-*`)

| ID | Issue & Location | Impact | Actionable Fix |
|---|---|---|---|
| **INT-01** | Dynamic inputs are rebuilt with defaults (`R/mod_1_06_model.R:277-478`, `R/mod_1_04_weather.R:43-241`, `R/mod_1_01_sample.R:98-120`, `R/mod_2_01_weathersim.R:312-325`, `R/fct_policy_sim_compare.R:965-1012`) | **High** (Changing model type, interactions, fixed effects, weather variables, economies, baseline data, or re-running Step 3 wipes adjacent selections and filters) | Snapshot compatible current values with `isolate(input$...)` and restore them as `selected`; explicitly reset only values no longer valid. |
| **INT-02** | `mod_0_overview.R:413, 421-443` publishes `applied_connection` before metadata loads; swallows file errors and retains stale metadata across re-connects | **High** (App runs with new connection params but previous source's metadata; shows false "Connected" toast) | Reset metadata reactive values to `NULL` at start of connect; publish connection and success toast only after all metadata loads succeed. |
| **INT-03** | Step 2 and Step 3 aggregators prefer the live residual radio (`R/mod_2_02_results.R:337`, `R/fct_policy_sim_compare.R:424-426`) over the run-time setting | **High** (Flipping the Step 2 sidebar radio silently re-derives Step 2/3 tables and plots under a different residual regime without re-running) | Store `residuals` in `hist_sim`; make both aggregators prefer `hist_sim$residuals` and use the live input only before a run exists. |
| **INT-04** | `R/fct_policy_sim_compare.R:545-599` wraps scenario aggregations in `tryCatch(..., error = function(e) NULL)` | **Medium** (Failed scenarios silently vanish from results charts and threshold tables) | Collect scenario aggregation errors; display warning toast identifying dropped scenarios. |
| **INT-05** | Results use live labels/settings after fitting or simulation (`R/mod_1_07_results.R:108-185`, `R/mod_1_08_modelfit.R:74-95`, `R/fct_policy_sim_compare.R:428-431`); engine-conditional headings are created only on the first fit | **Medium** (Old results can be relabeled with new variables or historical years, while headings describe the first engine) | Snapshot labels, historical range, and engine specification into result objects; bind renderers and headings strictly to those snapshots. |
| **INT-06** | Survey reload and inner map/panel failures (`R/mod_1_02_surveystats.R:139-147, 181-223`) update microdata but can retain old `map_data()` / `cell_data()` | **Medium** (Map can show previous geography; failed panel join silently removes `loc_id_panel` and changes the VCV fallback) | Clear map/cell state at load start and on every failure; notify when `loc_id_panel` is unavailable because it changes inference. |
| **INT-07** | "Clear simulation results" (`R/mod_2_simulation.R:133-140`) nulls reactive data but does not call `removeTab()` | **Medium** (Results/Diagnostics tabs remain rendered with blank/error panes; empty state never returns) | Call `removeTab()` for each appended tab and reset `*_tab_added` reactive flags to `FALSE`. |
| **INT-08** | No provenance signature or stale-state invalidation links Step 1 fits, Step 2 simulations, and Step 3 policy results (`R/mod_1_07_results.R:87-88`, `R/mod_2_01_weathersim.R:582-583`, `R/mod_3_06_policy_sim.R:110-118`) | **High** (A refit or upstream re-run leaves completed downstream tabs presenting results from superseded data/model/weather inputs as current) | Store an immutable run signature in each result; on upstream changes clear dependent results or mark them stale and disable interpretation/export until re-run. |
| **INT-09** | Step 3 mutates survey diagnostic state before simulation succeeds and preserves prior policy outputs when a new run fails (`R/mod_3_06_policy_sim.R:133-145, 190-193, 268-272`) | **High** (Diagnostics can show the new policy-adjusted survey while Results still show the previous simulation, then the run ID and success toast present that mixture as new) | Build all outputs in locals; atomically publish reactive values and increment `sim_run_id` only after the complete run succeeds. |

---

## 5. Backend Performance & Safe Speedups (`PERF-*`)

### 5.1 Safe Speedups and Gated Prototypes

Wave 2 includes only output-neutral rows listed in the roadmap. Preview mode (PERF-16, Wave 3) and any WebGL map migration within PERF-36 are explicitly gated, output-affecting product/prototype choices.

| ID | Bottleneck & Location | Expected Win | Actionable Fix |
|---|---|---|---|
| **PERF-19** | `R/fct_policy_sim.R:696-714` (`resimulate_with_svy`) omits `svy_prepared` and `precomputed_train_aug` | **Large** | Precompute `train_aug` and `svy_prepared` once in `resimulate_with_svy()`; pass to `run_sim_pipeline()` to avoid redundant `predict(model, train_data)` per ensemble member. |
| **PERF-23** | `R/mod_1_02_surveystats.R:153-213` & `R/fct_loc_panel.R:96-158` execute ~6 separate remote DuckDB parquet scans over `h3_df` | **Medium-Large** | Materialize remote table once into a local DuckDB temporary table via `dplyr::compute(name = ..., temporary = TRUE)`. |
| **PERF-20** | `R/fct_simulations.R:471, 478` rebuilds `.svy_row_id` per simulation key | **Small** | Append `.svy_row_id` once in `fct_run_simulation.R:221-223` before entering the key loop. `$<-` shallow-copies the data-frame column list here; it does **not** cause two full $N \times P$ copies as previously reported. |
| **PERF-21** | `R/mod_3_06_policy_sim.R:224, 230-236` calls `format(timestamp, "%Y")` repeatedly inside year loop | **Medium** | Hoist `w_year <- as.integer(format(w_raw$timestamp, "%Y"))` outside loop and subset via index vector. |
| **PERF-22** | `R/fct_policy_decompose.R:449, 119` recomputes policy deltas and `stats::ecdf()` per scenario-year | **Medium** | Precompute weather-independent `.compute_policy_deltas()` and empirical CDF once per scenario; pass down to year loop. |
| **PERF-03** | `R/fct_rif_sim.R:40-46` recomputes `bw.SJ()` and full `density()` once for each of nine taus | **Medium** | Hoist bandwidth and density calculation outside the tau loop; evaluate at quantiles via `stats::approx()`. |
| **PERF-06** | Live `.geojson_bounds()` in `R/fct_surveystats.R:263-297` uses repeated `c(vec, x)` growth; the previously cited `.feature_centroid()` block has no callers | **Medium** | Track running min/max bounds directly; delete the dead centroid helper separately. |
| **PERF-07** | The categorical diagnostics path appends regression/scenario observations twice (`R/fct_sim_diag.R:125-188`) | **Medium correctness/performance** (bar proportions are unchanged within each duplicated source, but sample-size subtitle is doubled and memory/work are doubled) | Delete lines 160-188 and compute subtitle counts from the unduplicated source frames. |
| **PERF-10** | `R/fct_surveystats.R:895-907, 954-966` parses GeoJSON to R lists and retains raw strings | **Low** | Drop parsed list copies; compute bounding boxes directly from string attributes or DuckDB extents. |
| **PERF-11** | `R/fct_load_data.R:346-415` executes `CREATE OR REPLACE SECRET` on every `load_data()` call | **Low** | After fixing SQL quoting (SEC-01), apply the hash-cached secret check from Databricks (`.register_db_secret`) to S3, GCS, and Azure handlers. |
| **PERF-13** | `R/fct_get_weather.R:437-442, 641-709` re-fetches identical ERA5/CMIP6 parquet files; CMIP6 history is opened once for resolution probing and again for aggregation | **Medium** | Add a bounded disk cache keyed by source/version, codes, variables, date range, and transformation; reuse the loaded historical relation for the resolution probe. Do not use an unversioned key. |
| **PERF-16** | Existing `dev_mode` limits the ensemble to one model (`R/mod_2_01_weathersim.R:214-221, 555`) but leaves coefficient uncertainty enabled and is exposed as a development control | **Medium, explicitly output-changing** | Replace it with a clearly labeled Preview mode that combines one member with point estimates, reports that approximation in provenance, and is never presented as final output. This is not an output-neutral speedup. |
| **PERF-17** | `R/fct_weatherstats.R:771` & `R/fct_surveystats.R:820-851` use `do.call(rbind, parts)` on 1-row data frames | **Medium** | Replace with `dplyr::bind_rows(parts)`. |
| **PERF-24** | `R/fct_sim_diag.R:82, 443-458` re-filters `weather_raw` per weather variable in density panel | **Small-Medium** | Hoist `.filter_hist_weather()` outside variable loop; pass filtered frame down. |
| **PERF-25** | `R/fct_weatherstats.R:740-749` re-computes `interaction()` and `split()` per weather variable | **Small-Medium** | Hoist group interaction and split index construction outside variable iteration. |
| **PERF-27** | `R/fct_rif_sim.R:156-158` rebuilds training data `ecdf()` per simulation key | **Small** | Construct `stats::ecdf(train_data[[outcome]])` once in parent caller and pass in. |
| **PERF-28** | `R/fct_fit_model.R:751-757` calls `installed.packages()` on every model fit | **Small** | Replace with `requireNamespace(pkg, quietly = TRUE)`. |
| **PERF-29** | `R/fct_results.R:1093-1098` vs `1110-1115` builds `bins_df` twice identically | **Small** | Remove dead first block at lines 1093–1098. |
| **PERF-30** | `R/mod_2_02_results.R:310-376, 500-506` includes display-only `band_q` in `agg_workspace()`, so changing the coefficient-band dropdown destroys the full aggregation cache | **Medium** | Remove `band_q` from the value cache and apply it only when rendering bands; narrow poverty-line/bandwidth keys to methods that consume them. |
| **PERF-31** | Step 3 re-aggregates baseline and every scenario/member whenever aggregation method or deviation label changes (`R/fct_policy_sim_compare.R:492-619`) | **Medium-Large** | Add a per-method workspace cache keyed only by value-affecting inputs; move `cmp_deviation`-dependent label assembly downstream because deviation is applied after aggregation. |
| **PERF-32** | `run_sim_pipeline()` retains a full prediction frame while allocating the $N \times K$ design/factor matrices (`R/fct_predict_outcomes.R:162-163`, `R/fct_simulations.R:564-702`) | **Large memory; Small-Medium time** | After the RIF/SP corrections and required-vector extraction, release `out` before building factor loadings; preserve any `F_loading` attribute first. Output-neutral if ordered carefully. |
| **PERF-08** | `R/fct_model_select.R:230-240` scans the full frame once per wave to count non-missing values | **Small-Medium** | Compute identical 0/1 counts in one grouped pass. This is output-neutral for realistic row counts; preserve existing group order. |
| **PERF-34** | Default residual aggregation rebuilds the same ID-to-residual named vector and character keys for each year, member, method, and weighting (`R/fct_aggregation.R:215-221`) | **Small-Medium** | Cache the deterministic matched-ID lookup and residual variance with each pipeline; leave unmatched-ID sampling in the call path until DET-02 fixes it so RNG consumption is not shifted. |
| **PERF-36** | The map pipeline parses every geometry into nested R lists solely for bounds, retains both parsed and raw GeoJSON, and continuous colours can create nearly one `addGeoJSON()` layer per distinct value (`R/mod_1_02_surveystats.R:153-179`, `R/fct_surveystats.R:609-638, 895-909`, `R/fct_outcome.R:432-453`, `R/fct_weatherstats.R:1156-1221`) | **Medium-Large for many polygons** | First compute bounds beside geometry in DuckDB, keep only raw JSON, emit one per-feature-style GeoJSON layer, enable Canvas on the survey map too, and cache cell features by data/wave signature. These Leaflet edits are output-neutral; the optional migration is not. If targets still fail, prototype `deckglgeoarrow` polygon layers behind the same map contract: it is on CRAN but only v0.0.2 and requires a `mapgl` host plus `geoarrow`/`geoarrowWidget`/`nanoarrow`; `wk`/`sf` object classes are optional. Do not default to the older, largely dormant GeoJSON-based `deckgl`; verify compiled dependencies on Connect, Shiny proxy updates, tooltips, legends, fit bounds, offline assets, and packaging. |
| **PERF-37** | Map-module comments claim a custom htmlwidgets JSON encoder is installed from nonexistent `R/zzz.R` (`R/mod_1_02_surveystats.R:242-245`, `R/mod_1_03_outcome.R:202-205`) | **Low, but relevant to map migration/serialization diagnostics** | Remove stale comments or restore a tested package hook deliberately; do not suppress serialization warnings globally while benchmarking map payloads. |

### 5.2 Numerical Refactoring (Wave 5 — Requires Golden-File Validation)

| ID | Location | Potential Risk | Actionable Fix |
|---|---|---|---|
| **PERF-02** | `R/fct_get_weather.R:138-175, 528-529, 825-827` | Restructures SQL query plan; altered float sum order | Collapse V separate scans/joins into single wide `summarise(across(...))` + single join. |
| **PERF-05** | `R/fct_weatherstats.R:749-771` & `R/fct_surveystats.R:820-851` | Replaces custom loop with dplyr grouped aggregation | Rewrite custom `split/lapply` math as one grouped `summarise()` pass. |
| **PERF-09** | `R/fct_weatherstats.R:1346-1352` & `R/fct_surveystats.R:1100-1108` | Per-variable loop over full frame | Replace with `summarise(across(all_of(vars), ~mean(is.na(.x))))`. |
| **PERF-15** | `R/fct_simulations.R:656-657` vs `R/fct_predict_outcomes.R:154-157` | Modifies `fixest` row-dropping & offset handling | Reuse design matrix between prediction and uncertainty factor loading. |
| **PERF-26** | `R/fct_simulations.R:486-491` | Date parsing edge cases | Replace `as.integer(format(timestamp, "%m"))` with `as.POSIXlt(timestamp)$mon + 1L`. |
| **PERF-33** | `R/utils_mod_1_helpers.R:32-78` performs full-frame `split()` plus nested `rbind`; six Step 1 survey tables call it independently (`R/mod_1_02_surveystats.R:299-337`) and the weather table calls it again (`R/fct_weatherstats.R:1326-1337`), while `batch/R/batch_utils.R:246-444, 447-714` already implements grouped `collapse::GRP`/`fmean`/`fsd` engines | A naive port is **not output-neutral**: the app uses the reliability-weights unbiased denominator $\sum w-\sum w^2/\sum w$ (`utils_mod_1_helpers.R:58-62`, equivalent to `cov.wt(..., method = "unbiased")`), while `collapse::fsd(..., w=)` uses the frequency-weight denominator $\sum w-1$ and produced materially different SDs; the batch path also does not apply the app's `is.finite(x) & is.finite(w) & w > 0` mask. `GRP()` uses C-locale ordering and retains an `NA` group, whereas the current `split()` ordering is locale-dependent and drops `NA` keys. `collapse` is undeclared in `DESCRIPTION`/`renv.lock` | Reuse `GRP`, `fmean`, `fsum`, `fmin`, `fmax`, and `fnobs`, but preserve the app mask and compute variance with grouped $\sum w$, $\sum w^2$, and $\sum w(x-\bar{x})^2$; compute missingness separately on unmasked rows and explicitly drop `NA` group keys. Cache one grouping/result set across all survey tables and use the same engine for weather. Synthetic validation measured about 1.5-2.2x for weighted summaries, ~5x for missingness, and much lower peak memory; add a committed production-scale benchmark. Require exact N/min/max/missingness and two-decimal mean/SD parity, including invalid weights, all-missing/single-row groups, constants, `NA` keys, and an explicit deterministic ordering for non-ASCII keys. |

### 5.3 Map Rendering Decision Gate

1. Benchmark current Leaflet on representative 1k, 10k, and maximum-production polygon sets: server build time, serialized payload bytes, browser first paint, pan FPS, hover/popup latency, and peak browser memory.
2. Apply the output-neutral GeoJSON/bounds/layer-count fixes in PERF-10/PERF-36 and repeat the benchmark.
3. Prototype `deckglgeoarrow` only if optimized Leaflet misses agreed targets. Require identical classification/legend values, tooltip/popup behavior, retained map view, Shiny proxy updates, no external runtime CDN, fresh-clone Connect deployment, and a non-WebGL fallback. Keep protected geometry server-mediated; do not expose storage credentials/tokens or direct browser URLs to restricted GeoParquet.
4. Use provisional promotion targets at 10k polygons: exactly one data layer, server widget build <=300 ms, time-to-interactive <=1.5 s, and sustained pan/zoom >=45 FPS. Pursue WebGL/GeoArrow only if optimized Leaflet fails the maximum-production or 50k-polygon probe.

---

## 6. Reactivity & Pipeline Correctness (`REACT-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **REACT-01** | `R/mod_0_overview.R:401-413`: `return()` inside the `tryCatch` handler exits only that handler; a whitespace-only local path then proceeds to `applied_connection()` | **Medium** | Assign `new_path <- tryCatch(...)`; check `if (is.null(new_path)) return()` in observer scope. Also require `dir.exists()` before publishing a local connection. |
| **REACT-02** | No busy guards on loads, fit, or simulation actions (`R/mod_1_02_surveystats.R:101`, `R/mod_1_05_weatherstats.R:88`, `R/mod_1_07_results.R:63-92`, `R/mod_1_06_model.R:704-799`, `R/mod_2_01_weathersim.R:506-610`, `R/mod_3_scenario.R:307-309`) | **High** | Add observer-side `running <- reactiveVal(FALSE)` guards with `on.exit(running(FALSE))`; disable the triggering control while running. |
| **REACT-03** | `R/mod_1_02_surveystats.R:101-224` & `R/mod_1_05_weatherstats.R:88-163` re-execute full I/O on repeated clicks | **Medium** | Cache hash signature (`digest::digest()`) of inputs; short-circuit identical load requests. |
| **REACT-04** | Each Step 2 run deliberately regenerates the full currently configured scenario set (`R/fct_run_simulation.R:381-395`) and replaces it at `R/mod_2_01_weathersim.R:582-583` | **Info** (No result loss; the prior recommendation to merge would retain scenarios the user removed) | Keep replacement semantics; preserve compatible scenario-filter UI selections separately if desired. |
| **REACT-05** | `R/mod_3_06_policy_sim.R:190-272` can increment `sim_run_id` and display success after policy simulation/decomposition failure | **High** | Gate all state publication and success on a complete result; surface simulation and decomposition errors persistently, not only via console warnings. |
| **REACT-06** | `R/mod_2_02_results.R:872-1244` repeats similar `one_scenario()` band-assembly scaffolding across five reactives; shared math helpers are already centralized in `R/fct_uncertainty_helpers.R` | **Low** | Parameterize the repeated assembly only if doing so improves testability; do not re-extract `.apply_contrast_sd()` or `by_model_matrix()`. |
| **REACT-07** | `R/mod_2_02_results.R:364-376, 500-506` rebuilds a fresh cache and eagerly precomputes after every workspace dependency changes | **Medium** | Remove display-only dependencies (PERF-30) and key cached values by the method-specific inputs they actually consume; retain eager warming only for new simulation results. |
| **REACT-08** | Term extraction logic duplicated across `R/mod_3_02_infra.R:49-89`, `mod_3_03`, `mod_3_04`, `mod_3_05`, `mod_3_06` | **Medium** | Extract shared helper function in `utils_mod_1_helpers.R`. |
| **REACT-09** | `R/mod_3_scenario.R:307-309` calls child module's exported `run()` closure directly | **Low** | Pass reactive trigger parameter into child module server. |
| **REACT-10** | Unused reactive endpoints (`lasso_status` at `mod_1_06:700`, `sim_n` at `mod_2_01:619`); Step 3 poverty line missing debounce (`fct_policy_sim_compare.R:458` vs `mod_2_02:302`) | **Low** | Clean dead reactive returns; apply 400ms debounce to Step 3 poverty line input. |
| **REACT-11** | Parallel aggregation engines in `R/mod_2_02_results.R:288-518` vs `R/fct_policy_sim_compare.R:412-524` | **Low** | Consolidate Step 2 and Step 3 aggregation logic into unified `fct_aggregation.R` engine. |
| **REACT-12** | Per-key Step 2 errors are converted to `NULL` (`R/fct_run_simulation.R:276-325`), but the returned object has no failure ledger and `R/mod_2_01_weathersim.R:588-608` announces unconditional completion | **High** (Historical failure can yield no baseline; future-member failures silently narrow or remove ensemble groups) | Collect key/error pairs; fail the run if historical or all members of a requested group fail, otherwise publish a prominent partial-result warning and provenance counts. |
| **REACT-13** | Scenario filters are applied to most Step 2 outputs but not the hero point-range reactive (`R/mod_2_02_results.R:966-975` vs `1007, 1058, 1108, 1238`) | **High** (Unchecked scenarios remain in the main chart while disappearing from tables/other plots) | Filter the point-range loop through `selected_scenario_names()` and add a reactive test covering all output families. |
| **REACT-14** | Model fitting can warn and silently fall back from logistic to linear or from clustered to unclustered VCV (`R/fct_fit_model.R:781-790, 920-926`), while the Shiny observer reports only generic success (`R/mod_1_07_results.R:73-90`) | **High** (The fitted estimand/inference can differ from the user's selected model without persistent UI disclosure) | Capture structured warnings/fallback metadata in `model_fit`, show them in the settings/provenance banner, and require explicit acknowledgement for a model-family fallback. |

---

## 7. UI, UX & Accessibility Findings (`UI-*`)

| ID | Issue & Location | Severity | Actionable Fix |
|---|---|---|---|
| **UI-01** | `custom.css:120-129` sets `.shiny-output-error { visibility: hidden; }` while zero `validate(need())` calls exist | **High** | Remove blanket CSS hide; add explicit `validate(need(...))` to long renderers; style error states as visible warning cards. |
| **UI-02** | Config flyouts (`mod_1_04:87-239`, `mod_1_06:28-48`, `mod_2_01:43-57`) share fixed `left: 400px; top: 90px`, can remain open across accordion switches, and lack focus/Escape handling | **High** | Anchor one flyout to its sidebar, enforce one-open state, synchronize `aria-expanded`, move focus on open/close, and close on Escape. |
| **UI-03** | Visual headings/`tags$label` elements are not associated with controls whose Shiny `label = NULL` (`mod_2_01:69-156, 312-325`, `mod_2_02:813-821`, `mod_3_01_sp:117-194, 325-345`, shared Step 3 helpers including `mod_3_04_labor:126-206`) | **High** | Put text in each input's `label`, or use `<label for=...>` / `aria-labelledby`; preserve the visible grouped headings. |
| **UI-04** | Colorblind-unsafe palettes in `fct_simulations.R:23-25` (green/red SSP), `fct_outcome.R:418-421` (RYG), `fct_results.R` (Set1) | **High** | Standardize on Okabe-Ito / Viridis / Mako palettes centralized in `utils_plot_theme.R`. |
| **UI-10** | Collapsing the Lasso forced-covariates panel (`R/mod_1_06_model.R:456-474, 511-617`) removes inputs; live `selected_model()` then changes the apparent model contract while `model_fit` remains unchanged | **High** | Keep inputs registered with CSS visibility or snapshot forced-in/out and advanced settings at fit time; make all downstream consumers use the snapshot. |
| **UI-23** | Export is incomplete: only Step 2/3 threshold DTs expose CSV (`R/mod_2_02_results.R:1276-1284`, `R/fct_policy_sim_compare.R:1044-1052`); other tables, all plots, configuration, and a results bundle have no export | **High** | Phase 1: add CSV for aggregated result plot/table data and server-rendered PNG via `downloadHandler()` from the same ggplot builder (fixed dimensions/DPI), plus configuration JSON and a provenance sidecar/bundle. Prefer base PNG initially or declare `ragg` and add a direct lock entry if adopted; references in other packages' recorded `Suggests` do not make it available after a clean restore. Do not depend on widget screenshots or expose raw household rows in the wider-release UI without an explicit disclosure policy. |
| **UI-24** | Analysis configurations cannot be saved, shared, or restored (`enableBookmarking` inactive) | **High** | Implement JSON export/import of complete analysis configuration state (inputs, model specs, random seed). |
| **UI-25** | Steps 1, 2, and 3 openable in any order without prerequisite completion indicators | **Med-High** | Add completion badges (✓) to navigation header; include explicit "Prerequisites: ..." notices in overview empty states. |
| **UI-26** | Existing summaries/headers (`mod_1_04:292-338`, `mod_1_06:107-147`, `mod_2_01:342-395`, `mod_2_02:759-783`, `fct_policy_sim_compare:941-963`) omit source identity, exact model specification, seed, and run signature; Step 2 hardcodes "All models" even in dev mode (`mod_2_01:365`) | **Medium** | Standardize and complete provenance banners from immutable result metadata; show ensemble count/dev mode accurately. |
| **UI-28** | Core Step 2 simulation settings (baseline survey, historical/future periods) hidden inside flyout (`mod_2_01:59-120`) | **Medium** | Surface core simulation controls directly in sidebar; reserve flyout for advanced parameters. |
| **UI-29** | Run buttons silently no-op or return when prerequisites are absent (`mod_1_06:49`, `mod_2_01:506-508`, `mod_3_06_policy_sim.R:120-129`); Step 3 writes `sim_error` but never renders the declared `sim_status_ui` | **High** | Disable run buttons until ready, attach a missing-prerequisite explanation, and implement the persistent Step 3 status/error output. |
| **UI-30** | `R/mod_2_01_weathersim.R:438` silently drops future periods where `end <= start` | **Medium** | Display inline validation error when projection periods are inverted or equal. |
| **UI-31** | ~600 dead lines in Step 3 (`fct_policy_sim_compare.R:1157, 1429, 1550` `pol_*` trio; `mod_3_09:297` `.plot_beta_curves`) | **Low** | Delete unused functions, unreferenced output bindings, and commented UI blocks. |
| **UI-32** | Social protection targeting inputs (`mod_3_01_sp.R:197-216`) provide no pre-run feedback on eligible population size | **Medium** | Add dynamic text output showing estimated eligible household count based on active cutoff. |
| **UI-33** | "Change access by (%)" in `mod_3_02:139`, `mod_3_03:135` and "Change attainment by (%)" in `mod_3_05:138` mean the share of currently unserved observations flipped, not percentage-point change (`R/fct_policy_sim.R:93-119`) | **Medium** | Relabel to "Share of unserved gaining access/attainment (%)" and add explanatory help text. |
| **UI-34** | Labor manufacturing + services sliders can sum above 100%; agriculture is then clamped to zero (`R/mod_3_04_labor.R:213-242`) | **Medium** | Constrain the two editable shares to a combined 100% and announce the remaining agriculture share; UI/backend currently agree on the clamp, contrary to the prior wording. |
| **UI-35** | `DESCRIPTION:55-56` and `README.md:16` contain obsolete `welfare-weather-app` repository URLs | **Low** | Update repository and bug tracker URLs to `worldbank/wise-app`. |
| **UI-36** | All 21 `plotOutput()` sites omit alternative text (representative: `R/mod_2_02_results.R:196, 242`; `R/mod_2_03_diagnostics.R:51, 87, 114`); three Leaflet maps also have no text-equivalent summary | **Medium** | Supply concise reactive `alt` text for plots and adjacent text/table summaries for maps; use wrappers so dynamic output IDs receive the same treatment. |
| **UI-38** | Unchecking every Step 2 scenario checkbox silently re-adds the first scenario in server state (`R/mod_2_02_results.R:668-682`) | **Medium** | Prevent clearing the final choice or render a true no-scenario empty state; never show an unchecked scenario as selected. |
| **UI-39** | External navbar links have no accessible names beyond visible text/`title` (the GitHub link is icon-only) and omit `rel="noopener noreferrer"` (`R/app_ui.R:52-74`) | **Low** | Add explicit `aria-label` names (including destinations for Docs/Data) and safe `rel` attributes to all new-tab links. |
| **UI-40** | Coefficient-uncertainty help claims a Monte Carlo fallback for `avg_poverty` (`R/mod_2_01_weathersim.R:188-195`), but the current aggregation path is delta-only | **Medium** (Users are given an incorrect method description exactly where TEST-06 finds a broken gradient) | Remove the fallback claim and document the implemented gradient/band transform after TEST-06 is corrected. |
| **UI-41** | All chart outputs are static ggplots, but global widget conversion would serialize household-level values for distributions/diagnostics and can misrender complex geoms/transforms (21 `plotOutput()` sites; aggregated candidates at `R/mod_2_02_results.R:1246-1340`, `R/fct_policy_sim_compare.R:1014-1112`, `R/mod_3_09_decomposition.R:134-201`) | **Medium opportunity with confidentiality/rendering risk** | Keep static ggplot as the canonical/export path. After UI-23, prototype interactivity only for aggregated coefficient, scenario, exceedance, time-series, and decomposition charts; compare `ggiraph` and `plotly` empirically rather than prespecifying a winner, because ribbons, facets, log/logit axes, keyboard use, and widget payloads differ. Neither package is declared/locked; adoption requires `DESCRIPTION`, `renv.lock`, and clean Connect packaging validation. Restore `suspendWhenHidden = TRUE` for Step 3 chart widgets (`fct_policy_sim_compare.R:1028, 1099, 1112`) unless pre-rendering is measured as necessary. Keep household-level plots server-rendered and add CSV/text equivalents instead of browser-side microdata. |

### 7.1 Recommended Output Delivery Sequence

1. **Exports first:** expose the tidy aggregated frame behind each result plot as CSV and render PNG server-side from the same ggplot object. Include source/model/seed/run signature in filenames or a sidecar; raw household-row export requires an explicit disclosure policy and is out of scope for the default wider-release UI.
2. **Selective interactivity second:** prototype only aggregated decision charts. Preserve the static ggplot path for publication-quality PNG and as a fallback for unsupported geoms/transforms.
3. **No global conversion:** household-level distributions, residual diagnostics, binscatters, and policy histograms remain server-rendered to avoid sending microdata to browser widget JSON. Leaflet maps are already interactive.

---

## 8. Redundancy & Dead Code (`RED-*` & `DUP-*`)

| ID | Issue & Location | Actionable Fix |
|---|---|---|
| **RED-01** | `inst/mathjax/` is no longer present and the current `manifest.json` has no MathJax entries; the prior ~100 MB deletion recommendation is stale | No action. Retain this correction so the asset is not targeted again. |
| **RED-02** | `R/golem_utils_ui.R` (582 lines) and `R/golem_utils_server.R` (0 bytes) have zero call sites | Delete both files. |
| **RED-03** | `dev/archived_fct/` contains obsolete plotting functions | Delete directory. |
| **RED-04** | Byte-identical scripts in `dev/` vs `batch/R/` (`spec_curve.R`, `expand_weather_specs.R`) | Point `dev/` callers to `batch/R/` copies; delete `dev/` duplicates. |
| **RED-05** | `batch/R/spec_curve.R` (608 lines) auto-sourced by all batch scripts but never executed | Move file to `dev/archive/`. |
| **RED-06** | Six near-identical simulation scripts (`batch/04_run_sim_*.R`, ~11,500 lines total) | Replace with single canonical script parameterized via `Sys.getenv("WISEAPP_COUNTRY")` + YAML config. |
| **RED-07** | `R/fct_h3_check.R` (107 lines) exported and documented but never called | Wire into `batch/01a_sample_selection.R` QA pipeline or delete. |
| **RED-08** | Committed one-off scratch scripts in `dev/` and `batch/` | Move historical scripts to `dev/archive/`. |
| **RED-09** | Outdated `README.md` build transcripts and broad/unanchored `.Rbuildignore` patterns (`dev`, `tests`, `README.Rmd`) | Re-render README; replace broad matches with intentional anchored expressions. The previously reported `$run_dev.*` pattern is not present in the current file. |
| **DUP-01** | `resolve_band_q()` is defined twice with divergent `minmax` semantics (`R/fct_aggregation.R:157` winsorizes to 0.001/0.999; later-sourced `R/fct_sim_compare.R:76` overwrites it with 0/1) | Keep one authoritative function in `R/fct_aggregation.R` or `utils_math.R`, update all callers/tests, and remove the load-order dependency. |
| **DUP-02** | `.normalise_ssp()` is defined twice (`R/fct_simulations.R:34` vs later `R/fct_sim_compare.R:91`); the later definition is weaker and overwrites the first package-wide | Keep the robust implementation and delete/rename the local duplicate; add cases for abbreviated SSP names and suffixes. |
| **DUP-03** | `fct_policy_sim_compare.R` mirrors plotting and aggregation internals of `fct_sim_compare.R` | Extract shared series assembly, threshold tables, and exceedance rendering into neutral helper module. |

---

## 9. Testing & Packaging (`TEST-*`)

| ID | Issue & Location | Actionable Fix |
|---|---|---|
| **TEST-01** | No direct coverage for the core estimation/simulation engine (`fct_fit_model`, `run_sim_pipeline`, `fct_run_simulation`, `apply_policy_to_svy`); existing tests cover aggregation, uncertainty helpers, weather, and one Shiny module | Construct deterministic small-fixture tests for Step 1/2/3, including partial-key failure behavior and provenance signatures. |
| **TEST-02** | Unused imports in `DESCRIPTION` (`vip`, `relaimpo`, `markdown`) | Remove unused packages from `DESCRIPTION` and `NAMESPACE`. |
| **TEST-03** | `DESCRIPTION` has no `Suggests:` despite tests requiring `testthat` and `spelling`; built-package `R CMD check` reports undeclared test dependencies | Add `Suggests: testthat (>= 3.0.0), spelling` and `Config/testthat/edition: 3`; add `covr` only when coverage tooling is configured; ensure declared test tools are in `renv.lock`. |
| **TEST-04** | `CLAUDE.md` module/function/test inventory is stale | Refresh it from the current tree: 24 `mod_*.R`, 25 `fct_*.R`, 11 files under `tests/testthat/` plus `tests/spelling.R`. |
| **TEST-05** | `devtools::test()` fails 5 assertions; `R CMD check` on the built source package fails 6 and reports dependency/documentation warnings | **Release blocker.** Require a clean built-source check in CI. The extra installed-package failure is `months(24)` at `tests/testthat/test-fct_get_weather.R:668-672`, which relies on an attached `lubridate` search path not declared by the test. |
| **TEST-06** | Four failures at `tests/testthat/test-fct_aggregation_delta.R:138-158` have two causes: three assertions and `.claude/method_uncertainty.md:245-261` still describe the old “average welfare among the poor” functional, while `R/fct_aggregation_delta.R:191-204` drops the $1/\mu_i$ chain-rule factor for the current `mean(1 / welfare)` metric | Fix the derivative to `-1 / (n_ok * mu_i)` (weighted analogue `-w_i/(W\,mu_i)`), rewrite stale tests/method docs for “days needed to earn $1,” correct the documented band transform, and validate `F_agg` against finite differences for every method. |
| **TEST-07** | `tests/testthat/test-fct_weatherstats.R:73-89` expects per-wave weight normalization, while `R/fct_weatherstats.R:37-42` explicitly disables it and returns raw weights | Decide the statistical contract; either restore normalization and update warnings/docs, or change the stale test. Do not waive the failure without documenting multi-wave weight semantics. |
| **TEST-08** | Built-package check reports undeclared runtime use (`config`, `digest`, `Hmisc`, `htmlwidgets`, `jsonlite`, `matrixStats`), non-exported `fixest::vcov` calls (`R/fct_results.R:83-91`), and documentation/encoding problems | Declare runtime dependencies or remove usage; replace `fixest::vcov` with `stats::vcov`; regenerate Rd and rerun check from the built tarball. |
| **TEST-09** | Independent finite-difference validation finds the `prosperity_gap` delta gradient wrong in both branches (`R/fct_aggregation_delta.R:155-165`): it drops the $1/\mu_i$ factor, and the unweighted branch also divides by $N$ twice | **High numerical bug.** Use `-w_i/W * 28/mu_i` below $28 (or `-28/(N*mu_i)` unweighted), add weighted/unweighted finite-difference tests, and regenerate Step 2/3 uncertainty goldens. Point estimates are unaffected; coefficient bands are not. |

---

## 10. Deployment, Connections & Security (`DEP-*` / `SEC-*`)

| ID | Issue & Location | Severity / Impact | Actionable Fix |
|---|---|---|---|
| **DEP-01** | Connect branch of `.duck_load_ext()` skips both `INSTALL` and `LOAD` when `system.file()` finds no bundled binary, but still records the extension as loaded (`R/fct_load_data.R:56-93`). `inst/duckdb_extensions/` is ignored/untracked while `manifest.json` references files there. | **Critical** (Fresh-clone/Git-backed Connect deployment can permanently cache an unloaded `httpfs`/`h3`/`spatial` state for the worker; Azure also requests unbundled `azure`/`delta`) | Mark loaded only after successful `LOAD`; fail fast when a required bundle is absent; commit/package supported binaries or implement and test the currently unused authenticated download path; regenerate the deployment manifest from a clean clone. |
| **DEP-02** | Custom connection UI and backend disagree: blank S3 inputs shadow environment credentials; GCS `keyfile` and HF `token` are never consumed; Azure UI advertises SAS but backend builds an `AccountKey` string (`R/fct_connection.R:15-29`, `R/mod_0_overview.R:208-254`, `R/fct_load_data.R:343-415`) | **High** (Several advertised private-source connection paths cannot work; failures are then misreported as connected by INT-02) | Define and integration-test one supported auth contract per source; use empty-aware fallback for optional UI fields; remove or implement unsupported GCS/HF/SAS controls and help text. |
| **DEP-03** | Local validation accepts nonexistent paths (`R/fct_connection.R:49, 66-69`); the green badge validates fields, not reachability (`R/mod_0_overview.R:309-326`) | **Medium** (Users see “configured/connected” before any source is reachable) | Separate “configuration complete” from “connection verified”; require `dir.exists()`/metadata probe before publishing success. |
| **SEC-01** | S3/GCS/Azure `CREATE OR REPLACE SECRET` SQL interpolates credential text without escaping quotes (`R/fct_load_data.R:346-399`) | **High for local/custom connections** (A crafted credential can terminate the literal and execute SQL in the app's DuckDB process) | Centralize SQL literal quoting and escape `'` as `''` for every interpolated value, including `.register_db_secret()`; add adversarial quote tests before adding PERF-11 caching. |
| **SEC-02** | Materialized weather temp tables are cleaned only on the happy path (`R/fct_get_weather.R:519-523, 765-822, 847-860`) while DuckDB is process-wide (`R/fct_load_data.R:21-43`) | **Medium** (Errors/retries retain large tables until worker exit; final cleanup failure can discard an otherwise complete result) | Register every created table immediately in one `on.exit(..., add = TRUE)` cleanup ledger and make cleanup best-effort. |
| **SEC-03** | The process-wide DuckDB connection, views, tokens, and secrets have no session-end cleanup (`R/fct_load_data.R:21-43, 109-160`) | **Low on one-session Connect; Medium for repeated local `run_app()` sessions** | Disconnect and clear caches in `session$onSessionEnded`; store token hashes rather than plaintext cache markers; prefer temporary views where possible. |
| **SEC-04** | User/data-derived strings enter Leaflet popup HTML without explicit escaping (`R/fct_weatherstats.R:1192-1208`, `R/fct_surveystats.R:960-980`) | **Low in team-controlled Connect data; Medium for arbitrary local data** | HTML-escape labels/IDs before popup assembly and keep JSON encoding separate from HTML encoding. |
| **SEC-05** | `make_stats_dt()` sets `escape = FALSE` after inserting `<br>` but does not first escape data-derived economy/year/variable labels (`R/fct_surveystats.R:1116-1160`) | **Low-Medium for arbitrary local data** (Markup in custom survey/metadata values renders in the analyst's browser) | HTML-escape each original cell string before wrapping it; leave only the application-generated `<br>` markup unescaped. |
