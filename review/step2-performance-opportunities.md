# Step 2 Performance Opportunities

## Context

Step 2 has two workloads: **run-time simulation** (weather loading, survey join, prediction, factor loadings) and **post-run results computation** (aggregation, uncertainty, ensemble summaries). Most local micro-optimisations are already integrated. Remaining gains are architectural.

**Already integrated:** `S2-P17` (2.62× multi-period aggregation), `S2-P19` (24.5–29.6% weather-delta improvement locally, ~8.4% on Databricks), lazy Results aggregation, bounded caches, per-year index reuse (2.04s → 0.465s repeated scan).

**Already rejected:** survey join cache (regressed 11.5s → 18.5s), merging `predict.fixest()`/`model.matrix()` (different correctness contracts), automatic two-thread weather (historical/remote regressed), immediate key-level parallelism (memory/determinism risk), shared historical denominators (multi-variable weather workloads uncommon; reopen only if production profiling shows denominator work is material).

---

## Current Cost Centres

| Priority | Area | Notes |
|---|---|---|
| Highest | Weather loading & CMIP6 prep | DuckDB parquet reads, H3 joins, delta construction, rolling windows |
| High | Per-key join & prediction | Already reuses residuals, CDFs, Cholesky blocks, aligned designs |
| High | Serial key execution | Intentional; avoids memory multiplication, DuckDB contention, ordering issues |
| Lower | Results aggregation | Already heavily optimised; marginal remaining gains |

---

## Opportunities

### 1. Cross-Run Weather Reuse *(Priority: High; implemented)*

**Problem:** Full weather pipeline reruns even when only prediction/display settings changed (residual mode, uncertainty, aggregation controls).

**Solution:** Content-addressed weather cache keyed by a complete weather signature covering: variables, transformations, binning, locations, wave selection, historical dates, SSP/period, perturbation method, and data source/version.

Store reusable products at each pipeline stage: raw reads → location-month materialisation → transformed historical → CMIP6 deltas → perturbed future members. Use reference storage for large future members.

**Gate:** Compare cold vs. warm runs across historical-only, one SSP, and multi-SSP/period configurations on both local and remote backends. Verify exact frame parity and no stale DuckDB tables.

**Implementation:** `fct_run_simulation()` now uses the `weather_consumer` boundary
to persist complete prepared frames (after H3 aggregation, rolling windows,
transformations, rounding, and binning) in a content-addressed cache. The cache
uses atomic staging/publication, a manifest with canonical key order, bounded LRU
eviction, and automatic cold-run fallback for missing or corrupt entries. It is
controlled by `prepared_weather_cache = "auto" | "off" | "read_write"` and
`WISEAPP_PREPARED_WEATHER_CACHE_DIR` / `WISEAPP_PREPARED_WEATHER_CACHE_MAX_MB`.
Focused cold/warm tests pass with exact pipeline and member-weather parity; date
changes create a distinct cache entry. Production-scale warm-run and remote
backend characterization remains required before relying on the default mode.

---

### 2. Separate Weather Generation From Prediction *(Priority: High; implemented)*

**Problem:** `fct_run_simulation()` couples weather preparation and prediction in one pass. The same weather is regenerated when only model specification or uncertainty settings change.

**Solution:** Treat the two stages as independently cacheable:
```
Weather preparation → signed weather manifest
Weather manifest + model → prediction pipelines
```
The existing `weather_consumer` boundary already approximates this split.

**Gate:** Same as Opportunity 1, plus verify canonical key ordering, member-specific weather, and scenario grouping are preserved in the manifest.

**Implementation:** `prepare_weather_manifest()` now owns weather construction and
returns a versioned manifest containing canonical historical and future member
frames. `fct_run_simulation()` remains the compatibility wrapper and accepts the
manifest through `weather_manifest`, skipping weather construction while retaining
the existing per-key prediction and payload path. Focused tests prove two model
fits can consume one manifest with one weather-loader call and identical weather
frames. Full production-scale local/remote timing and RSS characterization is
still the rollout gate for the persistent cache, not a prerequisite for using the
manifest API.

---

### 3. Adaptive Weather Collection/Materialisation *(Priority: High; next implementation family)*

**Problem:** `get_weather()` currently chooses `fast` versus `bounded` mostly from caller settings. Large future workloads materialize substantial shared relations and returned frames, while small workloads benefit from one batch collect. The correct tradeoff is workload- and RSS-dependent.

**Implementation-ready work packets:**

- [x] **W3-A: profile the current weather stages.** Opt-in instrumentation now records parquet/cache reads, H3 harmonisation/weights, `loc_monthly`, `loc_weather_base`, climate reference, CMIP6 historical aggregation, delta materialisation, future perturbation, rolling, collection, cleanup timing, DuckDB table names/counts, collected rows/frame and serialized bytes, lazy-query SQL size, and process-tree RSS/deltas. Focused LKA local profiles cover historical, one-SSP/one-period, and two-SSP/two-period workloads with exact parity and zero leftover `lw_*` tables. External `/usr/bin/time -l` peak-RSS characterization remains the deployment gate.
- [x] **W3-B: benchmark shared CMIP6 historical materialisation.** Compare current lazy `h3_hist_raw` inside `.process_ssp()` with one materialized monthly historical relation outside the SSP loop. Local LKA and India tests produced exact output hashes, matching warnings/model exclusions, and no stale `lw_*` tables. The latest isolated LKA rerun found shared historical slightly slower across historical-only, one-SSP/one-period, and two-SSP/two-period cases (medians: 1.35s vs 1.32s, 5.42s vs 5.10s, and 14.52s vs 14.21s). External `/usr/bin/time -l` peak memory for the complete isolated matrix was 1.631 GB versus 1.626 GB current. India's earlier larger run showed a repeatable elapsed-time signal, while the LKA Databricks run showed only a small warmed-run gain; no consistent RSS benefit is demonstrated. Keep shared historical opt-in and do not enable it by default pending broader production-scale evidence.
- [x] **W3-C: benchmark future-period reuse.** Compare current per-period `.cmip6_h3_monthly()` branches with one monthly SSP relation plus period assignment/filtering. LKA disjoint and overlapping-period tests preserved exact hashes, ordering, boundaries, warnings, and cleanup, but India's larger workload made `shared_period` about 23% slower than current and raised peak RSS from about 3.1 GB to 6.7 GB. Reject `shared_period` as currently implemented; retain the current per-period path and do not add adaptive selection for it.
- [ ] **W3-D: implement the winning weather plan.** Keep `fast` for small workloads. Shared historical materialisation is parity-safe but did not win the latest isolated LKA timing or peak-RSS comparison. Do not implement `shared_period` or change defaults; consider only a bounded shared-historical path after production-scale local and remote RSS gates show a material end-to-end gain.
- [ ] **W3-E: validate adaptive selection.** Matrix: small/large country × one/multiple variables × historical/one SSP/many SSP-periods × local/remote. Acceptance requires no regression beyond measurement noise for small workloads and lower/equal peak RSS for large workloads.

Relevant code: `R/fct_get_weather.R:1016-1073`, `1284-1459`, `1493-1603`; existing options are `weather_collect` and `weather_threads`. Preserve canonical member order and `weather_consumer` metadata.

**Stop condition:** `shared_period` failed the larger-country RSS/time gate and is rejected as currently implemented. Retain the current per-period path. Shared historical materialisation remains opt-in only; if it does not improve end-to-end time without exceeding the RSS budget at production scale, retain the current path and do not add adaptive complexity.

**Shared-historical adoption risks:** Materialising `h3_hist_raw` adds a large DuckDB temporary relation and can increase peak process RSS, especially when the relation overlaps with future deltas, returned member frames, or other cached tables. It may also increase spill/temp-storage pressure, change remote parquet scan and network behavior, and retain resources longer on error paths. The latest isolated LKA benchmark showed exact parity, no stale tables, median shared-historical times of 1.35s/5.42s/14.52s versus 1.32s/5.10s/14.21s current, and external peak memory of 1.631 GB versus 1.626 GB current. The Databricks smoke benchmark showed only a small warmed-run gain with higher sampled RSS. These results do not support changing the default.

---

### 4. Survey-Weather Join Reduction *(Priority: High investigation; compact index rejected)*

**Problem:** The rejected cache retained a full survey projection and regressed construction time. The normal per-key path still rebuilds timestamp/key fields, selects columns, expands duplicate keys, and converts factors.

**Work packets:**

- [x] Instrument the actual inline join in `R/fct_simulations.R:568-576`; separately time timestamp preparation, key construction, `inner_join`, duplicate expansion, and factor conversion.
- [x] Benchmark a compact integer/radix weather-key index or precomputed weather-side key vectors. Preserve NA sentinels, duplicate survey rows, weather row order, factor levels, and unmatched-row behavior.
- [x] Measure per-key elapsed time, joined rows, match ratio, object bytes, and process-tree RSS for OLS/RIF, historical/future, one/many keys.
- [ ] Implement only if repeated join/key preparation is a material share of end-to-end runtime and the compact representation lowers RSS.

**Benchmark result:** the compact integer index was tested on the production
LKA all-years workload with two SSPs, two periods, OLS, and coefficient
uncertainty disabled. Across 63 keys, inline joins took 109.9--112.0 seconds
for cold runs and 107.7--113.5 seconds for warm runs. The compact cache took
117.1--119.4 seconds for cold runs and 119.0--119.9 seconds for warm runs,
approximately 7--11% slower. External peak RSS was 5.88 GB inline versus
7.07 GB cached. The compact implementation is therefore rejected and has been
reverted; the existing `join_cache` remains opt-in for compatibility only.
Both modes completed all 63 keys, but serialized result sizes differed (2.685 GB
inline versus 2.525 GB cached), so this benchmark does not establish
byte-for-byte output parity for the cache path.

The benchmark harness now supports the reproducible
`two_ssps_two_periods` workload alias. No alternative join function is enabled
without a new production-shaped benchmark showing both a runtime or RSS win
and exact output parity.

Do not reintroduce the full survey join cache without a new design that avoids retaining the full projection.

---

### 5. Prediction-Core Replay and RIF Delta *(Priority: Medium–High; prediction-heavy workloads)*

**Problem:** Future OLS prediction sums to approximately 18–22 seconds across 17 keys; RIF prediction is also material. Repeated uncertainty/display changes can recompute deterministic work.

**Work packets:**

- [x] Instrument `run_sim_pipeline()` per key: actual join, `predict_outcome`/`predict_rif`, design matrix, factor loading, post-prediction assembly, joined rows, and payload bytes. Add opt-in RIF-specific traces; do not use the old `prepare_hist_weather` proxy.
- [ ] Characterize a replay product containing joined row mapping, `y_point`, `sim_year`, weights, IDs, and required context. Define cache identity separately for model, weather manifest, residual mode, policy mode, log scale, and active coefficient mask. Defer implementation until production-scale traces show deterministic replay work is material.
- [x] Characterize an exact RIF weather-only paired-delta path using existing direct-RIF design deltas × coefficients for supported models. Synthetic parity passed for direct and fallback paths, but direct RIF was approximately 6% slower on the 800-row, nine-quantile fixture; retain the current oracle and do not add another delta path without a production-scale win.
- [ ] Implement the replay cache or RIF delta only after focused parity, ordering, missingness, deterministic RNG, Step 3 context, and RSS tests.

Relevant code: `R/fct_simulations.R:532-832`, `R/fct_rif_sim.R:458-695`, `R/fct_predict_outcomes.R:143-190`.

**Item 5 characterization:** Opt-in `WISEAPP_PREDICTION_PROFILE=1` traces record join mode/rows, prediction, design matrix, factor loading, RIF quantile assignment, baseline frame, direct pair, per-quantile prediction, factor-loading interpolation, assembly, frame bytes, and process-tree RSS. The synthetic benchmark is `dev/bench_prediction_core.R`; corrected results are in `dev/outputs/prediction-core/summary.csv`. OLS inline and compact-cache outputs are exact, but compact caching was approximately 34% slower. Direct and fallback RIF outputs are exact, but direct RIF was approximately 6% slower. These are characterization results, not a basis for a new default optimization.

---

### 6. Memory Retention and Copy Reduction *(Priority: Medium; measure first)*

**Work packets:**

- [ ] Measure retained sizes of `cached_weather`, `weather_raw` in each pipeline, joined frames, `F_loading`, and final payloads at `R/fct_run_simulation.R:520-703` and `R/fct_simulations.R:755-832`.
- [ ] Prototype reference/streaming retention only where diagnostics, Step 3, replay, and member provenance do not require the full frame.
- [ ] Benchmark narrower `fixest` prediction-frame handling and RIF `newdata_base`/`delta_mat` allocations; preserve FE row-drop and row-ID contracts.

Do not apply generic in-place/data.table changes without exact payload and ordering tests.

---

### 7. Bounded Key-Level Parallelism *(Priority: High potential / High risk; characterize only)*

**Prerequisite:** Complete W3 and join/prediction instrumentation. Serial future runs already reach approximately 4–7+ GiB process RSS.

**Work packets:**

- [ ] Run a two-worker experiment only, historical key serial-first, future keys in canonical order, worker-local DuckDB connections, parent-side result ordering.
- [ ] Compare wall time, process-tree peak RSS, remote I/O, failures, warnings, exact hashes, and deterministic RNG against serial execution for OLS/RIF and uncertainty on/off.
- [ ] Implement only if two workers provide a material wall-time gain without exceeding the configured RSS budget.

Do not implement immediately or infer safety from R object sizes.

---

### 8. Point-Estimate-Only Fast Path *(Priority: Medium–Low; implemented)*

When coefficient uncertainty is disabled, Cholesky/factor-loading construction is already skipped. Check whether uncertainty-specific aggregation, `F_agg` structures, and payload serialisation are also fully elided under `skip_coef_draws`. If not, make this an explicit fast path.

**Implementation:** deterministic point-estimate aggregation (`residuals = "none"` or
`"original"`) now bypasses welfare-gradient construction, coefficient variance,
and uncertainty-band transforms through `aggregate_point_estimate()`. Stochastic
residual modes retain the delta path because residual variance requires the
gradient. The single-method and multi-method aggregation paths share the fast
path, preserving the existing result schema with zero variance and point-valued
bands. Focused aggregation and W3-A characterization tests pass.

---

### 9. Further RIF Vectorisation *(Priority: Medium for RIF-heavy / Low for OLS-heavy; implemented)*

Direct RIF baseline reuse already yielded 3.15× improvement. Remaining targets: scenario design-matrix construction, quantile interpolation, per-key factor loading, repeated row-ID propagation. Do not trade away exact row ordering, quantile endpoints, or policy correction parity.

**Implementation:** `predict_rif()` now streams quantile deltas directly into the
interpolated result. It groups rows by the quantile interval they use, evaluates
only the required rows for each quantile, and avoids materialising the full
`N x K` delta matrix. Direct fixed-effect prediction accepts the same row groups,
preserving the existing coefficient and fixed-effect assembly contract. Endpoint,
ordering, direct/fallback, and full-matrix oracle tests pass. The remaining
`F_loading` interpolation path is intentionally unchanged because its grouped
row strategy already bounds peak memory and preserves exact factor-loading
parity.

---

## Recommended Investigation Order

1. **W3 weather profiling and reuse benchmarks** — retain stage instrumentation and characterize shared CMIP6 historical materialisation at production scale; do not pursue shared future-period materialisation in its current form.
2. **Adaptive weather collection/materialisation** — implement only a shared-historical or bounded hybrid plan if it passes elapsed-time and RSS gates; preserve the current fast path for small workloads.
3. **Actual survey-weather join profiling** — test compact key/index reuse; do not revive the rejected full survey cache.
4. **Prediction-core replay** — only if production-scale per-key traces show deterministic replay work is material; the current RIF paired-delta characterization is parity-safe but slower on the synthetic fixture.
5. **Memory retention/copy reduction** — target measured payload and prediction-frame copies.
6. **Bounded key parallelism characterization** — two workers only after join/prediction RSS is known; implementation requires a clear RSS-safe gain.

---

## Conclusion

Safe local optimisations are largely exhausted. Meaningful remaining speedups require:

1. Reusing prepared weather across runs and model simulations.
2. Removing repeated CMIP6 aggregation and choosing weather materialisation by workload.
3. Reusing deterministic prediction work where uncertainty/display settings do not change it.
4. Reducing measured join/payload allocations without changing ordering or public contracts.
5. Parallelizing keys only if process-tree RSS and deterministic parity permit it.

W3-A through W3-C are complete on local LKA and larger India workloads. The next
agent should characterize shared historical materialisation at production scale,
but should not implement shared-period reuse as currently designed. Implement W3-D
only when a candidate wins both elapsed-time and RSS gates. The survey join and prediction
packets are parallel investigations, not permission to change those paths without
characterization. Every implementation must run focused parity tests, the full
package suite, `git diff --check`, and cleanup checks for temporary `lw_*` tables.
