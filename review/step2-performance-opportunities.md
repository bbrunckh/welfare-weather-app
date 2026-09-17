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

- [ ] **W3-A: profile the current weather stages.** Add temporary or benchmark-only timers around parquet/cache reads, H3 harmonisation/weights, `loc_monthly`, `loc_weather_base`, climate reference, CMIP6 historical aggregation, delta materialisation, perturbation/rolling, and collection. Record rows, serialized frame bytes, DuckDB table names, elapsed time, process-tree RSS, and cleanup.
- [ ] **W3-B: benchmark shared CMIP6 historical materialisation.** Compare current lazy `h3_hist_raw` inside `.process_ssp()` with one materialized monthly historical relation outside the SSP loop. Test one and multiple SSPs, local/remote, exact output hashes, warnings, model exclusions, elapsed time, RSS, and no stale `lw_*` tables.
- [ ] **W3-C: benchmark future-period reuse.** Compare current per-period `.cmip6_h3_monthly()` branches with one monthly SSP relation plus period assignment/filtering. Test disjoint and overlapping periods, one/many SSPs, exact duplicate/order/boundary parity, elapsed time, RSS, and cleanup.
- [ ] **W3-D: implement the winning weather plan.** Keep `fast` for small workloads. Add a bounded per-period or hybrid path when estimated rows/bytes exceed a configured budget. Do not change default behavior until W3-B/W3-C pass parity and RSS gates.
- [ ] **W3-E: validate adaptive selection.** Matrix: small/large country × one/multiple variables × historical/one SSP/many SSP-periods × local/remote. Acceptance requires no regression beyond measurement noise for small workloads and lower/equal peak RSS for large workloads.

Relevant code: `R/fct_get_weather.R:1016-1073`, `1284-1459`, `1493-1603`; existing options are `weather_collect` and `weather_threads`. Preserve canonical member order and `weather_consumer` metadata.

**Stop condition:** If W3-B/W3-C do not improve end-to-end time or RSS, retain the current path and do not add adaptive complexity.

---

### 4. Survey-Weather Join Reduction *(Priority: High investigation; not the rejected full join cache)*

**Problem:** The rejected cache retained a full survey projection and regressed construction time. The normal per-key path still rebuilds timestamp/key fields, selects columns, expands duplicate keys, and converts factors.

**Work packets:**

- [ ] Instrument the actual inline join in `R/fct_simulations.R:568-576`; separately time timestamp preparation, key construction, `inner_join`, duplicate expansion, and factor conversion.
- [ ] Benchmark a compact integer/radix weather-key index or precomputed weather-side key vectors. Preserve NA sentinels, duplicate survey rows, weather row order, factor levels, and unmatched-row behavior.
- [ ] Measure per-key elapsed time, joined rows, match ratio, object bytes, and process-tree RSS for OLS/RIF, historical/future, one/many keys.
- [ ] Implement only if repeated join/key preparation is a material share of end-to-end runtime and the compact representation lowers RSS.

Do not reintroduce the full survey join cache without a new design that avoids retaining the full projection.

---

### 5. Prediction-Core Replay and RIF Delta *(Priority: Medium–High; prediction-heavy workloads)*

**Problem:** Future OLS prediction sums to approximately 18–22 seconds across 17 keys; RIF prediction is also material. Repeated uncertainty/display changes can recompute deterministic work.

**Work packets:**

- [ ] Instrument `run_sim_pipeline()` per key: actual join, `predict_outcome`/`predict_rif`, design matrix, factor loading, post-prediction assembly, joined rows, and payload bytes. Add RIF-specific traces; do not use the old `prepare_hist_weather` proxy.
- [ ] Characterize a replay product containing joined row mapping, `y_point`, `sim_year`, weights, IDs, and required context. Define cache identity separately for model, weather manifest, residual mode, policy mode, log scale, and active coefficient mask.
- [ ] Characterize an exact RIF weather-only paired-delta path using design deltas × coefficients for supported models. Keep the current oracle for factors, offsets, unsupported formulas, FE edge cases, policy mode, and parity failures.
- [ ] Implement the replay cache or RIF delta only after focused parity, ordering, missingness, deterministic RNG, Step 3 context, and RSS tests.

Relevant code: `R/fct_simulations.R:532-832`, `R/fct_rif_sim.R:458-695`, `R/fct_predict_outcomes.R:143-190`.

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

### 8. Point-Estimate-Only Fast Path *(Priority: Medium–Low)*

When coefficient uncertainty is disabled, Cholesky/factor-loading construction is already skipped. Check whether uncertainty-specific aggregation, `F_agg` structures, and payload serialisation are also fully elided under `skip_coef_draws`. If not, make this an explicit fast path.

---

### 9. Further RIF Vectorisation *(Priority: Medium for RIF-heavy / Low for OLS-heavy)*

Direct RIF baseline reuse already yielded 3.15× improvement. Remaining targets: scenario design-matrix construction, quantile interpolation, per-key factor loading, repeated row-ID propagation. Do not trade away exact row ordering, quantile endpoints, or policy correction parity.

---

## Recommended Investigation Order

1. **W3 weather profiling and reuse benchmarks** — profile stages, then test shared CMIP6 historical materialisation and monthly future-period reuse.
2. **Adaptive weather collection/materialisation** — implement only the winning W3 plan; preserve the current fast path for small workloads.
3. **Actual survey-weather join profiling** — test compact key/index reuse; do not revive the rejected full survey cache.
4. **Prediction-core replay or RIF paired-delta** — choose based on per-key traces and exact replay/delta characterization.
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

The next agent should start with W3-A through W3-C, then implement W3-D only when a
candidate wins both elapsed-time and RSS gates. The survey join and prediction
packets are parallel investigations, not permission to change those paths without
characterization. Every implementation must run focused parity tests, the full
package suite, `git diff --check`, and cleanup checks for temporary `lw_*` tables.
