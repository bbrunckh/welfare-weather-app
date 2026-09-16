# Performance Audit — Remaining Work

**Audit revision:** `aa42136` | **Dev head:** S2-P9 (`df7d31b`, following S2-P6) | **Date:** 2026-09-14

---

## Status

| Wave | Batch | Findings | Status |
|---|---|---|---|
| 0 | W0-A | S3-P12 | Integrated |
| 1 | W1-A | P3, P7, P8, P14, P16 | Integrated |
| 1 | W1-B | P2, P9 | Integrated |
| 1 | W1-C | P1, P5 | Integrated |
| 1 | W1-D | S2-P1, S2-P2, S2-P3, S2-P11, S2-P12 | Integrated |
| 1 | W1-E | S3-P2, S3-P3, S3-P5 | Integrated |
| 2 | W2-A | P6, P11 | Integrated |
| 2 | W2-B | S2-P4, S2-P5, S2-P8, S2-P14 | Integrated |
| 2 | W2-C | S2-P7, S2-P10 | Integrated |
| 2 | W2-D | S3-P4, S3-P7, S3-P10, S3-P13 | Integrated |
| 2 | W2-E | S3-P6, S3-P8, S3-P9, S3-P11, S3-C1 | Integrated |
| **3** | **W3-A** | **S2-P21, S2-P22** | **Integrated** |
| **3** | **W3-B** | **S3-P1** | **Integrated** |
| **3** | **S2-P6** | **Bounded Results aggregation cache** | **Integrated** |
| **3** | **S2-P9** | **Reference-weather store ownership** | **Integrated** |
| **3** | **S2-P20** | **Shared RIF design and FE indexing** | **Integrated** |
| **3** | **S3-P15** | **Shared per-year row indexing through aggregation preparation cache** | **Integrated** |

S2-P6, S2-P9, S2-P20, S2-P17, and S3-P15 are integrated. P15 is removed from the authorized scope and is not required for this delivery. S2-P16 has an implemented, opt-in characterization path; its automatic two-thread rollout is gate-failed for the current delivery because the focused evidence does not meet the speed gate and remote execution regresses. Production remains pinned to one thread.

---

## 1. Wave 3

Start all worktrees from the Wave 2 integrated revision.

### W3-A — Step 2 Aggregation Throughput

**Files:** `R/fct_aggregation.R`, `R/fct_aggregation_delta.R`, `R/mod_2_02_results.R`

**S2-P6 dependency:** S2-P6 must be resolved before or alongside W3-A. If approval (§2) is concurrent, integrate S2-P6 first. Otherwise W3-A must include a minimal scoped cache bound as a prerequisite.

#### S2-P21 — Lazy Weighting Arms

`R/mod_2_02_results.R`

Every method eagerly computes both weighted and unweighted results. `weight_key()` always selects the weighted branch for surveys with weights, so the unweighted arm is built and cached with no active consumer in normal use.

**Change:** Compute each arm lazily on first request. Retain the unweighted fallback when no weight is available and for verified export/compatibility consumers. Do not change the nested `weighted`/`unweighted` schema.

**Impact:** Up to ~50% of aggregation CPU and transient allocation for weighted workloads.

#### S2-P22 — Shared Per-Year Preparation

`R/fct_aggregation.R`, `R/fct_aggregation_delta.R`

Each method and arm independently repeats year grouping, row slicing, validity filtering, residual matching, back-transformation, and factor-loading slicing.

**Change:** Build a bounded preparation cache per pipeline containing year row indices, valid-row masks, normalized weights, and deterministic per-year residual vectors. Reuse across methods; do not cache `N x P` method-specific matrices. Pair with the S2-P6 bound so this cache cannot grow unbounded.

**Impact:** Moderate-to-high during multi-method Results sessions.

#### W3-A Gate

- Both weighting arms available on demand; values, gradients, residual pairing, and per-year RNG preserved exactly across all methods and residual modes for Steps 2 and 3. **Passed:** strict parity covered 72 method/arm/residual cases; the focused W3-A characterization suite passed 42 assertions.
- Preparation cache stays O(N) per pipeline and bounded. **Passed:** the shared preparation cache is bounded at 32 entries and the Results cache at 8 entries; cache-bound and lazy-arm tests are included in the characterization suite.
- Full package test suite passed: 2,589 assertions, 0 failures. Results tests passed 149 assertions, and aggregation-delta tests passed 37 assertions.
- Commit: `d7d8153` (`Optimize Step 2 aggregation caching`).

---

### W3-B — Step 3 Compact Future Decomposition

**Files:** `R/mod_3_06_policy_sim.R`, Step 3 payload/decomposition helpers and tests

**Dependency:** Start from the W3-A integrated revision.

#### S3-P1 — Compact Future Decomposition Retention

`R/mod_3_06_policy_sim.R`, `R/mod_3_09_decomposition.R`

The scenario/year loop row-binds a full 21-column household frame into `decomp_scenarios_rv`. At 231,087 rows this is 34.4 MB per year and ~426.7 MB for one 11-year scenario; multiple scenarios scale linearly. The UI consumes only: weighted channel summaries by scenario/year, channel-by-decile summaries, the per-household-weighted `delta_total` annual summary needed to rank and select adverse years (not a scalar — requires enough per-year information to reproduce the current weighted ranking for 1-in-10 and 1-in-20 selection), and scenario labels.

**Change:** Inside the loop, derive and immediately retain those four compact products, then release the full household frame before the next year. Keep the full historical household decomposition (used by Results incidence, decile display, and adverse recomputation). Any export of future household rows must be contracted explicitly before compaction proceeds.

**Impact:** Very high retained and transient memory reduction; also avoids a large final `bind_rows()`.

#### W3-B Gate

- Channel, scenario, year, model, and decile summaries and all exports identical to pre-compaction output. **Passed:** compact-vs-legacy characterization and export parity covered OLS and RIF; the W3-B suite passed 55 assertions.
- No downstream consumer requires discarded household rows (verified by the consumer audit and module/export contract tests). Historical `decomp_result` remains separate and unchanged.
- Substantial retained/serialized-size reduction demonstrated on an 11-year, one-scenario representative fixture: `object.size()` `203,358,848` to `28,080` bytes; serialized size `223,692,667` to `20,833` bytes. Controlled external max RSS was `1,223,786,496` bytes before and `725,811,200` bytes after.
- Fixed weighted baseline deciles, adverse-year selection, channel SE summaries, factor/bin behavior, scenario ordering, and empty-result behavior are unchanged under the contract tests.
- Full package suite passed 2,644 assertions with 0 failures. Commit: `7766333` (`Compact future decomposition retention`).
- Timing caveat: the isolated retention benchmark was slower (`0.009s` legacy vs `0.274s` compact median on 11 years x 50,000 rows); this is a memory-retention optimization, not a demonstrated elapsed-time optimization. Production end-to-end timing remains a follow-up.

---

### S3-P15 — Shared Per-Year Row Index

`R/fct_aggregation_delta.R`, `R/fct_aggregation.R`, Step 3 Results and policy comparison consumers

**Status:** Integrated through the W3-A shared aggregation path; no separate implementation is required.

**Validation:** `aggregate_pipeline_per_year()` obtains a bounded preparation cache containing per-year row indices, validity masks, normalized weights, and residual vectors. `aggregate_pipeline_table()` creates one cache per aggregation operation and passes it to every model pipeline. Step 3 Results passes `ws$prep_cache` for historical and scenario results; policy comparison passes shared context and the same table helper for baseline and scenario consumers; baseline/policy central-kernel paths also use the shared per-year aggregator. Focused W3-A characterization, policy comparison/cache, policy central-kernel, and Step 2 payload contract suites passed. A 550,000-row, 11-year probe measured `2.04s` for 100 repeated direct year scans versus `0.465s` using the prepared row indices. The cache is bounded and shared across weighting arms/methods without caching method-specific `N x P` matrices.

---

### Wave 3 Integration Order

1. Resolve S2-P6 or implement the minimal scoped cache prerequisite.
2. Integrate and validate W3-A.
3. Integrate and validate W3-B against the W3-A revision.
4. Run the complete Step 1–3 suite and the production-sized benchmark matrix.

---

## 2. Authorized but Blocked

These were authorized in the delivery plan but excluded from Wave 2 for insufficient test coverage. S2-P6 and S2-P9 are now integrated. No remaining item in this section is approved or required for this delivery.

### Scope Decision

P15 — Stable Weather Map Surfaces — is removed from the authorized scope. It is not required or approved for implementation in this delivery.

### S2-P6 — Bounded Results Aggregation Cache

`R/mod_2_02_results.R`, `R/fct_aggregation.R`

**Status:** Integrated in `6553bab` (`Complete bounded Results cache lifecycle`).

**Validation:** bounded LRU, method switching, poverty-line/bandwidth changes, export after eviction, stale-result display, recomputation parity, clear, and session-end lifecycle tests pass.

**Change:** Bounded LRU or current-plus-default cache; evict older poverty-line/bandwidth variants; expose object-size instrumentation in the benchmark harness.

**Note:** W3-A dependency is resolved.

### S2-P9 — Reference-Weather Store Ownership

`R/fct_run_simulation.R`, `R/mod_2_01_weathersim.R`, `R/mod_2_simulation.R`

**Status:** Integrated in `df7d31b` (`Complete reference weather store ownership`).

**Validation:** success, partial/total failure, rerun replacement, clear/session cleanup, historical-only behavior, and leased Step 3-style references are covered. A referenced store cannot be removed until all leases are released.

**Change:** One run-level store owner with explicit references from every published result. Clean the superseded store on atomic replacement only when Step 3 holds no reference. Historical-only runs must avoid creating an orphaned store or retain it for cleanup.

### S2-P20 — Shared RIF Design and FE Indexing

`R/fct_rif_sim.R`, RIF prediction tests, and the Step 2 benchmark harness

**Status:** Integrated in `237a433` (`Optimize shared RIF prediction design`).

**Investigation:** The current direct RIF path repeats fixed-effect matching, coefficient-column selection, and lazy design construction across nine quantiles. Controlled benchmarks showed approximately 1.9x lower per-key elapsed time and 48% lower allocation with shared FE indices/designs; tested outputs were bit-identical. Existing shared RIF preparation already improves 300k-row preparation by about 1.5x and roughly halves allocation versus repeated preparation.

**Implementation scope:** Add characterization tests first, then use a shared FE index and shared non-FE design where supported. Preserve the current fallback for unsupported fixest structures, public payloads, quantile ordering, and coefficient uncertainty behavior. Do not use the diagnostic stacked-dgemm variant unless parity and memory gates require it.

**Validation gate:** Bit-identical parity for all nine quantiles and both baseline/scenario designs; unsupported-model fallback coverage; focused RIF tests; full package suite; and a production-sized Colombia RIF Step 2 benchmark reporting end-to-end timing separately from weather-loading time, allocations, and RSS.

**Validation results:** Focused RIF, cross-step aggregation, and full package suites passed. A 50,000-row fixed-effect comparison measured `41.5 ms` and `94.2 MB` allocation for the prior repeated-design path versus `16.5 ms` and `50.4 MB` for the shared path, with exact output parity. The production Colombia RIF benchmark reached the real weather workload but exceeded the available run window before completion; weather loading dominated the observed end-to-end run, so production-scale end-to-end speedup and RSS remain a follow-up gate.

### S2-P17 — Materialized Location-Month Weather

`R/fct_get_weather.R`

**Status:** Integrated in `e1404d4` (`Materialize location-month weather relations`); remote validation recorded in `9b928a7`.

**Investigation:** Historical `loc_monthly` and per-SSP location-month delta relations remain lazy and are re-executed across future periods. A 3-SSP x 3-period workload can repeat the spatial aggregation roughly 10 times for historical weather and 18 times for CMIP6 delta construction/completeness evaluation. Controlled local probes support an estimated 2–4x weather-query reduction for the full 3x3 workload, with a possible 60–120 MB longer-lived DuckDB footprint.

**Implementation gate:** Confirm plans with `EXPLAIN ANALYZE`, materialize once per call/SSP with bounded cleanup, prove bit-level parity, and measure local/remote elapsed time and process-tree RSS before treating the optimization as passed.

**Validation results:** Multi-period weather tests and the full package suite passed. Historical location-month weather and each SSP's complete period-tagged location-month delta relation are materialized once, with per-period intermediates cleaned up after collection. A controlled Colombia parquet experiment reduced repeated three-period spatial aggregation from `0.826s` to `0.315s` at one thread (`2.62x`), `0.484s` to `0.218s` at two threads (`2.22x`), and `0.366s` to `0.193s` at four threads (`1.90x`). The remote Databricks Colombia 2018 two-period app path returned `47/47` exact output matches and no remaining `lw_*` temporary tables; cold uncached elapsed time was `72.14s` legacy versus `69.52s` materialized (`1.04x`). Remote I/O dominates the end-to-end path, so the larger isolated gain does not transfer directly to wall time.

### S2-P16 — Bounded DuckDB Thread Scaling

`R/fct_get_weather.R`

**Status:** Implemented as an opt-in characterization path; automatic rollout is gate-failed for the current delivery. The public default is `weather_threads="auto"`, which selects one thread. Explicit `weather_threads="2"` remains available for targeted testing only; `threads=1` is the production default and fallback.

**Investigation:** On a local IND workload, weather aggregation medians were 16.1s at one thread, 9.8s at two, 7.7s at four, and 6.0s at eight. Peak RSS increased from 1.66 GB at one thread to 2.36 GB at four and 2.47 GB at eight. Multi-threaded output differed from the one-thread baseline by approximately 1e-13 in final floating-point bits.

**Step 1 — weather-output determinism policy:** Adopt canonical weather values rather than requiring DuckDB's parallel floating-point reduction to be bit-identical. The fixed policy is `round(..., digits = 5)` for finite selected weather values. Apply it after weather-side SQL aggregation, rolling, transformation, and perturbation have been collected, but before historical bin-break computation or `cut()`. Apply the same policy to every returned historical/future frame and the `continuous_weather` attribute; leave keys, dates, bin labels, missingness, and model metadata unchanged. The policy rounds weather covariates only, never fitted coefficients, welfare outputs, standard errors, or other downstream estimates. The precision is a code-level contract, not a user setting, so thread mode cannot create different cache or payload identities. The 5-decimal policy bounds direct rounding error at `5e-6` per finite weather value.

**Step 2 — precision characterization:** Compare full-precision one-thread output with two-thread output at candidate precisions of 3 and 5 decimal places. The acceptance comparison is rounded one-thread versus rounded two-thread output, with exact equality required for values, row/key ordering, names, model completeness, warnings/ledger behavior, `stored_breaks`, bin labels, and `continuous_weather`. A focused production-shaped local Colombia weather-only probe found approximately `1.0e-12` maximum raw drift and zero differing weather cells at both 3 and 5 decimals; 5 decimals is retained because it is less aggressive while still removing the observed drift. The focused matrix covered one variable, historical and one compact future period, continuous and binned output, additive and multiplicative perturbations, and bounded collection without model/pipeline work; all cases passed at both precisions. This characterization is sufficient for the current rollout decision; broader aggregators, cache states, downstream OLS/RIF, and representative Databricks coverage are not required unless S2-P16 is reopened.

**Step 3 — bounded two-thread rollout:** Add an explicit bounded runtime control allowing only one or two DuckDB threads, defaulting to one. Two-thread execution is opt-in and may be selected only when the deployment has at least two entitled CPUs and the projected/observed process-tree RSS remains within the configured weather budget; invalid or higher values must not be accepted. Preserve the existing connection-level `threads` restoration on every exit path. Run the complete weather characterization with two threads first, while retaining one thread as the automatic/default fallback when CPU or RSS gates are not met. Require at least a 1.2x local weather-construction improvement, peak RSS within the deployment budget and no more than 1.25x the one-thread reference target, and no more than 5% remote elapsed-time regression. If only local/cached workloads pass, do not enable two threads globally for remote backends. Add tests for mode restoration, repeated-call determinism, rounded one-/two-thread parity, cache cold/warm identity, historical/future and binned outputs, and fallback behavior. Until all numerical, scientific-distortion, CPU, RSS, and backend gates pass, production remains pinned to one thread.

**Implementation:** `get_weather()` now accepts `weather_threads="auto"`, `"1"`, or `"2"`; `fct_run_simulation()` and the Step 2 benchmark/UI plumbing forward the mode. The selector is bounded at two threads, requires a local backend, two entitled CPUs, a large estimated workload, and RSS headroom for enabled `auto`; `WISEAPP_WEATHER_THREADS_AUTO_ENABLE` is deliberately off by default. Returned weather covariates are rounded to 5 decimal places before binning, with keys, missingness, dates, and metadata unchanged. Focused selector, forwarding, determinism, rounding, and one-/two-thread parity tests pass; the full package suite passes. The dedicated local Colombia weather-only benchmark completed in under one second for historical `t`: `0.558s` at one thread versus `0.756s` at two threads, with 8,364 rows. One compact future period was faster at two threads (`3.045s` versus `3.480s` additive; `2.996s` versus `3.571s` multiplicative/binned), but the approximately `1.14x` gain is below the required `1.2x` gate and the historical workload regresses. A cold uncached Databricks Colombia 2018 two-period run returned 47 outputs in `109s` at one thread, `123s` at explicit two threads, and `87.22s` at `auto` (which correctly selected one thread because the backend was remote); remote output parity and temporary-table cleanup remained exact. Automatic two-thread rollout is therefore gate-failed and remains disabled.

## 3. Not Authorized

These require a separate authorization decision. No implementation without explicit approval.

### Step 1

| ID | Finding | Potential benefit | Blocked by |
|---|---|---|---|
| P4 | Compact retained fit-snapshot | 237.6 MB → ~63.7 MB illustrative projection; high session RSS saving when stale fit coexists with new weather data | Needs persisted schema decision |
| P10 | Grouped outcome map aggregation | Low-to-moderate CPU/allocation for large map inputs | Outside Sets A–D |
| P12 | Consolidated LASSO candidate matrix | Low-to-moderate setup gain; cross-validation dominates elapsed time | Outside Sets A–D |
| P13 | Direct RIF coeftable extraction | Moderate result-preparation speedup if VCV fallback probing is material | Unresolved numerical-parity risk |
| P17 | Deduplicated weather SQL denominators | Low-to-moderate; textual duplication may not imply duplicate execution — needs `EXPLAIN ANALYZE` first | Conditional on query-plan evidence |

### Step 2

| ID | Finding | Potential benefit | Blocked by |
|---|---|---|---|
| S2-P13 | Remove deep copy at async boundary | Potentially high startup peak-RSS reduction when async is enabled; no gain on current synchronous path | Async backend not yet selected |
| S2-P15 | Key-level parallelism | Wall-clock reduction when prediction dominates; may be net loss from serialization overhead and duplicated model/survey memory | Deferred until post-Sets 2A/2B RSS is known |
| S2-P17 | Materialized location-month weather | Potentially high for 3-SSP/3-period runs (spatial aggregation paid once); memory impact ambiguous — depends on temporary-table lifetime | Needs query-plan comparison and RSS measurement |
| S2-P18 | Shared historical weight denominators | Moderate with many weather variables; low with one or two; DuckDB may already eliminate repeated expressions | Needs plan inspection and complete-case verification |
| S2-P19 | Fused future-model completeness filter | Moderate-to-high if the query plan executes the H3/delta relation twice | Needs query-plan confirmation |
| S2-P20 | Shared RIF design and FE indexing | Potentially high CPU reduction for fixed-effect RIF; point-prediction and loading-reuse variants need separate benchmarks due to peak-RSS trade-off | Needs production RIF benchmark |

### Step 3

| ID | Finding | Potential benefit | Blocked by |
|---|---|---|---|
| S3-P14 | Lazy paired policy pipelines | Potentially substantial retained-memory reduction for large baselines and many members; limited CPU benefit | Needs benchmark fixture and serialized-payload compatibility tests |
| S3-P15 | Per-year row index | ~1.2x–2x for long panels; avoids O(N × years) logical scans per method | Integrated through the bounded shared aggregation-preparation cache |
| S3-P16 | Direct model/year lookup | Near-free to implement; prevents quadratic growth with more years/models | Overlaps shared aggregation path — implement once for both |
| S3-P17 | Typed policy-diff pass | Moderate allocation reduction; removes duplicated comparison logic across policy construction, no-op detection, decomposition, and Diagnostics | Deferred; should replace S3-P2 caches, not coexist |
| S3-P18 | Direct residual ID lookup | ~1.05x–1.2x isolated; compounds across aggregation methods | Overlaps shared aggregation path — implement once for both |

---

## 4. Delivery Rules

1. Read the complete finding, equivalence requirements, and validation plan before editing.
2. Write characterization tests for current behavior before any compute or schema rewrite; cover edge cases from the audit, not only the happy path.
3. Make the smallest compatible change. No new runtime dependencies, no renamed public inputs/outputs, no broadened scope without coordinator approval.
4. Preserve: canonical ordering, factor levels, duplicate-key behavior, deterministic RNG, failure ledgers, stale-result preservation, member-specific weather.
5. Run focused tests and `git diff --check`. Report changed files, timings, allocations or RSS, and residual risk.
6. Stop and report rather than silently relaxing semantics when strict parity fails.
7. Do not modify this document from implementation worktrees.

The coordinator integrates only passing batches, reruns cross-step contracts after shared helper changes, and creates each wave from the newly integrated revision.

---

## 5. Acceptance Gate

Complete when:

- every remaining authorized ID is implemented or recorded as gate-failed with evidence;
- `git diff --check` and the full test suite pass;
- cold/warm Step 2 and Step 3 benchmarks record elapsed time, allocation, retained/serialized size, and external process-tree RSS;
- coverage includes: small and large country, OLS and RIF, historical-only and future scenarios, compact and reference weather, uncertainty on and off;
- production data is read-only; no benchmark artifact enters the production source;
- Step 2 payload/replay, Step 3 Results/Diagnostics/Decomposition/stale state/exports all pass end-to-end;
- no unauthorized item, new runtime dependency, or unrelated refactor is included.
