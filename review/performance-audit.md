# Performance Audit - Handoff

**Last integrated code:** `5ce2f23` (`Materialize future deltas before completeness`)
**Date:** 2026-09-16
**Scope:** Step 2 weather/prediction/aggregation and Step 3 consumers.
**Rule:** Do not implement a candidate without characterization, parity, RSS, and cleanup evidence.

## Current State

| Area | Status | Evidence |
|---|---|---|
| W0-W2 findings | Integrated | Historical remediation; no open work here |
| W3-A / S2-P21, S2-P22 | Integrated | Lazy arms, bounded shared preparation/Results caches; strict parity; `d7d8153` |
| W3-B / S3-P1 | Integrated | Compact future decomposition; export parity; RSS `1.224 GB -> 0.726 GB`; `7766333` |
| S2-P6 | Integrated | Bounded Results cache lifecycle; `6553bab` |
| S2-P9 | Integrated | Reference-weather ownership/leases/cleanup; `df7d31b` |
| S2-P17 | Integrated | Materialized location-month weather; exact remote parity; `e1404d4`, validation `9b928a7` |
| S2-P19 | Integrated | Materialized `loc_deltas_all` before completeness; exact parity/cleanup; `5ce2f23` |
| S2-P20 | Integrated | Within-key and cross-key direct-RIF baseline design/FE reuse; exact parity; working-tree validation |
| S3-P15 | Integrated | Per-year indices through shared aggregation preparation cache |
| S2-P16 | Gate-failed for automatic rollout | Five-decimal weather contract; `auto` and production remain at one thread |
| P15 | Removed | Not authorized for this delivery |

## Integrated Evidence

- **W3-A:** 72 strict method/arm/residual parity cases; preparation cache bounded at 32 entries and Results cache at 8; full suite passed at integration (`2,589` assertions).
- **W3-B:** OLS/RIF compact/export parity; serialized size `223.7 MB -> 20.8 KB`; full suite passed at integration (`2,644` assertions).
- **S2-P17:** Local isolated three-period aggregation improved `2.62x` at one thread. Remote Colombia app path had exact `47/47` parity and no leftover `lw_*` tables; cold elapsed `72.14s -> 69.52s` (`1.04x`).
- **S2-P19:** Local one-variable matrix improvements: `24.5%` for 1 SSP/1 period, `25.3%` for 1 SSP/3 periods, `27.4%` for 2 SSPs/1 period, `29.6%` for 2 SSPs/3 periods. Databricks one SSP/one period improved `70.96s -> 65.0s` (`8.4%`), with exact `24`-output/`200,736`-row parity. Materialization has higher RSS risk and needs peak-RSS measurement before broader rollout.
- **S2-P16:** Five-decimal rounding made one/two-thread weather outputs equal across focused historical/future continuous/binned additive/multiplicative cases. Future local gain was only about `1.14x`; historical and remote workloads regressed. Automatic two-thread selection stays disabled.
- **S3-P15:** Shared preparation cache covers Step 3 Results, policy comparison, and central-kernel consumers. A `550,000`-row/11-year probe reduced repeated year scans `2.04s -> 0.465s` over 100 runs.
- **Cross-key direct-RIF baseline reuse:** One-entry per-run cache retains only baseline design/index objects; scenario design remains key-specific. A 50,000-row training/10,000-row expanded nine-quantile probe improved five-key preparation from `0.082s -> 0.026s` (`3.15x`), with one miss and five hits. Baseline/scenario designs were each approximately `0.15 MB`. Focused direct/fallback parity, factor/FE invalidation, unsupported-model fallback, Step 2 contract/payload, and full package tests passed.

## Next Candidates

### 1. Selective Survey-Side Join Cache

**Location:** `R/fct_run_simulation.R`, `R/fct_simulations.R`
**Status:** Gate-failed for automatic enablement; explicit opt-in remains available.

The cache preserves exact fixture output, duplicate-key expansion, canonical ordering, and NA-key behavior. However, corrected isolated probes show cache construction dominates repeated joins: at `231,087` survey rows and `8,364` weather rows, five uncached joins took `0.078s` versus `0.786s` including cache construction. At `250,000` survey rows, five joins took `0.074s` uncached versus `0.776s` cached. The retained cache was approximately `44-48 MB`, compared with an `11-12 MB` survey in these probes. A prior LKA full-pipeline comparison also regressed from approximately `11.5s` to `18.5s` and increased RSS. External isolated process-tree RSS was effectively flat (`525.9 MB` cached versus `522.7 MB` uncached), but did not offset the elapsed-time and retained-memory costs.

**Decision:** Do not add thresholded automatic enablement. Keep `join_cache = FALSE` by default and retain the path only for deliberate workload-specific experiments. A future redesign should avoid retaining the full survey projection, or amortize construction through a longer-lived shared key index, before reopening this candidate.

### 2. PERF-15 Prediction-Matrix Reuse

**Location:** `R/fct_simulations.R`, `R/fct_predict_outcomes.R`
**Status:** Deferred characterization from the independent review.

Investigate reuse of the RIF-first prediction/design representation between simulation and prediction paths. Keep `predict.fixest()` as the oracle until row dropping, offsets, factor levels, exclusions, and downstream outputs are proven equivalent.

### Additional Independent Step 2 Review

These are unimplemented follow-ups from an independent read-only review of the prediction and policy paths.

**Highest-confidence, investigate first:**

- Remove the dead RIF weather copy in `R/fct_rif_sim.R`; it allocates an `N x weather-columns` object that is not needed for restoration.
- Add a no-copy aligned-design fast path when `model.matrix()` columns already exactly match coefficient order.
- Propagate already-computed residual variance into aggregation instead of recomputing `var()` per year.
- Avoid full covariance reconstruction when selecting active RIF blocks; operate directly on the relevant Cholesky block after orientation validation.

**Potentially valuable but contract-sensitive:**

- Cache RIF-policy survey projections across keys; preserve `.svy_row_id`, factor levels, and policy columns.
- Cache policy deltas across keys and reuse location grouping across hazard variables; preserve NA, tie, fallback, and invalidation behavior.
- Stream RIF delta interpolation or replace repeated quantile-pair scans with one grouping pass; preserve endpoint, interval, and row-order semantics.
- Pre-index RIF coefficient curves to avoid repeated filtering and `approx()` calls inside weather/term loops.

**Lower priority:**

- Reduce shared-payload resolution copies.
- Enable central-only policy correction when downstream consumers do not require variance/SE vectors.

Each candidate requires a focused microbenchmark, exact output/parity checks, and peak-RSS measurement before implementation. No candidate is authorized by this section.

### Deferred or Deprioritized

| Candidate | Decision |
|---|---|
| S2-P18 shared historical denominators | Deprioritized; multi-variable weather workloads are uncommon. |
| S2-P15 key-level parallelism | Deferred; possible future investigation after serial prediction/RSS characterization. Do not implement now. |
| S3-P16 direct model/year lookup | Defer standalone; low absolute cost, include only in shared aggregation cleanup. |
| S3-P14/S3-P17/S3-P18 | Separate Step 3 work; require their own authorization and characterization. |
| S2-P13 async deep-copy removal | No current synchronous-path benefit; revisit only after an async backend exists. |

## Delivery Rules

1. Characterize current output, warnings, ordering, missingness, failure ledger, and cleanup before changing computation.
2. Preserve canonical ordering, factor levels, duplicate-key behavior, deterministic RNG, member-specific weather, and public payloads.
3. Use focused weather-only or prediction-only benchmarks; do not use the broad Step 2 harness for candidate triage.
4. Report cold/warm elapsed time, allocations or object size, retained/serialized size where relevant, external process-tree RSS, and output parity.
5. Production data is read-only. Do not leave benchmark artifacts or temporary tables.
6. Run focused tests, the full package suite, and `git diff --check` before integration.

## Acceptance Gate

- Every pursued candidate is integrated or explicitly gate-failed with evidence.
- Focused and full tests pass with no unauthorized scope or runtime dependency.
- Coverage includes small/large country, OLS/RIF, historical/future, compact/reference weather, uncertainty on/off where relevant.
- Step 2 payload/replay and Step 3 Results/Diagnostics/Decomposition/stale-state/export contracts pass.
- Remote and local cleanup leaves no temporary `lw_*` tables.
