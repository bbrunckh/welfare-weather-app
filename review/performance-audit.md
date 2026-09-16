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
| S2-P20 | Integrated | Within-key direct-RIF design/FE reuse; exact parity; `237a433` |
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

## Next Candidates

### 1. Cross-Key Direct-RIF Baseline Reuse

**Location:** `R/fct_rif_sim.R`, `R/fct_simulations.R`
**Status:** Characterization candidate; extension of S2-P20.

`.direct_rif_design_cache()` still rebuilds baseline `model.matrix()`, fixed-effect indices, and coefficient-column mappings for every weather key. A 50,000-row, nine-quantile probe measured `0.269s` for five per-key preparations versus `0.075s` when baseline structures were reused.

**Next gate:** Prove bit-identical nine-quantile baseline/scenario parity, unsupported-model fallback, factor/FE edge cases, and peak RSS. Retain only the baseline design/index objects; scenario design remains key-specific.

### 2. Selective Survey-Side Join Cache

**Location:** `R/fct_run_simulation.R`, `R/fct_simulations.R`
**Status:** Existing opt-in path; default remains disabled.

For Colombia 2018, `8,364` future weather rows joined to `231,087` projected survey rows took `0.516s` normally versus `0.269s` with the cache over five joins. Cache overhead was approximately `1 MB` over the retained `122 MB` projected survey. An older LKA full-pipeline test regressed with the cache, so the isolated win is not sufficient.

**Next gate:** Small/large country, historical/future, OLS/RIF, one/many keys, cold/warm, and external process-tree RSS. If consistently positive, use a workload threshold rather than unconditional enablement.

### 3. PERF-15 Prediction-Matrix Reuse

**Location:** `R/fct_simulations.R`, `R/fct_predict_outcomes.R`
**Status:** Deferred characterization from the independent review.

Investigate reuse of the RIF-first prediction/design representation between simulation and prediction paths. Keep `predict.fixest()` as the oracle until row dropping, offsets, factor levels, exclusions, and downstream outputs are proven equivalent.

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
