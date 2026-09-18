# Step 2 Optimization Decision Record

**Date:** 2026-09-08
**Status:** Consolidated authoritative record.
**Scope:** Step 2 weather loading, simulation pipelines, result retention,
aggregation, diagnostics, and Step 3 consumers.

## Production Defaults

The application now uses the following defaults:

```text
payload_mode            = "compact"
weather_storage         = "memory"
weather_collect         = "fast"
direct_rif_predictions  = TRUE
join_cache              = FALSE
parallel execution     = deferred
```

Legacy behavior remains available where explicitly supported:

- `payload_mode = "legacy"`;
- `weather_storage = "reference"` for memory-constrained deployments;
- `weather_collect = "bounded"` for lower-RSS collection;
- `direct_rif_predictions = FALSE` for diagnosis/fallback comparison.

The rejected survey join cache and row-batched RIF prediction paths remain
disabled. No Step 2 key-level parallelism is enabled.

## Implemented Improvements

### Compute and contracts

- Added the pure `step2_compute()` boundary with input validation, immutable
  snapshots, RNG restoration, process-local DuckDB initialization, stage events,
  and run/signature metadata.
- Added characterization tests for result fields, row alignment, model-specific
  weather, failure-ledger semantics, deterministic output, and downstream
  consumers.
- Historical failure and whole-group failure remain fatal; partial ensemble
  failures remain publishable with requested/succeeded counts and a ledger.

### Compact payload

- Moved duplicated residual metadata to `shared_context`.
- Retained `y_point`, `F_loading`, row IDs, alignment fields, and every model's
  own `weather_raw`.
- Updated Results, aggregation, comparisons, diagnostics, and policy consumers.
- Compact serialized reductions measured approximately 14% (LKA), 33% (Iran),
  and 34% (India) in historical OLS workloads.

### Future-weather lifecycle

- Released temporary DuckDB delta, perturbation, and rolling tables earlier.
- Added fast one-collect and bounded model-wise collection strategies.
- Added run-scoped reference-backed weather as an opt-in memory escape hatch;
  it is not the production speed path.

### Weather SQL and parquet work

- H3 mapping scans project only `h3`, survey identifiers, and `pop_2020`, with a
  fallback for older mappings without population weights.
- SSP scans are row-pruned to the baseline/future date envelope and column-pruned
  to model, H3, timestamp, and selected weather variables.
- Incomplete models are filtered before location-delta materialization.
- Normalized population-weighted H3 location mappings are materialized once and
  reused across historical/future joins.
- Shared SSP-wide monthly materialization was tested and removed because it
  increased multi-SSP memory pressure and still hit the R vector limit.

### RIF prediction

- Direct fixed-effect prediction is enabled for supported standard RIF models.
- Quantile coefficient and fixed-effect metadata is precomputed once per
  simulation and reused across members.
- Each member retains its own weather-dependent design matrix.
- Unsupported structures and unseen fixed-effect levels automatically fall back
  to `fixest::predict()`.

## Measured Results

### Weather projection pruning

LKA, one SSP/period, OLS, compact payload, in-memory weather, fast collection,
17 successful keys:

| Measure | Before | After |
|---|---:|---:|
| Median elapsed | 34.27 s | 29.09 s |
| Median weather | 19.41 s | 15.95 s |
| Successful keys | 17/17 | 17/17 |

### SSP row pruning

LKA, same workload after projection pruning:

| Measure | Before | After |
|---|---:|---:|
| Median elapsed | 29.09 s | 24.88 s |
| Median weather | 15.95 s | 13.25 s |
| Median pipeline | 10.22 s | 8.87 s |
| Successful keys | 17/17 | 17/17 |

India OLS completed 17/17 keys with approximately 96 seconds median elapsed and
6.43 GiB sampled RSS in the one-SSP/one-period workload.

### Direct RIF prediction

India, one SSP/period, uncertainty enabled, 17 successful keys:

| Measure | Fixest prediction | Direct RIF |
|---|---:|---:|
| Median elapsed | 323.11 s | 257.33 s |
| Median pipeline | 256.89 s | 202.14 s |
| Sampled RSS | 6.72 GiB | 6.51 GiB |
| Successful keys | 17/17 | 17/17 |

After metadata reuse, LKA direct-RIF median elapsed time improved from 65.94 to
54.99 seconds and median pipeline time from 46.36 to 37.78 seconds. The LKA
metadata run showed higher RSS, so the India-scale memory envelope remains an
operational constraint.

## Rejected Experiments

- **Survey join cache:** increased LKA pipeline time from approximately 11.5 to
  18.5 seconds and increased RSS.
- **Row-batched RIF prediction:** increased RIF pipeline time approximately
  threefold.
- **Reference weather as default:** reduced memory but increased elapsed time by
  roughly 80% due to per-member disk reads.
- **Shared SSP monthly cache:** worked for one SSP/three periods but increased
  multi-SSP retention and hit the 16 GB R vector limit.
- **Broad collapse/kit substitution:** isolated primitives were fast, but exact
  production aggregation replacements did not improve runtime.
- **Initial mirai prototype:** failed at captured-closure worker serialization;
  it was removed before any production enablement.

## Correctness and Testing

The relevant weather, transformation, preparation, RIF, simulation, policy,
contract, payload, aggregation, and diagnostics suites pass after the accepted
changes. Structural fields remain exact: keys, ordering, dimensions,
missingness, labels, counts, and failure semantics. Numeric outputs may use the
predeclared absolute/relative tolerance policy when floating-point evaluation
order changes.

The existing `fixest` `train_aug` warning/fallback remains environment-specific
and should be resolved or explicitly accepted before using fine-grained CPU
comparisons as a deployment gate.

## Deferred Work

Parallel execution is intentionally deferred. The next parallel attempt, if
needed, must use a top-level worker-safe function, explicit serialized inputs,
worker-owned DuckDB connections, deterministic per-key seeds, serialized errors,
parent-side canonical ordering, and an externally enforced process-tree RSS
budget. Start with two workers only after the serial production baseline is
validated under deployment conditions.

Further optimization priority, if required:

1. Reduce direct-RIF metadata/F-loading RSS without giving back its speed gain.
2. Resolve the `train_aug` environment warning.
3. Validate compact-default Step 3 policy and diagnostics workflows in the
   deployment environment.
4. Reconsider mirai only if serial latency remains unacceptable after those
   checks.
