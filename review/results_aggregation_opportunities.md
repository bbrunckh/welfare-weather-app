# Results Aggregation: Next Work

## Current Status

The multi-method aggregation path is implemented for weighted and unweighted
Step 2 and Step 3 Results. It computes the standard method suite together,
shares preparation and per-year work, and preserves the existing public cache
contracts.

Real LKA benchmark, nine weighted methods, 652,680 rows and 30 years:

| Path | Median | Allocation |
|---|---:|---:|
| Existing method-by-method path | 1.56 s | 3.05 GB |
| Multi-method path | 0.57 s | 1.06 GB |

Focused and full test suites pass. The implementation is committed in
`6c132b7` and `34016d3`.

## Next Candidate

### Shared Step 2 Baseline Reuse in Step 3

Test whether the historical baseline aggregation prepared for Step 2 can be
reused by Step 3 without changing outputs or cache invalidation.

Measure:

- Step 2 Results followed by Step 3 baseline Results
- Cold and warm elapsed time
- Preparation-cache hits and misses
- Peak process RSS
- Exact baseline values, uncertainty, gradients, and ordering
- Step 3 policy contrasts and decomposition outputs

Implemented in `6bc4e57`. A bounded session cache shares compact historical
aggregation suites between Step 2 and Step 3. The original Step 2 run
signature and all aggregation controls are part of the cache key; policy arms
remain separate. Focused and full tests pass.

## Recent Optimisations

### Factor-loading blocks

Per-year `F_loading` slices are now prepared once and reused across methods
when coefficient uncertainty is enabled. A focused 80,000-row, ten-year
probe reduced repeated allocation from `48.6 MB` to `22.5 MB` and elapsed time
from `15.3 ms` to `13.2 ms`. Focused parity tests pass.

### Indexed ensemble lookup

Per-model aggregation results are now indexed by year before assembly. A
12-model, 30-year probe reduced lookup time from `6.23 ms` to `0.82 ms`
(`7.6x`), with exact first-result semantics for duplicate years.

## Next Candidates

1. Reduce cache-key hashing only if profiling shows large-vector digests are
   material.
2. Persist compact aggregate tables for fixed batch/replay workflows.
3. Revisit compact internal result representation if list allocation is shown
   to dominate after these changes.

## Do Not Pursue Yet

- Broad replacement of base R functions with `collapse` or `kit`; the main
  gain came from shared work, not individual function substitution.
- Eager pre-aggregation that removes lazy control handling; poverty line,
  bandwidth, residual, weighting, and uncertainty settings remain interactive.
- Further sorting optimisation until profiling confirms sorting is material
  after the multi-method implementation.
