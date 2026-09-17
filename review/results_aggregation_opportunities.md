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

If parity is exact, implement a shared, run-scoped baseline aggregation object.
Reuse only identical baseline inputs; policy arms remain separate.

## Later Candidates

Only pursue these after baseline reuse is measured:

1. Avoid repeated `F_loading[idx, ]` allocations when coefficient uncertainty
   is enabled.
2. Index per-model results by year to avoid repeated ensemble-year scans.
3. Reduce cache-key hashing if profiling shows large-vector digests are costly.
4. Persist compact aggregate tables for fixed batch/replay workflows.

## Do Not Pursue Yet

- Broad replacement of base R functions with `collapse` or `kit`; the main
  gain came from shared work, not individual function substitution.
- Eager pre-aggregation that removes lazy control handling; poverty line,
  bandwidth, residual, weighting, and uncertainty settings remain interactive.
- Further sorting optimisation until profiling confirms sorting is material
  after the multi-method implementation.
