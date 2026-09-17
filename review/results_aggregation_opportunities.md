# Results Aggregation Opportunities

## Context

Steps 2 and 3 share the core aggregation path: `aggregate_pipeline_table()` → `aggregate_pipeline_per_year()` → `aggregate_with_uncertainty_delta()`. Step 2 uses it in `mod_2_02_results.R`; Step 3 via `fct_policy_sim_compare.R`.

**Already integrated:** shared preparation cache, cached residuals/variance/year indices, lazy weighted/unweighted arms, bounded Step 2 and Step 3 caches, analytic uncertainty, compact pipeline context. Repeated year scans reduced from 2.04s → 0.465s on a 550,000-row probe.

**Benchmark note:** `dev/bench_step2.R` timed out on OneDrive metadata reads and did not reach aggregation. Conclusions below are based on code inspection and existing audit evidence, not new timings. Use a local synced data copy for future benchmarks.

**Pre-aggregation verdict:** Do not replace lazy aggregation with eager pre-aggregation. Users change poverty line, bandwidth, weighting, residual mode, and uncertainty settings interactively; Step 3 requires `F_agg` gradients for contrasts, not just `value/lower/upper`. Use tiered pre-aggregation instead (see Opportunity 8).

---

## Opportunities

### 1. Multi-Method Single-Pass Aggregation *(Priority: High)*

**Problem:** `aggregate_pipeline_table()` is called once per method. Each call loops over all years and recomputes shared quantities (valid-row subset, `mu = exp(y_point + residual)`, weight totals, normalised weights) independently.

**Solution:** Process each pipeline/year once, computing all requested methods together. Share: row filtering, exponentiation, weights, and `F_loading' * h` calculations. Methods still have distinct gradients (median, Gini, headcount), but row-level work is done once.

**Constraint:** Keep lazy method arms for interactive rendering. The multi-method evaluator is for batch/precompute paths, or triggered lazily after the first method has forced preparation.

---

### 2. Shared Sorting for Median and Gini *(Priority: Medium–High)*

**Problem:** Weighted median and Gini each sort welfare independently per year.

**Solution:** Sort once per year; reuse the sorted order and cumulative weights for both. Also reuse poverty masks and weighted totals across headcount, gap, and FGT2.

**Constraint:** Sorting semantics must stay identical — NA/non-finite filtering, weight handling, zero/negative weights, tie handling, weighted median definition, and Gini formula.

---

### 3. Avoid Repeated `F_loading[idx, ]` Copies *(Priority: Medium–High when uncertainty enabled)*

**Problem:** Every method/year subsets `F_loading` as `F_idx <- F_full[idx, , drop = FALSE]`, a potentially expensive allocation across many years, methods, and ensemble members.

**Solution:** Compute `F_agg = F_loading_year' %*% h` via direct indexed cross-product without materialising a copied slice, or compute all method gradients against one slice before releasing it.

**Constraint:** Verify floating-point ordering is unchanged. R matrix ops may be faster on contiguous slices — benchmark before committing.

---

### 4. Reuse Step 2 Baseline Aggregation in Step 3 *(Priority: Medium)*

**Problem:** Step 3 re-aggregates the historical baseline independently, despite it being derived from the Step 2 result.

**Solution:** Expose a shared prepared baseline object (per-year indices, `mu`, residual draws, weight metadata, baseline `F_agg`) reusable across Step 2 Results, Step 3 baseline comparison, and decomposition consumers.

**Constraint:** Only the truly identical baseline arm should be shared; Step 3 policy arms may use different residual settings or uncertainty semantics.

---

### 5. Compact Immutable Per-Year Prepared State *(Priority: Medium)*

**Problem:** Per-year computation still repeatedly subsets `pipe$y_point[idx]`, `F_loading[idx, ]`, and `prep$weights[[i]]` across methods.

**Solution:** Extend the preparation cache with compact per-year objects: precomputed `mu`, weighted/unweighted denominators, `F_loading'` transposed representation, and per-year finite-row indices. Distinguish poverty-independent quantities from poverty-line-dependent and bandwidth-dependent quantities so invalidation behaviour is preserved.

---

### 6. Cache-Key Identity Instead of Full Digest *(Priority: Medium — measure first)*

**Problem:** `.aggregation_preparation_key()` digests large vectors (`sim_year`, `y_point`, `weight`, `id_vec`, residuals) on every cache access across methods and weighting arms.

**Solution:** Key on immutable pipeline/run identity already present in the payload (pipeline member ID, residual mode, seed, log flag) instead of hashing full vectors.

**Constraint:** Identity must be immutable and attached at pipeline publication time; a weak key risks stale preparation.

---

### 7. Persist Fixed-Run Aggregate Tables *(Priority: Medium for batch; Low for single interactive session)*

**Problem:** Session-local caches are lost on rerun or new session.

**Solution:** Persist compact per-year aggregate outputs (point values, coefficient/residual variance, ensemble spread, `F_agg`, provenance) keyed by an immutable signature covering: run, pipeline, method, weighting, poverty line, bandwidth, residual mode, uncertainty mode, schema version.

**Constraint:** Cache **results only**, not household pipelines. Requires cache invalidation, filesystem coordination, and privacy review.

---

### 8. Tiered Pre-Aggregation *(Priority: Low–Medium)*

Rather than eager pre-aggregation (incompatible with interactive controls), use three tiers:

- **Tier 1 (always):** Retain compact prepared per-year state — indices, masks, `mu`, weights, residual variance, invariant gradients.
- **Tier 2 (optional warm-up):** After publishing the first result, warm the default `mean` method and optionally the full standard suite in the background.
- **Tier 3 (batch only):** Persist complete aggregate tables when the run configuration is fixed and fully captured by a signature.

---

### 9. Indexed Ensemble Assembly *(Priority: Low–Medium)*

**Problem:** `aggregate_pipeline_table()` scans all model results per year with `vapply(..., identical(x$sim_year, year), ...)` for every method and scenario.

**Solution:** Index each model's results as `model → year → result` once; ensemble assembly becomes a direct lookup. More relevant for Step 3 with many policy scenarios and ensemble members.

---

### 10. Compact Internal Result Representation *(Priority: Low)*

**Problem:** Each per-year aggregation constructs a named R list (point value, bands, variance, gradient, `draw_values = NULL`, year), then `aggregate_pipeline_table()` repackages into list-columns — repeated across many years, members, methods, and Step 3 arms.

**Solution:** Use compact numeric vectors/matrices internally during aggregation; build the public list-column schema once at the output boundary.

---

## Priority Ranking

| # | Opportunity | Value | Risk |
|---|---|---|---|
| 1 | Multi-method single-pass aggregation | High | Medium |
| 2 | Shared sorting (median + Gini) | Medium–High | Low |
| 3 | Avoid `F_loading` row copies | Medium–High (uncertainty on) | Medium |
| 4 | Reuse Step 2 baseline in Step 3 | Medium | Medium |
| 5 | Compact per-year prepared state | Medium | Medium |
| 6 | Cache-key identity | Medium (if hashing is material) | Medium |
| 7 | Persist fixed-run aggregate tables | High (batch) | Medium |
| 8 | Tiered pre-aggregation | Low–Medium | Low |
| 9 | Indexed ensemble assembly | Low–Medium | Low |
| 10 | Compact internal result representation | Low | Medium |

## Benchmark Plan

Run `dev/bench_step2.R` against a **local synced data copy** (not OneDrive). Benchmark aggregation independently from weather/prediction:

1. Run Step 2 once to produce a result payload; extract historical and future pipelines.
2. Time each method with fresh and warm preparation caches, weighted/unweighted, uncertainty off/on.
3. Time full nine-method suite aggregation.
4. Time Step 3 baseline, policy, and scenario aggregation separately.
5. Measure: elapsed time, allocation size, cache hit rates, peak process-tree RSS, exact output parity.

Dimensions: LKA and India-sized; historical-only and multi-SSP/period; OLS and RIF; one and many ensemble members; one and many policy arms.

## Conclusion

The strongest near-term aggregation experiment is a **single-pass multi-method evaluator with shared sorting and shared per-year state**, benchmarked locally. Reusing the Step 2 historical baseline in Step 3 is the clearest cross-module win. Persist aggregate tables only for fixed batch runs with a fully specified signature. Do not replace lazy aggregation with eager pre-aggregation.
