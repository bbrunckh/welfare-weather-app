# Speed opportunities: weather pipeline (`fct_get_weather.R`)

**Status: candidate — unvalidated. Consider for implementation; not enforced (see `optimization_guidelines.md`). Objective: raw speed; validate on the Section 9 payloads.**

Ranked by expected impact. Read the current file first — several optimizations from the
original proposal are already in (ZSTD disk cache + eviction, vectorized `roll_exprs`
with `FILTER`, configurable thread pinning); the traps at the end list prior regressions
to avoid repeating.

## 1. Disk-cache the location-month table (new)

The heaviest work — h3 spatial join + population weighting + rolling windows —
re-executes on every `get_weather()` call even when the raw remote slices are cached.
Extend the existing `.wx_cache_load()` pattern one level up: COPY the materialized
`loc_monthly` (post-join, pre-transformation) to the disk cache, keyed by
(survey fnames, weather vars, date span, h3 target resolution). Interactive re-runs
(changed scenario params, same surveys/vars) then skip the join entirely.

Risk: cache invalidation must include everything the join consumes. Reuse the existing
cache dir, tmp+rename race handling, and LRU eviction.

## 2. Vectorize `.apply_transformations()` (~591–604)

One `dplyr::mutate()` per variable in a `for` loop re-translates SQL per variable.
Replace with a single `mutate(!!!trans_exprs)` built once from `dbplyr::sql()` strings.
Gain scales with the number of transformed variables. Low risk.

## 3. Simplify `.pop_weighted_mean()` (~1088–1102)

Replace the triple `if_else`/`across` NA-guard with
`SUM(var * pop_2020) / SUM(pop_2020)` — SQL NULL handling excludes missing values from
both terms, removing per-row branching in the hottest join. Verify all-NA group parity
(R returns `NA_real_`; SQL yields NULL → same after `collect()`, but confirm on real data).

## 4. Conditional materialization

`dplyr::compute()` writes 4–5 temp tables per call. Materialize only when a relation has
≥2 consumers (multi-batch reuse is deliberate, PERF-02); use lazy views otherwise
(single-batch runs). Benchmark the multi-period × multi-SSP worst case before changing
any shared site — re-executing a view per batch can be slower than one materialization.

## 5. Window `MEDIAN` cost

`MEDIAN(...) OVER (ROWS BETWEEN …)` sorts per window frame (O(n log n) per frame) while
AVG/MIN/MAX are linear. If "Median" temporal aggregation is common in real payloads,
quantify its share before micro-optimizing elsewhere.

## 6. Micro / high-risk

- Cache key `digest::digest` → `rlang::hash` (~309): micro; changes cache filenames.
- In-DB breakpoints (`quantile_cont()`): different algorithm than `stats::quantile()`
  (type 7) and must reproduce seeded `kmeans()` — parity break risk vs
  `test-determinism.R`; only with explicit parity work.

## Traps

- Do **not** drop the `FILTER (WHERE x IS NOT NULL)` clause from `roll_exprs`.
- Do not hardcode `SET threads TO 1` — the `weather_threads` argument owns pinning.
- Never adopt rewrites that drop the future/SSP + CMIP6 pathway (an earlier proposal
  version did).
