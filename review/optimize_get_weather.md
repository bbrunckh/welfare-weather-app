# Speed opportunities: weather pipeline (`fct_get_weather.R`)

**Status: candidate — unvalidated. Consider for implementation; not enforced (see `optimization_guidelines.md`). Objective: raw speed; validate on the Section 9 payloads.**
**Update 2026-09-18: #1–#3 implemented and validated (see ADOPTED markers + new traps); #4–#6 deferred/rejected.**

Ranked by expected impact. Read the current file first — several optimizations from the
original proposal are already in (ZSTD disk cache + eviction, vectorized `roll_exprs`
with `FILTER`, configurable thread pinning); the traps at the end list prior regressions
to avoid repeating.

## 1. Disk-cache the location-month table (new) — ADOPTED 2026-09-18 (extended)

The heaviest work — h3 spatial join + population weighting + rolling windows —
re-executes on every `get_weather()` call even when the raw remote slices are cached.
Extend the existing `.wx_cache_load()` pattern one level up: COPY the materialized
`loc_monthly` (post-join, pre-transformation) to the disk cache, keyed by
(survey fnames, weather vars, date span, h3 target resolution). Interactive re-runs
(changed scenario params, same surveys/vars) then skip the join entirely.

Risk: cache invalidation must include everything the join consumes. Reuse the existing
cache dir, tmp+rename race handling, and LRU eviction.

## 2. Vectorize `.apply_transformations()` (~591–604) — ADOPTED 2026-09-18

One `dplyr::mutate()` per variable in a `for` loop re-translates SQL per variable.
Replace with a single `mutate(!!!trans_exprs)` built once from `dbplyr::sql()` strings.
Gain scales with the number of transformed variables. Low risk.

## 3. Simplify `.pop_weighted_mean()` (~1088–1102) — ADOPTED 2026-09-18, parity-corrected

Replace the triple `if_else`/`across` NA-guard with
`SUM(var * pop_2020) / SUM(pop_2020)` — SQL NULL handling excludes missing values from
both terms, removing per-row branching in the hottest join. Verify all-NA group parity
(R returns `NA_real_`; SQL yields NULL → same after `collect()`, but confirm on real data).

## 4. Conditional materialization — DEFERRED (quantify-first, unchanged)

`dplyr::compute()` writes 4–5 temp tables per call. Materialize only when a relation has
≥2 consumers (multi-batch reuse is deliberate, PERF-02); use lazy views otherwise
(single-batch runs). Benchmark the multi-period × multi-SSP worst case before changing
any shared site — re-executing a view per batch can be slower than one materialization.

## 5. Window `MEDIAN` cost — DEFERRED (quantify share first, unchanged)

`MEDIAN(...) OVER (ROWS BETWEEN …)` sorts per window frame (O(n log n) per frame) while
AVG/MIN/MAX are linear. If "Median" temporal aggregation is common in real payloads,
quantify its share before micro-optimizing elsewhere.

## 6. Micro / high-risk — REJECTED for now (unchanged rationale)

- Cache key `digest::digest` → `rlang::hash` (~309): micro; changes cache filenames.
- In-DB breakpoints (`quantile_cont()`): different algorithm than `stats::quantile()`
  (type 7) and must reproduce seeded `kmeans()` — parity break risk vs
  `test-determinism.R`; only with explicit parity work.

## Traps

- Do **not** drop the `FILTER (WHERE x IS NOT NULL)` clause from `roll_exprs`.
- Do not hardcode `SET threads TO 1` — the `weather_threads` argument owns pinning.
- Never adopt rewrites that drop the future/SSP + CMIP6 pathway (an earlier proposal
  version did).
- **#3 as originally written is NOT parity-safe.** `SUM(var * pop_2020) / SUM(pop_2020)`
  includes the population of NA rows in the denominator; the app's semantics exclude it.
  Partial-NA groups occur on the CMIP6 delta path (deltas are not pre-filtered the way
  historical weather vars are). Adopted form keeps the non-NA denominator:
  `SUM(x * pop) / SUM(if_else(!is.na(x), pop, 0))` + the `> 0` all-NA guard — 2
  aggregates instead of ~4 per-variable passes.

## Adopted evidence (2026-09-18)

- **#1 (extended)**: both `h3_weights` (the h3 file scan + population summarise) and
  `loc_monthly` (post-join, post-pop-weight, pre-rolling) are disk-cached via
  `.wx_loc_cache_store()/.wx_loc_cache_load()/.wx_loc_cache_key()` in
  `fct_get_weather.R`. Key covers both cache version constants, weather + h3 fnames,
  weather vars, date span, and the harmonised H3 resolutions. Applies to local backends
  too (the join cost does not depend on the parquet location); honours
  `WISEAPP_WEATHER_CACHE_DISABLE`; shares tmp+rename race handling and LRU eviction with
  the raw cache. Cache-hit path materialises the parquet into a fresh temp table, so all
  downstream consumers are unchanged.
- **#2**: one `mutate(!!!trans_exprs)` replacing the per-variable loop.
- **Validation**: 515 tests PASS with the cache enabled *and* disabled (the enabled run
  caught a real bug: the cache probe must not overwrite `h3_slim` on miss). W3 harness
  (`dev/bench_w3_weather.R`, LKA 2012+2016, threads=1): result SHA hashes identical
  baseline vs modified on all four cases (historical, one_ssp_one_period,
  two_ssps_two_periods, boundary_overlap). Cold == warm output hashes (IRN).
- **Compute timings** (harness disables the loc cache → isolates #2/#3; LKA small
  payload): historical 1.25–1.45 → 1.50–1.73 s; one_ssp 4.71–5.00 → 4.85–5.05 s;
  two_ssps×two_periods 13.80–14.71 → 13.89–14.41 s; boundary_overlap 7.40–8.14 →
  7.66–8.23 s — within noise; #2/#3 are small wins at this scale, no regression.
- **Cache timings** (IRN 2009+2020, one_ssp, warm): 2.40–2.66 s (disabled) →
  1.91–2.27 s (~20–25% faster re-runs); cold run pays one ZSTD COPY per cached table
  (~3 s on IRN). Stage profile: h3_weights 0.122 s + loc_monthly 0.556 s → ~0.14 s of
  cache-hit loads. Larger relative wins expected on remote backends (raw fetch skipped
  on warm runs via the existing raw cache).
- **Peak RSS** (IRN, `/usr/bin/time -l`, one_ssp): 814 MB (warm) vs 780 MB (disabled) —
  within allocator noise; survey data dominates. Cache footprint after tests + LKA +
  IRN keys: 20 MB (budget default 2048 MB, shared LRU eviction).
