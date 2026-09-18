# Optimization work tracking

Status per item: `☐` todo · `◐` in progress · `☑` done (merged + benchmarked).
Log entries at the bottom (date, batch, result). Benchmarks per `optimization_guidelines.md` §9.

## Batch 1 — Shared infrastructure (serial, one agent, prerequisite)

- ☐ Data fetch path: DuckDB → Arrow → `data.table` for `feols()` (guidelines §3)
- ☐ Weather pipeline opportunities (`review/optimize_get_weather.md`): loc-month disk cache, vectorized transformations, pop-weight SQL, conditional materialization
- ☐ Welfare statistics: one-sort-per-group — `collapse` grouped vs Rcpp kernel (`review/optimize_aggregation_welfare.md`)
- ☐ RIF bandwidth (`review/optimize_rif.md`) — only if profiling shows it hot

## Batch 2 — Module UI migrations (parallel, one agent per module, worktrees)

- ☐ Tables → `reactable` + client-side CSV download (guidelines §6) — enumerate target modules at start
- ☐ Charts → `echarts4r` + proxies (guidelines §7) — enumerate target modules at start
- Maps excluded (`fct_hexmap.R` bridge is final).

## Batch 3 — Integration (serial)

- ☐ `devtools::document()` (roxygen conflicts resolve by re-run, not hand-merge)
- ☐ Full test suite + `devtools::check()`
- ☐ §9 benchmarks, both payloads, before/after timings + peak memory recorded here

Merge order: Batch 1 first → Batch 2 branches rebase onto it → Batch 3.

## Log

- 2026-09-18 — Tracking started; guidelines §0 + proposal docs (`review/optimize_*.md`) committed.
