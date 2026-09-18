# Optimization work tracking

Status per item: `☐` todo · `◐` in progress · `☑` done (merged + benchmarked).
Log entries at the bottom (date, batch, result). Benchmarks per `optimization_guidelines.md` §9.

## Batch 1 — Shared infrastructure (serial, one agent, prerequisite)

- ☑ App startup + mod_0 (ad hoc, 2026-09-18): vendored map engine moved out of the `bundle_resources()` scan tree (`inst/app/vendor/`) — scripts now execute once per page load instead of twice (~1.2 MB duplicated JS parse/execute saved per session). Follow-ups logged below.
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
- 2026-09-18 — App startup + mod_0 pass (Batch 1, ad hoc):
  - **Implemented**: map engine single-serve (see checked item above); manifest regenerated for the new file set; `AGENTS.md` path updated. Tests: `test-fct_hexmap.R` (61), `test-step-badges.R` (44) pass; resolved page dependencies verified (`golem_resources` = `custom.js` only; `wiseapp-hexmap` = engine once, strict order).
  - **Measured, no action**: warm `app_ui()` build ~85 ms/session — dominated by htmltools tag machinery, not app code; `mod_0` server does no eager work at session start.
  - **Follow-up candidates** (not implemented, recorded for a later batch):
    - mod_0: move remote-source metadata load off the main thread via the existing mirai singleton (guidelines §8). Blocked by the credential-scrub contract for user-entered S3/GCS credentials — needs the scrub path extended first. First session per Connect process currently blocks on the Databricks HTTP fetch.
    - Hex engine lazy-load: only mod_1_05 renders map containers dynamically; mod_1_02/mod_1_03 containers are static so the engine loads at page start regardless. Would require restructuring those containers to actually defer.
  - Note: `R/fct_get_weather.R` carried unrelated in-flight work (loc-month cache) — left out of this commit.
