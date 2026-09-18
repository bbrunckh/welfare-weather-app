# R Shiny App Optimization Guidelines for Coding Agents

Standards for optimizing this R Shiny app (golem framework). **Goal: make the app as fast and responsive as possible**, for every concurrent session, not just the one triggering a given computation. The main risk to guard against: blocking the single-threaded main R process, and duplicating memory unnecessarily across reactive boundaries or worker processes.

These guidelines reflect current best practice and package versions as of September 2026 and should be revisited periodically, as this space (mirai/crew, polars-R, Arrow interop, DuckDB) is evolving quickly.

## 0. App-Specific Context

Architecture overview is in `AGENTS.md`; constraints it does not cover:

- No `global.R` — Section 8's one-time daemon-pool setup belongs in `run_app()` / top-level `app_server.R`.
- Ingestion is already DuckDB over `.parquet` (`fct_load_data.R`): optimize query/fetch paths, not storage format.
- Extend the existing process-wide mirai singleton in `fct_step2_async.R` for async work (queue memory cap, `try_mirai()`, `onStop()` teardown); never create a second pool. In dev, workers load the package via `pkgload::load_all()`.
- Credentials may cross to workers by explicit decision (2026-09-18): Step 2 snapshots are still scrubbed (`.wise_step2_async_connection_params()`) and workers resolve secrets from their own environment; the Overview metadata task passes connection params verbatim because user-entered credentials exist only in the params list. Never ship credentials to remote (`url=`-based) daemons — env-based resolution only.
- Model fitting runs through `ENGINE_REGISTRY` (`fct_fit_model.R`) fitting 3 progressive specs across 4 engines — respect the contract; don't special-case one engine outside it.
- Maps run on the custom hexmap bridge (`fct_hexmap.R`) — this is why Section 7 excludes maps; do not migrate or replace it.
- Existing bounded caches (e.g. the aggregation preparation cache in `fct_aggregation.R`) must stay bounded per Section 4.
- `DT`, `ggplot2`, and synchronous `future.apply` are still in use, so Sections 6–8 describe migration targets, not the status quo.

**Candidate opportunities** (`review/optimize_*.md` — considered for implementation, never enforced): `optimize_get_weather.md` (weather pipeline), `optimize_rif.md` (RIF quantile regression), `optimize_aggregation_welfare.md` (welfare statistics aggregation). Each is a ranked list of remaining speed opportunities with expected impact, parity risks, and traps found in earlier proposals — read the relevant one before touching that area, verify against the current code, and benchmark per Section 9 before adopting.

## 1. Data Layer: Zero-Copy & Out-of-Core Processing

Do not load full datasets into R session memory when the operation can be pushed to an engine that works on disk or in columnar memory instead.

- Store raw data as `.parquet` (via `nanoparquet` or `arrow`), not `.csv`. Loads 10–15x faster and compresses far better.
- For aggregation/transformation, benchmark and use whichever of the following is fastest in production for the specific workload: `duckdb` (SQL pushdown, out-of-core), `collapse` (fast C-based in-memory manipulation), `data.table`, or a compiled Rcpp routine. There is no single mandated engine — pick per-workload based on measured performance, not assumption.
- Pin the DuckDB engine/R package to the current LTS release line (`1.4.x`, e.g. 1.4.5+) for production, not the latest non-LTS minor (`1.5.x`). DuckDB 2.0.0 is expected around October 2026; hold off adopting it in production until it (or a subsequent LTS) has stabilized.
- `polars` (R bindings, LazyFrame + Arrow) is still not on CRAN as of September 2026 (install via R-multiverse/R-universe) and remains younger/less battle-tested in R specifically than `duckdb`/`data.table`, though the underlying engine is mature. Treat it as an experimental candidate in benchmarks, not a default choice, and re-evaluate as the R package matures toward a CRAN release.
- Never call `collect()`, `as.data.frame()`, or `dbFetch()` earlier than necessary. Filter/join/aggregate in the engine; only pull the already-reduced result into R.
- Where a materialized R object (data.table, matrix, vector) must be shared across multiple `mirai` workers or reused across concurrent sessions, consider `mori` (Posit, shared-memory R objects via ALTREP, first released ~April 2026) instead of letting each worker serialize its own copy. All processes map the same physical memory; only a small handle is passed across the boundary. It's new, so validate behavior/stability for this app's object types before relying on it broadly — see also Section 8.

## 2. Fast Simulation & RNG

- Move hot loops (matrix transforms, Monte Carlo, iterative simulation) out of interpreted R and into Rcpp/RcppArmadillo.
- Use `dqrng` instead of base `runif()`/`rnorm()` for any simulation that runs in parallel — base RNG is single-threaded and not safe across workers; `dqrng`'s PCG generators are thread-safe.
- Simulation kernels should take plain numeric/matrix inputs so they can run identically on the main session or inside a background worker without extra marshaling.

## 3. Model Fitting Optimization: fixest (current priority)

The app fits models via `fixest`. Optimization effort should focus here first.

- `fixest` ultimately requires a standard `data.frame`/`data.table` input, so a literal zero-copy transfer into `feols()` isn't possible — but the *path* to that structure matters. Avoid manual, row-wise, or type-coercing conversions from a DuckDB result. Instead, fetch via Arrow's optimized fetch path (e.g. `duckdb_fetch_arrow()`) and convert to `data.table` using Arrow's native converter, which is materially faster and lower-memory than a generic `as.data.frame()` on a DBI result:

```r
# DuckDB -> Arrow (optimized fetch) -> data.table -> fixest
con |>
  dbSendQuery("SELECT ...") |>
  duckdb::duckdb_fetch_arrow() |>
  as.data.frame() |>          # Arrow's optimized converter, not a generic DBI fetch
  data.table::setDT() |>
  fixest::feols(y ~ x1 + x2 | fe1 + fe2, data = _)
```

  Verify exact function names against installed `duckdb`/`fixest`/`arrow` versions — the requirement is: minimize copies and avoid a slow/generic conversion step between DuckDB and `feols()`.
- Where many small/fast regressions are run in a loop (e.g., cell-by-cell), prefer `collapse` or `RcppArmadillo::fastLm()` over repeated `lm()`/`feols()` calls if `fixest`'s own vectorized multi-estimation (`feols()` with multiple LHS/fixed-effect combinations) doesn't already cover the case.
- Keep `fixest` current (0.14.x+). The 0.14 line reworked the internal demeaning algorithm for meaningfully faster fixed-effects estimation on large data — upgrading the package itself is a valid, low-effort optimization independent of any data-pipeline change.
- `lightgbm`/`xgboost` are flagged as a **future** modeling engine only. Do not migrate existing `fixest` workflows to them without an explicit decision to do so; this is not a current optimization target.

## 4. Smart Caching

- Wrap expensive, pure functions (simulation runs, model fits, heavy aggregations) in `memoise::memoise()`, backed by `cachem::cache_mem()` (in-memory) or a disk cache if results should persist across restarts or sessions.
- Memoised functions must receive plain, hashable arguments (numbers, strings, vectors) — not reactive expressions, connections, or environments — or cache-hit detection breaks.
- For cache-key hashing, use `secretbase` (fast, zero-copy streaming hashes; e.g. SHA-256/XXH3) instead of `memoise`'s default digest backend when memoising large objects (big data.tables, matrices). It hashes without fully serializing the object into memory first, avoiding a memory spike on every cache lookup, and produces cross-platform-reproducible hashes. `secretbase` is already the hashing engine behind the `targets` package's caching, so it's a proven approach for this exact problem.
- Set cache size/eviction limits; an unbounded cache on a long-running server is itself a memory leak.

## 5. Application & Session Architecture

- Never store large data frames in `reactiveVal()`/`reactiveValues()`. Store file paths or DuckDB connection objects instead, and materialize data only at the output layer (the render function that needs actual rows).
- Close every DuckDB connection and remove every temp file on session end, via `session$onSessionEnded()`. Leaked connections/files degrade the server over time and can exhaust file handles or disk.

## 6. Tables (UI Layer)

- Use `reactable` for all rendered tables. There are no multi-million-row table use cases in this app, so `DT`/server-side pagination is not needed.
- Every rendered table must include a CSV download button, implemented client-side via `Reactable.downloadDataCSV()` (JavaScript), not an R-side download handler.

## 7. Figures (UI Layer)

- Convert existing `ggplot2` outputs to `echarts4r`. This offloads pan/zoom/tooltip/legend interaction to the browser and keeps the Shiny event loop free for compute, and gives a native download button for free. `echarts4r` is the current best-performing choice among mainstream interactive R charting libraries (ahead of `plotly` at scale; `highcharter` also carries a commercial licensing requirement not applicable here). It is actively maintained (0.5.x line as of 2026, now wrapping ECharts JS v6) — keep it current to pick up renderer performance improvements.
- Where a chart updates in response to an input change but doesn't need a full re-render (e.g., highlighting, filtering a series), use `echarts4r` proxies (`echarts4rProxy()`) to patch the existing chart client-side instead of re-rendering it server-side.
- Exclude maps from this migration. Map rendering is already optimized; the remaining cost there is data preparation, which should instead be addressed through Section 1 (data layer), not a charting library swap.

## 8. Asynchronous Execution

Standard: `ExtendedTask` (UI/session layer) + `mirai` (worker backend).

- Do not use `future`/`future_promise()` for high-throughput paths; `mirai` resolves via event-driven NNG sockets with materially lower overhead and is Shiny's recommended async backend.
- Never create workers inside a `server()` function. Create the daemon pool once at app startup (`global.R` or golem's equivalent app-startup hook), not per-session. Register a shutdown hook (`onStop()`) so daemons terminate cleanly when the app stops.
- Create, use, and destroy any resource needed inside a task (DB connections, file handles) entirely within the `mirai()`/task code block. Do not share connections or sockets from the parent session into a worker.
- Pass minimal environments into workers — explicit `.args` or `environment()` with only the required variables, never the full main-session environment. For large shared objects (e.g. a common reference dataset all tasks need), prefer a `mori` shared-memory object over passing/serializing the object itself through `.args` on every task dispatch (see Section 1).
- Use `crew` (built on `mirai`) only when workload is spiky/unpredictable and autoscaling genuinely helps — e.g., unpredictable bursts of concurrent simulations across sessions. For steady, predictable load, a fixed `mirai` daemon pool sized to Posit Connect's max processes (see Section 10) is simpler and avoids `crew`'s scale-up/scale-down overhead. Don't add `crew` by default; justify it against the actual load pattern first. If used, size workers to the deployment's available cores (leave at least one free for the main process) and set `seconds_idle`/`tasks_max` to reclaim idle or long-lived workers. Use the controller's `autoscale()`/`descale()`/`started()` methods (current `crew` API) for Shiny integration rather than manually managing worker counts.
- For backpressure under heavy concurrent load, prefer newer `mirai` features where available — a queue memory cap (`daemons(memory = ...)`) and non-blocking submission (`try_mirai()`) — over letting tasks queue unbounded, since an unbounded queue degrades responsiveness for all users, not just the requester.
- Use `bslib::input_task_button()` to trigger long-running tasks: prevents double-submission and provides a built-in loading indicator without hand-rolled debounce/spinner logic.

## 9. Benchmarking

Benchmark optimizations against real app data before merging, using the local dataset folder: `~/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents`.

- Reuse the existing benchmark harness (`dev/bench_*.R`, `dev/run_step2_benchmark.sh`, results under `dev/outputs/`) rather than writing ad-hoc timing scripts.
- **Smaller payload**: BFA surveys (all years), default weather and climate scenario.
- **Larger payload**: IRN surveys (all years), default weather and climate scenario. For this payload, also benchmark a multi-period, multi-SSP climate scenario combination, since that is the realistic worst-case load for climate-scenario processing.

Report before/after timings and peak memory for both payloads when submitting a performance-related change.

## 10. Deployment / Runtime Parameters

The production web app is deployed on Posit Connect with the following runtime settings. These are editable and should be considered part of the optimization surface, not fixed constraints — e.g., worker/daemon pool sizing (Section 8) should be chosen with these in mind, and changes to them may themselves be a valid optimization.

- Max RAM: `0 GiB` (no limit)
- Min processes: `1`
- Max processes: `8`
- Max connections per process: `1`
- Load factor: `0.5`
- Initial timeout: `120 s`
- Idle timeout per process: `60 s`
- Connection timeout: `3600 s`
- Read timeout: `3600 s`
