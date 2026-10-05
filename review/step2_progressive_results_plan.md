# Step 2 progressive results: plan

Status: proposal (inspection and planning only, nothing implemented).
Date: 2026-10-05.
Scope: Step 2 simulation run lifecycle. This covers the async worker (`R/fct_step2_async.R`), the compute path (`R/fct_step2_compute.R`, `R/fct_run_simulation.R`), the sidebar run controls (`R/mod_2_01_weathersim.R`) and the Results tab (`R/mod_2_02_results.R`).

## 1. Goal

Step 2 runs for a long time. BFA with 3 SSPs x 3 periods takes about 170 s, and one IRN SSP/period takes 80-95 s. Today the user gets:

- a status line with an indeterminate striped bar,
- a provisional historical-mean table in the sidebar,
- and then, all at once, the full Results tab.

The goal is to make the Results tab charts fill in progressively. Historical results would appear first, then each SSP x period scenario as soon as its climate-model ensemble finishes. The run would feel like streamed data, and the guarantees the current coordinator gives (atomic publication, cancellation, staleness, Step 3 isolation) would stay intact.

## 2. Current state (what exists)

### 2.1 Run lifecycle

1. `mod_2_01_weathersim.R` builds an ordinary-object snapshot and calls `.wise_step2_async_submit()`. Credentials are scrubbed at this point.
2. `.wise_step2_async_dispatch()` sends one `mirai::try_mirai()` task to the single process-wide daemon. The queue is FIFO across sessions, and a session's newest run supersedes its older one.
3. The worker (`step2_async_worker()`) calls `step2_compute()` and then `fct_run_simulation()`.
4. Weather is streamed per key: `get_weather(weather_consumer = consume_key)`. Historical is emitted first. Then, for each SSP (outer loop) and each period (inner loop), all CMIP6 members are collected in one batch and handed to `consume_key()` one member at a time. Each member runs `run_sim_pipeline()` immediately.
5. After the historical key, `preview_fn` computes a mean-only aggregation (`aggregate_pipeline_tables_multi(methods = "mean")`). The worker writes it to `preview.rds` plus a descriptor.
6. The worker reports progress by writing `progress.rds` atomically. The main process polls `progress.rds` and the preview descriptor every 0.5 s via `later` (`.wise_step2_async_start_poll()`).
7. On completion the worker writes `result.qs2` and a manifest. The main process validates them, reads the full result (`.wise_step2_async_read_manifest()`), and commits `hist_sim()` and `saved_scenarios()` together under a file lock (`.wise_step2_async_commit()`).
8. `mod_2_02_results.R` inserts the Results tab on the first non-NULL `hist_sim()`. All charts are then derived lazily *on the main process*: `hist_agg_rv()`/`scenario_agg_rv()` feed `derived_results_frame_rv()`, which feeds the bands, curves, thresholds and exceedance data, which feed the echarts4r and reactable outputs.

### 2.2 Measured costs (existing benchmark outputs in `dev/outputs/`)

| Payload | Worker elapsed | Result size | qs2 read on main | Main-thread aggregation after adoption |
|---|---:|---:|---:|---:|
| BFA OLS, 1 SSP x 1 period (17 pipelines) | ~18 s | 60 MB | 0.6 s | mean 1.1 s; full suite 6.0 s |
| BFA OLS, 3 SSP x 3 periods (145 pipelines) | ~168 s | 493 MB | 4.3 s | not measured (expect ~9x the 1x1 figures) |
| IRN OLS, 1 SSP x 1 period (17 pipelines) | 82-95 s | 256 MB | 3.7 s | mean 10.1 s; full suite 16.8 s |

Sources: `stream-step2-readiness/readiness.csv`, `stream-step2-bfa-timeline/step2_aggregation.csv`, `step3-annual-phase4-irn/step2_aggregation.csv`.

The benchmark's aggregation timing harness may not match the in-app call shape exactly, so re-measure these before relying on them (Phase 0). Even so, the numbers point to two things:

- **There is a hidden second wait after "Results ready".** The main process reads the result (seconds) and then aggregates every scenario for the selected method. At IRN scale, or with several scenarios, that is tens of seconds of blocked main R process before the first chart paints. Posit Connect runs one connection per process, so the block only affects the user who started the run, but that user's UI is frozen.
- **The natural streaming unit already exists in the worker.** Each SSP x period group completes in sequence, roughly 18 s per group for BFA and 80 s for IRN. The pipelines for that group are in worker memory at that moment.

### 2.3 Constraints that must hold

- Atomic publication (`review/archive/async-step-execution-mirai-plan.md`). `hist_sim()` and `saved_scenarios()`, which Step 3, Diagnostics and exports consume, change only on full, validated adoption. A partial or cancelled run must never replace the previous committed run.
- Generation, dependency-signature and session-ended guards on every callback (the existing `session_callback` pattern and `live_sim_sig()` checks).
- Guidelines section 8: one process-wide daemon pool, minimal payloads across the boundary, no credentials in snapshots.
- Determinism. Aggregation uses explicit seeded streams (`seed = WISEAPP_DEFAULT_SEED` in `fct_aggregation.R`), so worker-side and main-side aggregation can be bit-identical. A test must enforce this.

## 3. Feasibility verdict

Feasible, with moderate effort. Most of the risk is in the Results module refactor, not in the async layer. The pieces are:

- **Worker side: low risk.** `consume_key()` already knows each member's group. What is missing is a "group complete" signal and a call to the existing aggregation function per group, mirroring what `preview_fn` already does for historical. The shared context used for aggregation is the same object for historical and every scenario (`compact_step2_result()` assigns one `step2_shared_context()` to all), so the worker can build it once after historical.
- **Transport: low risk.** The preview channel (atomic write, lock, descriptor, size bound, root containment, generation/digest checks) is exactly the pattern needed. It can be generalised from "one preview" to "an ordered list of partials".
- **Results tab: medium risk.** All charts derive from two reactives, `hist_agg_rv()` and `scenario_agg_rv()`, whose values are small aggregated tables. That is the right injection point. However, the module has 41 direct `hist_sim()` call sites (`so`, `has_weights`, `chol_obj`, `residuals`, `svy`, `.sig`, `pipeline`), and tab insertion is keyed on `hist_sim()`. Those reads need to go through one "display source" abstraction.

## 4. Options considered

**A. The worker aggregates each completed group and streams compact summary tables (recommended).**

How it works: when a group completes, the worker runs `aggregate_pipeline_tables_multi()` on that group's compacted pipelines, for the display defaults captured at submit, and writes the result tables as a small partial. The Results tab renders provisional charts from these tables. On adoption, the same tables seed the main-process aggregation cache.

- Pros:
  - Payloads are tiny (KB to low MB, not tens or hundreds of MB).
  - No main-thread blocking per partial.
  - It removes the post-adoption aggregation wait.
  - Every chart that derives from aggregated tables can stream: headline cards, annual distribution, adverse dot, exceedance, threshold table and uncertainty sources.
- Cons:
  - Adds worker time (roughly 1 s per group for BFA and 10 s for IRN with mean only, more with the full method suite). This is a cost of moving work, not new work: the main process does the same aggregation today after adoption.
  - During streaming only the precomputed method and poverty line are available.
  - Incidence-by-decile needs full pipelines, so it waits for completion.

**B. Stream each group's full pipelines to the main process and feed the existing reactives.**

- Pros: almost no Results-module change, and every control works during streaming.
- Cons:
  - Each partial costs 50-250 MB of serialisation, plus a main-thread read.
  - Main-thread aggregation runs on every arrival.
  - `agg_workspace()` depends on `saved_scenarios()`, so every arrival clears the cache and re-aggregates all scenarios (quadratic).
  - The main process holds a duplicate of the result until adoption.
  - It would make responsiveness worse, not better.

Rejected.

**C. Split the run into one mirai task per SSP x period group (native promise per group, no file polling).**

- Pros:
  - Idiomatic promises.
  - Natural per-group delivery.
  - Opens the door to later parallelism.
- Cons:
  - `get_weather()` computes the baseline and deltas for all SSPs and periods in one DuckDB session with shared temp tables, so splitting repeats that setup.
  - `chol_obj`, `train_aug`, `svy_prepared`, the RIF caches and the snapshot would be re-shipped or recomputed per task, or cached in daemon globals, which is fragile.
  - The FIFO and supersession logic would have to handle job families.
  - Atomic adoption becomes a multi-task barrier.
  - It is a large rewrite of a coordinator that was only just hardened and user-tested (tracking log, 2026-10-01).

Rejected for now. Revisit only if multi-daemon parallelism becomes a goal.

**D. Keep the current UX and only improve progress text (determinate stepper and ETA).** This is a cheap, standalone win. It is included as an early phase of A, not as an alternative.

## 5. Recommended design (Option A)

### 5.1 Worker: group completion and partial summaries

In `fct_run_simulation()`:

1. **Group-complete signal.** Extend the `weather_consumer` metadata in `get_weather()` with `member_index` and `period_members`. In the fast path these are known from `length(period_out)`; in the bounded path, from `length(model_names)`. `consume_key()` already receives `metadata`. When `member_index == period_members`, the group is complete. Fallback when metadata is absent (injected test loaders, manifest/cache path): the group is complete when the first key of a different group arrives, or at loop end.
2. **Shared display context.** Build the `step2_shared_context()` once after historical. The preview code already does this; lift it out of the `preview_fn` block so historical and scenario partials use the identical object that `compact_step2_result()` will later attach.
3. **`partial_fn(summary)`.** This generalises `preview_fn`. For historical and then for each completed group, aggregate with exactly the arguments `mod_2_02_results.R` uses:
   - `.build_hist_for_method()` / `.build_scn_for_method()`: weighted when weights exist, otherwise unweighted;
   - `prosperity_gap` goes through `aggregate_pipeline_table()`, not the suite;
   - `band_q = c(lo = .10, hi = .90)`, the default bandwidth of 0.05, and the default seed;
   - `model_ids = names(pipes)`.

   Factor these argument sets into one helper (for example, `step2_display_aggregation()` in `fct_aggregation.R`) that both the module and the worker call. Without that, the two sides will drift.
4. **Display settings captured in the snapshot (decided, see section 10):** the worker computes the selected method only. The snapshot captures the Results tab's live method, poverty line and bandwidth at submit. On a first run, or when the outcome changed so the method is unsupported, it falls back to `"mean"` and the poverty line resolved exactly as `.results_content_ui()` does from `so$povline`. A user who wants to stream a poverty metric can set it before clicking Run. The full method suite is not precomputed.
5. **Payload per partial:**
   - `kind` (`historical` or `scenario`), `label` (the display key, built by a shared helper extracted from the `new_scenarios` assembly loop so labels match adoption exactly), `ordinal`, `groups_total`, `n_models` and `n_models_requested`;
   - `method`, `weighted`, `pov_line` and `bandwidth`;
   - the aggregated table(s) in the same tibble shape the module caches. This includes the `F_agg_all` and `value_all_sd` list-columns, so contrast SDs keep working. That is roughly 25 coefficients x models x years doubles, which is well under 1 MB at IRN scale. Verify the size in Phase 0 and keep a hard cap.
6. **Release memory.** Release the group's staging objects only after adoption assembly, as today. The partial holds aggregated tables only.

The existing mean-only historical `preview_fn` becomes the `historical` partial. The bespoke sidebar preview table is retired (see 5.4).

### 5.2 Transport: generalise preview to partials

In `fct_step2_async.R`:

- The worker writes `partials/<seq>.rds` atomically, under the existing publication lock and after `checkpoint_fn()`. It then rewrites `partials-index.rds`, which lists entries with `seq`, `kind`, `label`, `path`, `size`, `job_id`, `generation` and the digest.
- `.wise_step2_async_poll_partials()` replaces `.wise_step2_async_poll_preview()`. On each 0.5 s tick it reads the index (size-capped), validates each new `seq > job$partials_delivered` with the same checks used today (identity, digest, root containment, per-file cap of about 2 MiB, at most `groups_total + 1` entries, schema check on the table columns), and calls `job$on_partial(partial, job)` in order.
- Progress records gain optional fields: `groups_total`, `groups_done`, `current_label`, `member_index` and `period_members`. Bump the progress schema from 1 to 2, and keep the validator accepting both while tests migrate.
- Cancellation, supersession and session-end detach clear `on_partial` along with the other callbacks. No other change is needed: retired jobs already stop polling, and the artifact directory is removed on settle.

The rest of the coordinator does not change: FIFO, `try_mirai` backpressure, locks, settle and retire.

### 5.3 Main process: the provisional display source

In `mod_2_01_weathersim.R`:

- Add `live_run <- reactiveVal(NULL)`, holding the run's generation, signature, captured display defaults, `groups_total`, partials received so far (an ordered list) and progress. `on_partial` appends to it under the same guards as `on_preview` today (generation, `live_sim_sig()`, `session_ended`).
- Clear `live_run` on cancel, stale, failure and successful adoption (the final transfer is covered in 5.5).
- Export `live_run` in the module API. Step 3 and Diagnostics do **not** receive it.

In `mod_2_02_results.R`, introduce one internal `results_source()` reactive. It returns either:

- **committed**: built from `hist_sim()` and `saved_scenarios()`, with today's lazy aggregation;
- **provisional**: built from `live_run()`, using precomputed tables plus pending scenario labels.

Both have the same fields: `so`, `has_weights`, `has_draws`, `residuals`, `scenario_names`, `pending_names`, `hist_agg(method)`, `scn_agg(method)`, `methods_available`, `mode`.

Then route the direct `hist_sim()` reads through it:

- `hist_agg_rv()` and `scenario_agg_rv()`;
- `agg_methods()`, `weight_key()`, `has_draws()` and `selected_scenario_names()`;
- the headline-card metadata and the summary card;
- the tab-insertion observer, keyed on the source's `so` rather than `hist_sim()`;
- `incidence_data_rv()`: committed only, with a placeholder in provisional mode;
- export registrations (`wise_export_*`): committed only, so exports never include provisional data.

`derived_results_frame_rv()` and everything downstream do not change, because they consume tables only.

This is the main refactor. Keep it mechanical: no behaviour change in committed mode, proven by the existing module tests plus a characterisation snapshot taken before the change.

### 5.4 UX

**Sidebar run panel** (replaces the indeterminate bar and the preview table):

- A determinate progress bar: `(1 + groups_done + member_index / period_members) / (1 + groups_total)`.
- A compact checklist: Historical, then one row per SSP x period. Completed rows get a check mark, the current row shows "7 / 16 climate models", and pending rows are muted.
- After the first future group finishes, show an ETA: mean group duration x groups remaining, rounded to "about N min". Show no ETA before that, because weather loading dominates early.
- Keep the Stop button and the cancelled/stale messages.

**Results tab:**

- On the first run, the tab appears when the historical partial lands. That is a few seconds after the weather load, rather than at the end.
- On a re-run, the tab switches to the live run when its historical partial arrives (decided). Until then, the previous committed run stays on screen with the banner already showing "New run in progress".
- A slim banner at the top of the tab: "Simulation in progress: 3 of 9 scenarios ready. Results are provisional; Step 3 and exports use the last completed run." It also includes a mini progress bar and the Stop link.
- Pending scenarios appear as muted placeholder categories with a "computing" label, so chart axes and legend order stay fixed and nothing jumps as data arrives. This needs a `pending` flag in the echart builders' input (`echart_annual_distribution()`, `echart_pointrange_climate()` and the adverse-dot and exceedance builders). The headline cards show a skeleton card per pending scenario.
- Charts re-render once per partial (at most `groups_total + 1` times). ECharts' default update animation makes new series grow in, which gives the streaming feel. `echarts4rProxy()` patching is optional polish later, not needed for v1.
- Controls during provisional mode (decided):
  - The other method pills and the poverty-line input are disabled, with a tooltip: "Available when the simulation completes". The selected method stays highlighted. The banner adds one line: "Other summary methods unlock when the run finishes."
  - The deviation, ensemble-band and display-type toggles stay enabled, because they are applied on top of the precomputed tables and need no new aggregation.
  - Controls unlock on adoption. Other methods and poverty lines then go through today's lazy main-side aggregation.
  - `pill_toggle()` (`R/utils_ui.R`) has no disabled state today; add one (a `disabled` argument plus an update path), or disable via a CSS class on the container. Do not add `shinyjs`. Re-enable on every exit path (complete, cancel, stale, failure).
- On completion, the banner changes to "Results complete" and fades. Values do not visibly change, because of the parity guarantee in 5.5. Keep the existing completion and failure notifications.
- On cancel or failure, the view reverts to the committed run if one exists. Otherwise the Results tab is removed and the empty state returns.

### 5.5 Adoption without the second wait

On successful adoption, after `hist_sim()` and `saved_scenarios()` are committed, the new `agg_workspace()` is seeded from the last `live_run()` tables. The method/key entries are inserted as already-built lazy aggregation objects (`.new_lazy_aggregation_method_list` with `built = TRUE`). Seeding happens only when the run signature, method, poverty line, bandwidth, residuals and skip-draws values all match. As a result, the first committed render costs table reshaping only, not re-aggregation.

The table seeded from the worker and the table the main process would compute must be identical. This is enforced by a test (section 6). If they ever diverge, the seeding is skipped (it is a cache) and correctness is unaffected.

Out of scope here but worth noting: the remaining adoption cost is the main-thread `qs2_read` of 0.25-0.5 GB (about 4 s at BFA 3x3 or IRN 1x1). Lazy per-scenario pipeline loading, which keeps member pipelines on disk until incidence, Diagnostics or Step 3 need them, is a separate follow-up candidate.

### 5.6 Synchronous fallback

When `WISEAPP_ASYNC_STEP2=0` (`withProgress` path), the main thread is blocked, so partials cannot render. Pass no `partial_fn` there, and leave the behaviour unchanged.

## 6. Tests

- `test-fct_run_simulation.R`: partial order (historical, then each group in SSP/period order). `groups_total` is correct. The group-complete signal works with and without consumer metadata (fast, bounded, manifest and test-injected loaders). A partial failure inside a group reports `n_models < n_models_requested`. A whole-group failure still throws at the end, after the earlier partials.
- **Parity (critical):** for OLS and RIF fixtures, the worker partial tables are `identical()` to the main-side `.build_hist_for_method()` / `.build_scn_for_method()` output for the same method, weighting and poverty line, including `prosperity_gap` and unweighted surveys.
- `test-fct_step2_async*.R`: index and partial validation rejects a foreign job or generation, an oversize file, a path outside the root, too many entries, or a bad schema. Partials are delivered once and in order. Cancel, supersede and session-end stop delivery. Progress schema 2 is accepted.
- `testServer` for `mod_2_02_results`:
  - committed mode is unchanged (characterisation snapshot of the derived frame, bands and curves before and after the refactor);
  - provisional mode renders with partial scenarios;
  - exports and incidence are suppressed in provisional mode;
  - the view reverts on cancel;
  - the cache is seeded on adoption (no aggregation call; assert via the cache hit counters already exposed by `aggregation_cache()`).
- Real-worker smoke test (existing harness): BFA 1x1 and 3x3, checking that partials arrive and adoption matches.

## 7. Benchmarks (guidelines section 9)

Use `dev/bench_step2.R` and `dev/run_step2_benchmark.sh`. Add these timeline fields: click to first feedback, click to historical chart, per-group arrival times, click to final adoption, and main-thread blocked time from adoption to first committed render.

| Payload | Measure before and after |
|---|---|
| BFA, all years, default weather + 1 SSP x 1 period | time to first chart, total time, worker overhead of per-group aggregation, peak RSS |
| BFA, 3 SSP x 3 periods | the same, plus partial count and sizes |
| IRN, all years, default + multi-SSP, multi-period | the same; the worst case for aggregation overhead |

Decision gates:

- Worker overhead from per-group selected-method aggregation should be at most about 10% of worker elapsed (existing data suggests about 6% for BFA and about 12% for IRN with `mean`), and click-to-fully-interactive should improve. If IRN exceeds the gate, profile the aggregation call shape before going further. Do not revisit the full suite without new evidence.
- Partial size should be at most 2 MiB at IRN scale.
- Peak RSS should not regress materially.

## 8. Phasing

| Phase | Content | Size | Depends on |
|---|---|---|---|
| 0 | Re-measure the current timeline, including in-app main-thread aggregation and adoption block | S | none |
| 1 | `get_weather` member metadata and the group-complete signal; progress schema 2 fields | S | none |
| 2 | Sidebar determinate stepper and ETA (ships alone, visible win) | S | 1 |
| 3 | Shared `step2_display_aggregation()` helper, the worker `partial_fn` and the parity tests | M | 1 |
| 4 | Partials transport (generalising preview) and `live_run` in mod_2_01 | M | 3 |
| 5 | `results_source()` refactor in mod_2_02, with committed mode unchanged | M-L | none (can start in parallel after the characterisation snapshot) |
| 6 | Provisional mode: banner, pending placeholders, control gating, revert paths | M | 4, 5 |
| 7 | Cache seeding on adoption, then benchmarks and the tracking log update | S-M | 6 |

Phases 1-2 are low risk and immediately useful. Phases 3-4 can land behind an option (for example, `WISEAPP_STEP2_STREAM=1`) before the UI uses them.

## 9. Risks

- **Results-module regression.** It is about 2,000 lines with many `hist_sim()` reads. Mitigate with the characterisation snapshot, a mechanical refactor first and the behaviour change second.
- **Worker and main aggregation drift.** This would cause a visible jump on adoption and stale cache seeds. Mitigate with the shared helper, the parity test and seeding only on exact key match.
- **Provisional data leaking into Step 3 or exports.** Mitigate by never giving `live_run` to Step 3 or Diagnostics and gating export registrations on committed mode, with tests asserting both.
- **Chart churn.** Mitigate with fixed category order, pending placeholders and at most `groups_total + 1` re-renders.
- **RIF engine.** The preview already aggregates RIF pipelines, but confirm that RIF scenario aggregation uses the same path and seed in Phase 3.
- **Index rewrite races.** Mitigate by writing the index atomically under the existing lock and having the reader tolerate a missing or partial index (the same tolerance as `progress.rds` today).

## 10. Decisions (2026-10-05)

1. **Re-run behaviour: switch to the live run.** When a committed result is on screen, the Results tab switches to the live run as soon as its historical partial lands, with the provisional banner. On cancel or failure it reverts to the committed run. (User decision.)
2. **Selected method only; no full suite.** Existing measurements put the full suite at about 5.5x the mean-only cost for BFA (6.0 s vs 1.1 s per group, which is +33% on an 18 s group) and about 1.7x for IRN (16.8 s vs 10.1 s). Selected-only costs about 6% (BFA) and 12% (IRN), and that time is not new work, because the main process does the same aggregation after adoption today. Most users only look at the default method while waiting, and the full suite would also make every partial roughly 9x larger. Capturing the live method and poverty line at submit covers users who want a poverty metric streamed. (Recommendation, accepted pending user confirmation.)
3. **Disable, don't defer.** Other methods and the poverty line are disabled during streaming, with an "Available when the simulation completes" tooltip. The alternative, allowing a selection with an "updates on completion" note, leaves the charts either empty or labelled with a metric they are not showing. It would also need a worker back-channel to compute the new method for the remaining groups and backfill the finished ones, which is new coordinator complexity for little benefit. Display-only toggles (deviation, ensemble band, chart type) stay live. (Recommendation, accepted pending user confirmation.)
