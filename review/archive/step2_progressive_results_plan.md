# Step 2 progressive results: plan

Status: phases 1-7 implemented in the working tree (uncommitted), unit-tested; in-browser validation, section 7 benchmarks and RIF parity outstanding. Interface: `review/step2_live_run_contract.md`.
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
2. **Superseded (2026-10-05): stream all suite methods.** Re-measured at real scale (BFA, 2 SSP x 1 period, current kernel): the full suite costs about 2.6 s per scenario versus 1.1-1.7 s for one method, and the suite partial is about 0.45 MB (8 methods, BFA). The worker now streams every suite method; prosperity gap only when it is the captured method. Method switching works during streaming, and Phase 7 seeds every method on adoption. Original decision: selected method only, based on older benchmark figures (suite 5.5x mean for BFA).
3. **Disable, don't defer.** Other methods and the poverty line are disabled during streaming, with an "Available when the simulation completes" tooltip. The alternative, allowing a selection with an "updates on completion" note, leaves the charts either empty or labelled with a metric they are not showing. It would also need a worker back-channel to compute the new method for the remaining groups and backfill the finished ones, which is new coordinator complexity for little benefit. Display-only toggles (deviation, ensemble band, chart type) stay live. (Recommendation, accepted pending user confirmation.)


## Appendix A: mod_2_02_results.R hist_sim()/saved_scenarios() call sites

Inventory taken on branch `dev` against `R/mod_2_02_results.R` (2058 lines) with `grep -nE 'hist_sim\(|saved_scenarios\(' `. There are 48 grep matches: 2 are comments (line 489 and line 1965, ignored), leaving 46 call-site lines (some lines read the reactive twice). Counts by class: **S = 24, C = 11, L = 11**.

Classes: **S** display source (must work in provisional mode, will read `results_source()`); **C** committed only (needs full pipelines/svy, or is an export/side effect; suppressed or placeholder in provisional mode); **L** lifecycle (tab, cache, choices sync).

| Line | Enclosing reactive/output/observer | Fields read | Class |
|---|---|---|---|
| 394 | `output$simulation_summary_ui` (renderUI) | whole object (`simulation_summary_card()` reads `$so`, `$sim_summary`, `$hist_label`) | S (ambiguous, see below) |
| 395 | `output$simulation_summary_ui` | whole list; names and `$n_models` per scenario | S (ambiguous) |
| 411 | `headline_cards_data_rv` | whole object (`step2_headline_cards()` reads `$so`, `$sim_summary`, `$svy` (nrow only)) | S (ambiguous) |
| 412 | `headline_cards_data_rv` | whole list (length only) | S |
| 419 | `headline_cards_data_rv` (metadata block) | `$so`, `$analysis_unit` | S |
| 446 | `agg_methods` | null-check (`req`) | S |
| 447 | `agg_methods` | `$so` | S |
| 604 | `aggregation_cache()` (plain function) | null-check (isolate) | L |
| 612 | `agg_workspace` | null-check (`req`) | C |
| 621 | `agg_workspace` (`ws$hs`) | whole object; downstream `$pipeline`, `$so`, `$.sig`, `$shared_context` | C |
| 622 | `agg_workspace` (`ws$sc`) | whole list; downstream `$pipelines`, `$so`, `$shared_context` | C |
| 623 | `agg_workspace` (`ws$res`) | `$residuals` | C (the `residuals` field itself is S, see contract) |
| 635 | `observeEvent(hist_sim())` cache clear | trigger | L |
| 637 | same | null-check | L |
| 876 | `scenario_agg_rv` | null-check (`req`) | S |
| 877 | `scenario_agg_rv` | length | S |
| 895 | `weight_key` (two reads) | null-check, `$has_weights` | S |
| 1036 | `has_draws` | null-check (`req`) | S |
| 1037 | `has_draws` | `$chol_obj` (null-check only) | S |
| 1042 | `selected_scenario_names` | names | S |
| 1331 | `exceedance_curves_rv` (two reads) | null-check, `$so` | S |
| 1432 | `threshold_table_rv` (two reads) | null-check, `$so` | S |
| 1648 | `annual_distribution_chart` | `$so` | S |
| 1662 | `incidence_data_rv` | `req(hist_sim(), saved_scenarios())` null-checks | C |
| 1667 | `incidence_data_rv` | `$so$transform` | C |
| 1668 | `incidence_data_rv` (two reads) | `$svy`, `$survey` | C |
| 1673 | `incidence_data_rv` | `saved_scenarios()[[nm]]$pipelines` (or `$pipeline`) | C |
| 1676 | `incidence_data_rv` | `$so$name`, `$pipeline` | C |
| 1705 | `wise_export_table("climate_distributional_incidence_data")` fun | `$so` | C |
| 1721 | `annual_distribution_export` (export fun) | `$so` | C |
| 1749 | `threshold_table_df` (on-screen table and export) | null-check, `$so` | S |
| 1752 | `threshold_table_df` (two reads) | null-check, `$sim_summary$historical_years` | S |
| 1775 | `uncertainty_chart` | `$so` (only `metric_metadata()$format`) | S |
| 1803 | `adverse_dot_data_rv` | `$so` | S |
| 1814 | `adverse_dot_data_rv` | `$so` | S |
| 1828 | `adverse_dot_chart` | `$so` | S |
| 1894 | `exceedance_chart` | `$so` | S |
| 1930 | `observeEvent(hist_sim())` tab insert/remove | trigger | L |
| 1932 | same | null-check | L |
| 1990 | same (`insertUI`, `.results_content_ui`) | `$so` | L |
| 1998 | `observeEvent(hist_sim())` select tab | trigger | L |
| 2000 | same | null-check | L |
| 2012 | `observeEvent(hist_sim())` method choices sync | trigger | L |
| 2014 | same | `req($so)` | L |
| 2015 | same | `$so` | L |
| 2052 | return API `timeseries_curves` (Diagnostics) | `$so` | S (ambiguous) |

Note on the 1749/1752 and 1648/1705/1721 groupings: the builder closures (`threshold_table_df`, `annual_distribution_chart`) are shared between on-screen output and the `wise_export_*` fun. The display use is S. The export registrations themselves stay committed-only (see below), so no per-line split is needed.

### Ambiguous call sites

- **394/395 (summary card)**: the card reads `$sim_summary`, `$hist_label` and per-scenario `$n_models`. Not currently in the worker partial contract. Either add `sim_summary`/`hist_label`/`n_models` per scenario to the partial, or show a reduced card in provisional mode.
- **411 (headline cards)**: `step2_headline_cards()` reads `hist_sim$svy` only for `nrow()` (prediction count) and `$sim_summary`. Provisional: pass a `svy_nrow` scalar (or accept the "Unavailable" prediction note). It also uses `length(saved_scenarios)` for the scenario tally; in provisional mode this must be the number of *landed* scenarios (the "Simulation years" card then understates total runs; decide whether to show it as "so far").
- **1752**: needs `sim_summary$historical_years` for `n_hist_years`; falls back to `max(tbl$n_obs)` when missing, so provisional is safe even without it, but the fallback may differ from committed. Prefer including `historical_years` in the partial.
- **2052 (return API)**: Diagnostics consumes `timeseries_curves`. Plan 5.3 says Step 3 and Diagnostics do not receive `live_run`. Decide whether the export of this reactive stays tied to committed (`hist_sim()`) or follows `results_source()`. Recommended: keep it committed-only (use committed `so`), so the Diagnostics tab is not driven by provisional data.
- **623 `$residuals`**: committed-only inside the workspace, but the S side also needs it (display of the residual note, and the seeding match in 5.5).
- **`selected_scenario_names()` filters** (lines 1176, 1250, 1315, 1400, 1593): in provisional mode these must filter to *landed* names only; pending names are handled separately by the placeholders.
- **`scenario_agg_rv` `req(saved_scenarios())`** (876): in provisional mode there may be zero landed scenarios (historical only). The current `NULL` return path (`length == 0`) already covers it.

### Proposed `results_source()` contract

One internal `reactive` returning a plain list (locked), recomputed on `hist_sim()`, `saved_scenarios()`, `live_run()` (the module needs a new `live_run` argument, default `reactive(NULL)`). Which source wins: provisional when `live_run()` has a historical partial for the current run (decision 1 in section 10), otherwise committed.

| Field | Type | Committed source expression | Provisional source (worker partials) |
|---|---|---|---|
| `mode` | `"committed"`/`"provisional"` | `"committed"` | `"provisional"` |
| `has_data` | logical | `!is.null(hist_sim())` | `!is.null(live$historical)` |
| `so` | list | `hist_sim()$so` | `live$so` (captured at submit, same object as the committed one would get) |
| `has_weights` | logical | `isTRUE(hist_sim()$has_weights)` | `isTRUE(live$has_weights)` |
| `has_draws` | logical | `!is.null(hist_sim()$chol_obj)` | `isTRUE(live$has_draws)` |
| `residuals` | chr | `hist_sim()$residuals %\|\|% residuals() %\|\|% "original"` | `live$residuals` |
| `analysis_unit` | chr/NULL | `hist_sim()$analysis_unit` | `live$analysis_unit` |
| `sim_summary` | list | `hist_sim()$sim_summary` | `live$sim_summary` (at minimum `historical_years`) |
| `hist_label` | chr | `hist_sim()$hist_label` | `live$hist_label` |
| `svy_nrow` | integer/NA | `nrow(hist_sim()$svy %\|\|% hist_sim()$survey)` | `live$svy_nrow` |
| `scenario_names` | chr | `names(saved_scenarios())` (or `character(0)`) | names of landed scenarios, in landing order |
| `scenario_n_models` | named integer | `lengths(lapply(saved_scenarios(), function(s) s$pipelines))` or `$n_models` | per-partial `n_models` |
| `pending_names` | chr | `character(0)` | planned scenario names (from `live$plan`) minus landed |
| `methods_available` | chr | `unname(hist_aggregate_choices(so$type, so$name))` | `live$method` (single entry) |
| `locked_method` | chr/NULL | `NULL` | `live$method` (disables other pills) |
| `locked_pov_line`, `locked_bandwidth` | numeric/NULL | `NULL` | values captured at submit (disable the numeric inputs) |
| `hist_agg(method, pov_line, bandwidth)` | function returning `list(unweighted=, weighted=)`, each `list(<method>=tibble)` (lazy-list or plain) | `.get_hist_agg(method)` (existing workspace, existing cache key) | wrap `live$hist_tbl` in the same shape; return `NULL` if `method` differs from `live$method` |
| `scn_agg(method, pov_line, bandwidth)` | named list label -> same shape, or `NULL` if none | `.get_scn_agg(method)` | wrap `live$scn_tbl[[label]]` for landed labels only |
| `progress` | list (`groups_done`, `groups_total`) | `NULL` | from `live_run()` (banner only) |

Notes: `.lazy_aggregation_table()` already accepts plain tables, so provisional `hist_agg`/`scn_agg` can return plain nested lists with no lazy wrapper. The `pov_line`/`bandwidth` arguments exist only so the committed path keeps its cache key; the provisional path ignores them (inputs are disabled).

**Stay committed-only (read `hist_sim()`/`saved_scenarios()` directly, not through `results_source()`)**: `agg_workspace` (and so `.get_hist_agg`/`.get_scn_agg` in committed mode, and the 5.5 seeding hook), `incidence_data_rv`, every `wise_export_*` fun, `aggregation_cache()`, the stale/cache-clear observer, and the Diagnostics return API (recommended).

**Become pure functions of `results_source()`** (all S rows): `agg_methods`, `.selected_method` (via `methods_available`/`locked_method`), `hist_agg_rv`, `scenario_agg_rv`, `weight_key`, `has_draws`, `selected_scenario_names`, `hist_ref_val`, `hist_F_agg_ref`, `agg_hist`, `derived_results_frame_rv` and everything downstream (pointrange/headline bands, curves, variance breakdown, exceedance, threshold table, adverse dot data, headline cards), `simulation_summary_ui`, `headline_cards_ui`, `output$annual_distribution_plot`, `output$adverse_dot_plot`, `output$exceedance_plot`, `output$uncertainty_sources_plot`, `output$summary_threshold_table`. Provisional caveat: `hist_F_agg_ref`/`.apply_contrast_sd` need the `F_agg_all` matrix column in the partial tables; it is present in the `aggregate_pipeline_table()` output, so the worker must not strip it (otherwise deviation modes would silently fall back to level-CI SDs; flag in the parity test).

The tab-insertion observers (1930, 1998, 2012) key on `results_source()$has_data` and `results_source()$so`; they are L and stay lifecycle observers but must (a) not rebuild the pane when only `mode` flips committed to provisional with an unchanged `so`, and (b) re-send the control disabled state after `insertUI` (see below).

### `wise_export_*` registrations (all committed-only)

All are in `R/mod_2_02_results.R`; each `fun` reads reactives that, in provisional mode, would be fed by `results_source()`, so each must be gated on `results_source()$mode == "committed"` (return `NULL`/empty, or better, read a committed-only frozen view).

| Line | Key | Kind |
|---|---|---|
| 1189 | `climate_adverse_support` | table (reads `attr(threshold_table_rv(), "adverse_support")`) |
| 1604 | `climate_headline_summary` | table |
| 1633 | `climate_outcome_distribution` | figure (`pointrange_chart`) |
| 1691 | `climate_distributional_incidence` | figure |
| 1699 | `climate_distributional_incidence_data` | table |
| 1727 | `climate_annual_distribution` | figure |
| 1735 | `climate_annual_distribution_data` | table |
| 1786 | `climate_uncertainty_sources` | figure |
| 1841 | `climate_adverse_return_periods` | figure |
| 1850 | `climate_outcome_thresholds` | table |
| 1904 | `climate_exceedance_curve` | figure |

Since the shared closures (`threshold_table_df`, `annual_distribution_chart`, `adverse_dot_chart`, `exceedance_chart`, `uncertainty_chart`, `pointrange_chart`, `headline_cards_data_rv`) feed both display and export, the cleanest gate is a wrapper at registration time: `fun = function() if (committed()) <original>() else NULL` (check how `wise_export_*` treats a NULL/empty result before relying on that). Alternatively, build exports from the existing committed-mode reactive chain duplicated over `committed_source()`; not recommended (doubles reactives).

### Output ids and `suspendWhenHidden`

Explicit `outputOptions(..., suspendWhenHidden = TRUE)` (this is also the Shiny default, so they are no-ops in effect, kept for documentation):

- `uncertainty_sources_plot` (line 1784)
- `adverse_dot_plot` (line 1839)
- `annual_distribution_plot` (line 2029)
- `summary_threshold_table` (line 2030)
- `exceedance_plot` (line 2031)

Other outputs with default (suspended when hidden): `stale_banner` (381), `simulation_summary_ui` (392), `headline_cards_ui` (433). No output uses `suspendWhenHidden = FALSE`, so charts only render while the Results tab is visible; a provisional render therefore costs nothing while the user is on another tab. (Provisional partials will still invalidate `results_source()` reactives, but lazy reactive consumers do not run until an output requests them.)

### Changes needed for provisional UI (inspection only, no code edited)

**(a) `pill_toggle()` disabled state** (`R/utils_ui.R:966`, CSS `inst/app/www/custom.css` near line 1559).

Current: `pill_toggle(inputId, choices, selected, label, width, choiceNames, choiceValues, extra_class, layout)` wraps `shiny::radioButtons` and only appends classes. No server-side update path exists, and `inst/app/www` has no custom message handlers (only `inst/app/vendor/hexmap.js` registers one), and `shinyjs` is not allowed.

1. Add two arguments: `disabled = character(0)` (choice *values* to disable; `TRUE` means all) and `disabled_tooltip = NULL`. Initial render: for each `<input type=radio value=v>` whose value is in `disabled`, add `disabled` and `aria-disabled="true"` on the input and class `pill-disabled` plus `title = disabled_tooltip` on the enclosing `label.radio-inline` / `.form-check-inline` (use `htmltools::tagQuery(rb)$find("input")`; do this after building `rb`, before the class append). The selected pill is never disabled.
2. Add `update_pill_toggle(session, inputId, disabled = NULL, tooltip = NULL)` in `utils_ui.R`. It calls `session$sendCustomMessage("wise_pill_toggle_state", list(id = session$ns(inputId), disabled = I(as.character(disabled)), tooltip = tooltip))`. Do not use `session$sendInputMessage`: the radio binding only understands `value`/`label`/`options`.
3. Add a small handler to a new `inst/app/www/pill_toggle.js` (loaded the same way other `www` assets are): find `input[name="<id>"]`, set `.disabled` and toggle `pill-disabled` on the parent label per value, set `title`. `disabled = I(character(0))` clears everything (re-enable).
4. CSS: `.pill-toggle .pill-disabled { opacity: .45; cursor: not-allowed; }` and `.pill-toggle .pill-disabled > span { pointer-events: auto }` (the radio input itself already has `pointer-events: none`, so clicks reach the label; the `disabled` radio then ignores the click and no `input$` change is sent). Do not put `pointer-events: none` on the label, or the `title` tooltip will not show.
5. Server wiring in `mod_2_02_results_server`: one `observe()` on `results_source()$mode` (and on `results_tab_added()`/the insertUI flush) calls `update_pill_toggle(session, "cmp_agg_method", disabled = setdiff(all_choices, locked_method), tooltip = "Available when the simulation completes")` when provisional, and `disabled = character(0)` when committed. Because this derives from `mode`, every exit path (complete, cancel, stale, failure) re-enables with no extra code. The re-run path rebuilds the pane via `insertUI`, which discards client state, so re-send inside `session$onFlushed(once = TRUE)` after the insert. `pov_line` (a `numericInput`) cannot use the pill handler; send `{id, disabled}` for it through the same handler (generalise the handler to also match `#id` inputs by `.prop("disabled")`), and also gate its `conditionalPanel` hint. Keep the `.selected_method()` guard: if `cmp_agg_method` ever differs from `locked_method` in provisional mode, `.selected_method()` must return `locked_method`.

**(b) Pending-scenario placeholder categories** (`R/fct_sim_compare.R`, `R/fct_policy_sim_compare.R`, `R/utils_ui.R`).

Shared design: add a `pending = character(0)` argument (scenario labels still computing) to each builder, build the fixed category order from `union(present, pending)` with the same sort/ordering rules as today, and render pending categories as an axis label with a muted style (`axisLabel.rich` with grey colour, label suffixed with "(computing)") plus no data series. Because ordering is derived from the union, a scenario landing later does not shift any other category. `pending` is passed from `results_source()$pending_names`; committed mode passes `character(0)` and output must be identical (covered by the characterisation snapshot).

- **`echart_annual_distribution()`** (`fct_sim_compare.R:3086`) is a thin wrapper over `echart_step3_annual_distribution()` (`fct_policy_sim_compare.R:218`), which Step 3 also uses. Add `pending` to both (default `character(0)` keeps Step 3 unchanged). In the inner builder: `scenario_levels <- c("Historical", sort(unique(c(df$scenario[df$scenario != "Historical"], pending))))`; `n_rows <- length(scenario_levels)`; `y_breaks <- seq_len(n_rows)` instead of `sort(unique(df$row_y))` (today a category without data would vanish); `band_ys` is computed from `y_breaks` already. Palette: the SSP shading loop uses `scenario_levels`, so pending members get their final shade automatically; use the muted colour only on the axis label (`y_labs`), appending "\n(computing)".
- **`echart_pointrange_climate()`** (`fct_sim_compare.R:2765`) -> `.pointrange_prep()` (`:144`): add `pending` to both. Before the `ordered_levels` loop, build `fut_df_levels <- rbind(fut_df[, c("ssp_short","yr_lbl")], parsed pending)` where pending labels go through `.normalise_ssp`/`.parse_year`/`SSP_SHORT_LABELS` as for real rows; use it for `ssps_present`/`yrs_present`/`ssp_yrs`. In the series loop nothing is drawn for pending (no row in `df`); only `ordered_levels` gains the category, and `x_label_map` gets the muted "(computing)" label. Note: in the current module this builder is used only by `pointrange_chart()` (export-only; there is no mounted Results output), so it is low priority and can be skipped for v1.
- **`echart_step2_adverse_dot()`** (`:2948`): add `pending`. `scenario_levels` (used for colours and for the dodge slot) must include pending so dumbbell slots in each return-period row stay fixed: change the `dodge_offset` computation to use `length(scenario_levels)` and `match(scenario_key, scenario_levels)` slots instead of `k = length(idx)`, otherwise points shift when a scenario lands. Add a legend entry per pending scenario with a muted marker and name "<label> (computing)" (an empty `scatter` series with no data).
- **`echart_exceedance()`** (`:3458`): add `pending`. Today palette order follows the data; add an empty dashed grey `line` series per pending label (name "<label> (computing)") so the legend order is stable. The curves themselves are unchanged.
- **Headline cards** (`step2_headline_cards()` at `:1076`, `headline_cards_ui()` at `utils_ui.R:247`): the cards are not per scenario today (5 fixed cards; the future cards use the *first* future row as `focus`). Concrete: add `pending = character(0)`; when `pending` is non-empty and there are no future rows, return the historical card(s) plus skeleton cards for the future-dependent ones; in `headline_cards_ui()` the existing `card$class` hook already adds a class, so emit `class = "skeleton"`, `value = "Computing..."`, and add `.headline-card.skeleton` CSS (muted background and text). If the plan's "one skeleton per pending scenario" is wanted instead, that is a new per-scenario card layout and should be confirmed with the user first (flagged as ambiguous; recommended: keep five fixed cards, skeleton the future ones until the first future partial lands, then fill them and keep `focus` as the first landed future scenario, noting "n of N scenarios").
