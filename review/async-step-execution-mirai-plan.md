# Asynchronous Step Execution Plan

## Objective

Keep the Shiny event loop responsive while expensive Step 1, Step 2, and Step 3 work executes sequentially in one background worker daemon. Preserve current serial computation semantics, reactive contracts, atomic result publication, cancellation, and stale-result protection.

This is a responsiveness and orchestration change, not a claim of faster numerical execution.

## Deployment Context

The deployment UI currently shows:

- Max RAM: `0 GiB` (no limit)
- Min processes: `1`
- Max processes: `8`
- Max connections per process: `1`
- Load factor: `0.5`
- Initial timeout: `120 s`
- Idle timeout per process: `60 s`
- Connection timeout: `3600 s`
- Read timeout: `3600 s`

The proposed architecture assumes one heavy job per Shiny process at a time. It does not require increasing max processes or connections. Process/RAM settings must be tuned only after measuring concurrent-user memory, because a background daemon adds a separate R process and duplicates serialized inputs/model state.

## Recommended Architecture

### 1. One daemon pool per Shiny worker process

Initialize mirai at application startup, not inside a reactive observer:

```r
mirai::daemons(1L)
```

Shut it down with the application lifecycle:

```r
onStop(function() mirai::daemons(0L))
```

Start with one daemon. This serializes heavy Step 1/2/3 jobs and prevents memory multiplication. Do not start one daemon per session unless deployment capacity and RAM are explicitly budgeted.

Use `mirai::ExtendedTask` or an equivalent application coordinator so the main process submits work and receives completion callbacks through Shiny's event loop.

### 2. Shared FIFO job coordinator

Add a small coordinator with explicit states:

```text
idle -> queued -> running -> succeeded
                         -> failed
                         -> cancelled
                         -> stale
```

Each job record contains:

- `job_id`
- session identifier
- generation number
- job kind: `step1_stats`, `step1_model`, `step2_simulation`, `step3_policy`
- dependency signature
- immutable input snapshot
- status and timestamps
- cancellation handle
- artifact directory/manifest

The coordinator must dispatch only one heavy job to the single daemon. A later job queues rather than running concurrently. Job completion triggers dispatch of the next eligible job.

Do not allow a Step 2 run to begin from stale Step 1 inputs. A dependency-aware queue may mark dependent jobs stale when their predecessor changes.

## Worker Boundary

Worker entry points must be package-level functions, never closures capturing a Shiny server environment:

```r
run_step1_stats_worker <- function(snapshot) { ... }
run_step1_model_worker <- function(snapshot) { ... }
run_step2_worker <- function(snapshot) { ... }
run_step3_worker <- function(snapshot) { ... }
```

The worker receives only serializable values:

- plain data frames and lists
- model specifications or explicitly serializable model snapshots
- connection parameters, not live connections
- content-addressed cache keys and artifact paths
- deterministic seed
- job ID and generation

The worker must never receive:

- `session`
- reactive expressions or `reactiveVal` objects
- `DBIConnection` objects
- Shiny progress objects
- promises or mirai handles
- functions closing over the server environment

Every worker-created DuckDB/remote connection must be opened and closed inside the worker using `on.exit()`.

## Result and Artifact Contract

Do not return large Step 2/3 result objects through mirai by default. Return a small manifest after writing artifacts atomically:

```r
list(
  job_id = job_id,
  generation = generation,
  status = "succeeded",
  artifact_root = artifact_root,
  result_signature = result_signature,
  payload_manifest = payload_manifest,
  warnings = warnings
)
```

Use the existing prepared-weather cache and weather-store/reference-storage mechanisms where possible. Worker writes must use staging plus atomic publication. A failed or cancelled job must remove its staging directory and release any worker-owned leases.

The main Shiny process validates the manifest, checks job/generation/signature, then loads or references the result and updates reactive values. Reactive state is never mutated in worker code.

## Step Boundaries

### Step 1: survey/weather/outcome statistics

First determine which operations are actually expensive. Keep small reactive summaries synchronous unless profiling shows they block the UI.

Likely candidates:

- survey statistics over large loaded data
- weather statistics over large historical panels
- outcome statistics over large survey frames
- Lasso
- model fitting and post-fit diagnostics

Capture all inputs at click time. Do not let workers read live reactive state. Publish the completed result only after all requested statistics or model artifacts succeed.

The existing model-fit observer currently performs Lasso/model work directly in `mod_1_06_model.R`; refactor the heavy computation into a package-level worker function and leave validation, notifications, and reactive assignment in the main process.

### Step 2: weather simulation

This is the best first migration target because `fct_run_simulation()` already has a relatively clean function boundary and returns a structured result.

Move the call currently made inside `mod_2_01_weathersim.R` into `run_step2_worker(snapshot)`. The worker should:

1. Load/open data using worker-local connection parameters.
2. Build or reuse prepared weather through the content-addressed cache.
3. Run the existing serial simulation path.
4. Write large payloads/weather references atomically.
5. Return a result manifest.

On completion, the main process applies the existing metadata (`hist_label`, `sim_summary`, `.sig`, lease handling) and updates `hist_sim()` and `saved_scenarios()` in one commit.

Step 3 consumers already observe these reactive values, so successful publication automatically invalidates Step 2 results/diagnostics and exposes the new baseline to Step 3.

### Step 3: policy simulation/decomposition

Move the expensive body of the policy observer in `mod_3_06_policy_sim.R` into `run_step3_worker(snapshot)`.

Keep the existing atomic publish behavior on the main process. The worker computes:

- policy-modified survey data
- baseline and policy simulations
- policy effects
- decompositions
- adverse decompositions
- diagnostics snapshots

The main process publishes all reactive values only after the complete worker manifest validates. This preserves the current guarantee that a partial policy run never replaces a previous successful run.

Step 3 should depend on a captured Step 2 signature. If Step 2 changes while Step 3 is running, discard the old Step 3 result.

## Automatic Handoff

Use explicit completion callbacks:

```text
Step 1 success
  -> publish model/statistics reactives
  -> invalidate dependent Step 2 state
  -> optionally enqueue requested Step 2 job

Step 2 success
  -> publish hist_sim and saved_scenarios
  -> Step 2 results/diagnostics reactives invalidate
  -> Step 3 sees the new baseline

Step 3 success
  -> publish policy results/decompositions/diagnostics
```

Do not call Step 2 or Step 3 directly from inside the worker. Handoff occurs only after the main process validates and publishes the predecessor result.

For automatic chained runs, store a workflow generation and dependency signatures. A failed predecessor cancels dependent queued jobs and leaves the last successful reactive result visible.

## Staleness and Cancellation

Every submitted job captures:

- `generation`
- model signature
- Step 2 signature or Step 3 policy signature
- selected survey/weather/outcome signatures
- seed

On completion, publish only if all signatures still match current session state. Otherwise mark the result stale and clean up artifacts.

Cancellation requirements:

- expose a cancel action while a job is running
- call `mirai::stop_mirai()` on the active handle
- stop accepting dependent work for the cancelled generation
- clean worker temporary directories and release weather leases
- restore the UI to idle/cancelled without destroying the previous successful result

Use a per-session cancellation handle, but keep scheduling global to the process daemon.

## Progress and UI Responsiveness

Do not update Shiny progress objects inside workers.

Initial version:

- use `bslib::input_task_button()` or equivalent to disable only the active run button
- show queued/running/succeeded/failed/cancelled status
- keep a timer or status output reactive to prove the session remains responsive

Later version:

- worker emits coarse stage events: `loading`, `fitting`, `weather`, `simulation`, `finalizing`
- main process receives events and updates a reactive status value
- per-key progress is deferred because it adds coordination and serialization overhead

## Memory and Deployment Tuning

The daemon is an additional R process. With max processes set to 8 and one connection per process, the practical capacity is determined by both Shiny process count and daemon RAM. Measure process-tree RSS for:

- idle Shiny process plus idle daemon
- Step 1 model job
- Step 2 cold and warm jobs
- Step 3 policy job
- two simultaneous users queued on one daemon
- multiple Shiny processes each with one daemon

Recommended initial deployment posture:

- daemon count: `1` per Shiny worker process
- max connections per process: keep `1` unless load testing proves safe
- max processes: start lower than `8` if each process owns a daemon and RAM is constrained
- max RAM: configure a real budget rather than `0` once peak RSS is characterized
- load factor: retain `0.5` initially; increase only after queue/RSS tests
- initial timeout: greater than worst-case daemon startup and first job submission
- read/connection timeout: retain `3600` for long runs
- idle timeout: ensure it does not repeatedly destroy/recreate daemons during active use

Increasing max processes without an explicit RAM budget may multiply Step 2/3 memory. Increasing max connections per process can violate the one-heavy-job invariant unless the coordinator rejects concurrent heavy jobs.

## Implementation Sequence

1. Add mirai/promise dependencies and app-lifecycle daemon setup behind an environment feature flag. **Implemented for Step 2; async is enabled by default (`WISEAPP_ASYNC_STEP2=1`) and `WISEAPP_ASYNC_STEP2=0` is the rollback switch.**
2. Implement a coordinator with FIFO queue, one active job, generation/signature checks, cancellation, and cleanup. **Implemented in `R/fct_step2_async.R`; one daemon per Shiny process, FIFO across sessions, newest same-session request replaces queued/active work.**
3. Add a minimal generic worker/result-manifest contract and tests. **Worker and atomic RDS manifest contract implemented; tests remain to be added.**
4. Migrate Step 2 only; preserve serial internals and existing result publication. **Implemented: `step2_compute()` runs unchanged numerical code in the worker; the main process retains existing reactive publication and lease handoff.**
5. Add a UI responsiveness test and a Step 2 success/failure/cancellation integration test.
6. Migrate Step 3 policy simulation using the same coordinator and atomic publish contract.
7. Migrate expensive Step 1 model/Lasso work.
8. Profile survey/weather/outcome statistics and migrate only material operations.
9. Run concurrent-user load tests and tune process/RAM/connection settings.
10. Enable by default only after artifact cleanup, stale-result, cancellation, and RSS tests pass. **Async Step 2 is currently enabled by default per implementation request; the remaining test and load-validation work is tracked below.**

## Implementation Status

- [x] Process-wide one-daemon Step 2 coordinator and lifecycle shutdown.
- [x] Serializable Step 2 snapshot with credentials removed before worker submission.
- [x] Worker-side package initialization and environment-only credential lookup.
- [x] Atomic result and manifest artifacts under `WISEAPP_ASYNC_ARTIFACT_ROOT` or a process-specific `tempdir()` fallback.
- [x] Shared FIFO scheduling, stale generation/signature suppression, and hard cancellation.
- [x] Subtle `Stop simulation` control and queued/running status display.
- [x] Indeterminate proxy progress UI with coarse local stage labels; worker status-file writes and polling removed to reduce coordination overhead.
- [x] Persistent daemon package initialization via `mirai::everywhere()`; jobs no longer call `pkgload::load_all()` repeatedly.
- [x] Immediate queued progress state and initial 2% progress bar before deferred snapshot submission.
- [x] Main-process lease adoption and atomic reactive publication.
- [x] Add worker/coordinator/manifest contract tests (transport normalization, credential stripping, manifest validation, queued cancellation cleanup, adopted-artifact preservation).
- [x] Verify synchronous Step 2 numerical parity with a real cross-process mirai worker using deterministic weather/pipeline fixtures.
- [ ] Verify cancellation, stale completion, artifact cleanup, lease cleanup, and UI responsiveness.
- [ ] Measure worker and process-tree RSS under concurrent sessions before deployment tuning.

Latest validation: the complete source-loaded `testthat` suite passes after
normalizing async worker date/scenario fields and replacing the unsupported
`clock-o` icon with `spinner`. The suite emits existing DuckDB/plot warnings
but no test failures.

Focused validation: `test-step2-async.R` and `test-step2-compute.R` pass,
including the real cross-process worker parity test. The full suite was not run
after these focused changes.

Mori was evaluated for shared Step 2 snapshots but rejected for production.
It caused fixest ALTREP/formula errors, tibble row-name warnings, and additional
startup/transfer overhead in the real app. Step 2 snapshots remain ordinary
serialized R values; no Mori dependency or runtime sharing is used.

The async dispatcher uses a configurable 512 MB
queued-payload cap (`WISEAPP_ASYNC_QUEUE_MEMORY_MB`) and retries jobs when
capacity is unavailable.

Startup and transfer diagnostics are now opt-in with
`WISEAPP_ASYNC_METRICS=1`; the default avoids an extra object-size and full
snapshot serialization pass. Async jobs record click-to-submit and
submit-to-worker timings in their status artifacts. Worker initialization now
uses `pkgload::load_all()` only when `wiseapp` is an explicitly detected
development package; installed deployments load the installed namespace
directly and never invoke `pkgload::load_all()`.

The async Step 2 path now preserves the synchronous weather settings, defaulting
to in-memory weather storage and honoring `WISEAPP_STEP2_WEATHER_STORAGE`,
`WISEAPP_STEP2_WEATHER_COLLECT`, and `WISEAPP_STEP2_WEATHER_THREADS`. Submission
is deferred until after the queued UI flush, while reactive reads are isolated;
this lets the initial progress state paint before snapshot capture. Worker
memory is reclaimed at the start and end of computation with `gc()`.

Performance rollback: prepared-weather cache execution is now opt-in again;
the default `prepared_weather_cache` mode is `"off"`, while explicit cache
manifests and cache tests remain supported. The hot production path therefore
uses direct weather loading and keeps mirai async orchestration without the
cache staging/assembly overhead. A one-cold BFA benchmark (2018 + 2021, RIF,
`t`, SSP3-7.0, 2025-2035, memory weather) measured 28.79 seconds total,
27.68 seconds weather, 6.35 seconds pipeline, zero result-assembly seconds,
and approximately 3.14 GB sampled process-tree RSS. The full package test
suite passed after this selective rollback, with existing DuckDB/plot warnings
only.

Focused benchmark with the deterministic Step 2 fixture (same seed, memory
weather mode, five repetitions) measured a synchronous median of approximately
1 ms and an async median of approximately 269 ms. This fixture is intentionally
small and demonstrates fixed orchestration overhead, not production workload
performance. A production BFA snapshot was not available in the local
workspace, so no claim is made about the 32-second interactive run from this
benchmark. The full suite was not run.

Validation note: R syntax validation passed. Full package tests
were not runnable in the current environment because existing required packages
`brand.yml`, `katex`, `ranger`, and `xgboost` are not installed.

## Required Tests

- Worker functions accept only serializable snapshots.
- DBI connections are created and closed inside workers.
- Successful Step 2 result exactly matches synchronous output fingerprint.
- Successful Step 3 result exactly matches synchronous output fingerprint.
- Step 1 model/statistics result parity.
- FIFO scheduling with one daemon.
- A second job queues and does not run concurrently.
- Failed predecessor prevents dependent handoff.
- Cancellation cleans temporary artifacts and preserves previous result.
- Stale completion cannot overwrite a newer run.
- Atomic artifact publication leaves no partial manifest.
- Weather-store leases survive worker completion and release on failure/cancel.
- Shiny timer/UI remains responsive while a worker sleeps or runs.
- Worker and main process RSS are measured separately and as process-tree RSS.
- Multiple sessions do not exceed the configured RAM budget.

## Acceptance Criteria

The first production-ready milestone is not a speedup. It requires:

- main Shiny process remains responsive during Step 2
- one heavy job per process is enforced
- Step 2 output parity is exact
- cancellation and stale-result handling are reliable
- no leaked DuckDB connections, daemons, leases, or artifact directories
- documented peak process-tree RSS
- deployment process/RAM settings validated under concurrent-user load

Only after this milestone should additional daemon capacity or any key-level parallelism be reconsidered.
