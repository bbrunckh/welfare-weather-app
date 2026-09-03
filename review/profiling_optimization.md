# wise-app — Profiling, Responsiveness & Throughput Plan (v3)

This revision separates two objectives that were previously mixed together:

1. **Responsiveness:** long computations must not block a user's Shiny session.
2. **Throughput:** Step 2 should complete faster without exceeding production memory limits.

The first objective does not require parallelizing the simulation key loop. The second must be justified by production measurements.

## Engineering principles

Apply these throughout the work rather than treating them as a separate cleanup exercise.

- **Shared core, thin adapters.** Shiny modules and batch scripts should call the same canonical compute API. Keep UI state, progress, logging presentation, and deployment setup outside the computational core.
- **Consolidate behavior, not names.** Combine implementations only when their algorithm, input contract, output schema, ordering, side effects, and failure semantics are equivalent. Preserve thin context-specific wrappers when business meaning, defaults, permissions, or user-facing behavior differ. Do not replace duplication with a large function controlled by many mode flags.
- **Characterize before consolidating.** Capture current outputs, warnings, errors, ordering, defaults, and side effects in tests before merging duplicate functions or scripts.
- **Stable contracts.** Document the required fields, types, ordering, diagnostics, failure-ledger shape, and version/signature metadata returned by each public compute entry point and per-key function.
- **Validate at boundaries.** Check required columns and types, key uniqueness, finite values where required, scenario-year compatibility, model/simulation signatures, and deterministic ordering once at the public compute boundary. Internal helpers may then rely on those invariants.
- **Dependency direction.** Core computation must not depend on Shiny modules. Shiny and batch layers may depend on core functions, never the reverse. Keep data access separate enough that pure transformations can be tested without credentials or live DuckDB connections.
- **Policy versus mechanism.** Keep scientific/modeling choices distinct from operational controls such as worker counts, cache limits, retries, and diagnostics. Centralize operational defaults and validate all environment-variable overrides.
- **Prefer composition over a giant function.** One orchestration entry point may coordinate small, testable transformations; it should not absorb every implementation detail.

## Scope and established baseline

Do not rework optimizations already implemented and verified:

- Lazy DuckDB data loading and credential caching (`fct_load_data.R`)
- LRU disk cache for weather parquet fetches, bit-identical verified (`PERF-13`)
- RIF KDE hoisted out of the tau loop (`PERF-03`); fit objects slimmed post-fit
- Conditional parallel LASSO with an empirically tuned threshold (`parallel_min_n = 20000`)
- Shared simulation objects and matrix operations hoisted out of the per-key loop where possible
- `.busy_guard()` (`REACT-02`) preventing overlapping fit/simulation runs
- Dated, ID-tagged remediation and verification conventions

Confirmed gaps:

- Step 1 and Step 2 computations run synchronously inside `withProgress()` and block their Shiny session.
- The Step 2 simulation key loop is deliberately serial. A previous parallel implementation was removed after memory problems on large countries.

Step 3 is not assumed to be a throughput bottleneck. It reuses Step 2's baseline and applies the policy arm analytically through `apply_policy_delta_to_baseline()`. Profile its elapsed time before moving it to a worker; asynchronous execution has overhead and may not be warranted for a short operation.

---

## Success criteria

Set the final numeric thresholds after the baseline benchmark, but agree on the decision rules before implementation.

### Responsiveness

- A session-level heartbeat or unrelated test output continues to update while Step 1 or Step 2 runs.
- A second invocation cannot overlap the first for the same session.
- Inputs used by a run are snapshotted at invocation; later UI edits cannot silently alter the in-flight job.
- A result produced from stale inputs is not committed as current output.
- Errors release the busy state and are shown to the user without leaving the session stuck.
- Session closure, worker failure, and deployment restart have documented behavior.

### Correctness

- For deterministic paths, synchronous and worker-based execution is bit-identical on the same fixture, package lockfile, seed, and environment.
- If parallel reduction order or numerical libraries make bit identity impractical, define and approve field-level equivalence tolerances before implementation. Do not weaken the standard after observing a failure.
- Existing `INT-08`, staleness, `REACT-02`, weather-cache, and simulation tests continue to pass.

### Throughput and resource use

- A parallel implementation ships only if the measured **button-to-usable-result latency** on the production-representative large-country workload improves materially. Use **2x** as the provisional gate, including task dispatch, worker startup, data transfer, cache behavior, result assembly, and UI commit.
- Define "production-representative" in every benchmark record by country/workload identity, household count, simulation-key count, enabled feature path, cache state, deployment limits, and concurrent-session scenario.
- It must remain within the production memory envelope under the agreed concurrent-session scenario, with an explicit safety reserve.
- A faster single run is not accepted if it materially reduces service-level throughput or causes unacceptable latency for other sessions.

---

## Phase 0 — Resolve production and history constraints

Complete before designing key-level parallelism. This should take hours, not days.

1. **Recover the prior parallel failure.** Inspect the introducing and reverting commits, deployment logs if retained, and ask the implementer. Classify the failure as OOM/cgroup kill, aggregate RSS growth, swap or memory pressure, excessive serialization, cache contention, worker failure, or negative speedup. Preserve the evidence in a short note.
2. **Confirm the Posit Connect execution envelope.** Record per-process and application limits, whether limits apply to a process or its complete process tree, CPU entitlement, deployment replica count, expected concurrent active jobs, timeout/reaping behavior, and whether child processes are supported under the deployment configuration.
3. **Confirm deployed package compatibility.** Verify the installed/locked Shiny, bslib, mirai, DuckDB, and related versions support the intended APIs on the production R version. Do not design against current online examples if the lockfile is older.
4. **Trace the real Step 2 call graph.** Confirm whether `fct_rif_sim.R`, `fct_sim_diag.R`, weather retrieval, and DuckDB access execute for every key or only for selected modes.
5. **Identify representative workloads.** Choose at least a typical country and the largest supported country. Prefer sanitized real fixtures. Determine whether the batch scripts already exercise these workloads.

**Exit criterion:** enough evidence to construct representative benchmarks and to say whether child-process parallelism is operationally possible. This phase may rule out Phase 5 without affecting the responsiveness track.

---

## Phase 1 — Establish a reproducible baseline

### 1.1 Benchmark harness

Create a development-only harness under `dev/`, following the existing `bench_lasso.R` convention. It must run core functions directly, outside Shiny, using explicit inputs and seeds. Avoid using UI automation as the primary performance benchmark.

Use a small workload matrix rather than one synthetic fixture:

- Typical country, normal feature path
- Largest supported country, normal feature path
- Largest supported country with RIF/diagnostics enabled, if those paths are optional
- Cold-cache and warm-cache variants where weather or DuckDB caches materially affect the run

If a real large-country fixture cannot be retained, create a deterministic synthetic fixture and document how it differs from real survey data. Do not use synthetic LASSO timings to retune production thresholds unless its convergence and sparsity behavior are shown to be representative.

### 1.2 Measurement method

- Record end-to-end elapsed time and per-key elapsed time.
- Sample **aggregate RSS across the complete process tree**, not only the parent R process. Record main-process peak and worker peaks separately where available.
- Record CPU utilization and effective core use to distinguish CPU saturation from I/O waits.
- Measure serialized size and serialization/deserialization time for candidate task inputs and outputs using the actual objects.
- Record cache state, fixture identity, scenario/key count, seed, package lockfile revision, commit SHA, machine/deployment limits, and repetitions with every result.
- Use multiple repetitions and report median plus spread; do not make a go/no-go decision from one run.
- Use `profvis` for R-level diagnosis on Step 1 and one representative Step 2 key. Use explicit timers and OS/container metrics for compiled code and full multi-minute runs.
- Run `reactlog` separately through a Step 1→2→3 walkthrough to detect unintended invalidations across the `INT-08` boundary.

### 1.3 Required outputs

Produce a compact benchmark report containing:

- Step 1, Step 2, and Step 3 elapsed times
- Per-key timing distribution and evidence of key-size imbalance
- Parent/session baseline and peak RSS
- Worker initialized baseline RSS
- Incremental worker peak while processing a key
- Aggregate process-tree peak RSS
- Input/output serialization size and elapsed time
- Cold- versus warm-cache effect
- Candidate bottlenecks ranked by measured contribution
- An agreed production wall-clock target and concurrent-session test scenario

Do not assume that RSS is additive or that worker memory is constant; use measured concurrent-worker runs in Phase 5 before selecting the production worker count.

---

## Phase 2 — Extract pure compute entry points

Do this before introducing `ExtendedTask` or key-level parallelism. It reduces the risk of changing Shiny orchestration and computation simultaneously.

- Extract explicit, testable compute entry points for model fitting/LASSO and Step 2 simulation. A **pure compute entry point** has no reactive reads, Shiny session/progress objects, closures over mutable parent state, live database connections in its arguments, implicit RNG state, or UI side effects. Inputs and outputs must be ordinary serializable R objects or stable data references.
- Define and document a stable result contract for each entry point, including field names and types, canonical key ordering, diagnostics, failure-ledger shape, and run/schema/signature metadata.
- Validate input invariants once at the entry point: required columns/types, unique keys, finite values where required, compatible model/simulation signatures, valid scenario-year combinations, and deterministic ordering.
- Snapshot all run inputs at button invocation. Include a run/signature ID in the task input and result.
- Inventory process-local dependencies: RNG kind and seed, options, locale/time zone where relevant, environment variables, credentials, package namespaces, DuckDB extensions/connections, temporary directories, and caches.
- Separate computational messages from UI progress reporting. Core functions may emit structured events or logs, but must not require a Shiny reactive context.
- Keep the existing serial path as the reference implementation.

**Verification:** add characterization tests before changing structure, then require direct calls through the extracted entry points to reproduce the pre-refactor outputs, warnings, errors, ordering, defaults, and relevant side effects before any asynchronous execution is added. Define the comparison method by artifact: for example, serialized-object hashes where stable, ordered data-frame equality, and explicit field-level tolerances only where approved in advance.

This extraction—not consolidation of the batch scripts—is the prerequisite for later parallel work.

---

## Phase 3 — Make long runs non-blocking

Implement one workflow at a time, starting with Step 2 because it is the clearest long-running user path. Then apply the proven pattern to Step 1. Move Step 3 only if Phase 1 shows that its duration justifies worker overhead.

Use `shiny::ExtendedTask` with the deployed supported backend and `bslib::input_task_button()` where compatible.

### Required behavior

- Pass the immutable input snapshot to the task; do not read reactives inside the worker.
- Establish required process-local state explicitly inside the task.
- Set the RNG seed and RNG kind inside the task body where random draws occur.
- On completion, compare the result's signature with the current session signature before committing it. Mark mismatched results stale rather than overwriting current state.
- Release `.busy_guard()` on success, error, cancellation, and worker loss. Preserve the guard if it enforces cross-action rules that `ExtendedTask` does not; do not remove it solely because a task object rejects duplicate invocation.
- Define whether navigation/input editing is allowed during a run. Disable only controls whose mutation would be unsafe; responsiveness should not imply unrestricted state mutation.
- Replace `withProgress()` with an explicit UX: task-button busy state, clear running text, elapsed time if useful, and success/error status. Do not promise granular progress until there is a tested worker-to-session event channel.
- Define cancellation honestly. If cancellation only stops waiting for a result but does not terminate computation, label and implement it accordingly. Do not add a Cancel button that leaves an expensive orphan job running.
- Log run ID, session/deployment identifier, start/end/status, elapsed time, workload dimensions, and worker failure without logging credentials or household-level data.

### Cache and connection safety

- Confirm each worker opens and closes its own DuckDB connection; never serialize a live connection.
- Review the weather cache for cross-process and cross-session safety now. Use atomic write-to-temp-and-rename and a defined lock/idempotency strategy if concurrent misses can target the same cache key.
- Confirm credentials are acquired safely in a worker and are neither serialized into logs nor assumed to exist only in the parent process's cache.

### Verification

- Synchronous reference versus worker execution, same fixture and seed
- Responsiveness heartbeat test
- Double-click/repeated invocation test
- Input-change-during-run and stale-result test
- Worker error and session-disconnect test
- Cold-worker and warm-worker test
- Existing integration and staleness tests

**Deliverable:** ship the non-blocking serial-compute path independently. This is the minimum viable outcome.

---

## Phase 4 — Improve the serial Step 2 path first

Use the Phase 1 profile to reduce work and peak memory before introducing key-level workers. This makes the default path faster and lowers the cost of any later parallel implementation.

Investigate only measured candidates, including:

- Avoiding repeated materialization or copies of large household/weather objects
- Releasing key-local objects promptly and preventing result lists from retaining unnecessary intermediates
- Reducing returned per-key payloads to the final fields required downstream
- Moving measured aggregation/filtering work into DuckDB when it preserves semantics and avoids R-side materialization
- Chunking or streaming only where it reduces measured peak memory without excessive repeated scans
- Ensuring diagnostics/RIF work runs only when requested

DuckDB push-down is an option, not a predetermined answer. Compiled matrix/statistical work should not be rewritten as SQL unless profiling shows a clear data-movement or aggregation bottleneck.

**Verification:** bit-identical or pre-approved field-level equivalence against the serial reference; repeat the Phase 1 benchmark matrix.

**Decision point:** if the optimized serial path meets the wall-clock target, stop. Do not add process parallelism merely because it is technically possible.

---

## Phase 5 — Bounded key-level parallelism experiment

Proceed only if all of the following hold:

- Phase 0 confirms child workers are supported in production.
- Phase 4's optimized serial path still misses the target.
- Per-key work is sufficiently independent and coarse-grained.
- Serialization and startup costs leave a credible path to the provisional 2x benefit gate.
- Production memory and CPU headroom support more than one active key worker under the agreed concurrency scenario.

### Topology

Do not assume nested `mirai` is the right production topology. Phase 3 may already execute the entire Step 2 job in a worker; spawning a second worker pool from that process adds lifecycle, CPU, and memory complexity. Prototype and measure at least these feasible designs under the deployed package versions:

1. An outer `ExtendedTask` worker running the optimized serial key loop.
2. An outer task coordinating a separate, explicitly named bounded worker pool, if supported safely.
3. A single task layer in which key jobs are dispatched directly and results are assembled in the session or a coordinator, if this integrates cleanly with `ExtendedTask` and staleness handling.

Choose the simplest topology that meets the target. Reject any topology that can deadlock, silently fall back to serial execution, oversubscribe cores, or orphan workers during redeploy/session loss.

### Worker-count gate

Do not use a single algebraic formula as proof of safety. Allocation semantics vary, RSS is not perfectly additive, and concurrent sessions may not peak independently.

Use a calculation only to select candidates:

```text
candidate_workers ≈ floor(
  (job_memory_allowance - coordinator_peak - safety_reserve)
  / measured_incremental_worker_peak
)
```

Then validate candidate counts—starting at 2—with controlled concurrent-worker tests in the same cgroup/container limits as production. Repeat under the agreed number of simultaneous active sessions. The validated worker count, not the formula, is authoritative.

### Implementation requirements

- Use a standalone key function with explicit serializable inputs and no reactive/session state.
- Bound in-flight work; do not enqueue every large key with an unbounded payload at once.
- Prefer dynamic scheduling if key timings are materially imbalanced.
- Preserve deterministic per-key RNG streams independent of worker count and completion order.
- Reassemble results in canonical key order so completion order cannot change downstream behavior.
- Define failure semantics explicitly: historical-key precheck, per-key retries if any, `REACT-12` ledger assembly, partial-result disposal, and fail-fast versus collect-all behavior.
- Use one DuckDB connection per process and the cross-process-safe weather-cache path from Phase 3.
- Add `WISEAPP_SIM_PARALLEL_WORKERS`, defaulting to `1`, where `1` explicitly means the reference serial key loop and does not create an additional key-worker pool. Clamp unsafe values and log the effective count.
- Give every feature flag an owner, documented default and validation rule, operational instructions, and a review date or removal condition.
- Provide an operational rollback procedure in addition to the kill switch.

### Acceptance tests

- Serial versus parallel correctness across worker counts 1, 2, and the proposed maximum
- Identical results across different completion orders and repeated runs
- Worker failure, one-key failure, cache collision, session disconnect, and deployment shutdown behavior
- End-to-end elapsed time, aggregate process-tree RSS, CPU utilization, and service behavior under concurrent sessions
- Benefit gate met on the production-representative largest workload

If the experiment fails either the memory or benefit gate, retain the optimized serial path and do not ship key-level parallelism.

---

## Phase 6 — Revalidate LASSO thresholds

Run after the Phase 1 harness exists and independently of Step 2 parallelism.

Recheck `nrow(df) > 50000L` in `mod_1_06_model.R` and `parallel_min_n = 20000L` against representative real workloads. Account for worker startup, available cores, and concurrent-session effects. Close with either unchanged constants plus dated benchmark evidence, or a focused change with tests. Do not tune these thresholds from a replicated/jittered fixture alone.

---

## Phase 7 — Batch and duplication cleanup

Keep this out of the critical path unless inspection shows it directly supplies the compute entry point required by Phase 2.

- Consolidate the six near-duplicate `batch/04_run_sim_*.R` scripts (`RED-06`) around the extracted compute API, preserving each script's observable behavior and deployment parameters.
- Use batch execution as an optional low-risk test bed for bounded fan-out, but do not infer interactive Connect safety from batch infrastructure with different memory or concurrency limits.
- Review `fct_policy_sim_compare.R` versus `fct_sim_compare.R` (`DUP-03`) for duplicated computation as well as duplicated code.
- Before consolidating any candidate pair or script family, add characterization tests for outputs, warnings, errors, ordering, defaults, and side effects.
- Consolidate behaviorally equivalent logic into small shared helpers while retaining meaningful wrappers such as policy- and simulation-specific entry points. Avoid a generic function dominated by `mode` branches.
- Record intentional duplication when two implementations have different business semantics or are expected to evolve independently; not every textual duplicate should be removed.

This is a maintenance track, not automatically a prerequisite for production parallelism.

---

## Phase 8 — Hygiene (separate workstream)

Do not mix repository-wide cleanup with profiling, asynchronous orchestration, or numerical changes.

- Run `lintr` and targeted dependency review in separate pull requests.
- Treat dependency removal as its own compatibility task; overlapping-purpose packages are not necessarily interchangeable.
- Run a repository-wide `styler` pass only when functional branches are merged or quiescent. Add the formatting commit to `.git-blame-ignore-revs` if the repository supports that workflow.
- Standardize section banners, remove genuine debugging/dead code, and update stale comments. Preserve intentional operational logging and rationale comments.
- Scope first; do not automatically delete every `print()`, `cat()`, or `message()` without checking whether batch or deployment logs rely on it.
- Standardize structured operational logging around a run ID and fields such as stage, workload/country identifier, key count, cache state, elapsed time, peak memory, worker count, and outcome. Never log credentials or household-level values.
- Add scheduled or release-gate performance regression benchmarks on a controlled runner. Keep numerical correctness tests in normal CI; do not fail shared CI from one noisy timing observation. Compare repeated measurements and investigate material regressions against the recorded baseline.

```sh
grep -RnE 'print\(|browser\(\)|cat\(' R/ batch/
grep -RnE 'TODO|FIXME|XXX' R/ batch/
```

---

## Recommended sequence

```text
Phase 0: constraints and prior-failure evidence
    ↓
Phase 1: reproducible benchmark and resource baseline
    ↓
Phase 2: pure compute entry points
    ↓
Phase 3: non-blocking Step 2, then Step 1; ship independently
    ↓
Phase 4: optimize the serial Step 2 path
    ↓
Stop if target met
    ↓
Phase 5: bounded parallel experiment, only if gates pass

Phase 6: LASSO thresholds — after Phase 1, otherwise independent
Phase 7: batch/duplication cleanup — separate maintenance track
Phase 8: hygiene — separate pull requests outside functional work
```

## Decision record required at each gate

Use a short, dated record containing the commit, fixture/workload definition, deployment envelope, benchmark results, comparison method, correctness result, decision, owner, and any feature-flag review/removal date. Possible outcomes should be explicit:

- Ship non-blocking serial execution only.
- Ship non-blocking execution plus serial memory/time improvements.
- Ship bounded parallelism behind the default-off worker setting.
- Reject parallelism because of memory, service-throughput, correctness, or insufficient-benefit evidence.

