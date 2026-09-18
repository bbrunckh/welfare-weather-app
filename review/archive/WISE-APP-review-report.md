# WISE-APP — Outstanding Items for Implementation

**Source review:** 2026-09-08  
**Purpose:** Release-gate and deferred-work backlog only. Completed and historical remediation details are intentionally omitted.

## Release gate

### 1. Complete clean-environment package verification

**Priority:** Release blocker  
**Related ID:** TEST-05.

`R CMD build` succeeds, but the current built-source check has three unresolved test failures: one missing optional `furrr` dependency and two deferred-import assertions. Existing warnings/notes also need review in an environment with TeX and valid clock/HTML tooling.

Actions:

- Resolve the `furrr` test dependency issue by declaring, installing in CI/check, or explicitly skipping the test when unavailable.
- Fix or correctly update the two deferred-import assertions.
- Run built-source `R CMD check` in a clean environment with TeX and valid HTML/clock tooling.
- Classify each remaining warning/note as fixed, accepted with rationale, or release-blocking.

**Acceptance evidence:** Clean build and check logs, with zero unexpected test failures; documented disposition for any unavoidable notes.

## Deferred work

### 2. Run real backend authentication integration tests

**Priority:** Deferred  
**Related ID:** DEP-02.

Test every supported backend authentication contract against real, non-production test backends. The existing UI/configuration cleanup is complete; this task validates that the supported contracts actually connect and fail safely.

Cover:

- S3 credentials supplied through environment-based authentication, including blank UI fields falling through to environment credentials.
- GCS authentication using the supported HMAC-key contract and environment fallback.
- Azure authentication using Account Key as documented by the app.
- Failure behavior for invalid, incomplete, expired, or unavailable credentials/backends.
- Confirmation that secrets are not surfaced in notifications, logs, provenance, exported configuration, or browser payloads.

**Acceptance evidence:** A documented test matrix with backend, auth mode, outcome, and date; automated integration coverage where feasible; remediation of any contract mismatch; explicit skip rationale for any backend that cannot be tested.

### 3. Consolidate batch simulation scripts

**Priority:** Deferred; not a release blocker  
**Related ID:** RED-06.

Replace the six near-identical `batch/04_run_sim_*.R` scripts (approximately 11,500 lines total) with one canonical runner parameterized by `WISEAPP_COUNTRY` and YAML configuration.

Preserve current output contracts and country-specific behavior. Add regression or golden-output checks before removing the legacy scripts.

**Acceptance evidence:** One documented canonical runner, migrated country configurations, and parity checks for representative country runs.

### 4. Extract shared Step 2/Step 3 result plotting internals

**Priority:** Deferred; maintainability  
**Related ID:** DUP-03.

Reduce duplication between `fct_policy_sim_compare.R` and `fct_sim_compare.R` by extracting common series assembly, threshold-table generation, and exceedance rendering helpers.

Maintain module-specific cache, failure-ledger, and policy semantics. Do not combine code merely for structural similarity if it obscures distinct behavior.

**Acceptance evidence:** Shared helpers with targeted tests demonstrating unchanged Step 2 and Step 3 tables/plots, plus removal of duplicated implementations.

### 5. Characterize prediction-matrix reuse before refactoring

**Priority:** Deferred; performance with numerical risk  
**Related ID:** PERF-15.

Investigate reuse of the RIF-first prediction matrix between `R/fct_simulations.R` and `R/fct_predict_outcomes.R`. Retain `predict.fixest()` as the prediction oracle unless characterization proves equivalence.

The main risk is changing `fixest` row-dropping and offset behavior. Benchmark representative workloads and compare predictions, exclusions, offsets, and downstream simulation results against the current implementation.

**Acceptance evidence:** Benchmark and characterization report; golden/parity tests covering missing rows and offsets; refactor only if outputs are validated against `predict.fixest()`.

## Recommended execution order

1. Complete the clean-environment built-source package check.
2. Schedule backend authentication integration tests, RED-06, DUP-03, and PERF-15 as non-release work.

## Scope notes

Map optimization, session-end DuckDB cleanup, credential-safe configuration exports, aggregation consolidation, README regeneration, and weather-reference consolidation are complete. Do not reopen historical MapLibre/Leaflet migration notes unless a fresh deployment exposes a current defect.