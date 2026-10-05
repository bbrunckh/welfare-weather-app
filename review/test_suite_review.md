# Test Suite and Testability Review

Review of the WISE-APP test suite and the parts of the app architecture that affect testing, with recommendations for best practice, redundancy removal, run time, and GitHub CI. The implementation plan is in `review/test_suite_plan.md`.

- **Reviewed at:** commit `ae06d59` on `dev`, with uncommitted work in progress (2026-10-05).
- **Tooling at review:** R 4.5.3, testthat 3.3.2, shiny 1.14.0, shinytest2 0.5.1, macOS arm64.
- **Method:** static read of `tests/`, `R/`, `DESCRIPTION`, and build files; a timed per-file run of the whole suite under `devtools::load_all()`; a run in the installed-package (`R CMD check`) layout; a `covr` line-coverage run (section 7).

Numbers and file names below are a snapshot. The plan re-derives them before acting.

---

## 1. Summary

The suite is in better shape than most Shiny apps of this size. It has about 890 tests in 80 files, testthat 3rd edition, `testServer()` coverage of the main step modules, `local_mocked_bindings()` instead of hand-rolled mocks, `withr` for most state, mocked HTTP for every cloud backend, and no network calls to real services. A full serial run takes **about 150 seconds**, so run time is not the main problem.

The main problems are structural:

1. **There is no CI.** `.github/` contains only `.DS_Store`.
2. **The suite would not behave the same on CI as it does locally.** Several test-only packages are undeclared, so their tests skip silently. Source-scanning tests break or pass vacuously under `R CMD check`. One fixture downloads a DuckDB community extension.
3. **Shared fixtures don't exist.** There is no `helper-*.R` or `setup*.R`. Each file builds its own survey, weather, and pipeline fixtures, and global environment defaults (cache dirs, data path) are not pinned for the run.
4. **Organisation reflects project history, not the code.** Some files are named after work waves (`w3-a`, `wave2-e`, `perf02`, `phase4`). Some files are grab-bags spread across several `R/` files. A few tests assert Shiny's own semantics rather than app behaviour.
5. **There is no end-to-end layer.** Nothing exercises the browser: MapLibre/hexmap JS, `conditionalPanel` visibility, `update*Input()` restoration on config import, or the real step-to-step flow. `testServer()` cannot reach these.
6. **Coverage is uneven.** Overall line coverage is 74.5%, with the numerical core above 90%. Some user-facing code has little or none: the Step 3 policy-lever modules (about 5%), `mod_1_08_modelfit` (0%), `fct_predict_outcomes` (38%), `fct_step1_headline` (43%), and `app_server` (0%). See section 7.

Recommended target state:

- A three-tier suite: fast unit and module tests by default, a small "process" tier for real worker and browser processes, and one or two `shinytest2` end-to-end tests.
- One GitHub Actions workflow running the first two tiers on every push and PR, plus a separate E2E job.
- Shared helpers and a consistent `test-<R file>.R` layout.

---

## 2. Current state (evidence)

### 2.1 Size and timing

| Measure | Value |
|---|---|
| Test files | 80 (+ `tests/spelling.R`) |
| `test_that()` blocks | ~887 |
| Expectations (last recorded full pass) | ~3970 |
| Serial wall time, all files | **148 s** |
| Files under 1 s | 50 of 80 |
| Files 3 s or more (14 files) | ~105 s of the 148 s |

Slowest files:

| File | Seconds | Main cause |
|---|---|---|
| `test-policy-sim-compare-agg-cache.R` | 16.1 | Many full aggregation-suite rebuilds |
| `test-fct_get_weather.R` | 14.9 | Parquet + H3 fixture builds per test |
| `test-step2-payload.R` | 12.0 | Repeated small Step 2 runs |
| `test-fct-overview-metadata.R` | 11.2 | Real mirai worker; `WISEAPP_METADATA_LOAD_PARALLEL=1` in a cache test (6.2 s for one test) |
| `test-export-bundle.R` | 8.7 | Headless Chrome PNG render (6.0 s for one test) |
| `test-mod_2_02_results.R` | 7.7 | Module + aggregation |
| `test-step2-async.R` | 5.7 | Real mirai daemon (5.5 s for one test) |

Five tests that start a real worker process or browser take about 25 s together, roughly a sixth of the suite. They are also the most likely to be flaky on CI. During the timed run, one printed `Unhandled promise error: Chromote: timed out waiting for response to command Browser.close`.

### 2.2 Failures at review time

Three tests fail on the uncommitted tree. They appear tied to the in-progress adverse-year alignment work, not to the suite design:

- `test-fct-policy-metric-decompose.R`: "technical adverse deciles use the selected member-year support"
- `test-mod_3_07_results.R`: "technical decomposition cannot export logistic policy effects" (error: attempt to apply non-function)
- `test-visualization-contracts.R`: "step2 adverse dot data extracts supported periods and ensemble bounds"

They are recorded here only so a later reader doesn't mistake them for regressions caused by the test work.

### 2.3 What is already good (keep)

- testthat 3e, no `context()`, and no bare `expect_error(expr)` calls without a class or pattern.
- `testServer()` on module servers, asserting outputs and `session$getReturned()` contracts.
- `local_mocked_bindings()` throughout (43 uses). `httr2_mock` covers Databricks, S3, GCS, and HF metadata paths, so tests make no external HTTP calls.
- State-restore helpers for the process-wide DuckDB connection and metadata cache (`.overview_duck_state_restore()`, `.reset_overview_metadata_cache()`).
- Determinism tests (`test-determinism.R`), numerical oracle tests (delta method vs Monte Carlo, kernel vs reference), and characterization tests that pinned behaviour before refactors.
- Structural assertions on echarts/reactable widget payloads (`w$x$opts$series`) instead of image snapshots. That is the right choice for widgets.

---

## 3. Findings

Severity: **High** = blocks reliable CI or hides failures; **Medium** = maintainability or speed cost; **Low** = polish.

### High

**H1. No CI.** Nothing runs the suite automatically. The shiny 1.13 to 1.14 `conditionalPanel` regression documented in `test-conditional-panel-css-contract.R` is the kind of break CI catches on the dependency bump.

**H2. Test-only packages are undeclared, so CI would skip their tests silently.** `r-lib/actions/setup-r-dependencies` installs only what `DESCRIPTION` declares.
- `arrow`, `bit64`, `data.table`, and `duckdbfs` are used behind `skip_if_not_installed()` but not listed. The weather fixture builder in `test-fct_get_weather.R` skips without `arrow` and `bit64`, so about 30 weather tests would turn into skips on CI and still report green.
- `chromote` and `shinytest2` are used or needed but not in `Suggests`.
- In `R/`, `later::` (`fct_step2_async.R`, `fct_overview_metadata.R`) and `tidyselect::` (`fct_decomposition_summary.R`) are used without being declared. `R CMD check` warns about this.

**H3. Some tests only work from the source tree.** `R CMD check` and `covr::package_coverage(type = "tests")` run tests against the installed package from a copy of `tests/`, where `../../R/` and `dev/` do not exist. I reproduced that layout (installed package, `tests/` copied elsewhere, `test_dir(load_package = "installed")`). Beyond the 3 work-in-progress failures, **7 tests in 6 files break** (2 of them possibly a reproduction artifact, see the footnote), and 1 passes vacuously:

| File | Test | Result in installed layout |
|---|---|---|
| `test-export-wiring-contract.R` | dynamic export families have explicit coverage | 8 failures |
| `test-export-wiring-contract.R` | known UI-only outputs are not mistaken for exportable artefacts | 6 failures |
| `test-export-wiring-contract.R` | every literal CSV export key is registered | **passes vacuously** (`setdiff(empty, empty)`) |
| `test-csv-export-wiring-contract.R` | every reactable CSV button has an export-bundle registration | 2 failures (its fallback path is wrong) |
| `test-pipeline-runner.R` | pipeline prerequisite controls render while their pages are hidden | error (`readLines` on a missing file) |
| `test-bench-step3.R` | whole file | error (`source("../../dev/...")`) |
| `test-step2-async.R` | async worker matches synchronous Step 2 fixture output | error (real mirai worker)* |
| `test-fct-overview-metadata.R` | metadata bundle loads in a real mirai worker from verbatim params | error (real mirai worker)* |

\* Probably an artifact of the reproduction. Outside a dev (`load_all`) session the worker runs `library(wiseapp)` from its default library paths, which did not include the temporary library used here. `R CMD check` exports its library through `R_LIBS`, so these two may pass there. Verify on the first CI run.

`test-conditional-panel-css-contract.R` skips correctly in that layout, which shows the problem is known but handled inconsistently.

`covr::package_coverage(type = "tests")` also uses this layout and aborted on these failures during the review.

The source-contract tests are valuable because they catch real wiring bugs. They need one shared "source tree available" helper and a CI layout that runs tests against the source tree.

**H4. A fixture depends on the network.** `make_h3_con()` in `test-fct_get_weather.R` runs `INSTALL h3 FROM community`. On a clean runner this downloads from the DuckDB community repository. The tests then depend on that service and on a matching DuckDB build, and break offline. DuckDB also writes into `~/.duckdb` (the run printed this warning once per connection).

**H5. Global environment is not pinned for the test run.**
- `tools::R_user_dir("wiseapp", "cache")` is the default for the weather and prepared-weather caches. Only 3 files redirect them, so other tests that reach those code paths can read from or write to the developer's real cache.
- A developer `~/.Renviron` with `WISEAPP_DATA_PATH` or `WISEAPP_DATA_SOURCE` changes test behaviour. `test-step2-compute.R` already contains a comment working around this for one test.

### Medium

**M1. No shared fixtures or helpers.** Same-name builders are duplicated across files (`make_weather`, `make_vl`, `make_lasso_fixture`), and there are about 10 near-identical families (`step2_contract_*`, `step2_compute_*`, `phase4_*`, `w3a_*`, `pipeline_fixture`, `svy_fixture`, `policy_args`, …). Fixtures drift between files, and fixing one doesn't fix the others.

**M2. File organisation does not map to `R/`.**
- History-named files: `test-w3-a-aggregation-characterization.R`, `test-w3-b-future-decomposition-characterization.R`, `test-step3-wave2-e.R`, `test-perf02-weather-transformations.R`, `test-ui-migration-step3.R`, `test-rerun-regressions.R`, `test-visualization-contracts.R`.
- Inconsistent separators: `test-fct-outcome.R` vs `test-fct_hexmap.R`, `test-mod-0-overview.R` vs `test-mod_1_01_sample.R`.
- Misleading names: `test-uncertainty-decomposition.R` tests plotting helpers.
- About 76 references to tracking IDs (`PERF-30`, `DUP-01`, `INT-08`, …) in test names and comments. These are fine as a "why" comment and poor as the organising principle.

Consequences: `usethis::use_test()` and the editor's go-to-test jump don't work, and finding which tests cover a function needs a grep.

**M3. Tests of Shiny itself.** In `test-rerun-regressions.R`, these tests build toy servers to show how `observeEvent()` behaves. They document a past bug's mechanism but never call app code:
- "passing a reactive's value to observeEvent deafens it after one fire"
- "passing the reactive itself keeps the dependency alive"
- "ignoreInit swallows the first click when the event expr can throw"

The real-module tests later in the same file already cover the regressions.

**M4. Overlapping and fragmented files** (consolidation candidates; details in section 5):
- Two export-key wiring scanners that differ only in which button helper they scan.
- `make_regtable_df` tests in `test-table-csv-export.R` that belong with the results table tests.
- Step 2 contract, compute, and payload files that build three versions of the same small pipeline.
- Characterization files whose refactors have landed.

**M5. Benchmark smoke test in the unit suite.** `test-bench-step3.R` `source()`s `dev/bench_step3_helpers.R` and `dev/bench_step2_helpers.R`. `dev/` is in `.Rbuildignore`, so this file cannot run under `R CMD check`, and it tests developer tooling rather than the app.

**M6. Slow and flaky process tests are mixed into the default run.** The real mirai worker, headless Chrome, and parallel metadata tests (section 2.1) are worth keeping but should be opt-in locally and always-on in CI, behind one named skip helper.

**M7. Console noise.** App `message()` output (`[wiseapp] Running simulation pipelines...`, `K-means cutoffs for tx: …`, `[active_mask] …`, `[overview] …`) and worker start-up banners fill the test log and hide real warnings. The app has no verbosity switch, so tests can't silence it cleanly.

**M8. No end-to-end coverage of browser behaviour.** Areas that need a browser:

| Area | Why `testServer()` can't cover it |
|---|---|
| `inst/app/vendor/hexmap.js` (MapLibre + h3-js) | JS only; the R tests check the payload contract, not rendering |
| `conditionalPanel` / flyout visibility | CSS and JS; currently guarded only by a source scan |
| Config import (`wise_config_apply()`) | Uses `update*Input()`, which `testServer()` does not apply to inputs |
| The real Step 0 → 3 flow with `app_server` wiring | Module tests stub the upstream reactives |
| `suspendWhenHidden = FALSE` outputs on hidden pages | Rendering lifecycle is browser-driven |

### Low

**L1.** 59 files start with `library(testthat)` / `library(shiny)`. `tests/testthat.R` already attaches testthat, and the test environment inherits the package namespace, which imports shiny. About 220 `wiseapp:::` and 29 `testthat::` prefixes are also redundant, since tests run inside the namespace.

**L2.** 82 `set.seed()` calls change the global RNG for later tests; only 3 tests use `withr::local_seed()`.

**L3.** About 110 `expect_true(identical(...))`, `expect_true(x == y)`, and `expect_true(a %in% b)` assertions. On failure these print only "is not TRUE". `expect_identical()`, `expect_equal()`, `expect_contains()`, and `expect_in()` show the difference.

**L4.** Only 3 files use the same idiom to pull HTML out of a rendered output (`as.character(output$x$html)`). Not worth a helper yet.

**L5.** `tests/spelling.R` runs with `error = FALSE` and no `inst/WORDLIST`, so it never fails and gives no signal.

**L6.** Polling loops with a deadline (`test-step2-async.R`, `test-fct_wx_cache.R`) are acceptable as written. No change needed, but they belong to the process tier (M6).

---

## 4. Recommendations

Each recommendation names the finding it answers. The plan orders them.

### R1. Add GitHub Actions CI (H1)

One workflow, `.github/workflows/R-CMD-check.yaml`, on `ubuntu-latest` with the R release version. Ubuntu is enough because Posit Connect production is Linux.

- `r-lib/actions/setup-r` with `use-public-rspm: true` for binary installs. The dependency set is large (fixest, xgboost, ranger, duckdb, arrow, mice …), and source builds would dominate run time.
- `r-lib/actions/setup-r-dependencies` with `extra-packages: any::rcmdcheck` and `needs: check`. This caches the library between runs.
- Run tests against the source tree, not the installed check copy, so source-contract tests (H3) run for real:
  1. `rcmdcheck::rcmdcheck(args = c("--no-manual", "--no-tests"), error_on = "warning")` for package structure, `NAMESPACE`, and undeclared imports.
  2. `testthat::test_local(reporter = c("check", "junit"))` with `WISEAPP_TEST_PROCESS=true`, which runs tiers 1 and 2.
- Upload the JUnit XML and `testthat-problems.rds` as artifacts on failure.
- Cache the DuckDB extension directory between runs (R4).
- Add `^\.github$` to `.Rbuildignore`.
- Optional: a second job with `covr::codecov()` or `covr::report()` uploaded as an artifact. Make it non-blocking.

Expected CI time: about 8 to 12 minutes on a cold cache (dependency install dominates), then about 4 to 6 minutes warm.

### R2. Declare every dependency (H2)

- `Suggests`: `arrow`, `bit64`, `chromote`, `shinytest2`, plus any of `data.table` / `duckdbfs` that are still used after consolidation. Drop them if they turn out unnecessary (for example, write fixture parquet with DuckDB `COPY ... TO` instead of arrow).
- `Imports`: `later`, `tidyselect`. Check whether `R/` uses any other package via `::` without declaring it.
- Remove `skip_if_not_installed()` for packages in `Imports`. 151 of the 208 calls guard hard dependencies, so they add noise and can never trigger. Keep the skip only for `Suggests` packages.

### R3. Make source-contract tests robust (H3)

Add one helper, `skip_if_no_source()`, returning the package root or skipping with a clear message, and use it in every test that reads `R/` or `inst/` files from the source tree. Then:
- Under `test_local()` (local and CI): the tests run.
- Under `R CMD check`: they skip with a message instead of passing vacuously or failing.

Add a guard `expect_gt(length(files), 0)` so an empty file scan fails loudly.

### R4. Remove the network dependency from fixtures (H4)

- Prefer the H3 extension that is already cached or bundled. `inst/duckdb_extensions/` ships binaries, but they are platform-specific, so they don't help on a Linux runner.
- Fixture helper `local_h3_con()`: try `LOAD h3`, then one `INSTALL h3 FROM community`. If both fail, `skip("DuckDB h3 extension unavailable (offline?)")`. That is better than an error, and CI with a cached extension dir will still run the tests.
- Consider fixture H3 cells as hard-coded strings (computed once, written into the helper), so building a fixture needs no H3 SQL. Only tests of H3 behaviour then need the extension.
- In CI, cache the DuckDB extension directory keyed on the DuckDB version.

### R5. Pin the test environment in `tests/testthat/setup.R` (H5)

Use `withr::local_envvar(..., .local_envir = testthat::teardown_env())` to:
- Point `WISEAPP_WEATHER_CACHE_DIR` and `WISEAPP_PREPARED_WEATHER_CACHE_DIR` at a per-run temp dir.
- Unset `WISEAPP_DATA_PATH`, `WISEAPP_DATA_SOURCE`, and cloud credential variables, so a developer `.Renviron` can't leak in.
- Set `WISEAPP_ASYNC_SYNC=1` by default. Process-tier tests opt back out locally.
- Set `WISEAPP_METADATA_LOAD_PARALLEL=0` by default for the same reason.

Tests that need a different value keep using `withr::local_envvar()` locally.

### R6. Introduce shared helpers (M1, M6)

Files are loaded automatically by testthat and `load_all()`:

| File | Contents |
|---|---|
| `helper-skip.R` | `skip_if_no_source()`, `skip_if_not_process_tier()` (reads `WISEAPP_TEST_PROCESS`), `local_h3_con()` |
| `helper-fixtures-survey.R` | Survey list, variable list, survey and weather data frames |
| `helper-fixtures-weather.R` | `sw_continuous()`, `sw_binned()`, parquet + H3 directory builder |
| `helper-fixtures-pipeline.R` | One small fitted model + Step 2 pipeline + Step 3 policy fixture, built lazily and memoised per run |
| `helper-shiny.R` | Only if a repeated `testServer` argument bundle emerges; not pre-emptively |

Rules: a fixture moves to a helper only when two or more files use it, and helpers are plain functions with no side effects at load time. Memoised fixtures must return values the tests do not mutate. Copy before mutating, or rebuild cheaply.

### R7. Three test tiers (M6, M8)

| Tier | What | Local default | CI | Target time |
|---|---|---|---|---|
| 1. Unit + module | Pure `fct_*`/`utils_*` tests, `testServer()` module tests, source contracts | Always | Every push/PR | ≤ 90 s serial |
| 2. Process | Real mirai daemon, headless Chrome (webshot2/chromote), parallel metadata loads | Opt-in: `WISEAPP_TEST_PROCESS=true` | Every push/PR | ≤ 40 s |
| 3. End-to-end | `shinytest2` AppDriver against a synthetic local data directory | Opt-in: `WISEAPP_TEST_E2E=true` | Separate job | ≤ 3 min |

Each tier is a named skip helper, not `skip_on_cran()` / `skip_on_ci()`, because the package is not on CRAN and the goal is to run more on CI, not less.

Also try `Config/testthat/parallel: true` with `Config/testthat/start-first` listing the slowest files. testthat parallelism runs whole files in separate processes, so the process-wide `.duck` connection and caches stay per-worker. Adopt it only if 10 consecutive runs pass, and record the decision.

### R8. Reorganise to `test-<R file>.R` (M2, M4)

- One test file per `R/` file it mainly covers, named exactly after it: `R/fct_get_weather.R` → `tests/testthat/test-fct_get_weather.R`.
- Use descriptive names for cross-cutting concerns: `test-determinism.R`, `test-source-contracts.R`, `test-e2e-*.R`.
- Split files over ~600 lines by topic **inside** the same mapping, for example `test-fct_get_weather-binning.R` and `test-fct_get_weather-future.R`.
- Move history IDs (`PERF-30`, `INT-01` …) from test names into a trailing comment where they explain *why*. Test names describe behaviour.

### R9. Remove or consolidate redundant tests (M3, M4, M5)

See section 5 for the candidate list. The method for each candidate is in the plan (Phase 5). In short: a test is removed only if its assertions are duplicated elsewhere, or it only exercises dependency behaviour, **and** coverage does not drop for any `R/` line.

### R10. Hygiene sweep (L1 to L3, M7)

- Remove redundant `library()` calls and `wiseapp:::` / `testthat::` prefixes.
- Replace `set.seed()` with `withr::local_seed()`.
- Convert `expect_true(identical/==/%in%)` to the specific expectation **when a file is touched anyway**. Don't make a stand-alone churn PR.
- Console noise: add one package option, `wiseapp.verbose` (default `TRUE`), checked by a small internal `wise_inform()` used in place of bare `message()` for progress logs, and set it to `FALSE` in `setup.R`. Tests that assert on a log message use `expect_message()` with the option set back to `TRUE` locally. This is an `R/` change; keep it in its own PR.

### R11. Add a thin `shinytest2` layer (M8)

Recommended: **yes, but only 2 or 3 tests.** For an app this large, broad shinytest2 coverage is slow, brittle, and duplicates what `testServer()` already covers well. Spend the E2E budget where only a browser can help:

1. **Boot smoke:** the app starts against a synthetic local data dir, the overview metadata loads, the Step 1 sample UI renders, no JS console errors (`app$get_logs()`), and the hexmap container initialises.
2. **Golden path by config import:** upload a checked-in config JSON (the app's own export format), let the pipeline runner replay Steps 1 to 3, wait on step status, then assert a few stable values. This covers `update*Input()` restoration, `app_server` wiring, the async Step 2 path, and every step in one test.
3. Optional **visibility check:** toggle the one or two `conditionalPanel` flyouts that broke in the shiny 1.14 upgrade and assert visibility through `app$get_js()`.

Design rules:
- Use `app$wait_for_value()` / `app$expect_values()` on a short, explicit list of inputs, outputs, or exports. Avoid `expect_screenshot()`; it is OS- and font-dependent.
- Add `shiny::exportTestValues()` in `app_server` for pipeline step statuses and generations. That gives E2E tests stable hooks without scraping the DOM, and has no runtime cost outside test mode.
- Generate the synthetic data directory at test time from the same helper used by tier 1 (R6), not from committed binaries.
- Use small data: one economy, one survey year, about 200 households, 2 weather variables, 1 CMIP6 model, 1 SSP, and coefficient draws off. The golden path should finish in under 60 s.

### R12. Architecture recommendations that improve testability

These are **not** refactors to do for their own sake. Apply them when a module is being changed anyway.

- **Keep extracting pure logic out of the largest module servers.** `mod_3_09_decomposition.R` (~1850 lines, ~1240 in the server), `mod_2_02_results.R` (~2060), and `mod_1_05_weatherstats.R` (~1600) hold a lot of computation inside reactive closures. Anything that can be a plain function of its inputs belongs in `fct_*` (or a `utils_*` file) and gets a fast unit test. The module test then only checks wiring.
- **Keep module return APIs explicit.** The `step1_api`, `step2_api`, and `step3_api` lists are good test seams. Assert them with `session$getReturned()` rather than reaching into internals.
- **Add one `testServer(app_server, ...)` wiring test** in tier 1. It instantiates the whole reactive graph against the synthetic local data dir (auto-connect via `WISEAPP_DATA_SOURCE=local`) and checks that Step 0 publishes `survey_list` and Step 1 receives it. That is cheap, browser-free integration coverage of `app_server.R`, which has none today.
- **Test the pure pipeline runner without the browser.** `test-pipeline-runner.R` already does this; keep it as the main test of import sequencing, with E2E as the check that the browser side really applies the inputs.

---

## 5. Consolidation and removal candidates

This is the list at review time. The plan re-checks each one before acting.

| Candidate | Action | Reason |
|---|---|---|
| `test-rerun-regressions.R`: the 3 toy-server tests (M3) | **Remove**; keep the mechanism explanation as a comment beside the helper in `R/` | They test Shiny, not WISE-APP. The real-module tests in the same file cover the regressions |
| `test-export-wiring-contract.R` + `test-csv-export-wiring-contract.R` | **Merge** into `test-source-contracts.R` with one shared scanner | Same invariant (UI CSV key ⇒ bundle registration) for two button helpers, implemented twice |
| `test-conditional-panel-css-contract.R`, source part of `test-pipeline-runner.R` ("prerequisite controls render while hidden") | **Move** into `test-source-contracts.R` | One home for all static source scans, with the shared skip (R3) |
| `test-table-csv-export.R` | **Move** `make_regtable_df` tests to the `fct_results` table tests; the CSV-button test goes to `utils_ui` tests | It tests two unrelated files |
| `test-step3-wave2-e.R` | **Distribute** to the owning files (`mod_3_08`, `mod_3_09`, `fct_export`, policy-sim compare) | Grab-bag named after a work wave |
| `test-ui-migration-step3.R` | **Rename/move** into the chart-builder tests of the files it covers | The migration is done; the tests now just test chart builders |
| `test-uncertainty-decomposition.R` | **Move** into the `fct_sim_compare` tests | Misnamed; it tests plotting/table helpers |
| `test-perf02-weather-transformations.R` | **Fold** into the `fct_get_weather` / weather pipeline tests | Named after a perf ticket; tests a weather-transform contract |
| `test-w3-a-aggregation-characterization.R`, `test-w3-b-future-decomposition-characterization.R` | **Audit by unique coverage**: keep assertions that pin behaviour nothing else pins, move them to the owning file, delete the rest | Characterization tests are scaffolding for a refactor; once it lands they should become ordinary tests or go |
| `test-step2-contract.R`, `test-step2-compute.R`, `test-step2-payload.R` | **Share one fixture** (R6) and keep them as three files | They test different layers; the duplication is in the fixtures, not the assertions |
| `test-bench-step3.R` | **Move out of `tests/testthat/`** to a `dev/` self-check script, or delete | Tests developer bench tooling; cannot run under `R CMD check` |
| `test-fct-aggregation-kernel.R` vs `test-fct_aggregation_delta.R` vs `test-policy-sim-compare-agg-cache.R` | **Keep separate**, rename to the `R/` files they cover | Kernel oracle, delta-method numerics, and cache behaviour are distinct; overlap is fixture-only |
| `test-visualization-contracts.R` (468 lines, 72 `:::`) | **Split** by owning file (`fct_metric_registry`, `fct_sim_compare`, `fct_policy_sim_compare` …) | Cross-cutting name hides what is covered |

Expected effect: about 80 files become about 60 to 65 files mapped to `R/`. The test count drops slightly (tens, not hundreds), with no loss of line coverage. Most of the run-time saving comes from tiering (about 25 s moves to tier 2) and shared memoised fixtures, not from deletions.

---

## 6. Out of scope (deliberately)

These are left out on purpose. Each is a reasonable thing to do; none is needed to reach a reliable, fast CI suite.

| Area | Why out of scope | What covers it instead |
|---|---|---|
| Screenshot / visual regression (`expect_screenshot`, `vdiffr`) | High maintenance; OS and font dependent; echarts output is better tested structurally | Structural widget-payload assertions (existing) |
| Real cloud backends (S3, GCS, Azure, Databricks, HF) in CI | Needs secrets in CI and real buckets; slow and flaky | `httr2_mock` and connection-param unit tests (existing); manual pre-deploy smoke test on Posit Connect |
| JavaScript unit tests for `hexmap.js` (Node/Jest/Vitest) | Adds a second toolchain for one file | Payload-contract tests (existing) + E2E boot smoke (R11) |
| Load and concurrency testing (`shinyloadtest`) | A performance activity, not correctness | `review/optimization_guidelines.md` and `dev/bench_*` |
| Benchmarks in CI | Shared-runner timings are noisy | Local `dev/bench_*` per optimization guidelines |
| macOS / Windows CI matrix | Production is Linux; macOS is the dev machine | Local runs; add a weekly macOS job later if platform bugs appear |
| CRAN compliance | Not distributed via CRAN | `rcmdcheck` warnings-as-errors covers the useful subset |
| Statistical validation against external tools (Stata, other packages) | Research-validation activity; needs reference datasets | Oracle and determinism tests (existing) |
| Bulk module refactoring for testability | Large, risky, not needed for CI | Opportunistic extraction rule (R12) |
| Spelling enforcement | Low value for an app; currently non-blocking | Keep `tests/spelling.R` non-blocking, or delete it |
| Mutation testing | Tooling for R is immature | Coverage + review |

---

## 7. Coverage at review time

**Method.** I ran `covr::package_coverage(type = "none")` with `test_dir()` on the source `tests/` directory and `stop_on_failure = FALSE`. I excluded `test-step2-async.R`, `test-fct-overview-metadata.R`, and `test-bench-step3.R`, and dropped one truncated worker trace. `type = "tests"` aborts in the installed layout (H3), and killed mirai daemons leave unreadable trace files. The run took 3.6 minutes. Code executed inside mirai workers is not counted, so `fct_step2_async.R` and `fct_overview_metadata.R` are understated.

**Overall line coverage: 74.5%.**

Lowest-covered files, by size of the gap:

| File | Lines | Covered | Notes |
|---|---|---|---|
| `fct_results.R` | 3683 | 58% | Largest file; ~1500 uncovered lines, mostly result formatting branches by engine |
| `fct_step1_headline.R` | 978 | 43% | Step 1 headline numbers shown to users |
| `mod_2_01_weathersim.R` | 1086 | 59% | Step 2 scenario definition UI and server |
| `fct_sim_diag.R` | 1383 | 64% | Step 2 diagnostics |
| `fct_step2_async.R` | 681 | 5%* | *Real-worker tests excluded from this run |
| `mod_1_08_modelfit.R` | 420 | **0%** | No tests at all |
| `mod_3_02_infra.R`, `mod_3_03_digital.R`, `mod_3_04_labor.R`, `mod_3_05_education.R` | 599 total | **4 to 6%** | Step 3 policy-lever modules: only UI constructors run, servers untested |
| `mod_3_scenario.R`, `mod_2_simulation.R`, `mod_1_modelling.R` | 566 total | 20 to 37% | Parent modules; wiring only |
| `app_server.R`, `run_app.R` | 167 | **0%** | No app-level test |
| `fct_predict_outcomes.R` | 164 | 38% | Core prediction path; low for its importance |
| `fct_results_table2.R`, `fct_incidence.R` | 741 total | 54% | Results tables, incidence |

Best covered (above 92%): `fct_policy_sim_compare.R`, `mod_2_02_results.R`, `fct_policy_metric_decompose.R`, `fct_surveystats.R`, `fct_aggregation_delta.R`, `fct_metric_registry.R`, `fct_hexmap.R`, `utils_*`, and `mod_1_04_weather.R`.

What this means for the plan:

- The slowest test files (`test-policy-sim-compare-agg-cache.R`, `test-mod_2_02_results.R`, `test-step2-payload.R`) cover code that is already above 90%. They are the first place to look for redundant assertions, using the unique-coverage method (plan, Phase 6), and for cheaper shared fixtures.
- The biggest correctness gaps are not in the heavily tested numerical core. They are in the **Step 3 policy-lever modules, `mod_1_08_modelfit`, `fct_predict_outcomes`, `fct_step1_headline`, and app wiring**. A user-visible scenario or headline number can break there with no test failing.

### R13. Close priority coverage gaps

In priority order, with targets that are guides, not gates:

1. `fct_predict_outcomes.R`: unit tests per engine and per outcome transform (log back-transform, logistic), target ≥ 80%.
2. `mod_3_02` to `mod_3_05` policy-lever servers: one `testServer()` per module asserting the returned scenario object for a default and a non-default lever setting, target ≥ 60% each.
3. `mod_1_08_modelfit.R`: `testServer()` for the fit-diagnostics outputs with a fixture model, target ≥ 50%.
4. `fct_step1_headline.R`: unit tests of the headline values against hand-computed fixtures, target ≥ 70%.
5. `app_server.R`: covered by the Phase 7 wiring test and the Phase 8 E2E tests.
6. `fct_results.R`: don't aim for a percentage. Add tests only for branches that produce user-visible numbers (coefficient tables, translations), not presentation-only branches.

Don't set a global coverage gate in CI yet. Once these gaps are closed, consider a ratchet (fail if overall coverage drops by more than 1 point).
