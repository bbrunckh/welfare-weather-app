# Test Suite and Testability Review

Review of the WISE-APP test suite and the parts of the app architecture that affect testing. The implementation plan is in `review/test_suite_plan.md`.

- **Original review:** commit `ae06d59` on `dev` (2026-10-05).
- **Updated:** 2026-10-07, at commit `134170e` plus uncommitted CI-safety changes (section 2).
- **Tooling:** R 4.5.3, testthat 3.3.2, shiny 1.14.0, macOS arm64 locally; `ubuntu-latest` on CI.

Numbers are snapshots. The plan says which ones to re-derive before acting.

---

## 1. Summary

The suite is in good shape for a Shiny app this size: testthat 3rd edition, `testServer()` coverage of the step modules, `local_mocked_bindings()`, `withr` for state, mocked HTTP for every cloud backend, numerical oracle and determinism tests, and structural assertions on widget payloads instead of screenshots. A full source-tree run now has **1067 test blocks in 102 files, 0 failures, 0 skips, about 220 s** (cold, serial, including the real-worker and headless-Chrome tests).

The original review found that the suite would not behave the same on CI as locally. Most of that is now addressed (section 2). What remains is mostly maintainability and coverage, not reliability:

1. **CI has never run green.** A workflow existed but failed at dependency resolution (see 2.1). The fix is committed; the first run after a push is unverified.
2. **Fixtures are duplicated across files and test files are named after project history**, not after the `R/` file they cover. This costs maintainability, not correctness.
3. **No browser layer.** MapLibre/hexmap JS, `conditionalPanel` visibility, `update*Input()` restoration on config import, and the real Step 0 to 3 flow are not exercised.
4. **Coverage is uneven.** Step 3 policy-lever modules, `mod_1_08_modelfit`, `fct_predict_outcomes`, `fct_step1_headline`, and `app_server` had little or none at review time (section 6; numbers not re-measured).

---

## 2. Status of the original findings

### 2.1 What changed since the review

- The original review said `.github/` was empty. A workflow, `.github/workflows/R-CMD-check.yaml`, had already been added (commit `6ee04f3`). Its only run (#37439742696) failed before any test ran: `DESCRIPTION` pinned `duckdb (== 1.5.5)`, which CRAN and Posit Package Manager no longer served, so `setup-r-dependencies` could not solve the install.
- **Fixed (committed):** `c733a10` and `d13a585` upgrade the bundled DuckDB to 1.5.6 (binaries, version constant, checksums, `DESCRIPTION` pin, `AGENTS.md`, `manifest.json`). `h3` and `httpfs` 1.5.6 binaries exist for linux_amd64. **DuckDB 2.0 extensions were not published at the time of writing**, so 1.5.6 is the newest usable version.
- **Done in the working tree, not committed at the time of writing:** the CI-safety changes listed below.
- The three work-in-progress test failures recorded in the original review no longer occur.
- Test files grew from 80 to 102 since the original review.

### 2.2 Findings table

| Finding | Status |
|---|---|
| **H1** No CI | Workflow exists. Install blocker fixed (2.1). **Unverified until the first push.** The test step now runs the whole suite once from the checkout (`testthat::test_local()`); `R CMD check` runs with `--no-tests`. A cache for `~/.duckdb/extensions` was added. |
| **H2** Undeclared packages | `later` and `tidyselect` were already declared (remediation of 2026-10-06). `arrow`, `bit64`, `chromote`, `data.table` are in `Suggests`. **`duckdbfs` is not used anywhere** (`R/` says so); the review's advice to declare it was wrong, and its 22 `skip_if_not_installed()` calls were deleted instead. 215 redundant skips for hard `Imports` were removed. `shinytest2` is only needed if an E2E test is added. |
| **H3** Tests that only work from the source tree | Mostly already guarded with `skip_if(...)`. Resolved for CI by running tests from the source tree instead of the installed `R CMD check` copy. A shared `skip_if_no_source()` helper was **not** added (see section 4). |
| **H4** Fixture downloads the `h3` extension | `make_h3_con()` now tries `LOAD`, then one `INSTALL ... FROM community`, then skips with a message. CI caches the extension directory. |
| **H5** Global environment not pinned | `tests/testthat/setup-env.R` points the weather caches at a per-run temp directory and unsets `WISEAPP_DATA_*` and cloud credentials. |
| **M1** No shared fixtures | **Open.** Same-name builders and about 10 near-identical fixture families still exist. |
| **M2** Files named after history, not `R/` | **Open, deferred.** Files such as `w3-a`, `wave2-e`, `perf02`, `ui-migration-step3`, `rerun-regressions`, `visualization-contracts` remain. |
| **M3** Tests of Shiny itself (3 toy `observeEvent` tests in `test-rerun-regressions.R`) | **Open, small.** |
| **M4** Overlapping files (two export-key scanners; `make_regtable_df` in the CSV file) | **Open, small.** |
| **M5** Bench smoke test sources `dev/` | It already skips when `dev/` is absent and now runs in CI from the source tree (about 1 s). Left in place. |
| **M6** Slow process tests in the default run | **Decided not to tier** (section 4). About 30 s of the suite. |
| **M7** Console noise | **Open, deferred.** Needs an `R/` change (a verbosity option). |
| **M8** No browser tests | **Open, deferred.** |
| **L1** Redundant `library()` and `wiseapp:::` | Open, low value. |
| **L2** `set.seed()` leaks | Open, low value. |
| **L3** `expect_true(identical(...))` | Open. Fix only in files touched anyway. |
| **L5** `tests/spelling.R` never fails | Open. Keep non-blocking or delete. |

---

## 3. Findings still open

**Maintainability (Medium).**
- *Duplicated fixtures (M1).* Fixing one copy does not fix the others. A fixture should move to a `helper-*.R` file only when two or more files use it.
- *History-named, grab-bag files (M2, M4).* `usethis::use_test()` and editor go-to-test do not work, and finding the tests for a function needs a grep. Candidates (re-verify before acting):

| Candidate | Action |
|---|---|
| `test-rerun-regressions.R`: the 3 toy-server tests | Remove; they test `observeEvent()`, not app code. The real-module tests in the same file cover the regressions. |
| `test-export-wiring-contract.R` + `test-csv-export-wiring-contract.R` | Merge into one source-contract file with one scanner. |
| `test-table-csv-export.R` | Move `make_regtable_df` tests to the `fct_results` tests. |
| `test-step3-wave2-e.R`, `test-ui-migration-step3.R`, `test-uncertainty-decomposition.R`, `test-perf02-weather-transformations.R`, `test-visualization-contracts.R` | Redistribute to the files they test. |
| `test-w3-a-...`, `test-w3-b-...` characterization files | Keep assertions that nothing else pins; delete the rest. |
| Step 2 contract / compute / payload; aggregation kernel / delta / cache | Keep separate (different layers). Only the fixtures overlap. |

**Browser-only behaviour (M8).** `testServer()` cannot reach these:

| Area | Why |
|---|---|
| `inst/app/vendor/hexmap.js` (MapLibre + h3-js) | JS only; R tests check the payload contract, not rendering. |
| `conditionalPanel` and flyout visibility | CSS and JS; guarded only by a source scan (`test-conditional-panel-css-contract.R`). The shiny 1.13 to 1.14 regression was this kind. |
| Config import (`wise_config_apply()`) | Uses `update*Input()`, which `testServer()` does not apply. |
| The real Step 0 to 3 flow through `app_server` | Module tests stub the upstream reactives. |

**Console noise (M7).** App `message()` output (`[wiseapp] ...`, `K-means cutoffs ...`, `[overview] ...`) and worker start-up banners fill the test log and hide real warnings. There is no verbosity switch.

**Architecture notes for testability** (apply only when a module is being changed anyway):
- Keep extracting pure logic from the largest module servers (`mod_3_09_decomposition.R`, `mod_2_02_results.R`, `mod_1_05_weatherstats.R`) into `fct_*` files with fast unit tests.
- Keep the `step1_api` / `step2_api` / `step3_api` return lists explicit; assert them with `session$getReturned()`.
- A `testServer(app_server, ...)` wiring test against a synthetic local data directory would cover `app_server.R` (0% at review time) without a browser.

---

## 4. Decisions made (deliberately not doing)

| Idea from the original review | Decision | Reason |
|---|---|---|
| Three tiers with `WISEAPP_TEST_PROCESS` / `WISEAPP_TEST_E2E` skips | **Not doing** | Only about 30 s of tests start a real worker or browser, and CI would run them anyway. A tier would only hide tests locally. |
| `skip_if_no_source()` helper in every source-reading test | **Not doing** | Most tests already skip. CI now runs tests from the source tree, so the installed-layout problem does not arise. |
| Move `test-bench-step3.R` out of `tests/testthat/` | **Not doing** | Already guarded and cheap. |
| `testthat` parallel files | **Not doing** | At about 3 to 4 minutes serial it is not needed, and the process-wide DuckDB connection and mirai daemons make it risky. |
| Baseline tooling scripts (`dev/test_timings.R`, `dev/test_coverage.R`, `dev/test_installed_layout.R`) | **Not doing up front** | One-off commands answer the same questions; add a script only if it is run repeatedly. |
| Declare `duckdbfs` in `Suggests` | **Wrong; removed instead** | Unused. |

---

## 5. Out of scope (deliberately)

| Area | Why | What covers it instead |
|---|---|---|
| Screenshot / visual regression | Maintenance cost; OS and font dependent | Structural widget-payload assertions |
| Real cloud backends in CI | Needs secrets and real buckets; flaky | `httr2_mock` and connection-param tests; manual pre-deploy smoke test |
| JavaScript unit tests for `hexmap.js` | Second toolchain for one file | Payload-contract tests; optional E2E boot smoke |
| Load testing (`shinyloadtest`), benchmarks in CI | Performance work; shared runners are noisy | `review/optimization_guidelines.md`, `dev/bench_*` |
| macOS / Windows CI matrix | Production is Linux | Local runs |
| CRAN compliance, mutation testing, external statistical validation | Not needed for this app | `rcmdcheck`, coverage, oracle and determinism tests |

---

## 6. Coverage (snapshot from the original review; not re-measured)

**Method then:** `covr::package_coverage(type = "none")` with `test_dir()` on the source `tests/` directory and `stop_on_failure = FALSE`, excluding the real-worker test files. `type = "tests"` aborts on a failing test file, and killed mirai daemons leave unreadable trace files. Code executed inside mirai workers is not counted.

**Overall line coverage: 74.5%** (80 test files at that time).

| File | Coverage then | Notes |
|---|---|---|
| `fct_predict_outcomes.R` | 38% | Core prediction path. Highest priority. |
| `mod_3_02_infra.R`, `mod_3_03_digital.R`, `mod_3_04_labor.R`, `mod_3_05_education.R` | 4 to 6% | Only UI constructors run; servers untested. |
| `mod_1_08_modelfit.R` | 0% | No tests. |
| `fct_step1_headline.R` | 43% | Headline numbers shown to users. Tests were added since (`test-fct_step1_headline.R`, `test-step1-headline-cards.R`); re-measure. |
| `app_server.R`, `run_app.R` | 0% | No app-level test. |
| `fct_results.R` | 58% | Largest file. Only branches that produce user-visible numbers matter. |
| `fct_sim_diag.R`, `mod_2_01_weathersim.R` | 59 to 64% | |

Best covered (above 92%): `fct_policy_sim_compare.R`, `mod_2_02_results.R`, `fct_policy_metric_decompose.R`, `fct_surveystats.R`, `fct_aggregation_delta.R`, `fct_metric_registry.R`, `fct_hexmap.R`, `utils_*`.

The biggest correctness gaps are not in the heavily tested numerical core. They are in the Step 3 lever modules, `mod_1_08_modelfit`, `fct_predict_outcomes`, and app wiring: a user-visible scenario or number can break there with no test failing. Do not set a global coverage gate in CI until these are closed.
