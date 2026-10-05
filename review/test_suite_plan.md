# Test Suite Modernisation Plan

Implementation plan for the recommendations in `review/test_suite_review.md` (referenced as R1 to R13 and findings H/M/L).

**This plan does not assume the repository is still in the state it was reviewed in.** Other commits may land first. Every phase therefore:

1. starts with a **re-baseline** step that re-derives the file lists and numbers it acts on;
2. describes its targets by **rule** (for example, "files that read `../../R`"), with the review-time list only as a starting hint;
3. ends with **acceptance criteria** that can be checked mechanically.

If a hinted file no longer exists, or a finding is already fixed, skip that item and note it in the PR description.

## Ground rules

- **One phase per PR** (or several PRs for a large phase). Don't mix mechanical moves with behaviour changes in one commit.
- **The suite must stay green at every step.** If a test was failing before a phase, record it in the PR and don't fix it inside a refactor PR.
- **No loss of line coverage.** Compare `covr` per-file coverage before and after any phase that removes or moves tests (Phase 0 tooling).
- **No change to `R/` behaviour** except where a phase says so (Phases 1, 7, 8, 9); those changes are small and isolated.
- **Timing budget** (from R7): tier 1 ≤ 90 s serial locally; tier 2 ≤ 40 s; tier 3 ≤ 3 min; CI warm run ≤ 6 min for tiers 1 and 2.
- Mechanical renames (Phase 5) should land when no other open branch has heavy test edits, to avoid painful rebases.

## Phase overview

| Phase | Content | Recs | Touches `R/`? | Depends on |
|---|---|---|---|---|
| 0 | Baseline tooling (timings, coverage, installed-layout run) | — | No | — |
| 1 | Declare dependencies, pin test env, skip helpers | R2, R3, R4, R5 | `DESCRIPTION` only | 0 |
| 2 | GitHub Actions CI (tiers 1 and 2) | R1 | No | 1 |
| 3 | Shared fixtures and helpers | R6 | No | 1 |
| 4 | Test tiers and optional parallelism | R7 | No | 1, 3 |
| 5 | Mechanical renames to `test-<R file>.R` | R8 | No | 2 |
| 6 | Consolidation and removal | R9 | No | 0, 3, 5 |
| 7 | App-level wiring test + `exportTestValues()` | R12 | Yes (small) | 3 |
| 8 | `shinytest2` E2E tier + CI job | R11 | Yes (via 7) | 2, 3, 7 |
| 9 | Hygiene sweep and log verbosity option | R10 | Yes (small) | 5 |
| 10 | Documentation | — | No | all |
| 11 | Close priority coverage gaps | R13 | No | 3 (best after 5) |

Phases 3 to 4 and 7 to 8 can run in parallel with 5 to 6 if coordinated. Phase 11 is independent once shared fixtures exist and can be spread over time; one PR per target file. The order above is the low-conflict default.

---

## Phase 0 - Baseline tooling

**Goal:** repeatable measurement, so every later phase can prove "no slower, no less coverage".

Steps:

1. Add `dev/test_timings.R`, which runs each `tests/testthat/test-*.R` with `testthat::test_file(reporter = "silent", load_package = "none")` after one `devtools::load_all()`. It writes per-file and per-test seconds, failures, and skips to `dev/outputs/test_timings.csv`, and prints the total. Review-time result: 148 s; slowest files listed in the review, section 2.1.
2. Add `dev/test_coverage.R`, which runs `covr::package_coverage(type = "none", code = <test_dir on the source tests, stop_on_failure = FALSE>)`. Running the source `tests/` directory keeps source-contract tests working. It saves `dev/outputs/coverage.rds` and a per-file CSV. `type = "tests"` aborts on the first failing test file, which happened at review time, so don't use it for baselines. Exclude tests that start real mirai daemons (the process tier once Phase 4 exists; use a `filter` regex until then). Killed daemons leave truncated covr trace files, and `covr` then fails with `readRDS(f): error reading from connection`. This happened at review time.
3. Add `dev/test_installed_layout.R`, which installs the package into a temp library, copies `tests/` outside the source tree, and runs `test_dir(load_package = "installed")`. This reproduces the `R CMD check` layout and lists tests that fail or pass vacuously there (H3). Export `R_LIBS` (temp library first) before running, so that mirai workers, which call `library(wiseapp)` outside dev sessions, find the same install. Without that, worker tests fail for reasons that don't apply under real `R CMD check`.
4. Ensure `dev/outputs/` is gitignored, or write to `tempdir()` and print the path.

Re-baseline: run all three and save the outputs as the "before" for Phases 1 to 6.

Acceptance: all three scripts run to completion on a clean checkout and write their outputs.

---

## Phase 1 - Dependencies, environment, skip helpers

**Goal:** the suite behaves the same on a clean Linux runner as on a dev machine, and nothing skips silently.

### 1a. Dependencies (R2)

Re-baseline:

```r
# Packages used in tests via :: or skip_if_not_installed() but not declared
d   <- read.dcf("DESCRIPTION")
dep <- function(f) trimws(sub("\\(.*", "", strsplit(gsub("\n", " ", d[1, f]), ",")[[1]]))
declared <- c(dep("Imports"), dep("Suggests"), "base", "stats", "utils", "methods",
              "grDevices", "graphics", "tools", "wiseapp")
scan <- function(dir, re) unique(unlist(regmatches(
  txt <- unlist(lapply(list.files(dir, "\\.R$", full.names = TRUE), readLines)),
  gregexpr(re, txt, perl = TRUE))))
setdiff(scan("tests/testthat", "[A-Za-z][A-Za-z0-9.]+(?=:::?)"), declared)
setdiff(scan("R", "[A-Za-z][A-Za-z0-9.]+(?=::)"), declared)   # check hits by hand: false positives occur
```

Steps:

1. Add the genuinely used test-only packages to `Suggests`. Review-time hint: `arrow`, `bit64`, `chromote`, `shinytest2`, and possibly `data.table` / `duckdbfs`. Before adding a package used in only one place, check whether the test can avoid it. For example, write fixture parquet via DuckDB `COPY ... TO` instead of `arrow::write_parquet`.
2. Add the packages `R/` really uses via `::` to `Imports`. Review-time hint: `later`, `tidyselect`.
3. Remove `skip_if_not_installed()` calls for packages in `Imports`. Keep them for `Suggests` packages.

### 1b. Test environment (R5)

Create `tests/testthat/setup.R`:

```r
# Pin process-wide settings for the whole run; restored by testthat at teardown.
.wise_test_root <- withr::local_tempdir(.local_envir = testthat::teardown_env())
withr::local_envvar(
  WISEAPP_WEATHER_CACHE_DIR          = file.path(.wise_test_root, "cache"),
  WISEAPP_PREPARED_WEATHER_CACHE_DIR = file.path(.wise_test_root, "prepared"),
  WISEAPP_DATA_PATH   = NA, WISEAPP_DATA_SOURCE = NA,
  WISEAPP_ASYNC_SYNC  = "1",
  WISEAPP_METADATA_LOAD_PARALLEL = "0",
  .local_envir = testthat::teardown_env()
)
```

Re-derive the variable list at implementation time:

```sh
grep -rhoE 'Sys.getenv\("WISEAPP_[A-Z_]+"' R | sort -u
```

- Set every cache-directory variable to the temp root.
- Unset every data-source and credential variable listed in `AGENTS.md`.
- Check that each default above matches what the existing tests expect. If a test relied on the old default, give it an explicit `withr::local_envvar()` instead of changing the default.

### 1c. Skip and connection helpers (R3, R4)

Create `tests/testthat/helper-skip.R` with:

- `skip_if_no_source()`: returns the package source root (from `testthat::test_path("..", "..")` if it contains `DESCRIPTION` and `R/`), otherwise calls `skip("package source tree not available (installed-package test run)")`.
- `local_h3_con(.env = parent.frame())`: opens DuckDB with `bigint = "integer64"`, tries `LOAD h3`, then one `INSTALL h3 FROM community; LOAD h3`, otherwise skips with "DuckDB h3 extension unavailable". It registers `dbDisconnect(shutdown = TRUE)` via `withr::defer(envir = .env)`.

Then:

1. Find every test that reads source files:

   ```sh
   grep -lE 'test_path\("\.\.", "\.\."|"\.\./\.\./R"|Sys.glob\(file.path\("\.\.", "\.\."' tests/testthat/*.R
   ```

   Route each through `skip_if_no_source()`. Add `expect_gt(length(files), 0)` after every file scan so an empty scan can never pass.
2. Replace every `make_h3_con()`-style helper and any `INSTALL ... FROM community` in tests with `local_h3_con()`.

Acceptance:

- `dev/test_installed_layout.R` shows **no failures** caused by the layout. Source-contract tests skip with the message, and no test passes on an empty scan. Pre-existing unrelated failures are listed in the PR.
- `devtools::test()` locally is unchanged (same pass count ± tests that now correctly skip).
- With `arrow`/`bit64` installed, the weather fixture tests run and do not skip.

---

## Phase 2 - GitHub Actions CI (tiers 1 and 2)

**Goal:** every push and PR to `dev` / `main` runs the suite on Linux.

Steps:

1. Add `.github/workflows/R-CMD-check.yaml`. Sketch (check the current `r-lib/actions` major version at implementation time):

   ```yaml
   name: R-CMD-check
   on:
     push:
       branches: [main, dev]
     pull_request:
   permissions: read-all
   concurrency:
     group: ${{ github.workflow }}-${{ github.ref }}
     cancel-in-progress: true
   jobs:
     check:
       runs-on: ubuntu-latest
       timeout-minutes: 30
       env:
         GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
         NOT_CRAN: "true"
         WISEAPP_TEST_PROCESS: "true"
       steps:
         - uses: actions/checkout@v4
         - uses: r-lib/actions/setup-r@v2
           with: { use-public-rspm: true }
         - uses: r-lib/actions/setup-r-dependencies@v2
           with:
             extra-packages: any::rcmdcheck, any::devtools
             needs: check
         - uses: actions/cache@v4
           with:
             path: ~/.duckdb/extensions
             key: duckdb-ext-${{ runner.os }}-${{ hashFiles('DESCRIPTION') }}
         - name: Package structure check (no tests)
           run: rcmdcheck::rcmdcheck(args = c("--no-manual", "--no-tests"), error_on = "warning", check_dir = "check")
           shell: Rscript {0}
         - name: Tests (tiers 1+2, source tree)
           run: |
             testthat::test_local(reporter = testthat::MultiReporter$new(list(
               testthat::CheckReporter$new(),
               testthat::JunitReporter$new(file = "junit.xml"))))
           shell: Rscript {0}
         - uses: actions/upload-artifact@v4
           if: always()
           with:
             name: test-results
             path: |
               junit.xml
               check/**/*.Rout*
               tests/testthat/_snaps/**
   ```

   Notes:
   - `--no-tests` plus a separate `test_local()` step avoids running the suite twice, and runs the source-contract tests against the source tree (R3).
   - Whether `rcmdcheck` warnings fail the build: start with `error_on = "error"` if the first run shows pre-existing warnings, list them in an issue, and tighten to `"warning"` once they're fixed.
   - The package has `src/` (Rcpp), so the runner compiles it. Ubuntu runners include a toolchain.
   - Headless Chrome for tier 2: `ubuntu-latest` ships Google Chrome. If `chromote::find_chrome()` fails, add `browser-actions/setup-chrome`.
2. Add `^\.github$` to `.Rbuildignore`, and `^docs$` if `docs/` is committed.
3. Optional, non-blocking: a `coverage` job running `dev/test_coverage.R` and uploading the HTML report as an artifact.
4. Add a CI status badge to `README.Rmd` / `README.md` (optional).

Acceptance:

- The workflow is green on a branch cut from current `dev`, or red only on failures already listed as pre-existing.
- Warm-cache run time ≤ 6 min. Record cold and warm times in the PR.
- The JUnit artifact is uploaded on failure.

---

## Phase 3 - Shared fixtures and helpers

**Goal:** one definition per fixture (R6).

Re-baseline: list function definitions in test files and find duplicates and near-duplicates.

```sh
grep -hoE '^\.?[A-Za-z_][A-Za-z0-9_.]* <- function' tests/testthat/test-*.R | sort | uniq -c | sort -rn
```

Also scan for same-shaped builders under different names. Review-time families: `step2_contract_*`, `step2_compute_*`, `phase4_*`, `w3a_*`, `pipeline_fixture`, `svy_fixture`, `make_weather`, `make_vl`, `make_lasso_fixture`, `sw_continuous` / `sw_binned`, `make_test_fixtures` (weather parquet dir), and `make_survey_list`.

Steps:

1. Create helper files, grouped by domain:
   - `helper-fixtures-survey.R`
   - `helper-fixtures-weather.R`: selected-weather rows, plus a parquet/H3 directory builder using `local_h3_con()`.
   - `helper-fixtures-pipeline.R`: fitted model, Step 2 pipeline, Step 3 policy inputs.
   - `helper-fixtures-data-dir.R`: a complete synthetic local data directory in the layout the app expects (`metadata/survey_list.csv`, `metadata/variable_list.csv`, `metadata/cpi_ppp.csv`, `microdata/<unit>/<code>/…parquet`, `microdata/h3/<code>/…_h3.parquet`, historical and CMIP6 weather parquet). Derive the layout from `R/fct_overview_metadata.R`, `R/fct_sample.R`, and `R/fct_get_weather.R` at implementation time. Phases 7 and 8 reuse this.
2. Move a fixture into a helper only if **two or more files** use it, or it is the shared data-dir builder. Single-use builders stay local.
3. For expensive fixtures (anything that fits a model or runs a pipeline), memoise per run in a package-private environment inside the helper. Return values are treated as read-only, and tests that mutate must copy first. Name these `fixture_*()` so the memoisation is visible at the call site.
4. Replace call sites file by file. One commit per family keeps diffs reviewable.

Acceptance:

- No function name is defined in more than one test file (re-run the grep above).
- Coverage per `R/` file is unchanged (Phase 0 tooling).
- Tier-1 time is unchanged or lower. Memoised pipeline fixtures should cut the Step 2 and Step 3 files noticeably.

---

## Phase 4 - Tiers and optional parallelism

**Goal:** fast default local run; CI runs more (R7).

Steps:

1. In `helper-skip.R`, add:
   - `skip_if_not_process_tier()`: skips unless `WISEAPP_TEST_PROCESS` is `"true"`.
   - `skip_if_not_e2e()`: skips unless `WISEAPP_TEST_E2E` is `"true"`, and also skips if `shinytest2` / `chromote` are missing or `chromote::find_chrome()` fails.
2. Re-baseline the tier-2 candidates with `dev/test_timings.R` and grep:

   ```sh
   grep -lE 'mirai::daemons|chromote::|webshot2::|WISEAPP_METADATA_LOAD_PARALLEL *= *"1"|callr::' tests/testthat/*.R
   ```

   Rule: a test is tier 2 if it starts a **real** external process (mirai daemon, Chrome, callr/background R). A test is **not** tier 2 just because it's slow. Slow pure-R tests get faster fixtures instead (Phase 3).
3. Add `skip_if_not_process_tier()` at the top of those tests only, not whole files, unless the whole file is process-based.
4. Check the tests that set `WISEAPP_METADATA_LOAD_PARALLEL = "1"` without testing parallelism specifically. Switch them to `"0"` (the Phase 1 default). At review time, "remote metadata cache avoids repeat requests and can be disabled" took 6.2 s for this reason.
5. Optional experiment: parallel test files.
   - Add `Config/testthat/parallel: true` and `Config/testthat/start-first:` with the slowest 4 to 6 files from the timings CSV.
   - Run `devtools::test()` 10 times. Adopt only if all 10 pass and wall time drops ≥ 30%. Otherwise revert and record why in the PR (likely causes: the process-wide DuckDB connection, mirai daemons, shared temp paths).

Acceptance:

- Default `devtools::test()` (no env vars): ≤ 90 s on the dev machine, and 0 process-tier tests run.
- `WISEAPP_TEST_PROCESS=true`: process-tier tests run and pass.
- CI (Phase 2 env) runs both tiers.

---

## Phase 5 - Mechanical renames

**Goal:** `tests/testthat/test-<R file>.R` mapping (R8). Pure `git mv`, no content changes, so git records renames.

Re-baseline:

```sh
# test files whose name does not match an R/ file stem
for f in tests/testthat/test-*.R; do s=$(basename $f .R | sed 's/^test-//'); \
  [ -f "R/$s.R" ] || echo "$f"; done
```

Rules:

1. A file that mainly covers one `R/` file is renamed `test-<stem>.R`, using the `R/` file's exact stem (underscores). Review-time examples:
   - `test-fct-outcome.R` → `test-fct_outcome.R` (if `R/fct_outcome.R` is the target)
   - `test-mod-0-overview.R` → `test-mod_0_overview.R`
   - `test-fct-overview-metadata.R` → `test-fct_overview_metadata.R`
2. A file over ~600 lines covering one `R/` file can be split into `test-<stem>-<topic>.R`. Do splits in a separate commit from renames.
3. Cross-cutting files keep a descriptive name: `test-determinism.R`, `test-source-contracts.R` (created in Phase 6), `test-e2e-*.R` (Phase 8).
4. Grab-bags that span several `R/` files (for example `test-step3-wave2-e.R`, `test-visualization-contracts.R`) are **not** renamed here. Phase 6 redistributes them.
5. Where two test files would map to the same name, rename one with a `-<topic>` suffix. Merging is Phase 6.

Acceptance:

- `git diff --stat -M` shows only renames (100% similarity) in the rename commit.
- The pass count is identical.
- The re-baseline loop lists only cross-cutting files and Phase 6 grab-bags.

---

## Phase 6 - Consolidation and removal

**Goal:** remove tests that add no value, and put every remaining test in its owning file (R9).

### Method (apply to every candidate)

1. **Unique coverage check.** With `dev/test_coverage.R`, compute coverage for the full suite and for the suite **without** the candidate file or test. If any `R/` line is covered only by the candidate, the candidate (or the part covering that line) must be kept or moved, not deleted.
2. **Assertion check.** For each `test_that()` in the candidate, find where else the same behaviour is asserted. Search for the function under test across `tests/testthat`. Delete only when an equivalent or stronger assertion exists elsewhere, or the test exercises only a dependency (Shiny, base R) and not app code.
3. **Move, don't rewrite.** When redistributing, copy the `test_that()` block unchanged into the owning file, switching it to shared fixtures only if Phase 3 has a drop-in replacement.
4. **One candidate per commit**, with the decision and evidence (unique lines: none / list) in the commit message.

### Candidates (review-time; re-verify each)

| Candidate | Expected action |
|---|---|
| Toy-server tests in the rerun-regressions file that assert `observeEvent()` / `ignoreInit` semantics without app code | Delete. Move the "why" into a short comment beside the relevant helper in `R/`. The comment-only edit to `R/` is allowed |
| The two export-key wiring scanners (CSV button keys ⇒ bundle registrations) | Merge into `test-source-contracts.R` with one parameterised scanner covering every button helper |
| `conditionalPanel` CSS contract; source-scan part of the pipeline-runner tests | Move into `test-source-contracts.R` |
| `make_regtable_df` tests in the table-CSV file | Move to the `fct_results` table test file; the CSV-button test goes to the `utils_ui` tests |
| Wave-named grab-bag (`step3-wave2-e`) | Redistribute to owning files |
| UI-migration Step 3 chart tests | Move to the chart-builder tests of the owning files |
| Misnamed `uncertainty-decomposition` plotting tests | Move to the `fct_sim_compare` tests |
| `perf02` weather-transformation tests | Fold into the weather tests |
| `w3-a` / `w3-b` characterization files | Apply the method per test: keep unique pins (moved to owning files), delete duplicates |
| Visualization-contracts file | Split by owning file |
| Bench smoke test sourcing `dev/` | Move to `dev/` as a self-check script, or delete. It must not stay in `tests/testthat/` |

Do **not** consolidate files that test different layers but share fixtures (Step 2 contract / compute / payload; aggregation kernel / delta / cache). Phase 3 already removed their duplication.

Acceptance:

- Per-file line coverage of `R/` ≥ before (Phase 0 baseline), with any intentional drop justified line by line in the PR.
- No test file whose name refers to a work wave, phase, ticket, or migration.
- Tier-1 time ≤ the previous phase.

---

## Phase 7 - App-level wiring test and test hooks

**Goal:** browser-free integration of `app_server()` (R12) plus stable hooks for E2E.

Steps:

1. **Test hooks (small `R/` change).** In `app_server()`, export pipeline state for tests:

   ```r
   shiny::exportTestValues(
     step1_status = step1_api$fit_status(),   step1_gen = step1_api$fit_generation(),
     step2_status = step2_api$run_status(),   step2_gen = step2_api$run_generation(),
     step3_status = step3_api$run_status(),   step3_gen = step3_api$run_generation()
   )
   ```

   Re-derive the names from the current `pipeline_results` list in `app_server.R`. `exportTestValues()` does nothing unless the app runs in test mode.
2. **Wiring test.** Add `test-app_server.R`:
   - Build the synthetic data dir (Phase 3 helper).
   - Set `WISEAPP_DATA_SOURCE = "local"` and `WISEAPP_DATA_PATH = <dir>`.
   - Run `testServer(app_server, { ... })`.
   - Assert that Step 0 publishes a non-empty survey list, Step 1 receives it (via the Step 1 sample UI output or returned API), and the step badges render empty before any run.

   If `testServer(app_server)` hits something that needs a real session, such as `.duck_register_session()` `onSessionEnded` handling or `session$userData`, prefer a minimal fix in the test (a mocked binding) over changing app code. Record the limitation if it can't be done.

Acceptance:

- `test-app_server.R` runs in tier 1 in ≤ 10 s.
- No behaviour change in production mode. `exportTestValues` is inert outside `shiny.testmode`.

---

## Phase 8 - shinytest2 end-to-end tier

**Goal:** 2 or 3 browser tests covering what only a browser can (R11).

Steps:

1. **App entry point for tests.** Add `tests/testthat/apps/wise/app.R`:

   ```r
   # Launches the installed (CI) or load_all()'d (local) package against the
   # synthetic data dir passed via env vars by the test.
   if (requireNamespace("pkgload", quietly = TRUE) && nzchar(Sys.getenv("WISEAPP_E2E_SRC"))) {
     pkgload::load_all(Sys.getenv("WISEAPP_E2E_SRC"), quiet = TRUE)
   } else {
     library(wiseapp)
   }
   run_app()
   ```

   Check the current shinytest2 docs for the recommended pattern for golem/package apps at implementation time. Passing an app object to `AppDriver$new()` is an alternative.
2. **Fixture config.** Generate the golden-path config with the app's own export (`wise_config_snapshot()`) from a run against the synthetic data dir, commit it as `tests/testthat/fixtures/e2e_config.json`, and document in a header comment how to regenerate it. Keep the run small (see the review, R11): coefficient draws off, 1 SSP, 1 climate model.
3. **Tests** in `test-e2e-app.R`, each starting with `skip_if_not_e2e()`:
   - *Boot smoke:*
     - start `AppDriver` with env vars pointing at the synthetic data dir and `WISEAPP_ASYNC_SYNC=1`;
     - `app$wait_for_idle()`;
     - assert the overview survey list loaded via `app$get_value(output = ...)` or an exported value;
     - assert the hexmap container exists via `app$get_js()`;
     - assert no `"error"` level entries in `app$get_logs()` from the browser.
   - *Golden path:*
     - upload `e2e_config.json` to the import `fileInput` with `app$upload_file()` and press Start;
     - `app$wait_for_value(export = "step3_gen", ignore = list(NULL, 0L), timeout = 120000)`;
     - assert all three statuses are success and that a short, explicit list of outputs is non-empty.
     - Use `app$expect_values(export = TRUE)` only if the exported values are deterministic. Otherwise assert specific fields.
   - *(Optional) Visibility:* open the config flyout that regressed in shiny 1.14 and assert its `conditionalPanel` is visible and then hidden via `app$get_js()`.
4. **No screenshots.** Set `expect_values(screenshot_args = FALSE)` wherever `expect_values` is used.
5. **CI job** `e2e` in the same workflow (or a separate `e2e.yaml`):
   - `ubuntu-latest`, `needs: check`, env `WISEAPP_TEST_E2E=true`;
   - run only `testthat::test_local(filter = "e2e")`;
   - upload `tests/testthat/_snaps/` and the AppDriver logs on failure;
   - trigger: push to `main`/`dev`, PRs, nightly `schedule`, and `workflow_dispatch`.
   - If PR run time becomes a problem, drop PR triggering and keep push + nightly. Record the choice.

Acceptance:

- The E2E job passes 5 consecutive runs (flakiness check).
- Total E2E job time ≤ 3 min after dependency cache.
- Locally, `devtools::test()` without `WISEAPP_TEST_E2E` does not start a browser.

---

## Phase 9 - Hygiene sweep and verbosity

**Goal:** low-risk cleanup after files are in their final places (R10).

Steps (each a separate commit):

1. Remove top-of-file `library(testthat)` / `library(shiny)` calls, and `wiseapp:::` / `testthat::` prefixes in tests. Re-baseline with `grep -c`. Leave `pkg::` for packages that are not imported.
2. Replace `set.seed(n)` in tests with `withr::local_seed(n)`. Check the test still passes; tests relying on RNG state leaking between tests are bugs to fix, not preserve.
3. **Verbosity option (`R/` change, own PR):**
   - add an internal `wise_inform()` that calls `message()` only when `getOption("wiseapp.verbose", TRUE)`;
   - switch the progress/log `message()` calls to it (re-derive with `grep -n 'message("\[' R/*.R` and the cutoff/`[active_mask]`/`[overview]` logs);
   - set `options(wiseapp.verbose = FALSE)` in `setup.R` via `withr::local_options(.local_envir = teardown_env())`;
   - tests asserting a log message set it back to `TRUE` locally.
   - Do not touch `warning()` / errors.
4. Convert `expect_true(identical(...))`, `expect_true(x == y)`, and `expect_true(x %in% y)` to `expect_identical` / `expect_equal` / `expect_in` / `expect_contains` **only in files touched in this phase**. Not a repo-wide churn commit.
5. `tests/spelling.R`: either add `inst/WORDLIST` (`spelling::update_wordlist()`) and keep it non-blocking, or delete it. Pick one and record it.

Acceptance:

- The test log of a default run has no app progress chatter.
- Pass count unchanged.

---

## Phase 10 - Documentation

1. Update the **Testing** section of `AGENTS.md`:
   - tiers and env vars (`WISEAPP_TEST_PROCESS`, `WISEAPP_TEST_E2E`);
   - file-naming rule;
   - where helpers live;
   - "fixture to helper when used by ≥ 2 files";
   - how to run CI-equivalent locally;
   - the E2E config regeneration command.
2. Update the test-file count and "areas with dedicated coverage" in `AGENTS.md`.
3. Add a short status table to the end of `review/test_suite_review.md` recording which recommendations were done, skipped, or changed, with PR links. Move both review docs to `review/archive/` once complete, matching existing practice.

---

## Phase 11 - Close priority coverage gaps

**Goal:** tests for user-facing code that currently has little or none (R13).

Re-baseline: run `dev/test_coverage.R` and sort the per-file CSV by uncovered lines. Review-time priorities, re-checked against the fresh numbers:

1. `R/fct_predict_outcomes.R` (38%): unit tests per engine in `ENGINE_REGISTRY` and per outcome transform (identity, log back-transform, logistic probability). Use the Phase 3 fitted-model fixture.
2. `R/mod_3_02_infra.R`, `mod_3_03_digital.R`, `mod_3_04_labor.R`, `mod_3_05_education.R` (about 5% each): one `testServer()` per module. Assert the returned scenario object for the default state and for one non-default lever setting, and that a lever is reported inactive when its variable is missing from `variable_list`.
3. `R/mod_1_08_modelfit.R` (0%): `testServer()` with a fixture model; assert diagnostics outputs render and handle a missing or failed fit.
4. `R/fct_step1_headline.R` (43%): headline values against hand-computed expectations on a tiny fixture, including the log-outcome and binary-outcome paths.
5. `R/fct_results.R`: tests only for branches that compute user-visible numbers (coefficient tables, translations). Ignore presentation-only branches.

Rules:

- Tests go in the `test-<R file>.R` file for the target (Phase 5 convention).
- Prefer assertions on values and returned objects over rendered HTML.
- Targets in R13 are guides. Stop when the remaining uncovered lines are presentation-only or defensive.

Acceptance:

- Each target file's coverage rises to its R13 guide or the PR explains why not.
- Tier-1 time stays ≤ 90 s.

---

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Rename phase conflicts with feature branches editing tests | Land Phase 5 at a quiet point; renames are pure `git mv` so rebases follow them |
| Memoised fixtures leak mutations between tests | Fixtures are read-only by convention; mutate on copies; parallel file runs isolate processes anyway |
| CI dependency install is slow (fixest, xgboost, arrow, duckdb) | Public RSPM binaries + `setup-r-dependencies` cache |
| DuckDB community extension unavailable on CI | Cached extension dir; tests skip with a clear message rather than error; prefer hard-coded H3 cells in fixtures |
| `testServer(app_server)` blocked by session-level side effects | Mock the specific binding; if still blocked, record it and rely on the E2E boot smoke |
| E2E flakiness from timing | Wait on exported values, never `Sys.sleep()`; `WISEAPP_ASYNC_SYNC=1`; small fixture data |
| Coverage tooling aborting on a failing test | Use `type = "none"` + `test_dir(stop_on_failure = FALSE)` (Phase 0) |
