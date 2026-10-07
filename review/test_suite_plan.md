# Test Suite Plan (slimmed)

What is left to do from `review/test_suite_review.md`, kept deliberately small: a reliable CI run, a suite that stays fast, and targeted coverage where a broken number would reach users. The original eleven-phase plan was cut down (see "Decisions" in the review, section 4).

**Do not assume the repository is still in the state described here.** Each step starts by re-checking what it acts on, and ends with acceptance criteria that can be checked.

## Status at 2026-10-07

| Item | Status |
|---|---|
| DuckDB 1.5.6 upgrade (bundle, pin, checksums, manifest) | Done (`c733a10`, `d13a585`) |
| CI-safe suite: `duckdbfs` and redundant skips removed, `make_h3_con()` skips, `setup-env.R`, workflow with `--no-tests` then `test_local()` | Done (`598027f`) |
| CI roxygen2 pinned to `RoxygenNote` (8.x rewrites NAMESPACE layout) | Done (`4b42488`) |
| httpfs loaded in the credential-test seed helper; kernel random-draw tolerance 1e-10 | Done (`70cc61d`, `fe599ae`) |
| **First green GitHub run** | **Done: run 37618324871 on `fe599ae`.** 0 failed, 1 skipped, 7583 expectations. Cold: Tests step 6m33s, job 11m47s. Warm time not yet measured (the extension cache is first saved by that run). |
| Headless-Chrome PNG test skips on CI | In progress: workflow passes `--no-sandbox` to chromote and the skip message now carries the real error. Verify on the next run that the test runs; if it still skips, read the message. |
| `R CMD check` WARNING (undocumented `@param`, R2-CQ-01) and 3 NOTEs | Open. `error-on` stays `"error"` until the WARNING is fixed. |

## Ground rules

- One step per PR. Do not mix test moves with behaviour changes.
- The suite stays green at every step. Do not fix unrelated failures inside a refactor PR; record them.
- No change to `R/` behaviour unless a step says so.
- Move a fixture into a `helper-*.R` file only when two or more test files use it.
- Do not add a test tier, a skip-helper layer, or parallel test files unless a measured problem needs it (review, section 4).

---

## Step 1 - Get CI green

**Goal:** the workflow in `.github/workflows/R-CMD-check.yaml` passes on a push to `dev`.

1. Confirm the CI-safety changes are committed. Push `dev` (confirm with the user before pushing).
2. Watch the run: `gh run list --limit 3`, then `gh run view <id> --log-failed`.
3. Triage the first real failures. Likely causes, in order of probability:
   - **Dependency install:** `duckdb == 1.5.6` must resolve from Package Manager. If it does not, check that 1.5.6 is current; as a fallback, use a dated Package Manager snapshot.
   - **`R CMD check` warnings or errors** that never ran before (the workflow tolerates warnings; `error-on: "error"`). Roughly 25 functions lack `@param` entries (tracked as R2-CQ-01 in `review/REVIEW-2026-10-06-tracking.md`).
   - **`roxygenise()` changes `NAMESPACE`**, which fails the `git diff --exit-code NAMESPACE` step. Commit a regenerated `NAMESPACE`.
   - **`h3` extension** download or version mismatch on the runner (the extension cache is keyed on `DESCRIPTION`).
   - **Headless Chrome:** the PNG export test skips itself if no Chrome is found. If it skips on CI, add `browser-actions/setup-chrome` so it runs.
   - **Real mirai worker tests:** outside a `load_all()` session the worker runs `library(wiseapp)` from default library paths. `test_local()` uses `load_all`, so this should work; verify the two worker tests (`test-step2-async.R`, `test-fct-overview-metadata.R`) actually ran.
   - **Locale or timezone** differences from macOS. `setup-locale.R` already forces a UTF-8 character locale.
4. Look for silent skips in the CI log: the number of skipped tests should be near zero. A drop in test count versus the local run means something skipped.
5. Record cold and warm run times.

**Acceptance:**
- The workflow is green, or red only on failures listed in the PR as pre-existing.
- CI log shows about 1067 tests and no unexpected skips.
- Test step ≤ 8 minutes; whole job ≤ 15 minutes cold and ≤ 10 minutes warm. If it is slower, do Step 4 for the slowest files only.
- Once warnings are fixed, tighten `error-on` to `"warning"`.

---

## Step 2 - Small cleanups (optional, low risk)

Each is one commit. Re-verify each candidate before acting.

1. **Remove the three Shiny-semantics tests** in `test-rerun-regressions.R` ("passing a reactive's value to observeEvent deafens it...", "passing the reactive itself keeps the dependency alive", "ignoreInit swallows the first click..."). Keep the explanation of the mechanism as a short comment beside the relevant helper in `R/` (comment-only `R/` edit is allowed). Before deleting, run the file with and without them and confirm no `R/` line loses coverage:
   `covr::file_coverage()` or `covr::package_coverage(type = "none", code = ...)` on the source `tests/` directory (not `type = "tests"`).
2. **Merge the two export-key scanners** (`test-export-wiring-contract.R`, `test-csv-export-wiring-contract.R`) into one source-contract file with one scanner covering every button helper. Keep an `expect_gt(length(files), 0)` guard so an empty scan fails.
3. **Move `make_regtable_df` tests** from `test-table-csv-export.R` to the `fct_results` tests.
4. **Rename history-named files** only as a pure `git mv` in its own PR, at a time with no open branches editing tests. Rule: a file that mainly covers one `R/` file is named `test-<stem>.R` (for example `test-fct-outcome.R` to `test-fct_outcome.R`). Skip this step if it causes rebase pain.

**Acceptance:** the pass count drops only by the removed tests; per-file coverage of `R/` does not drop.

---

## Step 3 - Close priority coverage gaps

Do one target file per PR, spread over time. Re-measure first:

```r
# Source-tree coverage; excludes real-worker test files (their traces corrupt covr).
covr::package_coverage(type = "none", code = 'testthat::test_dir("tests/testthat", stop_on_failure = FALSE, load_package = "none")')
```

Priority order (targets are guides, not gates; stop when what is left is presentation-only or defensive):

1. `R/fct_predict_outcomes.R` (38% at review time, 164 lines): unit tests per engine in `ENGINE_REGISTRY` and per outcome transform (identity, log back-transform, logistic probability). Target 80% or more.
2. `R/mod_3_02_infra.R`, `mod_3_03_digital.R`, `mod_3_04_labor.R`, `mod_3_05_education.R` (about 5% each): one `testServer()` per module asserting the returned scenario object for the default state and one non-default lever setting, and that a lever is inactive when its variable is missing from `variable_list`. Target 60% each.
3. `R/mod_1_08_modelfit.R` (0%): `testServer()` with a fixture model; diagnostics outputs render and a missing or failed fit is handled. Target 50%.
4. `R/fct_step1_headline.R`: re-measure (new tests were added since the review). Add hand-computed fixtures for log-outcome and binary-outcome paths if still below 70%.
5. `R/app_server.R` (0%): a `testServer(app_server, ...)` wiring test against a synthetic local data directory (`WISEAPP_DATA_SOURCE=local`). It asserts that Step 0 publishes a non-empty survey list and Step 1 receives it. If `testServer(app_server)` hits session-only code, mock the specific binding rather than changing app code, and record the limit.
6. `R/fct_results.R`: only branches that compute user-visible numbers (coefficient tables, translations).

Rules: tests go in `test-<R file>.R`; assert on values and returned objects, not rendered HTML; keep the full suite time from growing by more than a few seconds per PR.

---

## Step 4 - Speed (only if Step 1 shows a problem)

Trigger: the CI test step exceeds about 8 minutes, or a local run exceeds about 4 minutes.

1. Re-time per file to find the slow ones (the review-time list is stale):
   ```r
   r <- as.data.frame(testthat::test_dir("tests/testthat", reporter = "silent", stop_on_failure = FALSE))
   aggregate(real ~ file, r, sum) |> (\(d) d[order(-d$real), ])() |> head(10)
   ```
2. For the top few files, build expensive fixtures (fitted models, Step 2 or Step 3 pipelines, weather parquet directories) once per run in a helper with a visibly named `fixture_*()` function, memoised in a package-private environment. Fixtures are read-only by convention; tests that mutate copy first.
3. At review time the slowest files were `test-policy-sim-compare-agg-cache.R`, `test-fct_get_weather.R`, `test-step2-payload.R`, `test-fct-overview-metadata.R`, `test-export-bundle.R`. The last two are slow because they start a real worker or headless Chrome; do not "fix" those, they are meant to be real.
4. Only if still too slow after memoising: consider an explicit skip for the real-process tests locally, controlled by one environment variable, always on in CI.

**Acceptance:** same pass count; same per-file coverage; measurable time drop.

Wider speed options, if more is wanted after memoising (in this order; each needs the per-file timing from item 1 and the coverage check from Step 2):

- Consolidate near-duplicate tests (the duplicated fixture families in the review, M1) so a fixture is built once.
- Drop non-essential tests only where the coverage method shows no `R/` line loses coverage and no distinct behaviour is pinned.
- Speed up the slowest remaining tests by shrinking inputs (rows, draws, years) without changing what is asserted.

CI reference at 2026-10-07: Tests step 6m33s cold against the 8-minute trigger, so Step 3 additions have only about 1.5 minutes of headroom.

---

## Backlog (do only if a concrete need appears)

| Item | When it is worth doing |
|---|---|
| Shared fixture helpers (`helper-fixtures-*.R`) for all duplicated builders | When fixture drift causes a real bug, or Step 4 needs it. |
| `shinytest2` end-to-end tests (boot smoke; golden path by config import; one `conditionalPanel` visibility check) | After CI is stable and a browser-only regression recurs. Keep to 2 or 3 tests. Needs `shinytest2` in `Suggests`, `shiny::exportTestValues()` hooks in `app_server`, and a synthetic data-directory helper. |
| App verbosity option (`wiseapp.verbose`) to silence progress messages in tests | When log noise hides real warnings. It is an `R/` change; keep it in its own PR. |
| Hygiene sweep (`library()` calls, `wiseapp:::` prefixes, `set.seed()` to `withr::local_seed()`, `expect_true(identical(...))`) | Only in files touched anyway. No repo-wide churn commit. |
| `tests/spelling.R` | Keep non-blocking or delete; add `inst/WORDLIST` if kept. |
| Coverage report job in CI | If wanted; make it non-blocking and upload as an artifact. |
| Coverage ratchet (fail if overall coverage drops more than 1 point) | After the Step 3 gaps are closed. |

---

## Documentation (at the end)

- Update the **Testing** section of `AGENTS.md`: test-file count; the env pinning in `setup-env.R`; "run the suite from the source tree" (`devtools::test()` or `testthat::test_local()`, as CI does); the rule that a fixture becomes a helper when two or more files use it.
- When this plan is finished, move `review/test_suite_review.md` and this plan to `review/archive/`, matching existing practice.

## Risks

| Risk | Mitigation |
|---|---|
| First CI run reveals many `R CMD check` warnings | `error-on: "error"` is already set; list the warnings in an issue and tighten later. |
| `h3` extension download fails on the runner | The fixture skips with a message; the extension directory is cached. |
| Silent skips make CI look greener than it is | Compare the CI test and skip counts with a local run. |
| Memoised fixtures leak mutations between tests (Step 4) | Fixtures are read-only by convention; mutate copies. |
| Rename PRs conflict with feature branches | Pure `git mv`, at a quiet point. |
