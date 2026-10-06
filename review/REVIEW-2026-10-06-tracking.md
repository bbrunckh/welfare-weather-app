# Review remediation tracking (REVIEW-2026-10-06)

Source of truth for findings: `review/REVIEW-2026-10-06.md` (immutable; reviewed commit `df37cdb`). This file tracks only status. Finding text, evidence and remediation advice stay in the report; section numbers below (`§`) refer to it.

Status per item: `☐` todo · `◐` in progress · `☑` done (merged + verified) · `✗` won't fix / deferred (reason in Notes) · `–` already fixed or not an issue.
Sev: C critical · H high · M medium · L low. Effort: S < 1 day · M 1-3 days · L > 3 days.

## Working rules

- One branch per batch off `dev` (`fix/rev-b1-deploy`, ...). Within a batch, one commit per finding ID, with the ID in the commit subject.
- A finding is `☑` only when (a) a regression test exists that fails before and passes after (or the item is config/docs and a reproducible command shows the fix), (b) the narrowest relevant `devtools::test(filter = ...)` passes, and (c) the Notes cell records the commit and what was verified.
- Numerical changes (CR-BUG-01/02, R2-BUG-01/02/03/06, R2-BUG-04) need a Decision log entry below with before/after BFA headline numbers, because outputs move.
- Performance changes follow `review/optimization_guidelines.md` §9 (equivalence gate, before/after timing and peak RSS on BFA and IRN). Timings in the report come from a 4-vCPU cloud container; re-measure locally and do not copy them.
- Run `devtools::document()`, `devtools::check()` and the full suite once per batch, with the tree quiescent. Regenerate `manifest.json` (`dev/00_make_manifest.R`) at the end of any batch that changes dependencies or `R/` files.
- Do not push or deploy from a batch without an explicit go-ahead.

Batch order: B1 -> B2 -> B3 -> B4 are the report's "Now" list (§11 items 1-5). B5 is low-risk and can merge early. B6 follows the correctness batches so benchmarks are taken on correct numerics. B7-B9 are triage.

## Batch overview

| Batch | Scope | Items | Status | Branch / PR |
|---|---|---:|---|---|
| B1 | Deployment blockers and dependency drift | 8 | ◐ | |
| B2 | Security | 15 | ◐ | fix/rev-w1-security, fix/rev-w1-weather | |
| B3 | Headline numerics (Steps 1-2) | 9 | ◐ | fix/rev-w1-weather, fix/rev-w1-delta | |
| B4 | Step 3 levers and decomposition | 10 | ◐ | fix/rev-w1-levers | |
| B5 | Robustness, CI and test hygiene | 13 | ◐ | |
| B6 | Performance (ranked, §5.6) | 23 | ☐ | |
| B7 | Remaining Medium and Low bugs | 33 | ◐ | fix/rev-w1-cleanup | |
| B8 | Accessibility | 16 | ☐ | |
| B9 | Code quality and docs | 11 | ◐ | fix/rev-w1-cleanup | |

## B1 - Deployment blockers and dependency drift (§10.1, §3)

Goal: a Connect deploy from the manifest renders the UI, survives the first Step 3 run, and keeps loading DuckDB extensions. Regenerate the manifest last, once R2-OPS-01/02/04/05/07 are done.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| R2-OPS-01 | H | S | ☑ | Declare `brand.yml` in `DESCRIPTION` (or whitelist in manifest script) | Declared in Imports; UI builds from declared deps (test-deploy-contract). |
| R2-OPS-02 | H | S | ☑ | Port `mod_3_09` tables from DT to reactable (`.step2_reactable()` pattern); remove DT; fix CSV buttons | mod_3_09 tables now reactable via .step2_reactable; DT no longer referenced in R/. Also clears the 8 DT test blocks. |
| R2-SEC-06 | H | S | ☑ | Vendor the brand font (`font_face()` local files); pin or vendor the CARTO basemap style, or document egress | Open Sans vendored in inst/app/fonts (+OFL, whitelisted in .gitignore); page renders with no Google hosts. CARTO tiles cannot be vendored: egress requirement documented in AGENTS.md. |
| R2-OPS-05 | M | S | ◐ | Pin `duckdb` in `DESCRIPTION` to the version the bundled extensions target; check version + SHA-256 at load; set persistent `extension_directory`; decide on Azure (`azure`/`delta` not bundled) | Pinned duckdb (== 1.5.5); bundle version + SHA-256 checked before INSTALL on Connect (tests). Persistent extension_directory not needed: 1.5.5 already uses shared ~/.duckdb. Open: Azure/delta not bundled (decide drop vs bundle); move to 1.4.x LTS. |
| R2-OPS-04 | M | S | ☑ | Remove `mice::futuremice()` / `furrr` path (run MI sequentially or on the mirai singleton) | Future/futuremice path removed; imputed LASSO runs sequentially. future, future.apply dropped from Imports; run_lasso_selection lost use_parallel/n_workers/parallel_min_n/globals_max_size. |
| R2-OPS-07 | M | S | ☑ | Raise `Depends` to R >= 4.4; declare `later`, `tidyselect`; move `ranger`/`xgboost`/`parsnip` to Suggests | Depends R >= 4.4.0; later, tidyselect declared; parsnip/ranger/xgboost to Suggests. pkgload stays (app.R load_all). |
| R2-OPS-03 | M-H | S | ◐ | Regenerate `manifest.json`; add CI/script check that every Import and `R/*.R` file is in it | Needs a commit first: dev/00_make_manifest.R stages git HEAD. test-deploy-contract manifest checks fail until it is regenerated. |
| CR-SEC-09 (part) | L | S | ☑ | `rel="noopener noreferrer"` on the two `target="_blank"` links | rel=noopener on the two remaining links; test-deploy-contract. |

## B2 - Security (§3)

Goal: close the credential-mixing paths first (CR-SEC-01, R2-SEC-01); the rest are hardening. Rotate the service-principal secret after CR-SEC-01 ships. Each fix gets a `testServer` or offline regression test.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| CR-SEC-01 | C | M | ☑ | Auto-connect: do not register apply observer or read connection inputs; never mix UI and env fields; allowlist source types and Databricks hosts (https, no user-info/port) | dbb179e: auto-connect mode registers no apply/source observers and connection_params() refuses; connection all-UI or all-env ($origin field); source-type allowlist; Databricks host = configured host or https Databricks domain, no user-info/port/path, re-checked in workers; testServer + offline tests. Behaviour: partial UI entry no longer picks up env creds; auto-connect failure needs reload. Rotate SP secret after release. |
| R2-SEC-01 | H | M | ☑ | Track connection provenance; UI connections pass user credentials to the local worker or refuse async; re-validate allowlist in worker | 8b5f537: UI connections passed whole (with creds) to the local daemon, env connections still scrubbed; .connection_field() never fills UI fields from env; UI Azure without creds errors (no managed-identity fallback); tests. No runtime check that daemons are local (comment only). |
| CR-SEC-02 | H | M | ☐ | Allowlist buckets/volume roots/local roots in config; disable `local` in production; DuckDB `enable_external_access`/`allowed_directories`/`lock_configuration`; validate path tokens | |
| CR-SEC-03 | H | M | ☐ | `SCOPE` on every `CREATE SECRET`; per-session secret names dropped at session end; per-session or per-task connection | |
| CR-SEC-04 | H | S-M | ☑ | Source identity in weather cache keys; private 0700 dir; `tempfile(tmpdir=)` + rename; LRU via `Sys.setFileTime()`; no eviction under live view; `.sql_literal()` on paths | 3319344: source identity in cache keys (no creds), tempfile+rename, LRU via Sys.setFileTime, no eviction younger than async timeout, .sql_literal paths, 0700 cache dir; tests. Not done: h3 mappings still on disk; fct_step2_compute.R:131 base dir not 0700. Existing cache entries invalidated once. |
| R2-SEC-02 | M | S-M | ☐ | Clear secrets/token cache/views in `on.exit()` of tasks carrying credentials | Depends on CR-SEC-03 design |
| CR-SEC-05 | M | S | ☐ | Drop `^overview-` connection ids on config import; fix success message; deployment label in exports/provenance | |
| CR-SEC-06 | M | S | ☑ | `httr2` `req_timeout()`, `req_retry()`, summarised errors | 15723cd: req_timeout 30 s token / 300 s file, req_retry 3 tries on transient errors, status-only messages; tests check request config (httr2 mock skips retry loop). |
| CR-SEC-07 | M | L | ☐ | Per-run size limits from config; cap Step 2 queue by bytes; separate small profile for metadata | Rest of async coverage tracked under CR-PERF-04 (B6) |
| CR-SEC-08 | L-M | S | ☐ | Central `wise_user_error()`: log full condition with id, show short classified message | |
| R2-SEC-03 | L | S | ☑ | Hash token-cache key (SHA-256); cap and expire entries | 872a8d5: SHA-256 (digest) key, expiry-5 min drop, cap 16; test. |
| R2-SEC-04 | L | S | ☑ | `dir.create(mode = "0700")` for configured artifact/weather roots; unlink in `onStop()` | 24590bf: artifact and weather-store roots 0700; app-created artifact root removed in onStop. Configured weather-store root not removed (ownership unclear). |
| R2-SEC-05 | L | S | ☑ | `on.exit(unlink())` for `.export_write_echarts()` temp files | e2221c5: temp html and _files removed on every exit path; test. |
| CR-SEC-09 (rest) | L | S | ☐ | Move inline JS/CSS to `custom.js`/`custom.css` (enables CSP); extension version/checksum check | `rel` links in B1 |
| R2-SEC-07 | Info | S | ☐ | Fix identity/storage of the prepared-weather persistent cache before ever enabling it | Off by default; may close as ✗ |

## B3 - Headline numerics, Steps 1-2 (§4.1, §4.2)

Goal: correct the biased headline numbers. One finding per commit. Each needs a Decision log entry and a BFA before/after check (OLS and RIF where relevant). Do R2-BUG-01 first (smallest, unlocks sync/async parity tests).

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| R2-BUG-01 | H | S | ☑ | `dates <- as.Date(dates)` at top of `get_weather()`; async-vs-sync parity test with transformed variable and window ending before 2020 | 7e460ee: as.Date(dates) in get_weather(); parity test (character vs Date, transformed vars, window ending before 2020). BFA: no change with default 1991-2020 window; 1981-2010 window changed 91-100% of rows before fix. |
| CR-BUG-01 | H | S | ☑ | CMIP6 baseline: union raw monthly rows (hist `< 2015-01-01`, SSP `>=`), one `AVG` per (model, h3, month); characterisation test with unequal part lengths | 4e644aa: hist rows < 2015, SSP rows >= 2015, one AVG per (model, h3, month); unequal-length + overlap test. BFA delta-t SSP3-7.0 2025-35: 0.619 -> 0.755 C (matches report). See Decision log. |
| CR-BUG-02 | H | M | ☐ | One `outcome_to_model_scale()` used in training, `predict_rif`, decomposition context and level channels; store scale on `model_fit`; assert in `predict_rif`; LCU test | Until done, disable LCU for RIF and SP lever. Pairs with R2-BUG-04 (B4) |
| R2-BUG-03 | H | S | ☑ | Median gradient: smoothed-quantile derivative `h_i = k_i mu_i / sum k`; test vs Monte Carlo (target ratio ~1, was 0.18 on BFA) | e734037: smoothed-quantile derivative k_i = w phi((m-mu)/b)/b, h = k mu / sum k, b = bw.nrd0. Synthetic delta/MC SD 0.00/0.25/0.01 -> 1.00/1.01/1.01; BFA median 0.011 -> 0.971; mean/Gini/headcount bit-identical. |
| R2-BUG-02 | H | S | ☑ | Drop NA rows consistently from `h` and `F` (or `h = 0`), count and report; assert row alignment at `step2_compute()` | e77f50a: non-finite rows dropped from h and F, count returned as n_coef_dropped; apply_band_transform() guards non-finite point. Test: one NA -> var_coef within 1% of clean. Not yet surfaced in UI. |
| CR-BUG-07 | L | S | ☑ | Fix `rowids` comment; assert prediction/row alignment | 290e183: rowids comment corrected; predict_outcomes() length check and .step2_compute_assert_alignment() at step2_compute(); tests. |
| R2-BUG-28 | M | S-M | ☑ | Count and report NA household-year predictions per year/member; decide impute/exclude/fail | 843e416: per-year n_na_dropped on per-year results plus one [wiseapp] message per preparation; report only (rows used unchanged). Not yet surfaced in UI. |
| R2-BUG-29 | M | S | ☑ | Guard zero reference SD in standardised anomalies; floor or drop month with warning | 0870dbe: zero reference SD -> NA plus one warning with counts (user decision: no floor); test. |
| CR-BUG-06 | M | S | ☐ | Require full coverage for CMIP6 members or report/exclude | Feeds R2-BUG-02 |

## B4 - Step 3 levers and decomposition (§4.1, §4.2)

Goal: correct policy results. Keep separate from B3 so output changes are attributable. R2-BUG-04 depends on CR-BUG-02's scale function.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| CR-BUG-08 | H | S | ☑ | One exact term matcher (`%in%` or anchored escaped regex) shared by UI, gating and term maps | 0f620d9: shared exact matcher .policy_lever_terms()/.lever_in_model() for UI gating, apply_policy_to_svy and decompose term maps; test-policy-lever-matching (fails before, passes after). |
| R2-BUG-04 | H | S | ☐ | Store currency with SP config; convert per row or force PPP input | After CR-BUG-02 |
| R2-BUG-05 | H | S | ☑ | Run labour reallocation only when a sector target changed; init sliders to observed shares | 0d30ae5: sector targets NULL until a slider moves; reallocation only when a target is set; sliders start at observed weighted shares; test-policy-labor-sectors (synthetic: 188/133/149 -> 470/0/0 before, unchanged after; reach 312 -> 30). |
| R2-BUG-06 | H (RIF) | M | ☐ | Repositioning: subtract survey-time weather (`W_t - W_svy`); parity test vs `predict_rif` | Reference implementation shares the wrong convention; fix both |
| R2-BUG-07 | M | M | ☐ | SP effect against predicted year-t level; level channels from predicted states | |
| R2-BUG-12 | M | S | ☑ | Per-lever `wise_seed(seed, "policy", "<lever>")` streams | e47f815: per-lever wise_seed streams; SP preview uses the sp stream. Changes draws vs earlier runs with the same seed. test-policy-lever-streams. |
| R2-BUG-13 | M | S-M | ☐ | NA outcome/covariate rows: restrict to referenced rows, treat NA as untreated, report count | |
| R2-BUG-14 | M | S | ☐ | Block Step 3 when Step 2 is stale, or snapshot `mf` into `hist_sim` | Also CR-BUG-14 residual |
| R2-BUG-17 | L | S | ☑ | `idx[sample.int(length(idx), k)]` in all nine places | 6a71600: idx[sample.int(length(idx), k)] at all 9 sites; test-policy-sample-single. |
| R2-BUG-26 | L | S | ☐ | Weighted ECDF in legacy decile decomposition; weighted policy input diagnostics; consistent ensemble ranking | |

## B5 - Robustness, CI and test hygiene (§10.1-10.3)

Goal: availability fix plus guardrails so B1-type regressions cannot recur. Safe to merge early.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| R2-OPS-06 | H | S | ☑ | Check `mirai::status()$connections` before dispatch and after connection-error rejection; relaunch daemon; `.timeout` with clear error | Daemon liveness check + relaunch (15 s grace), per-task timeouts (WISEAPP_ASYNC_TIMEOUT_MIN=90, METADATA_TIMEOUT_SEC=300), readable errors. Real-daemon tests (kill, relaunch, timeout). |
| CR-OPS-01 | M | M | ◐ | GitHub Actions: `check-r-package` in declared-deps-only library, `NOT_CRAN=true`, `LANG=C.UTF-8`, manifest-sync check, non-blocking `lintr`, `shinytest2` smoke; `.Rbuildignore` add `^docs$`, `^\.github$` | .github/workflows/R-CMD-check.yaml (check in declared-deps library, UTF-8, document + NAMESPACE drift, contract tests, non-blocking lintr), .lintr, .Rbuildignore. Not yet run on GitHub; fails on errors only until Rd @param gaps are fixed; shinytest2/axe is B8. |
| R2-OPS-08 | M | S | ◐ | Fix Step 2 benchmark harness (time inside `consume_key`, opt-in memory profiling, Databricks mode, Linux `time`); fix or delete broken UI benches | Weather time excludes pipeline time, real keys, memory profile opt-in, Databricks mode, portable wrapper, broken UI benches and hard-coded paths fixed; smoke-run on BFA. Not done: .profile_record() ordering in fct_get_weather.R. |
| R2-OPS-09 | L | S | ◐ | Fix fragile tests (mtime sleeps, top-level skips, zip skips, `set.seed`, spelling) | Fixed: mtime sleep, top-level fixture skip, 12 zip skips. Left: localhost:1 polling, set.seed vs local_seed, spelling never fails. |
| CR-PERF-13 / R2-PERF-08 | M | S | ◐ | DuckDB `memory_limit`, `temp_directory`, `threads`; fixest/data.table threads from config; `availableCores()` | Env-configurable (WISEAPP_DUCKDB_MEMORY_LIMIT/THREADS/TEMP_DIR, WISEAPP_THREADS); defaults unchanged apart from a per-process spill dir. Needs chosen values for Connect. |
| CR-PERF-15 | M | S | ✗ | Deploy installed package, not `load_all()` | Deferred (2026-10-06, user decision): keep `load_all()` in app.R and git-backed deploys. Installing the built package needs a GitHub remote in the manifest; revisit if cold starts matter. Cheaper lever: Min Processes / idle timeout on Connect. |
| Test: brittle snapshot | L | S | ☑ | `test-mod_2_02_results-characterization.R`: compare with tolerance instead of rounding to 10 sig. digits | Characterization snapshot now json2 with 8-decimal rounding and tolerance 1e-4; snapshot file regenerated. |
| Test: C-locale non-ASCII | L | S | ☑ | ASCII-safe strings in `test-fct_model_card.R`, `test-fct_weather_pipeline.R`, `test-mod_3_07_results.R`, `test-fct-results-table2-reactable.R`; remove non-ASCII from five `R/` files | tests/testthat/setup-locale.R forces a UTF-8 LC_CTYPE; non-ASCII removed from R/ code (escapes). |
| Test: check-only failures | L | S | ☑ | `test-export-wiring-contract.R`, `test-csv-export-wiring-contract.R`, `test-pipeline-runner.R`, `test-bench-step3.R`, `test-step2-async.R:298` | Source-tree tests skip when R/ or dev/ is absent; async worker test mirrors production load. |
| R CMD check warnings/notes | L | S | ◐ | Undeclared test pkgs (`arrow`, `bit64`, `chromote`, `data.table`, `later`), global-binding notes, top-level `docs/` | Fixed: non-ASCII, undeclared test pkgs, ^docs$/^\.github$. Left: Rd @param gaps (~25 functions), global-binding notes. |
| Run-stage log | L | S | ☑ | One structured log line per stage (run id, stage, keys, cache state, elapsed, peak RSS, outcome); no credentials/household data | R/utils_log.R .wise_log_stage(); Step 2 settle/worker and Step 3 run; allowlisted, sanitized fields; WISEAPP_STAGE_LOG=0 disables. |
| RED-06 / batch dupes | M | M | ◐ | One parameterised batch driver over `step2_compute()`; fix `batch/02_weather_stats.R:218` | Fixed batch/02 weather_agg_for call (R2-BUG-25) and added parse + signature smoke tests. Consolidating the five 04_run_sim copies not done (needs a decision). |
| R2-CQ-01 | L | S | ◐ | Track `man/` or mark helpers `@noRd`; update `NEWS.md` | NEWS.md updated; CI runs document() so man/ stays untracked. Rd @param gaps remain. |

## B6 - Performance (§5.6, ranked)

Goal: measured wins, in the report's rank order. Prerequisite: B3/B4 merged and R2-OPS-08 done. Each item records before/after timing and peak RSS on BFA and IRN (including IRN multi-SSP x multi-period), plus the equivalence gate.

| Rank | ID | Eff | Status | Task | Gate | Notes |
|---:|---|---|---|---|---|---|
| 1 | R2-PERF-06 | M | ☐ | Memoise `metric_decomposition` by (run, method, poverty line); stop forcing it from unmounted `metric_curve_plot1/2` | Bit-identical | 11-105 s per method switch |
| 2 | CR-PERF-10 | S | ☐ | `gc()` only when RSS guard trips (`fct_get_weather.R:1864, 1898`) | Bit-identical | Prototype -19%/-35%; +2-11% RSS; re-measure on IRN |
| 3 | R2-PERF-01 | M | ☐ | Shrink Step 2 result (household-constant vectors once, one weather table per scenario, drop `hist_sim_result$svy`); lazy per-scenario read off main thread | Bit-identical | Est. -30-40% |
| 4 | R2-PERF-04 | S-M | ☐ | Poverty-line change recomputes only poverty methods; columnar threshold table; Step 3 baseline arm reuses Step 2 suites | Bit-identical | 60.8 s on 3x3 |
| 5 | CR-PERF-04 | M-L | ☐ | Step 1 weather, LASSO/fit, Step 3, exports as tasks on the mirai singleton with `input_task_button()` | Sync vs worker bit-identical | Also covers CR-SEC-07 |
| 6 | CR-PERF-08 | S | ☐ | Matrix-encoded chart series, binned/subsampled smoother, `bindCache()` | JSON/visual parity | Also R2-BUG-27 (loess series likely not drawn) |
| 7 | R2-PERF-02 | S-M | ☐ | Slim worker snapshot; no duplicate `fit_multi` | Bit-identical | |
| 8 | R2-PERF-13 | S | ☐ | Emit displayed method's partial suite first, rest lazily | Bit-identical | 21% of OLS 3x3 worker time |
| 9 | R2-PERF-10 / CR-PERF-02 | S-M | ☐ | Normalise weather cache keys (Date vs character, year-aligned spans); reuse Step 1 weather for Step 2 historical | Bit-identical | Depends on R2-BUG-01 |
| 10 | R2-PERF-03 | S-M | ☐ | Identity-keyed prep cache storing row indices, bounded by bytes | Bit-identical | Do not enlarge entry count (2.3 GB) |
| 11 | R2-PERF-14 | S | ☐ | Drop `spatial` extension (bbox from `h3_cell_to_lat/lng`) | Bbox parity | 1.5 s per process |
| 12 | CR-PERF-15 | S | – | Tracked in B5 | | |
| 13 | CR-PERF-13 / R2-PERF-08 | S | – | Tracked in B5 | | |
| 14 | R2-PERF-11 | S | ☐ | Memoise LASSO by inputs | Bit-identical | 3.0 s per click |
| 15 | R2-PERF-05, R2-PERF-12 | S | ☐ | Step 2 chart payloads; Diagnostics weather re-reads | Visual | |
| 16 | CR-PERF-03 | L | ☐ | Factorised per-key predictor behind a flag | Pre-agreed tolerance | Agree tolerances first |
| 17 | R2-PERF-09 | S | ☐ | Re-measure fast vs bounded weather collection | Bit-identical | |
| 18 | R2-PERF-07 | S-M | ☐ | `stop_mirai()` on cancel; checkpoints between SQL stages | n/a | |
| 19 | CR-PERF-14 | S | ☐ | One debounced MutationObserver | n/a | |
| 20 | CR-PERF-05 | L | ✗ | Offline weather pre-aggregation with data team | | Out of app scope; would fix CR-BUG-01 by construction |
| – | R2-PERF-03/P5 | | ✗ | `split()` instead of `which()` | | Slower at BFA scale (P5); keep only as memory item |
| – | igraph removal (idea, 2026-10-06) | S-M | ☐ | Replace `igraph::components()` in `loc_panel()` (only use, `R/fct_loc_panel.R`). Benchmarked on local data (edges built exactly as `loc_panel()` does; partition identical to igraph in every case). **Base R is NOT as fast:** igraph 0.8 s vs union-find 7.9 s vs vectorised label propagation 2.5 s on TJK (6.5M edges); 0.2 / 2.4 / 0.5 s on GAB (1.7M); synthetic 200k vertices 0.32 / 0.70 / 0.59 s. **All-in-DuckDB** (edges kept in DuckDB, SQL min-label propagation, collect only id+label; 8-16 passes) is about equal or slightly faster end to end: BFA 0.06-0.16 vs 0.20 s, GAB 1.75-1.80 vs 1.39-1.76 s, MRT 3.4-4.3 vs 4.2-5.3 s, TJK 6.7-7.4 vs 7.3-8.0 s. BFA-size graphs (1.2k vertices, 5.7k edges) are trivial either way. Value is dropping `igraph` (and its libglpk system library), not speed. Keep labels via the existing stable-relabel step and `test-determinism.R:96`. | Dense countries are the real cost: edge lists reach 1.7-6.5M rows and the pair self-join + collect takes 1.2-11 s (TJK 11.3 s cold), far more than the component step. Reducing candidate pairs (for example blocking by cell before the self-join) is the larger win. Scratch scripts were in the session scratchpad only. |
| – | CR-PERF-09 | S | ☐ | List only `microdata/<unit>/` locally | | Low |
| – | CR-PERF-12 | S | ☐ | `Hmisc` -> `collapse::fquantile(w=)`; ranger/xgboost/parsnip to Suggests | | Overlaps R2-OPS-07 |

Rejected experiments not reopened (§11): Arrow fetch path, `csw()` stepwise fits, SQL `ORDER BY` for survey sort, `bw.SJ` subsampling, batched gemm, `shared_period` plan, reference weather as default, shared SSP monthly cache.

## B7 - Remaining Medium and Low bugs (§4.2, §4.3)

Triage each: fix, or mark `✗` with a reason. Group by file to keep diffs small.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| CR-BUG-03 | M | S | ☑ | Compute RIF after the complete-case filter | eb7c6f0: prepare_outcome/RIF after complete-case filter; test-fct_fit_model (synthetic tau 0.9: 0.78 vs 0.58 before). BFA default spec: no rows dropped, zero change. |
| CR-BUG-04 | M | S | ☐ | Weighted targeting quantile; model-card note on unweighted fits (method-owner decision on weighting) | Needs decision |
| CR-BUG-05 | M | M | ☐ | Gap-aware lag windows (`RANGE BETWEEN INTERVAL`); per-variable NA handling | |
| R2-BUG-08 | M | M | ☐ | LASSO: partial out FE (FWL) or sparse factor FE | |
| R2-BUG-09 | M | S | ☑ | `vcov = "hetero"` or relabel "HC1 robust" | 7f157c4: relabelled unclustered SEs as IID (non-robust); vcov unchanged (user decision); test. |
| R2-BUG-10 | M | S | ☑ | Cluster the RIF heterogeneity Wald test; disclose N*K cap | e402f9c: stacked feols clustered on model cluster (else household row id); cap returns NA + note shown on card; tests. |
| R2-BUG-11 | M | S | ☑ | `req()` a successful LASSO when covariates = Lasso | b2735f8: selected_model() lasso_missing attr; fit observer notifies + req(FALSE) (status failure, no hang); has_controls = n_covs > 0; tests. |
| R2-BUG-15 | M | S | ☑ | Do not round threshold table before pivoting | c5f7295: raw values pivoted; .threshold_col_defs() formats display only (4 dp within [-1,1], else 2); CSVs raw; also used by fct_policy_sim_compare.R; test. |
| CR-BUG-16 | M | S | ☑ | Join guards (`relationship`, `unmatched`); report dropped rows | 45ff019: many-to-one/one-to-one join guards + dropped-record messages/notifications (weather join, CPI/PPP, panel id); BFA: 407/14,186 records dropped, now reported; tests. |
| R2-BUG-16 | M | S | ☐ | Align labels ("median" vs mean) and plotting position | |
| R2-BUG-27 | M | S | ☑ | `unname()` loess predictions | 49a4649: unname() loess predictions; htmlwidgets JSON test. |
| CR-BUG-10 | L | S | ☐ | One Gini definition; NA-safe weighted median | Only without weights |
| CR-BUG-11 | L | S | ☑ | Four orphan inputs in `mod_2_02` | f8b564f: orphan inputs replaced by defaults (bw 0.05, p10_p90, scenario_x_year, show_coef FALSE); tests updated. |
| CR-BUG-12 | L | S | ☑ | Namespace `#results_section` | 72a8d9f: results_section container and selectors use ns(); static test. |
| CR-BUG-13 | L | S | ☑ | Keep GCM names in `model_n` | |
| CR-BUG-15 | L | S | ☑ | Move S3 methods out of module closure | 073f290: methods at top level, registered as S3 in NAMESPACE (needed for testServer dispatch); laziness test. |
| CR-BUG-17 | L | S | ☑ | `ORDER BY` before `head(1)` for H3 resolution | 6ebfcb5: .h3_resolution() over all rows, errors on mixed resolutions; test. CMIP6 path still falls back silently to target resolution on error (flag). |
| CR-BUG-18 | L | S | ☑ | Consistent `skip_coef_draws` flag; safe env parsing | |
| CR-BUG-19 | L | S | ◐ | `detectCores()` -> `availableCores()` | mod_1_06 fixed earlier (6ee04f3). Left: fct_get_weather.R:53 detectCores (NA-safe, not container-aware); parallelly installed but undeclared. |
| R2-BUG-18 | L | S | ◐ | `withr::with_seed()`; `tempfile()` ids | 1c0d9db: withr::with_seed jitter in utils_mod_1_helpers.R; test. Left: runif ids in fct_step2_async.R:130, fct_step2_payload.R:403. |
| R2-BUG-19 | L | S | ☑ | RIF coefficient plot: add covariance term | 3cd6bc3: sqrt(w'Vw) from RIF sub-fit VCV (diag fallback); test. Pre-existing: echart_weather_effect_plot RIF poly curve uses x_mean 0 and misses I(I(temp^2)) term (linear only). |
| R2-BUG-20 | L | S | ☑ | NA-safe role-flag comparisons | 1edd698: %in% NA-safe role filters (model_select, fit_model Lasso pool, surveystats); model card role counts; tests. |
| R2-BUG-21 | L | S | ☑ | Weather-load short-circuit must consider outcome | 9037208: outcome in mod_1_05 short-circuit key (re-runs weather load, ~1.5 s warm); test. |
| R2-BUG-22 | L | S | ☑ | Bin ordering regex: handle minus signs | 1098d5f: signed/-Inf bin ordering regex; fixed regmatches misalignment; test. |
| R2-BUG-23 | L | S | ☐ | Pre-2015 period starts; accurate artifact-cap error | |
| R2-BUG-24 | L | S | ☐ | Contrast SD residual variance; fallback error scaling; incidence weights; residual-mode checks | |
| R2-BUG-25 | L | S | ☐ | `batch/02_weather_stats.R:218` signature | Same as RED-06 task |
| CR-PERF-07 | M | M | ☐ | Slim `model_fit` | Overlaps R2-PERF-02 |
| Info: LASSO leakage | – | – | – | Checked, not borne out on real metadata; keep a central exclusion list anyway | |
| CR-BUG-09 | – | – | – | Fixed before this review | |
| CR-BUG-20 | – | – | – | Fixed before this review | |
| CR-PERF-01 | L | – | – | Mostly fixed; leftover dead work <= 0.03 s | |
| CR-PERF-06 | M | – | – | Covered by R2-PERF-03/04 | |

## B8 - Accessibility, WCAG 2.2 AA (§9)

Quick wins first (R2-A11Y-01/03, CR-A11Y-01..04), then the map table alternative and axe in CI (after CI exists in B5).

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| R2-A11Y-03 | M | S | ☑ | `page_navbar(lang = "en")` | |
| CR-A11Y-01 | M | S | ☐ | `:focus-visible` outline on `.wise-info-icon` | |
| R2-A11Y-01 | M | S | ☐ | Restore focus style on sidebar accordion headers | |
| CR-A11Y-02 | M | S | ☐ | Text colours >= 4.5:1 (`#5f6f7d`, status colours, hero text) | |
| CR-A11Y-03 | M | S | ☐ | `mod_1_06` inline red/grey: theme tokens + icon/prefix | |
| CR-A11Y-04 | M | S-M | ☐ | `aria_label` on `pill_toggle`, `wave_toggle_slider`, `label = NULL` inputs | |
| R2-A11Y-02 | M | M | ☐ | Focus ring and selected pill >= 3:1 | |
| CR-A11Y-08 | M | M | ☐ | "View as table" for map; ECharts `aria` | |
| CR-A11Y-09 | M | M | ☐ | axe-core via `shinytest2` in CI | Needs B5 CI |
| CR-A11Y-05 | L-M | S | ☐ | Tooltip role/`aria-describedby`/Escape; map tooltips dismissible | |
| R2-A11Y-04 | L | S | ☐ | Darker Okabe-Ito variants or markers; darker label | |
| R2-A11Y-05 | L | S | ☐ | Skip link, `<main>`, heading levels, `aria-live` status, specific names | |
| R2-A11Y-06 | L | S | ☐ | Pan buttons/keyboard pan; >= 24 px targets; remove dead click input | |
| CR-A11Y-06 | – | – | – | Fixed by bslib 0.12 | Prefer real `<button>` |
| CR-A11Y-07 | – | – | – | Fixed in live UI | Dead `make_regtable()` only |
| (info) alt text | L | S | ☐ | `fct_weatherstats.R:1905, 1912` when `alts` is NULL | |

## B9 - Code quality and docs (§8)

Largest, least urgent; one PR per file with characterisation tests. Delete dead code before splitting files.

| ID | Sev | Eff | Status | Task | Notes |
|---|---|---|---|---|---|
| CR-CQ-08 | L | S | ☑ | Delete dead exports/internals/UI stubs, ~500 unmounted lines in `mod_3_09`; fix `.step2_compute_copy()` warnings | 4e0a678: deleted dead exports/internals/UI stubs (~1,490 lines incl. mod_3_09 437-950, make_regtable, plot_resid_weather, .run_simulation_parallel_chunk); NAMESPACE regenerated. Left (see log): compute_cluster_counts, COEF_VCOV_SPEC_MOULTON, .wise_step2_async_find_stores, .step2_compute_copy, test-only plot helpers. |
| CR-CQ-05 | L | S | ◐ | Remove five defensive `exists()` checks | 6c32786: removed guards in fct_run_simulation.R and fct_rif_sim.R (+ source-scan test). Left: fct_load_data.R:293, fct_policy_decompose.R:703-706, fct_simulations.R:395. |
| CR-CQ-09 | L | S | ☐ | One default data path from config | |
| CR-CQ-10 | L | S | ☐ | Update `AGENTS.md` (test count, deleted dev scripts, residual modes, engines, Step 0 sources, install one-liner, `test_dir`) | Stale refs in `fct_load_data.R:241`, `dev/00_make_manifest.R:16` |
| R2-CQ-01 | L | S | ☐ | Track `man/` or `@noRd`; update `NEWS.md` | |
| CR-CQ-07 | L | M | ☐ | One integer `year` type | |
| CR-CQ-03 | L-M | M | ☐ | One `cachem` helper; `bindCache()` on run-pure outputs | Overlaps R2-PERF-03/04 |
| CR-CQ-04 | M | M | ☐ | Define outputs once at module level (58 in observers) | |
| CR-CQ-01 | M | L | ☐ | Move static builders out; split three giant server functions | |
| CR-CQ-02 | M | L | ☐ | Shared Results-pane module for Steps 2/3 | |
| CR-CQ-06 | – | – | – | Fixed | |

## Decision log (numerical changes)

Record before/after for the BFA headline numbers whenever a batch changes outputs. Baseline values from §4.1 (BFA, OLS unless stated, SSP3-7.0, 2025-35):

| Quantity | Baseline (df37cdb) | After B3 | After B4 | Notes |
|---|---:|---:|---:|---|
| Mean welfare change | -2.52% | -2.84% (CR-BUG-01 only) | | Local re-run (t only, 22 members, year + panel-location FE, point est.): -2.35% -> -2.84%. t + spei6 (15 members): -3.51% -> -3.71%. Baseline def. differs from report, so use local before. |
| Poverty headcount change ($3/day) | +1.70 pp | +1.88 pp (CR-BUG-01 only) | | Local: +1.53 -> +1.88 pp (t only); +2.43 -> +2.54 pp (t + spei6). |
| Median welfare change | -2.59% | -2.94% (CR-BUG-01 only) | | Local: -2.42% -> -2.94% (t only); -3.58% -> -3.72% (t + spei6). |
| RIF historical mean welfare (PPP $/day) | 4.22 (LCU model) | | | PPP model: 4.40 (CR-BUG-02) |
| RIF poverty headcount change | +2.27 pp (LCU model) | | | PPP model: +2.06 pp |
| Median delta SD / Monte Carlo SD | 0.18 | 0.971 | | Local BFA (OLS, FE year + loc_id_panel, 400 draws): 0.011 -> 0.971 (R2-BUG-03). |

## Performance baselines (BFA, report container, medians)

Re-measure locally before B6 and replace these. Source: §1.2, §5.

| Measurement | Report value | Local before | Local after |
|---|---:|---:|---:|
| Step 2 worker, default scenario, warm (OLS) | 34 s | | |
| Step 2 worker, 3x3, warm (OLS / RIF) | 289 / 332 s | | |
| Step 2 peak RSS, 3x3 | 6.8 GB | | |
| Step 2 artifact, 3x3 (OLS / RIF) | 521 MB / 1.03 GB | | |
| Step 2 Results poverty-line change, default / 3x3 | 5.8 / 60.8 s | | |
| Step 3 Run, default / 3x3 | 5.9 / 47.0 s | | |
| Step 3 aggregation-method switch, default / 3x3 | 11.4 / 104.7 s | | |
| Step 1 residual diagnostics (N = 13.4k) | 8.7 s | | |
| App cold start to first response | 7.0 s | | |

## Log

- 2026-10-06 - Tracker created from `REVIEW-2026-10-06.md` (all findings `☐` except items already fixed). No code changed.
- 2026-10-06 - B1 and B5 implemented on `dev` (uncommitted). Verified: new tests (deploy-contract, duckdb-bundle, resource-limits, step2-async-daemon, stage-log, batch-scripts) and the affected existing test files pass; Step 2 benchmark harness smoke-run on local BFA (weather 17.1 s vs pipelines 3.7 s of 21.3 s total). Open: manifest regeneration (needs commit), CR-PERF-15, RED-06 consolidation, default resource-limit values, Azure extension decision.
- 2026-10-06 - Manifest regenerated and committed (b89aa7d). CR-PERF-15 deferred by decision (keep option 1: `load_all()`, git deploy).
- 2026-10-06 - Pushed dev (d2a6991) and redeployed on Posit Connect from the regenerated manifest: deploy succeeded (restore + start). In-app checks on Connect (Step 3 run, stage log lines with rss_mb, auto-connect) still to be confirmed.
- 2026-10-06 - Wave 1 started: five parallel worktree branches off dev (security, weather, delta, levers, cleanup), merged locally into dev after tests. Decisions: R2-BUG-29 -> NA + warning (no floor); R2-BUG-09 -> relabel to IID (no vcov change); R2-BUG-28 -> count/report only; CR-BUG-01/R2-BUG-01/R2-BUG-03 fixed now with BFA before/after.
- 2026-10-06 - Benchmarked igraph vs base-R and all-in-DuckDB connected components for `loc_panel()` (see B6 row "igraph removal"). No code changed.
- 2026-10-06 - fix/rev-w1-levers merged into dev (8610124): CR-BUG-08, R2-BUG-05, R2-BUG-17, R2-BUG-12. Verified: devtools::test(filter='policy|determinism|sp-reach|step3|decompos') 1798 passed, 0 failed. Other wave-1 branches (security, weather, delta, cleanup) resumed after a usage pause.
- 2026-10-06 - fix/rev-w1-weather merged into dev (2b9a386): R2-BUG-01, CR-BUG-01, R2-BUG-29, CR-BUG-17, CR-SEC-04. Agent verified weather test files (get_weather 144, wx_cache 26, perf02 11, determinism 43, weather_select 83, run_simulation 89, step2-compute 36 passed). CR-BUG-01 moves headline numbers (Decision log). Observations: CR-BUG-06 excludes 7/22 SSP3-7.0 members with t + spei6 (warning only); existing get_weather tests write to the real user cache dir.
- 2026-10-06 - fix/rev-w1-delta merged into dev (8af59ff): R2-BUG-03, R2-BUG-02, CR-BUG-07, R2-BUG-28. Follow-ups found: (a) delta gradients multiply by mu even for level (non-log) outcomes - every method affected on level outcomes, needs is_log in gradient_for_method (numerics, needs decision log); (b) NA weights make point estimates NA (drop vs fail: product decision); (c) n_coef_dropped / n_na_dropped not yet shown in UI (fct_aggregation.R / mod_2_02).
- 2026-10-06 - fix/rev-w1-security-dev merged into dev (049739c): CR-SEC-01, R2-SEC-01, R2-SEC-03, CR-SEC-06, R2-SEC-04, R2-SEC-05. Agent verified connection 129, overview 26, overview-metadata 47, step2-async 111, step2-payload 119, step2-compute 36, export-bundle 156, provenance 43, weathersim 66 passed. R2-SEC-02 now more important: UI credentials reach the shared worker during Step 2. Remaining CR-SEC-08 instance: Databricks file errors include host/volume URL.
- 2026-10-06 - fix/rev-w1-cleanup merged into dev (4a953e0): CR-CQ-08, CR-BUG-11/12/15, R2-BUG-15/27/22/09 done; CR-CQ-05, R2-BUG-18, CR-BUG-19 partial (remainders in files owned by other wave-1 agents). devtools::document(): no NAMESPACE drift. Further dead-code candidates (test-only plot helpers, welfare_stats_suite, .wise_navy, okabe-ito scales, unused mod_3_09 args) listed for a later CR-CQ-08 pass.
- 2026-10-06 - Wave 1 integrated on dev (4a953e0). Full suite: 7130 passed, 0 failed, 0 errors (30 'Unknown or uninitialised column' warnings in test-rerun-regressions.R are pre-existing at fb5e4db). devtools::document(): no NAMESPACE drift. Manifest regenerated; test-deploy-contract passes.
- 2026-10-06 - Wave 2 started (branches off dev 7d13c3a): fix/rev-w2-security (R2-SEC-02, local-daemon check, CR-SEC-05, CR-SEC-08 partial, R2-BUG-18 rest, CR-BUG-18, R2-BUG-23, leftovers), fix/rev-w2-step1 (CR-BUG-03, R2-BUG-10/11/19/20/21, CR-BUG-16 report-only), fix/rev-w2-step3 (R2-BUG-14, R2-BUG-13, level-outcome delta gradients, CR-BUG-10, CR-BUG-13, leftovers), fix/rev-w2-a11y (B8 quick wins), fix/rev-w2-weather (CR-BUG-05, CR-PERF-10, R2-PERF-14). Decisions: R2-BUG-14 -> block Step 3 when model changed; R2-BUG-13 -> NA rows untreated + reported; level-outcome gradients -> fix now with MC check; CR-SEC-05 -> drop overview-* ids on import, exports/provenance record source type + origin only.
- 2026-10-06 - Wave 2 mostly blocked: a hook rewrites git to `rtk git` and the worktree-isolation guard refuses it, so security, step3, a11y and weather agents stopped without changes (items back to todo in practice). Step 1 agent finished all 7 findings as patches (plumbing-only git); coordinator applied and committed them on dev (eb7c6f0..45ff019). Agent ran 19+ related test files (0 failures); full suite on dev not run yet (run was denied by the permission classifier).
- 2026-10-06 - Full suite on dev after Step 1 commits (45ff019): 7185 passed, 0 failed, 0 errors. Manifest regenerated. Wave 2 remainder (security, step3, a11y, weather) not started: worktree agents need git; pending user edit of rtk config ([hooks] exclude_commands = ["git"]) since the isolation guard rejects `rtk git`.
