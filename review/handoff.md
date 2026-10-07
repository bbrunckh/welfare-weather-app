# Handoff: review remediation (updated 2026-10-07, wave 2 integrated)

Status lives in `review/REVIEW-2026-10-06-tracking.md` (Log and Decision log are current). Wave 2 (security, step3, a11y, weather) is complete and merged into dev; full suite 7418 passed, 0 failed. The per-branch notes below are kept as history.

## Next
- Visual check in a browser: app-wide focus ring, darker hero gradient, main landmark wrapper, slider aria JS.
- Ask user before deleting stale branches fix/rev-w1-*, fix/rev-w2-*, worktree-agent-* (incl. probe worktree-agent-aa28cae4b01775578) and the locked worktrees in .claude/worktrees/.
- Decisions pending: drop spatial from the DuckDB bundle; the deferred list below.
- Follow-ups: R2-BUG-14 seed in fit signature (mod_1_07); R2-BUG-13 end-to-end check; CR-SEC-08 remaining display sites; R2-A11Y-05 untitled popovers.

## Done
- Wave 1 and wave-2 Step 1 are merged on `dev`. R2-A11Y-03, CR-BUG-13 and CR-BUG-18 were committed directly on dev (aced553..a183e4a).
- Full suite on dev a183e4a: 7185 passed, 0 failed, 0 errors.
- The rtk hook fix works: worktree agents can run git (`[hooks] exclude_commands = ["git"]`).

## Wave 2 status (paused for usage; nothing merged or pushed)

All four branches are cut from dev a183e4a. The worktrees live in `.claude/worktrees/agent-*`, are locked, and some hold uncommitted work. Agent rules are in the session scratchpad `common.md`; the text is repeated in the "Agent rules" section below.

### fix/rev-w2-security (worktree agent-a5f5a7ad5ecf57f0c)
Committed and narrow-tested:
- 792efe2 R2-BUG-18 rest: async and lease ids from `basename(tempfile())`, so the global RNG is untouched.
- 57040da CR-CQ-05: removed the guard at `fct_load_data.R` ~293.
- ea8d9e6 CR-CQ-08: removed `.step2_compute_copy()` and `.wise_step2_async_find_stores()`.
- 3fb783b CR-SEC-04: Step 2 base cache dir is created with mode 0700.
- e3a5a9c R2-BUG-23: period sliders start at 2015 and pre-2015 periods are excluded. A Step 2 result over 2 GB now gives a clear error.
- cbfd3f5 R2-SEC-01: the local-daemon check refuses to send UI credentials to non-local mirai daemons (Step 2 and Overview metadata).

Uncommitted: R2-SEC-02 is partial. `R/fct_load_data.R` has an untested `.duck_drop_credentials()`. To finish it:
1. Add `clear_credentials = FALSE` to `step2_async_worker()` and `load_overview_metadata()`, with `on.exit(.duck_drop_credentials(), add = TRUE)` when the flag is TRUE.
2. In Step 2 dispatch, pass `identical(cp$origin, "ui") && !.wise_step2_async_sync()`. Never clear in sync mode, because that would break the main session's lazy tables.
3. In the metadata `try_mirai`, pass `identical(params$origin, "ui")`.
4. Add tests.

Not started:
- CR-SEC-05: drop `^overview-` ids on config import, fix the import success message, and record only type and origin in exports and provenance.
- CR-SEC-08: `wise_user_error()`. Error paths to cover: the Databricks file error, which still shows the host and volume URL, and the Step 2 async error display.

Notes:
- The `try_mirai` body in `fct_overview_metadata.R` is under-indented by one level (cosmetic).
- The agent reported that one `git add` was rejected by a permission prompt and that it re-ran the same command.

### fix/rev-w2-step3 (worktree agent-a3d3188e8ebbe8eed)
Committed (worktree clean):
- 360fb45 R2-BUG-03 (level-outcome delta gradients): `gradient_for_method(is_log=)`; log outcomes bit-identical (hash) for all 9 methods. Delta SD / MC SD, level outcome:
  - Synthetic (N 3000, 4000 draws): mean 40.5 -> 1.007, median 40.3 -> 1.007, headcount 12.5 -> 1.001, gap 23.2 -> 1.001, fgt2 18.2 -> 0.985, gini 29.6 -> 1.002, prosperity gap 17.2 -> 0.950, avg poverty 28.8 -> 0.984.
  - BFA (real welfare rescaled, real weights, synthetic loading): mean 3.52 -> 1.012, median 2.35 -> 1.013, headcount 3.00 -> 1.009, gap 1.79 -> 1.001, fgt2 1.49 -> 0.995, gini 3.53 -> 0.987, prosperity gap / avg poverty 1.22 -> 0.873 (1/welfare curvature).
- 553f07e CR-BUG-10: one Gini definition (weighted covariance form) in the resolver, the C++ kernel (`src/welfare_stats.cpp`, needs recompile) and `aggregate_outcome`. The weighted median drops NA-welfare rows.
  - Unweighted Gini was the correct value + 1/n: BFA 0.408984 -> 0.408845; synthetic 0.44232 -> 0.39232.
  - Weighted Gini unchanged (BFA 0.373913).
  - Weighted median with an NA row: 2.875 -> 2.209.
  - NA weights are still poisoned (deferred).
- bce425f R2-BUG-14: Step 3 is blocked when `model_fit()$.sig` differs from `hist_sim$.sig$fit_sig`. mod_2_01 is untouched. Gap: the seed is not in the fit signature, so a ranger/xgboost refit after a seed-only change is not caught.
- 14121f1 R2-BUG-13: NA outcome/lever/transfer rows get zero deltas. `n_na_untreated` is returned and shown as a notification. Before: decomposition errored ("Canonical main channel ... nonfinite"). After: ok, other rows equal to the clean run. Only the decomposition path was checked, not a full end-to-end Step 3 with NA welfare. The wide filter was not re-run after the final names fix.

Not started:
- CR-CQ-05: guards remain in `fct_policy_decompose.R` (~715-725) and `fct_simulations.R` (~395).
- CR-CQ-08: `compute_cluster_counts`, `COEF_VCOV_SPEC_MOULTON`, the `key_workers` argument and unused mod_3_09 arguments are not yet deleted.

### fix/rev-w2-a11y (worktree agent-a95d44f13662dc030)
Committed:
- 21174b8 CR-A11Y-01
- d308089 R2-A11Y-01
- 2d5db35 R2-A11Y-02: an app-wide solid 2px focus ring via bs_theme Sass vars; selected pill at 4.62:1.
- d59961b CR-A11Y-02: muted text `#5f6f7d` at 5.18:1. The hero gradient end changes `#0071bc` -> `#005a96`, which is a visible change. Full table in the agent report: status colours `#00843d` / `#d4192b`.
- 8dfc791 CR-A11Y-03: `mod_1_06` prerequisites use the amber `no_data_warning()` with an icon.

New test file: `test-a11y-contract.R`.

Uncommitted and untested: CR-A11Y-04 is partial. `pill_toggle(aria_label=)` is in `utils_ui.R`, and call sites are updated in mod_2_02, mod_2_03, mod_3_01, mod_1_03, mod_1_05, mod_0_overview and fct_policy_sim_compare. Left to do:
- (a) Give `mod_3_01` `targeting`/`transfer_n_payments` a visually-hidden label.
- (b) Add a `custom.js` hook so `.irs-line` gets role slider plus `aria-labelledby` (this also affects `mod_3_04`).
- (c) Add a `pill_toggle` test and run the filters for the touched modules.

Not started: R2-A11Y-05 and the alt-text item (`fct_weatherstats.R` ~1905/1912).

Do a visual check after merge: the focus ring and the hero colour.

### fix/rev-w2-weather (worktree agent-afa31635595a0fe60)
Committed:
- 968776d R2-PERF-14: bbox comes from `h3_cell_to_boundary_wkt()` and `spatial` is no longer loaded. Parity over 72 files and 1.82M cells: identical, max diff 0. Saves 2.73 s INSTALL+LOAD per process on a cold extension dir (0.05 s / 18 MB when cached). The bundle is left as is: spatial .gz (27.6 MB), the SHA pin, `.core_extensions` and the manifest. Dropping it is a separate decision; the pin and manifest must change together.
- c6ab54f CR-PERF-10: `gc()` runs only when the RSS guard trips. Results are bit-identical: hashes match for BFA hist/1x1/3x3 and IRN hist/1x1.

CR-PERF-10 timings and peak RSS, before -> after:

| Case | Time | Peak RSS |
|---|---|---|
| BFA 1x1 | 12.5 -> 11.8 s | 2.45 -> 2.19 GB |
| BFA 3x3 | 110 -> 97 s (-12%) | 7.2-7.4 -> 5.7-7.1 GB |
| IRN 1x1 | 29.2 -> 30.7 s | 5.59 -> 5.25 GB |
| IRN 3x3 | 294/286 -> 317/305 s (+6%, other agents were loading the machine) | 7.8/8.8 -> 6.5/7.9 GB; hash identical |

The default 4096 MB `WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB` is what limits the gain.

Uncommitted: only the failing regression test for CR-BUG-05, in `test-fct_get_weather.R`. Steps to finish:
1. In both rolling windows in `fct_get_weather.R` (~1391 historical, ~1543 climate), switch to `ORDER BY (YEAR*12+MONTH) RANGE BETWEEN a PRECEDING AND b PRECEDING`.
2. Remove the `if_all(!is.na)` raw-weather filter (~1249).
3. Bump `WISEAPP_WX_LOC_CACHE_VERSION` and `STEP2_PREPARED_WEATHER_CACHE_VERSION` to v2. The prepared-weather cache version lives in `fct_step2_payload.R` (owned by the security batch), and `test-step2-payload.R` lines 579 and 609 hardcode v1.
4. Diff changed rows against the baselines in the session scratchpad `out/` (`base-r1_*.rds`, about 4 GB). These sit under /private/tmp and may be cleaned; if they are gone, regenerate them with `bench_wx.R`.

## Resume plan
1. For each worktree: run `git -C .claude/worktrees/agent-<id> status --short` and `git log --oneline dev..<branch>`.
2. Finish the uncommitted items: resume each agent, or launch a new worktree agent with `git switch <branch>`. Do not commit the half-done work as it stands.
3. Integrate by merging each branch into dev with `--no-ff`. Expected overlaps:
   - security and weather both touch `fct_step2_payload.R` (the CR-BUG-05 cache version).
   - security and step3 both touch the Step 2 signature in `mod_2_01`.
   - a11y's CR-A11Y-04 touches many modules.
4. Run `devtools::document()`, then the full `devtools::test()`. Commit, then run `dev/00_make_manifest.R`.
5. Update the tracker (status, SHAs, decision-log numbers from the agent reports above) and commit.
6. Ask the user before deleting the stale branches `fix/rev-w1-*`, `fix/rev-w2-*`, `worktree-agent-*` (including the probe `worktree-agent-aa28cae4b01775578`) and the locked worktrees.

## Agent rules
- Each agent runs `git switch -c <branch> refs/heads/dev` first.
- One commit per finding ID, with message `<ID>: <summary>`. ASCII only. No attribution lines. Never push. Agents do not edit the tracker or this file.
- Do not circumvent guards. Run narrow tests per finding and add regression tests. Never read .Renviron or credentials.

## Decisions already made (do not re-ask)
- R2-BUG-14: block Step 3 until Step 2 is rerun.
- R2-BUG-13: treat NA rows as untreated and report the count.
- Level-outcome gradients: fix now, with a Monte Carlo check.
- CR-SEC-05: drop `overview-*` ids on import; exports and provenance record only type and origin.
- Numerics: log before/after in the tracker decision log.

## Still deferred (need decisions or config)
- CR-BUG-02 and R2-BUG-04 (currency/scale design), R2-BUG-06/07.
- CR-SEC-02 (allowlist config), CR-SEC-03 (secret SCOPE design), CR-SEC-09 (inline JS/CSS move).
- CR-BUG-06 (partial CMIP6 members), CR-BUG-04 (weighting), NA survey weights (drop vs fail).
- CR-BUG-19 (would add parallelly), R2-BUG-08 (LASSO FE), R2-BUG-16 (plotting position), R2-BUG-24, R2-BUG-26.
- Most of B6 performance, CR-A11Y-05/08/09, R2-A11Y-04/06, B9 refactors.
- New: whether to drop the spatial extension from the DuckDB bundle (R2-PERF-14 follow-up).
- Ops: rotate the Databricks service-principal secret after the CR-SEC-01 release.
