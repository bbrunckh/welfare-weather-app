# Handoff: review remediation (updated 2026-10-06, wave 2 partly blocked)

Status lives in `review/REVIEW-2026-10-06-tracking.md` (see its Log section). This file only covers the coordination state.

## Done
- Wave 1 (security, weather, delta, levers, cleanup) is merged into `dev` and verified: full suite 7130 passed, 0 failed. Manifest regenerated. Commit 7d13c3a.

## Wave 2 status

- Step 1 branch: all 7 findings are committed on dev (eb7c6f0..45ff019). Full suite: 7185 passed, 0 failed.
- The other four branches (security, step3, a11y, weather) were BLOCKED and changed nothing. A hook rewrites git to `rtk git`, and the built-in worktree-isolation guard refuses that. Fix: in `~/Library/Application Support/rtk/config.toml`, set `[hooks] exclude_commands = ["git"]` (user edit; the classifier denied it to Claude). Then re-launch those four with the same scopes, listed below.

### Original wave 2 scopes
Each branch is a worktree branch off 7d13c3a in `.claude/worktrees/agent-*`. Nothing is merged or pushed.

| Branch | Scope |
|---|---|
| fix/rev-w2-security | R2-SEC-02, local-daemon check, CR-SEC-05, CR-SEC-08 (partial), R2-BUG-18 remainder, CR-BUG-18, R2-BUG-23, CR-CQ-05/dead-code leftovers |
| fix/rev-w2-step1 | CR-BUG-03, R2-BUG-10, R2-BUG-11, R2-BUG-19, R2-BUG-20, R2-BUG-21, CR-BUG-16 (report only) |
| fix/rev-w2-step3 | R2-BUG-14 (block), R2-BUG-13 (untreated + report), level-outcome delta gradients, CR-BUG-10, CR-BUG-13, leftovers |
| fix/rev-w2-a11y | R2-A11Y-01/02/03/05 (cheap parts), CR-A11Y-01..04, alt text |
| fix/rev-w2-weather | CR-BUG-05, CR-PERF-10, R2-PERF-14 |

To resume after a pause:
1. Run `git worktree list` and `git log dev..<branch>` for each branch, and check `git status` in each worktree.
2. Merge finished branches with `--no-ff` into `dev`, stashing the tracker first.
3. Run the narrow tests for each branch, then the full suite, `devtools::document()` and `dev/00_make_manifest.R`.
4. Update the tracker and commit.

Overlap to watch when merging: step3 may add a model-identity field where the Step 2 signature is built (in mod_2_01, which the security branch owns). step1 and a11y both edit mod_1_06 and fct_weatherstats.R, but in different hunks.

## Still deferred (need decisions or config)
- CR-BUG-02 and R2-BUG-04 (currency/scale design), R2-BUG-06/07.
- CR-SEC-02 (allowlist config), CR-SEC-03 (secret SCOPE design), CR-SEC-09 (inline JS/CSS move).
- CR-BUG-06 (partial CMIP6 members), CR-BUG-04 (weighting), NA survey weights (drop vs fail).
- CR-BUG-19 (would add parallelly), R2-BUG-08 (LASSO FE), R2-BUG-16 (plotting position), R2-BUG-24, R2-BUG-26.
- Most of B6 performance, CR-A11Y-05/08/09, R2-A11Y-04/06, B9 refactors.
- Ops: rotate the Databricks service-principal secret after the CR-SEC-01 release.
