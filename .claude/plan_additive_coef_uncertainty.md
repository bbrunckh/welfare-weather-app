# Additive-Decomposition SE for Coefficient Uncertainty (default under `residuals = "original"`)

## Context

WISE-APP's delta-method aggregator currently propagates the **full** non-FE coefficient covariance into the standard error of every counterfactual aggregate, regardless of which coefficients are actually "active" in moving the prediction. Under the default residuals mode `"original"`, each household is anchored to its own training residual `ε̂_i = log y_i − x_i'β̂ − w_i'γ̂`, held fixed across coefficient draws ("Option A" in [`.claude/method_uncertainty.md`](.claude/method_uncertainty.md#L46) §1.2).

Under self-consistent residual recomputation (`ε̂_i*` updated alongside `β̂*`) and additive separability of the linear predictor, β-uncertainty on **non-weather covariates cancels exactly through the residual term**. Only the weather slice of the coefficient vector (direct weather effects + interactions involving weather) contributes to the variance of the counterfactual prediction (level *or* contrast, on log scale or after `exp` back-transform).

The current implementation departs from this analytic identity by holding the residual fixed at its point estimate while letting all of β̂ vary, which is conservative but inconsistent with the model's own additive-separability assumption.

**Goal:** make the default behaviour under `residuals = "original"` reflect this cancellation — only coefficients on variables that *actually change between baseline and counterfactual* contribute to `var_coef`. In Module 2 (weather simulation) that's the weather variables and their interactions. In Module 3 (policy simulation) it's the union of weather variables and the policy-modified variables (e.g. `electricity`, `internet`, `employed`, `imp_wat_rec`, …) plus all their interactions. Expose a new checkbox to recover the legacy (full-propagation) behaviour. Apply symmetrically to linear (fixest) and RIF (fixest_multi) engines. Update method documentation and on-screen captions to describe the new interpretation.

**Secondary benefit (speed):** under the default mask, `F_loading` is subset from N × K down to N × K_active, so the dominant `crossprod(F_loading, h)` step in the aggregator runs faster by a factor of K / K_active (typically 2–5×). Aggregation across many scenario × model × year combinations is the hot path in Module 2 and Module 3, so this is meaningful.

## Approach

**Unified auto-detection.** Rather than maintain two separate "active variable" lists (weather for Module 2, weather + policy diffs for Module 3), derive the active set *automatically* by comparing the counterfactual survey to the training survey column-by-column. A column is "active" iff at least one household-level value differs. This single rule covers Module 2 (weather substitution), Module 3 (policy modifications), Module 3 with a weather scenario (both move), and any future scenario type without code changes. Edge case: a zero-intensity policy scenario produces no active variables → mask collapses to the Module 2 mask automatically.

Concretely: build a length-K logical **`active_mask`** once per simulation run (TRUE = keep this coefficient's contribution to `var_coef`). `active_terms = names of columns where survey_counterfactual differs from survey_training`, excluding outcome, weights, metadata, FE columns, and `SP_TRANSFER_COL` (which is added post-prediction outside the regression). Attach `active_mask` to `chol_obj` so it flows transparently through every downstream consumer.

**Subsetting (faster than zeroing).** Inside `compute_factor_loading()` (linear) and `interpolate_F_loading()` (RIF), **subset** `F_loading` to its active columns: `F_loading <- F_loading[, active_mask, drop = FALSE]`. The aggregator's `F_agg = crossprod(F_loading, h)` then operates on a K_active-column matrix, yielding a K/K_active speedup over the legacy full path and zero downstream changes (the aggregator never indexes F_loading by coefficient name). Paired-contrast SE `||F_agg_scn − F_agg_base||` is preserved because both arms subset to the same columns (same `chol_obj`, same mask).

The cancellation argument generalises cleanly: a coefficient `β_j` on covariate `x_j` that is held *identical* between baseline and counterfactual cancels through the held-fixed residual, regardless of whether `x_j` is a non-weather control in Module 2 or an un-targeted control in Module 3. Only the coefficients on the variables the scenario actually moves remain active in the SE.

Gating rules for whether the mask is **constructed** (i.e., applied):

| Pipeline | `residuals` | `propagate_all_covariate_uncertainty` | Mask basis |
|---|---|---|---|
| Module 2 | `"original"` | FALSE (default) | weather coefficients only |
| Module 3 | `"original"` | FALSE (default) | weather **+** policy-modified-variable coefficients |
| any | `"original"` | TRUE | none — legacy (full β) |
| any | `"resample"` / `"normal"` / `"none"` | any | none — legacy (full β); cancellation argument doesn't hold |

The cancellation argument is specific to fixed-per-household residuals; under stochastic residual draws the inactive-coefficient uncertainty does not cancel.

## Critical files

- [R/fct_run_simulation.R](R/fct_run_simulation.R) — orchestrator; build `active_mask` (weather-only for Module 2), attach to `chol_obj`
- [R/fct_simulations.R:216,284](R/fct_simulations.R#L216) — `compute_chol_vcov()`, `compute_factor_loading()`
- [R/fct_rif_sim.R:120,238](R/fct_rif_sim.R#L120) — `predict_rif()`, `interpolate_F_loading()`
- [R/fct_results.R:66](R/fct_results.R#L66) — reuse `weather_coef_names()` for mask construction
- [R/fct_fit_model.R:776](R/fct_fit_model.R#L776) — source of `weather_terms`
- [R/fct_policy_sim.R](R/fct_policy_sim.R) — `resimulate_with_svy()` must forward the flag, detect policy-modified columns by diffing baseline vs. modified survey, and **rebuild** the active mask with the policy slice added before re-running the simulation pipeline
- [R/fct_policy_decompose.R:33-55](R/fct_policy_decompose.R#L33) — `.compute_policy_deltas()` already diffs survey columns (excluding weather, outcome, weights, metadata); reuse its column-detection logic to identify policy-modified variables
- [R/mod_2_01_weathersim.R:31,151-180,516,579](R/mod_2_01_weathersim.R#L151-L180) — new UI checkbox + plumbing
- [R/mod_2_simulation.R](R/mod_2_simulation.R) and [R/mod_3_scenario.R](R/mod_3_scenario.R), [R/mod_3_05_policy_sim.R:50,113](R/mod_3_05_policy_sim.R#L50) — forward reactive
- [R/mod_2_02_results.R:165-191](R/mod_2_02_results.R#L165) and [R/mod_3_06_results.R](R/mod_3_06_results.R) — caption updates
- [R/fct_sim_compare.R](R/fct_sim_compare.R), [R/fct_policy_sim_compare.R](R/fct_policy_sim_compare.R) — band-label strings
- [.claude/method_uncertainty.md](.claude/method_uncertainty.md) — §1.1, §6, §7 updates

## Implementation steps

### 1. Mask construction

Add two new internal helpers near [fct_results.R:66](R/fct_results.R#L66):

```
detect_active_terms(svy_cf, svy_train, exclude_cols) → character vector
  Returns names of columns in svy_cf whose household-level values differ
  from svy_train, excluding outcome, weights, FE columns, SP_TRANSFER_COL,
  and any metadata columns in exclude_cols.

build_active_coef_mask(coef_names, active_terms) → logical vector of length K
  Returns TRUE for every coefficient whose name involves a term in
  active_terms (via word-boundary regex matching, same as weather_coef_names).
```

**Unified construction point.** In `run_sim_pipeline()` at [fct_simulations.R:407](R/fct_simulations.R#L407), immediately before calling `compute_factor_loading()` (or `predict_rif()` for RIF), where both `survey_wd_sim` (counterfactual, with whatever scenario substitutions have been applied) and the training data are in scope, compute:

```
active_terms <- detect_active_terms(survey_wd_sim, train_data, exclude_cols)
chol_obj$active_mask <- build_active_coef_mask(names(chol_obj$beta), active_terms)
```

This runs **only if** `residuals == "original" && !propagate_all_covariate_uncertainty`; otherwise leave the mask absent (legacy full propagation).

This single hook handles both modules:
- **Module 2**: `survey_wd_sim` has weather columns substituted with scenario values; the diff against `train_data` returns the weather columns.
- **Module 3**: `survey_wd_sim` has been passed through `apply_policy_to_svy()` (by `resimulate_with_svy()` at [fct_policy_sim.R:582-703](R/fct_policy_sim.R#L582)) before reaching `run_sim_pipeline()`, so the diff returns weather columns + policy-modified columns (e.g., `electricity`, `internet`, `employed`). No special-case code needed in the policy module — the auto-detection picks it up because the data has already been mutated.

For RIF, the same logic runs but attaches the mask via `attr(chol_obj, "active_mask")` (list-of-tau structure); column-name consistency across tau is validated once at attach time.

The SP transfer column `.wiseapp_sp_transfer` is in the exclude list because it is added post-prediction (outside the regression) and has no coefficient in the model — it carries no β-uncertainty.

Defensive guards in `build_active_coef_mask()`:
- empty `active_terms` → return NULL with `warning("[build_active_coef_mask] no active terms; falling back to full propagation")` — happens naturally for zero-intensity scenarios and degenerates to legacy behaviour
- explicit `mask["(Intercept)"] <- FALSE` if intercept appears in coef names
- **binned weather / factor variable verification**: confirm fixest produces coef names like `weatherVar::level` (with `::` separator) so the `\bweatherVar\b` boundary matches. If binning or factor expansion produces `weatherVar_bin1` (underscore is a word character, boundary fails), extend the regex to `(^|[^A-Za-z0-9_])weatherVar([^A-Za-z0-9_]|$)` *or* augment by checking `startsWith(coef_name, paste0(term, "_"))`. Decide at implementation time by inspecting a real fit with binned weather **and** a labor-market policy fit with factor employment status.

### 2. Mask application — subset, don't zero

**Linear path** — extend `compute_factor_loading()` at [fct_simulations.R:284](R/fct_simulations.R#L284):

```
F <- X_nonFE %*% chol_obj$L          # N × K
if (!is.null(chol_obj$active_mask))
  F <- F[, chol_obj$active_mask, drop = FALSE]   # N × K_active
F
```

**RIF path** — extend `interpolate_F_loading()` at [fct_rif_sim.R:238](R/fct_rif_sim.R#L238) with a new `active_mask = NULL` parameter; subset each `F_cache[[k]] <- X_diff_list[[k]] %*% t(chol_list[[k]])` to its active columns (line 254-256) before the interpolation blend. Caller in `predict_rif()` reads `attr(chol_list_attr, "active_mask")` and forwards.

**Why subset over zero.** Zeroing produces an N × K matrix with many zero columns; the downstream `crossprod(F_loading, h)` still walks every entry and pays the full O(N · K) cost. Subsetting drops the inactive columns from F_loading entirely, so the crossprod is O(N · K_active). Same math (zero entries of `F_agg` are now simply absent), measurable speedup in the typical case where K_active is a small fraction of K (e.g. 5 weather coefs out of 25 total).

The aggregator at [fct_aggregation_delta.R:54-125](R/fct_aggregation_delta.R#L54) is **untouched** — it consumes `F_loading` verbatim and never indexes it by coefficient name. Paired contrasts inherit masking automatically because both arms run through the same masked F_loading construction (`F_agg_scn` and `F_agg_base` are both K_active-vectors, so their difference and norm are well-defined).

### 3. UI

In [mod_2_01_weathersim.R:151-180](R/mod_2_01_weathersim.R#L151) (existing "Model coefficient uncertainty" section), insert immediately after the master `include_coef_uncertainty` checkbox (line 31-35) and wrap in a `conditionalPanel` driven by the master:

```r
shiny::conditionalPanel(
  condition = sprintf("input['%s'] == true", ns("include_coef_uncertainty")),
  shiny::checkboxInput(
    inputId = ns("propagate_all_covariate_uncertainty"),
    label   = "Include uncertainty on all covariates",
    value   = FALSE
  ),
  shiny::helpText(
    "By default (unchecked), only coefficients on variables that change ",
    "between baseline and counterfactual contribute to the reported SE: ",
    "weather variables and their interactions in Step 2; weather plus ",
    "the policy-modified variables and their interactions in Step 3. ",
    "This reflects the fact that under 'original' residuals, uncertainty ",
    "on the unchanged covariates cancels through the held-fixed residual ",
    "term (additive-decomposition SE). Check to propagate uncertainty ",
    "from all covariates — more conservative, but inconsistent with the ",
    "model's own additive-separability assumption. Has no effect when ",
    "residuals are not 'original'.",
    style = "font-size:11px;"
  )
)
```

### 4. Reactive plumbing

Forward `isTRUE(input$propagate_all_covariate_uncertainty)` along the existing path that already carries `skip_coef_draws`:

- [mod_2_01_weathersim.R:516](R/mod_2_01_weathersim.R#L516) and `:579` — add to `fct_run_simulation()` call and exposed return list
- [mod_2_simulation.R](R/mod_2_simulation.R) — forward to `mod_2_02_results_server()` and re-export
- [app_server.R](R/app_server.R) — forward into `mod_3_scenario_server()`
- [mod_3_scenario.R](R/mod_3_scenario.R) → [mod_3_05_policy_sim.R:50,113](R/mod_3_05_policy_sim.R#L50) — accept reactive, stamp onto `hist_sim_baseline$propagate_all_covariate_uncertainty`
- [fct_policy_sim.R](R/fct_policy_sim.R) `resimulate_with_svy()` — read the flag from `hist_sim_baseline` and forward to `run_sim_pipeline()`. **No special policy-mask logic needed here** — because the survey has already been mutated by `apply_policy_to_svy()` before `run_sim_pipeline()` runs, the auto-detection in step 1 picks up the policy-modified columns automatically.

`fct_run_simulation()` and `run_sim_pipeline()` gain a new parameter `propagate_all_covariate_uncertainty = FALSE`. `predict_rif()` needs no new parameter — it reads `attr(chol_obj, "active_mask")` directly.

### 5. Caption / documentation updates

- [mod_2_02_results.R:165-191](R/mod_2_02_results.R#L165) — innermost-whisker caption: append "*Restricted to weather and weather-interaction coefficients under the default ('original') residuals; see Step 2 settings to widen.*"
- [mod_3_06_results.R](R/mod_3_06_results.R) — parallel caption, but worded for the policy context: "*Restricted to coefficients on weather, the policy-modified variables, and their interactions, under the default ('original') residuals; see Step 2 settings to widen.*"
- [fct_sim_compare.R](R/fct_sim_compare.R) `plot_pointrange_climate()` — update the inner-band label hint (weather-only)
- [fct_policy_sim_compare.R:1339-1345](R/fct_policy_sim_compare.R#L1339) — update inner-band label (weather + policy-modified)
- [.claude/method_uncertainty.md](.claude/method_uncertainty.md):
  - §1.1 — describe the new default and the cancellation derivation; cite Option A from §1.2. State explicitly that the "active" coefficient set is (weather + weather-interactions) for Module 2 and (weather + policy-modified + their interactions) for Module 3.
  - §5 (Paired Counterfactuals) — note that the active mask sharpens both the level SE *and* the contrast SE by the same construction.
  - §6 — note in the layered-display section that the innermost whisker now reflects active-coefficient-only uncertainty by default, with module-specific wording for Module 2 vs. Module 3.
  - §7 — add a row to the assumptions table: "Coefficient (default)" / "Mask inactive columns of F_loading" / "Additive separability of active variables in linear predictor; `residuals = 'original'`; inactive variables held identical between baseline and counterfactual"
  - New short §1.1.1 deriving the cancellation: `log ŷ_i^cf* = log y_i + (x_i^cf − x_i)'β̂_active*` under self-consistent residuals (where `x` ranges over the active variables only), or equivalently after F_loading masking. Use the same notation as the existing §2.1 setup.

## Verification

1. **Unit tests** (new `tests/testthat/test-weather-mask.R`):
   - `build_active_coef_mask(c("(Intercept)","tx","I(tx^2)","tx:urban","urban","log_age"), "tx")` returns `c(F,T,T,T,F,F)`.
   - Module 3 fixture: `build_active_coef_mask(c("(Intercept)","tx","tx:electricity","electricity","internet","log_age"), c("tx","electricity"))` returns `c(F,T,T,T,F,F)` — confirms policy variable AND its weather interaction are both kept.
   - Empty `weather_terms` returns NULL with warning.
   - Binned-weather coef names (capture from a real fit fixture) are correctly classified.

2. **Mathematical equivalence test:** for a fixture with mixed covariates, compute `var_coef` two ways — (a) mask F_loading columns then `||F'h||^2`; (b) compute unmasked `F_agg = F'h` then sum only the weather entries squared. Assert equal to machine precision.

3. **Degenerate-fit regression test:** a fit with *only* weather covariates → mask-on and mask-off must yield byte-identical `value_lo` / `value_hi`. Mirror on RIF.

4. **Manual end-to-end:**
   - Run `wiseapp::run_app()`, load a small country dataset, fit a model with non-weather covariates (e.g., urban, household size) and at least one policy-targetable variable (e.g., `electricity`).
   - In Step 2, leave `include_coef_uncertainty = TRUE` and `propagate_all_covariate_uncertainty = FALSE` (default). Note the innermost-whisker width in the results pane and the variance-contribution bar in Diagnostics.
   - Toggle `propagate_all_covariate_uncertainty = TRUE`, re-run. Confirm the coefficient band widens (var_coef increases monotonically).
   - Switch `residuals` to `"normal"`. Confirm the band is identical with the new checkbox in either state (mask not applied).
   - **Module 3 step:** run an infrastructure scenario that flips `electricity`. With default mask, confirm the Module 3 coefficient band is *wider* than the Module 2 band (because `electricity` and its weather interactions are now in the active set) but *narrower* than the legacy full-propagation band. Toggle full-propagation on and confirm Module 3 band matches Module 2 full-propagation band.
   - **Module 3 with zero-effect policy:** run a policy scenario where no variables actually change (e.g., 0% intensity). Confirm Module 3 coefficient band collapses to the Module 2 width.

5. **Contrast invariance snapshot:** add a snapshot test verifying that `F_agg_scn − F_agg_base` produces a zero entry at every inactive column when the mask is applied on both arms (both for the Module 2 weather contrast and the Module 3 policy contrast).

6. **Speed benchmark:** wrap a representative simulation in `system.time()` with the default mask vs `propagate_all_covariate_uncertainty = TRUE` (legacy full F_loading). Expect a measurable wall-clock improvement on the aggregator stage proportional to K / K_active. Capture before/after numbers in the PR description.

7. **Package check:** `devtools::document()` (for any new exported helpers) and `devtools::check()` clean.
