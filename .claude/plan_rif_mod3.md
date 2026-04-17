# Plan: RIF Quantile Regression — Module 3 (Policy Scenarios)

## Context

Module 3 uses `apply_policy_to_svy()` to modify survey covariates based on policy scenarios, then re-runs the prediction pipeline via `run_policy_pipeline()` → `run_sim_pipeline()`. Currently only infrastructure scenarios are wired (electricity, water, sanitation, health travel time). Social protection, digital, and labor are placeholders.

The original plan proposed an ambitious 8-step analytical decomposition (`simulate_policy_rif()`) with 3-channel separation (main effect / repositioning / interaction). **This is deferred** because:
1. It requires social protection to be wired first (the decomposition is designed for cash transfers + targeting)
2. For infrastructure scenarios (binary covariate flips), the standard re-prediction path already works well
3. Module 2's RIF prediction must be in place first — that's the prerequisite

**Updated approach:** Use the existing pipeline (`apply_policy_to_svy()` → RIF re-prediction) for Module 3. Add an optional distributional impact panel that leverages RIF's quantile-specific predictions.

## Prerequisite

Module 2's RIF prediction path must be implemented first (see `plan_rif_mod2.md`). Once `run_sim_pipeline()` handles `engine = "rif"` via `predict_rif()`, Module 3 works with minimal changes because `run_policy_pipeline()` delegates to `run_sim_pipeline()`.

## Files to Modify

| File | Action |
|------|--------|
| `R/mod_3_05_policy_sim.R` | Pass RIF fields (`fit_multi`, `taus`, `weather_cols`) to `run_policy_pipeline()` |
| `R/fct_policy_sim.R` | Already updated in Module 2 plan (forwarding new params). Optionally add `plot_policy_distributional()` |
| `R/mod_3_06_results.R` | Optionally add distributional impact panel for RIF |

## Step-by-step

### 1. Wire RIF params in `mod_3_05_policy_sim_server` (~5 lines)

In the `run_policy_pipeline()` call (around line 140), add:
```r
res <- run_policy_pipeline(
  hist_sim        = hs,
  saved_scenarios = saved_scenarios(),
  svy_mod         = svy_mod,
  sw              = sw,
  so              = so,
  model           = if (identical(mf$engine, "rif")) NULL else mf$fit3,
  residuals       = if (identical(mf$engine, "rif")) "none" else "normal",
  train_data      = mf$train_data,
  engine          = mf$engine,
  fit_multi       = if (identical(mf$engine, "rif")) mf$fit3 else NULL,
  taus            = mf$taus,
  weather_cols    = mf$weather_terms
)
```

No changes to `apply_policy_to_svy()` — it modifies covariates regardless of engine. Infrastructure scenarios flip binary access variables and cap travel times identically whether downstream model is OLS or RIF.

### 2. Verify existing pipeline works with RIF

With Module 2's prediction path in place:
- `apply_policy_to_svy()` modifies covariates (e.g., sets `electricity = 1` for universal access)
- `run_policy_pipeline()` calls `run_sim_pipeline()` with modified covariates
- `predict_rif()` predicts under new weather + modified covariates, computing deltas from training baseline
- `plot_policy_comparison()` shows baseline vs policy point-range — engine-agnostic
- `policy_input_diagnostics()` compares covariate distributions — engine-agnostic

### 3. Optional: Add distributional impact panel (`R/fct_policy_sim.R`)

```r
plot_policy_distributional <- function(baseline_preds, policy_preds, so, n_quantiles = 10)
```

Groups individual predictions into deciles (by baseline welfare rank) and shows mean policy effect per decile. This is simpler than the full decomposition but captures the key RIF insight: policy impacts vary across the distribution.

- Input: Individual-level `preds` from baseline and policy `hist_sim`
- Group by `ntile(.y_baseline, 10)` or welfare decile
- Compute mean delta per decile: `mean(policy_outcome - baseline_outcome)`
- Plot: bar chart or lollipop, x = decile, y = mean welfare change

### 4. Optional: Render panel in `mod_3_06_results_server`

When `engine == "rif"`, conditionally render the distributional panel below the existing point-range comparison chart. Use `renderPlot` + `conditionalPanel` or `uiOutput` gated on engine type.

## What is explicitly deferred

These belong to a future phase when social protection is wired:

| Deferred item | Reason |
|---|---|
| `simulate_policy_rif()` — 8-step analytical decomposition | Requires cash transfer + targeting mechanics |
| `map_policy_to_rif_args()` — maps scenarios to RIF args | Requires all scenario types to be wired |
| `run_rif_policy_pipeline()` — separate orchestrator | Not needed when re-prediction path works |
| `plot_rif_decomposition_by_decile()` — stacked bar chart | Needs the 3-channel decomposition |
| `plot_rif_weather_protection()` — resilience gap chart | Needs the interaction channel |

When these are revisited, the code structure supports adding a parallel analytical path alongside the re-prediction pipeline, reading `rif_grid` directly for the decomposition.

## Verification

1. Configure infrastructure scenario (e.g., universal electricity) with RIF model → simulation completes
2. Point-range comparison chart renders (baseline vs policy) with plausible values
3. Diagnostics table shows covariate changes correctly
4. Future scenarios produce results with same structure
5. Standard (non-RIF) policy pipeline still works unchanged
6. Optional: distributional panel shows heterogeneous effects across deciles