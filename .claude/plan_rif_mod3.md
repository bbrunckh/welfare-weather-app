# Plan: RIF Quantile Regression — Module 3 (Policy Scenarios)

## Context

Module 3 uses `apply_policy_to_svy()` → `run_policy_pipeline()` → `run_sim_pipeline()`. Only infrastructure scenarios are wired. The approach is: modify covariates, re-predict, compare with baseline.

## Prerequisite

Module 2's RIF prediction path must work first (see `plan_rif_mod2.md`).

## How It Works for RIF (Simple Re-prediction)

Once Module 2's `predict_rif()` is in place, Module 3 works automatically:
1. `apply_policy_to_svy()` modifies covariates (e.g., `electricity = 1`) — engine-agnostic
2. `run_policy_pipeline()` calls `run_sim_pipeline()` with modified `svy_mod`
3. `run_sim_pipeline()` dispatches to `predict_rif()` for RIF engine
4. `predict_rif()` uses `svy_mod` for covariates and baseline weather
5. Comparison charts work as-is (engine-agnostic aggregation)

## Known Limitation: Direct Policy Effect Not Captured

The delta method computes: `delta = predict(scenario_weather, covariates) - predict(base_weather, covariates)`.

For Module 3, both predictions use the SAME (modified) covariates, so the direct policy effect (e.g., β_electricity × Δelectricity) **cancels out**. Only the **interaction** between policy and weather is captured (e.g., does electrification change how temperature affects welfare?).

**Impact:** The Module 3 comparison will show how the policy changes weather vulnerability, but NOT the direct welfare improvement from the policy itself. The direct effect is visible in the diagnostics table (`policy_input_diagnostics`).

**Future fix:** Compute total delta = predict(scenario_weather, modified_covariates) - predict(base_weather, **original** covariates). This requires passing the original svy alongside svy_mod. Deferred until the basic pipeline is validated.

## Files to Modify

| File | Action |
|------|--------|
| `R/mod_3_05_policy_sim.R` | Pass RIF fields to `run_policy_pipeline()` (~5 lines) |
| `R/fct_policy_sim.R` | Already updated in Module 2 plan (forwarding params) |

## Step-by-step

### 1. Wire RIF params in `mod_3_05_policy_sim_server`

```r
is_rif    <- identical(mf$engine, "rif")
residuals <- if (is_rif) "none" else "normal"

res <- run_policy_pipeline(
  ...,
  model        = if (is_rif) NULL else mf$fit3,
  residuals    = residuals,
  engine       = mf$engine,
  fit_multi    = if (is_rif) mf$fit3 else NULL,
  taus         = mf$taus,
  weather_cols = mf$weather_terms
)
```

### 2. No changes to `apply_policy_to_svy()`, `plot_policy_comparison()`, or `policy_input_diagnostics()`

These are all engine-agnostic — they operate on covariates or aggregated outcomes.

## What is Deferred

| Item | Reason |
|---|---|
| Full delta with original + modified covariates | Needs validation of basic pipeline first |
| `simulate_policy_rif()` analytical decomposition | Needs social protection to be wired |
| Distributional impact panel (decile bar chart) | Enhancement after basic pipeline works |
| `plot_rif_decomposition_by_decile()` | Needs decomposition |
| `plot_rif_weather_protection()` | Needs decomposition |

## Verification

1. Infrastructure scenario (e.g., universal electricity) with RIF → simulation completes
2. Point-range comparison chart renders (baseline vs policy)
3. Diagnostics table shows covariate changes correctly
4. Standard (non-RIF) policy pipeline unchanged
5. Note: policy effect in comparison chart will be SMALL (only interaction, not direct effect) — this is the known limitation, not a bug