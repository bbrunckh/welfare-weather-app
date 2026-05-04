# Plan: Efficient Parameter Uncertainty via Cholesky Factor Loading

## Status: IMPLEMENTED (commit pending)

All steps from the original plan have been implemented:

- **Step 0**: `COEF_VCOV_SPEC` changed to `~code + year + survname + loc_id`
- **Step 1a**: `compute_chol_vcov()` added, `draw_coefs()` removed
- **Step 1b**: `aggregate_with_uncertainty()` and `combine_ensemble_results()` added
- **Step 1c**: `run_sim_pipeline()` rewritten with factor-loading approach
- **Step 1d-f**: `aggregate_sim_preds()` kept (still used by some comment refs), `predict_outcome_vectorised()` removed, `beta_override` removed from `predict_outcome()`
- **Step 2**: `mod_2_01_weathersim.R` rewritten to use `compute_chol_vcov()` and store compact `(y_point, F_loading)` per model
- **Step 3**: `mod_2_02_results.R` `agg_hist`/`agg_scenarios` rewritten to call `aggregate_with_uncertainty()` + `combine_ensemble_results()`
- **Step 4**: `enhance_exceedance()` in `fct_sim_compare.R` updated with uncertainty ribbon bands (inner: coefficient uncertainty, outer: model spread)
- **Step 5**: `fct_policy_sim.R` and `fct_policy_sim_compare.R` updated for new schema
- **Step 6**: `mod_2_03_diagnostics.R` updated to use `aggregate_with_uncertainty()`
- **Step 7**: NAMESPACE regenerated, old exports removed

## Remaining (deferred):
- `aggregate_sim_preds()` definition is still in `fct_simulations.R` for backward compat (no active callers in updated code paths)
- Full integration testing via the app UI
- Verification tests (dev/test_factor_loading.R) not yet written
