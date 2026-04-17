# Plan: RIF Quantile Regression — Module 2 (Simulation)

## Context

Module 1 RIF is implemented: `fit_model()` returns `engine = "rif"`, `rif_grid` (tidy coefficients across 9 quantiles × 3 models), `taus`, and `fit3` as a `fixest_multi` (9 sub-models). Module 2 currently has no RIF handling — `predict_outcome()` would fail on a `fixest_multi` object. This plan adds a delta-method prediction path that leverages `stats::predict()` on individual fixest sub-models to properly handle polynomials, interactions, and absorbed fixed effects.

## Prediction Approach: Delta Method

For each household `i` at baseline quantile position `tau_i`:
1. `pred_new(tau_i)` = RIF prediction under scenario weather
2. `pred_base(tau_i)` = RIF prediction under training weather (same covariates)
3. `delta_i = pred_new(tau_i) - pred_base(tau_i)` = isolated weather effect
4. `y_sim_i = y_baseline_i + delta_i`

This approach:
- Uses `stats::predict()` on individual fixest sub-models, so polynomials/interactions/FEs are handled automatically
- Cancels out fixed effects (present in both pred_new and pred_base)
- Works on the log scale if outcome is log-transformed (back-transform handles it)
- Captures distributional heterogeneity natively — no residual simulation needed

## Files to Modify

| File | Action |
|------|--------|
| `R/fct_rif_sim.R` | Add `predict_rif()` |
| `R/fct_simulations.R` | Add RIF dispatch + new params in `run_sim_pipeline()` |
| `R/fct_policy_sim.R` | Forward new params in `run_policy_pipeline()` |
| `R/mod_2_01_weathersim.R` | Pass RIF fields; conditional residual UI |
| `R/mod_3_05_policy_sim.R` | Pass RIF fields |

## Step-by-step

### 1. Add `predict_rif()` to `R/fct_rif_sim.R`

```r
predict_rif <- function(fit_multi, newdata, train_data, taus, outcome, weather_cols)
```

**Parameters:**
- `fit_multi`: `fixest_multi` (`mf$fit3`) — 9 sub-models
- `newdata`: simulation data from `prepare_hist_weather()` (has scenario weather + survey covariates)
- `train_data`: `mf$train_data` (has original weather + outcome + RIF columns)
- `taus`: `c(0.1, 0.2, ..., 0.9)`
- `outcome`: name of outcome column (e.g., `"welfare"`)
- `weather_cols`: `mf$weather_terms` — raw weather variable names

**Algorithm:**
1. **Join keys:** `hh_keys <- intersect(c("code", "year", "survname", "loc_id", "int_month"), names(newdata))`
2. **Recover baseline outcome:** Left-join `unique(train_data[, c(hh_keys, outcome)])` onto newdata → `.y_baseline`
3. **Assign tau_i:** `F_hat <- ecdf(train_data[[outcome]])` → `tau_i <- pmin(pmax(F_hat(.y_baseline), min(taus)), max(taus))`
4. **Build baseline_data:** Copy newdata, replace `weather_cols` with training values via lookup join
5. **Predict at each tau:** For k in 1:9, `delta_mat[, k] <- predict(fit_multi[[k]], newdata) - predict(fit_multi[[k]], baseline_data)`
6. **Interpolate delta at tau_i:** `delta_i <- vapply(seq_len(nrow), \(i) approx(taus, delta_mat[i,], xout=tau_i[i])$y, numeric(1))`
7. **Assemble:** Return newdata with `.fitted = .y_baseline + delta_i`, `.residual = NA_real_`, `outcome = .fitted`, `.tau_i = tau_i`

**Edge cases:**
- Missing household matches: drop with warning
- Boundary taus: clamp to `[0.1, 0.9]`
- fixest predict warnings for unseen FE levels: suppress (delta method cancels FEs)

### 2. Add RIF dispatch in `run_sim_pipeline()` (`R/fct_simulations.R`)

**Extend signature** with three optional params:
```r
run_sim_pipeline <- function(weather_raw, svy, sw, so,
                             model, residuals, train_data, engine,
                             slim = FALSE,
                             fit_multi = NULL, taus = NULL, weather_cols = NULL)
```

**After `prepare_hist_weather()`, before back-transform:**
```r
if (identical(engine, "rif")) {
  preds <- tryCatch(
    predict_rif(fit_multi, survey_wd_sim, train_data, taus, so$name, weather_cols),
    error = function(e) { warning(...); NULL }
  )
} else {
  preds <- tryCatch(predict_outcome(...), ...)  # existing code
}
```

No changes to `apply_log_backtransform()` or slim logic — they work on the output columns.

### 3. Forward params in `run_policy_pipeline()` (`R/fct_policy_sim.R`)

**Extend signature:**
```r
run_policy_pipeline <- function(hist_sim, saved_scenarios, svy_mod, sw, so,
                                model, residuals, train_data, engine,
                                fit_multi = NULL, taus = NULL, weather_cols = NULL)
```

Forward `fit_multi`, `taus`, `weather_cols` to all `run_sim_pipeline()` calls inside (3 call sites: historical, per-model future loop, single-pass future fallback).

### 4. Wire up `mod_2_01_weathersim_server` (`R/mod_2_01_weathersim.R`)

**Extract RIF fields** (around line 398):
```r
model        <- if (identical(engine, "rif")) NULL else mf$fit3
fit_multi    <- if (identical(engine, "rif")) mf$fit3 else NULL
taus         <- mf$taus
weather_cols <- mf$weather_terms
```

**Pass to `run_sim_pipeline()`** calls (add `fit_multi`, `taus`, `weather_cols`).

**Residual UI:** When `engine == "rif"`, render a note ("RIF captures distributional heterogeneity natively — residual simulation is not applicable") instead of the residual radio buttons. Use `renderUI` conditional on `model_fit()$engine`.

### 5. Wire up `mod_3_05_policy_sim_server` (`R/mod_3_05_policy_sim.R`)

**In the `run_policy_pipeline()` call** (around line 140):
```r
res <- run_policy_pipeline(
  ...,
  fit_multi    = if (identical(mf$engine, "rif")) mf$fit3 else NULL,
  taus         = mf$taus,
  weather_cols = mf$weather_terms
)
```

## Key Considerations

- **Log-transformed outcomes:** Delta is on log scale. `apply_log_backtransform()` exponentiates `y_sim = log(y_base) + delta_log`, giving `y_base * exp(delta_log)` — correct multiplicative effect.
- **Performance:** The row-wise `approx()` interpolation could be slow. Consider bulk interpolation by binning rows into tau intervals.
- **Household key matching:** `train_data` may have duplicates on join keys. Use `unique()` on lookup tables.
- **fixest predict with new FE levels:** Delta method cancels FEs, so unseen year levels are OK.

## Verification

1. Select "Quantile regression" in Step 1 → run historical simulation → completes without error
2. Point-range chart renders with plausible values
3. Future climate scenarios work (SSP × period combinations)
4. Residual controls properly disabled/noted for RIF
5. Aggregation produces same-shaped output as standard engines
6. Standard fixest simulation path still works unchanged
7. Verify delta = 0 when scenario weather equals training weather