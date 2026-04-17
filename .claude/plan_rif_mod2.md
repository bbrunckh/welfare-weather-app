# Plan: RIF Quantile Regression — Module 2 (Simulation)

## Context

Module 1 RIF is implemented: `fit_model()` returns `engine = "rif"`, `rif_grid`, `taus`, and `fit3` as a `fixest_multi` (9 sub-models). Module 2 currently has no RIF handling.

## Issues from First Attempt

Three bugs in the first implementation:

1. **Memory explosion:** `merge()` calls created cartesian products when join keys weren't unique. Fix: use `match()` on pasted keys or row-id approach.
2. **203K unmatched rows:** `predict_rif` matched against `train_data`, which has fewer rows than `svy` because `fit_model()` drops incomplete cases (line 712: `df <- df[complete.cases(...), ]`). Fix: match against `svy` (which `prepare_hist_weather` was built from).
3. **Non-equivalent results:** Root cause TBD — see analysis below.

## What "Equivalent" Should Mean

The standard (linear regression) pipeline does:
```
newdata = prepare_hist_weather(weather_raw, svy, ...)  # svy + scenario weather
y_hat   = predict(fit3, newdata)                       # single fitted value per row
outcome = y_hat + residual_draw
→ aggregate by sim_year → point-range chart
```

For RIF to be "equivalent" at the **mean** level, the simulation should produce:
- Similar **mean** outcomes per sim_year (since the mean of quantile-specific effects ≈ the mean effect)
- Similar **trends** across years (weather impacts of similar magnitude)
- **Different** distributional stats (poverty rate, Gini) — that's the whole point of RIF

If the mean outcomes are wildly different, there's a bug. If only the distribution differs, that's expected.

## Prediction Approach: Delta Method (Revised)

For each household `i` at baseline quantile position `tau_i`:
```
pred_new(tau_i)  = RIF prediction under scenario weather + same covariates
pred_base(tau_i) = RIF prediction under baseline weather + same covariates
delta_i          = pred_new(tau_i) - pred_base(tau_i)
y_sim_i          = y_baseline_i + delta_i
```

### Why delta method (not direct prediction)?

`stats::predict(fit_multi[[k]], newdata)` returns predicted **RIF values**, not predicted welfare. RIF values are unconditional quantile partial effects — not interpretable as individual outcomes. The delta method converts this back to household-level outcomes: "given weather changed, how much does welfare shift at this household's quantile position?"

### Key Design Decisions (Revised)

**Matching approach: row-ID, not key matching.** Add `.svy_row_id` to `svy` before `prepare_hist_weather()`. The inner_join preserves this column, giving a reliable index back to `svy` without any key-matching fragility.

**Source for y_baseline and baseline weather: `svy`, not `train_data`.** `svy` is the raw survey data that `prepare_hist_weather` was built from — guaranteed to match. `train_data` is only used for the ecdf.

**No full-dataframe copies.** Swap weather columns in `newdata` in-place for each predict call, avoiding a second copy of the dataframe.

**Vectorised interpolation.** Use `findInterval()` + linear interpolation instead of row-wise `approx()`.

## Files to Modify

| File | Action |
|------|--------|
| `R/fct_rif_sim.R` | Add `predict_rif()` |
| `R/fct_simulations.R` | Add RIF dispatch + row-ID tagging in `run_sim_pipeline()` |
| `R/fct_policy_sim.R` | Forward new params in `run_policy_pipeline()` |
| `R/mod_2_01_weathersim.R` | Pass RIF fields; conditional residual UI |
| `R/mod_3_05_policy_sim.R` | Pass RIF fields |

## Step-by-step

### 1. Add `predict_rif()` to `R/fct_rif_sim.R`

```r
predict_rif <- function(fit_multi, newdata, svy, train_data, taus, outcome,
                        weather_cols)
```

**Parameters:**
- `fit_multi`: `fixest_multi` (`mf$fit3`) — 9 sub-models
- `newdata`: from `prepare_hist_weather()` — has scenario weather + survey covariates + `.svy_row_id`
- `svy`: the raw survey data passed to `prepare_hist_weather()` — has outcome + baseline weather. Used for lookups (guaranteed key alignment).
- `train_data`: `mf$train_data` — used ONLY for `ecdf()` (quantile assignment)
- `taus`: `c(0.1, 0.2, ..., 0.9)`
- `outcome`: name of outcome column
- `weather_cols`: `mf$weather_terms` — raw weather variable names

**Algorithm:**
1. **Row mapping via `.svy_row_id`:**
   ```r
   svy_row <- newdata$.svy_row_id
   ```
   Guard: stop if `.svy_row_id` is missing.

2. **Recover baseline outcome from `svy`:**
   ```r
   y_baseline <- svy[[outcome]][svy_row]
   ```

3. **Assign tau_i via ecdf (from `train_data`):**
   ```r
   F_hat <- ecdf(train_data[[outcome]])
   tau_i <- pmin(pmax(F_hat(y_baseline), min(taus)), max(taus))
   ```

4. **Predict deltas — swap weather in-place:**
   ```r
   saved_weather <- newdata[, weather_cols]  # scenario weather
   for (wc in weather_cols) newdata[[wc]] <- svy[[wc]][svy_row]  # replace with baseline

   for (k in 1:9) {
     pred_base <- predict(fit_multi[[k]], newdata)       # baseline weather
     for (wc) newdata[[wc]] <- saved_weather[[wc]]       # swap to scenario
     pred_new  <- predict(fit_multi[[k]], newdata)       # scenario weather
     for (wc) newdata[[wc]] <- svy[[wc]][svy_row]        # swap back to baseline
     delta_k <- pred_new - pred_base
     # accumulate interpolated delta_i using findInterval + linear weights
   }
   ```

5. **Assemble output:**
   ```r
   newdata$.fitted   <- y_baseline + delta_i
   newdata$.residual <- NA_real_
   newdata[[outcome]] <- y_baseline + delta_i
   ```

**Validation check in predict_rif:**
After the first sim_year's predictions, verify that when scenario weather ≈ baseline weather, delta ≈ 0. Log a diagnostic message with `mean(abs(delta_i))` so we can catch systematic errors early.

### 2. Add RIF dispatch + row-ID tagging in `run_sim_pipeline()` (`R/fct_simulations.R`)

**Extend signature** with optional params:
```r
run_sim_pipeline <- function(weather_raw, svy, sw, so,
                             model, residuals, train_data, engine,
                             slim = FALSE,
                             fit_multi = NULL, taus = NULL, weather_cols = NULL)
```

**Before calling `prepare_hist_weather()`, tag svy with row IDs for RIF:**
```r
if (identical(engine, "rif") && !is.null(fit_multi)) {
  svy$.svy_row_id <- seq_len(nrow(svy))
}
survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)
```

**After `prepare_hist_weather()`, dispatch:**
```r
if (identical(engine, "rif") && !is.null(fit_multi)) {
  preds <- tryCatch(
    predict_rif(fit_multi, survey_wd_sim, svy, train_data, taus, so$name, weather_cols),
    error = ...
  )
} else {
  preds <- tryCatch(predict_outcome(...), ...)
}
```

Rest of function (back-transform, slim) unchanged. `.svy_row_id` and `.tau_i` will be trimmed by the slim logic since they're not in the keep list.

### 3. Forward params in `run_policy_pipeline()` (`R/fct_policy_sim.R`)

Extend signature + forward to all 3 internal `run_sim_pipeline()` calls. Same as before.

### 4. Wire up `mod_2_01_weathersim_server` (`R/mod_2_01_weathersim.R`)

- Extract `fit_multi`, `taus`, `weather_cols` from `mf`
- Force `residuals = "none"` for RIF
- Pass new params to `run_sim_pipeline()`
- Conditional residual UI via `renderUI`

### 5. Wire up `mod_3_05_policy_sim_server` (`R/mod_3_05_policy_sim.R`)

Pass RIF fields to `run_policy_pipeline()`.

**Module 3 limitation (noted, not blocking):** The delta method captures weather effects at each quantile. For Module 3 policy scenarios, the comparison between baseline and policy captures how the **weather vulnerability** changes with the policy (e.g., does electrification make poor households less sensitive to heat?). The **direct** policy effect (e.g., electrification raises welfare by X) is NOT captured because it cancels in the delta. This is a known limitation — the direct effect shows up in the diagnostics table. A full fix would require computing a total delta = predict(scenario_weather, mod_covariates) - predict(base_weather, original_covariates), which we defer.

## Verification (expanded)

1. **Sanity check:** With historical weather only (no future scenarios), run both linear and quantile regression. Compare mean outcome per sim_year — they should be in the same ballpark.
2. **Delta = 0 test:** If sim_year weather equals training-year weather, delta should be ~0 for every household.
3. **Direction test:** If a year has extreme heat and the temperature coefficient is negative, mean predicted welfare should drop.
4. Point-range chart renders with plausible values
5. Future climate scenarios work
6. Residual UI properly disabled for RIF
7. Standard fixest path unchanged
8. Memory stays reasonable (no merge, no full copies)
