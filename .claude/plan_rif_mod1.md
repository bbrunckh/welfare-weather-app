# Plan: RIF Quantile Regression — Module 1 (Engine + Results)

## Overview

Add "Quantile regression" as a model type for continuous outcomes. This requires a new engine in the registry, adapted results display (beta curves instead of standard coefplots), and UI wiring.

## Files to modify/create

| File | Action |
|------|--------|
| `R/fct_rif_sim.R` | **CREATE** — `compute_rif()`, `build_rif_grid()` |
| `R/fct_fit_model.R` | Add `rif` engine to `ENGINE_REGISTRY`, extend `fit_model()` return |
| `R/fct_model_select.R` | Add "Quantile regression" to choices + engine mapping |
| `R/fct_results.R` | Add RIF-aware branches for coefplot, regtable, effect plots, diagnostics |
| `R/mod_1_07_results.R` | Pass `engine` and `rif_grid` to results functions |
| `R/mod_1_08_modelfit.R` | Handle `fixest_multi` in diagnostic plots |

## Step-by-step

### 1. Create `R/fct_rif_sim.R`

- `compute_rif(y, tau, bw=NULL)` — from test script lines 25–39
- `build_rif_grid(fits_multi, taus, model_id)` — tidy a fixest_multi into (tau, term, estimate, std.error, conf.low, conf.high, model)

### 2. Add `rif` engine to `R/fct_fit_model.R`

**ENGINE_REGISTRY$rif:**
- `requires`: `c("fixest", "broom")`
- `model_types`: `c("Quantile regression")`
- `build_formulas`: Same as fixest (FE via `|` syntax). Formula templates with original y_var — fit_one replaces LHS.
- `fit_one`: Read `opts$rif$rif_cols` and `opts$rif$taus`. Build stacked LHS `c(rif_10, ..., rif_90)`. Call `fixest::feols(stacked_fml, data, ...)`. Return `fixest_multi`.
- `make_spec`: `NULL`
- `prepare_outcome`: Compute 9 RIF columns via `compute_rif()`, store `taus`/`rif_cols` as attributes on df.

**Modify `fit_model()`:**
- After `prepare_outcome` (line 545): extract `rif_taus` and `rif_cols` from df attributes.
- After `engine_opts` (line 675): add `engine_opts$rif` with taus and rif_cols.
- After fitting (line 690): build `rif_grid` from all 3 fits via `build_rif_grid()`.
- Extend return list with `rif_grid` and `taus` (NULL for non-RIF).

### 3. Add model type in `R/fct_model_select.R`

- `model_type_choices()`: for numeric outcomes, `choices = c("Linear regression", "Quantile regression")`
- `infer_engine()`: add `"Quantile regression" = "rif"`

### 4. Adapt results in `R/fct_results.R`

- Add `is_rif_engine(engine)` helper
- `make_coefplot()`: RIF branch → `make_rif_coefplot()` — beta curves (x=tau, y=estimate, faceted by term, colored by model 1/2/3, CI ribbons)
- `make_regtable()`: RIF branch → `make_rif_regtable()` — HTML table with terms as rows, quantiles as columns, model 3 only
- `make_weather_effect_plot()`: RIF branch → `make_rif_weather_effect_plot()` — weather beta curve across quantiles
- `plot_diagnostics()`: use median model (tau=0.5, index 5)
- `plot_resid_weather()`: use median model
- `plot_pred_vs_actual()`: overlay predicted quantile lines on actual distribution
- `calc_fit_stats()`: per-quantile R² table
- `plot_relaimpo()`: use median model

### 5. Wire UI in `R/mod_1_07_results.R`

- Pass `engine` and `rif_grid` to results functions
- Adjust section headers for RIF

### 6. Wire diagnostics in `R/mod_1_08_modelfit.R`

- Extract median model from fixest_multi for diagnostics
- Pass `engine` to stat functions

## Verification

1. Select "Quantile regression" for a continuous outcome → model fits without error
2. Beta curve coefplot renders (faceted by term, 3 model specs)
3. Regtable shows coefficients across quantiles with significance stars
4. Weather effect plot shows beta curve with CI ribbon
5. Diagnostics tab works (residuals, fit stats, importance)
6. Standard fixest/ranger/xgboost pipelines still work unchanged
