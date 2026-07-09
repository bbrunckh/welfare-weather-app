---
name: add-model-engine
description: >
  Step-by-step guide for adding a new model engine (e.g., XGBoost, neural net,
  quantile forest) to WISE-APP. Covers the ENGINE_REGISTRY, model selection UI,
  prediction pipeline, results display, simulation, and policy modules.
  Use when adding a new modeling method end-to-end.
when_to_use: >
  When the user wants to add a new model type, register a new engine,
  or extend the modeling pipeline with a new estimation method.
user-invocable: true
allowed-tools: Read Grep Glob
argument-hint: "[engine-name]"
---

# Adding a New Model Engine to WISE-APP

This guide walks through every integration point for adding a new model engine.
The app uses a **registry pattern** — each engine is a named list in
`ENGINE_REGISTRY` with standardised functions. Downstream code dispatches on
the `engine` string returned by `fit_model()`.

## Architecture Overview

```
Step 0 (Data)
  ↓ survey_weather
Step 1 (Modelling)
  ├── fct_model_select.R    → user picks model type → infer_engine()
  ├── fct_fit_model.R       → ENGINE_REGISTRY → fit_model() returns mf
  ├── fct_results.R         → coefplot, regtable, weather effect, diagnostics
  ├── mod_1_07_results.R    → wires results UI
  └── mod_1_08_modelfit.R   → wires diagnostics UI
  ↓ model_fit reactive (mf)
Step 2 (Simulation)
  ├── fct_predict_outcomes.R → predict_outcome() dispatches on engine/class
  ├── fct_simulations.R      → run_sim_pipeline() → predict → backtransform → aggregate
  └── mod_2_01_weathersim.R  → passes model + engine to pipeline
  ↓ hist_sim, saved_scenarios
Step 3 (Policy)
  ├── fct_policy_sim.R       → apply_policy_to_svy() + run_policy_pipeline()
  └── mod_3_05_policy_sim.R  → re-runs pipeline on modified covariates
```

For implementation details on each file, see [reference.md](reference.md).

---

## Checklist (7 steps)

### 1. Register the engine in `R/fct_fit_model.R`

Add an entry to `ENGINE_REGISTRY` (defined ~line 35). Each engine is a named
list with **5 required functions**:

```r
ENGINE_REGISTRY$myengine <- list(
  requires    = c("pkg1", "pkg2"),
  model_types = c("My Model Type"),

  build_formulas = function(y_var, terms, fe_vars) { ... },
  fit_one        = function(formula, data, model_type, model_spec, opts) { ... },
  make_spec      = function(model_type, use_logit) { ... },
  prepare_outcome = function(df, y_var, use_logit) { ... }
)
```

| Function | Purpose | Return |
|----------|---------|--------|
| `build_formulas` | Constructs formula1 (weather), formula2 (+FE), formula3 (+covariates) | Named list of 3 formulas (or RHS strings) |
| `fit_one` | Fits a single model specification | Fitted model object |
| `make_spec` | Creates parsnip model spec (or `NULL` for non-parsnip) | `model_spec` or `NULL` |
| `prepare_outcome` | Coerces outcome column (e.g., integer for logistic, RIF columns for quantile) | Modified data frame |

**Key conventions:**
- `fit_one` is called 3 times (once per formula)
- `opts` carries engine-specific options (e.g., hyperparameters)
- The return of `fit_one` is stored as `mf$fit1`, `mf$fit2`, `mf$fit3`
- `fit_model()` returns `engine = "myengine"` in the output list

### 2. Wire model selection in `R/fct_model_select.R`

**`infer_engine()`** (~line 144): Add mapping from model type string to engine key:
```r
"My Model Type" = "myengine"
```

**`model_type_choices()`** (~line 78): Add the model type to the appropriate
outcome-type branch (numeric, logical, or both):
```r
# For numeric outcomes:
choices = c("Linear regression", "Quantile regression", "My Model Type")
```

### 3. Handle prediction in `R/fct_predict_outcomes.R`

**If your engine returns a standard model object** (supports `stats::predict()`
or `broom::augment()`), the existing fixest/parsnip/lm branches may already
work. Test this first.

**If your engine needs custom prediction**, add a branch in `predict_outcome()`
(~line 131):
```r
is_myengine <- !is.null(engine) && identical(engine, "myengine")
```

Then add prediction logic (~line 147) and residual extraction (~line 196).

The function must produce a data frame with columns:
- `.fitted`  — predicted values
- `.residual` — residual draw (or `NA_real_` if not applicable)
- `<outcome>` — `.fitted + .residual` (the simulated outcome)

### 4. Adapt results display in `R/fct_results.R`

**Standard regression engines** (produce coefficient tables): no changes needed.
The default branches use `fixest::coeftable()` or `broom::tidy()` which work
for most model types.

**Non-standard engines** (tree-based, distributional, etc.): add conditional
branches in these functions:

| Function | What it does | When to customise |
|----------|-------------|-------------------|
| `make_coefplot()` | Coefficient forest plot | Non-parametric models (e.g., variable importance instead) |
| `make_regtable()` | HTML regression table | Models without standard coefficient tables |
| `make_weather_effect_plot()` | Marginal effect of weather variable | Models needing custom partial-dependence plots |
| `calc_fit_stats()` | Fit statistics (R^2, N) | Models with different goodness-of-fit measures |

Pattern: `if (identical(engine, "myengine")) { ... } else { ... }`

These functions receive the `engine` parameter from `mod_1_07_results.R`.

### 5. Wire results/diagnostics modules

**`R/mod_1_07_results.R`** (~line 180): Update conditional UI labels if your
engine needs different section headers (e.g., "Variable importance" instead of
"Coefficient plot").

**`R/mod_1_08_modelfit.R`** (~line 54): If your model object needs unwrapping
for diagnostic functions (like RIF extracts the median sub-model), add a branch
in `extract_native_fit()` / `extract_rif_median()`.

For standard engines that return objects with `residuals()` and `fitted()`,
diagnostic plots work out of the box.

### 6. Integrate with simulation pipeline (Module 2)

**Standard engines** (where `stats::predict(model, newdata)` works):
No changes. `run_sim_pipeline()` calls `predict_outcome()` which dispatches
based on engine/class. Residual methods (none/original/normal/empirical) are
handled generically.

**Engines needing custom simulation** (like RIF's delta method):
- Add prediction function in `R/fct_rif_sim.R` (or a new `fct_*` file)
- Add dispatch branch in `run_sim_pipeline()` (`R/fct_simulations.R`)
- Extend `run_sim_pipeline()` signature with engine-specific params
- Forward those params through `run_policy_pipeline()` (`R/fct_policy_sim.R`)
- Wire in `mod_2_01_weathersim.R` (extract engine-specific fields from `mf`)

**Residual controls:** If residuals don't apply to your engine, conditionally
disable the residual radio buttons in the UI (see RIF implementation in
`mod_2_01_weathersim.R` for the `renderUI` pattern).

### 7. Integrate with policy pipeline (Module 3)

**Standard engines:** No changes needed. `run_policy_pipeline()` calls
`run_sim_pipeline()` which handles prediction. `apply_policy_to_svy()` modifies
covariates and is engine-agnostic.

**Engines with custom simulation:** Forward engine-specific params from
`mod_3_05_policy_sim.R` through `run_policy_pipeline()` to `run_sim_pipeline()`.

---

## fit_model() Return Structure

Every engine must produce a return list with these fields:

```r
list(
  fit1              = <fitted model 1 (weather only)>,
  fit2              = <fitted model 2 (+ FE)>,
  fit3              = <fitted model 3 (+ FE + controls)>,
  weather_terms     = <character vector of weather variable names>,
  interaction_terms = <character vector>,
  fe_terms          = <character vector>,
  y_var             = <outcome column name>,
  model_type        = <e.g., "linear", "logistic", "My Model Type">,
  engine            = <engine key, e.g., "myengine">,
  train_data        = <data frame used for fitting (after NA removal)>,
  formulas          = <list of 3 formulas>,
  # Engine-specific extras (optional):
  rif_grid          = NULL,  # RIF only
  taus              = NULL   # RIF only
)
```

Downstream code accesses `mf$fit3` for prediction, `mf$engine` for dispatching,
and `mf$train_data` for residual computation.

**Important:** `train_data` is the data AFTER `complete.cases()` filtering
(~line 712 of `fct_fit_model.R`). It may have fewer rows than the original
`survey_weather()`. When matching simulation data back to training data,
use `svy` (the raw survey data), not `train_data`.

---

## Testing Checklist

1. **Step 1:** Select new model type → fits without error for all 3 specs
2. **Step 1:** Coefficient plot / regression table render correctly
3. **Step 1:** Weather effect plot renders
4. **Step 1:** Diagnostics tab works (residuals, fit stats, importance)
5. **Step 2:** Historical simulation completes → point-range chart renders
6. **Step 2:** Future climate scenarios work (SSP x period)
7. **Step 2:** Residual controls behave correctly for the engine
8. **Step 3:** Policy scenario completes → comparison chart renders
9. **Regression:** Standard fixest/ranger paths still work unchanged