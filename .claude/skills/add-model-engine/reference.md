# Reference: File-by-File Integration Points

## R/fct_fit_model.R — Engine Registry & Fitting

**ENGINE_REGISTRY** (top of file, before `fit_model()`):
Currently contains engines: `fixest`, `ranger`, `xgboost`, `rif`.

Each entry's function signatures:

```r
build_formulas = function(y_var, terms, fe_vars)
# y_var: outcome column name
# terms: list with $hazard, $interactions_main, $covariates
# fe_vars: character vector of fixed-effect variables
# Returns: list(formula1, formula2, formula3)

fit_one = function(formula, data, model_type, model_spec, opts)
# formula: from build_formulas (formula object or RHS string)
# data: training data with outcome + predictors
# model_type: e.g., "linear", "logistic"
# model_spec: from make_spec() (parsnip spec or NULL)
# opts: list with engine-specific options (e.g., opts$fixest for clustering)
# Returns: fitted model object

make_spec = function(model_type, use_logit)
# Returns: parsnip model_spec or NULL

prepare_outcome = function(df, y_var, use_logit)
# Coerce/transform outcome column as needed
# Returns: modified data frame (may add columns, e.g., RIF columns)
# May set attributes on df (e.g., attr(df, "rif_taus"))
```

**fit_model() key locations** (search for these landmarks):
- `engine_key <- tolower(...)` — engine key lookup
- `backend$prepare_outcome(...)` — outcome preparation
- `rif_taus <- attr(df, "rif_taus")` — attribute extraction after prepare_outcome
- `backend$build_formulas(...)` — formula construction
- `df <- df[complete.cases(...)` — **filters rows!** train_data will have fewer rows than svy
- `engine_opts$rif <- ...` — engine-specific option injection
- `backend$fit_one(...)` — fitting call (called 3 times for fit1/fit2/fit3)
- The final `list(fit1 = ..., engine = engine_key, ...)` — return structure

---

## R/fct_model_select.R — Model Type → Engine Mapping

**infer_engine()** — `switch()` block mapping model type strings to engine keys:
```r
"Linear regression"  → "fixest"
"Logistic regression" → "fixest"
"Random forest"       → "ranger"
"XGBoost"             → "xgboost"
"Quantile regression" → "rif"
```

**model_type_choices()** — returns choices based on `outcome_type`:
- `"logical"`: Logistic, Linear
- `"numeric"` (default): Linear, Quantile regression

---

## R/fct_predict_outcomes.R — Prediction Dispatch

**predict_outcome()** signature:
```r
predict_outcome(model, newdata, type, residuals, id, outcome, train_data, engine)
```

**Engine detection** (near top of function body):
```r
is_fixest  <- inherits(model, "fixest") || identical(engine, "fixest")
is_parsnip <- !is_fixest && inherits(model, "model_fit")
# fallback: bare lm/glm
```

**Prediction branches** (in the "Compute fitted values" section):
- fixest: `stats::predict(model, newdata, type = "response")`
- parsnip: `broom::augment(model, new_data = newdata)`
- bare lm/glm: `broom::augment(model, newdata = newdata)`

**Residual branches** (in the "Compute residuals" section):
- fixest: `stats::residuals(model, type = ...)` + `stats::predict(model, newdata = train_data)`
- parsnip: `broom::augment(model, new_data = train_data)`
- bare: `broom::augment(model, data = train_data)`

**Output contract** — must produce data frame with:
- `.fitted` (numeric): predicted values
- `.residual` (numeric or NA): residual draw
- `<outcome>` (numeric): `.fitted + residual_draw`

---

## R/fct_results.R — Results Functions

All results functions receive `engine` as a parameter from mod_1_07_results.R.

**Engine-branching functions** (search for `identical(engine, "rif")`):

| Function | Branch pattern |
|----------|---------------|
| `make_coefplot()` | `if (identical(engine, "rif") && !is.null(rif_grid))` |
| `make_weather_effect_plot()` | `if (identical(engine, "rif") && !is.null(rif_grid))` |
| `make_regtable()` | `if (identical(engine, "rif") && !is.null(rif_grid))` |
| `calc_fit_stats()` | `if (identical(engine, "rif") && ...)` |

**Functions that work generically (no engine branching needed):**
- `plot_diagnostics()` — residuals vs fitted
- `plot_resid_weather()` — residuals vs weather variable
- `plot_pred_vs_actual()` — predicted vs actual distribution
- `plot_relaimpo()` — relative importance via coefficient magnitudes

---

## R/mod_1_07_results.R — Results UI Wiring

- `extract_native_fit(fit, mf$engine)` — unwraps native model from parsnip wrapper
- `engine = mf$engine` passed to all plot/table functions
- `is_rif <- identical(mf$engine, "rif")` — controls conditional UI labels (e.g., "UQR coefficients" vs "Coefficients")

---

## R/mod_1_08_modelfit.R — Diagnostics UI Wiring

- `extract_native_fit()` / `extract_rif_median()` — for RIF, returns tau=0.5 sub-model for diagnostics
- RIF-specific `pred_welf_dist` plot (quantile markers instead of standard predicted-vs-actual)
- `calc_fit_stats(model, is_logistic, engine = ...)` — engine passed for RIF branching
- Model summary header varies by engine

---

## R/fct_simulations.R — Simulation Pipeline

**run_sim_pipeline()** signature:
```r
run_sim_pipeline(weather_raw, svy, sw, so,
                 model, residuals, train_data, engine,
                 slim = FALSE,
                 # Engine-specific optional params (e.g., RIF):
                 fit_multi = NULL, taus = NULL, weather_cols = NULL)
```

Flow:
1. `prepare_hist_weather()` — inner-joins weather to survey on (code, year, survname, loc_id, int_month). **Drops** weather and outcome columns from survey side before joining.
2. `predict_outcome()` or engine-specific predict (e.g., `predict_rif()`)
3. `apply_log_backtransform()` — exponentiates if `so$transform == "log"`
4. Slim trimming (future scenarios only)

---

## R/fct_policy_sim.R — Policy Pipeline

**apply_policy_to_svy()**: Modifies covariate columns. Engine-agnostic.

**run_policy_pipeline()**: Re-runs `run_sim_pipeline()` on modified covariates
for historical + each future scenario. Has **3 internal call sites** to
`run_sim_pipeline()` — all must forward engine-specific params:
1. Historical simulation
2. Per-ensemble-model loop (future scenarios with `model` column)
3. Single-pass fallback (future scenarios without `model` column)

---

## R/mod_2_01_weathersim.R — Simulation Module Server

Inside `observeEvent(input$run_sim, ...)`:
- Extracts `model`, `engine`, `train_data` from `model_fit()`
- Calls `run_sim_pipeline()` per weather key in a loop
- Residual radio buttons rendered via `renderUI` (conditional on engine)

---

## R/mod_3_05_policy_sim.R — Policy Module Server

Inside `observeEvent(input$run_sim, ...)`:
- `apply_policy_to_svy()` call
- `run_policy_pipeline()` call — passes model, engine, train_data + engine-specific params

---

## Common Patterns

**Engine detection in downstream code:**
```r
# By engine string (preferred for dispatch):
if (identical(engine, "myengine")) { ... }

# By model class (for predict_outcome):
is_myengine <- inherits(model, "my_model_class")
```

**Passing engine-specific data through the pipeline:**
If your engine needs extra data beyond `model` (like RIF needs `fit_multi`,
`taus`, `weather_cols`):
1. Store it in the `fit_model()` return list
2. Extract it in the module server (mod_2_01, mod_3_05)
3. Add optional params to `run_sim_pipeline()` and `run_policy_pipeline()`
4. Forward them through all 3 internal `run_sim_pipeline()` calls in `run_policy_pipeline()`

**Residual handling:**
If residuals don't apply to your engine (e.g., quantile regression, Bayesian):
- Force `residuals = "none"` in the module server
- Conditionally render a UI note instead of the radio buttons via `renderUI`
- Set `.residual = NA_real_` in the prediction output