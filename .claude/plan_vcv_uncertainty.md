# Plan: Efficient Parameter Uncertainty via Cholesky Factor Loading

## Why this is faster

The current approach draws S coefficient vectors, then computes N household predictions for each draw — producing an N×S data frame per weather key. Even the vectorised path materializes all N×S values.

The factor-loading approach exploits the fact that `X β_s = X(β̂ + Lz_s) = Xβ̂ + (XL')z_s = ŷ + Fz_s`. Instead of recomputing N predictions S times, you compute the point prediction `ŷ` and the loading matrix `F = XL'` once per key. To get a perturbed prediction, you draw a cheap K-dimensional z (K = number of coefficients, typically 5–20) and multiply by F. You never build the N×S data frame.

| | Current | Factor-loading |
|---|---|---|
| Predictions per key | N × S | N × 1 |
| Stored per key | N×S data frame | N-vector + N×K matrix |
| Pre-aggregation at sim time | 18 combos × N×S rows | none (deferred to display) |
| Draw cost at display time | 0 (already done) | S × K (trivial) |
| UI change (method/poverty line) | filter pre-computed table | recompute from compact data |

For linear aggregates on non-log outcomes, you don't even need draws — `Var(weighted mean) = f̄'f̄` is a single dot product. Draws (S iterations of a K-vector) are only needed for non-linear aggregates (headcount, poverty gap) or log-transformed outcomes where `exp()` breaks the closed form.

Deferring aggregation also eliminates the bake-in problem: poverty line, aggregation method, and weights can be changed at display time without re-running the simulation.

------------------------------------------------------------------------

## Context

### What exists today (commit `9b5dd1e`)

The current implementation draws S full coefficient vectors from MVN(β̂, Σ) via `draw_coefs()` in `fct_simulations.R`, then loops through all N households × S draws per weather key. The vectorised path in `predict_outcome_vectorised()` in `fct_predict_outcomes.R` computes an N×S matrix multiply, which is faster than the sequential fallback but still produces N×S rows of predictions that must be stored and aggregated.

**Current flow:**
1. `draw_coefs()` → S×K matrix of coefficient draws (uses `MASS::mvrnorm`)
2. `run_sim_pipeline()` in `fct_simulations.R` → calls `predict_outcome_vectorised()` or sequential loop → N×S row data frame with `draw_id` column
3. `aggregate_sim_preds()` in `fct_simulations.R` → two-stage aggregation:
   - Stage 1: aggregate within each (draw_id, model, sim_year) → scalar per group
   - Stage 2: percentiles across draw_id → value_p05/p50/p95
4. Pre-aggregation in `mod_2_01_weathersim.R`: all 9 methods × 2 weighting combos are pre-computed and stored in `$agg` at simulation time → poverty line, aggregation method, and weights are **baked in**

**Current clustering:**
- `COEF_VCOV_SPEC = ~loc_id` in `fct_simulations.R` — used by `draw_coefs()` for the VCV matrix
- At model fitting time in `fct_fit_model.R`: `selected_model$cluster` is passed to fixest, but currently defaults to `character(0)` (no clustering at fit time) — the cluster UI is not wired

**Problems:**
- O(N × K × S) per key — slow and memory-heavy
- Aggregation choices baked in at simulation time (poverty line, method, weights)
- Future ensemble models are pre-aggregated per key then row-bound into `group_agg` — model spread is mixed into the same pool as coefficient draw spread, not cleanly separated
- SE clustering is only `~loc_id` — should be `~code + year + survname + loc_id` (survey-location groups)
- Exceedance plot (`enhance_exceedance()` in `fct_sim_compare.R`) shows only lines per scenario — no uncertainty bands

### What we want

Replace with an **analytical factor-loading approach**: compute `F = X_nonFE %*% chol(Σ)` once per key, store compact `(y_point, F_loading)` per key, defer aggregation to display time. Preserve per-model results for ensemble spread. Add shaded uncertainty bands to exceedance plot.

**Math:**

```
L = chol(Σ)                         # K × K, computed once from fitted model
F = X_nonFE %*% t(L)               # N × K, computed once per weather key
z_s ~ N(0, I_K)                     # K-dimensional draw (cheap)
y_i(s) = y_point_i + F_i · z_s     # perturbed household prediction
```

For linear aggregates (weighted mean, total) on non-log outcomes: `Var(agg) = f̄' f̄` — exact, no draws needed.
For non-linear aggregates (headcount, poverty gap, FGT2) or log-transformed outcomes: S draws of K-dim z, aggregate each draw to a scalar, take percentiles.

------------------------------------------------------------------------

## Step 0: Fix SE clustering — `R/fct_simulations.R`

**What:** Change `COEF_VCOV_SPEC` from `~loc_id` to `~code + year + survname + loc_id`.

**Where:** The `COEF_VCOV_SPEC` constant definition in `fct_simulations.R`.

**Why:** The regression pools observations across multiple surveys (country × year × survey-name). Within each survey-location group, weather shocks are shared, so errors are correlated. Clustering at `~loc_id` alone ignores the cross-survey dimension. The correct clustering level is the intersection of all four identifiers: `code` (country), `year`, `survname` (survey name), and `loc_id` (location).

**How:** Single-line change:

```r
# Old:
COEF_VCOV_SPEC <- ~loc_id

# New:
COEF_VCOV_SPEC <- ~code + year + survname + loc_id
```

**Implication:** This changes the VCV matrix Σ used for all uncertainty propagation. Wider clustering → larger SEs → wider confidence bands. The Cholesky factor L = chol(Σ) will be different, and all downstream uncertainty inherits this. No other code changes needed — `COEF_VCOV_SPEC` is already threaded through `draw_coefs()` and will be threaded through the new `compute_chol_vcov()`.

**Verification:** `vcov(fit, vcov = ~code + year + survname + loc_id)` must succeed — all four columns must be present in the model's data. These columns exist in the survey data (`mod_1_02_surveystats.R` uses `code, year, survname`; `loc_id` is the location identifier used throughout).

------------------------------------------------------------------------

## Step 1: Core functions — `R/fct_simulations.R`

### 1a. Replace `draw_coefs()` with `compute_chol_vcov()`

**What:** Delete `draw_coefs()`. Add a 3-line helper that returns the Cholesky factor of the clustered VCV matrix.

**Where:** Replace `draw_coefs()` in `fct_simulations.R`.

**Code:**

```r
#' Compute Cholesky Factor of Clustered VCV Matrix
#'
#' Returns the upper-triangular Cholesky factor of the variance-covariance
#' matrix for the non-FE coefficients. Used to construct factor loadings
#' F = X %*% t(L) for efficient uncertainty propagation.
#'
#' @param fit       A fitted fixest model object.
#' @param vcov_spec Formula passed to `vcov(fit, vcov = ...)`.
#'   Defaults to `COEF_VCOV_SPEC`.
#' @return A K × K upper-triangular matrix (Cholesky factor).
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  chol(vcov(fit, vcov = vcov_spec))
}
```

**Why:** The old `draw_coefs()` produced an S×K matrix of coefficient draws. The new approach only needs the Cholesky factor L (K×K) — the draws happen later as cheap K-dimensional z vectors inside `aggregate_with_uncertainty()`.

**Cleanup:** Remove `draw_coefs` from NAMESPACE. Add `compute_chol_vcov` to NAMESPACE.

### 1b. New `aggregate_with_uncertainty()`

**What:** The main new function. Computes any aggregate statistic with optional coefficient-uncertainty bands. This replaces the two-stage logic in `aggregate_sim_preds()`.

**Where:** Add after `compute_chol_vcov()` in `fct_simulations.R`.

**Signature:**

```r
#' Aggregate Predictions with Coefficient Uncertainty
#'
#' Computes an aggregate welfare statistic (mean, headcount, etc.) with
#' optional confidence bands from coefficient uncertainty via Cholesky
#' factor loadings. For linear aggregates on non-log outcomes, uses an
#' exact analytic formula. For non-linear aggregates or log-transformed
#' outcomes, uses S draws of a K-dimensional z-vector.
#'
#' @param y_point   Numeric N-vector of point-estimate fitted values
#'   (log-scale if outcome is log-transformed).
#' @param F_loading Numeric N x K matrix of factor loadings, or NULL
#'   (no uncertainty). Each row is household i's loading vector.
#' @param group_vec Factor or character N-vector defining groups
#'   (typically sim_year). Aggregation is done per group.
#' @param so        Selected-outcome list with at minimum `$name`,
#'   `$type`, `$transform`.
#' @param agg_method Character: aggregation method name passed to
#'   `aggregate_outcome()` (e.g., "mean", "headcount_ratio").
#' @param weights   Numeric N-vector of survey weights, or NULL.
#' @param pov_line  Numeric poverty line, or NULL. Required for
#'   headcount_ratio, gap, fgt2.
#' @param train_resid Numeric vector of training residuals for
#'   resampling, or NULL.
#' @param residual_method Character: "none", "original", "normal",
#'   or "resample".
#' @param id_vec    Character/integer N-vector of household IDs for
#'   "original" residual matching, or NULL.
#' @param S         Integer number of Monte Carlo draws for non-linear
#'   aggregates. Ignored for analytic path.
#' @param seed      Optional integer seed for reproducibility.
#'
#' @return A tibble with columns: `sim_year`, `value`, and optionally
#'   `value_p05`, `value_p50`, `value_p95` (when F_loading is non-NULL).
#' @export
aggregate_with_uncertainty <- function(
    y_point, F_loading, group_vec,
    so, agg_method, weights = NULL,
    pov_line = NULL,
    train_resid = NULL, residual_method = "none",
    id_vec = NULL,
    S = 200, seed = NULL)
```

**Logic per group** (split by `group_vec`, typically `sim_year`):

1. Determine path: `is_log <- isTRUE(so$transform == "log")`

2. Draw residuals (if `residual_method != "none"`): one N-vector, shared across all z-draws (residuals are independent of β). Use existing residual logic from `predict_outcome()`.

3. **Analytic path** (linear aggregate AND `is_log == FALSE`):
   - `y = y_point + resid` (or just `y_point` if no residuals)
   - `f_bar = colSums(w * F) / sum(w)` (K-vector: weighted average of loading rows)
   - `se = sqrt(sum(f_bar^2))`
   - `value = weighted.mean(y, w)`
   - `value_p05 = value + qnorm(0.05) * se`
   - `value_p95 = value + qnorm(0.95) * se`

4. **Monte Carlo path** (non-linear aggregate OR `is_log == TRUE`):
   - Set seed if provided
   - Draw S z-vectors: `Z = matrix(rnorm(S * K), nrow = S, ncol = K)`
   - For each s in 1:S:
     - `y_s = y_point + F %*% z_s + resid`
     - If log: `y_s = exp(y_s)`
     - Aggregate `y_s` to a scalar using `aggregate_outcome()` with the requested method
   - `value_p05, value_p50, value_p95 = quantile(scalars, c(0.05, 0.50, 0.95))`
   - `value = value_p50`

5. **No-uncertainty path** (`F_loading == NULL`):
   - Same as analytic but no SE calculation. Return `value` only, no p05/p95 columns.

**Output:** Tibble with `sim_year`, `value`, and optionally `value_p05`, `value_p50`, `value_p95`.

### 1b-ii. New `combine_ensemble_results()`

**What:** Takes a list of M per-model aggregation results (each a tibble from `aggregate_with_uncertainty()`) and produces a combined tibble with two layers of uncertainty: parameter uncertainty (inner) and model spread (outer).

**Where:** Add after `aggregate_with_uncertainty()` in `fct_simulations.R`.

**Code:**

```r
#' Combine Per-Model Aggregation Results
#'
#' Given M tibbles (one per GCM/ensemble model), each with per-year
#' aggregate values and optional coefficient-uncertainty bands, produce
#' a single tibble with:
#'
#' - `value`: multi-model median (used as the central line)
#' - `value_p05`/`value_p95`: average parameter-uncertainty band (inner)
#' - `model_min`/`model_max`: min/max of per-model values (outer)
#' - `model_values`: list-column of all M per-model values
#'
#' @param per_model_aggs List of M tibbles, each from
#'   `aggregate_with_uncertainty()`.
#' @return A tibble with columns: `sim_year`, `value`, `value_p05`,
#'   `value_p95`, `model_min`, `model_max`, `model_values`.
#' @export
combine_ensemble_results <- function(per_model_aggs) {
  # Stack all M tibbles, tag with model index
  tagged <- lapply(seq_along(per_model_aggs), function(m) {
    df <- per_model_aggs[[m]]
    df$model_idx <- m
    df
  })
  stacked <- dplyr::bind_rows(tagged)

  stacked |>
    dplyr::group_by(sim_year) |>
    dplyr::summarise(
      value       = median(value, na.rm = TRUE),
      # Inner band: mean of per-model parameter uncertainty bands
      value_p05   = if (all(c("value_p05") %in% names(dplyr::cur_data())))
                      mean(value_p05, na.rm = TRUE) else NA_real_,
      value_p95   = if (all(c("value_p95") %in% names(dplyr::cur_data())))
                      mean(value_p95, na.rm = TRUE) else NA_real_,
      # Outer band: actual model spread
      model_min   = min(value, na.rm = TRUE),
      model_max   = max(value, na.rm = TRUE),
      # Raw per-model values for plotting individual traces
      model_values = list(value),
      .groups     = "drop"
    )
}
```

**Key design choice:** The central `value` for future scenarios is the **median** across models, not the mean. This is what gets plotted as the line in the exceedance plot (per the user's request: "use the median as line for future scenarios").

### 1c. Modify `run_sim_pipeline()`

**What:** Replace the `coef_draws` parameter (S×K matrix) with `chol_Sigma` (K×K Cholesky factor). Eliminate the S-draw loop and N×S row output. Return compact per-household vectors + factor loading matrix.

**Where:** `run_sim_pipeline()` in `fct_simulations.R`. This is a substantial rewrite of the function body.

**New signature:**

```r
run_sim_pipeline <- function(weather_raw, svy, sw, so,
                             model, residuals, train_data, engine,
                             chol_Sigma = NULL) {
```

**New logic:**

1. `n_pre_join <- nrow(svy)`
2. `survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)`
3. `id_col <- if (residuals == "original") resolve_id_col(train_data, survey_wd_sim) else NULL`
4. Call `predict_outcome()` with `beta_override = NULL` (point estimate only, no draws):
   ```r
   preds <- predict_outcome(model, newdata = survey_wd_sim, residuals = "none",
                            outcome = so$name, id = NULL, train_data = train_data,
                            engine = engine, beta_override = NULL)
   ```
   Note: residuals = "none" here because residuals are deferred to aggregation time.
5. Extract `y_point <- preds$.fitted` (N-vector, log-scale if log outcome)
6. Do NOT apply `apply_log_backtransform()` here — that happens at aggregation time inside `aggregate_with_uncertainty()` where it interacts with the z-draws correctly (Jensen's inequality).
7. Extract `sim_year`, weight columns from preds.
8. Compute factor loadings (if `chol_Sigma` is non-NULL and engine is fixest):
   ```r
   X_nonFE <- model.matrix(model, data = survey_wd_sim, type = "rhs")
   # Trim to predicted rows (FE-absent row handling — fixest drops rows with
   # unobserved FE levels, so X_nonFE must match the rows that got predictions)
   X_nonFE <- X_nonFE[predicted_rows, , drop = FALSE]
   F_loading <- X_nonFE %*% t(chol_Sigma)   # N x K matrix multiply
   ```
   When `chol_Sigma` is NULL: `F_loading <- NULL`.

**New return schema:**

```r
list(
  y_point     = <N-vector>,         # fitted values (log-scale if log outcome)
  F_loading   = <N x K matrix>,     # NULL when no uncertainty
  sim_year    = <N-vector>,
  weight      = <N-vector or NULL>,
  id_vec      = <N-vector or NULL>, # for "original" residual matching
  n_pre_join  = <integer>,
  weather_raw = <data.frame>
)
```

**Remove:** The old S-draw `for` loop, `draw_id` column creation, `beta_override` call path, vectorised branch, sequential fallback, timing messages, `slim` parameter, `coef_draws` parameter.

### 1d. Remove `aggregate_sim_preds()`

**What:** Delete `aggregate_sim_preds()` from `fct_simulations.R` and remove from NAMESPACE. Its two-stage aggregation logic is replaced by `aggregate_with_uncertainty()`.

**Why:** The old function operated on N×S row data frames with `draw_id` columns. The new approach never creates those — it works directly with N-vectors and K-dimensional z-draws.

### 1e. Remove `predict_outcome_vectorised()`

**What:** Delete `predict_outcome_vectorised()` from `fct_predict_outcomes.R` and remove from NAMESPACE.

**Why:** This function existed to speed up the S-draw coefficient loop via matrix multiply. The factor-loading approach eliminates the need for S full predictions entirely — there is only one `predict_outcome()` call per key (point estimate), and the K-dimensional draws happen inside `aggregate_with_uncertainty()`.

### 1f. Simplify `predict_outcome()`

**What:** Remove the `beta_override` parameter and the delta-method code block from `predict_outcome()` in `fct_predict_outcomes.R`.

**Where:** Find the `beta_override` parameter in the function signature and the corresponding `if (!is.null(beta_override))` block that computes `fitted = fitted_base + X_nonFE * (beta_override - beta_pt)`.

**Why:** `beta_override` was used to substitute perturbed coefficients for each draw. With the factor-loading approach, `predict_outcome()` is only ever called once per key with the original point-estimate coefficients.

------------------------------------------------------------------------

## Step 2: Simulation server — `R/mod_2_01_weathersim.R`

### 2a. Before the key loop

**What:** Replace the `draw_coefs()` call with `compute_chol_vcov()`.

**Where:** Find the block that calls `draw_coefs()` just before the weather-key loop in the simulation `observeEvent`.

**Old code pattern:**
```r
coef_draws <- if (isTRUE(input$skip_coef_draws) || engine != "fixest") {
  NULL
} else {
  tryCatch(draw_coefs(fit = model, S = ..., method = "vcov",
                       vcov_spec = COEF_VCOV_SPEC, seed = 42L),
           error = function(e) { warning(...); NULL })
}
```

**New code:**
```r
chol_Sigma <- if (isTRUE(input$skip_coef_draws) || engine != "fixest") {
  NULL
} else {
  tryCatch(
    compute_chol_vcov(model, COEF_VCOV_SPEC),
    error = function(e) {
      warning("[mod_2_01] compute_chol_vcov() failed, falling back to point estimates: ",
              conditionMessage(e))
      NULL
    }
  )
}
```

**Why:** We only need the K×K Cholesky factor, not S×K draws. Computed once, reused for all keys. The `seed` parameter moves to `aggregate_with_uncertainty()` at display time.

### 2b. Key loop — simplified

**What:** For each key, call `run_sim_pipeline(..., chol_Sigma = chol_Sigma)` and store the compact result. No pre-aggregation.

**Historical key** (replaces the `is_hist` branch that currently pre-aggregates all methods):

```r
if (is_hist && is.null(hist_sim_result)) {
  hist_sim_result <- list(
    y_point         = out$y_point,
    F_loading       = out$F_loading,
    sim_year        = out$sim_year,
    weight          = out$weight,
    id_vec          = out$id_vec,
    so              = so,
    n_pre_join      = out$n_pre_join,
    weather_raw     = out$weather_raw,
    train_data      = train_data,
    residual_method = residuals,    # "none"/"original"/"normal"/"resample"
    has_weights     = !is.null(out$weight)
  )
}
```

**Future key** (replaces the branch that pre-aggregates and row-binds into `group_agg`):

```r
} else if (!is_hist) {
  ssp_code   <- sub("^(ssp[0-9]_[0-9]_[0-9])_.*$", "\\1", key)
  rest       <- sub(paste0("^", ssp_code, "_"), "", key)
  period_str <- sub("^([0-9]{4}_[0-9]{4})_.*$", "\\1", rest)
  model_name <- sub(paste0("^", period_str, "_"), "", rest)
  yr_parts   <- as.integer(strsplit(period_str, "_")[[1]])
  gk         <- paste0(ssp_code, "_", period_str)

  # Store per-model results — no pooling, no pre-aggregation
  if (is.null(group_data[[gk]])) {
    group_data[[gk]] <- list(
      models   = list(),
      sim_year = out$sim_year,
      weight   = out$weight,
      id_vec   = out$id_vec
    )
  }
  group_data[[gk]]$models[[length(group_data[[gk]]$models) + 1L]] <- list(
    y_point   = out$y_point,
    F_loading = out$F_loading,
    name      = model_name
  )

  # Retain representative weather (first model only) for diagnostics
  if (is.null(group_weather_rep[[gk]])) {
    group_weather_rep[[gk]] <- out$weather_raw
  }

  if (is.null(group_meta[[gk]])) {
    group_meta[[gk]] <- list(ssp_code = ssp_code, year_range = yr_parts)
  }

  rm(out); gc(verbose = FALSE)
}
```

**Key difference from old code:** No `agg_methods_all` loop, no `aggregate_sim_preds()` call, no `group_agg` accumulator. Each model's `(y_point, F_loading)` is stored as-is.

### 2c. Remove

- `agg_methods_all` definition (the list of 9 aggregation methods)
- The `lapply(agg_methods_all, ...)` pre-aggregation loops (for both historical and future keys)
- `group_agg` accumulator and `group_n` counter
- `pov_line_sim_val` bake-in (poverty line moves to display time)
- `weight_col_sim` detection (weights stored per-key in `out$weight`)
- The `$preds` field from `hist_sim_result` — no longer storing N×S rows
- Excessive timing/progress messages (simplify to key-level progress only)

### 2d. Future scenario storage

Replaces the block that builds `new_scenarios` from `group_agg`:

```r
for (gk in names(group_data)) {
  meta        <- group_meta[[gk]]
  ssp_pretty  <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
  period_lbl  <- paste0(meta$year_range[1], "-", meta$year_range[2])
  display_key <- paste0(ssp_pretty, " / ", period_lbl)
  new_scenarios[[display_key]] <- list(
    models      = group_data[[gk]]$models,      # list of M, each with y_point + F_loading + name
    sim_year    = group_data[[gk]]$sim_year,
    weight      = group_data[[gk]]$weight,
    id_vec      = group_data[[gk]]$id_vec,
    weather_raw = group_weather_rep[[gk]],
    so          = so,
    year_range  = meta$year_range,
    n_models    = length(group_data[[gk]]$models)
  )
}
rm(group_data, group_weather_rep, group_meta)
gc(verbose = FALSE)
```

### 2e. Return API

No change to the reactive return structure:
```r
list(
  hist_sim       = function() hist_sim(),
  saved_scenarios = function() saved_scenarios()
)
```

But the **schema** of what these reactives contain changes as described above.

------------------------------------------------------------------------

## Step 3: Results display — `R/mod_2_02_results.R`

### 3a. `agg_hist` reactive

**What:** Instead of filtering pre-aggregated `$agg`, call `aggregate_with_uncertainty()` on the stored `y_point` and `F_loading`.

**Where:** Replace the existing `agg_hist` reactive in `mod_2_02_results.R`.

```r
agg_hist <- reactive({
  req(hist_sim())
  h      <- hist_sim()
  method <- input$cmp_agg_method %||% "mean"

  agg <- aggregate_with_uncertainty(
    y_point         = h$y_point,
    F_loading       = h$F_loading,
    group_vec       = h$sim_year,
    so              = h$so,
    agg_method      = method,
    weights         = if (isTRUE(input$use_weights)) h$weight else NULL,
    pov_line        = if (method %in% c("headcount_ratio", "gap", "fgt2"))
                        as.numeric(input$cmp_pov_line) else NULL,
    train_resid     = h$train_data$.resid,
    residual_method = h$residual_method,
    id_vec          = h$id_vec,
    S               = as.integer(input$sim_n %||% 200)
  )

  # Deviation (optional)
  deviation <- input$cmp_deviation %||% "none"
  if (!identical(deviation, "none") && nrow(agg) > 0) {
    agg <- deviation_from_centre(agg, "sim_year", deviation, loss = FALSE)
    if ("deviation" %in% names(agg)) {
      agg$value <- agg$deviation; agg$deviation <- NULL
    }
  }

  x_label <- if (identical(deviation, "none")) label_agg_method(method)
             else paste0(label_agg_method(method), " — ", label_deviation(deviation))

  list(out = agg, x_label = x_label)
})
```

**Key behaviour:** Reactively recomputes when user changes aggregation method, poverty line, weights, or number of draws. No re-simulation needed.

### 3b. `agg_scenarios` reactive

**What:** For each future scenario, aggregate each model separately via `aggregate_with_uncertainty()`, then combine via `combine_ensemble_results()`.

**Where:** Replace the existing `agg_scenarios` reactive in `mod_2_02_results.R`.

```r
agg_scenarios <- reactive({
  sc <- saved_scenarios()
  if (length(sc) == 0) return(list())
  h      <- hist_sim()
  method <- input$cmp_agg_method %||% "mean"
  deviation <- input$cmp_deviation %||% "none"

  x_label <- if (identical(deviation, "none")) label_agg_method(method)
             else paste0(label_agg_method(method), " — ", label_deviation(deviation))

  result <- lapply(sc, function(s) {
    tryCatch({
      per_model <- lapply(s$models, function(mod) {
        aggregate_with_uncertainty(
          y_point         = mod$y_point,
          F_loading       = mod$F_loading,
          group_vec       = s$sim_year,
          so              = s$so,
          agg_method      = method,
          weights         = if (isTRUE(input$use_weights)) s$weight else NULL,
          pov_line        = if (method %in% c("headcount_ratio", "gap", "fgt2"))
                              as.numeric(input$cmp_pov_line) else NULL,
          train_resid     = h$train_data$.resid,
          residual_method = h$residual_method,
          id_vec          = s$id_vec,
          S               = as.integer(input$sim_n %||% 200)
        )
      })

      combined <- combine_ensemble_results(per_model)

      # Deviation relative to historical baseline
      if (!identical(deviation, "none") && nrow(combined) > 0) {
        hist_ref <- agg_hist()$out
        ref_val  <- if (identical(deviation, "mean")) mean(hist_ref$value, na.rm = TRUE)
                    else if (identical(deviation, "median")) median(hist_ref$value, na.rm = TRUE)
                    else NA_real_
        if (!is.na(ref_val)) {
          combined$value     <- combined$value - ref_val
          combined$value_p05 <- combined$value_p05 - ref_val
          combined$value_p95 <- combined$value_p95 - ref_val
          combined$model_min <- combined$model_min - ref_val
          combined$model_max <- combined$model_max - ref_val
          combined$model_values <- lapply(combined$model_values, function(v) v - ref_val)
        }
      }

      list(out = combined, x_label = x_label)
    }, error = function(e) {
      message("[agg_scenarios] ERROR: ", conditionMessage(e))
      NULL
    })
  })
  result
})
```

### 3c. Restore editable poverty line

**What:** Replace read-only `helpText` (currently showing the baked-in poverty line value) with a `numericInput` so the user can change the poverty line post-simulation.

**Where:** In the UI function of `mod_2_02_results.R`, find the poverty line display element.

```r
numericInput(ns("cmp_pov_line"), "Poverty line",
             value = hist_sim()$so$pov_line %||% 1.90,
             min = 0, step = 0.1)
```

### 3d. Deviation

Apply `deviation_from_centre()` **after** `aggregate_with_uncertainty()` / `combine_ensemble_results()` returns. Already shown in 3a and 3b code above. Not inside the aggregation function.

------------------------------------------------------------------------

## Step 4: Exceedance plot — `R/fct_sim_compare.R`

### 4a. Modify `enhance_exceedance()` to show uncertainty bands

**What:** Add shaded ribbon bands around the ECDF curves to represent:
- **Historical:** Coefficient uncertainty band only (shaded region around black curve)
- **Future scenarios:** Two nested bands:
  - Inner shaded band: coefficient uncertainty (parameter SE)
  - Outer shaded band: model spread (GCM disagreement)
  - Line: median across models (not mean)

**Where:** The `enhance_exceedance()` function in `fct_sim_compare.R`.

**How the data flows in:**

The `scenarios` list passed to `enhance_exceedance()` now contains tibbles with:
- Historical: `value`, `value_p05`, `value_p95` (from `aggregate_with_uncertainty()`)
- Future: `value` (multi-model median), `value_p05`/`value_p95` (avg parameter uncertainty), `model_min`/`model_max` (GCM spread), `model_values` (list-column of M values per year)

**Algorithm for exceedance bands:**

For each scenario group:

1. **Central ECDF** (the line):
   - Historical: ECDF of `value` across sim_years
   - Future: ECDF of `value` (multi-model median) across sim_years

2. **Coefficient uncertainty band** (inner ribbon):
   - Construct two ECDFs: one from all `value_p05` values, one from all `value_p95` values
   - At each exceedance probability y, the x-range between these two ECDFs defines the horizontal ribbon width
   - Implementation: for a shared grid of exceedance probabilities, find the corresponding x-quantiles from each ECDF, then `geom_ribbon()`

3. **Model spread band** (outer ribbon, future only):
   - Construct two ECDFs: one from all `model_min` values, one from all `model_max` values
   - Same approach: at each exceedance probability, the x-range defines the outer ribbon

**Implementation sketch:**

```r
# For each group, build ECDF data with band columns
build_ecdf_with_bands <- function(out_df, is_historical) {
  # Shared probability grid
  probs <- seq(0.01, 0.99, by = 0.01)

  # Central line
  x_central <- quantile(out_df$value, 1 - probs, na.rm = TRUE)

  result <- data.frame(exceed = probs, value = x_central)

  # Coefficient uncertainty band (inner)
  if ("value_p05" %in% names(out_df) && !all(is.na(out_df$value_p05))) {
    result$coef_lo <- quantile(out_df$value_p05, 1 - probs, na.rm = TRUE)
    result$coef_hi <- quantile(out_df$value_p95, 1 - probs, na.rm = TRUE)
  }

  # Model spread band (outer, future only)
  if (!is_historical && "model_min" %in% names(out_df)) {
    result$model_lo <- quantile(out_df$model_min, 1 - probs, na.rm = TRUE)
    result$model_hi <- quantile(out_df$model_max, 1 - probs, na.rm = TRUE)
  }

  result
}
```

**ggplot layers** (added before `geom_line()`):

```r
# Outer band: model spread (future only) — lighter, wider
if ("model_lo" %in% names(ecdf_df)) {
  p <- p + ggplot2::geom_ribbon(
    data = ecdf_df[!is.na(ecdf_df$model_lo), ],
    ggplot2::aes(x = NULL, ymin = model_lo, ymax = model_hi,
                 fill = ssp_key, y = NULL),
    alpha = 0.15, colour = NA
  )
}

# Inner band: coefficient uncertainty — darker, narrower
if ("coef_lo" %in% names(ecdf_df)) {
  p <- p + ggplot2::geom_ribbon(
    data = ecdf_df[!is.na(ecdf_df$coef_lo), ],
    ggplot2::aes(x = NULL, ymin = coef_lo, ymax = coef_hi,
                 fill = ssp_key, y = NULL),
    alpha = 0.25, colour = NA
  )
}
```

**Note on coord_flip():** The existing plot uses `coord_flip()` — value on x-axis, exceedance probability on y-axis. The ribbons must account for this: `ymin`/`ymax` in the `aes()` correspond to the horizontal range after flipping. The implementer should test that ribbons render correctly with `coord_flip()`. If problematic, switch to building the plot in the flipped orientation natively (exceedance on x, value on y) and drop `coord_flip()`.

**Legend addition:** Add a legend entry explaining the bands:

```r
p <- p + ggplot2::labs(
  caption = "Dark band: coefficient uncertainty (90% CI)\nLight band: climate model spread"
)
```

### 4b. Update `enhance_exceedance()` signature

Add parameter to control band display:

```r
enhance_exceedance <- function(scenarios, hist_agg, x_label,
                               return_period = TRUE, n_sim_years = NULL,
                               logit_x = FALSE,
                               show_bands = TRUE)
```

When `show_bands = FALSE`, skip ribbon layers (backward compatible).

------------------------------------------------------------------------

## Step 5: Module 3 — `R/mod_3_06_results.R`

### 5a. Update `agg_one()`

**What:** Replace `aggregate_sim_preds()` call with `aggregate_with_uncertainty()`.

**Where:** The `agg_one()` helper function in `mod_3_06_results.R`.

```r
agg_one <- function(sim_result, so, method, weights, pov_line,
                    train_resid, residual_method, id_vec, S) {
  aggregate_with_uncertainty(
    y_point         = sim_result$y_point,
    F_loading       = sim_result$F_loading,
    group_vec       = sim_result$sim_year,
    so              = so,
    agg_method      = method,
    weights         = weights,
    pov_line        = pov_line,
    train_resid     = train_resid,
    residual_method = residual_method,
    id_vec          = id_vec,
    S               = S
  )
}
```

### 5b. Policy pipeline

`run_policy_pipeline()` calls `run_sim_pipeline()` on policy-modified survey data. Pass `chol_Sigma` through — it's a model property, unchanged by policy interventions. `X_nonFE` is recomputed from the policy-modified newdata inside `run_sim_pipeline()`, so the factor loadings correctly reflect the modified covariates.

### 5c. Weight detection

Replace `"weight" %in% names(baseline_hist_sim()$preds)` with `!is.null(baseline_hist_sim()$weight)`.

------------------------------------------------------------------------

## Step 6: Diagnostics — `R/mod_2_03_diagnostics.R`

- **Ridge plot**: Re-enable using `y_point` (+ optionally drawn residuals, exp'd if log) for the historical welfare distribution. The `y_point` vector is stored in `hist_sim()$y_point`.
- **Uncertainty decomposition** (`plot_uncertainty_decomposition()` in `fct_sim_compare.R`):
  - Update `.decompose_scenario_uncertainty()` to handle the new output schema:
    - `$total`: for historical, use per-year `value` vector; for future, use all `model_values` (list-column) × years
    - `$annual`: mean across models per year
    - `$model`: mean across years per model (from `model_values`)
    - `$coef_lo`/`$coef_hi`: from `value_p05`/`value_p95`
  - The decomposition plot structure remains the same (4 facets: Annual variability, Model uncertainty, Coefficient uncertainty, Combined)
- **Scenario aggregation reactive**: Update to call `aggregate_with_uncertainty()` + `combine_ensemble_results()` (same pattern as `agg_scenarios` in Step 3b).

------------------------------------------------------------------------

## Step 7: Cleanup

- Remove `draw_coefs()` from `R/fct_simulations.R` and NAMESPACE
- Remove `predict_outcome_vectorised()` from `R/fct_predict_outcomes.R` and NAMESPACE
- Remove `beta_override` parameter and its implementation block from `predict_outcome()` in `R/fct_predict_outcomes.R`
- Remove `aggregate_sim_preds()` from `R/fct_simulations.R` and NAMESPACE
- Remove `draw_id` column handling everywhere (search for `draw_id` across all R files)
- Remove `$agg` field from schema references in mod_2_02, mod_2_03, mod_3_06
- Remove the `lapply(agg_methods_all, ...)` pre-aggregation blocks in mod_2_01
- Remove `slim` parameter from `run_sim_pipeline()`
- Add `compute_chol_vcov`, `aggregate_with_uncertainty`, `combine_ensemble_results` to NAMESPACE
- Clean up duplicate comment blocks in `fct_predict_outcomes.R`

------------------------------------------------------------------------

## Files Modified

| File | Summary |
|-----|---------|
| `R/fct_simulations.R` | Change `COEF_VCOV_SPEC` to `~code + year + survname + loc_id`. Add `compute_chol_vcov()`, `aggregate_with_uncertainty()`, `combine_ensemble_results()`. Rewrite `run_sim_pipeline()` to use factor-loading path. Remove `draw_coefs()`, `aggregate_sim_preds()`. |
| `R/fct_predict_outcomes.R` | Remove `predict_outcome_vectorised()`. Remove `beta_override` param and delta-method block from `predict_outcome()`. Clean up duplicate comments. |
| `R/mod_2_01_weathersim.R` | Replace `draw_coefs()` with `compute_chol_vcov()`. Remove pre-aggregation loops. Store compact `(y_point, F_loading)` per model. Accumulate per-model results for ensemble scenarios. |
| `R/mod_2_02_results.R` | Rewrite `agg_hist`/`agg_scenarios` to call `aggregate_with_uncertainty()` + `combine_ensemble_results()`. Restore editable poverty line input. |
| `R/fct_sim_compare.R` | Add shaded uncertainty bands to `enhance_exceedance()`: inner band for coefficient uncertainty, outer band for model spread. Update `.decompose_scenario_uncertainty()` for new schema. |
| `R/mod_3_06_results.R` | Update `agg_one()` for new schema. Pass `chol_Sigma` through policy pipeline. Fix weight detection. |
| `R/mod_2_03_diagnostics.R` | Update scenario aggregation reactive. Re-enable ridge plot from `y_point`. Update decomposition for new schema. |
| `NAMESPACE` | Export `compute_chol_vcov`, `aggregate_with_uncertainty`, `combine_ensemble_results`. Remove `draw_coefs`, `predict_outcome_vectorised`, `aggregate_sim_preds`. |

------------------------------------------------------------------------

## Key Decisions

1. **Clean replace**: Remove old draw loop, beta_override, draw_coefs(), predict_outcome_vectorised(), aggregate_sim_preds(), $agg schema. No backward-compat shims. Old code preserved in git history.
2. **SE clustering**: `~code + year + survname + loc_id` — clusters at the survey-location group level. More conservative than `~loc_id` alone. Propagates automatically through Cholesky factor to all uncertainty bands.
3. **Residuals drawn at aggregation time**: Store `train_resid` + `residual_method` (from the model fit). `aggregate_with_uncertainty()` draws fresh residuals per call. This means changing aggregation method/weights doesn't require re-simulation, and avoids the averaging-across-models variance shrinkage bug.
4. **Log-transformed outcomes**: Always Monte Carlo path (even for mean). `exp()` applied inside `aggregate_with_uncertainty()` per z-draw. Handles Jensen's inequality correctly — `E[exp(y)]` for correlated log-normals has no closed form.
5. **Ensemble spread preserved**: Per-model `(y_point, F_loading)` stored separately — no pooling/averaging. Each model is aggregated independently at display time. Two uncertainty layers: inner band (parameter uncertainty averaged across models) and outer band (inter-model spread as min/max). Memory cost is M x per scenario group (M typically 5-30).
6. **S parameter**: Kept in UI. Controls K-dimensional draws for non-linear aggregates and log outcomes. Unused for analytic path (linear aggregates on non-log outcomes). Default 200.
7. **Exceedance plot bands**: Historical shows one band (coefficient uncertainty, darker fill). Future shows two nested bands (inner = coefficient uncertainty, outer = model spread, lighter fill). Central line is median across models for future, point estimate for historical.
8. **Non-fixest engines**: `F_loading = NULL` -> point estimates only, no uncertainty bands.

------------------------------------------------------------------------

## Verification

1. **Unit test** — new `dev/test_factor_loading.R`:
   - `F %*% t(F) ~ X %*% Sigma %*% t(X)` within tolerance
   - Analytic mean CI matches Monte Carlo CI (S=10000) within 1%
   - Non-linear aggregate (headcount) produces p05 <= p50 <= p95
   - Log path: `exp(y + F %*% z + resid)` correct mean vs. smearing estimator
   - Per-model aggregation: M model results combine correctly via `combine_ensemble_results()`
   - Inner band (parameter uncertainty) narrower than outer band (model spread) when M > 1
   - `COEF_VCOV_SPEC` clustering produces valid VCV (no singular matrix errors)
2. **Integration test** — `wiseapp::run_app()`:
   - Skip uncertainty toggle: bands appear/disappear on exceedance plot
   - Switch aggregation method: reactive, no re-simulation
   - Change poverty line post-hoc: headcount updates without re-simulation
   - Module 3 policy comparison works with new schema
   - Uncertainty decomposition plot shows all 4 facets correctly
   - Exceedance plot shows nested bands for future, single band for historical
   - Exceedance plot ribbons render correctly with `coord_flip()`
3. **Performance**: Seconds per key (1 predict + 1 matmul) vs. minutes under old S-draw loop.
