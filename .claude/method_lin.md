# Linear Regression (fixest): Theory and Implementation in WISE-APP

## 1. Theoretical Background

### Conditional Mean Estimation via OLS

Linear regression estimates the effect of covariates on the **conditional mean** of the outcome distribution, E[Y|X]. The estimator solves:

```
β̂ = argmin_β  Σᵢ (yᵢ − xᵢ′β)²
```

Unlike RIF, which targets unconditional quantiles of Y, OLS produces a single coefficient per covariate that summarises the *average* marginal effect across the entire conditional distribution. Coefficients are interpreted as: *"a one-unit increase in X is associated with a β-unit change in the conditional mean of Y, holding other covariates fixed."*

### Fixed Effects Absorption

WISE-APP uses `fixest::feols()` (Bergé, 2018), which absorbs high-dimensional fixed effects via the Frisch-Waugh-Lovell theorem rather than expanding them into dummy variables. For a model `y ~ x | f1 + f2`, the algorithm iteratively demeans both `y` and `x` by `f1` and `f2` until convergence, then runs OLS on the demeaned data. This makes it tractable to absorb survey-wave, location, year, or household fixed effects with thousands or millions of levels.

### Interpretation

The coefficient β̂ from `feols(y ~ w + x | f)` estimates:

```
β̂_w ≈ ∂E[Y | X, W, FE] / ∂W
```

This is the within-group marginal effect of weather W on the conditional mean of welfare Y, net of time-invariant unobserved heterogeneity absorbed by the fixed effects. Unlike RIF, OLS imposes **a single slope across the welfare distribution** — rich and poor households are assumed to respond to weather identically (up to interaction terms with observed covariates).

### Logistic Regression (Same Engine)

The same `fixest` engine handles **logistic regression** for binary outcomes via `fixest::feglm(..., family = binomial("logit"))`. Coefficients are log-odds rather than marginal effects on the mean. Logistic mode is selected automatically when the outcome is logical or when `selected_model$type == "Logistic regression"`.

---

## 2. Implementation in WISE-APP

### 2.1 Outcome Transformation (`fct_results.R`)

Outcome transformations are applied **before** model fitting via `prepare_outcome_df()` in `fct_results.R` (called from `mod_1_06_model.R`). The pipeline is engine-agnostic:

1. **LCU back-conversion**: multiply by `ppp2021` when `units == "LCU"`.
2. **Log transformation**: apply `log()` when `transform == "log"`.
3. **Binary poor indicator**: create `welfare < povline` when `name == "poor"`.

The engine's own `prepare_outcome()` does nothing for linear regression; for logistic regression it coerces to integer 0/1 (`as.integer(as.logical(df[[y_var]]))`).

**Log-transformation is application-wide, not engine-specific.** The linear engine receives an already-log-transformed outcome when applicable; back-transformation happens later in the simulation pipeline via `apply_log_backtransform()`.

### 2.2 Model Fitting (`fct_fit_model.R`)

The `"fixest"` engine is registered in `ENGINE_REGISTRY` and supports both `"Linear regression"` and `"Logistic regression"` model types. Formulas are built with the absorbing-FE `|` syntax: `y ~ x | fe`.

The same progressive 3-model specification used for RIF applies:

- **Model 1** — Weather only: `y ~ hazard`
- **Model 2** — Weather + interactions + FE: `y ~ hazard + interactions_main | fe`
- **Model 3** — Weather + interactions + covariates + FE: `y ~ hazard + interactions_main + covariates | fe`

Each formula is fit by `fixest::feols()` (or `feglm()` for logistic). When the user has specified cluster variables, they are passed via the `cluster = ~v1 + v2` argument so standard errors are clustered. Unlike RIF, there is **no stacked multi-LHS call** — a single fixest model is returned per specification.

Interactions are configured via `interaction_mode` (default `"pairwise"` per `fct_model_select.R`). In `"pairwise"` mode, explicit `hazard:covariate` pairs are created. In `"saturated"` mode, R's `*` operator expands all lower-order terms automatically (e.g., `temperature * urban` expands to `temperature + urban + temperature:urban`).

### 2.3 Availability

The linear (`fixest`) engine is offered to the user as:

- **Linear regression** — available for numeric (continuous) outcomes, alongside RIF.
- **Logistic regression** — available for binary outcomes, alongside Linear regression.

Engine selection is handled in `R/fct_model_select.R` (`infer_engine()` and `model_type_choices()`).

### 2.4 Results Display (`fct_results.R`)

- **Coefficient plot** (`make_coefplot()`): standard point-range plot of β̂ ± 1.96·SE, faceted by covariate, with one panel per model specification (Model 1/2/3). SEs are extracted via `.fixest_coeftable(fit)`, which uses clustered `~loc_id_panel` SEs with a fallback chain (`~loc_id_panel → ~loc_id → "HC1" → "iid"`), consistent with the simulation pipeline's `COEF_VCOV_SPEC`.
- **Weather effect plot** (`make_weather_effect_plot()`): for continuous weather, predicts the response over a 50-point grid of the weather variable with delta-method confidence ribbons (`sqrt(diag(X V X′))`); for binned weather, plots bin-level coefficients with the omitted reference category labelled in the caption. Interactions with moderators are overlaid as multiple lines (at mean / ±SD of the moderator).
- **Regression table** (`make_regtable()`): AER-style HTML table with one column per model specification, coefficients on top and SEs in parentheses below, plus N, R², within-R², and FE indicators.
- **Diagnostics** (`plot_diagnostics()` in `fct_results.R`): residual-vs-fitted scatter with horizontal zero line and LOESS smooth, computed directly from the native fit via `extract_native_fit()`.

### 2.5 Simulation Pipeline (`fct_simulations.R`)

`run_sim_pipeline()` dispatches on engine. For the linear (`fixest`) branch:

1. **Weather joining** (`prepare_hist_weather()`): inner-join raw weather to survey covariates on `(code, year, survname, loc_id, int_month)`.
2. **Prediction**: a single call to `predict_outcome()` which wraps `stats::predict(model, newdata = survey_wd_sim, type = "response")`. Rows with FE levels unseen in training are silently dropped by fixest; `predict_outcome()` retains the `rowids` attribute and trims `newdata` to match.
3. **`y_point` extraction**: captured *before* back-transformation. When `so$transform == "log"`, `y_point` stays in log scale so that `aggregate_with_uncertainty_delta()` can apply `exp()` exactly once at aggregation time (with `is_log = TRUE`).
4. **Back-transform** (`apply_log_backtransform()`): exponentiate the outcome column for the diagnostic `preds` frame.
5. **SP cash transfer**: if a `.wiseapp_sp_transfer` column is present, add it to `preds[[outcome]]` *on the original (level) scale, after back-transform*. Then re-log `y_point` to maintain log-scale consistency for downstream uncertainty propagation.
6. **Factor loadings for uncertainty**: when `chol_obj` (or the legacy `chol_Sigma` alias) is supplied, compute `F_loading = X_nonFE %*% L` via `compute_factor_loading()`, where `L` is the Cholesky factor of the coefficient vcov and `X_nonFE` is the non-FE design matrix from `stats::model.matrix(model, ..., type = "rhs")`. `F_loading` (N × K) carries coefficient uncertainty forward into the closed-form delta-method aggregator (see §2.6). **Default scope:** under `residuals = "original"` the formula switches to `F_loading = X_nonFE[, active] %*% L_active` where `L_active = chol(Σ[active, active])'` is the lower-triangular Cholesky factor of the active block of Σ (pre-computed by `attach_active_mask()` in `fct_results.R`). This gives an N × K_active matrix whose Gramian is `X_a Σ_aa X_a'` — the correct additive-decomposition variance. *Note:* a naive `F[:, active]` subset of `F = X %*% L` would NOT give the same quantity when Σ has off-diagonals between active and inactive coefficients. Active columns are coefficients on variables that change between baseline and counterfactual — weather and weather-interactions in Module 2, plus policy-modified variables and their interactions in Module 3. The "Include uncertainty on all covariates" toggle in `mod_2_01_weathersim.R` reverts to the full N × K shape (legacy behaviour). See [`method_uncertainty.md`](method_uncertainty.md) §1.1.1 for the cancellation derivation.

#### Weather-only simulation

Straightforward: counterfactual weather is joined to the baseline survey, fed through `predict()`, and the resulting `ŷ_scen − ŷ_base` is the weather effect. No quantile interpolation, no delta method on coefficients — just a difference of two predictions.

#### Policy scenario simulation

The linear engine relies on **`fixest::predict()` evaluated on the policy-modified survey** to capture *all* policy channels simultaneously. There is no separate Stage A / Stage B correction as in RIF:

1. `apply_policy_to_survey()` (in `fct_policy_sim.R`) constructs `svy_policy` by flipping the policy covariates (e.g., turning on irrigation, electrification, formal employment) and tags eligible households with `.wiseapp_sp_transfer`.
2. `run_sim_pipeline()` is called with `svy = svy_policy` and `model = fit3` (the fully-specified model).
3. `fixest::predict()` evaluates the full linear predictor `x_policy′β̂ + FE`, so:
   - **Main effects** of changed covariates enter through `β_v · x_policy_v`.
   - **Weather × covariate interactions** activate automatically because the design matrix uses policy values (e.g., `drought × irrigation = drought_baseline × irrigation_post`).
4. SP cash is added post-backtransform in levels (same protocol as RIF, see §2.1).

**No `.compute_ols_policy_correction()` exists** — the prediction-space approach already captures every channel the model can represent. The policy correction helper (`.compute_rif_policy_correction()`) is RIF-specific because RIF's `predict_rif()` only computes a weather delta and needs covariate effects added back in.

### 2.6 Uncertainty Propagation (`fct_aggregation_delta.R`)

**The app uses a closed-form delta-method aggregator.** All callsites (`mod_2_02_results.R`, `fct_policy_sim_compare.R`) use `aggregate_with_uncertainty_delta()`. Monte Carlo aggregation has been removed.

Given the point-estimate log-welfare vector `y_point`, the factor loading `F_loading` (N × K, or N × K_active when the additive-decomposition mask is on — see §2.5 step 6), and an aggregate statistic `T = g(welfare)`, the delta method computes:

```
welfare_i = exp(y_point_i + resid_i)        # back-transform (is_log = TRUE)
h_i       = (∂T/∂welfare_i) · welfare_i      # welfare-scale gradient, lifted to log scale
F_agg     = F_loading' h                     # K(_active)-vector: per-coefficient gradient of T
var_coef  = ||F_agg||²                       # coefficient-variance contribution
var_resid = σ_e² · Σ h_i²                    # residual-variance (only for "normal" / "resample")
SE        = sqrt(var_coef + var_resid)
```

The aggregator is agnostic to whether `F_loading` has been subset — the masking happens upstream in `compute_factor_loading()`, and paired contrasts inherit it automatically because both arms share the same `chol_obj$active_mask`.

Bands are reported via `apply_band_transform()`, which applies a method-appropriate link before adding `z_q · SE`:
- **logit** scale for bounded ratios (`headcount_ratio`, `gap`, `fgt2`) to keep bands within [0, 1]
- **log** scale for strictly positive aggregates (`median`)
- **identity** for unbounded aggregates (`mean`, `total`, `gini`, `prosperity_gap`, `avg_poverty`)

Per-method gradient closures `gradient_for_method()` cover `mean`, `total`, `gap`, `prosperity_gap`, `fgt2`, `headcount_ratio`, `avg_poverty`, `median`, and `gini`. The headcount ratio uses a kernel-smoothed indicator `Φ((z − w)/b)` with bandwidth auto-tuned to the larger of `bandwidth_p0 · z_p` and the median per-obs welfare SE — undersmoothing under-counts threshold crossings as F_loading shrinks.

**Ensemble pooling** (`combine_ensemble_results()` in `fct_aggregation.R`) combines per-CMIP6-member outputs via the law of total variance:

```
var_within = mean(var_coef + var_resid)     # parametric uncertainty averaged across members
var_across = var(member point estimates)    # inter-model spread
var_pool   = var_within + var_across
```

The thin "coef" band reflects `var_pool` (analytic Gaussian); the thick "model spread" band reflects empirical percentiles of the member point estimates. Output keys include `value`, `value_lo`/`value_hi` (model-spread band), `coef_lo`/`coef_hi` (analytic pooled), `model_q10`/`model_q25`/`model_med`/`model_q75`/`model_q90`, and `value_all` (per-member point values).

**For paired counterfactuals** (e.g., scenario − historical on the same population), `aggregate_with_uncertainty_delta()` returns `F_agg` so callers can build contrast variance as `||F_agg_scn − F_agg_hist||²` rather than summing variances independently — this captures the correlation between the two predictions sharing the same coefficient draw.

### 2.7 Policy Decomposition (`fct_policy_decompose.R`)

The decomposition output is a *display/reporting* artifact, not a correction applied to predictions. `decompose_policy_effect()` dispatches on engine: linear calls `.decompose_ols()`, RIF calls `.decompose_rif()` (which delegates to `.compute_rif_channels()`). The linear decomposition reports the following channels:

##### Channel 1: Main effect (δ_main = δ_sp + δ_main_covar)

1. **SP cash transfer (δ_sp):** on log scale,
   ```
   δ_sp = log(exp(y_baseline) + SP_cash) − y_baseline
   ```
   on level scale, `δ_sp = SP_cash`.

2. **Covariate shift (δ_main_covar):** for each changed covariate v with Δv = x_policy − x_baseline,
   ```
   δ_main_covar += β_v · Δv
   ```
   where β_v is a single scalar coefficient from `fit3` (no τ-dependence). Factor-expanded names (e.g., `urban1`) are fuzzy-matched via `grep("^v", coef_names)` when the bare name is absent.

##### Channel 2: Interaction effect (δ_res2)

For each weather × policy-covariate pair, evaluated at the household's realised weather value:

- **Continuous weather:** `δ_res2 += β_{w:v} · w_hh · Δv`, where `w_hh` is the per-location mean weather joined to each household by `loc_id` (falling back to the grand mean if `loc_id` is missing).
- **Binned weather:** `δ_res2 += β_{w_lv:v} · 1(active bin) · Δv`, only for the modal bin per location.

Coefficient names are matched in both orderings (`w:v` and `v:w`) with a regex fallback (`^w[^:]*:v|^v[^:]*:w`). A warning is emitted if no interaction terms are found.

##### Channel 3: Repositioning (δ_res1) — **always zero for OLS**

OLS imposes a constant slope across the welfare distribution, so there is no repositioning channel. `delta_res1 = rep(0, n)` and `pct_res1 = rep(0, n)`. This is the key methodological gap relative to RIF: a cash transfer in the linear engine cannot reduce a household's weather sensitivity simply by raising its welfare level. The only way OLS captures heterogeneity in weather response is through explicit weather × covariate interaction terms in the model specification.

##### What the decomposition returns

The decomposition data frame includes `delta_main`, `delta_sp`, `delta_main_covar`, `delta_res1 = 0`, `delta_res2`, `delta_res = delta_res2`, and `delta_total = delta_main + delta_res2`, along with percentage versions `pct_main`, `pct_res1 = 0`, `pct_res2`, `pct_total` computed via `(exp(δ) − 1) · 100` (log-scale → percent). Households are assigned to deciles based on baseline welfare (`ecdf(y_baseline)(y_baseline)`), but the decile label is purely descriptive — it does not feed into any coefficient lookup, unlike RIF's τ.

**Crucial consistency note:** The simulation prediction (via `fixest::predict()` on `svy_policy`) and the decomposition (via `β · Δx` in `.decompose_ols()`) should yield identical total effects for simple OLS without fixed effects. With FE absorbed, they agree on the within-group change in expectations but differ in level (FE contributions appear in `predict()` but not in the β·Δx decomposition). The decomposition is therefore best read as *the marginal effect of the policy holding FE fixed*, not as an exact reconciliation with predicted welfare levels.

### 2.8 Conceptual Issues with the Policy Simulation

#### a) No distributional heterogeneity in weather response

The linear engine imposes a single weather coefficient across the entire welfare distribution. Climate impacts are assumed constant for rich and poor households unless the user explicitly includes weather × wealth interactions in the model. This is the most fundamental difference from RIF: it understates inequality in climate vulnerability whenever the true response varies across the distribution in ways not captured by observable interaction terms.

#### b) No indirect resilience effect of cash transfers

Because there is no repositioning channel, SP cash transfers can only build resilience through *direct interactions* with weather (e.g., a `cash × drought` term in the model). The "second-order" resilience effect that RIF captures automatically — wherein moving a household up the welfare distribution reduces its weather sensitivity — is invisible to the linear engine. Policy analyses that compare engines should expect smaller resilience effects under OLS.

#### c) Prediction vs. decomposition mismatch under FE

`fixest::predict()` returns full predictions including absorbed FE contributions; `.decompose_ols()` returns `β · Δx` using only the visible (non-FE) coefficients. For policy changes that do not interact with FE-absorbed structure, both routes give the same Δ, and the simulation pipeline (which uses `predict()`) is the authoritative number. The decomposition table is for *channel attribution*, not exact reconciliation with simulation outputs.

#### d) SP transfer scale mismatch

As with RIF, SP cash is added post-backtransform in levels (`preds[[outcome]] + .wiseapp_sp_transfer`), while the decomposition reports δ_sp on the model scale (log). Users comparing decomposition percentages to simulation outputs should be aware of this distinction; the percentage `(exp(δ_sp) − 1) · 100` reflects the proportional welfare lift, not the absolute dollar transfer.

#### e) Sensitivity to outliers and tail behaviour

OLS minimises squared residuals, so estimates are pulled toward fitting the densely-populated centre of the welfare distribution and can be insensitive to (or distorted by) tail observations. Where RIF estimates quantile-specific β_τ values that may differ markedly at τ = 0.1 vs τ = 0.9, the linear engine averages these into a single coefficient that may be a poor representation of either tail. This is especially relevant for poverty- and vulnerability-focused analyses, which are concerned precisely with the lower tail.

#### f) Missing interaction terms produce silent attribution failures

If the user does not include weather × covariate interactions in Model 3, `δ_res2 = 0` and the decomposition attributes the entire policy effect to δ_main_covar — even when the policy is meant to operate primarily through weather moderation (e.g., irrigation buffering drought). A warning is emitted when interactions are absent, but the simulation runs without error. Users must verify that the model specification matches the policy theory of change.

---

## 3. Assumptions

1. **Linearity in covariates**: The conditional mean E[Y|X] is a linear function of X (after any user-specified polynomial or interaction expansions). Misspecification of functional form biases all coefficients.

2. **Exogeneity / no endogenous regressors**: E[ε|X] = 0 — covariates are uncorrelated with the residual. Fixed effects absorb time-invariant unobserved heterogeneity, but time-varying confounders are untreated.

3. **Homogeneous slopes across the distribution**: A single β applies to all households regardless of welfare level. Heterogeneity must be modelled explicitly via interactions; it is not estimated automatically.

4. **No perfect multicollinearity**: Linearly dependent columns are dropped by fixest (with a warning).

5. **Independence (or correct clustering) of residuals**: SEs assume independence within clusters and arbitrary dependence across. Cluster choice is the user's responsibility; misclustering distorts inference.

6. **Stable functional form under counterfactuals**: The simulation assumes the fitted β estimated from observed weather variation extrapolates to scenario weather. Large climate shocks may push predictors outside the support of the training data.

7. **FE structure stable under policy**: Predictions assume policy changes do not alter the FE levels themselves (no new locations, years, or survey waves). Households in unseen FE levels are silently dropped by `fixest::predict()`.

---

## 4. Limitations

1. **No distributional effects**: Cannot identify whether weather harms the poor more than the rich without explicit interactions. This is the primary motivation for offering RIF as an alternative.

2. **No automatic resilience-via-welfare channel**: Cash transfers cannot reduce weather vulnerability except through pre-specified interaction terms.

3. **Single-summary inference**: One coefficient per covariate, one SE, one p-value. Heterogeneity diagnostics (e.g., are weather effects stable across quantiles?) require fitting a separate model (e.g., RIF or quantile regression).

4. **Silent row-dropping under FE**: `fixest::predict()` drops rows with FE levels absent from training. For policy simulation on a different population, this can silently remove households from the analysis without flagging.

5. **OLS sensitivity to outliers**: Squared-loss estimator is non-robust. A small number of extreme welfare observations can move all coefficients.

6. **Log-transform interpretation**: When the outcome is `log(welfare)`, coefficients are approximately proportional effects (semi-elasticities), but back-transformed predictions `exp(x′β)` are biased estimates of E[welfare|X] unless a Duan smearing or similar correction is applied — which WISE-APP does not do. The bias is small for low-variance residuals but can be substantial for highly variable welfare.

---

## 5. Suggestions for Improvement

1. ~~**Heteroskedasticity-robust SEs by default**~~ **[IMPLEMENTED]**: All display functions (`make_coefplot()`, `make_weather_effect_plot()`, `make_regtable()`) now use `.fixest_coeftable()` / `.fixest_vcov()` with clustered `~loc_id_panel` SEs and a fallback chain (`~loc_id_panel → ~loc_id → "HC1" → "iid"`), matching the simulation pipeline's `COEF_VCOV_SPEC`. The helpers are defined in `fct_results.R`.

2. **Duan smearing for log-transformed predictions**: When `so$transform == "log"`, apply the Duan smearing correction `exp(x′β̂) · mean(exp(ε̂))` to obtain unbiased estimates of E[welfare|X]. The current `exp(.)` back-transform underestimates the mean.

3. **Heterogeneity diagnostics**: Provide a tab that fits the same Model 3 specification by welfare decile (or with weather × decile interactions) and overlays the resulting effect estimates. This would flag specifications where the homogeneity assumption is violated and signal that RIF might be more appropriate.

4. **Explicit warning on dropped rows**: When `fixest::predict()` silently drops rows due to unseen FE levels, the simulation should report the count and percentage to the user. Currently this is buried in a warning that may be missed.

5. **Wild cluster bootstrap**: For small cluster counts, asymptotic cluster-robust SEs are anti-conservative. Offering a wild bootstrap option (`fwildclusterboot` package) would improve inference in surveys with few clusters.

6. **Decomposition / prediction reconciliation**: Add a diagnostic showing the gap between `Σ channels` from the decomposition and the actual `predict(svy_policy) − predict(svy_baseline)` from the simulation, to help users detect when FE absorption causes the decomposition to mis-attribute effects.

7. **Built-in weather × decile interactions option**: A toggle that auto-adds weather × welfare-decile interactions in Model 3, providing a lightweight alternative to RIF for users who want distributional effects without switching engines.

---

## References

- Bergé, L. (2018). Efficient estimation of maximum likelihood models with multiple fixed-effects: the `R` package `FENmlm`. *CREA Discussion Papers*, 2018-13.
- Cameron, A. C., & Trivedi, P. K. (2005). *Microeconometrics: Methods and Applications*. Cambridge University Press.
- Duan, N. (1983). Smearing estimate: a nonparametric retransformation method. *Journal of the American Statistical Association*, 78(383), 605–610.
- Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel Data* (2nd ed.). MIT Press.
