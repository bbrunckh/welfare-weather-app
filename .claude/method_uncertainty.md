# Uncertainty in WISE-APP Simulations: Sources, Propagation, and Analytic Standard Errors

This document describes how each source of uncertainty enters the WISE-APP simulation pipeline, how the closed-form delta-method aggregator translates them into standard errors and percentile bands, and the assumptions behind the analytic formulae for each aggregation option.

It complements [`method_lin.md`](method_lin.md) §2.6 and [`method_rif.md`](method_rif.md) §2.6, which describe engine-specific construction of the factor-loading matrix `F_loading`. All uncertainty propagation flows through `aggregate_with_uncertainty_delta()` in `fct_aggregation_delta.R` — the closed-form delta-method aggregator. Monte Carlo simulation has been removed from the codebase.

---

## 1. Sources of Uncertainty

WISE-APP recognises four orthogonal sources of uncertainty, propagated separately and composed via the law of total variance.

### 1.1 Coefficient uncertainty (parametric / sampling)

The regression coefficients `β̂` are random variables with covariance matrix `Σ = Var(β̂)`. Their uncertainty propagates into household-level predictions through the design matrix.

**Mechanism.** `compute_chol_vcov(fit)` extracts `Σ` from the fitted model via `fixest::vcov(fit, vcov = COEF_VCOV_SPEC)`. The default spec is `~loc_id` (cluster-robust at location), with a fallback chain `~loc_id → "HC1" → "iid"` if the requested spec fails. Two alternative constants are also defined: `COEF_VCOV_SPEC_MOULTON = ~loc_id:int_month` and `COEF_VCOV_SPEC_CONSERVATIVE = ~code + year + survname + loc_id`. The Cholesky factor `L` such that `L L' = Σ` is returned as part of `chol_obj`.

**Factor loading.** For each household `i` in the counterfactual survey, the non-FE design vector `x_i` is multiplied by `L`:

```
F_loading[i, ] = x_i' L      ∈ ℝ^K
```

where `K` is the number of non-FE coefficients (typically 5–20). This is computed in `compute_factor_loading(X_nonFE, chol_obj)`. The geometric meaning: under a standard-normal draw `z ~ N(0, I_K)`, the perturbed log-welfare for household `i` is

```
y_i^(s) = y_i^point + F_loading[i, ] · z_s,    with    Var(F_loading[i, ] · z) = x_i' Σ x_i
```

which is exactly the usual delta-method variance of a linear predictor. WISE-APP never draws `z` at runtime — the Cholesky factor is the only object needed for the closed-form variance below.

**RIF engines.** For RIF (`fixest_multi`), `compute_chol_vcov` returns a list of K Cholesky factors `{L_k}`, one per quantile fit. `interpolate_F_loading()` linearly blends `X_diff_k · L_k` across the quantile grid at each household's interpolated `τ_i`. See [`method_rif.md`](method_rif.md) §2.6.

### 1.2 Residual / idiosyncratic uncertainty

Training residuals `ε̂_i = y_i − x_i' β̂` capture variation in welfare not explained by the model. The application default is **`"original"`** — each simulation household is matched to its own training residual by ID. The UI exposes this on the Step 2 sidebar (`mod_2_01_weathersim.R`), and the function-signature default for `aggregate_with_uncertainty_delta()` is also `"original"`.

**Options** (handled by `draw_residuals_vec()` in `fct_aggregation.R`):

- `"original"` *(default)* — match each simulation household to its training residual by ID column; unmatched households fall back to a resampled training residual. Preserves individual-level heterogeneity across simulation years.
- `"resample"` — non-parametric: sample with replacement from the empirical residual distribution.
- `"normal"` — i.i.d. draws `ε_i ~ N(0, σ̂_ε²)` with `σ̂_ε² = var(ε̂)`. Assumes normal tails and homoskedasticity.
- `"none"` — diagnostic only: `resid_vec = 0`. Predictions reflect coefficient uncertainty only; for log-transformed welfare this *understates the mean* (because `E[exp(ε)] ≠ exp(0) = 1`).

**Residuals are drawn once and broadcast** to all coefficient draws ("Option A"). This is methodologically correct for *isolating* coefficient uncertainty — independent draws of `ε` and `β` are appropriate when the goal is to attribute variance to each source.

**Why `"original"` is the default.** Matching each household to its own residual preserves individual-level idiosyncratic variation (skills, preferences, location-specific shocks unmodelled by the regression) across counterfactual weather and policy scenarios. The implicit assumption is that this idiosyncratic component is invariant to the counterfactual — a plausible approximation for marginal weather perturbations but increasingly strained for large policy shifts that may themselves reshape the residual distribution.

**Variance contribution of `"original"` residuals.** Because matched residuals are deterministic per household (not random draws), they enter the mean but contribute *zero* to `var_resid` in the aggregator. The aggregator's residual variance term `var_resid = σ̂_ε² · Σ h_i²` is non-zero only for the stochastic modes (`"normal"`, `"resample"`); see `fct_aggregation_delta.R` lines 93–97. Under `"original"`, the residual is treated as a fixed (known) shift, and only `var_coef` propagates into the band.

**Automatic safety demotion.** When `train_aug` is `NULL` or lacks a `.resid` column (notably for RIF engines, which set `train_aug = NULL` by construction), `aggregate_with_uncertainty_delta()` silently demotes the request to `"none"` rather than erroring. RIF predicts `y_baseline + δ_i` rather than a model prediction in welfare units, so adding an additive residual on top of the household's *observed* baseline welfare is not meaningful.

### 1.3 Inter-annual weather variability

Climate scenarios deliver multiple simulation years per `(model_id, ssp, year-window)`. Aggregates are reported per `sim_year`, and **inter-annual variability is shown as the quantile spread across years**, not as an analytic variance.

In `mod_2_02_results.R` this appears as the year-axis spread of point estimates. In `fct_policy_sim_compare.R` the same quantile reduction is applied across `sim_year` within each model member. This is non-parametric and makes no Gaussian assumption.

### 1.4 Inter-model (CMIP6) spread

For climate projections, each ensemble member (`model_id`) is one GCM realisation. The pipeline runs `run_sim_pipeline()` independently for each member and aggregates per member; `combine_ensemble_results()` then composes them.

This source is treated **non-parametrically** for the "thick band" (empirical quantiles across members) and **analytically** in the pooled SE (variance of member point estimates).

---

## 2. The Delta-Method Aggregator

`aggregate_with_uncertainty_delta(y_point, F_loading, method, weights, pov_line, residuals, train_aug, …)` implements the closed-form variance propagation in O(N · K) time.

### 2.1 Setup

Define the per-household welfare-scale point estimate

```
μ_i  =  exp(y_point_i + resid_vec_i)     (is_log = TRUE)
μ_i  =  y_point_i + resid_vec_i          (is_log = FALSE)
```

Let `T = g(μ, w, z_p)` denote the aggregate statistic of interest (e.g., mean, headcount, Gini). The first-order Taylor expansion of `T` in `μ` gives

```
T(μ + Δμ) ≈ T(μ) + Σ_i (∂T/∂μ_i) · Δμ_i
```

Each perturbation is `Δμ_i = μ_i · (F_loading[i, ] · z + ε_i)`, where the `μ_i` factor comes from the chain rule when `is_log = TRUE` (perturbing `log μ_i` by `δ` shifts `μ_i` by `μ_i δ`). Define the **welfare-scale gradient lifted to log scale**:

```
h_i  =  (∂T/∂μ_i) · μ_i
```

Then

```
T − T̂ ≈ Σ_i h_i · (F_loading[i, ] · z + ε_i)
      =  (F_loading' h)' · z  +  h' ε
```

Let `F_agg = F_loading' h ∈ ℝ^K` (per-coefficient gradient of `T`). Under `z ~ N(0, I_K)` and `ε_i ~ (0, σ_ε²) i.i.d.`, the variance decomposes as

```
Var(T)  ≈  ||F_agg||²        +  σ_ε² · Σ_i h_i²
        ────────────────────    ─────────────────
        var_coef                 var_resid
```

### 2.2 Implementation

```r
F_agg     = crossprod(F_loading, h)         # K-vector
var_coef  = sum(F_agg^2)                     # ||F_agg||²
var_resid = σ̂_ε² · sum(h^2)                  # only for residuals ∈ {"normal", "resample"}
SE        = sqrt(var_coef + var_resid)
```

Under the default residual mode `"original"`, matched residuals are deterministic shifts: they raise `μ_i = exp(y_point_i + ε̂_i)` (and so reshape the gradient `h_i`) but do not contribute a stochastic variance term — `var_resid = 0`. The stochastic modes `"normal"` and `"resample"` add `σ̂_ε² · Σ h_i²` as shown above.

`F_agg` is returned to the caller so paired-counterfactual contrasts (e.g., scenario − baseline on the same population) can be computed as

```
SE_contrast = || F_agg_scn − F_agg_base ||
```

rather than the conservative `sqrt(var_scn + var_base)` that ignores the shared coefficient draw.

### 2.3 Band transforms (`apply_band_transform()`)

Some aggregates are bounded or strictly positive; building a symmetric Gaussian band around the point estimate would extend outside the natural support. The aggregator applies a method-appropriate link:

| Method | Scale | Transform |
|---|---|---|
| `headcount_ratio`, `gap`, `fgt2` | logit | `lo, hi = invlogit(logit(T) + z_{lo,hi} · SE / (T(1−T)))` |
| `median` | log | `lo, hi = exp(log T + z_{lo,hi} · SE / T)` |
| `mean`, `total`, `gini`, `prosperity_gap`, `avg_poverty` | identity | `lo, hi = T + z_{lo,hi} · SE` |

`z_{lo}`, `z_{hi}` are the standard-normal quantiles from `resolve_band_q()` (e.g., ±1.28 for an 80% band).

---

## 3. Per-Method Gradients and Analytic SEs

This section gives the closed-form gradient `h_i = (∂T/∂μ_i) · μ_i` and the resulting analytic SE for each aggregation option. Throughout, let `w̃_i = w_i / Σ w` be normalised weights (with `w̃_i = 1/N` when weights are absent), `z_p` the poverty line, and `μ̄ = Σ w̃_i μ_i` the weighted welfare mean.

### 3.1 Mean welfare (`"mean"`)

```
T = Σ w̃_i μ_i
∂T/∂μ_i = w̃_i
h_i = w̃_i · μ_i
SE  = || F_loading' (w̃ ⊙ μ) ||
```

**Assumptions:** Linearity of the conditional expectation; survey weights are exogenous. SE is symmetric on welfare scale — no transform.

### 3.2 Total welfare (`"total"`)

```
T = Σ w_i μ_i        (unnormalised)
h_i = w_i · μ_i
SE  = || F_loading' (w ⊙ μ) ||
```

Same form as the mean but scaled by total weight. Used for aggregate spend, GDP contribution, etc.

### 3.3 Poverty headcount (`"headcount_ratio"`)

```
T = Σ w̃_i · 1{μ_i < z_p}
```

The indicator function is not differentiable. WISE-APP **kernel-smooths it** as `Φ((z_p − μ)/b_w)` with auto-tuned bandwidth

```
b_w = max( bandwidth_p0 · z_p ,  median_i( μ_i · sqrt(Σ_k F_loading[i, k]²) ) )
```

The second term is the median per-obs welfare SE; bandwidths smaller than the typical perturbation magnitude under-count threshold crossings. `bandwidth_p0` defaults to 0.05 (5% of the poverty line).

```
h_i = − w̃_i · φ((z_p − μ_i)/b_w) · μ_i / b_w
SE  = || F_loading' h ||,  reported on logit scale
```

**Assumptions:** Smooth approximation to the step function. Bias is `O(b_w²)`; variance increases as `b_w → 0`. Logit band keeps reported endpoints in `(0, 1)`. The kernel bandwidth is *not* the user's epistemic uncertainty about the poverty line — it is a mathematical regulariser. For sharp poverty line analysis, set `bandwidth_p0` small and accept noisier gradients; for smoother attribution, raise it.

### 3.4 Poverty gap (`"gap"`)

```
T = Σ w̃_i · max(z_p − μ_i, 0) / z_p
∂T/∂μ_i = −w̃_i · 1{μ_i < z_p} / z_p
h_i = − w̃_i · μ_i · 1{μ_i < z_p} / z_p
SE  = || F_loading' h ||,  reported on logit scale
```

**Assumptions:** Differentiable almost everywhere; the kink at `μ = z_p` is a measure-zero set under the perturbation distribution. Logit band keeps the gap in `[0, 1]`.

### 3.5 Poverty severity / FGT(2) (`"fgt2"`)

```
T = Σ w̃_i · (max(z_p − μ_i, 0) / z_p)²
∂T/∂μ_i = −2 w̃_i · max(z_p − μ_i, 0) / z_p²
h_i = −2 w̃_i · μ_i · max(z_p − μ_i, 0) / z_p²
SE  = || F_loading' h ||,  reported on logit scale
```

FGT(2) penalises the depth of poverty quadratically. The squared term yields a continuously differentiable functional (no kink), so the delta-method approximation is sharper than for the headcount.

### 3.6 Prosperity gap (`"prosperity_gap"`)

```
T = Σ w̃_i · max(z_p − μ_i, 0)
h_i = − w̃_i · μ_i · 1{μ_i < z_p}
SE  = || F_loading' h ||,  identity scale
```

Same form as `gap` but unnormalised — reports the average dollar shortfall below the prosperity line. Identity-scale band because the support is `[0, z_p · Σ w]`, not `[0, 1]`.

### 3.7 Average welfare among poor (`"avg_poverty"`)

```
T = Σ_{i: μ_i < z_p} w_i μ_i  /  Σ_{i: μ_i < z_p} w_i
```

Quotient functional. Let `B = Σ w̃_i 1{μ_i < z_p}`. Implicit differentiation gives

```
∂T/∂μ_i = (w̃_i / B) · 1{μ_i < z_p} · (μ_i − T) / μ_i      (chain factor μ_i cancels with the log-scale lift)
h_i = (w̃_i / B) · 1{μ_i < z_p} · (μ_i − T) · μ_i
SE  = || F_loading' h ||,  identity scale
```

Falls back to `h = 0` when no households are poor (`B = 0`). Sign of `h_i` is positive for relatively wealthier poor households and negative for the very poorest, reflecting the leverage each poor household exerts on the conditional mean.

### 3.8 Median (`"median"`)

The median is a Hampel-style M-functional. Its influence function is

```
IF_i = (½ − 1{μ_i ≤ m}) / f(m)
```

where `f(m)` is the density of welfare at the median. WISE-APP estimates `f̂(m)` via `stats::density()` (Sheather-Jones bandwidth by default).

```
h_i = w̃_i · (½ − 1{μ_i ≤ m̂}) / f̂(m̂)
SE  = || F_loading' h ||,  reported on log scale
```

**Assumptions:** Quantile is uniquely defined (`f(m) > 0`). The density estimate is consistent — small samples or multimodal welfare distributions degrade the SE. When `f̂(m)` is non-finite or near zero, `h` is set to a zero vector and `SE = 0` is returned (a known limitation of plug-in influence functions).

### 3.9 Gini coefficient (`"gini"`)

WISE-APP uses a **partial-derivative gradient** of the weighted Gini *holding the rank ordering fixed*. This is the appropriate functional derivative when propagating regression coefficient uncertainty (which shifts welfare levels but rarely reorders households within a single perturbation).

```
G = (1/μ̄) · Σ_i w̃_i · μ_(i) · (2 F_(i) − 1)        where F_(i) = cumsum(w̃) − w̃/2 in sorted order
∂G/∂μ_i = (w̃_i / μ̄) · (2 F_(i) − 1 − G)
h_i = w̃_i · μ_i · (2 F_(i) − 1 − G) / μ̄
SE  = || F_loading' h ||,  identity scale
```

**Why partial (not Monti 1991 influence function):** the Monti IF is the right object for *sample-variance* estimation of the Gini, but applying it in the chain rule through `μ_i = exp(x_i' β)` overstates the variance because it double-counts ranking variation. The partial derivative satisfies the scale-invariance check `Σ h_i = 0`, which the Monti IF does not under intercept shifts.

**Assumptions:** Local rank stability under coefficient perturbation (small `Σ`). For large coefficient SEs or weather shocks that cross-rank households (poor becomes rich), the partial derivative under-counts uncertainty by ignoring re-ordering. This is a known limitation; under WISE-APP's typical coefficient SEs the omitted term is second-order.

---

## 4. Ensemble Pooling: Combining Across CMIP6 Members

`combine_ensemble_results(member_results, band_q)` composes per-member outputs from `aggregate_with_uncertainty_delta()` into a single result with two complementary bands.

### 4.1 Inputs and structure

For each CMIP6 member `m ∈ {1, …, M}`, the aggregator returns `value_m`, `var_coef_m`, `var_resid_m`. `combine_ensemble_results` then computes:

```
value      = mean_m(value_m)
var_within = mean_m(var_coef_m + var_resid_m)         (Law of Total Variance: E[Var(T | m)])
var_across = var_m(value_m)                            (Law of Total Variance: Var(E[T | m]))
var_pool   = var_within + var_across
se_pool    = sqrt(var_pool)
```

### 4.2 Two bands

- **Thick band** (`value_lo`, `value_hi`): empirical percentiles of the member point estimates `{value_m}` at `band_q`. Reflects inter-model spread without parametric assumptions. Set to `min`/`max` when `band_q = "minmax"`.
- **Thin "coef" band** (`coef_lo`, `coef_hi`): analytic Gaussian band `value ± z · se_pool`. Reflects total parametric uncertainty (coefficient + residual + model spread treated as Gaussian).

When `M = 1` (historical or single member), `var_across = 0` and the thin band reflects coefficient + residual uncertainty only; the thick band collapses to a point.

Auxiliary outputs (`model_lo`, `model_q10`, `model_q25`, `model_med`, `model_q75`, `model_q90`, `model_hi`) report unconditional percentiles of `{value_m}` for the inter-model summary panel.

### 4.3 Assumptions of the pooled SE

1. **Independence of within- and across-member variance.** The law of total variance gives `Var(T) = E[Var(T|m)] + Var(E[T|m])` exactly when these are computed against the joint distribution. WISE-APP estimates the inner expectation by the sample mean across members; this is an unbiased plug-in.
2. **Members are exchangeable.** Treating each CMIP6 realisation as equally informative ignores weighting schemes used in IPCC assessments (e.g., model democracy vs. performance-based weighting). Future work could expose weights.
3. **Gaussian approximation for the thin band.** `coef_lo`, `coef_hi` use `qnorm(band_q)`. For small `M`, a `t`-distribution with `M−1` df would be more appropriate; for `M ≥ 10` the difference is negligible.
4. **No correlation across members.** Coefficient uncertainty is identical across members (same regression fit), so `var_within`'s coefficient part is not actually independent across `m`. The pooled formula treats it as if it were, which slightly overstates `var_pool` (the shared coefficient component should appear once, not `M` times averaged). This is a conservative approximation.

---

## 5. Paired Counterfactuals (Scenario − Baseline Contrasts)

For policy comparisons (Module 3) and weather-effect summaries (Module 2), the quantity of interest is the *difference* between scenario and baseline aggregates on the same population. A naive SE,

```
SE_naive = sqrt(SE_scn² + SE_base²),
```

ignores that both predictions use the same `β̂` draw and is therefore conservative. The correct delta-method contrast variance is

```
Var(T_scn − T_base) = || F_agg_scn − F_agg_base ||²
```

`aggregate_with_uncertainty_delta()` returns `F_agg` (the K-vector `F_loading' h`) precisely so callers can build this contrast. `fct_policy_sim_compare.R` and `fct_aggregation.R::combine_ensemble_results()` consume `F_agg` to produce paired-band ribbons.

**Assumption:** Both aggregates are evaluated at the same `(F_loading, weights)` up to the policy-modified design matrix. This holds for paired weather and policy scenarios in WISE-APP; it does not hold for comparisons across surveys or sample re-weighting.

---

## 6. Layered Display in the UI

The Module 3 comparison panel (see `fct_policy_sim_compare.R`) reports up to three concentric uncertainty bands:

1. **Innermost (coefficient uncertainty only).** From `var_coef` of `aggregate_with_uncertainty_delta()`. Shown when the user enables "Show coefficient uncertainty".
2. **Middle (inter-annual variability).** Empirical percentiles of per-`sim_year` point estimates within each member.
3. **Outermost (inter-model spread).** Empirical percentiles of member point estimates from `combine_ensemble_results()`.

The user-facing band-width selector (`"p10_p90"`, `"p025_p975"`, etc.) is resolved by `resolve_band_q()` and applied uniformly across all three layers, so wider bands widen every layer simultaneously.

---

## 7. Summary of Assumptions

| Source | Method | Key assumption |
|---|---|---|
| Coefficient | Delta method (`F_loading' h`) | Local linearity of `T` in `μ`; correct vcov spec (`~loc_id` default) |
| Residual (default `"original"`) | Matched per-household training residual | Idiosyncratic component invariant to the counterfactual; contributes to mean, not to `var_resid` |
| Residual (`"normal"`/`"resample"`) | Plug-in `σ̂_ε² · Σ h²` | Homoskedastic residuals; residuals independent of coefficients (Option A) |
| Inter-annual | Empirical percentiles across `sim_year` | Years are exchangeable within the climate window |
| Inter-model | Empirical percentiles across `model_id` | Members are exchangeable (no weighting) |
| Pooled SE | Law of total variance | Independence of within- and across-member variation |
| Headcount | Kernel-smoothed indicator | Bandwidth large enough to dominate per-obs welfare SE |
| Median | Hampel IF with KDE density | Unique quantile, well-behaved density at `m` |
| Gini | Rank-fixed partial derivative | Coefficient perturbations do not reorder households |
| Paired contrasts | `\|\| F_agg_scn − F_agg_base \|\|` | Same population, same vcov, same weights |

---

## 8. What the Aggregator Does *Not* Quantify

- **Model misspecification** (functional form, omitted variables). Coefficient SEs are conditional on the chosen specification.
- **Weather data measurement error.** Treated as exact in the prediction step.
- **Future weather realisation uncertainty** beyond what CMIP6 ensemble members capture (e.g., internal variability not represented in the run).
- **Poverty line uncertainty.** `z_p` is treated as known.
- **Survey weight uncertainty.** `w_i` is treated as known (no jackknife / replicate-weights step).
- **Bootstrap uncertainty for RIF density estimation.** The RIF transform uses a plug-in density `f̂(q_τ)`; uncertainty in this density is not propagated. See [`method_rif.md`](method_rif.md) §3.2.
- **Scenario uncertainty.** The choice of SSP, time horizon, and policy parameters is treated as given.

These limitations apply to all aggregation methods and should be borne in mind when interpreting reported bands as "total" uncertainty.

---

## References

- Wooldridge, J. M. (2010). *Econometric Analysis of Cross Section and Panel Data* (2nd ed.). MIT Press. [Delta method, ch. 12]
- Cameron, A. C., & Trivedi, P. K. (2005). *Microeconometrics: Methods and Applications*. Cambridge University Press. [Cluster-robust inference, ch. 24]
- Foster, J., Greer, J., & Thorbecke, E. (1984). A class of decomposable poverty measures. *Econometrica*, 52(3), 761–766.
- Monti, A. C. (1991). The study of the Gini concentration ratio by means of the influence function. *Statistica*, 51(4), 561–577.
- Hampel, F. R. (1974). The influence curve and its role in robust estimation. *JASA*, 69(346), 383–393.