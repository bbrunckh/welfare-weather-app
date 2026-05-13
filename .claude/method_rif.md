# RIF Regression: Theory and Implementation in WISE-APP

## 1. Theoretical Background

### Unconditional Quantile Regression via RIF

RIF regression estimates the effect of covariates on **unconditional quantiles** of the outcome distribution, following Firpo, Fortin & Lemieux (2009). Unlike conditional quantile regression (Koenker & Bassett, 1978), which estimates Q_τ(Y|X), RIF regression targets **unconditional** quantiles Q_τ(Y) — answering: *"How does a marginal change in X shift the τ-th quantile of the overall distribution?"*

### The RIF Transformation

The Recentered Influence Function for the τ-th quantile is:

```
RIF(y; q_τ) = q_τ + (τ - 1(y ≤ q_τ)) / f_Y(q_τ)
```

where:
- `q_τ = F_Y⁻¹(τ)` is the τ-th sample quantile
- `f_Y(q_τ)` is the density of Y evaluated at q_τ (estimated via kernel density with Sheather-Jones bandwidth)
- `1(·)` is the indicator function

Key property: `E[RIF(Y; q_τ)] = q_τ`, so OLS on the transformed outcome identifies the marginal effect on the unconditional quantile.

### Interpretation

The coefficient β_τ from regressing RIF(Y; q_τ) on X estimates:

```
β_τ ≈ ∂Q_τ(Y) / ∂E[X]
```

This is the effect of a marginal increase in the population mean of X on the τ-th quantile of Y. Coefficients that vary across τ indicate **heterogeneous effects across the distribution** — e.g., weather shocks that harm the poor (low quantiles) more than the wealthy.

---

## 2. Implementation in WISE-APP

> **Companion document.** The linear (`fixest`) engine — the alternative model choice in WISE-APP — is documented in [`method_lin.md`](method_lin.md). Where the two engines share machinery (outcome preparation, log back-transform, SP cash handling, FE absorption), this document refers across rather than duplicating.

### 2.1 Outcome Transformation (`fct_rif_sim.R`)

**Generic outcome prep is shared with the linear engine.** Log transformation, LCU back-conversion, and binary-poor indicator construction all happen in `prepare_outcome_df()` (`fct_results.R`) *before* the engine is invoked — they are application-wide, not RIF-specific. The engine receives an outcome column that is already on the model scale.

The engine's own `prepare_outcome()` then layers the RIF transformation on top, adding 9 columns (`rif_10`, `rif_20`, ..., `rif_90`) for τ ∈ {0.1, 0.2, …, 0.9}. For each τ, `compute_rif(y, tau, bw = NULL)` in `fct_rif_sim.R`:

1. Estimates q_τ via `stats::quantile()`
2. Estimates f_Y(q_τ) via `stats::density()` with Sheather-Jones bandwidth (`bw.SJ`)
3. Applies the RIF formula element-wise

### 2.2 Model Fitting (`fct_fit_model.R`)

The `"rif"` engine is registered in `ENGINE_REGISTRY`. It exploits **fixest's multi-LHS `feols()`** to stack all 9 quantiles into a single call:

```r
c(rif_10, rif_20, ..., rif_90) ~ weather + covariates | fixed_effects
```

This returns a `fixest_multi` object (a list of 9 `fixest` fits), one per quantile. The same 3-model progressive specification used for linear regression applies — identical formula structure to the `fixest` engine:

- **Model 1**: Weather only (no FE): `c(rif_10,...,rif_90) ~ hazard`
- **Model 2**: Weather + interactions + FE: `c(...) ~ hazard + interactions_main | fe`
- **Model 3**: Weather + interactions + covariates + FE: `c(...) ~ hazard + interactions_main + covariates | fe`

Results are collected into a `rif_grid` data frame with columns `(tau, term, estimate, std.error, conf.low, conf.high, model)` via `build_rif_grid()`.

### 2.3 Availability

RIF is offered only for **numeric** (continuous) outcomes. Binary/logical outcomes are restricted to logistic or linear regression.

### 2.4 Results Display (`fct_results.R`)

- **Coefficient plot**: Beta-curve plots showing β̂_τ vs. τ, faceted by covariate, with confidence ribbons, overlaying all 3 model specifications.
- **Regression table**: HTML table with quantile columns (τ = 0.1 through 0.9), showing coefficients + standard errors per term, plus per-quantile N and within-R².
- **Diagnostics**: Uses the τ = 0.5 (median) fit as a representative model for residual plots and Q-Q plots. Predicted-vs-actual plots are skipped (the RIF outcome is not directly interpretable as welfare).

### 2.5 Simulation Pipeline (`fct_simulations.R`, `fct_rif_sim.R`)

`predict_rif()` implements a **delta-method** approach for counterfactual prediction:

1. Each household is assigned a quantile position τ_i by evaluating the empirical CDF of the **training** outcome at the household's own baseline outcome value: τ_i = F̂(y_i). Concretely, `stats::ecdf(train_y)` is built from the training sample, then applied to each household's observed welfare y_i to get its percentile rank in the overall distribution. The result is clamped to [min(τ), max(τ)] (i.e. [0.1, 0.9]) so it falls within the fitted quantile grid.
2. For each grid quantile k, the model predicts under baseline and scenario weather: δ_k = ŷ_k^scen − ŷ_k^base.
3. Each household's weather effect δ_i is obtained by **linearly interpolating** δ_k at τ_i across the quantile grid (`interpolate_delta()`).
4. Final prediction: ŷ_i = y_i^baseline + δ_i.

#### Weather-only simulation

For weather-only scenarios (no policy), `predict_rif()` is called with the original survey. The result is simply `y_baseline + δ_weather`.

#### Policy scenario simulation

When a policy modifies covariates (e.g., flipping an infrastructure binary, adding a cash transfer), `run_sim_pipeline()` runs two stages. The architecture is **predict-at-baseline + channel decomposition**: rather than letting `fixest::predict()` blend covariate effects into the weather delta, the pipeline predicts at baseline covariates and then explicitly adds every policy channel from the decomposition helper.

**Stage A — Baseline weather delta:** in policy mode, `svy_for_predict = svy_baseline` (not the policy-modified survey; see `fct_simulations.R` ~line 444). `predict_rif()` is therefore evaluated at *baseline* covariates and returns `y_baseline + δ_weather` computed with the unchanged policy levers. This isolates the pure weather effect at status-quo covariates and leaves *all* policy channels — including weather×covariate interactions — to be added explicitly in Stage B.

**Stage B — Full policy correction via `.compute_rif_policy_correction()`:** the correction returns `channels$delta_total` from `.compute_rif_channels()` (in `fct_policy_decompose.R`) — the same helper the decomposition display calls. `delta_total = δ_sp + δ_main_covar + δ_res1 + δ_res2` is added to `y_point` on the model (log) scale:

```r
y_point <- y_point + corr[out$.svy_row_id]   # fct_simulations.R ~line 542
```

Because `δ_sp` is already inside `delta_total`, the level-scale SP block later in `run_sim_pipeline()` is **skipped in RIF policy mode** (`if (!is_rif_policy && SP_TRANSFER_COL %in% names(out)) …`). The cash transfer therefore enters in log scale via `δ_sp = log(exp(y_baseline) + SP_cash) − y_baseline`, not in levels post-backtransform.

This is the single most important consistency property of the current RIF policy pipeline: **the simulation y_point and the decomposition display are produced by the same function with the same inputs, so the channel totals reconcile exactly with the simulated welfare level** (up to the back-transform, which is monotone).

##### Channel 1: Main effect (δ_main)

The main effect captures the direct welfare shift from the policy, composed of two sub-channels:

1. **SP cash transfer (δ_sp):** Converted to model scale. For log-transformed outcomes:
   ```
   δ_sp = log(exp(y_baseline) + SP_cash) − y_baseline
   ```

2. **Covariate shift (δ_main_covar):** For each changed covariate v with Δv = x_policy − x_baseline:
   ```
   δ_main_covar += β_v(τ_i^pre) × Δv
   ```
   where β_v(τ_i^pre) is the coefficient for v interpolated from the beta curve at the household's pre-policy quantile position.

**Both SP and covariate shifts are summed:** `δ_main = δ_sp + δ_main_covar`. This combined main effect determines the welfare shift used for repositioning (below).

##### Channel 2: Resilience effect (δ_res = δ_res1 + δ_res2)

The resilience channel captures how the policy changes the household's *sensitivity to weather*. It has two sub-channels, both of which modify weather-related welfare effects:

**a) Repositioning (δ_res1):** The main effect (including SP) shifts household welfare. Because RIF weather coefficients vary across τ, a household at a different welfare level faces different weather vulnerability:

```
y_post_main = y_baseline + δ_main          # welfare after main effect (SP + covariates)
τ_i^post = F̂_baseline(y_post_main)         # new position on the baseline welfare-to-τ mapping

For each weather variable w:
  δ_res1 += [β_w(τ_i^post) − β_w(τ_i^pre)] × w_baseline
```

**Crucially, SP contributes to repositioning.** Because δ_sp is included in δ_main, a cash transfer shifts the household's welfare level upward *before* the new quantile position is computed. This means SP not only provides direct income support but also indirectly reduces weather vulnerability by moving the household to a less vulnerable part of the welfare distribution.

**Why the baseline CDF is appropriate here:** The baseline CDF is used as a *mapping function* from welfare levels to positions on the empirically-estimated beta curve, not as a claim about the household's rank in the post-policy distribution. The beta curve β_w(τ) captures how weather vulnerability varies with welfare — it was estimated from the cross-section of households at different welfare levels. A household whose welfare rises from $100 to $200 should face the weather vulnerability observed for $200-level households in the baseline data, regardless of whether other households also became richer. The baseline CDF simply translates dollar amounts into the τ-index needed to read off the beta curve.

**Interpretation:** If weather vulnerability is higher at lower quantiles (β_w more negative at low τ), then a policy that raises welfare — whether through direct transfers, infrastructure, or other channels — reduces weather vulnerability. This is the key value-add of the distributional approach: it captures how poverty-reducing interventions build climate resilience as a second-order effect.

**b) Interaction (δ_res2):** The direct change in weather×covariate interaction terms, evaluated at the **post-policy** quantile position:
```
δ_res2 += β_{w×x}(τ_i^post) × w_baseline × Δx
```

Like repositioning, δ_res2 is evaluated at τ_i^**post** — the household's quantile position after the main effect (SP + covariate shifts) has moved its welfare level. This is conceptually consistent: both resilience sub-channels ask "what is this household's weather sensitivity at its *new* welfare level?" The interaction beta, like the main weather beta, should reflect vulnerability at the post-policy position. SP cash transfers therefore contribute to both resilience sub-channels by shifting the welfare level that determines τ_i^post.

**Both sub-channels are weather-sensitive** — they modify welfare only through the weather channel. In the absence of weather shocks, both δ_res1 and δ_res2 are zero.

##### What the correction returns

The correction returns `channels$delta_total = δ_sp + δ_main_covar + δ_res1 + δ_res2` from `.compute_rif_channels()`. **All four channels are added to `y_point` in log scale.** Notes on each:

- **δ_sp** enters in log scale (`log(exp(y_baseline) + SP_cash) − y_baseline`) and drives `τ_i^post` via `delta_main`. The level-scale SP block in `run_sim_pipeline()` is gated on `!is_rif_policy` and is therefore inactive in this mode — cash is *not* added a second time post-backtransform.
- **δ_main_covar** uses `β_v(τ_i^pre) · Δv` summed over changed covariates.
- **δ_res1** is the repositioning channel: `[β_w(τ_i^post) − β_w(τ_i^pre)] · w_baseline` for continuous weather, summed over weather variables (with an analogous binned-weather branch).
- **δ_res2** is the policy × weather interaction channel: `β_{w×v}(τ_i^post) · w_hh · Δv`, evaluated at `τ_i^post`.

`τ_i^pre = F̂(y_baseline)` uses the training-outcome `ecdf()`; `τ_i^post = baseline_cdf(y_baseline + δ_main)` uses the baseline-sample `ecdf()`. Both are clamped to `[min(taus), max(taus)] = [0.1, 0.9]`.

**Full prediction assembly (in `run_sim_pipeline()`):**
```
ŷ_policy = backtransform( y_baseline
                          + δ_weather(baseline covariates)   # Stage A: predict_rif at svy_baseline
                          + δ_sp + δ_main_covar              # Stage B: delta_main
                          + δ_res1                           # Stage B: repositioning
                          + δ_res2 )                         # Stage B: interaction
```

There is no post-backtransform SP addition in RIF policy mode.

##### Decomposition display (separate from simulation)

The decomposition table (in `decompose_policy_effect()`) reports the same `.compute_rif_channels()` output broken into named columns:
- `delta_main` = δ_sp + δ_main_covar (total main effect)
- `delta_res` = δ_res1 + δ_res2 (total resilience effect)
- `delta_total` = δ_main + δ_res (all channels combined)

**Decomposition–simulation reconciliation.** Both routes call `.compute_rif_channels()` with the same `(svy_baseline, svy_policy, weather_raw, rif_grid, taus, train_data)`, and the simulation correction is *exactly* `channels$delta_total` from that call. The reconciliation is therefore exact in model scale: per-household `δ_total` in the decomposition table equals the policy-minus-baseline change in pre-backtransform `y_point`. This is a structural advantage over the linear engine, where the `β·Δx` decomposition and the `fixest::predict()` simulation can diverge under FE absorption (see [`method_lin.md`](method_lin.md) §2.8c).

### 2.6 Uncertainty Propagation (`fct_aggregation_delta.R`, `fct_rif_sim.R`)

**The RIF engine uses the same closed-form delta-method aggregator as the linear engine.** See [`method_lin.md`](method_lin.md) §2.6 for the variance formula, band transforms, and ensemble pooling. This section covers only the RIF-specific construction of `F_loading`.

**Per-tau Cholesky factors.** `compute_chol_vcov(fit_multi)` dispatches on the `fixest_multi` object and returns a *list* of K Cholesky factors `L_k` (one per quantile fit), rather than the single L produced for a scalar `feols` fit.

**Quantile-interpolated F_loading** (`interpolate_F_loading()` in `fct_rif_sim.R`). For each scenario the K per-quantile design-matrix differences `X_diff_k = X_scenario_k − X_baseline_k` are pre-cached, multiplied to give `F_k = X_diff_k %*% t(L_k)`, then linearly blended at each household's `τ_i`:

```
idx_lo, idx_hi = bracketing grid points of τ_i  (clamped to [0.1, 0.9])
w_i            = (τ_i − τ_idx_lo) / (τ_idx_hi − τ_idx_lo)
F_loading_i    = (1 − w_i) · F_idx_lo[i, ] + w_i · F_idx_hi[i, ]
```

The interpolated `F_loading` (N × K_coef) is attached as `attr(out, "F_loading")` on the prediction frame and consumed by `aggregate_with_uncertainty_delta()` exactly as in the linear path. Because both `X_diff_k` (the *difference* between scenario and baseline design) and `L_k` are interpolated, the resulting variance reflects coefficient uncertainty in the *weather delta* — consistent with `predict_rif()` returning a weather delta rather than an absolute level.

**Caveats specific to the RIF uncertainty path:**

- **No cross-tau covariance.** Each `L_k` encodes only the variance of the τ-th regression; the K RIF fits are treated as independent in the interpolation. Cross-quantile correlation in the underlying RIF coefficients is ignored, which can understate uncertainty when adjacent quantile fits share most of their sample.
- **Residual variance is zero for RIF.** `train_aug` is set to `NULL` in `run_sim_pipeline()` for the RIF branch (`is_rif` short-circuit), so the residual options (`"original"`, `"normal"`, `"resample"`) cannot be used; the aggregator's `var_resid` term is always zero for RIF. This is intentional — the RIF prediction is `y_baseline + δ_i` rather than a model prediction in welfare units, so an additive residual draw on top of the household's *observed* baseline welfare is not meaningful.
- **Clamping at the tail.** Households below τ = 0.1 or above τ = 0.9 receive the F_loading of the boundary quantile, so their uncertainty bands collapse to the boundary's coefficient covariance with no extrapolation penalty.

### 2.7 Conceptual Issues with the Policy Simulation

#### a) Single Σ approximation in the resilience channel variance

`.compute_rif_channels()` accumulates per-channel variance under a **diagonal-Σ approximation**: `var_res1 += haz² · (SE_pre² + SE_post²)`, treating the τ_pre and τ_post coefficient draws as independent. Because both are interpolated from the same per-τ fits, they are in fact correlated; the diagonal approximation therefore *overstates* the variance of δ_res1 (and similarly δ_res2). This is the channel-level analogue of the cross-tau independence issue in `interpolate_F_loading()` (§2.6).

Note that the channel variances are returned in `sd_main`/`sd_res1`/`sd_res2`/`sd_total` for the decomposition display only — the simulation correction is called with `skip_coef = TRUE` (see `.compute_rif_policy_correction()`), so SE propagation in the Results pane flows entirely through `F_loading`, not through these channel SDs.

#### b) Prediction-space vs. coefficient-space inconsistency

The Stage A weather delta works in **prediction space** (full model predictions at each τ, including fixed effects). The Stage B correction works in **coefficient space** (extracting betas and multiplying by Δx). These agree for simple OLS but can diverge when fixed effects are absorbed — predictions include FE contributions that the beta extraction doesn't see. In practice, the delta approach (differencing two predictions) cancels fixed effects in Stage A, so the inconsistency mainly affects whether Stage B's channel sum exactly reproduces what a hypothetical `predict()`-at-policy-covariates would have returned. Because the current pipeline never compares the two routes (Stage A is evaluated at baseline only), this is a conceptual rather than a numerical concern.

#### c) Structural assumption in repositioning and interaction

The repositioning and interaction channels assume that weather vulnerability is a function of *welfare level*, not of unobserved household characteristics correlated with welfare. A household moved from $100 to $200 by a cash transfer is assumed to have the same weather vulnerability (both main weather betas and interaction betas) as a household that was *originally* at $200. In reality, originally-poor households may differ in unobserved ways (e.g., asset quality, social networks, geographic exposure) that make them more vulnerable than originally-rich households at the same welfare level. The RIF betas at high τ were estimated from originally-rich households; applying them to formerly-poor households is a "rank invariance"-style assumption. This applies to both δ_res1 (weather betas) and δ_res2 (interaction betas) since both are now evaluated at τ_i^post. This is inherent to the method and difficult to test without panel data or instrumental variation.

#### d) SP transfer enters in log scale (RIF policy mode)

Unlike the linear engine — which adds `SP_cash` to predicted welfare *in levels* after back-transform — the RIF policy path adds `δ_sp = log(exp(y_baseline) + SP_cash) − y_baseline` in log scale inside `delta_total`, and the level-scale SP block in `run_sim_pipeline()` is skipped. The two are algebraically equivalent (`exp(y + δ_sp) = exp(y) + SP_cash`), but the path matters for uncertainty propagation: the log-scale addition enters before the band transform and shows up as a level shift in the centred Gaussian band; the level-scale addition would instead shift the band uniformly after the transform. Currently δ_sp is treated as deterministic (no SE), so this distinction is numerical-precision only.

#### e) Sparse quantile grid and linear interpolation

With only 9 grid points (τ = 0.1, …, 0.9), linear interpolation can miss curvature in the beta curve. This is especially problematic for the repositioning channel, where small changes in τ_i are multiplied by beta differences that may be poorly approximated between grid points. Households below the 10th percentile (often the policy-relevant population) are clamped to τ = 0.1, receiving identical treatment regardless of how far into the tail they fall.

---

## 3. Assumptions

1. **Linearity in the RIF**: The method assumes a linear relationship between covariates and the RIF-transformed outcome. This is a local approximation — it holds well for small marginal changes but can be inaccurate for large distributional shifts.

2. **Kernel density estimation**: The density f_Y(q_τ) is estimated nonparametrically. Results at extreme quantiles (tails) are sensitive to bandwidth choice and sparse data.

3. **Quantile grid discretisation**: The app uses a fixed grid τ ∈ {0.1, …, 0.9}. Effects below the 10th or above the 90th percentile are not estimated, and interpolation between grid points assumes smoothness.

4. **Independence of RIF regressions**: Each quantile is estimated independently via OLS. There is no constraint ensuring monotonicity or consistency across quantiles.

5. **Standard OLS assumptions**: Within each quantile, the usual assumptions apply — correct specification, exogeneity, no perfect multicollinearity. Fixed effects address time-invariant unobserved heterogeneity.

6. **Stable density under counterfactuals**: The simulation pipeline uses the baseline density and CDF for quantile assignment. Under large shocks, the actual post-shock distribution may differ substantially.

---

## 4. Limitations

1. **No tail quantiles**: The grid stops at τ = 0.1 and τ = 0.9. The most vulnerable (bottom 10%) and wealthiest (top 10%) are excluded from direct estimation.

2. **Fixed bandwidth**: `bw.SJ` is computed once on the full sample. It is not adapted per survey wave or subgroup.

3. **No monotonicity enforcement**: Nothing prevents quantile crossing in predictions (where the predicted 20th percentile exceeds the predicted 30th percentile).

4. **Simulation approximation**: The delta-interpolation approach is a first-order approximation. It does not account for general equilibrium effects or reranking of households across quantiles.

5. **Diagnostics are limited**: Using only the median (τ = 0.5) fit for residual diagnostics may miss misspecification at other quantiles.

6. **Computational cost**: Fitting 9 separate OLS regressions (× 3 specifications) is manageable, but the simulation step requires per-household interpolation which scales linearly in sample size.

7. **Silent row-dropping under fixed effects**: Each per-quantile fit is a `fixest::feols()` call, and `predict_rif()` invokes `fixest::predict()` internally. When the policy-modified or counterfactual survey contains FE levels (locations, years, survey waves) absent from training, fixest silently drops those rows. This is the same issue as in the linear engine (see [`method_lin.md`](method_lin.md) §4) — affected households disappear from the simulation without a user-visible flag. The risk is highest when simulating on a survey drawn from a population that differs structurally from the estimation sample.

---

## 5. Suggestions for Improvement

1. **Extend the quantile grid**: Add τ ∈ {0.05, 0.95} or allow user-configurable grids to capture tail effects, which are often the most policy-relevant.

2. **Quantile-specific diagnostics**: Provide residual and goodness-of-fit plots for each quantile (or at least for a user-selected subset), not just the median.

3. **Bootstrap inference**: Standard errors from OLS on the RIF are consistent but can understate uncertainty because the RIF itself is estimated. A bootstrap (or analytical correction) would improve coverage.

4. **Monotonicity rearrangement**: Apply Chernozhukov, Fernández-Val & Galichon (2010) rearrangement to ensure predicted quantile functions are non-crossing.

5. **Adaptive bandwidth**: Consider per-wave or per-subgroup bandwidth selection, especially when pooling surveys with different sample sizes or distributional shapes.

6. **Comparison to conditional quantile regression**: Offer CQR (e.g., via `quantreg::rq()`) as an alternative, with a note explaining the different interpretation (conditional vs. unconditional effects).

7. **Decomposition output**: Since RIF regression naturally supports Oaxaca-Blinder-style decompositions, consider exposing composition vs. structure effects in the results tab.

---

## References

- Firpo, S., Fortin, N. M., & Lemieux, T. (2009). Unconditional quantile regressions. *Econometrica*, 77(3), 953–973.
- Chernozhukov, V., Fernández-Val, I., & Galichon, A. (2010). Quantile and probability curves without crossing. *Econometrica*, 78(3), 1093–1125.
- Koenker, R., & Bassett, G. (1978). Regression quantiles. *Econometrica*, 46(1), 33–50.
