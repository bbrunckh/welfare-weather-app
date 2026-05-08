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

### 2.1 Outcome Transformation (`fct_rif_sim.R`)

`compute_rif(y, tau, bw = NULL)` transforms the raw outcome into its RIF at quantile τ:

1. Estimates q_τ via `stats::quantile()`
2. Estimates f_Y(q_τ) via `stats::density()` with Sheather-Jones bandwidth (`bw.SJ`)
3. Applies the RIF formula element-wise

The engine's `prepare_outcome()` creates 9 RIF columns (`rif_10`, `rif_20`, ..., `rif_90`) for τ ∈ {0.1, 0.2, …, 0.9}.

### 2.2 Model Fitting (`fct_fit_model.R`)

The `"rif"` engine is registered in `ENGINE_REGISTRY`. It exploits **fixest's multi-LHS `feols()`** to stack all 9 quantiles into a single call:

```r
c(rif_10, rif_20, ..., rif_90) ~ weather + covariates | fixed_effects
```

This returns a `fixest_multi` object (a list of 9 `fixest` fits), one per quantile. The same 3-model progressive specification used for linear regression applies:

- **Model 1**: Weather only
- **Model 2**: Weather + covariates
- **Model 3**: Weather + covariates + interactions

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

When a policy modifies covariates (e.g., flipping an infrastructure binary, adding a cash transfer), the simulation has two stages:

**Stage A — Weather delta with policy covariates:** `predict_rif()` is called with `svy = svy_policy` (the policy-modified survey). Because the scenario predictions use the policy covariates, any weather×covariate interaction terms activate through the weather delta automatically. For example, if the model includes `drought × irrigation`, and the policy turns on irrigation, the weather delta already reflects the moderated drought effect.

**Stage B — Policy correction via `.compute_rif_policy_correction()`:** This adds the channels that `predict_rif()` misses — effects of covariate changes that operate *outside* the weather delta. The correction delegates to `.compute_rif_channels()` (in `fct_policy_decompose.R`), which is the single source of truth for all channel calculations — both the simulation correction and the decomposition display call the same function.

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

The correction returns `δ_main_covar + δ_res1` only:
- δ_sp is excluded because the SP cash transfer is added post-backtransform in levels (to avoid log-scale distortion of direct transfers)
- δ_res2 is excluded because it is already captured inside Stage A's weather delta (which evaluates predictions at policy covariates, activating interaction terms)

**Full prediction assembly (in `run_sim_pipeline()`):**
```
ŷ_policy = backtransform(y_baseline + δ_weather(policy covars) + δ_main_covar + δ_res1) + SP_cash
```

##### Decomposition display (separate from simulation)

The decomposition table (in `decompose_policy_effect()`) reports *all* channels for transparency, including δ_sp and δ_res2, even though the simulation pipeline handles them differently. The decomposition output includes:
- `delta_main` = δ_sp + δ_main_covar (total main effect)
- `delta_res` = δ_res1 + δ_res2 (total resilience effect)
- `delta_total` = δ_main + δ_res (all channels combined)

### 2.6 Conceptual Issues with the Policy Simulation

#### a) Potential interaction term double-counting

The exclusion of δ_res2 from the simulation correction rests on the claim that `predict_rif()` already captures weather×covariate interactions because it evaluates at policy covariates. This is approximately but not exactly correct. `predict_rif()` computes δ_weather as the difference in full model predictions between scenario and baseline weather, both evaluated at policy covariates. For the interaction term β_{w×x} × w × x, the prediction difference captures β_{w×x} × Δw × x_policy. Meanwhile δ_res2 = β_{w×x}(τ_i^post) × w_base × Δx. These are complementary terms in a full decomposition, but whether they overlap depends on the exact prediction path. For small Δw and Δx the approximation is good; for large joint changes there may be a missing cross-term Δw × Δx.

#### b) Prediction-space vs. coefficient-space inconsistency

`predict_rif()` works in **prediction space** (full model predictions at each τ, including fixed effects). The policy correction works in **coefficient space** (extracting betas and multiplying by Δx). These agree for simple OLS but can diverge when fixed effects are absorbed — predictions include FE contributions that the beta extraction doesn't see. In practice, the delta approach (differencing two predictions) cancels fixed effects in Stage A, so the inconsistency mainly affects the main-effect correction in Stage B.

#### c) Structural assumption in repositioning and interaction

The repositioning and interaction channels assume that weather vulnerability is a function of *welfare level*, not of unobserved household characteristics correlated with welfare. A household moved from $100 to $200 by a cash transfer is assumed to have the same weather vulnerability (both main weather betas and interaction betas) as a household that was *originally* at $200. In reality, originally-poor households may differ in unobserved ways (e.g., asset quality, social networks, geographic exposure) that make them more vulnerable than originally-rich households at the same welfare level. The RIF betas at high τ were estimated from originally-rich households; applying them to formerly-poor households is a "rank invariance"-style assumption. This applies to both δ_res1 (weather betas) and δ_res2 (interaction betas) since both are now evaluated at τ_i^post. This is inherent to the method and difficult to test without panel data or instrumental variation.

#### d) SP transfer scale mismatch

The SP direct effect is added post-backtransform in levels, while its influence on repositioning (through δ_res1) is computed in log/model scale. This is deliberate — the repositioning captures the *indirect* resilience effect while the direct transfer is a cash addition — but the two operate on different scales. The decomposition table reports δ_sp in model scale (log), while the simulation adds SP_cash in levels. Users comparing the decomposition percentages to simulation outputs need to be aware of this distinction.

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
