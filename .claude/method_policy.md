# Policy Scenarios: How Step 3 Settings Modify the Survey Data

This document explains how the user's choices in Module 3 (Step 3 — Policy Scenarios) translate into per-household modifications of the survey-weather data frame, and how that modified frame feeds the simulation and decomposition pipelines.

The flow is engine-agnostic: the same `apply_policy_to_svy()` transformation runs for both the linear (`fixest`) and RIF engines. Where the engines differ in how they consume the modified survey, this doc cross-references [`method_lin.md`](method_lin.md) §2.5 and [`method_rif.md`](method_rif.md) §2.5.

---

## 1. End-to-End Flow

```
USER INPUTS (Step 3 sidebar tabs)
  │
  ├─ Social Protection      → sp_scenario()       (mod_3_01_sp.R)
  ├─ Infrastructure         → infra_scenario()    (mod_3_02_infra.R)
  ├─ Digital Inclusion      → digital_scenario()  (mod_3_03_digital.R)
  └─ Labor Market           → labor_scenario()    (mod_3_04_labor.R)
                                    │
        [user clicks "Run simulation"]
                                    ▼
  mod_3_05_policy_sim_server::run()              (mod_3_05_policy_sim.R)
    │
    ├─ apply_policy_to_svy(svy, infra, sp, digital, labor)         (fct_policy_sim.R)
    │     → svy_mod  (policy-modified survey-weather frame)
    │
    ├─ resimulate_with_svy(svy_mod, …, svy_baseline = svy)          (fct_policy_sim.R)
    │     → policy_hist_sim, policy_saved_scenarios
    │
    └─ decompose_policy_effect(svy_baseline = svy,
                               svy_policy   = svy_mod, …)           (fct_policy_decompose.R)
          → decomp_result, decomp_scenarios
                                    ▼
        Module 3 Results tab        (mod_3_06_results.R)
        Module 3 Decomposition tab  (mod_3_08_decomposition.R)
        Module 3 Diagnostics tab    (mod_3_07_diagnostics.R)
```

The single point of survey mutation is **`apply_policy_to_svy()`** in `fct_policy_sim.R`. Every downstream consumer (results pane, decomposition, diagnostics) sees the same `svy_mod` produced by that function. There is no second site that modifies covariates.

---

## 2. The Survey Frame and the Columns Policies Touch

Step 1's `survey_weather()` returns a household × weather data frame `svy` (one row per household × month, depending on the join). `apply_policy_to_svy()` inspects which of the following columns are present and only modifies those it finds:

| Domain | Column | Type | Modifier UI |
|---|---|---|---|
| Infrastructure | `electricity`, `imp_wat_rec`, `imp_san_rec`, `piped`, `piped_to_prem`, `imp_wat_san_rec` | binary 0/1 | mod_3_02 |
| Infrastructure | `ttime_health` | numeric (minutes) | mod_3_02 |
| Digital | `internet`, `cellphone` | binary 0/1 | mod_3_03 |
| Labor | `employed`, `selfemployed`, `unemployed` | mutually exclusive 0/1 | mod_3_04 |
| Labor | `agriculture`, `industry`, `services` | mutually exclusive 0/1 (employed workers only) | mod_3_04 |
| Social protection | `welfare` (read only — for targeting) | numeric | mod_3_01 |
| Social protection | `._sp_transfer` (= `SP_TRANSFER_COL`) | numeric (daily $/hh), added | mod_3_01 |
| Targeting / budget | weight column matched by regex `^weight$\|^hhweight$\|^wgt$\|^pw$` (case-insensitive) | numeric | implicit |

The constant `SP_TRANSFER_COL <- ".wiseapp_sp_transfer"` is defined at the top of `fct_policy_sim.R` (line 15). Many UI inputs in mod_3_02 / mod_3_03 are gated on the corresponding covariate appearing in the **selected Model 3 coefficients** — if a covariate isn't in the model, its slider is hidden because changing it would have no effect on predictions.

---

## 3. Social Protection (`mod_3_01_sp.R`)

### 3.1 UI inputs

| Input | Values | Purpose |
|---|---|---|
| `sp_type` | `"regular"` (shock-responsive scaffolded but disabled) | Program type |
| `targeting` | `"universal"` / `"exante_poor"` / `"pmt"` | Targeting rule |
| `targeting_threshold_pct` | 5–60 (default 20) | Welfare-percentile cutoff for `exante_poor` |
| `pmt_variable` | numeric column name | PMT proxy variable |
| `pmt_cutoff` | slider (continuous) or radio (binary) | PMT threshold |
| `inclusion_error_pct` | 0–30 (default 10) | Share of *non*-eligibles wrongly included |
| `exclusion_error_pct` | 0–30 (default 10) | Share of eligibles wrongly excluded |
| `budget_mode` | `"transfer_first"` / `"budget_first"` | Sizing mode |
| `transfer_amount_usd` | USD per payment (transfer_first) | Per-payment amount |
| `transfer_n_payments` | 2–24 (default 6) | Payments per year |
| `budget_fixed` | USD (budget_first, default 1,000,000) | Total annual budget |

### 3.2 Reactive output

`sp_scenario()` returns a named list with the keys above (`sp_type`, `targeting`, `budget_mode`, `targeting_threshold`, `pmt_variable`, `pmt_cutoff`, `inclusion_error_pct`, `exclusion_error_pct`, `transfer_amount_usd`, `transfer_n_payments`, `budget_fixed`).

### 3.3 Eligibility — `.determine_sp_eligibility(svy, sp)` (`fct_policy_sim.R` ~line 134)

Three branches:

- **`"universal"`** — `eligible <- rep(TRUE, n)`. Inclusion/exclusion error blocks are skipped.
- **`"exante_poor"`** — `q <- quantile(svy$welfare, threshold/100)` then `eligible <- !is.na(welfare) & welfare <= q`. Uses the **baseline** welfare distribution; "ex-ante" refers to pre-policy welfare.
- **`"pmt"`** — reads `svy[[pmt_variable]]`. If the column has exactly two unique values `{0, 1}`, eligibility is `col == pmt_cutoff` (binary match). Otherwise eligibility is `col <= pmt_cutoff` (continuous threshold). Returns all-FALSE if the variable is missing or the cutoff is NA.

Targeting errors are applied after the rule (skipped for `"universal"`):
```
inclusion error:  sample round(n_non_eligible · incl_rate / 100)
                  from non-eligibles and mark them TRUE
exclusion error:  sample round(n_eligible · excl_rate / 100)
                  from eligibles and mark them FALSE
```

### 3.4 Data modification — SP transfer column

`apply_policy_to_svy()` adds a daily per-household transfer amount in the column `SP_TRANSFER_COL = ".wiseapp_sp_transfer"`. Two budget modes (`fct_policy_sim.R` ~lines 515–555):

**`"transfer_first"`**:
```r
annual_hh <- transfer_amount_usd * n_payments
daily_hh  <- annual_hh / 365
svy[[SP_TRANSFER_COL]] <- ifelse(eligible, daily_hh, 0)
```

**`"budget_first"`**:
```r
w_elig    <- sum(svy[weight_col][eligible])   # 0 if no weight col
divisor   <- if (w_elig > 0) w_elig else sum(eligible)
annual_hh <- budget_fixed / divisor
daily_hh  <- annual_hh / 365
svy[[SP_TRANSFER_COL]] <- ifelse(eligible, daily_hh, 0)
```

The weighted-divisor logic ensures the *population-level* annual spend `sum(transfer · weight) · 365` equals `budget_fixed`. Without it, the realised total would scale with the mean eligible weight.

### 3.5 How the transfer enters predictions

For the **linear engine**, the transfer is added post-prediction in level scale (`fct_simulations.R` ~lines 549–569). When `so$transform == "log"`, the boost is re-logged: `y_point <- log(exp(y_point) + sp_vec)`. See [`method_lin.md`](method_lin.md) §2.5.

For the **RIF engine in policy mode**, the level-scale SP block is **skipped** because `δ_sp` is already inside the policy correction's `delta_total` (added in log scale via `.compute_rif_channels()`). See [`method_rif.md`](method_rif.md) §2.5.

Cost reporting (realised spend, cost per beneficiary) is **not** currently surfaced in the UI — only the simulated welfare effects are.

---

## 4. Infrastructure (`mod_3_02_infra.R`)

### 4.1 UI inputs

Each domain has the same two-control pattern via the helper `infra_access_ui()`:

- `{var}_universal` — checkbox: set everyone with access
- `{var}_pct` — slider in −20 … +100 % (only shown if `_universal` is unchecked)

The `{var}` slugs and the survey columns they target:

| UI slug | Survey column |
|---|---|
| `elec` | `electricity` |
| `water` | `imp_wat_rec` |
| `sanitation` | `imp_san_rec` |
| `piped` | `piped` |
| `piped_to_prem` | `piped_to_prem` |
| `imp_wat_san` | `imp_wat_san_rec` |

Each pair is rendered **only if the corresponding column appears in the Model 3 coefficient table**, so the UI auto-hides interventions the model can't price.

Health travel time has a different control:
- `health_mode` — radio: `"pct"` (multiplicative change −100…+20 %) or `"max"` (cap, 15–240 min, default 60)
- `health_travel_pct` / `health_travel_max`

### 4.2 Reactive output

`infra_scenario()` returns a list with `*_universal` (logical) and `*_access_change_pct` (integer) for each domain, plus `health_mode`, `health_travel_pct`, `health_travel_max`.

### 4.3 Data modification — `.apply_binary_access(x, universal, change_pct)`

```
universal = TRUE
  → return ifelse(is.na(x), NA, 1L)   # NAs preserved

universal = FALSE, change_pct > 0
  → idx0   = which(x == 0)
    n_flip = round(length(idx0) * change_pct / 100)
    sample(idx0, n_flip) → x[flip] <- 1L

universal = FALSE, change_pct < 0
  → idx1   = which(x == 1)
    n_flip = round(length(idx1) * |change_pct| / 100)
    sample(idx1, n_flip) → x[flip] <- 0L

change_pct == 0
  → return x unchanged
```

The flip is **random**, drawn via `sample()` without seeding. Different runs of the same policy produce different individual reassignments but identical aggregate counts. NAs are excluded from both pools.

### 4.4 Data modification — `.apply_health_travel(x, mode, pct, max_min)`

- `mode = "pct"`: `x * (1 + pct/100)`. Negative `pct` reduces travel time (a service improvement).
- `mode = "max"`: `pmin(x, max_min)` — cap, leaving households already below the cap unchanged.

### 4.5 Activation gate

`apply_policy_to_svy()` short-circuits the entire infra block when *all* infra inputs are zero / non-universal (see `has_infra_change` at `fct_policy_sim.R` ~lines 214–227). This avoids touching the survey when the user hasn't changed any infra slider.

---

## 5. Digital Inclusion (`mod_3_03_digital.R`)

Same shape as infrastructure, with two pairs:

| UI slug | Survey column |
|---|---|
| `internet` | `internet` |
| `mobile` | `cellphone` |

`digital_scenario()` returns `internet_universal`, `internet_access_change_pct`, `mobile_universal`, `mobile_access_change_pct`. Modifications go through the same `.apply_binary_access()` helper as infrastructure.

---

## 6. Labor Market (`mod_3_04_labor.R`)

### 6.1 UI inputs

- `labor_emp` — slider in −20 … +20 percentage points (default 0). Shown if any of `employed` / `selfemployed` / `unemployed` is in the model.
- `sector_manufacturing`, `sector_services` — sliders in 0 … 100 %. Agriculture is derived as `100 − manufacturing − services`. Shown only when all three of `agriculture` / `industry` / `services` are in the model.

### 6.2 Reactive output

`labor_scenario()` returns `employment_change_pp`, `sector_manufacturing`, `sector_services`, `sector_agriculture`.

### 6.3 Data modification — employment shift (`fct_policy_sim.R` ~lines 315–366)

Let `n_total = nrow(svy)` and `emp_change = employment_change_pp / 100`:

**Positive shift** (move people from `unemployed` to `employed`/`selfemployed`):
1. Compute the current employed:self-employed ratio among already-working observations.
2. `n_flip = min(round(n_total · emp_change), n_unemployed)`.
3. `sample(unemp_idx, n_flip)` and split that sample by the ratio. Set `unemployed <- 0`, then `employed <- 1` or `selfemployed <- 1` to maintain the existing mix.

**Negative shift** (move people from working to `unemployed`):
1. Pool `c(employed_idx, selfemp_idx)`.
2. `n_flip = min(round(n_total · |emp_change|), pool_size)`.
3. Sample, set `employed <- 0`, `selfemployed <- 0`, `unemployed <- 1`.

The sample is unseeded, so individual reassignments are random; the aggregate change in employment rate is exact (modulo `round()`).

### 6.4 Data modification — sectoral reallocation (`fct_policy_sim.R` ~lines 368–505)

The algorithm targets the sector mix `(p_ag, p_ind, p_ser) = (sector_agriculture, sector_manufacturing, sector_services) / 100` over **working** households (`employed == 1 | selfemployed == 1`). It minimises moves by only reallocating workers from sectors *above* their target to sectors *below* their target, in priority order:
1. Agriculture → industry / services
2. Industry → agriculture / services
3. Services → agriculture / industry

Each move flips `agriculture` / `industry` / `services` to keep them mutually exclusive. Non-working households are untouched.

---

## 7. Orchestration (`mod_3_05_policy_sim.R`)

### 7.1 Trigger

The "Run simulation" button (`mod_3_scenario.R` ~line 246) calls `s5$run()` in `mod_3_05_policy_sim_server`. The run function:

1. Requires Step 1 (`model_fit`) and Step 2 (`hist_sim`) to be available; otherwise records a `sim_error` and returns.
2. Captures the live residuals choice from Module 2 (`residuals() %||% "original"`) and stamps it onto `hist_sim$residuals` so that downstream display layers use the same residual treatment Module 2 is rendering with.
3. Stores `svy` in `baseline_svy_rv` and computes `svy_mod = apply_policy_to_svy(svy, infra, sp, digital, labor)` into `policy_svy_rv`.
4. **Baseline arm** — passes Step 2's `hist_sim` and `saved_scenarios` through verbatim. The baseline survey is unchanged, so re-running the pipeline would reproduce Step 2 exactly; passing through avoids the cost.
5. **Policy arm** — `resimulate_with_svy(svy_mod, sw, so, mf, hist_sim_baseline = hs_for_resim, saved_scenarios_baseline = ss, svy_baseline = svy)`. The `svy_baseline = svy` argument is what enables RIF policy mode in `run_sim_pipeline()` (it triggers `is_rif_policy` and the Stage B correction — see [`method_rif.md`](method_rif.md) §2.5).
6. Decomposes effects via `decompose_policy_effect()` against *historical mean weather* (`hs$weather_raw`), and again per `(saved scenario, sim_year)` for the time-resolved diagnostics in the Decomposition tab.
7. Increments `sim_run_id` so downstream tabs re-render.

### 7.2 Return API

| Reactive | Contents |
|---|---|
| `baseline_svy` | unmodified `svy` |
| `policy_svy` | `svy_mod` from `apply_policy_to_svy()` |
| `baseline_hist_sim`, `baseline_saved_scenarios` | Step 2 outputs, stamped with current residuals choice |
| `policy_hist_sim`, `policy_saved_scenarios` | Outputs of `resimulate_with_svy()` against `svy_mod` |
| `decomp_result` | `decompose_policy_effect()` against historical mean weather |
| `decomp_scenarios` | Same, expanded over `(scenario, sim_year)` |
| `sim_run_id` | Integer counter; triggers downstream tab inserts |

### 7.3 Re-simulation — `resimulate_with_svy()` (`fct_policy_sim.R` ~line 582)

Reuses the Step 2 fit and coefficient draws, threading the policy survey through `run_sim_pipeline()`:

```r
run_sim_pipeline(
  weather_raw  = <hist or scenario weather>,
  svy          = svy_mod,
  model        = mf$fit3   (or extract_rif_median for RIF),
  fit_multi    = mf$fit3   (RIF only — multi-LHS fixest object),
  chol_obj     = hist_sim_baseline$chol_obj,
  taus         = mf$taus,
  weather_cols = mf$weather_terms,
  rif_grid     = mf$rif_grid,
  svy_baseline = svy_baseline,    # ← enables RIF policy mode
  …
)
```

The output schema matches Step 2's `run_full_simulation()`: `hist_sim` nests its single run under `$pipeline`, each saved scenario carries a named `$pipelines` list — one entry per CMIP6 ensemble member. This is what gives Module 3 the same inter-model spread display Module 2 produces.

---

## 8. How `svy_mod` Feeds the Simulation

### 8.1 Covariate channels (linear and RIF)

For any survey column the model uses as a regressor, `apply_policy_to_svy()`'s flips show up directly in the design matrix at prediction time:

- **Linear engine**: `fixest::predict(model, newdata = survey_wd_sim, type = "response")` evaluates the full linear predictor at the policy values. Main effects (`β_v · x_policy_v`) and interactions (`β_{w:v} · w · x_policy_v`) both activate automatically. See [`method_lin.md`](method_lin.md) §2.5.
- **RIF engine**: `predict_rif()` is called *at baseline covariates* (`svy_for_predict = svy_baseline` when `is_rif_policy`), and the full channel decomposition is then added explicitly via `.compute_rif_policy_correction()` returning `delta_total = δ_sp + δ_main_covar + δ_res1 + δ_res2`. See [`method_rif.md`](method_rif.md) §2.5.

### 8.2 SP cash channel

The `._sp_transfer` column is **not a regressor** — the model was not fit with cash transfers as a covariate. `run_sim_pipeline()` therefore reads the column post-prediction and adds it as a level-scale welfare boost:

```r
welfare_lvl <- exp(y_point) + sp_vec
y_point     <- log(welfare_lvl)            # re-logged when so$transform == "log"
```

This is mathematically equivalent to `δ_sp = log(exp(y) + SP_cash) − y`, which is also the form used by `.compute_rif_channels()` and `.decompose_ols()` for the decomposition's `δ_sp` channel. The level-scale block is gated on `!is_rif_policy && SP_TRANSFER_COL %in% names(out)` — see §3.5.

### 8.3 Uncertainty propagation

Policy modifications affect the design matrix `X_policy`, which feeds the factor loading `F_loading = X_policy %*% L` (linear) or the interpolated per-τ blend (RIF). Coefficient uncertainty therefore propagates through the policy survey exactly as it does through the baseline survey — see [`method_uncertainty.md`](method_uncertainty.md) §1.1 and §5 (paired contrasts).

**Additive-decomposition active mask.** Under the default coefficient-uncertainty mode (§1.1.1 of `method_uncertainty.md`), `resimulate_with_svy()` rebuilds the active mask after `apply_policy_to_svy()` runs: `active_terms = weather_terms ∪ detect_modified_cols(svy_policy, train_data)`. The detected columns are exactly those the policy flipped (e.g. `electricity`, `internet`, `employed`, `imp_wat_rec`), so the policy arm's `F_loading` keeps coefficients on (weather + policy-modified + their interactions) and drops everything else. The mask is stripped first to avoid inheriting Module 2's weather-only mask. The legacy full-β propagation is restored when the Step-2 "Include uncertainty on all covariates" toggle is set, which `mod_3_05_policy_sim.R` stamps onto `hist_sim_baseline$propagate_all_covariate_uncertainty` for `resimulate_with_svy()` to read.

The SP transfer itself is treated as deterministic (no SE) — `apply_policy_to_svy()` writes a fixed daily amount per household.

---

## 9. How `svy_mod` Feeds the Decomposition

`decompose_policy_effect()` (`fct_policy_decompose.R` ~line 454) dispatches on engine:

- **Linear** → `.decompose_ols()` computes `δ_sp`, `δ_main_covar = Σ β_v · Δv`, `δ_res2 = Σ β_{w:v} · w · Δv`, and `δ_res1 = 0`. See [`method_lin.md`](method_lin.md) §2.7.
- **RIF** → `.decompose_rif()` delegates to `.compute_rif_channels()`, the same helper the simulation correction calls. The decomposition table and the simulation `y_point` therefore reconcile exactly in model scale. See [`method_rif.md`](method_rif.md) §2.5.

`.compute_policy_deltas(svy_baseline, svy_policy, outcome, weather_cols)` builds the per-household `Δv = x_policy − x_baseline` vectors that both `δ_main_covar` and `δ_res2` consume. It compares the two frames column-by-column, so any covariate touched by `apply_policy_to_svy()` (binary flips, sector reallocations, health travel time scaling) shows up as a non-zero `Δv` for the affected rows.

`hazard_values` for the decomposition are built by `.compute_hazard_values()` and represent per-location mean (continuous weather) or per-location modal bin (factor weather) — see `fct_policy_decompose.R` ~line 284.

---

## 10. Inputs the Survey *Cannot* Express

A few inputs in the Module 3 sidebar produce **no change** to `svy_mod` and therefore have **no effect** on simulated welfare:

- **Targeting accuracy errors when `targeting = "universal"`** — `.determine_sp_eligibility()` skips the error blocks, so `inclusion_error_pct` / `exclusion_error_pct` are ignored.
- **PMT inputs when `targeting ≠ "pmt"`** — `pmt_variable` and `pmt_cutoff` are read only inside the PMT branch.
- **Sector mix when employment columns are absent** — the sector reallocation is gated on `all(c("employed", "selfemployed", "agriculture", "industry", "services") %in% cols)`.
- **Any infra/digital slider for a column not in the survey** — `apply_policy_to_svy()` guards each modifier with `if ("<col>" %in% cols)`.
- **Any covariate not in the Model 3 fit** — the slider may be present (if the column exists in the survey) and the survey will be modified, but the prediction is unchanged because no β multiplies it.

This last point is the most common source of confusion: a user can flip a covariate that the model never saw, and the policy survey will show the change, but neither the simulation nor the decomposition will move. The Diagnostics tab (`mod_3_07_diagnostics.R`) compares `baseline_svy` and `policy_svy` directly so the user can verify the modification independent of the model.

---

## 11. Summary Table

| User input | Module | `apply_policy_to_svy()` effect on `svy` | Enters prediction via |
|---|---|---|---|
| SP transfer amount, eligibility, errors | mod_3_01 | Adds `._sp_transfer` (daily $/hh) on eligible rows | Post-prediction level-scale boost (linear) or `δ_sp` inside `delta_total` (RIF) |
| Infrastructure access toggles | mod_3_02 | Flips 0/1 in `electricity`, `imp_wat_rec`, `imp_san_rec`, `piped`, `piped_to_prem`, `imp_wat_san_rec` | β·Δx through the design matrix |
| Health travel time | mod_3_02 | Scales / caps `ttime_health` | β·Δx through the design matrix |
| Internet / mobile access | mod_3_03 | Flips 0/1 in `internet`, `cellphone` | β·Δx through the design matrix |
| Employment rate change | mod_3_04 | Re-balances `employed` / `selfemployed` / `unemployed` | β·Δx through the design matrix |
| Sector mix | mod_3_04 | Re-balances `agriculture` / `industry` / `services` (working only) | β·Δx through the design matrix |

For every row above, weather × covariate interactions in Model 3 also activate when the corresponding column is flipped — captured automatically by the linear engine's `predict()` and via `δ_res2` in the RIF decomposition / correction.
