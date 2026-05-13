# Climate Scenarios: Constructing Counterfactual Future Weather in WISE-APP

This document describes how WISE-APP turns the user's Step 2 climate-scenario settings (historical period, SSP choices, future horizons) into a perturbed monthly weather series that feeds the simulation pipeline.

The app implements the **delta method** (a.k.a. change-factor or perturbation method) at monthly resolution, applied per CMIP6 ensemble member and per location. Variance scaling and Quantile Delta Mapping (QDM) are not implemented — see §6.

For the conceptual background, see [issue #32](https://github.com/bbrunckh/welfare-weather-app/issues/32) "Climate change perturbation method", which documents the delta method (and discusses QDM as a more elaborate alternative).

---

## 1. Method Overview

The delta method assumes that GCMs cannot reliably simulate the absolute level of temperature or precipitation at a specific location, but can credibly project the *change* between a historical and a future period. The change ("delta") is computed from the GCM and applied to an observed historical series, which is treated as the local climatological anchor.

For each weather variable v, each CMIP6 ensemble member m, each (location, calendar month) pair (ℓ, μ):

```
Δ_add(v, m, ℓ, μ) = μ̄_fut(v, m, ℓ, μ) − μ̄_hist(v, m, ℓ, μ)               ← additive (temperature, indices)
Δ_mul(v, m, ℓ, μ) = (μ̄_fut(v, m, ℓ, μ) + ε) / (μ̄_hist(v, m, ℓ, μ) + ε)    ← multiplicative (precipitation, day counts)
```

where `μ̄_hist` / `μ̄_fut` are the GCM's monthly climatology means over the user-chosen historical and future periods, and `ε = 0.001` is a guard against division by zero in dry months.

The perturbed weather is then assembled per observed timestamp t (which falls in calendar month μ(t)):

```
x_fut(v, m, ℓ, t) = x_obs(v, ℓ, t) + Δ_add(v, m, ℓ, μ(t))    [additive]
                  = x_obs(v, ℓ, t) × Δ_mul(v, m, ℓ, μ(t))    [multiplicative]
```

The result inherits the inter-annual variability, autocorrelation, and seasonality of the observed series; only the **mean of each calendar month** shifts. 

---

## 2. User Inputs (`mod_2_01_weathersim.R`)

The Step 2 sidebar exposes these climate-relevant controls:

| Input | UI control | Default | Purpose |
|---|---|---|---|
| `hist_years` | range slider (1950–2024) | `c(1991, 2020)` | Historical period — sets both the *observed* anchor window and the *GCM historical* baseline used to compute the delta. A `<30-year` warning is shown (line 367). |
| `climate` | checkbox group | `"ssp3_7_0"` | One or more SSPs from `{ssp2_4_5, ssp3_7_0, ssp5_8_5}`. |
| `fut_start_{1,2,3}` / `fut_end_{1,2,3}` | numeric pairs | period 1: `2025–2035` | Up to three future periods. Empty pairs are ignored. Each period is a year band over which the GCM future climatology is averaged. |
| `baseline_survey` | select (multi) | Step 1 default | Survey × year(s) that define the microdata population. |
| `residuals` | radio | `"original"` | Residual-draw mode (not climate-specific — see [`method_uncertainty.md`](method_uncertainty.md) §1.2). |
| `dev_mode` | checkbox | off | Caps each SSP × period at 1 GCM member for fast iteration. |

Two scenario tables are built from these inputs:

- `selected_hist()` — one row, `type = "historical"`, `year_range = list(c(hist_years[1], hist_years[2]))`. (mod_2_01_weathersim.R ~lines 390–399)
- `selected_fut()` — one row per `(SSP × future_period)`, `type = "future"`, `method = "delta"`. (~lines 413–441)

When the user clicks **Run simulation**, both tables are passed to `fct_run_simulation()` which orchestrates the weather pull and the simulation per scenario.

---

## 3. Observed Historical Weather

`get_weather()` (`fct_get_weather.R`) builds the historical anchor in three steps. 

### 3.1 Spatial harmonisation (H3)

ERA5-Land observations and microdata are joined on a common H3 grid resolution. `.harmonise_h3()` selects the coarser of the two resolutions and uses `h3_cell_to_parent()` to map fine cells up to that resolution (DuckDB H3 extension). Households are joined to weather via `h3_slim`.

### 3.2 Population-weighted spatial aggregation to `loc_id`

`.pop_weighted_mean()` reduces each H3 cell's weather to per-`loc_id`, per-`timestamp` values weighted by population:

```
loc_monthly: (code, year, survname, loc_id, timestamp, v1, v2, …)
```

with one row per location × month spanning `(hist_years[1] − max_lag_months) … hist_years[2]`. This is materialised as a temporary DuckDB table `loc_weather_base` and reused for every SSP × period in the climate branch.

### 3.3 Rolling window

For each weather variable, a window expression of the form

```sql
agg_fn(v) FILTER (WHERE v IS NOT NULL)
  OVER (PARTITION BY code, year, survname, loc_id
        ORDER BY timestamp
        ROWS BETWEEN ref_end PRECEDING AND ref_start PRECEDING)
```

implements the user's chosen lag window from Step 1 (e.g., 3-month trailing mean of `tx`). The lag window is the same for historical and future weather — the climate branch re-applies it on the perturbed series (§4.5) so that each GCM member rolls over its own perturbed values.

The historical scenario output filters `loc_weather_base` to the survey-month `sim_dates` and (optionally) applies an anomaly transformation. It is returned to the caller as `result[["historical"]]`.

---

## 4. Future Weather Construction (Delta Method)

The climate branch (`fct_get_weather.R` ~lines 526–797) runs only when one or more future scenarios are present. It is a single DuckDB pipeline expressed via `dbplyr`.

### 4.1 Perturbation method per variable

`build_perturbation_method()` in `fct_simulations.R` (~line 724) classifies each weather variable:

```r
method <- ifelse(selected_weather$units %in% c("mm", "days"),
                 "multiplicative",
                 "additive")
```

- **Multiplicative**: units `"mm"` (precipitation totals) or `"days"` (e.g., heavy-rain day counts).
- **Additive**: everything else — most importantly °C temperature and dimensionless indices.

This is the standard convention from the delta-method literature (precipitation respects the non-negativity constraint better under a ratio; temperature anomalies are naturally additive).

### 4.2 CMIP6 monthly climatology

`.cmip6_h3_monthly(fnames, ts_start, ts_end)` (`fct_get_weather.R` ~line 626) reads CMIP6 monthly parquet files (`hazard/weather/projections/{code}/{code}_cmip6_{ssp|historical}.parquet`), restricts to `[ts_start, ts_end]`, and aggregates to the long-term monthly climatology:

```r
dplyr::group_by(model, h3, month) |>
  dplyr::summarise(across(all_of(weather_vars), ~ mean(.x, na.rm = TRUE)))
```

**The aggregation is by *calendar month*, not by `(year, month)`.** A 30-year window therefore collapses to 12 values per `(model, h3)` (one per calendar month). This is the canonical Step 2 of the delta method.

`.cmip6_h3_monthly()` is invoked three times per SSP:

| Call | Files | Window | Purpose |
|---|---|---|---|
| `h3_hist_raw` | `*_cmip6_historical.parquet` | `[baseline_start, baseline_end]` | GCM historical baseline (per `hist_years`) |
| `h3_ssp_raw` | `*_cmip6_{ssp}.parquet` | `[baseline_start, baseline_end]` | SSP file's coverage *of* the historical window (CMIP6 SSP files often extend a few years before 2015) |
| `h3_fut` | `*_cmip6_{ssp}.parquet` | `[fp_start, fp_end]` | Future climatology per period |

`h3_hist_raw` and `h3_ssp_raw` are unioned and re-aggregated to form `h3_hist` — a single monthly climatology that spans both halves of the baseline window. This guards against the cut-off year (typically 2014/2015) splitting the user's historical period across two CMIP6 files.

> **Implication of using `hist_years` as the GCM baseline.** Unlike textbook implementations (which often fix the GCM baseline to WMO normals 1961–1990 or 1991–2020), WISE-APP uses **the user's `hist_years`** as the GCM baseline too. This is internally consistent — both sides of the delta and the observed anchor are over the same window — but it means a user choosing, say, a 10-year baseline gets a less stable GCM climatology than a user picking the full 1991–2020 window. The UI's `<30-year` warning (mod_2_01_weathersim.R ~line 367) is the only nudge.

### 4.3 Per-(model, H3, month) delta

`.make_delta_exprs(perturbation_method, weather_vars, epsilon)` (~line 537) emits one SQL expression per weather variable:

```r
if (method[[v]] == "additive") {
  sql("<v>_fut - <v>_hist")
} else {
  sql("(<v>_fut + <epsilon>) / (<v>_hist + <epsilon>)")   # epsilon = 0.001
}
```

These are applied to `inner_join(h3_hist, h3_fut, by = c("model","h3","month"))`, producing:

```
h3_deltas: (model, h3, month, delta_v1, delta_v2, …)     -- 12 rows per (model, h3)
```

The `epsilon = 0.001` constant (`fct_get_weather.R` ~line 361) is the dry-month guard recommended by [issue #32, comment 1, §3](https://github.com/bbrunckh/welfare-weather-app/issues/32).

### 4.4 Spatial aggregation of the delta to `loc_id`

`h3_deltas` is joined to `h3_slim` on `h3_cmip6` (which may itself be a coarsened H3 cell when CMIP6 grids are coarser than the microdata, see ~lines 605–624) and aggregated to `loc_id` via population-weighted mean:

```r
loc_deltas_by_model <- h3_deltas |>
  inner_join(h3_slim, by = c("h3" = "h3_cmip6")) |>
  group_by(model, code, year, survname, loc_id, month) |>
  .pop_weighted_mean(delta_vars)
```

This is materialised as a temporary DuckDB table for hash-join planning. Models with no complete delta rows are filtered out with a warning (`fct_get_weather.R` ~lines 730–744).

### 4.5 Applying the delta to the observed series

`.make_perturb_exprs()` (~line 552) emits one SQL expression per variable:

```r
if (method[[v]] == "multiplicative") sql("<v> * delta_<v>")
else                                  sql("<v> + delta_<v>")
```

`loc_monthly` (the observed monthly weather built in §3.2) is joined to `loc_deltas_by_model` on `(code, year, survname, loc_id, month)` — note that `month = MONTH(timestamp)` is recomputed at join time so each observed timestamp receives the delta for its calendar month — and the perturbation expressions are evaluated:

```r
perturbed <- loc_monthly |>
  mutate(month = MONTH(timestamp)) |>
  inner_join(loc_deltas_by_model, by = c("code","year","survname","loc_id","month")) |>
  mutate(!!!perturb_exprs) |>
  select(model, code, year, survname, loc_id, timestamp, all_of(weather_vars))
```

This materialises one perturbed weather row per `(model, code, year, survname, loc_id, timestamp)`.

### 4.6 Rolling window and transformations on the perturbed series

The rolling window is **re-applied to the perturbed series**, with `model` added to `PARTITION BY` so each GCM member has its own independent window:

```sql
agg_fn(v) FILTER (WHERE v IS NOT NULL)
  OVER (PARTITION BY model, code, year, survname, loc_id
        ORDER BY timestamp
        ROWS BETWEEN ref_end PRECEDING AND ref_start PRECEDING)
```

Anomaly transformations (`.apply_transformations()`, ~line 131) are then applied:

- `"Deviation from mean"`: `x − ref_mean(month)`
- `"Standardized anomaly"`: `(x − ref_mean(month)) / ref_sd(month)`

> **Note on the reference period for transformations.** The `ref_mean` / `ref_sd` are computed over the fixed window **1991-01-01 to 2020-12-31** in `loc_weather_base` (`.apply_transformations()` ~lines 144–155), regardless of the user's `hist_years` choice. This is a deliberate choice — the anomaly reference is the WMO climate normal, distinct from the delta-method baseline. Users picking a non-default `hist_years` should be aware that the GCM delta and the anomaly transformation use different reference windows.

Finally the batch is filtered to the requested `sim_dates`, collected into R, split by `model`, and returned in `result[[paste0(ssp_i, "_", fp_label, "_", make.names(model_name))]]`.

### 4.7 Output schema

`get_weather()` returns a named list of data frames:

```
result$historical                            # ERA5 over hist_years (one frame)
result$ssp2_4_5_2025_2035_MPI.ESM1.2.HR      # perturbed weather, one frame per
result$ssp2_4_5_2025_2035_ACCESS.ESM1.5      #   (SSP, future period, GCM member)
result$ssp3_7_0_2025_2035_MPI.ESM1.2.HR
…
```

Each data frame has the same column schema:

```
code, year, survname, loc_id, timestamp, v1, v2, …
```

with one row per `(loc_id, timestamp)` for the requested `sim_dates`. Downstream, `fct_run_simulation.R` groups these by `(ssp, future_period)` to form `saved_scenarios`, with `pipelines = list(<one entry per GCM member>)` — this is what gives Module 2 / Module 3 their inter-model spread (see [`method_uncertainty.md`](method_uncertainty.md) §1.4 and §4).

---

## 5. End-to-End Flow

```
USER (mod_2_01_weathersim.R)
  hist_years        ── selected_hist()  ── ┐
  climate (SSPs)    ── selected_fut()   ── │
  fut_start/end_*                          │
                                            ▼
                              fct_run_simulation()
                                            │
                                            ▼
                              get_weather()  (fct_get_weather.R)
                                            │
              ┌─────────────────────────────┼──────────────────────────────────────┐
              ▼                             ▼                                       ▼
       Historical branch              Climate branch (per SSP)                  …
       (§3)                           (§4)
         │                              │
         │                              ├─ .cmip6_h3_monthly(hist_files, baseline)  ── h3_hist_raw
         │                              ├─ .cmip6_h3_monthly(ssp_files,  baseline)  ── h3_ssp_raw
         │                              ├─ union + group_by(model, h3, month)        ── h3_hist
         │                              │
         │                              └─ for each future period:
         │                                   ├─ .cmip6_h3_monthly(ssp_files, fp)     ── h3_fut
         │                                   ├─ inner_join + .make_delta_exprs       ── h3_deltas
         │                                   ├─ inner_join h3_slim + pop-weighted    ── loc_deltas_by_model
         │                                   ├─ filter incomplete models
         │                                   ├─ inner_join loc_monthly + perturb     ── perturbed
         │                                   ├─ rolling window + transformations
         │                                   └─ split by model
         ▼                                   ▼
   result$historical               result$<ssp>_<period>_<model>
                                            │
                                            ▼
                              run_sim_pipeline()  (fct_simulations.R, per scenario × per member)
                                            │
                                            ▼
                              aggregate_with_uncertainty_delta()
                                            │
                                            ▼
                              combine_ensemble_results()         ← pools across members
                                            │
                                            ▼
                              mod_2_02_results.R / mod_3_06_results.R
```

---

## 6. What Is Not Implemented

[Issue #32](https://github.com/bbrunckh/welfare-weather-app/issues/32) discusses several extensions to the basic delta method. The current implementation is the simplest form:

| Feature | Status |
|---|---|
| Additive delta (T, indices) | ✅ Implemented |
| Multiplicative delta (precip, day counts) | ✅ Implemented with `ε = 0.001` dry-month guard |
| Monthly stratification (12 separate deltas) | ✅ Per `(model, loc_id, calendar month)` |
| Multi-GCM ensemble | ✅ One perturbed frame per member; spread shown in results |
| Multi-SSP, multi-period | ✅ Independent batches per `(SSP × period)` |
| Population-weighted spatial aggregation | ✅ Via `.pop_weighted_mean()` |
| Variance scaling (issue #32 §5) | ❌ Not implemented. Perturbed-series variance is inherited from the *observed* record. The σ_fut/σ_hist ratio recommended for temperature is not applied. |
| Quantile Delta Mapping (issue #32 comment 2) | ❌ Not implemented. Only first-moment (mean) deltas; tail-specific changes are not propagated. Sensitive analyses of extremes should be interpreted with this in mind. |
| Wet-day frequency correction | ❌ Multiplicative delta is applied uniformly; changes in dry/wet day frequency are not separately modelled. |

The decision to ship only the standard delta method reflects the typical use case (mean welfare impacts in country-level economic analysis), where the trade-off of distributional fidelity for tractability and interpretability is accepted. QDM and variance scaling remain open for future work — see issue #32 for the comparison.

---

## 7. Uncertainty Implications

Once perturbed weather is produced, the simulation pipeline propagates four uncertainty sources documented in [`method_uncertainty.md`](method_uncertainty.md):

| Source | How climate scenarios contribute |
|---|---|
| Coefficient uncertainty | Same Cholesky factor `L` from Step 1, regardless of scenario. Each member's design matrix sees its own perturbed weather, so `F_loading` differs across members but the underlying `β̂` covariance does not. |
| Residual variability | Inherited from training residuals; perturbation does not change residuals. |
| Inter-annual variability | Each member's perturbed series has 30+ years (or whatever `hist_years` spans), giving `sim_year` axis spread in the results. **The delta method does not change the year-to-year variability of the perturbed series** — that's the variance-scaling gap noted above. |
| Inter-model (GCM) spread | One member per CMIP6 model produces a distinct point estimate; `combine_ensemble_results()` reports the spread as the "thick band" (see [`method_uncertainty.md`](method_uncertainty.md) §4). |

Because variance scaling is not applied, **the "inter-annual" component of uncertainty is the observed historical variability, not a GCM-projected future variability**. Practically, this means the per-`sim_year` spread in future scenarios is the same shape as in the historical scenario, shifted by the per-(model, month) delta. Inter-model spread is the dominant source of "climate uncertainty" in the UI bands.

---

## References

- Issue [#32 *Climate change perturbation method*](https://github.com/bbrunckh/welfare-weather-app/issues/32) — comment 1 details the delta method (the implemented approach); comment 2 details QDM as a possible extension.
- Hay, L. E., et al. (2000). Statistical downscaling for the assessment of climate impacts. *J. Climate*, 13(18), 3268–3284.
- Cannon, A. J., Sobie, S. R., & Murdock, T. Q. (2015). Bias correction of GCM precipitation by Quantile Delta Mapping. *J. Climate*, 28(17), 6938–6959.
- Maurer, E. P., & Hidalgo, H. G. (2008). Utility of daily vs. monthly large-scale climate data: an intercomparison of two statistical downscaling methods. *Hydrol. Earth Syst. Sci.*, 12(2), 551–563.
- Maraun, D. (2016). Bias correcting climate change simulations — a critical review. *Current Climate Change Reports*, 2(4), 211–220.
