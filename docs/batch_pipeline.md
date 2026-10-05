# Batch Pipeline — WISE-APP

Scripts in `batch/` run the full WISE-APP pipeline headlessly (no Shiny UI).
All are sourced directly: `source("batch/XX_name.R")`.
All configuration is in **Section 1** of each script — nothing else needs editing.

---

## Scripts

| Script | Purpose | Status |
|--------|---------|--------|
| `01_survey_stats.R` | Survey summary stats, interview-date plots, location maps, welfare distributions | Complete |
| `02_weather_stats.R` | Weather summary stats and distribution plots per country/variable | Complete |
| `03_run_mod1.R` | Grid of model fits (OLS / RIF × FE profiles × covariate specs × interactions) → coefficient and fit-stat CSVs | Complete |
| `04_run_sim.R` | Same model-fitting grid → climate simulations (mod_2) → policy simulations (mod_3) | **Step 1 complete; Steps 2–3 stubs** |

Shared helpers loaded by all scripts: `batch/R/aaa_load.R`, `batch_utils.R`, `expand_weather_specs.R`, `spec_curve.R`.

---

## Output directory layout

`OUT_DIR` defaults to `dev/outputs/` (set in Section 1 of each script).

```
OUT_DIR/
├── survey_stats/
│   ├── survey_stats.csv
│   ├── interview_dates/{CODE}_interview_dates.png
│   ├── location_maps/{CODE}_location_map.png
│   └── welfare_distributions/{CODE}_welfare_dist.png
│
├── weather_stats/
│   ├── weather_stats.csv
│   └── weather_distributions/{CODE}_{BASEVAR}_dist.png
│
├── model_fit/                         ← written by 03_run_mod1.R
│   ├── model_coefficients.csv
│   ├── model_fit_stats.csv
│   ├── _failures.csv
│   └── _interactions_not_available.csv
│
├── simulations/                       ← written by 04_run_sim.R (Step 2, not yet implemented)
│   ├── outcomes.csv
│   ├── sim_stats.csv
│   └── _failures.csv
│
└── policy/                            ← written by 04_run_sim.R (Step 3, not yet implemented)
    ├── policy_outcomes.csv
    └── _failures.csv
```

---

## `01_survey_stats.R` — survey_stats.csv columns

One row per `(code, year, variable)`. All numeric stats are sample-weighted. `pct_missing` is expressed as 0–100.

Poverty binary indicators (`poor300`, `poor420`, `poor830`) are auto-injected as variables (mean = poverty headcount rate at $3.00/$4.20/$8.30 PPP/day).

| Column group | Columns |
|---|---|
| Identifiers | `code, economy, survname, year` |
| Variable metadata | `var_group` (`outcome / policy / hh / ind / area / firm / other`), `variable` |
| Full-sample stats | `mean, sd, min, max, n_unique, n, pct_missing` |
| Interview date stats | `n_dates, min_date, max_date, avg_dates_per_loc` |
| Subsample stats (repeated with prefix) | `with_loc_*`, `without_loc_*`, `within_loc_*` — each has `mean, sd, min, max, n_unique, n, pct_missing` |

`within_loc_*` stats are computed on loc_id-demeaned values (within-location variation only).

---

## `02_weather_stats.R` — weather_stats.csv columns

One row per `(code, year, variable)` for each weather spec in `WEATHER_SPECS`. An additional row per `(code, variable)` is appended for the climate reference period (identified by `survname = "Climate reference YYYY-YYYY"`, `year = NA`; unweighted, `pct_missing = NA`).

| Column group | Columns |
|---|---|
| Identifiers | `code, economy, survname, year` |
| Variable metadata | `variable, ref_period` (e.g. `1to12m`), `temporal_agg, transformation, wx_spec` |
| Full-sample stats (weighted) | `mean, sd, min, max, n_unique, n, pct_missing` |
| Percentiles (weighted) | `p10, p20, p30, p40, p50, p60, p70, p80, p90` |
| Within-location | `within_loc_mean, within_loc_sd` (loc_id-demeaned) |
| Spatial variation | `n_unique_per_loc` (mean distinct values per loc_id) |

---

## `03_run_mod1.R` — model fitting

**Grid dimensions** (all `[GRID]` settings expand via `expand.grid()`):
- `WEATHER_SPECS` — named list of weather profiles (use `expand_weather_specs()` or define manually)
- `MODEL_TYPE` — `"Linear regression"`, `"Unconditional quantile regression (RIF)"`
- `INTERACTIONS` — list of interaction variable vectors; `character(0)` = none
- `FIXED_EFFECTS` — named list of FE vectors (e.g. `year_admin1 = c("year","gaul1_code")`)
- `COVARIATE_SPECS` — named list; each entry is `method = "User-defined"` (supply vars by role) or `method = "Lasso"`

**Key design:**
- `fit1` (weather only) and `fit2` (weather + FE) are deduplicated across covariate profiles
- `fit3` (weather + FE + controls) is always unique per spec
- Lasso: multiple-imputation stability selection via `run_lasso_selection()`; supports parallelisation via `LASSO_USE_PARALLEL`
- Post-treatment variables (`outcome == 1` in `variable_list.csv`) are excluded from covariate candidate pool via `exclude_selected_vars()`
- Checkpoints every 20 specs to `_checkpoint_*.csv`; cleaned up on successful completion

**Columns in `model_coefficients.csv`:** `code, weather, engine, fe_profile, cov_profile, cov_method, interaction, fixedeffects, model, term, estimate, std.error, statistic, p.value, tau, estimand`

**Columns in `model_fit_stats.csv`:** same metadata columns + `r2, r2_adj, r2_within, aic, n_obs_fit, n_hh_total, n_hh_weather, pct_weather, lasso_selected`

---

## `04_run_sim.R` — simulations + policy

Shares Section 1 model-fitting config with `03_run_mod1.R` (identical settings block).

**Additional config sections:**

*Section 1B — Simulation (mod_2):*
- `HIST_YEARS` — historical weather distribution window (default 1991–2020)
- `FUT_PERIOD_1/2/3` — up to 3 future projection periods; `NULL` to skip
- `SSPS` — SSP scenarios (`"ssp2_4_5"`, `"ssp3_7_0"`, `"ssp5_8_5"`); `character(0)` = historical only
- `RESIDUALS` — `"original"` (recommended) / `"resample"` / `"normal"` / `"none"`
- `INCLUDE_COEF_UNCERTAINTY`, `PROPAGATE_ALL_COVARIATE_UNCERTAINTY` — delta-method SE controls
- `SIM_N` — MC fallback draws (delta-method used for standard aggregates)

*Section 1C — Policy (mod_3):*
- `SP_SCENARIO` — cash transfer (targeting, budget mode, transfer amount, frequency)
- `INFRA_SCENARIO` — infrastructure access changes (electricity, water, sanitation, piped, health travel time)
- `DIGITAL_SCENARIO` — internet and mobile access
- `LABOR_SCENARIO` — employment rate change and sectoral composition

**Pipeline sections:**
- Section 2: shared setup (connection, metadata, survey list, grid)
- Section 3 ✅: model fitting loop — populates `fit_store` (in-memory, not saved to disk)
- Section 4 🔲: climate simulations — calls `fct_run_simulation()` per spec
- Section 5 🔲: policy simulations — calls `apply_policy_to_svy()` + `resimulate_with_svy()` or `apply_policy_delta_to_baseline()`

**`fit_store` structure** (keyed by `spec_label`):

```r
fit_store[["GNB_t_12m_ols_year_admin1_hhsize_urban_area_noInter"]] <- list(
  mf            = <fit_model() output>,   # fit3, engine, train_data, weather_terms, taus
  svy_baseline  = <data.frame>,           # most-recent survey year → passed to fct_run_simulation() as svy
  svy_wx        = <data.frame>,           # full multi-year merged data → base for apply_policy_to_svy()
  ss            = <selected_surveys df>,
  sw            = <selected_weather df>,
  so            = <selected_outcome>,
  stored_breaks = <named list or NULL>,
  wx_col_names  = <character vector>,
  code, wx_name, mt_label, fe_label, cov_label, inter_label
)
```