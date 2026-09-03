# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

**WISE-APP** (Weather Impact Simulation and Evaluation for Adaptation Policy and Planning) is an R Shiny web application built by the World Bank. It estimates relationships between weather and household welfare, simulates welfare outcomes under climate scenarios, and evaluates policy/adaptation strategies.

- R package name: `wiseapp`
- Framework: [Golem](https://thinkr-open.github.io/golem/) (production-ready Shiny scaffolding)
- R version: 4.5.3 (pinned via `renv.lock`)
- License: MIT

## Development Commands

```r
# Install dependencies (restore from renv.lock)
renv::restore()

# Run the app locally
wiseapp::run_app()
# or from the R console:
source("R/run_app.R"); run_app()

# Document (regenerate man/ and NAMESPACE)
devtools::document()

# Load all R code during development
devtools::load_all()

# Check package
devtools::check()

# Run tests
testthat::test_dir("tests")
# or
devtools::test()
```

Development workflow scripts are in `dev/01_start.R`, `dev/02_dev.R`, and `dev/03_deploy.R`.

## Architecture

### 3-Step Pipeline (tabs in the UI)

1. **Step 0 – Overview** (`mod_0_overview`): Data source configuration (local/S3/GCS/Azure/HuggingFace/Databricks), loads survey metadata.
2. **Step 1 – Modelling** (`mod_1_modelling` + 8 sub-modules): Select sample → explore data → define outcome variable → pick weather variables → configure model → view results.
3. **Step 2 – Simulation** (`mod_2_simulation` + 3 sub-modules): Define historical/future weather scenarios (including CMIP6 climate models), generate welfare predictions.
4. **Step 3 – Policy Scenarios** (`mod_3_scenario` + 5 sub-modules): Model social protection, infrastructure, digital, and labor market interventions and estimate welfare impacts.

Reactive data flows forward through the pipeline: Step 0 outputs feed Step 1, which feeds Steps 2 and 3.

### Code Organization

```
R/
├── app_ui.R / app_server.R    # Top-level app wiring
├── app_config.R               # Environment detection (dev/Posit Connect/Databricks)
├── run_app.R                  # Entry point
├── mod_*.R                    # 24 Shiny modules (each has a UI and server function)
└── fct_*.R                    # 25 business logic files (no Shiny dependencies)
```

**`fct_` files are the core engine:**
- `fct_connection.R` – builds connection params for each storage backend
- `fct_load_data.R` – data ingestion and validation via DuckDB
- `fct_fit_model.R` – **engine registry**: pluggable modeling backends (fixest, ranger, xgboost, RIF)
- `fct_predict_outcomes.R` – prediction pipeline for fitted models
- `fct_simulations.R` – orchestrates the simulation pipeline (historical + future weather)
- `fct_sim_compare.R` – visualization and comparison functions (exceedance curves, threshold tables)
- `fct_results.R` – output formatting, coefficient plots, tables
- `fct_policy_sim.R` – policy scenario variable discovery and placeholder UI
- `fct_policy_decompose.R` – **policy effect decomposition** (main effect + resilience: repositioning + interaction)
- `fct_rif_sim.R` – Recentered Influence Function (RIF) quantile regression helpers
- `fct_weatherstats.R` – weather statistics computation
- `fct_hexmap.R` – **hex-map engine bridge**: vendored MapLibre GL + h3-js assets (`inst/app/www/vendor/`, pins + sha256 in-file), payload contract (`hexmap_payload()` — columnar cell ids/values/ramp stops, no geometry on the wire), senders (`hexmap_update`/`hexmap_clear`/`hexmap_fit`) and the `hexmap_ui()` container. Browser side: `inst/app/www/hexmap.js` (lazy boot, WebGL2 probe → `input$<id>_webgl` with Leaflet fallback, camera persistence, queued messages). Maps using it: sample density (mod_1_02), outcome coverage (mod_1_03), per-variable weather maps (mod_1_05); Leaflet builders remain as the runtime fallback.

### Modeling Engine Registry

`fct_fit_model.R` defines `ENGINE_REGISTRY` with four fields per engine:
- `$requires` – package dependencies
- `$model_types` – supported model types (Linear regression, Logistic regression, Quantile regression)
- `$build_formulas` – creates nested formulae for progressive models
- `$fit_one` – fits a single model with the backend's API
- `$make_spec` – creates parsnip model spec (NULL for fixest/RIF)
- `$prepare_outcome` – coerces outcome to required type

**Engines:**
| Engine | Description | Model Types |
|--------|-------------|-------------|
| `fixest` | High-dimensional fixed effects (feols/feglm) | Linear, Logistic |
| `ranger` | Random forest via parsnip + ranger | Linear |
| `xgboost` | XGBoost via parsnip | Linear, Logistic |
| `rif` | Unconditional quantile regression via RIF (Firpo et al. 2009) | Quantile |

### Data Backends

DuckDB is the unified query engine with extension-based storage support:

| Backend | Required DuckDB Extension | Auth Method |
|---------|---------------------------|-------------|
| S3 | httpfs | AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY |
| GCS | httpfs | GCS_KEY_ID / GCS_SECRET |
| Azure | azure + delta | Account key or service principal |
| Databricks | httpfs | OAuth2 M2M (DATABRICKS_HOST/CLIENT_ID/SECRET) |
| Local | none | File paths |

See `fct_connection.R` and `fct_load_data.R` for implementation details.

## Key Patterns

### Shiny Architecture
- **Shiny modules**: Every major UI section is a Golem-style module (`mod_NAME_ui()` / `mod_NAME_server()`). Sub-modules are nested inside parent modules.
- **Reactive chain**: Outputs of one module (model object, selection state) are passed as reactive inputs to downstream modules.

### Modeling Pipeline
- **Progressive models**: Three nested specifications (weather only → + FE → + FE + controls) fitted identically for comparison.
- **Log-transform handling**: When welfare outcome is log-transformed, predictions are automatically back-transformed (`exp()`) throughout the pipeline.
- **Coefficient uncertainty propagation**: Cholesky factor of VCV matrix (`compute_chol_vcov`) enables fast Monte Carlo draws via factor loading matrix (`compute_factor_loading`), ~200x speedup over full-dimension draws.

### Policy Analysis
- **Policy decomposition**: Total effect = Main effect + Resilience effect (Repositioning + Interaction) — see `fct_policy_decompose.R`.
- **RIF (Recentered Influence Function)**: Unconditional quantile regression for distributional impact estimation (Firpo, Fortin & Lemieux 2009).
- **SP cash transfer column**: `.wiseapp_sp_transfer` is the single source of truth for social protection transfers.

### Simulation Pipeline
- **Historical simulation**: Weather data joined to survey panel (one first-of-month date per (survey month × year) combination).
- **Future simulation**: SSP scenarios (SSP2-4.5, SSP3-7.0, SSP5-8.5) with additive/multiplicative perturbations per variable units.
- **Residual handling**: Options for display-time residual simulation (`original`, `normal`, `resample`, `none`).

### Config
- **Config via `config` package**: `inst/golem-config.yml` holds environment-specific settings; `app_config.R` reads them plus environment variables for deployment detection.

## Deployment

Target platform: **Posit Connect**. The app auto-detects Databricks credentials when running on Connect. See `dev/03_deploy.R` for deployment steps.

Key environment variables for production:
- `DATABRICKS_HOST`, `DATABRICKS_CLIENT_ID`, `DATABRICKS_CLIENT_SECRET`, `DATABRICKS_VOLUME_PATH` (for Databricks backend)
- `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` (for S3)
- `GCS_KEY_ID`, `GCS_SECRET` (for Google Cloud Storage)
- `AZURE_STORAGE_KEY` or `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`, `AZURE_TENANT_ID` (for Azure)

## Git Remotes

- `origin`: user fork (`bbrunckh/wise-app`)
- `upstream`: official World Bank repo (`worldbank/wise-app`)

## Testing

Tests are in `tests/testthat/` (11 files) plus `tests/spelling.R`. Key test files:

| Test File | Coverage |
|-----------|----------|
| `test-active-mask.R` | Coefficient uncertainty decomposition |
| `test-fct_aggregation_delta.R` | Survey-level aggregation with uncertainty |
| `test-fct_connection.R` | Connection parameter building |
| `test-fct_get_weather.R` | Weather data retrieval (S3/Databricks) |
| `test-fct_results.R` | LCU/PPP outcome conversion and logging |
| `test-fct_rif_sim.R` | RIF quantile regression helpers |
| `test-fct_weather_select.R` | Weather variable selection |
| `test-fct_weatherstats.R` | Weather statistics computation |
| `test-fct_hexmap.R` | Hex-map engine: payload contract, senders, container markup |
| `test-fct-outcome-weather-payloads.R` | Outcome-coverage and weather hex-map payload builders |
| `test-mod_1_05_weatherstats.R` | Per-variable/per-wave weather map rendering |
| `test-policy-decomposition-uncertainty.R` | Policy effect decomposition uncertainty |
| `test-uncertainty-decomposition.R` | Variance decomposition helpers |
