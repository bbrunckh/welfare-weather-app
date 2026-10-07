# AGENTS.md

This file provides guidance to coding agents (Kilo, Claude Code, Codex, etc.) when working with code in this repository.

**Performance/optimization work**: read and follow `review/optimization_guidelines.md` — it defines the app's optimization standards, app-specific constraints, and benchmarking requirements.

## What This Is

**WISE-APP** (Weather Impact Simulation and Evaluation for Adaptation Policy and Planning) is an R Shiny web application built by the World Bank. It estimates relationships between weather and household welfare, simulates welfare outcomes under climate scenarios, and evaluates policy/adaptation strategies.

- R package name: `wiseapp`
- Framework: [Golem](https://thinkr-open.github.io/golem/) (production-ready Shiny scaffolding)
- R version: 4.5.3 (configured by the local development environment)
- License: MIT

## Development Commands

```r
# Install dependencies from DESCRIPTION
install.packages(read.dcf("DESCRIPTION")[1, "Imports"] |>
  strsplit(",\\s*") |>
  unlist())

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
4. **Step 3 – Policy Scenarios** (`mod_3_scenario` + 9 sub-modules): Model social protection, infrastructure, digital, labor market, and education interventions (`mod_3_01`-`mod_3_05`), then run the policy simulation, results, diagnostics, and decomposition (`mod_3_06`-`mod_3_09`).

Reactive data flows forward through the pipeline: Step 0 outputs feed Step 1, which feeds Steps 2 and 3.

### Code Organization

```
R/
├── app_ui.R / app_server.R    # Top-level app wiring
├── app_config.R               # Environment detection (dev/Posit Connect/Databricks)
├── run_app.R                  # Entry point
├── mod_*.R                    # 24 Shiny modules (each has a UI and server function)
├── fct_*.R                    # 41 business logic files (no Shiny dependencies)
└── utils_*.R                  # shared math, UI, plot-theme, and Step 1 helpers
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
- `fct_policy_metric_decompose.R` / `fct_decomposition_summary.R` – metric-aware Step 3 decomposition (per-metric channels) and its summaries
- `fct_metric_registry.R` – metric metadata (labels, units, direction, change kind, supported engines and uncertainty sources)
- `fct_rif_sim.R` – Recentered Influence Function (RIF) quantile regression helpers
- `fct_weatherstats.R` – weather statistics computation
- `fct_hexmap.R` – **hex-map engine bridge**: vendored MapLibre GL + h3-js (`inst/app/vendor/` — outside the `bundle_resources()` scan tree, served once via the explicit dependency; browser side `hexmap.js`), columnar payload contract (`hexmap_payload()`, senders `hexmap_update`/`hexmap_clear`/`hexmap_fit`), container `hexmap_ui()`. Used by mod_1_02, mod_1_03, mod_1_05; no Leaflet fallback — MapLibre is the only map surface. Asset pins and behavior details live in-file.
- `fct_step2_compute.R` – pure, serial Step 2 compute boundary (reference numerics remain in `fct_run_simulation()`)
- `fct_export.R` / `fct_provenance.R` – export bundles (config, tables, figures, metadata) and the immutable run-provenance record
- `fct_step2_async.R` – process-wide mirai async coordinator for Step 2 (FIFO queue, worker snapshots, secrets scrubbing) and the shared daemon pool for Overview metadata loads (credentials policy: see `review/optimization_guidelines.md` §0)

### Modeling Engine Registry

`fct_fit_model.R` defines `ENGINE_REGISTRY` with six fields per engine:
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
| GCS | httpfs | GCS_ACCESS_KEY_ID / GCS_SECRET_ACCESS_KEY |
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

Target platform: **Posit Connect**. Automatic startup data-source selection is controlled by `WISEAPP_DATA_SOURCE`; see the environment variables below and the deployment scripts for deployment steps.

Runtime egress: the app serves its fonts locally (`inst/app/fonts/`), but the hex map loads its basemap style and tiles from CARTO (`tiles.basemaps.cartocdn.com`, see `inst/app/vendor/hexmap.js`). The server must allow HTTPS to that host for map views; everything else needs only the configured data source.

Bundled DuckDB extensions (`inst/duckdb_extensions/`) are built for the exact `duckdb` version pinned in `DESCRIPTION` (currently 1.5.6) and are checked against pinned SHA-256 values in `R/fct_load_data.R` before they are installed on Connect. To upgrade DuckDB, rebuild the binaries and update the pin, the version constant and the checksums together.

Optional resource limits (unset = package/DuckDB defaults). Every Connect process runs a main R process plus a mirai daemon, each with its own in-memory DuckDB, so cap them on shared hosts:
- `WISEAPP_DUCKDB_MEMORY_LIMIT` (for example `4GB`) and `WISEAPP_DUCKDB_THREADS` (per DuckDB instance); `WISEAPP_DUCKDB_TEMP_DIR` (spill directory, default a per-process temp dir)
- `WISEAPP_THREADS` (fixest and collapse threads)
- `WISEAPP_ASYNC_TIMEOUT_MIN` (Step 2 run limit, default 90) and `WISEAPP_ASYNC_METADATA_TIMEOUT_SEC` (default 300); `0` disables
- `WISEAPP_STAGE_LOG=0` turns off the one-line-per-run stage log

Key environment variables for production:
- `WISEAPP_DATA_SOURCE` (automatic source selector: `local`, `s3`, `gcs`, `azure`, `hf`, or `databricks`)
- `WISEAPP_DATA_PATH` (local data backend)
- `DATABRICKS_HOST`, `DATABRICKS_CLIENT_ID`, `DATABRICKS_CLIENT_SECRET`, `DATABRICKS_VOLUME_PATH` (for Databricks backend)
- `S3_BUCKET`, `S3_PREFIX`, `S3_REGION`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` (for S3)
- `GCS_BUCKET`, `GCS_PREFIX`, `GCS_ACCESS_KEY_ID`, `GCS_SECRET_ACCESS_KEY` (for GCS)
- `AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_CONTAINER`, `AZURE_STORAGE_PREFIX`, `AZURE_STORAGE_KEY` or `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`, `AZURE_TENANT_ID` (for Azure)
- `HF_REPO`, `HF_SUBDIR` (for public Hugging Face datasets)

## Git Remotes

- `origin`: official World Bank repo dev branch (`worldbank/wise-app:dev`)

## Testing

Tests are in `tests/testthat/` (80 files, named after the `fct_`/`mod_` file or concept they cover, e.g. `test-fct_hexmap.R`, `test-active-mask.R`) plus `tests/spelling.R`. Areas with dedicated coverage: connection/data loading, model fitting + coefficient uncertainty decomposition, aggregation delta, RIF helpers, hexmap payload contract, policy decomposition uncertainty, metric-aware decomposition, weather selection/stats, export bundles, determinism.
