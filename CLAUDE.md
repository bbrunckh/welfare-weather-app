# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

**WISE-APP** (Weather Impact Simulation and Evaluation for Adaptation Policy and Planning) is an R Shiny web application built by the World Bank. It estimates relationships between weather and household welfare, simulates welfare outcomes under climate scenarios, and evaluates policy/adaptation strategies.

- R package name: `wiseapp`
- Framework: [Golem](https://thinkr-open.github.io/golem/) (production-ready Shiny scaffolding)
- R version: 4.5.3 (pinned via `renv.lock`)

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
├── mod_*.R                    # ~20 Shiny modules (each has a UI and server function)
└── fct_*.R                    # ~15 business logic files (no Shiny dependencies)
```

**`fct_` files are the core engine:**
- `fct_connection.R` – builds connection params for each storage backend
- `fct_load_data.R` – data ingestion and validation
- `fct_fit_model.R` – **engine registry**: pluggable modeling backends (fixest, ranger/random forest, glmnet/Lasso)
- `fct_predict_outcomes.R` – prediction pipeline for fitted models
- `fct_simulations.R` – orchestrates the simulation pipeline
- `fct_results.R` – output formatting, coefficient plots, tables

### Modeling Engine Registry

`fct_fit_model.R` uses a registry pattern — each modeling engine (fixest, ranger, glmnet) is registered and called through a common interface. Adding a new model type means adding an entry to this registry.

### Data Backends

The app supports multiple storage backends selectable at runtime:
- Local filesystem (dev/testing)
- AWS S3, Google Cloud Storage, Azure Blob Storage
- Hugging Face datasets
- Databricks (production on Posit Connect)

Data is stored as Parquet/Arrow files accessed via DuckDB.

## Key Patterns

- **Shiny modules**: Every major UI section is a Golem-style module (`mod_NAME_ui()` / `mod_NAME_server()`). Sub-modules are nested inside parent modules.
- **Reactive chain**: Outputs of one module (model object, selection state) are passed as reactive inputs to downstream modules.
- **Config via `config` package**: `inst/golem-config.yml` holds environment-specific settings; `app_config.R` reads them plus environment variables for deployment detection.
- **Log-transform handling**: When welfare outcome is log-transformed, predictions are automatically back-transformed throughout the pipeline.

## Deployment

Target platform: **Posit Connect**. The app auto-detects Databricks credentials when running on Connect. See `dev/03_deploy.R` for deployment steps.

Key environment variables for production:
- `DATABRICKS_HOST`, `DATABRICKS_TOKEN`, `DATABRICKS_CATALOG`, `DATABRICKS_SCHEMA` (for Databricks backend)
- Storage credentials for S3/GCS/Azure as needed

## Git Remotes

- `origin`: user fork (`bbrunckh/welfare-weather-app`)
- `upstream`: official World Bank repo (`worldbank/welfare-weather-app`)
