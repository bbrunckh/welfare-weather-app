# wiseapp 0.2.0

## Unreleased - 2026-10-06

Review remediation, batches 1 and 5 (`review/REVIEW-2026-10-06-tracking.md`).

### Fixed

- **Deployment.** `brand.yml`, `later` and `tidyselect` are now declared in
  `DESCRIPTION`; the Step 3 decomposition tables use reactable instead of the
  undeclared `DT` (the first Step 3 run no longer ends the session where `DT`
  is missing). Open Sans is served from `inst/app/fonts/` instead of being
  downloaded from Google Fonts on every page render. `duckdb` is pinned to the
  version the bundled extension binaries were built for (1.5.5), and the bundled
  binaries are version- and SHA-256-checked before they are installed.
- **Async worker.** A dead mirai daemon (for example killed for memory) is now
  detected and relaunched before the next run instead of hanging every later
  run in that process. Step 2 runs (90 min) and Overview metadata loads (5 min)
  have a timeout (`WISEAPP_ASYNC_TIMEOUT_MIN`,
  `WISEAPP_ASYNC_METADATA_TIMEOUT_SEC`) and report a readable error.

### Changed

- The parallel multiple-imputation LASSO path (`future`/`futuremice`) is removed;
  imputed LASSO runs sequentially. `future` and `future.apply` are no longer
  dependencies; `parsnip`, `ranger` and `xgboost` moved to Suggests.
- `Depends: R (>= 4.4.0)` (the code uses base `%||%`).
- New environment variables for shared hosts: `WISEAPP_DUCKDB_MEMORY_LIMIT`,
  `WISEAPP_DUCKDB_THREADS`, `WISEAPP_DUCKDB_TEMP_DIR`, `WISEAPP_THREADS`
  (fixest and collapse), `WISEAPP_STAGE_LOG` (see `AGENTS.md`).
- One log line per Step 2 and Step 3 run (stage, outcome, run id, elapsed,
  memory) is written to the process log.
- Step 2 benchmark harness: weather time no longer includes pipeline time,
  per-key rows carry their real key, memory profiling is opt-in
  (`WISEAPP_STEP2_MEMORY_PROFILE=1`), `WISEAPP_STEP2_DATA_SOURCE=databricks`
  reads remote data, and `dev/run_step2_benchmark.sh` runs on Linux.

## Unreleased — 2026-09-03

### Changed

- **Fixed: Weather-by-location maps could boot blank.** `hexmap.js` is served
  twice (golem's `bundle_resources()` scan plus the explicit htmlDependency),
  and each copy kept its own message queues/replay registry while both raced
  their MutationObservers. A re-rendered card could boot its replacement
  container from the copy holding no replay state, leaving the weather maps
  at the world view with no cells. The engine now carries a load-once guard
  (v1.0.5): the first copy owns the router, later copies are strict no-ops.
- **Leaflet removed as a dependency.** All Step-1 maps render through the
  MapLibre/H3 engine only: the WebGL fallback machinery (`<id>_webgl`
  inputs, `renderLeaflet` surfaces, `plot_sample_density_map()`,
  `plot_outcome_coverage_map()`, `plot_weather_loc_map()` and their
  GeoJSON/view-memory helpers) is gone, and the palettes use small local
  ramp builders (`R/fct_surveystats.R` `.ramp_numeric()`/`.ramp_factor()`).
  `leaflet`/`leaflet.providers` dropped from the production dependency metadata.
- Map engine migration (review §5.3 remediation): all Step-1 maps now render
  through a vendored MapLibre GL 5.24.0 + h3-js 4.1.0 browser engine
  (`inst/app/www/vendor/`, `inst/app/www/hexmap.js`, `R/fct_hexmap.R`). Maps
  send columnar payloads (H3 cell ids, values, colour-ramp stops) instead of
  serialized geometry; geometry is decoded client-side and colours are
  applied by MapLibre expressions. Camera persists across wave toggles.
  `map_data` (per-location aggregated geometry) removed
  end-to-end. No new R packages; deployment metadata unchanged.
- Sample-density allocation is now population-weighted:
  `allocate_units_to_cells()` spreads each location's sampled units across
  its H3 cells in proportion to `pop_2020` (even split when weights are
  missing or zero); per-cell totals still reconcile with the sample.
- Density colour scale is binned: cells averaging fewer than one household
  (locations whose units spread thin across many cells) share one pale
  "less than 1" bin, and the occupied range above is split at its
  quantiles — thin cells stay pale, rural variation stays visible, and
  city outliers compress into the dark end. A single global transform
  (log, sqrt) could not do both. The legend renders as discrete labelled
  bins, and the legend info popup opens downward so the card no longer
  clips it.
- Density map renamed "Location of interviews" (matching "Timing of
  interviews").
- Fixed stale map-widget assertions in `test-mod_1_05_weatherstats.R`
  (mapgl-era `__fill` property → Leaflet `style.fillColor` call format).

### Fixed

- Hex-map engine: the WebGL capability probe no longer fires a Shiny input
  event, so the card's renderUI never re-renders and replaces a booted map
  (blank map / basemap flash on wave toggles). The last payload per map is
  replayed onto replacement containers.
- Hex-map engine: first tile batch now renders reliably (render kicks after
  boot and after each payload) instead of stalling until a resize event.
- Hex-map engine: cells decode as `[lng, lat]` (`cellToBoundary(…,
  "geojson")`) — h3-js 4.x's default `[lat, lng]` mirrored every cell
  across the prime meridian/equator. Harness gained a coordinate-bounds
  regression check.
- Hex-map card layout: the map shell is a flex item of the card body, so
  the map and legend fit the card (no overflow below the card) and the
  legend clears the attribution control.

### Added

- Hex-map engine tests: payload contract, density/coverage/weather payload
  builders (`test-fct_hexmap.R`, `test-fct-outcome-weather-payloads.R`).
- Payload benchmark harness (`dev/archive/bench_maps.R`) with recorded
  results at 1k/10k/50k cells.
