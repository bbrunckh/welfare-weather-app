# wiseapp 0.1.0

## Unreleased — 2026-09-03

### Changed

- **Leaflet removed as a dependency.** All Step-1 maps render through the
  MapLibre/H3 engine only: the WebGL fallback machinery (`<id>_webgl`
  inputs, `renderLeaflet` surfaces, `plot_sample_density_map()`,
  `plot_outcome_coverage_map()`, `plot_weather_loc_map()` and their
  GeoJSON/view-memory helpers) is gone, and the palettes use small local
  ramp builders (`R/fct_surveystats.R` `.ramp_numeric()`/`.ramp_factor()`).
  `leaflet`/`leaflet.providers` dropped from `DESCRIPTION` and `renv.lock`.
- Map engine migration (review §5.3 remediation): all Step-1 maps now render
  through a vendored MapLibre GL 5.24.0 + h3-js 4.1.0 browser engine
  (`inst/app/www/vendor/`, `inst/app/www/hexmap.js`, `R/fct_hexmap.R`). Maps
  send columnar payloads (H3 cell ids, values, colour-ramp stops) instead of
  serialized geometry; geometry is decoded client-side and colours are
  applied by MapLibre expressions. Camera persists across wave toggles.
  `map_data` (per-location aggregated geometry) removed
  end-to-end. No new R packages; `renv.lock` unchanged.
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

