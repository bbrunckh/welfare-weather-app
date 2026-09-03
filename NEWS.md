# wiseapp 0.1.0

## Unreleased — 2026-09-03

### Changed

- Map engine migration (review §5.3 remediation): all Step-1 maps now render
  through a vendored MapLibre GL 5.24.0 + h3-js 4.1.0 browser engine
  (`inst/app/www/vendor/`, `inst/app/www/hexmap.js`, `R/fct_hexmap.R`). Maps
  send columnar payloads (H3 cell ids, values, colour-ramp stops) instead of
  serialized geometry; geometry is decoded client-side and colours are
  applied by MapLibre expressions. Camera persists across wave toggles;
  Leaflet builders remain as the runtime fallback when WebGL2 is
  unavailable. `map_data` (per-location aggregated geometry) removed
  end-to-end. No new R packages; `DESCRIPTION`/`renv.lock` unchanged.
- Fixed stale map-widget assertions in `test-mod_1_05_weatherstats.R`
  (mapgl-era `__fill` property → Leaflet `style.fillColor` call format).

### Added

- Hex-map engine tests: payload contract, density/coverage/weather payload
  builders (`test-fct_hexmap.R`, `test-fct-outcome-weather-payloads.R`).
- Payload benchmark harness (`dev/archive/bench_maps.R`) with recorded
  results at 1k/10k/50k cells.

