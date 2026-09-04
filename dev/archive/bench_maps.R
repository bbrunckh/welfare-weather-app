# ============================================================================ #
# bench_maps.R - MapLibre/H3 cell-migration benchmarks (migration plan §4.1).  #
#                                                                              #
# Measures, per the review §5.3 targets:                                       #
#   - server-side payload BUILD time at 1k / 10k / 50k synthetic cells         #
#     (target: <= 300 ms per data layer)                                       #
#   - payload BYTES on the wire (the old FeatureCollection strings carried     #
#     full geometry; the payload carries ids and values only)                  #
#                                                                              #
# Browser TTI (<= 1.5 s) and pan FPS (>= 45) need the real app in a browser -  #
# documented devtools snippet for that manual pass:                            #
#   1. Performance panel, no throttling, record a reload of the Survey stats   #
#      tab after clicking Load: TTI = first hexagon paint after map load       #
#      (first frame painting the "hex-fill" layer).                            #
#   2. Same recording, 5 s of pan/zoom across the map: FPS from the frames     #
#      row.                                                                    #
# Run: Rscript dev/bench_maps.R  (archive the output with the review report.)  #
# ============================================================================ #

pkgload::load_all(quiet = TRUE, export_all = FALSE)

make_bench_cells <- function(n_cells) {
  # Synthetic per-cell frame shaped like density_cells(): distinct res-7
  # index strings (the payload builder does not decode them - validity only
  # matters in the browser, and production ids come from the store) and
  # skewed log-normal-ish counts like a real sample (a few dense urban
  # cells). Deterministic via the app's seeding convention.
  set.seed(n_cells)
  data.frame(
    h3 = sprintf("89754e64%06x", seq_len(n_cells)),
    geom = rep('{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,1],[0,0]]]}',
           n_cells),
    n_units = round(exp(seq(log(0.5), log(500), length.out = n_cells)) *
                      runif(n_cells, 0.5, 1.5)),
    xmin = -19.6 + runif(n_cells, 0, 0.5),
    ymin = 27 + runif(n_cells, 0, 0.4),
    xmax = -19.1 + runif(n_cells, 0, 0.5),
    ymax = 27.4 + runif(n_cells, 0, 0.4),
    stringsAsFactors = FALSE
  )
}

bench_one <- function(n_cells, label) {
  cells <- make_bench_cells(n_cells)

  # Sample density (mod_1_02): log-ramp continuous payload.
  t_density <- system.time(
    pl <- wiseapp:::.density_hex_payload(cells, "households")
  )
  json <- shiny:::toJSON(list(id = "bench", payload = pl$payload))

  # Outcome coverage (mod_1_03): cell-merge + 3-stop ramp payload.
  cov_df <- data.frame(
    code = "TST", year = "2021", survname = "SRV", loc_id = cells$h3,
    welfare = sample(c(0, 1, NA), n_cells, replace = TRUE,
                     prob = c(0.45, 0.45, 0.1))
  )
  cmap <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = cells$h3, h3 = cells$h3, pop_2020 = 1
  )
  t_cov <- system.time(
    pc <- wiseapp:::.coverage_hex_payload(cells, cmap, cov_df, "welfare")
  )
  json_cov <- shiny:::toJSON(list(id = "bench", payload = pc$payload))

  cat(sprintf(
    paste0("%-6s density build %6.1f ms | %9.0f bytes | ",
           "coverage build %6.1f ms | %9.0f bytes\n"),
    label,
    t_density[["elapsed"]] * 1000, as.numeric(object.size(json)),
    t_cov[["elapsed"]] * 1000, as.numeric(object.size(json_cov))
  ))
}

cat("Review §5.3 target: payload build <= 300 ms per data layer.\n")
cat(sprintf("%-6s %s\n", "cells", "build time / wire bytes"))
bench_one(1e3, "1k")
bench_one(1e4, "10k")
bench_one(5e4, "50k")

# Production sample: the timings above run the exact builders the app calls
# on a real load. A live production measurement (real ids + counts) needs a
# database connection; run this inside a session after clicking Load:
#   bench_one(nrow(cell_data()$geom), "prod")
