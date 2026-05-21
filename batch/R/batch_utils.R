# =============================================================================
# batch/R/batch_utils.R
#
# Shared utilities for WISE-APP batch scripts.
# Sourced at the top of 01_survey_stats.R, 02_weather_stats.R, and
# 03_run_mod1.R (and by 04_run_all.R before sourcing those scripts).
# =============================================================================


# -----------------------------------------------------------------------------
# save_gg(): safe ggplot2::ggsave wrapper
# -----------------------------------------------------------------------------

save_gg <- function(p, path, width = 10, height = 6, dpi = 150) {
  if (is.null(p)) return(invisible(NULL))
  tryCatch(
    ggplot2::ggsave(path, p, width = width, height = height, dpi = dpi),
    error = function(e)
      message("  ggsave failed (", basename(path), "): ", conditionMessage(e))
  )
}


# -----------------------------------------------------------------------------
# weather_agg_for(): resolve temporal aggregation for a weather variable
# Mirrors the local helper in 03_run_mod1.R; kept here so 02_weather_stats.R
# can use the same logic without duplication.
# -----------------------------------------------------------------------------

weather_agg_for <- function(var, weather_agg_override = NULL) {
  DEFAULT_AGG <- c(t = "Mean", r = "Sum")
  weather_agg_override[[var]] %||% DEFAULT_AGG[[var]] %||% "Mean"
}


# -----------------------------------------------------------------------------
# plot_survey_map_static(): ggplot/sf static location map from H3 GeoJSON
#
# Accepts the same GeoJSON FeatureCollection produced by the H3 DuckDB pipeline
# in mod_1_02_surveystats.R (lines 142-157) and renders it as a ggplot map.
# Requires the 'sf' package. Returns NULL if sf is not available.
# -----------------------------------------------------------------------------

plot_survey_map_static <- function(geojson) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("  sf not available — skipping static map")
    return(invisible(NULL))
  }
  if (is.null(geojson) || length(geojson$features) == 0) return(invisible(NULL))

  tryCatch({
    # Build a plain GeoJSON string for sf::st_read (drop geom_json helper field)
    clean_features <- lapply(geojson$features, function(f) {
      list(
        type       = f$type,
        geometry   = f$geometry,
        properties = f$properties
      )
    })
    geojson_str <- jsonlite::toJSON(
      list(type = "FeatureCollection", features = clean_features),
      auto_unbox = TRUE
    )

    sf_obj <- sf::st_read(geojson_str, quiet = TRUE)
    sf_obj$code <- vapply(geojson$features, function(f) f$properties$code, character(1))

    ggplot2::ggplot(sf_obj) +
      ggplot2::geom_sf(ggplot2::aes(fill = code), colour = NA, alpha = 0.7) +
      ggplot2::labs(fill = "Country") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
  }, error = function(e) {
    message("  static map failed: ", conditionMessage(e))
    invisible(NULL)
  })
}


# -----------------------------------------------------------------------------
# build_h3_geojson(): replicate mod_1_02_surveystats H3 → GeoJSON pipeline
#
# Loads H3 parquet files for the given survey rows (ss) via load_data(),
# runs the DuckDB spatial aggregation, and assembles a GeoJSON FeatureCollection
# identical to what the Shiny app builds (mod_1_02_surveystats.R lines 110-157).
# Returns the geojson list, or NULL on failure.
# -----------------------------------------------------------------------------

build_h3_geojson <- function(ss, connection_params) {
  h3_fnames <- ss |>
    dplyr::distinct(code, year, survname, source) |>
    dplyr::mutate(fname = paste0(
      "microdata/h3/", code, "/",
      code, "_", year, "_", survname, "_", source, "_h3.parquet"
    )) |>
    dplyr::pull(fname)

  h3_df <- tryCatch(
    load_data(h3_fnames, connection_params),
    error = function(e) { message("  H3 load failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(h3_df)) return(NULL)

  tryCatch({
    con <- dbplyr::remote_con(h3_df)
    .duck_load_ext("spatial")
    .duck_load_ext("h3")

    loc_df <- h3_df |>
      dplyr::summarise(
        geom = st_asgeojson(st_union_agg(st_geomfromtext(h3_cell_to_boundary_wkt(h3)))),
        .by  = c(code, year, survname, loc_id)
      ) |>
      dplyr::collect() |>
      dplyr::filter(!is.na(geom), nchar(geom) > 2)

    features <- lapply(seq_len(nrow(loc_df)), function(i) {
      row <- loc_df[i, ]
      list(
        type      = "Feature",
        geometry  = jsonlite::fromJSON(row$geom),
        geom_json = row$geom,
        properties = list(
          code     = row$code,
          year     = row$year,
          survname = row$survname,
          loc_id   = row$loc_id
        )
      )
    })

    list(type = "FeatureCollection", features = features)
  }, error = function(e) {
    message("  H3 GeoJSON build failed: ", conditionMessage(e))
    NULL
  })
}
