# Archived static ggplot renderers from batch/R/batch_utils.R.
#
# Retained for development/reference use only; this file is not sourced by the
# batch scripts or included in the package runtime. `build_h3_geojson()` remains
# active in batch/R/batch_utils.R because it produces data, not a static plot.

# -----------------------------------------------------------------------------
# .ne_cache: session-level cache for Natural Earth vector layers
# Populated on first call to plot_survey_map_static(); reused for all countries.
# -----------------------------------------------------------------------------

.ne_cache <- new.env(parent = emptyenv())

.ne_layers <- function() {
  if (!exists("ready", envir = .ne_cache)) {
    suppressMessages(suppressWarnings({
      .ne_cache$land   <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
      .ne_cache$rivers <- rnaturalearth::ne_download(scale = "large",
                            type = "rivers_lake_centerlines", category = "physical",
                            returnclass = "sf")
      .ne_cache$lakes  <- rnaturalearth::ne_download(scale = "large",
                            type = "lakes", category = "physical", returnclass = "sf")
    }))
    .ne_cache$ready <- TRUE
  }
  list(land = .ne_cache$land, rivers = .ne_cache$rivers, lakes = .ne_cache$lakes)
}


# -----------------------------------------------------------------------------
# plot_survey_map_static(): vector basemap + H3 survey locations
#
# Uses rnaturalearth for a crisp, label-free vector basemap (land, rivers,
# lakes). Survey H3 polygons are semi-transparent so overlapping locations
# from multiple survey waves accumulate opacity.
# Requires sf, rnaturalearth, rnaturalearthdata (loaded by aaa_load.R).
# -----------------------------------------------------------------------------

plot_survey_map_static <- function(geojson) {
  if (is.null(geojson) || length(geojson$features) == 0) return(invisible(NULL))

  tryCatch({
    # Parse H3 polygons (drop the geom_json helper field)
    clean_features <- lapply(geojson$features, function(f) {
      list(type = f$type, geometry = f$geometry, properties = f$properties)
    })
    geojson_str <- jsonlite::toJSON(
      list(type = "FeatureCollection", features = clean_features),
      auto_unbox = TRUE
    )
    sf_obj <- sf::st_read(geojson_str, quiet = TRUE)

    # Attach survey-wave metadata
    sf_obj$code     <- vapply(geojson$features, function(f) f$properties$code,             character(1))
    sf_obj$year     <- vapply(geojson$features, function(f) as.integer(f$properties$year), integer(1))
    sf_obj$survname <- vapply(geojson$features, function(f) f$properties$survname,          character(1))
    sf_obj$wave     <- paste0(sf_obj$year, " ", sf_obj$survname)

    n_waves   <- length(unique(sf_obj$wave))
    n_locs    <- nrow(sf_obj)
    n_overlap <- sum(duplicated(sf::st_geometry(sf_obj)) |
                       duplicated(sf::st_geometry(sf_obj), fromLast = TRUE))

    # Bounding box with ~20% buffer, clamped to valid WGS84 range
    bb      <- sf::st_bbox(sf_obj)
    x_buf   <- (bb["xmax"] - bb["xmin"]) * 0.2
    y_buf   <- (bb["ymax"] - bb["ymin"]) * 0.2
    xlim    <- c(max(-180, bb["xmin"] - x_buf), min(180, bb["xmax"] + x_buf))
    ylim    <- c(max(-90,  bb["ymin"] - y_buf), min(90,  bb["ymax"] + y_buf))
    crop_bb <- c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])

    ne        <- suppressMessages(suppressWarnings(.ne_layers()))
    land_c    <- suppressWarnings(sf::st_crop(ne$land,   crop_bb))
    rivers_c  <- suppressWarnings(sf::st_crop(ne$rivers, crop_bb))
    lakes_c   <- suppressWarnings(sf::st_crop(ne$lakes,  crop_bb))

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = land_c,   fill = "#f5f2ee", colour = "#c0b8ae", linewidth = 0.25) +
      ggplot2::geom_sf(data = rivers_c, colour = "#9ecae1", linewidth = 0.3) +
      ggplot2::geom_sf(data = lakes_c,  fill = "#d6e8f0", colour = "#9ecae1", linewidth = 0.2) +
      # Survey locations — low alpha so stacked polygons accumulate darkness
      ggplot2::geom_sf(
        data      = sf_obj,
        ggplot2::aes(fill = wave),
        colour    = "grey30",
        linewidth = 0.1,
        alpha     = max(0.15, min(0.6, 1 / max(1, n_waves)))
      ) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::scale_fill_brewer(palette = "Set2", name = "Survey wave") +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 0.85))) +
      ggplot2::labs(
        caption = paste0(
          "Each polygon is an H3-level survey cluster (loc_id). ",
          "Overlapping polygons from multiple survey waves appear darker.\n",
          n_locs, " location–wave polygons across ", n_waves, " survey wave(s).",
          if (n_overlap > 0) paste0(" ", n_overlap, " location(s) covered by ≥2 waves.") else ""
        )
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#d6e8f0", colour = NA),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.text      = ggplot2::element_text(size = 8),
        plot.caption     = ggplot2::element_text(size = 7, colour = "grey40",
                                                  margin = ggplot2::margin(t = 6))
      )
  }, error = function(e) {
    message("  static map failed: ", conditionMessage(e))
    invisible(NULL)
  })
}


# -----------------------------------------------------------------------------
# plot_weather_dist_with_ref(): ridge density plot overlaying survey-period
# weather (coloured by countryyear) with a climate reference distribution
# (grey, labelled separately).
#
# Falls back to plain plot_weather_dist() if df_ref is NULL. Its supporting
# ridge-data and geometry helpers are shared with active app renderers and remain
# outside this archive.
# -----------------------------------------------------------------------------

plot_weather_dist_with_ref <- function(df, df_ref = NULL, hv, label,
                                       cont_binned, ref_label = "Climate ref.") {
  if (is.null(df_ref) || !(hv %in% names(df_ref))) {
    return(plot_weather_dist(df, hv = hv, label = label, cont_binned = cont_binned))
  }

  x_label <- stringr::str_wrap(paste0(label, "\n(as configured)"), 40)

  if (!is.na(cont_binned) && cont_binned == "Binned") {
    return(plot_weather_dist(df, hv = hv, label = label, cont_binned = cont_binned))
  }

  # Survey-period rows: keep countryyear groups
  df_survey <- df[is.finite(df[[hv]]), c(hv, "countryyear"), drop = FALSE]
  df_survey$source <- df_survey$countryyear

  # Reference rows: collapse to a single group
  df_ref_plot <- df_ref[is.finite(df_ref[[hv]]), hv, drop = FALSE]
  df_ref_plot$countryyear <- ref_label
  df_ref_plot$source      <- ref_label

  # Combine; reference rows use a fixed grey fill
  df_all <- rbind(df_survey, df_ref_plot)

  # Factor so reference plots at the bottom of the ridgeplot
  survey_levels <- sort(unique(df_survey$source))
  df_all$source <- factor(df_all$source, levels = c(ref_label, survey_levels))

  n_survey <- length(survey_levels)
  survey_colours <- scales::hue_pal()(n_survey)
  names(survey_colours) <- survey_levels
  all_colours <- c(setNames("#AAAAAA", ref_label), survey_colours)

  rd <- build_ridge_distribution_data(
    df_all,
    x_var      = hv,
    group_var  = "source",
    fill_var   = "source",
    ridge_var  = "source",
    n_bins     = 256L,
    n_grid     = 256L
  )
  if (is.null(rd)) return(invisible(NULL))

  ggplot2::ggplot(
    rd$data,
    ggplot2::aes(x = .data$x, y = .data$y,
                 group = .data$group, fill = .data$fill)
  ) +
    ridge_geometry_layers(scale = 2, alpha = 0.7, linewidth = 0.3) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(rd$ridges), labels = rd$ridges,
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::scale_fill_manual(values = all_colours) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = x_label, y = "", fill = "") +
    ggplot2::theme(legend.position = "none")
}
