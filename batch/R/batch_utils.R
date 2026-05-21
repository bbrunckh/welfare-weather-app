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
# Uses the same units-based logic as temporal_agg_default() in fct_weather_select.R:
# "Sum" for mm/days units, "Mean" for everything else.
# -----------------------------------------------------------------------------

weather_agg_for <- function(var, var_info, weather_agg_override = NULL) {
  units <- var_info$units[var_info$name == var]
  units <- if (length(units) == 0 || is.na(units[1])) "" else as.character(units[1])
  weather_agg_override[[var]] %||% temporal_agg_default(units)
}


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

# -----------------------------------------------------------------------------
# assign_var_group(): classify variable names into display groups matching the
# app's survey stats tab order:
#   outcome  — var_info$outcome == 1, plus poor300/poor420/poor830
#   policy   — vars in POLICY_DEFINITIONS, plus imp_wat_san_rec
#   hh       — var_info$hh  == 1, not already classified
#   ind      — var_info$ind == 1, not already classified
#   area     — var_info$area == 1, not already classified
#   firm     — var_info$firm == 1, not already classified
#   other    — anything remaining
# Returns a named character vector (names = variable names, values = group).
# -----------------------------------------------------------------------------

assign_var_group <- function(vars, var_info) {
  policy_vars <- unique(c(
    unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")),
    "imp_wat_san_rec"
  ))

  out_vars  <- var_info$name[!is.na(var_info$outcome)  & var_info$outcome  == 1]
  hh_vars   <- var_info$name[!is.na(var_info$hh)   & var_info$hh   == 1]
  ind_vars  <- var_info$name[!is.na(var_info$ind)  & var_info$ind  == 1]
  area_vars <- var_info$name[!is.na(var_info$area) & var_info$area == 1]
  firm_vars <- var_info$name[!is.na(var_info$firm) & var_info$firm == 1]

  group <- rep("other", length(vars))
  names(group) <- vars

  # Apply in reverse priority (earlier = higher priority, will overwrite later)
  group[vars %in% firm_vars]                           <- "firm"
  group[vars %in% area_vars]                           <- "area"
  group[vars %in% ind_vars]                            <- "ind"
  group[vars %in% hh_vars]                             <- "hh"
  group[vars %in% policy_vars]                         <- "policy"
  group[vars %in% c(out_vars, "poor300", "poor420", "poor830")] <- "outcome"

  group
}


# -----------------------------------------------------------------------------
# weighted_survey_stats(): collapse-based weighted summary stats in long format
#
# Columns returned (per code-year-variable):
#   code, economy, survname, year, variable,
#   mean, sd, min, max, n_unique, n, pct_missing          <- full sample
#   n_dates, min_date, max_date, avg_dates_per_loc         <- interview date stats
#   with_loc_mean  ... with_loc_pct_missing                <- loc_id & area_h3_7 not NA
#   without_loc_mean ... without_loc_pct_missing           <- loc_id or area_h3_7 is NA
#   within_loc_mean ... within_loc_pct_missing             <- with_loc, demeaned within loc_id
#
# poor300/poor420/poor830 are injected as binary 0/1 indicator variables
# (welfare < threshold) and appear as their own rows with the full stat set;
# mean = poverty headcount rate.
#
# All stats are sample-weighted (weight column).
# -----------------------------------------------------------------------------

weighted_survey_stats <- function(df, vars, weight = "weight", var_info = NULL) {
  vars <- intersect(vars, names(df))
  vars <- vars[vapply(df[vars], is.numeric, logical(1L))]
  if (!length(vars)) return(data.frame())

  # ---- Inject poverty binary indicators into df ----------------------------
  # poor300/420/830 = 1 if welfare < threshold; treated as regular variables
  # so they get the full stat set (mean = poverty rate) as their own rows.
  if ("welfare" %in% names(df)) {
    welf <- df[["welfare"]]
    poverty_lines <- c(poor300 = 3.00, poor420 = 4.20, poor830 = 8.30)
    for (nm in names(poverty_lines)) {
      df[[nm]] <- ifelse(is.na(welf), NA_real_, as.numeric(welf < poverty_lines[[nm]]))
    }
    vars <- c(vars, setdiff(names(poverty_lines), vars))
  }

  w   <- df[[weight]]
  grp <- collapse::GRP(df, by = c("code", "year"))

  # Total obs per group (for pct_missing denominator)
  total_n <- tabulate(grp$group.id, nbins = grp$N.groups)

  # Metadata: one row per code-year
  meta <- collapse::fsubset(
    df[, c("code", "year", "economy", "survname")],
    !duplicated(grp$group.id)
  )

  # with_loc / without_loc masks
  has_loc <- !is.na(df[["loc_id"]]) & !is.na(df[["area_h3_7"]])

  # ---- Interview date stats (per code-year, repeated for every variable) ----
  date_cols <- if ("timestamp" %in% names(df)) {
    ts <- df[["timestamp"]]
    as.data.frame(do.call(rbind, lapply(seq_len(grp$N.groups), function(i) {
      d <- ts[grp$group.id == i]
      d <- d[!is.na(d)]
      if (!length(d))
        return(data.frame(n_dates = NA_integer_, min_date = NA_character_, max_date = NA_character_,
                          stringsAsFactors = FALSE))
      data.frame(n_dates  = length(unique(d)),
                 min_date = as.character(min(d)),
                 max_date = as.character(max(d)),
                 stringsAsFactors = FALSE)
    })))
  } else {
    data.frame(n_dates = NA_integer_, min_date = NA_character_, max_date = NA_character_,
               stringsAsFactors = FALSE)[rep(1L, grp$N.groups), ]
  }

  # Group-level frame (code, year, poverty rates, date stats) — joined per variable row
  # ---- Average distinct interview dates per loc_id (with_loc subset) --------
  avg_dates_per_loc_col <- if ("timestamp" %in% names(df) && "loc_id" %in% names(df)) {
    vapply(seq_len(grp$N.groups), function(i) {
      mask_i <- grp$group.id == i & has_loc
      if (!any(mask_i)) return(NA_real_)
      loc  <- df[["loc_id"]][mask_i]
      ts   <- df[["timestamp"]][mask_i]
      locs <- unique(loc[!is.na(loc)])
      if (!length(locs)) return(NA_real_)
      n_dates_per_loc <- vapply(locs, function(l) {
        length(unique(ts[loc == l & !is.na(loc)]))
      }, integer(1L))
      mean(n_dates_per_loc, na.rm = TRUE)
    }, numeric(1L))
  } else {
    rep(NA_real_, grp$N.groups)
  }

  grp_level <- cbind(
    grp$groups, date_cols,
    data.frame(avg_dates_per_loc = avg_dates_per_loc_col, row.names = NULL)
  )

  # Helper: compute (mean, sd, min, max, n_unique, n, pct_missing) per group
  # for a subset defined by `mask` (NULL = full sample).
  # within_demean: if TRUE, demean x within loc_id before computing stats.
  ws_stats <- function(x, w_vec, grp_obj, mask = NULL, within_demean = FALSE) {
    if (!is.null(mask)) {
      if (!any(mask)) {
        empty <- rep(NA_real_, grp_obj$N.groups)
        return(list(mean = empty, sd = empty, min = empty, max = empty,
                    n_unique = as.integer(empty), n = as.integer(empty),
                    pct_missing = empty))
      }
      x       <- x[mask]
      w_vec   <- w_vec[mask]
      sub_grp <- collapse::GRP(df[mask, ], by = c("code", "year"))
      tot     <- tabulate(sub_grp$group.id, nbins = sub_grp$N.groups)
    } else {
      sub_grp <- grp_obj
      tot     <- total_n
    }

    if (within_demean && "loc_id" %in% names(df)) {
      loc_src   <- if (!is.null(mask)) df[["loc_id"]][mask] else df[["loc_id"]]
      loc_grp   <- collapse::GRP(data.frame(loc_id = loc_src), by = "loc_id")
      loc_means <- collapse::fmean(x, g = loc_grp, w = w_vec, na.rm = TRUE)
      x <- x - loc_means[loc_grp$group.id]
    }

    n_obs  <- collapse::fnobs(x, g = sub_grp)          # fnobs does not accept na.rm
    n_dist <- collapse::fndistinct(x, g = sub_grp, na.rm = TRUE)

    # Map sub_grp groups back to the master code-year group index
    sub_key    <- sub_grp$groups
    master_key <- grp_obj$groups
    idx <- match(
      paste(sub_key$code, sub_key$year),
      paste(master_key$code, master_key$year)
    )

    expand <- function(v) {
      out <- rep(NA_real_, grp_obj$N.groups)
      out[idx] <- v
      out
    }
    expand_int <- function(v) {
      out <- rep(NA_integer_, grp_obj$N.groups)
      out[idx] <- as.integer(v)
      out
    }

    list(
      mean        = expand(collapse::fmean(x, g = sub_grp, w = w_vec, na.rm = TRUE)),
      sd          = expand(collapse::fsd(x,   g = sub_grp, w = w_vec, na.rm = TRUE)),
      min         = expand(collapse::fmin(x,  g = sub_grp,             na.rm = TRUE)),
      max         = expand(collapse::fmax(x,  g = sub_grp,             na.rm = TRUE)),
      n_unique    = expand_int(n_dist),
      n           = expand_int(n_obs),
      pct_missing = expand(ifelse(tot > 0, 1 - n_obs / tot, NA_real_))
    )
  }

  rows <- lapply(vars, function(v) {
    x <- df[[v]]

    full      <- ws_stats(x, w, grp)
    with_l    <- ws_stats(x, w, grp, mask = has_loc)
    without_l <- ws_stats(x, w, grp, mask = !has_loc)
    within_l  <- ws_stats(x, w, grp, mask = has_loc, within_demean = TRUE)

    pfx <- function(lst, prefix) setNames(lst, paste0(prefix, names(lst)))

    cbind(
      grp_level,
      data.frame(variable = v, stringsAsFactors = FALSE),
      as.data.frame(full),
      as.data.frame(pfx(with_l,    "with_loc_")),
      as.data.frame(pfx(without_l, "without_loc_")),
      as.data.frame(pfx(within_l,  "within_loc_")),
      row.names = NULL
    )
  })

  out <- do.call(rbind, rows)
  out <- merge(meta, out, by = c("code", "year"))

  # ---- var_group -----------------------------------------------------------
  if (!is.null(var_info)) {
    vg <- assign_var_group(out$variable, var_info)
    out$var_group <- vg[out$variable]
  } else {
    out$var_group <- NA_character_
  }

  out <- out[order(out$code, out$variable, out$year), ]
  rownames(out) <- NULL

  # Final column order
  base_cols  <- c("code", "economy", "survname", "year", "var_group", "variable")
  stat_sfx   <- c("mean", "sd", "min", "max", "n_unique", "n", "pct_missing")
  extra_cols <- c(stat_sfx,
                  c("n_dates", "min_date", "max_date", "avg_dates_per_loc"),
                  paste0("with_loc_",    stat_sfx),
                  paste0("without_loc_", stat_sfx),
                  paste0("within_loc_",  stat_sfx))
  out <- out[, c(base_cols, extra_cols)]

  # pct_missing as percentages (0-100)
  pm_cols <- grep("pct_missing", names(out), value = TRUE)
  out[pm_cols] <- lapply(out[pm_cols], function(x) x * 100)

  # Round all numeric columns to 3 decimal places (preserves integer cols)
  num_cols <- names(out)[vapply(out, is.double, logical(1L))]
  out[num_cols] <- lapply(out[num_cols], round, digits = 3)

  out
}