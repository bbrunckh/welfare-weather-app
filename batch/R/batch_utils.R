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

# weighted_weather_stats(): collapse-based weighted summary stats for weather
# variables in long format. Continuous variables only.
#
# Arguments:
#   df             — merged survey+weather data frame (one row per respondent)
#   vars           — character vector of weather variable names (continuous only)
#   selected_weather — selected_weather metadata table (from build_selected_weather)
#                      must contain: name, ref_start, ref_end, temporalAgg, transformation
#   weight         — name of weight column (default "weight")
#   n_rows_base    — total rows BEFORE the weather inner-join (for pct_missing)
#   df_ref         — climate reference period weather (output of get_weather()$historical
#                    loaded over CLIMATE_REF_YEARS). Unweighted. One row per
#                    loc_id x date. Produces one extra row per variable with
#                    year = "ref_YYYY-YYYY" and unweighted stats only.
#   ref_years      — length-2 integer vector, e.g. c(1991L, 2020L). Used to
#                    label the reference rows.
#
# Columns returned (per code-year-variable):
#   code, economy, survname, year, variable, ref_period, temporal_agg, transformation
#   mean, sd, min, max, n_unique, n, pct_missing, p10, p20, ..., p90   <- full sample
#   within_loc_mean, within_loc_sd                                      <- demeaned within loc_id
#   n_unique_per_loc                                                     <- mean unique vals per loc
#
# All stats are sample-weighted (except reference rows which are unweighted).
# pct_missing is out of n_rows_base (pre-join total); NA for reference rows.
# -----------------------------------------------------------------------------

weighted_weather_stats <- function(df, vars, selected_weather,
                                   weight = "weight", n_rows_base = NULL,
                                   df_ref = NULL, ref_years = c(1991L, 2020L)) {
  vars <- intersect(vars, names(df))
  vars <- vars[vapply(df[vars], is.numeric, logical(1L))]
  if (!length(vars)) return(data.frame())

  w   <- df[[weight]]
  grp <- collapse::GRP(df, by = c("code", "year"))

  # n for pct_missing: use supplied base count if available, else nrow(df)
  if (!is.null(n_rows_base)) {
    # n_rows_base is a named vector: names = paste(code, year), values = n
    base_n <- n_rows_base[paste(df$code[!duplicated(grp$group.id)],
                                df$year[!duplicated(grp$group.id)])]
  } else {
    base_n <- tabulate(grp$group.id, nbins = grp$N.groups)
  }

  # Metadata: one row per code-year
  meta <- collapse::fsubset(
    df[, c("code", "year", "economy", "survname")],
    !duplicated(grp$group.id)
  )

  # with_loc mask
  has_loc <- !is.na(df[["loc_id"]]) & !is.na(df[["area_h3_7"]])

  # Percentile probs
  pctile_probs <- seq(0.1, 0.9, by = 0.1)
  pctile_names <- paste0("p", as.integer(pctile_probs * 100))

  # Helper: full weighted stats (mean, sd, min, max, n_unique, n, pct_missing, p10-p90)
  wx_stats <- function(x, w_vec, grp_obj, tot, mask = NULL) {
    if (!is.null(mask)) {
      if (!any(mask)) {
        empty <- rep(NA_real_, grp_obj$N.groups)
        empti <- as.integer(empty)
        pct_empty <- setNames(as.list(rep(NA_real_, length(pctile_names))), pctile_names)
        return(c(list(mean = empty, sd = empty, min = empty, max = empty,
                      n_unique = empti, n = empti, pct_missing = empty),
                 pct_empty))
      }
      x       <- x[mask]
      w_vec   <- w_vec[mask]
      sub_grp <- collapse::GRP(df[mask, ], by = c("code", "year"))
      tot_sub <- tabulate(sub_grp$group.id, nbins = sub_grp$N.groups)
    } else {
      sub_grp <- grp_obj
      tot_sub <- tot
    }

    n_obs  <- collapse::fnobs(x, g = sub_grp)
    n_dist <- collapse::fndistinct(x, g = sub_grp, na.rm = TRUE)

    sub_key    <- sub_grp$groups
    master_key <- grp_obj$groups
    idx <- match(paste(sub_key$code, sub_key$year),
                 paste(master_key$code, master_key$year))

    expand <- function(v) {
      out <- rep(NA_real_, grp_obj$N.groups); out[idx] <- v; out
    }
    expand_int <- function(v) {
      out <- rep(NA_integer_, grp_obj$N.groups); out[idx] <- as.integer(v); out
    }

    # Weighted percentiles per group (fquantile does not support g=)
    pctiles <- lapply(pctile_probs, function(p) {
      expand(vapply(seq_len(sub_grp$N.groups), function(i) {
        idx_i <- sub_grp$group.id == i
        xi <- x[idx_i]; wi <- w_vec[idx_i]
        ok <- is.finite(xi) & is.finite(wi) & wi > 0
        if (!any(ok)) return(NA_real_)
        collapse::fquantile(xi[ok], probs = p, w = wi[ok])
      }, numeric(1L)))
    })
    names(pctiles) <- pctile_names

    c(list(
      mean        = expand(collapse::fmean(x, g = sub_grp, w = w_vec, na.rm = TRUE)),
      sd          = expand(collapse::fsd(x,   g = sub_grp, w = w_vec, na.rm = TRUE)),
      min         = expand(collapse::fmin(x,  g = sub_grp,             na.rm = TRUE)),
      max         = expand(collapse::fmax(x,  g = sub_grp,             na.rm = TRUE)),
      n_unique    = expand_int(n_dist),
      n           = expand_int(n_obs),
      pct_missing = expand(ifelse(tot_sub > 0, 1 - n_obs / tot_sub, NA_real_))
    ), pctiles)
  }

  # Helper: within-loc demeaned mean and sd only (with_loc subset)
  wx_within_stats <- function(x, w_vec, grp_obj, mask) {
    empty <- rep(NA_real_, grp_obj$N.groups)
    if (!any(mask)) return(list(within_loc_mean = empty, within_loc_sd = empty))

    x_sub   <- x[mask]
    w_sub   <- w_vec[mask]
    sub_grp <- collapse::GRP(df[mask, ], by = c("code", "year"))

    if ("loc_id" %in% names(df)) {
      loc_src   <- df[["loc_id"]][mask]
      loc_grp   <- collapse::GRP(data.frame(loc_id = loc_src), by = "loc_id")
      loc_means <- collapse::fmean(x_sub, g = loc_grp, w = w_sub, na.rm = TRUE)
      x_sub <- x_sub - loc_means[loc_grp$group.id]
    }

    sub_key    <- sub_grp$groups
    master_key <- grp_obj$groups
    idx <- match(paste(sub_key$code, sub_key$year),
                 paste(master_key$code, master_key$year))

    expand <- function(v) {
      out <- rep(NA_real_, grp_obj$N.groups); out[idx] <- v; out
    }

    list(
      within_loc_mean = expand(collapse::fmean(x_sub, g = sub_grp, w = w_sub, na.rm = TRUE)),
      within_loc_sd   = expand(collapse::fsd(x_sub,   g = sub_grp, w = w_sub, na.rm = TRUE))
    )
  }

  # Helper: mean number of unique values per loc_id (with_loc subset, per code-year)
  n_unique_per_loc_fn <- function(x, grp_obj, mask) {
    vapply(seq_len(grp_obj$N.groups), function(i) {
      mask_i <- grp_obj$group.id == i & mask
      if (!any(mask_i)) return(NA_real_)
      loc  <- df[["loc_id"]][mask_i]
      xv   <- x[mask_i]
      locs <- unique(loc[!is.na(loc)])
      if (!length(locs)) return(NA_real_)
      mean(vapply(locs, function(l) {
        vals <- xv[loc == l & !is.na(loc)]
        length(unique(vals[!is.na(vals)]))
      }, numeric(1L)), na.rm = TRUE)
    }, numeric(1L))
  }

  rows <- lapply(vars, function(v) {
    sw_row <- selected_weather[selected_weather$name == v, ][1L, ]
    ref_period    <- if (nrow(sw_row) > 0) paste0(sw_row$ref_start, "to", sw_row$ref_end, "m") else NA_character_
    temporal_agg  <- if (nrow(sw_row) > 0) sw_row$temporalAgg    else NA_character_
    transformation <- if (nrow(sw_row) > 0) sw_row$transformation else NA_character_

    x <- df[[v]]

    full    <- wx_stats(x, w, grp, base_n)
    within  <- wx_within_stats(x, w, grp, mask = has_loc)
    nupl    <- n_unique_per_loc_fn(x, grp, mask = has_loc)

    cbind(
      grp$groups,
      data.frame(variable = v, ref_period = ref_period,
                 temporal_agg = temporal_agg, transformation = transformation,
                 stringsAsFactors = FALSE),
      as.data.frame(full),
      as.data.frame(within),
      data.frame(n_unique_per_loc = nupl),
      row.names = NULL
    )
  })

  out <- do.call(rbind, rows)
  out <- merge(meta, out, by = c("code", "year"))
  out <- out[order(out$code, out$variable, out$ref_period, out$year), ]
  rownames(out) <- NULL


  # ---- Climate reference rows (one per code x variable) --------------------
  # df_ref has loc_id so within_loc stats are computed (unweighted).
  # year = NA; pct_missing = NA (no survey merge denominator).
  if (!is.null(df_ref) && nrow(df_ref) > 0) {
    ref_survname <- paste0("Climate reference ", ref_years[1], "-", ref_years[2])
    ref_grp      <- collapse::GRP(df_ref, by = "code")
    ref_has_loc  <- !is.na(df_ref[["loc_id"]])

    # economy lookup from the survey rows already assembled in out
    code_economy <- unique(out[, c("code", "economy"), drop = FALSE])

    ref_rows <- lapply(intersect(vars, names(df_ref)), function(v) {
      x      <- df_ref[[v]]
      n_obs  <- collapse::fnobs(x, g = ref_grp)
      n_dist <- collapse::fndistinct(x, g = ref_grp, na.rm = TRUE)

      pctiles <- lapply(pctile_probs, function(p) {
        vapply(seq_len(ref_grp$N.groups), function(i) {
          xi <- x[ref_grp$group.id == i]; xi <- xi[is.finite(xi)]
          if (!length(xi)) return(NA_real_)
          collapse::fquantile(xi, probs = p)
        }, numeric(1L))
      })
      names(pctiles) <- pctile_names

      # within_loc: demean within loc_id (unweighted)
      within_mean <- within_sd <- rep(NA_real_, ref_grp$N.groups)
      if (any(ref_has_loc)) {
        x_sub    <- x[ref_has_loc]
        sub_grp  <- collapse::GRP(df_ref[ref_has_loc, ], by = "code")
        loc_grp  <- collapse::GRP(data.frame(loc_id = df_ref[["loc_id"]][ref_has_loc]),
                                  by = "loc_id")
        loc_means <- collapse::fmean(x_sub, g = loc_grp, na.rm = TRUE)
        x_dem     <- x_sub - loc_means[loc_grp$group.id]
        idx_m     <- match(sub_grp$groups$code, ref_grp$groups$code)
        within_mean[idx_m] <- collapse::fmean(x_dem, g = sub_grp, na.rm = TRUE)
        within_sd[idx_m]   <- collapse::fsd(x_dem,   g = sub_grp, na.rm = TRUE)
      }

      # n_unique_per_loc (unweighted)
      nupl <- vapply(seq_len(ref_grp$N.groups), function(i) {
        mask_i <- ref_grp$group.id == i & ref_has_loc
        if (!any(mask_i)) return(NA_real_)
        loc  <- df_ref[["loc_id"]][mask_i]
        xv   <- x[mask_i]
        locs <- unique(loc[!is.na(loc)])
        if (!length(locs)) return(NA_real_)
        mean(vapply(locs, function(l) {
          vals <- xv[loc == l & !is.na(loc)]
          length(unique(vals[!is.na(vals)]))
        }, numeric(1L)), na.rm = TRUE)
      }, numeric(1L))

      sw_row         <- selected_weather[selected_weather$name == v, ][1L, ]
      ref_period_v   <- if (nrow(sw_row) > 0) paste0(sw_row$ref_start, "to", sw_row$ref_end, "m") else NA_character_
      temporal_agg_v <- if (nrow(sw_row) > 0) sw_row$temporalAgg    else NA_character_
      transform_v    <- if (nrow(sw_row) > 0) sw_row$transformation else NA_character_

      cbind(
        ref_grp$groups,
        data.frame(
          year           = NA_character_,
          survname       = ref_survname,
          variable       = v,
          ref_period     = ref_period_v,
          temporal_agg   = temporal_agg_v,
          transformation = transform_v,
          mean           = collapse::fmean(x, g = ref_grp, na.rm = TRUE),
          sd             = collapse::fsd(x,   g = ref_grp, na.rm = TRUE),
          min            = collapse::fmin(x,  g = ref_grp, na.rm = TRUE),
          max            = collapse::fmax(x,  g = ref_grp, na.rm = TRUE),
          n_unique       = as.integer(n_dist),
          n              = as.integer(n_obs),
          pct_missing    = NA_real_,
          stringsAsFactors = FALSE
        ),
        as.data.frame(pctiles),
        data.frame(within_loc_mean  = within_mean,
                   within_loc_sd    = within_sd,
                   n_unique_per_loc = nupl,
                   row.names = NULL),
        row.names = NULL
      )
    })

    ref_out <- do.call(rbind, ref_rows)
    ref_out <- merge(ref_out, code_economy, by = "code", all.x = TRUE)
    out <- rbind(out, ref_out[, names(out)])
  }
  # Final column order
  base_cols  <- c("code", "economy", "survname", "year",
                  "variable", "ref_period", "temporal_agg", "transformation")
  stat_sfx   <- c("mean", "sd", "min", "max", "n_unique", "n", "pct_missing", pctile_names)
  extra_cols <- c(stat_sfx, "within_loc_mean", "within_loc_sd", "n_unique_per_loc")
  out <- out[, c(base_cols, extra_cols)]

  # pct_missing as percentages (0-100)
  pm_cols <- grep("pct_missing", names(out), value = TRUE)
  out[pm_cols] <- lapply(out[pm_cols], function(x) x * 100)

  # Round all double columns to 3 decimal places
  num_cols <- names(out)[vapply(out, is.double, logical(1L))]
  out[num_cols] <- lapply(out[num_cols], round, digits = 3)

  out
}

# -----------------------------------------------------------------------------
# plot_weather_dist_with_ref(): ridge density plot overlaying survey-period
# weather (coloured by countryyear) with a climate reference distribution
# (grey, labelled separately).
#
# Falls back to plain plot_weather_dist() if df_ref is NULL.
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

  bw <- tryCatch(stats::bw.nrd0(df_all[[hv]]), error = function(e) NULL)
  if (is.null(bw) || !is.finite(bw) || bw <= 0) bw <- NULL

  n_survey <- length(survey_levels)
  survey_colours <- scales::hue_pal()(n_survey)
  names(survey_colours) <- survey_levels
  all_colours <- c(setNames("#AAAAAA", ref_label), survey_colours)

  ggplot2::ggplot(
    df_all,
    ggplot2::aes(x = .data[[hv]], y = .data$source, fill = .data$source)
  ) +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 2, bandwidth = bw) +
    ggplot2::scale_fill_manual(values = all_colours) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = x_label, y = "", fill = "") +
    ggplot2::theme(legend.position = "none")
}


# -----------------------------------------------------------------------------
# clean_names(): normalise data-frame column names (lowercase, underscores)
# -----------------------------------------------------------------------------

clean_names <- function(df) {
  nms <- tolower(names(df))
  nms <- gsub("[. ]+", "_", nms)
  nms <- gsub("_+$", "", nms)
  names(df) <- nms
  df
}


# -----------------------------------------------------------------------------
# tidy_clustered(): extract clustered coefficient table from a fixest fit
# Replicates the UI's .fixest_coeftable() fallback chain and returns a
# broom-compatible data frame (term, estimate, std.error, statistic, p.value).
# -----------------------------------------------------------------------------

tidy_clustered <- function(fit) {
  ct <- tryCatch(
    .fixest_coeftable(fit),  # ~loc_id_panel -> ~loc_id -> HC1 -> iid
    error = function(e) NULL
  )

  if (is.null(ct)) {
    return(broom::tidy(fit))
  }
  data.frame(
    term      = rownames(ct),
    estimate  = ct[["Estimate"]],
    std.error = ct[["Std. Error"]],
    statistic = ct[["t value"]],
    p.value   = ct[["Pr(>|t|)"]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# -----------------------------------------------------------------------------
# extract_one_fit(): extract coefficients + fit stats from one model object
# Returns list(coefs = <df>, fit_stats = <df>).
# -----------------------------------------------------------------------------

extract_one_fit <- function(fit, model_label, code, wx_label, wx_vars,
                            interaction_var, survey_df, engine,
                            fe_label = NA_character_, fe_vec = NULL,
                            cov_label = NA_character_,
                            cov_method = NA_character_,
                            lasso_selected_vars = NA_character_,
                            taus = NULL) {
  if (is.null(fit)) return(NULL)

  fe_str     <- if (!is.null(fe_vec)) paste(fe_vec, collapse = ",") else NA_character_
  inter_str  <- if (length(interaction_var) > 0) interaction_var else NA_character_
  wx_present <- sum(stats::complete.cases(survey_df[, wx_vars, drop = FALSE]))
  is_rif     <- identical(engine, "rif") && !is.null(taus)

  meta <- data.frame(
    code = code, weather = wx_label, engine = engine,
    fe_profile = fe_label, cov_profile = cov_label, cov_method = cov_method,
    interaction = inter_str, fixedeffects = fe_str, model = model_label,
    stringsAsFactors = FALSE
  )

  append_meta <- function(df) cbind(meta[rep(1L, nrow(df)), , drop = FALSE], df)

  if (is_rif) {
    coefs <- tryCatch({
      cf <- dplyr::bind_rows(lapply(seq_along(taus), function(i) {
        cf_i <- tryCatch(tidy_clustered(fit[[i]]), error = function(e) NULL)
        if (is.null(cf_i)) return(NULL)
        cf_i$tau      <- taus[i]
        cf_i$estimand <- sprintf("UQR p%d", round(taus[i] * 100))
        cf_i
      }))
      append_meta(cf)
    }, error = function(e) NULL)

    fit_stats <- tryCatch({
      fs <- dplyr::bind_rows(lapply(seq_along(taus), function(i) {
        m <- fit[[i]]
        data.frame(
          tau       = taus[i],
          estimand  = sprintf("UQR p%d", round(taus[i] * 100)),
          r2        = tryCatch(fixest::r2(m, "r2"),  error = function(e) NA),
          r2_adj    = NA_real_,
          r2_within = tryCatch(fixest::r2(m, "wr2"), error = function(e) NA),
          aic       = NA_real_,
          n         = tryCatch(stats::nobs(m),       error = function(e) NA),
          stringsAsFactors = FALSE
        )
      }))
      fs$lasso_selected <- lasso_selected_vars
      append_meta(fs)
    }, error = function(e) NULL)

  } else {
    coefs <- tryCatch({
      cf <- tidy_clustered(fit)
      cf$tau <- NA_real_; cf$estimand <- "Mean"
      append_meta(cf)
    }, error = function(e) NULL)

    fit_stats <- tryCatch({
      fs <- data.frame(
        tau        = NA_real_, estimand = "Mean",
        r2         = tryCatch(fixest::r2(fit, "r2"),  error = function(e) NA),
        r2_adj     = tryCatch(fixest::r2(fit, "ar2"), error = function(e) NA),
        r2_within  = tryCatch(fixest::r2(fit, "wr2"), error = function(e) NA),
        aic        = tryCatch(stats::AIC(fit),        error = function(e) NA),
        n          = tryCatch(stats::nobs(fit),       error = function(e) NA),
        lasso_selected = lasso_selected_vars,
        stringsAsFactors = FALSE
      )
      append_meta(fs)
    }, error = function(e) NULL)
  }

  list(coefs = coefs, fit_stats = fit_stats)
}
