# Simulation diagnostics ----
# fct_sim_diag.R
#                                                                              #
# Pure diagnostic functions for Module 2 Diagnostics tab.                     #
# Called by mod_2_05_sim_diag_server() only.                                  #
# No Shiny, no reactives -- fully testable.                                   #
#                                                                              #
# Functions:                                                                   #
#   .add_int_month()             -- derive int_month from timestamp            #
#   .filter_hist_weather()       -- filter weather_raw to survey cells         #
#   .plot_density_one()          -- single-variable density (internal)         #
#   plot_weather_density_panel() -- side-by-side multi-var density panel       #
#   .kde_group()                 -- compute KDE for one group (internal)       #
#   build_ridge_kde_data()       -- pre-compute all KDE data (exported)        #
#   plot_year_anchored_ridge()   -- render ridge plot from kde_data            #


# Internal helpers ----

# Derive int_month from timestamp if not already present.
#' @noRd
.add_int_month <- function(df) {
  if ("int_month" %in% names(df)) {
    return(df)
  }
  if ("timestamp" %in% names(df)) {
    df$int_month <- as.integer(format(as.Date(df$timestamp), "%m"))
  }
  df
}


# Filter weather_raw to loc_id x int_month cells present in survey_weather.
# Derives cal_year from timestamp (never from the survey-round `year` join key).
# Deduplicates to one row per loc_id x int_month x cal_year.
#' @noRd
.filter_hist_weather <- function(weather_raw, survey_weather) {
  wr <- .add_int_month(weather_raw)
  wr$cal_year <- as.integer(format(as.Date(wr$timestamp), "%Y"))
  if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather)) {
    survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
  }
  sw_cells <- unique(survey_weather[, c("loc_id", "int_month")])
  wr <- merge(wr, sw_cells, by = c("loc_id", "int_month"))
  wr[!duplicated(wr[, c("loc_id", "int_month", "cal_year")]), ]
}


# Weather input density panel ----

# Single-variable density plot (internal).
#' @noRd
# Single-variable density plot (internal).
#' @param survey_weather Data frame with loc_id, int_month, timestamp.
#' @param weather_raw    Data frame. hist_sim()$weather_raw.
#' @param weather_var    Character scalar. Column name in weather_raw.
#' @param weather_label  Character scalar. Display label (title/x-axis).
#'   Defaults to weather_var when NULL.
#' @param scenario_weather Named list of perturbed weather_raw data frames.
#' @param active_scenarios Character vector. Subset of scenario names to show.
#' @param log_x          Logical. Log10 x-axis. Default FALSE.
#' @param show_legend    Logical. Show legend. Default TRUE.
#' @param show_regression Logical. Overlay regression input curve. Default FALSE.
#' @param hist_filt Optional pre-computed \code{.filter_hist_weather(weather_raw,
#'   survey_weather)}. Depends only on \code{weather_raw}/\code{survey_weather},
#'   not on \code{weather_var}, so callers that plot several variables against
#'   the same pair (e.g. \code{plot_weather_density_panel()}) can build this
#'   once and pass it in to avoid re-filtering per variable (see PERF-24).
#'   When \code{NULL} (default), it is built from \code{weather_raw}/
#'   \code{survey_weather} as before.
#' @param reg_filt Optional pre-computed regression-input subset of
#'   \code{hist_filt} (rows whose \code{cal_year} is present in
#'   \code{survey_weather}). Same rationale as \code{hist_filt}; ignored
#'   unless \code{hist_filt} is also supplied.
#' @noRd
.plot_density_one <- function(survey_weather,
                              weather_raw,
                              weather_var,
                              weather_label = NULL,
                              scenario_weather = NULL,
                              active_scenarios = NULL,
                              log_x = FALSE,
                              show_legend = TRUE,
                              show_regression = FALSE,
                              hist_filt = NULL,
                              reg_filt = NULL) {
  disp_label <- weather_label %||% weather_var

  if (!weather_var %in% names(weather_raw)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("'", disp_label, "' not found.")))
  }

  if (is.null(hist_filt)) {
    hist_filt <- .filter_hist_weather(weather_raw, survey_weather)

    if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather)) {
      survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
    }
    if ("timestamp" %in% names(survey_weather)) {
      survey_weather$cal_year <- as.integer(format(as.Date(survey_weather$timestamp), "%Y"))
    }
    sw_years <- unique(survey_weather$cal_year)
    reg_filt <- hist_filt[hist_filt$cal_year %in% sw_years, ]
  }

  # Detect variable type ----
  raw_col <- weather_raw[[weather_var]]
  is_factor <- is.factor(raw_col) || is.character(raw_col) ||
    (is.integer(raw_col) && length(unique(raw_col[is.finite(raw_col)])) <= 20)

  # ======================================================================
  # FACTOR / CATEGORICAL PATH
  # ======================================================================
  if (is_factor) {
    # Preserve original factor levels for correct bin ordering on x-axis
    raw_levels <- if (is.factor(raw_col)) levels(raw_col) else NULL

    .to_ordered_factor <- function(x) {
      x <- as.character(x)
      if (!is.null(raw_levels)) {
        factor(x, levels = raw_levels)
      } else {
        # No pre-existing levels: sort as-is but put special sentinels first
        lvls <- sort(unique(x[!is.na(x)]))
        factor(x, levels = lvls)
      }
    }

    hist_vals <- .to_ordered_factor(hist_filt[[weather_var]])
    hist_vals <- hist_vals[!is.na(hist_vals)]
    if (length(hist_vals) == 0) {
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "No values to plot."))
    }

    all_df <- data.frame(
      value = hist_vals,
      source = "Full historical",
      stringsAsFactors = FALSE
    )

    if (isTRUE(show_regression)) {
      reg_vals_fct <- .to_ordered_factor(reg_filt[[weather_var]])
      reg_vals_fct <- reg_vals_fct[!is.na(reg_vals_fct)]
      if (length(reg_vals_fct) > 0) {
        all_df <- rbind(all_df, data.frame(
          value = reg_vals_fct,
          source = "Model support",
          stringsAsFactors = FALSE
        ))
      }
    }

    if (!is.null(scenario_weather) && length(scenario_weather) > 0) {
      visible_nms <- names(scenario_weather)
      if (!is.null(active_scenarios)) {
        visible_nms <- intersect(visible_nms, active_scenarios)
      }
      for (scen_nm in visible_nms) {
        sw_df <- scenario_weather[[scen_nm]]
        if (is.null(sw_df) || !weather_var %in% names(sw_df)) next
        vals <- .to_ordered_factor(sw_df[[weather_var]])
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) next
        all_df <- rbind(all_df, data.frame(
          value = vals,
          source = scen_nm,
          stringsAsFactors = FALSE
        ))
      }
    }

    # Re-apply factor with correct level order after rbind (which drops factor)
    all_df$value <- factor(all_df$value,
      levels = if (!is.null(raw_levels)) {
        raw_levels
      } else {
        sort(unique(as.character(all_df$value)))
      }
    )

    all_df$source <- factor(all_df$source,
      levels = c(
        "Full historical", "Model support",
        setdiff(
          unique(all_df$source),
          c("Full historical", "Model support")
        )
      )
    )

    sources <- levels(all_df$source)
    colour_map <- vapply(sources, function(s) {
      if (s == "Full historical") {
        return(.wise_history)
      }
      if (s == "Model support") {
        return(.wise_support)
      }
      ssp_key <- .normalise_ssp(s)
      if (!is.na(ssp_key) && ssp_key %in% names(.ssp_colours)) {
        .ssp_colours[ssp_key]
      } else {
        "#cccccc"
      }
    }, character(1))
    fill_map <- colour_map
    fill_map["Model support"] <- "#ffffff" # white fill with a clear outline

    n_scen_shown <- length(unique(all_df$source)) -
      sum(c("Full historical", "Model support") %in% all_df$source)

    p <- ggplot2::ggplot(
      all_df,
      ggplot2::aes(
        x      = .data$value,
        y      = ggplot2::after_stat(prop),
        group  = .data$source,
        fill   = .data$source,
        colour = .data$source
      )
    ) +
      ggplot2::geom_bar(
        position  = ggplot2::position_dodge(preserve = "single"),
        alpha     = 0.6,
        linewidth = 0.4
      ) +
      ggplot2::scale_fill_manual(
        values   = fill_map,
        na.value = NA,
        name     = NULL,
        breaks   = sources
      ) +
      ggplot2::scale_colour_manual(
        values = colour_map,
        name   = NULL,
        breaks = sources,
        guide  = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = NULL,
        subtitle = NULL,
        x = disp_label,
        y = "Relative frequency"
      ) +
      theme_wise() +
      ggplot2::theme(
        legend.position = if (show_legend) "bottom" else "none",
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1)
      )

    return(p)
  }

  # ======================================================================
  # CONTINUOUS PATH  (unchanged from original)
  # ======================================================================
  hist_vals <- as.numeric(hist_filt[[weather_var]])
  hist_vals <- hist_vals[is.finite(hist_vals)]
  reg_vals <- if (isTRUE(show_regression)) {
    v <- as.numeric(reg_filt[[weather_var]])
    v[is.finite(v)]
  } else {
    numeric(0)
  }

  if (length(hist_vals) == 0) {
    return(blank_plot("No finite values to plot."))
  }

  # SSP scenario overlays ----
  ssp_colour_map <- character(0)
  ssp_linetype_map <- character(0)
  ssp_df_list <- list()

  if (!is.null(scenario_weather) && length(scenario_weather) > 0) {
    visible_nms <- names(scenario_weather)
    if (!is.null(active_scenarios)) {
      visible_nms <- intersect(visible_nms, active_scenarios)
    }

    yrs_visible <- vapply(
      visible_nms,
      function(nm) as.integer(sub("-.*", "", .parse_year(nm))),
      integer(1)
    )
    unique_yrs <- sort(unique(yrs_visible[!is.na(yrs_visible)]))
    yr_lty_palette <- c("solid", "dashed", "dotted", "dotdash", "longdash")
    yr_lty_map <- setNames(
      yr_lty_palette[seq_len(min(length(unique_yrs), length(yr_lty_palette)))],
      as.character(unique_yrs)
    )

    for (scen_nm in visible_nms) {
      sw_df <- scenario_weather[[scen_nm]]
      if (is.null(sw_df) || !weather_var %in% names(sw_df)) next
      vals <- as.numeric(sw_df[[weather_var]])
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) next
      ssp_key <- .normalise_ssp(scen_nm)
      col <- if (!is.na(ssp_key) && ssp_key %in% names(.ssp_colours)) {
        .ssp_colours[ssp_key]
      } else {
        "#cccccc"
      }
      yr_chr <- as.character(as.integer(sub("-.*", "", .parse_year(scen_nm))))
      lty <- if (!is.na(yr_chr) && yr_chr %in% names(yr_lty_map)) {
        yr_lty_map[yr_chr]
      } else {
        "solid"
      }
      ssp_colour_map[scen_nm] <- col
      ssp_linetype_map[scen_nm] <- lty
      ssp_df_list[[scen_nm]] <- data.frame(
        value = vals,
        source = scen_nm,
        stringsAsFactors = FALSE
      )
    }
  }

  colour_map <- ssp_colour_map
  linetype_map <- ssp_linetype_map
  if (isTRUE(show_regression)) {
    colour_map["Model support"] <- .wise_support
    linetype_map["Model support"] <- "dashed"
  }

  p <- ggplot2::ggplot()

  p <- p + ggplot2::geom_density(
    data      = data.frame(value = hist_vals, stringsAsFactors = FALSE),
    mapping   = ggplot2::aes(x = .data$value, fill = "Full historical"),
    colour    = NA,
    alpha     = 0.35,
    linewidth = 0.7
  )

  if (isTRUE(show_regression) && length(reg_vals) > 0) {
    p <- p + ggplot2::geom_density(
      data = data.frame(
        value = reg_vals, source = "Model support",
        stringsAsFactors = FALSE
      ),
      mapping = ggplot2::aes(x = .data$value, colour = .data$source),
      fill = NA,
      linetype = "dashed",
      linewidth = 0.9
    )
  }

  for (scen_nm in names(ssp_df_list)) {
    p <- p + ggplot2::geom_density(
      data = ssp_df_list[[scen_nm]],
      ggplot2::aes(x = .data$value, colour = .data$source),
      fill = NA,
      linetype = ssp_linetype_map[[scen_nm]],
      linewidth = 0.8
    )
  }

  fill_map_all <- c("Full historical" = .wise_history)

  p <- p +
    ggplot2::scale_fill_manual(
      values   = fill_map_all,
      na.value = NA,
      name     = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = colour_map,
      name   = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_map,
      name   = NULL,
      guide  = "none"
    )

  n_scen_shown <- length(ssp_df_list)
  p <- p +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = disp_label,
      y = "Density"
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = if (show_legend) "bottom" else "none"
    )

  if (isTRUE(log_x)) p <- p + ggplot2::scale_x_log10()
  p
}

#' Side-by-Side Weather Input Density Panel
#'
#' Renders one density plot per weather variable selected, arranged in a
#' single row using patchwork. The Full historical distribution is drawn as
#' a grey filled area; regression input (when show_regression = TRUE) as a
#' black dashed overlay; scenario perturbations as coloured lines.
#'
#' @param survey_weather   Data frame. Must contain loc_id, int_month, timestamp.
#' @param weather_raw      Data frame. hist_sim()$weather_raw.
#' @param weather_vars     Character vector. Column names to plot.
#' @param weather_labels   Named character vector (name -> label). Display
#'   labels for titles and x-axes. NULL falls back to column names.
#' @param scenario_weather Named list of perturbed weather_raw data frames.
#' @param active_scenarios Character vector. Subset of names(scenario_weather).
#' @param log_x            Logical or logical vector (one per variable).
#'   Log10 x-axis. Default FALSE.
#' @param show_regression  Logical. Overlay regression input curve. Default FALSE.
#'
#' @return A patchwork / ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_density scale_colour_manual
#'   scale_linetype_manual scale_x_log10 labs theme_minimal theme element_text
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom rlang .data
#' @export
plot_weather_density_panel <- function(survey_weather,
                                       weather_raw,
                                       weather_vars,
                                       weather_labels = NULL,
                                       scenario_weather = NULL,
                                       active_scenarios = NULL,
                                       log_x = FALSE,
                                       show_regression = FALSE) {
  stopifnot(
    is.data.frame(survey_weather),
    is.data.frame(weather_raw),
    is.character(weather_vars), length(weather_vars) >= 1
  )

  weather_vars <- intersect(weather_vars, names(weather_raw))
  if (length(weather_vars) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "No selected weather variables found in weather_raw."))
  }

  # Vectorise log_x to one value per variable
  if (length(log_x) == 1L) log_x <- rep(log_x, length(weather_vars))

  # Both filters depend only on weather_raw/survey_weather, not on the
  # individual weather_var, so build them once here and pass down instead of
  # recomputing per variable inside .plot_density_one() (see PERF-24).
  hist_filt <- .filter_hist_weather(weather_raw, survey_weather)
  sw_for_years <- survey_weather
  if (!"int_month" %in% names(sw_for_years) && "timestamp" %in% names(sw_for_years)) {
    sw_for_years$int_month <- as.integer(format(as.Date(sw_for_years$timestamp), "%m"))
  }
  if ("timestamp" %in% names(sw_for_years)) {
    sw_for_years$cal_year <- as.integer(format(as.Date(sw_for_years$timestamp), "%Y"))
  }
  sw_years <- unique(sw_for_years$cal_year)
  reg_filt <- hist_filt[hist_filt$cal_year %in% sw_years, ]

  panels <- lapply(seq_along(weather_vars), function(i) {
    wv <- weather_vars[[i]]
    lbl <- if (!is.null(weather_labels) && wv %in% names(weather_labels)) {
      weather_labels[[wv]]
    } else {
      wv
    }
    .plot_density_one(
      survey_weather   = survey_weather,
      weather_raw      = weather_raw,
      weather_var      = wv,
      weather_label    = lbl,
      scenario_weather = scenario_weather,
      active_scenarios = active_scenarios,
      log_x            = isTRUE(log_x[[i]]),
      show_legend      = TRUE,
      show_regression  = isTRUE(show_regression),
      hist_filt        = hist_filt,
      reg_filt         = reg_filt
    )
  })

  if (length(panels) == 1L) {
    return(panels[[1L]])
  }

  patchwork::wrap_plots(panels, nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

#' Tidy data behind the weather-support density panels.
#' @noRd
weather_density_data <- function(survey_weather, weather_raw, weather_vars,
                                 scenario_weather = NULL,
                                 active_scenarios = NULL,
                                 show_regression = TRUE) {
  weather_vars <- intersect(weather_vars, names(weather_raw))
  if (!length(weather_vars)) {
    return(data.frame())
  }
  hist_filt <- .filter_hist_weather(weather_raw, survey_weather)
  if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather)) {
    survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
  }
  if ("timestamp" %in% names(survey_weather)) {
    survey_weather$cal_year <- as.integer(format(as.Date(survey_weather$timestamp), "%Y"))
  }
  reg_filt <- hist_filt[hist_filt$cal_year %in% unique(survey_weather$cal_year), , drop = FALSE]
  rows <- list()
  add <- function(df, source) {
    if (is.null(df)) {
      return()
    }
    for (wv in weather_vars) {
      x <- suppressWarnings(as.numeric(df[[wv]]))
      rows[[length(rows) + 1L]] <<- data.frame(
        weather_variable = wv, source = source, value = x,
        stringsAsFactors = FALSE
      )
    }
  }
  add(reg_filt, "Model support")
  add(hist_filt, "Full historical archive")
  visible <- names(scenario_weather %||% list())
  if (!is.null(active_scenarios)) visible <- intersect(visible, active_scenarios)
  for (nm in visible) add(scenario_weather[[nm]], nm)
  dplyr::bind_rows(rows) |>
    dplyr::filter(is.finite(.data$value))
}

weather_support_summary <- function(regression_weather, scenario_weather,
                                    weather_vars, lower = 0.01, upper = 0.99,
                                    weather_specs = NULL,
                                    warn_share = 0.05) {
  if (is.null(regression_weather) || !is.data.frame(regression_weather)) {
    return(data.frame())
  }
  scenarios <- scenario_weather %||% list()
  rows <- lapply(intersect(weather_vars, names(regression_weather)), function(v) {
    spec <- if (!is.null(weather_specs) && "name" %in% names(weather_specs)) {
      weather_specs[weather_specs$name == v, , drop = FALSE]
    } else {
      NULL
    }
    is_binned <- !is.null(spec) && nrow(spec) &&
      identical(as.character(spec$cont_binned[1]), "Binned")
    if (is_binned) {
      ref <- as.character(regression_weather[[v]])
      ref <- ref[!is.na(ref) & nzchar(ref)]
      supported <- unique(ref)
      if (!length(supported)) {
        return(NULL)
      }
      reference_label <- paste(supported, collapse = ", ")
      robust <- c(NA_real_, NA_real_)
    } else {
      ref <- suppressWarnings(as.numeric(regression_weather[[v]]))
      ref <- ref[is.finite(ref)]
      if (!length(ref)) {
        return(NULL)
      }
      robust <- as.numeric(stats::quantile(ref, c(lower, upper),
        names = FALSE,
        na.rm = TRUE, type = 8
      ))
      supported <- NULL
      reference_label <- NA_character_
    }
    out <- lapply(names(scenarios), function(nm) {
      x_raw <- scenarios[[nm]][[v]]
      x <- if (is_binned) {
        x <- as.character(x_raw)
        x[!is.na(x) & nzchar(x)]
      } else {
        x <- suppressWarnings(as.numeric(x_raw))
        x[is.finite(x)]
      }
      if (!length(x)) {
        return(NULL)
      }
      outside <- if (is_binned) {
        !(x %in% supported)
      } else {
        x < robust[[1L]] | x > robust[[2L]]
      }
      data.frame(
        weather_variable = v, scenario = nm,
        n_reference = length(ref), n_scenario = length(x),
        robust_lo = robust[[1L]], robust_hi = robust[[2L]],
        reference_label = reference_label,
        is_binned = is_binned,
        outside_n = sum(outside), outside_share = mean(outside),
        warning = mean(outside) > warn_share,
        warning_rule = if (is_binned) {
          paste0(
            "Reference bins; warn above ",
            warn_share * 100, "% outside"
          )
        } else {
          paste0(
            "Robust ", lower * 100, "%-", upper * 100,
            "% reference interval; warn above ", warn_share * 100, "% outside"
          )
        },
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(out)
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# Year-anchored welfare ridge plot ----

# Compute KDE for one group using supplied bandwidth and x-range.
# Returns a data.frame(x, density_raw) on the shared n-point grid.
#' @noRd
.kde_group <- function(vals, bw, x_lo, x_hi, n = 512L) {
  vals <- as.numeric(vals)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(data.frame(
      x = seq(x_lo, x_hi, length.out = n),
      density_raw = 0
    ))
  }

  rd <- build_ridge_distribution_data(
    data.frame(.x = vals, .g = "ridge", .f = "ridge"),
    x_var = ".x",
    group_var = ".g",
    fill_var = ".f",
    n_bins = 256L,
    n_grid = n,
    x_range = c(x_lo, x_hi),
    bandwidth = bw
  )
  if (is.null(rd)) {
    return(data.frame(
      x = seq(x_lo, x_hi, length.out = n),
      density_raw = 0
    ))
  }
  data.frame(x = rd$data$x, density_raw = rd$data$height)
}


#' Pre-compute KDE Data for Year-Anchored Ridge Plot
#'
#' Pure function. Extracts per-group values, computes a single global pooled
#' bandwidth and P1-P99 x-range (in display-space), and stores the raw value
#' groups for \code{plot_year_anchored_ridge()} to consume.
#'
#' Separating KDE computation from rendering means the expensive
#' \code{stats::density()} calls only re-run when the underlying prediction
#' data changes, not on every slider tick.
#'
#' @param hist_preds    Data frame. \code{hist_sim()$preds}.
#' @param scenario_list Named list. Each element is
#'   \code{list(preds = <df>, so = <list>)}. Pass \code{list()} for
#'   historical-only.
#' @param outcome_name  Character scalar. Column to plot.
#' @param actual_vals   Optional numeric vector of observed outcome values
#'   (from the training data) included as an "Actual" comparison group.
#' @param log_scale     Logical. Compute BW and clip in log10-space. Default
#'   \code{FALSE}.
#'
#' @return Named list with slots: hist_groups, scenario_groups, global_bw,
#'   x_lo_tr, x_hi_tr, scen_nms, ssp_keys, fore_yrs_raw, sim_years,
#'   log_scale, outcome_name. Returns NULL if no finite values found.
#'
#' @export
build_ridge_kde_data <- function(hist_preds,
                                 scenario_list,
                                 outcome_name,
                                 actual_vals = NULL,
                                 log_scale = FALSE) {
  if (is.null(hist_preds) || !outcome_name %in% names(hist_preds)) {
    return(NULL)
  }

  yr_col <- intersect(c("sim_year", "year"), names(hist_preds))[1]
  if (is.na(yr_col)) {
    return(NULL)
  }

  hist_preds$.__yr <- as.integer(as.character(hist_preds[[yr_col]]))
  sim_years <- sort(unique(hist_preds$.__yr[!is.na(hist_preds$.__yr)]))

  hist_groups <- lapply(sim_years, function(yr) {
    vals <- as.numeric(hist_preds[[outcome_name]][hist_preds$.__yr == yr])
    vals[is.finite(vals)]
  })
  names(hist_groups) <- as.character(sim_years)
  hist_groups <- Filter(function(v) length(v) >= 2L, hist_groups)
  if (length(hist_groups) == 0L) {
    return(NULL)
  }

  scenario_groups <- list()
  yr_col_fut <- yr_col

  for (scen_nm in names(scenario_list)) {
    sp <- scenario_list[[scen_nm]]$preds
    if (is.null(sp) || !outcome_name %in% names(sp)) next
    if (!yr_col_fut %in% names(sp)) {
      alt <- intersect(c("sim_year", "year"), names(sp))[1]
      if (is.na(alt)) next
      yr_col_fut <- alt
    }
    sp$.__yr <- as.integer(as.character(sp[[yr_col_fut]]))
    for (yr in sim_years) {
      vals <- as.numeric(sp[[outcome_name]][sp$.__yr == yr])
      vals <- vals[is.finite(vals)]
      if (length(vals) < 2L) next
      if (is.null(scenario_groups[[scen_nm]])) {
        scenario_groups[[scen_nm]] <- list()
      }
      scenario_groups[[scen_nm]][[as.character(yr)]] <- vals
    }
  }

  all_vals <- c(
    unlist(hist_groups, use.names = FALSE),
    unlist(scenario_groups, use.names = FALSE)
  )
  all_vals <- all_vals[is.finite(all_vals)]
  # Build one compact histogram for bandwidth and clipping. This avoids sorting
  # every simulated draw just to determine the display range and KDE scale.
  global_rd <- build_ridge_distribution_data(
    data.frame(.x = all_vals, .g = "all", .f = "all"),
    x_var = ".x",
    group_var = ".g",
    fill_var = ".f",
    n_bins = 512L,
    n_grid = 64L
  )
  if (is.null(global_rd)) {
    return(NULL)
  }
  global_bw <- global_rd$bandwidth
  qs <- global_rd$quantile_range

  # Regression output:
  #   predicted = clean model fitted values (.fitted column, no residual noise)
  #              falls back to outcome_name column if .fitted absent
  #   actual    = observed survey outcome from train_data (passed in as actual_vals)
  #               back-transformed from log-scale when so$transform == "log"
  # predicted = unique back-transformed outcome values (outcome_name col is
  # already back-transformed by apply_log_backtransform(); .fitted is NOT).
  # Deduplicate across draws so bandwidth is estimated at training-sample scale.
  predicted_vals <- tryCatch(
    {
      v <- unique(as.numeric(hist_preds[[outcome_name]]))
      v[is.finite(v)]
    },
    error = function(e) numeric(0)
  )

  actual_vals_clean <- tryCatch(
    {
      if (is.null(actual_vals)) {
        numeric(0)
      } else {
        v <- as.numeric(actual_vals)
        v[is.finite(v)]
      }
    },
    error = function(e) numeric(0)
  )

  # Separate bandwidth for regression curves: estimated from the regression
  # sample alone so it is not dominated by the 30yr x N hist_groups pool.
  reg_bw <- if (length(predicted_vals) >= 2L) {
    pred_rd <- build_ridge_distribution_data(
      data.frame(.x = predicted_vals, .g = "pred", .f = "pred"),
      x_var = ".x", group_var = ".g", fill_var = ".f",
      n_bins = 256L, n_grid = 64L
    )
    if (is.null(pred_rd)) global_bw else pred_rd$bandwidth
  } else {
    global_bw
  }

  # Materialise every curve once while the source vectors are available. The
  # renderer can then switch between display modes without re-scanning the
  # simulation draws or re-running a KDE for each visible curve.
  hist_curves <- lapply(hist_groups, .kde_group,
    bw = global_bw, x_lo = qs[1L], x_hi = qs[2L]
  )
  scenario_curves <- lapply(scenario_groups, function(by_year) {
    lapply(by_year, .kde_group,
      bw = global_bw,
      x_lo = qs[1L], x_hi = qs[2L]
    )
  })
  scenario_pooled_curves <- lapply(scenario_groups, function(by_year) {
    vals <- unlist(by_year, use.names = FALSE)
    .kde_group(vals, bw = global_bw, x_lo = qs[1L], x_hi = qs[2L])
  })
  predicted_curve <- if (length(predicted_vals) >= 2L) {
    .kde_group(predicted_vals, bw = reg_bw, x_lo = qs[1L], x_hi = qs[2L])
  } else {
    NULL
  }
  actual_curve <- if (length(actual_vals_clean) >= 2L) {
    .kde_group(actual_vals_clean, bw = reg_bw, x_lo = qs[1L], x_hi = qs[2L])
  } else {
    NULL
  }

  scen_nms <- names(scenario_groups)
  ssp_keys <- vapply(scen_nms, .normalise_ssp, character(1))
  fore_yrs_raw <- vapply(scen_nms, function(nm) {
    m <- regmatches(nm, regexpr("[0-9]{4}", nm))
    if (length(m) == 0L) NA_integer_ else as.integer(m)
  }, integer(1))

  list(
    hist_groups = hist_groups,
    scenario_groups = scenario_groups,
    global_bw = global_bw,
    x_lo_tr = qs[[1L]],
    x_hi_tr = qs[[2L]],
    scen_nms = scen_nms,
    ssp_keys = ssp_keys,
    fore_yrs_raw = fore_yrs_raw,
    sim_years = sim_years,
    log_scale = log_scale,
    outcome_name = outcome_name,
    predicted_vals = predicted_vals,
    actual_vals = actual_vals_clean,
    reg_bw = reg_bw,
    ridge_curves = list(
      hist            = hist_curves,
      scenario        = scenario_curves,
      scenario_pooled = scenario_pooled_curves,
      predicted       = predicted_curve,
      actual          = actual_curve
    )
  )
}


#' Year-Anchored Welfare Outcome Ridge Plot
#'
#' Renders a ridge plot from pre-computed KDE data produced by
#' \code{build_ridge_kde_data()}. Two primary grouping modes:
#'
#' \describe{
#'   \item{\code{"hist_year"}}{One grey filled ridge per historical simulation
#'     year; scenario perturbations as coloured lines at the same baseline.}
#'   \item{\code{"scenario"}}{One grey filled ridge per scenario x forecast-year;
#'     grey-scale lines = historical years (darkest = most recent).}
#' }
#'
#' @param kde_data       List. Output of \code{build_ridge_kde_data()}.
#' @param x_label        Character scalar. X-axis label.
#' @param primary_group  Character. \code{"hist_year"} (default),
#'   \code{"scenario"}, or \code{"forecast_yr"}.
#' @param log_scale      Logical. Log10 x-axis. Overrides kde_data$log_scale.
#'   Default NULL (inherit from kde_data).
#' @param ridge_scale    Numeric. Vertical height multiplier. Default 1.5.
#' @param row_gap        Numeric. Spacing between rows. Default 1.0.
#' @param show_regression Logical. When TRUE, overlays regression sample
#'   predicted (black dashed) and actual outcome (black dotted) density
#'   curves from hist_preds training rows. Default FALSE.
#' @param scenario_names Character vector. Subset of scenario names to render.
#'   NULL (default) renders all scenarios in kde_data.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   scale_colour_manual scale_linetype_manual scale_y_continuous
#'   scale_x_log10 expansion labs theme_minimal theme element_text
#'   element_blank element_line guide_legend
#' @importFrom rlang .data
#' @export
plot_year_anchored_ridge <- function(kde_data,
                                     x_label,
                                     primary_group = "hist_year",
                                     log_scale = NULL,
                                     ridge_scale = 1.5,
                                     row_gap = 1.0,
                                     show_regression = FALSE,
                                     scenario_names = NULL) {
  if (is.null(kde_data)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "No simulation data available."))
  }

  use_log <- if (!is.null(log_scale)) isTRUE(log_scale) else isTRUE(kde_data$log_scale)

  hist_groups <- kde_data$hist_groups
  # Apply scenario filter: NULL means show all; otherwise subset to named keys.
  scenario_groups <- if (!is.null(scenario_names)) {
    kde_data$scenario_groups[intersect(scenario_names, names(kde_data$scenario_groups))]
  } else {
    kde_data$scenario_groups
  }
  global_bw <- kde_data$global_bw
  x_lo_tr <- kde_data$x_lo_tr
  x_hi_tr <- kde_data$x_hi_tr
  # Derive scen_nms / ssp_keys / fore_yrs_raw from filtered scenario_groups
  # so they stay in sync when scenario_names subsets the full KDE data.
  scen_nms <- names(scenario_groups)
  ssp_keys <- vapply(scen_nms, .normalise_ssp, character(1))
  fore_yrs_raw <- suppressWarnings(
    vapply(scen_nms, function(nm) as.integer(.parse_year(nm)), integer(1))
  )
  predicted_vals <- kde_data$predicted_vals %||% numeric(0)
  actual_vals <- kde_data$actual_vals %||% numeric(0)
  ridge_curves <- kde_data$ridge_curves %||% list()
  hist_curves <- ridge_curves$hist %||% list()
  scenario_curves <- ridge_curves$scenario %||% list()
  pooled_curves <- ridge_curves$scenario_pooled %||% list()


  hist_fill <- "#d0d0d0"
  hist_colour <- "#333333"
  hist_alpha <- 0.55

  yr_lty_palette <- c("solid", "dashed", "dotted", "longdash", "twodash")
  unique_fore_yrs <- sort(unique(fore_yrs_raw[!is.na(fore_yrs_raw)]))
  fore_yr_lty_map <- setNames(
    yr_lty_palette[seq_len(min(length(unique_fore_yrs), length(yr_lty_palette)))],
    as.character(unique_fore_yrs)
  )

  scen_colour_map <- setNames(
    vapply(ssp_keys, function(k) {
      if (!is.na(k) && k %in% names(.ssp_colours)) .ssp_colours[k] else "#aaaaaa"
    }, character(1)),
    scen_nms
  )
  scen_lty_map <- setNames(
    vapply(as.character(fore_yrs_raw), function(yr) {
      if (!is.na(yr) && yr %in% names(fore_yr_lty_map)) fore_yr_lty_map[yr] else "solid"
    }, character(1)),
    scen_nms
  )

  .run_kde <- function(vals, precomputed = NULL) {
    if (!is.null(precomputed)) {
      return(precomputed)
    }
    kv <- vals[is.finite(vals)]
    kd <- .kde_group(kv, global_bw, x_lo_tr, x_hi_tr)
    dns <- kd$density_raw
    dns <- dns / max(dns[is.finite(dns) & dns > 0], 1e-12) # normalise peak to 1
    list(x = kd$x, density_raw = dns)
  }
  # Mode A: hist_year primary                                             #
  # ==================================================================== #
  if (primary_group == "hist_year") {
    yr_rank <- setNames(
      seq_len(length(hist_groups)) * row_gap,
      names(hist_groups)
    )

    hist_ribbons <- list()
    scen_lines <- list()

    for (yr_chr in names(hist_groups)) {
      y_anch <- yr_rank[yr_chr]
      kd <- .run_kde(
        hist_groups[[yr_chr]], hist_curves[[yr_chr]]
      )

      hist_ribbons[[yr_chr]] <- data.frame(
        x = kd$x,
        ymin = y_anch,
        ymax = y_anch + kd$density_raw * ridge_scale,
        row.names = NULL,
        stringsAsFactors = FALSE
      )

      active_sub <- Filter(
        function(nm) !is.null(scenario_groups[[nm]][[yr_chr]]),
        scen_nms
      )
      for (nm in active_sub) {
        kd2 <- .run_kde(
          scenario_groups[[nm]][[yr_chr]],
          scenario_curves[[nm]][[yr_chr]]
        )
        scen_lines[[paste0(nm, "__", yr_chr)]] <- data.frame(
          x = kd2$x,
          y = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col = scen_colour_map[nm],
          lty = scen_lty_map[nm],
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(yr_rank)
    y_labels <- paste0("  ", names(yr_rank))
    subtitle <- paste0(
      "Primary: historical year.  Grey fill = base year.  ",
      "Coloured lines = scenario perturbations at same baseline.  ",
      "X clipped P1\u2013P99."
    )

    # ==================================================================== #
    # Mode B: scenario primary                                              #
    # ==================================================================== #
  } else if (primary_group == "scenario") {
    # scenario mode: sort row_keys by SSP then forecast year
    row_keys <- if (length(scen_nms) > 0) {
      scen_nms[order(ssp_keys[scen_nms], fore_yrs_raw[scen_nms])]
    } else {
      scen_nms
    }
    if (length(row_keys) == 0L) {
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "No scenario data. Run future scenarios first."))
    }

    row_rank <- setNames(seq_len(length(row_keys)) * row_gap, row_keys)

    yr_chrs <- names(hist_groups)
    n_yrs <- length(yr_chrs)
    grey_vals <- seq(0.75, 0.15, length.out = max(n_yrs, 1L))
    yr_grey <- setNames(grDevices::grey(grey_vals), yr_chrs)

    hist_ribbons <- list()
    scen_lines <- list()

    for (scen_nm in row_keys) {
      y_anch <- row_rank[scen_nm]
      all_scen_vals <- unlist(scenario_groups[[scen_nm]], use.names = FALSE)
      all_scen_vals <- all_scen_vals[is.finite(all_scen_vals)]
      if (length(all_scen_vals) >= 2L) {
        kd_base <- .run_kde(
          all_scen_vals, pooled_curves[[scen_nm]]
        )
        hist_ribbons[[scen_nm]] <- data.frame(
          x = kd_base$x,
          ymin = y_anch,
          ymax = y_anch + kd_base$density_raw * ridge_scale,
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
      for (yr_chr in yr_chrs) {
        yr_vals <- scenario_groups[[scen_nm]][[yr_chr]]
        if (is.null(yr_vals) || length(yr_vals) < 2L) next
        kd2 <- .run_kde(
          yr_vals, scenario_curves[[scen_nm]][[yr_chr]]
        )
        scen_lines[[paste0(scen_nm, "__", yr_chr)]] <- data.frame(
          x = kd2$x, y = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col = yr_grey[yr_chr], lty = "solid",
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(row_rank)
    y_labels <- paste0("  ", names(row_rank))
    subtitle <- paste0(
      "Primary: scenario * forecast year.  ",
      "Fill = pooled scenario distribution.  ",
      "Grey lines = individual historical years (darkest = most recent).  ",
      "X clipped P1-P99."
    )
  } else {
    # forecast_yr mode: sort row_keys by forecast year then SSP
    row_keys <- if (length(scen_nms) > 0) {
      scen_nms[order(fore_yrs_raw[scen_nms], ssp_keys[scen_nms])]
    } else {
      scen_nms
    }
    if (length(row_keys) == 0L) {
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "No scenario data. Run future scenarios first."))
    }

    row_rank <- setNames(seq_len(length(row_keys)) * row_gap, row_keys)

    yr_chrs <- names(hist_groups)
    n_yrs <- length(yr_chrs)
    grey_vals <- seq(0.75, 0.15, length.out = max(n_yrs, 1L))
    yr_grey <- setNames(grDevices::grey(grey_vals), yr_chrs)

    hist_ribbons <- list()
    scen_lines <- list()

    for (scen_nm in row_keys) {
      y_anch <- row_rank[scen_nm]
      all_scen_vals <- unlist(scenario_groups[[scen_nm]], use.names = FALSE)
      all_scen_vals <- all_scen_vals[is.finite(all_scen_vals)]
      if (length(all_scen_vals) >= 2L) {
        kd_base <- .run_kde(
          all_scen_vals, pooled_curves[[scen_nm]]
        )
        hist_ribbons[[scen_nm]] <- data.frame(
          x = kd_base$x,
          ymin = y_anch,
          ymax = y_anch + kd_base$density_raw * ridge_scale,
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
      for (yr_chr in yr_chrs) {
        yr_vals <- scenario_groups[[scen_nm]][[yr_chr]]
        if (is.null(yr_vals) || length(yr_vals) < 2L) next
        kd2 <- .run_kde(
          yr_vals, scenario_curves[[scen_nm]][[yr_chr]]
        )
        scen_lines[[paste0(scen_nm, "__", yr_chr)]] <- data.frame(
          x = kd2$x, y = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col = yr_grey[yr_chr], lty = "solid",
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(row_rank)
    y_labels <- paste0("  ", names(row_rank))
    subtitle <- paste0(
      "Primary: forecast year * scenario.  ",
      "Fill = pooled scenario distribution.  ",
      "Grey lines = individual historical years (darkest = most recent).  ",
      "X clipped P1-P99."
    )
  }

  # ==================================================================== #
  # Build ggplot (common to both modes)                                   #
  # ==================================================================== #
  p <- ggplot2::ggplot()

  for (key in names(hist_ribbons)) {
    d <- hist_ribbons[[key]]
    p <- p +
      ggplot2::geom_ribbon(
        data    = d,
        mapping = ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
        fill    = hist_fill,
        alpha   = hist_alpha,
        colour  = NA
      ) +
      ggplot2::geom_line(
        data      = d,
        mapping   = ggplot2::aes(x = .data$x, y = .data$ymax),
        colour    = hist_colour,
        linetype  = "solid",
        linewidth = 0.6
      )
  }

  for (key in names(scen_lines)) {
    d <- scen_lines[[key]]
    p <- p +
      ggplot2::geom_line(
        data      = d,
        mapping   = ggplot2::aes(x = .data$x, y = .data$y),
        colour    = d$line_col[1],
        linetype  = d$lty[1],
        linewidth = 0.65
      )
  }

  # Regression output overlay (when show_regression = TRUE) ----
  # Two curves at the same y_reg baseline:
  #   predicted_vals  -- dashed black line  (simulated outcome distribution)
  #   actual_vals     -- dotted black line  (observed survey outcomes)
  has_predicted <- isTRUE(show_regression) && length(predicted_vals) >= 2L
  has_actual <- isTRUE(show_regression) && length(actual_vals) >= 2L

  if (has_predicted || has_actual) {
    rank_vec <- tryCatch(yr_rank, error = function(e) {
      tryCatch(row_rank, error = function(e2) c(1))
    })
    y_reg <- min(as.numeric(rank_vec)) - row_gap * 0.8

    if (has_predicted) {
      reg_bw_use <- kde_data$reg_bw %||% global_bw
      kd_pred_raw <- ridge_curves$predicted %||%
        .kde_group(
          predicted_vals[is.finite(predicted_vals)],
          reg_bw_use, x_lo_tr, x_hi_tr
        )
      pred_dns <- kd_pred_raw$density_raw
      pred_dns <- pred_dns / max(pred_dns[is.finite(pred_dns) & pred_dns > 0], 1e-12)
      pred_df <- data.frame(
        x = kd_pred_raw$x,
        y = y_reg + pred_dns * ridge_scale * 0.85,
        ymin = y_reg,
        stringsAsFactors = FALSE
      )
      p <- p +
        ggplot2::geom_ribbon(
          data = pred_df,
          mapping = ggplot2::aes(
            x = .data$x, ymin = .data$ymin,
            ymax = .data$y
          ),
          fill = "#cccccc",
          alpha = 0.25,
          colour = NA
        ) +
        ggplot2::geom_line(
          data      = pred_df,
          mapping   = ggplot2::aes(x = .data$x, y = .data$y),
          colour    = "black",
          linetype  = "dashed",
          linewidth = 0.8
        )
    }

    if (has_actual) {
      reg_bw_use <- kde_data$reg_bw %||% global_bw
      act_vals_fi <- actual_vals[is.finite(actual_vals)]
      # Use reg_bw from predicted_vals since both are on the same scale
      kd_act_raw <- ridge_curves$actual %||%
        .kde_group(act_vals_fi, reg_bw_use, x_lo_tr, x_hi_tr)
      act_dns <- kd_act_raw$density_raw
      act_dns <- act_dns / max(act_dns[is.finite(act_dns) & act_dns > 0], 1e-12)
      act_df <- data.frame(
        x = kd_act_raw$x,
        y = y_reg + act_dns * ridge_scale * 0.85,
        ymin = y_reg,
        stringsAsFactors = FALSE
      )
      p <- p +
        ggplot2::geom_line(
          data      = act_df,
          mapping   = ggplot2::aes(x = .data$x, y = .data$y),
          colour    = "black",
          linetype  = "dotted",
          linewidth = 0.9
        )
    }

    # Extend y-axis to include the regression row
    y_breaks <- c(y_reg, y_breaks)
    y_labels <- c("  Regression", y_labels)
  }

  if (primary_group == "hist_year") {
    ssp_disp <- sort(unique(ssp_keys[!is.na(ssp_keys)]))
    ssp_col_vals <- vapply(ssp_disp, function(k) {
      if (k %in% names(.ssp_colours)) .ssp_colours[k] else "#aaaaaa"
    }, character(1))

    if (length(ssp_disp) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = data.frame(
            x = rep(NA_real_, length(ssp_disp)),
            y = rep(NA_real_, length(ssp_disp)),
            ssp_key = ssp_disp,
            stringsAsFactors = FALSE
          ),
          mapping = ggplot2::aes(x = .data$x, y = .data$y, colour = .data$ssp_key),
          size = 0,
          na.rm = TRUE
        ) +
        ggplot2::scale_colour_manual(
          name = "Climate scenario",
          values = setNames(ssp_col_vals, ssp_disp),
          guide = ggplot2::guide_legend(
            override.aes = list(linewidth = 2.5, linetype = "solid", size = 4)
          )
        )
    }

    if (length(fore_yr_lty_map) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = data.frame(
            x = rep(NA_real_, length(fore_yr_lty_map)),
            y = rep(NA_real_, length(fore_yr_lty_map)),
            fore_lbl = names(fore_yr_lty_map),
            stringsAsFactors = FALSE
          ),
          mapping = ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$fore_lbl),
          colour = "#555555",
          na.rm = TRUE
        ) +
        ggplot2::scale_linetype_manual(
          name = "Forecast year",
          values = fore_yr_lty_map,
          guide = ggplot2::guide_legend(
            override.aes = list(linewidth = 0.9, colour = "#555555")
          )
        )
    }
  }

  p <- p +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      expand = ggplot2::expansion(add = c(0.6, 1.0))
    ) +
    ggplot2::labs(
      title    = "Welfare output distributions",
      subtitle = subtitle,
      x        = x_label,
      y        = NULL
    ) +
    theme_wise() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92"),
      axis.text.y        = ggplot2::element_text(size = 9, colour = "#333333"),
      axis.text.x        = ggplot2::element_text(size = 9),
      plot.subtitle      = ggplot2::element_text(size = 9, colour = "grey45"),
      legend.position    = "top",
      legend.box         = "horizontal",
      legend.text        = ggplot2::element_text(size = 9),
      legend.title       = ggplot2::element_text(size = 9, face = "bold")
    )

  if (isTRUE(use_log)) p <- p + ggplot2::scale_x_log10()
  p
}

# ============================================================================
# Interactive (echarts4r) counterpart of the weather-density diagnostic panel
# (guidelines §7). The ggplot/patchwork builder above stays intact for the
# static fallback and export consumers.
#
# Design (documented judgment call): the patchwork multi-variable panel is
# collapsed into ONE echarts widget. With a single selected variable the
# widget uses the Step 1 ridge computation and value/share tooltips, with one
# row per source. Binned variables use percentage bars and Step 1 bin labels.
# With several continuous variables the curves become a ridgeline: each
# variable's densities are normalised and offset vertically on its own row,
# so one interactive widget replaces the patchwork without re-rendering
# per-variable plots. Densities are computed in R with the same parameters as
# ggplot's geom_density (bw = "nrd0", gaussian kernel, n = 512).
# ============================================================================

.e_diag_base <- function(height) {
  e <- echarts4r::e_charts(data.frame(x = 0:1, y = 0:1), x, height = height)
  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL
  e$x$opts$series <- NULL
  e$x$opts$legend <- NULL
  e$x$opts$tooltip <- NULL
  e$x$opts$grid <- NULL
  e
}

# One density curve as a line series (or filled area for the historical
# reference). Statistics precomputed in R.
.e_density_series <- function(name, x, y, colour, area = FALSE,
                              area_opacity = 0.35, line_type = "solid",
                              width = 1.2) {
  st <- list(
    name = name,
    type = "line",
    symbol = "none",
    z = if (area) 1 else 2,
    lineStyle = list(color = colour, width = width, type = line_type),
    itemStyle = list(color = colour),
    data = unname(lapply(seq_along(x), function(i) {
      list(value = list(unname(x[[i]]), unname(y[[i]])))
    }))
  )

  if (area) {
    st$areaStyle <- list(color = colour, opacity = area_opacity)
  } else {
    st$areaStyle <- list(color = "rgba(0,0,0,0)", opacity = 0)
  }
  list(st)
}

.diag_weather_source_label <- function(x) {
  if (identical(x, "Full historical")) return("Historical")
  if (identical(x, "Model support")) return(x)
  ssp <- .normalise_ssp(x)
  if (is.na(ssp)) {
    digits <- regmatches(x, regexpr("(?i)SSP[2345]", x, perl = TRUE))
    ssp <- if (length(digits) && nzchar(digits)) .normalise_ssp(toupper(digits)) else NA_character_
  }
  years <- regmatches(x, gregexpr("[0-9]{4}", x, perl = TRUE))[[1L]]
  if (!is.na(ssp)) return(if (length(years) >= 2L) paste(ssp, "/", paste(tail(years, 2L), collapse = "-")) else ssp)
  x
}

.diag_weather_spec_row <- function(weather_specs, weather_var) {
  if (is.null(weather_specs) || !is.data.frame(weather_specs) ||
    !all(c("name", "cont_binned") %in% names(weather_specs))) {
    return(NULL)
  }
  i <- match(weather_var, as.character(weather_specs$name))
  if (is.na(i)) NULL else weather_specs[i, , drop = FALSE]
}

.diag_weather_is_binned <- function(weather_specs, weather_var, raw_col) {
  spec <- .diag_weather_spec_row(weather_specs, weather_var)
  if (!is.null(spec) && !is.na(spec$cont_binned[[1L]])) {
    return(identical(as.character(spec$cont_binned[[1L]]), "Binned"))
  }
  is.factor(raw_col) || is.character(raw_col) ||
    (is.integer(raw_col) && length(unique(raw_col[!is.na(raw_col)])) <= 20L)
}

echart_weather_density_panel <- function(survey_weather,
                                         weather_raw,
                                         weather_vars,
                                         weather_labels = NULL,
                                         scenario_weather = NULL,
                                         active_scenarios = NULL,
                                         log_x = FALSE,
                                         show_regression = FALSE,
                                         height = "340px",
                                         weather_specs = NULL,
                                         stored_breaks = NULL) {
  if (is.null(weather_vars) || !is.character(weather_vars) ||
    !length(weather_vars)) {
    return(echart_blank("No selected weather variables found in weather_raw.",
      height = height
    ))
  }
  missing_vars <- setdiff(weather_vars, names(weather_raw))
  if (length(missing_vars)) {
    return(echart_blank(paste0(
      "Selected weather variable(s) not found: ", paste(missing_vars, collapse = ", "), "."
    ), height = height))
  }
  weather_vars <- intersect(weather_vars, names(weather_raw))
  if (!length(weather_vars)) {
    return(echart_blank("No selected weather variables found in weather_raw.",
      height = height
    ))
  }
  if (length(log_x) == 1L) log_x <- rep(log_x, length(weather_vars))

  disp_label <- function(wv) {
    if (!is.null(weather_labels) && wv %in% names(weather_labels)) {
      weather_labels[[wv]]
    } else {
      spec <- .diag_weather_spec_row(weather_specs, wv)
      if (!is.null(spec) && "label" %in% names(spec)) as.character(spec$label[[1L]]) else wv
    }
  }

  continuous_raw <- attr(weather_raw, "continuous_weather")
  stored_breaks <- stored_breaks %||% attr(weather_raw, "stored_breaks")

  e <- .e_diag_base(height)

  hist_filt <- .filter_hist_weather(weather_raw, survey_weather)
  if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather)) {
    survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
  }
  if ("timestamp" %in% names(survey_weather)) {
    survey_weather$cal_year <- as.integer(format(as.Date(survey_weather$timestamp), "%Y"))
  }
  continuous_filt <- if (is.data.frame(continuous_raw)) {
    .filter_hist_weather(continuous_raw, survey_weather)
  } else NULL
  if (!is.null(continuous_filt)) {
    continuous_filt <- continuous_filt[
      !duplicated(continuous_filt[, c("loc_id", "int_month", "cal_year")]), ,
      drop = FALSE
    ]
  }
  reg_filt <- hist_filt[hist_filt$cal_year %in% unique(survey_weather$cal_year), , drop = FALSE]
  visible_nms <- names(scenario_weather %||% list())
  if (!is.null(active_scenarios)) visible_nms <- intersect(visible_nms, active_scenarios)

  is_binned <- vapply(weather_vars, function(wv) {
    .diag_weather_is_binned(weather_specs, wv, weather_raw[[wv]])
  }, logical(1L))

  # ---- Multi-variable: one ridge widget for continuous selections -----------
  if (length(weather_vars) > 1L) {
    if (any(is_binned) && !all(is_binned)) {
      return(echart_blank(
        "Select either continuous or binned weather variables together; mixed selections use incompatible x axes.",
        height = height
      ))
    }
    if (all(is_binned)) {
      return(.echart_weather_binned_panel(
        hist_filt, reg_filt, weather_raw, weather_vars, weather_specs,
        scenario_weather, visible_nms, weather_labels, height, stored_breaks, show_regression
      ))
    }
    multi_ok <- TRUE
    curve_sets <- list()
    for (i in seq_along(weather_vars)) {
      wv <- weather_vars[[i]]
      raw_col <- weather_raw[[wv]]
      is_factor <- .diag_weather_is_binned(weather_specs, wv, raw_col)
      if (is_factor) {
        multi_ok <- FALSE
        break
      }
      hist_source <- if (isTRUE(is_binned[match(wv, weather_vars)]) &&
        is.data.frame(continuous_filt) && wv %in% names(continuous_filt)) {
        continuous_filt
      } else {
        hist_filt
      }
      hist_vals <- as.numeric(hist_source[[wv]])
      hist_vals <- hist_vals[is.finite(hist_vals)]
      if (!length(hist_vals)) {
        multi_ok <- FALSE
        break
      }
      d <- stats::density(hist_vals, bw = "nrd0", kernel = "gaussian", n = 512)
      curve_sets[[wv]] <- list(
        x = d$x,
        historical = d$y,
        scenarios = list(),
        support = numeric(0)
      )
      reg_source <- if (is_binned[[i]] && is.data.frame(continuous_filt) &&
        wv %in% names(continuous_filt)) continuous_filt else reg_filt
      if (is_binned[[i]] && "cal_year" %in% names(reg_source)) {
        reg_source <- reg_source[reg_source$cal_year %in% unique(survey_weather$cal_year), , drop = FALSE]
      }
      reg_vals <- as.numeric(reg_source[[wv]])
      reg_vals <- reg_vals[is.finite(reg_vals)]
      if (isTRUE(show_regression) && length(reg_vals)) {
        dr <- stats::density(reg_vals, bw = "nrd0", kernel = "gaussian", n = 512)
        curve_sets[[wv]]$support <- stats::approx(dr$x, dr$y, xout = d$x, rule = 2)$y
      }
      for (nm in visible_nms) {
        sw_df <- scenario_weather[[nm]]
        if (is.null(sw_df) || !wv %in% names(sw_df)) next
        vals <- as.numeric(sw_df[[wv]])
        vals <- vals[is.finite(vals)]
        if (!length(vals)) next
        ds <- stats::density(vals, bw = "nrd0", kernel = "gaussian", n = 512)
        curve_sets[[wv]]$scenarios[[nm]] <- ds$y
        # All curves for one variable share the historical grid's x range only
        # approximately; re-evaluate on the historical grid for alignment.
        curve_sets[[wv]]$scenarios[[nm]] <-
          stats::approx(ds$x, ds$y, xout = d$x, rule = 2)$y
      }
    }
    if (!multi_ok) {
      return(echart_blank(
        "No continuous historical distribution is available for every selected variable.",
        height = height
      ))
    } else {
      n_rows <- length(weather_vars)
      series <- list()
      ridge_scale <- 0.8
      for (i in seq_len(n_rows)) {
        wv <- weather_vars[[i]]
        cs <- curve_sets[[wv]]
        y0 <- i - 1L
        ymax_all <- max(unlist(c(list(cs$historical, cs$support), cs$scenarios)), na.rm = TRUE)
        if (!is.finite(ymax_all) || ymax_all <= 0) ymax_all <- 1
        series <- c(series, .e_density_series(
          paste(disp_label(wv), "| Historical"), cs$x,
          y0 + cs$historical / ymax_all * ridge_scale,
          .wise_history, area = TRUE
        ))
        if (length(cs$support)) {
          series <- c(series, .e_density_series(
            paste(disp_label(wv), "| Model support"), cs$x,
            y0 + cs$support / ymax_all * ridge_scale,
            .wise_support, line_type = "dashed", width = 1
          ))
        }
        for (nm in names(cs$scenarios)) {
          display_source <- .diag_weather_source_label(nm)
          ssp_key <- .normalise_ssp(display_source)
          col <- if (!is.na(ssp_key) && ssp_key %in% names(.ssp_colours)) {
            unname(.ssp_colours[[ssp_key]])
          } else {
            "#cccccc"
          }
          series <- c(series, .e_density_series(
            paste(disp_label(wv), "|", display_source), cs$x,
            y0 + cs$scenarios[[nm]] / ymax_all * ridge_scale,
            col, area = FALSE, width = 1
          ))
        }
      }
      e$x$opts$xAxis <- list(
        type = "value",
        name = NULL,
        axisLabel = wise_eaxis_label(),
        splitLine = wise_esplit_line(),
        axisLine = list(lineStyle = list(color = .wise_grid))
      )
      e$x$opts$yAxis <- list(
        type = "category",
        data = as.character(seq_len(n_rows) - 1L),
        axisLabel = wise_eaxis_label(
          interval = 0L,
          formatter = htmlwidgets::JS(sprintf(
            "function(v){ var m = %s; return m[v] === undefined ? '' : m[v]; }",
            jsonlite::toJSON(as.list(stats::setNames(
             as.list(vapply(weather_vars, function(wv) {
               spec <- .diag_weather_spec_row(weather_specs, wv)
               units <- if (!is.null(spec) && "units" %in% names(spec)) as.character(spec$units[[1L]]) else NULL
               .weather_display_axis_label(disp_label(wv), units)
             }, character(1L))),
              as.list(as.character(seq_len(n_rows) - 1L))
            )), auto_unbox = TRUE)
          ))
        ),
        axisLine = list(lineStyle = list(color = .wise_grid)),
        axisTick = list(show = FALSE),
        splitLine = wise_esplit_line(show = FALSE)
      )
      e$x$opts$series <- unname(series)
      e$x$opts$legend <- modifyList(
        list(top = 4, left = "center", orient = "horizontal", type = "scroll"),
        wise_elegend_style()
      )
      e$x$opts$tooltip <- list(trigger = "axis")
      e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 16, top = 42, bottom = 18)
      return(wise_echart_theme(e))
    }
  }

  # ---- Single variable -----------------------------------------------------
  wv <- weather_vars[[1L]]
  lbl <- disp_label(wv)
  if (is_binned[[1L]]) {
    return(.echart_weather_binned_panel(
      hist_filt, reg_filt, weather_raw, wv, weather_specs,
      scenario_weather, visible_nms, weather_labels, height, stored_breaks, show_regression
    ))
  }
  source_rows <- list()
  add_source <- function(source, values) {
    values <- suppressWarnings(as.numeric(values))
    values <- values[is.finite(values)]
    if (!length(values)) return(invisible(NULL))
    key <- source
    source_rows[[key]] <<- dplyr::bind_rows(source_rows[[key]], data.frame(
      x = values, source = source, group = key,
      stringsAsFactors = FALSE
    ))
    invisible(NULL)
  }
  historical_source <- if (is.data.frame(continuous_filt) && wv %in% names(continuous_filt)) {
    continuous_filt
  } else {
    hist_filt
  }
  add_source("Historical", historical_source[[wv]])

  support_source <- if (is.data.frame(continuous_filt) && wv %in% names(continuous_filt)) {
    continuous_filt[continuous_filt$cal_year %in% unique(survey_weather$cal_year), , drop = FALSE]
  } else {
    reg_filt
  }
  if (isTRUE(show_regression)) {
    add_source("Model support", support_source[[wv]])
  }

  source_colours <- c(Historical = .wise_history, `Model support` = .wise_support)
  source_dashes <- c(Historical = FALSE, `Model support` = TRUE)
  for (nm in visible_nms) {
    sw_df <- scenario_weather[[nm]]
    if (is.null(sw_df) || !wv %in% names(sw_df)) next
    source <- .diag_weather_source_label(nm)
    ssp_key <- .normalise_ssp(source)
    colour <- if (!is.na(ssp_key) && ssp_key %in% names(.ssp_colours)) {
      unname(.ssp_colours[[ssp_key]])
    } else {
      "#cccccc"
    }
    add_source(source, sw_df[[wv]])
    source_colours[[source]] <- colour
    source_dashes[[source]] <- FALSE
  }
  if (!length(source_rows) || is.null(source_rows$Historical)) {
    return(echart_blank("No finite values to plot.", height = height))
  }

  ridge_input <- do.call(rbind, unname(source_rows))
  ridge_input$group <- as.character(ridge_input$group)
  log_transform <- isTRUE(log_x[[1L]])
  all_values <- ridge_input$x[is.finite(ridge_input$x) &
    (!log_transform | ridge_input$x > 0)]
  x_range <- if (length(all_values)) {
    as.numeric(stats::quantile(all_values, c(0.01, 0.99), names = FALSE, type = 8))
  } else NULL
  rd <- build_ridge_distribution_data(
    ridge_input,
    x_var = "x", group_var = "group", fill_var = "group",
    ridge_var = "source", n_bins = 256L, n_grid = 256L,
    log_transform = log_transform,
    x_range = if (log_transform && !is.null(x_range)) log10(x_range) else x_range,
    bandwidth_scale = 0.85
  )
  if (is.null(rd)) return(echart_blank("No finite values to plot.", height = height))

  rd_groups <- as.character(rd$data$group)
  rd$data$tooltip_value <- unlist(lapply(unique(rd_groups), function(source) {
    values <- ridge_input$x[ridge_input$group == source]
    values <- values[is.finite(values) & (!log_transform | values > 0)]
    x_grid <- rd$data$x[rd_groups == source]
    ord <- order(values)
    values <- values[ord]
    vapply(x_grid, function(x) findInterval(x, values) / length(values), numeric(1L))
  }), use.names = FALSE)
  styles <- data.frame(
    group = names(source_rows),
    fill = unname(source_colours[names(source_rows)]),
    line = unname(source_colours[names(source_rows)]),
    dashed = unname(source_dashes[names(source_rows)]),
    stringsAsFactors = FALSE
  )
  spec <- .diag_weather_spec_row(weather_specs, wv)
  units <- if (!is.null(spec) && "units" %in% names(spec)) as.character(spec$units[[1L]]) else NULL
  chart <- ridge_echart_widget(
    rd$data, rd$ridges, rd$ridges, styles,
    height = height, log_scale = log_transform,
    x_name = stringr::str_wrap(.weather_display_axis_label(lbl, units), 40),
    tooltip_x_name = stringr::str_wrap(.weather_display_axis_label(lbl, units), 40),
    hide_extreme_x = TRUE
  )
  chart$x$opts$legend <- modifyList(
    list(top = 4, left = "center", orient = "horizontal",
      data = as.list(unname(names(source_rows)))),
    wise_elegend_style()
  )
  if (!is.null(x_range) && all(is.finite(x_range)) && x_range[[1L]] < x_range[[2L]]) {
    if (log_transform) {
      chart$x$opts$xAxis$min <- max(x_range[[1L]], .Machine$double.xmin)
      chart$x$opts$xAxis$max <- x_range[[2L]]
    } else {
      pad <- 0.025 * diff(x_range)
      chart$x$opts$xAxis$min <- x_range[[1L]] - pad
      chart$x$opts$xAxis$max <- x_range[[2L]] + pad
    }
  }
  chart$x$opts$grid$top <- 42
  wise_echart_theme(chart)
}

.echart_weather_binned_panel <- function(hist_filt, reg_filt, weather_raw,
                                         weather_vars, weather_specs,
                                         scenario_weather, visible_nms,
                                         weather_labels, height,
                                         stored_breaks = NULL, show_regression = TRUE) {
  multi <- length(weather_vars) > 1L
  source_values <- list(Historical = hist_filt)
  if (isTRUE(show_regression) && nrow(reg_filt)) source_values[["Model support"]] <- reg_filt
  scenario_labels <- vapply(visible_nms, .diag_weather_source_label, character(1L))
  for (i in seq_along(visible_nms)) {
    nm <- visible_nms[[i]]
    source <- scenario_labels[[i]]
    source_values[[source]] <- dplyr::bind_rows(source_values[[source]], scenario_weather[[nm]])
  }

  labels <- function(wv) {
    if (!is.null(weather_labels) && wv %in% names(weather_labels)) {
      as.character(weather_labels[[wv]])
    } else {
      spec <- .diag_weather_spec_row(weather_specs, wv)
      if (!is.null(spec) && "label" %in% names(spec)) as.character(spec$label[[1L]]) else wv
    }
  }
  colors <- function(source) {
    if (identical(source, "Historical")) return(.wise_history)
    if (identical(source, "Model support")) return(.wise_support)
    key <- .normalise_ssp(source)
    if (!is.na(key) && key %in% names(.ssp_colours)) unname(.ssp_colours[[key]]) else "#cccccc"
  }

  continuous_raw <- attr(weather_raw, "continuous_weather")
  continuous_filt <- if (is.data.frame(continuous_raw)) {
    .filter_hist_weather(continuous_raw, data.frame(
      loc_id = hist_filt$loc_id,
      int_month = hist_filt$int_month
    ))
  } else NULL
  if (!is.null(continuous_filt)) {
    continuous_filt <- continuous_filt[
      !duplicated(continuous_filt[, c("loc_id", "int_month", "cal_year")]), ,
      drop = FALSE
    ]
  }
  categories <- list()
  proportions <- list()
  for (wv in weather_vars) {
    brks <- stored_breaks[[wv]] %||% attr(weather_raw, "stored_breaks")[[wv]]
    lev <- if (!is.null(brks)) {
      levels(relabel_bin_levels(
        data.frame(value = factor(character(0), levels = levels(cut(
          brks[is.finite(brks)], breaks = brks,
          include.lowest = TRUE, right = TRUE
        )))),
        list(value = brks)
      )$value)
    } else if (is.factor(weather_raw[[wv]])) {
      .bin_level_order(levels(weather_raw[[wv]]))
    } else {
      vals <- unlist(lapply(source_values, function(df) {
        if (is.data.frame(df) && wv %in% names(df)) as.character(df[[wv]]) else character(0)
      }), use.names = FALSE)
      sort(unique(vals[!is.na(vals) & nzchar(vals)]))
    }
    lev <- .bin_level_order(lev)
    if (!length(lev)) next
    for (source in names(source_values)) {
      df <- source_values[[source]]
      if (!is.data.frame(df) || !wv %in% names(df)) next
      source_df <- df
      if (identical(source, "Historical") && is.data.frame(continuous_filt) && wv %in% names(continuous_filt)) {
        source_df <- continuous_filt
      } else if (identical(source, "Model support") && is.data.frame(continuous_filt) &&
        wv %in% names(continuous_filt)) {
        source_df <- continuous_filt[continuous_filt$cal_year %in% unique(reg_filt$cal_year), , drop = FALSE]
      }
      brks <- stored_breaks[[wv]] %||% attr(weather_raw, "stored_breaks")[[wv]]
      if (!is.null(brks) && is.numeric(source_df[[wv]])) {
        bin_id <- cut(as.numeric(source_df[[wv]]), breaks = brks,
          include.lowest = TRUE, right = TRUE, labels = FALSE)
        vals <- rep(NA_character_, length(bin_id))
        keep_bin <- !is.na(bin_id) & bin_id >= 1L & bin_id <= length(lev)
        vals[keep_bin] <- lev[bin_id[keep_bin]]
      } else {
        vals <- as.character(source_df[[wv]])
      }
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (!length(vals)) next
      key <- paste(wv, source, sep = "\r")
      categories[[key]] <- lev
      proportions[[key]] <- as.numeric(table(factor(vals, levels = lev))) / length(vals)
    }
  }
  if (!length(proportions)) return(echart_blank("No binned weather values to plot.", height = height))
  e <- .e_diag_base(height)
  cat_keys <- names(categories)
  var_levels <- function(wv) {
    keys <- cat_keys[startsWith(cat_keys, paste0(wv, "\r"))]
    if (length(keys)) categories[[keys[[1L]]]] else character(0)
  }
  if (multi) {
    # One categorical axis keeps every selected variable visible in one widget.
    all_labels <- unname(unlist(lapply(weather_vars, function(wv) {
      lev <- var_levels(wv)
      paste(labels(wv), vapply(lev, .weather_display_bin_label, character(1L)), sep = ": ")
    }), use.names = FALSE))
  } else {
    wv <- weather_vars[[1L]]
    all_labels <- unname(vapply(var_levels(wv), .weather_display_bin_label, character(1L)))
  }
  e$x$opts$xAxis <- list(type = "category", data = as.list(all_labels),
    name = if (multi) "Weather variable and bin" else {
      spec <- .diag_weather_spec_row(weather_specs, weather_vars[[1L]])
      units <- if (!is.null(spec) && "units" %in% names(spec)) as.character(spec$units[[1L]]) else NULL
      .weather_display_axis_label(labels(weather_vars[[1L]]), units, binned = TRUE)
    },
    nameLocation = "middle", nameGap = 36, nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(interval = 0L, rotate = if (multi) 35 else 0),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    splitLine = wise_esplit_line(show = FALSE))
  e$x$opts$yAxis <- list(type = "value",
    axisLabel = wise_eaxis_label(formatter = htmlwidgets::JS("function(v){return (100*v).toFixed(0)+'%';}")),
    splitLine = wise_esplit_line())

  series <- list()
  for (source in unique(sub(".*\\r", "", cat_keys))) {
    dat <- numeric(length(all_labels))
    cursor <- 0L
    for (wv in weather_vars) {
      lev <- var_levels(wv)
      key <- paste(wv, source, sep = "\r")
      value <- proportions[[key]] %||% rep(0, length(lev))
      if (multi) dat[cursor + seq_along(lev)] <- value
      else if (identical(wv, weather_vars[[1L]])) dat <- value
      cursor <- cursor + length(lev)
    }
    series[[length(series) + 1L]] <- list(name = source, type = "bar",
      barMaxWidth = 24, itemStyle = list(color = colors(source), opacity = 0.72),
      data = as.list(unname(dat)))
  }
  e$x$opts$series <- unname(series)
  e$x$opts$legend <- modifyList(list(top = 4, left = "center", orient = "horizontal",
    data = as.list(unname(unique(vapply(series, `[[`, character(1L), "name"))))), wise_elegend_style())
  e$x$opts$tooltip <- list(
    trigger = "axis", confine = TRUE, axisPointer = list(type = "shadow"),
    formatter = htmlwidgets::JS(
      "function(params){
        function esc(s){return String(s).replace(/[&<>\"']/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c];});}
        var rows=[];
        (params||[]).forEach(function(p){
          var v=Number(Array.isArray(p.value)?p.value[1]:p.value);
          if(!isFinite(v)) return;
          rows.push((p.marker||'')+esc(p.seriesName)+': <b>'+(100*v).toLocaleString('en-US',{maximumFractionDigits:1})+'%</b>');
        });
        return (params&&params.length?esc(params[0].axisValueLabel):'')+(rows.length?'<br/>'+rows.join('<br/>'):'');
      }"
    )
  )
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 16, top = 42, bottom = if (multi) 84 else 72)
  wise_echart_theme(e)
}
