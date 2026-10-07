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
#   .kde_group()                 -- compute KDE for one group (internal)       #
#   build_ridge_kde_data()       -- pre-compute all KDE data (exported)        #


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


# Weather input density data ----
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
  refs <- weather_support_reference(
    regression_weather, weather_vars, lower, upper, weather_specs
  )
  scenarios <- scenario_weather %||% list()
  rows <- lapply(refs, function(r) {
    dplyr::bind_rows(lapply(names(scenarios), function(nm) {
      weather_support_scenario(r, scenarios[[nm]][[r$variable]], nm, warn_share)
    }))
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# Reference support of each weather variable: the robust (default 1%-99%)
# interval of the historical values, or the set of supported bins. Computed
# once so many scenarios can be checked without re-ranking the reference.
weather_support_reference <- function(regression_weather, weather_vars,
                                      lower = 0.01, upper = 0.99,
                                      weather_specs = NULL) {
  if (is.null(regression_weather) || !is.data.frame(regression_weather)) {
    return(list())
  }
  refs <- lapply(intersect(weather_vars, names(regression_weather)), function(v) {
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
      list(
        variable = v, is_binned = TRUE, n_reference = length(ref),
        supported = supported, reference_label = paste(supported, collapse = ", "),
        robust = c(NA_real_, NA_real_), lower = lower, upper = upper
      )
    } else {
      ref <- suppressWarnings(as.numeric(regression_weather[[v]]))
      ref <- ref[is.finite(ref)]
      if (!length(ref)) {
        return(NULL)
      }
      list(
        variable = v, is_binned = FALSE, n_reference = length(ref),
        supported = NULL, reference_label = NA_character_,
        robust = as.numeric(stats::quantile(ref, c(lower, upper),
          names = FALSE, na.rm = TRUE, type = 8
        )),
        lower = lower, upper = upper
      )
    }
  })
  Filter(Negate(is.null), refs)
}

# Share of one scenario's values for one variable outside its reference
# support. NULL when the scenario has no usable values.
weather_support_scenario <- function(ref, x_raw, scenario, warn_share = 0.05) {
  x <- if (ref$is_binned) {
    x <- as.character(x_raw)
    x[!is.na(x) & nzchar(x)]
  } else {
    x <- suppressWarnings(as.numeric(x_raw))
    x[is.finite(x)]
  }
  if (!length(x)) {
    return(NULL)
  }
  outside <- if (ref$is_binned) {
    !(x %in% ref$supported)
  } else {
    x < ref$robust[[1L]] | x > ref$robust[[2L]]
  }
  data.frame(
    weather_variable = ref$variable, scenario = scenario,
    n_reference = ref$n_reference, n_scenario = length(x),
    robust_lo = ref$robust[[1L]], robust_hi = ref$robust[[2L]],
    reference_label = ref$reference_label,
    is_binned = ref$is_binned,
    outside_n = sum(outside), outside_share = mean(outside),
    warning = mean(outside) > warn_share,
    warning_rule = if (ref$is_binned) {
      paste0("Reference bins; warn above ", warn_share * 100, "% outside")
    } else {
      paste0(
        "Robust ", ref$lower * 100, "%-", ref$upper * 100,
        "% reference interval; warn above ", warn_share * 100, "% outside"
      )
    },
    stringsAsFactors = FALSE
  )
}

# Weather-support table for the Diagnostics tab from the summaries stored on
# the scenario entries during the run (pooled across climate-model members).
# NULL when any visible scenario has no stored summary (a run made before the
# summary existed), so the caller can fall back to recomputing from weather.
weather_support_from_stored <- function(scenarios, vars, visible = names(scenarios)) {
  visible <- intersect(visible, names(scenarios))
  if (!length(visible)) {
    return(NULL)
  }
  tbls <- lapply(scenarios[visible], function(e) e[["weather_support"]])
  if (!all(vapply(tbls, is.data.frame, logical(1L)))) {
    return(NULL)
  }
  out <- dplyr::bind_rows(tbls)
  out <- out[out$weather_variable %in% vars, , drop = FALSE]
  out[order(match(out$weather_variable, vars), match(out$scenario, visible)), ,
    drop = FALSE]
}

# Pool member-level support rows (one per climate-model member) into one row
# per weather variable for a scenario: counts add, the share is recomputed.
weather_support_pool <- function(rows, scenario, warn_share = 0.05) {
  rows <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(rows)) {
    return(NULL)
  }
  pooled <- lapply(split(rows, rows$weather_variable), function(d) {
    out <- d[1L, , drop = FALSE]
    out$scenario <- scenario
    out$n_scenario <- sum(d$n_scenario)
    out$outside_n <- sum(d$outside_n)
    out$outside_share <- out$outside_n / out$n_scenario
    out$warning <- out$outside_share > warn_share
    out$n_members <- nrow(d)
    out$max_member_share <- max(d$outside_share)
    out
  })
  dplyr::bind_rows(pooled)
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
#' groups for ridge diagnostics to consume.
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



# ============================================================================
# Interactive (echarts4r) weather-density diagnostic panel (guidelines §7).
#
# Design (documented judgment call): the patchwork multi-variable panel is
# collapsed into ONE echarts widget. With a single selected variable the
# widget uses the Step 1 ridge computation and value/share tooltips, with one
# row per source. Binned variables use percentage bars and Step 1 bin labels.
# With several continuous variables the curves become a ridgeline: each
# variable's densities are normalised and offset vertically on its own row,
# so one interactive widget replaces the patchwork without re-rendering
# per-variable plots. Densities use bw = "nrd0", a gaussian kernel, and n = 512.
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
