# ============================================================================ #
# fct_sim_diag.R                                                               #
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
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Internal helpers                                                             #
# ---------------------------------------------------------------------------- #

# Derive int_month from timestamp if not already present.
#' @noRd
.add_int_month <- function(df) {
  if ("int_month" %in% names(df)) return(df)
  if ("timestamp" %in% names(df))
    df$int_month <- as.integer(format(as.Date(df$timestamp), "%m"))
  df
}


# Filter weather_raw to loc_id x int_month cells present in survey_weather.
# Derives cal_year from timestamp (never from the survey-round `year` join key).
# Deduplicates to one row per loc_id x int_month x cal_year.
#' @noRd
.filter_hist_weather <- function(weather_raw, survey_weather) {
  wr <- .add_int_month(weather_raw)
  wr$cal_year <- as.integer(format(as.Date(wr$timestamp), "%Y"))
  if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather))
    survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
  sw_cells <- unique(survey_weather[, c("loc_id", "int_month")])
  wr        <- merge(wr, sw_cells, by = c("loc_id", "int_month"))
  wr[!duplicated(wr[, c("loc_id", "int_month", "cal_year")]), ]
}


# ---------------------------------------------------------------------------- #
# Weather input density panel                                                  #
# ---------------------------------------------------------------------------- #

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
#' @noRd
.plot_density_one <- function(survey_weather,
                              weather_raw,
                              weather_var,
                              weather_label    = NULL,
                              scenario_weather = NULL,
                              active_scenarios = NULL,
                              log_x            = FALSE,
                              show_legend      = TRUE,
                              show_regression  = FALSE) {

  disp_label <- weather_label %||% weather_var

  if (!weather_var %in% names(weather_raw))
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("'", disp_label, "' not found.")))

  hist_filt <- .filter_hist_weather(weather_raw, survey_weather)

  if (!"int_month" %in% names(survey_weather) && "timestamp" %in% names(survey_weather))
    survey_weather$int_month <- as.integer(format(as.Date(survey_weather$timestamp), "%m"))
  if ("timestamp" %in% names(survey_weather))
    survey_weather$cal_year <- as.integer(format(as.Date(survey_weather$timestamp), "%Y"))
  sw_years  <- unique(survey_weather$cal_year)
  reg_filt  <- hist_filt[hist_filt$cal_year %in% sw_years, ]

  hist_vals <- as.numeric(hist_filt[[weather_var]])
  hist_vals <- hist_vals[is.finite(hist_vals)]
  reg_vals  <- if (isTRUE(show_regression)) {
    v <- as.numeric(reg_filt[[weather_var]])
    v[is.finite(v)]
  } else numeric(0)

  if (length(hist_vals) == 0)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No finite values to plot."))

  # ---- SSP scenario overlays -------------------------------------------
  ssp_colour_map   <- character(0)
  ssp_linetype_map <- character(0)
  ssp_df_list      <- list()

  if (!is.null(scenario_weather) && length(scenario_weather) > 0) {
    visible_nms <- names(scenario_weather)
    if (!is.null(active_scenarios))
      visible_nms <- intersect(visible_nms, active_scenarios)

    yr_of <- function(nm) {
      m <- regmatches(nm, regexpr("[0-9]{4}(?= \u00b1)", nm, perl = TRUE))
      if (length(m) == 0L) NA_integer_ else as.integer(m)
    }
    yrs_visible    <- vapply(visible_nms, yr_of, integer(1))
    unique_yrs     <- sort(unique(yrs_visible[!is.na(yrs_visible)]))
    yr_lty_palette <- c("solid", "dashed", "dotted", "dotdash", "longdash")
    yr_lty_map     <- setNames(
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
      col     <- if (!is.na(ssp_key) && ssp_key %in% names(.ssp_colours))
        .ssp_colours[ssp_key] else "#cccccc"
      yr_chr  <- as.character(yr_of(scen_nm))
      lty     <- if (!is.na(yr_chr) && yr_chr %in% names(yr_lty_map))
        yr_lty_map[yr_chr] else "solid"
      ssp_colour_map[scen_nm]   <- col
      ssp_linetype_map[scen_nm] <- lty
      ssp_df_list[[scen_nm]]    <- data.frame(
        value  = vals,
        source = scen_nm,
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- Colour map: Full historical anchor + regression + scenarios ------
  colour_map   <- ssp_colour_map
  linetype_map <- ssp_linetype_map
  if (isTRUE(show_regression)) {
    colour_map["Regression input"]   <- "black"
    linetype_map["Regression input"] <- "dashed"
  }

  # ---- Build plot -------------------------------------------------------
  p <- ggplot2::ggplot()

  # Full historical: grey fill + colour mapped for legend swatch
  p <- p + ggplot2::geom_density(
    data      = data.frame(value = hist_vals, stringsAsFactors = FALSE),
    mapping   = ggplot2::aes(x = .data$value, fill = "Full historical"),
    colour    = NA,
    alpha     = 0.35,
    linewidth = 0.7
  )

  # Regression input: black dashed line, no fill (only when show_regression)
  if (isTRUE(show_regression) && length(reg_vals) > 0) {
    p <- p + ggplot2::geom_density(
      data      = data.frame(value = reg_vals, source = "Regression input",
                             stringsAsFactors = FALSE),
      mapping   = ggplot2::aes(x = .data$value, colour = .data$source),
      fill      = NA,
      linetype  = "dashed",
      linewidth = 0.9
    )
  }

  # SSP scenario overlay lines
  for (scen_nm in names(ssp_df_list)) {
    p <- p + ggplot2::geom_density(
      data      = ssp_df_list[[scen_nm]],
      ggplot2::aes(x = .data$value, colour = .data$source),
      fill      = NA,
      linetype  = ssp_linetype_map[[scen_nm]],
      linewidth = 0.8
    )
  }

  # Dual-scale legend:
  #   scale_fill_manual  -- grey swatch for Full historical
  #   scale_colour_manual -- dashed/coloured lines for Regression + SSPs
  # Legend: fill swatch for Full historical; colour/linetype for lines.
  # override.aes is intentionally avoided -- ggplot2 builds the correct
  # grey fill swatch automatically from the fill = src mapping.
  fill_map_all <- c("Full historical" = "#808080")

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
      title = disp_label,
      subtitle = paste0(
        "Hist: ", length(hist_vals), " cells",
        if (isTRUE(show_regression) && length(reg_vals) > 0)
          paste0("  |  Reg: ", length(reg_vals), " cells") else "",
        if (n_scen_shown > 0) paste0("  |  Scen: ", n_scen_shown) else ""
      ),
      x = disp_label,
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = if (show_legend) "bottom" else "none",
      plot.subtitle   = ggplot2::element_text(size = 9, colour = "grey40"),
      plot.title      = ggplot2::element_text(size = 11, face = "bold")
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
                                       weather_labels   = NULL,
                                       scenario_weather = NULL,
                                       active_scenarios = NULL,
                                       log_x            = FALSE,
                                       show_regression  = FALSE) {

  stopifnot(
    is.data.frame(survey_weather),
    is.data.frame(weather_raw),
    is.character(weather_vars), length(weather_vars) >= 1
  )

  weather_vars <- intersect(weather_vars, names(weather_raw))
  if (length(weather_vars) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "No selected weather variables found in weather_raw."))

  # Vectorise log_x to one value per variable
  if (length(log_x) == 1L) log_x <- rep(log_x, length(weather_vars))

  panels <- lapply(seq_along(weather_vars), function(i) {
    wv  <- weather_vars[[i]]
    lbl <- if (!is.null(weather_labels) && wv %in% names(weather_labels))
      weather_labels[[wv]] else wv
    .plot_density_one(
      survey_weather   = survey_weather,
      weather_raw      = weather_raw,
      weather_var      = wv,
      weather_label    = lbl,
      scenario_weather = scenario_weather,
      active_scenarios = active_scenarios,
      log_x            = isTRUE(log_x[[i]]),
      show_legend      = TRUE,
      show_regression  = isTRUE(show_regression)
    )
  })

  if (length(panels) == 1L) return(panels[[1L]])

  patchwork::wrap_plots(panels, nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------- #
# Year-anchored welfare ridge plot                                             #
# ---------------------------------------------------------------------------- #

# Compute KDE for one group using supplied bandwidth and x-range.
# Returns a data.frame(x, density_raw) on the shared n-point grid.
#' @noRd
.kde_group <- function(vals, bw, x_lo, x_hi, n = 512L) {
  if (length(vals) < 2L) {
    return(data.frame(x           = seq(x_lo, x_hi, length.out = n),
                      density_raw = 0))
  }
  d <- stats::density(vals, bw = bw, from = x_lo, to = x_hi, n = n)
  data.frame(x = d$x, density_raw = d$y)
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
                                 log_scale   = FALSE) {

  if (is.null(hist_preds) || !outcome_name %in% names(hist_preds))
    return(NULL)

  yr_col <- intersect(c("sim_year", "year"), names(hist_preds))[1]
  if (is.na(yr_col)) return(NULL)

  hist_preds$.__yr <- as.integer(as.character(hist_preds[[yr_col]]))
  sim_years        <- sort(unique(hist_preds$.__yr[!is.na(hist_preds$.__yr)]))

  hist_groups <- lapply(sim_years, function(yr) {
    vals <- as.numeric(hist_preds[[outcome_name]][hist_preds$.__yr == yr])
    vals[is.finite(vals)]
  })
  names(hist_groups) <- as.character(sim_years)
  hist_groups        <- Filter(function(v) length(v) >= 2L, hist_groups)
  if (length(hist_groups) == 0L) return(NULL)

  scenario_groups <- list()
  yr_col_fut      <- yr_col

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
      if (is.null(scenario_groups[[scen_nm]]))
        scenario_groups[[scen_nm]] <- list()
      scenario_groups[[scen_nm]][[as.character(yr)]] <- vals
    }
  }

  all_vals <- c(
    unlist(hist_groups,     use.names = FALSE),
    unlist(scenario_groups, use.names = FALSE)
  )
  all_vals <- all_vals[is.finite(all_vals)]
  # BW and P1-P99 clip always in linear space; log display via scale_x_log10().
  global_bw <- tryCatch(
    stats::bw.nrd0(all_vals),
    error = function(e)
      stats::bw.nrd0(all_vals[seq_len(min(1000L, length(all_vals)))])
  )

  qs <- stats::quantile(all_vals, probs = c(0.01, 0.99), na.rm = TRUE)

  # Regression output:
  #   predicted = clean model fitted values (.fitted column, no residual noise)
  #              falls back to outcome_name column if .fitted absent
  #   actual    = observed survey outcome from train_data (passed in as actual_vals)
  #               back-transformed from log-scale when so$transform == "log"
  # predicted = unique back-transformed outcome values (outcome_name col is
  # already back-transformed by apply_log_backtransform(); .fitted is NOT).
  # Deduplicate across draws so bandwidth is estimated at training-sample scale.
  predicted_vals <- tryCatch({
    v <- unique(as.numeric(hist_preds[[outcome_name]]))
    v[is.finite(v)]
  }, error = function(e) numeric(0))

  actual_vals_clean <- tryCatch({
    if (is.null(actual_vals)) return(numeric(0))
    v <- as.numeric(actual_vals)
    v[is.finite(v)]
  }, error = function(e) numeric(0))

  # Separate bandwidth for regression curves: estimated from the regression
  # sample alone so it is not dominated by the 30yr x N hist_groups pool.
  reg_bw <- if (length(predicted_vals) >= 2L) {
    tryCatch(stats::bw.nrd0(predicted_vals[is.finite(predicted_vals)]),
             error = function(e) global_bw)
  } else global_bw

  scen_nms     <- names(scenario_groups)
  ssp_keys     <- vapply(scen_nms, .normalise_ssp, character(1))
  fore_yrs_raw <- vapply(scen_nms, function(nm) {
    m <- regmatches(nm, regexpr("[0-9]{4}(?= \u00b1)", nm, perl = TRUE))
    if (length(m) == 0L) NA_integer_ else as.integer(m)
  }, integer(1))

  list(
    hist_groups     = hist_groups,
    scenario_groups = scenario_groups,
    global_bw       = global_bw,
    x_lo_tr         = qs[[1L]],
    x_hi_tr         = qs[[2L]],
    scen_nms        = scen_nms,
    ssp_keys        = ssp_keys,
    fore_yrs_raw    = fore_yrs_raw,
    sim_years       = sim_years,
    log_scale       = log_scale,
    outcome_name    = outcome_name,
    predicted_vals  = predicted_vals,
    actual_vals     = actual_vals_clean,
    reg_bw          = reg_bw
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
#' @param kde_data      List. Output of \code{build_ridge_kde_data()}.
#' @param x_label       Character scalar. X-axis label.
#' @param primary_group Character. \code{"hist_year"} (default) or
#'   \code{"scenario"}.
#' @param log_scale     Logical. Log10 x-axis. Overrides kde_data$log_scale.
#'   Default NULL (inherit from kde_data).
#' @param ridge_scale   Numeric. Vertical height multiplier. Default 1.5.
#' @param row_gap       Numeric. Spacing between rows. Default 1.0.
#' @param show_regression Logical. When TRUE, overlays regression sample
#'   predicted (black dashed) and actual outcome (black dotted) density
#'   curves from hist_preds training rows. Default FALSE.
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
                                     log_scale     = NULL,
                                     ridge_scale   = 1.5,
                                     row_gap         = 1.0,
                                     show_regression = FALSE) {

  if (is.null(kde_data))
    return(ggplot2::ggplot() + ggplot2::labs(title = "No simulation data available."))

  use_log <- if (!is.null(log_scale)) isTRUE(log_scale) else isTRUE(kde_data$log_scale)

  hist_groups     <- kde_data$hist_groups
  scenario_groups <- kde_data$scenario_groups
  global_bw       <- kde_data$global_bw
  x_lo_tr         <- kde_data$x_lo_tr
  x_hi_tr         <- kde_data$x_hi_tr
  scen_nms        <- kde_data$scen_nms
  ssp_keys        <- kde_data$ssp_keys
  fore_yrs_raw    <- kde_data$fore_yrs_raw
  predicted_vals  <- kde_data$predicted_vals  %||% numeric(0)
  actual_vals     <- kde_data$actual_vals     %||% numeric(0)


  hist_fill    <- "#d0d0d0"
  hist_colour  <- "#333333"
  hist_alpha   <- 0.55

  yr_lty_palette  <- c("solid", "dashed", "dotted", "longdash", "twodash")
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

  .run_kde <- function(vals) {
    kv <- vals[is.finite(vals)]
    kd <- .kde_group(kv, global_bw, x_lo_tr, x_hi_tr)
    dns  <- kd$density_raw
    dns  <- dns / max(dns[is.finite(dns) & dns > 0], 1e-12)  # normalise peak to 1
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
    scen_lines   <- list()

    for (yr_chr in names(hist_groups)) {
      y_anch <- yr_rank[yr_chr]
      kd     <- .run_kde(hist_groups[[yr_chr]])

      hist_ribbons[[yr_chr]] <- data.frame(
        x         = kd$x,
        ymin      = y_anch,
        ymax      = y_anch + kd$density_raw * ridge_scale,
        row.names = NULL,
        stringsAsFactors = FALSE
      )

      active_sub <- Filter(
        function(nm) !is.null(scenario_groups[[nm]][[yr_chr]]),
        scen_nms
      )
      for (nm in active_sub) {
        kd2 <- .run_kde(scenario_groups[[nm]][[yr_chr]])
        scen_lines[[paste0(nm, "__", yr_chr)]] <- data.frame(
          x         = kd2$x,
          y         = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col  = scen_colour_map[nm],
          lty       = scen_lty_map[nm],
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(yr_rank)
    y_labels <- paste0("  ", names(yr_rank))
    subtitle  <- paste0(
      "Primary: historical year.  Grey fill = base year.  ",
      "Coloured lines = scenario perturbations at same baseline.  ",
      "X clipped P1\u2013P99."
    )

  # ==================================================================== #
  # Mode B: scenario primary                                              #
  # ==================================================================== #
  } else if (primary_group == "scenario") {

    # scenario mode: sort row_keys by SSP then forecast year
    row_keys <- if (length(scen_nms) > 0)
      scen_nms[order(ssp_keys[scen_nms], fore_yrs_raw[scen_nms])]
    else scen_nms
    if (length(row_keys) == 0L)
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "No scenario data. Run future scenarios first."))

    row_rank <- setNames(seq_len(length(row_keys)) * row_gap, row_keys)

    yr_chrs   <- names(hist_groups)
    n_yrs     <- length(yr_chrs)
    grey_vals <- seq(0.75, 0.15, length.out = max(n_yrs, 1L))
    yr_grey   <- setNames(grDevices::grey(grey_vals), yr_chrs)

    hist_ribbons <- list()
    scen_lines   <- list()

    for (scen_nm in row_keys) {
      y_anch        <- row_rank[scen_nm]
      all_scen_vals <- unlist(scenario_groups[[scen_nm]], use.names = FALSE)
      all_scen_vals <- all_scen_vals[is.finite(all_scen_vals)]
      if (length(all_scen_vals) >= 2L) {
        kd_base <- .run_kde(all_scen_vals)
        hist_ribbons[[scen_nm]] <- data.frame(
          x         = kd_base$x,
          ymin      = y_anch,
          ymax      = y_anch + kd_base$density_raw * ridge_scale,
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
      for (yr_chr in yr_chrs) {
        yr_vals <- scenario_groups[[scen_nm]][[yr_chr]]
        if (is.null(yr_vals) || length(yr_vals) < 2L) next
        kd2 <- .run_kde(yr_vals)
        scen_lines[[paste0(scen_nm, "__", yr_chr)]] <- data.frame(
          x = kd2$x, y = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col = yr_grey[yr_chr], lty = "solid",
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(row_rank)
    y_labels  <- paste0("  ", names(row_rank))
    subtitle  <- paste0(
      "Primary: scenario × forecast year.  ",
      "Fill = pooled scenario distribution.  ",
      "Grey lines = individual historical years (darkest = most recent).  ",
      "X clipped P1–P99."
    )

  } else {

    # forecast_yr mode: sort row_keys by forecast year then SSP
    row_keys <- if (length(scen_nms) > 0)
      scen_nms[order(fore_yrs_raw[scen_nms], ssp_keys[scen_nms])]
    else scen_nms
    if (length(row_keys) == 0L)
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "No scenario data. Run future scenarios first."))

    row_rank <- setNames(seq_len(length(row_keys)) * row_gap, row_keys)

    yr_chrs   <- names(hist_groups)
    n_yrs     <- length(yr_chrs)
    grey_vals <- seq(0.75, 0.15, length.out = max(n_yrs, 1L))
    yr_grey   <- setNames(grDevices::grey(grey_vals), yr_chrs)

    hist_ribbons <- list()
    scen_lines   <- list()

    for (scen_nm in row_keys) {
      y_anch        <- row_rank[scen_nm]
      all_scen_vals <- unlist(scenario_groups[[scen_nm]], use.names = FALSE)
      all_scen_vals <- all_scen_vals[is.finite(all_scen_vals)]
      if (length(all_scen_vals) >= 2L) {
        kd_base <- .run_kde(all_scen_vals)
        hist_ribbons[[scen_nm]] <- data.frame(
          x         = kd_base$x,
          ymin      = y_anch,
          ymax      = y_anch + kd_base$density_raw * ridge_scale,
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
      for (yr_chr in yr_chrs) {
        yr_vals <- scenario_groups[[scen_nm]][[yr_chr]]
        if (is.null(yr_vals) || length(yr_vals) < 2L) next
        kd2 <- .run_kde(yr_vals)
        scen_lines[[paste0(scen_nm, "__", yr_chr)]] <- data.frame(
          x = kd2$x, y = y_anch + kd2$density_raw * ridge_scale * 0.85,
          line_col = yr_grey[yr_chr], lty = "solid",
          row.names = NULL, stringsAsFactors = FALSE
        )
      }
    }

    y_breaks <- as.numeric(row_rank)
    y_labels  <- paste0("  ", names(row_rank))
    subtitle  <- paste0(
      "Primary: forecast year × scenario.  ",
      "Fill = pooled scenario distribution.  ",
      "Grey lines = individual historical years (darkest = most recent).  ",
      "X clipped P1–P99."
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

  # -- Regression output overlay (when show_regression = TRUE) ----------
  # Two curves at the same y_reg baseline:
  #   predicted_vals  -- dashed black line  (simulated outcome distribution)
  #   actual_vals     -- dotted black line  (observed survey outcomes)
  has_predicted <- isTRUE(show_regression) && length(predicted_vals) >= 2L
  has_actual    <- isTRUE(show_regression) && length(actual_vals)    >= 2L

  if (has_predicted || has_actual) {
    rank_vec <- tryCatch(yr_rank, error = function(e)
                  tryCatch(row_rank, error = function(e2) c(1)))
    y_reg <- min(as.numeric(rank_vec)) - row_gap * 0.8

    if (has_predicted) {
      reg_bw_use  <- kde_data$reg_bw %||% global_bw
      kd_pred_raw <- .kde_group(predicted_vals[is.finite(predicted_vals)],
                                reg_bw_use, x_lo_tr, x_hi_tr)
      pred_dns    <- kd_pred_raw$density_raw
      pred_dns    <- pred_dns / max(pred_dns[is.finite(pred_dns) & pred_dns > 0], 1e-12)
      pred_df <- data.frame(
        x    = kd_pred_raw$x,
        y    = y_reg + pred_dns * ridge_scale * 0.85,
        ymin = y_reg,
        stringsAsFactors = FALSE
      )
      p <- p +
        ggplot2::geom_ribbon(
          data    = pred_df,
          mapping = ggplot2::aes(x = .data$x, ymin = .data$ymin,
                                 ymax = .data$y),
          fill    = "#cccccc",
          alpha   = 0.25,
          colour  = NA
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
      reg_bw_use  <- kde_data$reg_bw %||% global_bw
      act_vals_fi <- actual_vals[is.finite(actual_vals)]
      # Use reg_bw from predicted_vals since both are on the same scale
      kd_act_raw  <- .kde_group(act_vals_fi, reg_bw_use, x_lo_tr, x_hi_tr)
      act_dns     <- kd_act_raw$density_raw
      act_dns     <- act_dns / max(act_dns[is.finite(act_dns) & act_dns > 0], 1e-12)
      act_df <- data.frame(
        x    = kd_act_raw$x,
        y    = y_reg + act_dns * ridge_scale * 0.85,
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
    ssp_disp     <- sort(unique(ssp_keys[!is.na(ssp_keys)]))
    ssp_col_vals <- vapply(ssp_disp, function(k) {
      if (k %in% names(.ssp_colours)) .ssp_colours[k] else "#aaaaaa"
    }, character(1))

    if (length(ssp_disp) > 0) {
      p <- p +
        ggplot2::geom_point(
          data    = data.frame(x       = rep(NA_real_, length(ssp_disp)),
                               y       = rep(NA_real_, length(ssp_disp)),
                               ssp_key = ssp_disp,
                               stringsAsFactors = FALSE),
          mapping = ggplot2::aes(x = .data$x, y = .data$y, colour = .data$ssp_key),
          size    = 0,
          na.rm   = TRUE
        ) +
        ggplot2::scale_colour_manual(
          name   = "Climate scenario",
          values = setNames(ssp_col_vals, ssp_disp),
          guide  = ggplot2::guide_legend(
            override.aes = list(linewidth = 2.5, linetype = "solid", size = 4)
          )
        )
    }

    if (length(fore_yr_lty_map) > 0) {
      p <- p +
        ggplot2::geom_line(
          data    = data.frame(x        = rep(NA_real_, length(fore_yr_lty_map)),
                               y        = rep(NA_real_, length(fore_yr_lty_map)),
                               fore_lbl = names(fore_yr_lty_map),
                               stringsAsFactors = FALSE),
          mapping = ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$fore_lbl),
          colour  = "#555555",
          na.rm   = TRUE
        ) +
        ggplot2::scale_linetype_manual(
          name   = "Forecast year",
          values = fore_yr_lty_map,
          guide  = ggplot2::guide_legend(
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
    ggplot2::theme_minimal(base_size = 12) +
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
