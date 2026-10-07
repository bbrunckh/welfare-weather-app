# Archived static ggplot renderers.
#
# These definitions are retained for development/reference use only and are
# not part of the installed package runtime. Data preparation and echarts
# counterparts remain in their original R/ files.

#' Grouped Point-Range Chart Comparing Scenarios
#'
#' The primary Results tab chart. For each scenario shows:
#'   - A dot at the mean of annual simulated values
#'   - A thick coloured bar for the calculated weather variation
#'   - A thin line for coefficient uncertainty (user-selected band)
#'   - A dashed grey horizontal reference line at the Historical mean
#'
#' Groups are ordered Historical | spacer | SSP2 years | spacer | SSP3 years |
#' spacer | SSP5 years. Colour follows SSP family; shade follows year rank.
#'
#' @param bands_tbl  Data frame of per-scenario/per-year aggregates with a
#'   central value and band columns (from the Step 2 band assembly).
#' @param x_label   Scalar character y-axis label (outcome units).
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"} x-axis ordering.
#' @param show_coef Logical. Show the thin coefficient-uncertainty line.
#'   Default TRUE.
#' @param show_annual Logical. Show the inter-annual interval. The decision-
#'   first results view leaves this off because annual variation is shown in a
#'   separate distribution plot.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_x_discrete labs theme_minimal theme
#'   element_blank element_text margin
#' @importFrom colorspace lighten
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @noRd
plot_pointrange_climate <- function(bands_tbl,
                                    x_label = "",
                                    group_order = "scenario_x_year",
                                    show_coef = TRUE,
                                    show_annual = FALSE) {
  if (is.null(bands_tbl) || nrow(bands_tbl) == 0L) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }

  prep <- .pointrange_prep(bands_tbl, group_order)
  df <- prep$df
  has_source <- prep$has_source
  colour_palette <- prep$palette
  ordered_levels <- prep$ordered_levels
  x_label_map <- prep$x_label_map

  if (nrow(df) == 0L) {
    return(blank_plot("Run a future simulation to see scenario comparisons."))
  }

  # plot: nested bands + dot ----
  # When a `source` column is present we dodge Baseline vs Policy side-by-side
  # within each scenario. Otherwise (Mod 2) the chart is single-source and
  # the dodge collapses to no-op via a single-level factor.
  pos <- if (has_source) {
    ggplot2::position_dodge(width = 0.55)
  } else {
    ggplot2::position_identity()
  }
  aes_base <- if (has_source) {
    ggplot2::aes(
      x = .data$pt_key, colour = .data$colour_key,
      group = .data$source
    )
  } else {
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key)
  }
  p <- ggplot2::ggplot(df, aes_base)

  p <- p + ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$intermod_lo, ymax = .data$intermod_hi),
    linewidth = 6.0, alpha = 0.6, na.rm = TRUE, position = pos
  )

  if (isTRUE(show_annual)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$interann_lo, ymax = .data$interann_hi),
      linewidth = 3.5, alpha = 1.0, na.rm = TRUE, position = pos
    )
  }

  if (isTRUE(show_coef)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$coef_lo, ymax = .data$coef_hi),
      linewidth = 1.2, colour = .wise_support, na.rm = TRUE, position = pos
    )
  }

  if (has_source) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        y = .data$value, shape = .data$source,
        fill = .data$source
      ),
      size = 3, stroke = 1.2, colour = .wise_support, na.rm = TRUE, position = pos
    ) +
      ggplot2::scale_shape_manual(
        values = c(Baseline = 21, Policy = 23), name = NULL, drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(Baseline = "white", Policy = .wise_policy),
        name = NULL, drop = FALSE
      )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      size = 3, shape = 21, fill = "white", colour = .wise_support, na.rm = TRUE
    )
  }

  p +
    ggplot2::scale_colour_manual(values = colour_palette, guide = "none") +
    ggplot2::scale_x_discrete(
      limits = ordered_levels,
      labels = x_label_map, drop = FALSE
    ) +
    ggplot2::labs(title = NULL, x = NULL, y = x_label) +
    theme_wise() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position    = if (has_source) "top" else "none"
    )
}

#' Plot paired baseline-to-policy effects
#'
#' @param tbl Scenario-level paired effect estimates and uncertainty bounds.
#' @param x_label Outcome-axis label.
#' @return A ggplot object.
paired_effect_plot <- function(tbl, x_label = "Policy effect (outcome units)") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Paired policy effects are unavailable."))
  }
  tbl$scenario <- factor(tbl$scenario, levels = rev(unique(tbl$scenario)))
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$value, y = .data$scenario)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$intermod_lo,
        xend = .data$intermod_hi,
        y = .data$scenario, yend = .data$scenario
      ),
      linewidth = 5, colour = .wise_policy, alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$coef_lo,
        xend = .data$coef_hi,
        y = .data$scenario, yend = .data$scenario
      ),
      linewidth = 1.2, colour = .wise_support, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      shape = 21, size = 3.2, fill = .wise_policy,
      colour = .wise_policy_dark, stroke = 0.8, na.rm = TRUE
    ) +
    ggplot2::labs(
      x = x_label, y = NULL,
      subtitle = paste(
        if ("center_method" %in% names(tbl) &&
          all(tbl$center_method == "equal_model_mean")) "Equal-model mean." else "Median across climate-model means.",
        "Thick interval = ensemble spread; thin interval = coefficient uncertainty (baseline-X approximation)."
      )
    ) +
    theme_wise()
}

#' Plot annual outcome distributions by scenario
#'
#' Displays weather-year outcomes as horizontal violin or boxplot rows.
#'
#' @param tbl Annual scenario outcomes, optionally including source groups.
#' @param x_label Outcome-axis label.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param plot_type Distribution geometry: `"violin"` or `"boxplot"`.
#' @return A ggplot object.
plot_annual_distribution <- function(tbl, x_label = "Outcome (outcome units)",
                                     title = NULL, subtitle = NULL,
                                     plot_type = "violin") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("No annual simulation results available."))
  }
  df <- tbl
  df$scenario <- as.character(df$scenario)
  df$period <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .parse_year, character(1L))
  )
  df$ssp <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .normalise_ssp, character(1L))
  )
  scenario_levels <- c("Historical", sort(unique(df$scenario[df$scenario != "Historical"])))
  scenario_palette <- c(Historical = .wise_history)
  for (ssp in unique(df$ssp[df$ssp != "Historical"])) {
    members <- scenario_levels[scenario_levels != "Historical"]
    members <- members[vapply(members, function(s) {
      identical(.normalise_ssp(s), ssp)
    }, logical(1L))]
    members <- members[order(vapply(members, .parse_year, character(1L)))]
    base_col <- if (ssp %in% names(.ssp_colours)) {
      unname(.ssp_colours[[ssp]])
    } else {
      "#0072B2"
    }
    shades <- if (length(members) > 1L) {
      colorspace::lighten(base_col, seq(0.30, 0, length.out = length(members)))
    } else {
      base_col
    }
    scenario_palette[members] <- shades
  }
  df$scenario_key <- factor(df$scenario, levels = scenario_levels)

  has_source <- "source" %in% names(df) && length(unique(df$source)) > 1L
  plot_type <- match.arg(plot_type, c("violin", "boxplot"))

  # Reference line: the historical *baseline* mean. With both sources
  # present, pooling them would average baseline and policy outcomes.
  hist_vals <- if (has_source) {
    h <- df$value[df$scenario == "Historical" & df$source == "Baseline"]
    if (!length(h)) df$value[df$scenario == "Historical"] else h
  } else {
    df$value[df$scenario == "Historical"]
  }
  hist_mean <- if (length(hist_vals)) mean(hist_vals, na.rm = TRUE) else NA_real_

  # Horizontal Design-B/D layout: one row per scenario on a numeric y (so the
  # row banding lines up), Historical on top, and a single shared outcome
  # axis. The Violin/Boxplot toggle picks the row rendering: full violins
  # over dots (Design B) or slim boxes over faded dots (Design D).
  n_rows <- length(scenario_levels)
  df$row_y <- n_rows + 1L - as.integer(df$scenario_key)

  x_span <- diff(range(df$value, na.rm = TRUE))
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.015 * x_span else 0

  y_breaks <- sort(unique(df$row_y))
  # Rows count top-down from n_rows, so break value b maps back to level
  # index n_rows + 1 - b. Two-line gutter label: scenario name over period.
  y_labs <- vapply(y_breaks, function(b) {
    sub(" / ", "\n", scenario_levels[n_rows + 1L - b], fixed = TRUE)
  }, character(1L))

  # Alternating light-grey banding behind every second scenario row (counting
  # from the top), drawn first so gridlines show through.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  row_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  if (has_source) {
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
    # Two series share each scenario row: baseline takes the upper half-slot,
    # policy the lower one, so the pairs read as one group.
    src_off <- c(Baseline = 0.19, Policy = -0.19)
    df$y_off <- unname(src_off[as.character(df$source)])
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_rect(
      data = row_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    )

  if (is.finite(hist_mean)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = hist_mean, linetype = "dashed",
        colour = .wise_zero, linewidth = 0.5
      ) +
      ggplot2::annotate(
        "text",
        x = hist_mean, y = n_rows + 0.45, label = "Historical mean",
        hjust = 0, vjust = 0.5, size = 3.9, colour = .wise_zero
      )
  }

  # One-time Baseline/Policy captions on the top row, like the adverse
  # dumbbells: each anchored just past the right-most draw of its series.
  if (has_source) {
    top_scen <- scenario_levels[[1L]]
    top_row_y <- n_rows
    cap_df <- do.call(rbind, lapply(names(src_off), function(src) {
      vals <- df$value[df$scenario == top_scen & df$source == src]
      vals <- vals[is.finite(vals)]
      if (!length(vals)) {
        return(NULL)
      }
      data.frame(
        lab_x = max(vals) + lab_gap,
        y = top_row_y + unname(src_off[[src]]),
        label = src
      )
    }))
  } else {
    cap_df <- NULL
  }

  if (has_source) {
    distribution_layer <- if (identical(plot_type, "violin")) {
      ggplot2::geom_violin(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          fill = .data$scenario_key, alpha = .data$source,
          group = interaction(.data$scenario_key, .data$source)
        ),
        orientation = "y", scale = "width", width = 0.34, colour = NA,
        na.rm = TRUE
      )
    } else {
      ggplot2::geom_boxplot(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          fill = .data$scenario_key, alpha = .data$source,
          group = interaction(.data$scenario_key, .data$source)
        ),
        orientation = "y", width = 0.16, outlier.shape = NA,
        colour = .wise_support, na.rm = TRUE
      )
    }

    mean_df <- stats::aggregate(value ~ scenario_key + source,
      data = df,
      FUN = mean, na.rm = TRUE
    )
    mean_df$y_off <- unname(src_off[as.character(mean_df$source)])
    mean_df$row_y <- n_rows + 1L - as.integer(mean_df$scenario_key)

    p <- p + distribution_layer +
      ggplot2::geom_point(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          colour = .data$scenario_key, alpha = .data$source
        ),
        # Jitter within the row only - never along the value axis, or the
        # dots would smear past their true outcome values.
        position = ggplot2::position_jitter(width = 0, height = 0.07),
        size = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df[mean_df$source == "Baseline", , drop = FALSE],
        ggplot2::aes(x = .data$value, y = .data$row_y + .data$y_off),
        shape = 21, size = 3.0, fill = "white", colour = .wise_slate,
        stroke = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df[mean_df$source == "Policy", , drop = FALSE],
        ggplot2::aes(x = .data$value, y = .data$row_y + .data$y_off),
        shape = 21, size = 3.4, fill = .wise_policy, colour = .wise_policy_dark,
        stroke = 1.0, na.rm = TRUE
      ) +
      ggplot2::scale_alpha_manual(
        values = c(Baseline = 0.30, Policy = 0.85), guide = "none"
      )
  } else {
    distribution_layer <- if (identical(plot_type, "violin")) {
      ggplot2::geom_violin(
        ggplot2::aes(y = .data$row_y, fill = .data$scenario_key),
        orientation = "y", scale = "width", width = 0.62, alpha = 0.28,
        colour = NA, na.rm = TRUE
      )
    } else {
      ggplot2::geom_boxplot(
        ggplot2::aes(y = .data$row_y, fill = .data$scenario_key),
        orientation = "y", width = 0.30, outlier.shape = NA,
        alpha = 0.45, colour = .wise_support, na.rm = TRUE
      )
    }

    dot_jitter <- if (identical(plot_type, "violin")) {
      ggplot2::position_jitter(width = 0, height = 0.18)
    } else {
      ggplot2::position_jitter(width = 0, height = 0.26)
    }
    dot_alpha <- if (identical(plot_type, "violin")) 0.40 else 0.30

    mean_df <- stats::aggregate(value ~ scenario_key,
      data = df,
      FUN = mean, na.rm = TRUE
    )
    mean_df$row_y <- n_rows + 1L - as.integer(mean_df$scenario_key)

    p <- p + distribution_layer +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$row_y, colour = .data$scenario_key),
        position = dot_jitter, alpha = dot_alpha, size = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df,
        ggplot2::aes(x = .data$value, y = .data$row_y),
        shape = 21, size = 3.0, fill = "white", colour = .wise_slate,
        stroke = 1.0, na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::scale_fill_manual(
      values = scenario_palette,
      na.value = .wise_history, guide = "none"
    ) +
    ggplot2::scale_colour_manual(
      values = scenario_palette,
      na.value = .wise_history, guide = "none"
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks, labels = y_labs,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      # Reserve right-hand room for the one-time Baseline/Policy captions.
      expand = ggplot2::expansion(mult = c(0.02, if (has_source) 0.12 else 0.02))
    ) +
    ggplot2::labs(x = x_label, y = NULL, title = title, subtitle = subtitle) +
    theme_wise() +
    ggplot2::theme(legend.position = "none")

  if (!is.null(cap_df)) {
    p <- p + ggplot2::geom_text(
      data = cap_df,
      ggplot2::aes(x = .data$lab_x, y = .data$y, label = .data$label),
      hjust = 0, size = 3.9, fontface = "bold",
      colour = c(.wise_slate, .wise_policy_dark), inherit.aes = FALSE,
      show.legend = FALSE, na.rm = TRUE
    )
  }
  p
}

#' Render Step 2 Return-Period Dot Plot (Figure S2-4)
#'
#' Design D: return-period names sit on the y axis outside the panel, the
#' scenario legend is replaced by direct labels anchored at the right end of
#' each top-row series, and Historical uses the exceedance plot's dark
#' support colour. Scenario-coloured bands keep encoding climate-model spread.
#' @noRd
plot_step2_adverse_dot <- function(tbl, x_label = "Outcome level",
                                   title = NULL, subtitle = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Return-period outcomes are unavailable."))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  tbl$scenario_key <- factor(
    ifelse(tbl$is_historical, "Historical", as.character(tbl$scenario)),
    levels = scenario_levels
  )
  # Vertical dodge: multiple scenarios share each return-period row, so
  # offset the markers per scenario to keep them readable. Historical
  # keeps the lower slot; each future scenario takes its own slot above it.
  # Wide multi-SSP runs need generous slot spacing to avoid crowding.
  dodge_width <- 0.6
  tbl$rp_y <- as.integer(tbl$rp_label)
  tbl$dodge_offset <- stats::ave(
    seq_len(nrow(tbl)),
    tbl$rp_y,
    FUN = function(idx) {
      k <- length(idx)
      if (k <= 1L) {
        return(0)
      }
      seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k)[
        order(match(as.character(tbl$scenario_key[idx]), scenario_levels))
      ] * (dodge_width / max(k - 1L, 1))
    }
  )

  # Direct scenario labels replace the colour legend: anchored just past the
  # right end of each series' top-row (Expected) marker, with the x expansion
  # enlarged so the longest label stays inside the panel.
  top_y <- max(tbl$rp_y)
  top_rows <- tbl[tbl$rp_y == top_y, , drop = FALSE]
  # One label per scenario even if a scenario contributes several top rows.
  top_rows <- top_rows[!duplicated(top_rows$scenario_key), , drop = FALSE]

  x_vals <- c(tbl$value, tbl$intermod_lo, tbl$intermod_hi)
  x_span <- diff(range(x_vals, na.rm = TRUE))
  # Uniform label gap, in data units, so every series label clears its
  # marker by the same distance.
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.012 * x_span else 0

  top_rows$lab_x <- vapply(seq_len(nrow(top_rows)), function(i) {
    r <- top_rows[i, ]
    hi <- r$intermod_hi[[1L]]
    if (!is.finite(hi)) hi <- r$value[[1L]]
    hi + lab_gap
  }, numeric(1L))
  top_rows$lab_col <- unname(scenario_colours[as.character(top_rows$scenario_key)])

  right_mult <- if (is.finite(x_span) && x_span > 0) {
    max(0.12, max(nchar(as.character(top_rows$scenario_key)), 0L) * 0.012)
  } else {
    0.05
  }

  y_breaks <- sort(unique(tbl$rp_y))
  y_labs <- levels(tbl$rp_label)[y_breaks]

  # Alternating light-grey banding behind every second return-period row
  # (counting from the top), so the markers that share a row read as one
  # group. Drawn first, i.e. behind every geom.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  rp_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  p <- ggplot2::ggplot(tbl, ggplot2::aes(
    y = .data$rp_y + .data$dodge_offset,
    x = .data$value,
    colour = .data$scenario_key,
    fill = .data$scenario_key
  )) +
    ggplot2::geom_rect(
      data = rp_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$intermod_lo, xend = .data$intermod_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      linewidth = 2.0, alpha = 0.5, lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 3.4, stroke = 1.1, shape = 21, na.rm = TRUE) +
    # One-time series labels on the top row, coloured per scenario.
    ggplot2::geom_text(
      data = top_rows,
      ggplot2::aes(
        x = .data$lab_x, y = .data$rp_y + .data$dodge_offset,
        label = as.character(.data$scenario_key)
      ),
      colour = top_rows$lab_col, hjust = 0, size = 3.9,
      fontface = "bold", show.legend = FALSE, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = scenario_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = scenario_colours, guide = "none") +
    # Return-period names are the y-axis text, outside the panel like every
    # other chart.
    ggplot2::scale_y_continuous(
      breaks = y_breaks, labels = y_labs,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.02, right_mult))
    ) +
    ggplot2::labs(
      x = x_label, y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = "none"
    )
  # Multiple future periods stay on one shared x-axis: every period variant
  # is its own scenario row with its own top-row label, so no faceting.
  p
}

#' Enhanced Exceedance Probability Curve with Return Period Axis
#'
#' Colour identifies the SSP family and line type identifies the projection
#' period (for example, `SSP3-7.0 / 2025-2035`). Historical is navy and solid.
#' When baseline and policy series are present, endpoint markers distinguish
#' them; their lines retain the same scenario colour, period linetype, and
#' uniform width.
#' Optional logit probability axis.
#'
#' Each SSP x period combination has an inter-model ribbon and a central
#' across-model median curve. When source-tagged data are supplied (as in the
#' policy comparison), baseline and policy remain distinguishable even when
#' their ribbons overlap.
#'
#' @param curves_tbl    Data frame. Tidy exceedance curves with one row per
#'   (scenario, period, model, rank) welfare value.
#' @param x_label       Axis label for the welfare outcome.
#' @param return_period Logical. Show return period lines. Default TRUE.
#' @param n_sim_years   Integer. Triggers reliability annotation.
#' @param logit_x       Logical. Use logit scale on the probability axis to
#'   emphasise both tails symmetrically. Default FALSE.
#' @param band_q          Named numeric vector \code{c(lo =, hi =)} quantiles
#'   for the coefficient band. Default \code{c(0.10, 0.90)}.
#' @param ensemble_band_q Named numeric vector quantiles for the inter-model
#'   ensemble band. Default \code{c(0, 1)} (full range).
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   scale_linewidth_manual coord_flip guide_legend element_text
#' @importFrom scales logit_trans
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @noRd
enhance_exceedance <- function(curves_tbl,
                               x_label,
                               return_period = TRUE,
                               n_sim_years = NULL,
                               logit_x = FALSE,
                               band_q = c(lo = 0.10, hi = 0.90),
                               ensemble_band_q = c(lo = 0, hi = 1)) {
  if (is.null(curves_tbl) || nrow(curves_tbl) == 0L) {
    return(blank_plot("Run a simulation to see exceedance probabilities."))
  }

  # Per-scenario summary at each rank ----
  # For each (scenario, rank) collapse across models:
  #   central_at_rank    = median of welfare_val across models
  #   intermod_lo/hi     = quantile across models at ensemble_band_q
  #   coef_lo/hi_at_rank = median(welfare_val +/- z * coef_sd) across models
  z_lo <- if (!is.null(band_q)) stats::qnorm(band_q[["lo"]]) else NA_real_
  z_hi <- if (!is.null(band_q)) stats::qnorm(band_q[["hi"]]) else NA_real_

  has_source <- "source" %in% names(curves_tbl)
  if (has_source) {
    curves_tbl <- curves_tbl[!(curves_tbl$is_historical &
      curves_tbl$source == "Policy"), , drop = FALSE]
    grp_cols <- c("scenario", "source", "rank")
  } else {
    grp_cols <- c("scenario", "rank")
  }

  agg_df <- curves_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      exceed_prob = dplyr::first(.data$exceed_prob),
      central = stats::median(.data$welfare_val, na.rm = TRUE),
      intermod_lo = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["lo"]],
        na.rm = TRUE
      )),
      intermod_hi = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["hi"]],
        na.rm = TRUE
      )),
      n_models = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups = "drop"
    )

  # Coefficient band: apply a scenario-level typical SE as a constant offset
  # from the central curve. Using the per-rank coef_sd directly produces noisy,
  # self-crossing dashed lines because the per-rank SD wiggles. A single
  # typical SE per scenario gives clean parallels.
  if (!is.null(band_q)) {
    sd_grp <- if (has_source) c("scenario", "source") else "scenario"
    coef_sd_scn <- curves_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sd_grp))) |>
      dplyr::summarise(
        coef_sd_typ = stats::median(.data$coef_sd, na.rm = TRUE),
        .groups = "drop"
      )
    agg_df <- dplyr::left_join(agg_df, coef_sd_scn, by = sd_grp)
    agg_df$coef_lo <- agg_df$central + z_lo * agg_df$coef_sd_typ
    agg_df$coef_hi <- agg_df$central + z_hi * agg_df$coef_sd_typ
  } else {
    agg_df$coef_lo <- NA_real_
    agg_df$coef_hi <- NA_real_
  }
  if (has_source) {
    agg_df$source <- factor(agg_df$source, levels = c("Baseline", "Policy"))
    agg_df <- agg_df[order(agg_df$scenario, agg_df$source, agg_df$rank), ,
      drop = FALSE
    ]
    agg_df$line_id <- paste(agg_df$scenario, agg_df$source, sep = " | ")
  } else {
    agg_df <- agg_df[order(agg_df$scenario, agg_df$rank), , drop = FALSE]
    agg_df$line_id <- as.character(agg_df$scenario)
  }

  # Aesthetic mappings. Keep each legend tied to one meaningful dimension:
  # SSP family (colour), period (linetype), and, for policy comparisons, source
  # (linewidth). Mapping `scenario` to both colour and linetype created a
  # duplicated scenario legend and hid the source distinction in overlapping
  # ribbons.
  agg_df$ssp_key <- ifelse(agg_df$is_historical, "Historical",
    vapply(agg_df$scenario, .normalise_ssp, character(1L))
  )
  agg_df$yr_lbl <- ifelse(agg_df$is_historical, "Historical",
    vapply(agg_df$scenario, .parse_year, character(1L))
  )

  fut_yr_labels <- sort(unique(agg_df$yr_lbl[agg_df$yr_lbl != "Historical"]))
  yr_styles <- .resolve_year_styles(fut_yr_labels)
  present_ssps <- sort(unique(agg_df$ssp_key[agg_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = .wise_support,
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(agg_df$scenario[!agg_df$is_historical])))
  )
  scenario_colour_map <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(colour_map_ssp)) unname(colour_map_ssp[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  scenario_linetype_map <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return("solid")
    }
    unname(ltype_map_yr[[.parse_year(s)]] %||% "solid")
  }, character(1L)), scenario_levels)
  agg_df$scenario_key <- factor(
    ifelse(agg_df$is_historical, "Historical", as.character(agg_df$scenario)),
    levels = scenario_levels
  )
  agg_df$ssp_key <- factor(agg_df$ssp_key, levels = c("Historical", present_ssps))
  agg_df$yr_lbl <- factor(agg_df$yr_lbl, levels = c("Historical", fut_yr_labels))
  if (has_source) {
    agg_df$source <- factor(agg_df$source, levels = c("Baseline", "Policy"))
    # All ribbons take the scenario colour; the series distinction lives in
    # the endpoint markers and one-time captions only.
    ribbon_palette <- scenario_colour_map
    agg_df$ribbon_key <- as.character(agg_df$scenario_key)
    agg_df$line_key <- as.character(agg_df$scenario_key)
  } else {
    ribbon_palette <- scenario_colour_map
    agg_df$ribbon_key <- as.character(agg_df$scenario_key)
    agg_df$line_key <- as.character(agg_df$scenario_key)
  }
  hist_df <- agg_df[agg_df$is_historical, , drop = FALSE]
  fut_mod_df <- agg_df[!agg_df$is_historical, , drop = FALSE]
  fut_baseline_df <- if (has_source) {
    fut_mod_df[fut_mod_df$source == "Baseline", , drop = FALSE]
  } else {
    fut_mod_df
  }
  fut_policy_df <- if (has_source) {
    fut_mod_df[fut_mod_df$source == "Policy", , drop = FALSE]
  } else {
    fut_mod_df[0, , drop = FALSE]
  }

  # Plot ----
  # Layer order (back to front): inter-model ribbon (future) -> coefficient
  # band (optional) -> ensemble median curves.
  p <- ggplot2::ggplot(
    agg_df,
    ggplot2::aes(
      x        = .data$central,
      y        = .data$exceed_prob,
      colour   = .data$line_key,
      linetype = .data$scenario_key,
      group    = .data$line_id
    )
  )

  # Inter-model ribbons for each future series. Baseline and policy are both
  # simulated across climate models, so each has its own spread. Every band
  # takes its scenario's colour, matching its median line; the series
  # (baseline vs policy) is distinguished only by the endpoint markers and
  # the one-time captions.
  show_ens_ribbon <- !is.null(ensemble_band_q) &&
    (ensemble_band_q[["hi"]] > ensemble_band_q[["lo"]])
  if (nrow(fut_mod_df) > 0L && isTRUE(show_ens_ribbon)) {
    ribbon_aes <- ggplot2::aes(
      y = .data$exceed_prob, xmin = .data$intermod_lo,
      xmax = .data$intermod_hi, fill = .data$ribbon_key,
      group = .data$line_id
    )
    if (has_source) {
      p <- p +
        ggplot2::geom_ribbon(
          data = fut_baseline_df, mapping = ribbon_aes,
          alpha = 0.12, inherit.aes = FALSE
        ) +
        ggplot2::geom_ribbon(
          data = fut_policy_df, mapping = ribbon_aes,
          alpha = 0.12, inherit.aes = FALSE
        )
    } else {
      p <- p + ggplot2::geom_ribbon(
        data = fut_mod_df, mapping = ribbon_aes,
        alpha = 0.18, inherit.aes = FALSE
      )
    }
  }

  # Coefficient uncertainty band: drawn as a pair of dashed outline curves
  # (lo and hi) instead of a filled ribbon, so it remains visible regardless
  # of whether it falls inside or outside the inter-model ribbon.
  if (!is.null(band_q) && any(!is.na(agg_df$coef_lo))) {
    coef_df <- agg_df[!is.na(agg_df$coef_lo), , drop = FALSE]
    coef_aes_lo <- ggplot2::aes(
      x = .data$coef_lo, y = .data$exceed_prob,
      colour = .data$line_key,
      linetype = .data$scenario_key,
      group = .data$line_id
    )
    coef_aes_hi <- ggplot2::aes(
      x = .data$coef_hi, y = .data$exceed_prob,
      colour = .data$line_key,
      linetype = .data$scenario_key,
      group = .data$line_id
    )
    p <- p +
      ggplot2::geom_line(
        data = coef_df, mapping = coef_aes_lo,
        linetype = "dashed", linewidth = 0.5,
        inherit.aes = FALSE, show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = coef_df, mapping = coef_aes_hi,
        linetype = "dashed", linewidth = 0.5,
        inherit.aes = FALSE, show.legend = FALSE
      )
  }

  # Central median lines. Future lines show the across-model median at each
  # exceedance probability, making the centre of the ensemble explicit. All
  # lines share the theme's default width and the scenario colours; the
  # series (baseline vs policy) is distinguished by the endpoint markers.
  p <- p +
    ggplot2::geom_line(data = hist_df, linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(data = fut_mod_df, linewidth = 0.9, na.rm = TRUE)
  # No historical-mean reference line here: the exceedance curve aggregates
  # the adverse tail only, so its mean would not match the full-sample
  # historical mean shown on the other charts.
  p <- p +
    ggplot2::scale_color_manual(
      values = scenario_colour_map,
      breaks = scenario_levels,
      labels = scenario_levels,
      name   = NULL,
      guide  = "none"
    ) +
    ggplot2::scale_fill_manual(
      values   = ribbon_palette,
      na.value = "grey70",
      guide    = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = scenario_linetype_map,
      breaks = scenario_levels,
      name   = NULL,
      guide  = "none"
    )
  endpoint_rows <- dplyr::bind_rows(lapply(split(agg_df, agg_df$line_id), function(x) {
    x <- x[which.max(x$exceed_prob), , drop = FALSE]
    # Labels carry the scenario name only; the series (baseline vs policy) is
    # encoded by the endpoint marker and the one-time captions below.
    x$curve_label <- as.character(x$scenario_key)
    # Label colour must match the colour the line is actually drawn in, so
    # every label takes its scenario colour from the map.
    x$label_col <- unname(scenario_colour_map[[as.character(x$scenario_key)]]) %||%
      .wise_slate
    x
  }))
  max_prob <- max(agg_df$exceed_prob, na.rm = TRUE)
  label_prob <- max_prob
  # One text layer per series so each label can take its line's exact colour
  # as a constant without touching the plot-wide colour scale.
  label_layers <- lapply(seq_len(nrow(endpoint_rows)), function(i) {
    ggplot2::geom_text(
      data = endpoint_rows[i, , drop = FALSE],
      ggplot2::aes(x = .data$central, y = label_prob, label = .data$curve_label),
      colour = endpoint_rows$label_col[[i]],
      # Anchor each label just past its line's end marker and left-align it,
      # so it starts off the right-hand side of the curves and reads into the
      # axis expansion gutter, clear of every line regardless of curve
      # direction. Size matches the series labels on the other results plots.
      hjust = -0.12, size = 3.9, fontface = "bold", show.legend = FALSE,
      inherit.aes = FALSE
    )
  })
  p <- p + label_layers

  # Endpoint markers on each future series: an open circle (baseline) and a
  # filled vermillion circle (policy) at the right-hand end of the curve, so
  # the pair reads like the adverse plot's dumbbells even where a scenario's
  # SSP colour matches the policy accent (SSP5-8.5).
  if (has_source) {
    fut_end <- endpoint_rows[!endpoint_rows$is_historical, , drop = FALSE]
    base_end <- fut_end[fut_end$source == "Baseline", , drop = FALSE]
    pol_end <- fut_end[fut_end$source == "Policy", , drop = FALSE]
    if (nrow(base_end) > 0L) {
      p <- p + ggplot2::geom_point(
        data = base_end,
        ggplot2::aes(
          x = .data$central, y = .data$exceed_prob,
          colour = .data$scenario_key
        ),
        shape = 21, fill = "white", stroke = 1.2, size = 3.0,
        inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
      )
    }
    if (nrow(pol_end) > 0L) {
      p <- p + ggplot2::geom_point(
        data = pol_end,
        ggplot2::aes(x = .data$central, y = .data$exceed_prob),
        shape = 21, fill = .wise_policy, colour = .wise_policy_dark,
        stroke = 1.1, size = 3.4,
        inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
      )
    }
    # One-time Baseline/Policy captions on the topmost pair only, echoing the
    # adverse plot's caption convention: the caption sits just outside its
    # marker along the outcome axis.
    if (nrow(pol_end) > 0L && nrow(base_end) > 0L) {
      top_pol <- pol_end[which.max(pol_end$central), , drop = FALSE]
      top_base <- base_end[base_end$scenario_key == top_pol$scenario_key, ,
        drop = FALSE
      ]
      if (nrow(top_base) > 0L) {
        x_span <- diff(range(agg_df$central, na.rm = TRUE))
        # Both captions sit just ABOVE their marker, in the clear gap between
        # the pair's curves, so each caption visibly attaches to its own
        # endpoint instead of drifting toward the neighbouring series.
        cap_off <- if (is.finite(x_span) && x_span > 0) 0.04 * x_span else 0
        cap_df <- rbind(
          data.frame(
            x = top_pol$central, y = top_pol$exceed_prob,
            off = cap_off, label = "Policy", col = .wise_policy_dark
          ),
          data.frame(
            x = top_base$central, y = top_base$exceed_prob,
            off = cap_off, label = "Baseline", col = .wise_slate
          )
        )
        p <- p + ggplot2::geom_text(
          data = cap_df,
          ggplot2::aes(
            x = .data$x + .data$off, y = .data$y,
            label = .data$label
          ),
          colour = cap_df$col, hjust = 0.5, size = 3.9, fontface = "bold",
          inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
        )
      }
    }
  }

  p <- p +
    ggplot2::labs(
      x = x_label,
      y = if (max(agg_df$exceed_prob, na.rm = TRUE) <= 0.55) "Annual adverse exceedance probability (AEP)" else "Annual exceedance probability"
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = "none",
      # The probability ticks bunch where the log/logit scale compresses
      # (e.g. 1-in-5 vs 1-in-10); keep them a notch smaller than the theme
      # default so the two-line labels stay clear of each other. Under
      # coord_flip the bottom probability axis is themed by axis.text.x.
      axis.text.x = ggplot2::element_text(size = 11, colour = .wise_slate)
    ) +
    ggplot2::coord_flip()

  # Probability axis scaling ----
  # Return-period guides are carried by the axis ticks alone; the redundant
  # in-panel dashed guide lines and their floating labels were removed.
  is_adverse_tail <- max(agg_df$exceed_prob, na.rm = TRUE) <= 0.55
  support_years <- if (!is.null(n_sim_years) && is.finite(n_sim_years)) {
    max(2L, floor(n_sim_years))
  } else {
    NA_integer_
  }
  supported_rp <- function(x) {
    if (!is.finite(support_years)) {
      return(x)
    }
    denom <- suppressWarnings(as.numeric(sub(".*:", "", names(x))))
    x[is.na(denom) | denom <= support_years]
  }
  rp_tick_label <- function(nm, prob) {
    paste0(
      sub(":", " in ", nm), "\n(",
      scales::percent(prob, accuracy = 1), ")"
    )
  }
  if (is_adverse_tail) {
    # Adverse tail: log scale covering only periods supported by the
    # available simulated years. Do not imply a 1-in-50 estimate from 30 years.
    log_rp <- c("1:2" = 0.50, RP_LOW)
    log_rp <- supported_rp(log_rp)
    log_rp <- log_rp[order(log_rp, decreasing = TRUE)]
    log_breaks <- unname(log_rp)
    log_labels <- mapply(rp_tick_label, names(log_rp), unname(log_rp),
      USE.NAMES = FALSE
    )
    min_prob <- max(min(agg_df$exceed_prob, na.rm = TRUE), 0.005)
    keep_b <- log_breaks >= min_prob * 0.9
    low_lim <- min(min_prob * 0.9, min(log_breaks[keep_b]) * 0.9)
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::log10_trans(),
      breaks = log_breaks[keep_b],
      labels = log_labels[keep_b],
      limits = c(low_lim, 0.55),
      # Right-hand gutter so the endpoint series labels sit clear of the
      # curves while remaining inside the plot window.
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  } else if (isTRUE(logit_x)) {
    rp_low <- supported_rp(RP_LOW)
    rp_high <- supported_rp(RP_HIGH)
    # The complementary periods (1 - RP_HIGH) land on the same exceedance
    # probabilities as RP_LOW; dedupe so each tick renders once.
    logit_breaks <- sort(unique(c(
      unname(rp_low), 0.50,
      1 - unname(rp_high)
    )))
    # Label each tick from its own exceedance probability ("1 in 10 (10%)"),
    # so complementary-period breaks read in the same convention.
    logit_labels <- ifelse(
      logit_breaks == 0.5, "Median",
      paste0(
        "1 in ", format(round(1 / logit_breaks)), "\n(",
        scales::percent(logit_breaks, accuracy = 1), ")"
      )
    )
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::logit_trans(),
      breaks = logit_breaks,
      labels = logit_labels,
      limits = c(0.005, 0.995),
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  }
  p
}
