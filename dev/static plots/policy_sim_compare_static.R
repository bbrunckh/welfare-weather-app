# Archived static ggplot renderers.
#
# These definitions are retained for development/reference use only and are
# not part of the installed package runtime. Data preparation and echarts
# counterparts remain in their original R/ files.

#' Render the Step 3 adverse return-period comparison
#'
#' @param tbl A `step3_adverse_dot_data()` data frame.
#' @param x_label Outcome-axis title.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @return A ggplot object.
plot_step3_adverse_dot <- function(tbl, x_label = "Outcome level",
                                   title = NULL, subtitle = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Return-period outcomes are unavailable."))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  # Scenario colours follow the exceedance plot's scheme: Historical in the
  # dark support navy, futures in their fixed SSP colour.
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
  # offset the dumbbells per scenario to keep them readable. Historical
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

  # Direct labeling replaces the legend. Series names anchor just past the
  # right-most end of each top-row (Expected) dumbbell - baseline end
  # included, so the label stays on the right-hand side even when the
  # outcome direction flips and policy sits left of baseline. The x
  # expansion is enlarged so the longest label stays inside the panel.
  top_y <- max(tbl$rp_y)
  top_rows <- tbl[tbl$rp_y == top_y, , drop = FALSE]
  # One label per scenario even if a scenario contributes several top rows.
  top_rows <- top_rows[!duplicated(top_rows$scenario_key), , drop = FALSE]

  x_vals <- c(
    tbl$baseline_val, tbl$policy_val, tbl$policy_lo, tbl$policy_hi,
    tbl$base_lo, tbl$base_hi
  )
  x_span <- diff(range(x_vals, na.rm = TRUE))
  # Uniform label gap, in data units, so every series label clears its
  # marker by the same distance regardless of bands or direction.
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.012 * x_span else 0

  top_rows$lab_x <- vapply(seq_len(nrow(top_rows)), function(i) {
    r <- top_rows[i, ]
    hi <- suppressWarnings(max(r$policy_hi[[1L]], r$policy_val[[1L]],
      r$base_hi[[1L]], r$baseline_val[[1L]],
      na.rm = TRUE
    ))
    if (!is.finite(hi)) hi <- r$policy_val[[1L]]
    hi + lab_gap
  }, numeric(1L))
  top_rows$lab_col <- unname(scenario_colours[as.character(top_rows$scenario_key)])
  # One-time Baseline/Policy captions on the topmost dumbbell of the top row.
  cap_row <- top_rows[which.max(top_rows$dodge_offset), , drop = FALSE]

  right_mult <- if (is.finite(x_span) && x_span > 0) {
    max(0.12, max(nchar(as.character(top_rows$scenario_key)), 0L) * 0.012)
  } else {
    0.05
  }

  y_breaks <- sort(unique(tbl$rp_y))
  y_labs <- levels(tbl$rp_label)[y_breaks]

  # Alternating light-grey banding behind every second return-period row
  # (counting from the top), so the dumbbells that share a row read as one
  # group. Drawn first, i.e. behind every geom.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  rp_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  p <- ggplot2::ggplot(tbl, ggplot2::aes(y = .data$rp_y + .data$dodge_offset)) +
    ggplot2::geom_rect(
      data = rp_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    # Climate-model spread bands, future scenarios only (Historical runs a
    # single climate model): same width for both sources; baseline in
    # transparent blue, policy in transparent policy vermillion.
    ggplot2::geom_segment(
      data = subset(tbl, !is_historical),
      ggplot2::aes(
        x = .data$base_lo, xend = .data$base_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      colour = "#0072B2", alpha = 0.40, linewidth = 2.4,
      lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      data = subset(tbl, !is_historical),
      ggplot2::aes(
        x = .data$policy_lo, xend = .data$policy_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      colour = .wise_policy, alpha = 0.40, linewidth = 2.4,
      lineend = "round", na.rm = TRUE
    ) +
    # Baseline -> policy connector: solid, in the scenario colour, with a
    # large open arrowhead so the policy direction reads at a glance.
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$baseline_val, xend = .data$policy_val,
        yend = .data$rp_y + .data$dodge_offset,
        colour = .data$scenario_key
      ),
      linewidth = 1.0, lineend = "round",
      arrow = ggplot2::arrow(length = ggplot2::unit(10, "pt"), type = "open"),
      na.rm = TRUE, show.legend = FALSE
    ) +
    # Baseline point: open marker in the scenario colour.
    ggplot2::geom_point(
      ggplot2::aes(x = .data$baseline_val, colour = .data$scenario_key),
      fill = "white", shape = 21, stroke = 1.1, size = 3.4, na.rm = TRUE
    ) +
    # Policy point: filled policy vermillion.
    ggplot2::geom_point(
      ggplot2::aes(x = .data$policy_val),
      fill = .wise_policy, colour = .wise_policy_dark, shape = 21,
      stroke = 1.0, size = 3.4, na.rm = TRUE
    ) +
    # One-time Baseline/Policy captions above the topmost dumbbell only.
    ggplot2::geom_text(
      data = cap_row,
      ggplot2::aes(x = .data$baseline_val, y = .data$rp_y + .data$dodge_offset),
      label = "Baseline", vjust = -1.4, size = 3.5, fontface = "bold",
      colour = .wise_slate, show.legend = FALSE, inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = cap_row,
      ggplot2::aes(x = .data$policy_val, y = .data$rp_y + .data$dodge_offset),
      label = "Policy", vjust = -1.4, size = 3.5, fontface = "bold",
      colour = .wise_policy_dark, show.legend = FALSE, inherit.aes = FALSE
    ) +
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
