# Archived static outcome distribution plot.
#
# This source is kept for reference and is not part of the installed package.
# It uses package helpers `.welfare_dist_binary_bars()` and
# `.outcome_density_palette()`, which remain in R/fct_outcome.R for the
# interactive ECharts renderer.

#' Plot outcome distribution by survey wave
#'
#' Calls `ridge_distribution_plot()` on the specified outcome column. The
#' shared helper aggregates to a fixed histogram before smoothing, so plot
#' construction remains responsive for very large country samples.
#' For welfare, overlays dashed poverty line references. Numeric outcomes use
#' log scale when all values are positive.
#'
#' @param df A data frame with a `countryyear` column for ridge grouping.
#' @param outcome Single character string - outcome variable name. Defaults to
#'   `"welfare"`.
#' @param label Human-readable label for the x axis.
#' @param type Outcome type: `"numeric"` or `"logical"`.
#' @param poverty_lines A data frame with columns `value` and `label`.
#'   Only used when `outcome == "welfare"`.
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly.
#'
#' @export
plot_welfare_dist <- function(df,
                              outcome = "welfare",
                              label = NULL,
                              type = "numeric",
                              poverty_lines = welfare_poverty_lines(),
                              wave_labels = NULL) {
  if (is.null(df) || !(outcome %in% names(df))) {
    return(invisible(NULL))
  }

  vals <- df[[outcome]][!is.na(df[[outcome]])]
  type_l <- tolower(type %||% "")
  is_binary <- type_l %in% c("logical", "binary", "boolean") ||
    (length(vals) > 0 && is.numeric(vals) && all(vals %in% c(0, 1)))
  x_label <- label %||% outcome
  if (identical(outcome, "welfare")) x_label <- "$ per day (2021 PPP)"

  # Binary outcomes are discrete proportions, not continuous densities. A
  # 100% stacked bar makes the 0/1 shares immediately readable and avoids the
  # unnecessary histogram/KDE pass used for continuous outcomes.
  if (is_binary) {
    bb <- .welfare_dist_binary_bars(df, outcome, wave_labels)
    if (is.null(bb)) {
      return(invisible(NULL))
    }
    bars <- bb$bars
    display_waves <- bb$display_waves

    return(
      ggplot2::ggplot(
        bars,
        ggplot2::aes(
          x = .data$countryyear,
          ymin = .data$ymin,
          ymax = .data$ymax,
          fill = .data$value
        )
      ) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = as.numeric(.data$countryyear) - 0.45,
            xmax = as.numeric(.data$countryyear) + 0.45,
            ymin = .data$ymin,
            ymax = .data$ymax
          ),
          colour = "white",
          linewidth = 0.25
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = as.numeric(.data$countryyear),
            y = (.data$ymin + .data$ymax) / 2,
            label = .data$label,
            colour = .data$label_colour
          ),
          size = 3,
          fontface = "bold",
          na.rm = TRUE
        ) +
        ggplot2::scale_fill_manual(
          # Match the interview-location palette: pale blue for the
          # baseline state, World Bank blue for the positive state.
          values = c(No = "#D9EFF8", Yes = "#0071BC"),
          drop = FALSE,
          name = NULL
        ) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_y_continuous(
          labels = scales::label_percent(),
          limits = c(0, 1),
          expand = ggplot2::expansion(mult = c(0, 0.03))
        ) +
        ggplot2::labs(
          x = "Survey wave",
          y = "Share of observations"
        ) +
        ggplot2::scale_x_discrete(labels = display_waves) +
        theme_wise() +
        ggplot2::theme(
          legend.position = "top",
          axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        )
    )
  }

  use_log <- identical(type, "numeric") &&
    all(df[[outcome]][!is.na(df[[outcome]])] > 0)

  p <- ridge_distribution_plot(
    df,
    x_var         = outcome,
    fill_var      = "code",
    x_label       = x_label,
    wrap_width    = 40,
    log_transform = use_log,
    group_labels  = wave_labels
  )

  if (is.null(p)) {
    return(invisible(NULL))
  }

  p <- p + ggplot2::scale_fill_manual(
    values = .outcome_density_palette(df$code),
    drop   = FALSE,
    guide  = "none"
  )

  if (identical(outcome, "welfare") && !is.null(poverty_lines)) {
    for (i in seq_len(nrow(poverty_lines))) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = poverty_lines$value[i],
          linetype   = "dashed",
          color      = .wise_marker,
          linewidth  = 0.5
        ) +
        ggplot2::annotate(
          "text",
          x     = poverty_lines$value[i] * 1.15,
          y     = 0.5,
          label = poverty_lines$label[i],
          angle = 90,
          size  = 3.2,
          color = .wise_marker,
          hjust = 0
        )
    }
  }

  p
}
