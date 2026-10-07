# Archived static ggplot renderers.
#
# These definitions are retained for development/reference use only and are
# not part of the installed package runtime. Data preparation and echarts
# counterparts remain in their original R/ files.

#' Plot timing of survey interviews by month.
#'
#' @param plot_data A data frame with columns `month_num` (integer 1-12),
#'   `hh` (integer count), `economy` (character), and `countryyear`
#'   (character), as returned by `summarise_interview_dates()`.
#' @param variant Display style: `"grouped"` (default), `"faceted"`, or
#'   `"heatmap"`.
#' @param unit_label Y-axis noun for the observation unit. Defaults to
#'   `"Households"`; use `"Individuals"` or `"Firms"` when the selected
#'   survey files are at those levels.
#' @param palette Fill palette. The default `"sequential"` assigns each
#'   survey wave its colour from the shared wave palette (same series as the
#'   weather and outcome distribution charts). Other options are
#'   `"okabe_ito"`, `"wise"`, and `"blue"`.
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `plot_data` is
#'   `NULL` or has zero rows.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap geom_tile geom_text
#'   scale_x_discrete scale_fill_manual scale_fill_gradient scale_y_continuous
#'   labs theme element_text
plot_interview_dates <- function(plot_data,
                                 variant = c("grouped", "faceted", "heatmap"),
                                 unit_label = "Households",
                                 palette = c("sequential", "okabe_ito", "wise", "blue"),
                                 wave_labels = NULL) {
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(invisible(NULL))
  }
  variant <- match.arg(variant)
  palette <- match.arg(palette)
  unit_label <- as.character(unit_label)[1L]
  if (is.na(unit_label) || !nzchar(unit_label)) unit_label <- "Observations"

  month_labels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  plot_data <- as.data.frame(plot_data)
  plot_data$month_fct <- factor(
    plot_data$month_num,
    levels = 1:12,
    labels = month_labels
  )
  info <- .interview_wave_info(plot_data, wave_labels)
  waves <- info$waves
  display_waves <- info$display
  plot_data$countryyear <- factor(plot_data$countryyear, levels = waves)
  wave_cols <- .interview_wave_colors(palette, waves)

  if (variant == "heatmap") {
    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$month_fct, y = .data$countryyear,
          fill = .data$hh
        )
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::comma(.data$hh)),
          size = 3, colour = "#1D2A35"
        ) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_fill_gradientn(
          colours = wise_seq_ramp(100),
          labels = scales::label_number(big.mark = ","),
          name = unit_label
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        theme_wise(base_size = 13) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = grid::unit(1.5, "cm")
        )
    )
  }

  p <- if (variant == "faceted") {
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$month_fct, y = .data$hh)
    ) +
      ggplot2::geom_col(fill = "#0071BC", width = 0.72) +
      ggplot2::facet_wrap(~countryyear, ncol = 1, scales = "free_y")
  } else {
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$month_fct, y = .data$hh,
        fill = .data$countryyear
      )
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge2(width = 0.82, preserve = "single"),
        width = 0.72, colour = "white", linewidth = 0.2
      ) +
      ggplot2::scale_fill_manual(
        values = wave_cols, breaks = waves, labels = display_waves, drop = FALSE,
        name = NULL
      )
  }

  p +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(x = NULL, y = unit_label) +
    theme_wise(base_size = 13) +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_line(colour = "#5B6B79"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E3E9EE"),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = if (variant == "faceted") "none" else "top",
      legend.justification = "left",
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}
