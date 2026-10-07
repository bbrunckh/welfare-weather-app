# Archived static ggplot renderers.
#
# These definitions are retained for development/reference use only and are
# not part of the installed package runtime. Data preparation and echarts
# counterparts remain in their original R/ files.

#' Ridge distribution plot helper
#'
#' @param df A data.frame.
#' @param x_var Column name for the x-axis.
#' @param group_var Column name for the ridges (y-axis).
#' @param fill_var Column name for the fill aesthetic.
#' @param x_label Optional x-axis label.
#' @param wrap_width Optional integer to wrap x-axis label text.
#' @param log_transform Logical; if TRUE, applies log10 transformation to x-axis. Default FALSE.
#'
#' @return A ggplot object or NULL if inputs are invalid.
#'
#' @noRd
ridge_distribution_plot <- function(
  df,
  x_var,
  group_var = "countryyear",
  fill_var = "code",
  x_label = NULL,
  wrap_width = NULL,
  log_transform = FALSE,
  group_labels = NULL
) {
  agg <- build_ridge_distribution_data(
    df,
    x_var = x_var,
    group_var = group_var,
    fill_var = fill_var,
    log_transform = log_transform
  )
  if (is.null(agg)) {
    return(NULL)
  }

  label <- x_label
  if (!is.null(label) && !is.null(wrap_width)) {
    label <- stringr::str_wrap(label, wrap_width)
  }

  # Add log transform note to label if applicable
  if (log_transform && !is.null(label)) {
    label <- paste0(label, " (log scale)")
  }

  display_groups <- agg$groups
  if (!is.null(group_labels)) {
    mapped <- unname(group_labels[display_groups])
    keep <- !is.na(mapped) & nzchar(mapped)
    display_groups[keep] <- mapped[keep]
  }

  p <- ggplot2::ggplot(
    agg$data,
    ggplot2::aes(
      x = .data$x, y = .data$y,
      group = .data$group, fill = .data$fill
    )
  ) +
    ridge_geometry_layers(scale = 2, alpha = 0.7, linewidth = 0.3) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(agg$groups),
      labels = display_groups,
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    theme_wise() +
    ggplot2::labs(
      title = "",
      x = label %||% x_var,
      y = "",
      fill = ""
    ) +
    ggplot2::theme(legend.position = "none")

  # Apply log10 scale to x-axis if requested
  if (log_transform) {
    p <- p + ggplot2::scale_x_log10(
      labels = scales::comma_format()
    )
  }

  p
}

#' Native ggplot2 layers for precomputed ridgeline data
#'
#' A ribbon plus its upper outline reproduces the visual ridge from
#' `build_ridge_distribution_data()` without a specialised geometry package.
#'
#' @param scale Numeric height multiplier.
#' @param alpha Ribbon transparency.
#' @param linewidth Upper outline width.
#' @param colour Outline colour. Set to `NULL` to inherit a mapped colour.
#'
#' @return A list of ggplot2 layers.
#' @noRd
ridge_geometry_layers <- function(scale = 1, alpha = 0.7, linewidth = 0.3,
                                  colour = "black") {
  ribbon <- ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$y,
      ymax = .data$y + .data$height * scale
    ),
    alpha = alpha,
    colour = NA
  )
  line_mapping <- ggplot2::aes(y = .data$y + .data$height * scale)
  line <- if (is.null(colour)) {
    ggplot2::geom_line(line_mapping, linewidth = linewidth)
  } else {
    ggplot2::geom_line(
      line_mapping,
      linewidth = linewidth, colour = colour
    )
  }
  list(ribbon, line)
}
