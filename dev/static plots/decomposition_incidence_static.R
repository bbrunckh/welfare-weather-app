# Archived static ggplot renderers.
#
# These definitions are retained for development/reference use only and are
# not part of the installed package runtime. Data preparation and echarts
# counterparts remain in their original R/ files.

#' Plot policy decomposition channels by baseline decile
#'
#' Renders the precomputed channel summary as stacked effects with a total
#' effect marker.
#'
#' @param tbl Decile-level channel summary.
#' @param is_rif Logical; include RIF repositioning channels when applicable.
#' @return A ggplot object.
plot_decomposition_channels_by_decile <- function(tbl, is_rif = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Decile decomposition is unavailable for this run.", size = 4))
  }
  is_rif <- if (is.null(is_rif)) {
    "repositioning_percent" %in% names(tbl) &&
      any(abs(tbl$repositioning_percent) > 1e-12, na.rm = TRUE)
  } else {
    isTRUE(is_rif)
  }
  channel_cols <- c(
    "cash_transfer_percent", "covariate_shift_percent",
    if (is_rif) "repositioning_percent", "interaction_percent"
  )
  active <- vapply(channel_cols, function(col) {
    any(abs(tbl[[col]]) > 1e-12, na.rm = TRUE)
  }, logical(1L))
  if (any(active)) channel_cols <- channel_cols[active]
  channel_labels <- c(
    cash_transfer_percent = "SP direct effect",
    covariate_shift_percent = "Main effect (covariate shift)",
    repositioning_percent = "Resilience - Repositioning effect",
    interaction_percent = "Resilience - Interaction effect"
  )
  long <- tidyr::pivot_longer(
    tbl[, c("decile", channel_cols)],
    cols = tidyselect::all_of(channel_cols),
    names_to = "channel", values_to = "effect"
  )
  long$channel <- factor(unname(channel_labels[long$channel]),
    levels = unname(channel_labels[channel_cols])
  )
  # Colorblind-safe quartet from the shared categorical palette; the total
  # marker stays neutral dark so it cannot be confused with a channel.
  colours <- c(
    "SP direct effect" = .wise_cat[[1]],
    "Main effect (covariate shift)" = .wise_cat[[4]],
    "Resilience - Repositioning effect" = .wise_cat[[3]],
    "Resilience - Interaction effect" = .wise_cat[[6]]
  )
  ggplot2::ggplot(long, ggplot2::aes(
    x = factor(.data$decile), y = .data$effect,
    fill = .data$channel
  )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_col(position = "stack", width = 0.62) +
    ggplot2::geom_point(
      data = tbl,
      ggplot2::aes(x = factor(.data$decile), y = .data$total_percent),
      inherit.aes = FALSE, shape = 21, fill = "white",
      colour = .wise_support, size = 2.8, stroke = 1.1
    ) +
    ggplot2::scale_fill_manual(values = colours, drop = FALSE) +
    ggplot2::labs(
      x = "Fixed observed baseline welfare decile (1 = poorest)",
      y = "Policy effect (percent change)", fill = "Channel",
      subtitle = NULL
    ) +
    theme_wise(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot simulated incidence by fixed baseline decile
#'
#' @param tbl Decile-level scenario effects.
#' @param y_label Axis label for the effect.
#' @return A ggplot object.
plot_incidence_by_decile <- function(tbl, y_label = "Household-level simulated welfare effect") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Distributional incidence is unavailable."))
  }
  if (!"scenario" %in% names(tbl)) tbl$scenario <- "Effect"
  multi_basis <- "basis" %in% names(tbl) && length(unique(tbl$basis)) > 1L
  if (multi_basis) {
    tbl$basis <- factor(tbl$basis, levels = unname(.decomp_basis_choices),
      labels = names(.decomp_basis_choices))
  }
  p <- ggplot2::ggplot(tbl, ggplot2::aes(
    x = factor(.data$decile), y = .data$effect,
    fill = .data$scenario
  )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
    wise_scale_fill_cat(name = NULL) +
    ggplot2::labs(
      x = "Fixed observed baseline welfare decile (1 = poorest)",
      y = y_label,
      caption = "Deciles use weighted observed baseline welfare and are not re-ranked under simulated conditions."
    ) +
    theme_wise(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
  if (multi_basis) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$basis))
  p
}
