# Archived from fct_sim_compare.R
# plot_bar_climate() has no active call sites in any module.
# Retained here for reference only.

# ---------------------------------------------------------------------------- #

#' Bar Chart Comparing Scenarios at Timeframes
#'
#' @param scenarios    Named list; each element is from aggregate_sim_preds().
#' @param hist_agg     Full result of aggregate_sim_preds() for historical.
#' @param centre_years Integer vector of anchor years, or NULL for summary mode.
#' @param band_width   Half-width in years. Defaults to 10L.
#' @param show_error   Logical. Draw p10/p90 error bars. Defaults to TRUE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar geom_hline annotate
#'   labs theme_minimal theme scale_fill_manual position_dodge element_text
#' @importFrom colorspace lighten
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @export
plot_bar_climate <- function(scenarios,
                             hist_agg,
                             centre_years = c(2030L, 2040L, 2050L),
                             band_width   = 10L,
                             show_error   = TRUE) {

  stopifnot(is.list(scenarios), length(scenarios) > 0)
  stopifnot(is.list(hist_agg), all(c("out", "x_label") %in% names(hist_agg)))

  summarise_band <- function(vals) {
    if (length(vals) == 0) return(NULL)
    list(mean = mean(vals, na.rm = TRUE),
         p10  = as.numeric(stats::quantile(vals, 0.10, na.rm = TRUE)),
         p90  = as.numeric(stats::quantile(vals, 0.90, na.rm = TRUE)))
  }
  band_vals <- function(out_df, cy, bw) {
    if (is.null(cy)) return(out_df$value)
    out_df$value[out_df$sim_year >= cy - bw & out_df$sim_year <= cy + bw]
  }

  cy_list <- if (is.null(centre_years)) list(NULL) else as.list(centre_years)

  rows <- lapply(names(scenarios), function(nm) {
    sc_out   <- scenarios[[nm]]$out
    ssp_key  <- .normalise_ssp(nm)
    yr_label <- .parse_year(nm)
    lapply(cy_list, function(cy) {
      vals <- band_vals(sc_out, cy, band_width)
      s    <- summarise_band(vals)
      if (is.null(s)) return(NULL)
      data.frame(
        scenario    = nm,
        ssp_key     = if (is.na(ssp_key)) "Historical" else ssp_key,
        anchor_year = if (is.null(cy)) "All years" else as.character(as.integer(cy)),
        yr_label    = if (is.na(yr_label)) "Historical" else yr_label,
        mean = s$mean, p10 = s$p10, p90 = s$p90,
        stringsAsFactors = FALSE
      )
    })
  })

  plot_df <- dplyr::bind_rows(Filter(Negate(is.null), unlist(rows, recursive = FALSE)))

  hist_in_scenarios <- Filter(function(nm) is.na(.normalise_ssp(nm)), names(scenarios))
  if (length(hist_in_scenarios) == 0) {
    hist_s <- summarise_band(hist_agg$out$value)
    if (!is.null(hist_s)) {
      hist_row <- data.frame(
        scenario = "Historical", ssp_key = "Historical",
        anchor_year = "Historical", yr_label = "Historical",
        mean = hist_s$mean, p10 = hist_s$p10, p90 = hist_s$p90,
        stringsAsFactors = FALSE
      )
      plot_df <- dplyr::bind_rows(hist_row, plot_df)
    }
  }

  if (nrow(plot_df) == 0)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data in selected range"))

  anchor_levels <- c("Historical",
    sort(unique(plot_df$anchor_year[plot_df$anchor_year != "Historical"])))
  plot_df$anchor_year <- factor(plot_df$anchor_year, levels = anchor_levels)
  plot_df$fill_key    <- paste(plot_df$ssp_key, plot_df$yr_label, sep = "__")

  fut_yr_labels <- sort(unique(plot_df$yr_label[plot_df$yr_label != "Historical"]))
  n_yrs         <- length(fut_yr_labels)
  shade_palette <- c("Historical__Historical" = "black")
  for (ssp in intersect(names(.ssp_colours), unique(plot_df$ssp_key))) {
    base_col <- .ssp_colours[ssp]
    if (n_yrs == 0) next
    lightens <- seq(0.45, 0.0, length.out = n_yrs)
    for (i in seq_along(fut_yr_labels)) {
      fk <- paste(ssp, fut_yr_labels[i], sep = "__")
      shade_palette[fk] <- colorspace::lighten(base_col, lightens[i])
    }
  }

  present_fill_keys <- unique(plot_df$fill_key)
  nice_labels       <- sub("__", " / ", present_fill_keys)
  hist_mean         <- mean(hist_agg$out$value, na.rm = TRUE)
  dodge             <- ggplot2::position_dodge(width = 0.8)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$anchor_year, y = .data$mean,
                 fill = .data$fill_key, group = .data$scenario)
  ) +
    ggplot2::geom_col(position = dodge, width = 0.7) +
    ggplot2::geom_hline(yintercept = hist_mean, linetype = "dashed",
                        colour = "grey30", linewidth = 0.6) +
    ggplot2::annotate("text", x = Inf, y = hist_mean,
                      label = "Hist. mean", hjust = 1.05, vjust = -0.5,
                      size = 3, colour = "grey30") +
    ggplot2::scale_fill_manual(
      values = shade_palette, breaks = present_fill_keys,
      labels = nice_labels, name = "Climate scenario / Timeframe"
    ) +
    ggplot2::labs(
      title = "Distribution by climate scenario and timeframe",
      x     = if (is.null(centre_years)) NULL else "Timeframe",
      y     = hist_agg$x_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom"
    )

  if (isTRUE(show_error)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$p10, ymax = .data$p90),
      position = dodge, width = 0.25, linewidth = 0.5, colour = "grey40"
    )
  }
  p
}


