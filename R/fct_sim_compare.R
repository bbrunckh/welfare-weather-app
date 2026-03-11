# ============================================================================ #
# fct_sim_compare.R
#
# Pure comparison/visualisation functions for Module 2.
# Called by mod_2_06_sim_compare_server() only.
#
# Functions:
#   .resolve_year_styles() -- internal; dynamic linetype map by sorted year
#   plot_bar_climate()     -- grouped bar chart (SSP x timeframe)
#   build_impact_table()   -- return-period threshold table
#   enhance_exceedance()   -- exceedance curve with return-period axis
# ============================================================================ #


# ---- Internal: dynamic year linetype helper --------------------------------
.resolve_year_styles <- function(year_labels) {
  linetypes <- c("solid", "dashed", "dotted", "longdash", "twodash")
  years     <- sort(unique(as.numeric(as.character(year_labels))))
  n         <- length(years)
  lty       <- linetypes[seq_len(min(n, length(linetypes)))]
  list(
    linetype_map = setNames(lty,         as.character(years)),
    alpha_map    = setNames(rep(1.0, n), as.character(years))
  )
}


# ---------------------------------------------------------------------------- #
# Bar chart: scenario comparison                                               #
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

  # --- helpers ---
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

  # --- future scenario rows ---
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

  # --- historical row ---
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

  # --- dynamic shaded fill palette: same SSP -> darker shade for later years ---
  plot_df$fill_key <- paste(plot_df$ssp_key, plot_df$yr_label, sep = "__")

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

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)
  dodge     <- ggplot2::position_dodge(width = 0.8)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$anchor_year, y = .data$mean,
                 fill = .data$fill_key,
                 group = .data$scenario)
  ) +
    ggplot2::geom_col(position = dodge, width = 0.7) +
    ggplot2::geom_hline(yintercept = hist_mean, linetype = "dashed",
                        colour = "grey30", linewidth = 0.6) +
    ggplot2::annotate("text", x = Inf, y = hist_mean,
                      label = "Hist. mean", hjust = 1.05, vjust = -0.5,
                      size = 3, colour = "grey30") +
    ggplot2::scale_fill_manual(
      values = shade_palette,
      breaks = present_fill_keys,
      labels = nice_labels,
      name   = "Climate scenario / Timeframe"
    ) +
    ggplot2::labs(
      title = "Distribution by climate scenario and timeframe",
      x     = if (is.null(centre_years)) NULL else "Timeframe",
      y     = hist_agg$x_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
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


# ---------------------------------------------------------------------------- #
# Impact table                                                                 #
# ---------------------------------------------------------------------------- #

#' Build a Welfare Impact Table at Return Period Thresholds
#' @param scenarios             Named list from `aggregate_sim_preds()`.
#' @param hist_agg              Historical aggregate from `aggregate_sim_preds()`.
#' @param thresholds            Character vector of threshold labels.
#' @param hist_centre_years     Anchor years for historical. Defaults to 2010L.
#' @param scenario_centre_years Anchor years for future scenarios.
#' @param band_width            Half-width in years.
#' @param n_sim_years           Total years; n<40 flags 1:20, n<100 flags 1:50.
#' @return Data frame: scenario, anchor_year, threshold, value, reliable.
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @export
build_impact_table <- function(scenarios,
                               hist_agg,
                               thresholds              = c("mean","1:5","1:10","1:20","1:50"),
                               hist_centre_years       = 2010L,
                               scenario_centre_years   = c(2030L, 2040L, 2050L),
                               band_width              = 10L,
                               n_sim_years             = NULL) {

  rp_map <- c("1:5" = 0.20, "1:10" = 0.10, "1:20" = 0.05, "1:50" = 0.02)

  extract_val <- function(vals, threshold) {
    if (threshold == "mean") mean(vals, na.rm = TRUE)
    else as.numeric(stats::quantile(vals, probs = 1 - rp_map[threshold], na.rm = TRUE))
  }

  make_rows <- function(nm, out_df, centre_years) {
    dplyr::bind_rows(lapply(centre_years, function(cy) {
      band     <- out_df[out_df$sim_year >= cy - band_width &
                           out_df$sim_year <= cy + band_width, , drop = FALSE]
      vals     <- band$value
      in_range <- length(vals) > 0
      dplyr::bind_rows(lapply(thresholds, function(th) {
        reliable <- TRUE
        if (!is.null(n_sim_years)) {
          if (th == "1:20" && n_sim_years < 40)  reliable <- FALSE
          if (th == "1:50" && n_sim_years < 100) reliable <- FALSE
        }
        data.frame(
          scenario    = nm,
          anchor_year = as.integer(cy),
          threshold   = th,
          value       = if (in_range) extract_val(vals, th) else NA_real_,
          reliable    = reliable,
          in_range    = in_range,
          stringsAsFactors = FALSE
        )
      }))
    }))
  }

  hist_rows <- make_rows("Historical", hist_agg$out, hist_centre_years)
  sc_rows   <- dplyr::bind_rows(lapply(names(scenarios), function(nm) {
    make_rows(nm, scenarios[[nm]]$out, scenario_centre_years)
  }))
  dplyr::bind_rows(hist_rows, sc_rows)
}


# ---------------------------------------------------------------------------- #
# Enhanced exceedance curve                                                    #
# ---------------------------------------------------------------------------- #

#' Enhanced Exceedance Probability Curve with Return Period Axis
#'
#' Colour = climate scenario (fixed palette). Line type = timeframe (dynamic;
#' smallest year = solid, ascending). Historical = black/solid, in legend.
#' Return period lines are black; unreliable thresholds labelled in grey (*).
#'
#' @param scenarios     Named list from `aggregate_sim_preds()`.
#' @param hist_agg      Historical aggregate from `aggregate_sim_preds()`.
#' @param x_label       Axis label for the welfare outcome.
#' @param return_period Logical. Show return period lines. Default TRUE.
#' @param n_sim_years   Integer. Triggers reliability annotation.
#' @param show_error    Logical stub (reserved). Default FALSE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes stat_ecdf geom_vline geom_hline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   after_stat coord_flip guide_legend element_text
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
enhance_exceedance <- function(scenarios,
                               hist_agg,
                               x_label,
                               return_period = TRUE,
                               n_sim_years   = NULL,
                               show_error    = FALSE) {

  stopifnot(is.list(scenarios), length(scenarios) > 0)
  labels <- names(scenarios)

  # --- build long data frame ---
  long_df <- dplyr::bind_rows(lapply(seq_along(scenarios), function(i) {
    nm      <- labels[i]
    vals    <- scenarios[[nm]]$out$value
    ssp_key <- .normalise_ssp(nm)
    yr      <- .parse_year(nm)
    data.frame(
      value   = vals,
      group   = nm,
      ssp_key = if (is.na(ssp_key)) "Historical" else ssp_key,
      yr      = if (is.na(yr))      "Historical" else as.character(yr),
      stringsAsFactors = FALSE
    )
  }))
  long_df$group <- factor(long_df$group, levels = labels)

  # --- dynamic year linetype map (futures only) ---
  fut_yr_labels <- sort(unique(long_df$yr[long_df$yr != "Historical"]))
  yr_styles     <- .resolve_year_styles(fut_yr_labels)

  # --- colour map: keyed by ssp_key ---
  present_ssps   <- sort(unique(long_df$ssp_key[long_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = "black",
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )

  # --- linetype map: keyed by yr ---
  present_yrs  <- names(yr_styles$linetype_map)
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)

  # --- base plot: colour = ssp_key, linetype = yr ---
  p <- ggplot2::ggplot(
    long_df, ggplot2::aes(x = value, colour = ssp_key, linetype = yr)
  ) +
    ggplot2::stat_ecdf(
      mapping   = ggplot2::aes(y = 1 - ggplot2::after_stat(y)),
      geom      = "line", linewidth = 0.9
    ) +
    ggplot2::geom_vline(xintercept = hist_mean, linetype = "dotted",
                        colour = "black", linewidth = 0.5) +
    # "Hist. mean" label on the right (high-probability side)
    ggplot2::annotate("text", x = hist_mean, y = 0.97,
                      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
                      size = 2.8, colour = "grey30") +
    ggplot2::scale_color_manual(
      values = colour_map_ssp,
      breaks = c("Historical", present_ssps),
      labels = c("Historical", present_ssps),
      name   = "Climate scenario",
      guide = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = c("Historical", present_yrs),
      labels = c("Historical", present_yrs),
      name   = "Timeframe",
      guide = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::labs(
      title = "Exceedance probability by climate scenario",
      x     = x_label,
      y     = "Annual exceedance probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::coord_flip()

  # --- return period lines ---
  if (isTRUE(return_period)) {
    rp_map <- c("1:5" = 0.20, "1:10" = 0.10, "1:20" = 0.05, "1:50" = 0.02)
    for (nm in names(rp_map)) {
      prob      <- rp_map[nm]
      reliable  <- is.null(n_sim_years) ||
        (!(nm == "1:20" && n_sim_years < 40) &&
         !(nm == "1:50" && n_sim_years < 100))
      rp_label  <- if (reliable) nm else paste0(nm, "*")
      label_col <- if (reliable) "black" else "grey50"
      p <- p +
        ggplot2::geom_hline(yintercept = prob, linetype = "dashed",
                            colour = "black", linewidth = 0.4) +
        ggplot2::annotate("text", x = -Inf, y = prob, label = rp_label,
                          hjust = -0.1, vjust = -0.3,
                          size = 3, colour = label_col)
    }
    if (!is.null(n_sim_years) && n_sim_years < 100) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = 0.02,
        label  = paste0("\u26a0 1:50 estimate unreliable (n = ", n_sim_years, " yrs)"),
        hjust  = 1.05, vjust = 1.5, size = 3, colour = "grey50"
      )
    }
  }
  p
}
