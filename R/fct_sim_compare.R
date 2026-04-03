# ============================================================================ #
# fct_sim_compare.R
#
# Pure comparison and visualisation functions for Module 2.
# Called by mod_2_06_sim_compare_server() only.
#
# Functions:
#   label_agg_method(key)     -- human-readable label for aggregation method
#   label_deviation(key)      -- human-readable label for deviation choice
#   .resolve_year_styles()    -- internal; dynamic linetype map by sorted year
#   build_threshold_table_df()  -- return-period threshold data frame for DT
#   plot_threshold_points()     -- point plot of return-period thresholds (free y-axis)
#   enhance_exceedance()        -- exceedance curve with symmetric return-period lines
#
# NOTE: plot_bar_climate() has been archived to
#   dev/archived_fct/plot_bar_climate_archived.R (no active call sites).
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# Label helpers                                                                #
# ---------------------------------------------------------------------------- #

#' Human-Readable Label for Aggregation Method
#'
#' Converts the internal `agg_method` key used in `selectInput` to a short
#' display string for table headers and the results banner.
#'
#' @param key Character. One of `"mean"`, `"median"`, `"headcount_ratio"`,
#'   `"gap"`, `"fgt2"`, `"gini"`.
#' @return A character string.
#' @export
label_agg_method <- function(key) {
  switch(key,
    mean            = "Mean",
    median          = "Median",
    total           = "Total",
    headcount_ratio = "Poverty rate",
    gap             = "Poverty gap",
    fgt2            = "Poverty severity",
    gini            = "Gini coefficient",
    key   # fallback: return key unchanged
  )
}

#' Human-Readable Label for Deviation Choice
#'
#' Converts the internal `deviation` key to a short display string.
#'
#' @param key Character. One of `"none"`, `"mean"`, `"median"`.
#' @return A character string.
#' @export
label_deviation <- function(key) {
  switch(key,
    none   = "raw value",
    mean   = "deviation from mean year",
    median = "deviation from median year",
    key
  )
}

# ---- Internal helpers: parse scenario key components ----------------------
# Strips the trailing " / P{pct}" suffix before regex-matching so helpers
# work identically on old keys ("SSP2-4.5 / 2050") and new keys
# ("SSP2-4.5 / 2050 / P50").
.normalise_ssp <- function(nm) {
  nm_clean <- sub(" / P[0-9]+$", "", nm)
  m <- regexpr("SSP[0-9]-[0-9.]+", nm_clean)
  if (m == -1L) return(NA_character_)
  raw <- regmatches(nm_clean, m)
  switch(raw,
    "SSP2-4.5" = "SSP2-4.5",
    "SSP3-7.0" = "SSP3-7.0",
    "SSP5-8.5" = "SSP5-8.5",
    raw
  )
}

#' Extract ensemble percentile label from a scenario key.
#' @param nm Character. e.g. "SSP2-4.5 / 2050 / P50".
#' @return e.g. "P50", or NA_character_ for keys without a percentile suffix.
#' @noRd
.parse_percentile <- function(nm) {
  m <- regexpr("P[0-9]+$", nm)
  if (m == -1L) return(NA_character_)
  regmatches(nm, m)
}

# ---- Internal: dynamic year linetype helper --------------------------------
.resolve_year_styles <- function(year_labels) {
  linetypes <- c("solid", "dashed", "dotted", "longdash", "twodash")
  years     <- sort(unique(year_labels))
  n         <- length(years)
  lty       <- linetypes[seq_len(min(n, length(linetypes)))]
  list(
    linetype_map = setNames(lty,         years),
    alpha_map    = setNames(rep(1.0, n), years)
  )
}

# ---- Internal: sort percentile / member labels within one SSP x year group -
# Percentile mode (all labels match "P{digits}"): sorted numerically ascending.
# All-members mode: sorted by ascending mean value within the group.
.sort_pct_labels <- function(pcts, fut_df, ssp, yr) {
  is_pct <- all(grepl("^P[0-9]+$", pcts))
  if (is_pct) {
    nums <- as.integer(sub("^P", "", pcts))
    return(pcts[order(nums)])
  }
  # All-members: sort by mean value ascending
  means <- vapply(pcts, function(p) {
    rows <- fut_df$ssp_short == ssp & fut_df$yr == yr & fut_df$pct_label == p
    if (!any(rows)) return(NA_real_)
    fut_df$mean[rows][1]
  }, numeric(1))
  pcts[order(means, na.last = TRUE)]
}

# ---------------------------------------------------------------------------- #
# Grouped point-range chart (mean + 90% CI + 95% CI)                    #
# ---------------------------------------------------------------------------- #

#' Grouped Point-Range Chart Comparing Scenarios
#'
#' The primary Results tab chart. For each scenario shows:
#'   - A dot at the mean of annual simulated values
#'   - A thick coloured line for the 90% CI (P5-P95)
#'   - A thin grey line for the 95% CI (P2.5-P97.5)
#'   - A dashed grey horizontal reference line at the Historical mean
#'
#' Groups are ordered Historical | spacer | SSP2 years | spacer | SSP3 years |
#' spacer | SSP5 years. Colour follows SSP family; shade follows year rank.
#'
#' @param scenarios Named list; each element is from aggregate_sim_preds().
#' @param hist_agg  Full result of aggregate_sim_preds() for historical.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_x_discrete labs theme_minimal theme
#'   element_blank element_text margin
#' @importFrom colorspace lighten
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @export
plot_pointrange_climate <- function(scenarios, hist_agg,
                                    group_order = "scenario_x_year") {

  stopifnot(is.list(hist_agg), all(c("out", "x_label") %in% names(hist_agg)))

  # ---- summarise one vector into mean + CI bounds --------------------------
  summarise_vals <- function(vals) {
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2) return(NULL)
    data.frame(
      mean  = mean(vals),
      lo95  = as.numeric(stats::quantile(vals, 0.025)),
      hi95  = as.numeric(stats::quantile(vals, 0.975)),
      lo90  = as.numeric(stats::quantile(vals, 0.05)),
      hi90  = as.numeric(stats::quantile(vals, 0.95)),
      stringsAsFactors = FALSE
    )
  }

  # ---- historical summary --------------------------------------------------
  hist_s <- summarise_vals(hist_agg$out$value)
  if (is.null(hist_s)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }
  hist_mean <- hist_s$mean
  hist_s$pt_key     <- "Historical"
  hist_s$colour_key <- "Historical"

  # ---- future scenario summaries ------------------------------------------
  fut_df        <- NULL
  ssp_short_map <- c("SSP2-4.5" = "SSP2", "SSP3-7.0" = "SSP3", "SSP5-8.5" = "SSP5")

  if (length(scenarios) > 0) {
    future_nms <- names(scenarios)[vapply(names(scenarios),
      function(nm) !is.na(.normalise_ssp(nm)), logical(1))]

    if (length(future_nms) > 0) {
      rows <- lapply(future_nms, function(nm) {
        s <- summarise_vals(scenarios[[nm]]$out$value)
        if (is.null(s)) return(NULL)
        ssp_key   <- .normalise_ssp(nm)
        ssp_short <- ssp_short_map[ssp_key] %||% ssp_key
        yr        <- .parse_year(nm)                       # "2020-2040"
        pct       <- .parse_percentile(nm)
        # For "all members" mode pct is NA; extract the member name from the
        # last " / " segment of the display key instead.
        pct_label <- if (!is.na(pct)) pct else {
          parts <- strsplit(nm, " / ", fixed = TRUE)[[1]]
          if (length(parts) >= 3) parts[length(parts)] else nm
        }
        s$pt_key      <- paste0(ssp_short, "\n", yr, "\n", pct_label)
        s$colour_key  <- paste(ssp_key, yr, pct_label, sep = "__")
        s$ssp_key     <- ssp_key
        s$ssp_short   <- ssp_short
        s$yr          <- yr
        s$pct_label   <- pct_label
        s
      })
      fut_df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
    }
  }

  # ---- colour palette: Historical = grey; one shade per SSP x year x member
  # Percentile mode: P10 lighter, P50 = base, P90 darker.
  # All-members mode: evenly spread shades within each SSP x year group.
  pct_lighten_named <- c("P10" = 0.40, "P50" = 0.0, "P90" = -0.25)

  # Canonical member order from fut_df (insertion order preserved)
  pct_order_all <- if (!is.null(fut_df) && nrow(fut_df) > 0)
    unique(fut_df$pct_label) else character(0)
  is_pct_mode <- all(pct_order_all %in% c("P10", "P50", "P90"))

  colour_palette <- c("Historical" = "#808080")
  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    yrs_present <- sort(unique(fut_df$yr))
    n_yrs       <- length(yrs_present)
    yr_lighten  <- if (n_yrs > 1) seq(0.30, 0.0, length.out = n_yrs) else 0.0
    for (ssp in intersect(names(.ssp_colours), unique(fut_df$ssp_key))) {
      base_col <- .ssp_colours[ssp]
      for (i in seq_along(yrs_present)) {
        yr_col    <- colorspace::lighten(base_col, yr_lighten[i])
        pcts_here <- unique(fut_df$pct_label[
          fut_df$ssp_key == ssp & fut_df$yr == yrs_present[i]])
        # Sort so the lighten ramp aligns with x-axis order:
        # numeric order for P{n} mode, ascending mean for all-members mode.
        pcts_here <- .sort_pct_labels(
          pcts_here, fut_df,
          ssp_short_map[ssp] %||% ssp,
          yrs_present[i]
        )
        n_pcts    <- length(pcts_here)
        lighten_vals <- if (all(pcts_here %in% names(pct_lighten_named))) {
          pct_lighten_named[pcts_here]
        } else {
          seq(0.35, -0.25, length.out = max(n_pcts, 1L))
        }
        for (j in seq_along(pcts_here)) {
          ck                 <- paste(ssp, yrs_present[i], pcts_here[j], sep = "__")
          colour_palette[ck] <- colorspace::lighten(yr_col, lighten_vals[j])
        }
      }
    }
  }

  # ---- x-axis factor levels with spacers -----------------------------------
  ordered_levels <- "Historical"
  spacer_ids     <- character(0)

  if (!is.null(fut_df) && nrow(fut_df) > 0) {

    if (isTRUE(group_order == "year_x_scenario")) {
      yrs_present  <- sort(unique(fut_df$yr))
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"), unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (yr_i in yrs_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        for (ssp in ssps_present) {
          pcts_here <- unique(fut_df$pct_label[
            fut_df$ssp_short == ssp & fut_df$yr == yr_i])
          pcts_here <- .sort_pct_labels(pcts_here, fut_df, ssp, yr_i)
          ordered_levels <- c(ordered_levels,
            paste0(ssp, "\n", yr_i, "\n", pcts_here))
        }
      }
    } else {
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"), unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (ssp in ssps_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(fut_df$yr[fut_df$ssp_short == ssp]))
        for (yr_i in ssp_yrs) {
          pcts_here <- unique(fut_df$pct_label[
            fut_df$ssp_short == ssp & fut_df$yr == yr_i])
          pcts_here <- .sort_pct_labels(pcts_here, fut_df, ssp, yr_i)
          ordered_levels <- c(ordered_levels,
            paste0(ssp, "\n", yr_i, "\n", pcts_here))
        }
      }
    }
  }

  # Show axis label on the centre tick of each SSP x year group.
  # Percentile mode: centre = median percentile, flanks blank.
  # All-members mode: centre = middle member (by mean), others show member name only.
  centre_ticks <- character(0)
  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    for (ssp in unique(fut_df$ssp_short)) {
      for (yr_i in unique(fut_df$yr[fut_df$ssp_short == ssp])) {
        grp_pcts <- unique(fut_df$pct_label[
          fut_df$ssp_short == ssp & fut_df$yr == yr_i])
        grp_pcts <- .sort_pct_labels(grp_pcts, fut_df, ssp, yr_i)
        mid <- grp_pcts[ceiling(length(grp_pcts) / 2)]
        centre_ticks <- c(centre_ticks, paste0(ssp, "\n", yr_i, "\n", mid))
      }
    }
  }

  x_label_map <- setNames(
    vapply(ordered_levels, function(lv) {
      if (lv %in% spacer_ids) return("")
      if (lv == "Historical")  return(lv)
      if (is_pct_mode) {
        # Percentile mode: show SSP+period on centre tick, blank on all flanks
        if (lv %in% centre_ticks)          return(sub("\n[^\n]+$", "", lv))
        if (grepl("\nP[0-9]+$", lv))       return("")
        return(lv)
      } else {
        # All-members: show SSP+period on centre tick, member name on others
        if (lv %in% centre_ticks) {
          parts <- strsplit(lv, "\n")[[1]]
          return(paste(parts[1], parts[2], sep = "\n"))
        }
        parts <- strsplit(lv, "\n")[[1]]
        return(if (length(parts) >= 3) parts[3] else lv)
      }
    }, character(1)),
    ordered_levels
  )
  data_levels <- setdiff(ordered_levels, spacer_ids)

  # ---- combine into one data frame -----------------------------------------
  hist_row <- hist_s[, c("pt_key", "colour_key", "mean", "lo90", "hi90", "lo95", "hi95")]
  fut_rows <- if (!is.null(fut_df) && nrow(fut_df) > 0)
    fut_df[, c("pt_key", "colour_key", "mean", "lo90", "hi90", "lo95", "hi95")] else NULL

  plot_df <- dplyr::bind_rows(hist_row, fut_rows)
  plot_df$pt_key <- factor(plot_df$pt_key, levels = ordered_levels)
  plot_df <- plot_df[plot_df$pt_key %in% data_levels, ]

  if (nrow(plot_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a future simulation to see scenario comparisons."))

  # ---- plot ----------------------------------------------------------------
  # Per-point size and linewidth encoding: P50 full; P10/P90 smaller/thinner
  pct_size_map      <- c("P10" = 1.8, "P50" = 3.0, "P90" = 1.8, "Historical" = 3.0)
  pct_linewidth_map <- c("P10" = 1.2, "P50" = 2.0, "P90" = 1.2, "Historical" = 2.0)

  get_pct <- function(lv) {
    m <- regexpr("P[0-9]+$", lv)
    if (m > 0) regmatches(lv, m) else "Historical"
  }

  default_sz <- if (is_pct_mode) 2.2 else 2.2
  default_lw <- if (is_pct_mode) 1.4 else 1.4
  plot_df$pt_size <- vapply(as.character(plot_df$pt_key), function(k) {
    v <- pct_size_map[get_pct(k)]; if (!is.na(v)) v else default_sz
  }, numeric(1))
  plot_df$pt_lw   <- vapply(as.character(plot_df$pt_key), function(k) {
    v <- pct_linewidth_map[get_pct(k)]; if (!is.na(v)) v else default_lw
  }, numeric(1))

  ggplot2::ggplot(plot_df,
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key)
  ) +
    # 95% CI  thin, grey underlay
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
      linewidth = 0.5, colour = "grey70"
    ) +
    # 90% CI — linewidth encodes percentile
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo90, ymax = .data$hi90,
                   linewidth = I(.data$pt_lw))
    ) +
    # Mean dot — size encodes percentile
    ggplot2::geom_point(
      ggplot2::aes(y = .data$mean, size = I(.data$pt_size)),
      shape = 21, fill = "white", stroke = 1.4
    ) +
    # Historical mean reference line
    ggplot2::geom_hline(
      yintercept = hist_mean, linetype = "dashed",
      colour = "#808080", linewidth = 0.55
    ) +
    ggplot2::scale_colour_manual(values = colour_palette, guide = "none") +
    ggplot2::scale_x_discrete(
      limits = ordered_levels,
      labels = x_label_map,
      drop   = FALSE
    ) +
    ggplot2::labs(
      title = NULL,
      x     = NULL,
      y     = hist_agg$x_label
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = "none"
    )
}

# ---------------------------------------------------------------------------- #
# Threshold table data frame                                                   #
# ---------------------------------------------------------------------------- #

#' Build Return-Period Threshold Table Data Frame
#'
#' Pure function that converts a named list of aggregated series (output of
#' \code{aggregate_sim_preds()}) into a sorted data frame suitable for
#' \code{DT::datatable()}. Replaces the inline wrangling that previously
#' lived inside \code{renderDT} in mod_2_06_sim_compare_server().
#'
#' @param all_series  Named list. Each element must have \code{$out$value}.
#'   Typically \code{c(list(Historical = agg_hist), agg_scenarios)}.
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"}.
#'
#' @return A data frame with columns: Scenario, Obs, one column per
#'   return-period threshold label, sorted by \code{group_order}. Returns
#'   NULL when every series has insufficient data.
#'
#' @importFrom stats quantile median
#' @export
build_threshold_table_df <- function(all_series, group_order = "scenario_x_year") {

  rows <- lapply(names(all_series), function(nm) {
    vals <- all_series[[nm]]$out$value
    vals <- vals[is.finite(vals)]
    n    <- length(vals)

    keep_low  <- names(RP_LOW)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
    keep_high <- names(RP_HIGH)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
    if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

    low_df <- if (length(keep_low) > 0)
      as.data.frame(t(sapply(keep_low,  function(th) round(stats::quantile(vals, RP_LOW[th]),  3))))
    else data.frame()
    high_df <- if (length(keep_high) > 0)
      as.data.frame(t(sapply(keep_high, function(th) round(stats::quantile(vals, RP_HIGH[th]), 3))))
    else data.frame()

    names(low_df)  <- keep_low
    names(high_df) <- keep_high
    median_df <- data.frame("1:1" = round(stats::median(vals), 3), check.names = FALSE)

    cbind(data.frame(Scenario = nm, Obs = n, check.names = FALSE),
          low_df, median_df, high_df)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
    r[all_cols]
  })
  df <- do.call(rbind, rows)

  # Sort by group_order: Historical rows first, then SSP rows.
  # SSP rows carry a " / P{pct}" suffix — extract SSP, year, and percentile
  # for clean three-level sorting (P10 < P50 < P90 within each SSP/year group).
  hist_rows <- df[df$Scenario == "Historical" | !grepl("^SSP", df$Scenario), , drop = FALSE]
  ssp_rows  <- df[grepl("^SSP", df$Scenario), , drop = FALSE]

  if (nrow(ssp_rows) > 0) {
    ssp_rows$ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
    # Use full YYYY-YYYY range for sort (lexicographic is correct for year ranges)
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows$Scenario)
    ssp_rows$yr_sort  <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows$Scenario, yr_m),
      regmatches(ssp_rows$Scenario, regexpr("[0-9]{4}", ssp_rows$Scenario))
    )
    pct_m <- regexpr("P([0-9]+)$", ssp_rows$Scenario)
    ssp_rows$pct_sort <- ifelse(
      pct_m > 0,
      as.integer(regmatches(ssp_rows$Scenario, regexpr("[0-9]+$", ssp_rows$Scenario))),
      50L
    )
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario"))
      ssp_rows[order(ssp_rows$yr_sort, ssp_rows$ssp_sort, ssp_rows$pct_sort), ]
    else
      ssp_rows[order(ssp_rows$ssp_sort, ssp_rows$yr_sort, ssp_rows$pct_sort), ]
    ssp_rows$ssp_sort <- NULL
    ssp_rows$yr_sort  <- NULL
    ssp_rows$pct_sort <- NULL
  }

  rbind(hist_rows, ssp_rows)
}



# ---------------------------------------------------------------------------- #
# Threshold point chart                                                        #
# ---------------------------------------------------------------------------- #

#' Threshold Point Chart Across Scenarios
#'
#' Scatter plot showing the simulated outcome value at each return-period
#' threshold for every scenario. Points are dodged per threshold; the y-axis
#' is free-scaled so small differences between scenarios are visible.
#' The median (1:1) threshold is highlighted with a larger filled point.
#' Colour follows the SSP palette; Historical is grey.
#'
#' @param all_series  Named list. Each element must have \code{$out$alue}.
#'   Same input as \code{build_threshold_table_df()}.
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"}. Controls scenario order in the legend.
#'
#' @return A ggplot object, or a blank placeholder when data are insufficient.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_colour_manual
#'   scale_size_manual scale_shape_manual labs theme_minimal theme
#'   element_text element_blank element_line
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_threshold_points <- function(all_series, group_order = "scenario_x_year") {

  df <- build_threshold_table_df(all_series, group_order)
  if (is.null(df) || nrow(df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Insufficient data for threshold chart."))

  thresh_cols <- setdiff(names(df), c("Scenario", "Obs"))
  if (length(thresh_cols) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "No threshold columns found."))

  # Pivot to long: one row per Scenario x threshold.
  long <- tidyr::pivot_longer(
    df,
    cols      = tidyr::all_of(thresh_cols),
    names_to  = "threshold",
    values_to = "value"
  )
  long$threshold <- factor(long$threshold, levels = thresh_cols)
  long$Scenario  <- factor(long$Scenario,  levels = unique(df$Scenario))

  # Median column: large filled circle; all others: smaller open circle.
  long$is_median <- long$threshold == "1:1"

  # ---- Colour palette: mirrors plot_pointrange_climate() -----------------
  scenarios_present <- levels(long$Scenario)
  colour_map <- vapply(scenarios_present, function(nm) {
    ssp <- .normalise_ssp(nm)
    if (!is.na(ssp) && ssp %in% names(.ssp_colours)) .ssp_colours[ssp] else "#808080"
  }, character(1))

  # ---- Plot --------------------------------------------------------------
  ggplot2::ggplot(long,
    ggplot2::aes(
      x      = .data$threshold,
      y      = .data$value,
      colour = .data$Scenario,
      size   = .data$is_median,
      shape  = .data$is_median,
      group  = .data$Scenario
    )
  ) +
    ggplot2::geom_line(linewidth = 0.4, alpha = 0.5, na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = colour_map, name = "Scenario") +
    # Median: large filled circle (16); others: smaller open circle (1)
    ggplot2::scale_size_manual(
      values = c("TRUE" = 3.5, "FALSE" = 2.0),
      guide  = "none"
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 16L, "FALSE" = 1L),
      guide  = "none"
    ) +
    ggplot2::labs(
      title = "Outcome value at return-period thresholds",
      x     = "Return period threshold",
      y     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = "bottom"
    )
}

# ---------------------------------------------------------------------------- #
# Enhanced exceedance curve                                                    #
# ---------------------------------------------------------------------------- #

#' Enhanced Exceedance Probability Curve with Return Period Axis
#'
#' Colour = climate scenario (SSP family). Line type = future period (year
#' range). Line width = ensemble percentile (P50 thick, P10/P90 thin).
#' Historical = black/solid/medium. Optional logit probability axis.
#'
#' Each SSP x period x percentile combination is rendered as its own curve,
#' giving a full visual separation of the three ensemble spread lines per
#' scenario/period combination.
#'
#' @param scenarios     Named list from `aggregate_sim_preds()`.
#' @param hist_agg      Historical aggregate from `aggregate_sim_preds()`.
#' @param x_label       Axis label for the welfare outcome.
#' @param return_period Logical. Show return period lines. Default TRUE.
#' @param n_sim_years   Integer. Triggers reliability annotation.
#' @param logit_x       Logical. Use logit scale on the probability axis to
#'   emphasise both tails symmetrically. Default FALSE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   scale_linewidth_manual coord_flip guide_legend element_text
#' @importFrom scales logit_trans
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
enhance_exceedance <- function(scenarios,
                               hist_agg,
                               x_label,
                               return_period = TRUE,
                               n_sim_years   = NULL,
                               logit_x       = FALSE) {

  stopifnot(is.list(scenarios), length(scenarios) > 0)
  labels <- names(scenarios)

  # ---- Build per-group long data frame ------------------------------------
  long_df <- dplyr::bind_rows(lapply(seq_along(scenarios), function(i) {
    nm      <- labels[i]
    vals    <- scenarios[[nm]]$out$value
    ssp_key <- .normalise_ssp(nm)
    yr      <- .parse_year(nm)
    pct_raw <- .parse_percentile(nm)
    # For all-members mode, extract member name from the last " / " segment
    pct <- if (!is.na(pct_raw)) {
      pct_raw
    } else if (!is.na(ssp_key)) {
      parts <- strsplit(nm, " / ", fixed = TRUE)[[1]]
      if (length(parts) >= 3) parts[length(parts)] else "Member"
    } else {
      NA_character_
    }
    data.frame(
      value   = vals,
      group   = nm,
      ssp_key = if (is.na(ssp_key)) "Historical" else ssp_key,
      yr      = if (is.na(yr))      "Historical" else yr,
      pct     = if (is.na(pct))     "Historical" else pct,
      stringsAsFactors = FALSE
    )
  }))
  long_df$group <- factor(long_df$group, levels = labels)

  # ---- Aesthetic mappings -------------------------------------------------
  fut_yr_labels <- sort(unique(long_df$yr[long_df$yr != "Historical"]))
  yr_styles     <- .resolve_year_styles(fut_yr_labels)

  present_ssps   <- sort(unique(long_df$ssp_key[long_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = "black",
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )

  present_yrs  <- names(yr_styles$linetype_map)
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  # Linewidth/alpha encodes the percentile or member dimension.
  # Percentile mode (P10/P50/P90): P50 full weight, flanks narrow and faded.
  # All-members mode: uniform linewidth, evenly spread alpha.
  present_pcts    <- unique(long_df$pct[long_df$pct != "Historical"])
  is_pct_mode_exc <- all(present_pcts %in% c("P10", "P50", "P90"))

  if (is_pct_mode_exc) {
    lw_map_pct    <- c("Historical" = 0.9, "P10" = 0.45, "P50" = 0.9, "P90" = 0.45)
    alpha_map_pct <- c("Historical" = 1.0, "P10" = 0.55, "P50" = 1.0, "P90" = 0.55)
  } else {
    n_m           <- length(present_pcts)
    alphas        <- if (n_m > 1) seq(0.40, 0.90, length.out = n_m) else 0.75
    lw_map_pct    <- c(setNames(rep(0.65, n_m), present_pcts), "Historical" = 0.9)
    alpha_map_pct <- c(setNames(alphas,          present_pcts), "Historical" = 1.0)
  }

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)
  eps       <- if (isTRUE(logit_x)) 0.005 else 0

  # ---- Build ECDF per group -----------------------------------------------
  ecdf_df <- dplyr::bind_rows(lapply(levels(long_df$group), function(grp) {
    sub    <- long_df[long_df$group == grp & is.finite(long_df$value), ]
    if (nrow(sub) == 0) return(NULL)
    fn     <- stats::ecdf(sub$value)
    xs     <- sort(unique(sub$value))
    exceed <- 1 - fn(xs)
    keep   <- exceed > eps & exceed < (1 - eps)
    if (!any(keep)) return(NULL)
    data.frame(
      value   = xs[keep],
      exceed  = exceed[keep],
      group   = grp,
      ssp_key = sub$ssp_key[1],
      yr      = sub$yr[1],
      pct     = sub$pct[1],
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(ecdf_df) || nrow(ecdf_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  ecdf_df$alpha <- alpha_map_pct[ecdf_df$pct]
  ecdf_df$lw    <- lw_map_pct[ecdf_df$pct]

  # ---- Build percentile / member legend labels ---------------------------
  pct_legend_vals  <- if (is_pct_mode_exc)
    intersect(c("P10", "P50", "P90"), present_pcts)
  else
    present_pcts   # show all member names
  pct_lw_legend    <- lw_map_pct[c("Historical",   pct_legend_vals)]
  pct_alpha_legend <- alpha_map_pct[c("Historical", pct_legend_vals)]

  ann_y <- if (isTRUE(logit_x)) 0.97 else 0.95

  # ---- Plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(
    ecdf_df,
    ggplot2::aes(
      x         = value,
      y         = exceed,
      colour    = ssp_key,
      linetype  = yr,
      linewidth = pct,
      alpha     = I(.data$alpha),
      group     = group
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      xintercept = hist_mean, linetype = "dotted",
      colour = "black", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = hist_mean, y = ann_y,
      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
      size = 2.8, colour = "grey30"
    ) +
    ggplot2::scale_color_manual(
      values = colour_map_ssp,
      breaks = c("Historical", present_ssps),
      labels = c("Historical", present_ssps),
      name   = "Climate scenario",
      guide  = ggplot2::guide_legend(order = 1,
                                     override.aes = list(linewidth = 0.9,
                                                         alpha = 1))
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = c("Historical", present_yrs),
      labels = c("Historical", present_yrs),
      name   = "Period",
      guide  = ggplot2::guide_legend(order = 2,
                                     override.aes = list(linewidth = 0.9,
                                                         alpha = 1))
    ) +
    ggplot2::scale_linewidth_manual(
      values = lw_map_pct,
      breaks = c("Historical", pct_legend_vals),
      labels = c("Historical", pct_legend_vals),
      name   = "Ensemble percentile",
      guide  = if (length(pct_legend_vals) > 0)
        ggplot2::guide_legend(order = 3, override.aes = list(alpha = 1))
      else "none"
    ) +
    ggplot2::labs(
      title = "Exceedance probability by climate scenario",
      x     = x_label,
      y     = "Annual exceedance probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12,
                                              hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::coord_flip()

  # ---- Return period lines (both tails, symmetric) -----------------------
  if (isTRUE(return_period)) {
    rp_all <- c(RP_LOW, RP_HIGH)
    for (nm in names(rp_all)) {
      prob      <- rp_all[nm]
      reliable  <- is.null(n_sim_years) ||
        (!(nm == "1:20" && n_sim_years < 40) &&
         !(nm == "1:50" && n_sim_years < 100))
      rp_label  <- if (reliable) nm else paste0(nm, "*")
      label_col <- if (reliable) "grey40" else "grey65"
      p <- p +
        ggplot2::geom_hline(
          yintercept = prob, linetype = "dashed",
          colour = "grey60", linewidth = 0.35
        ) +
        ggplot2::annotate(
          "text", x = -Inf, y = prob, label = rp_label,
          hjust = -0.1, vjust = -0.3, size = 2.8, colour = label_col
        )
    }
    if (!is.null(n_sim_years) && n_sim_years < 100) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = 0.02,
        label  = paste0("\u26a0 unreliable (n = ", n_sim_years, " yrs)"),
        hjust  = 1.05, vjust = 1.5, size = 2.8, colour = "grey50"
      )
    }
  }

  # ---- Optional logit probability axis -----------------------------------
  if (isTRUE(logit_x)) {
    logit_breaks <- c(unname(RP_LOW), 0.50, rev(1 - unname(RP_LOW)))
    logit_labels <- c(names(RP_LOW), "Median", rev(names(RP_HIGH)))
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::logit_trans(),
      breaks = logit_breaks,
      labels = logit_labels,
      limits = c(0.005, 0.995)
    )
  }
  p
}
