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
#   plot_pointrange_climate() -- HERO: grouped point-range chart (SSP x timeframe)
#   build_impact_table()      -- return-period threshold table
#   enhance_exceedance()      -- exceedance curve with symmetric return-period lines
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
    headcount_ratio = "Poverty headcount ratio",
    gap             = "Poverty gap",
    fgt2            = "FGT2 severity index",
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
# HERO: Grouped point-range chart (mean + 90% CI + 95% CI)                    #
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
        yr        <- as.integer(.parse_year(nm))
        s$pt_key      <- paste0(ssp_short, "\n", yr)
        s$colour_key  <- paste(ssp_key, as.character(yr), sep = "__")
        s$ssp_key     <- ssp_key
        s$ssp_short   <- ssp_short
        s$yr          <- as.character(yr)
        s
      })
      fut_df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
    }
  }

  # ---- colour palette: Historical = grey; SSPs with year lightness ----------
  colour_palette <- c("Historical" = "#808080")
  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    yrs_present <- sort(unique(as.integer(fut_df$yr)))
    n_yrs       <- length(yrs_present)
    light_seq   <- if (n_yrs > 1) seq(0.45, 0.0, length.out = n_yrs) else 0.0
    for (ssp in intersect(names(.ssp_colours), unique(fut_df$ssp_key))) {
      base_col <- .ssp_colours[ssp]
      for (i in seq_along(yrs_present)) {
        ck                 <- paste(ssp, yrs_present[i], sep = "__")
        colour_palette[ck] <- colorspace::lighten(base_col, light_seq[i])
      }
    }
  }

  # ---- x-axis factor levels with spacers -----------------------------------
  ordered_levels <- "Historical"
  spacer_ids     <- character(0)

  if (!is.null(fut_df) && nrow(fut_df) > 0) {

    if (isTRUE(group_order == "year_x_scenario")) {
      # Outer: year; inner: SSP
      yrs_present  <- sort(unique(as.integer(fut_df$yr)))
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"), unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (yr_i in yrs_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        for (ssp in ssps_present) {
          if (any(fut_df$ssp_short == ssp & fut_df$yr == as.character(yr_i))) {
            ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
          }
        }
      }
    } else {
      # Default: Scenario x Year — outer: SSP; inner: year
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"), unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (ssp in ssps_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(as.integer(fut_df$yr[fut_df$ssp_short == ssp])))
        for (yr_i in ssp_yrs) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    }
  }

  x_label_map <- setNames(
    ifelse(ordered_levels %in% spacer_ids, "", ordered_levels),
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
  ggplot2::ggplot(plot_df,
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key)
  ) +
    # 95% CI  thin, grey underlay
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
      linewidth = 0.5, colour = "grey70"
    ) +
    # 90% CI  thicker, scenario colour
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo90, ymax = .data$hi90),
      linewidth = 2.0
    ) +
    # Mean dot  scenario colour, white centre
    ggplot2::geom_point(
      ggplot2::aes(y = .data$mean),
      size = 3.0, shape = 21,
      fill = "white", stroke = 1.4
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
                               thresholds              = c("mean", "1:5", "1:10", "1:20", "1:50"),
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
#' Colour = climate scenario (fixed palette). Line type = timeframe (dynamic).
#' Historical = black/solid. Optional log10 probability axis.
#'
#' @param scenarios     Named list from `aggregate_sim_preds()`.
#' @param hist_agg      Historical aggregate from `aggregate_sim_preds()`.
#' @param x_label       Axis label for the welfare outcome.
#' @param return_period Logical. Show return period lines. Default TRUE.
#' @param n_sim_years   Integer. Triggers reliability annotation.
#' @param logit_x       Logical. Use logit scale on the probability axis to
#'   emphasise both tails symmetrically. Default FALSE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes stat_ecdf geom_vline geom_hline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   after_stat coord_flip guide_legend element_text
#' @importFrom scales label_percent logit_trans
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

  fut_yr_labels <- sort(unique(long_df$yr[long_df$yr != "Historical"]))
  yr_styles     <- .resolve_year_styles(fut_yr_labels)

  present_ssps   <- sort(unique(long_df$ssp_key[long_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = "black",
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )
  present_yrs  <- names(yr_styles$linetype_map)
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)


  # Logit limits: keep exceedance strictly within (eps, 1-eps) so the
  # logit transform never hits -Inf / +Inf. Log10 only needs > 0.
  eps <- if (isTRUE(logit_x)) 0.005 else 0

  ecdf_df <- dplyr::bind_rows(lapply(levels(long_df$group), function(grp) {
    sub    <- long_df[long_df$group == grp & is.finite(long_df$value), ]
    if (nrow(sub) == 0) return(NULL)
    fn     <- stats::ecdf(sub$value)
    xs     <- sort(unique(sub$value))
    exceed <- 1 - fn(xs)
    keep   <- exceed > eps & exceed < (1 - eps)
    # Guard: when all observations share one value (e.g. band = 1 year),
    # ecdf() returns exceed = 0 for the single unique point, so keep is all
    # FALSE. The data.frame() call below would then have 0-length vectors
    # mixed with 1-length scalars, crashing with 'differing number of rows'.
    if (!any(keep)) return(NULL)
    data.frame(
      value            = xs[keep],
      exceed           = exceed[keep],
      group            = grp,
      ssp_key          = sub$ssp_key[1],
      yr               = sub$yr[1],
      stringsAsFactors = FALSE
    )
  }))

  # annotation y-position: use 0.95 on linear scale, 0.97 on logit (avoid clipping)
  ann_y <- if (isTRUE(logit_x)) 0.97 else 0.95

  p <- ggplot2::ggplot(
    ecdf_df, ggplot2::aes(x = value, y = exceed, colour = ssp_key, linetype = yr)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = hist_mean, linetype = "dotted",
                        colour = "black", linewidth = 0.5) +
    ggplot2::annotate("text", x = hist_mean, y = ann_y,
                      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
                      size = 2.8, colour = "grey30") +
    ggplot2::scale_color_manual(
      values = colour_map_ssp,
      breaks = c("Historical", present_ssps),
      labels = c("Historical", present_ssps),
      name   = "Climate scenario",
      guide  = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = c("Historical", present_yrs),
      labels = c("Historical", present_yrs),
      name   = "Timeframe",
      guide  = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::labs(
      title = "Exceedance probability by climate scenario",
      x     = x_label,
      y     = "Annual exceedance probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(size = 10, colour = "grey40"),
      legend.position = "bottom"
    ) +
    ggplot2::coord_flip()

  # --- return period lines (both tails, symmetric) ---
  if (isTRUE(return_period)) {
    # RP_LOW / RP_HIGH are defined in fct_simulations.R and cover both tails.
    rp_all <- c(RP_LOW, RP_HIGH)

    for (nm in names(rp_all)) {
      prob <- rp_all[nm]
      # Reliability check only applies to the rare low-probability lines
      reliable  <- is.null(n_sim_years) ||
        (!(nm == "1:20" && n_sim_years < 40) &&
         !(nm == "1:50" && n_sim_years < 100))
      rp_label  <- if (reliable) nm else paste0(nm, "*")
      label_col <- if (reliable) "grey40" else "grey65"
      p <- p +
        ggplot2::geom_hline(yintercept = prob, linetype = "dashed",
                            colour = "grey60", linewidth = 0.35) +
        ggplot2::annotate("text", x = -Inf, y = prob, label = rp_label,
                          hjust = -0.1, vjust = -0.3, size = 2.8, colour = label_col)
    }
    if (!is.null(n_sim_years) && n_sim_years < 100) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = 0.02,
        label  = paste0("\u26a0 unreliable (n = ", n_sim_years, " yrs)"),
        hjust  = 1.05, vjust = 1.5, size = 2.8, colour = "grey50"
      )
    }
  }

  # --- optional logit probability axis (applied to y after coord_flip) ---
  # Emphasises both tails symmetrically. ecdf_df is pre-clipped to (eps, 1-eps).
  if (isTRUE(logit_x)) {
    # Logit breaks align with RP_LOW / RP_HIGH probability values
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
