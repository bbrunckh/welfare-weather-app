# ============================================================================ #
# fct_sim_compare.R
#
# Pure comparison and visualisation functions for Module 2.
# Called by mod_2_02_results.R
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
    prosperity_gap  = "Prosperity gap",
    avg_poverty     = "Average poverty",
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

# ---------------------------------------------------------------------------- #
# Band quantile resolver                                                       #
# ---------------------------------------------------------------------------- #

#' Resolve Uncertainty Band Key to Quantile Pair
#'
#' Converts the UI uncertainty band selector key to a named numeric vector
#' used by plot_pointrange_climate() and summarise_vals().
#'
#' @param band_key Character. One of "p25_p75", "p20_p80", "p10_p90",
#'   "p05_p95", "p025_p975", "p005_p995", "minmax".
#' @return Named numeric vector c(lo = ..., hi = ...).
#' @export
resolve_band_q <- function(band_key) {
  switch(band_key,
    p25_p75   = c(lo = 0.25,  hi = 0.75),
    p20_p80   = c(lo = 0.20,  hi = 0.80),
    p10_p90   = c(lo = 0.10,  hi = 0.90),
    p05_p95   = c(lo = 0.05,  hi = 0.95),
    p025_p975 = c(lo = 0.025, hi = 0.975),
    p005_p995 = c(lo = 0.005, hi = 0.995),
    minmax    = c(lo = 0.00,  hi = 1.00),
    c(lo = 0.10, hi = 0.90)   # default fallback
  )
}

# ---- Internal helpers: parse scenario key components ----------------------
# Keys are of the form "SSP2-4.5 / 2025-2035".
.normalise_ssp <- function(nm) {
  m <- regexpr("SSP[0-9]-[0-9.]+", nm)
  if (m == -1L) return(NA_character_)
  raw <- regmatches(nm, m)
  switch(raw,
    "SSP2-4.5" = "SSP2-4.5",
    "SSP3-7.0" = "SSP3-7.0",
    "SSP5-8.5" = "SSP5-8.5",
    raw
  )
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

# ---------------------------------------------------------------------------- #
# Shared CI summary helper                                                     #
# ---------------------------------------------------------------------------- #

# plot_pointrange_climate uses coef_lo/coef_hi from draw_values instead.
.summarise_vals <- function(x, band_q = c(lo = 0.10, hi = 0.90)) {
  if (length(x) == 0L || all(is.na(x))) return(NULL)
  list(
    mean    = mean(x, na.rm = TRUE),
    lo_full = unname(quantile(x, band_q[["lo"]], na.rm = TRUE)),
    hi_full = unname(quantile(x, band_q[["hi"]], na.rm = TRUE))
  )
}

# Decompose a scenario's aggregated $out tibble into three uncertainty sources.
# Returns a named list:
#   $total  — all N_models × N_years values (combined)
#   $annual — model-averaged annual means   (N_years values)
#   $model  — year-averaged model means     (N_models values; NULL for historical)
.decompose_scenario_uncertainty <- function(out_df) {
  has_model <- "model" %in% names(out_df)
  # Coefficient uncertainty: when draw_id is present, use draw-level value_p50
  # (the median across draws) as the per-draw aggregate, then spread across
  # draw_id gives the coefficient uncertainty distribution.
  has_coef  <- all(c("value_p05", "value_p50", "value_p95") %in% names(out_df))
  total     <- out_df$value
  annual    <- if (has_model)
    as.numeric(tapply(out_df$value, out_df$sim_year, mean, na.rm = TRUE))
  else total
  model_means <- if (has_model)
    as.numeric(tapply(out_df$value, out_df$model, mean, na.rm = TRUE))
  else NULL
  # Coefficient uncertainty band: p05 and p95 are already aggregated scalars
  # per (sim_year [, model]) from aggregate_sim_preds() Stage 2.
  coef_lo <- if (has_coef) out_df$value_p05 else NULL
  coef_hi <- if (has_coef) out_df$value_p95 else NULL
  list(total = total, annual = annual, model = model_means,
       coef_lo = coef_lo, coef_hi = coef_hi)
}

# ---------------------------------------------------------------------------- #
# Grouped point-range chart (mean + 90% CI + 95% CI)                    #
# ---------------------------------------------------------------------------- #

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
                                    group_order = "scenario_x_year",
                                    coef_bands_tbl = NULL,
                                    band_q = c(lo = 0.1, hi = 0.9),
                                    ensemble_band_q = c(lo = 0, hi = 1),
                                    hist_ref = 0) {

  stopifnot(is.list(hist_agg), all(c("out", "x_label") %in% names(hist_agg)))


  # ---- historical summary --------------------------------------------------
  hist_s <- .summarise_vals(hist_agg$out$value, c(lo = 0, hi = 1)) #Historical Band always full range
  if (is.null(hist_s)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }
  hist_mean <- hist_s$mean
  hist_s$pt_key     <- "Historical"
  hist_s$colour_key <- "Historical"
  hist_bands <- if (!is.null(coef_bands_tbl))
  dplyr::filter(coef_bands_tbl, scenario == "Historical") else NULL
  hist_s$coef_lo <- if (!is.null(hist_bands) && nrow(hist_bands) > 0)
    mean(hist_bands$value_lo, na.rm = TRUE) else NA_real_
  hist_s$coef_hi <- if (!is.null(hist_bands) && nrow(hist_bands) > 0)
    mean(hist_bands$value_hi, na.rm = TRUE) else NA_real_


  # ---- future scenario summaries ------------------------------------------
  # Each scenario key is "SSP2-4.5 / 2025-2035" (one entry per SSP x period,
  # with all ensemble model predictions already pooled in the preds data frame).
  fut_df        <- NULL


  if (length(scenarios) > 0) {
    future_nms <- names(scenarios)[vapply(names(scenarios),
      function(nm) !is.na(.normalise_ssp(nm)), logical(1))]

    if (length(future_nms) > 0) {
      rows <- lapply(future_nms, function(nm) {
        out_df <- scenarios[[nm]]$out

        is_future <- !is.null(out_df$value_all) &&
                    any(vapply(out_df$value_all, length, integer(1L)) > 1L)

        s <- if (is_future) {
          trimmed <- unlist(lapply(out_df$value_all, function(yr_vals) {
            lo <- quantile(yr_vals, ensemble_band_q[["lo"]], na.rm = TRUE)
            hi <- quantile(yr_vals, ensemble_band_q[["hi"]], na.rm = TRUE)
            yr_vals[yr_vals >= lo & yr_vals <= hi]
          }))
          s <- .summarise_vals(trimmed, c(lo = 0, hi = 1))
          # Apply deviation shift to thick bar edges — hist_ref = 0 when no deviation
          s$lo_full <- s$lo_full - hist_ref
          s$hi_full <- s$hi_full - hist_ref
          s
        } else {
          .summarise_vals(out_df$value, band_q)
        }

        if (is.null(s)) return(NULL)

        if (is_future) {
          if (!is.null(coef_bands_tbl)) {
            sc_bands  <- dplyr::filter(coef_bands_tbl, scenario == nm)
            s$coef_lo <- if (nrow(sc_bands) > 0)
              mean(sc_bands$value_lo, na.rm = TRUE) else NA_real_
            s$coef_hi <- if (nrow(sc_bands) > 0)
              mean(sc_bands$value_hi, na.rm = TRUE) else NA_real_
            s$mean    <- mean(unlist(out_df$value_all), na.rm = TRUE) - hist_ref
          } else {
            s$coef_lo <- NA_real_
            s$coef_hi <- NA_real_
            s$mean    <- mean(unlist(out_df$value_all), na.rm = TRUE) - hist_ref
          }
        } else {
          sc_bands  <- if (!is.null(coef_bands_tbl))
            dplyr::filter(coef_bands_tbl, scenario == nm) else NULL
          s$coef_lo <- if (!is.null(sc_bands) && nrow(sc_bands) > 0)
            mean(sc_bands$value_lo, na.rm = TRUE) else NA_real_
          s$coef_hi <- if (!is.null(sc_bands) && nrow(sc_bands) > 0)
            mean(sc_bands$value_hi, na.rm = TRUE) else NA_real_
        }

        ssp_key       <- .normalise_ssp(nm)
        ssp_short     <- SSP_SHORT_LABELS[ssp_key] %||% ssp_key
        yr            <- .parse_year(nm)
        s$pt_key      <- paste0(ssp_short, "\n", yr)
        s$colour_key  <- paste(ssp_key, yr, sep = "__")
        s$ssp_key     <- ssp_key
        s$ssp_short   <- ssp_short
        s$yr          <- yr
        s
      })
      fut_df <- dplyr::bind_rows(Filter(Negate(is.null), rows))
    }
  }

  # ---- colour palette: Historical = grey; one colour per SSP x year --------
  colour_palette <- c("Historical" = "#808080")
  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    yrs_present <- sort(unique(fut_df$yr))
    n_yrs       <- length(yrs_present)
    yr_lighten  <- if (n_yrs > 1) seq(0.30, 0.0, length.out = n_yrs) else 0.0
    for (ssp in intersect(names(.ssp_colours), unique(fut_df$ssp_key))) {
      base_col <- .ssp_colours[ssp]
      for (i in seq_along(yrs_present)) {
        ck <- paste(ssp, yrs_present[i], sep = "__")
        colour_palette[ck] <- colorspace::lighten(base_col, yr_lighten[i])
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
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
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
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    }
  }

  x_label_map <- setNames(
    vapply(ordered_levels, function(lv) {
      if (lv %in% spacer_ids) return("")
      if (lv == "Historical")  return(lv)
      lv  # SSP\nyear label shown as-is
    }, character(1)),
    ordered_levels
  )
  data_levels <- setdiff(ordered_levels, spacer_ids)

  # ---- combine into one data frame -----------------------------------------
  cols     <- c("pt_key", "colour_key", "mean", "lo_full", "hi_full", "coef_lo", "coef_hi")
  hist_row <- as.data.frame(hist_s)[, intersect(cols, names(hist_s)), drop = FALSE]
  fut_rows <- if (!is.null(fut_df) && nrow(fut_df) > 0)
    as.data.frame(fut_df)[, intersect(cols, names(fut_df))] else NULL

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
    # Thick band — weather + model uncertainty (bottom layer)
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo_full, ymax = .data$hi_full),
      linewidth = 4.0, alpha = 1,  # doubled from 2.0
      na.rm     = TRUE
    ) +
    # Thin line — coefficient uncertainty (top layer, always visible)
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$coef_lo, ymax = .data$coef_hi),
      linewidth = 1.5,
      colour = "black",   # thicker than before so visible against thick band
      na.rm    = TRUE
    ) +
    #Geom Point
    ggplot2::geom_point(
      ggplot2::aes(y = .data$mean),
      size   = 3,
      shape  = 21,
      fill   = "white",
      colour = "black",
      na.rm  = TRUE
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
build_threshold_table_df <- function(all_series, group_order = "scenario_x_year",
                               band_q          = c(lo = 0.10, hi = 0.90),
                               ensemble_band_q = c(lo = 0, hi = 1),
                               hist_ref = 0) {


  rows <- lapply(names(all_series), function(nm) {
    out     <- all_series[[nm]]$out
    is_fut  <- "value_all" %in% names(out) &&
               any(vapply(out$value_all, length, integer(1L)) > 1L)

    # Central values — ensemble trimmed per year (matches exceedance curve)
    vals <- if (is_fut) {
      v <- unlist(lapply(out$value_all, function(yr_vals) {
        lo <- quantile(yr_vals, ensemble_band_q[["lo"]], na.rm = TRUE)
        hi <- quantile(yr_vals, ensemble_band_q[["hi"]], na.rm = TRUE)
        yr_vals[yr_vals >= lo & yr_vals <= hi]
      }))
      v[is.finite(v)] - hist_ref #future value_all is raw - apply hist_ref shift here
    } else {
      out$value[is.finite(out$value)]   # historical — 30 values unchanged
    }
    n <- length(vals)

    keep_low  <- names(RP_LOW)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
    keep_high <- names(RP_HIGH)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
    if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

    # Helper — build one row from a value vector
    build_row <- function(vals_in, nm, row_label) {
      vals_in <- vals_in[is.finite(vals_in)]
      if (length(vals_in) < 2L) return(NULL)
      # Exceedance prob p maps to sorted position (1 - p):
      # low exceedance (e.g. 1:50 = 0.02) → high outcome value → quantile at 0.98
      # high exceedance (e.g. 49:50 = 0.98) → low outcome value → quantile at 0.02
      low_df <- if (length(keep_low) > 0)
        as.data.frame(t(sapply(keep_low,
          function(th) round(stats::quantile(vals_in, 1 - RP_LOW[th]), 3))))
      else data.frame()
      high_df <- if (length(keep_high) > 0)
        as.data.frame(t(sapply(keep_high,
          function(th) round(stats::quantile(vals_in, 1 - RP_HIGH[th]), 3))))
      else data.frame()
      names(low_df)  <- keep_low
      names(high_df) <- keep_high
      median_df <- data.frame(
        "1:1" = round(stats::median(vals_in), 3),
        check.names = FALSE
      )
      cbind(
        data.frame(
          Scenario = nm,           # original scenario name
          Estimate = row_label,    # "Central", "Lower (p10)", "Upper (p90)"
          Obs      = length(vals_in),
          check.names = FALSE
        ),
        low_df, median_df, high_df
      )
    }

    # Central row
    central <- build_row(vals,    nm, "Central")
    

    # Lower/Upper rows — from value_lo/value_hi if available

    # Build draw_curves [S × n_years] — identical method to enhance_exceedance()
    # so that lo_vals/hi_vals are derived from the same structure as the ribbon.
    dv_list   <- out$draw_values
    n_pt      <- length(vals)   # already ensemble-trimmed above

    draw_curves_tbl <- if (!is.null(band_q) && !is.null(dv_list) &&
                           length(dv_list) > 0L && n_pt >= 2L) {
      if (is_fut) {
        n_mod_yr <- length(out$value_all[[1L]])
        S_loc    <- round(length(dv_list[[1L]]) / n_mod_yr)
        dc <- matrix(NA_real_, nrow = S_loc, ncol = n_pt)
        for (s in seq_len(S_loc)) {
          yr_s <- unlist(lapply(seq_along(dv_list), function(i) {
            yr_vals  <- out$value_all[[i]]
            lo       <- quantile(yr_vals, ensemble_band_q[["lo"]], na.rm = TRUE)
            hi       <- quantile(yr_vals, ensemble_band_q[["hi"]], na.rm = TRUE)
            keep_idx <- which(yr_vals >= lo & yr_vals <= hi)
            if (length(keep_idx) == 0L) return(NULL)
            dv         <- dv_list[[i]]
            n_mod      <- length(yr_vals)
            S_i        <- round(length(dv) / n_mod)
            draw_mat_i <- matrix(dv, nrow = n_mod, ncol = S_i)
            draw_mat_i[keep_idx, s, drop = TRUE]
          }))
          if (length(yr_s) == n_pt)
            dc[s, ] <- sort(yr_s, decreasing = FALSE)
        }
        dc
      } else {
        # Historical: dv_list[[year]][s] — reshape to [S × n_years]
        S_loc    <- length(dv_list[[1L]])
        draw_mat <- do.call(rbind, lapply(dv_list, as.numeric))
        dc <- matrix(NA_real_, nrow = S_loc, ncol = n_pt)
        for (s in seq_len(S_loc)) {
          dc[s, ] <- sort(draw_mat[, s], decreasing = FALSE)
        }
        dc
      }
    } else NULL

    # band_lo/band_hi vectors — column-wise quantile, same as ribbon
    # lo_vals/hi_vals: sorted values at the band_q edges for build_row()
    n_draws <- if (!is.null(draw_curves_tbl)) sum(!is.na(draw_curves_tbl)) else 0L
    lo_vals <- if (!is.null(draw_curves_tbl)) {
      apply(draw_curves_tbl, 2, quantile, band_q[["lo"]], na.rm = TRUE)
    } else numeric(0)

    hi_vals <- if (!is.null(draw_curves_tbl)) {
      apply(draw_curves_tbl, 2, quantile, band_q[["hi"]], na.rm = TRUE)
    } else numeric(0)

    lo_vals <- lo_vals - hist_ref
    hi_vals <- hi_vals - hist_ref

    if (length(lo_vals) >= 2L && length(hi_vals) >= 2L) {
    # lo_vals/hi_vals are the ribbon band edges — sorted ascending, length = n_pt.
    # Map each RP threshold directly to its position in the sorted vector:
    #   exceedance prob p → position index = round(p * n_pt)
    # This mirrors exactly how the ribbon reads band_lo/band_hi at each prob.
    build_band_row <- function(band_vec, nm, row_label) {
      n <- length(band_vec)
      # band_vec is sorted ascending. Exceedance prob p maps to sorted position:
      #   k = round(n * (1 - p)) + 1
      # RP_LOW[th]  = low exceedance prob  (e.g. 0.02 for 1:50) → high index → high value
      # RP_HIGH[th] = high exceedance prob (e.g. 0.98 for 49:50) → low index → low value
      low_df <- if (length(keep_low) > 0)
        as.data.frame(t(sapply(keep_low, function(th) {
          idx <- min(n, round(n * (1 - RP_LOW[th])) + 1L)
          round(band_vec[idx], 3)
        })))
      else data.frame()
      high_df <- if (length(keep_high) > 0)
        as.data.frame(t(sapply(keep_high, function(th) {
          idx <- max(1L, round(n * (1 - RP_HIGH[th])) + 1L)
          round(band_vec[idx], 3)
        })))
      else data.frame()
      names(low_df)  <- keep_low
      names(high_df) <- keep_high
      median_df <- data.frame("1:1" = round(stats::median(band_vec), 3),
                              check.names = FALSE)
      cbind(
        data.frame(Scenario = nm, Estimate = row_label,
                  Obs = n_draws, check.names = FALSE),
        low_df, median_df, high_df
      )
    }
    lower <- build_band_row(lo_vals, nm, "Lower")
    upper <- build_band_row(hi_vals, nm, "Upper")
    dplyr::bind_rows(central, lower, upper)
  } else {
    central
  }
})

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  all_cols <- {
    seen <- unique(unlist(lapply(rows, names)))
    fixed <- c("Scenario", "Estimate", "Obs",
              names(RP_LOW), "1:1", names(RP_HIGH))
    c(fixed[fixed %in% seen], setdiff(seen, fixed))
  }
  rows <- lapply(rows, function(r) {
    for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
    r[all_cols]
  })
  df <- do.call(rbind, rows)

  # Sort by group_order: Historical rows first, then SSP rows.
  # SSP rows carry a " / P{pct}" suffix — extract SSP, year, and percentile
  # for clean three-level sorting (P10 < P50 < P90 within each SSP/year group).
  # Add estimate sort order — Central first, then Lower, then Upper
  df$.est_order <- dplyr::case_when(
    df$Estimate == "Upper"   ~ 1L,
    df$Estimate == "Central" ~ 2L,
    df$Estimate == "Lower"   ~ 3L,
    TRUE                     ~ 4L
  )

  hist_rows <- df[df$Scenario == "Historical", , drop = FALSE]
  ssp_rows  <- df[df$Scenario != "Historical", , drop = FALSE]

  # Sort hist rows: Central → Lower → Upper
  if (nrow(hist_rows) > 0)
    hist_rows <- hist_rows[order(hist_rows$.est_order), ]

  # Sort SSP rows: by SSP → year → Central/Lower/Upper
  if (nrow(ssp_rows) > 0) {
    ssp_rows$ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows$Scenario)
    ssp_rows$yr_sort  <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows$Scenario, yr_m),
      regmatches(ssp_rows$Scenario,
                 regexpr("[0-9]{4}", ssp_rows$Scenario))
    )
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario"))
      ssp_rows[order(ssp_rows$yr_sort,
                     ssp_rows$ssp_sort,
                     ssp_rows$.est_order), ]
    else
      ssp_rows[order(ssp_rows$ssp_sort,
                     ssp_rows$yr_sort,
                     ssp_rows$.est_order), ]
    ssp_rows$ssp_sort  <- NULL
    ssp_rows$yr_sort   <- NULL
  }

  # Remove helper column and return
  hist_rows$.est_order <- NULL
  ssp_rows$.est_order  <- NULL
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
                               logit_x       = FALSE,
                               ribbon_data   = NULL,
                               band_q          = c(lo = 0.10, hi = 0.90),
                               ensemble_band_q = c(lo = 0, hi = 1),
                               hist_ref = 0) {

  stopifnot(is.list(scenarios), length(scenarios) > 0)
  labels <- names(scenarios)

    # ---- Build central line + ribbon per scenario (Option B) ---------------
    exceedance_df <- dplyr::bind_rows(lapply(
      c("Historical", names(scenarios)),
      function(nm) {
        is_hist <- nm == "Historical"

        # ---- Point estimates (central line) ----------------------------------
        if (is_hist) {
          pt_vals  <- hist_agg$out$value
          dv_list  <- hist_agg$out$draw_values
          ssp_key  <- "Historical"
          yr_label <- "Historical"
        } else {
          out_df  <- scenarios[[nm]]$out
          
          

          # Per-year ensemble trim — controlled by ensemble_band_q
          pt_vals <- unlist(lapply(out_df$value_all, function(yr_vals) {
            lo <- quantile(yr_vals, ensemble_band_q[["lo"]], na.rm = TRUE)
            hi <- quantile(yr_vals, ensemble_band_q[["hi"]], na.rm = TRUE)
            yr_vals[yr_vals >= lo & yr_vals <= hi]
          }))
          # Apply deviation shift — hist_ref = 0 when no deviation selected.
          # draw_values used for ribbon are shifted via ribbon_data (already deviated).
          pt_vals  <- pt_vals - hist_ref
          dv_list  <- out_df$draw_values
          ssp_key  <- .normalise_ssp(nm)
          yr_label <- .parse_year(nm)
        }

        if (length(pt_vals) == 0L) return(NULL)

        # Central curve — point estimate ECDF (deterministic, no MC noise)
        n_years       <- length(pt_vals)
        central_curve <- sort(pt_vals, decreasing = FALSE)
        probs         <- rev((seq_len(n_years) - 0.5) / n_years)

        # ---- Ribbon — S complete exceedance curves --------------------------
        if (!is.null(band_q) && !is.null(dv_list) && length(dv_list) > 0L) {

          if (is_hist) {
            # Historical: draw_values = list of S-length vectors per year
            # Reshape to [n_years × S] matrix
            S_loc    <- length(dv_list[[1L]])
            draw_mat <- do.call(rbind, lapply(dv_list, as.numeric))
            # draw_mat[year, draw s] → for draw s sort all years
            draw_curves <- matrix(NA_real_, nrow = S_loc, ncol = n_years)
            for (s in seq_len(S_loc)) {
              draw_curves[s, ] <- sort(draw_mat[, s], decreasing = FALSE)
            }

          } else {
            # Future: draw_values[[i]] = [n_mod × S] stored column-major
            # Filter to same ensemble subset as pt_vals per year
            n_mod_yr <- length(out_df$value_all[[1L]])
            S_loc    <- round(length(dv_list[[1L]]) / n_mod_yr)
            draw_curves <- matrix(NA_real_, nrow = S_loc, ncol = n_years)

            for (s in seq_len(S_loc)) {
              yr_vals_s <- unlist(lapply(seq_along(dv_list), function(i) {
                yr_vals  <- out_df$value_all[[i]]
                lo       <- quantile(yr_vals, ensemble_band_q[["lo"]], na.rm = TRUE)
                hi       <- quantile(yr_vals, ensemble_band_q[["hi"]], na.rm = TRUE)
                keep_idx <- which(yr_vals >= lo & yr_vals <= hi)
                if (length(keep_idx) == 0L) return(NULL)
                dv         <- dv_list[[i]]
                n_mod      <- length(yr_vals)
                S_i        <- round(length(dv) / n_mod)
                draw_mat_i <- matrix(dv, nrow = n_mod, ncol = S_i)
                draw_mat_i[keep_idx, s, drop = TRUE]
              }))
              if (length(yr_vals_s) == n_years)
                draw_curves[s, ] <- sort(yr_vals_s, decreasing = FALSE)
            }
          }

          # Apply band_q — controlled by coefficient uncertainty selector
          lo_q    <- band_q[["lo"]]
          hi_q    <- band_q[["hi"]]
          band_lo <- apply(draw_curves, 2, quantile, lo_q, na.rm = TRUE) - hist_ref
          band_hi <- apply(draw_curves, 2, quantile, hi_q, na.rm = TRUE) - hist_ref

        } else {
          # Coefficient uncertainty OFF — no ribbon
          band_lo <- rep(NA_real_, n_years)
          band_hi <- rep(NA_real_, n_years)
        }

        data.frame(
          welfare_val = central_curve,
          exceed_prob = probs,
          band_lo     = band_lo,
          band_hi     = band_hi,
          ssp_key     = if (is.na(ssp_key)) "Historical" else ssp_key,
          yr          = if (is.na(yr_label)) "Historical" else yr_label,
          scenario    = nm,
          stringsAsFactors = FALSE
        )
      }
    ))

    # ---- Aesthetic mappings -------------------------------------------------
    fut_yr_labels <- sort(unique(exceedance_df$yr[exceedance_df$yr != "Historical"]))
    yr_styles     <- .resolve_year_styles(fut_yr_labels)
    present_ssps  <- sort(unique(exceedance_df$ssp_key[exceedance_df$ssp_key != "Historical"]))
    colour_map_ssp <- c(
      "Historical" = "black",
      .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
    )
    present_yrs  <- names(yr_styles$linetype_map)
    ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)
    hist_mean    <- mean(hist_agg$out$value, na.rm = TRUE)

    # ---- Early exit ---------------------------------------------------------
    if (is.null(exceedance_df) || nrow(exceedance_df) == 0L)
      return(ggplot2::ggplot() +
        ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

    ann_y <- if (isTRUE(logit_x)) 0.97 else 0.95

    # ---- Plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(
    exceedance_df,
    ggplot2::aes(
      x        = welfare_val,
      y        = exceed_prob,
      colour   = ssp_key,
      linetype = yr,
      group    = scenario
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +

    # Coefficient uncertainty ribbon — band_q controlled
    { if (!is.null(band_q) && any(!is.na(exceedance_df$band_lo))) {
        ggplot2::geom_ribbon(
          data    = exceedance_df[!is.na(exceedance_df$band_lo), ],
          mapping = ggplot2::aes(
            y     = .data$exceed_prob,
            xmin  = .data$band_lo,
            xmax  = .data$band_hi,
            fill  = .data$ssp_key,
            group = .data$scenario
          ),
          alpha       = 0.15,
          inherit.aes = FALSE
        )
      } else NULL
    } +
    
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
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_fill_manual(
      values   = colour_map_ssp,
      na.value = "grey70",
      guide    = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = present_yrs,
      labels = present_yrs,
      name   = "Period",
      guide  = ggplot2::guide_legend(order = 2,
                                     override.aes = list(linewidth = 0.9))
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
