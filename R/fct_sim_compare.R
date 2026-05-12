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
#   plot_pointrange_climate() -- hero chart with three nested uncertainty bands
#   build_threshold_table_df()  -- return-period threshold data frame for DT
#   enhance_exceedance()        -- per-model exceedance curves + inter-model ribbon
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

# plot_pointrange_climate uses coef_lo/coef_hi from the analytic envelope tbl.
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
plot_pointrange_climate <- function(bands_tbl,
                                    x_label     = "",
                                    group_order = "scenario_x_year",
                                    show_coef   = TRUE) {

  if (is.null(bands_tbl) || nrow(bands_tbl) == 0L) {
    return(ggplot2::ggplot() +
           ggplot2::labs(title = "Run a simulation to see results."))
  }

  df <- bands_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
    # Historical from the policy source is identical to baseline historical —
    # drop it so the chart shows one historical dot (under Baseline).
    df <- df[!(df$is_historical & df$source == "Policy"), , drop = FALSE]
  }
  df$ssp_key  <- ifelse(df$is_historical, "Historical",
                        vapply(df$scenario, .normalise_ssp, character(1L)))
  df$yr_lbl   <- ifelse(df$is_historical, "Historical",
                        vapply(df$scenario, .parse_year, character(1L)))
  df$ssp_short <- ifelse(df$is_historical, "Historical",
                         SSP_SHORT_LABELS[df$ssp_key] %||% df$ssp_key)
  df$pt_key   <- ifelse(df$is_historical, "Historical",
                        paste0(df$ssp_short, "\n", df$yr_lbl))
  df$colour_key <- ifelse(df$is_historical, "Historical",
                          paste(df$ssp_key, df$yr_lbl, sep = "__"))

  fut_df <- df[!df$is_historical, , drop = FALSE]

  # ---- colour palette: Historical = grey; one colour per SSP x year --------
  colour_palette <- c("Historical" = "#808080")
  if (nrow(fut_df) > 0L) {
    yrs_present <- sort(unique(fut_df$yr_lbl))
    n_yrs       <- length(yrs_present)
    yr_lighten  <- if (n_yrs > 1L) seq(0.30, 0.0, length.out = n_yrs) else 0.0
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
  if (nrow(fut_df) > 0L) {
    ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"), unique(fut_df$ssp_short))
    spacer_n <- 0L
    if (isTRUE(group_order == "year_x_scenario")) {
      yrs_present <- sort(unique(fut_df$yr_lbl))
      for (yr_i in yrs_present) {
        spacer_n <- spacer_n + 1L
        sid <- strrep(" ", spacer_n)
        spacer_ids <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        for (ssp in ssps_present)
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
      }
    } else {
      for (ssp in ssps_present) {
        spacer_n <- spacer_n + 1L
        sid <- strrep(" ", spacer_n)
        spacer_ids <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(fut_df$yr_lbl[fut_df$ssp_short == ssp]))
        for (yr_i in ssp_yrs)
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
      }
    }
  }
  x_label_map <- setNames(
    vapply(ordered_levels, function(lv) {
      if (lv %in% spacer_ids) "" else lv
    }, character(1L)),
    ordered_levels
  )
  data_levels <- setdiff(ordered_levels, spacer_ids)
  df$pt_key <- factor(df$pt_key, levels = ordered_levels)
  df <- df[df$pt_key %in% data_levels, , drop = FALSE]

  if (nrow(df) == 0L)
    return(ggplot2::ggplot() +
           ggplot2::labs(title = "Run a future simulation to see scenario comparisons."))

  # ---- plot: nested bands + dot --------------------------------------------
  # When a `source` column is present we dodge Baseline vs Policy side-by-side
  # within each scenario. Otherwise (Mod 2) the chart is single-source and
  # the dodge collapses to no-op via a single-level factor.
  pos <- if (has_source) ggplot2::position_dodge(width = 0.55)
         else ggplot2::position_identity()
  aes_base <- if (has_source)
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key,
                 group = .data$source)
  else
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key)
  p <- ggplot2::ggplot(df, aes_base)

  # Outermost (when present): "total" whisker combining all three sources
  # assuming independence: SE = sqrt(var_coef + var_within + var_across).
  if (all(c("total_lo", "total_hi") %in% names(df))) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$total_lo, ymax = .data$total_hi),
      linewidth = 0.6, colour = "grey40", alpha = 0.8, na.rm = TRUE,
      position = pos
    ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$total_lo, ymax = .data$total_hi),
        width = 0.25, linewidth = 0.4, colour = "grey40", na.rm = TRUE,
        position = pos, inherit.aes = TRUE
      )
  }

  p <- p +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$intermod_lo, ymax = .data$intermod_hi),
      linewidth = 6.0, alpha = 0.6, na.rm = TRUE, position = pos
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$interann_lo, ymax = .data$interann_hi),
      linewidth = 3.5, alpha = 1.0, na.rm = TRUE, position = pos
    )

  if (isTRUE(show_coef)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$coef_lo, ymax = .data$coef_hi),
      linewidth = 1.2, colour = "black", na.rm = TRUE, position = pos
    )
  }

  if (has_source) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value, shape = .data$source,
                   fill  = .data$source),
      size = 3, stroke = 1.2, colour = "black", na.rm = TRUE, position = pos
    ) +
      ggplot2::scale_shape_manual(
        values = c(Baseline = 21, Policy = 23), name = NULL, drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(Baseline = "white", Policy = "#d32f2f"),
        name   = NULL, drop = FALSE
      )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      size = 3, shape = 21, fill = "white", colour = "black", na.rm = TRUE
    )
  }

  p +
    ggplot2::scale_colour_manual(values = colour_palette, guide = "none") +
    ggplot2::scale_x_discrete(limits = ordered_levels,
                              labels = x_label_map, drop = FALSE) +
    ggplot2::labs(title = NULL, x = NULL, y = x_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = if (has_source) "top" else "none"
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
build_threshold_table_df <- function(threshold_tbl,
                                     group_order = "scenario_x_year",
                                     show_coef   = TRUE) {

  if (is.null(threshold_tbl) || nrow(threshold_tbl) == 0L) return(NULL)
  df <- threshold_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    # Historical row from the Policy source is identical to Baseline by
    # construction (policy doesn't apply to historical) — drop it.
    df <- df[!(grepl("^Historical", df$scenario) & df$source == "Policy"),
             , drop = FALSE]
  }

  # Optionally hide the coefficient-band rows (any "Coef Pxx" label).
  if (!isTRUE(show_coef)) {
    df <- df[!grepl("^Coef ", df$Estimate), , drop = FALSE]
  }
  if (nrow(df) == 0L) return(NULL)

  # Pivot: one column per RP threshold, value rounded.
  rp_levels <- unique(df$rp_label)
  df$value_round <- round(df$value, 3)

  pivot_cols <- if (has_source)
    c("scenario", "source", "Estimate", "rp_label", "n_obs", "value_round")
  else
    c("scenario", "Estimate", "rp_label", "n_obs", "value_round")
  wide <- tidyr::pivot_wider(
    df[, pivot_cols],
    names_from  = "rp_label",
    values_from = "value_round"
  )
  wide <- as.data.frame(wide)
  wide <- dplyr::rename(wide, Scenario = scenario, Obs = n_obs)
  if (has_source) wide <- dplyr::rename(wide, Source = source)

  # Reorder columns to canonical sequential order: 1:50, 1:20, ..., 1:1, ...,
  # 19:20, 49:50. RPs that didn't survive the n-year reliability filter are
  # simply absent from `names(wide)` and are skipped.
  canonical <- c(names(RP_LOW), "1:1", names(RP_HIGH))
  rp_present <- intersect(canonical, names(wide))
  lead_cols  <- if (has_source) c("Scenario", "Source", "Estimate", "Obs")
                else c("Scenario", "Estimate", "Obs")
  wide <- wide[, c(lead_cols, rp_present), drop = FALSE]

  # Sort rows: Historical first, then SSPs. Within each scenario the
  # Estimate rows are arranged concentrically around the Central P50:
  #   Total low -> Coef low -> Ensemble low -> Central -> Ensemble high
  #   -> Coef high -> Total high
  # Lower vs upper side is read from the percentile (< 50 or > 50, or the
  # special "min"/"max" tokens). Family distance from the central row is
  # Total = 3 (outermost), Coef = 2, Ensemble = 1.
  .est_rank <- function(est) {
    fam_dist <- ifelse(grepl("^Total ",    est), 3L,
                ifelse(grepl("^Coef ",     est), 2L,
                ifelse(grepl("^Ensemble ", est), 1L, 0L)))
    pct <- suppressWarnings(as.integer(sub(".*P", "", est)))
    pct <- ifelse(grepl(" min$", est),  0L,
           ifelse(grepl(" max$", est), 100L, pct))
    pct[is.na(pct)] <- 50L
    side <- ifelse(pct < 50, -1L, ifelse(pct > 50, 1L, 0L))
    side * fam_dist
  }
  wide$.est_order <- .est_rank(wide$Estimate)

  hist_rows <- wide[wide$Scenario == "Historical", , drop = FALSE]
  ssp_rows  <- wide[wide$Scenario != "Historical", , drop = FALSE]

  if (nrow(hist_rows) > 0)
    hist_rows <- hist_rows[order(hist_rows$.est_order), , drop = FALSE]

  if (nrow(ssp_rows) > 0) {
    ssp_rows$.ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows$Scenario)
    ssp_rows$.yr_sort  <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows$Scenario, yr_m),
      regmatches(ssp_rows$Scenario, regexpr("[0-9]{4}", ssp_rows$Scenario))
    )
    src_sort <- if (has_source)
      match(ssp_rows$Source, c("Baseline", "Policy")) else 1L
    ssp_rows$.src_sort <- src_sort
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario"))
      ssp_rows[order(ssp_rows$.yr_sort, ssp_rows$.ssp_sort,
                     ssp_rows$.src_sort, ssp_rows$.est_order), , drop = FALSE]
    else
      ssp_rows[order(ssp_rows$.ssp_sort, ssp_rows$.yr_sort,
                     ssp_rows$.src_sort, ssp_rows$.est_order), , drop = FALSE]
    ssp_rows$.ssp_sort <- NULL
    ssp_rows$.yr_sort  <- NULL
    ssp_rows$.src_sort <- NULL
  }

  hist_rows$.est_order <- NULL
  ssp_rows$.est_order  <- NULL
  rbind(hist_rows, ssp_rows)
}



# ---------------------------------------------------------------------------- #
# Time-series spaghetti + envelope plot                                        #
# ---------------------------------------------------------------------------- #

#' Per-Model Time-Series Spaghetti With Ensemble Envelope
#'
#' Draws one thin translucent line per (scenario, model) over sim_year, with
#' a bold across-model median curve and a translucent inter-model ribbon
#' on top. Historical scenarios are drawn as a single bold line (no ribbon
#' — only one "model"). The aim is to show model disagreement directly
#' alongside year-to-year variation.
#'
#' @param ts_tbl Tibble with columns: scenario, model_id, sim_year, value,
#'   is_historical.
#' @param x_label Y-axis label.
#' @param ensemble_band_q Named numeric `c(lo, hi)` quantile pair for the
#'   inter-model ribbon.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_color_manual
#'   scale_fill_manual labs theme_minimal theme
#' @importFrom dplyr group_by summarise first n
#' @importFrom rlang .data
#' @export
plot_timeseries_spaghetti <- function(ts_tbl,
                                       x_label         = "",
                                       ensemble_band_q = c(lo = 0, hi = 1)) {
  if (is.null(ts_tbl) || nrow(ts_tbl) == 0L)
    return(ggplot2::ggplot() +
           ggplot2::labs(title = "Run a simulation to see model trajectories."))

  df <- ts_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    df <- df[!(df$is_historical & df$source == "Policy"), , drop = FALSE]
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
  }
  df$ssp_key <- ifelse(df$is_historical, "Historical",
                       vapply(df$scenario, .normalise_ssp, character(1L)))
  df$yr_lbl  <- ifelse(df$is_historical, "Historical",
                       vapply(df$scenario, .parse_year, character(1L)))

  fut_yr_labels  <- sort(unique(df$yr_lbl[df$yr_lbl != "Historical"]))
  yr_styles      <- .resolve_year_styles(fut_yr_labels)
  present_ssps   <- sort(unique(df$ssp_key[df$ssp_key != "Historical"]))
  colour_map_ssp <- c("Historical" = "black",
                      .ssp_colours[intersect(names(.ssp_colours),
                                             present_ssps)])
  ltype_map_yr   <- c("Historical" = "solid", yr_styles$linetype_map)

  # Combined legend: per-scenario colour (SSP) and linetype (period).
  scen_levels <- c("Historical",
                   sort(unique(df$scenario[!df$is_historical])))
  scen_colour_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") return(unname(colour_map_ssp[["Historical"]]))
    unname(colour_map_ssp[[.normalise_ssp(s)]] %||% "grey50")
  }, character(1L))
  scen_ltype_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") return(unname(ltype_map_yr[["Historical"]]))
    yr <- .parse_year(s)
    unname(ltype_map_yr[[yr]] %||% "solid")
  }, character(1L))
  df$scenario_f <- factor(df$scenario, levels = scen_levels)

  # Per-(scenario [, source], sim_year) envelope across models
  env_grp <- if (has_source) c("scenario", "source", "sim_year")
             else c("scenario", "sim_year")
  env_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(env_grp))) |>
    dplyr::summarise(
      central       = stats::median(.data$value, na.rm = TRUE),
      lo            = unname(stats::quantile(.data$value,
                                              ensemble_band_q[["lo"]],
                                              na.rm = TRUE)),
      hi            = unname(stats::quantile(.data$value,
                                              ensemble_band_q[["hi"]],
                                              na.rm = TRUE)),
      n_models      = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups       = "drop"
    )
  env_df$scenario_f <- factor(env_df$scenario, levels = scen_levels)

  fut_env <- env_df[!env_df$is_historical & env_df$n_models > 1L, ,
                    drop = FALSE]

  p <- ggplot2::ggplot()

  # Inter-model ribbon (futures only, when >1 model)
  if (nrow(fut_env) > 0L) {
    ribbon_aes <- if (has_source)
      ggplot2::aes(x = .data$sim_year, ymin = .data$lo, ymax = .data$hi,
                   fill = .data$scenario_f,
                   group = interaction(.data$scenario_f, .data$source))
    else
      ggplot2::aes(x = .data$sim_year, ymin = .data$lo, ymax = .data$hi,
                   fill = .data$scenario_f, group = .data$scenario_f)
    p <- p + ggplot2::geom_ribbon(
      data        = fut_env,
      mapping     = ribbon_aes,
      alpha       = if (has_source) 0.10 else 0.15,
      show.legend = FALSE
    )
  }

  # Spaghetti: thin translucent line per (scenario [, source], model)
  spaghetti_aes <- if (has_source)
    ggplot2::aes(x = .data$sim_year, y = .data$value,
                 colour   = .data$scenario_f,
                 linetype = .data$scenario_f,
                 alpha    = .data$source,
                 group = interaction(.data$scenario_f, .data$source,
                                     .data$model_id))
  else
    ggplot2::aes(x = .data$sim_year, y = .data$value,
                 colour = .data$scenario_f, linetype = .data$scenario_f,
                 group  = interaction(.data$scenario_f, .data$model_id))
  p <- p + ggplot2::geom_line(
    data        = df,
    mapping     = spaghetti_aes,
    linewidth   = 0.3,
    alpha       = if (has_source) NULL else 0.35,
    na.rm       = TRUE,
    show.legend = FALSE
  )

  # Bold median curve per (scenario [, source])
  median_aes <- if (has_source)
    ggplot2::aes(x = .data$sim_year, y = .data$central,
                 colour = .data$scenario_f, linetype = .data$scenario_f,
                 alpha  = .data$source,
                 group  = interaction(.data$scenario_f, .data$source))
  else
    ggplot2::aes(x = .data$sim_year, y = .data$central,
                 colour = .data$scenario_f, linetype = .data$scenario_f,
                 group  = .data$scenario_f)
  p <- p + ggplot2::geom_line(
    data = env_df, mapping = median_aes,
    linewidth = 1.1, na.rm = TRUE
  )

  p +
    ggplot2::scale_color_manual(
      values = scen_colour_map, breaks = scen_levels,
      name   = "Scenario",
      guide  = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_fill_manual(
      values = scen_colour_map, breaks = scen_levels, guide = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = scen_ltype_map, breaks = scen_levels,
      name   = "Scenario"
    )
  if (has_source) {
    p <- p + ggplot2::scale_alpha_manual(
      values = c(Baseline = 0.35, Policy = 1.0),
      breaks = c("Baseline", "Policy"),
      name   = "Source",
      guide  = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    )
  }
  p +
    ggplot2::labs(x = "Simulation year", y = x_label) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------- #
# Variance-contribution stacked bar                                            #
# ---------------------------------------------------------------------------- #

#' Stacked Variance-Contribution Bar by Scenario
#'
#' For each scenario, plots a 100% stacked horizontal bar showing the share
#' of total predictive variance contributed by:
#'   - Coefficient uncertainty (regression-fit per-outcome SE)
#'   - Inter-annual variability (within-model year-to-year)
#'   - Inter-model spread (across-model disagreement; future only)
#'
#' Assumes the three sources are independent so total var = sum of components.
#'
#' @param var_tbl Tibble with columns: scenario, var_coef, var_within,
#'   var_across, is_historical.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual scale_x_continuous
#'   labs theme_minimal theme coord_flip
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_variance_contribution <- function(var_tbl) {
  if (is.null(var_tbl) || nrow(var_tbl) == 0L)
    return(ggplot2::ggplot() +
           ggplot2::labs(title = "Run a simulation to see variance contributions."))

  df <- var_tbl
  df$var_total <- df$var_coef + df$var_within + df$var_across
  # Guard against div-by-zero
  df$var_total <- ifelse(df$var_total > 0, df$var_total, 1)
  df$share_coef    <- df$var_coef    / df$var_total
  df$share_within  <- df$var_within  / df$var_total
  df$share_across  <- df$var_across  / df$var_total

  long <- tidyr::pivot_longer(
    df[, c("scenario", "share_coef", "share_within", "share_across")],
    cols      = c("share_coef", "share_within", "share_across"),
    names_to  = "source",
    values_to = "share"
  )
  long$source <- factor(long$source,
                        levels = c("share_across", "share_within", "share_coef"),
                        labels = c("Inter-model spread",
                                   "Inter-annual variability",
                                   "Coefficient uncertainty"))
  # Preserve scenario order from input
  long$scenario <- factor(long$scenario, levels = rev(unique(df$scenario)))

  fill_map <- c(
    "Coefficient uncertainty"  = "#4a90d9",
    "Inter-annual variability" = "#f4a261",
    "Inter-model spread"       = "#7a5195"
  )

  ggplot2::ggplot(long,
    ggplot2::aes(x = .data$scenario, y = .data$share, fill = .data$source)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = fill_map, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Share of total predictive variance") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position    = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip()
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
enhance_exceedance <- function(curves_tbl,
                               x_label,
                               return_period = TRUE,
                               n_sim_years   = NULL,
                               logit_x       = FALSE,
                               band_q          = c(lo = 0.10, hi = 0.90),
                               ensemble_band_q = c(lo = 0, hi = 1)) {

  if (is.null(curves_tbl) || nrow(curves_tbl) == 0L)
    return(ggplot2::ggplot() +
           ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  # ---- Per-scenario summary at each rank ---------------------------------
  # For each (scenario, rank) collapse across models:
  #   central_at_rank    = median of welfare_val across models
  #   intermod_lo/hi     = quantile across models at ensemble_band_q
  #   coef_lo/hi_at_rank = median(welfare_val ± z * coef_sd) across models
  z_lo <- if (!is.null(band_q)) stats::qnorm(band_q[["lo"]]) else NA_real_
  z_hi <- if (!is.null(band_q)) stats::qnorm(band_q[["hi"]]) else NA_real_

  has_source <- "source" %in% names(curves_tbl)
  if (has_source) {
    curves_tbl <- curves_tbl[!(curves_tbl$is_historical &
                               curves_tbl$source == "Policy"), , drop = FALSE]
    grp_cols <- c("scenario", "source", "rank")
  } else {
    grp_cols <- c("scenario", "rank")
  }

  agg_df <- curves_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      exceed_prob   = dplyr::first(.data$exceed_prob),
      central       = stats::median(.data$welfare_val, na.rm = TRUE),
      intermod_lo   = unname(stats::quantile(.data$welfare_val,
                                              ensemble_band_q[["lo"]], na.rm = TRUE)),
      intermod_hi   = unname(stats::quantile(.data$welfare_val,
                                              ensemble_band_q[["hi"]], na.rm = TRUE)),
      n_models      = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups       = "drop"
    )

  # Coefficient band: apply a scenario-level typical SE as a constant offset
  # from the central curve. Using the per-rank coef_sd directly produces noisy,
  # self-crossing dashed lines because the per-rank SD wiggles. A single
  # typical SE per scenario gives clean parallels.
  if (!is.null(band_q)) {
    sd_grp <- if (has_source) c("scenario", "source") else "scenario"
    coef_sd_scn <- curves_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sd_grp))) |>
      dplyr::summarise(coef_sd_typ = stats::median(.data$coef_sd, na.rm = TRUE),
                       .groups = "drop")
    agg_df <- dplyr::left_join(agg_df, coef_sd_scn, by = sd_grp)
    agg_df$coef_lo <- agg_df$central + z_lo * agg_df$coef_sd_typ
    agg_df$coef_hi <- agg_df$central + z_hi * agg_df$coef_sd_typ
  } else {
    agg_df$coef_lo <- NA_real_
    agg_df$coef_hi <- NA_real_
  }
  if (has_source) {
    agg_df$source <- factor(agg_df$source, levels = c("Baseline", "Policy"))
    agg_df <- agg_df[order(agg_df$scenario, agg_df$source, agg_df$rank), ,
                     drop = FALSE]
    agg_df$line_id <- paste(agg_df$scenario, agg_df$source, sep = " | ")
  } else {
    agg_df <- agg_df[order(agg_df$scenario, agg_df$rank), , drop = FALSE]
    agg_df$line_id <- as.character(agg_df$scenario)
  }

  # Aesthetic mappings
  fut_df  <- agg_df[!agg_df$is_historical, , drop = FALSE]
  hist_df <- agg_df[ agg_df$is_historical, , drop = FALSE]

  agg_df$ssp_key <- ifelse(agg_df$is_historical, "Historical",
                           vapply(agg_df$scenario, .normalise_ssp, character(1L)))
  agg_df$yr_lbl  <- ifelse(agg_df$is_historical, "Historical",
                           vapply(agg_df$scenario, .parse_year, character(1L)))

  fut_yr_labels  <- sort(unique(agg_df$yr_lbl[agg_df$yr_lbl != "Historical"]))
  yr_styles      <- .resolve_year_styles(fut_yr_labels)
  present_ssps   <- sort(unique(agg_df$ssp_key[agg_df$ssp_key != "Historical"]))
  colour_map_ssp <- c("Historical" = "black",
                      .ssp_colours[intersect(names(.ssp_colours), present_ssps)])
  ltype_map_yr   <- c("Historical" = "solid", yr_styles$linetype_map)
  hist_mean      <- if (nrow(hist_df) > 0L) mean(hist_df$central, na.rm = TRUE) else NA_real_
  ann_y          <- if (isTRUE(logit_x)) 0.97 else 0.95

  # Combined legend: each scenario gets one entry with its colour (SSP) and
  # linetype (period). Build parallel maps keyed by the scenario string.
  scen_levels <- c("Historical",
                    sort(unique(agg_df$scenario[!agg_df$is_historical])))
  scen_colour_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") return(unname(colour_map_ssp[["Historical"]]))
    unname(colour_map_ssp[[.normalise_ssp(s)]] %||% "grey50")
  }, character(1L))
  scen_ltype_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") return(unname(ltype_map_yr[["Historical"]]))
    yr <- .parse_year(s)
    unname(ltype_map_yr[[yr]] %||% "solid")
  }, character(1L))
  agg_df$scenario <- factor(agg_df$scenario, levels = scen_levels)

    # ---- Plot ---------------------------------------------------------------
  # Layer order (back to front): inter-model ribbon (future) -> coefficient
  # ribbon (optional) -> median curve.
  fut_mod_df <- agg_df[!agg_df$is_historical, , drop = FALSE]

  p <- if (has_source) ggplot2::ggplot(
    agg_df,
    ggplot2::aes(
      x        = .data$central,
      y        = .data$exceed_prob,
      colour   = .data$scenario,
      linetype = .data$scenario,
      alpha    = .data$source,
      group    = .data$line_id
    )
  ) else ggplot2::ggplot(
    agg_df,
    ggplot2::aes(
      x        = .data$central,
      y        = .data$exceed_prob,
      colour   = .data$scenario,
      linetype = .data$scenario,
      group    = .data$scenario
    )
  )

  # Inter-model ribbon (futures only). When source is present we draw a
  # ribbon per source — both faded so the baseline ribbon stays readable.
  if (nrow(fut_mod_df) > 0L) {
    ribbon_aes <- if (has_source)
      ggplot2::aes(y = .data$exceed_prob, xmin = .data$intermod_lo,
                   xmax = .data$intermod_hi, fill = .data$scenario,
                   group = .data$line_id)
    else
      ggplot2::aes(y = .data$exceed_prob, xmin = .data$intermod_lo,
                   xmax = .data$intermod_hi, fill = .data$scenario,
                   group = .data$scenario)
    p <- p + ggplot2::geom_ribbon(
      data    = fut_mod_df,
      mapping = ribbon_aes,
      alpha       = if (has_source) 0.10 else 0.18,
      inherit.aes = FALSE
    )
  }

  # Coefficient uncertainty band: drawn as a pair of dashed outline curves
  # (lo and hi) instead of a filled ribbon, so it remains visible regardless
  # of whether it falls inside or outside the inter-model ribbon.
  if (!is.null(band_q) && any(!is.na(agg_df$coef_lo))) {
    coef_df <- agg_df[!is.na(agg_df$coef_lo), , drop = FALSE]
    coef_aes_lo <- if (has_source)
      ggplot2::aes(x = .data$coef_lo, y = .data$exceed_prob,
                   colour = .data$scenario, alpha = .data$source,
                   group  = .data$line_id)
    else
      ggplot2::aes(x = .data$coef_lo, y = .data$exceed_prob,
                   colour = .data$scenario, group = .data$scenario)
    coef_aes_hi <- if (has_source)
      ggplot2::aes(x = .data$coef_hi, y = .data$exceed_prob,
                   colour = .data$scenario, alpha = .data$source,
                   group  = .data$line_id)
    else
      ggplot2::aes(x = .data$coef_hi, y = .data$exceed_prob,
                   colour = .data$scenario, group = .data$scenario)
    p <- p +
      ggplot2::geom_line(data = coef_df, mapping = coef_aes_lo,
                         linetype = "dashed", linewidth = 0.5,
                         inherit.aes = FALSE, show.legend = FALSE) +
      ggplot2::geom_line(data = coef_df, mapping = coef_aes_hi,
                         linetype = "dashed", linewidth = 0.5,
                         inherit.aes = FALSE, show.legend = FALSE)
  }

  p <- p +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(
      xintercept = hist_mean, linetype = "dotted",
      colour = "black", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = hist_mean, y = ann_y,
      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
      size = 2.8, colour = "grey30"
    ) +
    # Merged legend: a single entry per scenario showing its colour (SSP)
    # and linetype (period). Both scales share the same `name` and `breaks`
    # so ggplot collapses them into one combined legend.
    ggplot2::scale_color_manual(
      values = scen_colour_map,
      breaks = scen_levels,
      name   = "Scenario",
      guide  = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_fill_manual(
      values   = scen_colour_map,
      breaks   = scen_levels,
      na.value = "grey70",
      guide    = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = scen_ltype_map,
      breaks = scen_levels,
      name   = "Scenario",
      guide  = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    )
  if (has_source) {
    p <- p + ggplot2::scale_alpha_manual(
      values = c(Baseline = 0.35, Policy = 1.0),
      breaks = c("Baseline", "Policy"),
      name   = "Source",
      guide  = ggplot2::guide_legend(order = 3,
                                     override.aes = list(linewidth = 0.9))
    )
  }
  p <- p +
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
