# Simulation comparison and visualisation ----
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
# NOTE: plot_bar_climate() had no active call sites and was removed
#   (formerly archived under dev/archived_fct/plot_bar_climate_archived.R).

# Label helpers ----

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
    key # fallback: return key unchanged
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

# Band quantile resolver ----

#' Resolve Uncertainty Band Key to Quantile Pair
#'
#' Converts the UI uncertainty band selector key to a named numeric vector
#' used by plot_pointrange_climate() and summarise_vals().
#'
#' Single authoritative definition (DUP-01): the former duplicate in
#' fct_aggregation.R - whose "minmax" winsorised to 0.001/0.999 instead of the
#' full 0/1 range used here - has been deleted. This variant matches the
#' behaviour every caller has seen at runtime (fct_sim_compare.R is sourced
#' after fct_aggregation.R and previously overwrote it).
#'
#' @param band_key Character. One of "p25_p75", "p20_p80", "p10_p90",
#'   "p05_p95", "p025_p975", "p005_p995", "minmax". Unknown keys fall back to
#'   the p10_p90 pair.
#' @return Named numeric vector c(lo = ..., hi = ...).
#' @export
resolve_band_q <- function(band_key) {
  switch(band_key,
    p25_p75 = c(lo = 0.25, hi = 0.75),
    p20_p80 = c(lo = 0.20, hi = 0.80),
    p10_p90 = c(lo = 0.10, hi = 0.90),
    p05_p95 = c(lo = 0.05, hi = 0.95),
    p025_p975 = c(lo = 0.025, hi = 0.975),
    p005_p995 = c(lo = 0.005, hi = 0.995),
    minmax = c(lo = 0.00, hi = 1.00),
    c(lo = 0.10, hi = 0.90) # default fallback
  )
}

# Internal helpers: parse scenario key components ----
# .normalise_ssp() and .parse_year() live in fct_simulations.R (single robust
# implementation - do not re-define them here).

# Internal: dynamic year linetype helper ----
.resolve_year_styles <- function(year_labels) {
  linetypes <- c("solid", "dashed", "dotted", "longdash", "twodash")
  years <- sort(unique(year_labels))
  n <- length(years)
  lty <- linetypes[seq_len(min(n, length(linetypes)))]
  list(
    linetype_map = setNames(lty, years),
    alpha_map    = setNames(rep(1.0, n), years)
  )
}

# Grouped point-range chart ----

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
#' @param bands_tbl  Data frame of per-scenario/per-year aggregates with a
#'   central value and band columns (from the Step 2 band assembly).
#' @param x_label   Scalar character y-axis label (outcome units).
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"} x-axis ordering.
#' @param show_coef Logical. Show the thin coefficient-uncertainty line.
#'   Default TRUE.
#' @param show_annual Logical. Show the inter-annual interval. The decision-
#'   first results view leaves this off because annual variation is shown in a
#'   separate distribution plot.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_x_discrete labs theme_minimal theme
#'   element_blank element_text margin
#' @importFrom colorspace lighten
#' @importFrom dplyr bind_rows
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @export
# Shared preparation behind plot_pointrange_climate() and its echarts
# counterpart echart_pointrange_climate(): scenario keys, the SSP x period
# colour palette, and the category order with spacer gutters. Pure data prep
# - no rendering - so both surfaces stay in lockstep (guidelines §7).
# @noRd
.pointrange_prep <- function(bands_tbl, group_order = "scenario_x_year") {
  df <- bands_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
    # Historical from the policy source is identical to baseline historical -
    # drop it so the chart shows one historical dot (under Baseline).
    df <- df[!(df$is_historical & df$source == "Policy"), , drop = FALSE]
  }
  df$ssp_key <- ifelse(df$is_historical, "Historical",
    vapply(df$scenario, .normalise_ssp, character(1L))
  )
  df$yr_lbl <- ifelse(df$is_historical, "Historical",
    vapply(df$scenario, .parse_year, character(1L))
  )
  df$ssp_short <- ifelse(df$is_historical, "Historical",
    SSP_SHORT_LABELS[df$ssp_key] %||% df$ssp_key
  )
  df$pt_key <- ifelse(df$is_historical, "Historical",
    paste0(df$ssp_short, "\n", df$yr_lbl)
  )
  df$colour_key <- ifelse(df$is_historical, "Historical",
    paste(df$ssp_key, df$yr_lbl, sep = "__")
  )

  fut_df <- df[!df$is_historical, , drop = FALSE]

  # colour palette: Historical = grey; one colour per SSP x year ----
  colour_palette <- c("Historical" = "#808080")
  if (nrow(fut_df) > 0L) {
    yrs_present <- sort(unique(fut_df$yr_lbl))
    n_yrs <- length(yrs_present)
    yr_lighten <- if (n_yrs > 1L) seq(0.30, 0.0, length.out = n_yrs) else 0.0
    for (ssp in intersect(names(.ssp_colours), unique(fut_df$ssp_key))) {
      base_col <- .ssp_colours[ssp]
      for (i in seq_along(yrs_present)) {
        ck <- paste(ssp, yrs_present[i], sep = "__")
        colour_palette[ck] <- colorspace::lighten(base_col, yr_lighten[i])
      }
    }
  }

  # x-axis factor levels with spacers ----
  ordered_levels <- "Historical"
  spacer_ids <- character(0)
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
        for (ssp in ssps_present) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    } else {
      for (ssp in ssps_present) {
        spacer_n <- spacer_n + 1L
        sid <- strrep(" ", spacer_n)
        spacer_ids <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(fut_df$yr_lbl[fut_df$ssp_short == ssp]))
        for (yr_i in ssp_yrs) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
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

  list(
    df = df,
    palette = colour_palette,
    ordered_levels = ordered_levels,
    spacer_ids = spacer_ids,
    x_label_map = x_label_map,
    data_levels = data_levels,
    has_source = has_source
  )
}

plot_pointrange_climate <- function(bands_tbl,
                                    x_label = "",
                                    group_order = "scenario_x_year",
                                    show_coef = TRUE,
                                    show_annual = FALSE) {
  if (is.null(bands_tbl) || nrow(bands_tbl) == 0L) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }

  prep <- .pointrange_prep(bands_tbl, group_order)
  df <- prep$df
  has_source <- prep$has_source
  colour_palette <- prep$palette
  ordered_levels <- prep$ordered_levels
  spacer_ids <- prep$spacer_ids
  x_label_map <- prep$x_label_map
  data_levels <- prep$data_levels

  if (nrow(df) == 0L) {
    return(blank_plot("Run a future simulation to see scenario comparisons."))
  }

  # plot: nested bands + dot ----
  # When a `source` column is present we dodge Baseline vs Policy side-by-side
  # within each scenario. Otherwise (Mod 2) the chart is single-source and
  # the dodge collapses to no-op via a single-level factor.
  pos <- if (has_source) {
    ggplot2::position_dodge(width = 0.55)
  } else {
    ggplot2::position_identity()
  }
  aes_base <- if (has_source) {
    ggplot2::aes(
      x = .data$pt_key, colour = .data$colour_key,
      group = .data$source
    )
  } else {
    ggplot2::aes(x = .data$pt_key, colour = .data$colour_key)
  }
  p <- ggplot2::ggplot(df, aes_base)

  p <- p + ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$intermod_lo, ymax = .data$intermod_hi),
    linewidth = 6.0, alpha = 0.6, na.rm = TRUE, position = pos
  )

  if (isTRUE(show_annual)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$interann_lo, ymax = .data$interann_hi),
      linewidth = 3.5, alpha = 1.0, na.rm = TRUE, position = pos
    )
  }

  if (isTRUE(show_coef)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$coef_lo, ymax = .data$coef_hi),
      linewidth = 1.2, colour = .wise_support, na.rm = TRUE, position = pos
    )
  }

  if (has_source) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        y = .data$value, shape = .data$source,
        fill = .data$source
      ),
      size = 3, stroke = 1.2, colour = .wise_support, na.rm = TRUE, position = pos
    ) +
      ggplot2::scale_shape_manual(
        values = c(Baseline = 21, Policy = 23), name = NULL, drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(Baseline = "white", Policy = .wise_policy),
        name = NULL, drop = FALSE
      )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      size = 3, shape = 21, fill = "white", colour = .wise_support, na.rm = TRUE
    )
  }

  p +
    ggplot2::scale_colour_manual(values = colour_palette, guide = "none") +
    ggplot2::scale_x_discrete(
      limits = ordered_levels,
      labels = x_label_map, drop = FALSE
    ) +
    ggplot2::labs(title = NULL, x = NULL, y = x_label) +
    theme_wise() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position    = if (has_source) "top" else "none"
    )
}

# Decision-first annual and paired summaries ----

# Build paired model-year effects from the two arms' canonical aggregate
# tables. F_agg is used whenever available so the coefficient interval retains
# baseline-policy covariance instead of treating the arms as independent.
paired_model_year_effects <- function(baseline_tbl, policy_tbl) {
  if (is.null(baseline_tbl) || is.null(policy_tbl) ||
    !nrow(baseline_tbl) || !nrow(policy_tbl)) {
    return(tibble::tibble())
  }

  rows <- lapply(intersect(baseline_tbl$sim_year, policy_tbl$sim_year), function(yr) {
    b <- baseline_tbl[baseline_tbl$sim_year == yr, , drop = FALSE][1L, ]
    p <- policy_tbl[policy_tbl$sim_year == yr, , drop = FALSE][1L, ]
    b_ids <- as.character(b$model_id[[1L]])
    p_ids <- as.character(p$model_id[[1L]])
    ids <- intersect(b_ids, p_ids)
    if (!length(ids)) {
      return(NULL)
    }
    b_vals <- stats::setNames(as.numeric(b$value_all[[1L]]), b_ids)
    p_vals <- stats::setNames(as.numeric(p$value_all[[1L]]), p_ids)
    b_sd <- stats::setNames(as.numeric(b$value_all_sd[[1L]]), b_ids)
    p_sd <- stats::setNames(as.numeric(p$value_all_sd[[1L]]), p_ids)
    b_f <- b$F_agg_all[[1L]]
    p_f <- p$F_agg_all[[1L]]
    out <- lapply(ids, function(id) {
      i_b <- match(id, b_ids)
      i_p <- match(id, p_ids)
      f_b <- if (is.matrix(b_f) && nrow(b_f) >= i_b) b_f[i_b, ] else NULL
      f_p <- if (is.matrix(p_f) && nrow(p_f) >= i_p) p_f[i_p, ] else NULL
      sd_effect <- if (!is.null(f_b) && !is.null(f_p) &&
        length(f_b) == length(f_p)) {
        sqrt(sum((f_p - f_b)^2, na.rm = TRUE))
      } else {
        sqrt((b_sd[[id]] %||% 0)^2 + (p_sd[[id]] %||% 0)^2)
      }
      tibble::tibble(
        sim_year = yr, model_id = id,
        baseline = b_vals[[id]], policy = p_vals[[id]],
        effect = p_vals[[id]] - b_vals[[id]],
        effect_sd = sd_effect,
        effect_gradient = list(if (!is.null(f_b) && !is.null(f_p) &&
          length(f_b) == length(f_p)) {
          f_p - f_b
        } else {
          NULL
        })
      )
    })
    dplyr::bind_rows(out)
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# The mean option also reports matched endpoint levels. The median default
# preserves secondary callers; medians of levels/components need not reconcile.
paired_effect_summary <- function(effect_tbl,
                                  band_q = c(lo = 0.10, hi = 0.90),
                                  scenario = "",
                                  center = c("median", "equal_model_mean")) {
  center <- match.arg(center)
  if (is.null(effect_tbl) || !nrow(effect_tbl)) {
    return(NULL)
  }
  models <- split(effect_tbl, effect_tbl$model_id)
  model_rows <- lapply(models, function(x) {
    valid <- is.finite(x$effect)
    if (identical(center, "equal_model_mean")) {
      valid <- valid & is.finite(x$baseline) & is.finite(x$policy)
    }
    x <- x[valid, , drop = FALSE]
    if (!nrow(x)) {
      return(NULL)
    }
    q <- stats::quantile(x$effect, probs = band_q, na.rm = TRUE, names = FALSE)
    gradients <- if ("effect_gradient" %in% names(x)) {
      x$effect_gradient[
        vapply(
          x$effect_gradient,
          function(g) is.numeric(g) && length(g) > 0L, logical(1L)
        )
      ]
    } else {
      list()
    }
    coef_sd <- if (length(gradients) == nrow(x)) {
      g <- Reduce(`+`, gradients) / nrow(x)
      sqrt(sum(g * g, na.rm = TRUE))
    } else {
      sqrt(mean(x$effect_sd^2, na.rm = TRUE) / max(nrow(x), 1L))
    }
    tibble::tibble(
      model_id = x$model_id[[1L]],
      mean_effect = mean(x$effect),
      baseline = if ("baseline" %in% names(x)) mean(x$baseline) else NA_real_,
      policy = if ("policy" %in% names(x)) mean(x$policy) else NA_real_,
      annual_lo = q[[1L]], annual_hi = q[[2L]],
      coef_sd = coef_sd,
      n_years = nrow(x)
    )
  })
  model_rows <- dplyr::bind_rows(Filter(Negate(is.null), model_rows))
  if (!nrow(model_rows)) {
    return(NULL)
  }
  center_fn <- if (identical(center, "equal_model_mean")) mean else stats::median
  center_value <- center_fn(model_rows$mean_effect, na.rm = TRUE)
  intermod <- stats::quantile(model_rows$mean_effect,
    probs = band_q,
    na.rm = TRUE, names = FALSE
  )
  interann <- c(
    lo = mean(model_rows$annual_lo, na.rm = TRUE),
    hi = mean(model_rows$annual_hi, na.rm = TRUE)
  )
  z <- stats::qnorm(band_q)
  coef_sd <- mean(model_rows$coef_sd, na.rm = TRUE)
  tibble::tibble(
    scenario = scenario, value = center_value,
    baseline = center_fn(model_rows$baseline, na.rm = TRUE),
    policy = center_fn(model_rows$policy, na.rm = TRUE),
    center_method = if (identical(center, "median")) "median_model_mean" else center,
    coef_lo = center_value + z[[1L]] * coef_sd,
    coef_hi = center_value + z[[2L]] * coef_sd,
    interann_lo = interann[[1L]], interann_hi = interann[[2L]],
    intermod_lo = intermod[[1L]], intermod_hi = intermod[[2L]],
    n_models = nrow(model_rows), n_years = min(model_rows$n_years),
    n_model_years = sum(model_rows$n_years),
    n_dropped_model_years = nrow(effect_tbl) - sum(model_rows$n_years)
  )
}

# Equal-probability tail contrast: calculate the adverse quantile separately
# in each arm for each matched model, then subtract policy minus baseline.
paired_equal_probability_effects <- function(effect_tbl, probs) {
  if (is.null(effect_tbl) || !nrow(effect_tbl) || !length(probs)) {
    return(tibble::tibble())
  }
  rows <- lapply(split(effect_tbl, effect_tbl$model_id), function(x) {
    do.call(rbind, lapply(probs, function(prob) {
      if (sum(is.finite(x$baseline)) < 2L || sum(is.finite(x$policy)) < 2L) {
        return(NULL)
      }
      tibble::tibble(
        model_id = x$model_id[[1L]], probability = prob,
        baseline = as.numeric(stats::quantile(x$baseline, prob,
          na.rm = TRUE, names = FALSE
        )),
        policy = as.numeric(stats::quantile(x$policy, prob,
          na.rm = TRUE, names = FALSE
        ))
      ) |>
        dplyr::mutate(effect = policy - baseline)
    }))
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

paired_effect_plot <- function(tbl, x_label = "Policy effect (outcome units)") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Paired policy effects are unavailable."))
  }
  tbl$scenario <- factor(tbl$scenario, levels = rev(unique(tbl$scenario)))
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$value, y = .data$scenario)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$intermod_lo,
        xend = .data$intermod_hi,
        y = .data$scenario, yend = .data$scenario
      ),
      linewidth = 5, colour = .wise_policy, alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$coef_lo,
        xend = .data$coef_hi,
        y = .data$scenario, yend = .data$scenario
      ),
      linewidth = 1.2, colour = .wise_support, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      shape = 21, size = 3.2, fill = .wise_policy,
      colour = .wise_policy_dark, stroke = 0.8, na.rm = TRUE
    ) +
    ggplot2::labs(
      x = x_label, y = NULL,
      subtitle = paste(
        if ("center_method" %in% names(tbl) &&
          all(tbl$center_method == "equal_model_mean")) "Equal-model mean." else "Median across climate-model means.",
        "Thick interval = ensemble spread; thin interval = coefficient uncertainty (baseline-X approximation)."
      )
    ) +
    theme_wise()
}


plot_annual_distribution <- function(tbl, x_label = "Outcome (outcome units)",
                                     title = NULL, subtitle = NULL,
                                     plot_type = "violin") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("No annual simulation results available."))
  }
  df <- tbl
  df$scenario <- as.character(df$scenario)
  df$period <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .parse_year, character(1L))
  )
  df$ssp <- ifelse(df$scenario == "Historical", "Historical",
    vapply(df$scenario, .normalise_ssp, character(1L))
  )
  scenario_levels <- c("Historical", sort(unique(df$scenario[df$scenario != "Historical"])))
  scenario_palette <- c(Historical = .wise_history)
  for (ssp in unique(df$ssp[df$ssp != "Historical"])) {
    members <- scenario_levels[scenario_levels != "Historical"]
    members <- members[vapply(members, function(s) {
      identical(.normalise_ssp(s), ssp)
    }, logical(1L))]
    members <- members[order(vapply(members, .parse_year, character(1L)))]
    base_col <- if (ssp %in% names(.ssp_colours)) {
      unname(.ssp_colours[[ssp]])
    } else {
      "#0072B2"
    }
    shades <- if (length(members) > 1L) {
      colorspace::lighten(base_col, seq(0.30, 0, length.out = length(members)))
    } else {
      base_col
    }
    scenario_palette[members] <- shades
  }
  df$scenario_key <- factor(df$scenario, levels = scenario_levels)

  has_source <- "source" %in% names(df) && length(unique(df$source)) > 1L
  plot_type <- match.arg(plot_type, c("violin", "boxplot"))

  # Reference line: the historical *baseline* mean. With both sources
  # present, pooling them would average baseline and policy outcomes.
  hist_vals <- if (has_source) {
    h <- df$value[df$scenario == "Historical" & df$source == "Baseline"]
    if (!length(h)) df$value[df$scenario == "Historical"] else h
  } else {
    df$value[df$scenario == "Historical"]
  }
  hist_mean <- if (length(hist_vals)) mean(hist_vals, na.rm = TRUE) else NA_real_

  # Horizontal Design-B/D layout: one row per scenario on a numeric y (so the
  # row banding lines up), Historical on top, and a single shared outcome
  # axis. The Violin/Boxplot toggle picks the row rendering: full violins
  # over dots (Design B) or slim boxes over faded dots (Design D).
  n_rows <- length(scenario_levels)
  df$row_y <- n_rows + 1L - as.integer(df$scenario_key)

  x_span <- diff(range(df$value, na.rm = TRUE))
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.015 * x_span else 0

  y_breaks <- sort(unique(df$row_y))
  # Rows count top-down from n_rows, so break value b maps back to level
  # index n_rows + 1 - b. Two-line gutter label: scenario name over period.
  y_labs <- vapply(y_breaks, function(b) {
    sub(" / ", "\n", scenario_levels[n_rows + 1L - b], fixed = TRUE)
  }, character(1L))

  # Alternating light-grey banding behind every second scenario row (counting
  # from the top), drawn first so gridlines show through.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  row_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  if (has_source) {
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
    # Two series share each scenario row: baseline takes the upper half-slot,
    # policy the lower one, so the pairs read as one group.
    src_off <- c(Baseline = 0.19, Policy = -0.19)
    df$y_off <- unname(src_off[as.character(df$source)])
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_rect(
      data = row_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    )

  if (is.finite(hist_mean)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = hist_mean, linetype = "dashed",
        colour = .wise_zero, linewidth = 0.5
      ) +
      ggplot2::annotate(
        "text",
        x = hist_mean, y = n_rows + 0.45, label = "Historical mean",
        hjust = 0, vjust = 0.5, size = 3.9, colour = .wise_zero
      )
  }

  # One-time Baseline/Policy captions on the top row, like the adverse
  # dumbbells: each anchored just past the right-most draw of its series.
  if (has_source) {
    top_scen <- scenario_levels[[1L]]
    top_row_y <- n_rows
    cap_df <- do.call(rbind, lapply(names(src_off), function(src) {
      vals <- df$value[df$scenario == top_scen & df$source == src]
      vals <- vals[is.finite(vals)]
      if (!length(vals)) {
        return(NULL)
      }
      data.frame(
        lab_x = max(vals) + lab_gap,
        y = top_row_y + unname(src_off[[src]]),
        label = src
      )
    }))
  } else {
    cap_df <- NULL
  }

  if (has_source) {
    distribution_layer <- if (identical(plot_type, "violin")) {
      ggplot2::geom_violin(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          fill = .data$scenario_key, alpha = .data$source,
          group = interaction(.data$scenario_key, .data$source)
        ),
        orientation = "y", scale = "width", width = 0.34, colour = NA,
        na.rm = TRUE
      )
    } else {
      ggplot2::geom_boxplot(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          fill = .data$scenario_key, alpha = .data$source,
          group = interaction(.data$scenario_key, .data$source)
        ),
        orientation = "y", width = 0.16, outlier.shape = NA,
        colour = .wise_support, na.rm = TRUE
      )
    }

    mean_df <- stats::aggregate(value ~ scenario_key + source,
      data = df,
      FUN = mean, na.rm = TRUE
    )
    mean_df$y_off <- unname(src_off[as.character(mean_df$source)])
    mean_df$row_y <- n_rows + 1L - as.integer(mean_df$scenario_key)

    p <- p + distribution_layer +
      ggplot2::geom_point(
        ggplot2::aes(
          y = .data$row_y + .data$y_off,
          colour = .data$scenario_key, alpha = .data$source
        ),
        # Jitter within the row only - never along the value axis, or the
        # dots would smear past their true outcome values.
        position = ggplot2::position_jitter(width = 0, height = 0.07),
        size = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df[mean_df$source == "Baseline", , drop = FALSE],
        ggplot2::aes(x = .data$value, y = .data$row_y + .data$y_off),
        shape = 21, size = 3.0, fill = "white", colour = .wise_slate,
        stroke = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df[mean_df$source == "Policy", , drop = FALSE],
        ggplot2::aes(x = .data$value, y = .data$row_y + .data$y_off),
        shape = 21, size = 3.4, fill = .wise_policy, colour = .wise_policy_dark,
        stroke = 1.0, na.rm = TRUE
      ) +
      ggplot2::scale_alpha_manual(
        values = c(Baseline = 0.30, Policy = 0.85), guide = "none"
      )
  } else {
    distribution_layer <- if (identical(plot_type, "violin")) {
      ggplot2::geom_violin(
        ggplot2::aes(y = .data$row_y, fill = .data$scenario_key),
        orientation = "y", scale = "width", width = 0.62, alpha = 0.28,
        colour = NA, na.rm = TRUE
      )
    } else {
      ggplot2::geom_boxplot(
        ggplot2::aes(y = .data$row_y, fill = .data$scenario_key),
        orientation = "y", width = 0.30, outlier.shape = NA,
        alpha = 0.45, colour = .wise_support, na.rm = TRUE
      )
    }

    dot_jitter <- if (identical(plot_type, "violin")) {
      ggplot2::position_jitter(width = 0, height = 0.18)
    } else {
      ggplot2::position_jitter(width = 0, height = 0.26)
    }
    dot_alpha <- if (identical(plot_type, "violin")) 0.40 else 0.30

    mean_df <- stats::aggregate(value ~ scenario_key,
      data = df,
      FUN = mean, na.rm = TRUE
    )
    mean_df$row_y <- n_rows + 1L - as.integer(mean_df$scenario_key)

    p <- p + distribution_layer +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$row_y, colour = .data$scenario_key),
        position = dot_jitter, alpha = dot_alpha, size = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = mean_df,
        ggplot2::aes(x = .data$value, y = .data$row_y),
        shape = 21, size = 3.0, fill = "white", colour = .wise_slate,
        stroke = 1.0, na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::scale_fill_manual(
      values = scenario_palette,
      na.value = .wise_history, guide = "none"
    ) +
    ggplot2::scale_colour_manual(
      values = scenario_palette,
      na.value = .wise_history, guide = "none"
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks, labels = y_labs,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      # Reserve right-hand room for the one-time Baseline/Policy captions.
      expand = ggplot2::expansion(mult = c(0.02, if (has_source) 0.12 else 0.02))
    ) +
    ggplot2::labs(x = x_label, y = NULL, title = title, subtitle = subtitle) +
    theme_wise() +
    ggplot2::theme(legend.position = "none")

  if (!is.null(cap_df)) {
    p <- p + ggplot2::geom_text(
      data = cap_df,
      ggplot2::aes(x = .data$lab_x, y = .data$y, label = .data$label),
      hjust = 0, size = 3.9, fontface = "bold",
      colour = c(.wise_slate, .wise_policy_dark), inherit.aes = FALSE,
      show.legend = FALSE, na.rm = TRUE
    )
  }
  p
}


paired_adverse_effect_table <- function(effect_tbl,
                                        metric = NULL,
                                        band_q = c(lo = 0.10, hi = 0.90)) {
  if (is.null(effect_tbl) || !nrow(effect_tbl)) {
    return(tibble::tibble())
  }
  probs <- c(
    "Expected" = 0.50, "Adverse 1-in-5" = 0.20,
    "Adverse 1-in-10" = 0.10, "Adverse 1-in-20" = 0.05
  )
  spec <- metric_metadata(metric %||% "mean")
  target <- if (identical(spec$adverse_tail, "high")) 1 - probs else probs
  rows <- lapply(split(effect_tbl, effect_tbl$model_id), function(x) {
    do.call(rbind, lapply(seq_along(probs), function(i) {
      b <- x$baseline[is.finite(x$baseline)]
      p <- x$policy[is.finite(x$policy)]
      # An empirical 1-in-N tail is reported only when the model has at least
      # N usable weather years. This avoids presenting the single most extreme
      # draw as a supported return-period estimate.
      support_n <- ceiling(1 / min(probs[[i]], 1 - probs[[i]]))
      if (length(b) < support_n || length(p) < support_n) {
        return(NULL)
      }
      data.frame(
        model_id = x$model_id[[1L]], period = names(probs)[[i]],
        probability = probs[[i]],
        baseline = as.numeric(stats::quantile(b, target[[i]], names = FALSE)),
        policy = as.numeric(stats::quantile(p, target[[i]], names = FALSE)),
        stringsAsFactors = FALSE
      )
    }))
  })
  long <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(long)) {
    return(long)
  }
  dplyr::group_by(long, .data$period, .data$probability) |>
    dplyr::summarise(
      baseline = stats::median(.data$baseline, na.rm = TRUE),
      policy = stats::median(.data$policy, na.rm = TRUE),
      effect = .data$policy - .data$baseline,
      ensemble_lo = stats::quantile(.data$policy - .data$baseline,
        band_q[[1L]],
        na.rm = TRUE
      ),
      ensemble_hi = stats::quantile(.data$policy - .data$baseline,
        band_q[[2L]],
        na.rm = TRUE
      ),
      n_models = dplyr::n_distinct(.data$model_id),
      n_weather_years = dplyr::n(),
      .groups = "drop"
    )
}

#' Data behind the Step 2 return-period dot plot (Figure S2-4)
#' @noRd
step2_adverse_dot_data <- function(threshold_tbl, method = "mean", so = NULL) {
  if (is.null(threshold_tbl) || !nrow(threshold_tbl)) {
    return(tibble::tibble())
  }
  rp_map <- metric_decision_return_periods(method, so)
  keep_rps <- unname(rp_map)
  tbl <- threshold_tbl[threshold_tbl$rp_name %in% keep_rps, , drop = FALSE]
  if (!nrow(tbl)) {
    return(tibble::tibble())
  }

  central <- tbl[tbl$Estimate == "Central (P50)", , drop = FALSE]
  if (!nrow(central)) {
    return(tibble::tibble())
  }
  central$rp_label <- names(rp_map)[match(central$rp_name, unname(rp_map))]

  ens_rows <- tbl[grepl("^Ensemble ", tbl$Estimate), , drop = FALSE]
  if (nrow(ens_rows)) {
    # The displayed labels vary between min/max and Pxx. Within each
    # scenario, threshold_table_rv creates the complete lower vector before
    # the complete upper vector, so split by scenario before pairing RPs.
    parts <- lapply(split(ens_rows, ens_rows$scenario), function(x) {
      n_each <- nrow(x) %/% 2L
      if (n_each < 1L || nrow(x) != 2L * n_each) {
        return(NULL)
      }
      list(
        lo = x[seq_len(n_each), , drop = FALSE],
        hi = x[seq.int(n_each + 1L, nrow(x)), , drop = FALSE]
      )
    })
    parts <- Filter(Negate(is.null), parts)
    ens_lo <- dplyr::bind_rows(lapply(parts, `[[`, "lo"))
    ens_hi <- dplyr::bind_rows(lapply(parts, `[[`, "hi"))
  } else {
    ens_lo <- ens_hi <- tbl[FALSE, , drop = FALSE]
  }

  central$intermod_lo <- NA_real_
  central$intermod_hi <- NA_real_
  for (i in seq_len(nrow(central))) {
    sc <- central$scenario[[i]]
    rp <- central$rp_name[[i]]
    lo_val <- ens_lo$value[ens_lo$scenario == sc & ens_lo$rp_name == rp]
    hi_val <- ens_hi$value[ens_hi$scenario == sc & ens_hi$rp_name == rp]
    if (length(lo_val)) central$intermod_lo[[i]] <- lo_val[[1L]]
    if (length(hi_val)) central$intermod_hi[[i]] <- hi_val[[1L]]
  }

  central$ssp_key <- ifelse(central$is_historical, "Historical",
    vapply(central$scenario, .normalise_ssp, character(1L))
  )
  central$yr_lbl <- ifelse(central$is_historical, "Historical",
    vapply(central$scenario, .parse_year, character(1L))
  )
  central$rp_label <- factor(
    central$rp_label,
    levels = rev(c(
      "Expected", "Adverse 1-in-5", "Adverse 1-in-10",
      "Adverse 1-in-20", "Adverse 1-in-50"
    ))
  )
  central
}

#' Render Step 2 Return-Period Dot Plot (Figure S2-4)
#'
#' Design D: return-period names sit on the y axis outside the panel, the
#' scenario legend is replaced by direct labels anchored at the right end of
#' each top-row series, and Historical uses the exceedance plot's dark
#' support colour. Scenario-coloured bands keep encoding climate-model spread.
#' @noRd
plot_step2_adverse_dot <- function(tbl, x_label = "Outcome level",
                                   title = NULL, subtitle = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Return-period outcomes are unavailable."))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  tbl$scenario_key <- factor(
    ifelse(tbl$is_historical, "Historical", as.character(tbl$scenario)),
    levels = scenario_levels
  )
  # Vertical dodge: multiple scenarios share each return-period row, so
  # offset the markers per scenario to keep them readable. Historical
  # keeps the lower slot; each future scenario takes its own slot above it.
  # Wide multi-SSP runs need generous slot spacing to avoid crowding.
  dodge_width <- 0.6
  tbl$rp_y <- as.integer(tbl$rp_label)
  tbl$dodge_offset <- stats::ave(
    seq_len(nrow(tbl)),
    tbl$rp_y,
    FUN = function(idx) {
      k <- length(idx)
      if (k <= 1L) {
        return(0)
      }
      seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k)[
        order(match(as.character(tbl$scenario_key[idx]), scenario_levels))
      ] * (dodge_width / max(k - 1L, 1))
    }
  )

  # Direct scenario labels replace the colour legend: anchored just past the
  # right end of each series' top-row (Expected) marker, with the x expansion
  # enlarged so the longest label stays inside the panel.
  top_y <- max(tbl$rp_y)
  top_rows <- tbl[tbl$rp_y == top_y, , drop = FALSE]
  # One label per scenario even if a scenario contributes several top rows.
  top_rows <- top_rows[!duplicated(top_rows$scenario_key), , drop = FALSE]

  x_vals <- c(tbl$value, tbl$intermod_lo, tbl$intermod_hi)
  x_span <- diff(range(x_vals, na.rm = TRUE))
  # Uniform label gap, in data units, so every series label clears its
  # marker by the same distance.
  lab_gap <- if (is.finite(x_span) && x_span > 0) 0.012 * x_span else 0

  top_rows$lab_x <- vapply(seq_len(nrow(top_rows)), function(i) {
    r <- top_rows[i, ]
    hi <- r$intermod_hi[[1L]]
    if (!is.finite(hi)) hi <- r$value[[1L]]
    hi + lab_gap
  }, numeric(1L))
  top_rows$lab_col <- unname(scenario_colours[as.character(top_rows$scenario_key)])

  right_mult <- if (is.finite(x_span) && x_span > 0) {
    max(0.12, max(nchar(as.character(top_rows$scenario_key)), 0L) * 0.012)
  } else {
    0.05
  }

  y_breaks <- sort(unique(tbl$rp_y))
  y_labs <- levels(tbl$rp_label)[y_breaks]

  # Alternating light-grey banding behind every second return-period row
  # (counting from the top), so the markers that share a row read as one
  # group. Drawn first, i.e. behind every geom.
  band_ys <- y_breaks[(max(y_breaks) - y_breaks) %% 2 == 1]
  rp_band_data <- data.frame(
    ymin = band_ys - 0.45, ymax = band_ys + 0.45
  )

  p <- ggplot2::ggplot(tbl, ggplot2::aes(
    y = .data$rp_y + .data$dodge_offset,
    x = .data$value,
    colour = .data$scenario_key,
    fill = .data$scenario_key
  )) +
    ggplot2::geom_rect(
      data = rp_band_data,
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
      xmin = -Inf, xmax = Inf, fill = ggplot2::alpha("#F7F9FB", 0.5), colour = NA,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$intermod_lo, xend = .data$intermod_hi,
        yend = .data$rp_y + .data$dodge_offset
      ),
      linewidth = 2.0, alpha = 0.5, lineend = "round", na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 3.4, stroke = 1.1, shape = 21, na.rm = TRUE) +
    # One-time series labels on the top row, coloured per scenario.
    ggplot2::geom_text(
      data = top_rows,
      ggplot2::aes(
        x = .data$lab_x, y = .data$rp_y + .data$dodge_offset,
        label = as.character(.data$scenario_key)
      ),
      colour = top_rows$lab_col, hjust = 0, size = 3.9,
      fontface = "bold", show.legend = FALSE, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = scenario_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = scenario_colours, guide = "none") +
    # Return-period names are the y-axis text, outside the panel like every
    # other chart.
    ggplot2::scale_y_continuous(
      breaks = y_breaks, labels = y_labs,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.02, right_mult))
    ) +
    ggplot2::labs(
      x = x_label, y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = "none"
    )
  # Multiple future periods stay on one shared x-axis: every period variant
  # is its own scenario row with its own top-row label, so no faceting.
  p
}

# Step 2 headline cards ----

#' Build Step 2 Results Headline Cards
#'
#' Pure function returning a list of 5 card specifications for
#' \code{headline_cards_ui()}, matching the \code{mod_1} design language:
#' \enumerate{
#'   \item Typical weather year outcome (Scenario expected, historical baseline, change)
#'   \item Adverse weather year outcomes (1-in-10 and 1-in-20 thresholds)
#'   \item Weather-year range (range across years: inter-annual weather variability)
#'   \item Climate-model spread (range across models & coefficient uncertainty)
#'   \item Simulation years (tally of simulation years: scenarios * models * years)
#' }
#'
#' @param bands Data frame from \code{pointrange_bands_rv()}.
#' @param threshold_tbl Data frame from \code{threshold_table_rv()}.
#' @param hist_sim List from \code{hist_sim()}.
#' @param saved_scenarios List of saved future scenarios.
#' @param method Character aggregation method (default "mean").
#' @param timeseries_curves Data frame from \code{timeseries_curves_rv()}.
#'
#' @return A list of 5 card lists ready for \code{headline_cards_ui()}.
#' @noRd
step2_headline_cards <- function(bands,
                                 threshold_tbl = NULL,
                                 hist_sim = NULL,
                                 saved_scenarios = list(),
                                 method = "mean",
                                 timeseries_curves = NULL,
                                 deviation = "none",
                                 metadata = NULL) {
  if (is.null(bands) || !nrow(bands) ||
    !all(c("is_historical", "scenario") %in% names(bands))) {
    return(NULL)
  }

  hist_rows <- bands[bands$is_historical, , drop = FALSE]
  fut_rows <- bands[!bands$is_historical, , drop = FALSE]
  if (!nrow(hist_rows) && !nrow(fut_rows)) {
    return(NULL)
  }

  hist <- if (nrow(hist_rows)) hist_rows[1L, ] else fut_rows[1L, ]
  focus <- if (nrow(fut_rows)) fut_rows[1L, ] else hist
  has_future <- nrow(fut_rows) > 0L

  so <- if (!is.null(hist_sim)) hist_sim$so else NULL
  metadata <- metadata %||% metric_metadata(method, so)
  is_change <- !identical(deviation, "none")
  deviation_label <- switch(deviation,
    none = "Outcome level",
    mean = "Difference from historical mean",
    median = "Difference from historical median",
    "Outcome level"
  )
  display_value <- function(x, change = is_change) {
    if (length(x) != 1L || !is.finite(suppressWarnings(as.numeric(x)))) {
      return("Unavailable")
    }
    if (isTRUE(change)) {
      return(format_metric_value(as.numeric(x), metadata, change = TRUE, digits = 2))
    }
    value <- format_metric_value(as.numeric(x), metadata, change = FALSE, digits = 2)
    if (identical(metadata$format, "percent")) return(value)
    unit <- metadata$level_unit %||% "outcome units"
    suffix <- paste0(" ", unit)
    if (endsWith(value, suffix)) substr(value, 1L, nchar(value) - nchar(suffix)) else value
  }

  # 1. Typical weather year outcome
  val_1 <- if (has_future) {
    paste0(display_value(hist$value), " vs ", display_value(focus$value))
  } else {
    display_value(hist$value)
  }

  line1_1 <- if (has_future) {
    paste(if (is_change) deviation_label else "Outcome level", "· Historical vs SSP")
  } else {
    paste(if (is_change) deviation_label else "Outcome level", "· Historical baseline")
  }
  # The expected outcome is the mean across simulated weather years. This is
  # separate from the selected household-level aggregation within each year.
  weather_year_label <- "Years averaged within model; climate models weighted equally"

  card1 <- list(
    label = "Expected outcome",
    value = val_1,
    note = paste(line1_1, weather_year_label, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_1),
      shiny::tags$div(style = "font-weight: 600;", weather_year_label)
    ),
    info = paste(
      "Expected annual aggregate outcome under the historical baseline compared with the",
      "focus climate scenario (mean across weather years and climate models).",
      "Differences reflect simulated climate conditions for the fixed survey population.",
      metric_context_note(metadata)
    )
  )
  card1$value_native <- as.numeric(c(hist$value, if (has_future) focus$value else NA_real_))
  card1$change_native <- if (has_future) focus$value - hist$value else NA_real_
  card1$change_display <- if (has_future) {
    display_value(focus$value - hist$value, change = TRUE)
  } else {
    ""
  }
  if (nzchar(card1$change_display)) {
    card1$note <- paste(card1$note, paste("Difference:", card1$change_display), sep = " · ")
    card1$note_html <- shiny::tagList(card1$note_html, shiny::tags$div(paste("Difference:", card1$change_display)))
  }

  # 2. Adverse weather year outcomes (1-in-20 year)
  v20_hist <- NA_real_
  v20_ssp <- NA_real_
  if (!is.null(threshold_tbl) && nrow(threshold_tbl) &&
    all(c("scenario", "rp_name", "Estimate") %in% names(threshold_tbl))) {
    rp_map <- metric_decision_return_periods(method %||% "mean", so)
    rp_20 <- unname(rp_map[["Adverse 1-in-20"]])
    r20_hist <- threshold_tbl[threshold_tbl$scenario == "Historical" &
      threshold_tbl$rp_name == rp_20 &
      threshold_tbl$Estimate == "Central (P50)", , drop = FALSE]
    r20_ssp <- threshold_tbl[threshold_tbl$scenario == focus$scenario &
      threshold_tbl$rp_name == rp_20 &
      threshold_tbl$Estimate == "Central (P50)", , drop = FALSE]
    if (nrow(r20_hist) && is.finite(r20_hist$value[[1L]])) v20_hist <- r20_hist$value[[1L]]
    if (nrow(r20_ssp) && is.finite(r20_ssp$value[[1L]])) v20_ssp <- r20_ssp$value[[1L]]
  }

  if (has_future && is.finite(v20_hist) && is.finite(v20_ssp)) {
    val_2 <- paste0(display_value(v20_hist, change = FALSE), " vs ", display_value(v20_ssp, change = FALSE))
    line1_2 <- paste(if (is_change) deviation_label else "Outcome level", "· Historical vs SSP")
    line2_2 <- "1-in-20 year"
  } else if (!has_future && is.finite(v20_hist)) {
    val_2 <- display_value(v20_hist, change = FALSE)
    line1_2 <- paste(if (is_change) deviation_label else "Outcome level", "· Historical baseline")
    line2_2 <- "1-in-20 year"
  } else if (has_future && is.finite(v20_ssp)) {
    val_2 <- display_value(v20_ssp, change = FALSE)
    line1_2 <- as.character(focus$scenario)
    line2_2 <- "1-in-20 year"
  } else {
    val_2 <- "Unavailable"
    line1_2 <- "Requires \u226520 weather years"
    line2_2 <- "1-in-20 year"
  }

  card2 <- list(
    label = "Adverse weather years",
    value = val_2,
    note = paste(line1_2, line2_2, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_2),
      shiny::tags$div(style = "font-weight: 600;", line2_2)
    ),
    info = paste(
      "Simulated aggregate outcome in adverse 1-in-20 weather years under the historical",
      "baseline compared with the focus climate regime. A 1-in-20 year event occurs in",
      "approximately 5% of simulated weather years. The adverse tail is determined",
      "automatically by the selected metric. Thresholds use the median across climate models,",
      "not the equal-model-mean expected headline.",
      metric_context_note(metadata)
    )
  )
  card2$value_native <- as.numeric(c(v20_hist, v20_ssp))
  card2$change_native <- if (has_future && is.finite(v20_hist) && is.finite(v20_ssp)) {
    v20_ssp - v20_hist
  } else {
    NA_real_
  }
  card2$change_display <- if (has_future && is.finite(v20_hist) && is.finite(v20_ssp)) {
    display_value(v20_ssp - v20_hist, change = TRUE)
  } else {
    ""
  }
  if (nzchar(card2$change_display)) {
    card2$note <- paste(card2$note, paste("Difference:", card2$change_display), sep = " · ")
    card2$note_html <- shiny::tagList(card2$note_html, shiny::tags$div(paste("Difference:", card2$change_display)))
  }

  # 3. Range across years (inter-annual weather variability). For future
  # scenarios, first calculate each model's observed year range, then average
  # the lower and upper endpoints across models. This is different from taking
  # quantiles of the pooled model-year values, which can overweight extremes.
  finite_range <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) {
      return(c(lo = NA_real_, hi = NA_real_))
    }
    c(lo = min(x), hi = max(x))
  }
  scenario_year_range <- function(scenario) {
    if (is.null(timeseries_curves) || !nrow(timeseries_curves) ||
      !all(c("scenario", "value") %in% names(timeseries_curves))) {
      return(c(lo = NA_real_, hi = NA_real_))
    }
    x <- timeseries_curves[timeseries_curves$scenario == scenario, , drop = FALSE]
    if (!nrow(x)) {
      return(c(lo = NA_real_, hi = NA_real_))
    }
    if ("model_id" %in% names(x)) {
      by_model <- split(x$value, x$model_id)
      ranges <- lapply(by_model, finite_range)
      ranges <- ranges[vapply(ranges, function(r) all(is.finite(r)), logical(1L))]
      if (!length(ranges)) {
        return(c(lo = NA_real_, hi = NA_real_))
      }
      c(
        lo = mean(vapply(ranges, `[[`, numeric(1L), "lo"), na.rm = TRUE),
        hi = mean(vapply(ranges, `[[`, numeric(1L), "hi"), na.rm = TRUE)
      )
    } else {
      finite_range(x$value)
    }
  }
  hist_range <- scenario_year_range("Historical")
  focus_range <- scenario_year_range(focus$scenario)
  if (!is.finite(focus_range[["lo"]])) {
    focus_range <- c(lo = focus$interann_lo, hi = focus$interann_hi)
  }
  if (!is.finite(hist_range[["lo"]])) {
    hist_range <- c(lo = hist$interann_lo, hi = hist$interann_hi)
  }

  val_3 <- if (has_future && all(is.finite(focus_range))) {
    paste(display_value(focus_range[["lo"]]), "to", display_value(focus_range[["hi"]]))
  } else if (all(is.finite(hist_range))) {
    paste(display_value(hist_range[["lo"]]), "to", display_value(hist_range[["hi"]]))
  } else {
    "Unavailable"
  }

  line1_3 <- if (has_future && all(is.finite(hist_range))) {
    paste0("Hist: ", display_value(hist_range[["lo"]]), " to ", display_value(hist_range[["hi"]]))
  } else {
    "Historical baseline"
  }
  line2_3 <- "Inter-annual weather variability"

  card3 <- list(
    label = "Range across years",
    value = val_3,
    note = paste(line1_3, line2_3, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_3),
      shiny::tags$div(style = "font-weight: 600;", line2_3)
    ),
    info = paste(
      "Range of annual population aggregate outcomes across simulated weather-year",
      "realizations within the selected climate regime. This characterises",
      "year-to-year weather fluctuations, not household inequality or continuous",
      "time forecasting."
    )
  )
  card3$value_range_native <- if (has_future && all(is.finite(focus_range))) {
    as.numeric(focus_range)
  } else if (all(is.finite(hist_range))) {
    as.numeric(hist_range)
  } else {
    c(NA_real_, NA_real_)
  }

  # 4. Climate-model spread (full range of model means at expected outcome)
  n_mods <- suppressWarnings(as.integer(focus$n_models %||% 1L))[1L]
  if (!is.finite(n_mods) || n_mods < 1L) n_mods <- 1L

  focus_model_means <- if (!is.null(timeseries_curves) && nrow(timeseries_curves) &&
    all(c("scenario", "value") %in% names(timeseries_curves))) {
    x <- timeseries_curves[timeseries_curves$scenario == focus$scenario, , drop = FALSE]
    if (nrow(x) && "model_id" %in% names(x)) {
      tapply(x$value, x$model_id, mean, na.rm = TRUE)
    } else {
      numeric(0L)
    }
  } else {
    numeric(0L)
  }
  focus_model_means <- as.numeric(focus_model_means[is.finite(focus_model_means)])

  val_4 <- if (has_future && length(focus_model_means) > 1L) {
    paste(display_value(min(focus_model_means)), "to", display_value(max(focus_model_means)))
  } else if (has_future) {
    display_value(focus$value)
  } else {
    "Not applicable"
  }

  line1_4 <- if (has_future && length(focus_model_means) > 1L) {
    paste0("Full range across ", length(focus_model_means), " models")
  } else if (has_future) {
    "Single climate model"
  } else {
    "Single historical climate series"
  }
  line2_4 <- "CMIP6 model disagreement"

  card4 <- list(
    label = "Climate-model spread",
    value = val_4,
    note = paste(line1_4, line2_4, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(line1_4),
      shiny::tags$div(style = "font-weight: 600;", line2_4)
    ),
    info = paste(
      "Range of expected annual aggregate outcomes across CMIP6 climate models for",
      "the focus scenario. Reflects climate projection disagreement, evaluated at the",
      "central expected outcome."
    )
  )
  card4$value_range_native <- if (has_future && length(focus_model_means) > 1L) {
    c(min(focus_model_means), max(focus_model_means))
  } else if (has_future) {
    c(focus$value, NA_real_)
  } else {
    c(NA_real_, NA_real_)
  }

  # 5. Tally of simulation years (e.g. scenarios * models * years...)
  run_info <- if (!is.null(hist_sim)) hist_sim$sim_summary %||% list() else list()

  # Number of simulation years per model (evaluated on focus scenario)
  hist_years <- run_info$historical_years %||% integer(0)
  focus_curves <- if (!is.null(timeseries_curves) && nrow(timeseries_curves)) {
    timeseries_curves[timeseries_curves$scenario == focus$scenario, , drop = FALSE]
  } else {
    NULL
  }

  n_years <- if (!is.null(focus_curves) && nrow(focus_curves)) {
    length(unique(focus_curves$sim_year))
  } else if (!is.null(timeseries_curves) && nrow(timeseries_curves)) {
    length(unique(timeseries_curves$sim_year))
  } else if (length(hist_years) >= 2L && is.finite(hist_years[1]) && is.finite(hist_years[2])) {
    as.integer(hist_years[2] - hist_years[1] + 1L)
  } else {
    30L
  }

  n_scenarios <- length(saved_scenarios %||% list())

  # Total simulated model-years across all scenarios, models, and weather years
  total_runs <- if (!is.null(timeseries_curves) && nrow(timeseries_curves)) {
    nrow(timeseries_curves)
  } else if (is.finite(run_info$total_runs %||% NA_integer_) && run_info$total_runs > 0L && n_mods <= 1L) {
    run_info$total_runs
  } else {
    (max(n_scenarios, 1L) * n_mods + (if (has_future) 1L else 0L)) * n_years
  }

  val_5 <- format(total_runs, big.mark = ",")

  line1_5 <- if (n_scenarios > 0L) {
    paste0(
      "(", n_scenarios, if (n_scenarios == 1L) " SSP \u00d7 " else " SSPs \u00d7 ",
      n_mods, " models + 1 historical) \u00d7 ", n_years, " yrs"
    )
  } else {
    paste0("1 historical \u00d7 ", n_years, " yrs")
  }

  card5 <- list(
    label = "Simulation years",
    value = val_5,
    note = line1_5,
    note_html = shiny::tagList(
      shiny::tags$div(line1_5)
    ),
    class = "neutral",
    info = paste(
      "Total number of simulated population aggregates across all configured",
      "climate scenarios, ensemble climate models, and annual weather draws",
      "(scenarios \u00d7 models \u00d7 weather years)."
    )
  )
  card5$value_native <- as.numeric(total_runs)

  cards <- list(card1, card2, card3, card4, card5)
  for (i in seq_along(cards)) {
    cards[[i]]$metadata <- metadata
    cards[[i]]$deviation <- deviation
    cards[[i]]$summary_method <- if (i %in% c(1L, 3L, 4L)) {
      "equal_model_mean"
    } else if (i == 2L) {
      "median_across_climate_models"
    } else {
      "simulation_count"
    }
  }
  cards
}

#' Convert Step 2 Headline Cards to a Tidy Data Frame
#'
#' @param cards List returned by \code{step2_headline_cards()}.
#' @param metadata Optional metric metadata from \code{metric_metadata()}.
#' @param summary Optional summary context to append to every row.
#' @return A tidy data frame with display values, native numeric values, and context.
#' @noRd
step2_headline_df <- function(cards, metadata = NULL, summary = NULL) {
  if (is.null(cards) || !length(cards)) {
    return(data.frame(
      Metric = character(0), Value = character(0), Note = character(0),
      Historical_native = numeric(0), Focus_native = numeric(0),
      Difference_native = numeric(0), Difference_display = character(0),
      Range_lower_native = numeric(0), Range_upper_native = numeric(0),
      Native_unit = character(0), Display_unit = character(0),
      Threshold_kind = character(0), Threshold_value = numeric(0),
      Threshold_unit = character(0), Analysis_unit = character(0),
      Weight_interpretation = character(0), Deviation = character(0),
      Summary_method = character(0), Summary_scope = character(0), Context = character(0),
      stringsAsFactors = FALSE
    ))
  }
  metadata <- metadata %||% cards[[1L]]$metadata %||% metric_metadata()
  number_or_na <- function(x, i = NULL) {
    if (is.null(x)) return(NA_real_)
    if (!is.null(i)) {
      if (length(x) < i) return(NA_real_)
      x <- x[[i]]
    }
    x <- suppressWarnings(as.numeric(x))[1L]
    if (length(x) && is.finite(x)) x else NA_real_
  }
  summary_method <- vapply(cards, function(card) {
    as.character(card$summary_method %||% "")
  }, character(1L))
  context <- summary %||% metric_context_note(metadata)
  data.frame(
    Metric = vapply(cards, function(c) as.character(c$label %||% ""), character(1L)),
    Value = vapply(cards, function(c) as.character(c$value %||% ""), character(1L)),
    Note = vapply(cards, function(c) as.character(c$note %||% ""), character(1L)),
    Historical_native = vapply(cards, function(c) number_or_na(c$value_native, 1L), numeric(1L)),
    Focus_native = vapply(cards, function(c) number_or_na(c$value_native, 2L), numeric(1L)),
    Difference_native = vapply(cards, function(c) number_or_na(c$change_native), numeric(1L)),
    Difference_display = vapply(cards, function(c) as.character(c$change_display %||% ""), character(1L)),
    Range_lower_native = vapply(cards, function(c) number_or_na(c$value_range_native, 1L), numeric(1L)),
    Range_upper_native = vapply(cards, function(c) number_or_na(c$value_range_native, 2L), numeric(1L)),
    Native_unit = rep(metadata$native_unit %||% "outcome units", length(cards)),
    Display_unit = vapply(cards, function(card) {
      if (!identical(card$deviation %||% "none", "none")) {
        metadata$change_unit %||% "outcome units"
      } else metadata$level_unit %||% "outcome units"
    }, character(1L)),
    Threshold_kind = rep(metadata$threshold_kind %||% "none", length(cards)),
    Threshold_value = rep(number_or_na(metadata$threshold_value), length(cards)),
    Threshold_unit = rep(metadata$threshold_unit %||% "", length(cards)),
    Analysis_unit = rep(metadata$analysis_unit %||% "", length(cards)),
    Weight_interpretation = rep(metadata$weight_interpretation %||% "", length(cards)),
    Deviation = vapply(cards, function(c) as.character(c$deviation %||% "none"), character(1L)),
    Summary_method = summary_method,
    Summary_scope = rep(paste(
      if (identical(metadata$weighted, TRUE)) "survey-weighted annual aggregate" else if (identical(metadata$weighted, FALSE)) "unweighted annual aggregate" else "annual aggregate; weight status unknown",
      "for fixed survey population; focus climate scenario"
    ), length(cards)),
    Context = rep(as.character(context)[1L], length(cards)),
    stringsAsFactors = FALSE
  )
}

# Threshold table data frame ----

#' Build Return-Period Threshold Table Data Frame
#'
#' Pure function that converts a named list of aggregated series (output of
#' \code{aggregate_sim_preds()}) into a sorted data frame suitable for
#' \code{DT::datatable()}. Replaces the inline wrangling that previously
#' lived inside \code{renderDT} in mod_2_06_sim_compare_server().
#'
#' @param threshold_tbl Data frame. Band-assembled threshold frame with
#'   columns `scenario`, `source`, `rp_label`, `Estimate`, and `value`.
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"}.
#' @param show_coef Logical. When `FALSE`, coefficient-band rows (labels
#'   starting with `"Coef "`) are dropped.
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
                                     show_coef = TRUE,
                                     adverse_only = FALSE,
                                     method = "mean",
                                     so = NULL,
                                     n_hist_years = NULL) {
  if (is.null(threshold_tbl) || nrow(threshold_tbl) == 0L) {
    return(NULL)
  }
  df <- threshold_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    # Historical row from the Policy source is identical to Baseline by
    # construction (policy doesn't apply to historical) - drop it.
    df <- df[!(grepl("^Historical", df$scenario) & df$source == "Policy"), ,
      drop = FALSE
    ]
  }

  # Optionally hide the coefficient-band rows (any "Coef Pxx" label).
  if (!isTRUE(show_coef)) {
    df <- df[!grepl("^Coef ", df$Estimate), , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    return(NULL)
  }

  if (isTRUE(adverse_only)) {
    rp_map <- metric_decision_return_periods(method, so)
    max_yrs <- if (!is.null(n_hist_years) && is.finite(n_hist_years)) {
      as.integer(n_hist_years)
    } else {
      max(df$n_obs, na.rm = TRUE)
    }
    if (max_yrs < 50L) {
      rp_map <- rp_map[!names(rp_map) %in% c("Adverse 1-in-50", "Adverse 1 in 50")]
    }
    df <- df[df$rp_name %in% unname(rp_map), , drop = FALSE]
    if (nrow(df) == 0L) {
      return(NULL)
    }

    label_lookup <- names(rp_map)
    names(label_lookup) <- unname(rp_map)
    df$rp_label <- label_lookup[df$rp_name]
    df$rp_label <- gsub("-", " ", df$rp_label)
  }

  # Pivot: one column per RP threshold, value rounded.
  rp_levels <- unique(df$rp_label)
  df$value_round <- round(df$value, 2)

  pivot_cols <- if (has_source) {
    c("scenario", "source", "Estimate", "rp_label", "value_round")
  } else {
    c("scenario", "Estimate", "rp_label", "value_round")
  }
  if (!isTRUE(adverse_only)) pivot_cols <- c(pivot_cols, "n_obs")

  wide <- tidyr::pivot_wider(
    df[, pivot_cols],
    names_from  = "rp_label",
    values_from = "value_round",
    values_fn   = function(x) mean(x, na.rm = TRUE)
  )
  wide <- as.data.frame(wide)

  if (isTRUE(adverse_only)) {
    wide <- dplyr::rename(wide, `Scenario / Period` = scenario)
    if (has_source) wide <- dplyr::rename(wide, Source = source)
    rp_canonical <- c("Expected", "Adverse 1 in 5", "Adverse 1 in 10", "Adverse 1 in 20", "Adverse 1 in 50")
    rp_present <- intersect(rp_canonical, names(wide))
    lead_cols <- if (has_source) {
      c("Scenario / Period", "Source", "Estimate")
    } else {
      c("Scenario / Period", "Estimate")
    }
    wide <- wide[, c(lead_cols, rp_present), drop = FALSE]
    scenario_col <- "Scenario / Period"
  } else {
    wide <- dplyr::rename(wide, Scenario = scenario, Obs = n_obs)
    if (has_source) wide <- dplyr::rename(wide, Source = source)
    canonical <- c(names(RP_LOW), "1:1", names(RP_HIGH))
    rp_present <- intersect(canonical, names(wide))
    lead_cols <- if (has_source) {
      c("Scenario", "Source", "Estimate", "Obs")
    } else {
      c("Scenario", "Estimate", "Obs")
    }
    wide <- wide[, c(lead_cols, rp_present), drop = FALSE]
    scenario_col <- "Scenario"
  }

  # Sort rows: Historical first, then SSPs. Within each scenario the
  # Estimate rows are arranged concentrically around the Central P50:
  #   Pooled low -> Coef low -> Ensemble low -> Central -> Ensemble high
  #   -> Coef high -> Pooled high
  # Lower vs upper side is read from the percentile (< 50 or > 50, or the
  # special "min"/"max" tokens). Family distance from the central row is
  # Pooled = 3 (outermost), Coef = 2, Ensemble = 1.
  .est_rank <- function(est) {
    fam_dist <- ifelse(grepl("^Pooled ", est), 3L,
      ifelse(grepl("^Coef ", est), 2L,
        ifelse(grepl("^Ensemble ", est), 1L, 0L)
      )
    )
    pct <- suppressWarnings(as.integer(sub(".*P", "", est)))
    pct <- ifelse(grepl(" min$", est), 0L,
      ifelse(grepl(" max$", est), 100L, pct)
    )
    pct[is.na(pct)] <- 50L
    side <- ifelse(pct < 50, -1L, ifelse(pct > 50, 1L, 0L))
    side * fam_dist
  }
  wide$.est_order <- .est_rank(wide$Estimate)

  hist_rows <- wide[wide[[scenario_col]] == "Historical", , drop = FALSE]
  ssp_rows <- wide[wide[[scenario_col]] != "Historical", , drop = FALSE]

  if (nrow(hist_rows) > 0) {
    hist_rows <- hist_rows[order(hist_rows$.est_order), , drop = FALSE]
  }

  if (nrow(ssp_rows) > 0) {
    ssp_rows$.ssp_sort <- sub(" /.*", "", ssp_rows[[scenario_col]])
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows[[scenario_col]])
    ssp_rows$.yr_sort <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows[[scenario_col]], yr_m),
      regmatches(ssp_rows[[scenario_col]], regexpr("[0-9]{4}", ssp_rows[[scenario_col]]))
    )
    src_sort <- if (has_source) {
      match(ssp_rows$Source, c("Baseline", "Policy"))
    } else {
      1L
    }
    ssp_rows$.src_sort <- src_sort
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario")) {
      ssp_rows[order(
        ssp_rows$.yr_sort, ssp_rows$.ssp_sort,
        ssp_rows$.src_sort, ssp_rows$.est_order
      ), , drop = FALSE]
    } else {
      ssp_rows[order(
        ssp_rows$.ssp_sort, ssp_rows$.yr_sort,
        ssp_rows$.src_sort, ssp_rows$.est_order
      ), , drop = FALSE]
    }
    ssp_rows$.ssp_sort <- NULL
    ssp_rows$.yr_sort <- NULL
    ssp_rows$.src_sort <- NULL
  }

  hist_rows$.est_order <- NULL
  ssp_rows$.est_order <- NULL
  rbind(hist_rows, ssp_rows)
}


# Time-series spaghetti and envelope plot ----

#' Per-Model Time-Series Spaghetti With Ensemble Envelope
#'
#' Draws one thin translucent line per (scenario, model) over sim_year, with
#' a bold across-model median curve and a translucent inter-model ribbon
#' on top. Historical scenarios are drawn as a single bold line (no ribbon
#' - only one "model"). The aim is to show model disagreement directly
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
                                      x_label = "",
                                      ensemble_band_q = c(lo = 0, hi = 1)) {
  if (is.null(ts_tbl) || nrow(ts_tbl) == 0L) {
    return(blank_plot("Run a simulation to see model trajectories."))
  }

  df <- ts_tbl
  has_source <- "source" %in% names(df)
  if (has_source) {
    df <- df[!(df$is_historical & df$source == "Policy"), , drop = FALSE]
    df$source <- factor(df$source, levels = c("Baseline", "Policy"))
  }
  df$ssp_key <- ifelse(df$is_historical, "Historical",
    vapply(df$scenario, .normalise_ssp, character(1L))
  )
  df$yr_lbl <- ifelse(df$is_historical, "Historical",
    vapply(df$scenario, .parse_year, character(1L))
  )

  fut_yr_labels <- sort(unique(df$yr_lbl[df$yr_lbl != "Historical"]))
  yr_styles <- .resolve_year_styles(fut_yr_labels)
  present_ssps <- sort(unique(df$ssp_key[df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = .wise_support,
    .ssp_colours[intersect(
      names(.ssp_colours),
      present_ssps
    )]
  )
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  # Combined legend: per-scenario colour (SSP) and linetype (period).
  scen_levels <- c(
    "Historical",
    sort(unique(df$scenario[!df$is_historical]))
  )
  scen_colour_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") {
      return(unname(colour_map_ssp[["Historical"]]))
    }
    unname(colour_map_ssp[[.normalise_ssp(s)]] %||% "grey50")
  }, character(1L))
  scen_ltype_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") {
      return(unname(ltype_map_yr[["Historical"]]))
    }
    yr <- .parse_year(s)
    unname(ltype_map_yr[[yr]] %||% "solid")
  }, character(1L))
  df$scenario_f <- factor(df$scenario, levels = scen_levels)

  # Per-(scenario [, source], sim_year) envelope across models
  env_grp <- if (has_source) {
    c("scenario", "source", "sim_year")
  } else {
    c("scenario", "sim_year")
  }
  env_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(env_grp))) |>
    dplyr::summarise(
      central = stats::median(.data$value, na.rm = TRUE),
      lo = unname(stats::quantile(.data$value,
        ensemble_band_q[["lo"]],
        na.rm = TRUE
      )),
      hi = unname(stats::quantile(.data$value,
        ensemble_band_q[["hi"]],
        na.rm = TRUE
      )),
      n_models = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups = "drop"
    )
  env_df$scenario_f <- factor(env_df$scenario, levels = scen_levels)

  fut_env <- env_df[!env_df$is_historical & env_df$n_models > 1L, ,
    drop = FALSE
  ]

  p <- ggplot2::ggplot()

  # Inter-model ribbon (futures only, when >1 model)
  if (nrow(fut_env) > 0L) {
    ribbon_aes <- if (has_source) {
      ggplot2::aes(
        x = .data$sim_year, ymin = .data$lo, ymax = .data$hi,
        fill = .data$scenario_f,
        group = interaction(.data$scenario_f, .data$source)
      )
    } else {
      ggplot2::aes(
        x = .data$sim_year, ymin = .data$lo, ymax = .data$hi,
        fill = .data$scenario_f, group = .data$scenario_f
      )
    }
    p <- p + ggplot2::geom_ribbon(
      data        = fut_env,
      mapping     = ribbon_aes,
      alpha       = if (has_source) 0.10 else 0.15,
      show.legend = FALSE
    )
  }

  # Spaghetti: thin translucent line per (scenario [, source], model)
  spaghetti_aes <- if (has_source) {
    ggplot2::aes(
      x = .data$sim_year, y = .data$value,
      colour = .data$scenario_f,
      linetype = .data$scenario_f,
      alpha = .data$source,
      group = interaction(
        .data$scenario_f, .data$source,
        .data$model_id
      )
    )
  } else {
    ggplot2::aes(
      x = .data$sim_year, y = .data$value,
      colour = .data$scenario_f, linetype = .data$scenario_f,
      group = interaction(.data$scenario_f, .data$model_id)
    )
  }
  p <- p + if (has_source) {
    ggplot2::geom_line(
      data = df, mapping = spaghetti_aes,
      linewidth = 0.3, na.rm = TRUE, show.legend = FALSE
    )
  } else {
    ggplot2::geom_line(
      data = df, mapping = spaghetti_aes,
      linewidth = 0.3, alpha = 0.35, na.rm = TRUE, show.legend = FALSE
    )
  }

  # Bold median curve per (scenario [, source])
  median_aes <- if (has_source) {
    ggplot2::aes(
      x = .data$sim_year, y = .data$central,
      colour = .data$scenario_f, linetype = .data$scenario_f,
      alpha = .data$source,
      group = interaction(.data$scenario_f, .data$source)
    )
  } else {
    ggplot2::aes(
      x = .data$sim_year, y = .data$central,
      colour = .data$scenario_f, linetype = .data$scenario_f,
      group = .data$scenario_f
    )
  }
  p <- p + ggplot2::geom_line(
    data = env_df, mapping = median_aes,
    linewidth = 1.1, na.rm = TRUE
  )

  p <- p +
    ggplot2::scale_color_manual(
      values = scen_colour_map, breaks = scen_levels,
      name = "Scenario",
      guide = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_fill_manual(
      values = scen_colour_map, breaks = scen_levels, guide = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = scen_ltype_map, breaks = scen_levels,
      name = "Scenario"
    )
  if (has_source) {
    p <- p + ggplot2::scale_alpha_manual(
      values = c(Baseline = 0.35, Policy = 1.0),
      breaks = c("Baseline", "Policy"),
      name   = "Source",
      guide  = ggplot2::guide_legend(override.aes = list(linewidth = 0.9))
    )
  }
  p <- p +
    ggplot2::labs(
      x = "Historical weather-year draw (simulated)",
      y = x_label,
      subtitle = NULL
    ) +
    theme_wise() +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = NULL),
      linetype = ggplot2::guide_legend(title = NULL),
      alpha = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::theme(legend.position = "bottom")
  p
}

# Variance-contribution stacked bar ----

#' Aligned SD-Contribution Bars by Scenario
#'
#' For each scenario, plots a horizontal bar stacking each uncertainty
#' source's standard deviation contribution (sqrt of its variance):
#'   - Coefficient uncertainty (regression-fit per-outcome SE)
#'   - Inter-annual variability (within-model year-to-year)
#'   - Inter-model spread (across-model disagreement; future only)
#'
#' Each bar is one source's SD on the outcome scale. Bars are deliberately
#' aligned rather than stacked: SD components are not additive and covariance
#' assumptions must not be hidden in the visual encoding.
#'
#' @param var_tbl Tibble with columns: scenario, var_coef, var_within,
#'   var_across, is_historical.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual
#'   scale_y_continuous labs theme_minimal theme coord_flip
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_variance_contribution <- function(var_tbl) {
  if (is.null(var_tbl) || nrow(var_tbl) == 0L) {
    return(blank_plot("Run a simulation to see SD contributions."))
  }

  df <- var_tbl
  df$sd_coef <- sqrt(pmax(df$var_coef, 0))
  df$sd_within <- sqrt(pmax(df$var_within, 0))
  df$sd_across <- sqrt(pmax(df$var_across, 0))
  long <- tidyr::pivot_longer(
    df[, c("scenario", "sd_coef", "sd_within", "sd_across")],
    cols      = c("sd_coef", "sd_within", "sd_across"),
    names_to  = "source",
    values_to = "sd"
  )

  long$source <- factor(long$source,
    levels = c("sd_across", "sd_within", "sd_coef"),
    labels = c(
      "Inter-model spread",
      "Inter-annual variability",
      "Coefficient uncertainty"
    )
  )
  long$scenario <- factor(long$scenario, levels = rev(unique(df$scenario)))

  fill_map <- c(
    "Coefficient uncertainty"  = .wise_cat[[1]], # blue
    "Inter-annual variability" = .wise_cat[[2]], # vermillion
    "Inter-model spread"       = .wise_cat[[3]] # bluish green
  )

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$scenario, y = .data$sd, fill = .data$source)
  ) +
    ggplot2::geom_col(width = 0.7, position = "dodge") +
    ggplot2::scale_fill_manual(values = fill_map, name = NULL) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL,
      y = "Standard deviation (outcome units)",
      subtitle = NULL
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position    = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip()
}

variance_component_data <- function(var_tbl, include_shares = FALSE,
                                    share_tolerance = 1e-12) {
  if (is.null(var_tbl) || !nrow(var_tbl)) {
    return(tibble::tibble())
  }
  df <- var_tbl
  df$sd_coef <- sqrt(pmax(df$var_coef %||% 0, 0))
  df$sd_within <- sqrt(pmax(df$var_within %||% 0, 0))
  df$sd_across <- sqrt(pmax(df$var_across %||% 0, 0))
  out <- tidyr::pivot_longer(
    df[, intersect(c("scenario", "sd_coef", "sd_within", "sd_across"), names(df)),
      drop = FALSE
    ],
    cols = c("sd_coef", "sd_within", "sd_across"),
    names_to = "source", values_to = "sd"
  )
  out$variance <- out$sd^2
  if (isTRUE(include_shares)) {
    totals <- stats::setNames(
      tapply(out$variance, out$scenario, sum, na.rm = TRUE),
      unique(out$scenario)
    )
    out$share_approx <- out$variance / pmax(
      totals[as.character(out$scenario)],
      share_tolerance
    )
    out$share_warning <- "Approximate zero-covariance share; not a full variance decomposition."
  } else {
    out$share_approx <- NA_real_
    out$share_warning <- NA_character_
  }
  out
}

model_robustness_data <- function(ts_tbl, band_q = c(lo = 0.10, hi = 0.90)) {
  if (is.null(ts_tbl) || !nrow(ts_tbl) ||
    !all(c("scenario", "model_id", "sim_year", "value") %in% names(ts_tbl))) {
    return(tibble::tibble())
  }
  x <- dplyr::group_by(ts_tbl, .data$scenario, .data$model_id) |>
    dplyr::summarise(
      model_mean = mean(.data$value, na.rm = TRUE),
      n_weather_years = sum(is.finite(.data$value)), .groups = "drop"
    )
  centers <- dplyr::group_by(x, .data$scenario) |>
    dplyr::summarise(
      center = stats::median(.data$model_mean, na.rm = TRUE),
      ensemble_lo = stats::quantile(.data$model_mean, band_q[[1L]], na.rm = TRUE),
      ensemble_hi = stats::quantile(.data$model_mean, band_q[[2L]], na.rm = TRUE),
      n_models = dplyr::n_distinct(.data$model_id), .groups = "drop"
    )
  dplyr::left_join(x, centers, by = "scenario")
}

plot_model_robustness <- function(tbl, x_label = "Expected annual outcome") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Climate-model robustness is unavailable."))
  }
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$model_mean, y = .data$scenario)) +
    ggplot2::geom_point(size = 2, colour = .wise_support, alpha = 0.7) +
    ggplot2::geom_point(
      data = unique(tbl[c("scenario", "center")]),
      ggplot2::aes(x = .data$center, y = .data$scenario),
      shape = 21, fill = "#009E73", colour = .wise_support,
      size = 3
    ) +
    ggplot2::labs(x = x_label, y = NULL) +
    theme_wise()
}

# Enhanced exceedance curve ----

#' Enhanced Exceedance Probability Curve with Return Period Axis
#'
#' Colour identifies the SSP family and line type identifies the projection
#' period (for example, `SSP3-7.0 / 2025-2035`). Historical is navy and solid.
#' When baseline and policy series are present, endpoint markers distinguish
#' them; their lines retain the same scenario colour, period linetype, and
#' uniform width.
#' Optional logit probability axis.
#'
#' Each SSP x period combination has an inter-model ribbon and a central
#' across-model median curve. When source-tagged data are supplied (as in the
#' policy comparison), baseline and policy remain distinguishable even when
#' their ribbons overlap.
#'
#' @param curves_tbl    Data frame. Tidy exceedance curves with one row per
#'   (scenario, period, model, rank) welfare value.
#' @param x_label       Axis label for the welfare outcome.
#' @param return_period Logical. Show return period lines. Default TRUE.
#' @param n_sim_years   Integer. Triggers reliability annotation.
#' @param logit_x       Logical. Use logit scale on the probability axis to
#'   emphasise both tails symmetrically. Default FALSE.
#' @param band_q          Named numeric vector \code{c(lo =, hi =)} quantiles
#'   for the coefficient band. Default \code{c(0.10, 0.90)}.
#' @param ensemble_band_q Named numeric vector quantiles for the inter-model
#'   ensemble band. Default \code{c(0, 1)} (full range).
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
                               n_sim_years = NULL,
                               logit_x = FALSE,
                               band_q = c(lo = 0.10, hi = 0.90),
                               ensemble_band_q = c(lo = 0, hi = 1)) {
  if (is.null(curves_tbl) || nrow(curves_tbl) == 0L) {
    return(blank_plot("Run a simulation to see exceedance probabilities."))
  }

  # Per-scenario summary at each rank ----
  # For each (scenario, rank) collapse across models:
  #   central_at_rank    = median of welfare_val across models
  #   intermod_lo/hi     = quantile across models at ensemble_band_q
  #   coef_lo/hi_at_rank = median(welfare_val +/- z * coef_sd) across models
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
      exceed_prob = dplyr::first(.data$exceed_prob),
      central = stats::median(.data$welfare_val, na.rm = TRUE),
      intermod_lo = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["lo"]],
        na.rm = TRUE
      )),
      intermod_hi = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["hi"]],
        na.rm = TRUE
      )),
      n_models = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups = "drop"
    )

  # Coefficient band: apply a scenario-level typical SE as a constant offset
  # from the central curve. Using the per-rank coef_sd directly produces noisy,
  # self-crossing dashed lines because the per-rank SD wiggles. A single
  # typical SE per scenario gives clean parallels.
  if (!is.null(band_q)) {
    sd_grp <- if (has_source) c("scenario", "source") else "scenario"
    coef_sd_scn <- curves_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sd_grp))) |>
      dplyr::summarise(
        coef_sd_typ = stats::median(.data$coef_sd, na.rm = TRUE),
        .groups = "drop"
      )
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
      drop = FALSE
    ]
    agg_df$line_id <- paste(agg_df$scenario, agg_df$source, sep = " | ")
  } else {
    agg_df <- agg_df[order(agg_df$scenario, agg_df$rank), , drop = FALSE]
    agg_df$line_id <- as.character(agg_df$scenario)
  }

  # Aesthetic mappings. Keep each legend tied to one meaningful dimension:
  # SSP family (colour), period (linetype), and, for policy comparisons, source
  # (linewidth). Mapping `scenario` to both colour and linetype created a
  # duplicated scenario legend and hid the source distinction in overlapping
  # ribbons.
  agg_df$ssp_key <- ifelse(agg_df$is_historical, "Historical",
    vapply(agg_df$scenario, .normalise_ssp, character(1L))
  )
  agg_df$yr_lbl <- ifelse(agg_df$is_historical, "Historical",
    vapply(agg_df$scenario, .parse_year, character(1L))
  )

  fut_yr_labels <- sort(unique(agg_df$yr_lbl[agg_df$yr_lbl != "Historical"]))
  yr_styles <- .resolve_year_styles(fut_yr_labels)
  present_ssps <- sort(unique(agg_df$ssp_key[agg_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = .wise_support,
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )
  ltype_map_yr <- c("Historical" = "solid", yr_styles$linetype_map)

  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(agg_df$scenario[!agg_df$is_historical])))
  )
  scenario_colour_map <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(colour_map_ssp)) unname(colour_map_ssp[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  scenario_linetype_map <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return("solid")
    }
    unname(ltype_map_yr[[.parse_year(s)]] %||% "solid")
  }, character(1L)), scenario_levels)
  agg_df$scenario_key <- factor(
    ifelse(agg_df$is_historical, "Historical", as.character(agg_df$scenario)),
    levels = scenario_levels
  )
  agg_df$ssp_key <- factor(agg_df$ssp_key, levels = c("Historical", present_ssps))
  agg_df$yr_lbl <- factor(agg_df$yr_lbl, levels = c("Historical", fut_yr_labels))
  if (has_source) {
    agg_df$source <- factor(agg_df$source, levels = c("Baseline", "Policy"))
    # All ribbons take the scenario colour; the series distinction lives in
    # the endpoint markers and one-time captions only.
    ribbon_palette <- scenario_colour_map
    agg_df$ribbon_key <- as.character(agg_df$scenario_key)
    agg_df$line_key <- as.character(agg_df$scenario_key)
  } else {
    ribbon_palette <- scenario_colour_map
    agg_df$ribbon_key <- as.character(agg_df$scenario_key)
    agg_df$line_key <- as.character(agg_df$scenario_key)
  }
  hist_df <- agg_df[agg_df$is_historical, , drop = FALSE]
  fut_mod_df <- agg_df[!agg_df$is_historical, , drop = FALSE]
  fut_baseline_df <- if (has_source) {
    fut_mod_df[fut_mod_df$source == "Baseline", , drop = FALSE]
  } else {
    fut_mod_df
  }
  fut_policy_df <- if (has_source) {
    fut_mod_df[fut_mod_df$source == "Policy", , drop = FALSE]
  } else {
    fut_mod_df[0, , drop = FALSE]
  }

  # Plot ----
  # Layer order (back to front): inter-model ribbon (future) -> coefficient
  # band (optional) -> ensemble median curves.
  p <- ggplot2::ggplot(
    agg_df,
    ggplot2::aes(
      x        = .data$central,
      y        = .data$exceed_prob,
      colour   = .data$line_key,
      linetype = .data$scenario_key,
      group    = .data$line_id
    )
  )

  # Inter-model ribbons for each future series. Baseline and policy are both
  # simulated across climate models, so each has its own spread. Every band
  # takes its scenario's colour, matching its median line; the series
  # (baseline vs policy) is distinguished only by the endpoint markers and
  # the one-time captions.
  show_ens_ribbon <- !is.null(ensemble_band_q) &&
    (ensemble_band_q[["hi"]] > ensemble_band_q[["lo"]])
  if (nrow(fut_mod_df) > 0L && isTRUE(show_ens_ribbon)) {
    ribbon_aes <- ggplot2::aes(
      y = .data$exceed_prob, xmin = .data$intermod_lo,
      xmax = .data$intermod_hi, fill = .data$ribbon_key,
      group = .data$line_id
    )
    if (has_source) {
      p <- p +
        ggplot2::geom_ribbon(
          data = fut_baseline_df, mapping = ribbon_aes,
          alpha = 0.12, inherit.aes = FALSE
        ) +
        ggplot2::geom_ribbon(
          data = fut_policy_df, mapping = ribbon_aes,
          alpha = 0.12, inherit.aes = FALSE
        )
    } else {
      p <- p + ggplot2::geom_ribbon(
        data = fut_mod_df, mapping = ribbon_aes,
        alpha = 0.18, inherit.aes = FALSE
      )
    }
  }

  # Coefficient uncertainty band: drawn as a pair of dashed outline curves
  # (lo and hi) instead of a filled ribbon, so it remains visible regardless
  # of whether it falls inside or outside the inter-model ribbon.
  if (!is.null(band_q) && any(!is.na(agg_df$coef_lo))) {
    coef_df <- agg_df[!is.na(agg_df$coef_lo), , drop = FALSE]
    coef_aes_lo <- ggplot2::aes(
      x = .data$coef_lo, y = .data$exceed_prob,
      colour = .data$line_key,
      linetype = .data$scenario_key,
      group = .data$line_id
    )
    coef_aes_hi <- ggplot2::aes(
      x = .data$coef_hi, y = .data$exceed_prob,
      colour = .data$line_key,
      linetype = .data$scenario_key,
      group = .data$line_id
    )
    p <- p +
      ggplot2::geom_line(
        data = coef_df, mapping = coef_aes_lo,
        linetype = "dashed", linewidth = 0.5,
        inherit.aes = FALSE, show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = coef_df, mapping = coef_aes_hi,
        linetype = "dashed", linewidth = 0.5,
        inherit.aes = FALSE, show.legend = FALSE
      )
  }

  # Central median lines. Future lines show the across-model median at each
  # exceedance probability, making the centre of the ensemble explicit. All
  # lines share the theme's default width and the scenario colours; the
  # series (baseline vs policy) is distinguished by the endpoint markers.
  p <- p +
    ggplot2::geom_line(data = hist_df, linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(data = fut_mod_df, linewidth = 0.9, na.rm = TRUE)
  # No historical-mean reference line here: the exceedance curve aggregates
  # the adverse tail only, so its mean would not match the full-sample
  # historical mean shown on the other charts.
  p <- p +
    ggplot2::scale_color_manual(
      values = scenario_colour_map,
      breaks = scenario_levels,
      labels = scenario_levels,
      name   = NULL,
      guide  = "none"
    ) +
    ggplot2::scale_fill_manual(
      values   = ribbon_palette,
      na.value = "grey70",
      guide    = "none"
    ) +
    ggplot2::scale_linetype_manual(
      values = scenario_linetype_map,
      breaks = scenario_levels,
      name   = NULL,
      guide  = "none"
    )
  endpoint_rows <- dplyr::bind_rows(lapply(split(agg_df, agg_df$line_id), function(x) {
    x <- x[which.max(x$exceed_prob), , drop = FALSE]
    # Labels carry the scenario name only; the series (baseline vs policy) is
    # encoded by the endpoint marker and the one-time captions below.
    x$curve_label <- as.character(x$scenario_key)
    # Label colour must match the colour the line is actually drawn in, so
    # every label takes its scenario colour from the map.
    x$label_col <- unname(scenario_colour_map[[as.character(x$scenario_key)]]) %||%
      .wise_slate
    x
  }))
  max_prob <- max(agg_df$exceed_prob, na.rm = TRUE)
  label_prob <- max_prob
  # One text layer per series so each label can take its line's exact colour
  # as a constant without touching the plot-wide colour scale.
  label_layers <- lapply(seq_len(nrow(endpoint_rows)), function(i) {
    ggplot2::geom_text(
      data = endpoint_rows[i, , drop = FALSE],
      ggplot2::aes(x = .data$central, y = label_prob, label = .data$curve_label),
      colour = endpoint_rows$label_col[[i]],
      # Anchor each label just past its line's end marker and left-align it,
      # so it starts off the right-hand side of the curves and reads into the
      # axis expansion gutter, clear of every line regardless of curve
      # direction. Size matches the series labels on the other results plots.
      hjust = -0.12, size = 3.9, fontface = "bold", show.legend = FALSE,
      inherit.aes = FALSE
    )
  })
  p <- p + label_layers

  # Endpoint markers on each future series: an open circle (baseline) and a
  # filled vermillion circle (policy) at the right-hand end of the curve, so
  # the pair reads like the adverse plot's dumbbells even where a scenario's
  # SSP colour matches the policy accent (SSP5-8.5).
  if (has_source) {
    fut_end <- endpoint_rows[!endpoint_rows$is_historical, , drop = FALSE]
    base_end <- fut_end[fut_end$source == "Baseline", , drop = FALSE]
    pol_end <- fut_end[fut_end$source == "Policy", , drop = FALSE]
    if (nrow(base_end) > 0L) {
      p <- p + ggplot2::geom_point(
        data = base_end,
        ggplot2::aes(
          x = .data$central, y = .data$exceed_prob,
          colour = .data$scenario_key
        ),
        shape = 21, fill = "white", stroke = 1.2, size = 3.0,
        inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
      )
    }
    if (nrow(pol_end) > 0L) {
      p <- p + ggplot2::geom_point(
        data = pol_end,
        ggplot2::aes(x = .data$central, y = .data$exceed_prob),
        shape = 21, fill = .wise_policy, colour = .wise_policy_dark,
        stroke = 1.1, size = 3.4,
        inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
      )
    }
    # One-time Baseline/Policy captions on the topmost pair only, echoing the
    # adverse plot's caption convention: the caption sits just outside its
    # marker along the outcome axis.
    if (nrow(pol_end) > 0L && nrow(base_end) > 0L) {
      top_pol <- pol_end[which.max(pol_end$central), , drop = FALSE]
      top_base <- base_end[base_end$scenario_key == top_pol$scenario_key, ,
        drop = FALSE
      ]
      if (nrow(top_base) > 0L) {
        x_span <- diff(range(agg_df$central, na.rm = TRUE))
        # Both captions sit just ABOVE their marker, in the clear gap between
        # the pair's curves, so each caption visibly attaches to its own
        # endpoint instead of drifting toward the neighbouring series.
        cap_off <- if (is.finite(x_span) && x_span > 0) 0.04 * x_span else 0
        cap_df <- rbind(
          data.frame(
            x = top_pol$central, y = top_pol$exceed_prob,
            off = cap_off, label = "Policy", col = .wise_policy_dark
          ),
          data.frame(
            x = top_base$central, y = top_base$exceed_prob,
            off = cap_off, label = "Baseline", col = .wise_slate
          )
        )
        p <- p + ggplot2::geom_text(
          data = cap_df,
          ggplot2::aes(
            x = .data$x + .data$off, y = .data$y,
            label = .data$label
          ),
          colour = cap_df$col, hjust = 0.5, size = 3.9, fontface = "bold",
          inherit.aes = FALSE, show.legend = FALSE, na.rm = TRUE
        )
      }
    }
  }

  p <- p +
    ggplot2::labs(
      x = x_label,
      y = if (max(agg_df$exceed_prob, na.rm = TRUE) <= 0.55) "Annual adverse exceedance probability (AEP)" else "Annual exceedance probability"
    ) +
    theme_wise() +
    ggplot2::theme(
      legend.position = "none",
      # The probability ticks bunch where the log/logit scale compresses
      # (e.g. 1-in-5 vs 1-in-10); keep them a notch smaller than the theme
      # default so the two-line labels stay clear of each other. Under
      # coord_flip the bottom probability axis is themed by axis.text.x.
      axis.text.x = ggplot2::element_text(size = 11, colour = .wise_slate)
    ) +
    ggplot2::coord_flip()

  # Probability axis scaling ----
  # Return-period guides are carried by the axis ticks alone; the redundant
  # in-panel dashed guide lines and their floating labels were removed.
  is_adverse_tail <- max(agg_df$exceed_prob, na.rm = TRUE) <= 0.55
  support_years <- if (!is.null(n_sim_years) && is.finite(n_sim_years)) {
    max(2L, floor(n_sim_years))
  } else {
    NA_integer_
  }
  supported_rp <- function(x) {
    if (!is.finite(support_years)) {
      return(x)
    }
    denom <- suppressWarnings(as.numeric(sub(".*:", "", names(x))))
    x[is.na(denom) | denom <= support_years]
  }
  rp_tick_label <- function(nm, prob) {
    paste0(
      sub(":", " in ", nm), "\n(",
      scales::percent(prob, accuracy = 1), ")"
    )
  }
  if (is_adverse_tail) {
    # Adverse tail: log scale covering only periods supported by the
    # available simulated years. Do not imply a 1-in-50 estimate from 30 years.
    log_rp <- c("1:2" = 0.50, RP_LOW)
    log_rp <- supported_rp(log_rp)
    log_rp <- log_rp[order(log_rp, decreasing = TRUE)]
    log_breaks <- unname(log_rp)
    log_labels <- mapply(rp_tick_label, names(log_rp), unname(log_rp),
      USE.NAMES = FALSE
    )
    min_prob <- max(min(agg_df$exceed_prob, na.rm = TRUE), 0.005)
    keep_b <- log_breaks >= min_prob * 0.9
    low_lim <- min(min_prob * 0.9, min(log_breaks[keep_b]) * 0.9)
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::log10_trans(),
      breaks = log_breaks[keep_b],
      labels = log_labels[keep_b],
      limits = c(low_lim, 0.55),
      # Right-hand gutter so the endpoint series labels sit clear of the
      # curves while remaining inside the plot window.
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  } else if (isTRUE(logit_x)) {
    rp_low <- supported_rp(RP_LOW)
    rp_high <- supported_rp(RP_HIGH)
    # The complementary periods (1 - RP_HIGH) land on the same exceedance
    # probabilities as RP_LOW; dedupe so each tick renders once.
    logit_breaks <- sort(unique(c(
      unname(rp_low), 0.50,
      1 - unname(rp_high)
    )))
    # Label each tick from its own exceedance probability ("1 in 10 (10%)"),
    # so complementary-period breaks read in the same convention.
    logit_labels <- ifelse(
      logit_breaks == 0.5, "Median",
      paste0(
        "1 in ", format(round(1 / logit_breaks)), "\n(",
        scales::percent(logit_breaks, accuracy = 1), ")"
      )
    )
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::logit_trans(),
      breaks = logit_breaks,
      labels = logit_labels,
      limits = c(0.005, 0.995),
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    )
  }
  p
}

# ============================================================================
# Interactive (echarts4r) counterparts of the ggplot builders above.
# Guidelines §7: same statistics computed in R with the same parameters,
# echarts only draws precomputed values; theme tokens come from
# utils_plot_theme.R (wise_eaxis_label / wise_echart_theme, etc.).
# The ggplot builders above remain the static fallback and are still used by
# Step 3 policy comparisons - nothing here replaces them.
# ============================================================================

# Empty widget shell. echarts4r 0.5.x rejects single-column and <2-row frames
# in e_charts(); a two-column two-row dummy is never drawn on. Builders then
# inject their own axes/series into $x$opts (e_axis_*() re-enters e_charts()
# and would hit the same single-column defect).
.e_step2_base <- function(height) {
  e <- echarts4r::e_charts(data.frame(x = 0:1, y = 0:1), x, height = height)
  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL
  e$x$opts$series <- NULL
  e$x$opts$legend <- NULL
  e$x$opts$tooltip <- NULL
  e$x$opts$grid <- NULL
  e
}

# Floating band between lo and hi drawn as a stacked pair of bars: a fully
# transparent base bar up to `lo`, then the coloured (hi - lo) segment.
# `cats` are the category-axis labels; `horizontal = TRUE` places the
# category axis on y (ECharts data pairs are always [x, y]). Returns the two
# series; tooltip entries are suppressed because the pair encodes one visual
# band.
.e_band_pair <- function(cats, lo, hi, colour, stack, bar_width,
                         opacity = 1, z = 1, horizontal = FALSE) {
  n <- length(cats)
  pair_data <- function(get_val, style) {
    lapply(seq_len(n), function(i) {
      v <- get_val(i)
      list(
        value = if (horizontal) {
          list(v, cats[[i]])
        } else {
          list(cats[[i]], v)
        },
        itemStyle = style
      )
    })
  }
  transparent <- list(color = "rgba(0,0,0,0)")
  list(
    list(
      name = paste0(stack, "__base"), type = "bar", stack = stack,
      barWidth = bar_width, silent = TRUE, z = z,
      tooltip = list(show = FALSE),
      itemStyle = transparent,
      data = pair_data(
        function(i) if (is.finite(lo[i])) lo[i] else 0,
        transparent
      )
    ),
    list(
      name = paste0(stack, "__band"), type = "bar", stack = stack,
      barWidth = bar_width, silent = TRUE, z = z,
      tooltip = list(show = FALSE),
      data = pair_data(
        function(i) {
          if (is.finite(lo[i]) && is.finite(hi[i])) {
            max(hi[i] - lo[i], 0)
          } else {
            NA_real_
          }
        },
        list(color = colour, opacity = opacity)
      )
    )
  )
}

# Scatter marker series (type "scatter") with per-point value pairs.
# Returns a one-element list so callers can append with c().
.e_dot_series <- function(name, x, y, colour, fill = "white", size = 9,
                          border_width = 1.2, labels = NULL) {
  n <- length(x)
  data <- lapply(seq_len(n), function(i) {
    pt <- list(value = list(x[[i]], y[[i]]))
    if (!is.null(labels) && !is.na(labels[[i]]) && nzchar(labels[[i]])) {
      pt$label <- list(
        show = TRUE,
        position = "right",
        distance = 6,
        fontWeight = "bold",
        color = colour,
        fontSize = 13,
        formatter = labels[[i]]
      )
    }
    pt
  })
  st <- list(
    name = name,
    type = "scatter",
    symbolSize = size,
    z = 3,
    itemStyle = list(
      color = fill,
      borderColor = colour,
      borderWidth = border_width
    ),
    data = data
  )
  list(st)
}

# Line series over numeric (x, y) pairs with optional point labels on the
# last finite point (the endpoint scenario labels the ggplot builders drew
# with geom_text).
.e_line_series <- function(name, x, y, colour, width = 2, type = "solid",
                           opacity = 1, endpoint_label = FALSE,
                           label_x = NULL, area = NULL, stack = NULL,
                           z = 2) {
  n <- length(x)
  last <- which(is.finite(y))
  last <- if (length(last)) max(last) else 0L
  data <- lapply(seq_len(n), function(i) {
    pt <- list(value = list(x[[i]], y[[i]]))
    if (endpoint_label && i == last && is.finite(y[[i]])) {
      pt$label <- list(
        show = TRUE,
        position = "right",
        distance = 6,
        fontWeight = "bold",
        color = colour,
        fontSize = 13,
        formatter = name
      )
    }
    pt
  })
  st <- list(
    name = name,
    type = "line",
    lineStyle = list(color = colour, width = width, type = type,
                     opacity = opacity),
    itemStyle = list(color = colour),
    symbol = "none",
    z = z,
    data = data
  )
  if (!is.null(area)) st$areaStyle <- area
  if (!is.null(stack)) st$stack <- stack
  list(st)
}

# A filled uncertainty ribbon represented by one closed line polygon. ECharts'
# stacked lower/upper area series can render the upper segment from zero when
# the lower series has a transparent area, so an explicit polygon is safer for
# browser and export renderers.
.e_line_band <- function(name, x, lower, upper, colour, opacity = 0.2,
                         lower_type = "solid", z = 1) {
  ok <- is.finite(x) & is.finite(lower) & is.finite(upper)
  if (!any(ok)) return(list())
  x <- x[ok]
  lower <- lower[ok]
  upper <- pmax(upper[ok], lower)
  polygon <- rbind(
    cbind(x, lower),
    cbind(rev(x), rev(upper))
  )

  list(
    list(
      name = name,
      type = "line",
      symbol = "none",
      silent = TRUE,
      legendHoverLink = FALSE,
      tooltip = list(show = FALSE),
      lineStyle = list(
        color = "rgba(0,0,0,0)",
        width = 0,
        type = lower_type,
        opacity = 0
      ),
      areaStyle = list(color = colour, opacity = opacity),
      z = z,
      data = unname(polygon)
    )
  )
}

# Deterministic even-stride downsample for raw series above `cap` points
# (guidelines §7). Aggregated series are never downsampled.
.e_downsample_idx <- function(n, cap = 10000L) {
  if (n <= cap) {
    return(seq_len(n))
  }
  # Even stride keeps the first and last point and spreads the rest evenly,
  # so the shape of the series is preserved without randomisation.
  sort(unique(as.integer(floor(seq(1L, n, length.out = cap)))))
}

# ---- echart_pointrange_climate ----------------------------------------------

#' Interactive pointrange (Step 2 Results point-range chart)
#'
#' echarts counterpart of [plot_pointrange_climate()] for the Step 2 Results
#' surface. Reuses the exact same data preparation ([.pointrange_prep()]).
#' Nested bands are drawn as floating bar pairs (transparent base + coloured
#' segment) and the central estimate as an open scatter marker, matching the
#' ggplot linerange widths: ensemble 6pt -> barWidth 14, annual 3.5pt -> 8,
#' coefficient 1.2pt -> 3.
#'
#' Source-dodged policy comparisons (Baseline vs Policy) stay on the ggplot
#' builder; this builder draws the single-source Step 2 chart and simply
#' co-locates co-located categories when a `source` column is present.
#'
#' @return An `echarts4r` widget (never NULL; empty states return
#'   [echart_blank()] with the ggplot builder's message).
#' @noRd
echart_pointrange_climate <- function(bands_tbl,
                                      x_label = "",
                                      group_order = "scenario_x_year",
                                      show_coef = TRUE,
                                      show_annual = FALSE,
                                      height = "600px") {
  if (is.null(bands_tbl) || nrow(bands_tbl) == 0L) {
    return(echart_blank("Run a simulation to see results.", height = height))
  }
  prep <- .pointrange_prep(bands_tbl, group_order)
  df <- prep$df
  if (nrow(df) == 0L) {
    return(echart_blank(
      "Run a future simulation to see scenario comparisons.",
      height = height
    ))
  }
  palette <- prep$palette
  ordered_levels <- prep$ordered_levels
  x_label_map <- prep$x_label_map

  e <- .e_step2_base(height)
  series <- list()
  for (i in seq_len(nrow(df))) {
    r <- df[i, , drop = FALSE]
    col <- unname(palette[[r$colour_key]] %||% "#808080")
    # Unique stack per row: ECharts offsets different stacks within one
    # category, so co-located rows (source-dodged data) read side by side.
    st <- paste0("b", i)
    if (is.finite(r$intermod_lo) && is.finite(r$intermod_hi)) {
      series <- c(series, .e_band_pair(
        ordered_levels, r$intermod_lo, r$intermod_hi, col,
        stack = paste0("im_", st), bar_width = 14, opacity = 0.6, z = 1
      ))
    }
    if (isTRUE(show_annual) && is.finite(r$interann_lo) &&
      is.finite(r$interann_hi)) {
      series <- c(series, .e_band_pair(
        ordered_levels, r$interann_lo, r$interann_hi, col,
        stack = paste0("ia_", st), bar_width = 8, opacity = 1, z = 1
      ))
    }
    if (isTRUE(show_coef) && is.finite(r$coef_lo) && is.finite(r$coef_hi)) {
      series <- c(series, .e_band_pair(
        ordered_levels, r$coef_lo, r$coef_hi, .wise_support,
        stack = paste0("cf_", st), bar_width = 3, opacity = 1, z = 2
      ))
    }
    series <- c(series, .e_dot_series(
      as.character(r$colour_key),
      as.character(r$pt_key), r$value,
      colour = .wise_support, fill = "white", size = 11,
      border_width = 1.2
    ))
  }

  e$x$opts$xAxis <- list(
    type = "category",
    data = ordered_levels,
    axisLabel = wise_eaxis_label(interval = 0L, formatter = htmlwidgets::JS(
      "function(v){ return v === undefined ? '' : v; }"
    )),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(alignWithLabel = TRUE),
    splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$yAxis <- list(
    type = "value",
    name = x_label,
    nameLocation = "middle",
    nameGap = 48,
    nameRotate = 90,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(),
    splitLine = wise_esplit_line()
  )
  e$x$opts$series <- series
  e$x$opts$tooltip <- list(
    trigger = "axis",
    axisPointer = list(type = "shadow")
  )
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 14, top = 16, bottom = 8)
  wise_echart_theme(e)
}

# ---- echart_variance_contribution -------------------------------------------

#' Interactive uncertainty-source bars
#'
#' echarts counterpart of [plot_variance_contribution()]: one horizontal bar
#' per (scenario, source) on the same sqrt-of-variance statistics, Okabe-Ito
#' source colours, legend bottom (the ggplot showed one).
#' @noRd
echart_variance_contribution <- function(var_tbl, height = "300px", percent = FALSE) {
  if (is.null(var_tbl) || nrow(var_tbl) == 0L) {
    return(echart_blank("Run a simulation to see SD contributions.", height = height))
  }
  df <- var_tbl
  df$sd_coef <- sqrt(pmax(df$var_coef, 0))
  df$sd_within <- sqrt(pmax(df$var_within, 0))
  df$sd_across <- sqrt(pmax(df$var_across, 0))

  src_levels <- c(
    "Inter-model spread", "Inter-annual variability", "Coefficient uncertainty"
  )
  src_cols <- c(.wise_cat[[3]], .wise_cat[[2]], .wise_cat[[1]])
  # Horizontal rows: scenarios top-down in first-appearance order (ggplot
  # coord_flip with levels rev(unique(df$scenario)) puts Historical on top).
  cats <- rev(unique(as.character(df$scenario)))
  cat_idx <- setNames(seq_along(cats) - 1L, cats)

  e <- .e_step2_base(height)
  sd_cols <- c("sd_across", "sd_within", "sd_coef")
  series <- lapply(seq_along(src_levels), function(i) {
    vals <- vapply(cats, function(sc) {
      v <- df[[sd_cols[[i]]]][df$scenario == sc]
      if (length(v)) v[[1L]] else NA_real_
    }, numeric(1))
    list(
      name = src_levels[[i]],
      type = "bar",
      barGap = "15%",
      barMaxWidth = 22,
      data = lapply(seq_along(cats), function(j) {
        list(value = list(vals[[j]], cat_idx[[j]]))
      }),
      itemStyle = list(color = unname(src_cols[[i]]))
    )
  })

  e$x$opts$xAxis <- list(
    type = "value",
    name = if (percent) "Standard deviation (percentage points)" else "Standard deviation (outcome units)",
    nameLocation = "middle",
    nameGap = 30,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = .wise_result_axis_label(if (percent) "(percent)" else ""),
    splitLine = wise_esplit_line(),
    axisLine = list(lineStyle = list(color = .wise_grid))
  )
  e$x$opts$yAxis <- list(
    type = "category",
    data = cats,
    axisLabel = wise_eaxis_label(),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(show = FALSE),
    splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$series <- series
  e$x$opts$legend <- modifyList(
    list(bottom = 0, left = 0, orient = "horizontal"),
    wise_elegend_style()
  )
  e$x$opts$tooltip <- list(
    trigger = "axis", confine = TRUE, axisPointer = list(type = "shadow"),
    formatter = htmlwidgets::JS(sprintf("function(ps) {
      function esc(s) { return String(s).replace(/[&<>\"']/g, function(c) {
        return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]; }); }
      if (!ps.length) return '';
      var percent = %s;
      return esc(ps[0].axisValueLabel) + '<br>' + ps.map(function(p) {
        var v = Array.isArray(p.value) ? p.value[0] : p.value;
        return p.marker + esc(p.seriesName) + ': <b>' +
          (Number.isFinite(v) ? (percent ? v*100 : v).toLocaleString('en-US', {maximumFractionDigits: 3}) + (percent ? ' pp' : '') : 'Not available') + '</b>';
      }).join('<br>');
    }", tolower(as.character(percent))))
  )
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 24, top = 24, bottom = 88)
  e$x$opts$color <- unname(src_cols)
  wise_echart_theme(e)
}

# ---- echart_step2_adverse_dot -----------------------------------------------

#' Interactive adverse return-period dot plot
#'
#' echarts counterpart of [plot_step2_adverse_dot()]: per scenario, the
#' ensemble spread as a floating bar pair and the central estimate as an open
#' marker, scenario labels anchored right of the top-row (Expected) markers
#' exactly like the ggplot direct labels. No legend (the ggplot used direct
#' labels only).
#' @noRd
echart_step2_adverse_dot <- function(tbl, x_label = "Outcome level",
                                     height = "380px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("Return-period outcomes are unavailable.", height = height))
  }
  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(tbl$scenario[!tbl$is_historical])))
  )
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  tbl$scenario_key <- ifelse(tbl$is_historical, "Historical", as.character(tbl$scenario))
  tbl$rp_y <- as.integer(tbl$rp_label)
  # Vertical dodge as in the ggplot builder: scenarios sharing a return-period
  # row get distinct slots so their dumbbells stay readable.
  dodge_width <- 0.6
  tbl$dodge_offset <- stats::ave(
    seq_len(nrow(tbl)),
    tbl$rp_y,
    FUN = function(idx) {
      k <- length(idx)
      if (k <= 1L) {
        return(0)
      }
      seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k)[
        order(match(tbl$scenario_key[idx], scenario_levels))
      ] * (dodge_width / max(k - 1L, 1))
    }
  )

  y_breaks <- sort(unique(tbl$rp_y))
  y_labels <- levels(tbl$rp_label)[y_breaks]

  top_y <- max(tbl$rp_y)
  top_keys <- unique(tbl$scenario_key[tbl$rp_y == top_y])

  e <- .e_step2_base(height)
  series <- list()
  for (scn in scenario_levels) {
    rows <- tbl[tbl$scenario_key == scn, , drop = FALSE]
    if (!nrow(rows)) next
    col <- unname(scenario_colours[[scn]])
    y_pos <- rows$rp_y + rows$dodge_offset
    # Match each spread interval to its scenario's vertically-dodged marker.
    for (j in seq_len(nrow(rows))) {
      if (is.finite(rows$intermod_lo[[j]]) && is.finite(rows$intermod_hi[[j]])) {
        series <- c(series, list(list(
          name = paste0("rp_", scn, "_", j, "__spread"),
          type = "line",
          data = unname(rbind(
            c(rows$intermod_lo[[j]], y_pos[[j]]),
            c(rows$intermod_hi[[j]], y_pos[[j]])
          )),
          symbol = "none",
          silent = TRUE,
          tooltip = list(show = FALSE),
          lineStyle = list(color = col, width = 3, opacity = 0.4, cap = "round"),
          z = 1
        )))
      }
    }
    labs <- rep(NA_character_, nrow(rows))
    lab_rows <- rows$rp_y == top_y & rows$scenario_key %in% top_keys
    # One label per scenario, on its first Expected-row marker.
    if (any(lab_rows)) {
      labs[[which(lab_rows)[[1L]]]] <- scn
    }
    dots <- .e_dot_series(
      scn, rows$value, y_pos,
      colour = col, fill = col, size = 8, border_width = 1.1,
      labels = labs
    )[[1L]]
    dots$data <- lapply(seq_len(nrow(rows)), function(i) {
      modifyList(dots$data[[i]], list(
        scenario = scn, rp_label = as.character(rows$rp_label[[i]]),
        outcome = rows$value[[i]], lo = rows$intermod_lo[[i]], hi = rows$intermod_hi[[i]]
      ))
    })
    series <- c(series, list(dots))
  }

  x_vals <- c(tbl$value, tbl$intermod_lo, tbl$intermod_hi)
  x_vals <- x_vals[is.finite(x_vals)]
  if (!length(x_vals)) {
    return(echart_blank("No finite adverse-year outcomes available.", height = height))
  }
  x_span <- diff(range(x_vals))
  if (!is.finite(x_span) || x_span <= 0) x_span <- max(abs(x_vals), 1) * 0.1
  e$x$opts$xAxis <- list(
    type = "value",
    min = min(x_vals) - 0.02 * x_span,
    max = max(x_vals) + 0.12 * x_span,
    name = x_label,
    nameLocation = "middle",
    nameGap = 30,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = .wise_result_axis_label(x_label),
    splitLine = wise_esplit_line(),
    axisLine = list(lineStyle = list(color = .wise_grid))
  )
  y_label_map <- stats::setNames(as.list(y_labels), as.character(y_breaks))
  names(y_label_map) <- as.character(y_breaks)
  e$x$opts$yAxis <- list(
    type = "value",
    min = 0,
    max = max(y_breaks) + 1,
    interval = 1,
    axisLabel = wise_eaxis_label(
      showMinLabel = FALSE,
      showMaxLabel = FALSE,
      formatter = htmlwidgets::JS(sprintf(
        "function(v){ var m = %s; return m[String(Math.round(v))] || ''; }",
        jsonlite::toJSON(y_label_map, auto_unbox = TRUE)
      ))
    ),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(show = FALSE),
    splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$series <- series
  e$x$opts$tooltip <- list(trigger = "item", confine = TRUE, formatter = .wise_result_tooltip(x_label))
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 90, top = 12, bottom = 58)
  wise_echart_theme(e)
}

# ---- echart_annual_distribution ---------------------------------------------

#' Interactive annual outcome distribution
#'
#' Step 2 counterpart of [plot_annual_distribution()]. Delegates to the
#' shared Step 3 ECharts builder so both steps use the same annual distribution
#' geometry, labels, and interaction.
#' @noRd
echart_annual_distribution <- function(tbl, x_label = "Outcome (outcome units)",
                                       plot_type = "violin",
                                       height = "470px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("No annual simulation results available.", height = height))
  }
  echart_step3_annual_distribution(
    tbl, x_label = x_label, plot_type = plot_type, height = height
  )
}

# ---- echart_timeseries_spaghetti --------------------------------------------

#' Interactive per-model trajectories with ensemble envelope
#'
#' echarts counterpart of [plot_timeseries_spaghetti()]: same envelope
#' statistics (per (scenario, sim_year) median and quantiles at
#' `ensemble_band_q`), one thin translucent line per model, one bold median
#' line per scenario/source, and a custom polygon for the inter-model ribbon.
#' @noRd
echart_timeseries_spaghetti <- function(ts_tbl, x_label = "",
                                        ensemble_band_q = c(lo = 0, hi = 1),
                                        height = "380px") {
  if (is.null(ts_tbl) || nrow(ts_tbl) == 0L) {
    return(echart_blank("Run a simulation to see model trajectories.", height = height))
  }
  df <- ts_tbl
  has_source <- "source" %in% names(df)
  required <- c("scenario", "model_id", "sim_year", "value", "is_historical")
  if (!all(required %in% names(df))) {
    return(echart_blank("No model trajectories available.", height = height))
  }
  if (has_source) {
    df <- df[!(df$is_historical %in% TRUE & df$source == "Policy"), , drop = FALSE]
  }
  df$scenario <- as.character(df$scenario)
  df$model_id <- as.character(df$model_id)
  df$is_historical <- df$is_historical %in% TRUE
  df$sim_year <- suppressWarnings(as.numeric(as.character(df$sim_year)))
  df$value <- suppressWarnings(as.numeric(as.character(df$value)))
  keep <- !is.na(df$scenario) & nzchar(df$scenario) &
    !is.na(df$model_id) & nzchar(df$model_id) &
    is.finite(df$sim_year) & is.finite(df$value)
  if (has_source) {
    df$source <- as.character(df$source)
    keep <- keep & !is.na(df$source) & nzchar(df$source)
  }
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) {
    return(echart_blank("No finite model trajectories available.", height = height))
  }

  scen_levels <- c("Historical", sort(unique(df$scenario[!df$is_historical])))
  scen_levels <- scen_levels[scen_levels %in% df$scenario]
  scen_colour_map <- vapply(scen_levels, function(s) {
    if (s == "Historical") {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else "grey50"
  }, character(1L))

  env_grp <- c("scenario", if (has_source) "source", "sim_year")
  env_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(env_grp))) |>
    dplyr::summarise(
      central = stats::median(.data$value, na.rm = TRUE),
      lo = unname(stats::quantile(.data$value, ensemble_band_q[["lo"]], na.rm = TRUE)),
      hi = unname(stats::quantile(.data$value, ensemble_band_q[["hi"]], na.rm = TRUE)),
      n_models = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups = "drop"
    )
  fut_env <- env_df[!env_df$is_historical & env_df$n_models > 1L, , drop = FALSE]

  e <- .e_step2_base(height)
  series <- list()
  legend_names <- character(0)

  # Inter-model envelope (futures only, when >1 model): explicit data-space
  # polygon, so neither the lower edge nor the fill is anchored at zero.
  env_groups <- unique(fut_env[intersect(c("scenario", "source"), names(fut_env))])
  for (i in seq_len(nrow(env_groups))) {
    group <- env_groups[i, , drop = FALSE]
    scn <- as.character(group$scenario[[1L]])
    env <- fut_env[as.character(fut_env$scenario) == scn, , drop = FALSE]
    if (has_source) env <- env[as.character(env$source) == as.character(group$source[[1L]]), , drop = FALSE]
    env <- env[order(env$sim_year), , drop = FALSE]
    col <- unname(scen_colour_map[[scn]] %||% "grey50")
    band_name <- paste(c(scn, if (has_source) as.character(group$source[[1L]]), "envelope"), collapse = " / ")
    points <- unname(rbind(cbind(env$sim_year, env$lo),
      cbind(rev(env$sim_year), rev(env$hi))))
    series <- c(series, list(list(
      name = band_name,
      type = "custom",
      renderItem = htmlwidgets::JS(sprintf(
        "function(params, api) { var pts = %s; return {type: 'polygon', shape: {points: pts.map(function(p){return api.coord(p);})}, style: {fill: '%s', opacity: 0.16, stroke: 'none'}}; }",
        jsonlite::toJSON(points, digits = NA), col
      )),
      data = list(list(min(env$sim_year), min(env$lo), max(env$sim_year), max(env$hi))),
      encode = list(x = c(0, 2), y = c(1, 3)),
      itemStyle = list(color = col),
      silent = TRUE,
      tooltip = list(show = FALSE),
      legendHoverLink = FALSE,
      z = 1
    )))
  }

  # Spaghetti: one thin translucent line per (scenario, model).
  spaghetti_grp <- c("scenario", if (has_source) "source", "model_id")
  model_groups <- unique(df[spaghetti_grp])
  for (i in seq_len(nrow(model_groups))) {
    group <- model_groups[i, , drop = FALSE]
    scn <- as.character(group$scenario[[1L]])
    rows <- df[as.character(df$scenario) == scn & df$model_id == as.character(group$model_id[[1L]]), , drop = FALSE]
    if (has_source) rows <- rows[as.character(rows$source) == as.character(group$source[[1L]]), , drop = FALSE]
    rows <- rows[order(rows$sim_year), , drop = FALSE]
    # Guard: raw model lines above 10k points are deterministically
    # downsampled (even stride); aggregated envelopes stay exact.
    idx <- .e_downsample_idx(nrow(rows))
    xs <- as.numeric(rows$sim_year)[idx]
    ys <- as.numeric(rows$value)[idx]
    col <- unname(scen_colour_map[[scn]] %||% "grey50")
    member_col <- intersect(c("member_id", "member"), names(rows))
    model_name <- as.character(group$model_id[[1L]])
    source_name <- if (has_source) as.character(group$source[[1L]]) else NULL
    raw_name <- paste(c(scn, source_name, model_name), collapse = " / ")
    raw <- list(
      name = raw_name, type = "line", symbol = "none", showSymbol = FALSE, z = 2,
      lineStyle = list(color = col, width = 1, opacity = 0.35),
      itemStyle = list(color = col),
      data = lapply(seq_along(xs), function(j) {
        row_idx <- idx[[j]]
        point <- list(
          value = list(xs[[j]], ys[[j]]),
          scenario = scn,
          source = source_name,
          model = model_name,
          kind = "model"
        )
        if (length(member_col) && !is.na(rows[[member_col[[1L]]]][[row_idx]])) {
          point$member <- as.character(rows[[member_col[[1L]]]][[row_idx]])
        }
        point
      })
    )
    series <- c(series, list(raw))
  }

  # Bold across-model median per scenario/source (legend entry).
  median_groups <- unique(env_df[intersect(c("scenario", "source"), names(env_df))])
  for (i in seq_len(nrow(median_groups))) {
    group <- median_groups[i, , drop = FALSE]
    scn <- as.character(group$scenario[[1L]])
    env <- env_df[as.character(env_df$scenario) == scn, , drop = FALSE]
    if (has_source) env <- env[as.character(env$source) == as.character(group$source[[1L]]), , drop = FALSE]
    if (!nrow(env)) next
    env <- env[order(env$sim_year), , drop = FALSE]
    label <- paste(c(scn, if (has_source) as.character(group$source[[1L]])), collapse = " / ")
    line <- .e_line_series(
      label, as.numeric(env$sim_year), env$central,
      colour = unname(scen_colour_map[[scn]] %||% "grey50"),
      width = 3, opacity = 1
    )[[1L]]
    line$data <- lapply(seq_len(nrow(env)), function(j) list(
      value = list(as.numeric(env$sim_year[[j]]), as.numeric(env$central[[j]])),
      scenario = scn,
      source = if (has_source) as.character(group$source[[1L]]) else NULL,
      model = "Across-model median",
      kind = "median"
    ))
    line$z <- 4
    series <- c(series, list(line))
    legend_names <- c(legend_names, label)
  }

  y_vals <- c(df$value, env_df$lo, env_df$hi, env_df$central)
  y_vals <- y_vals[is.finite(y_vals)]
  if (!length(y_vals)) {
    return(echart_blank("No finite model trajectories available.", height = height))
  }
  x_vals <- df$sim_year[is.finite(df$sim_year)]
  x_span <- diff(range(x_vals))
  x_pad <- if (x_span > 0) x_span * 0.005 else max(abs(x_vals[[1L]]) * 0.005, 0.01)
  y_span <- diff(range(y_vals))
  y_pad <- if (y_span > 0) y_span * 0.04 else max(abs(y_vals[[1L]]) * 0.04, 0.01)
  percent <- nzchar(.wise_result_rate_unit(x_label))
  e$x$opts$xAxis <- list(
    type = "value",
    min = min(x_vals) - x_pad,
    max = max(x_vals) + x_pad,
    name = "Historical weather-year draw (simulated)",
    nameLocation = "middle",
    nameGap = 30,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(formatter = htmlwidgets::JS(
      "function(v){ return String(Math.round(v)); }"
    )),
    splitLine = wise_esplit_line(),
    axisLine = list(lineStyle = list(color = .wise_grid))
  )
  e$x$opts$yAxis <- list(
    type = "value",
    min = min(y_vals) - y_pad,
    max = max(y_vals) + y_pad,
    name = x_label,
    nameLocation = "end",
    nameGap = 20,
    nameRotate = 0,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(align = "left"),
    axisLabel = .wise_result_axis_label(x_label),
    splitLine = wise_esplit_line()
  )
  e$x$opts$series <- series
  e$x$opts$legend <- modifyList(
    list(top = 4, left = "center", orient = "horizontal", data = as.list(legend_names)),
    wise_elegend_style()
  )
  e$x$opts$tooltip <- list(
    trigger = "axis", confine = TRUE,
    axisPointer = list(type = "line", snap = TRUE),
    formatter = htmlwidgets::JS(sprintf("function(ps) {
      function esc(s) { return String(s == null ? '' : s).replace(/[&<>\"']/g, function(c) {
        return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]; }); }
      var percent = %s;
      function num(v) { return Number.isFinite(v) ? (percent ? v*100 : v).toLocaleString('en-US', {maximumFractionDigits: percent ? 1 : 3}) + %s : 'Not available'; }
      var groups = {}, order = [];
      ps.forEach(function(p) {
        var d = p.data || {}; if (d.kind !== 'model') return;
        var label = d.scenario + (d.source ? ' / ' + d.source : '');
        if (!groups[label]) { groups[label] = {values:[], marker:p.marker}; order.push(label); }
        var v = p.value[1]; if (Number.isFinite(v)) groups[label].values.push(v);
      });
      var out = order.map(function(label) {
        var g = groups[label], v = g.values.sort(function(a,b){return a-b;}), n = v.length;
        if (!n) return '';
        var median = n%%2 ? v[(n-1)/2] : (v[n/2-1]+v[n/2])/2;
        return g.marker + '<b>' + esc(label) + '</b>: ' + (n === 1 ? num(v[0]) :
          'Min ' + num(v[0]) + ' / Median ' + num(median) + ' / Max ' + num(v[n-1]));
      });
      if (!out.length) return '';
      return '<b>Weather year ' + String(Math.round(ps[0].value[0])) + '</b><br>' + out.join('<br>');
    }", tolower(as.character(percent)),
      jsonlite::toJSON(.wise_result_rate_unit(x_label), auto_unbox = TRUE))
  ))
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 20, top = 86, bottom = 54)
  wise_echart_theme(e)
}

# ---- echart_model_robustness -------------------------------------------------

#' Interactive climate-model robustness scatter
#'
#' echarts counterpart of [plot_model_robustness()]: one point per climate
#' model's mean across weather-year draws in its scenario colour, plus the
#' orange ensemble-median marker per scenario. Item tooltip.
#' @noRd
echart_model_robustness <- function(tbl, x_label = "Expected annual outcome",
                                    height = "420px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("Climate-model robustness is unavailable.", height = height))
  }
  cats <- unique(as.character(tbl$scenario))
  cat_idx <- setNames(as.character(seq_along(cats) - 1L), cats)
  colours <- stats::setNames(vapply(cats, function(s) {
    if (identical(s, "Historical")) return(.wise_history)
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1)), cats)

  e <- .e_step2_base(height)
  means <- list(
    name = "Model means",
    type = "scatter",
    symbolSize = 9,
    z = 2,
    itemStyle = list(color = .wise_support, opacity = 0.7),
    data = lapply(seq_len(nrow(tbl)), function(i) {
      list(
        value = list(tbl$model_mean[[i]], cat_idx[[as.character(tbl$scenario)[[i]]]]),
        scenario = as.character(tbl$scenario[[i]]),
        model = if ("model_id" %in% names(tbl)) as.character(tbl$model_id[[i]]) else NULL,
        n_years = if ("n_weather_years" %in% names(tbl)) tbl$n_weather_years[[i]] else NULL,
        itemStyle = list(color = unname(colours[[as.character(tbl$scenario[[i]])]]), opacity = .65)
      )
    })
  )
  centers <- unique(tbl[c("scenario", "center")])
  centers <- list(
    name = "Ensemble median",
    type = "scatter",
    symbolSize = 13,
    z = 3,
    itemStyle = list(color = .wise_policy, borderColor = .wise_policy_dark, borderWidth = 1.2),
    data = lapply(seq_len(nrow(centers)), function(i) {
      list(value = list(centers$center[[i]], cat_idx[[as.character(centers$scenario)[[i]]]]),
        scenario = as.character(centers$scenario[[i]]))
    })
  )
  e$x$opts$xAxis <- list(
    type = "value",
    scale = TRUE,
    name = x_label,
    nameLocation = "middle",
    nameGap = 30,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = .wise_result_axis_label(x_label),
    splitLine = wise_esplit_line(),
    axisLine = list(lineStyle = list(color = .wise_grid))
  )
  e$x$opts$yAxis <- list(
    type = "category",
    data = as.character(seq_along(cats) - 1L),
    axisLabel = wise_eaxis_label(
      interval = 0L,
      formatter = htmlwidgets::JS(sprintf(
        "function(v){ var m = %s; return m[v] === undefined ? '' : m[v]; }",
        jsonlite::toJSON(as.list(stats::setNames(
          as.list(sub(" / ", "\n", cats, fixed = TRUE)),
          as.character(seq_along(cats) - 1L)
        )), auto_unbox = TRUE)
      ))
    ),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(show = FALSE),
    splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$series <- list(means, centers)
  e$x$opts$tooltip <- list(trigger = "item", confine = TRUE,
    formatter = htmlwidgets::JS(sprintf("function(p) {
      var d = p.data || {};
      function esc(s) { return String(s).replace(/[&<>\"']/g, function(c) {
        return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]; }); }
      var percent = %s, v = p.value[0];
      var label = d.model ? 'Model mean' : 'Ensemble median';
      var text = '<b>' + esc(d.scenario) + '</b>';
      if (d.model) text += '<br>Model/member: ' + esc(d.model);
      text += '<br>' + label + ': <b>' + (Number.isFinite(v) ?
        (percent ? v*100 : v).toLocaleString('en-US', {maximumFractionDigits: percent ? 1 : 3}) + %s : 'Not available') + '</b>';
      if (Number.isFinite(d.n_years)) text += '<br>Weather years: ' + d.n_years;
      return text;
    }", tolower(as.character(nzchar(.wise_result_rate_unit(x_label)))),
      jsonlite::toJSON(.wise_result_rate_unit(x_label), auto_unbox = TRUE))))
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 24, top = 24, bottom = 62)
  wise_echart_theme(e)
}

# ---- echart_exceedance -------------------------------------------------------

#' Interactive exceedance probability curve
#'
#' echarts counterpart of [enhance_exceedance()]. Same per-(scenario, rank)
#' statistics: across-model median curve, inter-model quantile band and
#' coefficient band (both reduced to their two boundary curves - see the
#' judgment note below), endpoint scenario labels.
#'
#' Orientation: the ggplot builder drew outcome on x and probability on y
#' under coord_flip(); the echarts widget keeps that reading (outcome on the
#' value axis) with the probability axis horizontal.
#'
#' Judgment calls, documented for review:
#' - Uncertainty ribbons are explicit closed polygons rather than stacked-area
#'   bands. ECharts can anchor a transparent lower stack segment at zero, which
#'   produces an incorrect ribbon on the log probability axis.
#' - logit_x has no echarts counterpart: the probability axis uses log10
#'   with the same supported return-period tick labels (percent-formatted).
#'   Custom label and tick positions use ECharts >= 5.5.1.
#' @noRd
echart_exceedance <- function(curves_tbl,
                              x_label,
                              n_sim_years = NULL,
                              logit_x = FALSE,
                              band_q = c(lo = 0.10, hi = 0.90),
                              ensemble_band_q = c(lo = 0, hi = 1),
                              height = "400px") {
  if (is.null(curves_tbl) || nrow(curves_tbl) == 0L) {
    return(echart_blank("Run a simulation to see exceedance probabilities.", height = height))
  }

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
      exceed_prob = dplyr::first(.data$exceed_prob),
      central = stats::median(.data$welfare_val, na.rm = TRUE),
      intermod_lo = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["lo"]], na.rm = TRUE
      )),
      intermod_hi = unname(stats::quantile(.data$welfare_val,
        ensemble_band_q[["hi"]], na.rm = TRUE
      )),
      n_models = dplyr::n(),
      is_historical = any(.data$is_historical),
      .groups = "drop"
    )

  if (!is.null(band_q)) {
    sd_grp <- if (has_source) c("scenario", "source") else "scenario"
    coef_sd_scn <- curves_tbl |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sd_grp))) |>
      dplyr::summarise(
        coef_sd_typ = stats::median(.data$coef_sd, na.rm = TRUE),
        .groups = "drop"
      )
    agg_df <- dplyr::left_join(agg_df, coef_sd_scn, by = sd_grp)
    agg_df$coef_lo <- agg_df$central + stats::qnorm(band_q[["lo"]]) * agg_df$coef_sd_typ
    agg_df$coef_hi <- agg_df$central + stats::qnorm(band_q[["hi"]]) * agg_df$coef_sd_typ
  } else {
    agg_df$coef_lo <- NA_real_
    agg_df$coef_hi <- NA_real_
  }
  if (has_source) {
    agg_df$line_id <- paste(agg_df$scenario, agg_df$source, sep = " | ")
  } else {
    agg_df$line_id <- as.character(agg_df$scenario)
  }

  scenario_levels <- c(
    "Historical",
    sort(unique(as.character(agg_df$scenario[!agg_df$is_historical])))
  )
  scenario_colour_map <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_support)
    }
    ssp <- .normalise_ssp(s)
    if (ssp %in% names(.ssp_colours)) unname(.ssp_colours[[ssp]]) else .wise_slate
  }, character(1L)), scenario_levels)
  # Period linetypes from the ggplot builder's year-style resolution.
  agg_df$yr_lbl <- ifelse(agg_df$is_historical, "Historical",
    vapply(agg_df$scenario, .parse_year, character(1L))
  )
  fut_yr_labels <- sort(unique(agg_df$yr_lbl[agg_df$yr_lbl != "Historical"]))
  yr_styles <- .resolve_year_styles(fut_yr_labels)
  line_type_map <- c("Historical" = "solid", yr_styles$linetype_map)
  e_type_of <- function(scn) {
    yr <- .parse_year(scn)
    t <- if (!is.null(yr) && !is.na(yr) && yr %in% names(line_type_map)) {
      unname(line_type_map[[yr]])
    } else {
      "solid"
    }
    if (identical(t, "dotdash") || identical(t, "longdash")) "dashed" else
      if (identical(t, "twodash")) "dashed" else t
  }

  max_prob <- max(agg_df$exceed_prob, na.rm = TRUE)
  is_adverse_tail <- max_prob <= 0.55
  support_years <- if (!is.null(n_sim_years) && is.finite(n_sim_years)) {
    max(2L, floor(n_sim_years))
  } else {
    NA_integer_
  }
  supported_rp <- function(x) {
    if (!is.finite(support_years)) {
      return(x)
    }
    denom <- suppressWarnings(as.numeric(sub(".*:", "", names(x))))
    x[is.na(denom) | denom <= support_years]
  }
  rp_tick_label <- function(nm, prob) {
    paste0(
      sub(":", " in ", nm), "\n(",
      scales::percent(prob, accuracy = 1), ")"
    )
  }

  e <- .e_step2_base(height)
  series <- list()
  show_ens <- !is.null(ensemble_band_q) &&
    (ensemble_band_q[["hi"]] > ensemble_band_q[["lo"]])

  for (lid in unique(as.character(agg_df$line_id))) {
    rows <- agg_df[agg_df$line_id == lid, , drop = FALSE]
    rows <- rows[order(rows$exceed_prob), , drop = FALSE]
    scn <- as.character(rows$scenario[[1L]])
    col <- unname(scenario_colour_map[[scn]] %||% .wise_slate)
    lty <- e_type_of(scn)
    if (has_source && identical(as.character(rows$source[[1L]]), "Policy")) {
      col <- .wise_policy
      lty <- "dashed"
    }
    p <- rows$exceed_prob

    if (show_ens && !rows$is_historical[[1L]]) {
      series <- c(series, .e_line_band(
        paste0(lid, "__ensemble"), p, rows$intermod_lo, rows$intermod_hi,
        col, opacity = 0.2, z = 1
      ))
    }
    if (any(is.finite(rows$coef_lo))) {
      ok <- is.finite(rows$coef_lo) & is.finite(rows$coef_hi)
      series <- c(series, .e_line_band(
        paste0(lid, "__coefficient"), p[ok], rows$coef_lo[ok],
        rows$coef_hi[ok], col, opacity = 0.12,
        lower_type = "dashed", z = 1
      ))
    }
    line <- .e_line_series(lid, p, rows$central, col, width = 2, type = lty)[[1L]]
    line$endLabel <- list(
      show = TRUE, formatter = gsub(" / | \\| ", "\n", lid),
      color = col, fontSize = 12, fontWeight = "bold", distance = 8
    )
    line$labelLayout <- list(moveOverlap = "shiftY")
    line$emphasis <- list(focus = "series")
    series <- c(series, list(line))
  }

  # Probability axis: log10 with return-period ticks in the adverse tail
  # (same support rule as the ggplot builder); percent-formatted labels.
  if (is_adverse_tail) {
    log_rp <- c("1:2" = 0.50, RP_LOW)
    log_rp <- supported_rp(log_rp)
    log_rp <- log_rp[order(log_rp, decreasing = TRUE)]
    log_breaks <- unname(log_rp)
    log_labels <- mapply(rp_tick_label, names(log_rp), unname(log_rp),
      USE.NAMES = FALSE
    )
    min_prob <- max(min(agg_df$exceed_prob, na.rm = TRUE), 0.005)
    keep_b <- log_breaks >= min_prob * 0.9
    low_lim <- min(min_prob * 0.9, min(log_breaks[keep_b]) * 0.9)
    tick_vals <- log_breaks[keep_b]
    tick_labels <- log_labels[keep_b]
    x_min <- low_lim
    x_max <- 0.55
  } else {
    rp_low <- supported_rp(RP_LOW)
    rp_high <- supported_rp(RP_HIGH)
    tick_vals <- sort(unique(c(
      unname(rp_low), 0.50,
      1 - unname(rp_high)
    )))
    tick_labels <- ifelse(
      tick_vals == 0.5, "Median",
      paste0(
        "1 in ", format(round(1 / tick_vals)), "\n(",
        scales::percent(tick_vals, accuracy = 1), ")"
      )
    )
    x_min <- 0.005
    x_max <- 0.995
  }

  e$x$opts$xAxis <- list(
    type = "log",
    min = x_min,
    max = x_max,
    name = if (is_adverse_tail) {
      "Annual adverse exceedance probability (AEP)"
    } else {
      "Annual exceedance probability"
    },
    nameLocation = "middle",
    nameGap = 48,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(),
    axisTick = list(show = TRUE, customValues = as.list(tick_vals)),
    axisLabel = wise_eaxis_label(
      fontSize = 11,
      showMinLabel = TRUE,
      showMaxLabel = TRUE,
      hideOverlap = FALSE,
      customValues = as.list(tick_vals),
      formatter = htmlwidgets::JS(sprintf(
        "function(v){ var m = %s; return m[v] === undefined ? v : m[v]; }",
        jsonlite::toJSON(as.list(stats::setNames(
          as.list(tick_labels), as.list(tick_vals)
        )), auto_unbox = TRUE)
      ))
    ),
    splitLine = wise_esplit_line(),
    axisLine = list(lineStyle = list(color = .wise_grid))
  )
  e$x$opts$yAxis <- list(
    type = "value",
    scale = TRUE,
    name = x_label,
    nameLocation = "middle",
    nameGap = 48,
    nameRotate = 90,
    nameMoveOverlap = FALSE,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = .wise_result_axis_label(x_label),
    splitLine = wise_esplit_line()
  )
  e$x$opts$series <- series
  e$x$opts$tooltip <- list(
    trigger = "axis", confine = TRUE,
    axisPointer = list(type = "line", snap = TRUE),
    formatter = htmlwidgets::JS(sprintf("function(ps) {
      function esc(s) { return String(s).replace(/[&<>\"']/g, function(c) {
        return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c]; }); }
      ps = ps.filter(function(p) { return p.seriesName.indexOf('__') === -1; });
      if (!ps.length) return '';
      var prob = ps[0].value[0];
      var heading = (prob * 100).toLocaleString('en-US', {maximumFractionDigits: 1}) + '%% annual probability';
      if (prob > 0) heading += ' (1 in ' + (1 / prob).toLocaleString('en-US', {maximumFractionDigits: 1}) + ')';
      var percent = %s;
      return heading + '<br>' + ps.map(function(p) {
        var v = p.value[1];
        return p.marker + esc(p.seriesName) + ': <b>' +
          (Number.isFinite(v) ? (percent ? v*100 : v).toLocaleString('en-US', {maximumFractionDigits: percent ? 1 : 3}) + %s : 'Not available') + '</b>';
      }).join('<br>');
    }", tolower(as.character(nzchar(.wise_result_rate_unit(x_label)))),
      jsonlite::toJSON(.wise_result_rate_unit(x_label), auto_unbox = TRUE)))
  )
  # Right-hand gutter so the endpoint scenario labels stay clear of the
  # curves, mirroring the ggplot builder's 22% expansion.
  e$x$opts$grid <- list(containLabel = TRUE, left = 65, right = 145, top = 28, bottom = 82)
  wise_echart_theme(e)
}

# ============================================================================
# Shared reactable styling for the Step 2 tables (guidelines §6). Mirrors the
# mod_1_02 .stats_reactable() recipe: compact client-side table, soft-wrapped
# text cells, raw (unrounded) values in the data - display rounding only via
# colFormat where the old DT rounded.
# ============================================================================

.step2_reactable <- function(tab, col_defs = NULL, default_page_size = 10) {
  cols <- lapply(names(tab), function(nm) {
    if (!is.null(col_defs) && !is.null(col_defs[[nm]])) {
      return(col_defs[[nm]])
    }
    reactable::colDef(class = "wise-dt-wrap", minWidth = 70)
  })
  names(cols) <- names(tab)
  reactable::reactable(
    tab,
    columns = cols,
    compact = TRUE,
    searchable = FALSE,
    defaultPageSize = default_page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    highlight = TRUE
  )
}

.step2_reactable_note <- function(note) {
  .step2_reactable(data.frame(Note = note))
}
