# ============================================================================ #
# Step 3 decomposition summaries and headline reconciliation.                 #
# ============================================================================ #

.weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

.decomp_weights <- function(decomp_df) {
  w <- if ("weight" %in% names(decomp_df)) {
    suppressWarnings(as.numeric(decomp_df$weight))
  } else rep(1, nrow(decomp_df))
  w[!is.finite(w) | w < 0] <- NA_real_
  w
}

decomposition_summary_data <- function(decomp_df, is_rif = TRUE,
                                        tolerance = 1e-10) {
  if (is.null(decomp_df) || !is.data.frame(decomp_df) || !nrow(decomp_df)) {
    # Keep the empty result schema stable. Shiny renders the decomposition
    # outputs before the first simulation, so callers must be able to inspect
    # these columns even when there are no rows yet.
    return(tibble::tibble(
      channel_id = character(),
      channel = character(),
      parent = character(),
      model_value = numeric(),
      log_points = numeric(),
      percent = numeric(),
      share_of_total = numeric(),
      is_rif = logical()
    ))
  }
  w <- .decomp_weights(decomp_df)
  zero <- rep(0, nrow(decomp_df))
  main <- decomp_df$delta_main %||% zero
  res1 <- if (is_rif) decomp_df$delta_res1 %||% zero else zero
  res2 <- decomp_df$delta_res2 %||% zero
  direct_total <- decomp_df$delta_total %||% (main + res1 + res2)
  level <- main
  resilience <- res1 + res2
  rows <- tibble::tibble(
    channel_id = c("total", "level", "cash_transfer", "covariate_shift",
                   "resilience", "repositioning", "interaction"),
    channel = c("Total", "Main effect", "Cash transfer", "Covariate shift",
                "Resilience", "Repositioning", "Interaction"),
    parent = c(NA_character_, "total", "level", "level", "total",
               "resilience", "resilience"),
    model_value = c(
      .weighted_mean_safe(direct_total, w),
      .weighted_mean_safe(level, w),
      .weighted_mean_safe(decomp_df$delta_sp %||% zero, w),
      .weighted_mean_safe(decomp_df$delta_main_covar %||% (main - (decomp_df$delta_sp %||% zero)), w),
      .weighted_mean_safe(resilience, w),
      .weighted_mean_safe(res1, w),
      .weighted_mean_safe(res2, w)
    ),
    stringsAsFactors = FALSE
  )
  total <- rows$model_value[[1L]]
  level_value <- rows$model_value[[2L]]
  resilience_value <- rows$model_value[[5L]]
  reconciled_total <- level_value + resilience_value
  residual <- total - reconciled_total
  rows$log_points <- rows$model_value
  rows$percent <- log_effect_to_percent(rows$model_value)
  rows$share_of_total <- if (is.finite(total) && abs(total) > tolerance &&
                             abs(total) >= 0.01 * sum(abs(c(level_value, resilience_value)))) {
    rows$model_value / total
  } else rep(NA_real_, nrow(rows))
  rows$is_rif <- is_rif
  attr(rows, "reconciliation") <- list(
    total = total,
    level_plus_resilience = reconciled_total,
    residual = residual,
    tolerance = tolerance,
    status = if (is.finite(residual) && abs(residual) <= tolerance) "reconciled" else "not_reconciled"
  )
  rows
}

decomposition_reconciliation <- function(summary_df) {
  attr(summary_df, "reconciliation") %||% list(
    status = "unavailable", residual = NA_real_, tolerance = NA_real_
  )
}

plot_decomposition_headline <- function(summary_df,
                                        y_label = "Policy effect (percent change)") {
  if (is.null(summary_df) || !nrow(summary_df)) {
    return(blank_plot("Decomposition is unavailable."))
  }
  ids <- c("level", "resilience", "total")
  df <- summary_df[summary_df$channel_id %in% ids, , drop = FALSE]
  df$channel <- factor(df$channel, levels = c("Main effect", "Resilience", "Total"))
  if (!"scenario" %in% names(df)) df$scenario <- "Historical"
  scenario_levels <- unique(as.character(df$scenario))
  # Scenario colours follow the shared semantic mapping: grey for the
  # historical baseline, fixed SSP hues for climate scenarios, then the
  # categorical palette for anything else.
  scenario_colours <- stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) return(.wise_history)
    k <- .normalise_ssp(s)
    if (!is.na(k) && k %in% names(.ssp_colours)) return(unname(.ssp_colours[[k]]))
    idx <- match(s, scenario_levels)
    unname(.wise_cat[idx])
  }, character(1L)), scenario_levels)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$channel, y = .data$percent,
                                   fill = .data$scenario)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_col(width = 0.72, colour = .wise_support,
                      position = ggplot2::position_dodge(width = 0.78)) +
    ggplot2::scale_fill_manual(values = scenario_colours, name = NULL) +
    ggplot2::labs(x = NULL, y = y_label) +
    theme_wise(base_size = 13)
}

decomposition_explanation <- function(is_rif) {
  if (is_rif) {
    list(
      title = "RIF decomposition: main effect and resilience channels",
      text = paste(
        "Main effect includes the cash transfer and covariate shift.",
        "Resilience includes repositioning along the estimated welfare-quantile",
        "weather-sensitivity curve and the weather-policy interaction.",
        "RIF interpolation is limited to the estimated quantile grid."
      )
    )
  } else {
    list(
      title = "OLS decomposition: main effect and interaction channels",
      text = paste(
        "OLS has no repositioning channel because its weather coefficients are",
        "constant. Main effect includes the cash transfer and covariate shift; resilience",
        "is represented by the weather-policy interaction only."
      )
    )
  }
}

decomposition_channels_by_decile <- function(decomp_df, svy = NULL,
                                              outcome = "welfare",
                                              is_rif = NULL,
                                              baseline_deciles = NULL) {
  if (is.null(decomp_df) || !nrow(decomp_df)) return(tibble::tibble())
  is_rif <- if (is.null(is_rif)) {
    "delta_res1" %in% names(decomp_df) &&
      any(abs(decomp_df$delta_res1 %||% 0) > 1e-12, na.rm = TRUE)
  } else isTRUE(is_rif)
  if (!is.null(svy)) {
    dec <- baseline_deciles %||% weighted_baseline_deciles(
      svy, outcome, baseline_weight_column(svy)
    )
    ids <- suppressWarnings(as.integer(decomp_df$id))
    mapped <- is.finite(ids) & ids >= 1L & ids <= length(dec)
    stored_deciles <- if ("decile" %in% names(decomp_df))
      suppressWarnings(as.integer(decomp_df$decile)) else integer(0)
    decomp_df$decile <- NA_integer_
    decomp_df$decile[mapped] <- dec[ids[mapped]]
    if (!any(is.finite(decomp_df$decile)) && length(stored_deciles) == nrow(decomp_df)) {
      decomp_df$decile <- stored_deciles
    }
  }
  decomp_df <- decomp_df[is.finite(decomp_df$decile), , drop = FALSE]
  if (!nrow(decomp_df)) return(tibble::tibble())
  w <- .decomp_weights(decomp_df)
  main <- decomp_df$delta_main %||% rep(0, nrow(decomp_df))
  direct <- decomp_df$delta_sp %||% rep(0, nrow(decomp_df))
  covariate <- decomp_df$delta_main_covar %||% (main - direct)
  repositioning <- decomp_df$delta_res1 %||% rep(0, nrow(decomp_df))
  interaction <- decomp_df$delta_res2 %||% rep(0, nrow(decomp_df))
  res <- (decomp_df$delta_res1 %||% rep(0, nrow(decomp_df))) +
    (decomp_df$delta_res2 %||% rep(0, nrow(decomp_df)))
  total <- decomp_df$delta_total %||% (main + res)
  out <- dplyr::bind_rows(lapply(sort(unique(decomp_df$decile)), function(d) {
    ok <- decomp_df$decile == d
    tibble::tibble(
      decile = d,
      level_log = .weighted_mean_safe(main[ok], w[ok]),
      resilience_log = .weighted_mean_safe(res[ok], w[ok]),
      total_log = .weighted_mean_safe(total[ok], w[ok]),
      level_percent = log_effect_to_percent(level_log),
      resilience_percent = log_effect_to_percent(resilience_log),
      main_percent = log_effect_to_percent(.weighted_mean_safe(main[ok], w[ok])),
      cash_transfer_percent = log_effect_to_percent(
        .weighted_mean_safe(direct[ok], w[ok])
      ),
      covariate_shift_percent = log_effect_to_percent(
        .weighted_mean_safe(covariate[ok], w[ok])
      ),
      repositioning_percent = log_effect_to_percent(
        .weighted_mean_safe(repositioning[ok], w[ok])
      ),
      interaction_percent = log_effect_to_percent(
        .weighted_mean_safe(interaction[ok], w[ok])
      ),
      total_percent = log_effect_to_percent(total_log),
      n_households = sum(ok, na.rm = TRUE),
      weighted_population = sum(w[ok], na.rm = TRUE)
    )
  }))
  out
}

plot_decomposition_channels_by_decile <- function(tbl, is_rif = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Decile decomposition is unavailable for this run.", size = 4))
  }
  is_rif <- if (is.null(is_rif)) "repositioning_percent" %in% names(tbl) &&
    any(abs(tbl$repositioning_percent) > 1e-12, na.rm = TRUE) else isTRUE(is_rif)
  channel_cols <- c(
    "cash_transfer_percent", "covariate_shift_percent",
    if (is_rif) "repositioning_percent", "interaction_percent"
  )
  active <- vapply(channel_cols, function(col)
    any(abs(tbl[[col]]) > 1e-12, na.rm = TRUE), logical(1L))
  if (any(active)) channel_cols <- channel_cols[active]
  channel_labels <- c(
    cash_transfer_percent = "SP direct effect",
    covariate_shift_percent = "Main effect (covariate shift)",
    repositioning_percent = "Resilience - Repositioning effect",
    interaction_percent = "Resilience - Interaction effect"
  )
  long <- tidyr::pivot_longer(
    tbl[, c("decile", channel_cols)], cols = tidyselect::all_of(channel_cols),
    names_to = "channel", values_to = "effect"
  )
  long$channel <- factor(unname(channel_labels[long$channel]),
                         levels = unname(channel_labels[channel_cols]))
  # Colorblind-safe quartet from the shared categorical palette; the total
  # marker stays neutral dark so it cannot be confused with a channel.
  colours <- c(
    "SP direct effect" = .wise_cat[[1]],
    "Main effect (covariate shift)" = .wise_cat[[4]],
    "Resilience - Repositioning effect" = .wise_cat[[3]],
    "Resilience - Interaction effect" = .wise_cat[[6]]
  )
  ggplot2::ggplot(long, ggplot2::aes(x = factor(.data$decile), y = .data$effect,
                                     fill = .data$channel)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_col(position = "stack", width = 0.62) +
    ggplot2::geom_point(data = tbl,
                        ggplot2::aes(x = factor(.data$decile), y = .data$total_percent),
                        inherit.aes = FALSE, shape = 21, fill = "white",
                        colour = .wise_support, size = 2.8, stroke = 1.1) +
    ggplot2::scale_fill_manual(values = colours, drop = FALSE) +
    ggplot2::labs(x = "Fixed observed baseline welfare decile (1 = poorest)",
                  y = "Policy effect (percent change)", fill = "Channel",
                  subtitle = NULL) +
    theme_wise(base_size = 13) + ggplot2::theme(legend.position = "bottom")
}

decomposition_decile_export <- function(tbl, is_rif = FALSE) {
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  cols <- c(
    decile = "Baseline welfare decile",
    cash_transfer_percent = "Direct transfer effect (%)",
    covariate_shift_percent = "Covariate shift effect (%)",
    interaction_percent = "Weather-policy interaction (%)",
    total_percent = "Total policy effect (%)",
    n_households = "Sample units",
    weighted_population = "Population represented"
  )
  if (isTRUE(is_rif)) {
    cols <- append(cols, c(repositioning_percent = "Repositioning effect (%)"),
                   after = 3L)
  }
  cols <- cols[names(cols) %in% names(tbl)]
  out <- as.data.frame(tbl[, names(cols), drop = FALSE])
  names(out) <- unname(cols)
  out
}
