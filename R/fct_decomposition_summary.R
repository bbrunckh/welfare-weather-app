# Step 3 decomposition summaries ----

.weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[ok], w[ok])
}

.decomp_weights <- function(decomp_df) {
  w <- if ("weight" %in% names(decomp_df)) {
    suppressWarnings(as.numeric(decomp_df$weight))
  } else {
    rep(1, nrow(decomp_df))
  }
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
    channel_id = c(
      "total", "level", "cash_transfer", "covariate_shift",
      "resilience", "repositioning", "interaction"
    ),
    channel = c(
      "Total", "Main effect", "Cash transfer", "Covariate shift",
      "Resilience", "Repositioning", "Interaction"
    ),
    parent = c(
      NA_character_, "total", "level", "level", "total",
      "resilience", "resilience"
    ),
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
  } else {
    rep(NA_real_, nrow(rows))
  }
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

.decomposition_outcome_channels <- function(decomp_df, so, baseline_svy = NULL) {
  if (is.null(decomp_df) || !is.data.frame(decomp_df) || !nrow(decomp_df)) {
    return(data.frame())
  }
  main <- decomp_df$delta_main %||% rep(0, nrow(decomp_df))
  repositioning <- decomp_df$delta_res1 %||% rep(0, nrow(decomp_df))
  interaction <- decomp_df$delta_res2 %||% rep(0, nrow(decomp_df))
  main <- suppressWarnings(as.numeric(main))
  repositioning <- suppressWarnings(as.numeric(repositioning))
  interaction <- suppressWarnings(as.numeric(interaction))
  is_log <- identical(so$transform %||% "", "log")
  ids <- if ("id" %in% names(decomp_df)) suppressWarnings(as.integer(decomp_df$id)) else rep(NA_integer_, nrow(decomp_df))
  outcome <- so$name %||% ""
  if (is_log && (is.null(baseline_svy) || !outcome %in% names(baseline_svy) ||
      (any(is.finite(ids)) && (any(!is.finite(ids)) || any(ids < 1L | ids > nrow(baseline_svy)))))) {
    return(data.frame())
  }
  if (is_log) {
    if ("baseline_mean" %in% names(decomp_df)) {
      baseline <- suppressWarnings(as.numeric(decomp_df$baseline_mean))
    } else if (all(is.finite(ids))) {
      baseline <- suppressWarnings(as.numeric(baseline_svy[[outcome]][ids]))
    } else if ("baseline_annual" %in% names(decomp_df)) {
      field <- "baseline_annual"
      baseline <- suppressWarnings(as.numeric(decomp_df[[field]]))
    } else if ("baseline_annual" %in% names(decomp_df)) {
      baseline <- suppressWarnings(as.numeric(decomp_df$baseline_annual))
    } else if ("decile" %in% names(decomp_df)) {
      baseline_deciles <- weighted_baseline_deciles(baseline_svy, outcome,
        baseline_weight_column(baseline_svy))
      weights <- .decomp_weights(baseline_svy)
      baseline_by_decile <- vapply(sort(unique(decomp_df$decile)), function(d) {
        keep <- baseline_deciles == d
        .weighted_mean_safe(as.numeric(baseline_svy[[outcome]][keep]), weights[keep])
      }, numeric(1))
      baseline <- unname(baseline_by_decile[as.character(decomp_df$decile)])
    } else {
      baseline <- rep(.weighted_mean_safe(as.numeric(baseline_svy[[outcome]]),
        .decomp_weights(baseline_svy)), nrow(decomp_df))
    }
    out <- data.frame(
      main = (exp(main) - 1) * baseline,
      repositioning = exp(main) * (exp(repositioning) - 1) * baseline,
      interaction = exp(main + repositioning) * (exp(interaction) - 1) * baseline,
      total = (exp(main + repositioning + interaction) - 1) * baseline
    )
  } else {
    out <- data.frame(main = main, repositioning = repositioning,
      interaction = interaction,
      total = decomp_df$delta_total %||% (main + repositioning + interaction))
  }
  out$weight <- .decomp_weights(decomp_df)
  if (!is.null(baseline_svy) && all(is.finite(ids))) {
    deciles <- weighted_baseline_deciles(baseline_svy, so$name %||% "welfare",
      baseline_weight_column(baseline_svy))
    mapped <- is.finite(ids) & ids >= 1L & ids <= length(deciles)
    out$decile <- NA_integer_
    out$decile[mapped] <- deciles[ids[mapped]]
  } else if ("decile" %in% names(decomp_df)) out$decile <- decomp_df$decile
  out
}

.decomposition_outcome_summary <- function(decomp_df, so, baseline_svy = NULL) {
  channels <- .decomposition_outcome_channels(decomp_df, so, baseline_svy)
  if (!nrow(channels)) return(data.frame())
  data.frame(
    channel = c("Main effect", "Repositioning", "Interaction", "Total"),
    value = vapply(c("main", "repositioning", "interaction", "total"), function(field) {
      .weighted_mean_safe(channels[[field]], channels$weight)
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

.decomposition_outcome_deciles <- function(decomp_df, so, baseline_svy = NULL) {
  channels <- .decomposition_outcome_channels(decomp_df, so, baseline_svy)
  if (!nrow(channels) || !"decile" %in% names(channels)) return(data.frame())
  dplyr::bind_rows(lapply(sort(unique(channels$decile[is.finite(channels$decile)])), function(d) {
    rows <- channels[channels$decile == d, , drop = FALSE]
    data.frame(decile = as.integer(d),
      main = .weighted_mean_safe(rows$main, rows$weight),
      repositioning = .weighted_mean_safe(rows$repositioning, rows$weight),
      interaction = .weighted_mean_safe(rows$interaction, rows$weight),
      total = .weighted_mean_safe(rows$total, rows$weight))
  }))
}

.decomposition_outcome_unit <- function(so) {
  metric_metadata("mean", so)$change_unit %||% "outcome units"
}

echart_outcome_decomposition_headline <- function(data, y_label = "Weighted average change (outcome units)", height = "360px") {
  if (is.null(data) || !nrow(data)) return(echart_blank("Decomposition is unavailable.", height))
  data$channel <- factor(data$channel,
    levels = c("Main effect", "Repositioning", "Interaction", "Total"))
  fmt <- htmlwidgets::JS("function(v){ if (v == null || isNaN(v)) return '-'; return Number(v).toLocaleString('en-US',{maximumFractionDigits:2}); }")
  e <- data |>
    echarts4r::e_charts(channel, height = height) |>
    echarts4r::e_bar(value, name = "Weighted average change") |>
    echarts4r::e_color(c(.wise_cat[[4]], .wise_cat[[3]], .wise_cat[[6]], .wise_support)) |>
    echarts4r::e_x_axis(axisLabel = wise_eaxis_label(fontSize = 13),
      axisTick = list(alignWithLabel = TRUE), axisLine = list(lineStyle = list(color = .wise_grid))) |>
    echarts4r::e_y_axis(name = y_label, nameLocation = "end", nameRotate = 0,
      nameGap = 20, nameTextStyle = wise_eaxis_name(align = "left"),
      axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 54, right = 14, top = 32, bottom = 38) |>
    wise_echart_theme()
  e$x$opts$xAxis[[1L]]$data <- as.character(data$channel)
  e$x$opts$legend <- list(show = FALSE)
  e$x$opts$series[[1L]]$data <- as.list(data$value)
  # One series, one colour per channel bar (matches the decile chart palette).
  e$x$opts$series[[1L]]$colorBy <- "data"
  e$x$opts$yAxis[[1L]]$name <- y_label
  e$x$opts$yAxis[[1L]]$nameLocation <- "end"
  e$x$opts$yAxis[[1L]]$nameRotate <- 0
  .wise_zero_markline(e)
}

echart_outcome_decomposition_deciles <- function(data, y_label = "Weighted average change (outcome units)", height = "450px") {
  if (is.null(data) || !nrow(data)) return(echart_blank("Decile decomposition is unavailable for this run.", height))
  channel_names <- c(main = "Main effect", repositioning = "Repositioning", interaction = "Interaction")
  active <- names(channel_names)[vapply(names(channel_names), function(n) {
    values <- suppressWarnings(as.numeric(data[[n]]))
    any(abs(values) > 1e-12, na.rm = TRUE)
  }, logical(1))]
  if (!length(active)) active <- "main"
  long <- do.call(rbind, lapply(active, function(n) data.frame(
    decile = as.character(data$decile), channel = unname(channel_names[[n]]),
    value = data[[n]], stringsAsFactors = FALSE)))
  levels_decile <- as.character(sort(unique(data$decile)))
  long$decile <- factor(long$decile, levels = levels_decile)
  long$channel <- factor(long$channel, levels = unname(channel_names[active]))
  colours <- c("Main effect" = .wise_cat[[4]], "Repositioning" = .wise_cat[[3]],
    "Interaction" = .wise_cat[[6]])
  fmt <- htmlwidgets::JS("function(v){ if (v == null || isNaN(v)) return '-'; return Number(v).toLocaleString('en-US',{maximumFractionDigits:2}); }")
  e <- long |>
    echarts4r::group_by(channel) |>
    echarts4r::e_charts(decile, height = height) |>
    echarts4r::e_bar(value, stack = "channels") |>
    echarts4r::e_color(unname(colours[unname(channel_names[active])])) |>
    echarts4r::e_legend(orient = "horizontal", left = "center", top = 4) |>
    echarts4r::e_x_axis(name = "Fixed observed baseline welfare decile (1 = poorest)",
      nameLocation = "middle", nameGap = 28, nameTextStyle = wise_eaxis_name(fontSize = 13),
      axisLabel = wise_eaxis_label(fontSize = 13), axisTick = list(alignWithLabel = TRUE),
      axisLine = list(lineStyle = list(color = .wise_grid))) |>
    echarts4r::e_y_axis(name = y_label, nameLocation = "end", nameRotate = 0,
      nameGap = 20, nameTextStyle = wise_eaxis_name(align = "left"),
      axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 86, bottom = 62) |>
    wise_echart_theme() |>
    .wise_zero_markline()
  e$x$opts$legend$data <- as.list(unname(channel_names[active]))
  pts <- lapply(seq_len(nrow(data)), function(i) list(
    value = c(match(as.character(data$decile[[i]]), levels_decile) - 1L, data$total[[i]])))
  e$x$opts$series <- append(e$x$opts$series, list(list(type = "scatter", name = "Total effect",
    data = pts, symbol = "circle", symbolSize = 8,
    itemStyle = list(color = "white", borderColor = .wise_support, borderWidth = 1.2),
    tooltip = list(valueFormatter = fmt), z = 6)))
  e
}

decomposition_reconciliation <- function(summary_df) {
  attr(summary_df, "reconciliation") %||% list(
    status = "unavailable", residual = NA_real_, tolerance = NA_real_
  )
}

.policy_metric_decile_summary <- function(decile_annual, scenario, basis = "mean",
                                           adverse_support = NULL) {
  fields <- .policy_metric_fields
  if (!is.data.frame(decile_annual) || !nrow(decile_annual)) return(data.frame())
  data <- decile_annual[decile_annual$scenario == scenario, , drop = FALSE]
  if (!nrow(data)) return(data.frame())
  if (!identical(basis, "mean")) {
    probability <- if (identical(basis, "adverse_10")) 0.10 else 0.05
    support <- if (is.data.frame(adverse_support) && nrow(adverse_support)) {
      adverse_support[adverse_support$scenario == scenario &
        adverse_support$probability == probability, , drop = FALSE]
    } else data.frame()
    if (!nrow(support) || any(support$status != "ok")) return(data.frame())
    deciles <- sort(unique(data$decile))
    models <- unique(as.character(support$model_id))
    values <- list()
    for (decile in deciles) {
      by_model <- lapply(models, function(model) {
        rows <- data[data$decile == decile & as.character(data$model_id) == model, , drop = FALSE]
        rank <- support[as.character(support$model_id) == model, , drop = FALSE]
        if (!nrow(rows) || nrow(rank) != 1L) return(NULL)
        rows <- rows[order(rows$sim_year), , drop = FALSE]
        applied <- lapply(fields[1:4], function(field) apply_adverse_year_support(
          rows[[field]], rows$sim_year, rank))
        if (!all(vapply(applied, function(x) identical(x$status, "ok"), logical(1)))) return(NULL)
        setNames(vapply(applied, `[[`, numeric(1), "value"), fields[1:4])
      })
      if (any(vapply(by_model, is.null, logical(1)))) next
      states <- as.list(colMeans(do.call(rbind, by_model)))
      row <- as.data.frame(c(list(scenario = scenario, decile = as.integer(decile)),
        .policy_metric_contributions(states)), stringsAsFactors = FALSE)
      row$n_models <- length(models)
      values[[length(values) + 1L]] <- row
    }
    return(dplyr::bind_rows(values))
  }
  models <- split(data, as.character(data$model_id))
  values <- list()
  for (decile in sort(unique(data$decile))) {
    by_model <- lapply(models, function(rows) {
      rows <- rows[rows$decile == decile, , drop = FALSE]
      if (!nrow(rows) || any(!is.finite(as.matrix(rows[, fields, drop = FALSE])))) return(NULL)
      rows <- rows[order(rows$sim_year), , drop = FALSE]
      states <- as.matrix(rows[, fields[1:4], drop = FALSE])
      colMeans(states)
    })
    if (any(vapply(by_model, is.null, logical(1)))) next
    states <- as.list(colMeans(do.call(rbind, by_model)))[fields[1:4]]
    row <- as.data.frame(c(list(scenario = scenario, decile = as.integer(decile)),
      .policy_metric_contributions(states)), stringsAsFactors = FALSE)
    row$n_models <- length(models)
    values[[length(values) + 1L]] <- row
  }
  dplyr::bind_rows(values)
}

.policy_metric_headline_data <- function(result, scenario, basis = "mean") {
  if (!is.list(result) || !length(result$scenarios) || is.null(scenario)) return(data.frame())
  selected <- result$scenarios[[scenario]]
  if (is.null(selected) || !identical(selected$status, "ok")) return(data.frame())
  if (identical(basis, "mean")) {
    summary <- selected$summary
    if (!is.data.frame(summary) || nrow(summary) != 1L) return(data.frame())
    row <- summary[1L, , drop = FALSE]
    return(data.frame(scenario = scenario,
      channel_id = c("level", "resilience", "total"),
      channel = c("Main effect", "Resilience", "Total"),
      value = as.numeric(unlist(row[1L, c("main", "resilience", "total")], use.names = FALSE)),
      stringsAsFactors = FALSE))
  }
  probability <- if (identical(basis, "adverse_10")) 0.10 else 0.05
  rows <- selected$return_period
  rows <- if (is.data.frame(rows)) rows[rows$scope == "baseline_anchored" &
    rows$probability == probability & rows$status == "ok" &
    rows$scenario == scenario, , drop = FALSE] else data.frame()
  if (nrow(rows) != 1L) return(data.frame())
  data.frame(scenario = scenario, channel_id = c("level", "resilience", "total"),
    channel = c("Main effect", "Resilience", "Total"),
    value = as.numeric(unlist(rows[1L, c("main", "resilience", "total")], use.names = FALSE)),
    stringsAsFactors = FALSE)
}

.policy_metric_headline_summary <- function(result, basis = "mean") {
  if (!is.list(result) || !length(result$scenarios)) return(data.frame())
  values <- lapply(names(result$scenarios), function(scenario) {
    .policy_metric_headline_data(result, scenario, basis)
  })
  dplyr::bind_rows(values)
}

# Shared echarts colour mapping for decomposition scenarios: Historical in the
# muted history grey, fixed SSP hues for climate scenarios, then the
# categorical palette for anything else (same mapping as the ggplot builders).
#' @noRd
.decomp_scenario_colours <- function(scenario_levels) {
  stats::setNames(vapply(scenario_levels, function(s) {
    if (identical(s, "Historical")) {
      return(.wise_history)
    }
    k <- .normalise_ssp(s)
    if (!is.na(k) && k %in% names(.ssp_colours)) {
      return(unname(.ssp_colours[[k]]))
    }
    idx <- match(s, scenario_levels)
    unname(.wise_cat[idx])
  }, character(1L)), scenario_levels)
}

#' Headline decomposition chart (echarts4r)
#'
#' Grouped bars, one series per scenario, over the Main effect / Resilience /
#' Total channels.
#'
#' @param summary_df A decomposition summary as returned by
#'   `decomposition_summary_data()` (with a `scenario` column).
#' @param y_label Y-axis title.
#' @param height Widget height.
#'
#' @return An `echarts4r` widget; a blank placeholder widget when the inputs
#'   are unavailable.
#'
#' @noRd
echart_decomposition_headline <- function(summary_df,
                                          y_label = "Policy effect (percent change)",
                                          height = "360px") {
  if (is.null(summary_df) || !nrow(summary_df)) {
    return(echart_blank("Decomposition is unavailable.", height = height))
  }
  ids <- c("level", "resilience", "total")
  df <- summary_df[summary_df$channel_id %in% ids, , drop = FALSE]
  if (!nrow(df)) {
    return(echart_blank("Decomposition is unavailable.", height = height))
  }
  df$channel <- factor(df$channel, levels = c("Main effect", "Resilience", "Total"))
  if (!"scenario" %in% names(df)) df$scenario <- "Historical"
  scenario_levels <- unique(as.character(df$scenario))
  scenario_colours <- .decomp_scenario_colours(scenario_levels)

  long <- data.frame(
    channel = as.character(df$channel),
    scenario = as.character(df$scenario),
    percent = suppressWarnings(as.numeric(df$percent)),
    stringsAsFactors = FALSE
  )
  # echarts4r's e_charts() rejects single-column / single-row frames under the
  # default reorder; the grouped-bar pipe needs 3+ columns anyway.
  fmt <- htmlwidgets::JS(
    "function(v){ if (v == null || isNaN(v)) return '-';",
    " return Number(v).toFixed(2) + '%'; }"
  )
  long |>
    echarts4r::group_by(scenario) |>
    echarts4r::e_charts(channel, height = height) |>
    echarts4r::e_bar(percent) |>
    echarts4r::e_color(unname(scenario_colours)) |>
    echarts4r::e_legend(
      orient = "horizontal", left = "center", top = 4
    ) |>
    echarts4r::e_x_axis(
      axisLabel = wise_eaxis_label(fontSize = 13),
      axisTick = list(alignWithLabel = TRUE),
      axisLine = list(lineStyle = list(color = .wise_grid))
    ) |>
    echarts4r::e_y_axis(
      name = y_label,
      nameLocation = "end", nameRotate = 0, nameGap = 20,
      nameMoveOverlap = FALSE,
      nameTextStyle = wise_eaxis_name(align = "left"),
      axisLabel = wise_eaxis_label(formatter = htmlwidgets::JS(
        "function(v){return Number(v).toLocaleString('en-US',{maximumFractionDigits:1})+'%';}"
      )),
      splitLine = wise_esplit_line()
    ) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 86, bottom = 30) |>
    wise_echart_theme() |>
    .wise_zero_markline()
}

echart_policy_metric_headline <- function(summary_df, metadata, height = "360px") {
  if (is.null(summary_df) || !nrow(summary_df)) {
    return(echart_blank("Metric-aware decomposition is unavailable.", height = height))
  }
  summary_df <- summary_df[summary_df$channel %in% c("Main effect", "Resilience", "Total"), , drop = FALSE]
  if (!nrow(summary_df)) return(echart_blank("Metric-aware decomposition is unavailable.", height = height))
  summary_df$channel <- factor(summary_df$channel,
    levels = c("Main effect", "Resilience", "Total"))
  multiplier <- suppressWarnings(as.numeric(metadata$display_multiplier))[1L]
  if (!length(multiplier) || !is.finite(multiplier)) multiplier <- 1
  summary_df$display_value <- summary_df$value * multiplier
  fmt <- htmlwidgets::JS("function(v){ if (v == null || isNaN(v)) return '-'; return Number(v).toLocaleString('en-US',{maximumFractionDigits:2}); }")
  channels <- levels(droplevels(summary_df$channel))
  channel_colours <- c("Main effect" = .wise_cat[[4]],
    "Resilience" = .wise_cat[[3]], "Total" = .wise_support)
  e <- summary_df |>
    echarts4r::e_charts(channel, height = height) |>
    echarts4r::e_bar(display_value, name = "Selected metric change") |>
    echarts4r::e_x_axis(axisLabel = wise_eaxis_label(fontSize = 13),
      axisTick = list(alignWithLabel = TRUE), axisLine = list(lineStyle = list(color = .wise_grid))) |>
    echarts4r::e_y_axis(name = metadata$change_unit %||% "Selected metric change",
      nameLocation = "end", nameRotate = 0, nameGap = 20,
      nameTextStyle = wise_eaxis_name(align = "left"),
      axisLabel = wise_eaxis_label(), splitLine = wise_esplit_line()) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 24, bottom = 30) |>
    wise_echart_theme()
  e$x$opts$xAxis[[1L]]$data <- channels
  e$x$opts$legend <- list(show = FALSE)
  e$x$opts$series[[1L]]$data <- lapply(seq_len(nrow(summary_df)), function(i) {
    channel <- as.character(summary_df$channel[[i]])
    list(value = summary_df$display_value[[i]],
      itemStyle = list(color = unname(channel_colours[[channel]])))
  })
  .wise_zero_markline(e)
}

echart_policy_metric_deciles <- function(data, metadata, height = "450px") {
  if (is.null(data) || !nrow(data)) {
    return(echart_blank("Metric-aware decile decomposition is unavailable.", height = height))
  }
  channel_cols <- c("main", "repositioning", "interaction")
  labels <- c(main = "Main effect", repositioning = "Repositioning", interaction = "Interaction")
  active <- vapply(channel_cols, function(field) any(abs(data[[field]]) > 1e-12, na.rm = TRUE), logical(1))
  if (any(active)) channel_cols <- channel_cols[active]
  multiplier <- suppressWarnings(as.numeric(metadata$display_multiplier))[1L]
  if (!length(multiplier) || !is.finite(multiplier)) multiplier <- 1
  long <- do.call(rbind, lapply(channel_cols, function(field) data.frame(
    decile = as.character(data$decile), channel = unname(labels[[field]]),
    value = data[[field]] * multiplier, stringsAsFactors = FALSE)))
  levels_decile <- as.character(sort(unique(data$decile)))
  long$decile <- factor(long$decile, levels = levels_decile)
  long$channel <- factor(long$channel, levels = unname(labels[channel_cols]))
  colours <- stats::setNames(c(.wise_cat[[4]], .wise_cat[[3]], .wise_cat[[6]])[
    match(unname(labels[channel_cols]), c("Main effect", "Repositioning", "Interaction"))],
    unname(labels[channel_cols]))
  fmt <- htmlwidgets::JS("function(v){ if (v == null || isNaN(v)) return '-'; return Number(v).toLocaleString('en-US',{maximumFractionDigits:2}); }")
  e <- long |>
    echarts4r::group_by(channel) |>
    echarts4r::e_charts(decile, height = height) |>
    echarts4r::e_bar(value, stack = "channels") |>
    echarts4r::e_color(unname(colours)) |>
    echarts4r::e_legend(orient = "horizontal", left = "center", top = 4) |>
    echarts4r::e_x_axis(name = "Fixed observed baseline welfare decile (1 = poorest)",
      nameLocation = "middle", nameGap = 28, nameTextStyle = wise_eaxis_name(fontSize = 13),
      axisLabel = wise_eaxis_label(fontSize = 13), axisTick = list(alignWithLabel = TRUE),
      axisLine = list(lineStyle = list(color = .wise_grid))) |>
    echarts4r::e_y_axis(name = metadata$change_unit %||% "Selected metric change",
      nameLocation = "end", nameRotate = 0, nameGap = 20,
      nameTextStyle = wise_eaxis_name(align = "left"), axisLabel = wise_eaxis_label(),
      splitLine = wise_esplit_line()) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 86, bottom = 62) |>
    wise_echart_theme() |>
    .wise_zero_markline()
  marker_pts <- lapply(seq_len(nrow(data)), function(i) {
    d <- as.character(data$decile[[i]])
    if (!d %in% levels_decile || !is.finite(data$total[[i]])) return(NULL)
    list(value = c(match(d, levels_decile) - 1L, data$total[[i]] * multiplier))
  })
  e$x$opts$series <- append(e$x$opts$series, list(list(type = "scatter", name = "Total effect",
    data = Filter(Negate(is.null), marker_pts), symbol = "circle", symbolSize = 8,
    itemStyle = list(color = "white", borderColor = .wise_support, borderWidth = 1.2),
    tooltip = list(valueFormatter = fmt), z = 6)))
  e
}

# Inject a dashed zero reference line on the first series (echarts4r has no
# direct verb for a horizontal markLine at a fixed value).
#' @noRd
.wise_zero_markline <- function(e) {
  if (!length(e$x$opts$series)) {
    return(e)
  }
  e$x$opts$series[[1L]]$markLine <- list(
    silent = TRUE,
    symbol = "none",
    lineStyle = list(
      type = "dashed",
      color = .wise_zero,
      width = 1
    ),
    label = list(show = FALSE),
    data = list(list(yAxis = 0))
  )
  e
}

echart_decomposition_channels_by_decile <- function(tbl,
                                                    is_rif = NULL,
                                                    height = "450px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank(
      "Decile decomposition is unavailable for this run.",
      height = height
    ))
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
  colours <- c(
    "SP direct effect" = .wise_cat[[1]],
    "Main effect (covariate shift)" = .wise_cat[[4]],
    "Resilience - Repositioning effect" = .wise_cat[[3]],
    "Resilience - Interaction effect" = .wise_cat[[6]]
  )
  long <- do.call(rbind, lapply(channel_cols, function(col) {
    data.frame(
      decile = as.character(tbl$decile),
      channel = unname(channel_labels[[col]]),
      effect = suppressWarnings(as.numeric(tbl[[col]])),
      stringsAsFactors = FALSE
    )
  }))
  # Keep the ggplot's channel stacking order (first channel at the base) and
  # a numerically sorted decile category axis (marker series addresses
  # categories by index).
  decile_levels <- as.character(sort(unique(as.numeric(unique(long$decile)))))
  long$channel <- factor(long$channel,
    levels = unname(channel_labels[channel_cols])
  )
  long$decile <- factor(long$decile, levels = decile_levels)
  fmt <- htmlwidgets::JS(
    "function(v){ if (v == null || isNaN(v)) return '-';",
    " return Number(v).toFixed(2) + '%'; }"
  )
  e <- long |>
    echarts4r::group_by(channel) |>
    echarts4r::e_charts(decile, height = height) |>
    echarts4r::e_bar(effect, stack = "channels") |>
    echarts4r::e_color(unname(colours[unname(channel_labels[channel_cols])])) |>
    echarts4r::e_legend(
      orient = "horizontal", left = "center", top = 4,
      data = as.list(unname(channel_labels[channel_cols]))
    ) |>
    echarts4r::e_x_axis(
      name = "Fixed observed baseline welfare decile (1 = poorest)",
      nameLocation = "middle",
      nameGap = 28,
      nameMoveOverlap = FALSE,
      nameTextStyle = wise_eaxis_name(fontSize = 13),
      axisLabel = wise_eaxis_label(fontSize = 13),
      axisTick = list(alignWithLabel = TRUE),
      axisLine = list(lineStyle = list(color = .wise_grid))
    ) |>
    echarts4r::e_y_axis(
      name = "Policy effect (percent change)",
      nameLocation = "end", nameRotate = 0, nameGap = 20,
      nameMoveOverlap = FALSE,
      nameTextStyle = wise_eaxis_name(align = "left"),
      axisLabel = wise_eaxis_label(formatter = htmlwidgets::JS(
        "function(v){return Number(v).toLocaleString('en-US',{maximumFractionDigits:1})+'%';}"
      )),
      splitLine = wise_esplit_line()
    ) |>
    echarts4r::e_tooltip(trigger = "axis", valueFormatter = fmt) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 106, bottom = 62) |>
    wise_echart_theme() |>
    .wise_zero_markline()

  # Total-effect marker per decile (open dark-ringed dot in the ggplot
  # version, drawn outside the stacked channels). Injected as a raw scatter
  # series so it never enters the channel legend or the channel stack.
  marker_pts <- lapply(seq_len(nrow(tbl)), function(i) {
    d <- as.character(tbl$decile[[i]])
    if (!d %in% decile_levels || !is.finite(suppressWarnings(as.numeric(tbl$total_percent[[i]])))) {
      return(NULL)
    }
    list(value = c(match(d, decile_levels) - 1L, tbl$total_percent[[i]]))
  })
  e$x$opts$series <- append(e$x$opts$series, list(list(
    type = "scatter",
    name = "Total effect",
    data = Filter(Negate(is.null), marker_pts),
    symbol = "circle",
    symbolSize = 8,
    itemStyle = list(
      color = "white",
      borderColor = .wise_support,
      borderWidth = 1.2
    ),
    tooltip = list(valueFormatter = fmt),
    z = 6
  )))
  e
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
  if (is.null(decomp_df) || !nrow(decomp_df)) {
    return(tibble::tibble())
  }
  is_rif <- if (is.null(is_rif)) {
    "delta_res1" %in% names(decomp_df) &&
      any(abs(decomp_df$delta_res1 %||% 0) > 1e-12, na.rm = TRUE)
  } else {
    isTRUE(is_rif)
  }
  if (!is.null(svy)) {
    dec <- baseline_deciles %||% weighted_baseline_deciles(
      svy, outcome, baseline_weight_column(svy)
    )
    ids <- suppressWarnings(as.integer(decomp_df$id))
    mapped <- is.finite(ids) & ids >= 1L & ids <= length(dec)
    stored_deciles <- if ("decile" %in% names(decomp_df)) {
      suppressWarnings(as.integer(decomp_df$decile))
    } else {
      integer(0)
    }
    decomp_df$decile <- NA_integer_
    decomp_df$decile[mapped] <- dec[ids[mapped]]
    if (!any(is.finite(decomp_df$decile)) && length(stored_deciles) == nrow(decomp_df)) {
      decomp_df$decile <- stored_deciles
    }
  }
  decomp_df <- decomp_df[is.finite(decomp_df$decile), , drop = FALSE]
  if (!nrow(decomp_df)) {
    return(tibble::tibble())
  }
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

#' RIF weather-sensitivity beta curve (echarts4r)
#'
#' Browser-side counterpart of the RIF branch of `make_weather_effect_plot()`
#' as called from mod_3_09 (`fit = NULL`, `weather_df = NULL`,
#' `effect_scale = "model"`): weather coefficient against baseline welfare
#' quantile with a 95% CI ribbon. Statistics are reproduced with the same
#' parameters (model 3 grid, term matching, bin labels, moderator evaluation
#' at 0/1 when no weather frame is supplied); echarts only draws the
#' precomputed values.
#'
#' Judgment call vs the ggplot version: binned predictors draw one series per
#' bin in a single panel (the ggplot facets one panel per bin), and the
#' "Ribbon = 95% CI" caption is dropped (no in-chart text).
#'
#' @param rif_grid          The model fit's `rif_grid` data frame.
#' @param pred_var          Weather variable whose beta curve is drawn.
#' @param interaction_terms Interaction terms of the model specification.
#' @param label_fun         Variable-label function (as in Step 1 modules).
#' @param height            Widget height.
#'
#' @return An `echarts4r` widget (never `NULL`; empty inputs render the same
#'   user-facing messages as the ggplot builder).
#'
#' @noRd
echart_rif_weather_curve <- function(rif_grid, pred_var,
                                     interaction_terms = character(0),
                                     label_fun = identity,
                                     height = "400px") {
  tryCatch(
    {
      blank <- function(msg) echart_blank(msg, height = height)
      if (is.null(rif_grid) || !is.data.frame(rif_grid) || !nrow(rif_grid) ||
        is.null(pred_var) || !nzchar(pred_var)) {
        return(blank("No RIF terms found."))
      }
      grid3 <- rif_grid[rif_grid$model == 3L, , drop = FALSE]
      pred_esc <- gsub("([\\[\\]\\(\\)\\^\\$\\.\\*\\+\\?])", "\\\\\\1", pred_var)
      mask <- grepl(paste0("\\b", pred_esc, "\\b"), grid3$term)
      if (!any(mask)) {
        return(blank(paste0("No RIF terms found for '", pred_var, "'.")))
      }
      plot_data <- grid3[mask, ]
      taus <- sort(unique(plot_data$tau))
      n_terms <- length(unique(plot_data$term))
      has_int_terms <- any(grepl(":", plot_data$term, fixed = TRUE))
      y_lab <- "Effect (log points)"

      .bin_lower <- function(tm) {
        s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", tm)
        suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
      }
      .bin_label <- function(tm) {
        s <- sub(paste0("^", pred_esc, "[\\[\\(]"), "", tm)
        if (identical(s, tm)) {
          return(tm)
        }
        s <- sub("[])]$", "", s)
        parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1L]])
        if (length(parts) != 2L || any(!nzchar(parts))) {
          return(tm)
        }
        paste0(parts[[1L]], "\u2013", parts[[2L]])
      }

      curves <- if (n_terms > 1L && !has_int_terms) {
        tu <- unique(plot_data$term)
        tu <- tu[order(suppressWarnings(.bin_lower(tu)))]
        lapply(tu, function(tm) {
          d <- plot_data[plot_data$term == tm, ]
          d <- d[order(d$tau), ]
          list(name = .bin_label(tm), data = d)
        })
      } else if (n_terms == 1L) {
        d <- plot_data[order(plot_data$tau), ]
        list(list(name = "Beta", data = d))
      } else {
        # Moderated: pair main rows with interaction rows per bin and tau,
        # evaluating the combined effect at each moderator level. Module calls
        # pass no weather frame, so binary moderators evaluate at 0/1.
        protected <- gsub("::", "", plot_data$term, fixed = TRUE)
        parts <- strsplit(protected, ":", fixed = TRUE)
        weather_pat <- paste0("\\b", pred_esc, "\\b")
        is_int_row <- lengths(parts) > 1L
        main_part <- vapply(parts, function(p) {
          hit <- p[grepl(weather_pat, p)]
          if (length(hit) == 0) p[1] else hit[1]
        }, character(1))
        modx_var <- NULL
        modx_lab <- NULL
        if (length(interaction_terms) > 0) {
          mt <- interaction_terms[grepl(paste0("\\b", pred_esc, "\\b"), interaction_terms)]
          if (length(mt) > 0) {
            mp <- strsplit(mt[1], ":", fixed = TRUE)[[1L]]
            modx_var <- mp[mp != pred_var][1L]
            if (!is.na(modx_var) && nzchar(modx_var)) {
              modx_lab <- label_fun(modx_var)
            }
          }
        }
        modx_vals <- c(0, 1)
        plot_data$.bin_id <- main_part
        main_rows <- plot_data[!is_int_row, , drop = FALSE]
        int_rows <- plot_data[is_int_row, , drop = FALSE]
        combined <- do.call(rbind, lapply(modx_vals, function(v) {
          do.call(rbind, lapply(seq_len(nrow(main_rows)), function(j) {
            mr <- main_rows[j, , drop = FALSE]
            ir <- int_rows[int_rows$.bin_id == mr$.bin_id &
              int_rows$tau == mr$tau, , drop = FALSE]
            ie <- if (nrow(ir) > 0) ir$estimate[1] else 0
            ise <- if (nrow(ir) > 0) ir$std.error[1] else 0
            effect <- mr$estimate + v * ie
            se <- sqrt(mr$std.error^2 + v^2 * ise^2)
            data.frame(
              tau = mr$tau,
              .bin_id = mr$.bin_id,
              estimate = effect,
              conf.low = effect - 1.96 * se,
              conf.high = effect + 1.96 * se,
              modx = v,
              stringsAsFactors = FALSE
            )
          }))
        }))
        modx_lab_print <- modx_lab %||% (modx_var %||% "moderator")
        modx_name <- function(v) {
          v_chr <- as.character(v)
          num <- suppressWarnings(as.numeric(v_chr))
          if (!is.na(num) && num %in% c(0, 1)) {
            paste0(modx_lab_print, ": ", if (num == 1) "yes" else "no")
          } else if (!is.na(num)) {
            paste0(modx_lab_print, " = ", round(num, 2))
          } else {
            paste0(modx_lab_print, " = ", v_chr)
          }
        }
        # The ggplot facets per bin; a single echarts panel draws one series
        # per (bin, moderator level) so each tau appears once per series.
        bins <- unique(main_rows$.bin_id)
        bins <- bins[order(suppressWarnings(.bin_lower(bins)))]
        out <- lapply(bins, function(b) {
          lapply(modx_vals, function(v) {
            d <- combined[combined$modx == v & combined$.bin_id == b, ]
            d <- d[order(d$tau), ]
            nm <- modx_name(v)
            if (length(bins) > 1L) nm <- paste0(nm, " \u00b7 ", .bin_label(b))
            list(name = nm, data = d)
          })
        })
        unlist(out, recursive = FALSE)
      }

      cols <- if (length(curves) == 1L) {
        .wise_blue
      } else if (has_int_terms && n_terms > 1L) {
        .wise_cat[(seq_along(curves) - 1L) %% length(.wise_cat) + 1L]
      } else {
        wise_seq_ramp(length(curves))
      }

      series_list <- lapply(seq_along(curves), function(i) {
        d <- curves[[i]]$data
        col <- if (length(cols) == 1L) cols else cols[[i]]
        poly <- rbind(
          cbind(d$tau, d$conf.high),
          cbind(rev(d$tau), d$conf.low)
        )
        ribbon <- list(
          type = "line",
          name = paste0(curves[[i]]$name, " CI"),
          data = unname(poly),
          symbol = "none",
          lineStyle = list(width = 0, opacity = 0),
          areaStyle = list(color = col, opacity = 0.15),
          silent = TRUE,
          z = 1
        )
        curve <- list(
          type = "line",
          name = curves[[i]]$name,
          data = unname(cbind(d$tau, d$estimate)),
          symbol = "circle",
          symbolSize = 5,
          lineStyle = list(color = col, width = 1.1),
          itemStyle = list(color = col),
          z = 3
        )
        list(ribbon, curve)
      })
      series_list <- unlist(series_list, recursive = FALSE)

      tau_pct <- htmlwidgets::JS(
        "function(v){ return Math.round(Number(v) * 100) + '%'; }"
      )
      dummy_tau <- if (length(taus) >= 2L) taus[1:2] else c(taus, taus)
      e <- echarts4r::e_charts(
        data.frame(tau = dummy_tau, y = 0:1),
        tau,
        height = height
      )
      e$x$opts$series <- list()
      e$x$opts$xAxis <- list(
        type = "category",
        data = as.list(taus),
        boundaryGap = FALSE,
        name = "Welfare quantile",
        nameLocation = "middle",
        nameGap = 28,
        nameTextStyle = wise_eaxis_name(fontSize = 13),
        axisLabel = wise_eaxis_label(fontSize = 12, formatter = tau_pct),
        axisTick = list(alignWithLabel = TRUE),
        axisLine = list(lineStyle = list(color = .wise_grid))
      )
      e$x$opts$yAxis <- list(
        type = "value",
        name = y_lab,
        nameTextStyle = wise_eaxis_name(),
        axisLabel = wise_eaxis_label(),
        splitLine = wise_esplit_line()
      )
      e$x$opts$series <- series_list
      if (length(curves) > 1L) {
        e$x$opts$legend <- list(
          orient = "horizontal",
          left = 0,
          top = "bottom",
          data = as.list(vapply(curves, `[[`, character(1L), "name")),
          textStyle = list(color = .wise_charcoal, fontSize = 13)
        )
        e$x$opts$grid <- list(
          containLabel = TRUE, left = 8, right = 14, top = 24, bottom = 58
        )
      } else {
        e$x$opts$grid <- list(
          containLabel = TRUE, left = 8, right = 14, top = 24, bottom = 28
        )
      }
      e$x$opts$tooltip <- list(
        trigger = "axis",
        textStyle = list(color = .wise_charcoal, fontSize = 13)
      )
      e$x$opts$textStyle <- list(fontFamily = "Helvetica, Arial, sans-serif")
      e$x$opts$series[[1L]]$markLine <- list(
        silent = TRUE,
        symbol = "none",
        lineStyle = list(type = "dashed", color = .wise_zero, width = 1),
        label = list(show = FALSE),
        data = list(list(yAxis = 0))
      )
      e
    },
    error = function(e) {
      echart_blank(
        paste0("RIF effect plot error: ", conditionMessage(e)),
        height = height
      )
    }
  )
}

decomposition_decile_export <- function(tbl, is_rif = FALSE) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(NULL)
  }
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
      after = 3L
    )
  }
  cols <- cols[names(cols) %in% names(tbl)]
  out <- as.data.frame(tbl[, names(cols), drop = FALSE])
  names(out) <- unname(cols)
  out
}
