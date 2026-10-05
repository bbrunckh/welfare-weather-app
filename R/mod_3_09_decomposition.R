#' Select the simulated year corresponding to a weather basis.
#'
#' @noRd
select_decomp_weather_basis <- function(decomp_df, basis = "mean", so = NULL) {
  if (is.null(decomp_df) || !is.data.frame(decomp_df) || !nrow(decomp_df) ||
    identical(basis, "mean") || !"sim_year" %in% names(decomp_df) ||
    !"delta_total" %in% names(decomp_df)) {
    return(if (identical(basis, "mean")) decomp_df else decomp_df[0, , drop = FALSE])
  }

  target <- if (identical(basis, "adverse_10")) 0.10 else 0.05
  year_total <- tapply(seq_len(nrow(decomp_df)), decomp_df$sim_year, function(idx) {
    vals <- as.numeric(decomp_df$delta_total[idx])
    w <- if ("weight" %in% names(decomp_df)) as.numeric(decomp_df$weight[idx]) else rep(1, length(idx))
    ok <- is.finite(vals) & is.finite(w) & w > 0
    if (!any(ok)) {
      return(NA_real_)
    }
    stats::weighted.mean(vals[ok], w[ok])
  })
  year_total <- year_total[is.finite(year_total)]
  if (!length(year_total)) {
    return(decomp_df)
  }

  adverse_high <- identical(
    outcome_direction(so$name %||% "welfare", so$type %||% "numeric"),
    "lower_is_better"
  )
    ordered <- order(year_total, decreasing = adverse_high)
    take <- max(1L, ceiling(length(ordered) * target))
  selected_year <- names(year_total)[ordered[[take]]]
  decomp_df[as.character(decomp_df$sim_year) == selected_year, , drop = FALSE]
}

.compact_decomp_channels <- c(
  "delta_total", "delta_main", "delta_sp", "delta_main_covar",
  "delta_res", "delta_res1", "delta_res2"
)

.compact_decomp_stat <- function(values, weights) {
  values <- suppressWarnings(as.numeric(values))
  weights <- suppressWarnings(as.numeric(weights))
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) {
    return(c(sum = NA_real_, weight = NA_real_))
  }
  c(
    sum = sum(values[ok] * weights[ok]),
    weight = sum(weights[ok])
  )
}

.compact_decomp_values <- function(decomp_df) {
  n <- nrow(decomp_df)
  zero <- rep(0, n)
  main <- decomp_df$delta_main %||% zero
  direct <- decomp_df$delta_sp %||% zero
  covariate <- decomp_df$delta_main_covar %||% (main - direct)
  res1 <- decomp_df$delta_res1 %||% zero
  res2 <- decomp_df$delta_res2 %||% zero
  total <- decomp_df$delta_total %||% (main + res1 + res2)
  list(
    delta_total = total,
    delta_main = main,
    delta_sp = direct,
    delta_main_covar = covariate,
    delta_res = res1 + res2,
    delta_res1 = res1,
    delta_res2 = res2
  )
}

.compact_decomp_stats <- function(values, weights, prefix = "") {
  out <- lapply(values, .compact_decomp_stat, weights = weights)
  out <- unlist(out, use.names = TRUE)
  names(out) <- unlist(lapply(names(values), function(name) {
    c(paste0(prefix, "sum_", name), paste0(prefix, "weight_", name))
  }))
  out
}

.compact_decomp_deciles <- function(decomp_df, baseline_deciles) {
  ids <- suppressWarnings(as.integer(decomp_df$id))
  deciles <- if (!is.null(baseline_deciles) && length(baseline_deciles)) {
    mapped <- rep(NA_integer_, nrow(decomp_df))
    ok <- is.finite(ids) & ids >= 1L & ids <= length(baseline_deciles)
    mapped[ok] <- suppressWarnings(as.integer(baseline_deciles[ids[ok]]))
    mapped
  } else {
    rep(NA_integer_, nrow(decomp_df))
  }
  if (!any(is.finite(deciles)) && "decile" %in% names(decomp_df)) {
    deciles <- suppressWarnings(as.integer(decomp_df$decile))
  }
  deciles
}

.compact_future_decomposition <- function(decomp_df, scenario, sim_year,
                                          year_start = NA_integer_,
                                          year_end = NA_integer_,
                                          baseline_deciles = NULL,
                                          is_rif = FALSE, engine = NULL) {
  if (is.null(decomp_df) || !is.data.frame(decomp_df) || !nrow(decomp_df)) {
    return(NULL)
  }
  weights <- if ("weight" %in% names(decomp_df)) {
    suppressWarnings(as.numeric(decomp_df$weight))
  } else {
    rep(1, nrow(decomp_df))
  }
  weights[!is.finite(weights) | weights < 0] <- NA_real_
  values <- .compact_decomp_values(decomp_df)
  metadata <- list(
    scenario = as.character(scenario),
    sim_year = sim_year,
    year_start = year_start,
    year_end = year_end
  )
  channel <- as.data.frame(c(metadata, .compact_decomp_stats(values, weights)),
    stringsAsFactors = FALSE
  )
  channel$engine <- engine %||% if (isTRUE(is_rif)) "rif" else "fixest"
  channel$is_rif <- isTRUE(is_rif)

  deciles <- .compact_decomp_deciles(decomp_df, baseline_deciles)
  decile_rows <- lapply(sort(unique(deciles[is.finite(deciles)])), function(d) {
    ok <- deciles == d
    row <- c(metadata, list(
      decile = as.integer(d),
      n_households = sum(ok, na.rm = TRUE),
      weighted_population = sum(weights[ok], na.rm = TRUE)
    ))
    row <- c(row, .compact_decomp_stats(
      lapply(values, `[`, ok), weights[ok]
    ))
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  deciles_out <- if (length(decile_rows)) {
    dplyr::bind_rows(decile_rows)
  } else {
    data.frame()
  }
  if (nrow(deciles_out)) {
    deciles_out$decile <- as.integer(deciles_out$decile)
    deciles_out$n_households <- as.integer(deciles_out$n_households)
    deciles_out$weighted_population <- as.numeric(deciles_out$weighted_population)
  }

  list(
    channel = channel,
    decile = deciles_out,
    metadata = metadata,
    engine = channel$engine[[1L]],
    is_rif = isTRUE(is_rif)
  )
}

.bind_compact_future_decompositions <- function(parts, engine = NULL,
                                                is_rif = FALSE) {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    return(structure(list(
      channel_summary = data.frame(),
      decile_summary = data.frame(),
      scenario_metadata = data.frame(),
      engine = engine %||% if (isTRUE(is_rif)) "rif" else "fixest",
      is_rif = isTRUE(is_rif),
      scenario_order = character(0)
    ), class = c("wise_compact_decomp_scenarios", "list")))
  }
  channel <- dplyr::bind_rows(lapply(parts, `[[`, "channel"))
  decile <- dplyr::bind_rows(lapply(parts, `[[`, "decile"))
  scenario_order <- unique(vapply(parts, function(x) x$metadata$scenario, character(1L)))
  metadata <- unique(channel[, c("scenario", "year_start", "year_end", "engine", "is_rif"),
    drop = FALSE
  ])
  structure(list(
    channel_summary = channel,
    decile_summary = decile,
    scenario_metadata = metadata,
    engine = engine %||% parts[[1L]]$engine,
    is_rif = isTRUE(is_rif),
    scenario_order = scenario_order
  ), class = c("wise_compact_decomp_scenarios", "list"))
}

.is_compact_decomp_scenarios <- function(x) {
  inherits(x, "wise_compact_decomp_scenarios")
}

.compact_future_scenarios <- function(x) {
  if (!.is_compact_decomp_scenarios(x)) {
    return(character(0))
  }
  x$scenario_order %||% unique(as.character(x$channel_summary$scenario))
}

.compact_future_year <- function(x, scenario, basis = "mean", so = NULL) {
  rows <- x$channel_summary[
    as.character(x$channel_summary$scenario) == as.character(scenario), ,
    drop = FALSE
  ]
  if (!nrow(rows) || identical(basis, "mean")) {
    return(rows)
  }
  annual <- rows
  total <- annual$sum_delta_total / annual$weight_delta_total
  annual <- annual[is.finite(total), , drop = FALSE]
  total <- total[is.finite(total)]
  if (!nrow(annual) || !length(total)) {
    return(rows)
  }
  adverse_high <- identical(
    outcome_direction(so$name %||% "welfare", so$type %||% "numeric"),
    "lower_is_better"
  )
  ordered <- order(total, decreasing = adverse_high)
  take <- max(1L, min(length(ordered), round(length(ordered) *
    if (identical(basis, "adverse_10")) 0.10 else 0.05)))
  annual[ordered[[take]], , drop = FALSE]
}

.compact_supported_values <- function(rows, support_rows, probability) {
  support_rows <- support_rows[support_rows$probability == probability, , drop = FALSE]
  ids <- unique(as.character(support_rows$model_id))
  if (!length(ids) || !nrow(rows)) return(NULL)
  channels <- list()
  for (id in ids) {
    support_row <- support_rows[support_rows$model_id == id, , drop = FALSE]
    if (nrow(support_row) != 1L) return(NULL)
    members <- if (identical(id, "Historical")) "Historical" else id
    annual <- rows[as.character(rows$member) == members, , drop = FALSE]
    if (!nrow(annual) || !identical(support_row$status[[1L]], "ok")) return(NULL)
    support <- support_row[1L, , drop = FALSE]
    model <- setNames(vector("list", length(.compact_decomp_channels)), .compact_decomp_channels)
    for (name in .compact_decomp_channels) {
      sums <- annual[[paste0("sum_", name)]]
      weights <- annual[[paste0("weight_", name)]]
      annual_value <- ifelse(is.finite(sums) & is.finite(weights) & weights > 0, sums / weights, NA_real_)
      applied <- apply_adverse_year_support(annual_value, as.numeric(annual$sim_year), support)
      if (!identical(applied$status, "ok")) return(NULL)
      model[[name]] <- applied$value
    }
    baseline <- if (all(c("sum_baseline_y_point", "weight_baseline_y_point") %in% names(annual))) {
      annual$sum_baseline_y_point / annual$weight_baseline_y_point
    } else rep(NA_real_, nrow(annual))
    baseline_applied <- apply_adverse_year_support(baseline,
      as.numeric(annual$sim_year), support)
    if (!identical(baseline_applied$status, "ok") ||
        abs(baseline_applied$value - support_row$baseline_value[[1L]]) >
          .policy_metric_tolerance * max(1, abs(support_row$baseline_value[[1L]]))) return(NULL)
    channels[[id]] <- unlist(model, use.names = TRUE)
  }
  as.list(colMeans(do.call(rbind, channels)))
}

.compact_supported_decomp <- function(x, scenario, basis, metric_result,
                                      is_rif = x$is_rif) {
  probability <- if (identical(basis, "adverse_10")) 0.10 else if (identical(basis, "adverse_5")) 0.20 else 0.05
  support <- metric_result$adverse_support
  support <- if (is.data.frame(support) && nrow(support)) support[support$scenario == scenario, , drop = FALSE] else data.frame()
  selected_member_ids <- unique(as.character(support$model_id))
  rows <- x$channel_summary[as.character(x$channel_summary$scenario) == scenario &
    as.character(x$channel_summary$member) %in% selected_member_ids, , drop = FALSE]
  values <- .compact_supported_values(rows, support, probability)
  if (is.null(values)) return(data.frame())
  out <- as.data.frame(as.list(values), stringsAsFactors = FALSE)
  out$delta_res1 <- values$delta_res1
  out$weight <- 1
  out
}

.compact_future_combine <- function(rows, prefix = "") {
  if (is.null(rows) || !nrow(rows)) {
    return(setNames(numeric(0), character(0)))
  }
  out <- setNames(numeric(length(.compact_decomp_channels)), .compact_decomp_channels)
  for (name in .compact_decomp_channels) {
    sums <- rows[[paste0(prefix, "sum_", name)]]
    weights <- rows[[paste0(prefix, "weight_", name)]]
    ok <- is.finite(sums) & is.finite(weights) & weights > 0
    out[[name]] <- if (any(ok)) sum(sums[ok]) / sum(weights[ok]) else NA_real_
  }
  out
}

.compact_future_as_decomp <- function(rows, is_rif = FALSE) {
  if (is.null(rows) || !nrow(rows)) {
    return(data.frame())
  }
  values <- .compact_future_combine(rows)
  out <- as.data.frame(as.list(values), stringsAsFactors = FALSE)
  out$weight <- 1
  out$delta_res1 <- values[["delta_res1"]]
  out
}

.compact_future_summary <- function(x, scenario, basis = "mean", so = NULL,
                                    is_rif = x$is_rif, metric_result = NULL) {
  if (!identical(basis, "mean")) {
    probability <- if (identical(basis, "adverse_10")) 0.10 else if (identical(basis, "adverse_5")) 0.20 else 0.05
    support <- metric_result$adverse_support
    support <- if (is.data.frame(support) && nrow(support)) support[support$scenario == scenario, , drop = FALSE] else data.frame()
    rows <- x$channel_summary[as.character(x$channel_summary$scenario) == scenario &
      as.character(x$channel_summary$member) %in% unique(as.character(support$model_id)), , drop = FALSE]
    if (!nrow(rows)) return(decomposition_summary_data(NULL, is_rif))
    values <- .compact_supported_values(rows, support, probability)
    if (is.null(values)) return(decomposition_summary_data(NULL, is_rif))
    out <- as.data.frame(as.list(values), stringsAsFactors = FALSE)
    out$delta_res1 <- values$delta_res1
    out$weight <- 1
    return(decomposition_summary_data(out, is_rif = is_rif))
  }
  rows <- .compact_future_year(x, scenario, basis, so)
  if (!nrow(rows)) return(decomposition_summary_data(NULL, is_rif))
  decomposition_summary_data(
    .compact_future_as_decomp(rows, is_rif = is_rif),
    is_rif = is_rif
  )
}

.compact_future_decile_summary <- function(x, scenario, basis = "mean",
                                           so = NULL, is_rif = x$is_rif) {
  rows <- x$decile_summary[
    as.character(x$decile_summary$scenario) == as.character(scenario), ,
    drop = FALSE
  ]
  selected <- .compact_future_year(x, scenario, basis, so)
  if (nrow(selected) && !identical(basis, "mean")) {
    keep <- rows$sim_year %in% selected$sim_year
    if ("member" %in% names(rows) && "member" %in% names(selected)) {
      keep <- paste(rows$member, rows$sim_year) %in% paste(selected$member, selected$sim_year)
    }
    rows <- rows[keep, , drop = FALSE]
  }
  if (!nrow(rows)) {
    return(tibble::tibble())
  }
  if (!identical(basis, "mean")) {
    selected_years <- .compact_future_year(x, scenario, basis, so)
    if (nrow(selected_years)) {
      if ("member" %in% names(rows) && "member" %in% names(selected_years)) {
        rows <- rows[paste(rows$member, rows$sim_year) %in%
          paste(selected_years$member, selected_years$sim_year), , drop = FALSE]
      } else {
        rows <- rows[rows$sim_year %in% selected_years$sim_year, , drop = FALSE]
      }
    }
    if (!nrow(rows)) return(tibble::tibble())
  }
  out <- dplyr::bind_rows(lapply(sort(unique(rows$decile)), function(d) {
    group <- rows[rows$decile == d, , drop = FALSE]
    values <- .compact_future_combine(group)
    tibble::tibble(
      decile = as.integer(d),
      level_log = values[["delta_main"]],
      resilience_log = values[["delta_res"]],
      total_log = values[["delta_total"]],
      main = values[["delta_main"]],
      repositioning = values[["delta_res1"]],
      interaction = values[["delta_res2"]],
      total = values[["delta_total"]],
      level_percent = log_effect_to_percent(values[["delta_main"]]),
      resilience_percent = log_effect_to_percent(values[["delta_res"]]),
      main_percent = log_effect_to_percent(values[["delta_main"]]),
      cash_transfer_percent = log_effect_to_percent(values[["delta_sp"]]),
      covariate_shift_percent = log_effect_to_percent(values[["delta_main_covar"]]),
      repositioning_percent = log_effect_to_percent(values[["delta_res1"]]),
      interaction_percent = log_effect_to_percent(values[["delta_res2"]]),
      total_percent = log_effect_to_percent(values[["delta_total"]]),
      n_households = sum(group$n_households),
      weighted_population = sum(group$weighted_population)
    )
  }))
  if ("baseline_annual" %in% names(rows)) {
    baseline <- rows |>
      dplyr::group_by(decile) |>
      dplyr::summarise(baseline_annual = .weighted_mean_safe(
        baseline_annual, weight_baseline_y_point), .groups = "drop")
    out <- dplyr::left_join(out, baseline, by = "decile")
  }
  out
}

.prepare_decomp_adverse_bases <- function(weather_raw, hist_sim, so,
                                          metric_support = NULL) {
  raw <- step2_resolve_weather(weather_raw, hist_sim)
  if (is.null(raw) || !nrow(raw)) {
    return(list())
  }
  out <- list(mean = raw)
  if (!"timestamp" %in% names(raw) || is.null(hist_sim$pipeline) ||
    is.null(hist_sim$pipeline$y_point) || is.null(hist_sim$pipeline$sim_year)) {
    return(out)
  }
  years <- as.integer(format(raw$timestamp, "%Y"))
  year_values <- split(raw, years)
  pipe <- hist_sim$pipeline
  simulated_years <- split(seq_along(pipe$y_point), pipe$sim_year)
  annual_values <- vapply(simulated_years, function(idx) {
    vals <- as.numeric(pipe$y_point[idx])
    weights <- if (!is.null(pipe$weight)) as.numeric(pipe$weight[idx]) else NULL
    if (is.null(weights)) {
      mean(vals, na.rm = TRUE)
    } else {
      stats::weighted.mean(vals, weights, na.rm = TRUE)
    }
  }, numeric(1L))
  agg <- annual_values[names(annual_values) %in% names(year_values)]
  if (!length(agg)) {
    return(out)
  }
  probabilities <- c(adverse_5 = 0.20, adverse_10 = 0.10, adverse_20 = 0.05)
  for (basis in names(probabilities)) {
    probability <- probabilities[[basis]]
    support <- if (is.data.frame(metric_support) && nrow(metric_support)) {
      metric_support[metric_support$probability == probability & metric_support$status == "ok", , drop = FALSE]
    } else data.frame()
    if (nrow(support)) {
      years <- c(support$year_lo, support$year_hi)
      years <- unique(as.character(years[is.finite(suppressWarnings(as.numeric(years)))]))
      panels <- year_values[intersect(years, names(year_values))]
      if (length(panels)) out[[basis]] <- do.call(rbind, panels)
    }
  }
  out
}

.policy_metric_export_annotate <- function(data, result, scenario = NULL,
                                           export_scope = "production_prediction_rows",
                                           scale = "metric_aware", so = NULL,
                                           analysis_unit = NULL, status = NULL,
                                           reason = NULL) {
  if (is.null(data) || !is.data.frame(data)) data <- data.frame()
  metadata <- result$metadata %||% list()
  status <- status %||% result$status %||% "unavailable"
  reason <- reason %||% if (identical(status, "ok")) "" else result$reason
  if (!nrow(data)) {
    data <- data.frame(status = status, availability = status,
      reason = reason %||% "No rows are available for this export.",
      stringsAsFactors = FALSE)
  }
  n <- nrow(data)
  scalar <- function(x, fallback = NA) {
    if (is.null(x) || !length(x)) return(fallback)
    if (is.list(x) && !is.data.frame(x)) x <- unlist(x)
    if (!length(x) || all(is.na(x))) return(fallback)
    if (length(x) > 1L) return(paste(as.character(x), collapse = "; "))
    x[[1L]]
  }
  add <- function(name, value, fallback = NA_character_) {
    if (!name %in% names(data)) {
      data[[name]] <<- rep(scalar(value, fallback), n)
    }
  }
  # Retain row-level keys/status where present; add the immutable run snapshot
  # as scalar columns so CSVs remain interpretable outside the Shiny session.
  if (!"scenario" %in% names(data)) data$scenario <- rep(scalar(scenario), n)
  add("outcome_name", metadata$outcome_name)
  add("outcome_label", metadata$outcome_label)
  add("outcome_type", metadata$outcome_type %||% .metric_context_value(so, "type"))
  add("outcome_transform", metadata$outcome_transform %||% .metric_context_value(so, "transform"))
  add("metric_id", metadata$method)
  add("metric_label", metadata$label)
  add("native_unit", metadata$native_unit)
  add("level_unit", metadata$level_unit)
  add("change_unit", metadata$change_unit)
  add("display_multiplier", metadata$display_multiplier, NA_real_)
  add("number_format", metadata$format)
  add("threshold_kind", metadata$threshold_kind)
  add("threshold_value", metadata$threshold_value, NA_real_)
  add("threshold_unit", metadata$threshold_unit)
  add("currency_basis", metadata$currency_basis)
  add("time_basis", metadata$time_basis)
  add("welfare_denominator", metadata$welfare_denominator)
  add("missing_context_note", metadata$missing_context)
  add("analysis_unit", metadata$analysis_unit %||% analysis_unit)
  add("weight_interpretation", metadata$weight_interpretation)
  add("run_identity", metadata$run_identity)
  add("focus_scenario", metadata$focus_scenario)
  add("population_scope", metadata$population_scope)
  add("eligibility_caveat", metadata$eligibility_caveat)
  add("export_scope", export_scope)
  add("exposure_source", metadata$exposure_source)
  add("exposure_mapping_id", metadata$exposure_source)
  add("correction_version", metadata$correction_version)
  add("requested_residuals", metadata$requested_residuals)
  add("effective_residuals", metadata$effective_residuals)
  add("component_order", metadata$component_order)
  add("center_method", metadata$center_method %||% "equal_model_mean")
  add("scale", scale)
  add("uncertainty_status", metadata$uncertainty %||% "central_only")
  add("parity_tolerance", metadata$parity_tolerance, NA_real_)
  add("mixed_effective_residual_modes", metadata$mixed_effective_residuals, NA)
  add("component_method", "ordered_cumulative_state_differences")
  add("repositioning_status", if (isTRUE(metadata$repositioning_modeled)) "modeled" else "not_modeled_by_engine")
  add("interaction_status", if (isTRUE(metadata$interaction_included)) "included_in_fitted_model" else "not_included_in_fitted_model")
  if (!"availability" %in% names(data)) {
    data$availability <- if ("status" %in% names(data)) {
      as.character(data$status)
    } else rep(scalar(status, "unavailable"), n)
  }
  add("reason", reason %||% "")
  if (!isTRUE(metadata$repositioning_modeled) && "repositioning" %in% names(data)) {
    data$repositioning <- NA_real_
  }
  if (!isTRUE(metadata$interaction_included) && "interaction" %in% names(data)) {
    data$interaction <- NA_real_
  }
  if (!isTRUE(metadata$repositioning_modeled) && !isTRUE(metadata$interaction_included) &&
      "resilience" %in% names(data)) {
    data$resilience <- NA_real_
  }
  data
}

.policy_metric_export_context_fields <- c(
  "outcome_name", "outcome_label", "outcome_type", "outcome_transform",
  "metric_id", "metric_label", "native_unit", "level_unit", "change_unit",
  "display_multiplier", "number_format", "threshold_kind", "threshold_value",
  "threshold_unit", "currency_basis", "time_basis", "welfare_denominator",
  "missing_context_note", "analysis_unit", "weight_interpretation", "run_identity",
  "population_scope", "eligibility_caveat", "parity_tolerance",
  "mixed_effective_residual_modes",
  "focus_scenario", "export_scope", "exposure_source", "exposure_mapping_id",
  "correction_version", "requested_residuals", "effective_residuals",
  "component_order", "center_method", "scale", "uncertainty_status",
  "component_method"
)

#' 3_09_decomposition UI Function
#'
#' @description A shiny Module. Renders the policy effect decomposition
#'   visualizations: stacked bar chart by decile, beta curve (RIF only),
#'   and summary table.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_09_decomposition_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("stale_banner_ui")),
    shiny::uiOutput(ns("policy_summary_ui")),
    shiny::h4("What drives the total policy effect?",
      class = "diagnostic-section-heading"),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::div(
        style = "display:flex; justify-content:flex-end; flex-wrap:wrap; gap:14px; margin-bottom:12px;",
        shiny::uiOutput(ns("headline_weather_basis_ui")),
        shiny::uiOutput(ns("headline_scenario_ui"))
      ),
      wise_chart_output(ns("headline_decomp_plot"),
        "Weighted average main effect, repositioning, interaction, and total policy effect",
        height = "360px"),
      shiny::uiOutput(ns("headline_decomp_note_ui")),
      shiny::uiOutput(ns("interaction_warning_ui")),
      shiny::tags$details(
        class = "decomposition-data-details",
        shiny::tags$summary("View decomposition data"),
        shiny::div(class = "wise-reactable-controls",
          shiny::uiOutput(ns("headline_decomp_csv_ui"))),
        DT::DTOutput(ns("headline_decomp_table"))
      )
    ),
    shiny::h4("Who gains, and through which channel?",
      class = "diagnostic-section-heading"),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::div(
        style = "display:flex; justify-content:flex-end; flex-wrap:wrap; gap:14px; margin:16px 0 12px;",
        shiny::uiOutput(ns("decile_weather_basis_ui")),
        shiny::uiOutput(ns("decile_scenario_ui"))
      ),
      wise_chart_output(ns("decomp_bar_plot"),
        "Weighted average policy effect by baseline welfare decile", height = "450px"),
      shiny::uiOutput(ns("decomp_bar_note_ui")),
      shiny::tags$details(
        class = "decomposition-data-details",
        shiny::tags$summary("View decomposition data"),
        shiny::div(class = "wise-reactable-controls",
          shiny::uiOutput(ns("decile_decomp_csv_ui"))),
        DT::DTOutput(ns("decile_decomp_table"))
      ),
      shiny::tags$p(class = "diagnostic-note",
        "Effects use the selected outcome's registry mean units. Deciles are fixed from weighted baseline welfare; adverse-year choices select a historical weather year. Component, residual, and survey-sampling uncertainty are not shown.")
    ),
    shiny::uiOutput(ns("beta_curve_ui"))
  )
}

#' 3_09_decomposition Server Functions
#'
#' @param id Module id.
#' @param decomp_result Reactive data frame from decompose_policy_effect().
#' @param decomp_scenarios Reactive compact future decomposition payload.
#' @param model_fit Reactive model fit list (for rif_grid / engine detection).
#' @param so Reactive selected outcome metadata.
#' @param selected_policies Reactive selected policy scenario keys.
#' @param baseline_hist_sim Reactive Step 2-style baseline simulation result.
#' @param baseline_svy      Reactive baseline survey used for fixed deciles.
#' @param policy_svy        Reactive realized policy survey.
#' @param selected_weather Reactive selected weather specification.
#' @param policy_saved_scenarios Reactive named future scenario list.
#'
#' @noRd
mod_3_09_decomposition_server <- function(id,
                                          decomp_result = reactive(NULL),
                                          decomp_scenarios = reactive(list()),
                                          decomp_context = reactive(NULL),
                                          model_fit = reactive(NULL),
                                          variable_list = reactive(NULL),
                                          so = reactive(NULL),
                                          show_coef_uncertainty = reactive(TRUE),
                                          selected_policies = reactive(NULL),
                                          policy_scenarios = reactive(list()),
                                          baseline_hist_sim = reactive(NULL),
                                          baseline_svy = reactive(NULL),
                                          policy_svy = reactive(NULL),
                                          selected_weather = reactive(NULL),
                                          sp_scenario = reactive(NULL),
                                          infra_scenario = reactive(NULL),
                                          digital_scenario = reactive(NULL),
                                          labor_scenario = reactive(NULL),
                                          education_scenario = reactive(NULL),
                                           policy_saved_scenarios = reactive(list()),
                                           stale = reactive(FALSE),
                                           aggregation_method = reactive("mean"),
                                           poverty_line = reactive(NULL),
                                           analysis_unit = reactive(NULL),
                                           metric_decomposition = reactive(NULL),
                                           focus_scenario = reactive(NULL),
                                           metric_context = reactive(NULL),
                                           metric_adverse_support = reactive(NULL),
                                           metric_adverse_by_model = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    raw_decomp_result <- decomp_result
    raw_decomp_scenarios <- decomp_scenarios
    policy_method_status <- reactive({
      mf <- model_fit()
      .policy_endpoint_status(so(), decomp_context() %||% mf)
    })
    decomp_result <- reactive({
      if (!identical(policy_method_status()$status, "ok")) return(NULL)
      raw_decomp_result()
    })
    decomp_scenarios <- reactive({
      if (!identical(policy_method_status()$status, "ok")) return(list())
      raw_decomp_scenarios()
    })
    ns <- session$ns
    session$userData$wise_step3_stale <- stale
    output$stale_banner_ui <- shiny::renderUI({
      if (isTRUE(stale())) {
        .stale_banner(
          "Step 3 policy decomposition",
          note = NULL
        )
      } else {
        NULL
      }
    })

    is_rif <- reactive({
      mf <- model_fit()
      !is.null(mf) && identical(tolower(as.character(mf$engine %||% "")), "rif")
    })
    baseline_deciles <- reactive({
      ctx <- decomp_context()
      if (is.null(ctx)) {
        NULL
      } else if (exists(
        ".decomposition_context_baseline_deciles",
        mode = "function"
      )) {
        .decomposition_context_baseline_deciles(ctx)
      } else {
        ctx$baseline_deciles
      }
    })

    output$policy_summary_ui <- shiny::renderUI({
      policy_summary_card(
        selected_policies = selected_policies(),
        baseline_hist_sim = baseline_hist_sim(),
        selected_weather = selected_weather(),
        sp_scenario = sp_scenario(),
        infra_scenario = infra_scenario(),
        digital_scenario = digital_scenario(),
        labor_scenario = labor_scenario(),
        education_scenario = education_scenario(),
        policy_saved_scenarios = policy_saved_scenarios(),
        policy_scenarios = policy_scenarios()
      )
    })

    metric_scenario <- reactive({
      result <- metric_decomposition()
      scenarios <- names(result$scenarios %||% list())
      selected <- input$metric_scenario
      if (!is.null(selected) && selected %in% scenarios) return(selected)
      preferred <- focus_scenario() %||% result$metadata$focus_scenario
      if (!is.null(preferred) && preferred %in% scenarios) return(preferred)
      if (length(scenarios)) scenarios[[1L]] else NULL
    })
    selected_metric_result <- reactive({
      result <- metric_decomposition()
      scenario <- metric_scenario()
      if (is.null(scenario) || !is.list(result$scenarios[[scenario]])) return(NULL)
      result$scenarios[[scenario]]
    })
    metric_meta <- reactive({
      result <- metric_decomposition()
      if (is.list(result$metadata) && length(result$metadata)) result$metadata else metric_context()
    })
    metric_status_text <- function(selected, result = metric_decomposition(), tail = FALSE) {
      if (isTRUE(stale())) {
        return("Policy run is stale; metric-aware results are withheld.")
      }
      if (is.null(selected) || (!tail && !identical(selected$status, "ok"))) {
        return(selected$reason %||% result$reason %||% "Metric-aware channel attribution is unavailable.")
      }
      NULL
    }
    metric_display_status <- function(selected) {
      if (isTRUE(stale())) "unavailable" else selected$status %||% "unavailable"
    }
    metric_export_result <- function() {
      result <- metric_decomposition()
      if (!isTRUE(stale())) return(result)
      result$status <- "unavailable"
      result$reason <- "Policy run is stale; metric-aware exports are withheld."
      result$annual <- result$summary <- result$return_period <- data.frame()
      result$adverse_support <- result$adverse_by_model <- data.frame()
      result$endpoint_summary <- data.frame()
      result$scenarios <- list()
      result$mechanisms <- list()
      result
    }
    output$metric_scope_ui <- shiny::renderUI({
      result <- metric_decomposition()
      selected <- selected_metric_result()
      reason <- metric_status_text(selected, result, tail = TRUE)
      if (!is.null(reason)) {
        return(shiny::div(class = "alert alert-warning", role = "status", reason))
      }
      metadata <- metric_meta() %||% list()
      scenario <- metric_scenario() %||% "Selected scenario"
      row <- selected$summary[1L, , drop = FALSE]
      same_focus <- identical(scenario, result$metadata$focus_scenario %||% focus_scenario())
      shiny::tagList(
        shiny::div(class = "selection-card-pill", paste(
          metadata$outcome_label %||% metadata$outcome_name %||% "Selected outcome",
          metadata$label %||% metadata$method %||% "Selected metric",
          paste0("levels: ", metadata$level_unit %||% "native units"),
          paste0("changes: ", metadata$change_unit %||% "native units"),
          sep = " · "
        )),
        shiny::p(class = "diagnostic-note", metric_context_note(metadata)),
        shiny::p(class = "diagnostic-note", paste0(
          "Scenario: ", scenario,
          if (!same_focus) " · Different scenario from Results headline" else " · Results focus scenario",
          " · ", metadata$population_scope %||% "Fixed survey population",
          " · ", metadata$weight_interpretation %||% "Canonical annual metric aggregation",
          "; years averaged within model then climate models weighted equally",
          " · row-aligned annual weather correction (", metadata$correction_version %||% "unavailable", ")",
          " · ", metadata$component_order %||% "main -> repositioning -> interaction",
          " · central estimates only"
        )),
        if ("n_dropped_model_years" %in% names(row) && is.finite(row$n_dropped_model_years[[1L]]) && row$n_dropped_model_years[[1L]] > 0L) {
          shiny::p(class = "diagnostic-note", paste(row$n_dropped_model_years[[1L]],
            "matched model/year cells dropped from shared support."))
        }
      )
    })
    metric_contribution_data <- reactive({
      selected <- selected_metric_result()
      reason <- metric_status_text(selected)
      if (!is.null(reason)) return(.policy_metric_export_annotate(data.frame(),
        metric_decomposition(), metric_scenario(), export_scope = "expected_endpoint_equal_model_mean",
        so = so(), analysis_unit = analysis_unit(), status = metric_display_status(selected),
        reason = reason))
      metadata <- metric_meta() %||% list()
      row <- selected$summary[1L, , drop = FALSE]
      result <- metric_decomposition()
      repositioning_modeled <- isTRUE(result$metadata$repositioning_modeled)
      interaction_included <- isTRUE(result$metadata$interaction_included)
      value <- function(field, change = FALSE) {
        format_metric_value(row[[field]][[1L]], metadata, change = change)
      }
      out <- data.frame(
        `Cumulative state / contribution` = c("Baseline", "After main effect", "After repositioning", "Policy", "Main effect", "Repositioning", "Weather-policy interaction", "Resilience subtotal", "Total policy effect"),
        `Selected metric value` = c(value("baseline"), value("after_main"), value("after_repositioning"), value("policy"), value("main", TRUE), if (repositioning_modeled) value("repositioning", TRUE) else "Not modeled by this engine", if (interaction_included) value("interaction", TRUE) else "Not included in fitted model", if (repositioning_modeled || interaction_included) value("resilience", TRUE) else "Unavailable", value("total", TRUE)),
        `Native numeric value` = c(row$baseline, row$after_main, row$after_repositioning, row$policy, row$main, if (repositioning_modeled) row$repositioning else NA_real_, if (interaction_included) row$interaction else NA_real_, if (repositioning_modeled || interaction_included) row$resilience else NA_real_, row$total),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      out$native_field <- c("baseline", "after_main", "after_repositioning", "policy",
        "main", "repositioning", "interaction", "resilience", "total")
      out$scenario <- metric_scenario()
      out$center_method <- row$center_method[[1L]] %||% "equal_model_mean"
      out$n_models <- row$n_models[[1L]]
      out$n_model_years <- row$n_model_years[[1L]]
      out$n_dropped_model_years <- row$n_dropped_model_years[[1L]]
      out$scope <- row$scope[[1L]] %||% "production_prediction_rows"
      out <- .policy_metric_export_annotate(out, result, metric_scenario(),
        export_scope = "expected_endpoint_equal_model_mean", so = so(), analysis_unit = analysis_unit(),
        status = selected$status, reason = selected$reason)
      out
    })
    output$metric_contribution_table <- reactable::renderReactable({
      tbl <- metric_contribution_data()
      context_columns <- setdiff(names(tbl), c("Cumulative state / contribution",
        "Selected metric value", "Native numeric value", "native_field", "scenario",
        "center_method", "n_models", "n_model_years", "n_dropped_model_years",
        "scope", "availability", "reason"))
      columns <- if (all(c("Cumulative state / contribution", "Selected metric value", "Native numeric value") %in% names(tbl))) {
        c(list(
          `Cumulative state / contribution` = reactable::colDef(show = TRUE, minWidth = 230),
          `Selected metric value` = reactable::colDef(show = TRUE, minWidth = 180),
          `Native numeric value` = reactable::colDef(show = TRUE,
            format = reactable::colFormat(digits = 6)),
          native_field = reactable::colDef(show = FALSE),
          scenario = reactable::colDef(show = FALSE),
          center_method = reactable::colDef(show = FALSE),
          n_models = reactable::colDef(show = FALSE),
          n_model_years = reactable::colDef(show = FALSE),
          n_dropped_model_years = reactable::colDef(show = FALSE),
          scope = reactable::colDef(show = FALSE),
          availability = reactable::colDef(show = FALSE),
          reason = reactable::colDef(show = FALSE)
        ), stats::setNames(rep(list(reactable::colDef(show = FALSE)), length(context_columns)),
          context_columns))
      } else stats::setNames(lapply(names(tbl), function(name) {
        reactable::colDef(show = name %in% c("status", "availability", "reason"),
          class = "wise-dt-wrap", minWidth = 130)
      }), names(tbl))
      reactable::reactable(tbl, compact = TRUE, searchable = FALSE, pagination = FALSE,
        defaultColDef = reactable::colDef(show = FALSE), highlight = TRUE, rowStyle = function(index) {
          if (index == 8L) list(background = "#e8f3f5", fontWeight = "700") else NULL
        }, columns = columns)
    })
    output$metric_contribution_unit_ui <- shiny::renderUI({
      metadata <- metric_meta() %||% list()
      shiny::tags$p(class = "diagnostic-note", paste(
        "Displayed levels:", metadata$level_unit %||% "native outcome units",
        "· displayed changes:", metadata$change_unit %||% "native units",
        "· numeric values remain in native metric units in the table/export."
      ))
    })
    metric_tail_data <- reactive({
      selected <- selected_metric_result()
      result <- metric_decomposition()
      reason <- metric_status_text(selected, result, tail = TRUE)
      scenario <- metric_scenario()
      if (length(scenario) != 1L || is.na(scenario) || !nzchar(scenario)) scenario <- "Unavailable scenario"
      tails <- if (!isTRUE(stale())) result$return_period else NULL
      if (!is.data.frame(tails)) tails <- data.frame()
      tails <- tails[tails$scope == "baseline_anchored" & tails$scenario == scenario, , drop = FALSE]
      if (!nrow(tails)) {
        unavailable_reason <- reason
        if (length(unavailable_reason) != 1L || is.na(unavailable_reason) || !nzchar(unavailable_reason)) {
          unavailable_reason <- "Baseline-anchored adverse support unavailable."
        }
        tails <- data.frame(scenario = scenario, return_period = NA_real_, scope = "baseline_anchored",
          status = "unavailable", reason = unavailable_reason,
          baseline = NA_real_, after_main = NA_real_, after_repositioning = NA_real_, policy = NA_real_,
          main = NA_real_, repositioning = NA_real_, interaction = NA_real_, resilience = NA_real_, total = NA_real_,
          probability = NA_real_, center_method = "", quantile_method = "rank_interp_n_p_plus_half",
          n_models = NA_integer_, n_model_years = NA_integer_, adverse_basis = "baseline_selected_metric",
          year_lo = NA_real_, year_hi = NA_real_, rank_lo = NA_integer_, rank_hi = NA_integer_,
          weight_lo = NA_real_, weight_hi = NA_real_, stringsAsFactors = FALSE)
      }
      tails$return_period_label <- ifelse(is.finite(tails$return_period),
        paste0("Adverse 1-in-", format(tails$return_period, trim = TRUE), " year"),
        "Adverse quantile unavailable")
      meta <- metric_meta() %||% list()
      repositioning_modeled <- identical(result$mechanisms$repositioning_status, "modeled")
      interaction_included <- identical(result$mechanisms$interaction_status, "included")
      display <- function(field) {
        values <- if (field %in% names(tails)) tails[[field]] else rep(NA_real_, nrow(tails))
        vapply(values, format_metric_value, character(1), metadata = meta, change = TRUE)
      }
      tails$main_display <- display("main")
      tails$repositioning_display <- if (repositioning_modeled) display("repositioning") else "Not modeled by this engine"
      tails$interaction_display <- if (interaction_included) display("interaction") else "Not included in fitted model"
      tails$resilience_display <- if (repositioning_modeled || interaction_included) {
        display("resilience")
      } else "Unavailable"
      tails$total_display <- display("total")
      tails$scope_label <- rep("Baseline-anchored adverse quantile", nrow(tails))
      tails$availability <- ifelse(tails$status == "ok", "Available", tails$reason)
      for (field in c("baseline", "after_main", "after_repositioning", "policy",
                      "main", "repositioning", "interaction", "resilience", "total")) {
        tails[[paste0(field, "_native")]] <- if (field %in% names(tails)) tails[[field]] else NA_real_
      }
      if (!repositioning_modeled) tails$repositioning_native <- NA_real_
      if (!interaction_included) tails$interaction_native <- NA_real_
      if (!repositioning_modeled && !interaction_included) tails$resilience_native <- NA_real_
      for (field in c("center_method", "quantile_method")) {
        if (!field %in% names(tails)) tails[[field]] <- ""
      }
      out <- tails[, c("return_period_label", "scope_label", "main_display",
        "repositioning_display", "interaction_display", "resilience_display",
        "total_display", intersect(c("n_models", "n_model_years"), names(tails)),
        intersect(c("scenario", "member", "model_id", "sim_year", "adverse_basis", "year_lo", "year_hi", "rank_lo", "rank_hi", "weight_lo", "weight_hi"), names(tails)),
        "baseline_native", "after_main_native", "after_repositioning_native", "policy_native",
        "main_native", "repositioning_native", "interaction_native", "resilience_native",
        "total_native", "probability", "center_method", "quantile_method", "availability"), drop = FALSE]
      names(out) <- c("Return period", "Scope", "Main", "Repositioning", "Interaction",
        "Resilience", "Total", if ("n_models" %in% names(out)) "Models", if ("n_model_years" %in% names(out)) "Model-years",
        intersect(c("scenario", "member", "model_id", "sim_year", "adverse_basis", "year_lo", "year_hi", "rank_lo", "rank_hi", "weight_lo", "weight_hi"), names(tails)),
        "Baseline native", "After main native", "After repositioning native", "Policy native",
        "Main native", "Repositioning native", "Interaction native", "Resilience native",
        "Total native", "Probability", "Center method", "Quantile method", "Availability")
      out$scope_identifier <- tails$scope
      out$adverse_basis <- tails$adverse_basis
      out$year_lo <- tails$year_lo
      out$year_hi <- tails$year_hi
      out$rank_lo <- tails$rank_lo
      out$rank_hi <- tails$rank_hi
      out$weight_lo <- tails$weight_lo
      out$weight_hi <- tails$weight_hi
      out$status <- tails$status
      out$reason <- tails$reason
      .policy_metric_export_annotate(out, metric_decomposition(), metric_scenario(),
        export_scope = "baseline_anchored_adverse_quantiles", so = so(),
        analysis_unit = analysis_unit(), status = if (nrow(tails)) tails$status[[1L]] else "unavailable",
        reason = if (nrow(tails)) tails$reason[[1L]] else "Baseline-anchored adverse support is unavailable.")
    })
    output$metric_tail_table <- reactable::renderReactable({
      tbl <- metric_tail_data()
      visible <- c("Return period", "Scope", "Main", "Repositioning", "Interaction",
        "Resilience", "Total", "Models", "Model-years")
      if ("status" %in% names(tbl)) visible <- c(visible, "Availability", "reason")
      if (!"Return period" %in% names(tbl)) visible <- c(visible, "status", "availability", "reason")
      reactable::reactable(tbl, compact = TRUE, searchable = FALSE, defaultPageSize = 8,
        defaultColDef = reactable::colDef(show = FALSE), highlight = TRUE,
        columns = stats::setNames(lapply(names(tbl), function(name) {
          if (is.numeric(tbl[[name]])) reactable::colDef(show = name %in% visible,
            format = reactable::colFormat(digits = 2))
          else reactable::colDef(show = name %in% visible,
            class = "wise-dt-wrap", minWidth = 130)
        }), names(tbl)))
    })
    output$metric_mechanism_status_ui <- shiny::renderUI({
      selected <- selected_metric_result()
      result <- metric_decomposition()
      reason <- metric_status_text(selected, result)
      if (!is.null(reason)) return(shiny::div(class = "alert alert-warning", reason))
      shiny::tags$p(class = "diagnostic-note", paste(
        paste("Repositioning:", result$mechanisms$repositioning_status %||% "Unavailable"),
        paste("Interaction:", result$mechanisms$interaction_status %||% "Unavailable"),
        result$mechanisms$metadata$rank_convention %||% "Rank convention unavailable.",
        paste("Scope:", result$mechanisms$metadata$scope %||% "production prediction rows"),
        "Continuous changes are channel-implied model-outcome units per fitted weather-input unit; binned weather values are category-versus-reference contrasts, not per-unit slopes. Neither is a complete model derivative."
      ))
    })
    metric_mechanism_data <- reactive({
      selected <- selected_metric_result()
      result <- metric_decomposition()
      reason <- metric_status_text(selected, result)
      tbl <- if (is.null(reason)) result$mechanisms$summary else NULL
      if (!is.data.frame(tbl) || !nrow(tbl)) return(.policy_metric_export_annotate(data.frame(),
        result, metric_scenario(), scale = "model_scale", so = so(),
        analysis_unit = analysis_unit(), status = metric_display_status(selected),
        reason = reason %||% "Sensitivity mechanism values are not available for this run."))
      scenario <- metric_scenario()
      tbl <- tbl[tbl$scenario == scenario, , drop = FALSE]
      if (!nrow(tbl)) return(.policy_metric_export_annotate(data.frame(), result, scenario,
        scale = "model_scale", so = so(), analysis_unit = analysis_unit(),
        status = "unavailable", reason = "No weather-sensitivity rows are available for the selected scenario."))
      tbl$rank_movement <- ifelse(is.finite(tbl$tau_pre) & is.finite(tbl$tau_post),
        paste0(formatC(tbl$tau_pre, digits = 3, format = "f"), " -> ",
          formatC(tbl$tau_post, digits = 3, format = "f")), "Not modeled")
      out <- tbl[, c("scenario", intersect(c("member", "model_id", "sim_year"), names(tbl)),
        "hazard", "category", "contrast", "repositioning", "interaction",
        "positive_repositioning_share", "negative_repositioning_share",
        "positive_interaction_share", "negative_interaction_share", "tau_pre", "tau_post", "rank_movement",
        "model_units", "weather_units", "n_models"), drop = FALSE]
      out$scale <- "model_scale"
      out$uncertainty_status <- "central_only"
      result <- metric_decomposition()
      out <- .policy_metric_export_annotate(out, result, scenario,
        export_scope = "production_prediction_rows", scale = "model_scale",
        so = so(), analysis_unit = analysis_unit())
      out$rank_convention <- result$mechanisms$metadata$rank_convention %||% "fixed main-derived pre/post ranks"
      out$included_terms <- result$mechanisms$metadata$included_terms %||% "repositioning and interaction channel changes only"
      out$excluded_terms <- result$mechanisms$metadata$excluded_terms %||% "not a complete fitted-model derivative"
      out$availability <- "ok"
      out
    })

    metric_export_scenario_result <- function(result, scenario, field) {
      selected <- result$scenarios[[scenario]]
      if (is.null(selected) || !identical(selected$status, "ok")) {
        return(.policy_metric_export_annotate(data.frame(), result, scenario,
          export_scope = paste0("scenario_", field), so = so(), analysis_unit = analysis_unit(),
          status = selected$status %||% "unavailable",
          reason = selected$reason %||% result$reason))
      }
      data <- selected[[field]]
      if (identical(field, "annual") && nrow(data)) data$center_method <- "annual_model_year"
      .policy_metric_export_annotate(data, result, scenario,
        export_scope = paste0("scenario_", field), so = so(), analysis_unit = analysis_unit(),
        status = selected$status, reason = selected$reason)
    }
    metric_expected_export <- function() {
      result <- metric_export_result()
      scenario <- metric_scenario()
      selected <- result$scenarios[[scenario]]
      if (is.null(selected) || !identical(selected$status, "ok") ||
          !is.data.frame(selected$summary) || !nrow(selected$summary)) {
        endpoint <- result$endpoint_summary
        if (!is.data.frame(endpoint) || !nrow(endpoint)) {
          return(.policy_metric_export_annotate(data.frame(), result, scenario,
            export_scope = "expected_endpoint_equal_model_mean", so = so(), analysis_unit = analysis_unit(),
            status = selected$status %||% result$status,
            reason = selected$reason %||% result$reason))
        }
        endpoint <- endpoint[endpoint$scenario == scenario, , drop = FALSE]
        if (nrow(endpoint)) {
          endpoint$availability <- "endpoint_summary_available_channels_unavailable"
          endpoint$reason <- selected$reason %||% result$reason %||% "Channel attribution unavailable."
        }
        return(.policy_metric_export_annotate(endpoint, result, scenario,
          export_scope = "expected_endpoint_equal_model_mean_channels_unavailable",
          so = so(), analysis_unit = analysis_unit(), status = "endpoint_available_channels_unavailable",
          reason = selected$reason %||% result$reason))
      }
      expected <- selected$summary
      expected$center_method <- "equal_model_mean"
      .policy_metric_export_annotate(expected, result, scenario,
        export_scope = "expected_endpoint_equal_model_mean", so = so(), analysis_unit = analysis_unit(),
        status = selected$status, reason = selected$reason)
    }
    metric_annual_export <- function() {
      result <- metric_export_result()
      scenario <- metric_scenario()
      metric_export_scenario_result(result, scenario, "annual")
    }
    metric_tail_export <- function(scopes) {
      result <- metric_export_result()
      scenario <- metric_scenario()
      selected <- result$scenarios[[scenario]]
      rows <- if (is.data.frame(result$return_period)) result$return_period else NULL
      rows <- if (is.data.frame(rows)) rows[rows$scenario == scenario, , drop = FALSE] else data.frame()
      rows <- if (is.data.frame(rows) && nrow(rows)) rows[rows$scope %in% scopes, , drop = FALSE] else data.frame()
      if (!nrow(rows)) {
        rows <- data.frame(scenario = scenario, probability = NA_real_, return_period = NA_real_,
          status = "unavailable", reason = "Baseline-anchored adverse support unavailable.", baseline = NA_real_, after_main = NA_real_,
          after_repositioning = NA_real_, policy = NA_real_, main = NA_real_, repositioning = NA_real_,
          interaction = NA_real_, resilience = NA_real_, total = NA_real_, scope = "baseline_anchored",
          adverse_basis = "baseline_selected_metric", center_method = "equal_model_mean", quantile_method = "rank_interp_n_p_plus_half",
          year_lo = NA_real_, year_hi = NA_real_, rank_lo = NA_integer_, rank_hi = NA_integer_,
          weight_lo = NA_real_, weight_hi = NA_real_, stringsAsFactors = FALSE)
      }
      label <- "baseline_anchored_adverse_quantile"
      .policy_metric_export_annotate(rows, result, scenario,
        export_scope = label, so = so(), analysis_unit = analysis_unit(),
        status = if (nrow(rows)) selected$status %||% "unavailable" else "unavailable",
        reason = selected$reason %||% if (!nrow(rows)) "No rows for this adverse-attribution scope." else "")
    }
    metric_mechanism_export <- function() {
      result <- metric_export_result()
      scenario <- metric_scenario()
      selected <- result$scenarios[[scenario]]
      summary <- result$mechanisms$summary
      if (is.data.frame(summary) && nrow(summary)) summary <- summary[summary$scenario == scenario, , drop = FALSE]
      annual <- if (is.list(selected) && identical(selected$status, "ok")) selected$mechanisms else NULL
      if (is.data.frame(summary) && nrow(summary)) {
        summary$record_type <- "equal_model_mean_mechanism_summary"
        summary$center_method <- "equal_model_mean"
      }
      if (is.data.frame(annual) && nrow(annual)) {
        annual$record_type <- "annual_mechanism_diagnostic"
        annual$center_method <- "annual_model_year"
      }
      rows <- dplyr::bind_rows(summary, annual)
      rows <- .policy_metric_export_annotate(rows, result, scenario,
        export_scope = "production_prediction_rows", scale = "model_scale",
        so = so(), analysis_unit = analysis_unit(),
        status = selected$status %||% "unavailable", reason = selected$reason)
      rows$rank_convention <- result$mechanisms$metadata$rank_convention %||%
        "fixed main-derived pre/post ranks; interaction evaluated at post-main rank"
      rows$included_terms <- result$mechanisms$metadata$included_terms %||%
        "canonical repositioning and weather-policy interaction channel changes only"
      rows$excluded_terms <- result$mechanisms$metadata$excluded_terms %||%
        "not a complete derivative of nonlinear fitted model terms"
      rows$repositioning_status <- result$mechanisms$repositioning_status %||% "unavailable"
      rows$interaction_status <- result$mechanisms$interaction_status %||% "unavailable"
      rows
    }
    wise_export_table(
      key = "policy_metric_contributions",
      label = "Metric-aware expected and annual policy contributions",
      step = 3L,
      fun = function() {
        result <- metric_export_result()
        scenario <- metric_scenario()
        expected <- metric_expected_export()
        annual <- metric_annual_export()
        # Rows are explicitly tagged so the equal-model expected summary is
        # not mistaken for a model/year observation.
        if (nrow(expected)) expected$record_type <- "expected_summary"
        if (nrow(annual)) annual$record_type <- "annual_model_year"
        dplyr::bind_rows(expected, annual)
      },
      description = "Native metric-aware cumulative levels and ordered contributions for the equal-model expected summary and annual model-year states; central estimates only."
    )
    wise_export_table(
      key = "policy_metric_adverse_attribution",
      label = "Metric-aware adverse-weather attribution",
      step = 3L,
      fun = function() {
        adverse <- metric_tail_export("baseline_anchored")
        if (nrow(adverse)) adverse$record_type <- "baseline_anchored_adverse_quantile"
        adverse
      },
       description = "Native selected-metric cumulative levels and contributions under baseline-anchored interpolated support; SSP rows use an equal-model mean."
    )
    wise_export_table(
      key = "policy_adverse_support",
      label = "Policy baseline-anchored adverse support",
      step = 3L,
      fun = function() {
        result <- metric_export_result()
        rows <- result$adverse_support
        if (is.data.frame(rows) && nrow(rows)) rows[rows$scenario == metric_scenario(), , drop = FALSE] else data.frame()
      },
      description = "Per-model selected-metric baseline quantiles and interpolated year-rank support."
    )
    wise_export_table(
      key = "policy_adverse_by_model",
      label = "Policy adverse values by model",
      step = 3L,
      fun = function() {
        result <- metric_export_result()
        rows <- result$adverse_by_model
        if (is.data.frame(rows) && nrow(rows)) rows[rows$scenario == metric_scenario(), , drop = FALSE] else data.frame()
      },
      description = "Metric-aware state values and adjacent contributions by model under baseline-selected support."
    )
    wise_export_table(
      key = "policy_decomposition_adverse_support",
      label = "Technical adverse values by model",
      step = 3L,
      fun = function() {
        support <- metric_adverse_support()
        rows <- if (is.data.frame(support)) support else data.frame()
        if (nrow(rows)) rows$scale <- "model_scale"
        rows
      },
      description = "Results-owned year-rank support reused by technical model-scale diagnostics."
    )
    wise_export_table(
      key = "policy_weather_sensitivity",
      label = "Policy weather-sensitivity mechanisms",
      step = 3L,
      fun = metric_mechanism_export,
      description = "Hazard-specific channel-implied sensitivity changes and rank diagnostics on the model scale, distinct from selected-metric contributions; includes weather units, categories, support, and central-only uncertainty status."
    )
    output$metric_mechanism_table <- reactable::renderReactable({
      tbl <- metric_mechanism_data()
      visible <- c("hazard", "category", "contrast", "repositioning", "interaction",
        "positive_repositioning_share", "negative_repositioning_share",
        "positive_interaction_share", "negative_interaction_share", "rank_movement",
        "model_units", "weather_units", "n_models")
      if (!"hazard" %in% names(tbl)) visible <- c(visible, "status", "availability", "reason")
      reactable::reactable(tbl, compact = TRUE, searchable = FALSE, defaultPageSize = 10,
        defaultColDef = reactable::colDef(show = FALSE), highlight = TRUE,
        columns = stats::setNames(lapply(names(tbl), function(name) {
          if (is.numeric(tbl[[name]])) reactable::colDef(show = name %in% visible,
            format = reactable::colFormat(digits = 4))
          else reactable::colDef(show = name %in% visible,
            class = "wise-dt-wrap", minWidth = 130)
        }), names(tbl)))
    })
    output$metric_rif_curve_ui <- shiny::renderUI({
      result <- metric_decomposition()
      selected <- selected_metric_result()
      if (is.null(selected) || !identical(selected$status, "ok") ||
          !identical(result$mechanisms$repositioning_status, "modeled") ||
          is.null(result$mechanisms$fitted_curve)) return(NULL)
      shiny::tagList(
        shiny::h5("Unchanged Step 1 weather-sensitivity curve"),
        shiny::tags$p(class = "diagnostic-note", result$mechanisms$curve_scope),
        wise_chart_output(ns("metric_curve_plot1"),
          paste("Unchanged fitted curve for", model_fit()$weather_terms[[1L]]), height = "360px"),
        if (length(model_fit()$weather_terms) > 1L) {
          wise_chart_output(ns("metric_curve_plot2"),
            paste("Unchanged fitted curve for", model_fit()$weather_terms[[2L]]), height = "360px")
        }
      )
    })
    for (idx in seq_len(2L)) {
      local({
        i <- idx
        output[[paste0("metric_curve_plot", i)]] <- echarts4r::renderEcharts4r({
          result <- metric_decomposition()
          mf <- model_fit()
          req(!is.null(result$mechanisms$fitted_curve), length(mf$weather_terms) >= i)
          chart <- echart_rif_weather_curve(result$mechanisms$fitted_curve,
            mf$weather_terms[[i]], interaction_terms = mf$interaction_terms %||% character(),
            label_fun = get_label, height = "360px")
          req(!is.null(chart))
          chart
        })
        outputOptions(output, paste0("metric_curve_plot", i), suspendWhenHidden = FALSE)
      })
    }

    get_label <- function(var_name) {
      vl <- if (is.function(variable_list)) variable_list() else variable_list
      if (is.null(vl) || is.null(var_name) || length(var_name) == 0) {
        return(if (is.null(var_name)) "" else as.character(var_name))
      }
      idx <- match(var_name, vl$name)
      if (length(idx) == 0 || is.na(idx)) {
        var_name
      } else {
        as.character(vl$label[idx])
      }
    }

    weather_basis_label <- function(basis = "mean") {
      switch(basis %||% "mean",
        mean = "mean historical-baseline weather",
        adverse_10 = "adverse 1-in-10 historical weather year",
        adverse_20 = "adverse 1-in-20 historical weather year",
        "mean historical-baseline weather"
      )
    }

    historical_weather_basis_for_probability <- function(target_p) {
      ctx <- decomp_context()
      if (!is.null(ctx) && !is.null(ctx$adverse_bases)) {
        key <- if (is.null(target_p)) "mean" else if (identical(target_p, 0.20)) "adverse_5" else if (identical(target_p, 0.10)) "adverse_10" else "adverse_20"
        cached <- if (exists(".decomposition_context_adverse_basis",
          mode = "function"
        )) {
          .decomposition_context_adverse_basis(ctx, key)
        } else {
          ctx$adverse_bases[[key]]
        }
        if (!is.null(cached)) {
          return(cached)
        }
      }
      hs <- baseline_hist_sim()
      if (is.null(hs) || is.null(hs$weather_raw)) {
        return(NULL)
      }
      raw <- step2_resolve_weather(hs$weather_raw, hs)
      if (is.null(raw) || !nrow(raw)) {
        return(NULL)
      }
      if (is.null(target_p)) {
        return(raw)
      }
      if (!"timestamp" %in% names(raw)) {
        return(raw)
      }
      years <- as.integer(format(raw$timestamp, "%Y"))
      year_values <- split(raw, years)
      pipe <- hs$pipeline
      if (is.null(pipe) || is.null(pipe$y_point) || is.null(pipe$sim_year)) {
        return(raw)
      }
      simulated_years <- split(seq_along(pipe$y_point), pipe$sim_year)
      annual_values <- vapply(simulated_years, function(idx) {
        vals <- as.numeric(pipe$y_point[idx])
        weights <- if (!is.null(pipe$weight)) as.numeric(pipe$weight[idx]) else NULL
        if (is.null(weights)) {
          mean(vals, na.rm = TRUE)
        } else {
          stats::weighted.mean(vals, weights, na.rm = TRUE)
        }
      }, numeric(1L))
      # Select the most adverse observed annual outcome using the same metric
      # direction contract as the Results plots.
      agg <- annual_values[names(annual_values) %in% names(year_values)]
      adverse_high <- identical(
        outcome_direction(hs$so$name %||% "welfare", hs$so$type %||% "numeric"),
        "lower_is_better"
      )
      ordered <- order(agg, decreasing = adverse_high)
      take <- max(1L, min(length(ordered), ceiling(length(ordered) * target_p)))
      year_values[[names(agg)[ordered[[take]]]]]
    }

    historical_weather_for_basis <- function(basis) {
      if (identical(basis, "mean")) {
        return(historical_weather_basis_for_probability(NULL))
      }
      historical_weather_basis_for_probability(
        if (identical(basis, "adverse_10")) 0.10 else 0.05
      )
    }

    decomp_for_basis <- function(basis) {
      if (!identical(policy_method_status()$status, "ok")) return(NULL)
      if (identical(basis, "mean")) return(decomp_result())
      ctx <- decomp_context()
      cached_result <- if (!is.null(ctx)) {
        key <- if (identical(basis, "adverse_10")) "adverse_10" else "adverse_20"
        if (exists(".decomposition_context_adverse_result", mode = "function")) {
          .decomposition_context_adverse_result(ctx, key)
        } else {
          ctx$adverse_decompositions[[key]]
        }
      } else {
        NULL
      }
      if (!is.null(cached_result)) {
        return(if ("sim_year" %in% names(cached_result))
          select_decomp_weather_basis(cached_result, basis, so()) else cached_result)
      }
      hs <- baseline_hist_sim()
      svy_b <- baseline_svy()
      svy_p <- policy_svy()
      mf <- model_fit()
      if (is.null(hs) || is.null(svy_b) || is.null(svy_p) || is.null(mf)) {
        return(decomp_result())
      }
      tryCatch(
        decompose_policy_effect(
          svy_baseline = svy_b, svy_policy = svy_p, model_fit = mf,
          so = hs$so, weather_raw = historical_weather_for_basis(basis),
          skip_coef = !isTRUE(show_coef_uncertainty()),
          context = decomp_context(),
          run_identity = if (!is.null(decomp_context())) decomp_context()$run_identity else NULL
        ),
        error = function(e) select_decomp_weather_basis(decomp_result(), basis, so())
      )
    }
    selected_decomp_result <- reactive({
      decomp_for_basis(input$decomp_weather_basis %||% "mean")
    })

    decomposition_scenarios <- reactive({
      scenarios <- c(baseline_hist_sim()$hist_label %||% "Historical",
        .compact_future_scenarios(decomp_scenarios()))
      raw <- decomp_scenarios()
      if (is.data.frame(raw) && nrow(raw) && "scenario" %in% names(raw)) {
        scenarios <- c(scenarios, unique(as.character(raw$scenario)))
      }
      unique(scenarios)
    })
    headline_scenario_ui <- shiny::renderUI({
      scenarios <- decomposition_scenarios()
      pill_toggle(ns("headline_scenario"), label = "Scenario",
        choices = stats::setNames(scenarios, scenarios),
        selected = isolate(input$headline_scenario) %||% scenarios[[1L]],
        layout = "horizontal")
    })
    output$headline_scenario_ui <- headline_scenario_ui
    output$headline_weather_basis_ui <- shiny::renderUI({
      pill_toggle(ns("headline_weather_basis"), label = "Weather-year basis",
        choices = c("Mean" = "mean", "Adverse 1-in-10" = "adverse_10",
          "Adverse 1-in-20" = "adverse_20"),
        selected = isolate(input$headline_weather_basis) %||% "mean",
        layout = "horizontal")
    })
    output$decile_weather_basis_ui <- shiny::renderUI({
      pill_toggle(ns("decile_weather_basis"), label = "Weather-year basis",
        choices = c("Mean" = "mean", "Adverse 1-in-10" = "adverse_10",
          "Adverse 1-in-20" = "adverse_20"),
        selected = isolate(input$decile_weather_basis) %||% "mean",
        layout = "horizontal")
    })
    headline_decomp_data <- reactive({
      if (isTRUE(stale())) return(data.frame())
      if (!identical(policy_method_status()$status, "ok")) return(data.frame())
      scenario <- input$headline_scenario %||% baseline_hist_sim()$hist_label %||% "Historical"
      basis <- input$headline_weather_basis %||% "mean"
      is_historical <- identical(scenario, baseline_hist_sim()$hist_label %||% "Historical")
        res <- if (is_historical) decomp_for_basis(basis) else NULL
        if (!is_historical) {
          compact <- decomp_scenarios()
          if (.is_compact_decomp_scenarios(compact)) {
          rows <- .compact_future_year(compact, scenario, basis, so())
          res <- .compact_future_as_decomp(rows, is_rif())
          if (nrow(res) && identical(so()$transform %||% "", "log")) {
            res$baseline_annual <- .weighted_mean_safe(rows$baseline_annual,
              rows$weight_baseline_y_point)
          }
        } else if (is.data.frame(compact) && nrow(compact)) {
          res <- compact[compact$scenario == scenario, , drop = FALSE]
          res <- select_decomp_weather_basis(res, basis, so())
        } else res <- NULL
      }
      out <- if (!is_historical && .is_compact_decomp_scenarios(decomp_scenarios())) {
        .decomposition_outcome_summary(res, so(), baseline_svy())
      } else .decomposition_outcome_summary(res, so(), baseline_svy())
      if (nrow(out)) out$scenario <- scenario
      out
    })
    outcome_metric <- reactive({
      metric_metadata("mean", so(), poverty_line(), analysis_unit(), weighted = TRUE)
    })
    output$headline_decomp_note_ui <- renderUI({
      if (!identical(policy_method_status()$status, "ok")) {
        return(shiny::tags$p(class = "alert alert-warning", policy_method_status()$reason))
      }
      scenario <- input$headline_scenario %||% baseline_hist_sim()$hist_label %||% "Historical"
      metric <- outcome_metric()
      unit <- metric$change_unit %||% "outcome units"
      outcome <- metric$outcome_label %||% metric$outcome_name %||% so()$name %||% "selected outcome"
      shiny::tags$p(class = "diagnostic-note", paste0(
        "Weighted average change in ", outcome, " (", unit, ") for ", scenario,
        " under ", weather_basis_label(input$headline_weather_basis %||% "mean"), "."
      ))
    })
    # Zero-arg echarts closures shared by the on-screen render and the
    # export bundle (guidelines §7 pattern).
    headline_decomp_chart <- function() {
      echart_outcome_decomposition_headline(
        headline_decomp_data(),
        y_label = paste0("Weighted average change (", outcome_metric()$change_unit %||% "outcome units", ")"),
        height = "360px"
      )
    }
    output$headline_decomp_plot <- echarts4r::renderEcharts4r({
      ch <- headline_decomp_chart()
      req(!is.null(ch))
      ch
    })
    # The Decomposition UI is inserted after the server starts. Keep plots
    # live before their DOM nodes exist so they render immediately on tab open.
    outputOptions(output, "headline_decomp_plot", suspendWhenHidden = FALSE)
    wise_export_figure(
      key = "policy_decomposition_headline",
      label = "Headline mean outcome decomposition",
      step = 3L,
      fun = headline_decomp_chart,
      description = "Registry mean-metric weighted average decomposition in the selected outcome's change units.",
      width = 9, height = 5, stale = stale
    )
    wise_export_table(
      key = "policy_decomposition_headline_data",
      label = "Headline outcome decomposition",
      step = 3L,
      fun = function() {
        data <- headline_decomp_data()
        metric <- outcome_metric()
        if (!nrow(data)) return(data.frame(Message = "Decomposition data is not available for this selection."))
        n <- nrow(data)
        data.frame(Scenario = rep(data$scenario[[1L]], n),
          Metric = rep(metric$label %||% "Mean", n),
          Outcome = rep(metric$outcome_label %||% metric$outcome_name %||% "", n),
          `Change unit` = rep(metric$change_unit %||% "outcome units", n),
          `Effect component` = data$channel,
          `Weighted average change` = data$value,
          check.names = FALSE)
      },
      description = "Registry mean-metric weighted average decomposition in the selected outcome's change units.",
      stale = stale
    )

    # --- Stacked bar chart by decile ---
    # UI-48: register Step 3's decomposition figures for the export bundle.
    output$decile_scenario_ui <- shiny::renderUI({
      choices <- decomposition_scenarios()
      selected <- isolate(input$decile_scenario) %||% choices[[1L]]
      if (!selected %in% choices) selected <- choices[[1L]]
      pill_toggle(ns("decile_scenario"), label = "Scenario",
        choices = stats::setNames(choices, choices), selected = selected,
        layout = "horizontal")
    })

    decile_decomp_data <- reactive({
      if (isTRUE(stale()) || !identical(policy_method_status()$status, "ok")) return(data.frame())
      scenario <- input$decile_scenario %||% baseline_hist_sim()$hist_label %||% "Historical"
      basis <- input$decile_weather_basis %||% "mean"
      is_historical <- identical(scenario, baseline_hist_sim()$hist_label %||% "Historical")
      res <- if (is_historical) {
        decomp_for_basis(basis)
      } else if (.is_compact_decomp_scenarios(decomp_scenarios())) {
        compact <- .compact_future_decile_summary(decomp_scenarios(), scenario,
          basis, so(), is_rif())
        if (!nrow(compact)) return(data.frame())
        baseline_annual <- rep(NA_real_, nrow(compact))
        if (identical(so()$transform %||% "", "log")) {
          survey <- baseline_svy()
          baseline_decile <- weighted_baseline_deciles(survey, so()$name %||% "welfare",
            baseline_weight_column(survey))
          survey_weights <- .decomp_weights(survey)
          baseline_annual <- vapply(compact$decile, function(d) {
            keep <- baseline_decile == d
            .weighted_mean_safe(as.numeric(survey[[so()$name]][keep]), survey_weights[keep])
          }, numeric(1))
        }
        compact_decomp <- data.frame(decile = compact$decile,
          delta_main = compact$main, delta_res1 = compact$repositioning,
          delta_res2 = compact$interaction, delta_total = compact$total,
          weight = compact$weighted_population,
          baseline_annual = baseline_annual)
        return(structure(.decomposition_outcome_deciles(compact_decomp, so(), baseline_svy()),
          outcome_units = TRUE))
      } else {
        sc <- decomp_scenarios()
        selected <- if (is.data.frame(sc) && nrow(sc)) sc[sc$scenario == scenario, , drop = FALSE] else NULL
        select_decomp_weather_basis(selected, basis, so())
      }
      if (!is_historical && is.data.frame(res) && isTRUE(attr(res, "outcome_units"))) {
        return(res)
      }
      .decomposition_outcome_deciles(res, so(), baseline_svy())
    })

    output$headline_decomp_csv_ui <- shiny::renderUI({
      wise_reactable_csv_button(ns("headline_decomp_table"), "policy_decomposition_headline_data")
    })
    output$decile_decomp_csv_ui <- shiny::renderUI({
      wise_reactable_csv_button(ns("decile_decomp_table"), "policy_decomposition_channels_by_decile")
    })
    output$headline_decomp_table <- DT::renderDT({
      data <- headline_decomp_data()
      metric <- outcome_metric()
      if (!nrow(data)) return(DT::datatable(data.frame(Message =
        "Decomposition data is not available for this selection."), rownames = FALSE, options = list(dom = "t")))
      n <- nrow(data)
      DT::datatable(data.frame(Scenario = rep(data$scenario[[1L]], n),
        Metric = rep(metric$label %||% "Mean", n),
        Outcome = rep(metric$outcome_label %||% metric$outcome_name %||% "", n),
        `Change unit` = rep(metric$change_unit %||% "outcome units", n),
        `Effect component` = data$channel,
        `Weighted average change` = data$value, check.names = FALSE),
        rownames = FALSE, class = "compact stripe")
    })
    outputOptions(output, "headline_decomp_table", suspendWhenHidden = TRUE)
    output$decile_decomp_table <- DT::renderDT({
      data <- decile_decomp_data()
      if (!nrow(data)) return(DT::datatable(data.frame(Message =
        "Decomposition data is not available for this selection."), rownames = FALSE, options = list(dom = "t")))
      unit <- outcome_metric()$change_unit %||% "outcome units"
      channels <- data[c("main", "repositioning", "interaction", "total")]
      names(channels) <- paste(c("Main effect", "Repositioning", "Interaction", "Total policy effect"),
        paste0("(", unit, ")"))
      table <- stats::setNames(data.frame(data$decile, channels, check.names = FALSE),
        c("Baseline welfare decile", names(channels)))
      DT::datatable(table, rownames = FALSE, class = "compact stripe")
    })
    outputOptions(output, "decile_decomp_table", suspendWhenHidden = TRUE)
    decomp_bar_chart <- function() {
      echart_outcome_decomposition_deciles(
        decile_decomp_data(),
        y_label = paste0("Weighted average change (", outcome_metric()$change_unit %||% "outcome units", ")"),
        height = "450px"
      )
    }
    output$decomp_bar_plot <- echarts4r::renderEcharts4r({
      ch <- decomp_bar_chart()
      req(!is.null(ch))
      ch
    })
    outputOptions(output, "decomp_bar_plot", suspendWhenHidden = FALSE)
    wise_export_table(
      key = "policy_decomposition_channels_by_decile",
      label = "Mean outcome decomposition by baseline decile",
      step = 3L,
      fun = function() {
        data <- decile_decomp_data()
        if (!nrow(data)) return(data.frame(Message = "Decomposition data is not available for this selection."))
        unit <- outcome_metric()$change_unit %||% "outcome units"
        channels <- data[c("main", "repositioning", "interaction", "total")]
        names(channels) <- paste(c("Main effect", "Repositioning", "Interaction", "Total policy effect"),
          paste0("(", unit, ")"))
        stats::setNames(data.frame(data$decile, channels, check.names = FALSE),
          c("Baseline welfare decile", names(channels)))
      },
      description = "Weighted average mean-metric channel contributions in registry outcome units by fixed baseline welfare decile.",
      stale = stale
    )
    wise_export_figure(
      key = "policy_decomposition_channels_by_decile_plot",
      label = "Selected policy decomposition channels",
      step = 3L,
      fun = decomp_bar_chart,
      description = "Weighted average mean-metric channel contributions in registry outcome units by fixed baseline welfare decile.",
      width = 9, height = 6, stale = stale
    )
    wise_export_remove(c("policy_metric_contributions", "policy_metric_adverse_attribution",
      "policy_adverse_support", "policy_adverse_by_model", "policy_decomposition_adverse_support",
      "policy_weather_sensitivity"))
    output$decomp_bar_note_ui <- renderUI({
      basis <- input$decile_weather_basis %||% "mean"
      basis_label <- switch(basis,
        adverse_10 = "adverse 1-in-10 historical weather year",
        adverse_20 = "adverse 1-in-20 historical weather year",
        "mean historical-baseline weather"
      )
      shiny::tags$p(
        class = "diagnostic-note",
        paste0(
          "Decile 1 is the poorest. Bars show weighted mean-effect channel changes in ",
          outcome_metric()$change_unit %||% "outcome units", "; the marker is total policy change for ",
          basis_label, " under the ", input$decile_scenario %||% "Historical",
          " scenario. Deciles are fixed from weighted observed baseline welfare."
        )
      )
    })

    # --- Beta curve (RIF only): one panel per weather variable -------------
    output$beta_curve_ui <- renderUI({
      if (!is_rif()) {
        return(NULL)
      }
      mf <- model_fit()
      if (is.null(mf$rif_grid)) {
        return(NULL)
      }
      n_vars <- length(mf$weather_terms %||% character(0))
      if (n_vars == 0) {
        return(NULL)
      }

      shiny::tagList(
        shiny::h4(
          "How does weather sensitivity vary across the welfare distribution?",
          class = "diagnostic-section-heading"
        ),
        # RIF-only diagnostic; collapsed by default because it re-uses the
        # Step 1 model fit rather than the Step 3 policy simulation.
        shiny::tags$button(
          class = "btn btn-sm btn-outline-secondary mb-2",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("beta_curve_section")),
          `aria-expanded` = "false",
          `aria-controls` = ns("beta_curve_section"),
          "Show / hide weather-sensitivity curves"
        ),
        shiny::tags$p(
          class = "diagnostic-note",
          "These curves are defined by the model fit in Step 1 and do not change with the policy or climate selections in Steps 2-3."
        ),
        shiny::div(
          id = ns("beta_curve_section"),
          class = "collapse",
          shiny::div(
            class = "results-section-card diagnostic-section-card",
            weather_plot_layout(
              ns, n_vars,
              ids = c("beta_curve_plot1", "beta_curve_plot2"),
              height = "400px",
              alts = paste(
                "Beta curve plot: unconditional quantile regression weather",
                "sensitivity across welfare quantiles for",
                mf$weather_terms
              ),
              echarts = TRUE
            ),
            shiny::tags$p(
              class = "diagnostic-note",
              "Shows how weather sensitivity varies by quantile.",
              "Repositioning exists only for RIF models and arises when households move along this curve."
            )
          )
        )
      )
    })

    .render_beta_curve <- function(idx) {
      echarts4r::renderEcharts4r({
        req(is_rif(), model_fit())
        mf <- model_fit()
        req(length(mf$weather_terms) >= idx)
        ch <- echart_rif_weather_curve(
          mf$rif_grid,
          mf$weather_terms[idx],
          interaction_terms = mf$interaction_terms %||% character(0),
          label_fun = get_label,
          height = "400px"
        )
        req(!is.null(ch))
        ch
      })
    }

    output$beta_curve_plot1 <- .render_beta_curve(1L)
    output$beta_curve_plot2 <- .render_beta_curve(2L)
    outputOptions(output, "beta_curve_plot1", suspendWhenHidden = FALSE)
    outputOptions(output, "beta_curve_plot2", suspendWhenHidden = FALSE)

    for (idx in seq_len(2L)) {
      local({
        i <- idx
        wise_export_figure(
          key = paste0("policy_rif_weather_curve_", i),
          label = paste("RIF weather-sensitivity curve", i),
          step = 3L,
          fun = function() {
            req(is_rif(), model_fit())
            mf <- model_fit()
            req(length(mf$weather_terms) >= i)
            echart_rif_weather_curve(
              mf$rif_grid, mf$weather_terms[i],
              interaction_terms = mf$interaction_terms %||% character(0),
              label_fun = get_label,
              height = "400px"
            )
          },
          description = "RIF weather coefficient by baseline welfare quantile; interpolation is limited to the estimated grid.",
          width = 9, height = 6
        )
      })
    }

    technical_decomp_table <- reactive({
      if (!identical(policy_method_status()$status, "ok")) {
        return(data.frame(availability = "unsupported", reason = policy_method_status()$reason))
      }
      bases <- list(
        `Mean weather` = decomp_result(),
        `Adverse 1-in-5` = .decomposition_context_adverse_result(
          decomp_context(), "adverse_5"
        ),
        `Adverse 1-in-10` = .decomposition_context_adverse_result(
          decomp_context(), "adverse_10"
        ),
        `Adverse 1-in-20` = .decomposition_context_adverse_result(
          decomp_context(), "adverse_20"
        )
      )
      .build_decomp_table_by_basis(bases, is_rif())
    })

    # --- Interaction warning ---
    output$interaction_warning_ui <- renderUI({
      res <- decomp_result()
      if (is.null(res)) {
        return(NULL)
      }
      if (!"delta_res2" %in% names(res) || all(abs(res$delta_res2) < 1e-10)) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 10px; font-size: 13px;",
          shiny::icon("exclamation-triangle"),
          " No weather\u00d7policy interaction terms detected in the model. ",
          "The interaction channel is zero. To enable this channel, include ",
          "interaction terms between weather and policy variables in the ",
          "Step 1 model specification."
        )
      }
    })

    invisible(NULL)
  })
}


# Plot helpers ----

#' @noRd
.build_decomp_table <- function(decomp_df, is_rif) {
  w <- if ("weight" %in% names(decomp_df)) decomp_df$weight else rep(1, nrow(decomp_df))
  w[!is.finite(w) | w < 0] <- 0
  if (!sum(w) > 0) w <- rep(1, nrow(decomp_df))
  w_norm <- w / sum(w, na.rm = TRUE)
  has_sd <- all(c("sd_main", "sd_res1", "sd_res2", "sd_total") %in% names(decomp_df))

  # Aggregated SE on a log-scale delta given a per-household SD column.
  # Var(Sigma w*delta_i) ~ Sigma w_i^2 * Var(delta_i) under household independence.
  agg_se <- function(sd_col) {
    if (!has_sd || is.null(decomp_df[[sd_col]])) {
      return(NA_real_)
    }
    sqrt(sum((w_norm^2) * (decomp_df[[sd_col]])^2, na.rm = TRUE))
  }

  summary_row <- function(label, vals, sd_col = NULL) {
    mean_log <- stats::weighted.mean(vals, w, na.rm = TRUE)
    mean_pct <- (exp(mean_log) - 1) * 100
    se_log <- if (is.null(sd_col)) NA_real_ else agg_se(sd_col)
    se_pct <- if (is.na(se_log)) NA_real_ else abs(exp(mean_log)) * se_log * 100
    data.frame(
      Channel = label,
      `Mean (%)` = round(mean_pct, 2),
      `+/- SE (%)` = if (is.na(se_pct)) NA_real_ else round(se_pct, 2),
      check.names = FALSE
    )
  }

  rows <- list(
    summary_row("Total effect", decomp_df$delta_total, sd_col = "sd_total"),
    summary_row("Main effect (direct transfer and covariate shift)",
      decomp_df$delta_main,
      sd_col = "sd_main"
    ),
    summary_row("Direct transfer component", decomp_df$delta_sp)
  )

  if (is_rif) {
    rows <- c(rows, list(
      summary_row("Repositioning effect", decomp_df$delta_res1, sd_col = "sd_res1"),
      summary_row("Weather-policy interaction", decomp_df$delta_res2, sd_col = "sd_res2")
    ))
  } else {
    rows <- c(rows, list(
      summary_row("Weather-policy interaction", decomp_df$delta_res2, sd_col = "sd_res2")
    ))
  }

  df <- do.call(rbind, rows)

  data.frame(
    `Effect component` = trimws(df$Channel),
    `Mean effect (%)` = df$`Mean (%)`,
    `Coefficient SE (%)` = df$`+/- SE (%)`,
    check.names = FALSE
  )
}

.build_decomp_table_by_basis <- function(bases, is_rif) {
  if (is.null(bases) || !length(bases)) {
    return(data.frame())
  }
  tables <- lapply(bases, function(x) {
    if (is.null(x) || !is.data.frame(x) || !nrow(x)) {
      return(NULL)
    }
    .build_decomp_table(x, is_rif)
  })
  available <- Filter(Negate(is.null), tables)
  if (!length(available)) {
    return(data.frame())
  }
  template <- available[[1L]]
  out <- template["Effect component"]
  for (nm in names(tables)) {
    tbl <- tables[[nm]]
    if (is.null(tbl)) {
      out[[paste0(nm, " (%)")]] <- NA_real_
      next
    }
    out[[paste0(nm, " (%)")]] <- tbl[["Mean effect (%)"]]
    if (identical(nm, "Mean weather")) {
      out[["Coefficient SE (%)"]] <- tbl[["Coefficient SE (%)"]]
    }
  }
  out
}
