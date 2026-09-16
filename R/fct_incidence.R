# Distributional incidence summaries ----
# Fixed-baseline distributional incidence summaries.

weighted_baseline_deciles <- function(svy, outcome, weight = NULL) {
  if (is.null(svy) || !is.data.frame(svy) || !outcome %in% names(svy)) {
    return(integer(0))
  }
  y <- suppressWarnings(as.numeric(svy[[outcome]]))
  w <- if (!is.null(weight) && weight %in% names(svy)) {
    suppressWarnings(as.numeric(svy[[weight]]))
  } else rep(1, length(y))
  ok <- is.finite(y) & is.finite(w) & w > 0
  out <- rep(NA_integer_, length(y))
  if (!any(ok)) return(out)
  ord <- order(y[ok], na.last = NA)
  idx <- which(ok)[ord]
  cumulative <- cumsum(w[idx]) / sum(w[idx])
  out[idx] <- pmin(10L, pmax(1L, ceiling(cumulative * 10)))
  out
}

baseline_weight_column <- function(svy) {
  if (is.null(svy) || !is.data.frame(svy)) return(NULL)
  hits <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(svy),
               value = TRUE, ignore.case = TRUE)
  if (length(hits)) hits[[1L]] else NULL
}

.pipeline_household_values <- function(pipe, is_log = FALSE) {
  if (is.null(pipe) || is.null(pipe$y_point) || is.null(pipe$svy_row_id)) {
    return(data.frame())
  }
  y <- suppressWarnings(as.numeric(pipe$y_point))
  if (is_log) y <- exp(y)
  id <- suppressWarnings(as.integer(pipe$svy_row_id))
  yr <- pipe$sim_year %||% rep(NA_integer_, length(y))
  ok <- is.finite(y) & is.finite(id) & id > 0L
  if (!any(ok)) return(data.frame())
  data.frame(id = id[ok], sim_year = yr[ok], value = y[ok])
}

.mean_by_household <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame())
  dplyr::summarise(
    dplyr::group_by(df, .data$id),
    value = mean(.data$value, na.rm = TRUE),
    .groups = "drop"
  )
}

.select_pipeline_draws <- function(pipe, sim_year = NULL) {
  if (is.null(pipe) || is.null(sim_year) || is.null(pipe$sim_year)) return(pipe)
  keep <- pipe$sim_year %in% sim_year
  pipe[vapply(pipe, function(x) length(x) == length(keep), logical(1L))] <-
    lapply(pipe[vapply(pipe, function(x) length(x) == length(keep), logical(1L))], `[`, keep)
  pipe
}

#' Step 2 household effect by fixed baseline welfare decile.
#'
#' Each model/year is first reduced to a household value, then equally weighted
#' across models and years. Deciles are assigned once from observed baseline
#' welfare using survey weights and never re-ranked after simulation.
step2_incidence_by_decile <- function(svy, outcome, hist_pipeline,
                                      scenario_pipelines, is_log = FALSE,
                                      scenario = NULL, sim_year = NULL) {
  if (is.null(svy) || is.null(scenario_pipelines)) return(data.frame())
  if (!is.list(scenario_pipelines)) scenario_pipelines <- list(scenario_pipelines)
  weight_col <- baseline_weight_column(svy)
  decile <- weighted_baseline_deciles(svy, outcome, weight_col)
  hist <- .mean_by_household(.pipeline_household_values(
    .select_pipeline_draws(hist_pipeline, sim_year), is_log
  ))
  if (!nrow(hist)) return(data.frame())
  model_effects <- lapply(seq_along(scenario_pipelines), function(j) {
    pipe <- scenario_pipelines[[j]]
    future <- .mean_by_household(.pipeline_household_values(
      .select_pipeline_draws(pipe, sim_year), is_log
    ))
    if (!nrow(future)) return(NULL)
    names(future)[names(future) == "value"] <- "future"
    hist_one <- hist
    names(hist_one)[names(hist_one) == "value"] <- "baseline"
    out <- merge(hist_one, future, by = "id", all = FALSE)
    out$effect <- out$future - out$baseline
    out$model_id <- names(scenario_pipelines)[[j]] %||% paste0("model_", j)
    out
  })
  effects <- dplyr::bind_rows(model_effects)
  if (!nrow(effects)) return(effects)
  effects$decile <- decile[effects$id]
  effects$weight <- if (!is.null(weight_col)) {
    as.numeric(svy[[weight_col]])[effects$id]
  } else 1
  effects$scenario <- scenario %||% "Scenario"
  dplyr::filter(effects, is.finite(.data$decile), is.finite(.data$effect),
                is.finite(.data$weight), .data$weight > 0) |>
    dplyr::group_by(.data$scenario, .data$decile) |>
    dplyr::summarise(
      effect = stats::weighted.mean(.data$effect, .data$weight),
      n_models = dplyr::n_distinct(.data$model_id),
      n_households = dplyr::n_distinct(.data$id),
      n_draws = dplyr::n(),
      weighted_population = sum(.data$weight),
      .groups = "drop"
    )
}

step3_incidence_by_decile <- function(decomp, svy, outcome,
                                      baseline_deciles = NULL) {
  if (is.null(decomp) || !nrow(decomp) || is.null(svy)) return(data.frame())
  weight_col <- baseline_weight_column(svy)
  deciles <- baseline_deciles %||% weighted_baseline_deciles(svy, outcome, weight_col)
  if (!"id" %in% names(decomp)) decomp$id <- seq_len(nrow(decomp))
  decomp$decile <- deciles[as.integer(decomp$id)]
  decomp$weight <- if ("weight" %in% names(decomp)) decomp$weight else
    if (!is.null(weight_col)) as.numeric(svy[[weight_col]])[decomp$id] else 1
  value_col <- if ("delta_total" %in% names(decomp)) "delta_total" else "effect"
  dplyr::filter(decomp, is.finite(.data$decile), is.finite(.data[[value_col]]),
                is.finite(.data$weight), .data$weight > 0) |>
    dplyr::group_by(.data$decile) |>
    dplyr::summarise(
      effect = stats::weighted.mean(.data[[value_col]], .data$weight),
      n_households = dplyr::n(),
      weighted_population = sum(.data$weight),
      .groups = "drop"
    )
}

plot_incidence_by_decile <- function(tbl, y_label = "Household-level simulated welfare effect") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(blank_plot("Distributional incidence is unavailable."))
  }
  if (!"scenario" %in% names(tbl)) tbl$scenario <- "Effect"
  ggplot2::ggplot(tbl, ggplot2::aes(x = factor(.data$decile), y = .data$effect,
                                    fill = .data$scenario)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = .wise_zero) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
    wise_scale_fill_cat(name = NULL) +
    ggplot2::labs(x = "Fixed observed baseline welfare decile (1 = poorest)",
                  y = y_label,
                  caption = "Deciles use weighted observed baseline welfare and are not re-ranked under simulated conditions.") +
    theme_wise(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}
