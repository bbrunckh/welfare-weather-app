# Distributional incidence summaries ----
# Fixed-baseline distributional incidence summaries.

weighted_baseline_deciles <- function(svy, outcome, weight = NULL) {
  if (is.null(svy) || !is.data.frame(svy) || !outcome %in% names(svy)) {
    return(integer(0))
  }
  y <- suppressWarnings(as.numeric(svy[[outcome]]))
  w <- if (!is.null(weight) && weight %in% names(svy)) {
    suppressWarnings(as.numeric(svy[[weight]]))
  } else {
    rep(1, length(y))
  }
  ok <- is.finite(y) & is.finite(w) & w > 0
  out <- rep(NA_integer_, length(y))
  if (!any(ok)) {
    return(out)
  }
  ord <- order(y[ok], na.last = NA)
  idx <- which(ok)[ord]
  cumulative <- cumsum(w[idx]) / sum(w[idx])
  out[idx] <- pmin(10L, pmax(1L, ceiling(cumulative * 10)))
  out
}

baseline_weight_column <- function(svy) {
  if (is.null(svy) || !is.data.frame(svy)) {
    return(NULL)
  }
  hits <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(svy),
    value = TRUE, ignore.case = TRUE
  )
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
  if (!any(ok)) {
    return(data.frame())
  }
  data.frame(id = id[ok], sim_year = yr[ok], value = y[ok])
}

.mean_by_household <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame())
  }
  dplyr::summarise(
    dplyr::group_by(df, .data$id),
    value = mean(.data$value, na.rm = TRUE),
    .groups = "drop"
  )
}

.select_pipeline_draws <- function(pipe, sim_year = NULL) {
  if (is.null(pipe) || is.null(sim_year) || is.null(pipe$sim_year)) {
    return(pipe)
  }
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
  if (is.null(svy) || is.null(scenario_pipelines)) {
    return(data.frame())
  }
  if (!is.list(scenario_pipelines)) scenario_pipelines <- list(scenario_pipelines)
  weight_col <- baseline_weight_column(svy)
  decile <- weighted_baseline_deciles(svy, outcome, weight_col)
  hist <- .mean_by_household(.pipeline_household_values(
    .select_pipeline_draws(hist_pipeline, sim_year), is_log
  ))
  if (!nrow(hist)) {
    return(data.frame())
  }
  model_effects <- lapply(seq_along(scenario_pipelines), function(j) {
    pipe <- scenario_pipelines[[j]]
    future <- .mean_by_household(.pipeline_household_values(
      .select_pipeline_draws(pipe, sim_year), is_log
    ))
    if (!nrow(future)) {
      return(NULL)
    }
    names(future)[names(future) == "value"] <- "future"
    hist_one <- hist
    names(hist_one)[names(hist_one) == "value"] <- "baseline"
    out <- merge(hist_one, future, by = "id", all = FALSE)
    out$effect <- out$future - out$baseline
    out$model_id <- names(scenario_pipelines)[[j]] %||% paste0("model_", j)
    out
  })
  effects <- dplyr::bind_rows(model_effects)
  if (!nrow(effects)) {
    return(effects)
  }
  effects$decile <- decile[effects$id]
  effects$weight <- if (!is.null(weight_col)) {
    as.numeric(svy[[weight_col]])[effects$id]
  } else {
    1
  }
  effects$scenario <- scenario %||% "Scenario"
  dplyr::filter(
    effects, is.finite(.data$decile), is.finite(.data$effect),
    is.finite(.data$weight), .data$weight > 0
  ) |>
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

#' Step 3 policy effect by fixed baseline decile, on the decomposition basis.
#'
#' Reads the compact per-year channels used by the decomposition tab, so every
#' scenario (Historical included) follows the same weather-year bases as its
#' selector: the equal-model mean over simulated years or the shared adverse-year
#' rule, in outcome units. Bases that are unavailable (too few simulated years)
#' are omitted.
step3_incidence_by_decile <- function(compact, so = NULL, bases = .decomp_basis_choices) {
  if (!.is_compact_decomp_scenarios(compact)) {
    return(data.frame())
  }
  rows <- lapply(unname(bases), function(basis) {
    lapply(.compact_future_scenarios(compact), function(scenario) {
      deciles <- .compact_future_decile_summary(compact, scenario, basis, so)
      if (!nrow(deciles)) {
        return(NULL)
      }
      members <- .decomp_scenario_rows(compact, scenario, decile = TRUE)$member
      data.frame(
        scenario = scenario, basis = basis, decile = deciles$decile,
        effect = deciles$total,
        n_models = length(unique(as.character(members))),
        stringsAsFactors = FALSE
      )
    })
  })
  dplyr::bind_rows(rows)
}


# Interactive (echarts4r) renderer for the precomputed incidence summary.
# Same statistics: weighted mean effect per fixed
# baseline decile (already precomputed in `tbl`); echarts only draws the
# precomputed values (guidelines §7).
#
# Design notes: grouped vertical bars per scenario, Okabe-Ito palette via
# wise_echart_theme(), axis-trigger tooltip, legend bottom-left, and dashed
# zero reference line. The module UI carries the chart caption as static text.
echart_incidence_by_decile <- function(tbl,
                                       y_label = "Household-level simulated welfare effect",
                                       height = "420px") {
  if (is.null(tbl) || !nrow(tbl)) {
    return(echart_blank("Distributional incidence is unavailable.", height = height))
  }
  if (!"scenario" %in% names(tbl)) tbl$scenario <- "Effect"
  tbl$decile <- factor(tbl$decile,
    levels = sort(unique(as.integer(tbl$decile)))
  )
  scen_levels <- unique(as.character(tbl$scenario))
  tbl$scenario <- factor(tbl$scenario, levels = scen_levels)

  # Wide frame: one column per scenario so each series covers every decile
  # (echarts4r rejects single-column frames; wide keeps the dodged bars aligned).
  wide <- data.frame(
    decile = levels(tbl$decile),
    stringsAsFactors = FALSE
  )
  for (scn in scen_levels) {
    vals <- tbl$effect[as.character(tbl$scenario) == scn]
    idx <- match(as.integer(tbl$decile[as.character(tbl$scenario) == scn]),
      as.integer(levels(tbl$decile))
    )
    col <- rep(NA_real_, nlevels(tbl$decile))
    col[idx] <- vals
    wide[[scn]] <- col
  }

  e <- echarts4r::e_charts(wide, decile, reorder = FALSE, height = height)
  # Dodged bars per scenario, one series each, Okabe-Ito order (wise_scale_fill_cat).
  e$x$opts$series <- lapply(seq_along(scen_levels), function(i) {
    list(
      name = scen_levels[[i]],
      type = "bar",
      data = lapply(seq_len(nrow(wide)), function(r) {
        list(
          value = list(levels(tbl$decile)[[r]], wide[[scen_levels[[i]]]][[r]]),
          itemStyle = list(
            color = unname(.wise_cat[(i - 1L) %% length(.wise_cat) + 1L]),
            borderColor = "rgba(0,0,0,0)"
          )
        )
      }),
      barGap = "10%",
      barMaxWidth = 42
    )
  })
  e$x$opts$xAxis <- list(
    type = "category",
    data = as.character(levels(tbl$decile)),
    name = "Fixed observed baseline welfare decile (1 = poorest)",
    nameLocation = "middle",
    nameGap = 28,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(),
    axisLine = list(lineStyle = list(color = .wise_grid)),
    axisTick = list(alignWithLabel = TRUE),
    splitLine = wise_esplit_line(show = FALSE)
  )
  e$x$opts$yAxis <- list(
    type = "value",
    name = y_label,
    nameTextStyle = wise_eaxis_name(),
    axisLabel = wise_eaxis_label(),
    splitLine = wise_esplit_line()
  )
  e$x$opts$legend <- modifyList(
    list(bottom = 0, left = 0, orient = "horizontal"),
    wise_elegend_style()
  )
  e$x$opts$tooltip <- list(
    trigger = "axis",
    axisPointer = list(type = "shadow")
  )
  e$x$opts$grid <- list(containLabel = TRUE, left = 8, right = 14, top = 30, bottom = 46)
  e$x$opts$color <- unname(.wise_cat)
  # Dashed zero reference line.
  if (length(e$x$opts$series)) {
    e$x$opts$series[[1L]]$markLine <- list(
      silent = TRUE,
      symbol = "none",
      lineStyle = list(color = .wise_zero, type = "dashed", width = 1),
      label = list(show = FALSE),
      data = list(list(yAxis = 0))
    )
  }
  wise_echart_theme(e)
}
