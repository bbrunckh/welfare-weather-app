#' Select the simulated year corresponding to a weather basis.
#'
#' @noRd
select_decomp_weather_basis <- function(decomp_df, basis = "mean", so = NULL) {
  if (is.null(decomp_df) || !is.data.frame(decomp_df) || !nrow(decomp_df) ||
      identical(basis, "mean") || !"sim_year" %in% names(decomp_df) ||
      !"delta_total" %in% names(decomp_df)) {
    return(decomp_df)
  }

  target <- if (identical(basis, "adverse_10")) 0.10 else 0.05
  year_total <- tapply(seq_len(nrow(decomp_df)), decomp_df$sim_year, function(idx) {
    vals <- as.numeric(decomp_df$delta_total[idx])
    w <- if ("weight" %in% names(decomp_df)) as.numeric(decomp_df$weight[idx]) else rep(1, length(idx))
    ok <- is.finite(vals) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    stats::weighted.mean(vals[ok], w[ok])
  })
  year_total <- year_total[is.finite(year_total)]
  if (!length(year_total)) return(decomp_df)

  adverse_high <- identical(
    outcome_direction(so$name %||% "welfare", so$type %||% "numeric"),
    "lower_is_better"
  )
  ordered <- order(year_total, decreasing = adverse_high)
  take <- max(1L, min(length(ordered), round(length(ordered) * target)))
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

.compact_decomp_values <- function(decomp_df, is_rif = FALSE) {
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
  } else rep(NA_integer_, nrow(decomp_df))
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
  } else rep(1, nrow(decomp_df))
  weights[!is.finite(weights) | weights < 0] <- NA_real_
  values <- .compact_decomp_values(decomp_df, is_rif = is_rif)
  metadata <- list(
    scenario = as.character(scenario),
    sim_year = sim_year,
    year_start = year_start,
    year_end = year_end
  )
  channel <- as.data.frame(c(metadata, .compact_decomp_stats(values, weights)),
                           stringsAsFactors = FALSE)
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
  deciles_out <- if (length(decile_rows)) dplyr::bind_rows(decile_rows) else
    data.frame()
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
                             drop = FALSE])
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
  if (!.is_compact_decomp_scenarios(x)) return(character(0))
  x$scenario_order %||% unique(as.character(x$channel_summary$scenario))
}

.compact_future_year <- function(x, scenario, basis = "mean", so = NULL) {
  rows <- x$channel_summary[
    as.character(x$channel_summary$scenario) == as.character(scenario),
    , drop = FALSE
  ]
  if (!nrow(rows) || identical(basis, "mean")) return(rows)
  annual <- rows
  total <- annual$sum_delta_total / annual$weight_delta_total
  annual <- annual[is.finite(total), , drop = FALSE]
  total <- total[is.finite(total)]
  if (!nrow(annual) || !length(total)) return(rows)
  adverse_high <- identical(
    outcome_direction(so$name %||% "welfare", so$type %||% "numeric"),
    "lower_is_better"
  )
  ordered <- order(total, decreasing = adverse_high)
  take <- max(1L, min(length(ordered), round(length(ordered) *
    if (identical(basis, "adverse_10")) 0.10 else 0.05)))
  annual[ordered[[take]], , drop = FALSE]
}

.compact_future_combine <- function(rows, prefix = "") {
  if (is.null(rows) || !nrow(rows)) return(setNames(numeric(0), character(0)))
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
  if (is.null(rows) || !nrow(rows)) return(data.frame())
  values <- .compact_future_combine(rows)
  out <- as.data.frame(as.list(values), stringsAsFactors = FALSE)
  out$weight <- 1
  out$delta_res1 <- values[["delta_res1"]]
  out
}

.compact_future_summary <- function(x, scenario, basis = "mean", so = NULL,
                                    is_rif = x$is_rif) {
  rows <- .compact_future_year(x, scenario, basis, so)
  if (!nrow(rows)) return(decomposition_summary_data(NULL, is_rif))
  decomposition_summary_data(
    .compact_future_as_decomp(rows, is_rif = is_rif), is_rif = is_rif
  )
}

.compact_future_decile_summary <- function(x, scenario, basis = "mean",
                                           so = NULL, is_rif = x$is_rif) {
  rows <- x$decile_summary[
    as.character(x$decile_summary$scenario) == as.character(scenario),
    , drop = FALSE
  ]
  selected <- .compact_future_year(x, scenario, basis, so)
  if (nrow(selected) && !identical(basis, "mean")) {
    rows <- rows[rows$sim_year %in% selected$sim_year, , drop = FALSE]
  }
  if (!nrow(rows)) return(tibble::tibble())
  out <- dplyr::bind_rows(lapply(sort(unique(rows$decile)), function(d) {
    group <- rows[rows$decile == d, , drop = FALSE]
    values <- .compact_future_combine(group)
    tibble::tibble(
      decile = as.integer(d),
      level_log = values[["delta_main"]],
      resilience_log = values[["delta_res"]],
      total_log = values[["delta_total"]],
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
  out
}

.prepare_decomp_adverse_bases <- function(weather_raw, hist_sim, so) {
  raw <- step2_resolve_weather(weather_raw, hist_sim)
  if (is.null(raw) || !nrow(raw)) return(list())
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
    if (is.null(weights)) mean(vals, na.rm = TRUE) else
      stats::weighted.mean(vals, weights, na.rm = TRUE)
  }, numeric(1L))
  agg <- annual_values[names(annual_values) %in% names(year_values)]
  if (!length(agg)) return(out)
  adverse_high <- identical(
    outcome_direction(so$name %||% "welfare", so$type %||% "numeric"),
    "lower_is_better"
  )
  ordered <- order(agg, decreasing = adverse_high)
  probabilities <- c(adverse_5 = 0.20, adverse_10 = 0.10, adverse_20 = 0.05)
  for (basis in names(probabilities)) {
    take <- max(1L, min(length(ordered), round(length(ordered) * probabilities[[basis]])))
    out[[basis]] <- year_values[[names(agg)[ordered[[take]]]]]
  }
  out
}

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
    shiny::h4(
      "What drives the total policy effect?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      pill_toggle(
        ns("decomp_weather_basis"),
        label = "Weather-year basis",
        choices = c(
          "Mean" = "mean",
          "Adverse 1-in-10" = "adverse_10",
          "Adverse 1-in-20" = "adverse_20"
        ),
        selected = "mean",
        layout = "horizontal"
      ),
      wise_plot_output(ns("headline_decomp_plot"),
                       "Main effect, resilience, and total policy effect decomposition",
                       height = "360px"),
      # Table starts collapsed: the plot carries the message, the table is
      # there for anyone who wants the numbers.
      shiny::tags$button(
        class = "btn btn-sm btn-outline-secondary mt-2",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", ns("headline_decomp_table_wrap")),
        `aria-expanded` = "false",
        `aria-controls` = ns("headline_decomp_table_wrap"),
        "Show / hide data table"
      ),
      shiny::div(
        id = ns("headline_decomp_table_wrap"),
        class = "collapse",
        shiny::div(
          style = "margin-top: 10px;",
          DT::DTOutput(ns("headline_decomp_table")),
          shiny::uiOutput(ns("headline_decomp_note_ui"))
        )
      )
    ),

    shiny::h4(
      "Who gains, and through which channel?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::div(
        style = "display: flex; gap: 14px; flex-wrap: wrap; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          ns("decile_weather_basis"),
          label = "Weather-year basis",
          choices = c("Mean" = "mean", "Adverse 1-in-10" = "adverse_10", "Adverse 1-in-20" = "adverse_20"),
          selected = "mean",
          layout = "horizontal"
        ),
        shiny::uiOutput(ns("decile_scenario_ui"))
      ),
      wise_plot_output(ns("decomp_bar_plot"),
                       "Stacked policy-effect channels by baseline welfare decile",
                       height = "450px"),
      shiny::uiOutput(ns("decomp_bar_note_ui"))
    ),

    shiny::uiOutput(ns("beta_curve_ui")),

    shiny::h4(
      "How do decomposition channels vary by weather year?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      DT::DTOutput(ns("decomp_summary_table")),
      shiny::uiOutput(ns("interaction_warning_ui")),
      shiny::tags$p(class = "diagnostic-note",
                    "Columns show weighted policy effects under mean weather and adverse historical weather years. Coefficient SE is the delta-method standard error from the fitted coefficient covariance; it is not a 95% confidence interval. Residual and survey-sampling uncertainty are not included.")
    )
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
                                            decomp_result     = reactive(NULL),
                                            decomp_scenarios  = reactive(list()),
                                            decomp_context    = reactive(NULL),
                                           model_fit         = reactive(NULL),
                                           variable_list     = reactive(NULL),
                                           so                = reactive(NULL),
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
                                            stale = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session$userData$wise_step3_stale <- stale
    output$stale_banner_ui <- shiny::renderUI({
      if (isTRUE(stale())) .stale_banner(
        "Step 3 policy decomposition",
        note = NULL
      ) else NULL
    })

    is_rif <- reactive({
      mf <- model_fit()
      !is.null(mf) && identical(tolower(as.character(mf$engine %||% "")), "rif")
    })
    baseline_deciles <- reactive({
      ctx <- decomp_context()
      if (is.null(ctx)) NULL else if (exists(
        ".decomposition_context_baseline_deciles", mode = "function"
      )) {
        .decomposition_context_baseline_deciles(ctx)
      } else ctx$baseline_deciles
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

    get_label <- function(var_name) {
      vl <- if (is.function(variable_list)) variable_list() else variable_list
      if (is.null(vl) || is.null(var_name) || length(var_name) == 0) {
        return(if (is.null(var_name)) "" else as.character(var_name))
      }
      idx <- match(var_name, vl$name)
      if (length(idx) == 0 || is.na(idx)) var_name
      else as.character(vl$label[idx])
    }

    weather_basis_label <- reactive({
      switch(input$decomp_weather_basis %||% "mean",
             mean = "mean historical-baseline weather",
             adverse_10 = "adverse 1-in-10 historical weather year",
             adverse_20 = "adverse 1-in-20 historical weather year",
             "mean historical-baseline weather")
    })

    historical_weather_basis_for_probability <- function(target_p) {
      ctx <- decomp_context()
      if (!is.null(ctx) && !is.null(ctx$adverse_bases)) {
        key <- if (is.null(target_p)) "mean" else if (identical(target_p, 0.20)) "adverse_5" else if (identical(target_p, 0.10)) "adverse_10" else "adverse_20"
        cached <- if (exists(".decomposition_context_adverse_basis",
                            mode = "function")) {
          .decomposition_context_adverse_basis(ctx, key)
        } else ctx$adverse_bases[[key]]
        if (!is.null(cached)) return(cached)
      }
      hs <- baseline_hist_sim()
      if (is.null(hs) || is.null(hs$weather_raw)) return(NULL)
      raw <- step2_resolve_weather(hs$weather_raw, hs)
      if (is.null(raw) || !nrow(raw)) return(NULL)
      if (is.null(target_p)) return(raw)
      if (!"timestamp" %in% names(raw)) return(raw)
      years <- as.integer(format(raw$timestamp, "%Y"))
      year_values <- split(raw, years)
      pipe <- hs$pipeline
      if (is.null(pipe) || is.null(pipe$y_point) || is.null(pipe$sim_year)) return(raw)
      simulated_years <- split(seq_along(pipe$y_point), pipe$sim_year)
      annual_values <- vapply(simulated_years, function(idx) {
        vals <- as.numeric(pipe$y_point[idx])
        weights <- if (!is.null(pipe$weight)) as.numeric(pipe$weight[idx]) else NULL
        if (is.null(weights)) mean(vals, na.rm = TRUE) else
          stats::weighted.mean(vals, weights, na.rm = TRUE)
      }, numeric(1L))
      # Select the most adverse observed annual outcome using the same metric
      # direction contract as the Results plots.
      agg <- annual_values[names(annual_values) %in% names(year_values)]
      adverse_high <- identical(
        outcome_direction(hs$so$name %||% "welfare", hs$so$type %||% "numeric"),
        "lower_is_better"
      )
      ordered <- order(agg, decreasing = adverse_high)
      take <- max(1L, min(length(ordered), round(length(ordered) * target_p)))
      year_values[[names(agg)[ordered[[take]]]]]
    }

    historical_weather_for_basis <- function(basis) {
      if (identical(basis, "mean")) return(historical_weather_basis_for_probability(NULL))
      historical_weather_basis_for_probability(
        if (identical(basis, "adverse_10")) 0.10 else 0.05
      )
    }
    historical_weather_basis <- reactive({
      historical_weather_for_basis(input$decomp_weather_basis %||% "mean")
    })

    decomp_for_basis <- function(basis) {
      if (identical(basis, "mean")) return(decomp_result())
      ctx <- decomp_context()
      cached_result <- if (!is.null(ctx)) {
        key <- if (identical(basis, "adverse_10")) "adverse_10" else "adverse_20"
        if (exists(".decomposition_context_adverse_result", mode = "function")) {
          .decomposition_context_adverse_result(ctx, key)
        } else ctx$adverse_decompositions[[key]]
      } else NULL
      if (!is.null(cached_result)) return(cached_result)
      hs <- baseline_hist_sim()
      svy_b <- baseline_svy(); svy_p <- policy_svy(); mf <- model_fit()
      if (is.null(hs) || is.null(svy_b) || is.null(svy_p) || is.null(mf)) return(decomp_result())
      tryCatch(
        decompose_policy_effect(
          svy_baseline = svy_b, svy_policy = svy_p, model_fit = mf,
          so = hs$so, weather_raw = historical_weather_for_basis(basis),
          skip_coef = !isTRUE(show_coef_uncertainty()),
          context = decomp_context(),
          run_identity = if (!is.null(decomp_context())) decomp_context()$run_identity else NULL
        ),
        error = function(e) decomp_result()
      )
    }
    selected_decomp_result <- reactive({
      decomp_for_basis(input$decomp_weather_basis %||% "mean")
    })

    headline_decomp_data <- reactive({
      basis <- input$decomp_weather_basis %||% "mean"
      outcome <- so() %||% list()
      hist <- decomposition_summary_data(selected_decomp_result(), is_rif())
      hist$scenario <- "Historical"
      sc <- decomp_scenarios()
      if (.is_compact_decomp_scenarios(sc)) {
        scenarios <- .compact_future_scenarios(sc)
        if (!length(scenarios)) return(hist)
        future <- dplyr::bind_rows(lapply(sort(scenarios), function(scenario) {
          out <- .compact_future_summary(sc, scenario, basis, outcome, is_rif())
          out$scenario <- scenario
          out
        }))
      } else {
        if (is.null(sc) || !is.data.frame(sc) || !nrow(sc)) return(hist)
        future <- dplyr::bind_rows(lapply(split(sc, sc$scenario), function(x) {
          x <- select_decomp_weather_basis(x, basis, outcome)
          out <- decomposition_summary_data(x, is_rif())
          out$scenario <- as.character(x$scenario[[1L]])
          out
        }))
      }
      dplyr::bind_rows(hist, future)
    })
    output$headline_decomp_note_ui <- renderUI({
      shiny::tags$p(class = "diagnostic-note", paste0(
        "This figure summarizes the policy effect using ", weather_basis_label(),
        ". It is not a future climate-scenario result."
      ))
    })
    output$headline_decomp_plot <- renderPlot({
      req(headline_decomp_data())
      plot_decomposition_headline(headline_decomp_data())
    }, height = 360)
    # The Decomposition UI is inserted after the server starts. Keep plots
    # live before their DOM nodes exist so they render immediately on tab open.
    outputOptions(output, "headline_decomp_plot", suspendWhenHidden = FALSE)
    output$headline_decomp_table <- DT::renderDT({
      req(headline_decomp_data())
      tbl <- headline_decomp_data()
      tbl <- tbl[tbl$channel_id %in% c("level", "resilience", "total"), , drop = FALSE]
      tbl <- data.frame(
        Scenario = tbl$scenario,
        `Effect component` = tbl$channel,
        `Mean effect (%)` = round(tbl$percent, 2),
        `Share of total (%)` = round(100 * tbl$share_of_total, 1),
        check.names = FALSE
      )
      DT::datatable(
        tbl,
        rownames = FALSE, class = "compact stripe",
        extensions = "Buttons",
        options = list(dom = wise_csv_dom("t"),
                       buttons = wise_csv_button("policy_decomposition_headline_data", enabled = !isTRUE(stale())))
      )
    })
    outputOptions(output, "headline_decomp_table", suspendWhenHidden = FALSE)
    wise_export_figure(
      key = "policy_decomposition_headline",
      label = "Headline main effect and resilience decomposition",
      step = 3L,
      fun = function() plot_decomposition_headline(headline_decomp_data()),
      description = "Headline decomposition into main effect, resilience, and total effects with reconciliation on the model scale.",
      width = 9, height = 5
    )
    wise_export_table(
      key = "policy_decomposition_headline_data",
      label = "Headline decomposition data",
      step = 3L,
      fun = function() {
        out <- headline_decomp_data()
        out <- out[out$channel_id %in% c("level", "resilience", "total"), , drop = FALSE]
        data.frame(
          Scenario = out$scenario,
          `Effect component` = out$channel,
          `Mean effect (%)` = round(out$percent, 2),
          `Share of total (%)` = round(100 * out$share_of_total, 1),
          check.names = FALSE
        )
      },
      description = "Mean-historical-weather decomposition into main effect, resilience, and total policy effects."
    )

    # --- Stacked bar chart by decile ---
    # UI-48: register Step 3's decomposition figures for the export bundle.
    wise_export_figure(
      key   = "policy_decomposition_channels",
      label = "Policy effect by channel",
      step  = 3L,
      fun   = function() {
        res <- selected_decomp_result()
        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) return(NULL)
          plot_decomposition_channels_by_decile(
            decomposition_channels_by_decile(res, baseline_svy(), so()$name %||% "welfare", is_rif(), baseline_deciles()),
            is_rif()
        )
      },
      description = paste(
        "The policy effect split into its main effect and resilience",
        "channels (repositioning and weather interaction)."
      ),
      width = 9, height = 6
    )
    wise_export_table(
      key = "policy_decomposition_channels_by_decile",
      label = "Decomposition channels by baseline decile",
      step = 3L,
       fun = function() decomposition_decile_export(
          decomposition_channels_by_decile(
            selected_decomp_result(), baseline_svy(), so()$name %||% "welfare", is_rif(), baseline_deciles()
         ),
         is_rif()
       ),
        description = "Engine-specific decomposition channels, total, and counts by fixed weighted baseline welfare decile."
    )

    output$decile_scenario_ui <- shiny::renderUI({
      sc <- decomp_scenarios()
      choices <- c("Historical" = "Historical")
      if (.is_compact_decomp_scenarios(sc)) {
        future <- .compact_future_scenarios(sc)
        choices <- c(choices, stats::setNames(future, future))
      } else if (is.data.frame(sc) && nrow(sc) && "scenario" %in% names(sc)) {
        future <- unique(as.character(sc$scenario))
        choices <- c(choices, stats::setNames(future, future))
      }
      pill_toggle(ns("decile_scenario"), label = "Scenario", choices = choices,
                  selected = isolate(input$decile_scenario) %||% "Historical",
                  layout = "horizontal")
    })

    decile_decomp_data <- reactive({
      outcome <- so()
      if (is.null(outcome)) {
        return(tibble::tibble())
      }
      scenario <- input$decile_scenario %||% "Historical"
      basis <- input$decile_weather_basis %||% "mean"
      if (identical(scenario, "Historical")) {
        res <- decomp_for_basis(basis)
      } else if (.is_compact_decomp_scenarios(decomp_scenarios())) {
        return(.compact_future_decile_summary(
          decomp_scenarios(), scenario, basis, outcome, is_rif()
        ))
      } else {
        sc <- decomp_scenarios()
        res <- if (is.data.frame(sc) && nrow(sc))
          sc[sc$scenario == scenario, , drop = FALSE] else NULL
        res <- select_decomp_weather_basis(res, basis, outcome)
      }
      if (is.null(res) || !is.data.frame(res) || !nrow(res)) return(tibble::tibble())
      decomposition_channels_by_decile(
        res, baseline_svy(), outcome$name %||% "welfare", is_rif(), baseline_deciles()
      )
    })

    output$decomp_bar_plot <- shiny::renderPlot({
      plot_decomposition_channels_by_decile(
        decile_decomp_data(),
        is_rif()
      )
    }, height = 450)
    outputOptions(output, "decomp_bar_plot", suspendWhenHidden = FALSE)
    wise_export_figure(
      key = "policy_decomposition_channels_selected",
      label = "Selected policy decomposition channels",
      step = 3L,
      fun = function() {
        plot_decomposition_channels_by_decile(
          decile_decomp_data(),
          is_rif()
        )
      },
      description = "Policy-effect channels by fixed baseline welfare decile for the selected scenario and weather basis.",
      width = 9, height = 6
    )
    output$decomp_bar_note_ui <- renderUI({
      basis <- input$decile_weather_basis %||% "mean"
      basis_label <- switch(
        basis,
        adverse_10 = "adverse 1-in-10 historical weather year",
        adverse_20 = "adverse 1-in-20 historical weather year",
        "mean historical-baseline weather"
      )
      shiny::tags$p(
        class = "diagnostic-note",
        paste0(
          "Decile 1 is the poorest. Stacked bars separate direct transfer, covariate shift, weather-policy interaction, and - for RIF models only - repositioning. The marker shows the total policy effect for ",
          basis_label, " under the ", input$decile_scenario %||% "Historical",
          " scenario. Deciles are fixed from weighted observed baseline welfare."
        )
      )
    })

    # --- Beta curve (RIF only): one panel per weather variable -------------
    output$beta_curve_ui <- renderUI({
      if (!is_rif()) return(NULL)
      mf <- model_fit()
      if (is.null(mf$rif_grid)) return(NULL)
      n_vars <- length(mf$weather_terms %||% character(0))
      if (n_vars == 0) return(NULL)

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
              ids    = c("beta_curve_plot1", "beta_curve_plot2"),
              height = "400px",
              alts   = paste("Beta curve plot: unconditional quantile regression weather",
                             "sensitivity across welfare quantiles for",
                             mf$weather_terms)
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
      shiny::renderPlot({
        req(is_rif(), model_fit())
        mf <- model_fit()
        req(length(mf$weather_terms) >= idx)
        make_weather_effect_plot(
          fit               = NULL,
          pred_var          = mf$weather_terms[idx],
          interaction_terms = mf$interaction_terms %||% character(0),
          is_binned         = FALSE,
          label_fun         = get_label,
          engine            = "rif",
          rif_grid          = mf$rif_grid
        )
      })
    }

    output$beta_curve_plot1 <- .render_beta_curve(1L)
    output$beta_curve_plot2 <- .render_beta_curve(2L)
    outputOptions(output, "beta_curve_plot1", suspendWhenHidden = FALSE)
    outputOptions(output, "beta_curve_plot2", suspendWhenHidden = FALSE)

    for (idx in seq_len(2L)) local({
      i <- idx
      wise_export_figure(
        key = paste0("policy_rif_weather_curve_", i),
        label = paste("RIF weather-sensitivity curve", i),
        step = 3L,
        fun = function() {
          req(is_rif(), model_fit())
          mf <- model_fit()
          req(length(mf$weather_terms) >= i)
          make_weather_effect_plot(
            fit = NULL, pred_var = mf$weather_terms[i],
            interaction_terms = mf$interaction_terms %||% character(0),
            is_binned = FALSE, label_fun = get_label,
            engine = "rif", rif_grid = mf$rif_grid
          )
        },
        description = "RIF weather coefficient by baseline welfare quantile; interpolation is limited to the estimated grid.",
        width = 9, height = 6
      )
    })

    technical_decomp_table <- reactive({
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

    # --- Summary table ---
    output$decomp_summary_table <- DT::renderDT({
      req(technical_decomp_table())
      tbl <- technical_decomp_table()
      numeric_targets <- if (ncol(tbl) > 1L) seq.int(1L, ncol(tbl) - 1L) else integer(0)
      DT::datatable(
        tbl,
        rownames = FALSE, class = "compact stripe", extensions = "Buttons",
        options = list(
          dom = wise_csv_dom("t"), ordering = FALSE,
          buttons = wise_csv_button("policy_decomposition_summary", enabled = !isTRUE(stale())),
          columnDefs = list(list(className = "dt-right", targets = numeric_targets))
        )
      )
    })
    outputOptions(output, "decomp_summary_table", suspendWhenHidden = FALSE)
    wise_export_table(
      key = "policy_decomposition_summary",
      label = "Policy decomposition summary",
      step = 3L,
      fun = function() {
        technical_decomp_table()
      },
      description = "Weighted policy-effect decomposition under mean weather and adverse historical weather years."
    )

    # --- Interaction warning ---
    output$interaction_warning_ui <- renderUI({
      res <- decomp_result()
      if (is.null(res)) return(NULL)
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
    if (!has_sd || is.null(decomp_df[[sd_col]])) return(NA_real_)
    sqrt(sum((w_norm^2) * (decomp_df[[sd_col]])^2, na.rm = TRUE))
  }

  summary_row <- function(label, vals, sd_col = NULL) {
    mean_log <- stats::weighted.mean(vals, w, na.rm = TRUE)
    mean_pct <- (exp(mean_log) - 1) * 100
    se_log   <- if (is.null(sd_col)) NA_real_ else agg_se(sd_col)
    se_pct   <- if (is.na(se_log)) NA_real_ else abs(exp(mean_log)) * se_log * 100
    data.frame(
      Channel           = label,
      `Mean (log-pts)`  = round(mean_log, 4),
      `+/- SE (log-pts)`  = if (is.na(se_log)) NA_real_ else round(se_log, 4),
      `Mean (%)`        = round(mean_pct, 2),
      `+/- SE (%)`        = if (is.na(se_pct)) NA_real_ else round(se_pct, 2),
      `Median (%)`      = round(median((exp(vals) - 1) * 100), 2),
      check.names = FALSE
    )
  }

  rows <- list(
    summary_row("Total effect", decomp_df$delta_total, sd_col = "sd_total"),
    summary_row("Main effect (direct transfer and covariate shift)",
                decomp_df$delta_main, sd_col = "sd_main"),
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
  if (is.null(bases) || !length(bases)) return(data.frame())
  tables <- lapply(bases, function(x) {
    if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(NULL)
    .build_decomp_table(x, is_rif)
  })
  available <- Filter(Negate(is.null), tables)
  if (!length(available)) return(data.frame())
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
