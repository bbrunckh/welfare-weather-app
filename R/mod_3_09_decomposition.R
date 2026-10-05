# Weather-year basis: "mean" pools every simulated year; adverse bases pick the
# 1-in-N worst baseline year within each scenario-model, using the same rule as
# Step 2 and Step 3 results (`adverse_year_support()`: rank interpolation on the
# baseline annual mean outcome, direction from the outcome) and then average the
# models equally.
.decomp_basis_probability <- c(adverse_5 = 0.20, adverse_10 = 0.10, adverse_20 = 0.05)

.decomp_basis_choices <- c(
  "Mean" = "mean", "Adverse 1-in-5" = "adverse_5",
  "Adverse 1-in-10" = "adverse_10", "Adverse 1-in-20" = "adverse_20"
)

.decomp_scenario_rows <- function(x, scenario, decile = FALSE) {
  rows <- if (decile) x$decile_summary else x$channel_summary
  rows[as.character(rows$scenario) == as.character(scenario), , drop = FALSE]
}

# One support per scenario-model, ranked on the all-household baseline level.
.decomp_adverse_supports <- function(x, scenario, basis, so) {
  rows <- .decomp_scenario_rows(x, scenario)
  probability <- .decomp_basis_probability[[basis]]
  tail <- metric_metadata("mean", so)$adverse_tail
  members <- unique(as.character(rows$member))
  supports <- lapply(members, function(m) {
    r <- rows[as.character(rows$member) == m, , drop = FALSE]
    r <- r[order(r$sim_year), , drop = FALSE]
    adverse_year_support(r$sum_baseline_level / r$weight_baseline_level,
      as.numeric(r$sim_year), probability, tail)
  })
  stats::setNames(supports, members)
}

# Equal-model average of the channel values (and baseline level) at each
# model's adverse year(s). NULL when any model lacks support.
.decomp_adverse_values <- function(rows, supports) {
  if (!length(supports)) return(NULL)
  fields <- c(.compact_decomp_channels, .compact_level_channels, "baseline_level")
  by_model <- lapply(names(supports), function(m) {
    r <- rows[as.character(rows$member) == m, , drop = FALSE]
    vapply(fields, function(f) {
      value <- r[[paste0("sum_", f)]] / r[[paste0("weight_", f)]]
      apply_adverse_year_support(value, as.numeric(r$sim_year), supports[[m]])$value
    }, numeric(1))
  })
  values <- colMeans(do.call(rbind, by_model))
  if (anyNA(values)) NULL else values
}

# One-row decomposition frame for a scenario and weather basis (empty when the
# basis is unavailable, e.g. too few simulated years for the return period).
.compact_future_decomp <- function(x, scenario, basis = "mean", so = NULL) {
  rows <- .decomp_scenario_rows(x, scenario)
  if (!nrow(rows)) return(data.frame())
  if (identical(basis, "mean")) {
    values <- .compact_future_combine(rows)
    baseline <- .compact_member_mean(rows, "sum_baseline_level", "weight_baseline_level")
  } else {
    values <- .decomp_adverse_values(rows, .decomp_adverse_supports(x, scenario, basis, so))
    if (is.null(values)) return(data.frame())
    baseline <- values[["baseline_level"]]
  }
  out <- as.data.frame(as.list(values[c(.compact_decomp_channels, .compact_level_channels)]),
    stringsAsFactors = FALSE)
  out$weight <- 1
  out$baseline_annual <- baseline
  out
}

.compact_future_decile_summary <- function(x, scenario, basis = "mean",
                                           so = NULL, is_rif = x$is_rif) {
  rows <- .decomp_scenario_rows(x, scenario, decile = TRUE)
  if (!nrow(rows)) return(tibble::tibble())
  supports <- if (identical(basis, "mean")) NULL else .decomp_adverse_supports(x, scenario, basis, so)
  out <- dplyr::bind_rows(lapply(sort(unique(rows$decile)), function(d) {
    group <- rows[rows$decile == d, , drop = FALSE]
    if (identical(basis, "mean")) {
      values <- .compact_future_combine(group)
      baseline <- .compact_member_mean(group, "sum_baseline_level", "weight_baseline_level")
    } else {
      values <- .decomp_adverse_values(group, supports)
      if (is.null(values)) return(NULL)
      baseline <- values[["baseline_level"]]
    }
    tibble::tibble(
      decile = as.integer(d),
      level_log = values[["delta_main"]],
      resilience_log = values[["delta_res"]],
      total_log = values[["delta_total"]],
      main = values[["lvl_main"]],
      repositioning = values[["lvl_res1"]],
      interaction = values[["lvl_res2"]],
      total = values[["lvl_total"]],
      level_percent = log_effect_to_percent(values[["delta_main"]]),
      resilience_percent = log_effect_to_percent(values[["delta_res"]]),
      main_percent = log_effect_to_percent(values[["delta_main"]]),
      cash_transfer_percent = log_effect_to_percent(values[["delta_sp"]]),
      covariate_shift_percent = log_effect_to_percent(values[["delta_main_covar"]]),
      repositioning_percent = log_effect_to_percent(values[["delta_res1"]]),
      interaction_percent = log_effect_to_percent(values[["delta_res2"]]),
      total_percent = log_effect_to_percent(values[["delta_total"]]),
      n_households = sum(group$n_households),
      weighted_population = sum(group$weighted_population),
      baseline_annual = baseline
    )
  }))
  if (!nrow(out)) tibble::tibble() else out
}

.compact_decomp_channels <- c(
  "delta_total", "delta_main", "delta_sp", "delta_main_covar",
  "delta_res", "delta_res1", "delta_res2"
)

# Main / repositioning / interaction / total in outcome units, converted per
# household before aggregation (see `.policy_level_channels()`).
.compact_level_channels <- c("lvl_main", "lvl_res1", "lvl_res2", "lvl_total")

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

# Equal-model mean of one summed/weighted statistic: each member's years are
# pooled first (weighted mean over its simulated years), then members are
# averaged equally, matching the Step 2 and Step 3 results. Rows without a
# `member` column (single-source frames) are treated as one member.
.compact_member_mean <- function(rows, sum_col, weight_col) {
  sums <- rows[[sum_col]]
  weights <- rows[[weight_col]]
  ok <- is.finite(sums) & is.finite(weights) & weights > 0
  if (!any(ok)) return(NA_real_)
  member <- if ("member" %in% names(rows)) as.character(rows$member) else rep("", nrow(rows))
  by_member <- vapply(split(seq_along(ok)[ok], member[ok]),
    function(i) sum(sums[i]) / sum(weights[i]), numeric(1))
  mean(by_member)
}

.compact_future_combine <- function(rows, prefix = "") {
  if (is.null(rows) || !nrow(rows)) {
    return(setNames(numeric(0), character(0)))
  }
  channels <- c(.compact_decomp_channels, .compact_level_channels)
  out <- setNames(numeric(length(channels)), channels)
  for (name in channels) {
    out[[name]] <- .compact_member_mean(
      rows, paste0(prefix, "sum_", name), paste0(prefix, "weight_", name))
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
    shiny::uiOutput(ns("units_ui")),
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
        "Effects are weighted means by baseline welfare decile, in the outcome's units or percent of baseline as selected. Uncertainty is not shown.")
    ),
    shiny::uiOutput(ns("beta_curve_ui"))
  )
}

#' 3_09_decomposition Server Functions
#'
#' @param id Module id.
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
    raw_decomp_scenarios <- decomp_scenarios
    policy_method_status <- reactive({
      mf <- model_fit()
      .policy_endpoint_status(so(), decomp_context() %||% mf)
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

    # Pooled-year sentence (mean) or the shared adverse-year rule (see
    # `.decomp_basis_probability`), for the figure notes.
    weather_basis_text <- function(basis = "mean", outcome = "the outcome") {
      p <- .decomp_basis_probability[basis]
      if (is.na(p)) return("the mean across simulated weather years (models weighted equally)")
      paste0(
        "the adverse 1-in-", round(1 / p), " weather year, ranked on baseline mean ",
        outcome, " within each scenario-model (models weighted equally), as in Results"
      )
    }
    weather_basis_unavailable <- function(basis, has_data) {
      p <- .decomp_basis_probability[basis]
      if (is.na(p) || has_data) return(NULL)
      shiny::tags$p(class = "alert alert-warning", paste0(
        "Adverse 1-in-", round(1 / p), " is unavailable: it needs at least ",
        ceiling(1 / min(p, 1 - p)), " simulated weather years in every model of this scenario."
      ))
    }
    outcome_label_text <- function() {
      metric <- outcome_metric()
      metric$outcome_label %||% metric$outcome_name %||% so()$name %||% "outcome"
    }

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
      selected <- isolate(input$headline_scenario) %||% scenarios[[1L]]
      if (!selected %in% scenarios) selected <- scenarios[[1L]]
      pill_toggle(ns("headline_scenario"), label = "Scenario",
        choices = stats::setNames(scenarios, scenarios),
        selected = selected,
        layout = "horizontal")
    })
    output$headline_scenario_ui <- headline_scenario_ui
    output$headline_weather_basis_ui <- shiny::renderUI({
      pill_toggle(ns("headline_weather_basis"), label = "Weather-year basis",
        choices = .decomp_basis_choices,
        selected = isolate(input$headline_weather_basis) %||% "mean",
        layout = "horizontal")
    })
    output$decile_weather_basis_ui <- shiny::renderUI({
      pill_toggle(ns("decile_weather_basis"), label = "Weather-year basis",
        choices = .decomp_basis_choices,
        selected = isolate(input$decile_weather_basis) %||% "mean",
        layout = "horizontal")
    })
    # Every scenario (Historical included) reads the per-year compact channels,
    # so Mean is the equal-model mean over simulated years, as in Steps 2 and 3.
    hist_label <- function() baseline_hist_sim()$hist_label %||% "Historical"
    # Compact channels are already in outcome units, so they bypass the
    # log-to-level conversion applied to household-level decompositions.
    so_levels <- function() {
      out <- so()
      out$transform <- "none"
      out
    }
    compact_decomp_data <- function(scenario, basis) {
      compact <- decomp_scenarios()
      if (!.is_compact_decomp_scenarios(compact)) return(data.frame())
      res <- .compact_future_decomp(compact, scenario, basis, so())
      if (!nrow(res)) return(res)
      data.frame(delta_main = res$lvl_main, delta_res1 = res$lvl_res1,
        delta_res2 = res$lvl_res2, delta_total = res$lvl_total, weight = 1)
    }
    headline_decomp_res <- reactive({
      if (isTRUE(stale()) || !identical(policy_method_status()$status, "ok")) return(data.frame())
      scenario <- input$headline_scenario %||% hist_label()
      basis <- input$headline_weather_basis %||% "mean"
      compact_decomp_data(scenario, basis)
    })
    headline_decomp_data <- reactive({
      scenario <- input$headline_scenario %||% hist_label()
      out <- .decomposition_outcome_summary(headline_decomp_res(),
        so_levels(), baseline_svy())
      if (nrow(out)) out$scenario <- scenario
      if (nrow(out) && is_relative()) {
        out <- .decomposition_as_relative(out,
          .decomposition_baseline_mean(so(), baseline_svy()), "value")
      }
      out
    })
    outcome_metric <- reactive({
      metric_metadata("mean", so(), poverty_line(), analysis_unit(), weighted = TRUE)
    })
    # Relative change (% of observed baseline mean) only for level-type outcomes
    # with a positive baseline; rates and indices stay in their native change units.
    relative_available <- reactive({
      identical(outcome_metric()$change_kind, "absolute") &&
        is.finite(.decomposition_baseline_mean(so(), baseline_svy()))
    })
    output$units_ui <- shiny::renderUI({
      if (!isTRUE(relative_available())) return(NULL)
      shiny::div(style = "display:flex; justify-content:flex-end; margin-bottom:8px;",
        pill_toggle(ns("decomp_units"), label = "Units",
          choices = c("Absolute (outcome units)" = "absolute", "% of baseline" = "relative"),
          selected = isolate(input$decomp_units) %||% "absolute",
          layout = "horizontal"))
    })
    is_relative <- reactive(isTRUE(relative_available()) &&
      identical(input$decomp_units %||% "absolute", "relative"))
    change_unit_text <- reactive({
      if (is_relative()) "% of baseline mean" else outcome_metric()$change_unit %||% "outcome units"
    })
    output$headline_decomp_note_ui <- renderUI({
      if (!identical(policy_method_status()$status, "ok")) {
        return(shiny::tags$p(class = "alert alert-warning", policy_method_status()$reason))
      }
      scenario <- input$headline_scenario %||% hist_label()
      basis <- input$headline_weather_basis %||% "mean"
      unit <- change_unit_text()
      outcome <- outcome_label_text()
      res <- headline_decomp_res()
      if (!is.data.frame(res) || !nrow(res)) {
        return(weather_basis_unavailable(basis, FALSE) %||%
          shiny::tags$p(class = "diagnostic-note", "Decomposition is unavailable for this selection."))
      }
      shiny::tags$p(class = "diagnostic-note", paste0(
        "Weighted average change in ", outcome, " (", unit, ") under ", scenario,
        " for ", weather_basis_text(basis, outcome), ".",
        if (is_relative()) " Percent of observed baseline mean, the same for all channels."
      ))
    })
    # Zero-arg echarts closures shared by the on-screen render and the
    # export bundle (guidelines §7 pattern).
    headline_decomp_chart <- function() {
      echart_outcome_decomposition_headline(
        headline_decomp_data(),
        y_label = paste0("Weighted average change (", change_unit_text(), ")"),
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
          `Change unit` = rep(change_unit_text(), n),
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

    decile_relative <- function(data) {
      if (!nrow(data) || !is_relative()) return(data)
      .decomposition_as_relative(data,
        .decomposition_baseline_mean(so(), baseline_svy(), data$decile),
        c("main", "repositioning", "interaction", "total"))
    }
    decile_decomp_data <- reactive({
      if (isTRUE(stale()) || !identical(policy_method_status()$status, "ok")) return(data.frame())
      scenario <- input$decile_scenario %||% hist_label()
      basis <- input$decile_weather_basis %||% "mean"
      compact <- decomp_scenarios()
      if (!.is_compact_decomp_scenarios(compact)) return(data.frame())
      deciles <- .compact_future_decile_summary(compact, scenario, basis, so(), is_rif())
      if (!nrow(deciles)) return(data.frame())
      decile_relative(.decomposition_outcome_deciles(data.frame(decile = deciles$decile,
        delta_main = deciles$main, delta_res1 = deciles$repositioning,
        delta_res2 = deciles$interaction, delta_total = deciles$total,
        weight = deciles$weighted_population), so_levels(), baseline_svy()))
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
        `Change unit` = rep(change_unit_text(), n),
        `Effect component` = data$channel,
        `Weighted average change` = data$value, check.names = FALSE),
        rownames = FALSE, class = "compact stripe")
    })
    outputOptions(output, "headline_decomp_table", suspendWhenHidden = TRUE)
    output$decile_decomp_table <- DT::renderDT({
      data <- decile_decomp_data()
      if (!nrow(data)) return(DT::datatable(data.frame(Message =
        "Decomposition data is not available for this selection."), rownames = FALSE, options = list(dom = "t")))
      unit <- change_unit_text()
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
        y_label = paste0("Weighted average change (", change_unit_text(), ")"),
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
        unit <- change_unit_text()
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
      outcome <- outcome_label_text()
      unavailable <- weather_basis_unavailable(basis, nrow(decile_decomp_data()) > 0L)
      if (!is.null(unavailable)) return(unavailable)
      shiny::tags$p(
        class = "diagnostic-note",
        paste0(
          "Bars: weighted average change in ", outcome, " (",
          change_unit_text(),
          ") by channel; marker: total, for ", input$decile_scenario %||% hist_label(),
          " under ", weather_basis_text(basis, outcome),
          ". Decile 1 is the poorest, fixed from weighted baseline welfare.",
          if (is_relative()) " Percent of each decile's observed baseline mean."
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

    # --- Interaction warning ---
    output$interaction_warning_ui <- renderUI({
      compact <- decomp_scenarios()
      if (!.is_compact_decomp_scenarios(compact) || !nrow(compact$channel_summary)) {
        return(NULL)
      }
      if (all(abs(compact$channel_summary$sum_delta_res2) < 1e-10, na.rm = TRUE)) {
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
