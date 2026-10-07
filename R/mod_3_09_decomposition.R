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
        reactable::reactableOutput(ns("headline_decomp_table"))
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
        reactable::reactableOutput(ns("decile_decomp_table"))
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
                                          selected_policies = reactive(NULL),
                                          policy_scenarios = reactive(list()),
                                          baseline_hist_sim = reactive(NULL),
                                          baseline_svy = reactive(NULL),
                                          selected_weather = reactive(NULL),
                                          sp_scenario = reactive(NULL),
                                          infra_scenario = reactive(NULL),
                                          digital_scenario = reactive(NULL),
                                          labor_scenario = reactive(NULL),
                                          education_scenario = reactive(NULL),
                                           policy_saved_scenarios = reactive(list()),
                                           stale = reactive(FALSE),
                                           poverty_line = reactive(NULL),
                                           analysis_unit = reactive(NULL)) {
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
    # export bundle (guidelines sec. 7 pattern).
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
    output$headline_decomp_table <- reactable::renderReactable({
      data <- headline_decomp_data()
      metric <- outcome_metric()
      if (!nrow(data)) return(.step2_reactable_note(
        "Decomposition data is not available for this selection."))
      n <- nrow(data)
      tab <- data.frame(Scenario = rep(data$scenario[[1L]], n),
        Metric = rep(metric$label %||% "Mean", n),
        Outcome = rep(metric$outcome_label %||% metric$outcome_name %||% "", n),
        `Change unit` = rep(change_unit_text(), n),
        `Effect component` = data$channel,
        `Weighted average change` = data$value, check.names = FALSE)
      .step2_reactable(tab, col_defs = list(
        `Weighted average change` = reactable::colDef(
          format = reactable::colFormat(digits = 4), minWidth = 70)))
    })
    outputOptions(output, "headline_decomp_table", suspendWhenHidden = TRUE)
    output$decile_decomp_table <- reactable::renderReactable({
      data <- decile_decomp_data()
      if (!nrow(data)) return(.step2_reactable_note(
        "Decomposition data is not available for this selection."))
      unit <- change_unit_text()
      channels <- data[c("main", "repositioning", "interaction", "total")]
      names(channels) <- paste(c("Main effect", "Repositioning", "Interaction", "Total policy effect"),
        paste0("(", unit, ")"))
      table <- stats::setNames(data.frame(data$decile, channels, check.names = FALSE),
        c("Baseline welfare decile", names(channels)))
      num_defs <- stats::setNames(
        lapply(names(channels), function(nm) reactable::colDef(
          format = reactable::colFormat(digits = 4), minWidth = 70)),
        names(channels))
      .step2_reactable(table, col_defs = num_defs)
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
