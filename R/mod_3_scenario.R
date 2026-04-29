#' 3_scenario UI Function
#'
#' @description A shiny Module. Orchestrates the Step 3 policy scenario pipeline:
#'   social protection, infrastructure, labor market, digital & financial inclusion.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bsplus bs_accordion bs_append
#' @importFrom waiter autoWaiter spin_2 transparent
mod_3_scenario_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("How could policy and structural adjustments mitigate the welfare impacts of weather?"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("policy_info_ui")),
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title   = "Social protection",
            content = mod_3_01_sp_ui(ns("sp"))
          ) |>
          bs_append(
            title   = "Infrastructure",
            content = mod_3_02_infra_ui(ns("infra"))
          ) |>
          bs_append(
            title   = "Digital inclusion",
            content = mod_3_03_digital_ui(ns("digital"))
          ) |>
          bs_append(
            title   = "Labor market",
            content = mod_3_04_labor_ui(ns("labor"))
          ),
        hr(),
        uiOutput(ns("run_policy_sim_ui"))
      ),
      mainPanel(
        tabsetPanel(
          id = ns("step3_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            includeMarkdown(system.file("app/www/equation2.md", package = "wiseapp")),
            mod_3_05_policy_sim_ui(ns("policy_sim"))
          )
        )
      )
    )
  )
}

#' 3_scenario Server Functions
#'
#' Orchestrates sub-modules 01-05.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from `mod_0_overview_server()`.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from `mod_1_modelling_server()`.
#' @param selected_weather Reactive data frame of selected weather variables
#'   from `mod_1_modelling_server()`.
#' @param survey_weather   Reactive data frame of merged survey-weather data
#'   from `mod_1_modelling_server()`.
#' @param model_fit        Reactive list of fitted model objects from
#'   `mod_1_modelling_server()`.
#' @param hist_sim         Reactive returning the historical simulation list
#'   from `mod_2_simulation_server()`.
#' @param saved_scenarios  Reactive returning the named list of saved future
#'   scenarios from `mod_2_simulation_server()`.
#'
#' @noRd
mod_3_scenario_server <- function(id,
                                   connection_params,
                                   selected_outcome,
                                   selected_weather,
                                   selected_model,
                                   selected_policies = reactive(NULL),
                                   survey_weather,
                                   model_fit,
                                   hist_sim,
                                   saved_scenarios = reactive(list()),
                                   selected_hist   = reactive(NULL),
                                   variable_list   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Display selected policy scenarios above accordion -----------------

    output$policy_info_ui <- renderUI({
      pols <- selected_policies()
      if (is.null(pols) || length(pols) == 0) {
        return(div(
          class = "alert alert-warning",
          style = "padding: 8px; margin-bottom: 10px; font-size: 13px;",
          tags$strong("No policy scenarios selected."),
          " Go to Step 1 \u2192 Policy scenarios to select one (if desired)."
        ))
      }

      vl <- variable_list()
      items <- lapply(pols, function(k) {
        def <- POLICY_DEFINITIONS[[k]]
        if (is.null(def)) return(NULL)
        var_labels <- vapply(def$vars, function(v) {
          lbl <- if (!is.null(vl) && v %in% vl$name) vl$label[vl$name == v][1] else v
          paste0(lbl, " (", v, ")")
        }, character(1))
        tags$li(
          tags$strong(def$label),
          tags$br(),
          tags$small(class = "text-muted", paste(var_labels, collapse = ", "))
        )
      })

      div(
        class = "alert alert-info",
        style = "padding: 8px; margin-bottom: 10px; font-size: 13px;",
        tags$strong("Selected policy scenario:"),
        do.call(tags$ul, Filter(Negate(is.null), items))
      )
    })

    # ---- Social Protection scenario --------------------------------------

    s1 <- mod_3_01_sp_server(
      "sp",
      selected_outcome = selected_outcome,
      survey_weather   = survey_weather,
      variable_list    = variable_list
    )

    # ---- Infrastructure scenario -----------------------------------------

    s2 <- mod_3_02_infra_server(
      "infra",
      selected_model = selected_model,
      variable_list  = variable_list)

    # ---- Digital & financial inclusion scenario --------------------------

    s3 <- mod_3_03_digital_server(
      "digital",
      selected_model = selected_model,
      variable_list  = variable_list)

    # ---- Labor market scenario -------------------------------------------

    s4 <- mod_3_04_labor_server(
      "labor",
      selected_model = selected_model,
      variable_list  = variable_list)

    # ---- Policy simulation module (initialised once at startup) ----------

    s5 <- mod_3_05_policy_sim_server(
      "policy_sim",
      connection_params  = connection_params,
      selected_outcome   = selected_outcome,
      selected_weather   = selected_weather,
      survey_weather     = survey_weather,
      model_fit          = model_fit,
      hist_sim           = hist_sim,
      saved_scenarios    = saved_scenarios,
      sp_scenario        = s1$sp_scenario,
      infra_scenario     = s2$infra_scenario,
      digital_scenario   = s3$digital_scenario,
      labor_scenario     = s4$labor_scenario
    )

    # ---- Results tab: Step 3-specific module (baseline vs policy) --------
    mod_3_06_results_server(
      "results3",
      baseline_hist_sim        = hist_sim,
      baseline_saved_scenarios = saved_scenarios,
      policy_hist_sim          = s5$policy_hist_sim,
      policy_saved_scenarios   = s5$policy_saved_scenarios,
      baseline_svy             = s5$baseline_svy,
      policy_svy               = s5$policy_svy,
      selected_model           = selected_model,
      selected_hist            = selected_hist,
      sim_run_id               = s5$sim_run_id,
      tabset_id                = "step3_output_tabs",
      tabset_session           = session
    )

    # ---- Run policy simulation button (hidden for RIF engine) -----------

    output$run_policy_sim_ui <- renderUI({
      mf <- model_fit()
      if (!is.null(mf) && identical(mf$engine, "rif")) {
        div(
          class = "alert alert-warning",
          style = "font-size: 13px; margin-top: 4px;",
          tags$b("\u26a0 Simulations are not yet implemented for Quantile Regression (RIF)."),
          " Please select a different model engine to run simulations."
        )
      } else {
        actionButton(
          ns("run_policy_sim"),
          "Run simulation",
          class = "btn-primary",
          width = "100%"
        )
      }
    })

    # ---- Run policy simulation on button click ---------------------------

    observeEvent(input$run_policy_sim, {
      s5$run()
    })

    # ---- Return API ------------------------------------------------------

    list(
      policy_hist_sim        = s5$policy_hist_sim,
      policy_saved_scenarios = s5$policy_saved_scenarios
    )
  })
}