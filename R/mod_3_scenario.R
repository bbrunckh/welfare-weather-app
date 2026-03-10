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
        actionButton(
          ns("run_policy_sim"),
          "Run simulation",
          class = "btn-primary",
          width = "100%"
        )
      ),
      mainPanel(
        tabsetPanel(
          id = ns("step3_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Outputs will appear here after you run the policy simulation in the sidebar."),
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
#' @param hist_sim         Reactive returning the historical simulation raw
#'   predictions list from `mod_2_simulation_server()`.
#' @param fut_sim          Reactive returning the future simulation raw
#'   predictions list from `mod_2_simulation_server()`. May be `NULL` if the
#'   future simulation has not yet been run.
#'
#' @noRd
mod_3_scenario_server <- function(id,
                                   connection_params,
                                   selected_outcome,
                                   selected_weather,
                                   survey_weather,
                                   model_fit,
                                   hist_sim,
                                   fut_sim = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- Social Protection scenario --------------------------------------

    s1 <- mod_3_01_sp_server("sp")

    # ---- Infrastructure scenario -----------------------------------------

    s2 <- mod_3_02_infra_server("infra")

    # ---- Digital & financial inclusion scenario --------------------------

    s3 <- mod_3_03_digital_server("digital")

    # ---- Labor market scenario -------------------------------------------

    s4 <- mod_3_04_labor_server("labor")

    # ---- Policy simulation module (initialised once at startup) ----------

    s5 <- mod_3_05_policy_sim_server(
      "policy_sim",
      connection_params  = connection_params,
      selected_outcome   = selected_outcome,
      selected_weather   = selected_weather,
      survey_weather     = survey_weather,
      model_fit          = model_fit,
      hist_sim           = hist_sim,
      fut_sim            = fut_sim,
      sp_scenario        = s1$sp_scenario,
      infra_scenario     = s2$infra_scenario,
      digital_scenario   = s3$digital_scenario,
      labor_scenario     = s4$labor_scenario
    )

    # ---- Run policy simulation on button click ---------------------------

    observeEvent(input$run_policy_sim, {
      s5$run()
    })

    # ---- Return API ------------------------------------------------------

    list(
      selected_hist = s1$selected_hist,
      selected_fut  = s3$selected_fut,
      hist_sim      = s2$hist_sim,
      fut_sim       = s4$fut_sim
    )
  })
}