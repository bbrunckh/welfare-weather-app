#' 2_simulation UI Function
#'
#' @description A shiny Module. Orchestrates Step 2: unified sidebar for
#'   simulation configuration (mod_2_01_weathersim), Results tab
#'   (mod_2_02_results), and Diagnostics tab (mod_2_03_diagnostics).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom waiter autoWaiter spin_2 transparent
mod_2_simulation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("What welfare is expected given historical weather conditions? In future climate scenarios?"),
    sidebarLayout(
      sidebarPanel(
        mod_2_01_weathersim_ui(ns("weathersim")),
        shiny::hr(),
        shiny::actionButton(
          ns("clear_scenarios"),
          label = "Clear all scenarios",
          icon  = shiny::icon("trash"),
          width = "100%",
          style = "margin-top: 8px; color: #fff; background-color: #c0392b; border-color: #a93226;"
        )
      ),
      mainPanel(
        tabsetPanel(
          id = ns("step2_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Outputs will appear here after you configure settings and click 'Run simulation'."),
            includeMarkdown(system.file("app/www/equation2.md", package = "wiseapp"))
          )
        )
      )
    )
  )
}

#' 2_simulation Server Functions
#'
#' Orchestrates the unified sub-modules. Returns a flat API list consumed
#' by Step 3.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param selected_surveys Reactive data frame from the survey list.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param model_fit        Reactive list of fitted model objects.
#'
#' @noRd
mod_2_simulation_server <- function(id,
                                    connection_params,
                                    selected_outcome,
                                    selected_weather,
                                    selected_surveys,
                                    survey_weather,
                                    model_fit) {
  moduleServer(id, function(input, output, session) {

    # ---- 1. Unified sidebar + simulation engine ----------------------------
    s1 <- mod_2_01_weathersim_server(
      "weathersim",
      connection_params = connection_params,
      selected_outcome  = selected_outcome,
      selected_weather  = selected_weather,
      selected_surveys  = selected_surveys,
      survey_weather    = survey_weather,
      model_fit         = model_fit
    )

    # ---- 2. Results tab ----------------------------------------------------
    mod_2_02_results_server(
      "results",
      hist_sim        = s1$hist_sim,
      saved_scenarios = s1$saved_scenarios,
      selected_hist   = s1$selected_hist,
      tabset_id       = "step2_output_tabs",
      tabset_session  = session
    )

    # ---- 3. Diagnostics tab ------------------------------------------------
    mod_2_03_diagnostics_server(
      "diagnostics",
      hist_sim         = s1$hist_sim,
      saved_scenarios  = s1$saved_scenarios,
      survey_weather   = survey_weather,
      selected_weather = selected_weather,
      tabset_id        = "step2_output_tabs",
      tabset_session   = session
    )

    # ---- Clear scenarios button --------------------------------------------
    observeEvent(input$clear_scenarios, {
      s1$saved_scenarios(list())
      s1$hist_sim(NULL)
      shiny::showNotification(
        "All scenarios and historical baseline cleared. Re-run simulations to populate.",
        type = "message", duration = 4
      )
    })

    # ---- Return API --------------------------------------------------------
    list(
      selected_hist   = s1$selected_hist,
      selected_fut    = s1$selected_fut,
      hist_sim        = s1$hist_sim,
      saved_scenarios = s1$saved_scenarios
    )
  })
}
