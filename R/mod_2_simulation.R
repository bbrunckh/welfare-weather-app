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
mod_2_simulation_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      mod_2_01_weathersim_ui(ns("weathersim")),
      shiny::hr(),
      shiny::div(
        style = "display:flex; align-items:center; gap:0.4rem;",
        shiny::actionButton(
          ns("clear_scenarios"),
          label = "Clear simulation results",
          icon  = shiny::icon("trash"),
          width = "100%",
          class = "btn-outline-danger"
        ),
        info_popover(
          title = "Clear simulation results",
          shiny::p(paste(
            "Removes all saved future-scenario runs and the historical",
            "baseline from this session. Your settings are kept —",
            "re-run the simulation to regenerate results."
          ))
        )
      )
    ),
    h4("What welfare is expected given historical weather conditions? In future climate scenarios?",
       class = "step-question"),
    tabsetPanel(
      id = ns("step2_output_tabs"),
      tabPanel(
        title = "Overview",
        value = "overview",
        div(
          class = "empty-state",
          icon("cloud-sun-rain"),
          h5("No simulations yet"),
          p(paste(
            "Configure historical and future weather scenarios in the sidebar,",
            "then click 'Run simulation'.",
            "Outputs: outcome distributions by climate scenario, exceedance",
            "probabilities, and simulation diagnostics will appear here as new tabs."
          )),
          p(
            class = "text-muted small mb-0",
            paste(
              "Simulations for large surveys (tens of thousands of households)",
              "can take several minutes to run; charts take a few seconds to",
              "update after changing filters."
            )
          )
        ),
        welfare_equation_ui(predicted = TRUE)
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
                                    model_fit,
                                    stored_breaks = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # ---- 1. Unified sidebar + simulation engine ----------------------------
    s1 <- mod_2_01_weathersim_server(
      "weathersim",
      connection_params = connection_params,
      selected_outcome  = selected_outcome,
      selected_weather  = selected_weather,
      selected_surveys  = selected_surveys,
      survey_weather    = survey_weather,
      model_fit         = model_fit,
      stored_breaks     = stored_breaks
    )

    # ---- 2. Results tab ----------------------------------------------------
    s2 <- mod_2_02_results_server(
      "results",
      hist_sim        = s1$hist_sim,
      saved_scenarios = s1$saved_scenarios,
      selected_hist   = s1$selected_hist,
      tabset_id       = "step2_output_tabs",
      tabset_session  = session,
      sim_n           = s1$sim_n,
      residuals       = s1$residuals,
      skip_coef_draws = s1$skip_coef_draws
    )

    # ---- 3. Diagnostics tab ------------------------------------------------
    mod_2_03_diagnostics_server(
      "diagnostics",
      hist_sim           = s1$hist_sim,
      saved_scenarios    = s1$saved_scenarios,
      survey_weather     = survey_weather,
      selected_weather   = selected_weather,
      variance_breakdown = s2$variance_breakdown,
      timeseries_curves  = s2$timeseries_curves,
      tabset_id          = "step2_output_tabs",
      tabset_session     = session
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
      saved_scenarios = s1$saved_scenarios,
      skip_coef_draws = s1$skip_coef_draws,
      residuals       = s1$residuals,
      propagate_all_covariate_uncertainty = s1$propagate_all_covariate_uncertainty
    )
  })
}
