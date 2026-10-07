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
      mod_2_01_weathersim_ui(ns("weathersim"))
    ),
    h1("What welfare is expected given historical weather conditions? In future climate scenarios?",
       class = "step-question"),
    tabsetPanel(
      id = ns("step2_output_tabs"),
      tabPanel(
        title = "Overview",
        value = "overview",
        div(
          class = "empty-state overview-empty-state",
          icon("cloud-sun-rain"),
          h2(class = "h5", "No simulations yet"),
          p(paste(
            "Configure climate scenarios in the sidebar, then click",
            "'Run simulation'. Results will appear here as new tabs."
          ))
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
#' @param run_trigger      Optional reactive trigger for a programmatic run.
#' @param shared_aggregation_cache Optional shared session aggregation cache.
#'
#' @noRd
mod_2_simulation_server <- function(id,
                                    connection_params,
                                    selected_outcome,
                                    selected_weather,
                                    selected_surveys,
                                    survey_weather,
                                    model_fit,
                                    stored_breaks = reactive(NULL),
                                     survey_version = reactive(0L),
                                      run_trigger = reactive(NULL),
                                      shared_aggregation_cache = NULL) {
  moduleServer(id, function(input, output, session) {

    # Results-tab display settings (method, poverty line, bandwidth), written
    # by mod_2_02 in committed mode and read by mod_2_01 at submit.
    display_settings <- reactiveVal(NULL)

    # 1. Unified sidebar + simulation engine ----
    s1 <- mod_2_01_weathersim_server(
      "weathersim",
      connection_params = connection_params,
      selected_outcome  = selected_outcome,
      selected_weather  = selected_weather,
      selected_surveys  = selected_surveys,
      survey_weather    = survey_weather,
      model_fit         = model_fit,
      stored_breaks     = stored_breaks,
       survey_version    = survey_version,
       run_trigger       = run_trigger,
      display_settings  = display_settings
    )

    # 2. Results tab ----
    s2 <- mod_2_02_results_server(
      "results",
      hist_sim        = s1$hist_sim,
      saved_scenarios = s1$saved_scenarios,
      selected_hist   = s1$selected_hist,
      selected_weather = selected_weather,
      tabset_id       = "step2_output_tabs",
      tabset_session  = session,
       residuals       = s1$residuals,
       shared_aggregation_cache = shared_aggregation_cache,
      skip_coef_draws = s1$skip_coef_draws,
      stale           = s1$stale,
      live_run        = s1$live_run %||% reactive(NULL),
      adopted_partials = s1$adopted_partials %||% reactive(NULL),
      display_settings_out = display_settings
    )

    # 3. Diagnostics tab ----
    mod_2_03_diagnostics_server(
      "diagnostics",
      hist_sim           = s1$hist_sim,
      saved_scenarios    = s1$saved_scenarios,
      selected_hist      = s1$selected_hist,
      survey_weather     = survey_weather,
      selected_weather   = selected_weather,
      stored_breaks      = stored_breaks,
      timeseries_curves  = s2$timeseries_curves,
      tabset_id          = "step2_output_tabs",
      tabset_session     = session,
      stale              = s1$stale
    )

    # Return API ----
    list(
      selected_hist   = s1$selected_hist,
      hist_sim        = s1$hist_sim,
      saved_scenarios = s1$saved_scenarios,
      skip_coef_draws = s1$skip_coef_draws,
      residuals       = s1$residuals,
      propagate_all_covariate_uncertainty = s1$propagate_all_covariate_uncertainty,
      stale           = s1$stale,
      run_generation  = s1$run_generation,
      run_status      = s1$run_status
    )
  })
}
