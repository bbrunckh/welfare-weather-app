#' 1_modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom bsplus bs_accordion bs_append
#' @importFrom waiter autoWaiter spin_2 transparent
#'
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),

    sidebarLayout(

      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title = "1 Sample",
            content = mod_1_01_sample_ui(ns("sample"))
          ) |>
          bs_append(
            title = "2 Outcome Variable",
            content = tagList(
              mod_1_02_outcome_ui(ns("outcome")),
              mod_1_03_surveystats_ui(ns("surveystats"))
            )
          ) |>
          bs_append(
            title = "3 Weather variables",
            content = tagList(
              mod_1_04_weather_ui(ns("weather")),
              mod_1_05_weatherstats_ui(ns("weatherstats"))
            )
          ) |>
          bs_append(
            title = "4 Model",
            content = mod_1_06_model_ui(ns("model"))
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step1_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Outputs will appear here after you load data and click the relevant buttons in the sidebar."),
            includeMarkdown(system.file("app/www/equation.md", package = "wiseapp"))
          )
        )

      )
    )
  )
}

#' 1_modelling Server Functions
#'
#' @noRd
mod_1_modelling_server <- function(id, survey_list_master, pin_prefix, board, survey_metadata, varlist, weather_list, pov_lines) {
  moduleServer(id, function(input, output, session) {

    # Pass reactives
    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      survey_list_master = survey_list_master,
      pin_prefix = pin_prefix,
      board = board,
      survey_metadata = survey_metadata,
      varlist = varlist
    )

    mod_1_02_outcome_api <- mod_1_02_outcome_server(
      "outcome",
      survey_metadata = survey_metadata,
      varlist = varlist,
      country = mod_1_01_sample_api$selected_countries,
      survey_year = mod_1_01_sample_api$selected_years
    )

    # Survey stats module (adds a tab to the main panel after data load + button click)
    mod_1_03_surveystats_server(
      "surveystats",
      survey_data = mod_1_01_sample_api$survey_data,
      data_loaded = mod_1_01_sample_api$data_loaded,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      tabset_id = "step1_output_tabs",
      tabset_session = session,
      varlist = varlist,
      pov_lines = pov_lines,
      survey_geo = mod_1_01_sample_api$survey_geo
    )

    # Weather selection and configuration module
    mod_1_04_weather_api <- mod_1_04_weather_server(
      "weather",
      survey_data = mod_1_01_sample_api$survey_data,
      survey_data_files = mod_1_01_sample_api$survey_data_files,
      board = board,
      weather_list = weather_list,
      varlist = varlist,
      data_loaded = mod_1_01_sample_api$data_loaded
    )

    # Weather stats module (adds a tab after weather selection + button click)
    mod_1_05_weatherstats_server(
      "weatherstats",
      survey_weather = mod_1_04_weather_api$survey_weather,
      weather_vars = mod_1_04_weather_api$weather_vars,
      haz_vars = mod_1_04_weather_api$haz_vars,
      weather_list = mod_1_04_weather_api$weather_list,
      varlist = varlist,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      data_loaded = mod_1_01_sample_api$data_loaded,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    mod_1_model <- mod_1_06_model_server(
      "model",
      survey_weather = mod_1_04_weather_api$survey_weather,
      haz_vars = mod_1_04_weather_api$haz_vars,
      weather_settings = mod_1_04_weather_api$weather_settings,
      varlist = varlist,
      selected_outcome = mod_1_02_outcome_api$selected_outcome
    )

    mod_1_07_results_server(
      "results",
      model_fit = mod_1_model$model_fit,
      run_model = mod_1_model$run_model,
      haz_vars = mod_1_04_weather_api$haz_vars,
      weather_terms = mod_1_model$weather_terms,
      varlist = varlist,
      interaction_terms = mod_1_model$interaction_terms,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      outcome_label = mod_1_model$outcome_label,
      outcome_type = mod_1_model$outcome_type,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    mod_1_08_modelfit_server(
      "modelfit",
      model_fit = mod_1_model$model_fit,
      run_model = mod_1_model$run_model,
      haz_vars = mod_1_04_weather_api$haz_vars,
      weather_terms = mod_1_model$weather_terms,
      varlist = varlist,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      outcome_type = mod_1_model$outcome_type,
      outcome_label = mod_1_model$outcome_label,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    # Convenience reactive: "final model" (your convention is [[3]])
    final_model <- reactive({
      fits <- mod_1_model$model_fit()
      if (is.null(fits) || !length(fits)) return(NULL)
      fits[[3]] %||% fits[[length(fits)]]
    })

    # NEW: export Step 1 API for Step 2 / Step 3
    list(
      # nested (optional but nice)
      sample_api  = mod_1_01_sample_api,
      outcome_api = mod_1_02_outcome_api,
      weather_api = mod_1_04_weather_api,
      model_api   = mod_1_model,

      # flattened (what downstream modules will actually use)
      survey_data       = mod_1_01_sample_api$survey_data,
      survey_data_files = mod_1_01_sample_api$survey_data_files,
      data_loaded       = mod_1_01_sample_api$data_loaded,
      survey_geo        = mod_1_01_sample_api$survey_geo,

      selected_outcome  = mod_1_02_outcome_api$selected_outcome,

      survey_weather    = mod_1_04_weather_api$survey_weather,
      loc_weather       = mod_1_04_weather_api$loc_weather,
      weather_vars      = mod_1_04_weather_api$weather_vars,
      haz_vars          = mod_1_04_weather_api$haz_vars,
      haz_spec          = mod_1_04_weather_api$haz_spec,        # added in section 3
      weather_settings  = mod_1_04_weather_api$weather_settings,

      model_fit         = mod_1_model$model_fit,
      final_model       = final_model,
      outcome_type      = mod_1_model$outcome_type,
      outcome_label     = mod_1_model$outcome_label,

      pov_lines         = pov_lines
    )



  })
}
