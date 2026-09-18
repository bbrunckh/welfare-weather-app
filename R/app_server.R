#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Start the single process-wide Step 2 worker before modules submit work.
  .wise_step2_async_init()

  # SEC-03: data connections and credential caches are process-wide. Register
  # the root session so the final session in this process releases them.
  .duck_register_session(session)

  # Step 0: data connection, config, and metadata loading ----

  overview_api <- mod_0_overview_server(id = "overview")

  # The import pipeline fires these request channels in order. Module-owned
  # observers still handle real button clicks, so manual runs keep their
  # existing behavior.
  load_survey_trigger <- shiny::reactiveVal(NULL)
  load_weather_trigger <- shiny::reactiveVal(NULL)
  run_step1_trigger <- shiny::reactiveVal(NULL)
  run_step2_trigger <- shiny::reactiveVal(NULL)
  run_step3_trigger <- shiny::reactiveVal(NULL)
  aggregation_cache <- new_shared_aggregation_cache()

  # Step 1: modelling ----
  # Pass reactives from overview_api

  step1_api <- mod_1_modelling_server(
    id = "step1",
    connection_params = overview_api$connection_params,
    survey_list = overview_api$survey_list,
    variable_list = overview_api$variable_list,
    cpi_ppp = overview_api$cpi_ppp,
    run_trigger = run_step1_trigger,
    load_survey_trigger = load_survey_trigger,
    load_weather_trigger = load_weather_trigger
  )

  # Step 2: simulation ----
  # Pass selected Step 1 reactives

  step2_api <- mod_2_simulation_server(
    id                = "step2",
    connection_params = overview_api$connection_params,
    selected_outcome  = step1_api$selected_outcome,
    selected_weather  = step1_api$selected_weather,
    selected_surveys  = step1_api$selected_surveys,
    survey_weather    = step1_api$survey_weather,
    model_fit         = step1_api$model_fit,
    stored_breaks     = step1_api$stored_breaks,
    survey_version    = step1_api$survey_version,
     run_trigger       = run_step2_trigger,
     shared_aggregation_cache = aggregation_cache
  )

  # Step 3: policy scenarios ----
  # Pass selected Step 1 & 2 reactives
  step3_api <- mod_3_scenario_server(
    id = "step3",
    selected_outcome = step1_api$selected_outcome,
    selected_weather = step1_api$selected_weather,
    selected_model = step1_api$selected_model,
    selected_policies = step1_api$selected_policies,
    survey_weather = step1_api$survey_weather,
    survey_data = step1_api$survey_data,
    model_fit = step1_api$model_fit,
    hist_sim = step2_api$hist_sim,
    saved_scenarios = step2_api$saved_scenarios,
    selected_hist = step2_api$selected_hist,
    variable_list = overview_api$variable_list,
    analysis_unit = step1_api$analysis_unit,
    skip_coef_draws = step2_api$skip_coef_draws,
    residuals = step2_api$residuals,
    propagate_all_covariate_uncertainty =
      step2_api$propagate_all_covariate_uncertainty,
    survey_version = step1_api$survey_version,
    sim_stale = step2_api$stale,
     run_trigger = run_step3_trigger,
     shared_aggregation_cache = aggregation_cache
  )

  # Explicit completion contracts for the automatic configuration runner.
  # Generations advance only after a stage has published a successful result;
  # statuses distinguish failed attempts from unchanged cached state.
  pipeline_triggers <- list(
    load_survey  = load_survey_trigger,
    load_weather = load_weather_trigger,
    step1        = run_step1_trigger,
    step2        = run_step2_trigger,
    step3        = run_step3_trigger
  )
  pipeline_results <- list(
    load_survey = list(
      generation = step1_api$survey_load_done,
      status = step1_api$survey_load_status
    ),
    load_weather = list(
      generation = step1_api$weather_load_done,
      status = step1_api$weather_load_status
    ),
    step1 = list(
      generation = step1_api$fit_generation,
      status = step1_api$fit_status
    ),
    step2 = list(
      generation = step2_api$run_generation,
      status = step2_api$run_status
    ),
    step3 = list(
      generation = step3_api$run_generation,
      status = step3_api$run_status
    )
  )

  # Export registrations use these shared stale references so every step has
  # the same stale suppression contract, including imports.
  session$userData$wise_step1_stale <- step1_api$fit_stale
  session$userData$wise_step2_stale <- step2_api$stale
  session$userData$wise_step3_stale <- step3_api$stale

  config_imported <- shiny::reactiveVal(0L)

  # Navbar step status badges (UI-47) ----
  # Steps are freely navigable and results persist across tab switches, so the
  # navbar is the one place that shows every step's state at once. Each badge
  # reads the step's stored result plus its INT-08 stale flag, so it agrees
  # with the in-page stale banner by construction: a check while the results
  # match the current inputs, a reload arrow once an input has changed, and
  # nothing at all before the step has been run.

  output$step1_badge <- render_step_badge(
    has_result = step1_api$model_fit,
    is_stale = stale_after_import(
      step1_api$model_fit,
      step1_api$fit_stale, config_imported
    ),
    step_label = "Step 1 (model)"
  )

  output$step2_badge <- render_step_badge(
    has_result = step2_api$hist_sim,
    is_stale = stale_after_import(
      step2_api$hist_sim,
      step2_api$stale, config_imported
    ),
    step_label = "Step 2 (climate scenarios)"
  )

  output$step3_badge <- render_step_badge(
    has_result = step3_api$policy_hist_sim,
    is_stale = stale_after_import(
      step3_api$policy_hist_sim,
      step3_api$stale, config_imported
    ),
    step_label = "Step 3 (policy scenarios)"
  )

  # Run provenance (UI-49) ----
  # One record per completed step, built from the immutable metadata stored
  # with each result (`.snap` / `.sig`) rather than from live inputs - so it
  # describes the run that produced the results, not whatever is selected now.
  # The step banners and the export bundle's metadata read the same record.

  run_provenance <- reactive({
    cp <- read_connection_params(overview_api$connection_params)
    Filter(Negate(is.null), list(
      step1 = wise_provenance(1L, tryCatch(step1_api$model_fit(),
        error = function(e) NULL
      ),
      connection_params = cp,
      extra = list(stale = tryCatch(
        isTRUE(step1_api$fit_stale()),
        error = function(e) NA
      ))
      ),
      step2 = wise_provenance(2L, tryCatch(step2_api$hist_sim(),
        error = function(e) NULL
      ),
      connection_params = cp,
      extra = list(stale = tryCatch(
        isTRUE(step2_api$stale()),
        error = function(e) NA
      ))
      ),
      step3 = wise_provenance(3L, tryCatch(step3_api$policy_hist_sim(),
        error = function(e) NULL
      ),
      connection_params = cp,
      extra = list(stale = tryCatch(
        isTRUE(step3_api$stale()),
        error = function(e) NA
      ))
      )
    ))
  })

  # Export menu (UI-48) ----
  # Not a module: the configuration snapshot reads the *root* input object, so
  # every module's namespaced controls are captured in one pass.

  export_menu_server(input, output, session,
    provenance = run_provenance,
    seed = WISEAPP_DEFAULT_SEED,
    run_triggers = pipeline_triggers,
    step_results = pipeline_results,
    on_import = function(imported_seed = NULL) {
      if (!is.null(imported_seed) && is.finite(imported_seed)) {
        session$userData$wise_analysis_seed <- as.integer(imported_seed)
      }
      config_imported(isolate(config_imported()) + 1L)
    }
  )
}
