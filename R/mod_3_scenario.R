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
mod_3_scenario_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      uiOutput(ns("policy_info_ui")),
      bslib::accordion(
        id       = ns("accordion"),
        multiple = FALSE,
        open     = FALSE,
        bslib::accordion_panel(
          title = "Social protection",
          value = "Social protection",
          icon  = icon("hand-holding-dollar"),
          mod_3_01_sp_ui(ns("sp"))
        ),
        bslib::accordion_panel(
          title = "Infrastructure",
          value = "Infrastructure",
          icon  = icon("road"),
          mod_3_02_infra_ui(ns("infra"))
        ),
        bslib::accordion_panel(
          title = "Digital inclusion",
          value = "Digital inclusion",
          icon  = icon("wifi"),
          mod_3_03_digital_ui(ns("digital"))
        ),
        bslib::accordion_panel(
          title = "Labor market",
          value = "Labor market",
          icon  = icon("briefcase"),
          mod_3_04_labor_ui(ns("labor"))
        ),
        bslib::accordion_panel(
          title = "Education",
          value = "Education",
          icon  = icon("graduation-cap"),
          mod_3_05_education_ui(ns("education"))
        )
      ),
      hr(),
      uiOutput(ns("run3_prereq_ui")),
      uiOutput(ns("run_policy_sim_ui"))
    ),
    h4("How could policy and structural adjustments mitigate the welfare impacts of weather?",
       class = "step-question"),
    tabsetPanel(
      id = ns("step3_output_tabs"),
      tabPanel(
        title = "Overview",
        value = "overview",
        div(
          class = "empty-state",
          icon("scale-balanced"),
          h5("No policy simulations yet"),
          p(paste(
            "Configure one or more policy levers in the sidebar, then click",
            "'Run simulation' to compare baseline and policy outcomes.",
            "Outputs: baseline-vs-policy outcome comparisons, exceedance",
            "probabilities, diagnostics, and a decomposition of policy effects",
            "will appear here as new tabs."
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
        welfare_equation_ui(predicted = TRUE),
        mod_3_06_policy_sim_ui(ns("policy_sim"))
      )
    )
  )
}

#' 3_scenario Server Functions
#'
#' Orchestrates sub-modules 01-06.
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
                                   survey_weather = reactive(NULL),
                                   survey_data = reactive(NULL),
                                   model_fit,
                                   hist_sim,
                                   saved_scenarios = reactive(list()),
                                   selected_hist   = reactive(NULL),
                                   variable_list   = reactive(NULL),
                                   analysis_unit   = reactive("hh"),
                                   skip_coef_draws = reactive(FALSE),
                                   residuals       = reactive("original"),
                                   propagate_all_covariate_uncertainty =
                                     reactive(FALSE),
                                   survey_version  = reactive(0L),
                                   sim_stale       = reactive(FALSE)) {
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
        tags$strong("Active policy levers:"),
        do.call(tags$ul, Filter(Negate(is.null), items)),
        tags$small(
          class = "text-muted",
          paste(
            "Adjust these inputs (and any others) in the sections below -",
            "results update when you re-run the simulation."
          )
        )
      )
    })

    # ---- Social Protection scenario --------------------------------------

    s1 <- mod_3_01_sp_server(
      "sp",
      selected_outcome = selected_outcome,
      survey_weather   = survey_weather,
      variable_list    = variable_list,
      analysis_unit    = analysis_unit,
      # UI-32: the reach preview must estimate over the same survey the policy
      # run consumes (Step 2's baseline round), not the full multi-round frame.
      hist_sim         = hist_sim
    )

    # ---- Infrastructure scenario -----------------------------------------

    s2 <- mod_3_02_infra_server(
      "infra",
      selected_model = selected_model,
      survey_data    = survey_data,
      variable_list  = variable_list)

    # ---- Digital & financial inclusion scenario --------------------------

    s3 <- mod_3_03_digital_server(
      "digital",
      selected_model = selected_model,
      survey_data   = survey_data,
      variable_list  = variable_list)

    # ---- Labor market scenario -------------------------------------------

    s4 <- mod_3_04_labor_server(
      "labor",
      selected_model = selected_model,
      survey_data   = survey_data,
      variable_list  = variable_list)

    # ---- Education scenario ----------------------------------------------

    s5 <- mod_3_05_education_server(
      "education",
      selected_model = selected_model,
      survey_data   = survey_data,
      variable_list  = variable_list)

    # ---- Policy adjustment module ----------------------------------------

    s6 <- mod_3_06_policy_sim_server(
      "policy_sim",
      survey_weather    = survey_weather,
      sp_scenario       = s1$sp_scenario,
      infra_scenario    = s2$infra_scenario,
      digital_scenario  = s3$digital_scenario,
      labor_scenario    = s4$labor_scenario,
      education_scenario = s5$education_scenario,
      selected_model    = selected_model,
      model_fit         = model_fit,
      selected_weather  = selected_weather,
      hist_sim          = hist_sim,
      saved_scenarios   = saved_scenarios,
      analysis_unit     = analysis_unit,
      skip_coef_draws   = skip_coef_draws,
      residuals         = residuals,
      propagate_all_covariate_uncertainty = propagate_all_covariate_uncertainty,
      survey_version    = survey_version,
      sim_stale         = sim_stale,
      # REACT-09: fire the child's run trigger on button click. req() blocks
      # the NULL/zero state of the dynamically rendered button, so the trigger
      # only fires on real clicks.
      run_trigger       = reactive({ req(input$run_policy_sim); input$run_policy_sim })
    )

    # ---- Results tabs: Baseline & Policy (both re-simulated) -------------
    s7 <- mod_3_07_results_server(
      "results3",
      baseline_hist_sim        = s6$baseline_hist_sim,
      baseline_saved_scenarios = s6$baseline_saved_scenarios,
      policy_hist_sim          = s6$policy_hist_sim,
      policy_saved_scenarios   = s6$policy_saved_scenarios,
      selected_hist            = selected_hist,
      sim_run_id               = s6$sim_run_id,
      tabset_id                = "step3_output_tabs",
      tabset_session           = session,
      residuals                = residuals,
      stale                    = s6$stale
    )

    # ---- Diagnostics tab: before/after variable analysis ----------------
    mod_3_08_diagnostics_server(
      "diagnostics",
      baseline_svy   = s6$baseline_svy,
      policy_svy     = s6$policy_svy,
      sim_run_id     = s6$sim_run_id,
      tabset_id      = "step3_output_tabs",
      tabset_session = session,
      analysis_unit  = analysis_unit
    )

    # ---- Decomposition tab: effect channels -----------------------------
    mod_3_09_decomposition_server(
      "decomposition",
      decomp_result    = s6$decomp_result,
      decomp_scenarios = s6$decomp_scenarios,
      model_fit        = model_fit,
      variable_list    = variable_list,
      so            = reactive({
        hs <- hist_sim()
        if (!is.null(hs)) hs$so else NULL
      }),
      show_coef_uncertainty = s7$show_coef_uncertainty
    )

    # Wire decomposition tab into tabset on first successful run
    decomp_tab_added <- reactiveVal(FALSE)
    observeEvent(s6$sim_run_id(), {
      req(s6$sim_run_id() > 0, s6$decomp_result())
      if (!decomp_tab_added()) {
        shiny::appendTab(
          inputId = "step3_output_tabs",
          shiny::tabPanel(
            title = "Decomposition",
            value = "decomposition_tab",
            mod_3_09_decomposition_ui(ns("decomposition"))
          ),
          session = session
        )
        decomp_tab_added(TRUE)
      }
    }, ignoreInit = TRUE)

    # ---- Run policy simulation button ------------------------------------

    output$run_policy_sim_ui <- renderUI({
      actionButton(
        ns("run_policy_sim"),
        "Run simulation",
        class = "btn-primary",
        width = "100%"
      )
    })

    # ---- Run-button prerequisites (UI-44) --------------------------------
    # The policy run needs a Step 1 fit and a Step 2 simulation. Those were
    # only discovered inside run(), where an unmet upstream req() made the
    # click a silent no-op; name them here, before the click.
    run3_prereqs_missing <- reactive({
      missing <- character(0)
      mf <- tryCatch(model_fit(),        error = function(e) NULL)
      hs <- tryCatch(hist_sim(),         error = function(e) NULL)
      sw <- tryCatch(selected_weather(), error = function(e) NULL)
      if (is.null(sw) || nrow(as.data.frame(sw)) == 0)
        missing <- c(missing, "weather variables (Step 1)")
      if (is.null(mf))
        missing <- c(missing, "a fitted model (Step 1)")
      if (is.null(hs))
        missing <- c(missing, "a historical simulation (Step 2)")
      missing
    })

    output$run3_prereq_ui <- renderUI({
      missing <- run3_prereqs_missing()
      if (!length(missing)) return(NULL)
      div(
        class = "alert alert-warning",
        role  = "alert",
        style = "font-size: 13px; margin-bottom: 4px;",
        tags$b("Prerequisites: "), "you still need ",
        paste(missing, collapse = ", "), " to run a policy simulation."
      )
    })

    # REACT-02: keep the button disabled while the policy simulation runs,
    # and while a prerequisite is missing.
    observe({
      blocked <- isTRUE(s6$running()) || length(run3_prereqs_missing()) > 0
      tryCatch(
        shiny::updateActionButton(
          session, inputId = "run_policy_sim",
          disabled = blocked
        ),
        error = function(e) NULL
      )
    })

    # ---- Run policy simulation on button click ---------------------------
    # REACT-09: handled by the run_trigger reactive passed to the child.

    # ---- Return API ------------------------------------------------------

    list(
      policy_hist_sim        = s6$policy_hist_sim,
      policy_saved_scenarios = s6$policy_saved_scenarios,
      # UI-47: consumed by the navbar step badge in app_server.
      stale                  = s6$stale,
      sim_run_id             = s6$sim_run_id
    )
  })
}