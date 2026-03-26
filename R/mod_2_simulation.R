#' 2_simulation UI Function
#'
#' @description A shiny Module. Orchestrates the Step 2 simulation pipeline:
#'   historical climate selection, historical simulation, future climate
#'   selection, and future climate simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bsplus bs_accordion bs_append
#' @importFrom waiter autoWaiter spin_2 transparent
mod_2_simulation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("What welfare is expected given historical weather conditions? In future climate scenarios?"),
    sidebarLayout(
      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title   = "1 Historical climate",
            content = tagList(
              mod_2_01_historical_ui(ns("historical")),
              mod_2_02_historical_sim_ui(ns("historical_sim"))
            )
          ) |>
          bs_append(
            title   = "2 Future climate",
            content = tagList(
              mod_2_03_future_ui(ns("future")),
              mod_2_04_future_sim_ui(ns("future_sim"))
            )
          ),
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
            p("Outputs will appear here after you load data and make selections in the sidebar."),
            includeMarkdown(system.file("app/www/equation2.md", package = "wiseapp"))
          )
        )
      )
    )
  )
}

#' 2_simulation Server Functions
#'
#' Orchestrates sub-modules 01-06. Returns a flat API list consumed by Step 3.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from mod_1_modelling (s3).
#' @param selected_weather Reactive data frame of selected weather variables
#'   from mod_1_modelling (s4).
#' @param survey_weather   Reactive data frame of merged survey-weather data
#'   from mod_1_modelling (s5).
#' @param model_fit        Reactive list of fitted model objects from
#'   mod_1_modelling (s7).
#'
#' @noRd
mod_2_simulation_server <- function(id,
                                    connection_params,
                                    selected_outcome,
                                    selected_weather,
                                    survey_weather,
                                    model_fit) {
  moduleServer(id, function(input, output, session) {

    # ---- Scenario storage ---------------------------------------------------
    saved_scenarios <- reactiveVal(list())

    # ---- 1. Historical climate selection ------------------------------------
    s1 <- mod_2_01_historical_server("historical")

    # ---- 3. Future climate selection ----------------------------------------
    # NOTE: s3 is wired before s2 so that s2 can receive fut_sim from s4, which
    # in turn depends on s3. The declaration order here does NOT affect reactivity;
    # Shiny resolves the reactive graph lazily at run time.
    s3 <- mod_2_03_future_server("future", selected_hist = s1$selected_hist)

    # ---- 2. Historical simulation -------------------------------------------
    s2 <- mod_2_02_historical_sim_server(
      "historical_sim",
      connection_params = connection_params,
      selected_outcome  = selected_outcome,
      selected_weather  = selected_weather,
      survey_weather    = survey_weather,
      model_fit         = model_fit,
      selected_hist     = s1$selected_hist,
      fut_sim           = reactive(s4$fut_sim()),
      tabset_id         = "step2_output_tabs",
      tabset_session    = session
    )

    # ---- 4. Future simulation -----------------------------------------------
    # NOTE: forward reference to s4 via reactive() in s2 above is safe because
    # s4 is defined in the same moduleServer closure before any reactive graph
    # evaluation begins. s2$fut_sim_val is only resolved when input$run_hist_sim
    # fires, by which time s4 is already registered.
    s4 <- mod_2_04_future_sim_server(
      "future_sim",
      connection_params = connection_params,
      selected_outcome  = selected_outcome,
      selected_weather  = selected_weather,
      survey_weather    = survey_weather,
      model_fit         = model_fit,
      selected_hist     = s1$selected_hist,
      selected_fut      = s3$selected_fut,
      hist_run_id       = s2$hist_run_id
    )

    # ---- 5. Simulation diagnostics ------------------------------------------
    mod_2_05_sim_diag_server(
      "sim_diag",
      hist_sim         = s2$hist_sim,
      fut_sim          = s4$fut_sim,
      saved_scenarios  = saved_scenarios,
      survey_weather   = survey_weather,
      selected_weather = selected_weather,
      tabset_id        = "step2_output_tabs",
      tabset_session   = session
    )

    # ---- Clear scenarios button ---------------------------------------------
    observeEvent(input$clear_scenarios, {
      saved_scenarios(list())
      s2$hist_sim(NULL)
      shiny::showNotification(
        "All scenarios and historical baseline cleared. Re-run simulations to populate.",
        type = "message", duration = 4
      )
    })

    # ---- 6. Compare simulations ---------------------------------------------
    mod_2_06_sim_compare_server(
      "sim_compare",
      hist_sim        = s2$hist_sim,
      fut_sim         = s4$fut_sim,
      saved_scenarios = saved_scenarios,
      selected_hist   = s1$selected_hist
    )

    # ---- Return API ---------------------------------------------------------
    list(
      selected_hist   = s1$selected_hist,
      selected_fut    = s3$selected_fut,
      hist_sim        = s2$hist_sim,
      fut_sim         = s4$fut_sim,
      saved_scenarios = saved_scenarios
    )
  })
}
