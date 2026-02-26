#' 3_scenario UI Function
#'
#' @description A shiny Module. Provides the Step 3 "What if?" scenario
#'   explorer, allowing users to compare historical and future simulation
#'   baselines against alternative policy scenarios.
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
    waiter::autoWaiter(html = waiter::spin_2(), color = waiter::transparent(.5)),
    h4("What if...? Explore alternative climate and policy scenarios"),

    sidebarLayout(
      sidebarPanel(
        bsplus::bs_accordion(id = ns("accordion")) |>
          bsplus::bs_append(
            title   = "Policy scenario",
            content = tagList(
              p("Configure your policy scenario parameters here.")
            )
          ) |>
          bsplus::bs_append(
            title   = "Run scenario",
            content = tagList(
              p("Run scenario analysis.")
            )
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step3_output_tabs"),

          tabPanel(
            title = "Results",
            value = "results",
            h4("Scenario results"),
            bslib::card(
              bslib::card_header("Historical simulation baseline (Step 2)"),
              verbatimTextOutput(ns("baseline_summary"))
            ),
            tags$hr(),
            p("Scenario comparison results will be displayed here.")
          ),

          tabPanel(
            title = "Configuration",
            value = "config",
            h4("Scenario configuration"),
            p("Summary of scenario configuration and parameters will be shown here.")
          )
        )
      )
    )
  )
}

#' 3_scenario Server Functions
#'
#' Displays Step 2 simulation baselines and (in future) scenario comparison
#' results. Consumes the flat API returned by `mod_2_simulation_server()`.
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

    ns <- session$ns

    # ---- Baseline summary --------------------------------------------------
    # Display a concise summary of whatever Step 2 simulations have been run.

    output$baseline_summary <- renderText({
      hist <- tryCatch(hist_sim(), error = function(e) NULL)
      fut  <- if (!is.null(fut_sim)) tryCatch(fut_sim(), error = function(e) NULL) else NULL

      if (is.null(hist) && is.null(fut)) {
        return(
          "No Step 2 simulations available yet.\n\nRun a historical or future simulation in Step 2 first."
        )
      }

      so   <- tryCatch(selected_outcome(), error = function(e) NULL)
      sw   <- tryCatch(selected_weather(),  error = function(e) NULL)

      out <- character()

      if (!is.null(so)) {
        out <- c(out, paste0("Outcome:  ", so$name))
        if (!is.null(so$type)) out <- c(out, paste0("Type:     ", so$type))
      }

      if (!is.null(sw) && nrow(sw) > 0) {
        out <- c(out, paste0("Weather:  ", paste(sw$variable, collapse = ", ")))
      }

      if (!is.null(hist)) {
        n_hist <- if (is.data.frame(hist$preds)) nrow(hist$preds) else NA_integer_
        out <- c(out, paste0("Hist sim: ", if (!is.na(n_hist)) paste0(n_hist, " predictions") else "available"))
      }

      if (!is.null(fut)) {
        n_fut <- if (is.data.frame(fut$preds)) nrow(fut$preds) else NA_integer_
        out <- c(out, paste0("Fut sim:  ", if (!is.na(n_fut)) paste0(n_fut, " predictions") else "available"))
      }

      paste(out, collapse = "\n")
    })

    # ---- Return API --------------------------------------------------------
    list()
  })
}