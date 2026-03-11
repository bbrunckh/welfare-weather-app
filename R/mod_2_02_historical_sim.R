#' 2_02_historical_sim UI Function
#'
#' @description A shiny Module. Triggers and displays the historical
#'   welfare simulation (Phases B-E).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_02_historical_sim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::actionButton(
      ns("run_hist_sim"),
      "Run simulation",
      class = "btn-primary",
      style = "width: 100%; margin-top: 8px;"
    )
  )
}

#' 2_02_historical_sim Server Functions
#'
#' Runs the historical welfare simulation using the selected climate
#' configuration and Step 1 model fit.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param model_fit        Reactive list returned by fit_model() (via
#'   mod_1_07_results). Must contain \code{$fit3} (the fitted model object),
#'   \code{$engine} (character, e.g. \code{"fixest"} or \code{"lm"}), and
#'   \code{$train_data} (the complete-case data frame used for fitting, stored
#'   by fit_model() for consistent residual extraction).
#' @param selected_hist    Reactive one-row data frame from mod_2_01_historical.
#' @param fut_sim          Optional reactive returning the future simulation
#'   raw predictions list (from mod_2_04_future_sim). When non-NULL the future
#'   climate series is overlaid on the results plot.
#' @param tabset_id        Character id of the parent tabset panel.
#' @param tabset_session   Shiny session for the tabset.
#'
#'#' @details
#'   Internal state:
#'   \itemize{
#'     \item \code{pred_hist_raw} — raw predictions before aggregation;
#'       persisted so the compare module can re-aggregate on demand.
#'     \item \code{hist_run_id} — integer counter incremented on each run;
#'       future sim stores this id so stale overlays can be detected.
#'     \item \code{fut_sim_val} — normalised wrapper around the optional
#'       \code{fut_sim} argument; always callable as \code{fut_sim_val()}.
#'   }
#' @noRd
mod_2_02_historical_sim_server <- function(id,
                                            connection_params,
                                            selected_outcome,
                                            selected_weather,
                                            survey_weather,
                                            model_fit,
                                            selected_hist,
                                            fut_sim        = NULL,
                                            tabset_id,
                                            tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    pred_hist_raw  <- reactiveVal(NULL)   # raw predictions (pre-aggregation)
    hist_tab_added <- reactiveVal(FALSE)

    # normalise fut_sim — always callable as fut_sim_val() regardless of wiring
    fut_sim_val <- if (is.function(fut_sim)) fut_sim else reactive(NULL)

    # increments each time the historical sim runs; the future sim stores the
    # hist_run_id at the time it ran so the plot can detect a stale overlay
    hist_run_id <- reactiveVal(0L)

    # ---- aggregation choices depend on outcome type ------------------------
    aggregate_choices <- reactive({
      hist_aggregate_choices(selected_outcome()$type)
    })

    # ---- main simulation observer ------------------------------------------
    observeEvent(input$run_hist_sim, {
      req(selected_weather(), selected_outcome(),
          survey_weather(), selected_hist(), model_fit())

      sw <- selected_weather()
      so <- selected_outcome()
      sh <- selected_hist()
      svy <- survey_weather()
      mf  <- model_fit()

      # Unpack what predict_outcome() needs from the fit_model() result.
      # $fit3      — the fitted model object (parsnip model_fit or fixest)
      # $engine    — "fixest" or "lm"; passed explicitly so predict_outcome()
      #              does not have to rely solely on class detection
      # $train_data — complete-case data fit_model() used; this is the correct
      #               reference frame for residual extraction (not survey_weather(),
      #               which still contains rows dropped as incomplete cases)
      model      <- mf$fit3
      engine     <- mf$engine
      train_data <- mf$train_data

      # ---- load weather data -----------------------------------------------

      sim_dates <- build_hist_sim_dates(svy, unlist(sh$year_range))

      notif_load <- shiny::showNotification(
        "Loading historical weather data...", duration = NULL, type = "message"
      )

      weather_raw <- tryCatch(
        get_weather(svy, sw, sim_dates, connection_params()),
        error = function(e) {
          shiny::showNotification(
            paste("Failed to load weather data:", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )

      shiny::removeNotification(notif_load)
      req(!is.null(weather_raw))

      # join weather back to survey covariates
      survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)

      # ---- simulate outcomes -----------------------------------------------

      preds <- predict_outcome(
        model      = model,
        newdata    = survey_wd_sim,
        residuals  = sh$residuals,
        outcome    = so$name,
        id         = NULL,
        train_data = train_data,
        engine     = engine
      )

      # back-transform log outcomes
      preds <- apply_log_backtransform(preds, so)

      # store raw predictions — comparison plots rendered in mod_2_06
      pred_hist_raw(list(preds = preds, so = so))

      # bump run id — future sim overlay is now considered stale until re-run
      hist_run_id(hist_run_id() + 1L)

      # ---- append results tab once -----------------------------------------
      if (!hist_tab_added()) {
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "sim_tab",
            shiny::div(id = "compare_section")
          ),
          select  = TRUE,
          session = tabset_session
        )
        hist_tab_added(TRUE)
      } else {
        shiny::updateTabsetPanel(
          session = tabset_session,
          inputId = tabset_id,
          selected = "sim_tab"
        )
      }

    }, ignoreInit = TRUE)



    # expose hist_run_id so mod_2_04 can stamp its result
    list(
      hist_sim    = pred_hist_raw,
      hist_run_id = hist_run_id
    )
  })
}
