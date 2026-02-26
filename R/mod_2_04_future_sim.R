#' 2_04_future_sim UI Function
#'
#' @description A shiny Module. Triggers the future climate welfare simulation.
#'   Results are overlaid on the historical simulation Results tab - no
#'   separate tab is created.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_04_future_sim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::actionButton(
      ns("run_fut_sim"),
      "Run future simulation",
      class = "btn-primary",
      style = "width: 100%; margin-top: 8px;"
    )
  )
}

#' 2_04_future_sim Server Functions
#'
#' Runs the future climate welfare simulation using CMIP6 delta perturbation
#' applied to historical weather observations. Mirrors the pipeline of
#' mod_2_02_historical_sim_server() but passes SSP scenario arguments to
#' get_weather(). Results are stored in pred_fut_raw and overlaid on the
#' historical Results tab - no separate output tab is created here.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param model_fit        Reactive list returned by fit_model() (via
#'   mod_1_07_results). Must contain \code{$fit3} (the fitted model object),
#'   \code{$engine} (character), and \code{$train_data} (the complete-case
#'   data frame used for fitting).
#' @param selected_hist    Reactive one-row data frame from mod_2_01_historical.
#' @param selected_fut     Reactive one-row data frame from mod_2_03_future,
#'   containing columns year_range, ssp, method, and residuals.
#' @param hist_run_id      Reactive integer from mod_2_02_historical_sim that
#'   increments each time the historical sim runs. Stamped onto the result so
#'   the historical plot can detect and drop stale future overlays.
#'
#' @noRd
mod_2_04_future_sim_server <- function(id,
                                        connection_params,
                                        selected_outcome,
                                        selected_weather,
                                        survey_weather,
                                        model_fit,
                                        selected_hist,
                                        selected_fut,
                                        hist_run_id = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    pred_fut_raw <- reactiveVal(NULL)

    # ---- main simulation observer ------------------------------------------
    observeEvent(input$run_fut_sim, {
      req(selected_weather(), selected_outcome(), survey_weather(),
          selected_hist(), selected_fut(), model_fit())

      sw  <- selected_weather()
      so  <- selected_outcome()
      sh  <- selected_hist()
      sf  <- selected_fut()
      svy <- survey_weather()
      mf  <- model_fit()

      # Unpack what predict_outcome() needs from the fit_model() result.
      # Mirrors the same pattern used in mod_2_02_historical_sim_server().
      model      <- mf$fit3
      engine     <- mf$engine
      train_data <- mf$train_data

      # date grid uses survey months + full historical year range (same as
      # historical sim) so the CMIP6 baseline period matches exactly
      sim_dates <- build_hist_sim_dates(svy, sh$year_range)

      # future period from selected_fut year_range
      yr <- unlist(sf$year_range)
      future_period <- c(
        paste0(yr[1], "-01-01"),
        paste0(yr[2], "-12-31")
      )

      # perturbation method inferred from variable units
      perturbation_method <- build_perturbation_method(sw)

      notif_load <- shiny::showNotification(
        paste0("Loading future climate weather (", sf$ssp[1], ")..."),
        duration = NULL, type = "message"
      )

      weather_raw <- tryCatch(
        get_weather(
          survey_data         = svy,
          selected_weather    = sw,
          dates               = sim_dates,
          connection_params   = connection_params(),
          ssp                 = sf$ssp[1],
          future_period       = future_period,
          perturbation_method = perturbation_method
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Failed to load future weather data:", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )

      shiny::removeNotification(notif_load)
      req(!is.null(weather_raw))

      # join weather back to survey covariates
      survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)

      # simulate outcomes
      preds <- predict_outcome(
        model      = model,
        newdata    = survey_wd_sim,
        residuals  = sf$residuals,
        outcome    = so$name,
        id         = NULL,
        train_data = train_data,
        engine     = engine
      )

      preds <- apply_log_backtransform(preds, so)

      # stamp the current hist_run_id so the historical renderPlot knows
      # this overlay was produced from the same historical run
      pred_fut_raw(list(
        preds       = preds,
        so          = so,
        hist_run_id = hist_run_id()
      ))

      shiny::showNotification(
        paste0("Future simulation (", sf$ssp[1], ") complete - results overlaid on Results tab."),
        type = "message", duration = 4
      )

    }, ignoreInit = TRUE)

    list(fut_sim = pred_fut_raw)
  })
}