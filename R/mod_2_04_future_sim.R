#' 2_04_future_sim UI Function
#'
#' @description A shiny Module. Triggers the future climate welfare simulation.
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
      style = "width:100%; margin-top:8px;"
    )
  )
}

#' 2_04_future_sim Server Functions
#'
#' Runs the future climate welfare simulation using CMIP6 delta perturbation.
#' Iterates over every row of selected_fut (one row per SSP x anchor year),
#' calling get_weather() once per row. A persistent progress notification is
#' shown throughout and updated at each step.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param model_fit        Reactive list with fit3, engine, train_data.
#' @param selected_hist    Reactive one-row data frame from mod_2_01_historical.
#' @param selected_fut     Reactive data frame from mod_2_03_future:
#'   one row per SSP x anchor year.
#' @param hist_run_id      Reactive integer from mod_2_02_historical_sim.
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

    observeEvent(input$run_fut_sim, {
      req(selected_weather(), selected_outcome(), survey_weather(),
          selected_hist(), selected_fut(), model_fit())

      sw  <- selected_weather()
      so  <- selected_outcome()
      sh  <- selected_hist()
      sf  <- selected_fut()
      svy <- survey_weather()
      mf  <- model_fit()

      model      <- mf$fit3
      engine     <- mf$engine
      train_data <- mf$train_data

      sim_dates           <- build_hist_sim_dates(svy, unlist(sh$year_range))
      perturbation_method <- build_perturbation_method(sw)

      n_total    <- nrow(sf)
      preds_list <- vector("list", n_total)
      yr_lookup  <- vector("list", n_total)

      # ---- persistent progress notification ---------------------------------
      shiny::showNotification(
        ui          = paste0("Future simulation: starting (0 / ", n_total, ")..."),
        id          = "fut_sim_progress",
        duration    = NULL,
        type        = "message",
        closeButton = FALSE,
        session     = session
      )
      on.exit(
        shiny::removeNotification("fut_sim_progress", session = session),
        add = TRUE
      )

      for (i in seq_len(n_total)) {
        row         <- sf[i, , drop = FALSE]
        ssp_val     <- as.character(row$ssp)
        scenario_nm <- as.character(row$scenario_name)
        yr          <- unlist(row$year_range)

        # update the persistent notification in-place (no new ID = no leak)
        shiny::showNotification(
          ui          = paste0("Future simulation: ", scenario_nm,
                               " (", i, " / ", n_total, ")..."),
          id          = "fut_sim_progress",
          duration    = NULL,
          type        = "message",
          closeButton = FALSE,
          session     = session
        )

        future_period <- c(
          paste0(yr[1], "-01-01"),
          paste0(yr[2], "-12-31")
        )

        weather_raw <- tryCatch(
          get_weather(
            survey_data         = svy,
            selected_weather    = sw,
            dates               = sim_dates,
            connection_params   = connection_params(),
            ssp                 = ssp_val,
            future_period       = future_period,
            perturbation_method = perturbation_method
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("Failed: ", scenario_nm, "  ", conditionMessage(e)),
              type = "error", duration = 8
            )
            NULL
          }
        )

        if (is.null(weather_raw)) next

        survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)

        preds <- predict_outcome(
          model      = model,
          newdata    = survey_wd_sim,
          residuals  = as.character(row$residuals),
          outcome    = so$name,
          id         = NULL,
          train_data = train_data,
          engine     = engine
        )

        preds <- apply_log_backtransform(preds, so)

        preds_list[[i]] <- preds
        yr_lookup[[i]]  <- yr
      }

      # drop NULLs (failed runs)
      ok                  <- !vapply(preds_list, is.null, logical(1))
      preds_list          <- preds_list[ok]
      yr_lookup           <- yr_lookup[ok]
      names(preds_list)   <- sf$scenario_name[ok]
      names(yr_lookup)    <- sf$scenario_name[ok]

      if (length(preds_list) == 0) return()

      pred_fut_raw(list(
        preds_list  = preds_list,
        so          = so,
        hist_run_id = hist_run_id(),
        year_range  = yr_lookup
      ))

      completed <- paste(names(preds_list), collapse = ", ")
      shiny::showNotification(
        paste0("\u2713 Future simulation complete: ", completed, "."),
        type = "message", duration = 6
      )

    }, ignoreInit = TRUE)

    list(fut_sim = pred_fut_raw)
  })
}
