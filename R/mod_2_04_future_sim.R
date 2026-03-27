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
#' Iterates sequentially over every row of selected_fut (one row per SSP x
#' anchor year), calling get_weather() once per row. Progress is shown via
#' Shiny's withProgress so the user sees per-scenario updates.
#'
#' Sequential execution is used rather than parallel (furrr/future) because:
#'   1. get_weather() opens a DuckDB connection to parquet files on disk.
#'      Concurrent DuckDB writers/readers on the same files (especially on
#'      OneDrive) cause file-locking stalls.
#'   2. multisession on Windows serialises the full R environment per worker,
#'      adding overhead that exceeds the compute saving for typical n=9 runs.
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
    ns <- session$ns

    # ---- Internal state ----------------------------------------------------

    pred_fut_raw <- reactiveVal(NULL)

    # ---- observeEvent handlers ---------------------------------------------

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
      cp                  <- connection_params()
      n_total             <- nrow(sf)

      preds_list_raw <- vector("list", n_total)

      # ---- Sequential loop with per-scenario progress ----------------------
      # withProgress renders a progress bar in the Shiny UI. Each scenario
      # increments the bar so the user can see work is proceeding.
      shiny::withProgress(
        message = paste0("Running future simulation (0 / ", n_total, ")"),
        value   = 0,
        {
          for (i in seq_len(n_total)) {
            row       <- sf[i, , drop = FALSE]
            ssp_val   <- as.character(row$ssp)
            yr        <- unlist(row$year_range)
            scen_name <- row$scenario_name

            shiny::setProgress(
              value   = (i - 1) / n_total,
              message = paste0("Running future simulation (", i - 1, " / ", n_total, ")"),
              detail  = scen_name
            )

            future_period <- c(
              paste0(yr[1], "-01-01"),
              paste0(yr[2], "-12-31")
            )

            weather_raw <- tryCatch(
              {
                wr <- get_weather(
                  survey_data         = svy,
                  selected_weather    = sw,
                  dates               = sim_dates,
                  connection_params   = cp,
                  ssp                 = ssp_val,
                  future_period       = future_period,
                  perturbation_method = perturbation_method,
                  fixed_breaks        = mf$bin_cutoffs
                )
                wr$result_raw
              },
              error = function(e) {
                shiny::showNotification(
                  paste0("Failed to load weather for '", scen_name, "': ",
                         conditionMessage(e)),
                  type = "warning", duration = 6
                )
                NULL
              }
            )

            if (is.null(weather_raw)) {
              preds_list_raw[[i]] <- NULL
              next
            }

            # ---- Simulate outcomes -----------------------------------------
            # run_sim_pipeline(): join weather -> predict_outcome -> back-transform.
            # Returns list(preds, n_pre_join).

            pipeline_out <- run_sim_pipeline(
              weather_raw = weather_raw,
              svy         = svy,
              sw          = sw,
              so          = so,
              model       = model,
              residuals   = as.character(row$residuals),
              train_data  = train_data,
              engine      = engine
            )

            preds_list_raw[[i]] <- if (is.null(pipeline_out)) NULL else list(
              preds       = pipeline_out$preds,
              weather_raw = weather_raw
            )
          }

          shiny::setProgress(value = 1, message = "Future simulation complete")
        }
      )

      # ---- Collect results --------------------------------------------------

      names(preds_list_raw) <- sf$scenario_name

      ok         <- !vapply(preds_list_raw, is.null, logical(1))
      preds_list <- preds_list_raw[ok]

      failed_nms <- sf$scenario_name[!ok]
      if (length(failed_nms) > 0) {
        shiny::showNotification(
          paste0("Failed scenarios: ", paste(failed_nms, collapse = ", ")),
          type = "error", duration = 10
        )
      }

      if (length(preds_list) == 0) return()

      sf_rows          <- seq_len(n_total)
      yr_lookup        <- lapply(sf_rows[ok], function(i) unlist(sf[i, ]$year_range))
      names(yr_lookup) <- sf$scenario_name[ok]

      pred_fut_raw(list(
        preds_list  = preds_list,
        so          = so,
        hist_run_id = hist_run_id(),
        year_range  = yr_lookup
      ))

      shiny::showNotification(
        paste0("\u2713 Future simulation complete: ", length(preds_list), " / ",
               n_total, " scenario(s) succeeded."),
        type = "message", duration = 6
      )

    }, ignoreInit = TRUE)

    # ---- Return API --------------------------------------------------------

    list(fut_sim = pred_fut_raw)
  })
}
