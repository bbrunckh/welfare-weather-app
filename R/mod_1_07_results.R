#' 1_07_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_07_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::actionButton(ns("run_model"), "Run model",
                        class = "btn-primary", style = "width: 100%;")
  )
}

#' 1_07_results Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param selected_outcome Reactive one-row data frame from mod_1_03_outcome.
#' @param selected_weather Reactive data frame from mod_1_04_weather.
#' @param survey_weather  Reactive data frame from mod_1_05_weatherstats.
#' @param selected_model  Reactive list from mod_1_06_model.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset (defaults to parent).
#'
#' @noRd
mod_1_07_results_server <- function(id,
                                     variable_list,
                                     selected_surveys,
                                     selected_outcome,
                                     selected_weather,
                                     survey_weather,
                                     selected_model,
                                     tabset_id,
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Internal state ------------------------------------------------------

    model_fit_val     <- reactiveVal(NULL)
    results_tab_added <- reactiveVal(FALSE)

    # ---- Helpers -------------------------------------------------------------

    get_label <- function(var_name) {
      vl  <- if (is.function(variable_list)) variable_list() else variable_list
      if (is.null(vl)) return(var_name)
      idx <- match(var_name, vl$name)
      if (is.na(idx)) var_name else as.character(vl$label[idx])
    }

    native_fit <- function(fit) extract_native_fit(fit, model_fit_val()$engine)

    # ---- Run model -----------------------------------------------------------

    observeEvent(input$run_model, {
      req(selected_outcome(), selected_weather(), selected_model(), survey_weather())

      nid <- shiny::showNotification("Fitting models — please wait...",
                                     type = "message", duration = NULL,
                                     closeButton = FALSE)
      on.exit(shiny::removeNotification(nid), add = TRUE)

      df <- prepare_outcome_df(as.data.frame(survey_weather()), selected_outcome())

      fit_list <- tryCatch(
        fit_model(
          df               = df,
          selected_outcome = selected_outcome(),
          selected_weather = selected_weather(),
          selected_model   = selected_model()
        ),
        error = function(e) {
          shiny::showNotification(paste("Model failed:", conditionMessage(e)),
                                  type = "error", duration = 10)
          NULL
        }
      )

      if (!is.null(fit_list)) {
        model_fit_val(fit_list)
        shiny::showNotification("Models fitted successfully.",
                                type = "message", duration = 3)
      }
    }, ignoreInit = TRUE)

    # ---- Render outputs ------------------------------------------------------

    observeEvent(model_fit_val(), {
      req(model_fit_val())

      nid <- shiny::showNotification("Preparing results...",
                                     type = "message", duration = NULL,
                                     closeButton = FALSE)
      on.exit(shiny::removeNotification(nid), add = TRUE)

      mf      <- model_fit_val()
      out_lab <- get_label(mf$y_var)

      # Coefficient plot
      output$coefplot <- renderPlot({
        req(model_fit_val())
        mf <- model_fit_val()
        make_coefplot(
          fit1              = extract_native_fit(mf$fit1, mf$engine),
          fit2              = extract_native_fit(mf$fit2, mf$engine),
          fit3              = extract_native_fit(mf$fit3, mf$engine),
          weather_terms     = mf$weather_terms,
          interaction_terms = mf$interaction_terms,
          outcome_label     = selected_outcome()$label,
          label_fun         = get_label,
          engine            = mf$engine
        )
      })

      # Regression table
      output$regtable <- renderUI({
        req(model_fit_val())
        mf <- model_fit_val()
        make_regtable(
          fit1 = extract_native_fit(mf$fit1, mf$engine),
          fit2 = extract_native_fit(mf$fit2, mf$engine),
          fit3 = extract_native_fit(mf$fit3, mf$engine),
          weather_terms     = mf$weather_terms,
          interaction_terms = mf$interaction_terms,
          label_fun         = get_label,
          engine            = mf$engine
        )
      })


      # Marginal effects plots (one per weather variable)
      output$effectplot1 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
        mf <- model_fit_val()
        make_weather_effect_plot(
          fit               = native_fit(mf$fit3),
          pred_var          = mf$weather_terms[1],
          interaction_terms = mf$interaction_terms,
          is_binned         = identical(selected_weather()$cont_binned[1], "Binned"),
          label_fun         = get_label,
          engine            = mf$engine
        )
      })

      output$effectplot2 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
        mf <- model_fit_val()
        make_weather_effect_plot(
          fit               = native_fit(mf$fit3),
          pred_var          = mf$weather_terms[2],
          interaction_terms = mf$interaction_terms,
          is_binned         = identical(selected_weather()$cont_binned[2], "Binned"),
          label_fun         = get_label,
          engine            = mf$engine
        )
      })

      # ---- Add / switch Results tab -----------------------------------------

      if (!results_tab_added()) {
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "results",
            shiny::h4("Marginal effect of weather on outcome"),
            bslib::card(shiny::plotOutput(ns("coefplot"))),
            shiny::br(),
            shiny::h4("Predicted outcome vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("effectplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("effectplot2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Regression results"),
            shiny::div(
              style = "display:flex; justify-content:center;",
              shiny::uiOutput(ns("regtable"))
            )
          ),
          select  = TRUE,
          session = tabset_session
        )
        results_tab_added(TRUE)
      } else {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id,
                                     selected = "results"), silent = TRUE)
      }

      shiny::showNotification("Results ready.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ---- Return --------------------------------------------------------------

    list(model_fit = model_fit_val)
  })
}