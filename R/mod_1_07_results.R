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
  tagList()
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
#' @param fit_guard       Busy guard shared with mod_1_06 (REACT-02); optional.
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
                                     model_type,
                                     run_model,
                                     fit_guard = NULL,
                                     survey_version = reactive(0L),
                                     tabset_id,
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Internal state ------------------------------------------------------

    model_fit_val     <- reactiveVal(NULL)
    results_tab_added <- reactiveVal(FALSE)
    # INT-08: TRUE while the stored fit's run signature no longer matches the
    # current upstream inputs.
    stale             <- reactiveVal(FALSE)

    # ---- Run signature (INT-08) ----------------------------------------------
    # Immutable snapshot of everything the fit depends on; recomputed from
    # live inputs and compared with the stored fit's signature.

    .fit_sig_from_live <- function() {
      sw <- survey_weather()
      list(
        step           = "fit",
        survey_version = survey_version(),
        survey_shape   = if (is.null(sw)) NULL else c(nrow(sw), ncol(sw)),
        outcome        = .sig_plain(selected_outcome()),
        weather        = .sig_plain(selected_weather()),
        model          = .sig_plain(selected_model())
      )
    }

    observeEvent(survey_weather(), {
      mf <- model_fit_val()
      if (!is.null(mf) && !identical(.fit_sig_from_live(), mf$.sig)) stale(TRUE)
    }, ignoreInit = TRUE)
    observeEvent(selected_outcome(), {
      mf <- model_fit_val()
      if (!is.null(mf) && !identical(.fit_sig_from_live(), mf$.sig)) stale(TRUE)
    }, ignoreInit = TRUE)
    observeEvent(selected_weather(), {
      mf <- model_fit_val()
      if (!is.null(mf) && !identical(.fit_sig_from_live(), mf$.sig)) stale(TRUE)
    }, ignoreInit = TRUE)
    observeEvent(selected_model(), {
      mf <- model_fit_val()
      if (!is.null(mf) && !identical(.fit_sig_from_live(), mf$.sig)) stale(TRUE)
    }, ignoreInit = TRUE)
    observeEvent(survey_version(), {
      mf <- model_fit_val()
      if (!is.null(mf) && !identical(.fit_sig_from_live(), mf$.sig)) stale(TRUE)
    }, ignoreInit = TRUE)

    output$stale_banner <- renderUI({
      if (isTRUE(stale())) .stale_banner("Step 1 model results") else NULL
    })

    # REACT-14: persistent provenance banner for specification fallbacks
    # (logistic -> linear, clustered -> unclustered VCV) recorded by
    # fit_model(). The results below come from the fitted specification, so
    # the deviation from the requested one must stay visible.
    output$fallback_banner <- renderUI({
      fb <- model_fit_val()$fallbacks %||% list()
      if (!length(fb)) return(NULL)
      items <- lapply(fb, function(x) {
        shiny::tags$li(sprintf(
          "%s: requested %s, fitted %s (%s).",
          switch(x$kind,
                 model_family = "Model family",
                 vcv          = "Standard errors",
                 x$kind),
          x$requested, x$used, x$reason
        ))
      })
      shiny::div(
        class = "alert alert-warning",
        role  = "alert",
        style = "margin-bottom: 10px;",
        shiny::tags$b(
          "\u26a0 The fitted model differs from the requested specification."
        ),
        "The fit fell back as follows; all results below come from the",
        "fitted specification:",
        shiny::tags$ul(items)
      )
    })

    native_fit <- function(fit) extract_native_fit(fit, model_fit_val()$engine)

    # ---- Run model -----------------------------------------------------------
    # The "Run model" button lives in mod_1_06_model; the reactive `run_model`
    # parameter wraps that button's input counter and fires here on click.

    observeEvent(run_model(), {
      req(selected_outcome(), selected_weather(), selected_model(), survey_weather())
      # REACT-02: honour the shared mod_1_06 guard; one fit at a time.
      if (!is.null(fit_guard)) {
        if (!fit_guard$begin()) return(invisible(NULL))
        on.exit(fit_guard$end(), add = TRUE)
      }

      nid <- shiny::showNotification("Fitting models - please wait...",
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
        # INT-05: snapshot every label/setting the renderers need at fit time.
        # Result renderers must describe the fitted run, not whatever is
        # selected when they re-render.
        fit_list$.snap <- list(
          outcome        = selected_outcome(),
          weather        = selected_weather(),
          survey_weather = survey_weather(),
          variable_list  = if (is.function(variable_list)) variable_list() else variable_list
        )
        # INT-08: the run signature is stored with the result and compared
        # against live inputs; a mismatch marks the results stale.
        fit_list$.sig <- .fit_sig_from_live()
        stale(FALSE)
        model_fit_val(fit_list)
        shiny::showNotification("Models fitted successfully.",
                                type = "message", duration = 3)

        # REACT-14: disclose any specification fallback the fitter applied.
        # A model-family change (logistic -> linear) alters the estimand, so
        # it additionally requires explicit acknowledgement.
        fb <- fit_list$fallbacks %||% list()
        if (length(fb)) {
          shiny::showNotification(
            paste0(
              "Models fitted with specification fallbacks (see the banner ",
              "on the Results tab)."
            ),
            type = "warning", duration = 10
          )
          family_fb <- Filter(function(x) identical(x$kind, "model_family"), fb)
          if (length(family_fb)) {
            shiny::showModal(shiny::modalDialog(
              title = "Model family fallback",
              shiny::tags$p(
                "The requested logistic regression could not be fitted and",
                " the model fell back to linear. All Step 1-3 results use the",
                " fitted specification unless you re-fit:"
              ),
              shiny::tags$ul(
                lapply(family_fb, function(x)
                  shiny::tags$li(sprintf("%s (%s).", x$reason, x$used)))
              ),
              easyClose = FALSE,
              footer    = shiny::modalButton("I understand")
            ))
          }
        }
      }
    }, ignoreInit = TRUE)

    # ---- Render outputs ------------------------------------------------------

    observeEvent(model_fit_val(), {
      req(model_fit_val(), selected_weather())

      nid <- shiny::showNotification("Preparing results...",
                                     type = "message", duration = NULL,
                                     closeButton = FALSE)
      on.exit(shiny::removeNotification(nid), add = TRUE)

      mf      <- model_fit_val()
      snap    <- mf$.snap
      # INT-05: every renderer below binds to the fit-time snapshot; changing
      # the outcome/weather selections afterwards cannot relabel old results.
      label_fun <- .label_lookup(snap$variable_list)
      sw_snap    <- snap$weather
      outcome_snap <- snap$outcome

      # RIF coefficient plots: one per weather variable
      output$coefplot1 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
        mf <- model_fit_val()
        make_coefplot(
          fit1              = extract_native_fit(mf$fit1, mf$engine),
          fit2              = extract_native_fit(mf$fit2, mf$engine),
          fit3              = extract_native_fit(mf$fit3, mf$engine),
          weather_terms     = mf$weather_terms,
          interaction_terms = mf$interaction_terms,
          outcome_label     = outcome_snap$label,
          label_fun         = label_fun,
          engine            = mf$engine,
          rif_grid          = mf$rif_grid,
          pred_var          = mf$weather_terms[1]
        )
      })

      output$coefplot2 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
        mf <- model_fit_val()
        make_coefplot(
          fit1              = extract_native_fit(mf$fit1, mf$engine),
          fit2              = extract_native_fit(mf$fit2, mf$engine),
          fit3              = extract_native_fit(mf$fit3, mf$engine),
          weather_terms     = mf$weather_terms,
          interaction_terms = mf$interaction_terms,
          outcome_label     = outcome_snap$label,
          label_fun         = label_fun,
          engine            = mf$engine,
          rif_grid          = mf$rif_grid,
          pred_var          = mf$weather_terms[2]
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
          label_fun         = label_fun,
          engine            = mf$engine,
          is_logistic       = is_logistic_fit(mf),
          rif_grid          = mf$rif_grid
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
          is_binned         = identical(sw_snap$cont_binned[1], "Binned"),
          label_fun         = label_fun,
          engine            = mf$engine,
          selected_weather  = sw_snap,
          weather_df        = snap$survey_weather,
          rif_grid          = mf$rif_grid
        )
      })

      output$effectplot2 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
        mf <- model_fit_val()
        make_weather_effect_plot(
          fit               = native_fit(mf$fit3),
          pred_var          = mf$weather_terms[2],
          interaction_terms = mf$interaction_terms,
          is_binned         = identical(sw_snap$cont_binned[2], "Binned"),
          label_fun         = label_fun,
          engine            = mf$engine,
          selected_weather  = sw_snap,
          weather_df        = snap$survey_weather,
          rif_grid          = mf$rif_grid
        )
      })

      # ---- Add / switch Results tab -----------------------------------------

      # INT-05: engine-conditional headings are reactive outputs bound to the
      # current fit, so a re-fit with a different engine updates them instead
      # of describing the first engine forever.
      output$heading_effect <- renderUI({
        req(model_fit_val())
        shiny::h4(if (identical(model_fit_val()$engine, "rif"))
          "Weather sensitivity across the distribution"
          else "Predicted outcome vs weather")
      })
      output$heading_coef <- renderUI({
        req(model_fit_val())
        shiny::h4(if (identical(model_fit_val()$engine, "rif"))
          "UQR coefficients by model specification"
          else "Marginal effect of weather on outcome")
      })
      output$heading_table <- renderUI({
        req(model_fit_val())
        shiny::h4(if (identical(model_fit_val()$engine, "rif"))
          "Unconditional quantile regression results"
          else "Regression results")
      })

      # Reactive layouts so panels switch between 1 and 2 columns when the
      # model is re-fit with a different number of weather variables.
      output$effectplot_layout <- shiny::renderUI({
        req(model_fit_val())
        weather_plot_layout(
          ns, length(model_fit_val()$weather_terms %||% character(0)),
          ids    = c("effectplot1", "effectplot2"),
          height = "500px"
        )
      })
      output$coefplot_layout <- shiny::renderUI({
        req(model_fit_val())
        weather_plot_layout(
          ns, length(model_fit_val()$weather_terms %||% character(0)),
          ids    = c("coefplot1", "coefplot2"),
          height = "600px"
        )
      })

      if (!results_tab_added()) {
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "results",
            shiny::uiOutput(ns("fallback_banner")),
            shiny::uiOutput(ns("stale_banner")),
            shiny::uiOutput(ns("heading_effect")),
            shiny::uiOutput(ns("effectplot_layout")),
            shiny::br(),
            shiny::uiOutput(ns("heading_coef")),
            shiny::uiOutput(ns("coefplot_layout")),
            shiny::br(),
            shiny::uiOutput(ns("heading_table")),
            shiny::div(
              style = "display:flex; justify-content:center;",
              shiny::div(
                style = "overflow-x: auto; max-width: 100%;",
                shiny::uiOutput(ns("regtable"))
              )
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

    list(model_fit = model_fit_val, stale = stale)
  })
}