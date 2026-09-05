#' 1_08_modelfit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_08_modelfit_ui <- function(id) {
  tagList()
}

#' 1_08_modelfit Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_outcome Reactive one-row data frame from mod_1_03_outcome.
#' @param model_fit       Reactive list returned by fit_model() via
#'   mod_1_07_results.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset (defaults to parent).
#'
#' @noRd
mod_1_08_modelfit_server <- function(id,
                                      variable_list,
                                      selected_outcome,
                                      model_fit,
                                      tabset_id,
                                      survey_weather,
                                      fit_stale = reactive(FALSE),
                                      tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # INT-08: stale banner bound to the fit's staleness flag.
    output$fit_stale_banner <- shiny::renderUI({
      if (isTRUE(fit_stale())) .stale_banner("Step 1 model diagnostics") else NULL
    })

    # ---- Internal state -----------------------------------------------------

    modelfit_tab_added <- reactiveVal(FALSE)

    # ---- Helpers ------------------------------------------------------------

    # INT-05: bind diagnostic renderers to the fit-time snapshot so new
    # selections cannot relabel or re-frame an already-fitted model.
    fit_snap <- reactive({
      req(model_fit())
      model_fit()$.snap
    })
    snap_label_fun <- reactive({
      .label_lookup(fit_snap()$variable_list)
    })

    full_model <- reactive({
      req(model_fit())
      mf <- model_fit()
      fit <- extract_native_fit(mf$fit3, mf$engine)
      # For RIF: return the full fixest_multi for calc_fit_stats,
      # but diagnostic plots that need a single model use rif_single_model()
      fit
    })

    # Single representative model for diagnostics (median quantile for RIF)
    rif_single_model <- reactive({
      req(model_fit())
      mf <- model_fit()
      extract_rif_median(mf$fit3, mf$engine)
    })

    is_logistic <- reactive({
      req(model_fit())
      is_logistic_fit(model_fit())
    })

    # ---- Outputs ------------------------------------------------------------

    output$resid_weather1 <- renderPlot({
      req(full_model(), model_fit(), fit_snap())
      h <- model_fit()$weather_terms[1]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h, weather_df = fit_snap()$survey_weather,
                         x_label = snap_label_fun()(h))
    })

    output$resid_weather2 <- renderPlot({
      req(full_model(), model_fit(), length(model_fit()$weather_terms) >= 2, fit_snap())
      h <- model_fit()$weather_terms[2]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h, weather_df = fit_snap()$survey_weather,
                         x_label = snap_label_fun()(h))
    })

    # UI-48: one builder behind the plot and its export, so a downloaded PNG
    # is the figure on screen.
    pred_welf_fig <- function() {
      mf   <- model_fit()
      snap <- fit_snap()
      slf  <- snap_label_fun()
      if (is.null(mf) || is.null(snap)) return(NULL)
      if (identical(mf$engine, "rif")) {
        # RIF models predict the RIF-transformed outcome (effectively binary
        # per quantile), so the standard predicted-vs-actual histogram is not
        # meaningful. Instead show the original welfare distribution with
        # predicted quantile markers.
        y <- mf$train_data[[mf$y_var]]
        taus <- mf$taus
        q_vals <- stats::quantile(y, probs = taus, names = FALSE)
        q_df <- data.frame(tau = paste0("\u03c4=", taus), value = q_vals)
        ggplot2::ggplot(data.frame(y = y), ggplot2::aes(x = y)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
            fill = "steelblue", alpha = 0.7, bins = 30
          ) +
          ggplot2::geom_vline(data = q_df, ggplot2::aes(xintercept = value),
                              linetype = "dashed", colour = "orange", linewidth = 0.5) +
          ggplot2::geom_text(data = q_df,
                             ggplot2::aes(x = value, y = Inf, label = tau),
                             vjust = 1.5, hjust = -0.1, size = 3, colour = "orange") +
          ggplot2::labs(
            subtitle = "Welfare distribution with estimated quantiles",
            x = stringr::str_wrap(slf(snap$outcome$name), 40),
            y = "Share of households (%)"
          ) +
          theme_wise()
      } else {
        m <- rif_single_model()
        plot_pred_vs_actual(
          model         = m,
          is_logistic   = is_logistic(),
          outcome_label = slf(snap$outcome$name)
        )
      }
    }

    output$pred_welf_dist <- renderPlot({
      req(full_model(), fit_snap())
      pred_welf_fig()
    })

    # UI-45: one data frame behind both the table and its CSV export.
    additional_stats_df <- reactive({
      req(full_model(), model_fit())
      calc_fit_stats(
        model       = full_model(),
        is_logistic = is_logistic(),
        engine      = model_fit()$engine
      )
    })

    output$additional_stats <- renderTable(
      additional_stats_df(),
      striped = TRUE, hover = TRUE, bordered = TRUE
    )

    output$additional_stats_csv <- csv_download_handler(
      "model_fit_statistics",
      function() additional_stats_df()
    )

    wise_export_table(
      key   = "model_fit_statistics",
      label = "Model fit statistics",
      step  = 1L,
      fun   = function() tryCatch(additional_stats_df(),
                                  error = function(e) NULL),
      description = paste(
        "Goodness-of-fit measures for the full specification: R-squared,",
        "within R-squared and related statistics."
      )
    )

    # Relative importance plot (standardized coefficients)
    output$relaimpo <- renderPlot({
      req(full_model(), fit_snap())

      m <- rif_single_model()
      plot_relaimpo(
        model = m,
        var_info = fit_snap()$variable_list
      )
    })

    output$relaimpo_ui <- renderUI({
      req(model_fit())

      tagList(
        shiny::h4(
          "Relative importance of predictors",
          info_popover(
            p(paste(
              "Importance is computed as |\u03B2| \u00D7 sd(X), i.e. the absolute",
              "standardized coefficient. This fast method works for both",
              "linear and logistic models and handles interactions and many",
              "predictors robustly."
            ))
          )
        ),
        wise_plot_output(ns("relaimpo"),
                         "Bar plot of the relative importance of model predictors")
      )
    })

    output$diagnostic_plots <- renderPlot({
      req(full_model())
      m <- rif_single_model()
      plot_diagnostics(m, engine = model_fit()$engine)
    })

    # UI-48: model-fit figures for the export bundle. Each builder returns NULL
    # when its inputs are not ready, so an un-run step contributes nothing.
    local({
      guard <- function(f) function() {
        if (is.null(tryCatch(full_model(), error = function(e) NULL))) return(NULL)
        tryCatch(f(), error = function(e) NULL)
      }

      for (i in 1:2) local({
        idx <- i
        wise_export_figure(
          key   = paste0("residuals_vs_weather_", idx),
          label = paste0("Residuals vs weather ", idx),
          step  = 1L,
          fun   = guard(function() {
            mf <- model_fit()
            h  <- mf$weather_terms[idx]
            if (is.na(h) || is.null(h)) return(NULL)
            plot_resid_weather(rif_single_model(), h,
                               weather_df = fit_snap()$survey_weather,
                               x_label = snap_label_fun()(h))
          }),
          description = paste(
            "Model residuals against the realised weather variable, for",
            "checking that no systematic structure is left unexplained."
          ),
          width = 9, height = 6
        )
      })

      wise_export_figure(
        key   = "predicted_vs_actual_welfare",
        label = "Predicted vs actual welfare",
        step  = 1L,
        fun   = guard(function() pred_welf_fig()),
        description = paste(
          "Distribution of predicted welfare against observed welfare in the",
          "training data (for RIF models, the welfare distribution with",
          "predicted quantile markers)."
        ),
        width = 9, height = 6
      )

      wise_export_figure(
        key   = "relative_importance",
        label = "Relative importance of predictors",
        step  = 1L,
        fun   = guard(function() plot_relaimpo(rif_single_model(),
                                               var_info = fit_snap()$variable_list)),
        description = paste(
          "Absolute standardised coefficient |beta| x sd(X) per predictor,",
          "ranking how much each contributes to fitted welfare."
        ),
        width = 9, height = 6
      )

      wise_export_figure(
        key   = "model_diagnostics",
        label = "Model diagnostic plots",
        step  = 1L,
        fun   = guard(function() plot_diagnostics(rif_single_model(),
                                                  engine = model_fit()$engine)),
        description = "Standard regression diagnostic panels for the full specification.",
        width = 10, height = 8
      )
    })

    output$model_summary <- renderPrint({
      req(full_model())
      m <- rif_single_model()
      if (identical(model_fit()$engine, "rif")) {
        cat("Unconditional quantile regression (RIF) - Median quantile (tau = 0.5):\n\n")
      }
      vcov_spec <- tryCatch(.fixest_vcov_spec(m), error = function(e) NULL)
      if (is.null(vcov_spec)) {
        summary(m)
      } else {
        summary(m, vcov = vcov_spec)
      }
    })

    # ---- Add tab (once) -----------------------------------------------------

    # Reactive layout: 1 panel for 1 weather var, 2 side-by-side for >= 2.
    # Wrapping in renderUI keeps the layout in sync if the model is re-fit
    # with a different number of weather variables.
    output$resid_weather_layout <- shiny::renderUI({
      req(model_fit())
      wt <- model_fit()$weather_terms %||% character(0)
      weather_plot_layout(
        ns, length(wt),
        ids    = c("resid_weather1", "resid_weather2"),
        height = "300px",
        alts   = vapply(seq_len(max(length(wt), 1L)), function(i) {
          paste("Scatter plot of model residuals versus", wt[i],
                "with the fitted relationship")
        }, character(1))
      )
    })

    # INT-05: engine-conditional caption bound to the current fit, so a
    # re-fit with a different engine updates the wording.
    output$full_model_caption <- shiny::renderUI({
      req(model_fit())
      shiny::p(
        if (identical(model_fit()$engine, "rif"))
          "Full model (FE + controls) \u2014 per quantile"
        else
          "Full model (FE + controls)",
        style = "color: grey; font-size: 12px;"
      )
    })

    observeEvent(model_fit(), {
      req(model_fit())
      if (modelfit_tab_added()) return()

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Model fit",
          value = "model_fit",
          shiny::uiOutput(ns("fit_stale_banner")),
          shiny::h4(
            "Fit statistics",
            info_popover(
              p(paste(
                "Standard goodness-of-fit measures for the fitted model \u2014",
                "R-squared, within R-squared, and related statistics \u2014",
                "computed on the full model including fixed effects and controls."
              ))
            )
          ),
          shiny::uiOutput(ns("full_model_caption")),
          shiny::tableOutput(ns("additional_stats")),
          csv_download_link(ns("additional_stats_csv")),
          shiny::hr(),
          shiny::h4("Residuals vs weather"),
          shiny::uiOutput(ns("resid_weather_layout")),
          shiny::hr(),
          shiny::uiOutput(ns("relaimpo_ui")),
          shiny::hr(),
          shiny::h4("Predicted vs actual welfare"),
          bslib::card(wise_plot_output(
            ns("pred_welf_dist"),
            "Scatter plot of predicted welfare versus actual welfare in the training data"
          )),
          shiny::hr(),
          shiny::h4("Diagnostic plots"),
          bslib::card(wise_plot_output(
            ns("diagnostic_plots"),
            "Diagnostic plots for the fitted model: residuals versus fitted values and related checks"
          )),
          shiny::hr(),
          shiny::tags$details(
            shiny::tags$summary("Raw model summary (advanced)"),
            shiny::verbatimTextOutput(ns("model_summary"))
          )
        ),
        select  = FALSE,
        session = tabset_session
      )

      modelfit_tab_added(TRUE)
    }, ignoreInit = TRUE)

  })
}