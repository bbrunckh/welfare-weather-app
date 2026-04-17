#' 1_08_modelfit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom vip vi_permute
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
                                      tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Internal state -----------------------------------------------------

    modelfit_tab_added <- reactiveVal(FALSE)

    # ---- Helpers ------------------------------------------------------------

    get_label <- function(var_name) {
      vl <- if (is.function(variable_list)) variable_list() else variable_list
      if (is.null(vl)) return(var_name)
      idx <- match(var_name, vl$name)
      if (is.na(idx)) var_name else as.character(vl$label[idx])
    }

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
      req(full_model(), model_fit())
      h <- model_fit()$weather_terms[1]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h, weather_df = survey_weather(), x_label = get_label(h))
    })

    output$resid_weather2 <- renderPlot({
      req(full_model(), model_fit(), length(model_fit()$weather_terms) >= 2)
      h <- model_fit()$weather_terms[2]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h, weather_df = survey_weather(), x_label = get_label(h))
    })

    output$pred_welf_dist <- renderPlot({
      req(full_model(), selected_outcome())
      mf <- model_fit()
      if (identical(mf$engine, "rif")) {
        # RIF models predict the RIF-transformed outcome (effectively binary
        # per quantile), so the standard predicted-vs-actual histogram is not
        # meaningful. Instead show the original welfare distribution with
        # predicted quantile markers.
        y <- mf$train_data[[mf$y_var]]
        taus <- mf$taus
        q_vals <- stats::quantile(y, probs = taus, names = FALSE)
        q_df <- data.frame(tau = paste0("\u03C4=", taus), value = q_vals)
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
            title = "Welfare distribution with estimated quantiles",
            x = stringr::str_wrap(get_label(selected_outcome()$name), 40),
            y = "Share of households (%)"
          ) +
          ggplot2::theme_minimal(base_size = 14)
      } else {
        m <- rif_single_model()
        plot_pred_vs_actual(
          model         = m,
          is_logistic   = is_logistic(),
          outcome_label = get_label(selected_outcome()$name)
        )
      }
    })

    output$additional_stats <- renderTable({
      req(full_model(), model_fit())
      calc_fit_stats(
        model       = full_model(),
        is_logistic = is_logistic(),
        engine      = model_fit()$engine
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Relative importance plot (standardized coefficients)
    output$relaimpo <- renderPlot({
      req(full_model())

      vl <- if (is.function(variable_list)) variable_list() else variable_list
      m <- rif_single_model()
      plot_relaimpo(
        model = m,
        var_info = vl
      )
    })

    output$relaimpo_ui <- renderUI({
      req(model_fit())

      tagList(
        shiny::h4("Relative importance of predictors"),
        shiny::plotOutput(ns("relaimpo")),
        shiny::helpText(
          "Importance is computed as |β| × sd(X), i.e. the absolute standardized coefficient.
          This fast method works for both linear and logistic models and handles
          interactions and many predictors robustly.",
          style = "font-size: 12px;"
        )
      )
    })

    output$diagnostic_plots <- renderPlot({
      req(full_model())
      m <- rif_single_model()
      plot_diagnostics(m, engine = model_fit()$engine)
    })

    output$model_summary <- renderPrint({
      req(full_model())
      m <- rif_single_model()
      if (identical(model_fit()$engine, "rif")) {
        cat("Quantile regression (RIF) - Median quantile (tau = 0.5):\n\n")
      }
      summary(m)
    })

    # ---- Add tab (once) -----------------------------------------------------

    observeEvent(model_fit(), {
      req(model_fit())
      if (modelfit_tab_added()) return()

      has_two_weather <- length(model_fit()$weather_terms) >= 2

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Model fit",
          value = "model_fit",
          shiny::h4("Fit statistics"),
          shiny::p(
            if (identical(model_fit()$engine, "rif"))
              "Full model (FE + controls) \u2014 per quantile"
            else
              "Full model (FE + controls)",
            style = "color: grey; font-size: 12px;"
          ),
          shiny::tableOutput(ns("additional_stats")),
          shiny::hr(),
          shiny::h4("Residuals vs weather"),
          if (has_two_weather) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("resid_weather1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("resid_weather2"), height = "300px"))
            )
          } else {
            bslib::card(shiny::plotOutput(ns("resid_weather1"), height = "300px"))
          },
          shiny::hr(),
          shiny::uiOutput(ns("relaimpo_ui")),
          shiny::hr(),
          shiny::h4("Predicted vs actual welfare"),
          bslib::card(shiny::plotOutput(ns("pred_welf_dist"))),
          shiny::hr(),
          shiny::h4("Diagnostic plots"),
          bslib::card(shiny::plotOutput(ns("diagnostic_plots"))),
          shiny::hr(),
          shiny::h4("Model summary"),
          shiny::verbatimTextOutput(ns("model_summary"))
        ),
        select  = FALSE,
        session = tabset_session
      )

      modelfit_tab_added(TRUE)
    }, ignoreInit = TRUE)

  })
}