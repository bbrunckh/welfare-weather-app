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
      extract_native_fit(model_fit()$fit3, model_fit()$engine)
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
      plot_resid_weather(full_model(), h, get_label(h))
    })

    output$resid_weather2 <- renderPlot({
      req(full_model(), model_fit(), length(model_fit()$weather_terms) >= 2)
      h <- model_fit()$weather_terms[2]
      req(!is.na(h))
      plot_resid_weather(full_model(), h, get_label(h))
    })

    output$pred_welf_dist <- renderPlot({
      req(full_model(), selected_outcome())
      plot_pred_vs_actual(
        model         = full_model(),
        is_logistic   = is_logistic(),
        outcome_label = get_label(selected_outcome()$name)
      )
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

      model <- full_model()

      # Extract model matrix and coefficients safely
      mm <- tryCatch(model.matrix(model), error = function(e) NULL)
      coefs <- tryCatch(coef(model), error = function(e) NULL)

      if (is.null(mm) || is.null(coefs)) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate(
              "text", x = 0.5, y = 0.5,
              label = "Variable importance unavailable.",
              hjust = 0.5
            ) +
            ggplot2::theme_void()
        )
      }

      # Remove intercept
      keep <- names(coefs) != "(Intercept)"
      beta <- coefs[keep]

      if (length(beta) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate(
              "text", x = 0.5, y = 0.5,
              label = "No predictors available.",
              hjust = 0.5
            ) +
            ggplot2::theme_void()
        )
      }

      # Align matrix columns
      X <- mm[, names(beta), drop = FALSE]

      # Compute standardized importance
      sd_x <- apply(X, 2, stats::sd, na.rm = TRUE)
      sd_x[is.na(sd_x)] <- 0

      importance_df <- data.frame(
        Variable = names(beta),
        Importance = abs(beta) * sd_x,
        stringsAsFactors = FALSE
      )

      # Optional: attach human-readable labels
      vl <- if (is.function(variable_list)) variable_list() else variable_list
      if (!is.null(vl)) {
        importance_df <- dplyr::left_join(
          importance_df,
          vl[, c("name", "label"), drop = FALSE],
          by = c("Variable" = "name")
        )
        importance_df$label <- ifelse(
          is.na(importance_df$label),
          importance_df$Variable,
          importance_df$label
        )
      } else {
        importance_df$label <- importance_df$Variable
      }

      # Order and cap to top 30
      importance_df <- importance_df |>
        dplyr::arrange(dplyr::desc(Importance)) |>
        head(30)

      ggplot2::ggplot(
        importance_df,
        ggplot2::aes(x = reorder(label, Importance), y = Importance)
      ) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "",
          y = "Standardized coefficient importance"
        ) +
        ggplot2::theme_minimal()
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
      plot_diagnostics(full_model(), engine = model_fit()$engine)
    })

    output$model_summary <- renderPrint({
      req(full_model())
      summary(full_model())
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
          shiny::p("Full model (FE + controls)",
                   style = "color: grey; font-size: 12px;"),
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