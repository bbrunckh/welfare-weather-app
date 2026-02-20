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
#' @noRd
mod_1_08_modelfit_server <- function(
    id,
    varlist,
    selected_surveys,
    selected_outcome,
    selected_weather,
    model_fit,          # reactive — list returned by fit_weather_model()
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    modelfit_tab_added <- reactiveVal(FALSE)

    # ---- Helpers -------------------------------------------------------------

    # Unwrap parsnip fit to native lm/glm object
    native_fit <- function(parsnip_fit) parsnip_fit$fit

    # Look up a human-readable label for a variable name
    get_label <- function(var_name) {
      vl <- if (is.function(varlist)) varlist() else varlist
      if (is.null(vl)) return(var_name)
      idx <- match(var_name, vl$name)
      if (is.na(idx)) var_name else vl$label[idx]
    }

    # Convenience: the full model (fit3) as a native object
    full_model <- reactive({
      req(model_fit())
      native_fit(model_fit()$fit3)
    })

    # Is the model logistic?
    is_logistic <- reactive({
      req(model_fit())
      identical(model_fit()$model_type, "logistic")
    })

    # Outcome label from selected_outcome
    out_lab <- reactive({
      req(selected_outcome())
      get_label(selected_outcome()$name)
    })

    # ---- Outputs -------------------------------------------------------------

    # Residuals vs weather variable 1
    output$resid_weather1 <- renderPlot({
      req(full_model(), model_fit())
      mf     <- model_fit()
      model  <- full_model()
      h      <- mf$weather_terms[1]
      df     <- model.frame(model)
      req(h %in% names(df))

      plot_data <- data.frame(x = df[[h]], residuals = stats::residuals(model))
      ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = residuals)) +
        ggplot2::geom_point(alpha = 0.1) +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = stringr::str_wrap(get_label(h), 40), y = "Residuals")
    })

    # Residuals vs weather variable 2 (only shown when two weather vars selected)
    output$resid_weather2 <- renderPlot({
      req(full_model(), model_fit(), length(model_fit()$weather_terms) >= 2)
      mf     <- model_fit()
      model  <- full_model()
      h      <- mf$weather_terms[2]
      df     <- model.frame(model)
      req(h %in% names(df))

      plot_data <- data.frame(x = df[[h]], residuals = stats::residuals(model))
      ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = residuals)) +
        ggplot2::geom_point(alpha = 0.1) +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = stringr::str_wrap(get_label(h), 40), y = "Residuals")
    })

    # Predicted vs actual distribution (histogram for linear, confusion matrix for logistic)
    output$pred_welf_dist <- renderPlot({
      req(full_model())
      model <- full_model()

      if (!is_logistic()) {
        actual    <- model.frame(model)[[1]]
        predicted <- stats::predict(model)
        plot_data <- data.frame(
          Type   = rep(c("Survey", "Predicted"), each = length(actual)),
          Values = c(actual, predicted)
        )
        ggplot2::ggplot(plot_data, ggplot2::aes(x = Values, fill = Type)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
            position = "dodge", alpha = 0.7, bins = 30
          ) +
          ggplot2::scale_fill_manual(values = c("Survey" = "steelblue", "Predicted" = "orange")) +
          ggplot2::labs(x = stringr::str_wrap(out_lab(), 40), y = "Share of households (%)") +
          ggplot2::theme_minimal()

      } else {
        actual    <- model.frame(model)[[1]]
        predicted <- stats::predict(model, type = "response")
        conf_matrix <- table(
          Predicted = factor(ifelse(predicted > 0.5, 1, 0), levels = c(0, 1)),
          Actual    = factor(actual, levels = c(0, 1))
        )
        cm_df <- as.data.frame(conf_matrix)
        cm_df$Percent  <- cm_df$Freq / sum(conf_matrix) * 100
        levels(cm_df$Actual)    <- c("No", "Yes")
        levels(cm_df$Predicted) <- c("No", "Yes")

        ggplot2::ggplot(cm_df, ggplot2::aes(x = Actual, y = Predicted, fill = Percent)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Percent)), vjust = 1) +
          ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
          ggplot2::labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none")
      }
    })

    # Relative importance (linear only — uses relaimpo::calc.relimp)
    output$relaimpo <- renderPlot({
      req(full_model(), !is_logistic())
      model    <- full_model()
      total_r2 <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)

      rel_imp <- tryCatch(
        relaimpo::calc.relimp(model, type = "lmg"),
        error = function(e) e
      )

      if (inherits(rel_imp, "error")) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate(
              "text", x = 0.5, y = 0.5,
              label = paste("Relative importance unavailable:", conditionMessage(rel_imp)),
              hjust = 0.5
            ) +
            ggplot2::theme_void()
        )
      }

      vl <- if (is.function(varlist)) varlist() else varlist
      importance_df <- data.frame(
        Variable     = names(rel_imp$lmg),
        Contribution = as.numeric(rel_imp$lmg)
      )
      if (!is.null(vl)) {
        importance_df <- dplyr::left_join(
          importance_df,
          vl[, c("name", "label"), drop = FALSE],
          by = c("Variable" = "name")
        ) |>
          dplyr::mutate(label = dplyr::if_else(is.na(label), Variable, label))
      } else {
        importance_df$label <- importance_df$Variable
      }

      ggplot2::ggplot(importance_df, ggplot2::aes(x = reorder(label, Contribution), y = Contribution)) +
        ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
        ggplot2::geom_text(ggplot2::aes(label = round(Contribution, 3)), hjust = -0.2, size = 3.5) +
        ggplot2::geom_hline(yintercept = total_r2, linetype = "dashed", color = "red") +
        ggplot2::annotate(
          "text", x = Inf, y = total_r2,
          label = paste("Total R\u00b2 =", round(total_r2, 3)),
          hjust = 1.1, vjust = -0.5, color = "red", size = 3.5
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 30)) +
        ggplot2::labs(x = "", y = "R-squared contribution") +
        ggplot2::theme_minimal()
    })

    # Relative importance panel — only rendered for linear models
    output$relaimpo_ui <- renderUI({
      req(model_fit())
      if (!is_logistic()) {
        tagList(
          shiny::h4("Relative importance of predictors"),
          shiny::plotOutput(ns("relaimpo")),
          shiny::helpText(
            "Uses the LMG method (Lindeman, Merenda & Gold): average increase in R\u00b2
             when each predictor is added across all possible orderings.",
            style = "font-size: 12px;"
          )
        )
      }
    })

    # Fit statistics table
    output$additional_stats <- renderTable({
      req(full_model())
      model <- full_model()

      if (!is_logistic()) {
        s <- summary(model)
        data.frame(
          Statistic = c("Observations", "R-squared", "Adjusted R-squared", "F-statistic"),
          Value     = c(
            format(stats::nobs(model), big.mark = ","),
            round(s$r.squared, 3),
            round(s$adj.r.squared, 3),
            round(s$fstatistic[1], 1)
          )
        )
      } else {
        ll_model  <- as.numeric(stats::logLik(model))
        ll_null   <- as.numeric(-0.5 * model$null.deviance)
        mcfadden  <- round(1 - ll_model / ll_null, 3)

        actual    <- model.frame(model)[[1]]
        predicted <- stats::predict(model, type = "response")
        pred_class <- ifelse(predicted > 0.5, 1, 0)
        cm        <- table(Predicted = pred_class, Actual = actual)

        TP <- cm["1", "1"]; TN <- cm["0", "0"]
        FP <- cm["1", "0"]; FN <- cm["0", "1"]

        data.frame(
          Statistic = c("Observations", "McFadden R\u00b2", "AIC",
                        "Accuracy", "Precision", "Recall"),
          Value     = c(
            format(stats::nobs(model), big.mark = ","),
            mcfadden,
            round(stats::AIC(model)),
            round((TP + TN) / (TP + TN + FP + FN), 3),
            round(TP / (TP + FP), 3),
            round(TP / (TP + FN), 3)
          )
        )
      }
    })

    # Standard diagnostic plots
    output$diagnostic_plots <- renderPlot({
      req(full_model())
      par(mfrow = c(2, 2))
      plot(full_model())
    })

    # Full model summary (console-style)
    output$model_summary <- renderPrint({
      req(full_model())
      summary(full_model())
    })

    # ---- Add tab (once) ------------------------------------------------------

    observeEvent(model_fit(), {
      req(model_fit())

      if (!modelfit_tab_added()) {

        has_two_weather <- length(model_fit()$weather_terms) >= 2

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Model fit",
            value = "model_fit",
            shiny::h4("Fit statistics"),
            shiny::p("Full model (FE + controls)", style = "color: grey; font-size: 12px;"),
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

      }
    }, ignoreInit = TRUE)

  })
}