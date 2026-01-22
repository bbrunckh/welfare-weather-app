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
    model_fit,
    run_model,
    haz_vars,
    varlist,
    selected_outcome,
    outcome_type,
    outcome_label,
    tabset_id,
    tabset_session = NULL
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    modelfit_tab_added <- reactiveVal(FALSE)

    observeEvent(model_fit(), {
      req(model_fit())

      vl <- if (is.function(varlist)) varlist() else varlist
      label_lookup <- if (!is.null(vl)) stats::setNames(vl$label, vl$varname) else NULL

      out_lab <- outcome_label()
      if (is.null(out_lab)) out_lab <- selected_outcome() %||% "Outcome"

      if (!modelfit_tab_added()) {
        output$resid_weather1 <- renderPlot({
          req(model_fit())
          model <- model_fit()[[3]]
          plot_data <- model.frame(model) |>
            dplyr::select(dplyr::starts_with("haz_")) |>
            dplyr::mutate(residuals = stats::residuals(model))

          h <- colnames(dplyr::select(plot_data, dplyr::starts_with("haz_")))[1]
          xlabel <- label_lookup[h] %||% h

          ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[h]], y = residuals)) +
            ggplot2::geom_point(alpha = 0.1) +
            ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point") +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "", x = stringr::str_wrap(paste0(xlabel, "\n (as configured)"), 40), y = "Residuals")
        })

        output$resid_weather2 <- renderPlot({
          req(length(haz_vars()) > 1, model_fit())
          model <- model_fit()[[3]]
          plot_data <- model.frame(model) |>
            dplyr::select(dplyr::starts_with("haz_")) |>
            dplyr::mutate(residuals = stats::residuals(model))

          h <- colnames(dplyr::select(plot_data, dplyr::starts_with("haz_")))[2]
          xlabel <- label_lookup[h] %||% h

          ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[h]], y = residuals)) +
            ggplot2::geom_point(alpha = 0.1) +
            ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point") +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "", x = stringr::str_wrap(paste0(xlabel, "\n (as configured)"), 40), y = "Residuals")
        })

        output$pred_welf_dist <- renderPlot({
          req(model_fit())
          model <- model_fit()[[3]]
          is_binary <- isTRUE(outcome_type() == "Binary") && inherits(model, "glm") &&
            identical(stats::family(model)$family, "binomial")

          if (!is_binary) {
            actual_values <- model.frame(model)[[1]]
            predicted_values <- stats::predict(model)
            plot_data <- data.frame(
              Type = rep(c("Survey", "Predicted"), each = length(actual_values)),
              Values = c(actual_values, predicted_values)
            )

            ggplot2::ggplot(plot_data, ggplot2::aes(x = Values, fill = Type)) +
              ggplot2::geom_histogram(ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
                                       position = "dodge", alpha = 0.7, bins = 30) +
              ggplot2::labs(x = stringr::str_wrap(out_lab, 40), y = "Share of households (%)") +
              ggplot2::theme_minimal() +
              ggplot2::scale_fill_manual(values = c("Survey" = "steelblue", "Predicted" = "orange"))
          } else {
            actual <- model.frame(model)[[1]]
            predicted <- stats::predict(model, type = "response")
            conf_matrix <- table(
              Predicted = ifelse(predicted > 0.5, 1, 0),
              Actual = actual
            )
            cm_df <- as.data.frame(conf_matrix)
            colnames(cm_df) <- c("Predicted", "Actual", "Freq")
            total <- sum(conf_matrix)
            cm_df$Percent <- cm_df$Freq / total * 100
            cm_df$Actual <- factor(cm_df$Actual, levels = c(0, 1), labels = c("Not poor", "Poor"))
            cm_df$Predicted <- factor(cm_df$Predicted, levels = c(0, 1), labels = c("Not poor", "Poor"))

            ggplot2::ggplot(cm_df, ggplot2::aes(x = Actual, y = Predicted, fill = Percent)) +
              ggplot2::geom_tile(color = "white") +
              ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Percent)), vjust = 1) +
              ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
              ggplot2::labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
              ggplot2::theme_minimal() +
              ggplot2::theme(legend.position = "none")
          }
        })

        output$relaimpo <- renderPlot({
          req(model_fit(), outcome_type() == "Continuous")
          model <- model_fit()[[3]]
          total_r2 <- summary(model)$r.squared
          rel_importance <- relaimpo::calc.relimp(model, type = "lmg")

          importance_df <- data.frame(
            Variable = names(rel_importance$lmg),
            Contribution = rel_importance$lmg
          ) |>
            dplyr::left_join(vl[, c("varname", "label"), drop = FALSE], by = c("Variable" = "varname")) |>
            dplyr::mutate(label = dplyr::if_else(is.na(.data$label), .data$Variable, .data$label))

          ggplot2::ggplot(importance_df, ggplot2::aes(x = reorder(label, Contribution), y = Contribution, fill = Variable)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::geom_text(ggplot2::aes(label = round(Contribution, 3)), hjust = -0.3, size = 4) +
            ggplot2::geom_hline(yintercept = total_r2, linetype = "dashed", color = "red") +
            ggplot2::annotate("text", y = Inf, x = total_r2, label = paste("Total R² =", round(total_r2, 3)), hjust = 1.1, vjust = -0.5, color = "red") +
            ggplot2::coord_flip() +
            ggplot2::labs(x = "", y = "R-squared Contribution") +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::scale_x_discrete(labels = stringr::str_wrap)
        })

        output$model_summary <- renderPrint({
          req(model_fit())
          summary(model_fit()[[3]])
        })

        output$diagnostic_plots <- renderPlot({
          req(model_fit())
          par(mfrow = c(2, 2))
          plot(model_fit()[[3]])
        })

        output$additional_stats <- renderTable({
          req(model_fit())
          model <- model_fit()[[3]]
          is_binary <- isTRUE(outcome_type() == "Binary") && inherits(model, "glm") &&
            identical(stats::family(model)$family, "binomial")

          if (!is_binary) {
            data.frame(
              Stat = c("Observations", "Adjusted R-squared", "F-statistic"),
              Value = c(format(round(stats::nobs(model)), big.mark = ","), round(summary(model)$adj.r.squared, 3), round(summary(model)$fstatistic[1]))
            )
          } else {
            logLik_model <- stats::logLik(model)
            logLik_null <- -0.5 * model$null.deviance
            mcfadden_r2 <- as.numeric(1 - (logLik_model / logLik_null))

            actual <- model.frame(model)[[1]]
            predicted <- stats::predict(model, type = "response")
            conf_matrix <- table(
              Predicted = ifelse(predicted > 0.5, 1, 0),
              Actual = actual
            )

            TP <- conf_matrix["1", "1"]
            TN <- conf_matrix["0", "0"]
            FP <- conf_matrix["1", "0"]
            FN <- conf_matrix["0", "1"]

            accuracy <- (TP + TN) / (TP + TN + FP + FN)
            precision <- TP / (TP + FP)
            recall <- TP / (TP + FN)

            data.frame(
              Stat = c("Observations", "Pseudo R-squared", "AIC", "Accuracy", "Precision", "Recall"),
              Value = c(format(round(stats::nobs(model)), big.mark = ","), round(mcfadden_r2, 3), round(stats::AIC(model)), round(accuracy, 3), round(precision, 3), round(recall, 3))
            )
          }
        })

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Model fit",
            value = "model_fit",
            shiny::h4("Fit statistics"),
            shiny::p("(Model with FE and controls)"),
            shiny::tableOutput(ns("additional_stats")),
            shiny::h4("Residuals vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("resid_weather1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("resid_weather2"), height = "300px"))
            ),
            shiny::uiOutput(ns("relaimpo_ui")),
            shiny::h4("Predicted welfare"),
            shiny::plotOutput(ns("pred_welf_dist")),
            shiny::h4("Diagnostic plots"),
            shiny::plotOutput(ns("diagnostic_plots")),
            shiny::h4("Model summary"),
            shiny::verbatimTextOutput(ns("model_summary")),
            shiny::br()
          ),
          select = TRUE,
          session = tabset_session
        )

        output$relaimpo_ui <- renderUI({
          if (isTRUE(outcome_type() == "Continuous")) {
            tagList(
              shiny::h4("Contribution of Each Variable to Model R-squared"),
              shiny::plotOutput(ns("relaimpo")),
              shiny::helpText(
                "Uses the Lindeman, Merenda, and Gold (LMG) method, which calculates the average increase in R-squared when a predictor is added to the model across all possible orderings of predictors.",
                style = "font-size: 12px;"
              )
            )
          } else NULL
        })

        modelfit_tab_added(TRUE)
      }

      if (modelfit_tab_added()) {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "model_fit"), silent = TRUE)
      }
    }, ignoreInit = TRUE)
  })
}
