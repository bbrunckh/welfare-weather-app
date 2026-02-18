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
#' @noRd 
mod_1_07_results_server <- function(
    id,
    varlist,
    selected_surveys,
    selected_outcome,
    selected_weather,
    survey_weather,
    model_fit,
    tabset_id,
    tabset_session = NULL
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    results_tab_added <- reactiveVal(FALSE)

    observeEvent(model_fit(), {
      req(model_fit())

      vl <- if (is.function(varlist)) varlist() else varlist
      labels_df <- if (!is.null(vl)) vl[, c("name", "label"), drop = FALSE] else NULL

      get_term_label <- function(term, labels_df) {
        res <- get_name_label(term, varlist = vl)
        res$label %||% term
      }

      create_named_vector <- function(coefs_to_plot, labels_df) {
        named_vector <- vapply(coefs_to_plot, function(coef) {
          sub_terms <- unlist(strsplit(coef, ":"))
          term_labels <- vapply(sub_terms, get_term_label, labels_df = labels_df, FUN.VALUE = character(1))
          paste(term_labels, collapse = " * ")
        }, FUN.VALUE = character(1))
        stats::setNames(coefs_to_plot, named_vector)
      }

      out_lab <- outcome_label()
      if (is.null(out_lab)) {
        out_lab <- selected_outcome() %||% "Outcome"
      }

      if (!results_tab_added()) {

        output$regtable <- renderUI({
          req(model_fit(), labels_df)
          coefs <- names(stats::coef(model_fit()[[3]]))
          coefs_to_plot <- setdiff(coefs, "(Intercept)")
          named_coefs <- create_named_vector(coefs_to_plot, labels_df)
          ht <- jtools::export_summs(
            model_fit()[[1]], model_fit()[[2]], model_fit()[[3]],
            robust = "HC3",
            model.names = c("No FE", "FE", "FE + controls"),
            coefs = named_coefs,
            digits = 3
          )
          htmltools::HTML(huxtable::to_html(ht))
        })

        output$coefplot <- renderPlot({
          req(model_fit(), labels_df)
          coefs <- names(stats::coef(model_fit()[[3]]))
          weather_terms <- grep("haz", coefs, value = TRUE)
          named_coefs <- create_named_vector(weather_terms, labels_df)
          jtools::plot_summs(
            model_fit()[[1]], model_fit()[[2]], model_fit()[[3]],
            robust = "HC3",
            coefs = named_coefs,
            model.names = c("No FE", "FE", "FE + controls")
          ) +
            ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, 20)) +
            ggplot2::labs(x = stringr::str_wrap(paste0("Effect on ", out_lab), 50))
        })

        empty_plot <- function(msg) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, msg)
        }

        output$effectplot1 <- renderPlot({
          req(model_fit(), length(weather_terms()) > 0)
          jtools::effect_plot(
            model_fit()[[3]], pred = !!weather_terms()[1], 
            interval = TRUE, plot.points = FALSE, line.colors = "orange",
            x.label = stringr::str_wrap(get_name_label(weather_terms()[1], varlist = vl)$label, 40), 
            y.label = stringr::str_wrap(out_lab ,40))
        })

        output$effectplot2 <- renderPlot({
          req(model_fit(), length(weather_terms()) > 1)
          jtools::effect_plot(
            model_fit()[[3]], pred = !!weather_terms()[2], 
            interval = TRUE, plot.points = FALSE, line.colors = "orange",
            x.label = stringr::str_wrap(get_name_label(weather_terms()[2], varlist = vl)$label, 40), 
            y.label = stringr::str_wrap(out_lab ,40))
        })

        interactplot1_obj <- shiny::eventReactive(model_fit(), {
          req(model_fit(), interaction_terms(), length(weather_terms()) > 0)
          interactions::interact_plot(
            model_fit()[[3]], pred = !!weather_terms()[1], modx = !!interaction_terms()[1],
            interval = TRUE, plot.points = FALSE,
            x.label = stringr::str_wrap(get_name_label(weather_terms()[1], varlist = vl)$label, 40),
            y.label = stringr::str_wrap(out_lab, 40)
          ) + ggplot2::theme(legend.position = "bottom")
        }, ignoreInit = FALSE)

        output$interactplot1 <- renderPlot({
          p <- interactplot1_obj()
          if (is.null(p)) return(empty_plot("Run model after updating interaction terms."))
          p
        })

        interactplot2_obj <- shiny::eventReactive(model_fit(), {
          req(model_fit(), interaction_terms(), length(weather_terms()) > 1)
          interactions::interact_plot(
            model_fit()[[3]], pred = !!weather_terms()[2], modx = !!interaction_terms()[1],
            interval = TRUE, plot.points = FALSE,
            x.label = stringr::str_wrap(get_name_label(weather_terms()[2], varlist = vl)$label, 40),
            y.label = stringr::str_wrap(out_lab, 40)
          ) + ggplot2::theme(legend.position = "bottom")
        }, ignoreInit = FALSE)

        output$interactplot2 <- renderPlot({
          p <- interactplot2_obj()
          if (is.null(p)) return(empty_plot("Run model after updating interaction terms."))
          p
        })

        # output$interactplot2 <- renderPlot({
        #   req(interaction_terms(), length(weather_terms()) > 1, model_fit())
        #   interactions::interact_plot(
        #     model_fit()[[3]], pred = !!weather_terms()[2], modx = !!interaction_terms()[1],
        #     interval = TRUE, plot.points = FALSE,
        #     x.label = stringr::str_wrap(get_name_label(weather_terms()[2], varlist = vl)$label, 40),
        #     y.label = stringr::str_wrap(out_lab, 40)
        #   ) + ggplot2::theme(legend.position = "bottom")
        # })

        simslopes1_obj <- shiny::eventReactive(model_fit(), {
          req(model_fit(), interaction_terms(), length(weather_terms()) > 0)
          interactions::sim_slopes(model_fit()[[3]], pred = !!weather_terms()[1], modx = !!interaction_terms()[1])
        }, ignoreInit = FALSE)

        output$simslopes1 <- renderPlot({
          p <- simslopes1_obj()
          if (is.null(p)) return(empty_plot("Run model after updating interaction terms."))
          plot(p)
        })

        # output$simslopes1 <- renderPlot({
        #   req(interaction_terms(), model_fit(), weather_terms())
        #   plot(interactions::sim_slopes(model_fit()[[3]], pred = !!weather_terms()[1], modx = !!interaction_terms()[1]))
        # })

        simslopes2_obj <- shiny::eventReactive(model_fit(), {
          req(model_fit(), interaction_terms(), length(weather_terms()) > 1)
          interactions::sim_slopes(model_fit()[[3]], pred = !!weather_terms()[2], modx = !!interaction_terms()[1])
        }, ignoreInit = FALSE)

        output$simslopes2 <- renderPlot({
          p <- simslopes2_obj()
          if (is.null(p)) return(empty_plot("Run model after updating interaction terms."))
          plot(p)
        })

        # output$simslopes2 <- renderPlot({
        #   req(interaction_terms(), model_fit(), weather_terms())
        #   plot(interactions::sim_slopes(model_fit()[[3]], pred = !!weather_terms()[2], modx = !!interaction_terms()[1]))
        # })

        output$interactplots <- renderUI({
          if (length(interaction_terms()) > 0 && length(haz_vars()) > 1) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("interactplot2"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes2"), height = "300px"))
            )
          } else if (length(interaction_terms()) > 0) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes1"), height = "300px"))
            )
          } else {
            shiny::p("No interaction term specified.")
          }
        })

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "results",
            # shiny::h4("Model object (FE + controls)"),
            # shiny::verbatimTextOutput(ns("model_fit_obj")),
            # shiny::br(),
            shiny::h4("Marginal effect of weather on welfare"),
            bslib::card(shiny::plotOutput(ns("coefplot"))),
            shiny::br(),
            shiny::h4("Predicted welfare vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("effectplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("effectplot2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Interactions & adaptation"),
            shiny::uiOutput(ns("interactplots")),
            shiny::br(),
            shiny::h4("Regression results"),
            shiny::uiOutput(ns("regtable"))
          ),
          select = TRUE,
          session = tabset_session
        )

        results_tab_added(TRUE)
      }

    if (results_tab_added()) {
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "results"), silent = TRUE)
    }
  }, ignoreInit = TRUE)
})
}
