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
    model_fit,
    run_model,
    haz_vars,
    varlist,
  interactions,
    selected_outcome,
    outcome_label,
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
    label_lookup <- if (!is.null(vl)) stats::setNames(vl$label, vl$varname) else NULL
    labels_df <- if (!is.null(vl)) vl[, c("varname", "label"), drop = FALSE] else NULL

      get_term_label <- function(term, labels_df) {
        if (grepl("^I\\((.*)\\^2\\)$", term)) {
          varname <- sub("^I\\((.*)\\^2\\)$", "\\1", term)
          base_label <- labels_df$label[labels_df$varname == varname]
          if (!length(base_label) || is.na(base_label[[1]])) return(paste0(varname, "^2"))
          return(paste0(base_label[[1]], "^2"))
        }
        if (grepl("^I\\((.*)\\^3\\)$", term)) {
          varname <- sub("^I\\((.*)\\^3\\)$", "\\1", term)
          base_label <- labels_df$label[labels_df$varname == varname]
          if (!length(base_label) || is.na(base_label[[1]])) return(paste0(varname, "^3"))
          return(paste0(base_label[[1]], "^3"))
        }
        base_label <- labels_df$label[labels_df$varname == term]
        if (!length(base_label) || is.na(base_label[[1]])) return(term)
        base_label[[1]]
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

        output$effectplot1 <- renderPlot({
          req(model_fit(), haz_vars())
          jtools::effect_plot(
            model_fit()[[3]], pred = !!haz_vars()[1],
            interval = TRUE, plot.points = FALSE, line.colors = "orange",
            x.label = stringr::str_wrap(label_lookup[haz_vars()[1]] %||% haz_vars()[1], 40),
            y.label = stringr::str_wrap(out_lab, 40)
          )
        })

        output$effectplot2 <- renderPlot({
          req(length(haz_vars()) > 1, model_fit(), haz_vars())
          jtools::effect_plot(
            model_fit()[[3]], pred = !!haz_vars()[2],
            interval = TRUE, plot.points = FALSE, line.colors = "orange",
            x.label = stringr::str_wrap(label_lookup[haz_vars()[2]] %||% haz_vars()[2], 40),
            y.label = stringr::str_wrap(out_lab, 40)
          )
        })

        output$interactplot1 <- renderPlot({
          req(length(interactions()) > 0, model_fit(), haz_vars())
          mod <- interactions()
          interactions::interact_plot(
            model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod,
            interval = TRUE, plot.points = FALSE,
            x.label = stringr::str_wrap(label_lookup[haz_vars()[1]] %||% haz_vars()[1], 40),
            y.label = stringr::str_wrap(out_lab, 40)
          ) + ggplot2::theme(legend.position = "bottom")
        })

        output$interactplot2 <- renderPlot({
          req(length(interactions()) > 0, length(haz_vars()) > 1, model_fit(), haz_vars())
          mod <- interactions()
          interactions::interact_plot(
            model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod,
            interval = TRUE, plot.points = FALSE,
            x.label = stringr::str_wrap(label_lookup[haz_vars()[2]] %||% haz_vars()[2], 40),
            y.label = stringr::str_wrap(out_lab, 40)
          ) + ggplot2::theme(legend.position = "bottom")
        })

        output$simslopes1 <- renderPlot({
          req(length(interactions()) > 0, model_fit(), haz_vars())
          mod <- interactions()
          plot(interactions::sim_slopes(model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod))
        })

        output$simslopes2 <- renderPlot({
          req(length(interactions()) > 0, length(haz_vars()) > 1, model_fit(), haz_vars())
          mod <- interactions()
          plot(interactions::sim_slopes(model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod))
        })

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "results",
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

        output$interactplots <- renderUI({
          if (length(interactions()) > 0 && length(haz_vars()) > 1) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("interactplot2"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes2"), height = "300px"))
            )
          } else if (length(interactions()) > 0) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("simslopes1"), height = "300px"))
            )
          } else {
            shiny::p("No interaction term specified.")
          }
        })

        results_tab_added(TRUE)
      }

      if (results_tab_added()) {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "results"), silent = TRUE)
      }
    }, ignoreInit = TRUE)
  })
}
