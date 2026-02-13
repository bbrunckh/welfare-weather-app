#' 1_02_outcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_02_outcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("outcome_ui"))
    )
  )
}
    
#' 1_02_outcome Server Functions
#'
#' @noRd 
mod_1_02_outcome_server <- function(id, survey_metadata, varlist, country, survey_year) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    survey_metadata_r <- reactive({
      req(survey_metadata())
      survey_metadata()
    })
    varlist_r <- reactive({
      req(varlist())
      varlist()
    })

    # --------- Available outcome variables per selected country -----------
    available_outcomes <- reactive({
      req(country())

      outs <- lapply(country(), function(ctry) {

        # outcome groups for this country (comma-separated in survey_metadata$outcome_group)
        og_raw <- survey_metadata_r() %>%
          dplyr::filter(countryname == ctry) %>%
          dplyr::pull(outcome) %>%
          unique()

        og <- og_raw |>
          strsplit(",", fixed = TRUE) |>
          unlist(use.names = FALSE) |>
          trimws()

        og <- og[nzchar(og)]
        og <- unique(og)

        # match outcome groups to varlist$outcome and return named choices
        v <- varlist_r()
        opts <- v %>%
          dplyr::filter(.data$outcome %in% og)

        names_vec <- opts$name
        labels_vec <- if ("label" %in% names(opts)) opts$label else opts$name
        display <- ifelse(is.na(labels_vec) | labels_vec == "" | labels_vec == names_vec,
                          names_vec,
                          paste0(names_vec, " (", labels_vec, ")"))

        out <- stats::setNames(names_vec, display)
        out <- out[!is.na(names(out)) & !is.na(out)]
        out <- out[order(names(out))]
        out
      })

      names(outs) <- country()
      outs
    })


    # --------- Outcome selector UI -----------
    output$outcome_ui <- renderUI({
      req(country())

      country_name <- country()
      outs <- available_outcomes()[[country_name]]
      req(outs)

      selectizeInput(
        inputId = ns("outcome_var"),
        label   = paste("Outcome variable for", country_name),
        choices = outs,
        selected = outs[9],
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = paste("Select outcome for", country_name)
        )
      )
    })

    # Module return API
    list(
      available_outcomes = available_outcomes,
      selected_outcome = reactive({
        x <- input$outcome_var
        if (is.null(x) || length(x) == 0) return(NULL)
        x[[1]]
      })
    )
  })
}
