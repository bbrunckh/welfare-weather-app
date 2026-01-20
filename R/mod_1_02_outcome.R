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

    # --------- Available outcome variables per selected country ----------- ADJUST TO NEW METADATA STRUCTURE
    available_outcomes <- reactive({
      req(country())

      outs <- lapply(country(), function(ctry) {

        # outcome groups for this country (comma-separated in survey_metadata$outcome_group)
        og_raw <- survey_metadata_r() %>%
          dplyr::filter(countryname == ctry) %>%
          dplyr::pull(outcome_group) %>%
          unique()

        og <- og_raw |>
          strsplit(",", fixed = TRUE) |>
          unlist(use.names = FALSE) |>
          trimws()

        og <- og[nzchar(og)]
        og <- unique(og)

        # match outcome groups to varlist$outcome_grp and return var names
        varlist_r() %>%
          dplyr::filter(.data$outcome_grp %in% og) %>%
          dplyr::pull(.data$varname) %>%
          unique() %>%
          sort()
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
        selected = NULL,
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
