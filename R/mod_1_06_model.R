#' 1_06_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_06_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selected_outcome")),
    uiOutput(ns("selected_weather")),
    uiOutput(ns("model_selector_ui")),
    uiOutput(ns("model_specs_button_ui")),
    uiOutput(ns("model_specs_ui")),
    shiny::helpText(
      "More model types and covariate selection methods will be added in future updates.",
      style = "color: red; font-size: 12px;"
    )
  )
}

#' 1_06_model Server Functions
#'
#' @noRd 
mod_1_06_model_server <- function(
    id,
    variable_list,
    selected_surveys,
    selected_outcome,
    selected_weather,
    survey_weather
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- Helper: filter variable_list to valid vars present in survey_weather --------
    # "valid" = present in df AND at least 50% non-missing
    valid_variable_list <- reactive({
      req(survey_weather(), variable_list())
      df <- survey_weather()
      vl <- variable_list()
      valid_vars <- names(df)[colMeans(!is.na(df)) >= 0.5]
      vl[vl$name %in% valid_vars, , drop = FALSE]
    })

    # Subsets by variable role, built once and reused throughout
    fe_variable_list <- reactive({
      vl <- valid_variable_list()
      vl[vl$fe == 1, , drop = FALSE]
    })

    interaction_variable_list <- reactive({
      vl <- valid_variable_list()
      # Keep only variables flagged for interaction that are NOT numeric,
      # to avoid overfitting (numeric interaction vars would need binning first)
      vl[vl$interact == 1 & vl$type != "numeric", , drop = FALSE]
    })

    hh_variable_list <- reactive({
      vl <- valid_variable_list()
      vl[vl$hh == 1, , drop = FALSE]
    })

    area_variable_list <- reactive({
      vl <- valid_variable_list()
      vl[vl$area == 1, , drop = FALSE]
    })

    ind_variable_list <- reactive({
      vl <- valid_variable_list()
      vl[vl$ind == 1, , drop = FALSE]
    })

    firm_variable_list <- reactive({
      vl <- valid_variable_list()
      vl[vl$firm == 1, , drop = FALSE]
    })

    # --- Display selected outcome and weather for reference -------------------

    output$selected_outcome <- renderUI({
      req(selected_outcome())
      shiny::p(paste0("Selected outcome: ", selected_outcome()$label))
    })

    output$selected_weather <- renderUI({
      req(selected_weather())
      shiny::p(paste0("Selected weather: ",
        paste(selected_weather()$label, collapse = ", ")))
    })

    # --- Model type selector --------------------------------------------------

    output$model_selector_ui <- renderUI({

      if (is.null(selected_outcome()) || !length(selected_outcome())) {
        return(shiny::helpText(
          "Select an outcome variable to choose model type.",
          style = "color: red; font-size: 12px;"
        ))
      }

      if (is.null(survey_weather()) || !nrow(as.data.frame(survey_weather()))) {
        return(shiny::helpText(
          "Load survey and weather data to select model type.",
          style = "color: red; font-size: 12px;"
        ))
      }

      type <- selected_outcome()$type
      if (type == "logical") {
        model_choices <- c("Logistic regression", "Linear regression")
        label_text    <- "Classification model:"
      } else {
        model_choices <- c("Linear regression")
        label_text    <- "Regression model:"
      }

      shiny::radioButtons(
        inputId  = ns("model_type"),
        label    = label_text,
        choices  = model_choices
      )
    })

    # --- Model parameters toggle button --------------------------------------

    model_specs_open <- reactiveVal(FALSE)

    output$model_specs_button_ui <- renderUI({
      req(input$model_type)
      shiny::actionButton(ns("model_specs"), "Model parameters", style = "margin-bottom:12px;")
    })

    observeEvent(input$model_specs, {
      model_specs_open(!isTRUE(model_specs_open()))
    })

    # --- Model specification panel -------------------------------------------

    output$model_specs_ui <- renderUI({
      req(input$model_type)

      if (!isTRUE(model_specs_open())) return(NULL)

      if (input$model_type %in% c("Logistic regression", "Linear regression")) {

        interactions <- interaction_variable_list()
        fe           <- fe_variable_list()

        shiny::withMathJax(
          tagList(

            # Interaction variable with weather hazard
            # Only render if there are valid interaction variables available
            if (nrow(interactions) > 0) {
              shiny::selectizeInput(
                ns("interactions"),
                label    = "Interactions with \\(Haz_{kt}\\):",
                choices  = setNames(interactions$name, interactions$label),
                selected = if ("urban" %in% interactions$name) "urban" else NULL,
                multiple = TRUE,
                options  = list(
                  maxItems    = 1,
                  placeholder = "Select interaction variable"
                )
              )
            } else {
              shiny::helpText("No interaction variables available.", style = "color: grey; font-size: 12px;")
            },

            # Fixed effects
            # Only render if there are valid fixed effect variables available
            if (nrow(fe) > 0) {
              shiny::selectizeInput(
                ns("fixedeffects"),
                label    = "Fixed effects:",
                choices  = setNames(fe$name, fe$label),
                selected = intersect(c("year", "gaul1_code"), fe$name),
                multiple = TRUE,
                options  = list(placeholder = "Select (several) fixed effects")
              )
            } else {
              shiny::helpText("No fixed effect variables available.", style = "color: grey; font-size: 12px;")
            },

            # Covariate selection method
            shiny::radioButtons(
              ns("covariates"),
              "Covariate selection:",
              choices  = c("User-defined", "Lasso"),
              selected = "User-defined"
            ),
            shiny::helpText(
              "Lasso variable selection is yet to be implemented.",
              style = "color: red; font-size: 12px;"
            ),

            uiOutput(ns("covariate_inputs"))
          )
        )
      }
    })

    # --- Covariate inputs (user-defined or Lasso) ----------------------------

    # Helper: remove variables already used as outcome, weather, interaction,
    # or fixed effect from a candidate variable_list data frame
    exclude_already_selected <- function(candidate_vl) {
      exclude <- c(
        selected_outcome()$name,
        selected_weather()$name,
        input$interactions,
        input$fixedeffects
      )
      candidate_vl[!candidate_vl$name %in% exclude, , drop = FALSE]
    }

    output$covariate_inputs <- renderUI({
      req(input$covariates)

      if (input$covariates == "User-defined") {

        ind  <- exclude_already_selected(ind_variable_list())
        hh   <- exclude_already_selected(hh_variable_list())
        firm <- exclude_already_selected(firm_variable_list())
        area <- exclude_already_selected(area_variable_list())

        shiny::withMathJax(
          tagList(

            # Individual-level covariates
            if (nrow(ind) > 0) {
              shiny::selectizeInput(
                ns("indcov"),
                label    = "Individual characteristics \\(X_{ijt}\\):",
                choices  = setNames(ind$name, ind$label),
                selected = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Select individual covariates")
              )
            },

            # Household-level covariates
            if (nrow(hh) > 0) {
              shiny::selectizeInput(
                ns("hhcov"),
                label    = "Household characteristics \\(X_{ijt}\\):",
                choices  = setNames(hh$name, hh$label),
                selected = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Select household covariates")
              )
            },

            # Firm-level covariates
            if (nrow(firm) > 0) {
              shiny::selectizeInput(
                ns("firmcov"),
                label    = "Firm characteristics:",
                choices  = setNames(firm$name, firm$label),
                selected = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Select firm covariates")
              )
            },

            # Area-level covariates
            if (nrow(area) > 0) {
              shiny::selectizeInput(
                ns("areacov"),
                label    = "Area characteristics \\(E_{jt}\\):",
                choices  = setNames(area$name, area$label),
                selected = NULL,
                multiple = TRUE,
                options  = list(placeholder = "Select area covariates")
              )
            }
          )
        )

      } else if (input$covariates == "Lasso") {
        tagList(
          shiny::helpText(
            "Placeholder for Lasso variable selection parameters.",
            style = "font-size: 12px;"
          ),
          shiny::actionButton(ns("run_lasso"), "Run Lasso")
        )
      }
    })

    # --- Collect and return selected model spec --------------------------------

    selected_model <- reactive({
      list(
        type                = input$model_type,
        interactions        = input$interactions,
        fixedeffects        = input$fixedeffects,
        covariate_selection = input$covariates,
        hh_covariates       = input$hhcov,
        area_covariates     = input$areacov,
        ind_covariates      = input$indcov,
        firm_covariates     = input$firmcov
      )
    })

    list(
      selected_model = selected_model
    )
  })
}