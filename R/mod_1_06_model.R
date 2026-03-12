#' 1_06_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom mice mice complete as.mira pool
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
#' @param id               Module id.
#' @param variable_list    Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param selected_outcome Reactive data frame row for the selected outcome.
#' @param selected_weather Reactive data frame of selected weather specs.
#' @param survey_weather   Reactive data frame of merged survey + weather data.
#'
#' @noRd
mod_1_06_model_server <- function(id,
                                   variable_list,
                                   selected_surveys,
                                   selected_outcome,
                                   selected_weather,
                                   survey_weather) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Valid variable list (present + >= 50 % non-missing) ----------------

    valid_vl <- reactive({
      req(survey_weather(), variable_list())
      filter_valid_vars(survey_weather(), variable_list())
    })

    # ---- Role-filtered variable lists ---------------------------------------

    fe_vars      <- reactive(filter_vars_by_role(valid_vl(), "fe"))
    hh_vars      <- reactive(filter_vars_by_role(valid_vl(), "hh"))
    ind_vars     <- reactive(filter_vars_by_role(valid_vl(), "ind"))
    area_vars    <- reactive(filter_vars_by_role(valid_vl(), "area"))
    firm_vars    <- reactive(filter_vars_by_role(valid_vl(), "firm"))

    # Interaction vars: flagged for interaction AND not numeric (avoids
    # overfitting — numeric vars should be binned first)
    interact_vars <- reactive(
      filter_vars_by_role(valid_vl(), "interact", extra_filter = list(type = "numeric"))
    )

    # ---- Summary display ----------------------------------------------------

    output$selected_outcome <- renderUI({
      req(selected_outcome())
      shiny::p(paste0("Selected outcome: ", selected_outcome()$label))
    })

    output$selected_weather <- renderUI({
      req(selected_weather())
      shiny::p(paste0(
        "Selected weather: ",
        paste(selected_weather()$label, collapse = ", ")
      ))
    })

    # ---- Model type selector ------------------------------------------------

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

      mc <- model_type_choices(selected_outcome()$type)
      shiny::radioButtons(
        inputId  = ns("model_type"),
        label    = mc$label,
        choices  = mc$choices
      )
    })

    # ---- Model parameters toggle --------------------------------------------

    model_specs_open <- reactiveVal(FALSE)

    output$model_specs_button_ui <- renderUI({
      req(input$model_type)
      shiny::actionButton(ns("model_specs"), "Model parameters",
                          style = "margin-bottom:12px;")
    })

    observeEvent(input$model_specs, {
      model_specs_open(!isTRUE(model_specs_open()))
    })

    # ---- Model specification panel ------------------------------------------

    output$model_specs_ui <- renderUI({
      req(input$model_type)
      if (!isTRUE(model_specs_open())) return(NULL)

      ixn <- interact_vars()
      fe  <- fe_vars()

      shiny::withMathJax(tagList(

        # Interaction with weather hazard
        if (nrow(ixn) > 0) {
          shiny::selectizeInput(
            ns("interactions"),
            label    = "Interactions with \\(Haz_{kt}\\):",
            choices  = setNames(ixn$name, ixn$label),
            selected = if ("urban" %in% ixn$name) "urban" else NULL,
            multiple = TRUE,
            options  = list(maxItems = 1, placeholder = "Select interaction variable")
          )
        } else {
          shiny::helpText("No interaction variables available.",
                          style = "color: grey; font-size: 12px;")
        },

        # Fixed effects
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
          shiny::helpText("No fixed effect variables available.",
                          style = "color: grey; font-size: 12px;")
        },

        # Covariate selection method
        shiny::radioButtons(
          ns("covariates"),
          "Covariate selection:",
          choices  = c("User-defined", "Lasso"),
          selected = "User-defined"
        ),
        hr(),
        uiOutput(ns("covariate_inputs"))
      ))
    })

    # ---- Covariate inputs ---------------------------------------------------

    output$covariate_inputs <- renderUI({
      req(input$covariates)

      # --- USER-DEFINED COVARIATES ---------------------------------------------------------
      if (input$covariates == "User-defined") {

        ind  <- exclude_selected_vars(ind_vars(),  outcome_name = selected_outcome()$name, weather_names = selected_weather()$name, interactions = input$interactions, fixedeffects = input$fixedeffects)
        hh   <- exclude_selected_vars(hh_vars(),   outcome_name = selected_outcome()$name, weather_names = selected_weather()$name, interactions = input$interactions, fixedeffects = input$fixedeffects)
        firm <- exclude_selected_vars(firm_vars(),  outcome_name = selected_outcome()$name, weather_names = selected_weather()$name, interactions = input$interactions, fixedeffects = input$fixedeffects)
        area <- exclude_selected_vars(area_vars(),  outcome_name = selected_outcome()$name, weather_names = selected_weather()$name, interactions = input$interactions, fixedeffects = input$fixedeffects)

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
            },

            hr()

          )
        )

      } else if (input$covariates == "Lasso") {

        tagList(

          shiny::helpText(
            "Lasso will select from all available covariates except weather, interactions, and fixed effects.",
            style = "font-size: 12px;"
          ),

          # ------------------------------
          # Toggle for advanced settings
          # ------------------------------

          shiny::actionButton(
            ns("show_lasso_advanced"),
            "Show advanced settings",
            style = "margin-bottom:12px;"
          ),

          uiOutput(ns("lasso_advanced_ui")),

          shiny::actionButton(
            ns("run_lasso"),
            "Run Lasso",
            class = "btn-primary"
          ),

          hr()

        )
      }
    })

    lasso_advanced_open <- reactiveVal(FALSE)
    observeEvent(input$show_lasso_advanced, {
      lasso_advanced_open(!lasso_advanced_open())
    })

    output$lasso_advanced_ui <- renderUI({

      req(input$covariates == "Lasso")

      if (!lasso_advanced_open()) return(NULL)

      tagList(

        sliderInput(
          ns("lasso_alpha"),
          "Elastic Net Mixing (alpha):",
          min = 0,
          max = 1,
          value = 1,
          step = 0.1
        ),

        shiny::selectizeInput(
          ns("lasso_interactions"),
          label    = "Lambda choice:",
          choices  = c("lambda.1se", "lambda.min"),
          selected = "lambda.1se",
          multiple = FALSE,
          options  = list(
            maxItems    = 1,
            placeholder = "Select lambda choice"
          )
        ),

        sliderInput(
          ns("lasso_nfolds"),
          "Cross-validation folds:",
          min = 5,
          max = 20,
          value = 10,
          step = 1
        ),

        shiny::radioButtons(
          ns("lasso_standardize"),
          "Standardize predictors:",
          choices  = c("Standardize", "Do not standardize"),
          selected = "Standardize"
        ),

        sliderInput(
          ns("mi_m"),
          "Number of imputations (m)",
          min = 1,
          max = 20,
          value = 5,
          step = 1
        ),

        sliderInput(
          ns("mi_maxit"),
          "Imputation iterations",
          min = 1,
          max = 20,
          value = 5,
          step = 1
        ),

        sliderInput(
          ns("stability_threshold"),
          "Selection frequency threshold",
          min = 0.1,
          max = 1,
          value = 0.5,
          step = 0.05
        )

      )
    })

    lasso_status <- reactiveVal("idle")

    # --- LASSO MODEL ---------------------------------------------------------

    lasso_result <- eventReactive(input$run_lasso, {
      req(survey_weather())
      req(selected_outcome())
      req(selected_weather())

      withProgress(message = "Running Lasso...", value = 0, {
        incProgress(0.05, detail = "Preparing inputs")

        df <- prepare_outcome_df(as.data.frame(survey_weather()), selected_outcome())

        outcome_var <- trimws(as.character(selected_outcome()$name)[1])
        weather_vars <- as.character(selected_weather()$name)
        fe_vars      <- as.character(input$fixedeffects)
        int_vars     <- as.character(input$interactions)

        alpha_val <- if (is.null(input$lasso_alpha)) 1 else input$lasso_alpha
        lambda_choice <- if (is.null(input$lasso_interactions)) "lambda.1se" else input$lasso_interactions
        nfolds_val <- if (is.null(input$lasso_nfolds)) 10 else input$lasso_nfolds
        standardize_val <- if (is.null(input$lasso_standardize)) TRUE else {
          if (is.logical(input$lasso_standardize)) input$lasso_standardize else input$lasso_standardize == "Standardize"
        }
        m_val <- if (is.null(input$mi_m)) 5 else input$mi_m
        maxit_val <- if (is.null(input$mi_maxit)) 5 else input$mi_maxit
        threshold_val <- if (is.null(input$stability_threshold)) 0.5 else input$stability_threshold

        incProgress(0.15, detail = "Running MI + LASSO")

        run_lasso_selection(
          df = df,
          selected_outcome = selected_outcome(),
          weather_vars = weather_vars,
          fe_vars = fe_vars,
          int_vars = int_vars,
          valid_vl = valid_vl(),
          model_type = input$model_type,
          alpha = alpha_val,
          lambda_choice = lambda_choice,
          nfolds = nfolds_val,
          standardize = standardize_val,
          mi_m = m_val,
          mi_maxit = maxit_val,
          stability_threshold = threshold_val
        )
      })
    })

    # Showing notifications and updating status based on Lasso execution
    observeEvent(input$run_lasso, {
      lasso_status("running")
      showNotification("Lasso started...",
                      type = "message",
                      duration = 2)
      result <- tryCatch({
        lasso_result()
      }, error = function(e) {
        lasso_status("error")
        showNotification(
          paste("Lasso failed:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
      if (!is.null(result)) {
        lasso_status("done")
        showNotification(
          "Lasso completed successfully.",
          type = "message",
          duration = 3
        )
      }
    })

    # ---- Return API ---------------------------------------------------------

    selected_model <- reactive({

      req(input$model_type, input$covariates)

      # Resolve covariates by role — differs only for Lasso
      covs <- if (input$covariates == "Lasso") {
        selected <- lasso_result()$selected_covariates
        vl       <- valid_vl()
        list(
          hh   = vl$name[vl$hh   == 1 & vl$name %in% selected],
          area = vl$name[vl$area  == 1 & vl$name %in% selected],
          ind  = vl$name[vl$ind   == 1 & vl$name %in% selected],
          firm = vl$name[vl$firm  == 1 & vl$name %in% selected]
        )
      } else {
        list(hh = input$hhcov, area = input$areacov,
             ind = input$indcov, firm = input$firmcov)
      }

      build_selected_model(
        model_type          = input$model_type,
        interactions        = input$interactions,
        fixedeffects        = input$fixedeffects,
        covariate_selection = input$covariates,
        hh_covariates       = covs$hh,
        area_covariates     = covs$area,
        ind_covariates      = covs$ind,
        firm_covariates     = covs$firm,
        lasso_alpha         = input$lasso_alpha,
        lasso_lambda        = input$lasso_interactions,
        lasso_nfolds        = input$lasso_nfolds,
        lasso_standardize   = isTRUE(input$lasso_standardize == "Standardize"),
        mi_m                = input$mi_m,
        mi_maxit            = input$mi_maxit,
        stability_threshold = input$stability_threshold
      )

    })

    list(selected_model = selected_model)
  })
}