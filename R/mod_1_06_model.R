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
    uiOutput(ns("model_selector_ui")),
    shiny::actionButton(ns("model_specs"), "Model parameters", style = "margin-bottom:12px;"),
    uiOutput(ns("model_specs_ui")),
    shiny::helpText(
      "Extreme gradient boosting and random forest models are yet to be implemented.",
      style = "color: red; font-size: 12px;"
    ),
    shiny::hr(),
    shiny::actionButton(ns("run_model"), "Run model", style = "width: 100%;"),
    shiny::br()
  )
}
    
#' 1_06_model Server Functions
#'
#' @noRd 
mod_1_06_model_server <- function(
    id,
    survey_weather,
    haz_vars,
    weather_settings,
    varlist,
    selected_outcome
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    model_specs_open <- reactiveVal(FALSE)

    observeEvent(input$model_specs, {
      model_specs_open(!isTRUE(model_specs_open()))
    })

    varlist_r <- reactive({
      if (is.function(varlist)) varlist() else varlist
    })

    outcome_label <- reactive({
      so <- selected_outcome()
      vl <- varlist_r()
      if (is.null(so) || is.null(vl)) return(NULL)
      lab <- vl$label[vl$name == so]
      if (length(lab)) as.character(lab[[1]]) else so
    })

    outcome_type <- reactive({
      so <- selected_outcome()
      vl <- varlist_r()
      if (is.null(so)) return(NULL)
      if (!is.null(vl) && "type" %in% names(vl)) {
        dt <- vl$type[vl$name == so]
        if (length(dt) && !is.na(dt[[1]]) && tolower(dt[[1]]) == "logical") return("Binary")
      }
      df <- survey_weather()
      if (!is.null(df) && so %in% names(df)) {
        vals <- unique(stats::na.omit(df[[so]]))
        if (length(vals) <= 2) return("Binary")
      }
      "Continuous"
    })

    safe_varlist <- function(df, flag_col = NULL, types = NULL) {
      vl <- varlist_r()
      if (is.null(vl)) return(data.frame(name = character(), label = character(), stringsAsFactors = FALSE))

      keep <- rep(TRUE, nrow(vl))
      if (!is.null(flag_col) && flag_col %in% names(vl)) {
        keep <- keep & vl[[flag_col]] == 1
      }

      if (!is.null(types) && "type" %in% names(vl)) {
        keep <- keep & tolower(vl$type) %in% tolower(types)
      }

      out <- vl[keep, , drop = FALSE]
      if (nrow(out) == 0) return(data.frame(name = character(), label = character(), stringsAsFactors = FALSE))

      name_col <- "name"
      label_col <- "label"
      if (is.null(name_col)) return(data.frame(name = character(), label = character(), stringsAsFactors = FALSE))

      out <- out[, unique(c(name_col, label_col)), drop = FALSE]
      names(out)[names(out) == name_col] <- "name"
      if (!"label" %in% names(out)) out$label <- out$name

      if (!is.null(df)) {
        out <- out[out$name %in% names(df), , drop = FALSE]
      }

      out
    }

    hh_varlist <- reactive({
      df <- survey_weather()
      safe_varlist(df, flag_col = "hh", types = c("numeric", "integer", "logical"))
    })

    area_varlist <- reactive({
      df <- survey_weather()
      safe_varlist(df, flag_col = "area", types = c("numeric", "integer", "logical"))
    })

    fe_varlist <- reactive({
      df <- survey_weather()
      safe_varlist(df, flag_col = "fe", types = c("numeric", "integer", "logical", "character", "Date"))
    })

    extra_covariates <- reactive({
      vl <- varlist_r()
      df <- survey_weather()
      if (is.null(vl)) return(data.frame(name = character(), label = character(), stringsAsFactors = FALSE))

      base_vars <- c(selected_outcome(), haz_vars())
      hh_vars <- hh_varlist()$name
      area_vars <- area_varlist()$name
      fe_vars <- fe_varlist()$name

      exclude <- unique(c(base_vars, hh_vars, area_vars, fe_vars))
      name_col <- "name"
      if (is.null(name_col)) return(data.frame(name = character(), label = character(), stringsAsFactors = FALSE))

      out <- vl[vl[[name_col]] %in% names(df) & !vl[[name_col]] %in% exclude, , drop = FALSE]
      out <- out[, intersect(c(name_col, "label"), names(out)), drop = FALSE]
      names(out)[names(out) == name_col] <- "name"
      if (!"label" %in% names(out)) out$label <- out$name

      out
    })

    output$model_selector_ui <- renderUI({
      req(selected_outcome())
      type <- outcome_type()

      if (is.null(type)) return(NULL)

      if (type == "Binary") {
        model_choices <- c("Logistic regression", "Linear regression")
        label_text <- "Classification model:"
      } else {
        model_choices <- c("Linear regression")
        label_text <- "Regression model:"
      }

      tagList(
        shiny::p(paste0("A ", tolower(type), " welfare outcome is selected."), style = "font-size: 12px;"),
        shiny::radioButtons(
          inputId = ns("model_type"),
          label = label_text,
          choices = model_choices
        )
      )
    })

    output$model_specs_ui <- renderUI({
      req(varlist_r())

      if (!isTRUE(model_specs_open())) {
        return(NULL)
      }

      if (is.null(input$model_type)) {
        return(shiny::p("Select a model type to configure parameters."))
      }

      shiny::withMathJax(
        if (input$model_type %in% c("Logistic regression", "Linear regression")) {
          tagList(
            {
              df <- survey_weather()
              vl <- varlist_r()
              name_col <- "name"

              choices <- character(0)
              if (!is.null(name_col) && !is.null(vl) && "interact" %in% names(vl)) {
                has_hh <- "hh" %in% names(vl)
                has_outcome <- "outcome" %in% names(vl)

                ok_hh <- if (has_hh) vl$hh == 1 else rep(FALSE, nrow(vl))
                ok_outcome <- if (has_outcome) !is.na(vl$outcome) & nzchar(vl$outcome) else rep(FALSE, nrow(vl))

                interact_df <- vl[vl$interact == 1 & (ok_hh | ok_outcome), , drop = FALSE]
                interact_df <- interact_df[, intersect(c(name_col, "label"), names(interact_df)), drop = FALSE]
                names(interact_df)[names(interact_df) == name_col] <- "name"
                if (!"label" %in% names(interact_df)) interact_df$label <- interact_df$name

                if (!is.null(df)) {
                  interact_df <- interact_df[interact_df$name %in% names(df), , drop = FALSE]
                }

                exclude <- c(selected_outcome(), haz_vars())
                interact_df <- interact_df[!interact_df$name %in% exclude, , drop = FALSE]
                choices <- interact_df$label
              }

              shiny::selectizeInput(
                ns("interactions"),
                label = "Interactions with \\(Haz_{kt}\\):",
                choices = choices,
                selected = NULL,
                multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Select interaction variable")
              )
            },
            shiny::selectizeInput(
              ns("fixedeffects"),
              label = "Fixed effects",
              choices = fe_varlist()$label,
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "Please select (several) fixed effects")
            ),
            shiny::radioButtons(
              ns("covariates"),
              "Covariate selection:",
              choices = c("User-defined", "Lasso"),
              selected = "User-defined"
            ),
            shiny::helpText("Lasso variable selection is yet to be implemented.", style = "color: red; font-size: 12px;"),
            uiOutput(ns("covariate_inputs"))
          )
        }
      )
    })

    output$covariate_inputs <- renderUI({
      req(input$covariates)
      shiny::withMathJax(
        if (input$covariates == "User-defined") {
          tagList(
            {
              hh_choices <- hh_varlist()$label
              shiny::selectizeInput(
                ns("hhcov"),
                label = "Household characteristics \\(X_{hkt}\\)",
                choices = hh_choices,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Please select (several) hh covariates")
              )
            },
            {
              area_choices <- area_varlist()
              shiny::selectizeInput(
                ns("areacov"),
                label = "Area characteristics \\(E_{kt}\\)",
                choices = area_choices$label,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Please select (several) area covariates")
              )
            },
            {
              extra_choices <- extra_covariates()
              shiny::selectizeInput(
                ns("othercov"),
                label = "Additional covariates",
                choices = extra_choices$label,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Please select (several) extra covariates")
              )
            }
          )
        } else if (input$covariates == "Lasso") {
          tagList(
            shiny::helpText("Placeholder for Lasso variable selection parameters.", style = "font-size: 12px;"),
            shiny::actionButton(ns("run_lasso"), "Run Lasso")
          )
        }
      )
    })

    interaction_vars <- reactive({
      vl <- varlist_r()
      if (is.null(vl) || is.null(input$interactions) || !length(input$interactions)) return(character(0))
      name_col <- "name"
      if (is.null(name_col) || !"label" %in% names(vl)) return(character(0))
      label_to_var <- stats::setNames(vl[[name_col]], vl$label)
      vars <- label_to_var[input$interactions]
      vars[!is.na(vars)]
    })

    weather_terms_val <- reactiveVal(character(0))
    hh_cov_val <- reactiveVal(character(0))
    area_cov_val <- reactiveVal(character(0))
    other_cov_val <- reactiveVal(character(0))
    fe_val <- reactiveVal(character(0))
    y_var_val <- reactiveVal(NULL)

    model_fit_val <- reactiveVal(NULL)

    observeEvent(input$run_model, {
      id_run <- shiny::showNotification("Running model. Results presented once model finished running.", type = "message", duration = NULL)
      fit_list <- tryCatch({
        weather_terms_val(character(0))
        hh_cov_val(character(0))
        area_cov_val(character(0))
        other_cov_val(character(0))
        fe_val(character(0))
        y_var_val(NULL)

        if (is.null(selected_outcome()) || !length(selected_outcome())) {
          shiny::showNotification("Select an outcome before running the model.", type = "warning")
          return(NULL)
        }

        df <- survey_weather()
        if (is.null(df) || !nrow(as.data.frame(df))) {
          shiny::showNotification("Load survey and weather data before running the model.", type = "warning")
          return(NULL)
        }

        shiny::showNotification("Fitting model...", type = "message")
        y_var <- selected_outcome()
        y_var_val(y_var)
        if (!y_var %in% names(df)) {
          shiny::showNotification("Selected outcome is not available in survey data.", type = "warning")
          return(NULL)
        }

        weather_terms <- haz_vars()
        if (!is.null(df)) {
          weather_terms <- intersect(weather_terms, names(df))
        }
        if (is.null(weather_terms) || !length(weather_terms)) {
          shiny::showNotification("Weather variables are not available.", type = "warning")
          return(NULL)
        }

      if (!is.null(weather_settings) && is.function(weather_settings)) {
        ws <- weather_settings()
        if (!is.null(ws) && nrow(ws)) {
          for (w in weather_terms) {
            base <- sub("^haz_", "", w)
            poly_vals <- ws$polynomial[ws$varname == base]
            if (length(poly_vals)) {
              poly_vals <- poly_vals[[1]]
              if ("a" %in% poly_vals) weather_terms <- c(weather_terms, paste0("I(", w, "^2)"))
              if ("b" %in% poly_vals) weather_terms <- c(weather_terms, paste0("I(", w, "^3)"))
            }
          }
        }
      }

      weather_terms_val(weather_terms)

      label_to_var <- NULL
      vl <- varlist_r()
      name_col <- if (!is.null(vl) && "name" %in% names(vl)) "name" else "varname"
      if (!is.null(vl) && all(c("label", name_col) %in% names(vl))) {
        label_to_var <- stats::setNames(vl[[name_col]], vl$label)
      }

      interactions <- interaction_vars()

      interaction_terms <- character(0)
      if (length(interactions) > 0) {
        term_matrix <- outer(interactions, weather_terms, FUN = function(inter, weather) {
          sprintf("(%s * %s)", inter, weather)
        })
        interaction_terms <- c(t(term_matrix))
      }

      fe <- character(0)
      if (!is.null(input$fixedeffects) && length(input$fixedeffects) > 0 && !is.null(label_to_var)) {
        fe <- label_to_var[input$fixedeffects]
        fe <- fe[!is.na(fe)]
      }
      fe_val(fe)

      hh_cov <- character(0)
      area_cov <- character(0)
      other_cov <- character(0)
      if (!is.null(input$covariates) && input$covariates %in% c("User-defined", "Lasso") && !is.null(label_to_var)) {
        if (!is.null(input$hhcov)) {
          hh_cov <- label_to_var[input$hhcov]
          hh_cov <- hh_cov[!is.na(hh_cov)]
        }
        if (!is.null(input$areacov)) {
          area_cov <- label_to_var[input$areacov]
          area_cov <- area_cov[!is.na(area_cov)]
        }
        if (!is.null(input$othercov)) {
          other_cov <- label_to_var[input$othercov]
          other_cov <- other_cov[!is.na(other_cov)]
        }
      }
      hh_cov_val(hh_cov)
      area_cov_val(area_cov)
      other_cov_val(other_cov)

      build_formula <- function(y, terms) {
        terms <- terms[nzchar(terms)]
        terms <- terms[!is.na(terms)]
        if (!length(terms)) return(NULL)
        stats::as.formula(paste(y, "~", paste(terms, collapse = " + ")))
      }

      terms1 <- weather_terms
      terms2 <- c(weather_terms, fe)
      terms3 <- c(weather_terms, hh_cov, area_cov, other_cov, fe, interaction_terms)

      formula1 <- build_formula(y_var, terms1)
      formula2 <- build_formula(y_var, terms2)
      formula3 <- build_formula(y_var, terms3)

      if (is.null(formula1) || is.null(formula2) || is.null(formula3)) {
        shiny::showNotification("Model terms are incomplete. Check covariates and fixed effects selections.", type = "warning")
        return(NULL)
      }

      is_binary <- isTRUE(outcome_type() == "Binary")
      sw <- isolate(as.data.frame(df))

      use_logit <- is_binary && identical(input$model_type, "Logistic regression")
      if (use_logit) {
        y_vals <- sw[[y_var]]
        y_vals <- y_vals[!is.na(y_vals)]
        if (!length(y_vals) || any(!(y_vals %in% c(0, 1)))) {
          shiny::showNotification("Outcome values are not 0/1. Falling back to OLS.", type = "warning")
          use_logit <- FALSE
        }
      }
      fit_fun <- if (use_logit) {
        function(form) stats::glm(form, data = sw, family = stats::binomial(link = "logit"))
      } else {
        function(form) stats::lm(form, data = sw)
      }

      fix_call <- function(fit) {
        fit$call$formula <- stats::formula(fit)
        fit$call$data <- quote(sw)
        fit
      }

      fit1 <- fix_call(fit_fun(formula1))
      fit2 <- fix_call(fit_fun(formula2))
      fit3 <- fix_call(fit_fun(formula3))

        shiny::showNotification("Model fitted successfully.", type = "message")
        list(fit1, fit2, fit3)
      }, error = function(e) {
        shiny::showNotification(paste("Model failed:", conditionMessage(e)), type = "error")
        NULL
      })

      # remove the persistent 'running' notification
      try(if (exists("id_run") && !is.null(id_run)) shiny::removeNotification(id_run), silent = TRUE)

      model_fit_val(fit_list)
    }, ignoreInit = TRUE)

      list(
        model_fit = model_fit_val,
        outcome_type = outcome_type,
        outcome_label = outcome_label,
        interaction_terms = interaction_vars,
        weather_terms = weather_terms_val,
        hh_cov = hh_cov_val,
        area_cov = area_cov_val,
        other_cov = other_cov_val,
        fe = fe_val,
        y_var = y_var_val
      )
  })
}
