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

    varlist_r <- reactive({
      if (is.function(varlist)) varlist() else varlist
    })

    outcome_label <- reactive({
      so <- selected_outcome()
      vl <- varlist_r()
      if (is.null(so) || is.null(vl)) return(NULL)
      lab <- vl$label[vl$varname == so]
      if (length(lab)) as.character(lab[[1]]) else so
    })

    outcome_type <- reactive({
      so <- selected_outcome()
      vl <- varlist_r()
      if (is.null(so)) return(NULL)
      if (!is.null(vl) && "datatype" %in% names(vl)) {
        dt <- vl$datatype[vl$varname == so]
        if (length(dt) && !is.na(dt[[1]]) && dt[[1]] == "Binary") return("Binary")
      }
      df <- survey_weather()
      if (!is.null(df) && so %in% names(df)) {
        vals <- unique(stats::na.omit(df[[so]]))
        if (length(vals) <= 2) return("Binary")
      }
      "Continuous"
    })

    group_col <- reactive({
      vl <- varlist_r()
      if (is.null(vl)) return(NULL)
      intersect(c("wiseapp", "group", "category", "section", "module", "group_name"), names(vl))[1]
    })

    safe_varlist <- function(df, group_name = NULL, datatype = NULL) {
      vl <- varlist_r()
      if (is.null(vl)) return(data.frame(varname = character(), label = character(), stringsAsFactors = FALSE))

      keep <- rep(TRUE, nrow(vl))
      gcol <- group_col()

      if (!is.null(group_name) && !is.null(gcol) && !is.na(gcol)) {
        keep <- keep & (vl[[gcol]] == group_name)
      }

      if (!is.null(datatype) && "datatype" %in% names(vl)) {
        keep <- keep & (vl$datatype %in% datatype)
      }

      out <- vl[keep, , drop = FALSE]
      if (nrow(out) == 0) return(data.frame(varname = character(), label = character(), stringsAsFactors = FALSE))

      out <- out[, intersect(c("varname", "label"), names(out)), drop = FALSE]
      if (!"label" %in% names(out)) out$label <- out$varname

      if (!is.null(df)) {
        out <- out[out$varname %in% names(df), , drop = FALSE]
      }

      out
    }

    hh_varlist <- reactive({
      df <- survey_weather()
      safe_varlist(df, group_name = "HH characteristics", datatype = c("Numeric", "Binary", "Integer"))
    })

    area_varlist <- reactive({
      df <- survey_weather()
      safe_varlist(df, group_name = "Area characteristics", datatype = c("Numeric", "Binary", "Integer"))
    })

    fe_varlist <- reactive({
      df <- survey_weather()
      fe_include <- c(
        "year", "int_year", "int_month", "int_day", "int_date",
        "timestamp", "ADM1CD_c", "ADM2CD_c", "h3_loc_id", "eFUA_ID",
        "ID_UC_G0", "urb_id", "fbcz_id_num"
      )
      if (!is.null(df$code)) fe_include <- c("code", fe_include)

      out <- safe_varlist(df, group_name = "ID & Fixed effects")
      if (nrow(out) == 0) {
        out <- safe_varlist(df, group_name = NULL)
      }

      out[out$varname %in% fe_include, , drop = FALSE]
    })

    output$model_selector_ui <- renderUI({
      req(selected_outcome())
      type <- outcome_type()

      if (is.null(type)) return(NULL)

      if (type == "Binary") {
        model_choices <- c("Logistic regression")
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
      req(varlist_r(), input$model_type)
      nsid <- ns("model_specs")

      shiny::conditionalPanel(
        condition = paste0("input['", nsid, "'] % 2 == 1"),
        shiny::withMathJax(),
        if (input$model_type %in% c("Linear regression", "Logistic regression")) {
          tagList(
            {
              hh_choices <- hh_varlist()
              if ("datatype" %in% names(varlist_r())) {
                bin_vars <- varlist_r()[varlist_r()$datatype == "Binary", "varname"]
                hh_choices <- hh_choices[hh_choices$varname %in% bin_vars, , drop = FALSE]
              }
              choices <- hh_choices$label
              selected <- if (length(choices) && "Urban" %in% choices) "Urban" else if (length(choices)) choices[[1]] else NULL
              shiny::selectizeInput(
                ns("interactions"),
                label = "Interactions with \\(Haz_{kt}\\):",
                choices = choices,
                selected = selected,
                multiple = TRUE,
                options = list(maxItems = 1)
              )
            },
            shiny::selectizeInput(
              ns("fixedeffects"),
              label = "Fixed effects",
              choices = fe_varlist()$label,
              selected = {
                fe <- fe_varlist()
                if (nrow(fe) && "ADM1CD_c" %in% fe$varname) {
                  fe$label[fe$varname == "ADM1CD_c"]
                } else if (nrow(fe)) {
                  fe$label[[1]]
                } else NULL
              },
              multiple = TRUE
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
      req(input$covariates, input$interactions)
      shiny::withMathJax(
        if (input$covariates == "User-defined") {
          tagList(
            {
              hh_choices <- hh_varlist()$label
              selected <- intersect(c("Household size", input$interactions), hh_choices)
              if (!length(selected) && length(hh_choices)) selected <- hh_choices[[1]]
              shiny::selectizeInput(
                ns("hhcov"),
                label = "Household characteristics \\(X_{hkt}\\)",
                choices = hh_choices,
                selected = selected,
                multiple = TRUE
              )
            },
            {
              area_choices <- area_varlist()
              selected <- NULL
              if (nrow(area_choices) && "area_h3_7" %in% area_choices$varname) {
                selected <- area_choices$label[area_choices$varname == "area_h3_7"]
              } else if (nrow(area_choices)) {
                selected <- area_choices$label[[1]]
              }
              shiny::selectizeInput(
                ns("areacov"),
                label = "Area characteristics \\(E_{kt}\\)",
                choices = area_choices$label,
                selected = selected,
                multiple = TRUE
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
      label_to_var <- stats::setNames(vl$varname, vl$label)
      vars <- label_to_var[input$interactions]
      vars[!is.na(vars)]
    })

    model_fit <- eventReactive(input$run_model, {
      req(survey_weather(), selected_outcome())
      df <- survey_weather()
      y_var <- selected_outcome()
      if (!y_var %in% names(df)) {
        shiny::showNotification("Selected outcome is not available in survey data.", type = "warning")
        return(NULL)
      }

      weather_terms <- haz_vars()
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

      label_to_var <- NULL
      vl <- varlist_r()
      if (!is.null(vl) && all(c("label", "varname") %in% names(vl))) {
        label_to_var <- stats::setNames(vl$varname, vl$label)
      }

      interactions <- interaction_vars()

      interaction_terms <- ""
      if (length(interactions) > 0) {
        term_matrix <- outer(interactions, weather_terms, FUN = function(inter, weather) {
          sprintf("(%s * %s)", inter, weather)
        })
        interaction_terms <- paste(c(t(term_matrix)), collapse = " + ")
      }

      fe <- character(0)
      if (!is.null(input$fixedeffects) && length(input$fixedeffects) > 0 && !is.null(label_to_var)) {
        fe <- label_to_var[input$fixedeffects]
        fe <- fe[!is.na(fe)]
      }

      hh_cov <- character(0)
      area_cov <- character(0)
      if (!is.null(input$covariates) && input$covariates %in% c("User-defined", "Lasso") && !is.null(label_to_var)) {
        if (!is.null(input$hhcov)) {
          hh_cov <- label_to_var[input$hhcov]
          hh_cov <- hh_cov[!is.na(hh_cov)]
        }
        if (!is.null(input$areacov)) {
          area_cov <- label_to_var[input$areacov]
          area_cov <- area_cov[!is.na(area_cov)]
        }
      }

      main_effects <- paste(c(weather_terms, hh_cov, area_cov, fe), collapse = " + ")

      formula1 <- stats::as.formula(paste(y_var, "~", paste(weather_terms, collapse = " + ")))
      formula2 <- stats::as.formula(paste(y_var, "~", paste(c(weather_terms, fe), collapse = " + ")))
      formula3 <- stats::as.formula(paste(y_var, "~", paste(c(main_effects, interaction_terms), collapse = " + ")))

      is_binary <- isTRUE(outcome_type() == "Binary")
      sw <- isolate(as.data.frame(df))

      fit_fun <- if (is_binary) {
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

      list(fit1, fit2, fit3)
    }, ignoreInit = TRUE)

      list(
      run_model = reactive(input$run_model),
      model_fit = model_fit,
      outcome_type = outcome_type,
      outcome_label = outcome_label,
      interactions = interaction_vars
    )
  })
}
