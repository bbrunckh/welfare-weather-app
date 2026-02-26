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
            hr(),
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

      # --- USER-DEFINED COVARIATES ---------------------------------------------------------
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

        df <- as.data.frame(survey_weather())

        outcome_var  <- selected_outcome()$name
        weather_vars <- selected_weather()$name
        fe_vars      <- input$fixedeffects
        int_vars     <- input$interactions

        # --- advanced params (sensible defaults) --------------------------------
        alpha_val <- if (is.null(input$lasso_alpha)) 1 else input$lasso_alpha
        lambda_choice <- if (is.null(input$lasso_lambda)) "lambda.1se" else input$lasso_lambda
        nfolds_val <- if (is.null(input$lasso_nfolds)) 10 else input$lasso_nfolds
        standardize_val <- if (is.null(input$lasso_standardize)) TRUE else {
          # allow checkbox or character toggle "Standardize"
          if (is.logical(input$lasso_standardize)) input$lasso_standardize else input$lasso_standardize == "Standardize"
        }
        m_val <- if (is.null(input$mi_m)) 5 else input$mi_m
        maxit_val <- if (is.null(input$mi_maxit)) 5 else input$mi_maxit
        threshold_val <- if (is.null(input$stability_threshold)) 0.5 else input$stability_threshold

        incProgress(0.05, detail = "Preparing data")

        # -------------------------------------------------
        # 1) DROP ONLY IF OUTCOME MISSING
        # -------------------------------------------------
        if (!outcome_var %in% names(df)) stop("Outcome variable not found in data.")
        df <- df[!is.na(df[[outcome_var]]), , drop = FALSE]

        if (nrow(df) < 30) {
          stop("Too few observations after removing missing outcome.")
        }

        # -------------------------------------------------
        # 2) Build core formula (use formula-based interactions, NOT manual multiplication)
        # -------------------------------------------------
        interaction_terms <- character(0)
        if (!is.null(int_vars) && length(int_vars) > 0) {
          # create all iv:wv pairs using ":" so model.matrix handles factor/numeric combinations robustly
          interaction_terms <- as.vector(outer(int_vars, weather_vars, paste, sep = ":"))
        }

        # Keep only terms that actually exist in df (we'll check per-imputation again)
        core_terms <- unique(c(weather_vars, fe_vars, interaction_terms))
        core_terms <- core_terms[core_terms %in% names(df)]

        if (length(core_terms) == 0) {
          # permitted: no core terms, but risk of empty protected set
          core_formula <- as.formula("~ 1")
        } else {
          core_formula <- as.formula(paste("~", paste(core_terms, collapse = " + ")))
        }

        # -------------------------------------------------
        # 3) Define candidate covariates
        # -------------------------------------------------
        # Exclude outcome and any columns produced by model.matrix(core_formula) later.
        # To be conservative we exclude the literal core_terms and the outcome.
        exclude <- unique(c(outcome_var, core_terms))

        vl <- valid_variable_list()
        if (is.null(vl) || nrow(vl) == 0) {
          stop("Variable list not available or empty.")
        }
        allowed <- vl$name[vl$ind == 1 | vl$hh == 1 | vl$area == 1 | vl$firm == 1]

        candidate_vars <- intersect(setdiff(names(df), exclude), allowed)

        # Keep only numeric candidates for glmnet
        if (length(candidate_vars) > 0) {
          is_num <- sapply(df[, candidate_vars, drop = FALSE], is.numeric)
          candidate_vars <- candidate_vars[is_num]
        }

        if (length(candidate_vars) == 0) {
          stop("No valid numeric candidate covariates available for LASSO.")
        }

        incProgress(0.2, detail = "Defining multiple imputation strategy")

        # -------------------------------------------------
        # 4) MULTIPLE IMPUTATION (robust)
        # -------------------------------------------------
        m <- max(1, as.integer(m_val))

        # mice can fail if all-NA columns present; ensure candidate columns exist and have some non-NA
        non_all_na <- sapply(df[, candidate_vars, drop = FALSE], function(x) any(!is.na(x)))
        if (!all(non_all_na)) {
          # drop fully-missing candidate vars
          dropped <- candidate_vars[!non_all_na]
          candidate_vars <- candidate_vars[non_all_na]
          showNotification(paste("Dropped candidate vars with no observed values:", paste(dropped, collapse = ", ")), type = "warning")
        }
        if (length(candidate_vars) == 0) stop("No candidate variables with observed values remain for imputation/LASSO.")

        # run mice on only candidate_vars (lightweight: pmm). If mice errors, bubble up.
        imp <- tryCatch({
          mice::mice(df[, candidate_vars, drop = FALSE], m = m, maxit = max(1, as.integer(maxit_val)), method = "pmm", print = FALSE)
        }, error = function(e) {
          stop("mice failed: ", e$message)
        })

        # Prepare storage
        selected_list <- vector("list", m)
        final_models  <- vector("list", m)

        family_type <- if (input$model_type == "Logistic regression") "binomial" else "gaussian"

        incProgress(0.3, detail = "Running Lasso per imputation")

        # -------------------------------------------------
        # 5) LASSO per imputation (rebuild core design matrix after imputation)
        # -------------------------------------------------
        for (i in seq_len(m)) {
          incProgress(0.3 + 0.5 * (i - 1) / m, detail = paste("Lasso — imputation", i, "of", m))

          df_imp <- df
          # replace candidate columns with imputed values for this imputation
          df_imp[, candidate_vars] <- mice::complete(imp, action = i)

          # Rebuild core matrix via formula so factors/interactions are handled correctly
          mm_core <- tryCatch({
            stats::model.matrix(core_formula, data = df_imp)
          }, error = function(e) {
            showNotification(paste("model.matrix failed for core terms on imputation", i, ":", e$message), type = "error")
            return(NULL)
          })
          if (is.null(mm_core)) {
            selected_list[[i]] <- character(0)
            next
          }

          # X_core: drop intercept column if present
          X_core <- if (ncol(mm_core) > 1) mm_core[, -1, drop = FALSE] else matrix(nrow = nrow(df_imp), ncol = 0)

          # X_lasso: numeric candidate vars (after imputation)
          X_lasso <- as.matrix(df_imp[, candidate_vars, drop = FALSE])

          # Remove zero-variance columns from X_lasso (can't be penalized meaningfully)
          keep_cols <- apply(X_lasso, 2, function(x) length(unique(na.omit(x))) > 1)
          if (!all(keep_cols)) X_lasso <- X_lasso[, keep_cols, drop = FALSE]
          kept_candidate_vars <- candidate_vars[keep_cols]

          # if no lasso columns left, skip this imputation
          if (ncol(X_lasso) == 0) {
            selected_list[[i]] <- character(0)
            next
          }

          # Ensure rows match between X_core and X_lasso
          if (nrow(X_core) != nrow(X_lasso)) {
            # defensive: this should not happen since both are built from df_imp, but check anyway
            showNotification("Row mismatch between core and lasso matrices — aborting this imputation", type = "error")
            selected_list[[i]] <- character(0)
            next
          }

          X_full <- if (ncol(X_core) > 0) cbind(X_core, X_lasso) else X_lasso

          penalty <- c(rep(0, ncol(X_core)), rep(1, ncol(X_lasso)))

          # run cross-validated glmnet — wrap in tryCatch to avoid halting app
          cvfit <- tryCatch({
            glmnet::cv.glmnet(x = X_full, y = df_imp[[outcome_var]], alpha = alpha_val,
                              nfolds = max(2, as.integer(nfolds_val)),
                              family = family_type, standardize = standardize_val,
                              penalty.factor = penalty)
          }, error = function(e) {
            showNotification(paste("cv.glmnet failed on imputation", i, ":", e$message), type = "error")
            return(NULL)
          })
          if (is.null(cvfit)) {
            selected_list[[i]] <- character(0)
            next
          }

          # Extract coefficients at chosen lambda (lambda_choice can be "lambda.1se" or "lambda.min")
          coefs <- coef(cvfit, s = lambda_choice)
          sel <- rownames(coefs)[as.numeric(coefs) != 0]
          sel <- setdiff(sel, "(Intercept)")

          # keep only those that correspond to candidate (X_lasso) columns
          # note: glmnet names should match colnames(X_full); X_lasso columns are last ncol(X_lasso)
          sel <- intersect(sel, colnames(X_lasso))

          # map selections back to original candidate variable names (kept_candidate_vars)
          # Usually column names match candidate_vars, so this is direct
          selected_list[[i]] <- sel
        } # end imputations loop

        incProgress(0.9, detail = "Stability-based selection")

        # -------------------------------------------------
        # 6) Stability-based selection across imputations
        # -------------------------------------------------
        all_selected <- unique(unlist(selected_list))
        if (length(all_selected) == 0) {
          stop("No covariates selected across imputations.")
        }

        selection_freq <- sapply(all_selected, function(v) {
          mean(sapply(selected_list, function(x) v %in% x))
        })

        final_selected <- names(selection_freq)[selection_freq >= threshold_val]
        if (length(final_selected) == 0) {
          stop("No covariates stable across imputations.")
        }

        incProgress(0.95, detail = "Refitting final model across imputations")

        # -------------------------------------------------
        # 7) POST-LASSO REFIT per imputation
        # -------------------------------------------------
        # Build final formula using core_terms (formula syntax) + final_selected (raw var names)
        # final_selected are columns from X_lasso (should match candidate var names)
        final_rhs_terms <- c(core_terms, final_selected)
        final_rhs_terms <- unique(final_rhs_terms)
        final_formula <- as.formula(paste(outcome_var, "~", paste(final_rhs_terms, collapse = " + ")))

        for (i in seq_len(m)) {
          df_imp <- df
          df_imp[, candidate_vars] <- mice::complete(imp, action = i)

          # fit final model on imputed set
          final_models[[i]] <- tryCatch({
            if (family_type == "gaussian") {
              stats::lm(final_formula, data = df_imp)
            } else {
              stats::glm(final_formula, data = df_imp, family = stats::binomial())
            }
          }, error = function(e) {
            showNotification(paste("Final refit failed on imputation", i, ":", e$message), type = "warning")
            return(NULL)
          })
        }

        # Remove nulls (failed fits)
        final_models <- final_models[!sapply(final_models, is.null)]
        if (length(final_models) == 0) stop("All post-LASSO refits failed.")

        # -------------------------------------------------
        # 8) Pooling: try to pool if as.mira is available; otherwise return per-imputation list
        # -------------------------------------------------
        pooled_model <- NULL
        if (exists("as.mira", where = asNamespace("mice"), inherits = FALSE)) {
          # as.mira sometimes isn't exported; try mice::as.mira
          mira_obj <- tryCatch(mice::as.mira(final_models), error = function(e) NULL)
          if (!is.null(mira_obj)) {
            pooled_model <- tryCatch(mice::pool(mira_obj), error = function(e) {
              showNotification(paste("Pooling failed:", e$message), type = "warning")
              NULL
            })
          }
        }

        # If pooling not available, keep per-imputation models and warn
        if (is.null(pooled_model)) {
          showNotification("Pooled model not available; returning per-imputation models.", type = "message", duration = 3)
        }

        list(
          model = if (!is.null(pooled_model)) pooled_model else final_models,
          per_imputation_models = final_models,
          selected_covariates = final_selected,
          selection_frequency = selection_freq
        )
      }) # end withProgress
    }) # end eventReactive

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

    # --- Collect and return selected model spec --------------------------------

    selected_model <- reactive({

      if (input$covariates == "User-defined") {

        return(list(
          type                = input$model_type,
          interactions        = input$interactions,
          fixedeffects        = input$fixedeffects,
          covariate_selection = input$covariates,
          hh_covariates       = input$hhcov,
          area_covariates     = input$areacov,
          ind_covariates      = input$indcov,
          firm_covariates     = input$firmcov
        ))

      } else if (input$covariates == "Lasso") {

        selected <- lasso_result()$selected_covariates
        vl <- valid_variable_list()

        # Match selected variables back to roles
        hh_selected   <- vl$name[vl$hh == 1   & vl$name %in% selected]
        area_selected <- vl$name[vl$area == 1 & vl$name %in% selected]
        ind_selected  <- vl$name[vl$ind == 1  & vl$name %in% selected]
        firm_selected <- vl$name[vl$firm == 1 & vl$name %in% selected]

        return(list(
          type                = input$model_type,
          interactions        = input$interactions,
          fixedeffects        = input$fixedeffects,
          covariate_selection = input$covariates,
          hh_covariates       = hh_selected,
          area_covariates     = area_selected,
          ind_covariates      = ind_selected,
          firm_covariates     = firm_selected
        ))
      }

    })

    list(
      selected_model = selected_model
    )
  })
}