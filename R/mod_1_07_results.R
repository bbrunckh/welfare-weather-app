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
  ns <- NS(id)
  tagList(
    shiny::actionButton(ns("run_model"), "Run model", class = "btn-primary", style = "width: 100%;")
  )
}

#' 1_07_results Server Functions
#'
#' @noRd
mod_1_07_results_server <- function(
    id,
    variable_list,
    selected_surveys,
    selected_outcome,
    selected_weather,
    survey_weather,
    selected_model,
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    # ---- Internal state ------------------------------------------------------

    model_fit_val     <- reactiveVal(NULL)   # stores the list returned by fit_weather_model()
    results_tab_added <- reactiveVal(FALSE)

    # ---- Helpers ---------------------------------------------------------------

    # Unwrap parsnip fit to native lm/glm object (required by jtools/interactions)
    native_fit <- function(parsnip_fit) parsnip_fit$fit

    # Look up a human-readable label for a variable name from variable_list.
    get_label <- function(var_name) {
      vl <- if (is.function(variable_list)) variable_list() else variable_list
      if (is.null(vl)) return(var_name)
      idx <- match(var_name, vl$name)
      if (is.na(idx)) var_name else vl$label[idx]
    }

    # Helper to extract weather-related coef names from a fitted model
    # Covers: base terms, factor level expansions, polynomial terms,
    # and interaction terms
    weather_coef_names <- function(fit, weather_terms, interaction_terms) {
      all_coefs <- names(stats::coef(native_fit(fit)))

      # Match base weather terms (including factor level expansions like tx[1,2])
      weather_pattern <- paste(
        paste0("^", weather_terms, "$"),          # exact continuous match
        paste0("^", weather_terms, "[\\[\\(]"),   # factor level expansion
        paste0("^I\\(", weather_terms, "\\^"),    # polynomial terms
        sep = "|"
      )
      weather_pattern <- paste(weather_pattern, collapse = "|")

      # Match interaction terms (base and factor-expanded)
      interact_pattern <- if (length(interaction_terms) > 0) {
        paste(
          vapply(interaction_terms, function(t) {
            # escape special regex chars in the term string
            paste0("^", gsub("([\\(\\)\\^])", "\\\\\\1", t))
          }, character(1)),
          collapse = "|"
        )
      } else {
        NULL
      }

      keep <- grepl(weather_pattern, all_coefs)
      if (!is.null(interact_pattern)) {
        keep <- keep | grepl(interact_pattern, all_coefs)
      }
      all_coefs[keep]
    }

    # Turn a coefficient name (possibly "spei:urban" or "I(spei^2):urban")
    # into a readable label by looking up each component
    coef_label <- function(coef_name) {
      parts <- strsplit(coef_name, ":")[[1]]
      paste(vapply(parts, get_label, character(1)), collapse = " \u00d7 ")
    }

    # Build a named vector: names = readable labels, values = coefficient names
    # Used by jtools to select and label coefficients in plots / tables
    make_coef_map <- function(coef_names) {
      readable <- vapply(coef_names, coef_label, character(1))
      stats::setNames(coef_names, readable)
    }

    # Placeholder plot for when a required plot cannot be rendered
    empty_plot <- function(msg = "No data to display.") {
      graphics::plot.new()
      graphics::text(0.5, 0.5, msg, cex = 0.9, col = "grey40")
    }

    # ---- Run model on button click -------------------------------------------

    observeEvent(input$run_model, {

      req(selected_outcome(), selected_weather(), selected_model(), survey_weather())

      fitting_id <- shiny::showNotification(
        "Fitting models — please wait...",
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      on.exit(shiny::removeNotification(fitting_id), add = TRUE)

      # construct specified outcome variable
      so <- selected_outcome()
      df <- as.data.frame(survey_weather())

      # Step 1: convert to LCU if specified
      if (isTRUE(so$units == "LCU")) {
        df <- df |>
          dplyr::mutate(!!so$name := .data[[so$name]] * ppp2021)
      }
      # Step 2: log transform if specified
      if (isTRUE(so$transform == "log")) {
        df <- df |>
          dplyr::mutate(!!so$name := log(.data[[so$name]]))
      }
      # Step 3: create binary poor indicator if poverty line is specified
      if (isTRUE(!is.na(so$povline[1])) && isTRUE(so$name == "poor")) {
        df <- df |>
          dplyr::mutate(!!so$name := as.numeric(.data[["welfare"]] < so$povline[1]))
      }

      fit_list <- tryCatch({
        fit_weather_model(
          df               = df,
          selected_outcome = so,
          selected_weather = selected_weather(),
          selected_model   = selected_model()
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Model failed:", conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        NULL
      })

      if (!is.null(fit_list)) {
        model_fit_val(fit_list)
        shiny::showNotification("Models fitted successfully.", type = "message", duration = 3)
      }

    }, ignoreInit = TRUE)

    # ---- Render outputs and add Results tab ----------------------------------

    observeEvent(model_fit_val(), {
      req(model_fit_val())

      preparing_id <- shiny::showNotification(
        "Preparing results...",
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      on.exit(shiny::removeNotification(preparing_id), add = TRUE)

      mf      <- model_fit_val()
      out_lab <- get_label(mf$y_var)

      # ---- Coefficient plot --------------------------------------------------
      # Weather + interaction coefficients across the three progressive models

      output$coefplot <- renderPlot({
        req(model_fit_val())
        mf <- model_fit_val()

        # Use fit3 as the reference for which coefs exist; filter to those
        # present in all three models to avoid plot_summs alignment errors
        coef_names <- weather_coef_names(mf$fit3, mf$weather_terms, mf$interaction_terms)
        coef_names <- coef_names[
          coef_names %in% names(stats::coef(native_fit(mf$fit1))) &
          coef_names %in% names(stats::coef(native_fit(mf$fit2)))
        ]

        if (length(coef_names) == 0) {
          return(empty_plot("No weather coefficients found to plot."))
        }

        coefs_to_plot <- make_coef_map(coef_names)

        jtools::plot_summs(
          native_fit(mf$fit1),
          native_fit(mf$fit2),
          native_fit(mf$fit3),
          robust      = "HC3",
          coefs       = coefs_to_plot,
          model.names = c("No FE", "FE", "FE + controls")
        ) +
          ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, 25)) +
          ggplot2::labs(x = stringr::str_wrap(paste0("Effect on ", out_lab), 50))
      })

      # ---- Regression table --------------------------------------------------

      output$regtable <- renderUI({
        req(model_fit_val())
        mf <- model_fit_val()

        coef_names    <- weather_coef_names(mf$fit3, mf$weather_terms, mf$interaction_terms)
        coefs_to_plot <- make_coef_map(coef_names)

        ht <- jtools::export_summs(
          native_fit(mf$fit1),
          native_fit(mf$fit2),
          native_fit(mf$fit3),
          robust      = "HC3",
          model.names = c("No FE", "FE", "FE + controls"),
          coefs       = coefs_to_plot,
          digits      = 3
        )
        htmltools::HTML(huxtable::to_html(ht))
      })

      # ---- Effect plots ------------------------------------------------------
      # Predicted welfare vs each weather variable (full model, fit3).

      output$effectplot1 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
        
        mf <- model_fit_val()
        fit <- native_fit(mf$fit3) # Use the fully specified model
        
        # Extract metadata for the 1st weather variable
        pred_var <- mf$weather_terms[1]
        is_binned <- identical(selected_weather()$cont_binned[1], "Binned")
        
        make_weather_effect_plot(
          fit = fit, 
          pred_var = pred_var, 
          interaction_terms = mf$interaction_terms, 
          is_binned = is_binned,
          label_fun = get_label
        )
      })

      output$effectplot2 <- renderPlot({
        # Only render if at least 2 weather variables were selected
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 2)
        
        mf <- model_fit_val()
        fit <- native_fit(mf$fit3) 
        
        # Extract metadata for the 2nd weather variable
        pred_var <- mf$weather_terms[2]
        is_binned <- identical(selected_weather()$cont_binned[2], "Binned")
        
        make_weather_effect_plot(
          fit = fit, 
          pred_var = pred_var, 
          interaction_terms = mf$interaction_terms, 
          is_binned = is_binned,
          label_fun = get_label
        )
      })

      # ---- Add Results tab (once); switch to it on subsequent runs ------------

      if (!results_tab_added()) {
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Results",
            value = "results",
            shiny::h4("Marginal effect of weather on outcome"),
            bslib::card(shiny::plotOutput(ns("coefplot"))),
            shiny::br(),
            shiny::h4("Predicted outcome vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("effectplot1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("effectplot2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Regression results"),
            shiny::uiOutput(ns("regtable"))
          ),
          select  = TRUE,
          session = tabset_session
        )
        results_tab_added(TRUE)

      } else {
        try(
          shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "results"),
          silent = TRUE
        )
      }

      shiny::showNotification("Results ready.", type = "message", duration = 3)

    }, ignoreInit = TRUE)

    # ---- Return ---------------------------------------------------------------

    list(
      model_fit = model_fit_val
    )

  })
}