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
    varlist,
    selected_surveys,
    selected_outcome,
    selected_weather,
    survey_weather,
    selected_model,     # reactive list from mod_1_06_model
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

    # Look up a human-readable label for a variable name from varlist.
    # Strips the haz_ prefix added by mod_1_05_weatherstats before lookup,
    # since varlist$name stores the raw variable name (e.g. "spei" not "haz_spei").
    get_label <- function(var_name) {
      vl <- if (is.function(varlist)) varlist() else varlist
      if (is.null(vl)) return(var_name)
      lookup_name <- sub("^haz_", "", var_name)
      idx <- match(lookup_name, vl$name)
      if (is.na(idx)) var_name else vl$label[idx]
    }

    # Turn a coefficient name (possibly "haz_spei:urban" or "I(haz_spei^2):urban")
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
      if (!is.na(so$povline) && so$name == "poor") {
        df <- df |>
          dplyr::mutate(!!so$name := as.numeric(.data[["welfare"]] < so$povline))
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
        mf            <- model_fit_val()
        coefs_to_plot <- make_coef_map(c(mf$weather_terms, mf$interaction_terms))

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
        mf            <- model_fit_val()
        coefs_to_plot <- make_coef_map(c(mf$weather_terms, mf$interaction_terms))

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
      # jtools::effect_plot uses NSE for pred — must convert string to symbol
      # with rlang::sym() and inject with !! to avoid "pred not found" error.

      output$effectplot1 <- renderPlot({
        req(model_fit_val(), length(model_fit_val()$weather_terms) >= 1)
        mf   <- model_fit_val()
        pred <- rlang::sym(mf$weather_terms[1])
        jtools::effect_plot(
          native_fit(mf$fit3),
          pred        = !!pred,
          interval    = TRUE,
          plot.points = FALSE,
          line.colors = "orange",
          x.label     = stringr::str_wrap(get_label(mf$weather_terms[1]), 40),
          y.label     = stringr::str_wrap(out_lab, 40)
        )
      })

      output$effectplot2 <- renderPlot({
        mf <- model_fit_val()
        if (is.null(mf) || length(mf$weather_terms) < 2) {
          return(empty_plot("Select a second weather variable to see this plot."))
        }
        pred <- rlang::sym(mf$weather_terms[2])
        jtools::effect_plot(
          native_fit(mf$fit3),
          pred        = !!pred,
          interval    = TRUE,
          plot.points = FALSE,
          line.colors = "orange",
          x.label     = stringr::str_wrap(get_label(mf$weather_terms[2]), 40),
          y.label     = stringr::str_wrap(out_lab, 40)
        )
      })

      # ---- Interaction plots -------------------------------------------------
      # interact_plot and sim_slopes use the model's stored data frame directly.
      # modx_from_term() extracts the plain moderator name from an interaction
      # term string e.g. "haz_t:urban" -> "urban", "I(haz_t^2):urban" -> "urban"

      modx_from_term <- function(interaction_term) {
        parts <- strsplit(interaction_term, ":")[[1]]
        parts[!grepl("^haz_|^I\\(", parts)][1]
      }

      make_interact_plot <- function(weather_term, modx_term) {
        pred_sym <- rlang::sym(weather_term)
        modx_sym <- rlang::sym(modx_term)
        tryCatch(
          interactions::interact_plot(
            native_fit(model_fit_val()$fit3),
            pred        = !!pred_sym,
            modx        = !!modx_sym,
            interval    = TRUE,
            plot.points = FALSE,
            x.label     = stringr::str_wrap(get_label(weather_term), 40),
            y.label     = stringr::str_wrap(out_lab, 40)
          ) + ggplot2::theme(legend.position = "bottom"),
          error = function(e) empty_plot(paste("Interaction plot failed:", conditionMessage(e)))
        )
      }

      make_simslopes_plot <- function(weather_term, modx_term) {
        pred_sym <- rlang::sym(weather_term)
        modx_sym <- rlang::sym(modx_term)
        tryCatch(
          plot(interactions::sim_slopes(
            native_fit(model_fit_val()$fit3),
            pred = !!pred_sym,
            modx = !!modx_sym
          )),
          error = function(e) empty_plot(paste("Sim slopes failed:", conditionMessage(e)))
        )
      }

      output$interactplot1 <- renderPlot({
        mf <- model_fit_val()
        req(mf, length(mf$weather_terms) >= 1, length(mf$interaction_terms) >= 1)
        make_interact_plot(
          weather_term = mf$weather_terms[1],
          modx_term    = modx_from_term(mf$interaction_terms[1])
        )
      })

      output$simslopes1 <- renderPlot({
        mf <- model_fit_val()
        req(mf, length(mf$weather_terms) >= 1, length(mf$interaction_terms) >= 1)
        make_simslopes_plot(
          weather_term = mf$weather_terms[1],
          modx_term    = modx_from_term(mf$interaction_terms[1])
        )
      })

      output$interactplot2 <- renderPlot({
        mf <- model_fit_val()
        req(mf, length(mf$weather_terms) >= 2, length(mf$interaction_terms) >= 1)
        make_interact_plot(
          weather_term = mf$weather_terms[2],
          modx_term    = modx_from_term(mf$interaction_terms[1])
        )
      })

      output$simslopes2 <- renderPlot({
        mf <- model_fit_val()
        req(mf, length(mf$weather_terms) >= 2, length(mf$interaction_terms) >= 1)
        make_simslopes_plot(
          weather_term = mf$weather_terms[2],
          modx_term    = modx_from_term(mf$interaction_terms[1])
        )
      })

      # Dynamic interaction panel layout
      output$interactplots <- renderUI({
        mf           <- model_fit_val()
        has_interact <- length(mf$interaction_terms) > 0
        has_two_haz  <- length(mf$weather_terms) > 1

        if (!has_interact) {
          return(shiny::p("No interaction term specified.", style = "color: grey;"))
        }

        if (has_two_haz) {
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
            bslib::card(shiny::plotOutput(ns("simslopes1"),    height = "300px")),
            bslib::card(shiny::plotOutput(ns("interactplot2"), height = "300px")),
            bslib::card(shiny::plotOutput(ns("simslopes2"),    height = "300px"))
          )
        } else {
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(shiny::plotOutput(ns("interactplot1"), height = "300px")),
            bslib::card(shiny::plotOutput(ns("simslopes1"),    height = "300px"))
          )
        }
      })

      # ---- Add Results tab (once); switch to it on subsequent runs ------------

      if (!results_tab_added()) {
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