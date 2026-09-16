#' 1_08_modelfit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_08_modelfit_ui <- function(id) {
  tagList()
}

#' 1_08_modelfit Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_outcome Reactive one-row data frame from mod_1_03_outcome.
#' @param model_fit       Reactive list returned by fit_model() via
#'   mod_1_07_results.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset (defaults to parent).
#'
#' @noRd
mod_1_08_modelfit_server <- function(id,
                                     variable_list,
                                     selected_outcome,
                                     model_fit,
                                     tabset_id,
                                     survey_weather,
                                     fit_stale = reactive(FALSE),
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # INT-08: stale banner bound to the fit's staleness flag.
    output$fit_stale_banner <- shiny::renderUI({
      if (isTRUE(fit_stale())) .stale_banner("Step 1 model diagnostics") else NULL
    })

    # Keep the model specification visible on the diagnostics tab, matching
    # the snapshot-based card shown at the top of the Results tab.
    output$selected_model_card <- shiny::renderUI({
      mf <- model_fit()
      req(mf, mf$.snap, mf$.snap$model)
      snap <- mf$.snap
      label_fun <- .label_lookup(snap$variable_list)

      selection_summary_card(
        title = "Selected model",
        badge = model_covariate_badge(snap$model),
        rows = model_card_rows(
          snap$model,
          label_fun      = label_fun,
          outcome_label  = as.character(snap$outcome$label[1]),
          weather_labels = as.character(snap$weather$label)
        ),
        info = paste(
          "The fitted specification is shown as selected outcome, weather,",
          "interaction, and fixed-effect variables. Results",
          "reflect this run until you press Run model again."
        )
      )
    })

    # ---- Internal state -----------------------------------------------------

    modelfit_tab_added <- reactiveVal(FALSE)

    # ---- Helpers ------------------------------------------------------------

    # INT-05: bind diagnostic renderers to the fit-time snapshot so new
    # selections cannot relabel or re-frame an already-fitted model.
    fit_snap <- reactive({
      req(model_fit())
      model_fit()$.snap
    })
    snap_label_fun <- reactive({
      .label_lookup(fit_snap()$variable_list)
    })

    full_model <- reactive({
      req(model_fit())
      mf <- model_fit()
      fit <- extract_native_fit(mf$fit3, mf$engine)
      # For RIF: return the full fixest_multi for calc_fit_stats,
      # but diagnostic plots that need a single model use rif_single_model()
      fit
    })

    # Single representative model for diagnostics (median quantile for RIF)
    rif_single_model <- reactive({
      req(model_fit())
      mf <- model_fit()
      extract_rif_median(mf$fit3, mf$engine)
    })

    is_logistic <- reactive({
      req(model_fit())
      is_logistic_fit(model_fit())
    })

    # ---- Outputs ------------------------------------------------------------

    output$resid_weather1 <- renderPlot({
      req(full_model(), model_fit(), fit_snap())
      h <- model_fit()$weather_terms[1]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h,
        weather_df = fit_snap()$survey_weather,
        x_label = resid_axis_lab(h)
      )
    })

    output$resid_weather2 <- renderPlot({
      req(full_model(), model_fit(), length(model_fit()$weather_terms) >= 2, fit_snap())
      h <- model_fit()$weather_terms[2]
      req(!is.na(h))
      m <- rif_single_model()
      plot_resid_weather(m, h,
        weather_df = fit_snap()$survey_weather,
        x_label = resid_axis_lab(h)
      )
    })

    # Unit-complete x-axis label for the residual plots, mirroring the effect
    # plots on the Results tab ("Daily maximum temperature bins (deg C)").
    resid_axis_lab <- function(h) {
      slf <- snap_label_fun()
      lab <- wise_label_short(slf(h))
      w <- fit_snap()$weather
      un <- if (!is.null(w) && !is.null(w$name) && h %in% as.character(w$name)) {
        as.character(w$units[w$name == h][1])
      } else {
        NA_character_
      }
      binned <- !is.null(w) && !is.null(w$cont_binned) &&
        h %in% as.character(w$name) &&
        identical(as.character(w$cont_binned[w$name == h][1]), "Binned")
      if (is.null(un) || is.na(un) || !nzchar(un)) {
        if (binned) paste0(lab, " bins") else lab
      } else if (binned) {
        paste0(lab, " bins (", un, ")")
      } else {
        paste0(lab, " (", un, ")")
      }
    }

    # UI-48: one builder behind the plot and its export, so a downloaded PNG
    # is the figure on screen.
    pred_welf_fig <- function() {
      mf <- model_fit()
      snap <- fit_snap()
      slf <- snap_label_fun()
      if (is.null(mf) || is.null(snap)) {
        return(NULL)
      }
      if (identical(mf$engine, "rif")) {
        # RIF models predict the RIF-transformed outcome (effectively binary
        # per quantile), so the standard predicted-vs-actual histogram is not
        # meaningful. Instead show the original welfare distribution with
        # predicted quantile markers.
        y <- mf$train_data[[mf$y_var]]
        taus <- mf$taus
        q_vals <- stats::quantile(y, probs = taus, names = FALSE)
        q_df <- data.frame(tau = paste0("\u03c4=", taus), value = q_vals)
        ggplot2::ggplot(data.frame(y = y), ggplot2::aes(x = y)) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = 100 * ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))),
            fill = .wise_blue, alpha = 0.7, bins = 30
          ) +
          ggplot2::geom_vline(
            data = q_df, ggplot2::aes(xintercept = value),
            linetype = "dashed", colour = .wise_marker_alt,
            linewidth = 0.5
          ) +
          ggplot2::geom_text(
            data = q_df,
            ggplot2::aes(x = value, y = Inf, label = tau),
            vjust = 1.5, hjust = -0.1, size = 3.2,
            colour = .wise_marker_alt
          ) +
          ggplot2::labs(
            subtitle = "Welfare distribution with estimated quantiles",
            x = stringr::str_wrap(slf(snap$outcome$name), 40),
            y = "Share of households (%)"
          ) +
          theme_wise()
      } else {
        m <- rif_single_model()
        plot_pred_vs_actual(
          model         = m,
          is_logistic   = is_logistic(),
          outcome_label = slf(snap$outcome$name)
        )
      }
    }

    output$pred_welf_dist <- renderPlot({
      req(full_model(), fit_snap())
      pred_welf_fig()
    })

    # Approximate contribution of each term to the model's explained
    # variation (squared standardized coefficients, share of their sum).
    importance_fig <- function() {
      req(full_model(), model_fit(), fit_snap())
      plot_importance(rif_single_model(), label_fun = snap_label_fun())
    }

    output$importance_plot <- renderPlot({
      importance_fig()
    })

    # Residual diagnostics: residuals vs fitted + normal QQ (linear, LPM and
    # RIF), binned residual means for binary outcomes. One builder behind the
    # plot and its export (UI-48).
    residual_panels_fig <- function() {
      req(full_model(), model_fit())
      plot_residual_panels(rif_single_model(), is_logistic = is_logistic())
    }

    output$residual_panels <- renderPlot({
      residual_panels_fig()
    })

    # UI-45: one data frame behind both the table and its CSV export.
    additional_stats_df <- reactive({
      req(full_model(), model_fit())
      calc_fit_stats(
        model       = full_model(),
        is_logistic = is_logistic(),
        engine      = model_fit()$engine,
        taus        = model_fit()$taus
      )
    })

    output$additional_stats <- renderTable(
      additional_stats_df(),
      striped = TRUE, hover = TRUE, bordered = TRUE
    )

    output$additional_stats_csv <- csv_download_handler(
      "model_fit_statistics",
      function() additional_stats_df(),
      stale = fit_stale
    )

    wise_export_table(
      key = "model_fit_statistics",
      label = "Model fit statistics",
      step = 1L,
      fun = function() additional_stats_df(),
      description = paste(
        "Goodness-of-fit measures for the full specification: observations,",
        "R-squared (and adjusted/within variants), or McFadden R-squared and",
        "AIC for binary outcomes; per-quantile rows for RIF models."
      )
    )

    # UI-48: model-fit figures for the export bundle. Each builder req()s on
    # the same inputs its on-screen renderer does, so a not-run step throws
    # shiny.silent.error and is skipped quietly, while a genuine failure is
    # named in the README's "Not exported" section.
    for (i in 1:2) {
      local({
        idx <- i
        wise_export_figure(
          key = paste0("residuals_vs_weather_", idx),
          label = paste0("Residuals vs weather ", idx),
          step = 1L,
          fun = function() {
            req(full_model(), model_fit(), fit_snap())
            h <- model_fit()$weather_terms[idx]
            if (is.na(h) || is.null(h)) {
              return(NULL)
            }
            plot_resid_weather(rif_single_model(), h,
              weather_df = fit_snap()$survey_weather,
              x_label = resid_axis_lab(h)
            )
          },
          description = paste(
            "Model residuals against the realised weather variable, for",
            "checking that no systematic structure is left unexplained."
          ),
          width = 9, height = 6
        )
      })
    }

    wise_export_figure(
      key = "predicted_vs_actual_welfare",
      label = "Predicted vs actual welfare",
      step = 1L,
      fun = function() {
        req(full_model(), fit_snap())
        pred_welf_fig()
      },
      description = paste(
        "Distribution of predicted welfare against observed welfare in the",
        "training data (for binary outcomes, a calibration curve of observed",
        "vs predicted rates by decile of predicted risk; for RIF models, the",
        "welfare distribution with predicted quantile markers)."
      ),
      width = 9, height = 6
    )

    wise_export_figure(
      key = "r2_contribution",
      label = "Contribution to fit by term",
      step = 1L,
      fun = function() {
        importance_fig()
      },
      description = paste(
        "Approximate contribution of each term to the model's explained",
        "variation: squared standardized coefficients (|beta| x sd(x))^2 as a",
        "share of their sum. Fixed effects excluded; collinearity ignored."
      ),
      width = 9, height = 6
    )

    wise_export_figure(
      key = "model_diagnostics",
      label = "Model residual diagnostics",
      step = 1L,
      fun = function() {
        residual_panels_fig()
      },
      description = paste(
        "Residuals vs fitted values with a smooth trend and a normal",
        "quantile-quantile plot (for binary outcomes, binned residual means",
        "by decile of predicted risk)."
      ),
      width = 10, height = 5
    )

    model_summary_text <- function() {
      req(full_model())
      m <- rif_single_model()
      prefix <- if (identical(model_fit()$engine, "rif")) {
        "Unconditional quantile regression (RIF) - Median quantile (tau = 0.5):\n"
      } else {
        ""
      }
      vcov_spec <- tryCatch(.fixest_vcov_spec(m), error = function(e) NULL)
      summary_text <- capture.output(
        if (is.null(vcov_spec)) summary(m) else summary(m, vcov = vcov_spec)
      )
      c(prefix, summary_text)
    }

    output$model_summary <- renderPrint({
      cat(model_summary_text(), sep = "\n")
    })
    wise_export_table(
      key = "model_summary",
      label = "Raw model summary",
      step = 1L,
      fun = function() {
        data.frame(
          line = model_summary_text(),
          stringsAsFactors = FALSE
        )
      },
      description = "Text-formatted native model summary shown in the expandable Model fit panel."
    )

    # ---- Add tab (once) -----------------------------------------------------

    # Reactive layout: 1 panel for 1 weather var, 2 side-by-side for >= 2.
    # Wrapping in renderUI keeps the layout in sync if the model is re-fit
    # with a different number of weather variables.
    output$resid_weather_layout <- shiny::renderUI({
      req(model_fit())
      wt <- model_fit()$weather_terms %||% character(0)
      weather_plot_layout(
        ns, length(wt),
        ids = c("resid_weather1", "resid_weather2"),
        height = "300px",
        alts = vapply(seq_len(max(length(wt), 1L)), function(i) {
          paste(
            "Scatter plot of model residuals versus", wt[i],
            "with the fitted relationship"
          )
        }, character(1))
      )
    })

    observeEvent(model_fit(),
      {
        req(model_fit())
        if (modelfit_tab_added()) {
          return()
        }

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Model fit",
            value = "model_fit",
            shiny::uiOutput(ns("fit_stale_banner")),
            shiny::uiOutput(ns("selected_model_card")),
            bslib::layout_columns(
              col_widths = c(5, 7),
              shiny::div(
                shiny::h4(
                  "Fit statistics",
                  info_popover(
                    p(paste(
                      "Goodness-of-fit measures for the full specification",
                      "(fixed effects and controls); the fitted model is",
                      "described on the Results tab. Statistic names and",
                      "formatting match the fit snippet on the Results tab.",
                      "RIF models report one row per quantile."
                    ))
                  )
                ),
                shiny::tableOutput(ns("additional_stats")),
                csv_download_link(ns("additional_stats_csv"))
              ),
              shiny::div(
                shiny::h4(
                  "Predicted vs actual",
                  info_popover(
                    p(paste(
                      "How well the fitted model reproduces the outcome in the",
                      "training data \u2014 the distribution the simulations",
                      "build on. Linear models overlay the observed",
                      "distribution with the model's fitted values: the",
                      "systematic component only, with residuals not added",
                      "back, so the predicted distribution is narrower than",
                      "the observed one. Binary outcomes are checked by",
                      "calibration (observed vs predicted rates by decile of",
                      "predicted risk); RIF models show the welfare",
                      "distribution with the estimated quantiles."
                    ))
                  )
                ),
                bslib::card(wise_plot_output(
                  ns("pred_welf_dist"),
                  "Predicted versus actual welfare in the training data; calibration curve for binary outcomes, welfare distribution with quantile markers for RIF models"
                ))
              )
            ),
            shiny::hr(),
            shiny::h4(
              "Contribution to fit",
              info_popover(
                p(paste(
                  "Approximate contribution of each term to the model's",
                  "explained variation: squared standardized coefficients",
                  "(|beta| \u00d7 sd(x))\u00b2 as a share of their sum. Fixed",
                  "effects are excluded and collinearity is ignored, so treat",
                  "the ranking as indicative."
                ))
              )
            ),
            bslib::card(wise_plot_output(
              ns("importance_plot"),
              "Bar plot of each term's approximate share of the model's explained variation"
            )),
            shiny::hr(),
            shiny::h4(
              "Residuals vs weather",
              info_popover(
                p(paste(
                  "Model residuals against each realised weather variable.",
                  "Grey points are individual residuals, orange marks are",
                  "bin means, and the dashed line marks zero. Systematic",
                  "structure here means the fitted weather response \u2014 the",
                  "relationship the simulations build on \u2014 leaves patterns",
                  "unexplained."
                ))
              )
            ),
            shiny::uiOutput(ns("resid_weather_layout")),
            shiny::hr(),
            shiny::h4(
              "Residual diagnostics",
              info_popover(
                p(paste(
                  "Residuals vs fitted values with a smooth trend (left) and a",
                  "normal quantile-quantile plot of the residuals (right). For",
                  "binary outcomes, binned residual means by decile of predicted",
                  "risk are shown instead; bins should scatter around zero",
                  "without a trend."
                ))
              )
            ),
            bslib::card(wise_plot_output(
              ns("residual_panels"),
              "Diagnostic panels: residuals versus fitted values with a smooth trend, a normal quantile-quantile plot, and binned residual means for binary outcomes"
            )),
            shiny::hr(),
            shiny::tags$details(
              shiny::tags$summary("Raw model summary"),
              shiny::verbatimTextOutput(ns("model_summary"))
            )
          ),
          select = FALSE,
          session = tabset_session
        )

        modelfit_tab_added(TRUE)
      },
      ignoreInit = TRUE
    )
  })
}
