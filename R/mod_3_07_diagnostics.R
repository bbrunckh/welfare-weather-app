#' 3_07_diagnostics UI Function
#'
#' @description A shiny Module. The Diagnostics tab is inserted into the
#'   parent tabset on the first successful policy simulation run, so this
#'   UI returns nothing.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_07_diagnostics_ui <- function(id) {
  tagList()
}

#' 3_07_diagnostics Server Functions
#'
#' Displays before/after summary tables and histograms for all variables
#' manipulated by the policy scenarios (mod_3_01 through mod_3_04). Inserts
#' a Diagnostics tab into the parent tabset on the first successful run
#' and selects it.
#'
#' @param id               Module id.
#' @param baseline_svy     Reactive survey-weather df before adjustment.
#' @param policy_svy       Reactive survey-weather df after adjustment.
#' @param sim_run_id       Reactive trigger for invalidation; the tab is
#'   appended on the first run for which this is > 0.
#' @param tabset_id        Character id of the parent tabset to append to.
#' @param tabset_session   Shiny session for the parent tabset. Defaults
#'   to the parent session.
#'
#' @noRd
mod_3_07_diagnostics_server <- function(id,
                                         baseline_svy,
                                         policy_svy,
                                         sim_run_id = reactive(0L),
                                         tabset_id,
                                         tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    select_tab <- function(value) {
      if (is.null(tabset_id) || !nzchar(tabset_id)) return(invisible(FALSE))
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = value), silent = TRUE)
      invisible(TRUE)
    }

    diag_tab_added <- reactiveVal(FALSE)

    # ---- Diagnostics data preparation ---------------------------------------

    diag_data <- reactive({
      sim_run_id()
      b <- baseline_svy()
      p <- policy_svy()
      if (is.null(b) || is.null(p)) return(NULL)

      if ("._sp_transfer" %in% names(p)) {
        p$welfare <- p$welfare + p[["._sp_transfer"]]
      }

      transfer_sum <- if ("._sp_transfer" %in% names(p)) sum(p[["._sp_transfer"]], na.rm = TRUE)*365 else 0
      transfer_perhh <- if ("._sp_transfer" %in% names(p)) {
        v <- p[["._sp_transfer"]]
        elig <- v > 0 & !is.na(v)
        if (any(elig)) mean(v[elig]) * 365 else 0
      } else {
        0
      }

      vars <- detect_manipulated_vars(b, p)
      if (length(vars) == 0) return(list(status = "no_change"))

      list(
        manipulated_vars = vars,
        baseline_svy = b,
        policy_svy = p,
        transfer_sum = transfer_sum,
        transfer_perhh = transfer_perhh
      )
    })

    # ---- Transfer summary info box ------------------------------------------

    output$transfer_summary_ui <- renderUI({
    d <- diag_data()
    if (is.null(d) || is.list(d) && !is.null(d$status)) return(NULL)

    shiny::div(
        class = "alert alert-info",
        shiny::h5("Total SP Transfer"),
        shiny::p(paste0(
        "The total transfer amount across the population is approximately ",
        scales::comma(d$transfer_sum, prefix = "$"), 
        " (not scaled to population level yet). On average, this equates to about ",
        scales::comma(d$transfer_perhh, prefix = "$"),
        " per eligible household (per year)."
        ))
    )
    })

    outputOptions(output, "transfer_summary_ui", suspendWhenHidden = FALSE)

    # ---- Summary statistics table -------------------------------------------

    output$diag_summary_table <- DT::renderDT({
      d <- diag_data()
      if (is.null(d)) {
        return(DT::datatable(
          data.frame(
            Message = paste(
              "Select policy options and run simulation to see ",
              "diagnostics."
            )
          ),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      if (is.list(d) && !is.null(d$status)) {
        msg <- if (identical(d$status, "no_change"))
          "No variables were manipulated by the selected policy."
        else
          "Manipulated variables are non-numeric or absent."
        return(DT::datatable(
          data.frame(Message = msg),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      vars <- d$manipulated_vars
      if (length(vars) == 0) {
        return(DT::datatable(
          data.frame(Message = "No numeric variables to summarize."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      df <- policy_input_diagnostics(
        d$baseline_svy, d$policy_svy, vars = vars
      )
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No numeric variables to summarize."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      num_cols <- setdiff(names(df), "variable")
      df[num_cols] <- lapply(df[num_cols], function(x) signif(x, 4))

      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(pageLength = 25, dom = "tp", ordering = TRUE)
      )
    })

    outputOptions(output, "diag_summary_table", suspendWhenHidden = FALSE)

    # ---- Histogram plots container ------------------------------------------

    output$hist_plots_ui <- shiny::renderUI({
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) {
        return(shiny::div(
          class = "alert alert-info",
          "No variables to display."
        ))
      }

      vars <- d$manipulated_vars
      if (length(vars) == 0) {
        return(shiny::div(
          class = "alert alert-info",
          "No variables to display."
        ))
      }

      tags <- lapply(vars, function(var) {
        shiny::div(
          style = "margin-bottom: 30px;",
          shiny::h6(var, style = "margin-bottom: 8px; font-weight: 600;"),
          shiny::plotOutput(ns(paste0("hist_", var)), height = "300px")
        )
      })

      do.call(tagList, tags)
    })

    # ---- Per-variable histogram outputs -------------------------------------

    observeEvent(diag_data(), {
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) return()

      vars <- d$manipulated_vars
      if (length(vars) == 0) return()

      for (var in vars) {
        local({
          var_name <- var
          baseline_vals <- d$baseline_svy[[var_name]]
          policy_vals <- d$policy_svy[[var_name]]

          output[[paste0("hist_", var_name)]] <- renderPlot({
            .make_before_after_hist(
              baseline_vals, policy_vals, var_name
            )
          })
        })
      }
    }, ignoreInit = TRUE)

    # ---- Append Diagnostics tab on first successful run ---------------------

    observeEvent(sim_run_id(), {
      req(sim_run_id() > 0)

      if (!diag_tab_added()) {
        tryCatch(
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Diagnostics",
              value = "diag_tab",
              shiny::wellPanel(
                shiny::h4("Transfer Summary"),
                shiny::uiOutput(ns("transfer_summary_ui"))
              ),
              shiny::wellPanel(
                shiny::h4("Summary of Manipulated Variables"),
                shiny::tags$small(
                  class = "text-muted",
                  "Summary statistics (mean, SD) for variables changed by ",
                  "policy adjustments."
                ),
                DT::DTOutput(ns("diag_summary_table"))
              ),
              shiny::wellPanel(
                shiny::h4("Before/After Distributions"),
                shiny::tags$small(
                  class = "text-muted",
                  "Kernel density plots comparing baseline (grey) vs. ",
                  "policy-adjusted (red) distributions."
                ),
                shiny::uiOutput(ns("hist_plots_ui"))
              )
            ),
            select = TRUE,
            session = tabset_session
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Failed to add Diagnostics tab:", conditionMessage(e)),
              type = "error"
            )
          }
        )
        diag_tab_added(TRUE)
      }

      if (diag_tab_added()) select_tab("diag_tab")
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}


#' Make a before/after histogram for a single variable
#' @noRd
.make_before_after_hist <- function(baseline_vals, policy_vals,
                                    var_name) {
  baseline_clean <- baseline_vals[!is.na(baseline_vals)]
  policy_clean   <- policy_vals[!is.na(policy_vals)]

  if (length(baseline_clean) == 0 && length(policy_clean) == 0) {
    plot.new()
    graphics::title(main = "No data available")
    return(invisible(NULL))
  }

  all_vals <- c(baseline_clean, policy_clean)
  x_range <- range(all_vals, na.rm = TRUE)
  x_range <- x_range + c(-0.1, 0.1) * diff(x_range)

  dens_base <- if (length(baseline_clean) > 1)
    stats::density(baseline_clean, from = x_range[1], to = x_range[2])
  else NULL

  dens_poli <- if (length(policy_clean) > 1)
    stats::density(policy_clean, from = x_range[1], to = x_range[2])
  else NULL

  y_max <- 0
  if (!is.null(dens_base)) y_max <- max(y_max, max(dens_base$y))
  if (!is.null(dens_poli)) y_max <- max(y_max, max(dens_poli$y))

  plot(
    NULL,
    xlim = x_range,
    ylim = c(0, y_max * 1.1),
    xlab = var_name,
    ylab = "Density",
    main = paste0("Distribution: ", var_name),
    cex.main = 0.95
  )

  if (!is.null(dens_base)) {
    graphics::lines(dens_base, col = "#bdbdbd", lwd = 2.5, lty = 1)
  }
  if (!is.null(dens_poli)) {
    graphics::lines(dens_poli, col = "#d32f2f", lwd = 2.5, lty = 1)
  }

  legend_cols <- c(
    if (!is.null(dens_base)) "#bdbdbd" else NULL,
    if (!is.null(dens_poli)) "#d32f2f" else NULL
  )
  legend_labs <- c(
    if (!is.null(dens_base)) "Baseline" else NULL,
    if (!is.null(dens_poli)) "Policy-adjusted" else NULL
  )

  if (length(legend_cols) > 0) {
    graphics::legend("topright", legend = legend_labs, col = legend_cols,
                     lwd = 2.5, cex = 0.85)
  }

  invisible(NULL)
}
