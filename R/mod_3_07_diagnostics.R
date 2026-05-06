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

      # Detect survey weight column (same pattern as Step 2 / resimulate_with_svy)
      wt_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                     names(p), value = TRUE,
                     ignore.case = TRUE)[1]
      if (is.na(wt_col %||% NA)) wt_col <- NULL

      if ("._sp_transfer" %in% names(p)) {
        v <- p[["._sp_transfer"]]
        w <- if (!is.null(wt_col)) p[[wt_col]] else rep(1, length(v))
        ok <- is.finite(v) & is.finite(w)
        # Population-level total annual cost = sum(daily transfer * weight) * 365
        transfer_sum <- sum(v[ok] * w[ok]) * 365
        # Population-weighted mean over eligible households (annual)
        elig <- ok & v > 0
        transfer_perhh <- if (any(elig) && sum(w[elig]) > 0) {
          sum(v[elig] * w[elig]) / sum(w[elig]) * 365
        } else 0
      } else {
        transfer_sum   <- 0
        transfer_perhh <- 0
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

    output$transfer_summary_ui <- renderTable({
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) return(NULL)
      data.frame(
        Type  = c("Total transfer $ amount (population-level)", "Per-household $ equivalent (eligible households)"),
        Value = c(d$transfer_sum, d$transfer_perhh),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

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
          # class = "alert alert-info",
          "No variables to display."
        ))
      }

      vars <- d$manipulated_vars
      if (length(vars) == 0) {
        return(shiny::div(
          # class = "alert alert-info",
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
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Diagnostics",
            value = "diag_tab",
            shiny::h4("Total SP Transfer Amount"),
            shiny::tags$small(style = "color: #c00;", "Needs to be scale to population level."),
            shiny::tableOutput(ns("transfer_summary_ui")),
            shiny::div(style = "margin: 12px 0;"),
            shiny::h4("Summary of Manipulated Variables"),
            shiny::tags$small(
              class = "text-muted",
              "Summary statistics (mean, SD) for variables changed by ",
              "policy adjustments."
            ),
            DT::DTOutput(ns("diag_summary_table")),
            shiny::h4("Before/After Distributions"),
            shiny::tags$small(
              class = "text-muted",
              "Kernel density plots comparing baseline (grey) vs. ",
              "policy-adjusted (red) distributions."
            ),
            shiny::uiOutput(ns("hist_plots_ui"))
          ),
          select = FALSE,
          session = tabset_session
        )
        diag_tab_added(TRUE)
      }

    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
