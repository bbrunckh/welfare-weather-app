#' 3_08_diagnostics UI Function
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
mod_3_08_diagnostics_ui <- function(id) {
  tagList()
}

#' 3_08_diagnostics Server Functions
#'
#' Displays before/after summary tables and histograms for all variables
#' manipulated by the policy scenarios (mod_3_01 through mod_3_05). Only
#' variables still present in the Step 1 model are manipulated upstream by
#' \code{apply_policy_to_svy()}, so a variable dropped from Step 1 no longer
#' appears here. Inserts
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
mod_3_08_diagnostics_server <- function(id,
                                         baseline_svy,
                                         policy_svy,
                                         sim_run_id = reactive(0L),
                                         tabset_id,
                                         tabset_session = NULL,
                                         analysis_unit = reactive("hh")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    diag_tab_added <- reactiveVal(FALSE)

    # Helper: human-readable unit word ("individual" / "individuals" /
    # "Household" / "Households" / "Firm" / "Firms") driven by analysis_unit().
    unit_word <- function(plural = TRUE, capitalize = FALSE) {
      au <- tryCatch(analysis_unit(), error = function(e) "hh")
      au <- if (is.null(au) || !nzchar(au)) "hh" else au
      word <- switch(au,
        ind  = if (plural) "individuals" else "individual",
        hh   = if (plural) "households"  else "household",
        firm = if (plural) "firms"       else "firm",
        if (plural) "households" else "household"
      )
      if (capitalize) {
        paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
      } else {
        word
      }
    }

    # ---- Diagnostics data preparation ---------------------------------------

    diag_data <- reactive({
      sim_run_id()
      b <- baseline_svy()
      p <- policy_svy()
      if (is.null(b) || is.null(p)) return(NULL)

      if (SP_TRANSFER_COL %in% names(p)) {
        p$welfare <- p$welfare + p[[SP_TRANSFER_COL]]
      }

      # UI-32: this arithmetic now lives in `.sp_transfer_totals()`, which the
      # Step 3 sidebar's reach preview also calls - the two showed different
      # totals while each re-derived it, so there is one implementation.
      totals        <- .sp_transfer_totals(p, analysis_unit())
      transfer_sum  <- totals$total
      transfer_unit <- totals$per_unit

      vars <- detect_manipulated_vars(b, p)
      if (length(vars) == 0) return(list(status = "no_change"))

      list(
        manipulated_vars = vars,
        baseline_svy = b,
        policy_svy = p,
        transfer_sum = transfer_sum,
        transfer_pp = transfer_unit
      )
    })

    # ---- Transfer summary info box ------------------------------------------

    output$transfer_summary_ui <- DT::renderDT({
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) {
        return(DT::datatable(
          data.frame(Message = "No transfer data available."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      # UI-32: displayed figures are rounded to one decimal, matching the
      # Step 3 sidebar's reach preview (fmt_num()) so the same quantity never
      # appears at two precisions.
      df <- data.frame(
        Type  = c(
          "Total transfer $ amount (population-level)",
          paste0("Per-", unit_word(plural = FALSE),
                 " $ equivalent (eligible ", unit_word(plural = TRUE), ")")
        ),
        Value = fmt_num(c(d$transfer_sum, d$transfer_pp), prefix = "$"),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        extensions = "Buttons",
        options = list(dom = wise_csv_dom("t"), ordering = FALSE,
                       buttons = wise_csv_button("policy_transfer_summary"))
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

      # UI-32: one decimal everywhere, instead of 4 significant figures which
      # rendered as 0.1234 next to 12340 in the same column.
      num_cols <- setdiff(names(df), "variable")
      df[num_cols] <- lapply(df[num_cols], function(x) {
        if (is.numeric(x)) round(x, 1) else x
      })

      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        extensions = "Buttons",
        options = list(pageLength = 25, dom = wise_csv_dom("tp"),
                       ordering = TRUE,
                       buttons = wise_csv_button("policy_diagnostics"))
      )
    })

    outputOptions(output, "diag_summary_table", suspendWhenHidden = FALSE)

    # UI-48: register Step 3's diagnostics for the export bundle.
    wise_export_table(
      key   = "policy_transfer_summary",
      label = "Social protection transfer summary",
      step  = 3L,
      fun   = function() {
        d <- tryCatch(diag_data(), error = function(e) NULL)
        if (is.null(d) || !is.null(d$status)) return(NULL)
        data.frame(
          metric = c("total_transfer_population", "transfer_per_unit"),
          value  = round(c(d$transfer_sum, d$transfer_pp), 1),
          stringsAsFactors = FALSE
        )
      },
      description = paste(
        "Population-level annual cost of the social protection transfer and",
        "the per-recipient equivalent."
      )
    )

    wise_export_table(
      key   = "policy_input_diagnostics",
      label = "Policy input diagnostics",
      step  = 3L,
      fun   = function() {
        d <- tryCatch(diag_data(), error = function(e) NULL)
        if (is.null(d) || !is.null(d$status)) return(NULL)
        vars <- d$manipulated_vars
        if (!length(vars)) return(NULL)
        df <- policy_input_diagnostics(d$baseline_svy, d$policy_svy, vars = vars)
        if (is.null(df) || nrow(df) == 0) return(NULL)
        num <- setdiff(names(df), "variable")
        df[num] <- lapply(df[num], function(x) if (is.numeric(x)) round(x, 1) else x)
        df
      },
      description = paste(
        "Before/after summary of every covariate the policy scenario changed,",
        "so the levers that actually moved can be checked."
      )
    )

    # ---- Histogram plots container ------------------------------------------

    output$hist_plots_ui <- shiny::renderUI({
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) {
        return(shiny::div(
          "No variables to display."
        ))
      }

      vars <- d$manipulated_vars
      if (length(vars) == 0) {
        return(shiny::div(
          "No variables to display."
        ))
      }

      tags <- lapply(vars, function(var) {
        shiny::div(
          style = "margin-bottom: 30px;",
          shiny::h6(
            paste0(toupper(substr(var, 1, 1)), substr(var, 2, nchar(var))),
            style = "margin-bottom: 8px; font-weight: 600;"
          ),
          wise_plot_output(
            ns(paste0("hist_", var)),
            paste("Histogram of", var, "before and after the policy adjustment"),
            height = "300px"
          )
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
            shiny::h4("Total social protection transfer amount"),
            DT::DTOutput(ns("transfer_summary_ui")),
            shiny::div(style = "margin: 12px 0;"),
            shiny::h4("Summary of manipulated variables"),
            shiny::tags$small(
              class = "text-muted",
              "Summary statistics (mean, SD) for variables changed by ",
              "policy adjustments."
            ),
            DT::DTOutput(ns("diag_summary_table")),
            shiny::h4("Before/after distributions"),
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
