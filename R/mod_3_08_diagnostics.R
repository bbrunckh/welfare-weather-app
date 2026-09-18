#' Diagnostics tab content UI.
#' @noRd
.policy_display_name <- function(x) {
  x <- gsub("[_\\.]+", " ", x)
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", x)
  tools::toTitleCase(x)
}

.policy_input_table_raw <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  if (ncol(df) == 6L) {
    names(df) <- c(
      "Variable", "Baseline mean", "Policy mean",
      "Change in mean", "Baseline spread", "Policy spread"
    )
  } else if (ncol(df) == 7L) {
    names(df) <- c(
      "Variable", names(df)[2], "Baseline mean", "Policy mean",
      "Change in mean", "Baseline spread", "Policy spread"
    )
  } else {
    stop("Policy input summary must have six or seven columns.")
  }
  df$Variable <- .policy_display_name(df$Variable)
  df
}

.format_policy_input_table <- function(df) {
  df <- .policy_input_table_raw(df)
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  num_cols <- setdiff(names(df), "Variable")
  df[num_cols] <- lapply(df[num_cols], function(x) fmt_num(x, digits = 2))
  df
}

.policy_changed_counts <- function(baseline_svy, policy_svy, vars) {
  setNames(vapply(vars, function(v) {
    b <- baseline_svy[[v]]
    p <- policy_svy[[v]]
    sum((!is.na(b) & !is.na(p) & b != p) | (is.na(b) != is.na(p)), na.rm = TRUE)
  }, numeric(1L)), vars)
}

.policy_treatment_table_raw <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  data.frame(
    `Coverage status` = df$status,
    `Sample units` = suppressWarnings(as.numeric(df$n)),
    `Population represented` = suppressWarnings(as.numeric(df$weighted_n)),
    `Population share` = 100 * suppressWarnings(as.numeric(df$weighted_share)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.format_policy_treatment_table <- function(df) {
  raw <- .policy_treatment_table_raw(df)
  if (is.null(raw) || !nrow(raw)) {
    return(raw)
  }
  data.frame(
    `Coverage status` = raw$`Coverage status`,
    `Sample units` = fmt_num(raw$`Sample units`, digits = 0),
    `Population represented` = fmt_num(raw$`Population represented`, digits = 0),
    `Population share` = fmt_num(raw$`Population share`, digits = 1, suffix = "%"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.policy_treatment_explanation <- function(sp) {
  if (is.null(sp) || !is.list(sp)) {
    return(paste(
      "Eligibility follows the selected targeting rule; treatment is a positive transfer after assignment.",
      "This table shows counterfactual assignment, not observed cash receipt."
    ))
  }

  targeting <- sp$targeting %||% "exante_poor"
  targeting_text <- switch(targeting,
    exante_poor = paste0(
      "Eligibility is the bottom ", sp$targeting_threshold %||% 20,
      "% by baseline welfare."
    ),
    pmt = paste0(
      "Eligibility follows the selected proxy variable and cutoff."
    ),
    universal = "Universal targeting makes every baseline unit eligible and applies no targeting errors.",
    "Eligibility follows the selected targeting rule before targeting errors."
  )

  if (identical(targeting, "universal")) {
    return(paste(
      targeting_text,
      "Treatment is a positive transfer after assignment; this table shows counterfactual assignment."
    ))
  }

  incl <- sp$inclusion_error_pct %||% 0
  excl <- sp$exclusion_error_pct %||% 0
  paste(
    targeting_text,
    paste0(
      "The run then applies the selected targeting errors: ", incl,
      "% inclusion error can treat ineligible units, and ", excl,
      "% exclusion error can miss eligible units."
    ),
    "Treatment is a positive transfer after that draw; this is counterfactual assignment."
  )
}

.policy_component_table_raw <- function(df, analysis_unit = "hh") {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  unit_label <- if (identical(analysis_unit, "hh")) "Households affected / covered" else "Observations affected / covered"
  data.frame(
    `Policy component` = df$component,
    setNames(list(suppressWarnings(as.numeric(df$n_affected))), unit_label),
    `Population represented` = suppressWarnings(as.numeric(df$weighted_affected)),
    `Population share` = 100 * suppressWarnings(as.numeric(df$population_share)),
    `Realized cost` = suppressWarnings(as.numeric(df$realized_cost)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.format_policy_component_table <- function(df, analysis_unit = "hh") {
  raw <- .policy_component_table_raw(df, analysis_unit)
  if (is.null(raw) || !nrow(raw)) {
    return(raw)
  }
  data.frame(
    `Policy component` = raw$`Policy component`,
    raw[setdiff(names(raw), c(
      "Policy component", "Population represented", "Population share", "Realized cost"
    ))],
    `Population represented` = fmt_num(raw$`Population represented`, digits = 0),
    `Population share` = fmt_num(raw$`Population share`, digits = 1, suffix = "%"),
    `Realized cost` = fmt_num(raw$`Realized cost`, digits = 0, prefix = "$"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Shared reactable styling for the diagnostics tables (guidelines §6): raw
# values in the data, display rounding via colFormat; `formats` maps column
# names to colFormat argument lists. Fallback states pass a single-column
# Note frame (mod_1_02 pattern).
#' @noRd
.wise_diag_reactable <- function(df, formats = list()) {
  cols <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (is.numeric(x)) {
      fmt <- formats[[nm]] %||% list(digits = 2, separators = TRUE)
      reactable::colDef(
        format = do.call(reactable::colFormat, fmt),
        class = "wise-dt-wrap",
        minWidth = 90
      )
    } else if (is.character(x) || is.factor(x)) {
      reactable::colDef(class = "wise-dt-wrap", minWidth = 170)
    } else {
      reactable::colDef(class = "wise-dt-wrap", minWidth = 70)
    }
  })
  names(cols) <- names(df)
  reactable::reactable(
    df,
    columns = cols,
    compact = TRUE,
    searchable = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    highlight = TRUE
  )
}

.diagnostics_content_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("stale_banner_ui")),
    shiny::uiOutput(ns("policy_summary_ui")),
    shiny::h4(
      "Which variables changed?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::tags$p(
        class = "diagnostic-note",
        "Before/after summaries use the same baseline units."
      ),
      shiny::div(
        class = "wise-reactable-controls",
        wise_reactable_csv_button(ns("diag_summary_table"), "policy_input_diagnostics")
      ),
      reactable::reactableOutput(ns("diag_summary_table"))
    ),
    shiny::h4(
      "How did the policy change the baseline population?",
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::tags$p(
        class = "diagnostic-note",
        "Charts show constructed before/after inputs, not observed impacts."
      ),
      shiny::uiOutput(ns("hist_plots_ui"))
    ),
    shiny::h4(
      "What targeting rule was specified, and how did errors alter assignment?",
      info_popover(
        title = "Program scale and targeting",
        shiny::p(
          "Transfer totals are realized values in the policy-adjusted survey.",
          "The treatment table compares eligibility under the selected targeting",
          "rule with realized positive-transfer treatment after targeting errors."
        ),
        docs = TRUE
      ),
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::tags$p(
        class = "diagnostic-note",
        "Social protection counts positive transfers; other rows count changed covariates. Overlap counts units touched by both. Only social protection has a cost."
      ),
      shiny::div(
        class = "wise-reactable-controls",
        wise_reactable_csv_button(ns("transfer_summary_ui"), "policy_transfer_summary")
      ),
      reactable::reactableOutput(ns("transfer_summary_ui")),
      shiny::div(
        class = "wise-reactable-controls",
        wise_reactable_csv_button(ns("policy_component_table"), "policy_component_summary")
      ),
      reactable::reactableOutput(ns("policy_component_table")),
      shiny::h5("Eligibility versus realized social-protection treatment"),
      shiny::tags$p(
        class = "diagnostic-note",
        "Eligibility follows the selected rule; treatment is a positive transfer after errors. Rows show counterfactual assignment, not observed cash receipt."
      ),
      shiny::uiOutput(ns("treatment_explanation_ui")),
      shiny::div(
        class = "wise-reactable-controls",
        wise_reactable_csv_button(ns("treatment_table"), "policy_treatment_assignment")
      ),
      reactable::reactableOutput(ns("treatment_table"))
    ),
  )
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
#' @param diagnostic_summary Reactive immutable summary snapshot published by
#'   the policy runner after a successful run.
#' @param selected_policies Reactive selected policy scenario keys.
#' @param baseline_hist_sim Reactive Step 2-style baseline simulation result.
#' @param selected_weather Reactive selected weather specification.
#' @param policy_saved_scenarios Reactive named future scenario list.
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
                                        diagnostic_summary = reactive(NULL),
                                        sim_run_id = reactive(0L),
                                        tabset_id,
                                        tabset_session = NULL,
                                        analysis_unit = reactive("hh"),
                                        selected_policies = reactive(NULL),
                                        policy_scenarios = reactive(list()),
                                        baseline_hist_sim = reactive(NULL),
                                        selected_weather = reactive(NULL),
                                        sp_scenario = reactive(NULL),
                                        infra_scenario = reactive(NULL),
                                        digital_scenario = reactive(NULL),
                                        labor_scenario = reactive(NULL),
                                        education_scenario = reactive(NULL),
                                        policy_saved_scenarios = reactive(list()),
                                        stale = reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session$userData$wise_step3_stale <- stale

    output$stale_banner_ui <- shiny::renderUI({
      if (isTRUE(stale())) {
        .stale_banner(
          "Step 3 policy diagnostics",
          note = NULL
        )
      } else {
        NULL
      }
    })

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    diag_tab_added <- reactiveVal(FALSE)

    # Helper: human-readable unit word ("individual" / "individuals" /
    # "Household" / "Households" / "Firm" / "Firms") driven by analysis_unit().
    unit_word <- function(plural = TRUE, capitalize = FALSE, au = NULL) {
      au <- au %||% tryCatch(analysis_unit(), error = function(e) "hh")
      au <- if (is.null(au) || !nzchar(au)) "hh" else au
      word <- switch(au,
        ind  = if (plural) "individuals" else "individual",
        hh   = if (plural) "households" else "household",
        firm = if (plural) "firms" else "firm",
        if (plural) "households" else "household"
      )
      if (capitalize) {
        paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
      } else {
        word
      }
    }

    # Diagnostics data preparation ----

    # One successful run publishes one complete snapshot. Failed runs leave
    # this reactive value untouched, so renderers and exports keep the prior
    # internally consistent diagnostics instead of rebuilding from live state.
    diag_data <- reactive({
      sim_run_id()
      published <- diagnostic_summary()
      if (!is.null(published)) {
        return(published)
      }

      # Backward-compatible path for direct module callers that predate atomic
      # publication. Production supplies `diagnostic_summary`, so successful
      # runs never rescan the live survey frames here.
      .policy_diagnostics_snapshot(
        svy_baseline = baseline_svy(),
        svy_policy = policy_svy(),
        analysis_unit = analysis_unit(),
        sp = sp_scenario()
      )
    })

    # Transfer summary info box ----

    output$transfer_summary_ui <- reactable::renderReactable({
      d <- diag_data()
      if (is.null(d) || is.list(d) && !is.null(d$status)) {
        return(.wise_diag_reactable(
          data.frame(Note = "No transfer data available.")
        ))
      }
      # UI-32: displayed figures are rounded to one decimal, matching the
      # Step 3 sidebar's reach preview (fmt_num()) so the same quantity never
      # appears at two precisions. Raw values live in the data; rounding is
      # applied by colFormat.
      df <- data.frame(
        Type = c(
          "Total transfer $ amount (population-level)",
          paste0(
            "Per-", unit_word(plural = FALSE, au = d$analysis_unit),
            " $ equivalent (eligible ",
            unit_word(plural = TRUE, au = d$analysis_unit), ")"
          )
        ),
        Value = c(d$transfer_sum, d$transfer_pp),
        stringsAsFactors = FALSE
      )
      .wise_diag_reactable(df, formats = list(
        Value = list(digits = 1, prefix = "$", separators = TRUE)
      ))
    })

    outputOptions(output, "transfer_summary_ui", suspendWhenHidden = FALSE)

    # Summary statistics table ----

    output$diag_summary_table <- reactable::renderReactable({
      d <- diag_data()
      if (is.null(d)) {
        return(.wise_diag_reactable(
          data.frame(Note = paste(
            "Select policy options and run simulation to see ",
            "diagnostics."
          ))
        ))
      }
      if (is.list(d) && !is.null(d$status)) {
        msg <- if (identical(d$status, "no_change")) {
          "No variables were manipulated by the selected policy."
        } else {
          "Manipulated variables are non-numeric or absent."
        }
        return(.wise_diag_reactable(data.frame(Note = msg)))
      }

      vars <- d$manipulated_vars
      if (length(vars) == 0) {
        return(.wise_diag_reactable(
          data.frame(Note = "No numeric variables to summarize.")
        ))
      }

      df <- d$input_summary
      if (is.null(df) || nrow(df) == 0) {
        return(.wise_diag_reactable(
          data.frame(Note = "No numeric variables to summarize.")
        ))
      }

      counts <- d$changed_counts
      count_label <- if (identical(d$analysis_unit, "hh")) "Households changed" else "Observations changed"
      df[[count_label]] <- unname(counts[df$variable])
      df <- df[, c("variable", count_label, setdiff(names(df), c("variable", count_label))), drop = FALSE]
      # Raw values in the data; display rounding lives in colFormat.
      .wise_diag_reactable(.policy_input_table_raw(df))
    })

    outputOptions(output, "diag_summary_table", suspendWhenHidden = FALSE)

    # UI-48: register Step 3's diagnostics for the export bundle.
    wise_export_table(
      key = "policy_transfer_summary",
      label = "Social protection transfer summary",
      step = 3L,
      fun = function() {
        d <- diag_data()
        if (is.null(d) || !is.null(d$status)) {
          return(NULL)
        }
        data.frame(
          Type = c(
            "Total transfer $ amount (population-level)",
            paste0(
              "Per-", unit_word(plural = FALSE, au = d$analysis_unit),
              " $ equivalent (eligible ",
              unit_word(plural = TRUE, au = d$analysis_unit), ")"
            )
          ),
          Value = fmt_num(c(d$transfer_sum, d$transfer_pp), prefix = "$"),
          stringsAsFactors = FALSE
        )
      },
      stale = stale,
      description = paste(
        "Population-level annual cost of the social protection transfer and",
        "the per-recipient equivalent."
      )
    )

    wise_export_table(
      key = "policy_input_diagnostics",
      label = "Policy input diagnostics",
      step = 3L,
      fun = function() {
        d <- diag_data()
        if (is.null(d) || !is.null(d$status)) {
          return(NULL)
        }
        df <- d$input_summary
        if (is.null(df) || nrow(df) == 0) {
          return(NULL)
        }
        counts <- d$changed_counts
        count_label <- if (identical(d$analysis_unit, "hh")) "Households changed" else "Observations changed"
        df[[count_label]] <- unname(counts[df$variable])
        df <- df[, c("variable", count_label, setdiff(names(df), c("variable", count_label))), drop = FALSE]
        .format_policy_input_table(df)
      },
      stale = stale,
      description = paste(
        "Before/after summary of every covariate the policy scenario changed,",
        "so the levers that actually moved can be checked."
      )
    )

    # Histogram plots container ----

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
          wise_chart_output(
            ns(paste0("hist_", var)),
            paste("Histogram of", var, "before and after the policy adjustment"),
            height = "300px"
          )
        )
      })

      do.call(shiny::tagList, tags)
    })

    # Per-variable histogram outputs ----

    observeEvent(diag_data(),
      {
        d <- diag_data()
        if (is.null(d) || is.list(d) && !is.null(d$status)) {
          return()
        }

        vars <- d$manipulated_vars
        wise_export_retain(
          "policy_before_after_",
          paste0("policy_before_after_", vars)
        )
        if (length(vars) == 0) {
          return()
        }

        for (var in vars) {
          local({
            var_name <- var
            baseline_vals <- d$baseline_values[[var_name]]
            policy_vals <- d$policy_values[[var_name]]
            # Zero-arg echarts closure shared by the on-screen render and the
            # export bundle (guidelines §7 pattern).
            hist_chart <- function() {
              echart_before_after_hist(
                baseline_vals, policy_vals, var_name,
                height = "300px"
              )
            }

            output[[paste0("hist_", var_name)]] <- echarts4r::renderEcharts4r({
              ch <- hist_chart()
              req(!is.null(ch))
              ch
            })
            wise_export_figure(
              key = paste0("policy_before_after_", var_name),
              label = paste("Policy-adjusted before/after", var_name),
              step = 3L,
              fun = hist_chart,
              description = paste(
                "Baseline and policy-adjusted distributions for the manipulated",
                "variable", var_name, "."
              ),
              width = 9, height = 5,
              stale = stale
            )
          })
        }
      },
      ignoreInit = TRUE
    )

    # Append Diagnostics tab on first successful run ----

    observeEvent(sim_run_id(),
      {
        req(sim_run_id() > 0)

        if (!diag_tab_added()) {
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Diagnostics",
              value = "diag_tab",
              shiny::div(id = ns("diagnostics_section"))
            ),
            select = FALSE,
            session = tabset_session
          )
          shiny::insertUI(
            selector = paste0("#", ns("diagnostics_section")),
            where = "afterBegin",
            ui = .diagnostics_content_ui(ns),
            session = session
          )
          diag_tab_added(TRUE)
        }
      },
      ignoreInit = TRUE
    )

    output$policy_summary_ui <- shiny::renderUI({
      policy_summary_card(
        selected_policies = selected_policies(),
        baseline_hist_sim = baseline_hist_sim(),
        selected_weather = selected_weather(),
        sp_scenario = sp_scenario(),
        infra_scenario = infra_scenario(),
        digital_scenario = digital_scenario(),
        labor_scenario = labor_scenario(),
        education_scenario = education_scenario(),
        policy_saved_scenarios = policy_saved_scenarios(),
        policy_scenarios = policy_scenarios()
      )
    })

    output$treatment_table <- reactable::renderReactable({
      d <- diag_data()
      req(d)
      .wise_diag_reactable(
        .policy_treatment_table_raw(d$treatment_matrix),
        formats = list(
          `Sample units` = list(digits = 0, separators = TRUE),
          `Population represented` = list(digits = 0, separators = TRUE),
          `Population share` = list(digits = 1, suffix = "%")
        )
      )
    })
    output$treatment_explanation_ui <- shiny::renderUI({
      shiny::tags$p(
        class = "diagnostic-note",
        .policy_treatment_explanation(sp_scenario())
      )
    })
    output$policy_component_table <- reactable::renderReactable({
      d <- diag_data()
      req(d)
      unit_col <- if (identical(d$analysis_unit, "hh")) {
        "Households affected / covered"
      } else {
        "Observations affected / covered"
      }
      .wise_diag_reactable(
        .policy_component_table_raw(d$component_matrix, d$analysis_unit),
        formats = list(
          unit_col = list(digits = 0, separators = TRUE),
          `Population represented` = list(digits = 0, separators = TRUE),
          `Population share` = list(digits = 1, suffix = "%"),
          `Realized cost` = list(digits = 0, prefix = "$", separators = TRUE)
        )
      )
    })
    wise_export_table(
      key = "policy_treatment_assignment",
      label = "Eligibility versus realized treatment assignment",
      step = 3L,
      fun = function() {
        d <- diag_data()
        if (is.null(d) || !is.data.frame(d$treatment_matrix)) {
          return(NULL)
        }
        .format_policy_treatment_table(d$treatment_matrix)
      },
      stale = stale,
      description = paste(
        "Weighted eligibility versus realized positive-transfer treatment.",
        "Eligibility is measured before inclusion and exclusion errors;",
        "realized treatment includes the selected targeting-error draw."
      )
    )
    wise_export_table(
      key = "policy_component_summary",
      label = "Policy component coverage summary",
      step = 3L,
      fun = function() {
        d <- diag_data()
        if (is.null(d) || !is.data.frame(d$component_matrix)) {
          return(NULL)
        }
        .format_policy_component_table(d$component_matrix, d$analysis_unit)
      },
      stale = stale,
      description = "Population affected or covered by social protection and other modeled policy components, including overlap."
    )
    invisible(NULL)
  })
}
