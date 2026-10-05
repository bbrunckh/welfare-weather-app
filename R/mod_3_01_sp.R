#' 3_01_sp UI Function
#'
#' @description A shiny Module. Social protection scenario configuration.
#'   Allows the user to define a cash transfer program (shock-responsive
#'   or regular) with budget, targeting, transfer amount, timing and
#'   delivery parameters to be applied in the policy simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_01_sp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI-32: live reach/cost summary, matching the Step 1 weather/model cards.
    uiOutput(ns("sp_reach_ui")),

    # Program type - always visible ----
    uiOutput(ns("sp_type_ui")),

    # Collapsible configuration panel ----
    uiOutput(ns("sp_budget_amount_ui")),
    uiOutput(ns("sp_targeting_ui")),
    uiOutput(ns("sp_timing_ui"))
  )
}

# Patch an ionRangeSlider grid so major tick labels land exactly on the
# slider's step values. shiny's auto-grid (grid_num = n_steps /
# ceiling(n_steps / 10)) produces a fractional grid count for > 10 steps,
# which renders tick labels at irregular, off-step positions (e.g. a
# 5-60% slider showing a label at 54.5%). An explicit grid_num of the
# intended interval count keeps them evenly spaced and meaningful.
with_grid_num <- function(slider_tag, n) {
  for (i in seq_along(slider_tag$children)) {
    ch <- slider_tag$children[[i]]
    if (is.list(ch) && !is.null(ch$attribs) &&
      "data-grid-num" %in% names(ch$attribs)) {
      ch$attribs[["data-grid-num"]] <- n
      slider_tag$children[[i]] <- ch
      break
    }
  }
  slider_tag
}

#' 3_01_sp Server Functions
#'
#' @param id Module id.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from \code{mod_1_modelling_server()}.
#' @param survey_weather Reactive data frame of merged survey-weather data
#'   from \code{mod_1_modelling_server()}.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{sp_scenario}{Named list of social protection scenario parameters.}
#'   }
#'
#' @noRd
mod_3_01_sp_server <- function(id,
                               selected_outcome = reactive(NULL),
                               survey_weather = reactive(NULL),
                               variable_list = reactive(NULL),
                               analysis_unit = reactive("hh"),
                               hist_sim = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: human-readable unit word ("individual" / "individuals" /
    # "Household" / "Households" / "Firm" / "Firms") driven by the user's
    # selection in mod_1_01_sample (`input$unit`).
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

    # Proxy variable candidates (numeric, non-missing for welfare rows) ----
    pmt_candidates <- reactive({
      svy <- survey_weather()
      vl <- variable_list()
      so <- selected_outcome()
      if (is.null(svy) || is.null(vl) || is.null(so) || nrow(vl) == 0) {
        return(character(0))
      }

      # Filter variable_list to ind/hh/firm/area level only
      level_mask <- vapply(seq_len(nrow(vl)), function(i) {
        isTRUE(vl$ind[i] == 1L) || isTRUE(vl$hh[i] == 1L) || isTRUE(vl$firm[i] == 1L) || isTRUE(vl$area[i] == 1L)
      }, logical(1))
      cands <- vl$name[level_mask]

      # Intersect with survey columns
      cands <- intersect(cands, names(svy))
      if (length(cands) == 0) {
        return(character(0))
      }

      # Keep only numeric vars with no NAs where outcome is non-missing
      outcome_rows <- !is.na(svy[[so$name]])
      keep <- Filter(function(v) {
        col <- svy[[v]]
        is.numeric(col) && !any(is.na(col[outcome_rows]))
      }, cands)

      # Return named character: display_label -> variable_name
      setNames(keep, vapply(keep, function(v) {
        lbl <- vl$label[vl$name == v]
        if (length(lbl) > 0 && nzchar(lbl[[1]])) lbl[[1]] else v
      }, character(1)))
    })

    # 1. program type ----

    output$sp_type_ui <- renderUI({
      tagList(
        tags$div(
          class = "sp-program-label",
          tags$span(
            class = "sp-inline-label",
            tags$i(class = "fa fa-hand-holding-dollar me-1"),
            "Program"
          )
        ),
        tags$div(
          class = "sp-program-choice",
          pill_toggle(
            inputId = ns("sp_type"),
            label = NULL,
            choices = c(
              "Regular"          = "regular",
              "Shock-responsive" = "shock"
            ),
            selected = "regular"
          ),
          tags$span(class = "sp-program-suffix", "cash transfer")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("sp_type"), "'] == 'shock'"),
          tags$div(
            class = "alert alert-warning py-1 px-2 mb-2",
            tags$small(
              tags$i(class = "fa fa-circle-info me-1"),
              "Shock-responsive cash transfers are not yet implemented. This",
              " selection is for display only and will not change the simulation."
            )
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # 2. Trigger (shock-responsive only) ----
    #
    # Shock-responsive trigger and timing UIs were prototyped but are not
    # implemented; see the shock note in section 1. Commented stubs removed.

    # 3. Targeting ----

    output$sp_targeting_ui <- renderUI({
      tags$div(
        class = "sp-targeting",
        tags$div(
          class = "sp-targeting-label",
          tags$span(
            class = "sp-inline-label",
            tags$i(class = "fa fa-crosshairs me-1"),
            "Targeting"
          )
        ),
        tags$div(
          class = "sp-targeting-select",
          selectInput(
            inputId = ns("targeting"),
            label = NULL,
            choices = stats::setNames(
              c("universal", "exante_poor", "pmt"),
              c(
                "Universal",
                "Welfare below threshold (ex-ante poor)",
                paste(
                  unit_word(plural = FALSE, capitalize = TRUE),
                  "characteristics (Proxy)"
                )
              )
            ),
            selected = "universal"
          )
        ),

        # Threshold for ex-ante poor targeting
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] == 'exante_poor'"),
          sliderInput(
            inputId = ns("targeting_threshold_pct"),
            label = tags$span(
              tags$i(class = "fa fa-users me-1"),
              "Poorest (bottom x%)"
            ),
            min = 5, max = 60, value = 20, step = 5, post = "%"
          ) |>
            # 11 intervals -> majors at 5, 10, ..., 60
            with_grid_num(11)
        ),

        # Proxy variable and cutoff (only for Proxy targeting)
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] == 'pmt'"),
          uiOutput(ns("pmt_variable_ui")),
          uiOutput(ns("pmt_cutoff_ui"))
        ),

        # Inclusion/exclusion errors (only for non-universal targeting)
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] != 'universal'"),
          tags$details(
            tags$summary("Errors"),
            sliderInput(
              inputId = ns("inclusion_error_pct"),
              label = tags$div(
                tags$span(
                  tags$i(class = "fa fa-user-plus me-1"),
                  "Inclusion error (%)"
                ),
                tags$div(
                  class = "text-muted",
                  style = "font-size: 0.8em; font-weight: normal;",
                  paste0("Non-eligible ", unit_word(plural = TRUE), " included")
                )
              ),
              min = 0, max = 30, value = 10, step = 5, post = "%"
            ),
            sliderInput(
              inputId = ns("exclusion_error_pct"),
              label = tags$div(
                tags$span(
                  tags$i(class = "fa fa-user-minus me-1"),
                  "Exclusion error (%)"
                ),
                tags$div(
                  class = "text-muted",
                  style = "font-size: 0.8em; font-weight: normal;",
                  paste0("Eligible ", unit_word(plural = TRUE), " excluded")
                )
              ),
              min = 0, max = 30, value = 10, step = 5, post = "%"
            )
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # Proxy variable selector ----
    output$pmt_variable_ui <- renderUI({
      cands <- pmt_candidates()
      if (length(cands) == 0) {
        return(no_data_warning(
          "No suitable Proxy variable found. No covariates with",
          "non-missing values found (for selected outcome)."
        ))
      }
      selectInput(
        ns("pmt_variable"),
        label = tags$span(
          tags$i(class = "fa fa-list me-1"),
          "Proxy variable"
        ),
        choices = cands,
        selected = cands[[1]]
      )
    })

    # Proxy cutoff selector (type depends on variable) ----
    output$pmt_cutoff_ui <- renderUI({
      v <- input$pmt_variable
      req(v)
      svy <- survey_weather()
      req(svy, v %in% names(svy))

      col <- svy[[v]]
      col <- col[!is.na(col)]
      uniq <- sort(unique(col))

      if (length(uniq) == 2 && all(uniq %in% c(0, 1))) {
        # Binary variable: choose target value
        pill_toggle(
          ns("pmt_cutoff"),
          label = paste(
            "Target", unit_word(plural = TRUE),
            "where variable equals"
          ),
          choices = c("0" = 0, "1" = 1),
          selected = 0
        )
      } else {
        # Continuous variable: choose threshold
        sliderInput(
          ns("pmt_cutoff"),
          label = paste(
            "Include", unit_word(plural = TRUE),
            "with value \u2264"
          ),
          min = floor(min(col)),
          max = ceiling(max(col)),
          value = stats::quantile(col, 0.2),
          step = if (diff(range(col)) > 10) 1 else 0.1
        )
      }
    })

    # 4. Budget / transfer amount (linked) ----
    #
    # Two budget modes:
    #   "budget_first"   - user sets total budget (net of admin cost)
    #                      -> transfer per HH = (budget * (1 - admin%)) / n_beneficiaries
    #   "transfer_first" - user sets transfer per HH
    #                      -> total budget = (amount * n_beneficiaries) / (1 - admin%)
    #
    # Admin cost reduces the amount available for direct transfers in both modes.

    output$sp_budget_amount_ui <- renderUI({
      currency <- tryCatch(
        {
          so <- selected_outcome()
          if (is.null(so) || nrow(so) == 0 || !"units" %in% names(so)) {
            "PPP"
          } else {
            as.character(so$units[[1]])
          }
        },
        error = function(e) "PPP"
      )
      if (is.na(currency) || !nzchar(currency)) currency <- "PPP"

      tagList(
        # Amount and budget mode ----
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-money-bill-transfer me-1"),
          "Amount",
          info_popover(
            title = "Amount and budget mode",
            tags$p(
              tags$b(paste("$ per", unit_word(plural = FALSE))),
              "sets the transfer paid to each recipient", unit_word(plural = FALSE),
              "per payment. The annual cost then depends on the number of recipients",
              "and payments per year."
            ),
            tags$p(
              tags$b("Total budget"),
              "sets the total annual amount available. The transfer per recipient is",
              "derived from the configured budget and recipient population."
            )
          )
        ),
        tags$div(
          class = "sp-budget-mode",
          pill_toggle(
            inputId = ns("budget_mode"),
            label = NULL,
            choices = stats::setNames(
              c("transfer_first", "budget_first"),
              c(paste("$ per", unit_word(plural = FALSE)), "Total budget")
            ),
            selected = "transfer_first"
          )
        ),

        # Amount entry ----
        conditionalPanel(
          condition = paste0("input['", ns("budget_mode"), "'] == 'budget_first'"),
          tags$div(
            class = "sp-inline-control",
            tags$label(
              class = "sp-inline-label",
              `for` = ns("budget_fixed"),
              tags$i(class = "fa fa-dollar-sign me-1"),
              paste0("Amount (", currency, ")")
            ),
            numericInput(
              inputId = ns("budget_fixed"),
              label = NULL,
              value = 0, min = 0, step = 100000
            )
          )
        ),
        conditionalPanel(
          condition = paste0(
            "input['", ns("budget_mode"), "'] == 'transfer_first'"
          ),
          tags$div(
            class = "sp-inline-control",
            tags$label(
              class = "sp-inline-label",
              `for` = ns("transfer_amount_usd"),
              tags$i(class = "fa fa-dollar-sign me-1"),
              paste0("Amount (", currency, ")")
            ),
            numericInput(
              inputId = ns("transfer_amount_usd"),
              label = NULL,
              value = 0, min = 0, step = 10
            )
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # 5. Frequency and timing ----
    #
    # If sp_type == "regular":
    #   - hide one-off vs regular radio (always regular)
    #   - hide anticipatory vs ex-post (not applicable)
    #   - hide timeliness (not applicable)
    #   - show only number of payments

    output$sp_timing_ui <- renderUI({
      conditionalPanel(
        condition = paste0(
          "input['", ns("budget_mode"), "'] == 'transfer_first'"
        ),
        tagList(
          tags$label(
            class = "control-label",
            tags$i(class = "fa fa-clock me-1"),
            "Frequency and timing",
            info_popover(
              title = "Transfers per year",
              tags$p(
                "The transfer amount above is per payment. Annual support =",
                "payment amount \u00d7 transfers per year, which the simulation",
                "converts to a daily equivalent added to daily welfare.",
                "More transfers per year means a larger annual transfer for the",
                "same per-payment amount."
              )
            )
          ),

          # Number of payments - shown when regular (either via type or frequency)
          tags$div(
            class = "sp-inline-slider",
            tags$span(
              class = "sp-inline-label",
              tags$i(class = "fa fa-hashtag me-1"),
              "Transfers per year"
            ),
            with_grid_num(
              sliderInput(
                inputId = ns("transfer_n_payments"),
                label = NULL,
                min = 2, max = 24, value = 6, step = 1
              ),
              11
            )
          )
        )
      )
    })

    # Scenario specification ----
    #
    # UI-43: the whole module sits in a bslib accordion panel that starts
    # collapsed, and hidden outputs are suspended - so none of these inputs
    # existed until the user expanded "Social protection", and the scenario
    # below silently fell back to the `%||%` defaults (including sp_type
    # "shock", which is no longer one of the offered choices). Render them
    # eagerly so an untouched panel still reports its real defaults.
    lapply(
      c(
        "sp_type_ui", "sp_budget_amount_ui", "sp_targeting_ui",
        "pmt_variable_ui", "pmt_cutoff_ui", "sp_timing_ui"
      ),
      function(out_id) {
        shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
      }
    )

    # One definition of the scenario, read by both the reach preview below and
    # the module's return API - the preview cannot drift from what is run.
    sp_scenario_spec <- reactive({
      # Shock-responsive transfers are displayed in the selector but are not
      # implemented yet. Keep the returned scenario on the regular path until
      # trigger and timing logic is wired through the policy simulation.
      sp_type_val <- if (identical(input$sp_type, "shock")) {
        "regular"
      } else {
        input$sp_type %||% "regular"
      }
      is_regular <- TRUE
      list(
        # program type
        sp_type = sp_type_val,
        # Budget mode
        budget_mode = input$budget_mode %||% "transfer_first",
        # Default to 0 (not 1,000,000) so an un-touched budget can never
        # accidentally apply a million-USD transfer if budget-first mode is
        # selected before a budget is entered.
        budget_fixed = input$budget_fixed %||% 0,
        # Targeting
        targeting = input$targeting %||% "exante_poor",
        targeting_threshold = input$targeting_threshold_pct %||% 20,
        pmt_variable = input$pmt_variable %||% NA_character_,
        pmt_cutoff = input$pmt_cutoff %||% NA_real_,
        inclusion_error_pct = input$inclusion_error_pct %||% 10,
        exclusion_error_pct = input$exclusion_error_pct %||% 10,
        # Transfer amount
        transfer_amount_usd = input$transfer_amount_usd %||% 0,
        # Timing - regular programs always have n payments
        transfer_frequency =
          if (is_regular) {
            "regular"
          } else {
            input$transfer_frequency %||% "oneoff"
          },
        transfer_n_payments =
          if (is_regular) {
            input$transfer_n_payments %||% 6L
          } else {
            input$transfer_n_payments %||% 1L
          },
        transfer_timing =
          if (is_regular) {
            NA_character_
          } else {
            input$transfer_timing %||% "expost"
          },
        timeliness_weeks =
          if (is_regular) {
            NA_integer_
          } else {
            input$timeliness_weeks %||% 4L
          }
      )
    })

    # Live reach / cost preview (UI-32) ----
    #
    # Answers "who does this reach and what does it cost?" while the user is
    # still choosing a cutoff, instead of only after a full policy run. The
    # figures come from `.sp_scenario_reach()`, which reuses the run's own
    # eligibility function under the run's analysis seed and the diagnostics
    # tab's cost arithmetic - so this is the number the simulation will
    # produce, not a separate approximation of it.

    sp_preview_inputs <- shiny::debounce(reactive({
      hs <- tryCatch(hist_sim(), error = function(e) NULL)
      svy <- if (!is.null(hs$svy)) {
        hs$svy
      } else {
        tryCatch(survey_weather(), error = function(e) NULL)
      }
      list(
        hs = hs, svy = svy, spec = sp_scenario_spec(),
        analysis_unit = tryCatch(analysis_unit(), error = function(e) "hh"),
        display_type = input$sp_type %||% "regular"
      )
    }), 250)

    sp_reach <- reactive({
      # The frame must be the one the policy run will use. Step 2 may have
      # filtered survey_weather() down to a single baseline round; estimating
      # over every round instead inflates both the eligible population and the
      # cost, and shifts the welfare quantile that defines "ex-ante poor".
      # This mirrors `svy <- hs$svy %||% survey_weather()` in mod_3_06.
      preview <- sp_preview_inputs()
      hs <- preview$hs
      svy <- preview$svy
      if (is.null(svy)) {
        return(NULL)
      }
      r <- .sp_scenario_reach(
        svy           = svy,
        sp            = preview$spec,
        analysis_unit = preview$analysis_unit,
        seed          = wise_current_seed()
      )
      if (is.null(r)) {
        return(NULL)
      }
      # Record which frame these figures describe. Before Step 2 has run there
      # is no baseline round to filter to, so the preview covers every survey
      # round and will drop once Step 2 narrows it - worth saying, rather than
      # letting the number appear to change on its own.
      r$on_baseline <- !is.null(hs$svy)
      r
    })

    output$sp_reach_ui <- renderUI({
      preview <- sp_preview_inputs()
      r <- sp_reach()
      preview_spec <- preview$spec
      preview_unit <- preview$analysis_unit
      unit_pl <- unit_word(plural = TRUE, au = preview_unit)
      unit_sg <- unit_word(plural = FALSE, au = preview_unit)
      program_label <- if (identical(preview$display_type, "shock")) {
        "Shock-responsive"
      } else {
        "Regular"
      }
      program_title <- tags$span(
        class = "sp-summary-program",
        tags$span(class = "selection-card-badge", program_label),
        tags$span(class = "sp-summary-program-suffix", "cash transfer")
      )

      if (is.null(r)) {
        return(selection_summary_card(
          title = program_title,
          rows = list(list(
            name = paste("Recipient", unit_pl),
            pills = "Not available"
          )),
          compact = TRUE
        ))
      }

      # Report household recipients separately from their represented people.
      count_hint <- if (isTRUE(r$weighted)) {
        paste0(
          "Survey-weighted; ", fmt_count(r$n_rows), " of ",
          fmt_count(r$n_total), " sampled ", unit_pl
        )
      } else {
        paste0(
          "Unweighted sample count; ", fmt_count(r$n_rows), " of ",
          fmt_count(r$n_total), " sampled ", unit_pl
        )
      }

      cost_label <- "Estimated annual cost"

      targeting <- preview_spec$targeting
      targeting_info <- switch(targeting,
        exante_poor = paste(
          "The bottom", preview_spec$targeting_threshold,
          "% is selected by sampled", unit_pl, "before inclusion and exclusion",
          "errors. The reached share is survey-weighted, so it can differ from",
          "the cutoff when sampled", unit_pl, "represent different population",
          "sizes. Household size affects transfer cost, not this percentage."
        ),
        pmt = paste(
          "Eligibility follows the selected Proxy variable and cutoff. The reached",
          "share is survey-weighted; inclusion and exclusion errors are applied",
          "after the Proxy rule."
        ),
        `default` = paste(
          "Universal targeting reaches all sampled units; survey weights determine",
          "the population count and share."
        )
      )

      selection_summary_card(
        title = program_title,
        rows = list(
          selection_card_row(
            name  = paste("Recipient", unit_pl),
            pills = fmt_count(r$n_recipient_units)
          ),
          selection_card_row(
            name  = if (identical(analysis_unit(), "hh")) {
              "People represented"
            } else {
              "Population represented"
            },
            pills = fmt_count(r$n_pop)
          ),
          selection_card_row(
            name  = "Share of total population",
            pills = fmt_num(r$share_pct, suffix = "%")
          ),
          selection_card_row(
            name  = paste("Annual transfer per", unit_sg),
            pills = fmt_num(r$transfer_per_unit, digits = 2, prefix = "$")
          ),
          selection_card_row(
            name  = cost_label,
            pills = fmt_num(r$transfer_total, digits = 0, prefix = "$")
          )
        ),
        info = paste(
          count_hint,
          targeting_info,
          if (isTRUE(r$transfer_total <= 0)) {
            "No transfer is configured yet, so welfare would remain unchanged."
          } else {
            ""
          },
          if (!isTRUE(r$on_baseline)) {
            "Before Step 2 runs, this preview covers every survey round; after the run it uses the baseline round used by the simulation."
          } else {
            ""
          }
        ),
        compact = TRUE
      )
    })

    # Rendered eagerly for the same reason as the panels above (UI-43).
    shiny::outputOptions(output, "sp_reach_ui", suspendWhenHidden = FALSE)

    # Return API ----

    list(
      sp_scenario = sp_scenario_spec,
      sp_preview_inputs = sp_preview_inputs,
      sp_reach = sp_reach
    )
  })
}
