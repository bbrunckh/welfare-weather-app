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
    # ---- Program type — always visible -------------------------------
    uiOutput(ns("sp_type_ui")),

    # ---- Collapsible configuration panel -------------------------------
    uiOutput(ns("sp_trigger_ui")),
    uiOutput(ns("sp_budget_amount_ui")),
    uiOutput(ns("sp_targeting_ui")),
    uiOutput(ns("sp_timing_ui")),
    uiOutput(ns("sp_delivery_ui")),
    uiOutput(ns("sp_revenue_ui"))
  )
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
                                survey_weather   = reactive(NULL),
                                variable_list    = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- PMT variable candidates (numeric, non-missing for welfare rows) ----
    pmt_candidates <- reactive({
      svy <- survey_weather()
      vl  <- variable_list()
      so  <- selected_outcome()
      if (is.null(svy) || is.null(vl) || is.null(so) || nrow(vl) == 0) return(character(0))

      # Filter variable_list to ind/hh/firm/area level only
      level_mask <- vapply(seq_len(nrow(vl)), function(i) {
        isTRUE(vl$ind[i] == 1L) || isTRUE(vl$hh[i] == 1L) || isTRUE(vl$firm[i] == 1L) || isTRUE(vl$area[i] == 1L)
      }, logical(1))
      cands <- vl$name[level_mask]

      # Intersect with survey columns
      cands <- intersect(cands, names(svy))
      if (length(cands) == 0) return(character(0))

      # Keep only numeric vars with no NAs where outcome is non-missing
      outcome_rows <- !is.na(svy[[so$name]])
      keep <- Filter(function(v) {
        col <- svy[[v]]
        is.numeric(col) && !any(is.na(col[outcome_rows]))
      }, cands)

      # Return named character: display_label → variable_name
      setNames(keep, vapply(keep, function(v) {
        lbl <- vl$label[vl$name == v]
        if (length(lbl) > 0 && nzchar(lbl[[1]])) lbl[[1]] else v
      }, character(1)))
    })

    # ---- 1. program type ---------------------------------------------

    output$sp_type_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-hand-holding-dollar me-1"),
          "Program type"
        ),
        radioButtons(
          inputId  = ns("sp_type"),
          label    = NULL,
          choices  = c(
            # "Shock-responsive cash transfer" = "shock",
            "Regular cash transfer"          = "regular"
          ),
          selected = "regular"
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 2. Trigger (shock-responsive only) ----------------------------

    # output$sp_trigger_ui <- renderUI({
    #   req(input$sp_type == "shock")
    #   tagList(
    #     tags$label(
    #       class = "control-label",
    #       tags$i(class = "fa fa-triangle-exclamation me-1"),
    #       "Trigger type"
    #     ),
    #     selectInput(
    #       inputId  = ns("trigger_type"),
    #       label    = NULL,
    #       choices  = c(
    #         "Return period of event \u2265 x years"      = "return_period",
    #         "Weather variable exceeds x"                 = "weather_threshold",
    #         "Modelled welfare loss \u2265 $x"            = "welfare_loss",
    #         "Modelled increase in poverty gap \u2265 $x" = "poverty_increase"
    #       ),
    #       selected = "return_period"
    #     ),
    #     numericInput(
    #       inputId = ns("trigger_value"),
    #       label   = tags$span(
    #         tags$i(class = "fa fa-sliders me-1"),
    #         "Trigger value (x)"
    #       ),
    #       value = 10,
    #       min   = 0,
    #       step  = 1
    #     ),
    #     tags$hr(style = "margin: 8px 0;")
    #   )
    # })

    # ---- 3. Targeting --------------------------------------------------

    output$sp_targeting_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-crosshairs me-1"),
          "Targeting"
        ),
        selectInput(
          inputId  = ns("targeting"),
          label    = NULL,
          choices  = c(
            "Universal"                                 = "universal",
            "Welfare below threshold (ex-ante poor)"    = "exante_poor",
            "Household characteristics (PMT)"           = "pmt"
          ),
          selected = "universal"
        ),

        # Threshold for ex-ante poor targeting
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] == 'exante_poor'"),
          sliderInput(
            inputId = ns("targeting_threshold_pct"),
            label   = tags$span(
              tags$i(class = "fa fa-users me-1"),
              "Poorest (bottom x%)"
            ),
            min = 5, max = 60, value = 20, step = 5, post = "%"
          )
        ),

        # PMT variable and cutoff (only for PMT targeting)
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] == 'pmt'"),
          uiOutput(ns("pmt_variable_ui")),
          uiOutput(ns("pmt_cutoff_ui"))
        ),

        # Inclusion/exclusion errors (only for non-universal targeting)
        conditionalPanel(
          condition = paste0("input['", ns("targeting"), "'] != 'universal'"),
          sliderInput(
            inputId = ns("inclusion_error_pct"),
            label   = tags$span(
              tags$i(class = "fa fa-user-plus me-1"),
              "Inclusion error (%)"
            ),
            min = 0, max = 30, value = 10, step = 1, post = "%"
          ),
          sliderInput(
            inputId = ns("exclusion_error_pct"),
            label   = tags$span(
              tags$i(class = "fa fa-user-minus me-1"),
              "Exclusion error (%)"
            ),
            min = 0, max = 30, value = 10, step = 1, post = "%"
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- PMT variable selector ----
    output$pmt_variable_ui <- renderUI({
      cands <- pmt_candidates()
      if (length(cands) == 0) {
        return(div(
          class = "alert alert-warning",
          style = "font-size: 12px; padding: 8px; margin-bottom: 8px;",
          "No suitable PMT variable found. No covariates with non-missing",
          "values found (for selected outcome)."
        ))
      }
      selectInput(
        ns("pmt_variable"),
        label = tags$span(
          tags$i(class = "fa fa-list me-1"),
          "PMT variable"
        ),
        choices = cands,
        selected = cands[[1]]
      )
    })

    # ---- PMT cutoff selector (type depends on variable) ----
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
        radioButtons(
          ns("pmt_cutoff"),
          label = "Target households where variable equals",
          choices = c("0" = 0, "1" = 1),
          selected = 0,
          inline = TRUE
        )
      } else {
        # Continuous variable: choose threshold
        sliderInput(
          ns("pmt_cutoff"),
          label = "Include households with value \u2264",
          min   = floor(min(col)),
          max   = ceiling(max(col)),
          value = quantile(col, 0.2),
          step  = if (diff(range(col)) > 10) 1 else 0.1
        )
      }
    })

    # ---- 4. Budget / transfer amount (linked) --------------------------
    #
    # Two budget modes:
    #   "budget_first"   — user sets total budget (net of admin cost)
    #                      → transfer per HH = (budget * (1 - admin%)) / n_beneficiaries
    #   "transfer_first" — user sets transfer per HH
    #                      → total budget = (amount * n_beneficiaries) / (1 - admin%)
    #
    # Admin cost reduces the amount available for direct transfers in both modes.

    output$sp_budget_amount_ui <- renderUI({
      tagList(

        # -- Budget mode toggle ------------------------------------------
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-link me-1"),
          "Budget"
        ),
        radioButtons(
          inputId  = ns("budget_mode"),
          label    = NULL,
          choices  = c(

            "Set transfer per HH \u2192 derive total budget" = "transfer_first",
            "Set total budget \u2192 derive transfer per HH" = "budget_first"
          ),
          selected = "transfer_first"
        ),

        tags$hr(style = "margin: 4px 0;"),

        # -- Total budget (budget_first only) ----------------------------
        conditionalPanel(
          condition = paste0("input['", ns("budget_mode"), "'] == 'budget_first'"),
          tags$label(
            class = "control-label",
            tags$i(class = "fa fa-coins me-1"),
            "Total budget"
          ),
          # selectInput(
          #   inputId  = ns("budget_type"),
          #   label    = NULL,
          #   choices  = c(
          #     "Fixed amount"                                 = "fixed",
          #     "Share of modelled welfare loss (at trigger)"  = "welfare_share",
          #     "Proportional to modelled increase in poverty" = "poverty_prop",
          #     "Based on annual expected welfare loss"        = "annual_expected"
          #   ),
          #   selected = "fixed"
          # ),
          # conditionalPanel(
          #   condition = paste0("input['", ns("budget_type"), "'] == 'fixed'"),
            numericInput(
              inputId = ns("budget_fixed"),
              label   = tags$span(
                tags$i(class = "fa fa-dollar-sign me-1"),
                "Fixed budget (USD)"
              ),
              value = 1000000, min = 0, step = 100000
            ),
          # ),
          # conditionalPanel(
          #   condition = paste0(
          #     "input['", ns("budget_type"), "'] == 'welfare_share' || ",
          #     "input['", ns("budget_type"), "'] == 'poverty_prop'"
          #   ),
          #   sliderInput(
          #     inputId = ns("budget_share_pct"),
          #     label   = tags$span(
          #       tags$i(class = "fa fa-percent me-1"),
          #       "Share / proportion (%)"
          #     ),
          #     min = 1, max = 100, value = 50, step = 1, post = "%"
          #   )
          # )
        ),

        # -- Transfer amount ---------------------------------------------
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-money-bill-transfer me-1"),
          "Transfer amount"
        ),
        # selectInput(
        #   inputId  = ns("amount_type"),
        #   label    = NULL,
        #   choices  = c(
        #     "Equal across beneficiaries"             = "equal",
        #     "Varies by ex-ante welfare"              = "exante_welfare",
        #     "Varies by predicted welfare at trigger" = "predicted_welfare",
        #     "Varies by household characteristic"    = "hh_characteristic"
        #   ),
        #   selected = "equal"
        # ),
        conditionalPanel(
          condition = paste0(
            # "input['", ns("amount_type"), "'] == 'equal' && ",
            "input['", ns("budget_mode"), "'] == 'transfer_first'"
          ),
          numericInput(
            inputId = ns("transfer_amount_usd"),
            label   = tags$span(
              tags$i(class = "fa fa-dollar-sign me-1"),
              "Transfer per household ($)"
            ),
            value = 50, min = 0, step = 10
          )
        ),
        conditionalPanel(
          condition = paste0(
            # "input['", ns("amount_type"), "'] == 'equal' && ",
            "input['", ns("budget_mode"), "'] == 'budget_first'"
          ),
          tags$div(
            class = "alert alert-light p-2 mb-2",
            tags$small(
              tags$i(class = "fa fa-calculator me-1"),
              tags$strong("Derived: "),
              "transfer per HH = (total budget \u00d7 (1 \u2212 admin%)) \u00f7 beneficiaries"
            )
          )
        ),

        tags$hr(style = "margin: 4px 0;"),

        # -- Administration costs (reduces amount available) -------------
        # sliderInput(
        #   inputId = ns("admin_cost_pct"),
        #   label   = tags$span(
        #     tags$i(class = "fa fa-building me-1"),
        #     "Administration cost (% of total budget)"
        #   ),
        #   min = 0, max = 40, value = 10, step = 1, post = "%"
        # ),
        # tags$small(
        #   class = "text-muted d-block mb-2",
        #   tags$i(class = "fa fa-circle-info me-1"),
        #   "Admin cost is deducted from the total budget before computing transfer amounts."
        # ),

        # tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 5. Frequency and timing ---------------------------------------
    #
    # If sp_type == "regular":
    #   - hide one-off vs regular radio (always regular)
    #   - hide anticipatory vs ex-post (not applicable)
    #   - hide timeliness (not applicable)
    #   - show only number of payments

    output$sp_timing_ui <- renderUI({
      is_regular <- isTRUE(input$sp_type == "regular")
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-clock me-1"),
          "Frequency and timing"
        ),

        # One-off vs regular — hidden for regular programs
        # if (!is_regular) {
        #   radioButtons(
        #     inputId  = ns("transfer_frequency"),
        #     label    = tags$span(
        #       tags$i(class = "fa fa-rotate me-1"),
        #       "One-off vs regular"
        #     ),
        #     choices  = c("One-off" = "oneoff", "Regular" = "regular"),
        #     selected = isolate(input$transfer_frequency) %||% "oneoff",
        #     inline   = TRUE
        #   )
        # },

        # Number of payments — shown when regular (either via type or frequency)
        conditionalPanel(
          condition = paste0(
            "input['", ns("sp_type"), "'] == 'regular' || ",
            "input['", ns("transfer_frequency"), "'] == 'regular'"
          ),
          sliderInput(
            inputId = ns("transfer_n_payments"),
            label   = tags$span(
              tags$i(class = "fa fa-hashtag me-1"),
              "Number of payments per year"
            ),
            min = 2, max = 24, value = 6, step = 1
          )
        ),

        # Anticipatory vs ex-post — hidden for regular programs
        # if (!is_regular) {
        #   tagList(
        #     radioButtons(
        #       inputId  = ns("transfer_timing"),
        #       label    = tags$span(
        #         tags$i(class = "fa fa-calendar-check me-1"),
        #         "Anticipatory vs ex-post"
        #       ),
        #       choices  = c(
        #         "Anticipatory (pre-event)" = "anticipatory",
        #         "Ex-post (post-event)"     = "expost"
        #       ),
        #       selected = isolate(input$transfer_timing) %||% "expost",
        #       inline   = TRUE
        #     ),
        #     sliderInput(
        #       inputId = ns("timeliness_weeks"),
        #       label   = tags$span(
        #         tags$i(class = "fa fa-hourglass-half me-1"),
        #         "Timeliness (weeks after trigger)"
        #       ),
        #       min = 0, max = 26, value = 4, step = 1, post = " wks"
        #     )
        #   )
        # },

        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 6. Delivery system --------------------------------------------

    # output$sp_delivery_ui <- renderUI({
    #   tagList(
    #     tags$label(
    #       class = "control-label",
    #       tags$i(class = "fa fa-mobile-screen me-1"),
    #       "Delivery system"
    #     ),
    #     checkboxInput(
    #       inputId = ns("delivery_mobile_money"),
    #       label   = tagList(
    #         tags$i(class = "fa fa-wallet me-1"),
    #         "Mobile money (recipients need mobile wallet)"
    #       ),
    #       value = FALSE
    #     ),
    #     conditionalPanel(
    #       condition = paste0("input['", ns("delivery_mobile_money"), "']"),
    #       tags$small(
    #         class = "text-muted d-block mb-2",
    #         tags$i(class = "fa fa-circle-info me-1"),
    #         "Coverage will be constrained by mobile phone ownership rate in the simulation."
    #       )
    #     ),
    #     tags$hr(style = "margin: 8px 0;")
    #   )
    # })

    # ---- 7. Revenue source ---------------------------------------------

    # output$sp_revenue_ui <- renderUI({
    #   tagList(
    #     tags$label(
    #       class = "control-label",
    #       tags$i(class = "fa fa-money-bill-trend-up me-1"),
    #       "Revenue source"
    #     ),
    #     selectInput(
    #       inputId  = ns("revenue_source"),
    #       label    = NULL,
    #       choices  = c(
    #         "Government budget reallocation"         = "govt_reallocation",
    #         "Dedicated social protection budget"     = "sp_budget",
    #         "International aid / donor funding"      = "donor",
    #         "Contingency fund / reserve"             = "contingency",
    #         "Sovereign parametric insurance payout"  = "insurance",
    #         "Catastrophe bond trigger"               = "cat_bond",
    #         "Deficit spending / borrowing"           = "borrowing"
    #       ),
    #       selected = "govt_reallocation"
    #     ),
    #     tags$small(
    #       class = "text-muted d-block mb-2",
    #       tags$i(class = "fa fa-circle-info me-1"),
    #       "Revenue source affects fiscal cost interpretation in the simulation output."
    #     ),
    #     tags$hr(style = "margin: 8px 0;")
    #   )
    # })

    # ---- Return API ----------------------------------------------------

    list(
      sp_scenario = reactive({
        is_regular <- isTRUE(input$sp_type == "regular")
        list(
          # program type
          sp_type               = input$sp_type                 %||% "shock",
          # Trigger (shock only)
          # trigger_type          = input$trigger_type            %||% "return_period",
          # trigger_value         = input$trigger_value           %||% 10,
          # Budget mode
          budget_mode           = input$budget_mode             %||% "transfer_first",
          # budget_type           = input$budget_type             %||% "fixed",
          budget_fixed          = input$budget_fixed            %||% 1000000,
          # budget_share_pct      = input$budget_share_pct        %||% 50,
          # Targeting
          targeting             = input$targeting               %||% "exante_poor",
          targeting_threshold   = input$targeting_threshold_pct %||% 20,
          pmt_variable          = input$pmt_variable            %||% NA_character_,
          pmt_cutoff            = input$pmt_cutoff              %||% NA_real_,
          inclusion_error_pct   = input$inclusion_error_pct     %||% 10,
          exclusion_error_pct   = input$exclusion_error_pct     %||% 10,
          # Transfer amount
          # amount_type           = input$amount_type             %||% "equal",
          transfer_amount_usd   = input$transfer_amount_usd     %||% 0,
          # Admin cost (deducted from budget before transfer calculation)
          # admin_cost_pct        = input$admin_cost_pct          %||% 10,
          # Timing — regular programs always have n payments
          transfer_frequency =
            if (is_regular) "regular"
            else input$transfer_frequency %||% "oneoff",
          transfer_n_payments =
            if (is_regular) input$transfer_n_payments %||% 6L
            else input$transfer_n_payments %||% 1L,
          transfer_timing =
            if (is_regular) NA_character_
            else input$transfer_timing %||% "expost",
          timeliness_weeks =
            if (is_regular) NA_integer_
            else input$timeliness_weeks %||% 4L
          # Delivery
          # delivery_mobile_money = isTRUE(input$delivery_mobile_money),
          # Revenue source
          # revenue_source = input$revenue_source %||% "govt_reallocation"
        )
      })
    )

  })
}
