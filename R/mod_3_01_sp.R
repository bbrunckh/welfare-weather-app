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

    # ---- Configure button — toggles the rest ---------------------------
    actionButton(
      inputId = ns("sp_config_toggle"),
      label   = tagList(
        tags$i(class = "fa fa-sliders me-1"),
        "Configure SP"
      ),
      class = "btn btn-outline-secondary btn-sm mb-2 w-100"
    ),

    # ---- Collapsible configuration panel -------------------------------
    # conditionalPanel keyed on click count: odd = open, even = closed
    conditionalPanel(
      condition = "input.sp_config_toggle % 2 == 1",
      ns        = ns,
      uiOutput(ns("sp_trigger_ui")),
      uiOutput(ns("sp_budget_amount_ui")),
      uiOutput(ns("sp_targeting_ui")),
      uiOutput(ns("sp_timing_ui")),
      uiOutput(ns("sp_delivery_ui")),
      uiOutput(ns("sp_revenue_ui"))
    )
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
#'     \item{selected_hist}{Reactive character — selected historical period.}
#'   }
#'
#' @noRd
mod_3_01_sp_server <- function(id,
                                selected_outcome = reactive(NULL),
                                survey_weather   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Toggle config panel — update button label ---------------------
    observeEvent(input$sp_config_toggle, {
      is_open <- input$sp_config_toggle %% 2 == 1
      label <- if (is_open) {
        tagList(tags$i(class = "fa fa-chevron-up me-1"),  "Hide configuration")
      } else {
        tagList(tags$i(class = "fa fa-sliders me-1"),     "Configure program")
      }
      updateActionButton(session, "sp_config_toggle", label = label)
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
            "Shock-responsive cash transfer" = "shock",
            "Regular cash transfer"          = "regular"
          ),
          selected = "shock"
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 2. Trigger (shock-responsive only) ----------------------------

    output$sp_trigger_ui <- renderUI({
      req(input$sp_type == "shock")
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-triangle-exclamation me-1"),
          "Trigger type"
        ),
        selectInput(
          inputId  = ns("trigger_type"),
          label    = NULL,
          choices  = c(
            "Return period of event \u2265 x years"      = "return_period",
            "Weather variable exceeds x"          = "weather_threshold",
            "Modelled welfare loss \u2265 $x"            = "welfare_loss",
            "Modelled increase in poverty gap \u2265 $x" = "poverty_increase"
          ),
          selected = "return_period"
        ),
        numericInput(
          inputId = ns("trigger_value"),
          label   = tags$span(
            tags$i(class = "fa fa-sliders me-1"),
            "Trigger value (x)"
          ),
          value = 10,
          min   = 0,
          step  = 1
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

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
            "Universal"                                       = "universal",
            "Welfare below threshold (ex-ante poor)"         = "exante_poor",
            "Predicted welfare below threshold (at trigger)" = "predicted_poor",
            "Household characteristics (PMT)"                = "pmt",
            "Geographic \u2014 worst affected regions"       = "geographic"
          ),
          selected = "exante_poor"
        ),
        conditionalPanel(
          condition = paste0(
            "input['", ns("targeting"), "'] == 'exante_poor' || ",
            "input['", ns("targeting"), "'] == 'predicted_poor'"
          ),
          sliderInput(
            inputId = ns("targeting_threshold_pct"),
            label   = tags$span(
              tags$i(class = "fa fa-users me-1"),
              "Poorest (bottom x%)"
            ),
            min = 5, max = 60, value = 20, step = 5, post = "%"
          )
        ),
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
        ),
        tags$hr(style = "margin: 8px 0;")
      )
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
          "Budget mode"
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
          selectInput(
            inputId  = ns("budget_type"),
            label    = NULL,
            choices  = c(
              "Fixed amount"                                  = "fixed",
              "Share of modelled welfare loss (at trigger)"  = "welfare_share",
              "Proportional to modelled increase in poverty" = "poverty_prop",
              "Based on annual expected welfare loss"        = "annual_expected"
            ),
            selected = "fixed"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("budget_type"), "'] == 'fixed'"),
            numericInput(
              inputId = ns("budget_fixed"),
              label   = tags$span(
                tags$i(class = "fa fa-dollar-sign me-1"),
                "Fixed budget (USD)"
              ),
              value = 1000000, min = 0, step = 100000
            )
          ),
          conditionalPanel(
            condition = paste0(
              "input['", ns("budget_type"), "'] == 'welfare_share' || ",
              "input['", ns("budget_type"), "'] == 'poverty_prop'"
            ),
            sliderInput(
              inputId = ns("budget_share_pct"),
              label   = tags$span(
                tags$i(class = "fa fa-percent me-1"),
                "Share / proportion (%)"
              ),
              min = 1, max = 100, value = 50, step = 1, post = "%"
            )
          )
        ),

        # -- Transfer amount ---------------------------------------------
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-money-bill-transfer me-1"),
          "Transfer amount"
        ),
        selectInput(
          inputId  = ns("amount_type"),
          label    = NULL,
          choices  = c(
            "Equal across beneficiaries"             = "equal",
            "Varies by ex-ante welfare"              = "exante_welfare",
            "Varies by predicted welfare at trigger" = "predicted_welfare",
            "Varies by household characteristic"    = "hh_characteristic"
          ),
          selected = "equal"
        ),
        conditionalPanel(
          condition = paste0(
            "input['", ns("amount_type"), "'] == 'equal' && ",
            "input['", ns("budget_mode"), "'] == 'transfer_first'"
          ),
          numericInput(
            inputId = ns("transfer_amount_usd"),
            label   = tags$span(
              tags$i(class = "fa fa-dollar-sign me-1"),
              "Transfer per household (USD)"
            ),
            value = 50, min = 0, step = 10
          )
        ),
        conditionalPanel(
          condition = paste0(
            "input['", ns("amount_type"), "'] == 'equal' && ",
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
        sliderInput(
          inputId = ns("admin_cost_pct"),
          label   = tags$span(
            tags$i(class = "fa fa-building me-1"),
            "Administration cost (% of total budget)"
          ),
          min = 0, max = 40, value = 10, step = 1, post = "%"
        ),
        tags$small(
          class = "text-muted d-block mb-2",
          tags$i(class = "fa fa-circle-info me-1"),
          "Admin cost is deducted from the total budget before computing transfer amounts."
        ),

        tags$hr(style = "margin: 8px 0;")
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
        if (!is_regular) {
          radioButtons(
            inputId  = ns("transfer_frequency"),
            label    = tags$span(
              tags$i(class = "fa fa-rotate me-1"),
              "One-off vs regular"
            ),
            choices  = c("One-off" = "oneoff", "Regular" = "regular"),
            selected = isolate(input$transfer_frequency) %||% "oneoff",
            inline   = TRUE
          )
        },

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
        if (!is_regular) {
          tagList(
            radioButtons(
              inputId  = ns("transfer_timing"),
              label    = tags$span(
                tags$i(class = "fa fa-calendar-check me-1"),
                "Anticipatory vs ex-post"
              ),
              choices  = c(
                "Anticipatory (pre-event)" = "anticipatory",
                "Ex-post (post-event)"     = "expost"
              ),
              selected = isolate(input$transfer_timing) %||% "expost",
              inline   = TRUE
            ),
            sliderInput(
              inputId = ns("timeliness_weeks"),
              label   = tags$span(
                tags$i(class = "fa fa-hourglass-half me-1"),
                "Timeliness (weeks after trigger)"
              ),
              min = 0, max = 26, value = 4, step = 1, post = " wks"
            )
          )
        },

        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 6. Delivery system --------------------------------------------

    output$sp_delivery_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-mobile-screen me-1"),
          "Delivery system"
        ),
        checkboxInput(
          inputId = ns("delivery_mobile_money"),
          label   = tagList(
            tags$i(class = "fa fa-wallet me-1"),
            "Mobile money (recipients need mobile wallet)"
          ),
          value = FALSE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("delivery_mobile_money"), "']"),
          tags$small(
            class = "text-muted d-block mb-2",
            tags$i(class = "fa fa-circle-info me-1"),
            "Coverage will be constrained by mobile phone ownership rate in the simulation."
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- 7. Revenue source ---------------------------------------------

    output$sp_revenue_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-money-bill-trend-up me-1"),
          "Revenue source"
        ),
        selectInput(
          inputId  = ns("revenue_source"),
          label    = NULL,
          choices  = c(
            "Government budget reallocation"         = "govt_reallocation",
            "Dedicated social protection budget"     = "sp_budget",
            "International aid / donor funding"      = "donor",
            "Contingency fund / reserve"             = "contingency",
            "Sovereign parametric insurance payout"  = "insurance",
            "Catastrophe bond trigger"               = "cat_bond",
            "Deficit spending / borrowing"           = "borrowing"
          ),
          selected = "govt_reallocation"
        ),
        tags$small(
          class = "text-muted d-block mb-2",
          tags$i(class = "fa fa-circle-info me-1"),
          "Revenue source affects fiscal cost interpretation in the simulation output."
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- Return API ----------------------------------------------------

    list(
      sp_scenario = reactive({
        is_regular <- isTRUE(input$sp_type == "regular")
        list(
          # program type
          sp_type               = input$sp_type                 %||% "shock",
          # Trigger (shock only)
          trigger_type          = input$trigger_type            %||% "return_period",
          trigger_value         = input$trigger_value           %||% 10,
          # Budget mode
          budget_mode           = input$budget_mode             %||% "transfer_first",
          budget_type           = input$budget_type             %||% "fixed",
          budget_fixed          = input$budget_fixed            %||% 1000000,
          budget_share_pct      = input$budget_share_pct        %||% 50,
          # Targeting
          targeting             = input$targeting               %||% "exante_poor",
          targeting_threshold   = input$targeting_threshold_pct %||% 20,
          inclusion_error_pct   = input$inclusion_error_pct     %||% 10,
          exclusion_error_pct   = input$exclusion_error_pct     %||% 10,
          # Transfer amount
          amount_type           = input$amount_type             %||% "equal",
          transfer_amount_usd   = input$transfer_amount_usd     %||% 50,
          # Admin cost (deducted from budget before transfer calculation)
          admin_cost_pct        = input$admin_cost_pct          %||% 10,
          # Timing — regular programs always have n payments, no timing type
          transfer_frequency    = if (is_regular) "regular" else input$transfer_frequency %||% "oneoff",
          transfer_n_payments   = if (is_regular) input$transfer_n_payments %||% 12L else input$transfer_n_payments %||% 1L,
          transfer_timing       = if (is_regular) NA_character_ else input$transfer_timing %||% "expost",
          timeliness_weeks      = if (is_regular) NA_integer_   else input$timeliness_weeks %||% 4L,
          # Delivery
          delivery_mobile_money = isTRUE(input$delivery_mobile_money),
          # Revenue source
          revenue_source        = input$revenue_source          %||% "govt_reallocation"
        )
      }),
      selected_hist = reactive(NULL)
    )

  })
}
