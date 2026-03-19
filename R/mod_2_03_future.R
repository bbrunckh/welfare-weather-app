#' 2_03_future UI Function
#'
#' @description A shiny Module. Selects the future climate configuration
#'   (SSP scenarios, anchor years, band width, residual handling).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_03_future_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("future_timeframes")),
    uiOutput(ns("climate_scenario")),
    uiOutput(ns("fut_sim_specs_button")),
    uiOutput(ns("fut_sim_specs"))
  )
}

#' 2_03_future Server Functions
#'
#' Manages future climate anchor year selection, SSP selection, band width,
#' and residual handling. Returns selected_fut: one row per SSP x anchor year.
#'
#' @param id Module id.
#' @param selected_hist Reactive one-row data frame from mod_2_01_historical
#'   (used to include the historical period in scenario labels).
#' @noRd
mod_2_03_future_server <- function(id, selected_hist = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # SSP display choices — use SSP_SHORT_LABELS from fct_simulations.R
    # Keys match the climate file naming convention (ssp2_4_5, etc.)
    ssp_choices <- c(
      "SSP2-4.5" = "ssp2_4_5",
      "SSP3-7.0" = "ssp3_7_0",
      "SSP5-8.5" = "ssp5_8_5"
    )
    # Short labels indexed by ssp_choices value (for scenario name construction)
    ssp_labels <- setNames(
      SSP_SHORT_LABELS[names(ssp_choices)],
      unname(ssp_choices)
    )

    default_bw <- 10L

    # ---- Advanced settings toggle ------------------------------------------

    adv_open <- reactiveVal(FALSE)

    # ---- Future timeframes UI ----------------------------------------------

    output$future_timeframes <- renderUI({
      tagList(
        h6("Future timeframes", style = "margin-bottom:4px; font-weight:600;"),
        helpText(
          "Select which future periods to model.",
          "Each selected timeframe generates one scenario run per climate scenario.",
          style = "font-size:11px; margin-top:0; margin-bottom:6px;"
        ),
        # Simple default: three toggleable checkboxes
        checkboxInput(ns("use_2030"), "2030 \u00b110 years (2020-2040)", value = TRUE),
        checkboxInput(ns("use_2040"), "2040 \u00b110 years (2030-2050)", value = TRUE),
        checkboxInput(ns("use_2050"), "2050 \u00b110 years (2040-2060)", value = TRUE),
        tags$div(
          style = "margin-top:8px;",
          actionLink(ns("toggle_adv"), uiOutput(ns("adv_link_label")))
        ),
        uiOutput(ns("adv_settings_panel"))
      )
    })

    output$adv_link_label <- renderUI({
      if (isTRUE(adv_open())) {
        tags$span("\u25bc Advanced settings", style = "font-size:12px;")
      } else {
        tags$span("\u25ba Advanced settings", style = "font-size:12px;")
      }
    })

    observeEvent(input$toggle_adv, {
      adv_open(!isTRUE(adv_open()))
    })

    # ---- Advanced settings panel -------------------------------------------

    output$adv_settings_panel <- renderUI({
      if (!isTRUE(adv_open())) return(NULL)

      tagList(
        tags$hr(style = "margin: 8px 0;"),
        helpText(
          "Override the default timeframes by specifying anchor years and band",
          "width manually. Leave Year 2 and Year 3 blank to omit them.",
          style = "font-size:11px;"
        ),
        fluidRow(
          column(4, shiny::numericInput(ns("anchor1"), "Year 1",
                                        value = 2030, min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor2"), "Year 2 (optional)",
                                        value = NA, min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor3"), "Year 3 (optional)",
                                        value = NA, min = 2020, max = 2090, step = 5))
        ),
        shiny::sliderInput(
          ns("band_width"),
          label = "Band width (\u00b1 years around each selected year)",
          min = 5, max = 25, value = 10, step = 5, sep = ""
        ),
        uiOutput(ns("anchor_warnings"))
      )
    })

    # ---- Anchor year warnings (advanced mode) ------------------------------

    output$anchor_warnings <- renderUI({
      req(input$band_width)
      anchors <- c(input$anchor1, input$anchor2, input$anchor3)
      anchors <- anchors[!is.na(anchors)]
      bw      <- input$band_width
      n_yrs   <- bw * 2L

      warns <- character(0)
      if (n_yrs < 10)
        warns <- c(warns, paste0("\u26a0\ufe0f Band \u00b1", bw, "yr = ", n_yrs,
                                 " years. 1:5 requires \u22655 yrs, 1:10 requires \u226510 yrs."))
      if (length(anchors) > 1) {
        for (i in seq_len(length(anchors) - 1)) {
          for (j in (i + 1):length(anchors)) {
            if (abs(anchors[i] - anchors[j]) < 2 * bw)
              warns <- c(warns, paste0("\u26a0\ufe0f Years ", anchors[i], " and ",
                                       anchors[j], " bands overlap (\u00b1", bw, "yr)."))
          }
        }
      }
      if (length(warns) == 0) return(NULL)
      tagList(lapply(warns, function(w)
        helpText(w, style = "color:#c0392b; font-size:11px;")))
    })

    # ---- Climate scenario selector -----------------------------------------

    output$climate_scenario <- renderUI({
      tagList(
        checkboxGroupInput(
          inputId  = ns("climate"),
          label    = "Climate scenario(s)",
          choices  = ssp_choices,
          selected = unname(ssp_choices)
        ),
        helpText(
          "CMIP6 ensemble 'delta' fields are used to perturb historical observations.",
          style = "font-size:12px;"
        )
      )
    })

    # ---- Simulation parameters toggle --------------------------------------

    fut_sim_specs_open <- reactiveVal(FALSE)

    output$fut_sim_specs_button <- renderUI({
      shiny::actionButton(ns("fut_sim_specs_toggle"),
                          "Simulation parameters",
                          style = "margin-bottom:12px;")
    })

    observeEvent(input$fut_sim_specs_toggle, {
      fut_sim_specs_open(!isTRUE(fut_sim_specs_open()))
    })

    # ---- Simulation parameters panel ----------------------------------------
    # Uses shared residual_method_ui() from fct_simulations.R

    output$fut_sim_specs <- renderUI({
      if (!isTRUE(fut_sim_specs_open())) return(NULL)
      residual_method_ui(ns, "fut_sim_residuals")
    })

    # ---- Resolve active anchors and band width ------------------------------
    # When advanced panel is open AND has valid anchor1, use advanced values.
    # Otherwise fall back to the simple checkbox defaults.

    active_anchors <- reactive({
      if (isTRUE(adv_open()) && !is.na(input$anchor1)) {
        anchors <- c(input$anchor1, input$anchor2, input$anchor3)
        sort(unique(anchors[!is.na(anchors)]))
      } else {
        # Simple checkbox mode
        selected <- c(
          if (isTRUE(input$use_2030)) 2030L,
          if (isTRUE(input$use_2040)) 2040L,
          if (isTRUE(input$use_2050)) 2050L
        )
        if (length(selected) == 0) 2030L else selected
      }
    })

    active_bw <- reactive({
      if (isTRUE(adv_open()) && !is.na(input$band_width)) {
        as.integer(input$band_width)
      } else {
        default_bw
      }
    })

    # ---- selected_fut: one row per SSP x anchor year -----------------------

    selected_fut <- reactive({
      req(input$climate)

      anchors <- active_anchors()
      bw      <- active_bw()

      # Scenario names use "SSP2 / 2030 ±10yr" only.
      # The historical year range is NOT embedded — it is displayed separately
      # in the results header so that names remain clean and parseable by the
      # SSP / timeframe filter reactives in mod_2_06.

      rows <- lapply(input$climate, function(ssp) {
        prefix <- ssp_labels[ssp]
        lapply(anchors, function(anchor) {
          yr         <- c(anchor - bw, anchor + bw)
          scene_name <- paste0(prefix, " / ", anchor, " \u00b1", bw, "yr")
          data.frame(
            type          = "future",
            year_range    = I(list(yr)),
            ssp           = ssp,
            anchor_year   = as.integer(anchor),
            band_width    = bw,
            method        = "delta",
            residuals     = input$fut_sim_residuals %||% "original",
            scenario_name = scene_name,
            stringsAsFactors = FALSE
          )
        })
      })

      do.call(rbind, do.call(c, rows))
    })

    # ---- Module return API --------------------------------------------------

    list(selected_fut = selected_fut)
  })
}

