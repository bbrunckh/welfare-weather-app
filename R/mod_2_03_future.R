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
#' Scenario labels use hardcoded SSP prefixes (no user-editable label inputs).
#'
#' @param id Module id.
#'
#' @noRd
mod_2_03_future_server <- function(id, selected_hist = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ssp_labels <- c(
      "ssp2_4_5" = "SSP2",
      "ssp3_7_0" = "SSP3",
      "ssp5_8_5" = "SSP5"
    )
    ssp_choices <- c(
      "SSP2-4.5" = "ssp2_4_5",
      "SSP3-7.0" = "ssp3_7_0",
      "SSP5-8.5" = "ssp5_8_5"
    )

    # Default anchor years and band width
    default_anchors  <- c(2030L, 2040L, 2050L)
    default_bw       <- 10L

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
        checkboxInput(ns("use_2030"), "2030 ±10 years (2020–2040)", value = TRUE),
        checkboxInput(ns("use_2040"), "2040 ±10 years (2030–2050)", value = TRUE),
        checkboxInput(ns("use_2050"), "2050 ±10 years (2040–2060)", value = TRUE),
        tags$div(
          style = "margin-top:8px;",
          actionLink(ns("toggle_adv"), uiOutput(ns("adv_link_label")))
        ),
        uiOutput(ns("adv_settings_panel"))
      )
    })

    output$adv_link_label <- renderUI({
      if (isTRUE(adv_open())) {
        tags$span("▼ Advanced settings", style = "font-size:12px;")
      } else {
        tags$span("► Advanced settings", style = "font-size:12px;")
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
          "Override the default timeframes by specifying anchor years and band width manually.",
          "When set, these replace the checkbox selections above.",
          "Leave Year 2 and Year 3 blank to omit them.",
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
          label = "Band width (± years around each selected year)",
          min   = 5, max = 25, value = 10, step = 5, sep = ""
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
        warns <- c(warns, paste0("⚠️ Band ±", bw, "yr = ", n_yrs,
                                 " years. 1:5 requires ≥5 yrs, 1:10 requires ≥10 yrs."))
      if (length(anchors) > 1) {
        for (i in seq_len(length(anchors) - 1)) {
          for (j in (i + 1):length(anchors)) {
            if (abs(anchors[i] - anchors[j]) < 2 * bw)
              warns <- c(warns, paste0("⚠️ Years ", anchors[i], " and ",
                                       anchors[j], " bands overlap (±", bw, "yr)."))
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
      shiny::actionButton(ns("fut_sim_specs"), "Simulation parameters",
                          style = "margin-bottom:12px;")
    })

    observeEvent(input$fut_sim_specs, {
      fut_sim_specs_open(!isTRUE(fut_sim_specs_open()))
    })

    output$fut_sim_specs <- renderUI({
      if (!isTRUE(fut_sim_specs_open())) return(NULL)
      tagList(
        radioButtons(
          inputId  = ns("fut_sim_residuals"),
          label    = "Residuals method",
          choices  = residual_choices(),
          selected = "original"
        ),
        helpText(
          tags$b("none:"), " return fitted values only.", tags$br(),
          tags$b("original:"), " match each observation's own training residual by ID,",
          " preserving individual-level heterogeneity across simulation years.", tags$br(),
          tags$b("empirical:"), " resample residuals from the training distribution",
          " (non-parametric bootstrap).", tags$br(),
          tags$b("normal:"), " draw residuals from N(0, σ) where σ is the",
          " training residual SD.",
          style = "font-size:11px;"
        )
      )
    })

    # ---- Resolve active anchors & band width --------------------------------
    # When advanced panel is open AND has valid anchor1, use advanced values.
    # Otherwise fall back to the checkbox defaults.

    active_anchors <- reactive({
      if (isTRUE(adv_open()) && !is.na(input$anchor1)) {
        # Advanced mode
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

      # build historical year range suffix once
      hist_suffix <- ""
      if (!is.null(selected_hist)) {
        yr <- tryCatch(unlist(selected_hist()$year_range), error = function(e) NULL)
        if (!is.null(yr) && length(yr) == 2)
          hist_suffix <- paste0(" (", yr[1], "–", yr[2], ")")
      }

      rows <- lapply(input$climate, function(ssp) {
        prefix <- paste0(ssp_labels[ssp], hist_suffix)
        lapply(anchors, function(anchor) {
          yr         <- c(anchor - bw, anchor + bw)
          scene_name <- paste0(prefix, " / ", anchor, " ±", bw, "yr")
          data.frame(
            type          = "future",
            year_range    = I(list(yr)),
            ssp           = ssp,
            anchor_year   = as.integer(anchor),
            band_width    = bw,
            method        = input$climatemethod      %||% "delta",
            residuals     = input$fut_sim_residuals  %||% "original",
            scenario_name = scene_name,
            stringsAsFactors = FALSE
          )
        })
      })

      do.call(rbind, do.call(c, rows))
    })

    list(selected_fut = selected_fut)
  })
}

