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
    uiOutput(ns("anchor_year_inputs")),
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
    ssp_choices_rev <- setNames(names(ssp_choices), unname(ssp_choices))

    # ---- Anchor year inputs + band width ------------------------------------

    output$anchor_year_inputs <- renderUI({
      tagList(
        h6("Future timeframes", style = "margin-bottom:4px; font-weight:600;"),
        helpText(
          "Each timeframe generates one scenario run per climate scenario.",
          "Year range = Selected year \u00b1 band width.",
          style = "font-size:11px; margin-top:0;"
        ),
        fluidRow(
          column(4, shiny::numericInput(ns("anchor1"), "Year 1", value = 2030,
                                        min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor2"), "Year 2", value = 2050,
                                        min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor3"), "Year 3", value = 2070,
                                        min = 2020, max = 2090, step = 5))
        ),
        shiny::sliderInput(
          ns("band_width"),
          label = "Band width (\u00b1 years around each selected year)",
          min   = 5, max = 25, value = 10, step = 5, sep = ""
        ),
        uiOutput(ns("anchor_warnings"))
      )
    })

    # ---- Anchor year warnings -----------------------------------------------

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
      # check for overlapping bands
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

    # ---- Climate scenario selector + per-SSP name inputs -------------------

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
        ),
        uiOutput(ns("ssp_name_inputs"))
      )
    })

    # ---- Per-SSP name inputs -----------------------------------------------
    # One textInput per SSP (not per anchor year)  used as a prefix.
    # Final scenario name = "<ssp_name> / <anchor> \u00b1<band>yr"

    output$ssp_name_inputs <- renderUI({
      req(input$climate)

      # Build historical year range suffix if available
      hist_suffix <- ""
      if (!is.null(selected_hist)) {
        yr <- tryCatch(unlist(selected_hist()$year_range), error = function(e) NULL)
        if (!is.null(yr) && length(yr) == 2)
          hist_suffix <- paste0(" (", yr[1], "\u2013", yr[2], ")")
      }

      inputs <- lapply(input$climate, function(ssp) {
        input_id      <- paste0("scenario_name_", ssp)
        default_label <- paste0(ssp_labels[ssp], hist_suffix)
        textInput(
          inputId = ns(input_id),
          label = paste0("Label prefix: ", ssp_choices_rev[ssp]),
          value   = default_label
        )
      })
      do.call(tagList, inputs)
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
          tags$b("normal:"), " draw residuals from N(0, \u03c3) where \u03c3 is the",
          " training residual SD.",
          style = "font-size:11px;"
        )
      )
    })

    # ---- selected_fut: one row per SSP x anchor year -----------------------

    selected_fut <- reactive({
      req(input$climate, input$band_width)

      anchors <- c(input$anchor1, input$anchor2, input$anchor3)
      anchors <- sort(unique(anchors[!is.na(anchors)]))
      bw      <- as.integer(input$band_width)

      rows <- lapply(input$climate, function(ssp) {
        prefix   <- input[[paste0("scenario_name_", ssp)]] %||% ssp_labels[ssp]
        lapply(anchors, function(anchor) {
          yr         <- c(anchor - bw, anchor + bw)
          scene_name <- paste0(prefix, " / ", anchor, " \u00b1", bw, "yr")
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

