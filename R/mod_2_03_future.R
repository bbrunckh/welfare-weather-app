#' 2_03_future UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_2_03_future_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("future_selectors")),
    uiOutput(ns("fut_sim_specs_button")),
    uiOutput(ns("fut_sim_specs"))
  )
}

#' 2_03_future Server Functions
#'
#' Manages future climate forecast year selection, SSP selection, band width,
#' and residual handling. Returns selected_fut: one row per SSP x anchor year.
#'
#' @param id Module id.
#' @param selected_hist Reactive one-row data frame from mod_2_01_historical.
#' @noRd
mod_2_03_future_server <- function(id, selected_hist = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # SSP display choices
    ssp_choices <- c(
      "SSP2-4.5" = "ssp2_4_5",
      "SSP3-7.0" = "ssp3_7_0",
      "SSP5-8.5" = "ssp5_8_5"
    )
    ssp_labels <- setNames(
      SSP_SHORT_LABELS[names(ssp_choices)],
      unname(ssp_choices)
    )

    default_bw <- 0L

    # ---- Advanced settings toggle ------------------------------------------

    adv_open <- reactiveVal(FALSE)

    # ---- Combined selector UI: forecast years (left) + SSP (right) --------

    output$future_selectors <- renderUI({
      tagList(
        # Two-column selector row
        tags$div(
          style = "display:flex; gap:32px; flex-wrap:wrap; margin-bottom:8px;",

          # Left column: Forecast year(s)
          tags$div(
            style = "min-width:160px;",
            tags$h6(
              "Forecast year(s)",
              style = "margin-bottom:6px; font-weight:600;"
            ),
            checkboxInput(ns("use_2030"), "2030", value = TRUE),
            checkboxInput(ns("use_2040"), "2040", value = TRUE),
            checkboxInput(ns("use_2050"), "2050", value = TRUE)
          ),

          # Right column: Climate scenario(s)
          tags$div(
            style = "min-width:200px;",
            tags$h6(
              "Climate scenario(s)",
              style = "margin-bottom:6px; font-weight:600;"
            ),
            checkboxGroupInput(
              inputId  = ns("climate"),
              label    = NULL,
              choices  = ssp_choices,
              selected = unname(ssp_choices)
            )
          )
        ),

        # Description text (full width, below both columns)
        helpText(
          "Select which future periods to model.",
          "Each selected forecast year generates one scenario run per climate scenario.",
          style = "font-size:11px; margin-top:0; margin-bottom:4px;"
        ),
        helpText(
          "CMIP6 ensemble \u2018delta\u2019 fields are used to perturb historical observations.",
          style = "font-size:11px; margin-top:0; margin-bottom:8px;"
        ),

        # Advanced settings toggle (full width, below description)
        tags$div(
          style = "margin-top:4px;",
          actionLink(ns("toggle_adv"), uiOutput(ns("adv_link_label")))
        ),
        uiOutput(ns("adv_settings_panel"))
      )
    })

    # ---- Advanced settings toggle label ------------------------------------

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
          "Override the default forecast years by specifying anchor years and",
          "band width manually. Leave Year 2 and Year 3 blank to omit them.",
          style = "font-size:11px;"
        ),
        fluidRow(
          column(4, shiny::numericInput(ns("anchor1"), "Year 1",
                                        value = 2030, min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor2"), "Year 2 (optional)",
                                        value = NA,   min = 2020, max = 2090, step = 5)),
          column(4, shiny::numericInput(ns("anchor3"), "Year 3 (optional)",
                                        value = NA,   min = 2020, max = 2090, step = 5))
        ),
        shiny::sliderInput(
          ns("band_width"),
          label = "Band width (\u00b1 years around each selected year)",
          min = 0, max = 25, value = 0, step = 5, sep = ""
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

      warns <- character(0)
      if (length(anchors) > 1) {
        for (i in seq_len(length(anchors) - 1)) {
          for (j in (i + 1):length(anchors)) {
            if (abs(anchors[i] - anchors[j]) < 2 * bw)
              warns <- c(warns, paste0(
                "\u26a0\ufe0f Years ", anchors[i], " and ", anchors[j],
                " bands overlap (\u00b1", bw, "yr)."
              ))
          }
        }
      }
      if (length(warns) == 0) return(NULL)
      tagList(lapply(warns, function(w)
        helpText(w, style = "color:#c0392b; font-size:11px;")))
    })

    # ---- Simulation parameters toggle --------------------------------------

    fut_sim_specs_open <- reactiveVal(FALSE)

    output$fut_sim_specs_button <- renderUI({
      shiny::actionButton(
        ns("fut_sim_specs_toggle"),
        "Simulation parameters",
        style = "margin-bottom:12px;"
      )
    })

    observeEvent(input$fut_sim_specs_toggle, {
      fut_sim_specs_open(!isTRUE(fut_sim_specs_open()))
    })

    output$fut_sim_specs <- renderUI({
      if (!isTRUE(fut_sim_specs_open())) return(NULL)
      residual_method_ui(ns, "fut_sim_residuals")
    })

    # ---- Resolve active anchors and band width ------------------------------

    active_anchors <- reactive({
      if (isTRUE(adv_open()) && !is.na(input$anchor1)) {
        anchors <- c(input$anchor1, input$anchor2, input$anchor3)
        sort(unique(anchors[!is.na(anchors)]))
      } else {
        selected <- c(
          if (isTRUE(input$use_2030)) 2030L,
          if (isTRUE(input$use_2040)) 2040L,
          if (isTRUE(input$use_2050)) 2050L
        )
        if (length(selected) == 0L) 2030L else selected
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

      rows <- lapply(input$climate, function(ssp) {
        prefix <- ssp_labels[ssp]
        lapply(anchors, function(anchor) {
          yr <- c(anchor - bw, anchor + bw)
          # Scenario name: drop band suffix when bw = 0
          scene_name <- if (bw == 0L) {
            paste0(prefix, " / ", anchor)
          } else {
            paste0(prefix, " / ", anchor, " \u00b1", bw, "yr")
          }
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

