#DRK Version 20260218 0849

mod_3_scenario_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    waiter::autoWaiter(html = waiter::spin_2(), color = waiter::transparent(.5)),
    h4("What if...? Explore alternative climate and policy scenarios"),

    sidebarLayout(
      sidebarPanel(
        bsplus::bs_accordion(id = ns("accordion")) |>
          bsplus::bs_append(
            title = "Policy scenario",
            content = tagList(
              p("Configure your policy scenario parameters here.")
              # Add your scenario configuration inputs
            )
          ) |>
          bsplus::bs_append(
            title = "Run scenario",
            content = tagList(
              p("Run scenario analysis.")
            )
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step3_output_tabs"),

          tabPanel(
            title = "Results",
            value = "results",
            
h4("Scenario results"),

bslib::card(
  bslib::card_header("Baseline run (Step 2)"),
  verbatimTextOutput(ns("baseline_summary"))
),

tags$hr(),
p("Scenario comparison results will be displayed here.")
          ),

          tabPanel(
            title = "Configuration",
            value = "config",
            h4("Scenario configuration"),
            p("Summary of scenario configuration and parameters will be shown here.")
          )
        )
      )
    )
  )
}


mod_3_scenario_server <- function(id, step1, step2 = NULL, pov_lines, varlist) {
  moduleServer(id, function(input, output, session) {

    # Namespace helper for server-side UI generation
    ns <- session$ns


`%||%` <- function(a, b) if (!is.null(a)) a else b

baseline_run <- reactive({
  if (is.null(step2) || !is.list(step2) || is.null(step2$run)) return(NULL)
  tryCatch(step2$run(), error = function(e) NULL)
})

output$baseline_summary <- renderText({
  r <- baseline_run()
  if (is.null(r)) {
    return("No baseline Step 2 run available yet.\n\nRun a historical simulation in Step 2 first, then return here.")
  }

  meta <- r$meta %||% list()
  out <- character()

  if (!is.null(meta$step1_spec_summary) && nzchar(meta$step1_spec_summary)) {
    out <- c(out, paste0("Spec: ", meta$step1_spec_summary))
  }
  if (!is.null(meta$povline_label) && nzchar(meta$povline_label)) {
    out <- c(out, paste0("Poverty line: ", meta$povline_label))
  } else if (!is.null(meta$step1_spec) && is.list(meta$step1_spec)) {
    # fallback: attempt to read povline from spec
    so <- meta$step1_spec$outcome
    if (!is.null(so) && "povline" %in% names(so)) {
      pv <- suppressWarnings(as.numeric(so$povline[[1]]))
      if (is.finite(pv)) out <- c(out, paste0("Poverty line: ", pv))
    }
  }

  if (!is.null(meta$board_path) && nzchar(meta$board_path)) {
    out <- c(out, paste0("Board: ", meta$board_path))
  }

  sc <- meta$scenario_spec
  if (!is.null(sc) && is.list(sc) && !is.null(sc$id) && nzchar(sc$id)) {
    out <- c(out, paste0("Scenario ID: ", sc$id))
  }

  pe <- r$phase_e
  if (!is.null(pe) && isTRUE(pe$ok) && !is.null(pe$last_run)) {
    out <- c(out, paste0("Last run: ", as.character(pe$last_run)))
  }

  if (!length(out)) out <- c("Baseline run is available, but metadata is missing.")

  paste(out, collapse = "\n")
})

    # Return API for downstream use
    list(
      # Add your return values here
    )
  })
}