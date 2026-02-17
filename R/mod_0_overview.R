mod_0_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::includeMarkdown(
      system.file("app/www/welcome_message.md", package = "wiseapp")
    ),
    shiny::textInput(
      inputId = ns("folder_path"),
      label = "Enter folder path",
      value = "",
      placeholder = "/path/to/datafolder",
      width = "100%"
    ),
    shiny::actionButton(ns("apply_folder_path"), "Use this folder"),
    shiny::verbatimTextOutput(ns("folder_path_echo"))
  )
}

mod_0_overview_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_path <- shiny::reactive({
      p <- if (is.null(input$folder_path)) "" else input$folder_path
      trimws(p)
    })

    folder_path <- shiny::eventReactive(input$apply_folder_path, {
      p <- raw_path()
      shiny::req(nzchar(p))
      normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
    }, ignoreInit = TRUE)

    output$folder_path_echo <- shiny::renderText({
      if (input$apply_folder_path < 1) return("No folder path applied yet.")
      p <- folder_path()
      paste("Using data folder:", p)
    })

    list(
      folder_path = folder_path,
      apply_folder_path = shiny::reactive(input$apply_folder_path)
    )
  })
}