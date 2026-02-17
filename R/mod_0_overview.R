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
    shiny::verbatimTextOutput(ns("folder_path_echo")),
    shiny::verbatimTextOutput(ns("folder_contents_echo"))
  )
}

mod_0_overview_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    applied_folder_path <- shiny::reactiveVal(NULL)

    observeEvent(input$apply_folder_path, {
      p <- trimws(input$folder_path %||% "")
      if (!nzchar(p)) {
        applied_folder_path(NULL)
        return()
      }
      p <- normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
      applied_folder_path(p)
      message("[overview] applied folder: ", p)
    }, ignoreInit = TRUE)

    list(
      folder_path = shiny::reactive(applied_folder_path())
    )
  })
}