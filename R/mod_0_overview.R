mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown(
      system.file("app/www/welcome_message.md", package = "wiseapp")
    )
  )
}

mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}