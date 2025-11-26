#' @importFrom bsplus bs_accordion bs_append
NULL

mod_step1_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    waiter::autoWaiter(html = waiter::spin_2(), color = waiter::transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),

    sidebarLayout(
      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title = "1 Sample",
            content = mod_step1_sample_ui(ns("sample"))
          )
      ),

      mainPanel(

        includeMarkdown(
          system.file("app/www/equation.md", package = "wiseapp")
        )

      )
    )
  )
}

mod_step1_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    mod_step1_sample_server("sample")

  })
}