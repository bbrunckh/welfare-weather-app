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

mod_step1_server <- function(id, survey_list_master, pin_prefix, board) {
  moduleServer(id, function(input, output, session) {
    
    # Pass reactives into the child module; use the plain child id "sample"
    step1_api <- mod_step1_sample_server(
      "sample",
      survey_list_master = survey_list_master_r,
      pin_prefix = pin_prefix_r,
      board = board_r
    )
    
    # React when data is loaded
    observeEvent(step1_api$data_loaded(), {
      if (step1_api$data_loaded()) {
        df <- step1_api$survey_data()
        # do something with df
        message("Parent saw data_loaded; rows = ", nrow(df))
      }
    })
    
  })
}
