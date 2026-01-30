#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  navbarPage(
    title = tagList(
      "WISE-APP",
      tags$small(version, style = "color: #777; font-size: 0.5em;")
    ),

    # MathJax once
    header = tags$head(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
    ),

	# Page modules
    tabPanel("Overview", mod_overview_ui("overview_ui")), # welcome message
    tabPanel("Step 1 - Model welfare", mod_1_modelling_ui("step1")), # step 1 module
	  tabPanel("Step 2 - Simulate welfare", mod_2_simulation_ui("step2")) #step 2 module


  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(ext = 'png'),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "wiseapp"

		)
	)
}
