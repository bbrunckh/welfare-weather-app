#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Serve bundled MathJax from its own resource path (outside www/
  # to avoid bundle_resources() trying to include all 100+ JS files)
  shiny::addResourcePath("mathjax", system.file("mathjax", package = "wiseapp"))
  options(
    shiny.mathjax.url    = "mathjax/MathJax.js",
    shiny.mathjax.config = "config=TeX-AMS-MML_HTMLorMML"
  )

  bslib::page_navbar(
    title = tagList(
      "WISE-APP",
      tags$span(class = "app-version", golem::get_golem_version())
    ),
    window_title = "WISE-APP",
    theme = bslib::bs_theme(
      version = 5,
      brand   = app_sys("app/_brand.yml")
    ),
    navbar_options = bslib::navbar_options(theme = "dark", bg = "#002244"),

    header = tagList(
      golem_add_external_resources(),
      withMathJax(),
      shiny::useBusyIndicators(),
      shiny::busyIndicatorOptions(
        spinner_type  = "bars3",
        spinner_color = "#0071BC",
        spinner_size  = "36px"
      )
    ),

    # Page modules
    bslib::nav_panel(
      "Overview",
      icon = icon("house"),
      mod_0_overview_ui("overview")
    ),
    bslib::nav_panel(
      "Step 1 - Model welfare",
      icon = icon("chart-line"),
      mod_1_modelling_ui("step1")
    ),
    bslib::nav_panel(
      "Step 2 - Simulate welfare",
      icon = icon("cloud-sun-rain"),
      mod_2_simulation_ui("step2")
    ),
    bslib::nav_panel(
      "Step 3 - Policy scenarios",
      icon = icon("scale-balanced"),
      mod_3_scenario_ui("step3")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      tags$a(
        icon("book-open"), "Docs",
        href = "https://datanalytics-int.worldbank.org/content/a24b499b-46b7-420e-9e77-5475b45cc7c5",
        target = "_blank"
      )
    )
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
