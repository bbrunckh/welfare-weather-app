#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

	runtime <- load_runtime_data()

	# assign to reactiveVal/values for use across modules
	rv <- reactiveValues()
	rv$board <- runtime$board
	rv$pin_prefix <- runtime$pin_prefix
	rv$survey_list_master <- runtime$survey_list_master
	rv$varlist <- runtime$varlist
	rv$weather_list <- runtime$weather_list

	# Step 1 module
	step1_sample <- mod_step1_sample_server(
		id = "step1_sample",
		survey_list_master = rv$survey_list_master,  # defined in app_config.R
		pin_prefix = rv$pin_prefix,
		board = rv$board
		)

		# use the returned reactives
		observeEvent(step1_sample$survey_data(), {
		df <- step1_sample$survey_data()
		# do something with df
		}
		)
}
