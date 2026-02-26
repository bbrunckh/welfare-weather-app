#' 0_overview UI Function
#'
#' @description Landing page with welcome message and data connection
#' configuration. All connection options are rendered inline with no
#' child modules.
#'
#' @param id Module id.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_0_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown(
      system.file("app/www/welcome_message.md", package = "wiseapp")
    ),
    hr(),
    h4("Data"),
    selectInput(
      inputId  = ns("connection_type"),
      label    = "Source",
      choices  = c(
        "Local folder"         = "local",
        "Amazon S3"            = "s3",
        "Google Cloud Storage" = "gcs",
        "Azure Blob Storage"   = "azure",
        "Hugging Face"         = "hf"
      ),
      selected = "local"
    ),
    uiOutput(ns("connection_options_ui")),
    uiOutput(ns("connection_status_ui")),
    br(),
    actionButton(
      ns("apply_connection"),
      "Connect to data",
      class = "btn-primary",
      style = "width: 100%;"
    ),
    br(),
    br(),
    verbatimTextOutput(ns("folder_path_echo")),
    verbatimTextOutput(ns("folder_contents_echo"))
  )
}

#' 0_overview Server Function
#'
#' @param id Module id.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{$folder_path}}{Reactive character. Applied local folder path,
#'       or \code{NULL} for non-local connections or before connecting.}
#'     \item{\code{$connection_params}}{Reactive named list of all connection
#'       parameters for the selected data source type.}
#'   }
#'
#' @noRd
mod_0_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Per-type connection options UI -------------------------------------

    
    output$connection_options_ui <- renderUI({
      req(input$connection_type)

      switch(
        input$connection_type,

        "local" = tagList(
          textInput(
            ns("local_path"),
            label       = "Path",
            value       = "data/",
            placeholder = "/path/to/data"
          ),
          helpText(
            "Path to a local folder containing WISE-APP data files.",
            style = "font-size: 12px;"
          )
        ),

        "s3" = tagList(
          textInput(ns("s3_bucket"),     "S3 bucket",             placeholder = "my-bucket"),
          textInput(ns("s3_prefix"),     "Key prefix (optional)", placeholder = "data/"),
          textInput(ns("s3_region"),     "Region",                placeholder = "us-east-1"),
          textInput(ns("s3_key_id"),     "Access key ID",         placeholder = "AKIA..."),
          passwordInput(ns("s3_secret"), "Secret access key",     placeholder = ""),
          helpText(
            "Leave key ID and secret blank to use environment credentials (AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY).",
            style = "font-size: 12px;"
          )
        ),

        "gcs" = tagList(
          textInput(ns("gcs_bucket"),  "GCS bucket",           placeholder = "my-bucket"),
          textInput(ns("gcs_prefix"),  "Prefix (optional)",    placeholder = "data/"),
          textInput(ns("gcs_keyfile"), "Service account JSON", placeholder = "/path/to/key.json"),
          helpText(
            "Leave key file blank to use Application Default Credentials.",
            style = "font-size: 12px;"
          )
        ),

        "azure" = tagList(
          textInput(ns("azure_account"),   "Storage account",   placeholder = "myaccount"),
          textInput(ns("azure_container"), "Container",         placeholder = "my-container"),
          textInput(ns("azure_prefix"),    "Prefix (optional)", placeholder = "data/"),
          passwordInput(ns("azure_key"),   "Account key",       placeholder = ""),
          helpText(
            "Leave account key blank to use the AZURE_STORAGE_KEY environment variable.",
            style = "font-size: 12px;"
          )
        ),

        "hf" = tagList(
          textInput(ns("hf_repo"),          "Repository",              placeholder = "username/dataset-name"),
          textInput(ns("hf_subdir"),        "Subdirectory (optional)", placeholder = "data/"),
          passwordInput(ns("hf_token"),     "HF token (private repos)", placeholder = "hf_..."),
          helpText(
            "Leave token blank for public repositories.",
            style = "font-size: 12px;"
          )
        )
      )
    })

    # ---- Collect connection parameters (delegates to fct_connection.R) ------

    connection_params <- reactive({
      req(input$connection_type)
      build_connection_params(
        type            = input$connection_type,
        path            = input$local_path,
        s3_bucket       = input$s3_bucket,
        s3_prefix       = input$s3_prefix,
        s3_region       = input$s3_region,
        s3_key_id       = input$s3_key_id,
        s3_secret       = input$s3_secret,
        gcs_bucket      = input$gcs_bucket,
        gcs_prefix      = input$gcs_prefix,
        gcs_keyfile     = input$gcs_keyfile,
        azure_account   = input$azure_account,
        azure_container = input$azure_container,
        azure_prefix    = input$azure_prefix,
        azure_key       = input$azure_key,
        hf_repo         = input$hf_repo,
        hf_subdir       = input$hf_subdir,
        hf_token        = input$hf_token
      )
    })

    # ---- Validation (delegates to fct_connection.R) -------------------------

    connection_valid <- reactive({
      req(connection_params())
      validate_connection_params(connection_params())
    })

    output$connection_status_ui <- renderUI({
      req(input$connection_type)
      if (isTRUE(connection_valid())) {
        p(
          icon("circle-check"), " Connection configured.",
          style = "color: #2e7d32; font-size: 12px; margin-top: 4px;"
        )
      } else {
        p(
          icon("circle-exclamation"), " Fill in required fields above.",
          style = "color: #c62828; font-size: 12px; margin-top: 4px;"
        )
      }
    })

    # ---- Apply connection on button click -----------------------------------

    applied_connection <- reactiveVal(NULL)
    survey_list        <- reactiveVal(NULL)
    variable_list      <- reactiveVal(NULL)
    cpi_ppp            <- reactiveVal(NULL)
    pov_lines          <- reactiveVal(NULL)

    observeEvent(input$apply_connection, {

      if (!isTRUE(connection_valid())) {
        showNotification(
          "Please fill in all required connection fields before connecting.",
          type = "warning", duration = 4
        )
        return()
      }

      params <- connection_params()

      if (identical(params$type, "local")) {
        tryCatch({
          params$path <- normalise_local_path(params$path)
        }, error = function(e) {
          showNotification("Please enter a valid folder path.", type = "warning", duration = 4)
          return()
        })
        message("[overview] applied local folder: ", params$path)
      } else {
        message("[overview] applied connection: ", params$type)
      }

      applied_connection(params)

      # ---- Load metadata files ----------------------------------------------

      load_notif <- showNotification("Loading metadata files...", duration = NULL, type = "message")
      on.exit(removeNotification(load_notif), add = TRUE)

      tryCatch({
        survey_list(load_data("survey_list.csv", params, collect = TRUE))
        showNotification(paste0("Survey list loaded (", nrow(survey_list()), " rows)"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification("Failed to load survey_list.csv", type = "error", duration = 5)
      })

      tryCatch({
        variable_list(load_data("variable_list.csv", params, collect = TRUE))
        showNotification(paste0("Variable list loaded (", nrow(variable_list()), " rows)"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification("Failed to load variable_list.csv", type = "error", duration = 5)
      })

      tryCatch({
        cpi_ppp(load_data("cpi_ppp.csv", params, collect = TRUE))
        showNotification("CPI / PPP conversions loaded", type = "message", duration = 2)
      }, error = function(e) {
        showNotification("Failed to load cpi_ppp.csv", type = "error", duration = 5)
      })

      showNotification(paste0("Connected to ", input$connection_type, " data source."), type = "message", duration = 3)

      # poverty lines (delegates to fct_connection.R)
      pov_lines(default_poverty_lines())

    }, ignoreInit = TRUE)

    # ---- Diagnostic outputs -------------------------------------------------

    output$folder_path_echo <- renderText({
      p <- applied_connection()
      if (is.null(p)) return("No connection applied yet.")
      if (identical(p$type, "local")) paste0("Connected: local folder — ", p$path)
      else paste0("Connected: ", p$type)
    })

    output$folder_contents_echo <- renderText({
      p <- applied_connection()
      if (is.null(p) || !identical(p$type, "local")) return(NULL)
      if (!dir.exists(p$path)) return(paste0("Path does not exist: ", p$path))
      files <- list.files(p$path, recursive = FALSE)
      if (length(files) == 0) return("Folder is empty.")
      paste(files, collapse = "\n")
    })

    # ---- Return API ---------------------------------------------------------

    list(
      local_dir       = reactive({
        p <- applied_connection()
        if (is.null(p) || !identical(p$type, "local")) return(NULL)
        p$path
      }),
      connection_params = reactive(applied_connection()),
      survey_list       = reactive(survey_list()),
      variable_list     = reactive(variable_list()),
      cpi_ppp           = reactive(cpi_ppp()),
      pov_lines         = reactive(pov_lines())
    )
  })
}