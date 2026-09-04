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

  hero <- div(
    class = "hero-panel",
    div(
      class = "hero-panel-inner",
      tags$img(
        src = "www/logo.png",
        class = "hero-logo",
        alt = "WISE-APP logo"
      ),
      div(
        h1("Welcome to WISE-APP"),
        p(class = "hero-subtitle",
          "Weather Impact Simulation and Evaluation for Adaptation Policy and Planning"),
        p("WISE-APP is designed to stress-test household welfare under historical and projected climate scenarios, and to evaluate resilience-building policy interventions."),
        p("Work through the three steps below - each step builds on the previous one."),

        p(tags$a(
          icon("book-open"), "WISE-APP User Guide",
          href = "https://datanalytics.worldbank.org/wise-app-docs",
          target = "_blank"
        ))
      )
    )
  )

  step_card <- function(n, title, text) {
    bslib::card(
      class = "step-card",
      bslib::card_body(
        div(
          class = "step-card-title",
          tags$span(n, class = "step-badge"),
          h5(title)
        ),
        p(text)
      )
    )
  }

  steps <- bslib::layout_column_wrap(
    width = 1 / 3, fill = FALSE,
    step_card(
      "1", "Model welfare",
      "Estimate the empirical relationship between local weather and an outcome of interest - such as household consumption or poverty status - from survey microdata. The fitted model is the foundation for all subsequent steps."
    ),
    step_card(
      "2", "Climate scenarios",
      "Simulate the distribution of weather-driven welfare outcomes under historical conditions and for future climate projections (CMIP6 scenarios), by applying the model from Step 1."
    ),
    step_card(
      "3", "Policy scenarios",
      "Re-simulate welfare under counterfactual policy scenarios (social protection, infrastructure, labor, education...) to quantify welfare gains and evaluate changes in climate resilience against the Step 2 baseline."
    )
  )

  limitations_card <- bslib::card(
    class = "limitation-card",
    bslib::card_header(icon("triangle-exclamation"), " Limitations"),
    bslib::card_body(
      p(
        "WISE-APP is an illustrative stress-testing tool - not a forecast or a causal impact evaluation framework. Its estimates capture conditional statistical associations between local weather variability and household welfare. The User Guide includes",
        tags$a(
          "important caveats.",
          href = "https://datanalytics.worldbank.org/wise-app-docs/#limitations",
          target = "_blank"
        )
      )
    )
  )

  # On Posit Connect with Databricks env vars: skip the connection form
  # entirely and just show the status badge - server auto-connects on startup.
  data_card <- if (.auto_connect()) {
    bslib::card(
      class = "connect-card",
      bslib::card_header(icon("database"), " Data"),
      bslib::card_body(uiOutput(ns("connection_status_ui")))
    )
  } else {
    bslib::card(
      class = "connect-card",
      bslib::card_header(icon("database"), " Data"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(4, 8),
          div(
            div(
              class = "connection-source-field",
              tags$span("Source:", class = "connection-source-label"),
              wave_toggle_slider(
                ns("connection_type"),
                choices = c(
                  "Local folder" = "local",
                  "Databricks" = "databricks",
                  "GCS" = "gcs",
                  "S3" = "s3"
                ),
                selected = "local"
              )
            ),
            uiOutput(ns("connection_status_ui"))
          ),
            div(
              class = "connection-options-output",
              uiOutput(ns("connection_options_ui"))
            )
          ),
        div(
          class = "connection-action-row",
          actionButton(
            ns("apply_connection"),
            "Connect to data",
            class = "btn-primary"
          )
        )
      )
    )
  }

  overview_footer <- tags$footer(
    class = "overview-footer",
    div(
      class = "overview-footer-brand",
      tags$strong("WISE-APP"),
      tags$span("Distributional Impact of Policies | World Bank Group")
    ),
    div(
      class = "overview-footer-meta",
      tags$span("Version ", golem::get_golem_version()),
      tags$span(
        class = "overview-footer-links",
        tags$a(
          "Docs",
          href = "https://datanalytics.worldbank.org/wise-app-docs/",
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        tags$a(
          "Data",
          href = "https://github.com/worldbank/wise-app-data",
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        tags$a(
          "Source",
          href = "https://github.com/worldbank/wise-app",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      )
    )
  )

  div(
    class = "overview-shell",
    div(
      class = "overview-page",
      hero,
      steps,
      data_card,
      limitations_card
    ),
    overview_footer
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

      div(
        class = "connection-options",
        switch(
          input$connection_type,

          "local" = tagList(
            textInput(
              ns("local_path"),
              label       = "Path:",
              value       = "data/",
              placeholder = "/path/to/data"
            ),
            helpText(
              "Path to a local folder containing WISE-APP data files.",
              style = "font-size: 12px;"
            )
          ),

          "s3" = tagList(
            textInput(ns("s3_bucket"),     "S3 bucket:",             placeholder = "my-bucket"),
            textInput(ns("s3_prefix"),     "Key prefix (optional):", placeholder = "data/"),
            textInput(ns("s3_region"),     "Region:",                placeholder = "us-east-1"),
            textInput(ns("s3_key_id"),     "Access key ID:",         placeholder = "AKIA..."),
            passwordInput(ns("s3_secret"), "Secret access key:",     placeholder = ""),
            helpText(
              "Leave key ID and secret blank to use environment credentials (AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY).",
              style = "font-size: 12px;"
            )
          ),

          "gcs" = tagList(
            textInput(ns("gcs_bucket"),  "GCS bucket:",                  placeholder = "my-bucket"),
            textInput(ns("gcs_prefix"),  "Prefix (optional):",           placeholder = "data/"),
            textInput(ns("gcs_key_id"),  "HMAC access key ID (optional):", placeholder = ""),
            passwordInput(ns("gcs_secret"), "HMAC secret (optional):",    placeholder = ""),
            helpText(
              "GCS uses HMAC (interoperability) keys. Leave both blank to use",
              " GCS_ACCESS_KEY_ID / GCS_SECRET_ACCESS_KEY from .Renviron.",
              style = "font-size: 12px;"
            )
          ),

          "azure" = tagList(
            textInput(ns("azure_account"),           "Storage account:",           placeholder = "datalakeesouoprod"),
            textInput(ns("azure_container"),         "Container:",                 placeholder = "data"),
            textInput(ns("azure_prefix"),            "Prefix (optional):",         placeholder = "DAP/data/wiseapp/"),
            passwordInput(ns("azure_key"),           "Account key (optional):",    placeholder = ""),
            passwordInput(ns("azure_client_id"),     "Client ID (optional):",      placeholder = ""),
            passwordInput(ns("azure_client_secret"), "Client secret (optional):",  placeholder = ""),
            textInput(ns("azure_tenant_id"),         "Tenant ID (optional):",      placeholder = ""),
            helpText(
              "Credentials are optional if a service principal is set via AZURE_CLIENT_ID /",
              "AZURE_CLIENT_SECRET / AZURE_TENANT_ID in .Renviron.",
              "Alternatively supply the storage account key in the key field.",
              style = "font-size: 12px;"
            )
          ),

          "hf" = tagList(
            textInput(ns("hf_repo"),   "Repository:",               placeholder = "username/dataset-name"),
            textInput(ns("hf_subdir"), "Subdirectory (optional):",  placeholder = "data/"),
            helpText(
              "Public Hugging Face datasets only; private repositories and",
              " personal-access tokens are not supported by this connection type.",
              style = "font-size: 12px;"
            )
          ),

          "databricks" = tagList(
            textInput(
              ns("db_workspace"),
              "Workspace URL:",
              placeholder = "https://adb-xxxxxxxxxxxxxxxxx.xx.azuredatabricks.net"
            ),
            passwordInput(ns("db_client_id"),     "Client ID:",     placeholder = ""),
            passwordInput(ns("db_client_secret"),  "Client secret:", placeholder = ""),
            textInput(
              ns("db_volume_path"),
              "Volume path:",
              placeholder = "/Volumes/..."
            ),
            helpText(
              "Set DATABRICKS_HOST, DATABRICKS_CLIENT_ID, DATABRICKS_CLIENT_SECRET,",
              "and DATABRICKS_VOLUME_PATH in .Renviron to leave all fields blank.",
              style = "font-size: 12px;"
            )
          )
        )
      )
    })

    # ---- Collect connection parameters (delegates to fct_connection.R) ------

    connection_params <- reactive({
      req(input$connection_type)
      build_connection_params(
        type = input$connection_type,
        path = input$local_path,
        s3_bucket = input$s3_bucket,
        s3_prefix = input$s3_prefix,
        s3_region = input$s3_region,
        s3_key_id = input$s3_key_id,
        s3_secret = input$s3_secret,
        gcs_bucket = input$gcs_bucket,
        gcs_prefix = input$gcs_prefix,
        gcs_key_id = input$gcs_key_id,
        gcs_secret = input$gcs_secret,
        azure_account = input$azure_account,
        azure_container = input$azure_container,
        azure_prefix = input$azure_prefix,
        azure_key = input$azure_key,
        azure_client_id = input$azure_client_id,
        azure_client_secret = input$azure_client_secret,
        azure_tenant_id = input$azure_tenant_id,
        hf_repo = input$hf_repo,
        hf_subdir = input$hf_subdir,
        db_workspace = input$db_workspace,
        db_client_id = input$db_client_id,
        db_client_secret = input$db_client_secret,
        db_volume_path = input$db_volume_path
      )
    })

    # ---- Validation (delegates to fct_connection.R) -------------------------

    connection_valid <- reactive({
      params <- connection_params()
      req(params)
      validate_connection_params(params)
    })

    # Verified/failed connection state from the last connect attempt (NULL =
    # no attempt yet). "Verified" means the metadata actually loaded; the
    # plain field check below only says "configured" (DEP-03).
    connection_status <- reactiveVal(NULL)

    output$connection_status_ui <- renderUI({
      st <- connection_status()
      if (!is.null(st)) {
        if (identical(st$state, "connected")) {
          return(p(
            icon("circle-check"), " ", st$message,
            style = "color: #2e7d32; font-size: 0.87rem; margin-top: 4px;"
          ))
        }
        return(div(
          p(
            icon("circle-exclamation"), " ", st$message,
            style = "color: #c62828; font-weight: 600; font-size: 0.87rem; margin-top: 4px;"
          ),
          if (!is.null(st$detail)) p(
            st$detail, style = "color: #c62828; font-size: 0.87rem;"
          )
        ))
      }
      if (.auto_connect()) {
        return(p(
          icon("spinner", class = "fa-spin"), " Connecting to Databricks...",
          style = "color: var(--bs-secondary); font-size: 0.87rem; margin: 0;"
        ))
      }
      req(input$connection_type)
      if (isTRUE(connection_valid())) {
        p(
          icon("circle-check"), " Connection configured.",
          style = "color: #2e7d32; font-size: 0.87rem; margin-top: 4px;"
        )
      } else {
        p(
          icon("circle-exclamation"), " Fill in required fields above.",
          style = "color: #c62828; font-size: 0.87rem; margin-top: 4px;"
        )
      }
    })

    # ---- Apply connection on button click -----------------------------------

    # A source switch invalidates the previous attempt's status
    observeEvent(input$connection_type, {
      connection_status(NULL)
    }, ignoreInit = TRUE)

    applied_connection <- reactiveVal(NULL)
    survey_list        <- reactiveVal(NULL)
    variable_list      <- reactiveVal(NULL)
    cpi_ppp            <- reactiveVal(NULL)
    pov_lines          <- reactiveVal(NULL)
    publish_metadata <- function(metadata) {
      survey_list(metadata$survey_list)
      variable_list(metadata$variable_list)
      cpi_ppp(metadata$cpi_ppp)
      pov_lines(metadata$pov_lines)
    }

    # On Posit Connect with env vars set: auto-connect once on startup,
    # no button click or UI input required. Any failure (auth, network,
    # missing volume/metadata) rolls back and surfaces a visible error.
    if (.auto_connect()) {
      observe({
        auto_connect_fail <- function(e) {
          msg <- conditionMessage(e)
          message("[overview] auto-connect to Databricks failed: ", msg)
          # Prevent downstream work after a failed startup connection.
          applied_connection(NULL)
          showNotification(
            paste("Auto-connect to Databricks failed:", msg),
            type = "error", duration = 15
          )
          connection_status(list(
            state   = "error",
            message = "Failed to connect to Databricks.",
            detail  = paste0(
              msg, "\n\nCheck the DATABRICKS_HOST, DATABRICKS_CLIENT_ID, ",
              "DATABRICKS_CLIENT_SECRET and DATABRICKS_VOLUME_PATH environment ",
              "variables configured for this app on Posit Connect, then reload the app."
            )
          ))
        }

        tryCatch({
          params <- build_connection_params("databricks")
          message("[overview] auto-connecting to Databricks (Posit Connect)")

          metadata <- load_overview_metadata(params)
          publish_metadata(metadata)

          # Expose the connection only after metadata succeeds.
          applied_connection(params)
          connection_status(list(
            state = "connected", message = "Connected to Databricks.", detail = NULL
          ))
        }, error = function(e) auto_connect_fail(e))
      }) |> bindEvent(TRUE, once = TRUE)
    }

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
        # DEP-03: configuration is not reachability; verify the folder exists.
        path_ok <- tryCatch({
          params$path <- normalise_local_path(params$path)
          TRUE
        }, error = function(e) {
          showNotification("Please enter a valid folder path.", type = "warning", duration = 4)
          FALSE
        })
        if (!path_ok) return()
        if (!dir.exists(params$path)) {
          connection_status(list(
            state   = "error",
            message = "Local folder not found.",
            detail  = paste0(
              "The path does not exist or is not readable: ", params$path,
              "\nCheck the folder path, then reconnect."
            )
          ))
          showNotification(
            paste("Local folder not found:", params$path),
            type = "error", duration = 8
          )
          return()
        }
        message("[overview] applied local folder: ", params$path)
      } else {
        message("[overview] applied connection: ", params$type)
      }

      # INT-02: clear the previous source before loading the new bundle.
      applied_connection(NULL)
      survey_list(NULL)
      variable_list(NULL)
      cpi_ppp(NULL)
      pov_lines(NULL)

      # ---- Load metadata -----------------------------------------------------

      load_notif <- showNotification(
        "Loading metadata files...", duration = NULL, type = "message"
      )
      on.exit(removeNotification(load_notif), add = TRUE)

      metadata <- tryCatch(
        load_overview_metadata(params),
        error = function(e) e
      )

      if (inherits(metadata, "error")) {
        connection_status(list(
          state   = "error",
          message = paste0(
            "Could not load metadata from the ", params$type, " source."
          ),
          detail  = conditionMessage(metadata)
        ))
        showNotification(
          paste0(
            "Connection failed: metadata could not be loaded from the ",
            params$type, " source. See the status panel for details."
          ),
          type = "error", duration = 10
        )
        return()
      }

      publish_metadata(metadata)

      # Expose the connection only after metadata succeeds.
      applied_connection(params)
      connection_status(list(
        state   = "connected",
        message = paste0(
          "Connected to ", params$type, " data source (metadata verified)."
        ),
        detail  = NULL
      ))
      showNotification(
        paste0("Connected to ", params$type, " data source."),
        type = "message", duration = 3
      )

    }, ignoreInit = TRUE)

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
