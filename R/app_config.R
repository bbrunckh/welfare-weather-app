#' Access files in the current app (packaged path)
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "wiseapp")
}

#' Read App Config (always use packaged config)
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE
) {
  config_file <- app_sys("golem-config.yml")
  if (config_file == "") {
    stop("[app_config] packaged golem-config.yml not found. Did you set package name correctly?")
  }
  config::get(value = value, config = config, file = config_file, use_parent = use_parent)
}

# Deployment environment helpers ----

#' Return the configured automatic data source, or `NULL` when unset.
#' @noRd
.data_source <- function() {
  source <- tolower(trimws(Sys.getenv("WISEAPP_DATA_SOURCE", "")))
  if (nzchar(source)) source else NULL
}

#' TRUE when deployed on Posit Connect
#' @noRd
.on_posit_connect <- function() {
  identical(Sys.getenv("RSTUDIO_PRODUCT"), "CONNECT")
}

#' TRUE when the app should automatically load its configured data source.
#' @noRd
.auto_connect <- function() !is.null(.data_source())
