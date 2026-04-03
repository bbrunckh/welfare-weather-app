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

# ---- Deployment environment helpers ------------------------------------

#' TRUE when all four Databricks env vars are present
#' @noRd
.databricks_env_configured <- function() {
  nzchar(Sys.getenv("DATABRICKS_HOST"))          &&
  nzchar(Sys.getenv("DATABRICKS_CLIENT_ID"))     &&
  nzchar(Sys.getenv("DATABRICKS_CLIENT_SECRET")) &&
  nzchar(Sys.getenv("DATABRICKS_VOLUME_PATH"))
}

#' TRUE when deployed on Posit Connect
#' @noRd
.on_posit_connect <- function() {
  identical(Sys.getenv("RSTUDIO_PRODUCT"), "CONNECT")
}

#' TRUE when app should auto-connect to Databricks silently
#' (Posit Connect deployment with all env vars set)
#' @noRd
.auto_connect <- function() .on_posit_connect() && .databricks_env_configured()