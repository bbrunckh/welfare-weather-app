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

# Resource limits ----

#' Read a positive integer from an environment variable; `NA` when unset or invalid.
#' @noRd
.env_positive_int <- function(name) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, "")))
  if (is.na(value) || value < 1L) NA_integer_ else value
}

#' Cap the compute threads of fixest and collapse in this process.
#'
#' Controlled by `WISEAPP_THREADS`. Unset leaves the package defaults, which use
#' every core; set it on a shared host (for example Posit Connect with several
#' processes) so processes do not oversubscribe the CPU. DuckDB is limited
#' separately by `WISEAPP_DUCKDB_THREADS`.
#' @noRd
.wise_apply_thread_limits <- function() {
  n <- .env_positive_int("WISEAPP_THREADS")
  if (is.na(n)) return(invisible(FALSE))
  fixest::setFixest_nthreads(n)
  collapse::set_collapse(nthreads = n)
  invisible(TRUE)
}
