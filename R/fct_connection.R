#' Build connection parameters from raw input values
#'
#' One supported auth contract per source (DEP-02):
#' \itemize{
#'   \item local — filesystem path.
#'   \item s3 — HMAC keys: UI fields or AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY.
#'   \item gcs — HMAC (interop) keys: UI fields or GCS_ACCESS_KEY_ID / GCS_SECRET_ACCESS_KEY.
#'   \item azure — storage Account Key, or service principal via
#'     AZURE_CLIENT_ID / AZURE_CLIENT_SECRET / AZURE_TENANT_ID.
#'   \item hf — public repositories only (httpfs reads `hf://` directly).
#'   \item databricks — M2M service principal via environment variables.
#' }
#' Blank UI inputs never shadow environment credentials (`%|||%`).
#'
#' @param type One of "local", "s3", "gcs", "azure", "hf"
#' @param ... Named arguments specific to each type
#' @return A named list of connection parameters
#' @export
build_connection_params <- function(type, ...) {
  args <- list(...)
  # Like %||% but also treats empty strings as missing — needed so blank UI
  # inputs don't shadow .Renviron values
  `%|||%` <- function(a, b) if (!is.null(a) && nzchar(a %||% "")) a else b

  switch(
    type,
    "local" = list(type = "local", path = args$path %||% "data/"),
    "s3"    = list(type = "s3",    bucket = args$s3_bucket %||% "",
                   prefix = args$s3_prefix %||% "", region = args$s3_region %||% "us-east-1",
                   key_id = args$s3_key_id %|||% Sys.getenv("AWS_ACCESS_KEY_ID"),
                   secret = args$s3_secret %|||% Sys.getenv("AWS_SECRET_ACCESS_KEY")),
    "gcs"   = list(type = "gcs",   bucket = args$gcs_bucket  %||% "",
                   prefix = args$gcs_prefix  %||% "",
                   key_id = args$gcs_key_id %|||% Sys.getenv("GCS_ACCESS_KEY_ID"),
                   secret = args$gcs_secret %|||% Sys.getenv("GCS_SECRET_ACCESS_KEY")),
    "azure" = list(type           = "azure",
                   account        = args$azure_account        %||% "",
                   container      = args$azure_container      %||% "",
                   prefix         = args$azure_prefix         %||% "",
                   key            = args$azure_key            %||% "",
                   client_id      = args$azure_client_id      %|||% Sys.getenv("AZURE_CLIENT_ID"),
                   client_secret  = args$azure_client_secret  %|||% Sys.getenv("AZURE_CLIENT_SECRET"),
                   tenant_id      = args$azure_tenant_id      %|||% Sys.getenv("AZURE_TENANT_ID")),
    "hf"    = list(type = "hf",    repo = args$hf_repo   %||% "",
                   subdir = args$hf_subdir %||% ""),
    "databricks" = list(
      type          = "databricks",
      workspace     = args$db_workspace     %|||% Sys.getenv("DATABRICKS_HOST"),
      client_id     = args$db_client_id     %|||% Sys.getenv("DATABRICKS_CLIENT_ID"),
      client_secret = args$db_client_secret %|||% Sys.getenv("DATABRICKS_CLIENT_SECRET"),
      volume_path   = args$db_volume_path   %|||% Sys.getenv("DATABRICKS_VOLUME_PATH")
    ),
    stop("Unknown connection type: ", type)
  )
}

#' Validate a connection params list
#' @param params A named list as returned by `build_connection_params()`
#' @return `TRUE` if valid, `FALSE` otherwise
#' @export
validate_connection_params <- function(params) {
  if (is.null(params) || !is.list(params)) return(FALSE)
  switch(
    params$type,
    "local"      = nzchar(params$path      %||% ""),
    "s3"         = nzchar(params$bucket    %||% ""),
    "gcs"        = nzchar(params$bucket    %||% ""),
    "azure"      = nzchar(params$account   %||% "") && nzchar(params$container %||% ""),
    "hf"         = nzchar(params$repo      %||% ""),
    "databricks" = nzchar(params$workspace     %||% Sys.getenv("DATABRICKS_HOST")          %||% "") &&
                   nzchar(params$client_id     %||% Sys.getenv("DATABRICKS_CLIENT_ID")     %||% "") &&
                   nzchar(params$client_secret %||% Sys.getenv("DATABRICKS_CLIENT_SECRET") %||% "") &&
                   nzchar(params$volume_path   %||% Sys.getenv("DATABRICKS_VOLUME_PATH")   %||% ""),
    FALSE
  )
}

#' Normalise a local path
#' @param path Character string
#' @return Normalised absolute path
#' @export
normalise_local_path <- function(path) {
  p <- trimws(path %||% "")
  if (!nzchar(p)) stop("Path must be a non-empty string.")
  normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
}

#' Default poverty lines data frame
#' @return data.frame with columns ppp_year and ln
#' @export
default_poverty_lines <- function() {
  data.frame(
    ppp_year         = rep(2021, 3),
    ln               = c(3.00, 4.20, 8.30),
    stringsAsFactors = FALSE
  )
}