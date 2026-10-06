#' Build connection parameters from raw input values
#'
#' One supported auth contract per source (DEP-02):
#' \itemize{
#'   \item local - filesystem path.
#'   \item s3 - HMAC keys: UI fields or AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY.
#'   \item gcs - HMAC (interop) keys: UI fields or GCS_ACCESS_KEY_ID / GCS_SECRET_ACCESS_KEY.
#'   \item azure - storage Account Key, or service principal via
#'     AZURE_CLIENT_ID / AZURE_CLIENT_SECRET / AZURE_TENANT_ID.
#'   \item hf - public repositories only (httpfs reads `hf://` directly).
#'   \item databricks - M2M service principal via environment variables.
#' }
#' A connection is taken entirely from the supplied (UI) values or entirely
#' from the environment, never mixed field by field (CR-SEC-01): when every
#' field for the type is blank the environment configuration is used,
#' otherwise blank fields stay blank. The result records this in `$origin`
#' ("ui" or "env"); loaders never fill a missing "ui" field from the
#' environment.
#'
#' @param type One of "local", "s3", "gcs", "azure", "hf", "databricks"
#' @param ... Named arguments specific to each type
#' @return A named list of connection parameters
#' @export
build_connection_params <- function(type, ...) {
  type <- .check_connection_type(type)
  args <- list(...)
  fields <- .CONNECTION_FIELDS[[type]]
  ui <- lapply(fields, function(f) {
    value <- args[[f[["arg"]]]]
    if (is.character(value) && length(value) == 1L && !is.na(value)) value else ""
  })
  from_ui <- any(vapply(ui, nzchar, logical(1)))
  values <- if (from_ui) ui else lapply(fields, function(f) Sys.getenv(f[["env"]]))
  if (identical(type, "s3") && !nzchar(values$region)) values$region <- "us-east-1"
  c(list(type = type), values, list(origin = if (from_ui) "ui" else "env"))
}

# Supported connection types (allowlist).
.CONNECTION_TYPES <- c("local", "s3", "gcs", "azure", "hf", "databricks")

# Per type: parameter field -> UI argument name and environment variable.
.CONNECTION_FIELDS <- list(
  local = list(path = c(arg = "path", env = "WISEAPP_DATA_PATH")),
  s3 = list(
    bucket = c(arg = "s3_bucket", env = "S3_BUCKET"),
    prefix = c(arg = "s3_prefix", env = "S3_PREFIX"),
    region = c(arg = "s3_region", env = "S3_REGION"),
    key_id = c(arg = "s3_key_id", env = "AWS_ACCESS_KEY_ID"),
    secret = c(arg = "s3_secret", env = "AWS_SECRET_ACCESS_KEY")
  ),
  gcs = list(
    bucket = c(arg = "gcs_bucket", env = "GCS_BUCKET"),
    prefix = c(arg = "gcs_prefix", env = "GCS_PREFIX"),
    key_id = c(arg = "gcs_key_id", env = "GCS_ACCESS_KEY_ID"),
    secret = c(arg = "gcs_secret", env = "GCS_SECRET_ACCESS_KEY")
  ),
  azure = list(
    account = c(arg = "azure_account", env = "AZURE_STORAGE_ACCOUNT"),
    container = c(arg = "azure_container", env = "AZURE_STORAGE_CONTAINER"),
    prefix = c(arg = "azure_prefix", env = "AZURE_STORAGE_PREFIX"),
    key = c(arg = "azure_key", env = "AZURE_STORAGE_KEY"),
    client_id = c(arg = "azure_client_id", env = "AZURE_CLIENT_ID"),
    client_secret = c(arg = "azure_client_secret", env = "AZURE_CLIENT_SECRET"),
    tenant_id = c(arg = "azure_tenant_id", env = "AZURE_TENANT_ID")
  ),
  hf = list(
    repo = c(arg = "hf_repo", env = "HF_REPO"),
    subdir = c(arg = "hf_subdir", env = "HF_SUBDIR")
  ),
  databricks = list(
    workspace = c(arg = "db_workspace", env = "DATABRICKS_HOST"),
    client_id = c(arg = "db_client_id", env = "DATABRICKS_CLIENT_ID"),
    client_secret = c(arg = "db_client_secret", env = "DATABRICKS_CLIENT_SECRET"),
    volume_path = c(arg = "db_volume_path", env = "DATABRICKS_VOLUME_PATH")
  )
)

#' Check a connection type against the allowlist.
#' @noRd
.check_connection_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !type %in% .CONNECTION_TYPES) {
    stop("Unknown connection type: ", paste(format(type), collapse = ", "),
         call. = FALSE)
  }
  type
}

#' Read one connection field. Only environment-origin params fall back to the
#' environment; a "ui" connection never borrows server configuration.
#' @noRd
.connection_field <- function(params, name, env_var) {
  value <- params[[name]]
  if (identical(params$origin, "ui")) return(value %||% "")
  value %||% Sys.getenv(env_var)
}

# Databricks workspace domains accepted for a host that is not the configured
# DATABRICKS_HOST.
.DATABRICKS_HOST_SUFFIXES <- c(
  ".cloud.databricks.com", ".azuredatabricks.net", ".gcp.databricks.com"
)

#' Validate a Databricks workspace URL.
#'
#' A host identical to the configured `DATABRICKS_HOST` is accepted as
#' operator configuration. Any other host must be `https://<name>` with no user
#' info, port or path, and end in a Databricks workspace domain.
#' @return The host without trailing slashes ("" when unset, left to the
#'   caller's missing-configuration error); errors when not allowed.
#' @noRd
.validate_databricks_host <- function(host) {
  host <- sub("/+$", "", trimws(host %||% ""))
  configured <- sub("/+$", "", trimws(Sys.getenv("DATABRICKS_HOST")))
  if (!nzchar(host) || identical(host, configured)) return(host)
  name <- tolower(sub("^https://", "", host))
  ok <- grepl("^https://[A-Za-z0-9.-]+$", host) &&
    !startsWith(name, ".") &&
    any(endsWith(name, .DATABRICKS_HOST_SUFFIXES))
  if (!ok) {
    stop(
      "Databricks workspace must be an https:// URL on a Databricks domain ",
      "(no user info, port or path) or the configured DATABRICKS_HOST.",
      call. = FALSE
    )
  }
  host
}

#' Build connection parameters from the configured automatic data source
#'
#' `WISEAPP_DATA_SOURCE` selects the source; provider-specific fields are read
#' from their standard environment variable names by `build_connection_params`.
#'
#' @return A named connection parameter list.
#' @noRd
auto_connection_params <- function() {
  source <- tolower(trimws(Sys.getenv("WISEAPP_DATA_SOURCE", "")))
  if (!nzchar(source)) {
    return(NULL)
  }
  build_connection_params(source)
}

#' Validate a connection params list
#' @param params A named list as returned by `build_connection_params()`
#' @return `TRUE` if valid, `FALSE` otherwise
#' @export
validate_connection_params <- function(params) {
  if (is.null(params) || !is.list(params) || length(params$type) != 1L ||
      !isTRUE(params$type %in% .CONNECTION_TYPES)) {
    return(FALSE)
  }
  field <- function(name, env_var) .connection_field(params, name, env_var)
  switch(params$type,
    "local" = nzchar(field("path", "WISEAPP_DATA_PATH")),
    "s3" = nzchar(params$bucket %||% ""),
    "gcs" = nzchar(params$bucket %||% ""),
    "azure" = nzchar(params$account %||% "") && nzchar(params$container %||% ""),
    "hf" = nzchar(params$repo %||% ""),
    "databricks" = nzchar(field("workspace", "DATABRICKS_HOST")) &&
      nzchar(field("client_id", "DATABRICKS_CLIENT_ID")) &&
      nzchar(field("client_secret", "DATABRICKS_CLIENT_SECRET")) &&
      nzchar(field("volume_path", "DATABRICKS_VOLUME_PATH")) &&
      !inherits(tryCatch(
        .validate_databricks_host(field("workspace", "DATABRICKS_HOST")),
        error = identity
      ), "error"),
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
