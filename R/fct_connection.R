#' Build connection parameters from raw input values
#' @param type One of "local", "s3", "gcs", "azure", "hf"
#' @param ... Named arguments specific to each type
#' @return A named list of connection parameters
#' @export
build_connection_params <- function(type, ...) {
  args <- list(...)
  switch(
    type,
    "local" = list(type = "local", path = args$path %||% "data/"),
    "s3"    = list(type = "s3",    bucket = args$s3_bucket %||% "",
                   prefix = args$s3_prefix %||% "", region = args$s3_region %||% "us-east-1",
                   key_id = args$s3_key_id %||% "", secret = args$s3_secret %||% ""),
    "gcs"   = list(type = "gcs",   bucket = args$gcs_bucket  %||% "",
                   prefix = args$gcs_prefix  %||% "", keyfile = args$gcs_keyfile %||% ""),
    "azure" = list(type = "azure", account = args$azure_account   %||% "",
                   container = args$azure_container %||% "",
                   prefix = args$azure_prefix %||% "", key = args$azure_key %||% ""),
    "hf"    = list(type = "hf",    repo = args$hf_repo   %||% "",
                   subdir = args$hf_subdir %||% "", token = args$hf_token %||% ""),
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
    "local" = nzchar(params$path %||% ""),
    "s3"    = nzchar(params$bucket %||% ""),
    "gcs"   = nzchar(params$bucket %||% ""),
    "azure" = nzchar(params$account %||% "") && nzchar(params$container %||% ""),
    "hf"    = nzchar(params$repo %||% ""),
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