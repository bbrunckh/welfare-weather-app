#' Load data files lazily via DuckDB
#'
#' A unified data loading function that wraps \code{duckdbfs::open_dataset()} to
#' support local and remote file systems. Returns a lazy \code{dplyr::tbl()}
#' backed by a DuckDB connection — no data is read into memory until
#' \code{dplyr::collect()} is called.
#'
#' Supports \code{.parquet} and \code{.csv} files. File format is detected
#' automatically from the file extension unless \code{format} is specified
#' explicitly.
#'
#' Paths can be supplied as bare filenames (e.g. \code{"survey_list.csv"}) or
#' as full URIs (e.g. \code{"s3://my-bucket/data/survey_list.csv"}). Bare
#' filenames are resolved against \code{connection_params} automatically —
#' full URIs are passed through unchanged.
#'
#' For S3, GCS, and Azure sources, credentials are configured automatically
#' from \code{connection_params} before opening the dataset. Leave credential
#' fields empty to fall back on environment variables or default credential
#' chains.
#'
#' @param paths Character vector of file paths, bare filenames, URIs, or URLs
#'   to load. Bare filenames are resolved using \code{connection_params}.
#'
#' @param connection_params Named list of connection parameters as returned by
#'   \code{mod_0_overview_server()$connection_params}. Must contain at least
#'   \code{$type} (one of \code{"local"}, \code{"s3"}, \code{"gcs"},
#'   \code{"azure"}, \code{"hf"}). Credential fields are optional and default
#'   to environment variables when absent. Defaults to a local connection with
#'   path \code{"/data"}.
#'
#' @param format Character. File format: \code{"parquet"} or \code{"csv"}.
#'   If \code{NULL} (default), format is detected from the file extension of
#'   the first path.
#'
#' @param unify_schemas Logical. If \code{TRUE}, columns are unified by name
#'   across files with differing schemas (\code{union_by_name = TRUE} in
#'   DuckDB). Set to \code{FALSE} (default) when all files share the same
#'   schema for better performance.
#'
#' @param collect Logical. If \code{TRUE}, immediately collect the result into
#'   a \code{tibble}. If \code{FALSE} (default), return a lazy
#'   \code{dplyr::tbl()} for further filtering before collection.
#'
#' @return A lazy \code{dplyr::tbl()} (default) or a \code{tibble} if
#'   \code{collect = TRUE}.
#'
#' @examples
#' \dontrun{
#' params <- list(type = "local", path = "/data")
#'
#' # Bare filename — resolved to /data/survey_list.csv
#' survey_list <- load_data("survey_list.csv", params, collect = TRUE)
#'
#' # Full paths also work
#' tbl <- load_data(c("/data/nga_survey.parquet", "/data/eth_survey.parquet"), params)
#' df  <- dplyr::collect(dplyr::filter(tbl, year >= 2015))
#'
#' # S3 — bare filenames resolved to s3://my-bucket/data/survey_list.csv
#' params_s3 <- list(type = "s3", bucket = "my-bucket", prefix = "data/",
#'                   region = "us-east-1", key_id = "", secret = "")
#' survey_list <- load_data("survey_list.csv", params_s3, collect = TRUE)
#' }
#'
#' @importFrom duckdbfs open_dataset duckdb_s3_config
#' @importFrom dplyr collect
#' @noRd
load_data <- function(
    paths,
    connection_params = list(type = "local", path = "/data"),
    format            = NULL,
    unify_schemas     = FALSE,
    collect           = FALSE
) {

  if (length(paths) == 0) return(tibble::tibble())

  paths <- as.character(paths)
  type  <- connection_params$type %||% "local"

  # ---------------------------------------------------------------------------
  # 1. Resolve bare filenames to full paths / URIs
  #
  # A path is considered "bare" if it contains no URI scheme (://), no leading
  # slash, and no leading drive letter (Windows). Full URIs and absolute paths
  # are passed through unchanged.
  # ---------------------------------------------------------------------------

  is_bare <- function(p) {
    !grepl("://", p, fixed = TRUE) & !grepl("^[/~]|^[A-Za-z]:[/\\\\]", p)
  }

  resolve_path <- function(p) {
    if (!is_bare(p)) return(p)
    switch(
      type,
      "local" = file.path(connection_params$path %||% "data/", p),
      "s3"    = paste0("s3://",  connection_params$bucket,    "/", connection_params$prefix %||% "", p),
      "gcs"   = paste0("gs://",  connection_params$bucket,    "/", connection_params$prefix %||% "", p),
      "azure" = paste0("az://",  connection_params$container, "/", connection_params$prefix %||% "", p),
      "hf"    = paste0("hf://datasets/", connection_params$repo, "/", connection_params$subdir %||% "", p),
      p  # fallback: return unchanged
    )
  }

  paths <- vapply(paths, resolve_path, character(1), USE.NAMES = FALSE)

  # ---------------------------------------------------------------------------
  # 2. Detect format from extension if not supplied
  # ---------------------------------------------------------------------------

  if (is.null(format)) {
    ext    <- tolower(tools::file_ext(paths[1]))
    format <- switch(ext,
      "parquet" = "parquet",
      "csv"     = "csv",
      "tsv"     = "csv",
      "parquet"   # default
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Configure remote credentials if needed
  # ---------------------------------------------------------------------------

  if (type == "s3") {

    duckdbfs::duckdb_s3_config(
      s3_access_key_id     = connection_params$key_id %||% Sys.getenv("AWS_ACCESS_KEY_ID"),
      s3_secret_access_key = connection_params$secret %||% Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      s3_region            = connection_params$region  %||% "us-east-1"
    )

  } else if (type == "gcs") {

    duckdbfs::duckdb_s3_config(
      s3_endpoint          = "storage.googleapis.com",
      s3_access_key_id     = connection_params$key_id %||% Sys.getenv("GCS_ACCESS_KEY_ID"),
      s3_secret_access_key = connection_params$secret  %||% Sys.getenv("GCS_SECRET_ACCESS_KEY"),
      s3_url_style         = "path"
    )

  } else if (type == "azure") {

    key <- connection_params$key %||% Sys.getenv("AZURE_STORAGE_KEY")
    if (nzchar(key)) {
      Sys.setenv(AZURE_STORAGE_ACCOUNT = connection_params$account %||% "")
      Sys.setenv(AZURE_STORAGE_KEY     = key)
    }

  }

  # ---------------------------------------------------------------------------
  # 4. Normalise local paths
  # ---------------------------------------------------------------------------

  if (type == "local") {
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 5. Open dataset lazily
  # ---------------------------------------------------------------------------

  tbl <- tryCatch(
    duckdbfs::open_dataset(
      sources       = paths,
      format        = format,
      unify_schemas = unify_schemas
    ),
    error = function(e) {
      stop(sprintf(
        "load_data() failed to open dataset.\n  paths: %s\n  format: %s\n  error: %s",
        paste(head(paths, 3), collapse = ", "),
        format,
        conditionMessage(e)
      ))
    }
  )

  # ---------------------------------------------------------------------------
  # 6. Collect or return lazy
  # ---------------------------------------------------------------------------

  if (collect) dplyr::collect(tbl) else tbl
}