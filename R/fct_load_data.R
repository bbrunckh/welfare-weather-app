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
#' For Databricks Unity Catalog, use \code{type = "databricks"} with
#' \code{workspace}, \code{token}, \code{catalog}, \code{schema}, and
#' optionally \code{prefix} in \code{connection_params}. Bare table names are
#' resolved to \code{s3://|abfss://} Delta table paths via the Unity Catalog
#' external location, or passed as \code{delta_scan('...')} directly.
#'
#' @param paths Character vector of file paths, bare filenames, URIs, URLs,
#'   or Delta table paths to load. For Databricks, bare names are resolved
#'   to \code{<catalog>.<schema>.<table>} Delta paths automatically.
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
      "local"      = file.path(connection_params$path %||% "data/", p),
      "s3"         = paste0("s3://",  connection_params$bucket,    "/", connection_params$prefix %||% "", p),
      "gcs"        = paste0("gs://",  connection_params$bucket,    "/", connection_params$prefix %||% "", p),
      "azure"      = paste0("abfss://", connection_params$container, "@",
                             connection_params$account, ".dfs.core.windows.net/",
                             connection_params$prefix %||% "", p),
      "hf"         = paste0("hf://datasets/", connection_params$repo, "/", connection_params$subdir %||% "", p),
      "databricks" = paste0(
        connection_params$catalog %||% "main", ".",
        connection_params$schema  %||% "default", ".",
        p
      ),
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

    con           <- duckdbfs:::cached_connection()
    DBI::dbExecute(con, "INSTALL azure; LOAD azure;")
    DBI::dbExecute(con, "INSTALL delta;  LOAD delta;")

    key           <- connection_params$key           %||% Sys.getenv("AZURE_STORAGE_KEY")
    client_id     <- connection_params$client_id     %||% Sys.getenv("AZURE_CLIENT_ID")
    client_secret <- connection_params$client_secret %||% Sys.getenv("AZURE_CLIENT_SECRET")
    tenant_id     <- connection_params$tenant_id     %||% Sys.getenv("AZURE_TENANT_ID")

    if (nzchar(key)) {
      DBI::dbExecute(con, sprintf("
        CREATE OR REPLACE SECRET azure_secret (
          TYPE AZURE,
          CONNECTION_STRING 'AccountName=%s;AccountKey=%s'
        );", connection_params$account %||% "", key))
    } else if (nzchar(client_id) && nzchar(client_secret) && nzchar(tenant_id)) {
      DBI::dbExecute(con, sprintf("
        CREATE OR REPLACE SECRET azure_secret (
          TYPE AZURE,
          PROVIDER SERVICE_PRINCIPAL,
          TENANT_ID '%s',
          CLIENT_ID '%s',
          CLIENT_SECRET '%s'
        );", tenant_id, client_id, client_secret))
    } else {
      # No CLI available — try managed identity / workload identity (Azure VMs/AKS).
      # On a local work laptop without az CLI, supply credentials explicitly instead.
      tryCatch(
        DBI::dbExecute(con, "
          CREATE OR REPLACE SECRET azure_secret (
            TYPE AZURE,
            PROVIDER CREDENTIAL_CHAIN,
            CHAIN 'managed_identity;workload_identity'
          );"),
        error = function(e) {
          stop(
            "load_data(): No Azure credentials found. Provide one of:\n",
            "  1. Account key         : set azure_key in the connection panel, ",
            "or AZURE_STORAGE_KEY in .Renviron\n",
            "  2. SAS token           : set azure_key to a SAS token (?sv=...) ",
            "in the connection panel\n",
            "  3. Service principal   : AZURE_CLIENT_ID + AZURE_CLIENT_SECRET + ",
            "AZURE_TENANT_ID in .Renviron\n",
            "  (Azure CLI not available on this machine)"
          )
        }
      )
    }

  } else if (type == "databricks") {

    # Databricks uses its S3-compatible or ADLS endpoint exposed via Unity
    # Catalog external locations. We connect via the Databricks SQL connector
    # using a personal access token, then read Delta tables with delta_scan().
    #
    # Credentials are read from connection_params or environment variables:
    #   DATABRICKS_HOST  — e.g. "https://adb-123456.azuredatabricks.net"
    #   DATABRICKS_TOKEN — personal access token (dapi...)

    host  <- connection_params$workspace %||% Sys.getenv("DATABRICKS_HOST")
    token <- connection_params$token     %||% Sys.getenv("DATABRICKS_TOKEN")

    if (!nzchar(host) || !nzchar(token)) {
      stop(
        "load_data(): Databricks requires `workspace` and `token` in ",
        "connection_params, or DATABRICKS_HOST / DATABRICKS_TOKEN env vars."
      )
    }

    # Configure DuckDB's built-in Databricks connector (requires delta + httpfs
    # extensions). This sets up the Unity Catalog credential chain so that
    # delta_scan() can resolve <catalog>.<schema>.<table> paths directly.
    con <- duckdbfs:::get_default_connection()
    DBI::dbExecute(con, "INSTALL delta;   LOAD delta;")
    DBI::dbExecute(con, "INSTALL httpfs;  LOAD httpfs;")
    DBI::dbExecute(con, sprintf(
      "CREATE SECRET IF NOT EXISTS __databricks (
         TYPE        DATABRICKS,
         token       '%s',
         workspace   '%s'
       );",
      token, host
    ))

    # For Databricks the path IS the query — delta_scan('<catalog>.<schema>.<table>')
    # open_dataset() is bypassed; return a DBI result wrapped in a tbl instead.
    tbl <- tryCatch({
      scan_sql <- sprintf(
        "SELECT * FROM delta_scan('%s')",
        paste(paths, collapse = "', '")
      )
      dplyr::tbl(con, dbplyr::sql(scan_sql))
    }, error = function(e) {
      stop(sprintf(
        "load_data() failed to open Databricks Delta table.\n  paths: %s\n  error: %s",
        paste(head(paths, 3), collapse = ", "),
        conditionMessage(e)
      ))
    })

    if (collect) return(dplyr::collect(tbl)) else return(tbl)

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