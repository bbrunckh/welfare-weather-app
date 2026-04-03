# =============================================================================
# load_data.R
# =============================================================================
# Unified lazy data loader backed by DuckDB directly (no duckdbfs dependency).
#
# Design notes:
#   - A single DuckDB connection is owned at module level and initialised once
#     per R process.  This is safe on Posit Connect's default multi-process
#     model because each process is single-threaded from Shiny's perspective.
#   - Credentials, extensions, and OAuth tokens are cached in a single .duck
#     environment to avoid redundant network calls and SQL round-trips.
#   - httr2 is used throughout for HTTP (replaces httr).
#   - duckdbfs is no longer required.
# =============================================================================


# -----------------------------------------------------------------------------
# Module-level state
# -----------------------------------------------------------------------------

.duck <- new.env(parent = emptyenv())
# .duck$con          — the live DuckDB connection (set by .duck_con())
# .duck$extensions   — character vector of already-loaded extensions
# .duck$db_tokens    — named list: params_hash -> list(token, expires_at)
# .duck$db_secrets   — named list: params_hash -> token currently written as secret


#' Return (and if necessary initialise) the module-level DuckDB connection.
#'
#' Safe to call multiple times — returns the existing connection if it is still
#' valid, and creates a fresh one if the connection has been closed or the
#' process has just started.
#'
#' @return A DBI connection to an in-process DuckDB instance.
#' @noRd
.duck_con <- function() {
  if (!is.null(.duck$con) && DBI::dbIsValid(.duck$con)) return(.duck$con)
  .duck$con        <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  .duck$extensions <- character(0)
  .duck$db_tokens  <- list()
  .duck$db_secrets <- list()
  .duck$con
}

#' Install and load a DuckDB extension exactly once per process.
#'
#' On Posit Connect, fetches the binary from Databricks Files API to bypass
#' the unavailable extensions.duckdb.org endpoint. Locally, uses the standard
#' DuckDB INSTALL mechanism.
#'
#' @param ext        Extension name e.g. "httpfs", "spatial", "h3".
#' @param db_token   Bearer token — only required on Posit Connect.
#' @param ext_base_url Databricks Files API URL to the folder containing
#'   pre-uploaded .duckdb_extension binaries. Only required on Posit Connect.
#' @noRd
.duck_load_ext <- function(ext, db_token = NULL, ext_base_url = NULL) {
  if (ext %in% (.duck$extensions %||% character(0))) return(invisible(NULL))
  con <- .duck_con()

  if (.auto_connect()) {
    # Check for a bundled binary first (avoids any network call)
    bundled <- system.file(
      paste0("duckdb_extensions/", ext, ".duckdb_extension"),
      package = "wiseapp"
    )

    if (nzchar(bundled)) {
      DBI::dbExecute(con, sprintf("INSTALL '%s';", bundled))
      DBI::dbExecute(con, sprintf("LOAD '%s';",    ext))
    } 

  } else {
    # Local: load from cache, otherwise install from network if missing
      .core_extensions <- c("azure",  "delta", "httpfs", "spatial")
      tryCatch(
      DBI::dbExecute(con, sprintf("LOAD '%s';", ext)),
      error = function(e) {
        if (ext %in% .core_extensions) {
          DBI::dbExecute(con, sprintf("INSTALL '%s'; LOAD '%s';", ext, ext))
        } else {
          DBI::dbExecute(con, sprintf(
            "INSTALL '%s' FROM community; LOAD '%s';", ext, ext
          ))
        }
      }
    )
  }

  .duck$extensions <- c(.duck$extensions, ext)
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Credential helpers
# -----------------------------------------------------------------------------

#' Obtain a Databricks M2M OAuth token, reusing a cached one when valid.
#'
#' Tokens are cached keyed on a hash of (host, client_id, client_secret) and
#' reused until fewer than 5 minutes of lifetime remain.
#'
#' @param host          Databricks workspace URL.
#' @param client_id     OAuth2 client ID.
#' @param client_secret OAuth2 client secret.
#' @return Character scalar — bearer token.
#' @noRd
.get_db_token <- function(host, client_id, client_secret) {
  key    <- paste(host, client_id, client_secret, sep = "\n")
  cached <- .duck$db_tokens[[key]]

  if (!is.null(cached) &&
      difftime(cached$expires_at, Sys.time(), units = "secs") > 300) {
    return(cached$token)
  }

  resp <- httr2::request(paste0(host, "/oidc/v1/token")) |>
    httr2::req_auth_basic(client_id, client_secret) |>
    httr2::req_body_form(grant_type = "client_credentials", scope = "all-apis") |>
    httr2::req_options(http_version = 2L) |> 
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    stop("load_data(): Failed to obtain Databricks OAuth token: ",
         httr2::resp_status_desc(resp))
  }

  parsed <- httr2::resp_body_json(resp)

  .duck$db_tokens[[key]] <- list(
    token      = parsed$access_token,
    expires_at = Sys.time() + as.numeric(parsed$expires_in %||% 3600)
  )
  parsed$access_token
}


#' Register a Databricks bearer token as a DuckDB HTTP secret.
#'
#' Secrets are namespaced by a hash of the connection params so mixed
#' credentials within a process do not clobber each other.  The SQL is only
#' executed when the token has changed.
#'
#' @param con         DBI connection.
#' @param db_token    Character scalar — bearer token.
#' @param params_hash Character scalar — key identifying this credential set.
#' @noRd
.register_db_secret <- function(con, db_token, params_hash) {
  secret_name <- paste0("db_http_", params_hash)
  if (!identical(.duck$db_secrets[[params_hash]], db_token)) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE SECRET %s (TYPE http, BEARER_TOKEN '%s');",
      secret_name, db_token
    ))
    .duck$db_secrets[[params_hash]] <- db_token
  }
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# DuckDB SQL builder
# -----------------------------------------------------------------------------

#' Build a DuckDB read expression for a set of paths.
#'
#' @param paths         Character vector of resolved paths / URIs.
#' @param format        "parquet" or "csv".
#' @param unify_schemas Logical — pass union_by_name = true.
#' @return Character scalar — SQL fragment for use in a FROM clause.
#' @noRd
.build_read_expr <- function(paths, format, unify_schemas) {
  path_sql <- paste0(
    "[", paste0("'", gsub("'", "''", paths), "'", collapse = ", "), "]"
  )
  union_arg <- if (unify_schemas) ", union_by_name = true" else ""

  switch(format,
    parquet = sprintf("read_parquet(%s%s)",   path_sql, union_arg),
    csv     = sprintf("read_csv_auto(%s%s)",  path_sql, union_arg),
    stop("load_data(): Unsupported format '", format, "'. Use 'parquet' or 'csv'.")
  )
}


# -----------------------------------------------------------------------------
# Fast path: small CSV via Databricks Files REST API
# -----------------------------------------------------------------------------

#' Fetch a single CSV from the Databricks Files API directly.
#'
#' For small files the overhead of opening a DuckDB dataset over HTTP exceeds
#' the actual transfer time.  A direct GET + readr::read_csv is faster.
#'
#' @param url   Full Databricks Files API URL.
#' @param token Bearer token.
#' @return A tibble.
#' @noRd
.fetch_db_csv_direct <- function(url, token) {
  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_options(http_version = 2L) |> 
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    stop("load_data(): Failed to fetch CSV from Databricks (", url, "): ",
         httr2::resp_status_desc(resp))
  }

  readr::read_csv(httr2::resp_body_raw(resp), show_col_types = FALSE)
}



# -----------------------------------------------------------------------------
# Main function
# -----------------------------------------------------------------------------

#' Load data files lazily via DuckDB
#'
#' A unified data loading function that opens local or remote datasets via a
#' module-level DuckDB connection. Returns a lazy \code{dplyr::tbl()} — no
#' data is read into memory until \code{dplyr::collect()} is called.
#'
#' Supports \code{.parquet} and \code{.csv} files. Format is detected from the
#' file extension unless \code{format} is specified explicitly.
#'
#' Paths can be bare filenames resolved against \code{connection_params}, or
#' full URIs passed through unchanged.
#'
#' @param paths Character vector of file paths, bare filenames, or URIs.
#'
#' @param connection_params Named list. Must contain \code{$type}: one of
#'   \code{"local"}, \code{"s3"}, \code{"gcs"}, \code{"azure"},
#'   \code{"hf"}, \code{"databricks"}. Credential fields are optional and fall
#'   back to environment variables. Defaults to
#'   \code{list(type = "local", path = "/data")}.
#'
#' @param format \code{"parquet"} or \code{"csv"}. Detected from extension
#'   when \code{NULL} (default).
#'
#' @param unify_schemas Logical. Unify columns across files with differing
#'   schemas (\code{union_by_name}). Default \code{FALSE}.
#'
#' @param collect Logical. Collect into a \code{tibble} immediately.
#'   Default \code{FALSE}.
#'
#' @return A lazy \code{dplyr::tbl()} or a \code{tibble} when
#'   \code{collect = TRUE}.
#'
#' @examples
#' \dontrun{
#' params <- list(type = "local", path = "/data")
#'
#' # Bare filename — resolved to /data/survey_list.csv
#' survey_list <- load_data("survey_list.csv", params, collect = TRUE)
#'
#' # Lazy — filter before collecting
#' tbl <- load_data(c("/data/nga_survey.parquet", "/data/eth_survey.parquet"), params)
#' df  <- dplyr::collect(dplyr::filter(tbl, year >= 2015))
#'
#' # S3 — bare filenames resolved to s3://my-bucket/data/survey_list.csv
#' params_s3 <- list(type = "s3", bucket = "my-bucket", prefix = "data/",
#'                   region = "us-east-1", key_id = "", secret = "")
#' survey_list <- load_data("survey_list.csv", params_s3, collect = TRUE)
#' }
#'
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
  con   <- .duck_con()

  # ---------------------------------------------------------------------------
  # 1. Detect format early — fail before any network calls
  # ---------------------------------------------------------------------------

  if (is.null(format)) {
    ext    <- tolower(tools::file_ext(paths[1]))
    format <- switch(ext,
      "parquet" = "parquet",
      "csv"     = "csv",
      "tsv"     = "csv",
      "parquet"           # default
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Resolve bare filenames to full paths / URIs
  # ---------------------------------------------------------------------------

  is_bare <- function(p) {
    !grepl("://",                      p, fixed = TRUE) &
    !grepl("^[/~]|^[A-Za-z]:[/\\\\]", p)               &
    !grepl("^https?://",               p)
  }

  resolve_path <- function(p) {
    if (!is_bare(p)) return(p)
    switch(type,
      "local"      = file.path(connection_params$path %||% "data/", p),
      "s3"         = paste0("s3://",  connection_params$bucket, "/",
                             connection_params$prefix %||% "", p),
      "gcs"        = paste0("gs://",  connection_params$bucket, "/",
                             connection_params$prefix %||% "", p),
      "azure"      = paste0("abfss://", connection_params$container, "@",
                             connection_params$account,
                             ".dfs.core.windows.net/",
                             connection_params$prefix %||% "", p),
      "hf"         = paste0("hf://datasets/", connection_params$repo, "/",
                             connection_params$subdir %||% "", p),
      "databricks" = {
        host     <- connection_params$workspace   %||% Sys.getenv("DATABRICKS_HOST")
        vol_path <- connection_params$volume_path %||% Sys.getenv("DATABRICKS_VOLUME_PATH")
        if (!nzchar(vol_path %||% "")) stop(
          "load_data(): Set DATABRICKS_VOLUME_PATH in .Renviron:\n",
          "  DATABRICKS_VOLUME_PATH=/Volumes/catalog/schema/volume/path"
        )
        paste0(host, "/api/2.0/fs/files", sub("/$", "", vol_path), "/", p)
      },
      p
    )
  }

  paths <- vapply(paths, resolve_path, character(1), USE.NAMES = FALSE)

  # ---------------------------------------------------------------------------
  # 3. Configure credentials / load extensions per backend
  # ---------------------------------------------------------------------------

  if (type == "s3") {

    .duck_load_ext("httpfs")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE SECRET s3_secret (
         TYPE   S3,
         KEY_ID '%s',
         SECRET '%s',
         REGION '%s'
       );",
      connection_params$key_id %||% Sys.getenv("AWS_ACCESS_KEY_ID"),
      connection_params$secret %||% Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      connection_params$region  %||% "us-east-1"
    ))

  } else if (type == "gcs") {

    .duck_load_ext("httpfs")
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE SECRET gcs_secret (
         TYPE   GCS,
         KEY_ID '%s',
         SECRET '%s'
       );",
      connection_params$key_id %||% Sys.getenv("GCS_ACCESS_KEY_ID"),
      connection_params$secret  %||% Sys.getenv("GCS_SECRET_ACCESS_KEY")
    ))

  } else if (type == "azure") {

    .duck_load_ext("azure")
    .duck_load_ext("delta")

    key           <- connection_params$key           %||% Sys.getenv("AZURE_STORAGE_KEY")
    client_id     <- connection_params$client_id     %||% Sys.getenv("AZURE_CLIENT_ID")
    client_secret <- connection_params$client_secret %||% Sys.getenv("AZURE_CLIENT_SECRET")
    tenant_id     <- connection_params$tenant_id     %||% Sys.getenv("AZURE_TENANT_ID")

    if (nzchar(key)) {
      DBI::dbExecute(con, sprintf(
        "CREATE OR REPLACE SECRET azure_secret (
           TYPE              AZURE,
           CONNECTION_STRING 'AccountName=%s;AccountKey=%s'
         );",
        connection_params$account %||% "", key
      ))
    } else if (nzchar(client_id) && nzchar(client_secret) && nzchar(tenant_id)) {
      DBI::dbExecute(con, sprintf(
        "CREATE OR REPLACE SECRET azure_secret (
           TYPE          AZURE,
           PROVIDER      SERVICE_PRINCIPAL,
           TENANT_ID     '%s',
           CLIENT_ID     '%s',
           CLIENT_SECRET '%s'
         );",
        tenant_id, client_id, client_secret
      ))
    } else {
      tryCatch(
        DBI::dbExecute(con,
          "CREATE OR REPLACE SECRET azure_secret (
             TYPE     AZURE,
             PROVIDER CREDENTIAL_CHAIN,
             CHAIN    'managed_identity;workload_identity'
           );"
        ),
        error = function(e) stop(
          "load_data(): No Azure credentials found. Provide one of:\n",
          "  1. Account key     : AZURE_STORAGE_KEY\n",
          "  2. Service principal: AZURE_CLIENT_ID + AZURE_CLIENT_SECRET + AZURE_TENANT_ID\n"
        )
      )
    }

    } else if (type == "databricks") {

    host          <- connection_params$workspace     %||% Sys.getenv("DATABRICKS_HOST")
    client_id     <- connection_params$client_id     %||% Sys.getenv("DATABRICKS_CLIENT_ID")
    client_secret <- connection_params$client_secret %||% Sys.getenv("DATABRICKS_CLIENT_SECRET")

    if (!nzchar(host) || !nzchar(client_id) || !nzchar(client_secret)) stop(
      "load_data(): Databricks requires DATABRICKS_HOST, DATABRICKS_CLIENT_ID, ",
      "DATABRICKS_CLIENT_SECRET.\nSet via usethis::edit_r_environ()"
    )

    db_token    <- .get_db_token(host, client_id, client_secret)
    params_hash <- substr(digest::digest(list(host, client_id)), 1, 8)

    # Fast path: single CSV — bypass DuckDB entirely
    if (format == "csv" && length(paths) == 1L) {
      return(.fetch_db_csv_direct(paths, db_token))
    }

    # Build the extensions base URL — points to the folder you uploaded binaries to
    vol_path    <- connection_params$volume_path %||% Sys.getenv("DATABRICKS_VOLUME_PATH")
    ext_base_url <- paste0(
      host, "/api/2.0/fs/files",
      sub("/$", "", vol_path), "/duckdb_extensions"
    )

    .duck_load_ext("httpfs",  db_token, ext_base_url)
    .register_db_secret(con, db_token, params_hash)

  }

  # ---------------------------------------------------------------------------
  # 4. Normalise local paths
  # ---------------------------------------------------------------------------

  if (type == "local") {
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 5. Build the read expression and expose as a lazy tbl via a view
  # ---------------------------------------------------------------------------

  read_expr <- .build_read_expr(paths, format, unify_schemas)
  view_name <- paste0("_ld_", substr(digest::digest(read_expr), 1, 12))

  tryCatch(
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s;",
      view_name, read_expr
    )),
    error = function(e) stop(sprintf(
      "load_data(): Failed to open dataset.\n  paths : %s\n  format: %s\n  error : %s",
      paste(head(paths, 3), collapse = ", "), format, conditionMessage(e)
    ))
  )

  tbl <- dplyr::tbl(con, view_name)

  # ---------------------------------------------------------------------------
  # 6. Collect or return lazy
  # ---------------------------------------------------------------------------

  if (collect) dplyr::collect(tbl) else tbl
}