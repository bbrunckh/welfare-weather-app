# Data loading ----
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


# Module-level state ----

.duck <- new.env(parent = emptyenv())
# .duck$con          - the live DuckDB connection (set by .duck_con())
# .duck$extensions   - character vector of already-loaded extensions
# .duck$db_tokens    - named list: params_hash -> list(token, expires_at)
# .duck$db_secrets   - named list: params_hash / secret name -> credential digest
# .duck$active_sessions - number of root Shiny sessions using this process


#' Register the root Shiny session that owns this process's shared data state.
#'
#' The DuckDB connection is process-wide, so it must not be closed when one of
#' several concurrent sessions ends. Cleanup is deferred until the last root
#' session has ended; this also handles repeated local `run_app()` sessions.
#'
#' @param session A Shiny session object.
#' @noRd
.duck_register_session <- function(session) {
  if (is.null(session) || isTRUE(session$userData$wise_duck_registered)) {
    return(invisible(FALSE))
  }

  .duck$active_sessions <- as.integer(.duck$active_sessions %||% 0L) + 1L
  session$userData$wise_duck_registered <- TRUE
  session$userData$wise_duck_released <- FALSE
  session$onSessionEnded(function() {
    if (isTRUE(session$userData$wise_duck_released)) {
      return(invisible(FALSE))
    }
    session$userData$wise_duck_released <- TRUE
    .duck_release_session()
  })
  invisible(TRUE)
}


#' Release one root Shiny session and clean up when the process is idle.
#'
#' @noRd
.duck_release_session <- function() {
  active <- max(0L, as.integer(.duck$active_sessions %||% 1L) - 1L)
  .duck$active_sessions <- active
  if (active > 0L) {
    return(invisible(FALSE))
  }

  con <- .duck$con
  .duck$con <- NULL
  .duck$extensions <- character(0)
  .duck$db_tokens <- list()
  .duck$db_secrets <- list()
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }
  invisible(TRUE)
}


# Canonical identity columns used before the all-column tie-break. The fallback
# is required because not every supported survey level has a unique ID column.
.DETERMINISTIC_ORDER_KEYS <- c(
  "code", "year", "survname", "source", "loc_id",
  "hhid", "fid", "pid", "timestamp", "int_year", "int_month", "h3"
)

#' Collect a table using a stable, backend-independent row order
#'
#' DuckDB does not guarantee scan order, and the Databricks single-CSV path
#' bypasses DuckDB entirely. Collect first, then sort canonical identity fields
#' followed by every remaining scalar column so both paths have one ordering
#' contract. Completely identical rows may remain tied without affecting any
#' downstream result.
#'
#' @param data A data frame or lazy dplyr table.
#' @param keys Optional preferred ordering columns.
#'
#' @return A data frame with deterministic row order.
#' @noRd
collect_deterministic <- function(data, keys = NULL) {
  out <- if (inherits(data, "data.frame")) data else dplyr::collect(data)
  if (nrow(out) < 2L || ncol(out) == 0L) {
    return(out)
  }

  preferred <- unique(c(keys %||% character(0), .DETERMINISTIC_ORDER_KEYS))
  fallback <- sort(setdiff(names(out), preferred), method = "radix")
  order_cols <- c(intersect(preferred, names(out)), fallback)
  order_cols <- order_cols[vapply(out[order_cols], function(x) {
    is.atomic(x) && is.null(dim(x))
  }, logical(1))]

  if (length(order_cols) == 0L) {
    return(out)
  }
  dplyr::arrange(out, !!!rlang::syms(order_cols), .locale = "C")
}


# Shared path and Databricks HTTP helpers ----

.is_bare_data_path <- function(path) {
  !grepl("://", path, fixed = TRUE) &&
    !grepl("^[/~]|^[A-Za-z]:[/\\\\]", path)
}


.resolve_data_path <- function(path, connection_params) {
  if (!.is_bare_data_path(path)) {
    return(path)
  }

  type <- connection_params$type %||% "local"
  switch(type,
    "local" = file.path(connection_params$path %||% "data/", path),
    "s3" = paste0(
      "s3://", connection_params$bucket, "/",
      connection_params$prefix %||% "", path
    ),
    "gcs" = paste0(
      "gs://", connection_params$bucket, "/",
      connection_params$prefix %||% "", path
    ),
    "azure" = paste0(
      "abfss://", connection_params$container, "@",
      connection_params$account, ".dfs.core.windows.net/",
      connection_params$prefix %||% "", path
    ),
    "hf" = paste0(
      "hf://datasets/", connection_params$repo, "/",
      connection_params$subdir %||% "", path
    ),
    "databricks" = {
      db_params <- .databricks_connection_params(connection_params)
      host <- db_params$host
      vol_path <- db_params$volume_path
      if (!nzchar(vol_path %||% "")) {
        stop(
          "load_data(): Set DATABRICKS_VOLUME_PATH in .Renviron:\n",
          "  DATABRICKS_VOLUME_PATH=/Volumes/catalog/schema/volume/path"
        )
      }
      paste0(
        host, "/api/2.0/fs/files", sub("/$", "", vol_path), "/", path
      )
    },
    path
  )
}


# Resolves the Databricks connection and checks the host against the allowlist
# (CR-SEC-01) in every process that uses it, including async workers.
# A "ui"-origin connection never fills a missing field from the environment
# (R2-SEC-01).
.databricks_connection_params <- function(connection_params) {
  field <- function(name, env_var) .connection_field(connection_params, name, env_var)
  list(
    host = .validate_databricks_host(field("workspace", "DATABRICKS_HOST")),
    client_id = field("client_id", "DATABRICKS_CLIENT_ID"),
    client_secret = field("client_secret", "DATABRICKS_CLIENT_SECRET"),
    volume_path = field("volume_path", "DATABRICKS_VOLUME_PATH")
  )
}


#' Return (and if necessary initialise) the module-level DuckDB connection.
#'
#' Safe to call multiple times - returns the existing connection if it is still
#' valid, and creates a fresh one if the connection has been closed or the
#' process has just started.
#'
#' @return A DBI connection to an in-process DuckDB instance.
#' @noRd
.duck_con <- function() {
  if (!is.null(.duck$con) && DBI::dbIsValid(.duck$con)) {
    return(.duck$con)
  }
  .duck$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  .duck_apply_limits(.duck$con)
  .duck$extensions <- character(0)
  # Metadata may cache a token before DuckDB is initialized.
  if (is.null(.duck$db_tokens)) .duck$db_tokens <- list()
  .duck$db_secrets <- list()
  .duck$con
}

# DuckDB version the bundled extension binaries in inst/duckdb_extensions were
# built for. DuckDB refuses to load an extension built for another version, so
# DESCRIPTION pins duckdb to the same version. Rebuild the binaries, update
# these two constants and the DESCRIPTION pin together when upgrading.
.DUCKDB_BUNDLE_VERSION <- "1.5.5"
.DUCKDB_BUNDLE_SHA256 <- c(
  h3 = "5da3520ef7055e893e7551aaef0a6ed9073e3545f6fbb621e7524ab970bfdb8b",
  httpfs = "b2ee03ff84b8df7e24730a5362d652413cfdd7270bb17a43dad246e2164e3e00",
  spatial = "17df6f89c7f689d8f0a8cc42f1f8c3a9b34323b6286748e40337bb7a9d389f01"
)

#' Check that a bundled extension binary matches the installed DuckDB and the
#' pinned checksum before it is installed.
#' @noRd
.duck_check_bundled_ext <- function(ext, path) {
  installed <- as.character(utils::packageVersion("duckdb"))
  if (!identical(installed, .DUCKDB_BUNDLE_VERSION)) {
    stop(
      "The bundled DuckDB extensions were built for duckdb ",
      .DUCKDB_BUNDLE_VERSION, " but duckdb ", installed, " is installed. ",
      "Install duckdb ", .DUCKDB_BUNDLE_VERSION, " or rebuild the binaries ",
      "in inst/duckdb_extensions.",
      call. = FALSE
    )
  }
  expected <- unname(.DUCKDB_BUNDLE_SHA256[ext])
  # Only the compressed binaries are checksummed; an uncompressed fallback has
  # no pinned hash.
  if (grepl("\\.gz$", path) && !is.na(expected)) {
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual, expected)) {
      stop(
        "Bundled DuckDB extension '", ext, "' does not match its pinned ",
        "SHA-256 checksum; refusing to load it.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Apply per-process resource limits to a new DuckDB connection.
#'
#' Every process (the Shiny process and each async worker) owns its own
#' in-memory DuckDB, which by default may use 80% of the machine's RAM and every
#' core. On a shared host, set `WISEAPP_DUCKDB_MEMORY_LIMIT` (for example
#' "4GB") and `WISEAPP_DUCKDB_THREADS`. Spill files go to a per-process temp
#' directory (`WISEAPP_DUCKDB_TEMP_DIR` overrides it) instead of DuckDB's
#' default of `.tmp` in the working directory.
#' @noRd
.duck_apply_limits <- function(con) {
  limit <- trimws(Sys.getenv("WISEAPP_DUCKDB_MEMORY_LIMIT", ""))
  if (nzchar(limit)) {
    if (grepl("^[0-9]+(\\.[0-9]+)?[[:space:]]?(KB|MB|GB|TB|KiB|MiB|GiB|TiB)$", limit)) {
      DBI::dbExecute(con, sprintf("SET memory_limit = %s;", .sql_literal(limit)))
    } else {
      warning("Ignoring invalid WISEAPP_DUCKDB_MEMORY_LIMIT: ", limit, call. = FALSE)
    }
  }
  threads <- .env_positive_int("WISEAPP_DUCKDB_THREADS")
  if (!is.na(threads)) {
    DBI::dbExecute(con, sprintf("SET threads = %d;", threads))
  }
  temp_dir <- Sys.getenv("WISEAPP_DUCKDB_TEMP_DIR", "")
  if (!nzchar(temp_dir)) temp_dir <- file.path(tempdir(), "duckdb-spill")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(temp_dir)) {
    DBI::dbExecute(con, sprintf("SET temp_directory = %s;", .sql_literal(temp_dir)))
  }
  invisible(con)
}

#' Install and load a DuckDB extension exactly once per process.
#'
#' On Posit Connect, uses a binary bundled with the package (the public
#' extensions.duckdb.org endpoint is unreachable there) and fails fast when the
#' required bundle is absent. Locally, uses the standard DuckDB INSTALL
#' mechanism. The extension is recorded as loaded only after a successful
#' LOAD, so a failed load is retried on the next call instead of being
#' permanently cached.
#'
#' @param ext        Extension name e.g. "httpfs", "spatial", "h3".
#' @param db_token   Bearer token - only required on Posit Connect.
#' @param ext_base_url Databricks Files API URL to the folder containing
#'   pre-uploaded .duckdb_extension binaries. Only required on Posit Connect.
#' @noRd
.duck_load_ext <- function(ext, db_token = NULL, ext_base_url = NULL) {
  if (ext %in% (.duck$extensions %||% character(0))) {
    return(invisible(NULL))
  }
  con <- .duck_con()

  # Posit Connect cannot reach the public extension repository, so use the
  # bundled binary there. Automatic source selection is independent of this.
  if (.on_posit_connect()) {
    # Check for a bundled binary first (avoids any network call).
    # Prefer .gz (DuckDB INSTALL decompresses it automatically); fall back
    # to an uncompressed binary if present.
    bundled <- system.file(
      paste0("duckdb_extensions/", ext, ".duckdb_extension.gz"),
      package = "wiseapp"
    )
    if (!nzchar(bundled)) {
      bundled <- system.file(
        paste0("duckdb_extensions/", ext, ".duckdb_extension"),
        package = "wiseapp"
      )
    }

    if (!nzchar(bundled)) {
      stop(
        "DuckDB extension '", ext, "' is not bundled with this deployment ",
        "(expected inst/duckdb_extensions/", ext, ".duckdb_extension.gz). ",
        "Posit Connect cannot reach the public extension repository, so every ",
        "required extension binary must ship with the package.",
        call. = FALSE
      )
    }

    .duck_check_bundled_ext(ext, bundled)
    DBI::dbExecute(con, sprintf("INSTALL '%s';", bundled))
    DBI::dbExecute(con, sprintf("LOAD '%s';", ext))
  } else {
    # Local: load from cache, otherwise install from network if missing
    .core_extensions <- c("azure", "delta", "httpfs", "spatial")
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

# SQL literal quoting (SEC-01) ----

#' Escape and quote a character scalar as a safe DuckDB string literal.
#'
#' Doubles any embedded single quotes so a crafted value (e.g. a credential
#' containing an apostrophe or an injected `'; ...` tail) can never terminate
#' the literal, then wraps the result in single quotes. Values without quotes
#' pass through unchanged, so generated SQL is identical to naive
#' interpolation for ordinary inputs.
#'
#' @param x Character scalar to embed in SQL.
#' @return Character scalar - a single-quoted DuckDB string literal.
#' @noRd
.sql_literal <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

# Credential helpers ----

#' Obtain a Databricks M2M OAuth token, reusing a cached one when valid.
#'
#' Tokens are cached keyed on a SHA-256 of (host, client_id, client_secret)
#' and reused until fewer than 5 minutes of lifetime remain. Entries past that
#' point are dropped, and at most `.DB_TOKEN_CACHE_MAX` entries are kept
#' (soonest-expiring evicted first) (R2-SEC-03).
#'
#' @param host          Databricks workspace URL.
#' @param client_id     OAuth2 client ID.
#' @param client_secret OAuth2 client secret.
#' @return Character scalar - bearer token.
#' @noRd
.get_db_token <- function(host, client_id, client_secret) {
  if (is.null(.duck$db_tokens)) .duck$db_tokens <- list()
  key <- digest::digest(list(host, client_id, client_secret), algo = "sha256")
  usable <- function(entry) {
    difftime(entry$expires_at, Sys.time(), units = "secs") > 300
  }
  .duck$db_tokens <- Filter(usable, .duck$db_tokens)
  cached <- .duck$db_tokens[[key]]

  if (!is.null(cached)) {
    return(cached$token)
  }

  resp <- tryCatch(
    httr2::request(paste0(host, "/oidc/v1/token")) |>
      httr2::req_auth_basic(client_id, client_secret) |>
      httr2::req_body_form(grant_type = "client_credentials", scope = "all-apis") |>
      httr2::req_options(http_version = 2L) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      .db_req_limits(.DB_TOKEN_TIMEOUT_SEC) |>
      httr2::req_perform(),
    error = function(e) {
      stop("load_data(): Failed to obtain Databricks OAuth token: ",
           .http_error_summary(e), call. = FALSE)
    }
  )

  if (httr2::resp_is_error(resp)) {
    stop(
      "load_data(): Failed to obtain Databricks OAuth token: ",
      .http_error_summary(resp)
    )
  }

  parsed <- httr2::resp_body_json(resp)

  .duck$db_tokens[[key]] <- list(
    token      = parsed$access_token,
    expires_at = Sys.time() + as.numeric(parsed$expires_in %||% 3600)
  )
  excess <- length(.duck$db_tokens) - .DB_TOKEN_CACHE_MAX
  if (excess > 0L) {
    expiry <- vapply(.duck$db_tokens, function(e) as.numeric(e$expires_at), numeric(1))
    .duck$db_tokens[order(expiry)[seq_len(excess)]] <- NULL
  }
  parsed$access_token
}

# Upper bound on cached Databricks tokens per process (R2-SEC-03).
.DB_TOKEN_CACHE_MAX <- 16L

# Databricks HTTP limits (CR-SEC-06): a stalled request must not hold the
# shared worker. Transient statuses and connection failures are retried with
# a short exponential backoff (1 s, 2 s).
.DB_TOKEN_TIMEOUT_SEC <- 30
.DB_FILE_TIMEOUT_SEC <- 300
.DB_RETRY_STATUS <- c(429L, 500L, 502L, 503L, 504L)

.db_req_limits <- function(req, timeout) {
  req |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = 3L,
      retry_on_failure = TRUE,
      is_transient = \(resp) httr2::resp_status(resp) %in% .DB_RETRY_STATUS,
      backoff = \(attempt) 2^(attempt - 1)
    )
}

#' One-line summary of an HTTP failure: the status line for a response, or
#' the transport-level cause for a condition. Never includes request headers,
#' bodies or response bodies.
#' @noRd
.http_error_summary <- function(x) {
  if (inherits(x, "httr2_response")) {
    return(paste0("HTTP ", httr2::resp_status(x), " ", httr2::resp_status_desc(x)))
  }
  cause <- if (inherits(x$parent, "condition")) x$parent else x
  strsplit(conditionMessage(cause), "\n", fixed = TRUE)[[1]][1]
}


#' Register a Databricks bearer token as a DuckDB HTTP secret.
#'
#' Secrets are namespaced by a hash of the connection params so mixed
#' credentials within a process do not clobber each other.  The SQL is only
#' executed when the token has changed.
#'
#' @param con         DBI connection.
#' @param db_token    Character scalar - bearer token.
#' @param params_hash Character scalar - key identifying this credential set.
#' @noRd
.register_db_secret <- function(con, db_token, params_hash) {
  secret_name <- paste0("db_http_", params_hash)
  token_hash <- digest::digest(db_token)
  if (!identical(.duck$db_secrets[[params_hash]], token_hash)) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE SECRET %s (TYPE http, BEARER_TOKEN %s);",
      secret_name, .sql_literal(db_token)
    ))
    .duck$db_secrets[[params_hash]] <- token_hash
  }
  invisible(NULL)
}


#' Create a named DuckDB secret only when its resolved credentials changed.
#'
#' Applies the Databricks fast path to the object-store secrets: the SQL
#' round-trip is skipped when the credential tuple is unchanged since the last
#' call in this process. The cache lives in `.duck$db_secrets` and is reset
#' with the connection by `.duck_con()`, so a fresh connection always
#' re-registers. Only the credential digest is cached - never the values.
#'
#' @param con   DBI connection.
#' @param name  Secret name to (re)create.
#' @param creds Named list of resolved credential values (hash input only).
#' @param sql   Fully formatted CREATE OR REPLACE SECRET statement.
#' @noRd
.register_cached_secret <- function(con, name, creds, sql) {
  creds_hash <- substr(digest::digest(list(name, creds)), 1, 8)
  if (!identical(.duck$db_secrets[[name]], creds_hash)) {
    DBI::dbExecute(con, sql)
    .duck$db_secrets[[name]] <- creds_hash
  }
  invisible(NULL)
}


# DuckDB SQL builder ----

#' Build a DuckDB read expression for a set of paths.
#'
#' @param paths         Character vector of resolved paths / URIs.
#' @param format        "parquet" or "csv".
#' @param unify_schemas Logical - pass union_by_name = true.
#' @return Character scalar - SQL fragment for use in a FROM clause.
#' @noRd
.build_read_expr <- function(paths, format, unify_schemas) {
  path_sql <- paste0(
    "[", paste0("'", gsub("'", "''", paths), "'", collapse = ", "), "]"
  )
  union_arg <- if (unify_schemas) ", union_by_name = true" else ""

  switch(format,
    parquet = sprintf("read_parquet(%s%s)", path_sql, union_arg),
    csv     = sprintf("read_csv_auto(%s%s)", path_sql, union_arg),
    stop("load_data(): Unsupported format '", format, "'. Use 'parquet' or 'csv'.")
  )
}


# Fast path: small CSV via Databricks Files REST API ----

# Direct CSV loading avoids DuckDB setup for small Databricks files.
.fetch_db_csv_direct <- function(url, token) {
  resp <- tryCatch(
    .db_csv_request(url, token) |> httr2::req_perform(),
    error = identity
  )

  .parse_db_csv_response(resp, url)
}


.db_csv_request <- function(url, token) {
  httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", token), .redact = "Authorization") |>
    httr2::req_options(http_version = 2L) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    .db_req_limits(.DB_FILE_TIMEOUT_SEC)
}


.parse_db_csv_response <- function(resp, url) {
  if (inherits(resp, "error") || httr2::resp_is_error(resp)) {
    stop(
      "load_data(): Failed to fetch CSV from Databricks (", url, "): ",
      .http_error_summary(resp),
      call. = FALSE
    )
  }

  readr::read_csv(httr2::resp_body_raw(resp), show_col_types = FALSE)
}


# Main function ----

#' Load data files lazily via DuckDB
#'
#' A unified data loading function that opens local or remote datasets via a
#' module-level DuckDB connection. Returns a lazy \code{dplyr::tbl()} - no
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
#' @param collect Logical. Collect into a \code{tibble} immediately using a
#'   deterministic row order. Default \code{FALSE}.
#' @param order_by Optional character vector of preferred ordering columns when
#'   \code{collect = TRUE}. Canonical identity fields and remaining scalar
#'   columns are used as deterministic tie-breakers.
#'
#' @return A lazy \code{dplyr::tbl()} or a \code{tibble} when
#'   \code{collect = TRUE}.
#'
#' @examples
#' \dontrun{
#' params <- list(type = "local", path = "/data")
#'
#' # Bare filename - resolved to /data/survey_list.csv
#' survey_list <- load_data("survey_list.csv", params, collect = TRUE)
#'
#' # Lazy - filter before collecting
#' tbl <- load_data(c("/data/nga_survey.parquet", "/data/eth_survey.parquet"), params)
#' df <- dplyr::collect(dplyr::filter(tbl, year >= 2015))
#'
#' # S3 - bare filenames resolved to s3://my-bucket/data/survey_list.csv
#' params_s3 <- list(
#'   type = "s3", bucket = "my-bucket", prefix = "data/",
#'   region = "us-east-1", key_id = "", secret = ""
#' )
#' survey_list <- load_data("survey_list.csv", params_s3, collect = TRUE)
#' }
#'
#' @noRd
load_data <- function(
  paths,
  connection_params = list(type = "local", path = "/data"),
  format = NULL,
  unify_schemas = FALSE,
  collect = FALSE,
  order_by = NULL,
  preserve_order = FALSE
) {
  if (length(paths) == 0) {
    return(tibble::tibble())
  }

  paths <- as.character(paths)
  type <- .check_connection_type(connection_params$type %||% "local")
  con <- .duck_con()

  # 1. Detect format early - fail before any network calls ----

  if (is.null(format)) {
    ext <- tolower(tools::file_ext(paths[1]))
    format <- switch(ext,
      "parquet" = "parquet",
      "csv"     = "csv",
      "tsv"     = "csv",
      "parquet" # default
    )
  }

  # 2. Resolve bare filenames to full paths / URIs ----

  paths <- vapply(
    paths, .resolve_data_path, character(1),
    connection_params = connection_params, USE.NAMES = FALSE
  )

  # 3. Configure credentials / load extensions per backend ----

  # Environment fallback only for environment-origin params (R2-SEC-01).
  field <- function(name, env_var) .connection_field(connection_params, name, env_var)
  if (type == "s3") {
    .duck_load_ext("httpfs")
    s3_creds <- list(
      key_id = field("key_id", "AWS_ACCESS_KEY_ID"),
      secret = field("secret", "AWS_SECRET_ACCESS_KEY"),
      region = connection_params$region %||% "us-east-1"
    )
    .register_cached_secret(
      con, "s3_secret", s3_creds,
      sprintf(
        "CREATE OR REPLACE SECRET s3_secret (
           TYPE   S3,
           KEY_ID %s,
           SECRET %s,
           REGION %s
         );",
        .sql_literal(s3_creds$key_id),
        .sql_literal(s3_creds$secret),
        .sql_literal(s3_creds$region)
      )
    )
  } else if (type == "gcs") {
    .duck_load_ext("httpfs")
    gcs_creds <- list(
      key_id = field("key_id", "GCS_ACCESS_KEY_ID"),
      secret = field("secret", "GCS_SECRET_ACCESS_KEY")
    )
    .register_cached_secret(
      con, "gcs_secret", gcs_creds,
      sprintf(
        "CREATE OR REPLACE SECRET gcs_secret (
           TYPE   GCS,
           KEY_ID %s,
           SECRET %s
         );",
        .sql_literal(gcs_creds$key_id),
        .sql_literal(gcs_creds$secret)
      )
    )
  } else if (type == "azure") {
    .duck_load_ext("azure")
    .duck_load_ext("delta")

    key <- field("key", "AZURE_STORAGE_KEY")
    client_id <- field("client_id", "AZURE_CLIENT_ID")
    client_secret <- field("client_secret", "AZURE_CLIENT_SECRET")
    tenant_id <- field("tenant_id", "AZURE_TENANT_ID")

    if (nzchar(key)) {
      az_creds <- list(
        provider = "key",
        account  = connection_params$account %||% "",
        key      = key
      )
      .register_cached_secret(
        con, "azure_secret", az_creds,
        sprintf(
          "CREATE OR REPLACE SECRET azure_secret (
             TYPE              AZURE,
             CONNECTION_STRING %s
           );",
          .sql_literal(paste0(
            "AccountName=", az_creds$account,
            ";AccountKey=", az_creds$key
          ))
        )
      )
    } else if (nzchar(client_id) && nzchar(client_secret) && nzchar(tenant_id)) {
      az_creds <- list(
        provider      = "service_principal",
        tenant_id     = tenant_id,
        client_id     = client_id,
        client_secret = client_secret
      )
      .register_cached_secret(
        con, "azure_secret", az_creds,
        sprintf(
          "CREATE OR REPLACE SECRET azure_secret (
             TYPE          AZURE,
             PROVIDER      SERVICE_PRINCIPAL,
             TENANT_ID     %s,
             CLIENT_ID     %s,
             CLIENT_SECRET %s
           );",
          .sql_literal(tenant_id), .sql_literal(client_id), .sql_literal(client_secret)
        )
      )
    } else if (identical(connection_params$origin, "ui")) {
      # The credential chain is the server's identity; never lend it to a
      # user-chosen location (R2-SEC-01).
      stop(
        "load_data(): Enter an Azure account key or service principal, or ",
        "leave all fields blank to use the configured data source."
      )
    } else {
      tryCatch(
        DBI::dbExecute(
          con,
          "CREATE OR REPLACE SECRET azure_secret (
             TYPE     AZURE,
             PROVIDER CREDENTIAL_CHAIN,
             CHAIN    'managed_identity;workload_identity'
           );"
        ),
        error = function(e) {
          stop(
            "load_data(): No Azure credentials found. Provide one of:\n",
            "  1. Account key     : AZURE_STORAGE_KEY\n",
            "  2. Service principal: AZURE_CLIENT_ID + AZURE_CLIENT_SECRET + AZURE_TENANT_ID\n"
          )
        }
      )
    }
  } else if (type == "databricks") {
    db_params <- .databricks_connection_params(connection_params)
    host <- db_params$host
    client_id <- db_params$client_id
    client_secret <- db_params$client_secret

    if (!nzchar(host) || !nzchar(client_id) || !nzchar(client_secret)) {
      stop(
        "load_data(): Databricks requires DATABRICKS_HOST, DATABRICKS_CLIENT_ID, ",
        "DATABRICKS_CLIENT_SECRET.\nSet via usethis::edit_r_environ()"
      )
    }

    db_token <- .get_db_token(host, client_id, client_secret)
    params_hash <- substr(digest::digest(list(host, client_id)), 1, 8)

    # Fast path: single CSV - bypass DuckDB entirely
    if (format == "csv" && length(paths) == 1L) {
      out <- .fetch_db_csv_direct(paths, db_token)
      return(if (collect && !isTRUE(preserve_order)) {
        collect_deterministic(out, order_by)
      } else {
        out
      })
    }

    # Build the base URL for bundled DuckDB extensions.
    vol_path <- db_params$volume_path
    ext_base_url <- paste0(
      host, "/api/2.0/fs/files",
      sub("/$", "", vol_path), "/duckdb_extensions"
    )

    .duck_load_ext("httpfs", db_token, ext_base_url)
    .register_db_secret(con, db_token, params_hash)
  }

  # 4. Normalise local paths ----

  if (type == "local") {
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  }

  # 5. Build the read expression and expose as a lazy tbl via a view ----

  read_expr <- .build_read_expr(paths, format, unify_schemas)
  source_order_col <- "__wise_source_order"
  view_expr <- if (isTRUE(preserve_order)) {
    sprintf(
      "SELECT *, row_number() OVER () AS %s FROM %s",
      source_order_col, read_expr
    )
  } else {
    paste0("SELECT * FROM ", read_expr)
  }
  view_name <- paste0("_ld_", substr(digest::digest(read_expr), 1, 12))

  tryCatch(
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS %s;",
      view_name, view_expr
    )),
    error = function(e) {
      stop(sprintf(
        "load_data(): Failed to open dataset.\n  paths : %s\n  format: %s\n  error : %s",
        paste(head(paths, 3), collapse = ", "), format, conditionMessage(e)
      ))
    }
  )

  tbl <- dplyr::tbl(con, view_name)

  # 6. Collect or return lazy ----

  if (collect && !isTRUE(preserve_order)) {
    collect_deterministic(tbl, order_by)
  } else if (collect) {
    out <- dplyr::collect(tbl)
    if (source_order_col %in% names(out)) {
      out <- out[order(out[[source_order_col]], method = "radix"), , drop = FALSE]
      out[[source_order_col]] <- NULL
    }
    out
  } else {
    tbl
  }
}
