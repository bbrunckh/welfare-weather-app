# -----------------------------------------------------------------------------
# Overview metadata loading
# -----------------------------------------------------------------------------
# Databricks uses direct HTTP for small metadata files. Other sources use the
# existing sequential load_data() path.


OVERVIEW_METADATA_FILES <- c(
  survey_list = "metadata/survey_list.csv",
  variable_list = "metadata/variable_list.csv",
  cpi_ppp = "metadata/cpi_ppp.csv"
)

OVERVIEW_METADATA_CACHE_VERSION <- "v1"
OVERVIEW_METADATA_CACHE_MAX_N <- 8L
.overview_metadata_cache <- new.env(parent = emptyenv())

# Required columns are checked once after all source files are loaded.
OVERVIEW_METADATA_REQUIRED_COLUMNS <- list(
  survey_list = c(
    "economy", "code", "year", "survname", "level", "obs", "source"
  ),
  variable_list = c(
    "name", "label", "type", "units", "id", "outcome", "hazard",
    "ind", "hh", "firm", "area", "interact", "fe"
  ),
  cpi_ppp = c("code", "year", "data_level", "cpi", "ppp2021")
)


# -----------------------------------------------------------------------------
# Public loader
# -----------------------------------------------------------------------------

#' Read the Overview metadata bundle from a connection.
#'
#' Successful results contain all metadata needed by the Overview API. The
#' function does not mutate reactive state, so callers can publish the bundle
#' atomically after every file has loaded and passed validation.
#'
#' @param connection_params Named connection parameter list.
#' @param force_refresh Skip the process-local cache and reload the source.
#' @return A named list containing `survey_list`, `variable_list`, `cpi_ppp`,
#'   and `pov_lines`.
#' @noRd
load_overview_metadata <- function(connection_params, force_refresh = FALSE) {
  cache_key <- .overview_metadata_cache_key(connection_params)
  if (!isTRUE(force_refresh)) {
    cached <- .overview_metadata_cache_get(cache_key, connection_params)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  type <- connection_params$type %||% "local"

  if (identical(type, "databricks")) {
    out <- .load_overview_metadata_databricks(connection_params)
  } else {
    out <- .load_overview_metadata_sequential(connection_params)
  }

  out <- .validate_overview_metadata(out, include_pov_lines = FALSE)
  out <- .finish_overview_metadata(out)
  out <- .validate_overview_metadata(out)
  .overview_metadata_cache_set(cache_key, out)
  out
}


# -----------------------------------------------------------------------------
# Cache
# -----------------------------------------------------------------------------

# Cache keys include source identity, credential fingerprints, and local file
# signatures. Remote entries use a TTL because no source version is available.
.overview_metadata_cache_key <- function(connection_params) {
  type <- connection_params$type %||% "local"
  identity <- if (identical(type, "local")) {
    list(
      type = type,
      path = normalizePath(
        path.expand(connection_params$path %||% "data/"),
        winslash = "/",
        mustWork = FALSE
      ),
      files = .overview_metadata_local_signatures(connection_params)
    )
  } else if (identical(type, "databricks")) {
    db_params <- .databricks_connection_params(connection_params)
    list(
      type = type,
      host = sub("/+$", "", db_params$host),
      volume_path = sub("/+$", "", db_params$volume_path),
      client_id = db_params$client_id,
      client_secret = db_params$client_secret
    )
  } else {
    connection_params
  }

  secret_fields <- intersect(
    names(identity), c("secret", "client_secret", "key", "token")
  )
  for (field in secret_fields) {
    identity[[field]] <- digest::digest(identity[[field]])
  }
  digest::digest(list(OVERVIEW_METADATA_CACHE_VERSION, identity))
}


.overview_metadata_local_signatures <- function(connection_params) {
  root <- path.expand(connection_params$path %||% "data/")
  paths <- file.path(root, OVERVIEW_METADATA_FILES)
  info <- file.info(paths)
  out <- lapply(seq_along(paths), function(i) {
    list(
      path = normalizePath(paths[[i]], winslash = "/", mustWork = FALSE),
      exists = file.exists(paths[[i]]),
      size = info$size[[i]],
      mtime = as.numeric(info$mtime[[i]])
    )
  })
  stats::setNames(out, names(OVERVIEW_METADATA_FILES))
}


.overview_metadata_cache_get <- function(key, connection_params) {
  if (.overview_metadata_cache_disabled()) {
    return(NULL)
  }
  entry <- .overview_metadata_cache[[key]]
  if (is.null(entry)) {
    return(NULL)
  }
  type <- connection_params$type %||% "local"
  max_age <- if (identical(type, "local")) {
    Inf
  } else {
    .overview_metadata_cache_max_age()
  }
  if ((as.numeric(Sys.time()) - entry$created_at) > max_age) {
    rm(list = key, envir = .overview_metadata_cache)
    return(NULL)
  }
  entry$accessed_at <- as.numeric(Sys.time())
  .overview_metadata_cache[[key]] <- entry
  entry$value
}


.overview_metadata_cache_set <- function(key, value) {
  if (.overview_metadata_cache_disabled()) {
    return(invisible(FALSE))
  }
  now <- as.numeric(Sys.time())
  .overview_metadata_cache[[key]] <- list(
    created_at = now,
    accessed_at = now,
    value = value
  )
  keys <- ls(.overview_metadata_cache, all.names = TRUE)
  if (length(keys) > OVERVIEW_METADATA_CACHE_MAX_N) {
    accessed <- vapply(
      keys,
      function(key) .overview_metadata_cache[[key]]$accessed_at,
      numeric(1)
    )
    rm(list = keys[[which.min(accessed)]], envir = .overview_metadata_cache)
  }
  invisible(TRUE)
}


.overview_metadata_cache_disabled <- function() {
  value <- tolower(trimws(Sys.getenv("WISEAPP_METADATA_CACHE_DISABLE", "0")))
  if (value %in% c("0", "false", "no", "off", "")) {
    return(FALSE)
  }
  if (value %in% c("1", "true", "yes", "on")) {
    return(TRUE)
  }
  warning(
    "WISEAPP_METADATA_CACHE_DISABLE has an invalid value; using the enabled cache.",
    call. = FALSE
  )
  FALSE
}


.overview_metadata_cache_max_age <- function() {
  raw <- Sys.getenv("WISEAPP_METADATA_CACHE_MAX_AGE", "900")
  value <- suppressWarnings(as.numeric(raw))
  if (!is.na(value) && value >= 0) {
    return(value)
  }
  warning(
    "WISEAPP_METADATA_CACHE_MAX_AGE has an invalid value; using 900 seconds.",
    call. = FALSE
  )
  900
}


# -----------------------------------------------------------------------------
# Source loaders
# -----------------------------------------------------------------------------

.load_overview_metadata_sequential <- function(connection_params) {
  errors <- list()
  out <- list()

  load_one <- function(name, path) {
    tryCatch(
      load_data(path, connection_params, collect = TRUE),
      error = function(e) {
        errors[[name]] <<- conditionMessage(e)
        NULL
      }
    )
  }

  out$survey_list <- load_one(
    "survey_list", OVERVIEW_METADATA_FILES[["survey_list"]]
  )
  out$variable_list <- load_one(
    "variable_list", OVERVIEW_METADATA_FILES[["variable_list"]]
  )
  out$cpi_ppp <- load_one("cpi_ppp", OVERVIEW_METADATA_FILES[["cpi_ppp"]])

  if (length(errors) > 0L) .overview_metadata_error(errors)
  out
}


# -----------------------------------------------------------------------------
# Databricks request helpers
# -----------------------------------------------------------------------------

.load_overview_metadata_databricks <- function(connection_params) {
  db_params <- .databricks_connection_params(connection_params)
  if (!nzchar(db_params$host) || !nzchar(db_params$client_id) ||
    !nzchar(db_params$client_secret)) {
    stop(
      "load_data(): Databricks requires DATABRICKS_HOST, ",
      "DATABRICKS_CLIENT_ID, DATABRICKS_CLIENT_SECRET.\n",
      "Set via usethis::edit_r_environ()"
    )
  }

  urls <- vapply(
    OVERVIEW_METADATA_FILES,
    .resolve_data_path,
    character(1),
    connection_params = connection_params,
    USE.NAMES = TRUE
  )

  db_token <- .get_db_token(
    db_params$host, db_params$client_id, db_params$client_secret
  )

  reqs <- lapply(urls, function(url) {
    .db_csv_request(url, db_token) |>
      httr2::req_throttle(capacity = 3, fill_time_s = 1)
  })
  if (.overview_metadata_parallel_enabled()) {
    responses <- httr2::req_perform_parallel(
      reqs,
      on_error = "continue", progress = FALSE,
      max_active = length(reqs)
    )
  } else {
    responses <- lapply(
      reqs,
      function(req) {
        tryCatch(
          httr2::req_perform(req),
          error = identity
        )
      }
    )
  }

  names(responses) <- names(urls)
  errors <- list()
  out <- list()
  for (name in names(responses)) {
    parsed <- tryCatch(
      .parse_db_csv_response(responses[[name]], urls[[name]]) |>
        collect_deterministic(),
      error = function(e) {
        errors[[name]] <<- conditionMessage(e)
        NULL
      }
    )
    out[[name]] <- parsed
  }

  if (length(errors) > 0L) .overview_metadata_error(errors)

  out
}


# Invalid values use the safe default: enabled.
.overview_metadata_parallel_enabled <- function() {
  value <- tolower(trimws(Sys.getenv("WISEAPP_METADATA_LOAD_PARALLEL", "1")))
  if (value %in% c("0", "false", "no", "off")) {
    return(FALSE)
  }
  if (value %in% c("1", "true", "yes", "on", "")) {
    return(TRUE)
  }
  warning(
    "WISEAPP_METADATA_LOAD_PARALLEL has an invalid value; using the enabled path.",
    call. = FALSE
  )
  TRUE
}


.finish_overview_metadata <- function(metadata) {
  metadata$variable_list <- add_derived_policy_vars_to_vl(metadata$variable_list) |>
    dplyr::mutate(
      name = dplyr::if_else(name == "loc_id", "loc_id_panel", name)
    )
  metadata$pov_lines <- default_poverty_lines()
  metadata
}


.validate_overview_metadata <- function(metadata, include_pov_lines = TRUE) {
  required <- OVERVIEW_METADATA_REQUIRED_COLUMNS
  if (isTRUE(include_pov_lines)) required$pov_lines <- c("ppp_year", "ln")

  errors <- list()
  for (name in names(required)) {
    missing <- setdiff(required[[name]], names(metadata[[name]] %||% data.frame()))
    if (length(missing) > 0L) {
      errors[[name]] <- paste0(
        "missing columns: ", paste(missing, collapse = ", ")
      )
    }
  }

  if (length(errors) > 0L) .overview_metadata_error(errors, prefix = "Invalid metadata")
  metadata
}


.overview_metadata_error <- function(errors, prefix = "Could not load metadata") {
  detail <- vapply(
    names(errors),
    function(name) {
      paste0(
        OVERVIEW_METADATA_FILES[[name]] %||% name, ": ", errors[[name]]
      )
    },
    character(1)
  )
  stop(paste(c(prefix, detail), collapse = "\n"), call. = FALSE)
}
