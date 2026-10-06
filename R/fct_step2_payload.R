# Phase 4 shared-context and compact-payload helpers.
#
# Weather ownership contract (W2-B): get_weather() owns production of one
# canonical key in the order emitted; fct_run_simulation() owns consumption and
# publishes either an ordinary data frame, a shared-key descriptor, or a signed
# reference. Consumers call step2_resolve_weather() at their boundary. A shared
# descriptor never owns the key frame itself: its owner (the scenario) owns
# `weather_shared`, while the descriptor owns only member-specific values. This
# keeps Step 3's per-member weather and failure/order ledger intact without
# silently substituting a representative member.

STEP2_WEATHER_KEY_COLUMNS <- c(
  "code", "year", "survname", "loc_id", "timestamp"
)

# Prepared-weather cache (PERF-36) -------------------------------------------
# This cache stores the frames emitted by get_weather(), after spatial joins,
# rolling windows, transformations, rounding, and binning.  It is deliberately
# separate from the run-scoped reference store below: prepared weather is safe
# to reuse across model runs, while references are owned by one published run.
STEP2_PREPARED_WEATHER_CACHE_VERSION <- "v1"

.step2_prepared_weather_cache_root <- function(root = NULL) {
  root <- root %||% Sys.getenv("WISEAPP_PREPARED_WEATHER_CACHE_DIR")
  if (!nzchar(root)) root <- tools::R_user_dir("wiseapp", "cache")
  file.path(root, "prepared-weather", STEP2_PREPARED_WEATHER_CACHE_VERSION)
}

.step2_prepared_weather_cache_enabled <- function(mode = c("auto", "off", "read_write"),
                                                  default_loader = TRUE) {
  mode <- match.arg(mode)
  if (identical(mode, "off")) return(FALSE)
  if (identical(mode, "read_write")) return(TRUE)
  value <- Sys.getenv("WISEAPP_PREPARED_WEATHER_CACHE", unset = "1")
  isTRUE(default_loader) && value %in% c("1", "true", "TRUE", "yes", "YES")
}

.step2_prepared_weather_cache_size <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (!length(files)) return(0)
  sum(file.info(files)$size, na.rm = TRUE)
}

.step2_prepared_weather_cache_evict <- function(root, max_mb = NULL) {
  if (!dir.exists(root)) return(invisible(NULL))
  if (is.null(max_mb)) {
    max_mb <- suppressWarnings(as.numeric(Sys.getenv(
      "WISEAPP_PREPARED_WEATHER_CACHE_MAX_MB", "2048"
    )))
    if (!is.finite(max_mb) || max_mb < 0) max_mb <- 2048
  }
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[!grepl("/(\\.tmp-|\\.staging-)", dirs)]
  if (!length(dirs)) return(invisible(NULL))
  sizes <- vapply(dirs, .step2_prepared_weather_cache_size, numeric(1))
  total <- sum(sizes)
  if (total <= max_mb * 1024^2) return(invisible(NULL))
  info <- file.info(dirs)
  for (dir in dirs[order(info$mtime)]) {
    if (total <= max_mb * 1024^2) break
    i <- match(dir, dirs)
    unlink(dir, recursive = TRUE)
    if (!dir.exists(dir)) total <- total - sizes[[i]]
  }
  invisible(NULL)
}

.step2_prepared_weather_cache_signature <- function(sw, ss, survey_data, cp, sim_dates,
                                                    fp_list, ssps,
                                                    perturbation_method,
                                                    stored_breaks, epsilon = 0.001,
                                                    weather_source = "era5land",
                                                    proj_source = "cmip6") {
  survey_cols <- intersect(
    c("code", "year", "survname", "loc_id", "int_month", "timestamp"),
    names(survey_data)
  )
  list(
    schema = 1L,
    cache_version = STEP2_PREPARED_WEATHER_CACHE_VERSION,
    weather_source_version = WISEAPP_WX_CACHE_VERSION,
    data_version = Sys.getenv("WISEAPP_WEATHER_DATA_VERSION", unset = "unknown"),
    selected_weather = sw,
    selected_surveys = ss,
    survey_identity = if (length(survey_cols)) {
      survey_data[, survey_cols, drop = FALSE]
    } else {
      NULL
    },
    connection = cp[intersect(
      names(cp), c("type", "path", "bucket", "prefix", "container",
                   "account", "host", "catalog", "schema")
    )],
    sim_dates = as.character(sim_dates),
    future_period = lapply(fp_list %||% list(), as.character),
    ssps = as.character(ssps %||% character(0)),
    perturbation_method = perturbation_method,
    stored_breaks = stored_breaks,
    epsilon = epsilon,
    weather_source = weather_source,
    proj_source = proj_source
  ) |>
    digest::digest(algo = "sha256")
}

.step2_prepared_weather_cache_create <- function(signature, root = NULL) {
  root <- .step2_prepared_weather_cache_root(root)
  if (!dir.create(root, recursive = TRUE, showWarnings = FALSE) && !dir.exists(root)) {
    return(NULL)
  }
  target <- file.path(root, signature)
  cache <- new.env(parent = emptyenv())
  cache$root <- root
  cache$target <- target
  cache$stage <- NULL
  cache$hit <- dir.exists(target)
  cache$signature <- signature
  cache$keys <- character(0)
  if (!isTRUE(cache$hit)) .step2_prepared_weather_cache_open_stage(cache)
  cache
}

.step2_prepared_weather_cache_open_stage <- function(cache) {
  stage <- file.path(cache$root, tempfile(
    pattern = ".staging-", tmpdir = cache$root, fileext = ""
  ) |> basename())
  if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
    cache$stage <- NULL
    return(invisible(FALSE))
  }
  cache$stage <- stage
  cache$keys <- character(0)
  invisible(TRUE)
}

.step2_prepared_weather_cache_put <- function(cache, key, weather_raw) {
  if (is.null(cache$stage) || !is.data.frame(weather_raw)) return(invisible(NULL))
  path <- file.path(cache$stage, paste0(digest::digest(key, algo = "sha256"), ".rds"))
  tmp <- paste0(path, ".tmp")
  saveRDS(weather_raw, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not write prepared weather cache entry: ", key, call. = FALSE)
  }
  cache$keys <- unique(c(cache$keys, key))
  invisible(NULL)
}

.step2_prepared_weather_cache_publish <- function(cache, ssps, fp_list) {
  if (is.null(cache$stage)) return(invisible(FALSE))
  keys <- unique(cache$keys)
  complete <- "historical" %in% keys &&
    (length(ssps) == 0L || length(fp_list) == 0L ||
      length(setdiff(keys, "historical")) > 0L)
  if (!complete) {
    unlink(cache$stage, recursive = TRUE)
    return(invisible(FALSE))
  }
  manifest <- list(
    schema = 1L, signature = cache$signature, keys = keys,
    created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  saveRDS(manifest, file.path(cache$stage, "manifest.rds"))
  if (dir.exists(cache$target)) {
    unlink(cache$stage, recursive = TRUE)
  } else if (!file.rename(cache$stage, cache$target)) {
    unlink(cache$stage, recursive = TRUE)
    return(invisible(FALSE))
  }
  cache$stage <- NULL
  .step2_prepared_weather_cache_evict(cache$root)
  invisible(TRUE)
}

.step2_prepared_weather_cache_read <- function(cache, signature) {
  if (is.null(cache) || !isTRUE(cache$hit) || !dir.exists(cache$target)) return(NULL)
  manifest_path <- file.path(cache$target, "manifest.rds")
  manifest <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
  if (is.null(manifest) || !identical(manifest$schema, 1L) ||
    !identical(manifest$signature, signature) || !length(manifest$keys) ||
    !identical(manifest$keys[[1L]], "historical")) {
    unlink(cache$target, recursive = TRUE)
    cache$hit <- FALSE
    .step2_prepared_weather_cache_open_stage(cache)
    return(NULL)
  }
  frames <- tryCatch(lapply(manifest$keys, function(key) {
    path <- file.path(cache$target, paste0(digest::digest(key, algo = "sha256"), ".rds"))
    value <- readRDS(path)
    if (!is.data.frame(value)) stop("invalid prepared weather cache entry", call. = FALSE)
    value
  }), error = function(e) NULL)
  if (is.null(frames)) {
    unlink(cache$target, recursive = TRUE)
    cache$hit <- FALSE
    .step2_prepared_weather_cache_open_stage(cache)
    return(NULL)
  }
  if (!length(setdiff(manifest$keys, "historical")) &&
    length(manifest$keys) > 1L) {
    unlink(cache$target, recursive = TRUE)
    cache$hit <- FALSE
    .step2_prepared_weather_cache_open_stage(cache)
    return(NULL)
  }
  try(Sys.setFileTime(cache$target, Sys.time()), silent = TRUE)
  names(frames) <- manifest$keys
  frames
}

step2_weather_share_members <- function(members,
                                        key_columns = STEP2_WEATHER_KEY_COLUMNS) {
  if (!is.list(members) || !length(members)) {
    return(list(shared = NULL, members = members))
  }
  frames <- lapply(members, function(x) {
    if (!is.data.frame(x)) {
      return(NULL)
    }
    if (!all(key_columns %in% names(x))) {
      return(NULL)
    }
    x[, key_columns, drop = FALSE]
  })
  if (any(vapply(frames, is.null, logical(1)))) {
    return(list(shared = NULL, members = members))
  }
  same_keys <- all(vapply(
    frames[-1L], function(x) identical(x, frames[[1L]]),
    logical(1)
  ))
  if (!same_keys) {
    return(list(shared = NULL, members = members))
  }

  shared <- frames[[1L]]
  descriptors <- lapply(members, function(x) {
    value_columns <- setdiff(names(x), key_columns)
    list(
      schema = 2L,
      kind = "shared-weather-member",
      value_columns = value_columns,
      column_order = names(x),
      values = x[, value_columns, drop = FALSE]
    )
  })
  list(
    shared = list(
      schema = 2L, kind = "shared-weather-keys",
      key_columns = key_columns, keys = shared
    ),
    members = descriptors
  )
}

step2_weather_resolve_shared <- function(value, owner = NULL) {
  if (!is.list(value) || !identical(value$schema, 2L) ||
    !identical(value$kind, "shared-weather-member")) {
    return(NULL)
  }
  shared <- owner$weather_shared %||% NULL
  if (is.null(shared) || !identical(shared$schema, 2L) ||
    !identical(shared$kind, "shared-weather-keys")) {
    stop("Shared Step 2 weather member has no valid owning key frame.", call. = FALSE)
  }
  keys <- shared$keys
  values <- value$values
  if (!is.data.frame(keys) || !is.data.frame(values) ||
    nrow(keys) != nrow(values)) {
    stop("Shared Step 2 weather member has incompatible row counts.", call. = FALSE)
  }
  out <- cbind(keys, values[, setdiff(names(values), names(keys)), drop = FALSE])
  column_order <- value$column_order %||% names(out)
  out[, intersect(column_order, names(out)), drop = FALSE]
}

step2_weather_store_dir <- function(run_id, root = NULL) {
  root <- root %||% Sys.getenv("WISEAPP_STEP2_WEATHER_STORE_DIR")
  if (!nzchar(root)) root <- file.path(tempdir(), "wiseapp-step2-weather")
  file.path(root, paste0("run-", run_id))
}

step2_weather_store_create <- function(run_id, signature, root = NULL) {
  dir <- step2_weather_store_dir(run_id, root)
  # R2-SEC-04: private (0700) directories, including a configured root.
  if (!dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700") &&
      !dir.exists(dir)) {
    stop("Could not create Step 2 weather store: ", dir, call. = FALSE)
  }
  manifest <- list(
    schema = 1L,
    run_id = run_id,
    signature = signature,
    created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  saveRDS(manifest, file.path(dir, "manifest.rds"))
  store <- list(
    schema = manifest$schema, run_id = run_id, signature = signature,
    dir = dir, manifest = file.path(dir, "manifest.rds")
  )
  .step2_weather_store_register(store)
  store
}

.step2_weather_store_registry <- local({
  registry <- new.env(parent = emptyenv())
  registry$stores <- new.env(parent = emptyenv())
  registry$refs <- new.env(parent = emptyenv())
  registry$leases <- new.env(parent = emptyenv())
  registry
})

.step2_weather_store_key <- function(store) {
  dir <- if (is.list(store)) store$dir else as.character(store)[1L]
  if (length(dir) != 1L || !nzchar(dir)) {
    return(NA_character_)
  }
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

.step2_weather_store_register <- function(store) {
  key <- .step2_weather_store_key(store)
  if (is.na(key)) {
    return(invisible(NULL))
  }
  assign(key, store, envir = .step2_weather_store_registry$stores)
  if (!exists(key, envir = .step2_weather_store_registry$refs, inherits = FALSE)) {
    assign(key, 0L, envir = .step2_weather_store_registry$refs)
  }
  invisible(NULL)
}

.step2_weather_store_unlink <- function(key) {
  if (!is.na(key) && exists(key,
    envir = .step2_weather_store_registry$stores,
    inherits = FALSE
  )) {
    store <- get(key,
      envir = .step2_weather_store_registry$stores,
      inherits = FALSE
    )
    dir <- store$dir
    if (length(dir) && nzchar(dir) && dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
    rm(list = key, envir = .step2_weather_store_registry$stores)
  }
  if (!is.na(key) && exists(key,
    envir = .step2_weather_store_registry$refs,
    inherits = FALSE
  )) {
    rm(list = key, envir = .step2_weather_store_registry$refs)
  }
  lease_ids <- ls(envir = .step2_weather_store_registry$leases, all.names = TRUE)
  for (lease_id in lease_ids) {
    lease_keys <- get(lease_id,
      envir = .step2_weather_store_registry$leases,
      inherits = FALSE
    )
    if (key %in% lease_keys) {
      rm(list = lease_id, envir = .step2_weather_store_registry$leases)
    }
  }
  invisible(NULL)
}

step2_weather_store_acquire <- function(stores) {
  if (is.null(stores)) {
    return(NULL)
  }
  if (is.list(stores) && !is.null(stores$dir)) stores <- list(stores)
  if (!is.list(stores) || !length(stores)) {
    return(NULL)
  }
  keys <- unique(Filter(
    function(key) !is.na(key),
    vapply(stores, .step2_weather_store_key, character(1))
  ))
  if (!length(keys)) {
    return(NULL)
  }
  for (key in keys) {
    if (!exists(key,
      envir = .step2_weather_store_registry$stores,
      inherits = FALSE
    )) {
      store <- stores[[which(vapply(
        stores, .step2_weather_store_key,
        character(1)
      ) == key)[1L]]]
      .step2_weather_store_register(store)
    }
    refs <- if (exists(key,
      envir = .step2_weather_store_registry$refs,
      inherits = FALSE
    )) {
      get(key, envir = .step2_weather_store_registry$refs, inherits = FALSE)
    } else {
      0L
    }
    assign(key, refs + 1L, envir = .step2_weather_store_registry$refs)
  }
  lease_id <- paste0("lease-", substr(digest::digest(list(
    Sys.time(), keys,
    basename(tempfile())
  )), 1L, 20L))
  lease <- list(
    schema = 1L, kind = "step2-weather-store-lease",
    lease_id = lease_id, dirs = keys
  )
  assign(lease_id, keys, envir = .step2_weather_store_registry$leases)
  lease
}

step2_weather_store_release <- function(lease) {
  if (is.null(lease)) {
    return(invisible(NULL))
  }
  lease_id <- if (is.list(lease)) lease$lease_id else as.character(lease)[1L]
  if (length(lease_id) != 1L || !nzchar(lease_id) ||
    !exists(lease_id,
      envir = .step2_weather_store_registry$leases,
      inherits = FALSE
    )) {
    return(invisible(NULL))
  }
  keys <- get(lease_id,
    envir = .step2_weather_store_registry$leases,
    inherits = FALSE
  )
  rm(list = lease_id, envir = .step2_weather_store_registry$leases)
  for (key in keys) {
    if (!exists(key,
      envir = .step2_weather_store_registry$refs,
      inherits = FALSE
    )) {
      next
    }
    refs <- get(key,
      envir = .step2_weather_store_registry$refs,
      inherits = FALSE
    ) - 1L
    if (refs <= 0L) {
      .step2_weather_store_unlink(key)
    } else {
      assign(key, refs, envir = .step2_weather_store_registry$refs)
    }
  }
  invisible(NULL)
}

# Transfer a worker-created store to another R process without deleting its
# files. The receiving process calls step2_weather_store_acquire() and becomes
# responsible for the eventual filesystem cleanup.
step2_weather_store_detach <- function(lease) {
  if (is.null(lease)) {
    return(invisible(NULL))
  }
  lease_id <- if (is.list(lease)) lease$lease_id else as.character(lease)[1L]
  if (length(lease_id) != 1L || !nzchar(lease_id) ||
      !exists(lease_id, envir = .step2_weather_store_registry$leases,
              inherits = FALSE)) {
    return(invisible(NULL))
  }
  keys <- get(lease_id, envir = .step2_weather_store_registry$leases,
              inherits = FALSE)
  rm(list = lease_id, envir = .step2_weather_store_registry$leases)
  for (key in keys) {
    if (!exists(key, envir = .step2_weather_store_registry$refs,
                inherits = FALSE)) next
    refs <- get(key, envir = .step2_weather_store_registry$refs,
                inherits = FALSE) - 1L
    if (refs <= 0L) {
      rm(list = key, envir = .step2_weather_store_registry$refs)
      if (exists(key, envir = .step2_weather_store_registry$stores,
                 inherits = FALSE)) {
        rm(list = key, envir = .step2_weather_store_registry$stores)
      }
    } else {
      assign(key, refs, envir = .step2_weather_store_registry$refs)
    }
  }
  invisible(NULL)
}

step2_weather_store_acquire_scenarios <- function(scenarios) {
  if (!is.list(scenarios) || !length(scenarios)) {
    return(NULL)
  }
  stores <- lapply(scenarios, function(s) s$weather_store %||% NULL)
  stores <- Filter(Negate(is.null), stores)
  step2_weather_store_acquire(stores)
}

step2_weather_store_registry_snapshot <- function() {
  keys <- ls(envir = .step2_weather_store_registry$refs, all.names = TRUE)
  list(
    stores = keys,
    refs = if (length(keys)) {
      stats::setNames(vapply(keys, get, integer(1L),
        envir = .step2_weather_store_registry$refs,
        inherits = FALSE
      ), keys)
    } else {
      integer(0)
    },
    leases = ls(envir = .step2_weather_store_registry$leases, all.names = TRUE)
  )
}

step2_weather_store_put <- function(store, key, weather_raw) {
  stopifnot(
    is.list(store), is.character(key), length(key) == 1L,
    is.data.frame(weather_raw)
  )
  file_key <- digest::digest(key, algo = "sha256")
  path <- file.path(store$dir, paste0(file_key, ".rds"))
  tmp <- paste0(path, ".tmp")
  saveRDS(weather_raw, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not publish Step 2 weather reference: ", key, call. = FALSE)
  }
  list(
    schema = store$schema, run_id = store$run_id, key = key,
    file = path, signature = store$signature
  )
}

step2_weather_store_get <- function(reference, expected_signature = NULL) {
  if (is.null(reference) || !is.list(reference) ||
    !identical(reference$schema, 1L) ||
    !file.exists(reference$file)) {
    stop("Step 2 weather reference is missing or invalid.", call. = FALSE)
  }
  if (!is.null(expected_signature) &&
    !identical(reference$signature, expected_signature)) {
    stop("Step 2 weather reference is stale for the current run.", call. = FALSE)
  }
  readRDS(reference$file)
}

step2_weather_store_cleanup <- function(store) {
  key <- .step2_weather_store_key(store)
  if (!is.na(key) && exists(key,
    envir = .step2_weather_store_registry$refs,
    inherits = FALSE
  ) &&
    get(key,
      envir = .step2_weather_store_registry$refs,
      inherits = FALSE
    ) > 0L) {
    warning("Step 2 weather store is still referenced; use its lease to release it.",
      call. = FALSE
    )
    return(invisible(NULL))
  }
  .step2_weather_store_unlink(key)
  invisible(NULL)
}

step2_weather_reference <- function(value, expected_signature = NULL) {
  if (is.list(value) && !is.data.frame(value) &&
    identical(value$schema, 1L) && !is.null(value$file)) {
    return(step2_weather_store_get(value, expected_signature))
  }
  value
}

step2_resolve_weather <- function(value, owner = NULL) {
  # Tibbles are lists, but ordinary weather frames are data, not shared-key
  # descriptors; exclude them before touching $schema/$kind so the tibble
  # `$` accessor cannot emit "Unknown or uninitialised column" warnings.
  if (is.list(value) && !is.data.frame(value) &&
    identical(value$schema, 2L) &&
    identical(value$kind, "shared-weather-member")) {
    return(step2_weather_resolve_shared(value, owner))
  }
  signature <- owner$weather_signature %||% owner$signature %||% NULL
  step2_weather_reference(value, signature)
}

step2_shared_context <- function(train_aug = NULL,
                                 id_col = NULL,
                                 residuals = NULL,
                                 model_metadata = list()) {
  list(
    schema = 1L,
    train_aug = train_aug,
    id_col = id_col,
    residuals = residuals,
    model_metadata = model_metadata
  )
}

step2_pipeline_context <- function(pipe, shared_context = NULL) {
  shared_context <- shared_context %||% list()
  list(
    train_aug = pipe$train_aug %||% shared_context$train_aug,
    id_col = pipe$id_col %||% shared_context$id_col,
    residuals = pipe$residuals %||% shared_context$residuals
  )
}

.compact_residual_context <- function(train_aug, id_col, residuals,
                                      compact = TRUE) {
  if (!isTRUE(compact)) {
    return(train_aug)
  }
  if (is.null(train_aug) || identical(residuals, "none")) {
    return(NULL)
  }
  if (!".resid" %in% names(train_aug)) {
    return(train_aug)
  }
  keep <- ".resid"
  if (identical(residuals, "original") && !is.null(id_col) &&
    id_col %in% names(train_aug)) {
    keep <- c(id_col, keep)
  }
  train_aug[, keep, drop = FALSE]
}

.compact_pipeline <- function(pipe) {
  if (is.null(pipe) || !is.list(pipe)) {
    return(pipe)
  }
  pipe$train_aug <- NULL
  pipe$id_col <- NULL
  pipe
}

compact_step2_result <- function(result,
                                 train_aug,
                                 id_col,
                                 residuals,
                                 chol_obj,
                                 so,
                                 train_data,
                                 model_metadata = list()) {
  context <- step2_shared_context(
    train_aug = train_aug,
    id_col = id_col,
    residuals = residuals,
    model_metadata = model_metadata
  )
  if (!is.null(result$hist_sim_result)) {
    result$hist_sim_result$shared_context <- context
    result$hist_sim_result$pipeline <- .compact_pipeline(
      result$hist_sim_result$pipeline
    )
    result$hist_sim_result$train_data <- NULL
    result$hist_sim_result$cluster_counts <- NULL
  }
  result$new_scenarios <- lapply(result$new_scenarios %||% list(), function(s) {
    s$shared_context <- context
    s$pipelines <- lapply(s$pipelines %||% list(), .compact_pipeline)
    s
  })
  result$payload_mode <- "compact"
  result
}

.results_defensive_copy <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  unserialize(serialize(value, connection = NULL, version = 3L))
}

.results_frame_matrix <- function(frame, key) {
  .results_defensive_copy(frame$.matrices[[key]])
}

.results_frame_entry <- function(frame, label) {
  .results_defensive_copy(frame$.entries[[label]])
}
