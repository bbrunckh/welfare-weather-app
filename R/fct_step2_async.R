# Asynchronous Step 2 orchestration.
#
# The coordinator is process-wide: each Shiny worker owns one mirai daemon and
# this FIFO queue ensures that large Step 2 jobs never run concurrently in the
# same process. Numerical work remains in step2_compute().

.wise_step2_async_state <- local({
  state <- new.env(parent = emptyenv())
  state$started <- FALSE
  state$queue <- list()
  state$active <- NULL
  state$jobs <- new.env(parent = emptyenv())
  state$worker_initialized <- FALSE
  state
})

.wise_step2_async_enabled <- function() {
  value <- tolower(trimws(Sys.getenv("WISEAPP_ASYNC_STEP2", "1")))
  !value %in% c("0", "false", "no", "off")
}

# Debugging escape hatch: WISEAPP_ASYNC_SYNC=1 evaluates tasks in the main
# process instead of the daemon (mirai sync mode). Default is real workers.
.wise_step2_async_sync <- function() {
  value <- tolower(trimws(Sys.getenv("WISEAPP_ASYNC_SYNC", "0")))
  value %in% c("1", "true", "yes", "on")
}

.wise_step2_async_queue_memory <- function() {
  value <- suppressWarnings(as.numeric(
    Sys.getenv("WISEAPP_ASYNC_QUEUE_MEMORY_MB", "512")
  ))
  if (!is.finite(value) || value <= 0) NULL else value
}

.wise_step2_async_metrics_enabled <- function() {
  tolower(trimws(Sys.getenv("WISEAPP_ASYNC_METRICS", "0"))) %in%
    c("1", "true", "yes", "on")
}

.wise_step2_async_is_dev_package <- function() {
  isTRUE(
    requireNamespace("pkgload", quietly = TRUE) &&
      tryCatch(pkgload::is_dev_package("wiseapp"), error = function(e) FALSE)
  )
}

.wise_step2_async_snapshot_metrics <- function(snapshot) {
  if (!.wise_step2_async_metrics_enabled()) return(list())
  list(
    object_bytes = as.numeric(utils::object.size(snapshot)),
    serialized_bytes = tryCatch(
      length(serialize(snapshot, NULL, version = 3L)),
      error = function(e) NA_real_
    )
  )
}

.wise_step2_async_artifact_root <- function() {
  root <- Sys.getenv("WISEAPP_ASYNC_ARTIFACT_ROOT", "")
  if (!nzchar(root)) {
    root <- file.path(tempdir(), "wiseapp-step2-async", Sys.getpid())
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

.wise_step2_async_connection_params <- function(params) {
  if (is.null(params) || !is.list(params)) {
    return(params)
  }
  # Credentials are intentionally never serialized into a worker snapshot.
  # load_data() resolves these names from the worker's environment instead.
  secret_like <- grepl(
    "secret|key|token|password|credential|client_id|tenant",
    names(params), ignore.case = TRUE
  )
  out <- params[!secret_like]
  # Worker-side loaders resolve all secrets from their environment. Preserve
  # only explicit non-secret source configuration and never credential values.
  out
}

.wise_step2_async_id <- function() {
  paste0(
    "step2-", format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC"), "-",
    substr(digest::digest(list(Sys.getpid(), Sys.time(), runif(1L))), 1L, 16L)
  )
}

.wise_step2_async_notify <- function(job, status, detail = NULL) {
  job$status <- status
  job$status_detail <- detail
  if (is.function(job$on_status)) {
    try(job$on_status(status, job, detail), silent = TRUE)
  }
  invisible(NULL)
}

.wise_step2_async_cleanup_job <- function(job, remove_weather = TRUE) {
  if (!is.null(job$artifact_dir) && dir.exists(job$artifact_dir)) {
    unlink(job$artifact_dir, recursive = TRUE, force = TRUE)
  }
  if (isTRUE(remove_weather) && !is.null(job$weather_store_root) &&
      dir.exists(job$weather_store_root)) {
    unlink(job$weather_store_root, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}


.wise_step2_async_dispatch <- function() {
  state <- .wise_step2_async_state
  if (!is.null(state$active) || !length(state$queue)) {
    return(invisible(NULL))
  }

  job <- state$queue[[1L]]
  state$queue <- state$queue[-1L]
  state$active <- job
  assign(job$id, job, envir = state$jobs)
  .wise_step2_async_notify(job, "running")

  package_path <- getNamespaceInfo(asNamespace("wiseapp"), "path")
  development_package <- .wise_step2_async_is_dev_package()
  worker_expr <- quote({
    if (!isTRUE(getOption("wiseapp.async.worker_initialized", FALSE))) {
      if (isTRUE(development_package)) {
        pkgload::load_all(
          package_path, export_all = FALSE, helpers = FALSE,
          attach_testthat = FALSE, quiet = TRUE
        )
      } else {
        requireNamespace("wiseapp", quietly = TRUE)
      }
      options(wiseapp.async.worker_initialized = TRUE)
    }
    wiseapp:::step2_async_worker(
      snapshot = snapshot,
      job_id = job_id,
        generation = generation,
        artifact_dir = artifact_dir,
        weather_store_root = weather_store_root,
        submitted_at_epoch = submitted_at_epoch,
        seed = seed,
        run_id = run_id
    )
  })

  submit_started <- proc.time()[["elapsed"]]
  submitted_at_epoch <- as.numeric(Sys.time())
  mirai_job <- tryCatch(
    mirai::try_mirai(
      worker_expr,
      package_path = package_path,
      development_package = development_package,
      snapshot = job$snapshot,
      job_id = job$id,
      generation = job$generation,
      artifact_dir = job$artifact_dir,
      weather_store_root = job$weather_store_root,
      submitted_at_epoch = submitted_at_epoch,
      seed = job$seed,
      run_id = job$run_id,
      weather_storage = job$weather_storage,
      weather_collect = job$weather_collect,
      weather_threads = job$weather_threads
    ),
    error = function(e) e
  )
  if (inherits(mirai_job, "error")) {
    state$active <- NULL
    rm(list = job$id, envir = state$jobs)
    .wise_step2_async_cleanup_job(job)
    .wise_step2_async_notify(job, "failed", conditionMessage(mirai_job))
    if (is.function(job$on_error)) {
      try(job$on_error(mirai_job, job), silent = TRUE)
    }
    return(.wise_step2_async_dispatch())
  }
  if (is.null(mirai_job)) {
    # The dispatcher memory cap is backpressure, not a job failure. Put the
    # job back at the front and retry after the active queue drains, keeping
    # the Shiny event loop non-blocking.
    state$active <- NULL
    state$queue <- c(list(job), state$queue)
    .wise_step2_async_notify(job, "queued", "Waiting for async queue capacity")
    later::later(.wise_step2_async_dispatch, delay = 0.5)
    return(invisible(NULL))
  }

  job$metrics$submit_elapsed_ms <-
    (proc.time()[["elapsed"]] - submit_started) * 1000
  job$metrics$click_to_submit_ms <- if (is.finite(job$clicked_at_epoch)) {
    (submitted_at_epoch - job$clicked_at_epoch) * 1000
  } else {
    NA_real_
  }
  job$handle <- mirai_job
  state$active <- job
  assign(job$id, job, envir = state$jobs)
  promises::then(
    mirai_job,
    onFulfilled = function(manifest) {
      active <- state$active
      if (is.null(active) || !identical(active$id, job$id)) {
        return(invisible(NULL))
      }
      state$active <- NULL
      rm(list = job$id, envir = state$jobs)
      if (!is.list(manifest) || !identical(manifest$status, "succeeded")) {
        err <- simpleError("Step 2 worker returned an invalid result manifest.")
        .wise_step2_async_cleanup_job(job)
        .wise_step2_async_notify(job, "failed", conditionMessage(err))
        if (is.function(job$on_error)) try(job$on_error(err, job), silent = TRUE)
      } else if (is.function(job$on_result)) {
        callback_error <- tryCatch(
          job$on_result(manifest, job),
          error = function(e) e
        )
        if (inherits(callback_error, "error") && is.function(job$on_error)) {
          try(job$on_error(callback_error, job), silent = TRUE)
        }
      }
      .wise_step2_async_dispatch()
      invisible(NULL)
    },
    onRejected = function(error) {
      active <- state$active
      if (is.null(active) || !identical(active$id, job$id)) {
        return(invisible(NULL))
      }
      state$active <- NULL
      rm(list = job$id, envir = state$jobs)
      .wise_step2_async_cleanup_job(job)
      .wise_step2_async_notify(job, "failed", conditionMessage(error))
      if (is.function(job$on_error)) try(job$on_error(error, job), silent = TRUE)
      .wise_step2_async_dispatch()
      invisible(NULL)
    }
  )
  invisible(NULL)
}

.wise_step2_async_init <- function() {
  state <- .wise_step2_async_state
  if (!.wise_step2_async_enabled()) {
    return(FALSE)
  }
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop(
      "Asynchronous Step 2 is enabled but the 'mirai' package is unavailable.",
      call. = FALSE
    )
  }
  if (!isTRUE(state$started)) {
    mirai::daemons(
      1L, dispatcher = TRUE, memory = .wise_step2_async_queue_memory(),
      .compute = "default", sync = .wise_step2_async_sync()
    )
    package_path <- getNamespaceInfo(asNamespace("wiseapp"), "path")
    development_package <- .wise_step2_async_is_dev_package()
    initialized <- mirai::everywhere({
      if (isTRUE(development_package)) {
        pkgload::load_all(
          package_path, export_all = FALSE, helpers = FALSE,
          attach_testthat = FALSE, quiet = TRUE
        )
      } else {
        requireNamespace("wiseapp", quietly = TRUE)
      }
      options(wiseapp.async.worker_initialized = TRUE)
      TRUE
    }, package_path = package_path,
    development_package = development_package, .compute = "default")
    initialized <- mirai::collect_mirai(initialized)
    if (any(vapply(initialized, inherits, logical(1), what = "miraiError"))) {
      stop("Could not initialize the async Step 2 worker.", call. = FALSE)
    }
    state$worker_initialized <- TRUE
    state$started <- TRUE
    shiny::onStop(function() {
      if (isTRUE(state$started)) {
        mirai::daemons(0L)
        state$started <- FALSE
      }
    }, session = NULL)
  }
  TRUE
}

.wise_step2_async_submit <- function(snapshot,
                                     generation,
                                     session_id,
                                     seed,
                                     dependency_signature,
                                     clicked_at_epoch = NA_real_,
                                     on_status = NULL,
                                     on_result = NULL,
                                     on_error = NULL) {
  if (!.wise_step2_async_init()) {
    stop("Asynchronous Step 2 execution is unavailable.", call. = FALSE)
  }
  state <- .wise_step2_async_state
  id <- .wise_step2_async_id()
  root <- .wise_step2_async_artifact_root()
  job_dir <- file.path(root, "jobs", id)
  weather_root <- file.path(root, "weather", id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  job <- list(
    id = id,
    session_id = as.character(session_id %||% "unknown"),
    generation = as.integer(generation),
    seed = as.integer(seed),
    run_id = id,
    snapshot = snapshot,
    dependency_signature = dependency_signature,
    artifact_dir = job_dir,
    weather_store_root = weather_root,
    status = "queued",
    weather_storage = snapshot$weather_storage %||% match.arg(
      Sys.getenv("WISEAPP_STEP2_WEATHER_STORAGE", "memory"),
      c("memory", "reference")
    ),
    weather_collect = snapshot$weather_collect %||% match.arg(
      Sys.getenv("WISEAPP_STEP2_WEATHER_COLLECT", "fast"),
      c("fast", "bounded")
    ),
    weather_threads = snapshot$weather_threads %||% match.arg(
      Sys.getenv("WISEAPP_STEP2_WEATHER_THREADS", "auto"),
      c("auto", "1", "2")
    ),
    clicked_at_epoch = as.numeric(clicked_at_epoch),
    metrics = .wise_step2_async_snapshot_metrics(snapshot),
    handle = NULL,
    on_status = on_status,
    on_result = on_result,
    on_error = on_error
  )

  # A session's newest request supersedes work that has not started yet. An
  # active job is stopped as well, but jobs belonging to other sessions remain
  # FIFO queued behind it.
  same_session <- vapply(
    state$queue, function(x) identical(x$session_id, job$session_id), logical(1)
  )
  if (any(same_session)) {
    old <- state$queue[same_session]
    state$queue <- state$queue[!same_session]
    for (item in old) {
      .wise_step2_async_cleanup_job(item)
      .wise_step2_async_notify(item, "cancelled", "Superseded by a newer run.")
    }
  }
  if (!is.null(state$active) &&
      identical(state$active$session_id, job$session_id)) {
    .wise_step2_async_cancel(state$active$id, "Superseded by a newer run.")
  }

  assign(id, job, envir = state$jobs)
  state$queue[[length(state$queue) + 1L]] <- job
  .wise_step2_async_notify(job, "queued")
  .wise_step2_async_dispatch()
  job
}

.wise_step2_async_cancel <- function(job_id, reason = "Cancelled by the user.") {
  state <- .wise_step2_async_state
  job_id <- as.character(job_id %||% "")[1L]
  if (!nzchar(job_id)) return(invisible(FALSE))
  queued <- vapply(state$queue, function(x) identical(x$id, job_id), logical(1))
  if (any(queued)) {
    job <- state$queue[[which(queued)[1L]]]
    state$queue <- state$queue[!queued]
    rm(list = job_id, envir = state$jobs)
    .wise_step2_async_cleanup_job(job)
    .wise_step2_async_notify(job, "cancelled", reason)
    return(invisible(TRUE))
  }
  active <- state$active
  if (!is.null(active) && identical(active$id, job_id)) {
    state$active <- NULL
    rm(list = job_id, envir = state$jobs)
    if (!is.null(active$handle)) try(mirai::stop_mirai(active$handle), silent = TRUE)
    .wise_step2_async_cleanup_job(active)
    .wise_step2_async_notify(active, "cancelled", reason)
    .wise_step2_async_dispatch()
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

.wise_step2_async_read_manifest <- function(manifest, job) {
  if (!is.list(manifest) || !identical(manifest$schema, 1L) ||
      !identical(manifest$job_id, job$id) ||
      !identical(as.integer(manifest$generation), job$generation)) {
    stop("Step 2 result manifest does not match the submitted job.", call. = FALSE)
  }
  result_file <- manifest$result_file %||% ""
  if (!nzchar(result_file) || !file.exists(result_file)) {
    stop("Step 2 result artifact is missing.", call. = FALSE)
  }
  result <- readRDS(result_file)
  if (!is.list(result) || is.null(result$hist_sim_result)) {
    stop("Step 2 result artifact is invalid.", call. = FALSE)
  }
  result
}

.wise_step2_async_find_stores <- function(value) {
  if (is.list(value) && !is.null(value$dir) && !is.null(value$run_id)) {
    return(list(value))
  }
  if (!is.list(value)) return(list())
  unlist(lapply(value, .wise_step2_async_find_stores), recursive = FALSE)
}

.wise_step2_async_normalize_input <- function(input) {
  if (!is.list(input)) {
    return(input)
  }
  input$sim_dates <- as.character(input$sim_dates)
  input$ssps <- as.character(input$ssps)
  input$fp_list <- lapply(input$fp_list, as.character)
  input
}

#' Execute one serial Step 2 snapshot in a worker process.
#'
#' The function deliberately receives only ordinary serializable values. The
#' worker resolves credentials from its own environment and returns a compact
#' manifest after atomically writing the large result artifact.
#' @export
step2_async_worker <- function(snapshot,
                               job_id,
                               generation,
                               artifact_dir,
                               weather_store_root,
                               submitted_at_epoch = NA_real_,
                               seed,
                               run_id = job_id,
                               weather_storage = "memory",
                               weather_collect = "fast",
                               weather_threads = "auto",
                               weather_fn = get_weather,
                               pipeline_fn = run_sim_pipeline) {
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  result_file <- file.path(artifact_dir, "result.rds")
  manifest_file <- file.path(artifact_dir, "manifest.rds")
  on.exit({
    if (!file.exists(manifest_file) && dir.exists(artifact_dir)) {
      unlink(artifact_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  event_fn <- function(event) invisible(NULL)

  if (!is.list(snapshot) || !is.list(snapshot$input)) {
    stop("Step 2 worker requires an ordinary snapshot.", call. = FALSE)
  }
  # Shiny date inputs are Date vectors, while the serial simulation boundary
  # intentionally accepts transport-safe character dates. Normalize these
  # values at the worker boundary so async and synchronous runs share the same
  # simulation semantics.
  snapshot$input <- .wise_step2_async_normalize_input(snapshot$input)
  gc(verbose = FALSE)
  computed <- step2_compute(
    input = snapshot$input,
    seed = seed,
    run_id = run_id,
    cache_dir = snapshot$cache_dir %||% NULL,
    weather_storage = weather_storage,
    weather_store_root = weather_store_root,
    weather_collect = weather_collect,
    weather_threads = weather_threads,
    event_fn = event_fn,
    weather_fn = weather_fn,
    pipeline_fn = pipeline_fn
  )
  # The worker's registry is process-local. Return the durable store descriptor
  # in the artifact so the Shiny process can register and lease it on adoption.
  if (!is.null(computed$result$weather_store)) {
    step2_weather_store_detach(computed$result$weather_store_lease)
    computed$result$weather_store_lease <- NULL
  }
  rm(snapshot)
  gc(verbose = FALSE)
  tmp_result <- paste0(result_file, ".tmp")
  saveRDS(computed$result, tmp_result, version = 3L)
  if (!file.rename(tmp_result, result_file)) {
    unlink(tmp_result, force = TRUE)
    stop("Could not atomically publish Step 2 result artifact.", call. = FALSE)
  }
  manifest <- list(
    schema = 1L,
    job_id = job_id,
    generation = as.integer(generation),
    status = "succeeded",
    result_file = normalizePath(result_file, winslash = "/", mustWork = FALSE),
    result_signature = computed$signature,
    warnings = character(0),
    events = computed$events
  )
  tmp_manifest <- paste0(manifest_file, ".tmp")
  saveRDS(manifest, tmp_manifest, version = 3L)
  if (!file.rename(tmp_manifest, manifest_file)) {
    unlink(tmp_manifest, force = TRUE)
    stop("Could not atomically publish Step 2 manifest.", call. = FALSE)
  }
  manifest
}
