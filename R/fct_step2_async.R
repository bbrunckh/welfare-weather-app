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

# qs2 threads for the result artifact. Measured on BFA 2 SSP x 1 period
# (196 MB file): 1 -> 2 threads cut write 5.2 -> 2.2 s and read 3.9 -> 2.3 s;
# 4 threads gave no further read gain.
.WISE_STEP2_QS2_THREADS <- 2L

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

.wise_step2_async_job <- function(job_id) {
  state <- .wise_step2_async_state
  if (exists(job_id, envir = state$jobs, inherits = FALSE)) {
    get(job_id, envir = state$jobs, inherits = FALSE)
  } else {
    NULL
  }
}

.wise_step2_async_gate <- function(job, timeout = 0) {
  filelock::lock(job$lock_file, timeout = timeout)
}

.wise_step2_async_unlock <- function(lock) {
  if (!is.null(lock)) try(filelock::unlock(lock), silent = TRUE)
  invisible(NULL)
}

.wise_step2_async_write_control <- function(path, value) {
  tmp <- paste0(path, ".", Sys.getpid(), ".tmp")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(value, tmp, version = 3L)
  if (!file.rename(tmp, path)) stop("Could not publish Step 2 control record.", call. = FALSE)
  invisible(TRUE)
}

.wise_step2_async_retire_marker <- function(job) {
  if (!dir.exists(job$control_dir)) return(invisible(FALSE))
  .wise_step2_async_write_control(job$retired_file, list(
    schema = 1L, job_id = job$id, generation = job$generation,
    retired = TRUE
  ))
  invisible(TRUE)
}

.wise_step2_async_signature_digest <- function(signature) {
  digest::digest(signature, algo = "sha256")
}

.wise_step2_async_connection_params <- function(params) {
  if (is.null(params) || !is.list(params)) {
    return(params)
  }
  # R2-SEC-01: a UI connection carries the user's own credentials to the
  # local daemon (guidelines section 0; .wise_step2_async_launch() only starts
  # local daemons, never url= ones). The worker never fills a missing UI
  # field from its environment (.connection_field()).
  if (identical(params$origin, "ui")) {
    return(params)
  }
  # Environment connections are scrubbed: credentials are never serialized
  # into a worker snapshot and load_data() resolves them from the worker's
  # environment instead.
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

.wise_step2_async_cleanup_control <- function(job) {
  if (!is.null(job$control_dir) && dir.exists(job$control_dir)) {
    unlink(job$control_dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}


.wise_step2_async_dispatch <- function() {
  state <- .wise_step2_async_state
  # A slot held by a job that no longer exists is free (defensive: every
  # settle path releases it, but a stale slot would block the queue forever).
  if (!is.null(state$active) && is.null(.wise_step2_async_job(state$active))) {
    state$active <- NULL
  }
  if (!is.null(state$active) || !length(state$queue)) {
    return(invisible(NULL))
  }

  job_id <- state$queue[[1L]]
  state$queue <- state$queue[-1L]
  job <- .wise_step2_async_job(job_id)
  if (is.null(job) || isTRUE(job$retired)) return(.wise_step2_async_dispatch())
  state$active <- job_id
  job$active <- TRUE
  .wise_step2_async_notify(job, "running")
  artifact_dir <- job$artifact_dir
  control_dir <- job$control_dir
  lock_file <- job$lock_file
  retired_file <- job$retired_file
  progress_file <- job$progress_file
  dependency_signature_digest <- job$dependency_signature_digest
  weather_store_root <- job$weather_store_root
  generation <- job$generation
  seed <- job$seed
  run_id <- job$run_id
  weather_storage <- job$weather_storage
  weather_collect <- job$weather_collect
  weather_threads <- job$weather_threads
  job_id <- job$id
  snapshot <- job$snapshot

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
        loadNamespace("wiseapp")
      }
      options(wiseapp.async.worker_initialized = TRUE)
    }
    wiseapp:::step2_async_worker(
      snapshot = snapshot,
      job_id = job_id,
      generation = generation,
      artifact_dir = artifact_dir,
      control_dir = control_dir,
      lock_file = lock_file,
      retired_file = retired_file,
      progress_file = progress_file,
      dependency_signature_digest = dependency_signature_digest,
        weather_store_root = weather_store_root,
        submitted_at_epoch = submitted_at_epoch,
        seed = seed,
        run_id = run_id,
        weather_storage = weather_storage,
        weather_collect = weather_collect,
        weather_threads = weather_threads
    )
  })

  .wise_step2_async_ensure_daemon()
  submit_started <- proc.time()[["elapsed"]]
  submitted_at_epoch <- as.numeric(Sys.time())
  mirai_job <- tryCatch(
    mirai::try_mirai(
      worker_expr,
      .timeout = .wise_step2_async_timeout_ms("step2"),
      package_path = package_path,
      development_package = development_package,
      snapshot = job$snapshot,
      job_id = job$id,
      generation = job$generation,
      artifact_dir = job$artifact_dir,
      control_dir = job$control_dir,
      lock_file = job$lock_file,
      retired_file = job$retired_file,
      progress_file = job$progress_file,
      dependency_signature_digest = job$dependency_signature_digest,
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
    .wise_step2_async_settle(job, error = mirai_job)
    return(invisible(NULL))
  }
  if (is.null(mirai_job)) {
    # The dispatcher memory cap is backpressure, not a job failure. Put the
    # job back at the front and retry after the active queue drains, keeping
    # the Shiny event loop non-blocking.
    state$active <- NULL
    state$queue <- c(job$id, state$queue)
    job$active <- FALSE
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
  job$submitted_at_epoch <- submitted_at_epoch
  .wise_step2_async_start_poll(job$id)
  promises::then(
    mirai_job,
    onFulfilled = function(manifest) {
      job <- .wise_step2_async_job(job_id)
      if (is.null(job)) return(invisible(NULL))
      job$settled <- TRUE
      .wise_step2_async_stop_poll(job)
      .wise_step2_async_settle(job, manifest = manifest)
      invisible(NULL)
    },
    onRejected = function(error) {
      job <- .wise_step2_async_job(job_id)
      if (is.null(job)) return(invisible(NULL))
      job$settled <- TRUE
      .wise_step2_async_stop_poll(job)
      .wise_step2_async_settle(job, error = .wise_step2_async_describe_error(error))
      invisible(NULL)
    }
  )
  invisible(NULL)
}

.wise_step2_async_stop_poll <- function(job) {
  job$poll_active <- FALSE
  invisible(NULL)
}

.wise_step2_async_detach <- function(job_id) {
  job <- .wise_step2_async_job(as.character(job_id)[1L])
  if (is.null(job)) return(invisible(FALSE))
  job$on_status <- NULL
  job$on_progress <- NULL
  job$on_partial <- NULL
  job$on_result <- NULL
  job$on_error <- NULL
  job$detached <- TRUE
  invisible(TRUE)
}

.wise_step2_async_detach_session <- function(session_id) {
  state <- .wise_step2_async_state
  session_id <- as.character(session_id %||% "unknown")[1L]
  ids <- ls(state$jobs, all.names = TRUE)
  for (id in ids) {
    job <- .wise_step2_async_job(id)
    if (is.null(job) || !identical(job$session_id, session_id)) next
    .wise_step2_async_detach(id)
    .wise_step2_async_cancel(id, "Session ended.")
  }
  invisible(NULL)
}

.wise_step2_async_commit <- function(job, commit_fn) {
  if (!is.environment(job) || !is.function(commit_fn)) return(FALSE)
  current <- .wise_step2_async_job(job$id)
  if (is.null(current) || !identical(current, job) || isTRUE(current$retired) ||
      isTRUE(current$detached) || isTRUE(current$parent_transition)) return(FALSE)
  if (isTRUE(current$settling)) {
    if (isTRUE(current$retired) || isTRUE(current$detached) || file.exists(current$retired_file)) return(FALSE)
    current$parent_transition <- TRUE
    on.exit(current$parent_transition <- FALSE, add = TRUE)
    return(isTRUE(commit_fn()))
  }
  gate <- .wise_step2_async_gate(current, timeout = 0)
  if (is.null(gate)) return(FALSE)
  on.exit(.wise_step2_async_unlock(gate), add = TRUE)
  if (isTRUE(current$retired) || file.exists(current$retired_file)) return(FALSE)
  current$parent_transition <- TRUE
  on.exit(current$parent_transition <- FALSE, add = TRUE)
  isTRUE(commit_fn())
}

.wise_step2_async_settle <- function(job, manifest = NULL, error = NULL) {
  state <- .wise_step2_async_state
  current <- .wise_step2_async_job(job$id)
  if (is.null(current)) return(invisible(NULL))
  retired <- isTRUE(current$retired) || isTRUE(current$detached)
  gate <- .wise_step2_async_gate(current, timeout = 0)
  if (is.null(gate)) {
    later::later(function() .wise_step2_async_settle(job, manifest, error), delay = 0.1)
    return(invisible(FALSE))
  }
  on.exit(.wise_step2_async_unlock(gate), add = TRUE)
  if (file.exists(current$retired_file)) retired <- TRUE
  if (!is.null(error) || retired || !is.list(manifest) || !identical(manifest$status, "succeeded")) {
    if (retired && is.null(error)) {
      callback_error <- NULL
    } else if (is.null(error) && !retired) {
      callback_error <- simpleError("Step 2 worker returned an invalid result manifest.")
    } else {
      callback_error <- error
    }
    .wise_step2_async_unlock(gate)
    gate <- NULL
    .wise_step2_async_cleanup_job(current, remove_weather = TRUE)
    if (identical(state$active, current$id)) state$active <- NULL
    current$active <- FALSE
    .wise_log_stage(
      "step2_run", if (retired) "cancelled" else "failed", run_id = current$run_id,
      elapsed = if (is.null(current$submitted_at_epoch)) NULL else
        as.numeric(Sys.time()) - current$submitted_at_epoch
    )
    .wise_step2_async_notify(current, if (retired) "cancelled" else "failed",
      if (retired) current$retire_reason %||% "Session ended." else conditionMessage(error %||% callback_error))
    if (!retired && is.function(current$on_error)) try(current$on_error(error %||% callback_error, current), silent = TRUE)
    if (exists(current$id, envir = state$jobs, inherits = FALSE)) rm(list = current$id, envir = state$jobs)
    .wise_step2_async_cleanup_control(current)
    .wise_step2_async_dispatch()
    return(invisible(FALSE))
  }
  adopted <- FALSE
  callback_error <- NULL
  if (is.null(error) && !retired && is.list(manifest) &&
      identical(manifest$status, "succeeded") && is.function(current$on_result)) {
    current$settling <- TRUE
    on.exit(current$settling <- FALSE, add = TRUE)
    # Deliver partials written since the last poll before the result lands.
    try(.wise_step2_async_poll_partials(current), silent = TRUE)
    callback_error <- tryCatch({
      accepted <- current$on_result(manifest, current)
      adopted <- isTRUE(accepted)
      NULL
    }, error = function(e) e)
    if (!is.null(callback_error)) adopted <- FALSE
  }
  .wise_step2_async_unlock(gate)
  gate <- NULL
  # Release the worker slot; otherwise the next queued run never dispatches.
  if (identical(state$active, current$id)) state$active <- NULL
  current$active <- FALSE
  .wise_step2_async_cleanup_job(current, remove_weather = !adopted)
  if (exists(current$id, envir = state$jobs, inherits = FALSE)) {
    rm(list = current$id, envir = state$jobs)
  }
  .wise_log_stage(
    "step2_run",
    if (retired) "cancelled" else if (!is.null(callback_error)) "failed" else if (adopted) "succeeded" else "stale",
    run_id = current$run_id,
    elapsed = if (is.null(current$submitted_at_epoch)) NULL else
      as.numeric(Sys.time()) - current$submitted_at_epoch
  )
  if (retired) {
    .wise_step2_async_notify(current, "cancelled", current$retire_reason %||% "Session ended.")
  } else if (!is.null(callback_error)) {
    .wise_step2_async_notify(current, "failed", conditionMessage(callback_error))
    if (is.function(current$on_error)) try(current$on_error(callback_error, current), silent = TRUE)
  } else if (adopted) {
    .wise_step2_async_notify(current, "complete")
  } else {
    .wise_step2_async_notify(current, "stale", "Result was not adopted.")
  }
  .wise_step2_async_cleanup_control(current)
  .wise_step2_async_dispatch()
  invisible(adopted)
}

.wise_step2_async_start_poll <- function(job_id) {
  job <- .wise_step2_async_job(job_id)
  if (is.null(job) || isTRUE(job$poll_active)) return(invisible(NULL))
  job$poll_active <- TRUE
  started_at <- Sys.time()
  poll <- function() {
    current <- .wise_step2_async_job(job_id)
    if (is.null(current) || !isTRUE(current$poll_active) ||
        isTRUE(current$retired) || isTRUE(current$detached)) return(invisible(NULL))
    .wise_step2_async_poll_progress(current)
    if (as.numeric(difftime(Sys.time(), started_at, units = "secs")) > 86400) {
      current$poll_active <- FALSE
      return(invisible(NULL))
    }
    later::later(poll, delay = 0.5)
  }
  later::later(poll, delay = 0.5)
  invisible(NULL)
}

.WISE_STEP2_PROGRESS_INT_FIELDS <- c(
  "groups_total", "groups_done", "member_index", "period_members",
  "n_models", "n_models_requested"
)

.wise_step2_async_poll_progress <- function(job) {
  path <- job$progress_file
  if (!file.exists(path)) return(invisible(NULL))
  info <- file.info(path)
  if (is.na(info$size) || info$size > 65536) return(invisible(NULL))
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  .wise_step2_async_poll_partials(job)
  if (!is.list(value) || !value$schema %in% c(1L, 2L) ||
      !identical(value$job_id, job$id) ||
      !identical(value$generation, job$generation) ||
      !identical(value$dependency_signature_digest, job$dependency_signature_digest) ||
      !is.numeric(value$sequence) || length(value$sequence) != 1L ||
      !is.finite(value$sequence) || value$sequence <= job$progress_sequence ||
       !is.character(value$stage) || length(value$stage) != 1L ||
       !value$stage %in% c("initialize", "weather", "pipeline", "simulation", "publish") ||
       !is.character(value$status) || length(value$status) != 1L ||
      !value$status %in% c("started", "completed", "progress", "failed") ||
      !is.numeric(value$elapsed) || length(value$elapsed) != 1L ||
      !is.finite(value$elapsed) || value$elapsed < 0) return(invisible(NULL))
  job$progress_sequence <- as.integer(value$sequence)
  phases <- c("worker_started", "initialize", "weather", "pipeline", "simulation",
    "historical_ready", "preview_ready", "group_completed", "finalizing",
    "writing_result", "result_written", "manifest_written")
  phase <- value$phase %||% value$stage
  if (!is.character(phase) || length(phase) != 1L || !phase %in% phases) {
    return(invisible(NULL))
  }
  completed <- value$completed
  if (!is.null(completed) && (!is.numeric(completed) || length(completed) != 1L ||
      !is.finite(completed) || completed < 0)) return(invisible(NULL))
  extra <- list()
  for (nm in .WISE_STEP2_PROGRESS_INT_FIELDS) {
    v <- value[[nm]]
    if (is.null(v)) next
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0) {
      return(invisible(NULL))
    }
    extra[[nm]] <- as.integer(v)
  }
  if (!is.null(extra$groups_total)) job$groups_total <- extra$groups_total
  for (nm in c("label", "current_label")) {
    v <- value[[nm]]
    if (is.null(v)) next
    if (!is.character(v) || length(v) != 1L || is.na(v) || nchar(v) > 200L) {
      return(invisible(NULL))
    }
    extra[[nm]] <- v
  }
  if (is.function(job$on_progress)) {
    try(job$on_progress(c(list(stage = value$stage, status = value$status,
      phase = phase, completed = completed,
      elapsed = as.numeric(value$elapsed), sequence = job$progress_sequence),
      extra), job), silent = TRUE)
  }
  invisible(NULL)
}

.WISE_STEP2_PARTIAL_MAX_BYTES <- 2 * 1024 * 1024

# Validate one partial payload against the Step 2 live-run contract (schema 1).
.wise_step2_async_valid_partial <- function(p) {
  is.list(p) && identical(p$schema, 1L) &&
    is.character(p$kind) && length(p$kind) == 1L &&
    p$kind %in% c("historical", "scenario") &&
    is.character(p$label) && length(p$label) == 1L && !is.na(p$label) &&
    nchar(p$label) <= 200L &&
    is.numeric(p$ordinal) && length(p$ordinal) == 1L && is.finite(p$ordinal) &&
    is.data.frame(p$table) && nrow(p$table) > 0L &&
    is.list(p$display) &&
    is.character(p$display$method) && length(p$display$method) == 1L &&
    !is.na(p$display$method) &&
    is.character(p$display$weight_key) && length(p$display$weight_key) == 1L &&
    p$display$weight_key %in% c("weighted", "unweighted") &&
    .wise_step2_async_valid_partial_tables(p$tables, p$display$method)
}

# Optional `tables` (method -> table, all streamed methods): when present it
# must be a fully named list of non-empty data frames that includes the
# display method.
.wise_step2_async_valid_partial_tables <- function(tables, method) {
  if (is.null(tables)) return(TRUE)
  nms <- names(tables)
  is.list(tables) && !is.data.frame(tables) && length(tables) > 0L &&
    !is.null(nms) && !anyNA(nms) && all(nzchar(nms)) && !anyDuplicated(nms) &&
    all(vapply(tables, function(t) is.data.frame(t) && nrow(t) > 0L, logical(1))) &&
    method %in% nms
}

# Deliver new partials in ascending seq order; stops at the first invalid entry.
.wise_step2_async_poll_partials <- function(job) {
  if (isTRUE(job$retired) || isTRUE(job$detached)) return(invisible(0L))
  index_file <- job$partials_index_file
  if (is.null(index_file) || !file.exists(index_file)) return(invisible(0L))
  info <- file.info(index_file)
  if (is.na(info$size) || info$size > 256 * 1024) return(invisible(0L))
  index <- tryCatch(readRDS(index_file), error = function(e) NULL)
  if (!is.list(index) || !identical(index$schema, 1L) ||
      !identical(index$job_id, job$id) ||
      !identical(index$generation, job$generation) ||
      !identical(index$dependency_signature_digest, job$dependency_signature_digest) ||
      !is.list(index$entries)) return(invisible(0L))
  total <- job$groups_total
  cap <- if (is.numeric(total) && length(total) == 1L && is.finite(total)) total + 1L else 64L
  if (length(index$entries) > cap) return(invisible(0L))
  root <- tryCatch(normalizePath(job$artifact_dir, winslash = "/", mustWork = TRUE),
    error = function(e) NULL)
  if (is.null(root)) return(invisible(0L))
  seqs <- vapply(index$entries, function(e) {
    s <- if (is.list(e)) e$seq else NULL
    if (is.numeric(s) && length(s) == 1L && is.finite(s)) as.numeric(s) else NA_real_
  }, numeric(1))
  delivered <- 0L
  for (i in order(seqs)) {
    entry <- index$entries[[i]]
    if (is.na(seqs[i]) || seqs[i] <= job$partials_delivered) next
    if (seqs[i] != job$partials_delivered + 1) break
    if (isTRUE(job$retired) || isTRUE(job$detached)) break
    if (!is.character(entry$path) || length(entry$path) != 1L ||
        !is.numeric(entry$size) || length(entry$size) != 1L ||
        !is.finite(entry$size) || entry$size < 0 ||
        entry$size > .WISE_STEP2_PARTIAL_MAX_BYTES ||
        !file.exists(entry$path)) break
    path <- normalizePath(entry$path, winslash = "/", mustWork = TRUE)
    if (!startsWith(path, paste0(root, "/")) ||
        !identical(as.numeric(file.info(path)$size), as.numeric(entry$size))) break
    partial <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!.wise_step2_async_valid_partial(partial)) break
    if (is.function(job$on_partial)) try(job$on_partial(partial, job), silent = TRUE)
    job$partials_delivered <- as.integer(seqs[i])
    delivered <- delivered + 1L
  }
  invisible(delivered)
}

# Seconds after a (re)launch during which a daemon that has not connected yet
# is not treated as dead.
.WISE_ASYNC_DAEMON_GRACE_SEC <- 15

# Per-task wall-clock limit on the shared daemon, in milliseconds. A task that
# exceeds it is interrupted by mirai, so one stuck run cannot hold the single
# daemon (and every later run in this process) forever. NULL disables it.
.wise_step2_async_timeout_ms <- function(kind = c("step2", "metadata")) {
  kind <- match.arg(kind)
  spec <- switch(kind,
    step2 = list(env = "WISEAPP_ASYNC_TIMEOUT_MIN", default = "90", scale = 60 * 1000),
    metadata = list(env = "WISEAPP_ASYNC_METADATA_TIMEOUT_SEC", default = "300", scale = 1000)
  )
  value <- suppressWarnings(as.numeric(Sys.getenv(spec$env, spec$default)))
  if (!is.finite(value) || value <= 0) NULL else value * spec$scale
}

# TRUE when the daemon is connected to the dispatcher. mirai keeps accepting
# tasks when the daemon has died (for example killed for memory), but they then
# wait for a connection that never comes.
.wise_step2_async_daemon_alive <- function() {
  status <- tryCatch(mirai::status(.compute = "default"), error = function(e) NULL)
  is.list(status) && isTRUE(status$connections >= 1L)
}

.wise_step2_async_launch <- function() {
  state <- .wise_step2_async_state
  mirai::daemons(
    1L, dispatcher = TRUE, memory = .wise_step2_async_queue_memory(),
    .compute = "default", sync = .wise_step2_async_sync()
  )
  state$started <- TRUE
  state$launched_at <- proc.time()[["elapsed"]]
  invisible(TRUE)
}

# Relaunch the daemon when it has died. Tasks already waiting on the dead
# daemon are rejected by mirai (connection reset) and settle as failures.
.wise_step2_async_ensure_daemon <- function() {
  state <- .wise_step2_async_state
  if (!isTRUE(state$started) || .wise_step2_async_sync()) return(invisible(FALSE))
  launched_at <- state$launched_at %||% -Inf
  if (proc.time()[["elapsed"]] - launched_at < .WISE_ASYNC_DAEMON_GRACE_SEC ||
      .wise_step2_async_daemon_alive()) {
    return(invisible(FALSE))
  }
  try(mirai::daemons(0L, .compute = "default"), silent = TRUE)
  .wise_step2_async_launch()
  invisible(TRUE)
}

# Replace mirai's terse error values with a message a user can act on.
.wise_step2_async_describe_error <- function(e) {
  msg <- tryCatch(conditionMessage(e), error = function(err) "")
  text <- if (grepl("Timed out", msg, fixed = TRUE)) {
    "The background run took too long and was stopped. Try fewer scenarios or periods."
  } else if (grepl("Connection reset|Connection aborted|Object closed", msg)) {
    paste("The background worker stopped unexpectedly (often because it ran out of",
      "memory). It will be restarted; try fewer scenarios or periods.")
  } else {
    return(e)
  }
  structure(class = c("wise_async_error", "error", "condition"),
    list(message = text, call = NULL, parent = e))
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
    .wise_step2_async_launch()
    # The first task loads the package inside the daemon. Never wait for that
    # cold load in the Shiny process; task rejection reports initialization errors.
    shiny::onStop(function() {
      if (isTRUE(state$started)) {
        mirai::daemons(0L)
        state$started <- FALSE
      }
    }, session = NULL)
  } else {
    .wise_step2_async_ensure_daemon()
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
                                     on_progress = NULL,
                                     on_partial = NULL,
                                     on_result = NULL,
                                     on_error = NULL) {
  if (!.wise_step2_async_init()) {
    stop("Asynchronous Step 2 execution is unavailable.", call. = FALSE)
  }
  state <- .wise_step2_async_state
  id <- .wise_step2_async_id()
  root <- .wise_step2_async_artifact_root()
  job_dir <- file.path(root, "jobs", id)
  control_dir <- file.path(root, "control", id)
  weather_root <- file.path(root, "weather", id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(control_dir, recursive = TRUE, showWarnings = FALSE)
  digest <- .wise_step2_async_signature_digest(dependency_signature)
  snapshot$weather_storage <- snapshot$weather_storage %||% match.arg(
    Sys.getenv("WISEAPP_STEP2_WEATHER_STORAGE", "memory"), c("memory", "reference")
  )
  snapshot$weather_collect <- snapshot$weather_collect %||% match.arg(
    Sys.getenv("WISEAPP_STEP2_WEATHER_COLLECT", "fast"), c("fast", "bounded")
  )
  snapshot$weather_threads <- snapshot$weather_threads %||% match.arg(
    Sys.getenv("WISEAPP_STEP2_WEATHER_THREADS", "auto"), c("auto", "1", "2")
  )
  job <- list(
    id = id,
    session_id = as.character(session_id %||% "unknown"),
    generation = as.integer(generation),
    seed = as.integer(seed),
    run_id = id,
    snapshot = snapshot,
    dependency_signature = dependency_signature,
    dependency_signature_digest = digest,
    artifact_dir = job_dir,
    control_dir = control_dir,
    lock_file = file.path(control_dir, "publication.lock"),
    retired_file = file.path(control_dir, "retired.rds"),
    progress_file = file.path(job_dir, "progress.rds"),
    partials_index_file = file.path(job_dir, "partials-index.rds"),
    partials_delivered = 0L,
    weather_store_root = weather_root,
    status = "queued",
    weather_storage = snapshot$weather_storage,
    weather_collect = snapshot$weather_collect,
    weather_threads = snapshot$weather_threads,
    clicked_at_epoch = as.numeric(clicked_at_epoch),
    metrics = .wise_step2_async_snapshot_metrics(snapshot),
    handle = NULL,
    retired = FALSE,
    detached = FALSE,
    settled = FALSE,
    active = FALSE,
    poll_active = FALSE,
    progress_sequence = 0L,
    on_status = on_status,
    on_progress = on_progress,
    on_partial = on_partial,
    on_result = on_result,
    on_error = on_error
  )
  job <- list2env(job, parent = emptyenv())

  # A session's newest request supersedes queued work and retires active work.
  same_session <- vapply(state$queue, function(queued_id) {
    queued <- .wise_step2_async_job(queued_id)
    !is.null(queued) && identical(queued$session_id, job$session_id)
  }, logical(1))
  if (any(same_session)) {
    old <- state$queue[same_session]
    state$queue <- state$queue[!same_session]
    for (old_id in old) {
      item <- .wise_step2_async_job(old_id)
      if (is.null(item)) next
      rm(list = old_id, envir = state$jobs)
      .wise_step2_async_cleanup_job(item)
      .wise_step2_async_cleanup_control(item)
      .wise_step2_async_notify(item, "cancelled", "Superseded by a newer run.")
    }
  }
  if (!is.null(state$active)) {
    active <- .wise_step2_async_job(state$active)
    if (!is.null(active) && identical(active$session_id, job$session_id)) {
      .wise_step2_async_cancel(active$id, "Superseded by a newer run.")
    }
  }

  assign(id, job, envir = state$jobs)
  state$queue[[length(state$queue) + 1L]] <- id
  .wise_step2_async_notify(job, "queued")
  .wise_step2_async_dispatch()
  job
}

.wise_step2_async_cancel <- function(job_id, reason = "Cancelled by the user.") {
  state <- .wise_step2_async_state
  job_id <- as.character(job_id %||% "")[1L]
  if (!nzchar(job_id)) return(invisible(FALSE))
  queued <- vapply(state$queue, function(x) identical(x, job_id), logical(1))
  if (any(queued)) {
    job <- .wise_step2_async_job(job_id)
    state$queue <- state$queue[!queued]
    if (!is.null(job)) rm(list = job_id, envir = state$jobs)
    if (is.null(job)) return(invisible(FALSE))
    .wise_step2_async_cleanup_job(job)
    .wise_step2_async_cleanup_control(job)
    .wise_step2_async_notify(job, "cancelled", reason)
    .wise_step2_async_dispatch()
    return(invisible(TRUE))
  }
  active <- state$active
  if (!is.null(active) && identical(active, job_id)) {
    active <- .wise_step2_async_job(job_id)
    if (is.null(active)) return(invisible(FALSE))
    active$retired <- TRUE
    active$retire_reason <- reason
    active$on_status <- NULL
    active$on_progress <- NULL
    active$on_partial <- NULL
    active$on_result <- NULL
    active$on_error <- NULL
    .wise_step2_async_stop_poll(active)
    gate <- .wise_step2_async_gate(active, timeout = 0)
    if (is.null(gate)) {
      active$retire_pending <- TRUE
      retire <- function() {
        current <- .wise_step2_async_job(job_id)
        if (is.null(current) || !isTRUE(current$retired)) return(invisible(NULL))
        lock <- .wise_step2_async_gate(current, timeout = 0)
        if (is.null(lock)) return(later::later(retire, delay = 0.1))
        on.exit(.wise_step2_async_unlock(lock), add = TRUE)
        .wise_step2_async_retire_marker(current)
      }
      later::later(retire, delay = 0.1)
    } else {
      on.exit(.wise_step2_async_unlock(gate), add = TRUE)
      .wise_step2_async_retire_marker(active)
    }
    .wise_step2_async_notify(active, "cancelled", reason)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

.wise_step2_async_read_manifest <- function(manifest, job) {
  if (!is.list(manifest) || !identical(manifest$schema, 2L) ||
      !identical(manifest$job_id, job$id) ||
      !identical(as.integer(manifest$generation), job$generation) ||
      !identical(manifest$codec, "qs2") ||
      !identical(manifest$result_basename, "result.qs2") ||
      !identical(manifest$dependency_signature_digest,
        job$dependency_signature_digest %||% .wise_step2_async_signature_digest(job$dependency_signature))) {
    stop("Step 2 result manifest does not match the submitted job.", call. = FALSE)
  }
  root <- normalizePath(job$artifact_dir, winslash = "/", mustWork = TRUE)
  result_file <- file.path(root, manifest$result_basename)
  if (!identical(manifest$manifest_basename %||% "manifest.rds", "manifest.rds") ||
      !file.exists(file.path(root, "manifest.rds")) ||
      !file.exists(result_file) ||
      !startsWith(normalizePath(result_file, winslash = "/", mustWork = TRUE), paste0(root, "/")) ||
      !is.numeric(manifest$result_bytes) || length(manifest$result_bytes) != 1L ||
      !is.finite(manifest$result_bytes) || manifest$result_bytes < 0 ||
      manifest$result_bytes > 2 * 1024^3 ||
      !identical(as.numeric(file.info(result_file)$size), as.numeric(manifest$result_bytes))) {
    stop("Step 2 result artifact is missing.", call. = FALSE)
  }
  result <- qs2::qs_read(result_file, nthreads = .WISE_STEP2_QS2_THREADS, validate_checksum = TRUE)
  if (!is.list(result) || !is.list(result$hist_sim_result) ||
      !is.list(result$.run) || !identical(result$.run$id, job$id) ||
      !identical(result$.run$schema, 1L) ||
      !identical(result$.sig, manifest$result_signature)) {
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
                               control_dir = dirname(artifact_dir),
                               lock_file = file.path(control_dir, "publication.lock"),
                               retired_file = file.path(control_dir, "retired.rds"),
                               progress_file = file.path(artifact_dir, "progress.rds"),
                               dependency_signature_digest = "",
                               weather_store_root,
                               submitted_at_epoch = NA_real_,
                               seed,
                               run_id = job_id,
                               weather_storage = "memory",
                               weather_collect = "fast",
                               weather_threads = "auto",
                               weather_fn = get_weather,
                               pipeline_fn = run_sim_pipeline) {
  .wise_apply_thread_limits()
  worker_started <- proc.time()[["elapsed"]]
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  result_file <- file.path(artifact_dir, "result.qs2")
  manifest_file <- file.path(artifact_dir, "manifest.rds")
  dir.create(control_dir, recursive = TRUE, showWarnings = FALSE)
  partials_dir <- file.path(artifact_dir, "partials")
  partials_index_file <- file.path(artifact_dir, "partials-index.rds")
  partial_entries <- list()
  sequence <- 0L
  started <- proc.time()[["elapsed"]]
  last_phase <- NULL
  last_status <- NULL
  last_write <- -Inf
  sticky_groups <- list()
  event_fn <- function(event) {
    if (!is.list(event) || !event$stage %in% c("initialize", "weather", "pipeline", "simulation", "publish", "historical_ready", "pipeline_started", "pipeline_completed", "pipeline_failed", "group_completed") ||
        !event$status %in% c("started", "completed", "progress", "failed", "checkpoint")) return(invisible(NULL))
    phase <- event$phase %||% switch(event$stage,
      pipeline_started = "pipeline", pipeline_completed = "pipeline",
      pipeline_failed = "pipeline", group_completed = "group_completed",
      publish = "finalizing", event$stage)
    now <- proc.time()[["elapsed"]]
    # group_completed records are distinct events and are never throttled.
    if (!identical(phase, "group_completed") &&
        identical(phase, last_phase) && identical(event$status, last_status) &&
        now - last_write < 0.25) return(invisible(NULL))
    if (file.exists(retired_file)) return(invisible(NULL))
    sequence <<- sequence + 1L
    record <- list(
      schema = 2L, job_id = job_id, generation = as.integer(generation),
      dependency_signature_digest = dependency_signature_digest,
      sequence = sequence,
      phase = phase,
      stage = if (event$stage %in% c("pipeline_started", "pipeline_completed", "pipeline_failed", "group_completed")) "pipeline" else if (event$stage == "historical_ready") "simulation" else event$stage,
      status = if (event$status == "checkpoint") "progress" else event$status,
      elapsed = max(0, now - started)
    )
    if (!is.null(event$completed)) record$completed <- as.integer(event$completed)
    for (nm in .WISE_STEP2_PROGRESS_INT_FIELDS) {
      v <- event[[nm]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v) && v >= 0) {
        record[[nm]] <- as.integer(v)
        if (nm %in% c("groups_total", "groups_done")) sticky_groups[[nm]] <<- record[[nm]]
      }
    }
    # Progress is cumulative: later records without group fields (publish
    # events) still carry the last known values.
    for (nm in names(sticky_groups)) {
      if (is.null(record[[nm]])) record[[nm]] <- sticky_groups[[nm]]
    }
    lbl <- event$label %||% event$current_label
    if (is.character(lbl) && length(lbl) == 1L && !is.na(lbl) && nchar(lbl) <= 200L) {
      if (!is.null(event$label)) record$label <- lbl
      record$current_label <- lbl
    }
    try(.wise_step2_async_write_control(progress_file, record), silent = TRUE)
    last_phase <<- phase
    last_status <<- event$status
    last_write <<- now
    invisible(NULL)
  }
  checkpoint_fn <- function(...) {
    if (file.exists(retired_file)) {
      stop(structure(list(message = "Step 2 job was retired."),
        class = c("wiseapp_step2_cancelled", "error", "condition")))
    }
    invisible(TRUE)
  }

  if (!is.list(snapshot) || !is.list(snapshot$input)) {
    stop("Step 2 worker requires an ordinary snapshot.", call. = FALSE)
  }
  # Shiny date inputs are Date vectors, while the serial simulation boundary
  # intentionally accepts transport-safe character dates. Normalize these
  # values at the worker boundary so async and synchronous runs share the same
  # simulation semantics.
  snapshot$input <- .wise_step2_async_normalize_input(snapshot$input)
  checkpoint_fn()
  event_fn(list(stage = "initialize", status = "started", phase = "worker_started"))
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
    partial_fn = function(partial) {
      # Partials never fail the run (oversize or I/O problems are skipped);
      # only cancellation propagates.
      tryCatch({
        partial_seq <- length(partial_entries) + 1L
        tmp <- file.path(partials_dir, sprintf("%d.rds.tmp", partial_seq))
        on.exit(unlink(tmp, force = TRUE), add = TRUE)
        dir.create(partials_dir, recursive = TRUE, showWarnings = FALSE)
        saveRDS(partial, tmp, version = 3L)
        size <- file.info(tmp)$size
        if (is.na(size) || size > .WISE_STEP2_PARTIAL_MAX_BYTES) return(invisible(FALSE))
        partial_lock <- filelock::lock(lock_file, timeout = Inf)
        on.exit(.wise_step2_async_unlock(partial_lock), add = TRUE)
        checkpoint_fn()
        path <- file.path(partials_dir, sprintf("%d.rds", partial_seq))
        if (!file.rename(tmp, path)) return(invisible(FALSE))
        partial_entries[[length(partial_entries) + 1L]] <<- list(
          seq = partial_seq, kind = partial$kind, label = partial$label,
          ordinal = as.integer(partial$ordinal), path = path, size = as.numeric(size)
        )
        .wise_step2_async_write_control(partials_index_file, list(
          schema = 1L, job_id = job_id, generation = as.integer(generation),
          dependency_signature_digest = dependency_signature_digest,
          entries = partial_entries
        ))
        invisible(TRUE)
      }, error = function(e) {
        if (inherits(e, "wiseapp_step2_cancelled")) stop(e)
        invisible(FALSE)
      })
    },
    checkpoint_fn = checkpoint_fn,
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
  checkpoint_fn()
  event_fn(list(stage = "publish", status = "started", phase = "writing_result"))
  qs2::qs_save(computed$result, tmp_result, nthreads = .WISE_STEP2_QS2_THREADS)
  lock <- filelock::lock(lock_file, timeout = Inf)
  on.exit(.wise_step2_async_unlock(lock), add = TRUE)
  checkpoint_fn()
  if (!file.rename(tmp_result, result_file)) {
    unlink(tmp_result, force = TRUE)
    stop("Could not atomically publish Step 2 result artifact.", call. = FALSE)
  }
  event_fn(list(stage = "publish", status = "completed", phase = "result_written"))
  manifest <- list(
    schema = 2L,
    job_id = job_id,
    generation = as.integer(generation),
    status = "succeeded",
    manifest_basename = basename(manifest_file),
    codec = "qs2",
    result_basename = basename(result_file),
    result_bytes = as.numeric(file.info(result_file)$size),
    dependency_signature_digest = dependency_signature_digest,
    result_signature = computed$signature,
    qs2_version = as.character(utils::packageVersion("qs2"))
  )
  tmp_manifest <- paste0(manifest_file, ".tmp")
  saveRDS(manifest, tmp_manifest, version = 3L)
  if (!file.rename(tmp_manifest, manifest_file)) {
    unlink(tmp_manifest, force = TRUE)
    stop("Could not atomically publish Step 2 manifest.", call. = FALSE)
  }
  event_fn(list(stage = "publish", status = "completed", phase = "manifest_written"))
  .wise_step2_async_unlock(lock)
  lock <- NULL
  .wise_log_stage(
    "step2_worker", "succeeded", run_id = run_id,
    elapsed = proc.time()[["elapsed"]] - worker_started,
    keys = computed$result$n_keys
  )
  manifest
}
