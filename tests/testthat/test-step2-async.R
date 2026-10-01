testthat::test_that("async Step 2 snapshots normalize transport date fields", {
  input <- list(
    sim_dates = as.Date(c("2020-01-01", "2020-02-01")),
    ssps = factor("ssp2_4_5"),
    fp_list = list(as.Date(c("2030-01-01", "2040-12-31")))
  )
  normalized <- .wise_step2_async_normalize_input(input)
  testthat::expect_type(normalized$sim_dates, "character")
  testthat::expect_type(normalized$ssps, "character")
  testthat::expect_true(all(vapply(normalized$fp_list, is.character, logical(1))))
  testthat::expect_identical(normalized$sim_dates, as.character(input$sim_dates))
})

testthat::test_that("async connection snapshots never contain credentials", {
  params <- list(
    type = "s3", bucket = "bucket", prefix = "data/", region = "us-east-1",
    key_id = "should-not-cross-process", secret = "should-not-cross-process"
  )
  safe <- .wise_step2_async_connection_params(params)
  testthat::expect_identical(safe$type, "s3")
  testthat::expect_identical(safe$bucket, "bucket")
  testthat::expect_false(any(c("key_id", "secret") %in% names(safe)))
})

testthat::test_that("async manifest validation rejects mismatched jobs", {
  root <- tempfile("wiseapp-async-manifest-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  result_file <- file.path(root, "result.rds")
  saveRDS(list(hist_sim_result = list()), result_file)
  job <- list(id = "job-1", generation = 4L)
  manifest <- list(
    schema = 1L, job_id = "job-2", generation = 4L,
    status = "succeeded", result_file = result_file
  )
  testthat::expect_error(
    .wise_step2_async_read_manifest(manifest, job),
    "does not match"
  )
})

testthat::test_that("queued cancellation removes the job and its artifacts", {
  state <- .wise_step2_async_state
  old_queue <- state$queue
  old_active <- state$active
  on.exit({
    state$queue <- old_queue
    state$active <- old_active
  }, add = TRUE)
  root <- tempfile("wiseapp-async-cancel-")
  artifact_dir <- file.path(root, "artifact")
  weather_dir <- file.path(root, "weather")
  dir.create(artifact_dir, recursive = TRUE)
  dir.create(weather_dir, recursive = TRUE)
  events <- list()
  job <- list(
    id = "queued-cancel", session_id = "session-1", status = "queued",
    artifact_dir = artifact_dir, weather_store_root = weather_dir,
    on_status = function(status, ...) events <<- c(events, status)
  )
  job <- list2env(job, parent = emptyenv())
  assign(job$id, job, envir = state$jobs)
  state$queue <- job$id

  expect_true(.wise_step2_async_cancel(job$id))
  expect_length(state$queue, 0L)
  expect_false(exists(job$id, envir = state$jobs, inherits = FALSE))
  expect_false(dir.exists(artifact_dir))
  expect_false(dir.exists(weather_dir))
  expect_identical(events[[1L]], "cancelled")
})

testthat::test_that("active cancellation retires without stopping or releasing FIFO", {
  state <- .wise_step2_async_state
  old_queue <- state$queue
  old_active <- state$active
  on.exit({
    state$queue <- old_queue
    state$active <- old_active
    if (exists("retired-test", envir = state$jobs, inherits = FALSE)) {
      rm(list = "retired-test", envir = state$jobs)
    }
  }, add = TRUE)
  root <- tempfile("wiseapp-async-retire-")
  control <- file.path(root, "control")
  dir.create(control, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  job <- list2env(list(
    id = "retired-test", session_id = "session-retire", generation = 2L,
    status = "running", control_dir = control,
    lock_file = file.path(control, "publication.lock"),
    retired_file = file.path(control, "retired.rds"),
    retired = FALSE,
    weather_store_root = file.path(root, "weather"),
    artifact_dir = file.path(root, "artifact"), handle = structure(list(), class = "mirai"),
    on_status = NULL
  ), parent = emptyenv())
  dir.create(job$artifact_dir, recursive = TRUE)
  dir.create(job$weather_store_root, recursive = TRUE)
  assign(job$id, job, envir = state$jobs)
  state$active <- job$id
  state$queue <- "next-job"

  expect_true(.wise_step2_async_cancel(job$id, "superseded"))
  expect_true(job$retired)
  expect_identical(state$active, job$id)
  expect_identical(state$queue, "next-job")
  expect_true(file.exists(job$retired_file))
  expect_true(dir.exists(job$artifact_dir))
  expect_true(dir.exists(job$weather_store_root))
})

testthat::test_that("progress latest record validates identity and sequence", {
  root <- tempfile("wiseapp-async-progress-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(root, "progress.rds")
  progress <- list(
    schema = 1L, job_id = "job-progress", generation = 4L,
    dependency_signature_digest = "digest", sequence = 2L,
    stage = "pipeline", status = "progress", elapsed = 1.5,
    completed = 3L, detail = "must not be forwarded"
  )
  saveRDS(progress, path)
  received <- list()
  job <- list2env(list(
    id = progress$job_id, generation = progress$generation,
    dependency_signature_digest = progress$dependency_signature_digest,
    progress_file = path, progress_sequence = 0L,
    on_progress = function(record, ...) received[[length(received) + 1L]] <<- record
  ), parent = emptyenv())

  .wise_step2_async_poll_progress(job)
  expect_length(received, 1L)
  expect_identical(received[[1L]]$completed, 3L)
  expect_false("detail" %in% names(received[[1L]]))
  .wise_step2_async_poll_progress(job)
  expect_length(received, 1L)
})

testthat::test_that("async cleanup can preserve adopted weather artifacts", {
  root <- tempfile("wiseapp-async-adopt-")
  artifact_dir <- file.path(root, "artifact")
  weather_dir <- file.path(root, "weather")
  dir.create(artifact_dir, recursive = TRUE)
  dir.create(weather_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  job <- list(artifact_dir = artifact_dir, weather_store_root = weather_dir)

  .wise_step2_async_cleanup_job(job, remove_weather = FALSE)
  expect_false(dir.exists(artifact_dir))
  expect_true(dir.exists(weather_dir))
})

testthat::test_that("worker result artifacts are atomically published", {
  root <- tempfile("wiseapp-async-worker-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::skip_if_not(exists("step2_compute", mode = "function"))
  # The full numerical worker is covered by Step 2 parity fixtures. This test
  # verifies the worker rejects malformed snapshots before creating artifacts.
  testthat::expect_error(
    step2_async_worker(
      snapshot = list(input = NULL), job_id = "job-1", generation = 1L,
      artifact_dir = file.path(root, "job"),
      weather_store_root = file.path(root, "weather"), seed = 1L
    ),
    "requires an ordinary snapshot"
  )
  testthat::expect_false(file.exists(file.path(root, "job", "manifest.rds")))
})

testthat::test_that("async worker matches synchronous Step 2 fixture output", {
  skip_if_not_installed("mirai")
  input <- list(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
      label = "Welfare", stringsAsFactors = FALSE),
    svy = data.frame(
      hhid = c("hh01", "hh02"), code = "TST", year = 2020L,
      survname = "SRV", loc_id = "loc01", welfare = c(1, 2),
      temp = c(1, 1), weight = c(1, 2), stringsAsFactors = FALSE
    ),
    ss = NULL,
    mf = list(fit3 = NULL, engine = "fixest", train_data = data.frame(x = 1:2),
      weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")), ssps = "ssp2_4_5",
    residuals = "none", skip_coef_draws = TRUE,
    sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL, stored_breaks = NULL
  )
  weather <- local({
    hist <- data.frame(code = "TST", year = 2020L, survname = "SRV",
      loc_id = "loc01", temp = 1,
      timestamp = as.POSIXct("2020-06-01", tz = "UTC"), stringsAsFactors = FALSE)
    mean_key <- hist
    mean_key$timestamp <- as.POSIXct("2030-06-01", tz = "UTC")
    mean_key$temp <- 2
    hi_key <- mean_key
    hi_key$temp <- 3
    list(historical = hist,
      ssp2_4_5_2030_2040_ensemble_mean = mean_key,
      ssp2_4_5_2030_2040_ensemble_hi = hi_key)
  })
  pipeline <- function(weather_raw, ...) list(
    y_point = c(1, 2), F_loading = NULL, sim_year = c(2030L, 2030L),
    weight = c(1, 2), id_vec = c("hh01", "hh02"), id_col = "hhid",
    svy_row_id = c(1L, 2L), n_pre_join = 2L, weather_raw = weather_raw,
    train_aug = data.frame(hhid = c("hh01", "hh02"), .resid = c(0.1, -0.1))
  )
  run_id <- "async-parity"
  synchronous <- suppressWarnings(step2_compute(
    input, seed = 123L, run_id = run_id,
    weather_fn = function(...) weather,
    pipeline_fn = pipeline
  ))

  root <- tempfile("wiseapp-async-parity-")
  artifact_dir <- file.path(root, "artifact")
  weather_root <- file.path(root, "weather")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  package_path <- getNamespaceInfo(asNamespace("wiseapp"), "path")
  mirai::daemons(1L)
  on.exit(mirai::daemons(0L), add = TRUE)
  async <- mirai::mirai({
    pkgload::load_all(package_path, export_all = FALSE, helpers = FALSE,
      attach_testthat = FALSE, quiet = TRUE)
    wiseapp:::step2_async_worker(
      snapshot = list(input = input), job_id = "async-parity-job",
      generation = 1L, artifact_dir = artifact_dir,
       weather_store_root = weather_root, seed = 123L, run_id = "async-parity",
      weather_fn = function(...) weather_data,
      pipeline_fn = pipeline_fn
    )
  }, package_path = package_path, input = input,
  artifact_dir = artifact_dir, weather_root = weather_root,
  weather_data = weather,
  pipeline_fn = pipeline)
  deadline <- Sys.time() + 30
  while (mirai::unresolved(async) && Sys.time() < deadline) Sys.sleep(0.05)
  manifest <- async[]
  if (mirai::is_error_value(manifest)) {
    stop(as.character(manifest), call. = FALSE)
  }
  expect_identical(manifest$status, "succeeded")
  expect_identical(manifest$codec, "qs2")
  expect_identical(manifest$schema, 2L)
  worker_result <- qs2::qs_read(
    file.path(artifact_dir, manifest$result_basename),
    nthreads = 1L, validate_checksum = TRUE
  )

  expect_identical(worker_result$hist_sim_result$pipeline$y_point,
    synchronous$result$hist_sim_result$pipeline$y_point)
  expect_identical(worker_result$n_keys, synchronous$result$n_keys)
  expect_identical(worker_result$n_keys_ok, synchronous$result$n_keys_ok)
  expect_identical(worker_result$failures, synchronous$result$failures)
  expect_identical(manifest$result_signature, synchronous$signature)
})


testthat::test_that("async sync-mode flag defaults off and honors env override", {
  withr::local_envvar(WISEAPP_ASYNC_SYNC = NA)
  expect_false(.wise_step2_async_sync())

  withr::local_envvar(WISEAPP_ASYNC_SYNC = "1")
  expect_true(.wise_step2_async_sync())

  withr::local_envvar(WISEAPP_ASYNC_SYNC = "0")
  expect_false(.wise_step2_async_sync())

  withr::local_envvar(WISEAPP_ASYNC_SYNC = "garbage")
  expect_false(.wise_step2_async_sync())
})
