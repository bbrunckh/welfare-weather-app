# Shared mirai daemon health (R2-OPS-06): a dead daemon must be replaced, and a
# stuck task must not hold the daemon indefinitely.

wait_for <- function(cond, timeout = 15) {
  deadline <- Sys.time() + timeout
  while (!isTRUE(cond()) && Sys.time() < deadline) Sys.sleep(0.1)
  isTRUE(cond())
}

local_daemon <- function(env = parent.frame()) {
  state <- .wise_step2_async_state
  withr::local_envvar(WISEAPP_ASYNC_SYNC = "0", .local_envir = env)
  try(mirai::daemons(0L), silent = TRUE)
  state$started <- FALSE
  withr::defer({
    try(mirai::daemons(0L), silent = TRUE)
    state$started <- FALSE
  }, envir = env)
  .wise_step2_async_launch()
  expect_true(wait_for(.wise_step2_async_daemon_alive))
}

test_that("timeouts are read from the environment and can be disabled", {
  withr::local_envvar(WISEAPP_ASYNC_TIMEOUT_MIN = "2",
    WISEAPP_ASYNC_METADATA_TIMEOUT_SEC = "30")
  expect_identical(.wise_step2_async_timeout_ms("step2"), 120000)
  expect_identical(.wise_step2_async_timeout_ms("metadata"), 30000)

  withr::local_envvar(WISEAPP_ASYNC_TIMEOUT_MIN = "0",
    WISEAPP_ASYNC_METADATA_TIMEOUT_SEC = "nonsense")
  expect_null(.wise_step2_async_timeout_ms("step2"))
  expect_null(.wise_step2_async_timeout_ms("metadata"))

  withr::local_envvar(WISEAPP_ASYNC_TIMEOUT_MIN = NA)
  expect_identical(.wise_step2_async_timeout_ms("step2"), 90 * 60 * 1000)
})

test_that("mirai error values are described for users", {
  timed_out <- .wise_step2_async_describe_error(simpleError("5 | Timed out"))
  expect_s3_class(timed_out, "wise_async_error")
  expect_match(conditionMessage(timed_out), "too long")

  reset <- .wise_step2_async_describe_error(simpleError("19 | Connection reset"))
  expect_match(conditionMessage(reset), "stopped unexpectedly")

  other <- simpleError("boom")
  expect_identical(.wise_step2_async_describe_error(other), other)
})

test_that("a dead daemon is relaunched and runs tasks again", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  local_mocked_bindings(.WISE_ASYNC_DAEMON_GRACE_SEC = 0)
  local_daemon()

  pid <- mirai::mirai(Sys.getpid(), .compute = "default")
  mirai::call_mirai(pid)
  tools::pskill(pid$data, tools::SIGKILL)
  expect_true(wait_for(function() !.wise_step2_async_daemon_alive()))

  expect_true(.wise_step2_async_ensure_daemon())
  expect_true(wait_for(.wise_step2_async_daemon_alive))

  task <- mirai::mirai(1 + 1, .compute = "default")
  mirai::call_mirai(task)
  expect_identical(task$data, 2)
})

test_that("a daemon that is still starting is not relaunched", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  local_daemon()
  state <- .wise_step2_async_state
  state$launched_at <- proc.time()[["elapsed"]]
  local_mocked_bindings(.wise_step2_async_daemon_alive = function() FALSE)
  expect_false(.wise_step2_async_ensure_daemon())
})

test_that("a task past its timeout is interrupted and the daemon stays usable", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  local_daemon()

  slow <- mirai::try_mirai({Sys.sleep(30); "late"}, .compute = "default", .timeout = 500)
  mirai::call_mirai(slow)
  expect_true(mirai::is_error_value(slow$data))
  expect_identical(as.integer(slow$data), 5L)

  quick <- mirai::try_mirai("ok", .compute = "default")
  started <- Sys.time()
  mirai::call_mirai(quick)
  expect_identical(quick$data, "ok")
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 10)
})

# R2-SEC-01 follow-up: UI credentials only ever go to local daemons.
test_that("daemon transports are classified as local or remote", {
  skip_if_not_installed("mirai")
  url <- NULL
  local_mocked_bindings(status = function(...) list(connections = 1L, daemons = url),
    .package = "mirai")
  for (u in c("ipc:///tmp/abc", "abstract://abc", "inproc://abc")) {
    url <- u
    expect_true(.wise_async_daemons_local(), info = u)
  }
  for (u in c("tcp://10.0.0.5:5555", "tls+tcp://host:5555", "ws://h:1")) {
    url <- u
    expect_false(.wise_async_daemons_local(), info = u)
  }
  url <- 0L
  expect_false(.wise_async_daemons_local())

  url <- "tcp://10.0.0.5:5555"
  expect_error(.wise_async_require_local_daemon(list(type = "s3", origin = "ui")),
    "not local")
  expect_true(.wise_async_require_local_daemon(list(type = "s3", origin = "env")))
  url <- "ipc:///tmp/abc"
  expect_true(.wise_async_require_local_daemon(list(type = "s3", origin = "ui")))
})

test_that("the coordinator's own daemon is local", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  local_daemon()
  expect_true(.wise_async_daemons_local())
})

test_that("Step 2 dispatch refuses to send UI credentials to a remote daemon", {
  skip_if_not_installed("mirai")
  state <- .wise_step2_async_state
  old_active <- state$active
  old_queue <- state$queue
  on.exit({ state$active <- old_active; state$queue <- old_queue }, add = TRUE)
  local_mocked_bindings(.wise_step2_async_ensure_daemon = function() invisible(FALSE))
  local_mocked_bindings(
    status = function(...) list(connections = 1L, daemons = "tcp://10.0.0.5:5555"),
    try_mirai = function(...) stop("credentials must not be submitted"),
    .package = "mirai"
  )
  root <- withr::local_tempdir()
  ctl <- file.path(root, "control")
  dir.create(ctl)
  failure <- NULL
  job <- list2env(list(
    id = "remote-refused", session_id = "s", generation = 1L,
    snapshot = list(input = list(cp = list(type = "s3", origin = "ui",
      key_id = "user-key", secret = "user-secret"))),
    artifact_dir = file.path(root, "artifact"), control_dir = ctl,
    lock_file = file.path(ctl, "publication.lock"),
    retired_file = file.path(ctl, "retired.rds"),
    weather_store_root = file.path(root, "weather"),
    retired = FALSE, detached = FALSE,
    on_error = function(e, job) failure <<- e
  ), parent = emptyenv())
  assign(job$id, job, envir = state$jobs)
  state$active <- NULL
  state$queue <- list(job$id)
  .wise_step2_async_dispatch()
  expect_match(conditionMessage(failure), "not local")
  expect_null(state$active)
  expect_false(exists(job$id, envir = state$jobs, inherits = FALSE))
})

test_that("Overview metadata refuses to send UI credentials to a remote daemon", {
  skip_if_not_installed("mirai")
  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "1", WISEAPP_ASYNC_SYNC = "0")
  submissions <- 0L
  local_mocked_bindings(.wise_step2_async_init = function() TRUE)
  local_mocked_bindings(
    status = function(...) list(connections = 1L, daemons = "tcp://10.0.0.5:5555"),
    try_mirai = function(...) { submissions <<- submissions + 1L; NULL },
    .package = "mirai"
  )
  failure <- NULL
  .overview_metadata_load(
    list(type = "s3", bucket = "b", key_id = "k", secret = "s", origin = "ui"),
    function(x) stop("unexpected adoption"), function(e) failure <<- e,
    function() TRUE
  )
  expect_equal(submissions, 0L)
  expect_match(conditionMessage(failure), "not local")
})
