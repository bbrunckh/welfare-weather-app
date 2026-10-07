# Overview module automatic connection smoke tests.
#
# The connection flow uses WISEAPP_DATA_SOURCE plus source-specific variables.

library(testthat)

.status_html <- function(output) {
  # uiOutput renders carry html dependencies, so collapse to one string.
  tryCatch(
    paste(as.character(output$connection_status_ui), collapse = "\n"),
    error = function(e) ""
  )
}

.card_html <- function(output) {
  tryCatch(
    paste(as.character(output$connection_card_ui), collapse = "\n"),
    error = function(e) ""
  )
}


test_that("auto-connect surfaces failure when async is disabled", {
  # Env must be set in this frame (not a helper): withr restores at helper exit.
  withr::local_envvar(
    RSTUDIO_PRODUCT = "CONNECT",
    WISEAPP_DATA_SOURCE = "databricks",
    DATABRICKS_HOST = "https://localhost:1",
    DATABRICKS_CLIENT_ID = "test-client-id",
    DATABRICKS_CLIENT_SECRET = "test-client-secret",
    DATABRICKS_VOLUME_PATH = "/Volumes/test",
    WISEAPP_METADATA_CACHE_DISABLE = "1",
    WISEAPP_ASYNC_STEP2 = "0"
  )

  testServer(mod_0_overview_server, {
    deadline <- Sys.time() + 15
    repeat {
      later::run_now(0.05)
      if (grepl("Failed to connect to the configured data source", .status_html(output)) ||
        Sys.time() > deadline) {
        break
      }
    }
    expect_match(.status_html(output), "Failed to connect to the configured data source",
      fixed = TRUE
    )
    expect_match(.status_html(output), "WISEAPP_DATA_SOURCE", fixed = TRUE)
    # CR-SEC-01: auto-connect mode never falls back to the browser form.
    expect_false(grepl("Connect to data", .card_html(output), fixed = TRUE))
  })
})


test_that("auto-connect dispatches the metadata load to the mirai worker", {
  withr::local_envvar(
    RSTUDIO_PRODUCT = "CONNECT",
    WISEAPP_DATA_SOURCE = "databricks",
    DATABRICKS_HOST = "https://localhost:1",
    DATABRICKS_CLIENT_ID = "test-client-id",
    DATABRICKS_CLIENT_SECRET = "test-client-secret",
    DATABRICKS_VOLUME_PATH = "/Volumes/test",
    WISEAPP_METADATA_CACHE_DISABLE = "1",
    WISEAPP_ASYNC_STEP2 = "1",
    WISEAPP_ASYNC_SYNC = "0"
  )

  .acc <- new.env(parent = emptyenv())
  .acc$msgs <- character()
  outer_msgs <- testthat::capture_messages({
    testServer(mod_0_overview_server, {
      # Async dispatch must not block or fail during the init flush: the
      # status card still shows the connecting spinner while the worker task
      # is in flight (the mock session does not re-render outputs after
      # later callbacks, so terminal states are asserted via the sync
      # fallback test, which shares the same status plumbing).
      html <- .status_html(output)
      expect_match(html, "Connecting to data source", fixed = TRUE)
      expect_true(.wise_step2_async_state$started)
      # Pump the later loop while the session is open until the worker's
      # (refused-connection) error propagates back through the promise and
      # reaches auto_connect_fail, which persists the error state.
      deadline <- Sys.time() + 20
      repeat {
        new_msgs <- testthat::capture_messages(later::run_now(0.05))
        .acc$msgs <- c(.acc$msgs, new_msgs)
        if (any(grepl("automatic data-source connection failed", .acc$msgs)) ||
          Sys.time() > deadline) {
          break
        }
      }
    })
  })
  msgs <- c(outer_msgs, .acc$msgs)
  expect_true(any(grepl("auto-connecting to databricks", msgs)))
  expect_true(any(grepl("automatic data-source connection failed", msgs)))
})

test_that("manual metadata adoption ignores superseded and disconnected requests", {
  withr::local_envvar(WISEAPP_DATA_SOURCE = NA)
  requests <- list()
  local_mocked_bindings(.overview_metadata_load = function(params, on_result,
    on_error, is_current) {
    requests[[length(requests) + 1L]] <<- list(
      params = params, result = on_result, current = is_current)
  }, .package = "wiseapp")
  metadata <- list(survey_list = data.frame(code = "TST"),
    variable_list = data.frame(name = "welfare"), cpi_ppp = data.frame(cpi = 1),
    pov_lines = data.frame(line = 3))
  testServer(mod_0_overview_server, {
    session$setInputs(connection_type = "local", local_path = tempdir())
    session$setInputs(apply_connection = 1L)
    expect_length(requests, 1L)
    expect_null(applied_connection())
    session$setInputs(apply_connection = 2L)
    expect_false(requests[[1L]]$current())
    requests[[1L]]$result(metadata)
    expect_null(applied_connection())
    requests[[2L]]$result(metadata)
    expect_identical(survey_list(), metadata$survey_list)
    expect_identical(variable_list(), metadata$variable_list)
    expect_identical(cpi_ppp(), metadata$cpi_ppp)
    expect_identical(pov_lines(), metadata$pov_lines)
    expect_identical(applied_connection()$type, "local")
    session$setInputs(apply_connection = 3L)
    session$setInputs(local_path = paste0(tempdir(), "/changed"))
    expect_false(requests[[3L]]$current())
    session$close()
    expect_false(requests[[3L]]$current())
  })
})

test_that("warm automatic connection publishes parent metadata without a worker", {
  withr::local_envvar(WISEAPP_DATA_SOURCE = "local", WISEAPP_DATA_PATH = tempdir(),
    WISEAPP_METADATA_CACHE_DISABLE = "0")
  params <- auto_connection_params()
  metadata <- list(survey_list = data.frame(code = "TST"),
    variable_list = data.frame(name = "welfare"), cpi_ppp = data.frame(cpi = 1),
    pov_lines = data.frame(line = 3))
  overview_metadata_cache_store(params, metadata)
  on.exit(rm(list = .overview_metadata_cache_key(params),
    envir = .overview_metadata_cache), add = TRUE)
  local_mocked_bindings(.wise_step2_async_init = function() {
    stop("warm automatic connection must not start a worker")
  }, .package = "wiseapp")
  testServer(mod_0_overview_server, {
    session$flushReact()
    expect_identical(applied_connection(), params)
    expect_identical(survey_list(), metadata$survey_list)
    expect_identical(pov_lines(), metadata$pov_lines)
  })
})

test_that("auto-connect mode ignores browser connection inputs (CR-SEC-01)", {
  withr::local_envvar(
    WISEAPP_DATA_SOURCE = "databricks",
    DATABRICKS_HOST = "https://configured.cloud.databricks.com",
    DATABRICKS_CLIENT_ID = "env-client-id",
    DATABRICKS_CLIENT_SECRET = "env-client-secret",
    DATABRICKS_VOLUME_PATH = "/Volumes/env"
  )
  requests <- list()
  local_mocked_bindings(.overview_metadata_load = function(params, on_result,
    on_error, is_current) {
    requests[[length(requests) + 1L]] <<- params
  }, .package = "wiseapp")
  testServer(mod_0_overview_server, {
    session$flushReact()
    expect_length(requests, 1L)
    expect_identical(requests[[1L]]$origin, "env")
    # A crafted client sends connection fields and clicks apply anyway.
    session$setInputs(
      connection_type = "databricks",
      db_workspace = "https://attacker.cloud.databricks.com",
      db_client_id = "", db_client_secret = "", db_volume_path = "/Volumes/x"
    )
    session$setInputs(apply_connection = 1L)
    session$setInputs(apply_connection = 2L)
    expect_length(requests, 1L)
    expect_identical(requests[[1L]]$workspace,
                     "https://configured.cloud.databricks.com")
    expect_error(connection_params())
  })
})
