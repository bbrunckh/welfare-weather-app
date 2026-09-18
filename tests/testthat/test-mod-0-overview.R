# Overview module auto-connect smoke tests (Posit Connect / Databricks path).
#
# The auto-connect flow needs RSTUDIO_PRODUCT=CONNECT plus the four
# DATABRICKS_* variables; the host points at a refused local port so both the
# synchronous fallback and the async worker path fail fast and deterministically.

library(testthat)

.status_html <- function(output) {
  # uiOutput renders carry html dependencies, so collapse to one string.
  tryCatch(
    paste(as.character(output$connection_status_ui), collapse = "\n"),
    error = function(e) ""
  )
}


test_that("auto-connect surfaces failure when async is disabled", {
  skip_if_not_installed("httr2")
  # Env must be set in this frame (not a helper): withr restores at helper exit.
  withr::local_envvar(
    RSTUDIO_PRODUCT = "CONNECT",
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
      if (grepl("Failed to connect to Databricks", .status_html(output)) ||
        Sys.time() > deadline) {
        break
      }
    }
    expect_match(.status_html(output), "Failed to connect to Databricks",
      fixed = TRUE
    )
    expect_match(.status_html(output), "DATABRICKS_HOST", fixed = TRUE)
  })
})


test_that("auto-connect dispatches the metadata load to the mirai worker", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("httr2")
  withr::local_envvar(
    RSTUDIO_PRODUCT = "CONNECT",
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
      expect_match(html, "Connecting to Databricks", fixed = TRUE)
      expect_true(.wise_step2_async_state$started)
      # Pump the later loop while the session is open until the worker's
      # (refused-connection) error propagates back through the promise and
      # reaches auto_connect_fail, which persists the error state.
      deadline <- Sys.time() + 20
      repeat {
        new_msgs <- testthat::capture_messages(later::run_now(0.05))
        .acc$msgs <- c(.acc$msgs, new_msgs)
        if (any(grepl("auto-connect to Databricks failed", .acc$msgs)) ||
          Sys.time() > deadline) {
          break
        }
      }
    })
  })
  msgs <- c(outer_msgs, .acc$msgs)
  expect_true(any(grepl("auto-connecting to Databricks", msgs)))
  expect_true(any(grepl("auto-connect to Databricks failed", msgs)))
})
