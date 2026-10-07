# Run-stage log (observability for Posit Connect process logs).

capture_stage <- function(...) {
  msgs <- testthat::capture_messages(.wise_log_stage(...))
  expect_length(msgs, 1L)
  sub("\n$", "", msgs)
}

test_that("a stage line carries the run metadata as key=value fields", {
  line <- capture_stage("step2_run", "succeeded", run_id = "job-1", elapsed = 12.345,
    keys = 17, cache = "warm", rss_mb = 2011.4)
  expect_identical(
    line,
    "[wiseapp] stage=step2_run outcome=succeeded run=job-1 keys=17 cache=warm elapsed_s=12.3 rss_mb=2011"
  )
})

test_that("missing fields are left out", {
  line <- capture_stage("step3_run", "failed", elapsed = 2, rss_mb = NA)
  expect_identical(line, "[wiseapp] stage=step3_run outcome=failed elapsed_s=2")
})

test_that("values cannot add lines or fields", {
  line <- capture_stage("step2_run\nstage=forged", "ok outcome=forged",
    run_id = "a b\nc", rss_mb = NULL)
  expect_false(grepl("\n", line))
  # Only the real fields remain: one stage= and one outcome=.
  expect_identical(lengths(regmatches(line, gregexpr("stage=", line))), 1L)
  expect_identical(lengths(regmatches(line, gregexpr("outcome=", line))), 1L)
})

test_that("the stage log can be switched off", {
  withr::local_envvar(WISEAPP_STAGE_LOG = "0")
  expect_silent(.wise_log_stage("step2_run", "succeeded"))
})

test_that("resident memory is readable on this platform", {
  rss <- .wise_rss_mb()
  expect_true(is.na(rss) || rss > 0)
})

test_that("wise_user_error logs the full condition and shows a short message (CR-SEC-08)", {
  e <- simpleError(paste(
    "load_data(): Failed to open dataset.\n  paths : https://adb-1.example.net/api/2.0/fs/files/Volumes/cat/x.parquet",
    "\n  error : HTTP 404 Not Found"))
  logged <- capture_messages(text <- wise_user_error(e, "Simulation"))
  id <- sub(".*\\(error id ([0-9a-f]{8})\\)$", "\\1", text)
  expect_match(id, "^[0-9a-f]{8}$")
  expect_match(text, "^Simulation failed: A required data file was not found")
  expect_false(grepl("adb-1|/Volumes/", text))
  expect_match(paste(logged, collapse = ""), paste0("error id=", id), fixed = TRUE)
  expect_match(paste(logged, collapse = ""), "adb-1.example.net", fixed = TRUE)

  quiet <- function(x, ...) suppressMessages(wise_user_error(x, ...))
  expect_match(quiet(simpleError("Step 2 result too large: try fewer periods.")),
    "^Step 2 result too large: try fewer periods\\. \\(error id")
  expect_match(quiet(simpleError("cannot open file '/srv/data/x.csv'")),
    "^An unexpected error occurred")
  expect_match(quiet(simpleError("HTTP 401 Unauthorized")), "rejected the request")
  expect_match(quiet(simpleError("5 | Timed out")), "took too long")
  async <- structure(class = c("wise_async_error", "error", "condition"),
    list(message = "The background run took too long and was stopped.", call = NULL))
  expect_match(quiet(async), "^The background run took too long and was stopped\\.")
})

test_that("Databricks file errors keep host and volume out of the message (CR-SEC-08)", {
  url <- "https://adb-1.example.net/api/2.0/fs/files/Volumes/cat/sch/vol/metadata/survey_list.csv"
  resp <- httr2::response(status_code = 404, url = url)
  logged <- character(0)
  err <- withCallingHandlers(
    tryCatch(.parse_db_csv_response(resp, url), error = identity),
    message = function(m) {
      logged <<- c(logged, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msg <- conditionMessage(err)
  expect_match(msg, "Reading survey_list.csv from Databricks failed", fixed = TRUE)
  expect_match(msg, "not found")
  expect_false(grepl("adb-1|/Volumes/", msg))
  expect_true(any(grepl("adb-1.example.net", logged, fixed = TRUE)))
})

test_that("Step 2 failure notifications go through wise_user_error (CR-SEC-08)", {
  src <- testthat::test_path("..", "..", "R", "mod_2_01_weathersim.R")
  skip_if(!file.exists(src), "R/ source tree not available (installed package)")
  text <- readLines(src, warn = FALSE)
  expect_false(any(grepl("Simulation failed: \", conditionMessage", text, fixed = TRUE)))
  expect_length(grep("wise_user_error(", text, fixed = TRUE), 3L)
})
