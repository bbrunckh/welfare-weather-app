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
