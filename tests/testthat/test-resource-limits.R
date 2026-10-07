# Per-process resource limits (CR-PERF-13 / R2-PERF-08).

duck_setting <- function(con, name) {
  DBI::dbGetQuery(con, sprintf("SELECT current_setting('%s') AS v", name))$v
}

new_con <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

test_that("DuckDB memory limit and threads come from the environment", {
  withr::local_envvar(WISEAPP_DUCKDB_MEMORY_LIMIT = "512MB",
    WISEAPP_DUCKDB_THREADS = "2")
  con <- new_con()
  .duck_apply_limits(con)
  expect_match(duck_setting(con, "memory_limit"), "^(512|488)(\\.[0-9]+)? ?(MB|MiB)$")
  expect_identical(as.integer(duck_setting(con, "threads")), 2L)
})

test_that("an invalid memory limit is ignored with a warning", {
  withr::local_envvar(WISEAPP_DUCKDB_MEMORY_LIMIT = "lots; DROP TABLE x")
  con <- new_con()
  before <- duck_setting(con, "memory_limit")
  expect_warning(.duck_apply_limits(con), "invalid WISEAPP_DUCKDB_MEMORY_LIMIT")
  expect_identical(duck_setting(con, "memory_limit"), before)
})

test_that("without limits DuckDB keeps its defaults but spills to a temp directory", {
  withr::local_envvar(WISEAPP_DUCKDB_MEMORY_LIMIT = NA, WISEAPP_DUCKDB_THREADS = NA,
    WISEAPP_DUCKDB_TEMP_DIR = NA)
  con <- new_con()
  default_threads <- duck_setting(con, "threads")
  .duck_apply_limits(con)
  expect_identical(duck_setting(con, "threads"), default_threads)
  spill <- duck_setting(con, "temp_directory")
  expect_true(startsWith(normalizePath(spill, mustWork = FALSE),
    normalizePath(tempdir(), mustWork = FALSE)))
  expect_true(dir.exists(spill))
})

test_that("the thread cap applies to fixest and collapse", {
  old_fixest <- fixest::getFixest_nthreads()
  old_collapse <- collapse::get_collapse()$nthreads
  withr::defer({
    fixest::setFixest_nthreads(old_fixest)
    collapse::set_collapse(nthreads = old_collapse)
  })
  withr::local_envvar(WISEAPP_THREADS = "1")
  expect_true(.wise_apply_thread_limits())
  expect_identical(fixest::getFixest_nthreads(), 1L)
  expect_identical(collapse::get_collapse()$nthreads, 1L)

  withr::local_envvar(WISEAPP_THREADS = NA)
  expect_false(.wise_apply_thread_limits())
})

test_that("invalid thread values are ignored", {
  withr::local_envvar(WISEAPP_THREADS = "0")
  expect_false(.wise_apply_thread_limits())
  withr::local_envvar(WISEAPP_THREADS = "many")
  expect_false(.wise_apply_thread_limits())
})
