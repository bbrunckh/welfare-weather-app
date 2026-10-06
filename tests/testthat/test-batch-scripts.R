# Batch scripts are not run by the suite (they need real data and long runs),
# but they must at least parse, and calls to shared batch helpers must match
# the helper signatures.

batch_dir <- function() testthat::test_path("..", "..", "batch")

batch_files <- function() {
  list.files(batch_dir(), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
}

test_that("every batch script parses", {
  files <- batch_files()
  skip_if(!length(files), "batch/ not available (installed package)")
  for (f in files) {
    expect_no_error(parse(f, keep.source = FALSE))
  }
})

# Collect the argument counts of every call to `fun` in an expression tree.
call_arg_counts <- function(expr, fun) {
  if (is.call(expr)) {
    head <- expr[[1L]]
    here <- if (is.name(head) && identical(as.character(head), fun)) length(expr) - 1L else integer(0)
    return(c(here, unlist(lapply(as.list(expr), call_arg_counts, fun = fun), use.names = FALSE)))
  }
  if (is.expression(expr) || is.list(expr)) {
    return(unlist(lapply(as.list(expr), call_arg_counts, fun = fun), use.names = FALSE))
  }
  integer(0)
}

test_that("batch calls to weather_agg_for pass (var, var_info, override)", {
  files <- batch_files()
  skip_if(!length(files), "batch/ not available (installed package)")
  for (f in files) {
    counts <- call_arg_counts(parse(f, keep.source = FALSE), "weather_agg_for")
    expect_true(all(counts >= 2L & counts <= 3L), info = basename(f))
    # The two-argument form (var, override) used to be passed here by mistake:
    # it hands the override list in as var_info.
    calls <- paste(deparse(parse(f, keep.source = FALSE)), collapse = "\n")
    expect_false(grepl("weather_agg_for\\(v, WEATHER_AGG_OVERRIDE\\)", calls), info = basename(f))
  }
})

test_that("weather_agg_for sums mm and day units and averages the rest", {
  utils_file <- file.path(batch_dir(), "R", "batch_utils.R")
  skip_if(!file.exists(utils_file), "batch/ not available (installed package)")
  env <- new.env(parent = asNamespace("wiseapp"))
  sys.source(utils_file, envir = env)
  var_info <- data.frame(name = c("pr", "tx"), units = c("mm", "degC"))
  expect_identical(env$weather_agg_for("pr", var_info), "Sum")
  expect_identical(env$weather_agg_for("tx", var_info), "Mean")
  expect_identical(env$weather_agg_for("tx", var_info, list(tx = "Max")), "Max")
})
