library(testthat)

test_that("build_connection_params returns correct list for local", {
  p <- build_connection_params("local", path = "/data/foo")
  expect_equal(p$type, "local")
  expect_equal(p$path, "/data/foo")
})

test_that("build_connection_params errors on unknown type", {
  expect_error(build_connection_params("unknown"), "Unknown connection type")
})

test_that("validate_connection_params: local requires non-empty path", {
  expect_true(validate_connection_params(list(type = "local", path = "/data")))
  expect_false(validate_connection_params(list(type = "local", path = "")))
  expect_false(validate_connection_params(NULL))
})

test_that("validate_connection_params: s3 requires bucket", {
  expect_true(validate_connection_params(list(type = "s3", bucket = "my-bucket")))
  expect_false(validate_connection_params(list(type = "s3", bucket = "")))
})

test_that("validate_connection_params: azure requires account and container", {
  expect_true(validate_connection_params(list(type = "azure", account = "acc", container = "con")))
  expect_false(validate_connection_params(list(type = "azure", account = "acc", container = "")))
})

test_that("default_poverty_lines returns 3 rows with expected values", {
  pl <- default_poverty_lines()
  expect_s3_class(pl, "data.frame")
  expect_equal(nrow(pl), 3)
  expect_equal(pl$ln, c(3.00, 4.20, 8.30))
  expect_true(all(pl$ppp_year == 2021))
})

test_that("normalise_local_path errors on empty string", {
  expect_error(normalise_local_path(""), "non-empty")
})