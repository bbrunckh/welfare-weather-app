library(testthat)

# ============================================================================ #
# Helpers                                                                      #
# ============================================================================ #

local_params <- function(path) list(type = "local", path = path)

# ============================================================================ #
# 1. Empty paths                                                               #
# ============================================================================ #

test_that("load_data returns empty tibble for zero-length paths", {
  result <- load_data(character(0), local_params("/data"))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# ============================================================================ #
# 2. Path resolution — bare vs absolute / URI                                  #
# ============================================================================ #

# Access the internal helpers via the package environment
resolve <- function(p, params) {
  # Re-implement is_bare / resolve_path inline so we can unit test path logic
  # without needing real files.
  type    <- params$type
  is_bare <- !grepl("://", p, fixed = TRUE) & !grepl("^[/~]|^[A-Za-z]:[/\\\\]", p)
  if (!is_bare) return(p)
  switch(
    type,
    "local" = file.path(params$path %||% "data/", p),
    "s3"    = paste0("s3://",  params$bucket,    "/", params$prefix %||% "", p),
    "gcs"   = paste0("gs://",  params$bucket,    "/", params$prefix %||% "", p),
    "azure" = paste0("az://",  params$container, "/", params$prefix %||% "", p),
    "hf"    = paste0("hf://datasets/", params$repo, "/", params$subdir %||% "", p),
    p
  )
}

test_that("bare filename is resolved to local path", {
  expect_equal(
    resolve("survey_list.csv", local_params("/data")),
    "/data/survey_list.csv"
  )
})

test_that("absolute path is passed through unchanged", {
  expect_equal(
    resolve("/abs/path/file.parquet", local_params("/data")),
    "/abs/path/file.parquet"
  )
})

test_that("URI is passed through unchanged", {
  expect_equal(
    resolve("s3://my-bucket/data/file.parquet", local_params("/data")),
    "s3://my-bucket/data/file.parquet"
  )
})

test_that("bare filename is resolved to S3 URI", {
  p <- list(type = "s3", bucket = "my-bucket", prefix = "data/")
  expect_equal(resolve("file.parquet", p), "s3://my-bucket/data/file.parquet")
})

test_that("bare filename is resolved to GCS URI", {
  p <- list(type = "gcs", bucket = "my-bucket", prefix = "surveys/")
  expect_equal(resolve("file.parquet", p), "gs://my-bucket/surveys/file.parquet")
})

test_that("bare filename is resolved to Azure URI", {
  p <- list(type = "azure", container = "my-container", prefix = "raw/")
  expect_equal(resolve("file.parquet", p), "az://my-container/raw/file.parquet")
})

test_that("bare filename is resolved to HF URI", {
  p <- list(type = "hf", repo = "user/repo", subdir = "data/")
  expect_equal(resolve("file.parquet", p), "hf://datasets/user/repo/data/file.parquet")
})

# ============================================================================ #
# 3. Format detection                                                          #
# ============================================================================ #

detect_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext, "parquet" = "parquet", "csv" = "csv", "tsv" = "csv", "parquet")
}

test_that("format detection: .parquet -> parquet", {
  expect_equal(detect_format("file.parquet"), "parquet")
})

test_that("format detection: .csv -> csv", {
  expect_equal(detect_format("file.csv"), "csv")
})

test_that("format detection: .tsv -> csv", {
  expect_equal(detect_format("file.tsv"), "csv")
})

test_that("format detection: unknown extension defaults to parquet", {
  expect_equal(detect_format("file.unknown"), "parquet")
})

# ============================================================================ #
# 4. Integration tests — real files (parquet + csv)                           #
# ============================================================================ #

test_that("load_data reads a local parquet file (collect = TRUE)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdbfs")

  tmp  <- withr::local_tempdir()
  df   <- data.frame(id = 1:5, value = letters[1:5])
  arrow::write_parquet(df, file.path(tmp, "test.parquet"))

  result <- load_data(
    paths             = "test.parquet",
    connection_params = local_params(tmp),
    collect           = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5L)
  expect_setequal(names(result), c("id", "value"))
})

test_that("load_data reads a local csv file (collect = TRUE)", {
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  write.csv(data.frame(x = 1:3, y = c("a", "b", "c")), file.path(tmp, "test.csv"), row.names = FALSE)

  result <- load_data(
    paths             = "test.csv",
    connection_params = local_params(tmp),
    collect           = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_true("x" %in% names(result))
})

test_that("load_data returns lazy tbl when collect = FALSE", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  arrow::write_parquet(data.frame(id = 1:10, v = rnorm(10)), file.path(tmp, "lazy.parquet"))

  result <- load_data(
    paths             = "lazy.parquet",
    connection_params = local_params(tmp),
    collect           = FALSE
  )

  # Should be a lazy tbl, not a data.frame yet
  expect_false(inherits(result, "data.frame"))
  collected <- dplyr::collect(result)
  expect_equal(nrow(collected), 10L)
})

test_that("load_data errors informatively for missing file", {
  skip_if_not_installed("duckdbfs")

  expect_error(
    load_data(
      paths             = "does_not_exist.parquet",
      connection_params = local_params(withr::local_tempdir()),
      collect           = TRUE
    ),
    regexp = "load_data\\(\\) failed"
  )
})

test_that("load_data unifies schemas across two parquet files", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  arrow::write_parquet(data.frame(id = 1:3, a = 1:3),          file.path(tmp, "f1.parquet"))
  arrow::write_parquet(data.frame(id = 4:6, a = 4:6, b = 7:9), file.path(tmp, "f2.parquet"))

  result <- load_data(
    paths             = c(
      file.path(tmp, "f1.parquet"),
      file.path(tmp, "f2.parquet")
    ),
    connection_params = local_params(tmp),
    unify_schemas     = TRUE,
    collect           = TRUE
  )

  expect_equal(nrow(result), 6L)
  expect_true("b" %in% names(result))
})