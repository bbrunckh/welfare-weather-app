# Bundled DuckDB extensions (R2-OPS-05): the binaries shipped for Posit Connect
# are tied to one DuckDB version and are checked before they are installed.

bundled_path <- function(ext) {
  system.file(paste0("duckdb_extensions/", ext, ".duckdb_extension.gz"),
    package = "wiseapp")
}

test_that("pinned checksums match the bundled extension binaries", {
  for (ext in names(.DUCKDB_BUNDLE_SHA256)) {
    path <- bundled_path(ext)
    expect_true(nzchar(path), info = ext)
    expect_identical(
      digest::digest(path, algo = "sha256", file = TRUE),
      unname(.DUCKDB_BUNDLE_SHA256[ext]),
      info = ext
    )
  }
})

test_that("every bundled extension binary is pinned and nothing else ships", {
  dir <- system.file("duckdb_extensions", package = "wiseapp")
  shipped <- sub("\\.duckdb_extension\\.gz$", "",
    list.files(dir, pattern = "\\.duckdb_extension\\.gz$"))
  expect_setequal(shipped, names(.DUCKDB_BUNDLE_SHA256))
  expect_false("spatial" %in% shipped)
})

test_that("DESCRIPTION pins duckdb to the bundled extension version", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "wiseapp"),
    fields = "Imports")[1, "Imports"]
  expect_match(
    gsub("\\s+", " ", desc),
    sprintf("duckdb \\(== %s\\)", gsub(".", "\\.", .DUCKDB_BUNDLE_VERSION, fixed = TRUE))
  )
})

test_that("bundled extension check accepts a matching binary", {
  skip_if_not(identical(as.character(utils::packageVersion("duckdb")), .DUCKDB_BUNDLE_VERSION))
  expect_no_error(.duck_check_bundled_ext("httpfs", bundled_path("httpfs")))
})

test_that("bundled extension check rejects a modified binary", {
  skip_if_not(identical(as.character(utils::packageVersion("duckdb")), .DUCKDB_BUNDLE_VERSION))
  tampered <- withr::local_tempfile(fileext = ".duckdb_extension.gz")
  file.copy(bundled_path("h3"), tampered)
  cat("x", file = tampered, append = TRUE)
  expect_error(.duck_check_bundled_ext("h3", tampered), "SHA-256")
})

test_that("bundled extension check rejects a different DuckDB version", {
  testthat::local_mocked_bindings(.DUCKDB_BUNDLE_VERSION = "0.0.1")
  expect_error(
    .duck_check_bundled_ext("httpfs", bundled_path("httpfs")),
    "built for duckdb 0.0.1"
  )
})
