library(testthat)

test_that("build_connection_params returns correct list for local", {
  p <- build_connection_params("local", path = "/data/foo")
  expect_equal(p$type, "local")
  expect_equal(p$path, "/data/foo")
})

test_that("automatic connection parameters use the source selector", {
  withr::local_envvar(
    WISEAPP_DATA_SOURCE = "LOCAL",
    WISEAPP_DATA_PATH = "/data/foo"
  )
  p <- auto_connection_params()
  expect_identical(p, list(type = "local", path = "/data/foo"))
})

test_that("remote automatic parameters use standard provider variables", {
  withr::local_envvar(
    WISEAPP_DATA_SOURCE = "s3",
    S3_BUCKET = "bucket",
    S3_PREFIX = "prefix/",
    S3_REGION = "eu-west-1",
    AWS_ACCESS_KEY_ID = "key",
    AWS_SECRET_ACCESS_KEY = "secret"
  )
  p <- auto_connection_params()
  expect_equal(p[c("type", "bucket", "prefix", "region", "key_id", "secret")],
               list(type = "s3", bucket = "bucket", prefix = "prefix/",
                    region = "eu-west-1", key_id = "key", secret = "secret"))
})

test_that("automatic connection parameters are NULL when selector is unset", {
  withr::local_envvar(WISEAPP_DATA_SOURCE = "")
  expect_null(auto_connection_params())
})

test_that("automatic connection parameters reject an unknown source", {
  withr::local_envvar(WISEAPP_DATA_SOURCE = "unsupported")
  expect_error(auto_connection_params(), "Unknown connection type")
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

# -----------------------------------------------------------------------------
# SEC-01: safe SQL literal quoting for secret credentials
# -----------------------------------------------------------------------------

test_that(".sql_literal preserves plain values and escapes apostrophes", {
  expect_identical(.sql_literal("plain-token"), "'plain-token'")
  expect_identical(.sql_literal(""), "''")
  expect_identical(.sql_literal("O'Brien"), "'O''Brien'")
  expect_identical(
    .sql_literal("'; CREATE SECRET pwn; --"),
    "'''; CREATE SECRET pwn; --'"
  )
})

test_that(".sql_literal round-trips adversarial values", {
  adversarial <- c(
    "plain", "", "O'Brien", "a'b'c", "it''s",
    "'; DROP SECRET s; --", "' at start", "end '"
  )
  for (v in adversarial) {
    lit <- .sql_literal(v)
    expect_true(startsWith(lit, "'") && endsWith(lit, "'"), info = v)
    inner <- substr(lit, 2, nchar(lit) - 1)
    expect_identical(gsub("''", "'", inner, fixed = TRUE), v, info = v)
  }
})

.duck_state_restore <- function() {
  backup <- as.list(.duck)
  function() {
    new_con <- .duck$con
    rm(list = ls(.duck, all.names = TRUE), envir = .duck)
    list2env(backup, envir = .duck)
    if (!is.null(new_con) && !identical(new_con, backup$con) &&
        inherits(new_con, "duckdb_connection")) {
      try(DBI::dbDisconnect(new_con, shutdown = TRUE), silent = TRUE)
    }
  }
}

test_that(".register_db_secret quotes bearer tokens safely in secret SQL", {
  restore_duck <- .duck_state_restore()
  withr::defer(restore_duck())
  captured <- character(0)
  local_mocked_bindings(
    dbExecute = function(con, statement, ...) {
      captured <<- c(captured, statement)
      0L
    },
    .package = "DBI"
  )

  # Quote-free token: byte-identical to the previous naive interpolation
  .register_db_secret(NULL, "tok-123", "h1")
  expect_identical(
    captured[1],
    "CREATE OR REPLACE SECRET db_http_h1 (TYPE http, BEARER_TOKEN 'tok-123');"
  )

  # Adversarial token: apostrophe doubled, no unescaped terminator
  .register_db_secret(NULL, "O'Brien'; CREATE SECRET pwn; --", "h1")
  expect_identical(
    captured[2],
    paste0(
      "CREATE OR REPLACE SECRET db_http_h1 ",
      "(TYPE http, BEARER_TOKEN 'O''Brien''; CREATE SECRET pwn; --');"
    )
  )
  expect_identical(.duck$db_secrets$h1,
                   digest::digest("O'Brien'; CREATE SECRET pwn; --"))
  expect_length(captured, 2)
})

test_that("shared DuckDB state is released after the last root session ends", {
  skip_if_not_installed("duckdb")
  restore_duck <- .duck_state_restore()
  withr::defer(restore_duck())

  callbacks <- list()
  make_session <- function() {
    session <- new.env(parent = emptyenv())
    session$userData <- new.env(parent = emptyenv())
    session$onSessionEnded <- function(callback) {
      callbacks[[length(callbacks) + 1L]] <<- callback
    }
    session
  }

  first <- make_session()
  second <- make_session()
  expect_true(.duck_register_session(first))
  expect_false(.duck_register_session(first))
  expect_true(.duck_register_session(second))

  con <- .duck_con()
  DBI::dbExecute(con, "CREATE TEMP TABLE sec03_probe (value INTEGER)")
  .duck$db_tokens <- list(token = list(token = "transient-token"))
  .duck$db_secrets <- list(secret = digest::digest("transient-token"))

  callbacks[[1L]]()
  expect_true(DBI::dbIsValid(con))
  expect_equal(.duck$active_sessions, 1L)

  callbacks[[2L]]()
  expect_false(DBI::dbIsValid(con))
  expect_null(.duck$con)
  expect_identical(.duck$extensions, character(0))
  expect_identical(.duck$db_tokens, list())
  expect_identical(.duck$db_secrets, list())

  callbacks[[2L]]()
  expect_equal(.duck$active_sessions, 0L)
})

test_that("load_data s3 secret SQL escapes quote-bearing credentials", {
  skip_if_not_installed("duckdb")
  restore_duck <- .duck_state_restore()
  withr::defer(restore_duck())
  captured <- character(0)
  local_mocked_bindings(
    dbExecute = function(con, statement, ...) {
      captured <<- c(captured, statement)
      0L
    },
    .package = "DBI"
  )
  secret_sql <- function() {
    captured[grepl("CREATE OR REPLACE SECRET s3_secret", captured, fixed = TRUE)]
  }

  # Quote-free credentials
  try(load_data(
    "file.parquet",
    list(type = "s3", bucket = "bkt", key_id = "AKIA-KEY",
         secret = "plain-secret", region = "us-east-1")
  ), silent = TRUE)
  s3 <- secret_sql()
  expect_length(s3, 1)
  expect_match(s3, "KEY_ID 'AKIA-KEY'", fixed = TRUE)
  expect_match(s3, "SECRET 'plain-secret'", fixed = TRUE)
  expect_match(s3, "REGION 'us-east-1'", fixed = TRUE)

  # Adversarial: quote-bearing secret cannot terminate the literal
  try(load_data(
    "file.parquet",
    list(type = "s3", bucket = "bkt", key_id = "AKIA-KEY",
         secret = "O'Brien'; CREATE SECRET pwn; --", region = "us-east-1")
  ), silent = TRUE)
  s3 <- secret_sql()
  expect_length(s3, 2)
  expect_match(s3[2], "SECRET 'O''Brien''; CREATE SECRET pwn; --'", fixed = TRUE)
  expect_false(grepl("SECRET 'O'Brien'", s3[2], fixed = TRUE))
})

test_that("load_data gcs and azure secret SQL escapes quote-bearing credentials", {
  skip_if_not_installed("duckdb")
  restore_duck <- .duck_state_restore()
  withr::defer(restore_duck())
  captured <- character(0)
  local_mocked_bindings(
    dbExecute = function(con, statement, ...) {
      captured <<- c(captured, statement)
      0L
    },
    .package = "DBI"
  )

  # GCS
  try(load_data(
    "file.parquet",
    list(type = "gcs", bucket = "bkt", key_id = "G'ID", secret = "G'SEC")
  ), silent = TRUE)
  gcs <- captured[grepl("CREATE OR REPLACE SECRET gcs_secret", captured, fixed = TRUE)]
  expect_length(gcs, 1)
  expect_match(gcs, "KEY_ID 'G''ID'", fixed = TRUE)
  expect_match(gcs, "SECRET 'G''SEC'", fixed = TRUE)

  # Azure: account key path
  try(load_data(
    "file.parquet",
    list(type = "azure", container = "cont", account = "acc", key = "AZ'KEY")
  ), silent = TRUE)
  az <- captured[grepl("CREATE OR REPLACE SECRET azure_secret", captured, fixed = TRUE)]
  expect_length(az, 1)
  expect_match(az, "CONNECTION_STRING 'AccountName=acc;AccountKey=AZ''KEY'", fixed = TRUE)

  # Azure: service principal path
  try(load_data(
    "file.parquet",
    list(type = "azure", container = "cont", account = "acc", key = "",
         tenant_id = "T'ID", client_id = "C'ID", client_secret = "CS'EC")
  ), silent = TRUE)
  az <- captured[grepl("CREATE OR REPLACE SECRET azure_secret", captured, fixed = TRUE)]
  expect_length(az, 2)
  expect_match(az[2], "TENANT_ID\\s+'T''ID'")
  expect_match(az[2], "CLIENT_ID\\s+'C''ID'")
  expect_match(az[2], "CLIENT_SECRET\\s+'CS''EC'")
})
