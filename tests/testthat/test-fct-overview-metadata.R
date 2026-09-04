# Overview metadata loader tests.

library(testthat)


.overview_duck_state_restore <- function() {
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


.reset_overview_metadata_cache <- function() {
  rm(
    list = ls(.overview_metadata_cache, all.names = TRUE),
    envir = .overview_metadata_cache
  )
}


.overview_fixture <- function(path) {
  metadata_path <- file.path(path, "metadata")
  dir.create(metadata_path, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    data.frame(
      economy = "Testland",
      code = "TST",
      year = 2020,
      survname = "Survey",
      level = "hh",
      obs = 10,
      source = "source",
      stringsAsFactors = FALSE
    ),
    file.path(metadata_path, "survey_list.csv")
  )
  readr::write_csv(
    data.frame(
      name = "loc_id",
      label = "Location",
      type = "numeric",
      units = NA_character_,
      id = 1,
      outcome = 0,
      hazard = 0,
      ind = 0,
      hh = 1,
      firm = 0,
      area = 0,
      interact = 0,
      fe = 0,
      stringsAsFactors = FALSE
    ),
    file.path(metadata_path, "variable_list.csv")
  )
  readr::write_csv(
    data.frame(
      code = "TST",
      year = 2020,
      data_level = "hh",
      cpi = 1,
      ppp2021 = 1,
      stringsAsFactors = FALSE
    ),
    file.path(metadata_path, "cpi_ppp.csv")
  )
}


.overview_databricks_csv <- function(
  survey_rows = c(
    "Zulu,ZUL,2020,Survey,hh,10,source",
    "Testland,TST,2020,Survey,hh,10,source"
  )
) {
  list(
    survey_list = paste(
      c("economy,code,year,survname,level,obs,source", survey_rows),
      collapse = "\n"
    ),
    variable_list = paste(
      "name,label,type,units,id,outcome,hazard,ind,hh,firm,area,interact,fe",
      "loc_id,Location,numeric,,1,0,0,0,1,0,0,0,0",
      sep = "\n"
    ),
    cpi_ppp = paste(
      "code,year,data_level,cpi,ppp2021",
      "TST,2020,hh,1,1",
      sep = "\n"
    )
  )
}


.overview_databricks_params <- function(
  workspace = "https://workspace.example",
  client_secret = "secret"
) {
  list(
    type = "databricks",
    workspace = workspace,
    client_id = "client",
    client_secret = client_secret,
    volume_path = "/Volumes/catalog/schema/data"
  )
}


.overview_databricks_mock <- function(csv, token, on_request = NULL) {
  force(csv)
  force(token)
  force(on_request)

  function(req) {
    if (!is.null(on_request)) on_request(req)
    if (grepl("/oidc/v1/token$", req$url)) {
      return(httr2::response_json(
        body = list(access_token = token, expires_in = 3600),
        url = req$url
      ))
    }
    name <- names(csv)[vapply(
      names(csv), function(x) grepl(paste0("/", x, "\\.csv$"), req$url),
      logical(1)
    )]
    if (length(name) != 1L) {
      return(NULL)
    }
    httr2::response(body = charToRaw(csv[[name]]), url = req$url)
  }
}


test_that("shared data path resolution preserves connection contracts", {
  expect_equal(
    .resolve_data_path("file.csv", list(type = "local", path = "/data")),
    "/data/file.csv"
  )
  expect_equal(
    .resolve_data_path(
      "file.csv",
      list(type = "s3", bucket = "bucket", prefix = "prefix/")
    ),
    "s3://bucket/prefix/file.csv"
  )
  expect_equal(
    .resolve_data_path(
      "file.csv",
      list(type = "gcs", bucket = "bucket", prefix = "prefix/")
    ),
    "gs://bucket/prefix/file.csv"
  )
  expect_equal(
    .resolve_data_path(
      "file.csv",
      list(
        type = "azure", account = "account", container = "container",
        prefix = "prefix/"
      )
    ),
    "abfss://container@account.dfs.core.windows.net/prefix/file.csv"
  )
  expect_equal(
    .resolve_data_path(
      "file.csv", list(type = "hf", repo = "user/repo", subdir = "data/")
    ),
    "hf://datasets/user/repo/data/file.csv"
  )
  expect_equal(
    .resolve_data_path(
      "file.csv",
      list(
        type = "databricks", workspace = "https://workspace.example",
        volume_path = "/Volumes/catalog/schema/data"
      )
    ),
    paste0(
      "https://workspace.example/api/2.0/fs/files",
      "/Volumes/catalog/schema/data/file.csv"
    )
  )
  expect_identical(
    .resolve_data_path("https://example.com/file.csv", list(type = "local")),
    "https://example.com/file.csv"
  )
})


test_that("local Overview metadata loads as one validated bundle", {
  path <- tempfile("wiseapp-overview-")
  dir.create(path, recursive = TRUE)
  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "1")
  .overview_fixture(path)

  metadata <- load_overview_metadata(list(type = "local", path = path))

  expect_named(
    metadata,
    c("survey_list", "variable_list", "cpi_ppp", "pov_lines")
  )
  expect_equal(metadata$survey_list$code, "TST")
  expect_true("loc_id_panel" %in% metadata$variable_list$name)
  expect_false("loc_id" %in% metadata$variable_list$name)
  expect_equal(metadata$cpi_ppp$ppp2021, 1)
  expect_equal(metadata$pov_lines$ln, c(3, 4.2, 8.3))
})


test_that("Overview metadata validation reports missing columns", {
  metadata <- list(
    survey_list = data.frame(code = "TST"),
    variable_list = data.frame(name = "x"),
    cpi_ppp = data.frame(code = "TST"),
    pov_lines = data.frame(ppp_year = 2021, ln = 3)
  )

  expect_error(
    .validate_overview_metadata(metadata),
    "metadata/survey_list.csv: missing columns"
  )
  expect_error(
    .validate_overview_metadata(metadata),
    "metadata/variable_list.csv: missing columns"
  )
})


test_that("Databricks metadata requests preserve the bundle contract", {
  restore_duck <- .overview_duck_state_restore()
  withr::defer(restore_duck())
  withr::local_envvar(c(
    WISEAPP_METADATA_LOAD_PARALLEL = "1",
    WISEAPP_METADATA_CACHE_DISABLE = "1"
  ))

  withr::local_options(
    httr2_mock = .overview_databricks_mock(
      .overview_databricks_csv(), "test-token"
    )
  )
  params <- .overview_databricks_params()

  withr::local_envvar(WISEAPP_METADATA_LOAD_PARALLEL = "0")
  sequential <- load_overview_metadata(params)
  withr::local_envvar(WISEAPP_METADATA_LOAD_PARALLEL = "1")
  metadata <- load_overview_metadata(params)

  expect_equal(metadata$survey_list$code, c("TST", "ZUL"))
  expect_true("loc_id_panel" %in% metadata$variable_list$name)
  expect_equal(metadata$cpi_ppp$ppp2021, 1)
  expect_equal(metadata$pov_lines$ppp_year, rep(2021, 3))
  expect_equal(metadata, sequential)
})


test_that("Databricks metadata does not initialize DuckDB", {
  restore_duck <- .overview_duck_state_restore()
  withr::defer(restore_duck())
  rm(list = ls(.duck, all.names = TRUE), envir = .duck)
  withr::defer(.reset_overview_metadata_cache())
  withr::local_envvar(c(
    WISEAPP_METADATA_LOAD_PARALLEL = "1",
    WISEAPP_METADATA_CACHE_DISABLE = "1"
  ))

  withr::local_options(
    httr2_mock = .overview_databricks_mock(
      .overview_databricks_csv(c("Testland,TST,2020,Survey,hh,10,source")),
      "no-duck-token"
    )
  )

  load_overview_metadata(.overview_databricks_params(
    workspace = "https://no-duck-workspace.example"
  ))

  expect_false(exists("con", envir = .duck, inherits = FALSE))
})


test_that("Databricks token cache survives later DuckDB initialization", {
  restore_duck <- .overview_duck_state_restore()
  withr::defer(restore_duck())
  rm(list = ls(.duck, all.names = TRUE), envir = .duck)
  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "1")

  token_requests <- 0L
  mock <- function(req) {
    if (grepl("/oidc/v1/token$", req$url)) {
      token_requests <<- token_requests + 1L
      return(httr2::response_json(
        body = list(access_token = "persistent-token", expires_in = 3600),
        url = req$url
      ))
    }
    httr2::response(
      body = charToRaw("economy,code,year,survname,level,obs,source\nTestland,TST,2020,Survey,hh,10,source"),
      url = req$url
    )
  }
  withr::local_options(httr2_mock = mock)

  host <- "https://persistent-token-workspace.example"
  .get_db_token(host, "client", "secret")
  .duck_con()
  .get_db_token(host, "client", "secret")

  expect_equal(token_requests, 1L)
})


test_that("successful metadata loads are cached and local changes invalidate them", {
  path <- tempfile("wiseapp-overview-cache-")
  dir.create(path, recursive = TRUE)
  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "0")
  .reset_overview_metadata_cache()
  withr::defer(.reset_overview_metadata_cache())
  .overview_fixture(path)

  first <- load_overview_metadata(list(type = "local", path = path))
  expect_equal(first$survey_list$code, "TST")

  readr::write_csv(
    rbind(
      data.frame(
        economy = "Testland",
        code = "TST",
        year = 2020,
        survname = "Survey",
        level = "hh",
        obs = 10,
        source = "source",
        stringsAsFactors = FALSE
      ),
      data.frame(
        economy = "Zululand",
        code = "ZUL",
        year = 2021,
        survname = "Survey 2",
        level = "hh",
        obs = 20,
        source = "source",
        stringsAsFactors = FALSE
      )
    ),
    file.path(path, "metadata", "survey_list.csv")
  )

  second <- load_overview_metadata(list(type = "local", path = path))
  expect_equal(second$survey_list$code, c("TST", "ZUL"))
})


test_that("remote metadata cache avoids repeat requests and can be disabled", {
  restore_duck <- .overview_duck_state_restore()
  withr::defer(restore_duck())
  rm(list = ls(.duck, all.names = TRUE), envir = .duck)
  .reset_overview_metadata_cache()
  withr::defer(.reset_overview_metadata_cache())
  withr::local_envvar(c(
    WISEAPP_METADATA_LOAD_PARALLEL = "1",
    WISEAPP_METADATA_CACHE_DISABLE = "0"
  ))

  requests <- 0L
  withr::local_options(
    httr2_mock = .overview_databricks_mock(
      .overview_databricks_csv(c("Testland,TST,2020,Survey,hh,10,source")),
      "cache-token",
      function(req) requests <<- requests + 1L
    )
  )

  params <- .overview_databricks_params(
    workspace = "https://cache-workspace.example"
  )
  load_overview_metadata(params)
  expect_equal(requests, 4L)

  load_overview_metadata(params)
  expect_equal(requests, 4L)

  load_overview_metadata(params, force_refresh = TRUE)
  expect_equal(requests, 7L)

  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "1")
  load_overview_metadata(params)
  expect_equal(requests, 10L)

  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "0")
  .reset_overview_metadata_cache()
  load_overview_metadata(params)
  key <- .overview_metadata_cache_key(params)
  .overview_metadata_cache[[key]]$created_at <- as.numeric(Sys.time()) - 901
  load_overview_metadata(params)
  expect_equal(requests, 16L)

  .reset_overview_metadata_cache()
  load_overview_metadata(params)
  different_credentials <- .overview_databricks_params(
    workspace = "https://cache-workspace.example",
    client_secret = "different-secret"
  )
  load_overview_metadata(different_credentials)
  expect_equal(requests, 23L)
})


test_that("parallel flag uses the sequential Databricks request path when disabled", {
  withr::local_envvar(WISEAPP_METADATA_LOAD_PARALLEL = "0")
  expect_false(.overview_metadata_parallel_enabled())

  withr::local_envvar(WISEAPP_METADATA_LOAD_PARALLEL = "invalid")
  expect_warning(
    expect_true(.overview_metadata_parallel_enabled()),
    "invalid value"
  )
})


test_that("metadata cache settings validate environment overrides", {
  withr::local_envvar(WISEAPP_METADATA_CACHE_DISABLE = "invalid")
  expect_warning(
    expect_false(.overview_metadata_cache_disabled()),
    "invalid value"
  )

  withr::local_envvar(WISEAPP_METADATA_CACHE_MAX_AGE = "invalid")
  expect_warning(
    expect_equal(.overview_metadata_cache_max_age(), 900),
    "invalid value"
  )
})
