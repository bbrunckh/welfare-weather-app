# ============================================================================ #
# tests/testthat/test-export-bundle.R                                          #
# UI-48: an analysis must be exportable as a self-describing bundle -          #
#        configuration, tables, figures, manifest and README - and importable  #
#        again.                                                                #
# ============================================================================ #

library(testthat)
library(shiny)

item <- function(key, kind = "table", step = 1L, fun = NULL, label = key,
                 description = "Description.") {
  list(key = key, label = label, step = as.integer(step), kind = kind,
       fun = fun %||% function() data.frame(a = 1:2, b = c("x", "y")),
       description = description, width = 6, height = 4)
}

# ---- Registry ---------------------------------------------------------------

test_that("modules register artefacts through the shared session userData", {
  inner <- function(id) moduleServer(id, function(input, output, session) {
    wise_export_table("inner_table", "Inner", 1L,
                      function() data.frame(x = 1), session = session)
  })
  outer <- function(input, output, session) {
    inner("child")
    wise_export_figure("root_fig", "Root", 2L, function() NULL,
                       session = session)
  }
  testServer(outer, {
    items <- wise_export_items(session)
    # A module registered without any change to its server signature.
    expect_setequal(names(items), c("inner_table", "root_fig"))
  })
})

test_that("re-registering a key replaces rather than duplicates", {
  testServer(function(input, output, session) {
    for (i in 1:3) {
      wise_export_table("t", paste("Version", i), 1L,
                        function() data.frame(x = i), session = session)
    }
  }, {
    items <- wise_export_items(session)
    expect_length(items, 1L)
    expect_equal(items[["t"]]$label, "Version 3")
  })
})

test_that("registry ordering is stable: step, then tables before figures", {
  testServer(function(input, output, session) {
    wise_export_figure("z_fig", "Z", 1L, function() NULL, session = session)
    wise_export_table("b_tbl", "B", 2L, function() NULL, session = session)
    wise_export_table("a_tbl", "A", 1L, function() NULL, session = session)
  }, {
    expect_equal(names(wise_export_items(session)),
                 c("a_tbl", "z_fig", "b_tbl"))
  })
})

test_that("registering outside a session is a no-op rather than an error", {
  expect_silent(wise_export_table("x", "X", 1L, function() NULL,
                                  session = NULL))
  expect_length(wise_export_items(NULL), 0L)
})


# ---- File naming ------------------------------------------------------------

test_that("file names follow the documented NN_stepN_slug.ext scheme", {
  expect_equal(.export_filename(1, 1, "survey_summary", "table"),
               "01_step1_survey-summary.csv")
  expect_equal(.export_filename(12, 3, "policy_effect", "figure"),
               "12_step3_policy-effect.png")
  # Sorting by name reproduces bundle order past nine files.
  nm <- vapply(1:11, function(i) .export_filename(i, 1, "x", "table"),
               character(1))
  expect_equal(nm, sort(nm))
})

test_that("slugs are filesystem- and archive-safe", {
  expect_equal(.export_slug("Survey Summary (HH)"), "survey-summary-hh")
  expect_equal(.export_slug("a//b__c"), "a-b-c")
  expect_equal(.export_slug("---"), "item")
  expect_false(grepl("[^a-z0-9-]", .export_slug("Ünïcödé / 50%")))
})


# ---- Bundle assembly --------------------------------------------------------

test_that("a bundle contains the artefacts, manifest and README", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("survey_summary"), item("coefficients"))

  wise_export_bundle(zf, items, config = list(app_version = "0.1.0"),
                     provenance = list())

  files <- utils::unzip(zf, list = TRUE)$Name
  expect_true(all(c("manifest.csv", "README.md", "configuration.json") %in% files))
  expect_true("01_step1_survey-summary.csv" %in% files)
  expect_true("02_step1_coefficients.csv" %in% files)
})

test_that("surfaces that produced nothing are skipped, numbering stays contiguous", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(
    item("first"),
    item("never_run", fun = function() NULL),          # step not run
    item("empty", fun = function() data.frame()),      # ran, no rows
    item("second")
  )
  mf <- wise_export_bundle(zf, items, config = NULL)

  expect_equal(nrow(mf), 2L)
  expect_equal(mf$file, c("01_step1_first.csv", "02_step1_second.csv"))
})

test_that("an artefact that errors is skipped with a warning, not fatal", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("ok"), item("boom", fun = function() stop("kaboom")))

  expect_warning(mf <- wise_export_bundle(zf, items, config = NULL), "kaboom")
  expect_equal(mf$file, "01_step1_ok.csv")
  expect_true(file.exists(zf))
})

test_that("include= selects which parts are written", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  skip_if_not_installed("ggplot2")
  fig <- item("plot", kind = "figure",
              fun = function() ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
                ggplot2::geom_point())
  items <- list(item("tbl"), fig)

  zf1 <- withr::local_tempfile(fileext = ".zip")
  wise_export_bundle(zf1, items, config = list(), include = "tables")
  f1 <- utils::unzip(zf1, list = TRUE)$Name
  expect_true(any(grepl("\\.csv$", setdiff(f1, "manifest.csv"))))
  expect_false(any(grepl("\\.png$", f1)))

  zf2 <- withr::local_tempfile(fileext = ".zip")
  wise_export_bundle(zf2, items, config = NULL, include = "figures")
  f2 <- utils::unzip(zf2, list = TRUE)$Name
  expect_true(any(grepl("\\.png$", f2)))
  expect_false("configuration.json" %in% f2)
})


# ---- README -----------------------------------------------------------------

test_that("the README documents the naming scheme and every file", {
  entries <- list(
    list(file = "01_step1_a.csv", kind = "table",
         step_label = "Step 1 - Model welfare", label = "A",
         description = "First table.", rows = 10, cols = 3)
  )
  md <- paste(wise_export_readme(entries, list(), list()), collapse = "\n")

  expect_match(md, "NN_stepN_artefact-name.ext", fixed = TRUE)
  expect_match(md, "01_step1_a.csv", fixed = TRUE)
  expect_match(md, "First table.", fixed = TRUE)
  expect_match(md, "manifest.csv", fixed = TRUE)
  # The naming scheme is explained field by field.
  for (part in c("`NN`", "`stepN`", "`artefact-name`", "`.ext`")) {
    expect_match(md, part, fixed = TRUE)
  }
})

test_that("the README states provenance and never leaks credentials", {
  prov <- list(step1 = wise_provenance(
    1L,
    result = list(.sig = list(step = "fit"), engine = "fixest",
                  .snap = list(outcome = data.frame(label = "Welfare"),
                               weather = data.frame(label = "Max temp"),
                               survey_weather = data.frame(a = 1:42))),
    connection_params = list(type = "databricks",
                             workspace = "https://adb-1.example.net",
                             client_secret = "TOP-SECRET-VALUE",
                             volume_path = "/Volumes/cat/sch/vol")
  ))
  md <- paste(wise_export_readme(list(), prov, list()), collapse = "\n")

  expect_match(md, "databricks", fixed = TRUE)
  expect_match(md, "https://adb-1.example.net", fixed = TRUE)
  expect_match(md, "Welfare", fixed = TRUE)
  expect_match(md, "Run signature", fixed = TRUE)
  # The identity of the source is recorded; the means of reading it is not.
  expect_false(grepl("TOP-SECRET-VALUE", md, fixed = TRUE))
  expect_match(md, "redacted", fixed = TRUE)
})

test_that("the README is honest when nothing has been run", {
  md <- paste(wise_export_readme(list(), list(), list()), collapse = "\n")
  expect_match(md, "No tables or figures were exported", fixed = TRUE)
  expect_match(md, "no run to describe", fixed = TRUE)
})


# ---- Configuration ----------------------------------------------------------

test_that("the config snapshot captures every namespaced input in one pass", {
  testServer(function(input, output, session) {
    session$userData$snap <- NULL
  }, {
    session$setInputs(
      `step1-model-model_type`   = "Linear regression",
      `step1-weather-var_select` = c("tx", "r"),
      `step3-sp-transfer_amount_usd` = 50
    )
    cfg <- wise_config_snapshot(input, seed = 99L)
    expect_equal(cfg$random_seed, 99L)
    expect_equal(cfg$inputs$`step1-model-model_type`, "Linear regression")
    expect_equal(cfg$inputs$`step1-weather-var_select`, c("tx", "r"))
    expect_equal(cfg$inputs$`step3-sp-transfer_amount_usd`, 50)
  })
})

test_that("transient UI state is excluded from the exported configuration", {
  # Run counters would re-fire models on import; panel toggles and DT state
  # describe the browser, not the analysis.
  drop <- c("step1-model-run_model", "step2-sim-run_sim",
            "step1-model-model_settings_toggle", "step1-model-show_lasso_force",
            "stats_rows_current", "tbl_search", "map_zoom",
            "step3-run_policy_sim")
  keep <- c("step1-model-model_type", "step3-sp-targeting",
            "step1-model-fixedeffects")
  expect_false(any(.export_keep_input(drop)))
  expect_true(all(.export_keep_input(keep)))
})

test_that("a config round-trips through JSON", {
  cfg <- list(wiseapp_config_version = 1L, random_seed = 123L,
              inputs = list(a = "x", b = c(1, 2, 3), c = TRUE))
  f <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(cfg, f, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  back <- jsonlite::read_json(f, simplifyVector = TRUE)

  expect_equal(back$random_seed, 123L)
  expect_equal(back$inputs$a, "x")
  expect_equal(back$inputs$b, c(1, 2, 3))
  expect_true(back$inputs$c)
})

test_that("applying a config sends values and reports what could not be placed", {
  sent <- list()
  fake <- list(sendInputMessage = function(id, msg) {
    sent[[id]] <<- msg$value
    invisible(NULL)
  })
  cfg <- list(inputs = list(present = "yes", absent = "no"))

  res <- wise_config_apply(cfg, fake, existing = "present")
  expect_equal(res$applied, "present")
  # Controls inside renderUI() do not exist yet; they are reported, not lost.
  expect_equal(res$pending, "absent")
  expect_equal(sent$present, "yes")
  expect_null(sent$absent)
})

test_that("applying an empty config is a no-op", {
  res <- wise_config_apply(list(inputs = list()), list(), character(0))
  expect_length(res$applied, 0L)
  expect_length(res$pending, 0L)
})


test_that("the archive writer falls back when no system zip is present", {
  skip_if_not_installed("zip")
  d <- withr::local_tempdir()
  writeLines("a,b\n1,2", file.path(d, "x.csv"))
  zf <- withr::local_tempfile(fileext = ".zip")

  # Force the fallback path by hiding the system binary.
  withr::local_envvar(PATH = "")
  .export_zip(zf, d, "x.csv")

  expect_true(file.exists(zf))
  expect_equal(utils::unzip(zf, list = TRUE)$Name, "x.csv")
})

test_that("the archive writer says what to do when it cannot write", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "x.csv"))
  withr::local_envvar(PATH = "")
  local_mocked_bindings(
    requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    .export_zip(withr::local_tempfile(fileext = ".zip"), d, "x.csv"),
    "Configuration only"
  )
})


# ---- Writer robustness -------------------------------------------------------
#
# A table that could not be written used to propagate out of the download
# handler, so Shiny answered with its HTML error page - which the browser
# saved under the .zip name. One bad table must cost that table, not the
# bundle.

test_that("list columns are flattened instead of aborting write.csv", {
  df <- data.frame(name = c("tx", "r"), label = c("Max temp", "Precip"),
                   stringsAsFactors = FALSE)
  df$polynomial   <- list(c(1, 2), NULL)
  df$customBreaks <- list(NULL, c(0, 10, 20))

  # This is the exact failure the app hit: "unimplemented type 'list'".
  expect_error(utils::write.csv(df, withr::local_tempfile()), "unimplemented type")

  flat <- .export_flatten_df(df)
  expect_false(any(vapply(flat, is.list, logical(1))))
  expect_equal(flat$polynomial, c("1; 2", NA_character_))
  expect_equal(flat$customBreaks, c(NA_character_, "0; 10; 20"))
  expect_silent(utils::write.csv(flat, withr::local_tempfile(), row.names = FALSE))
})

test_that("flattening leaves ordinary columns untouched", {
  df <- data.frame(n = 1:3, x = c(1.5, 2.5, 3.5), s = letters[1:3],
                   f = factor(c("a", "b", "a")),
                   d = as.Date("2026-01-01") + 0:2,
                   stringsAsFactors = FALSE)
  flat <- .export_flatten_df(df)
  expect_equal(flat$n, df$n)
  expect_equal(flat$x, df$x)
  expect_equal(flat$s, df$s)
  expect_equal(flat$d, df$d)
  expect_s3_class(flat$f, "factor")
})

test_that("flattening handles matrix columns and empty frames", {
  df <- data.frame(a = 1:2)
  df$m <- matrix(1:4, nrow = 2)
  expect_equal(.export_flatten_df(df)$m, c("1; 3", "2; 4"))
  expect_equal(ncol(.export_flatten_df(data.frame())), 0L)
})

test_that("a table that cannot be written is skipped, not fatal", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  hostile <- item("hostile", fun = function() {
    d <- data.frame(x = 1); d$fn <- list(mean); d
  })
  zf <- withr::local_tempfile(fileext = ".zip")
  # Even a column of closures cannot take the bundle down.
  expect_silent(mf <- wise_export_bundle(zf, list(item("ok"), hostile),
                                          config = NULL))
  expect_true(file.exists(zf))
  expect_equal(nrow(mf), 2L)
})

test_that("a figure whose builder throws is skipped and named in the README", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  skip_if_not_installed("ggplot2")
  items <- list(
    item("good_fig", kind = "figure",
         fun = function() ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
           ggplot2::geom_point()),
    item("bad_fig", kind = "figure", label = "Broken figure",
         fun = function() stop("plot builder failed"))
  )
  zf <- withr::local_tempfile(fileext = ".zip")
  expect_warning(mf <- wise_export_bundle(zf, items, config = NULL),
                 "plot builder failed")

  expect_equal(nrow(mf), 1L)
  d <- withr::local_tempdir()
  utils::unzip(zf, exdir = d)
  md <- paste(readLines(file.path(d, "README.md")), collapse = "\n")
  # Named rather than silently missing.
  expect_match(md, "## Not exported", fixed = TRUE)
  expect_match(md, "Broken figure", fixed = TRUE)
  expect_match(md, "plot builder failed", fixed = TRUE)
})

test_that("a real list-column table round-trips through a bundle", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  sw <- data.frame(name = "tx", label = "Max temp", stringsAsFactors = FALSE)
  sw$polynomial <- list(c(1, 2))
  zf <- withr::local_tempfile(fileext = ".zip")
  wise_export_bundle(zf, list(item("weather_specification", fun = function() sw)),
                     config = NULL)

  d <- withr::local_tempdir()
  utils::unzip(zf, exdir = d)
  csv <- list.files(d, pattern = "weather-specification.*csv$", full.names = TRUE)
  expect_length(csv, 1L)
  back <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(back$polynomial, "1; 2")
})
