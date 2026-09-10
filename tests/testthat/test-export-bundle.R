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

test_that("surfaces that produced nothing are skipped; numbering keeps their slots", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(
    item("first"),
    item("never_run", fun = function() NULL),          # step not run
    item("empty", fun = function() data.frame()),      # ran, no rows
    item("second")
  )
  mf <- wise_export_bundle(zf, items, config = NULL)

  # Numbers follow the registry's fixed order: an absent surface leaves a
  # gap instead of shifting every later file (UI-57).
  expect_equal(nrow(mf), 2L)
  expect_equal(mf$file, c("01_step1_first.csv", "04_step1_second.csv"))
})

test_that("file names are stable across bundles whose steps differ (UI-57)", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  items_all_ok <- list(item("first"), item("ok"), item("ok2"), item("second"))
  items_partial <- list(
    item("first"),
    item("never_run", fun = function() NULL),   # not run in this session
    item("boom", fun = function() stop("kaboom")),
    item("second")
  )
  mf1 <- wise_export_bundle(withr::local_tempfile(fileext = ".zip"),
                            items_all_ok, config = NULL)
  suppressWarnings(
    mf2 <- wise_export_bundle(withr::local_tempfile(fileext = ".zip"),
                              items_partial, config = NULL)
  )
  # The same key keeps the same number whether or not its neighbours produced
  # anything, so two bundles of one analysis diff file by file.
  expect_equal(mf2$file[mf2$file == "04_step1_second.csv"],
               mf1$file[mf1$file == "04_step1_second.csv"])
})

test_that("bundle assembly does not consume the session RNG (UI-58)", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("first"), item("never_run", fun = function() NULL))
  before <- .Random.seed
  wise_export_bundle(zf, items, config = NULL)
  expect_identical(.Random.seed, before)
})

test_that("an artefact that errors is skipped with a warning, not fatal", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("ok"), item("boom", fun = function() stop("kaboom")))

  expect_warning(mf <- wise_export_bundle(zf, items, config = NULL), "kaboom")
  expect_equal(mf$file, "01_step1_ok.csv")
  # The skip rides the manifest as an attribute so callers can report counts.
  expect_length(attr(mf, "skipped"), 1L)
  expect_match(attr(mf, "skipped")[[1]]$note, "kaboom")
  expect_true(file.exists(zf))
})

test_that("the bundle reports per-item progress (UI-59)", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  seen <- list(); n <- 0L
  wise_export_bundle(
    withr::local_tempfile(fileext = ".zip"),
    list(item("a"), item("b"), item("never", fun = function() NULL)),
    config = NULL,
    progress = function(i, total, label) {
      n <<- n + 1L
      seen[[n]] <<- list(i = i, total = total, label = label)
    }
  )
  # The callback fires per registered surface, ready or not, so the progress
  # bar reaches 100% even when a surface contributes nothing.
  expect_length(seen, 3L)
  expect_equal(seen[[1]]$i, 1L)
  expect_equal(seen[[1]]$total, 3L)
  expect_equal(seen[[1]]$label, "a")
  expect_equal(seen[[3]]$i, 3L)
  expect_equal(seen[[3]]$label, "never")
})

test_that("a req() throw means not-ready: skipped quietly, not named (UI-53)", {
  res <- .export_write_item(item("pending", fun = function() shiny::req(NULL)),
                            tempdir(), "x.csv")
  expect_null(res)
})

test_that("a genuine builder failure is named, not silently dropped (UI-53)", {
  res <- .export_write_item(item("boom", fun = function() stop("grid mismatch")),
                            tempdir(), "x.csv")
  expect_equal(res$status, "error")
  expect_match(res$note, "grid mismatch")
})

test_that("a req() throw inside a registered artefact leaves no failure note", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("ok"),
                item("pending", kind = "figure",
                     fun = function() shiny::req(FALSE)))

  expect_silent(mf <- wise_export_bundle(zf, items, config = NULL))
  expect_equal(mf$file, "01_step1_ok.csv")

  d <- withr::local_tempdir()
  utils::unzip(zf, exdir = d)
  md <- paste(readLines(file.path(d, "README.md")), collapse = "\n")
  # Not-run is not a failure: no "Not exported" section at all.
  expect_no_match(md, "## Not exported", fixed = TRUE)
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

test_that("a stale run is flagged in the README provenance block (UI-54)", {
  mk <- function(stale) list(step1 = wise_provenance(
    1L, result = list(.sig = list(step = "fit")),
    connection_params = list(type = "local"), extra = list(stale = stale)))
  md <- paste(wise_export_readme(list(), mk(TRUE), list()), collapse = "\n")
  expect_match(md, "Results stale", fixed = TRUE)
  expect_match(md, "current inputs have changed", fixed = TRUE)
  # A fresh run records nothing about staleness.
  expect_no_match(paste(wise_export_readme(list(), mk(FALSE), list()),
                        collapse = "\n"), "Results stale", fixed = TRUE)
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
  # describe the browser, not the analysis. apply_connection is a replayable
  # click too: importing it would re-fire the Connect attempt (SEC-06).
  drop <- c("step1-model-run_model", "step2-sim-run_sim",
            "step1-model-model_settings_toggle", "step1-model-show_lasso_force",
            "stats_rows_current", "tbl_search", "map_zoom",
            "step3-run_policy_sim", "overview-apply_connection",
            "import_config_file")
  # Result-shaping toggles travel: the exported figures read them (UI-56).
  keep <- c("step1-model-model_type", "step3-sp-targeting",
            "step1-model-fixedeffects",
            "step2-cmp-show_model_spread", "step2-cmp-show_coef_uncertainty",
            "step2-cmp-show_return_period", "step2-diag-show_regression_input")
  expect_false(any(.export_keep_input(drop)))
  expect_true(all(.export_keep_input(keep)))
})

test_that("credential-shaped inputs are never exported (SEC-06)", {
  # Every secret/key field the Overview connection form asks for. The bundle
  # is meant to be shared, so none of these may ride the snapshot - and the
  # README's "never its credentials" claim must hold.
  secretish <- c(
    "overview-s3_key_id", "overview-s3_secret",
    "overview-gcs_key_id", "overview-gcs_secret",
    "overview-azure_key", "overview-azure_client_id",
    "overview-azure_client_secret", "overview-azure_tenant_id",
    "overview-db_client_id", "overview-db_client_secret"
  )
  expect_false(any(.export_keep_input(secretish)))
  # Source identity (never the means of reading it) survives.
  expect_true(all(.export_keep_input(c(
    "overview-connection_type", "overview-db_workspace",
    "overview-db_volume_path", "overview-s3_bucket", "overview-s3_prefix",
    "overview-s3_region", "overview-gcs_bucket", "overview-hf_repo",
    "overview-hf_subdir", "overview-local_path"))))
})

test_that("a snapshot of a filled connection form carries no secrets", {
  testServer(function(input, output, session) {
    session$userData$snap <- NULL
  }, {
    session$setInputs(
      `overview-connection_type` = "s3",
      `overview-s3_bucket`       = "my-bucket",
      `overview-s3_key_id`       = "AKIAEXAMPLE",
      `overview-s3_secret`       = "TOP-SECRET-VALUE"
    )
    cfg <- wise_config_snapshot(input, seed = 99L)
    js  <- jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null", digits = NA)
    expect_false(grepl("TOP-SECRET-VALUE", js, fixed = TRUE))
    expect_false(grepl("AKIAEXAMPLE", js, fixed = TRUE))
    expect_equal(cfg$inputs$`overview-s3_bucket`, "my-bucket")
    expect_false("overview-s3_secret" %in% names(cfg$inputs))
  })
})

test_that("importing an exported snapshot never pushes credentials or counters", {
  testServer(function(input, output, session) {}, {
    session$setInputs(
      `overview-s3_bucket` = "b", `overview-s3_key_id` = "AKIAEXAMPLE",
      `overview-s3_secret` = "TOP-SECRET-VALUE",
      `overview-apply_connection` = 3L
    )
    f <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(wise_config_snapshot(input, seed = 1L), f,
                         auto_unbox = TRUE, pretty = TRUE, digits = NA)
    back <- jsonlite::read_json(f, simplifyVector = TRUE)

    sent <- list()
    fake <- list(sendInputMessage = function(id, msg) {
      sent[[id]] <<- msg$value
      invisible(NULL)
    })
    res <- wise_config_apply(back, fake, existing = c(
      "overview-s3_bucket", "overview-s3_key_id", "overview-s3_secret",
      "overview-apply_connection"))
    expect_equal(res$applied, "overview-s3_bucket")
    expect_equal(sent$`overview-s3_bucket`, "b")
    expect_false("overview-apply_connection" %in% names(sent))
    expect_false("overview-s3_key_id" %in% names(sent))
  })
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


# ---- Import validation & deferred retry (UI-52) ------------------------------

test_that("the import validator rejects non-WISE-APP and newer-format files", {
  av <- "1.2.3"
  expect_false(.import_validate(list(inputs = list()), app_version = av)$ok)
  expect_false(.import_validate(list(app_version = av), app_version = av)$ok)
  expect_false(.import_validate(
    list(wiseapp_config_version = 2L, inputs = list(a = 1)),
    app_version = av)$ok)
  ok <- .import_validate(list(wiseapp_config_version = 1L, app_version = av,
                              inputs = list(a = 1)), app_version = av)
  expect_true(ok$ok)
  expect_length(ok$notes, 0L)
  # A different app version warns but does not reject.
  warn <- .import_validate(list(wiseapp_config_version = 1L,
                                app_version = "9.9.9", inputs = list(a = 1)),
                           app_version = av)
  expect_true(warn$ok)
  expect_match(warn$notes[1], "9.9.9")
  # An unknown session version cannot claim a mismatch.
  expect_length(
    .import_validate(list(wiseapp_config_version = 1L, app_version = "9.9.9",
                          inputs = list(a = 1)),
                     app_version = NA_character_)$notes, 0L)
})

test_that("already-in-force values are not counted as changes", {
  expect_true(.import_value_same(50, 50))
  expect_true(.import_value_same(50L, 50))   # JSON round-trip: int -> double
  expect_true(.import_value_same(c("tx", "r"), c("tx", "r")))
  expect_false(.import_value_same(50, 51))
  expect_false(.import_value_same("a", "b"))
  expect_false(.import_value_same(NULL, 1))
  expect_true(.import_value_same(NULL, NULL))
})

# The deferred-retry mechanism end to end. The proxy session records what the
# retry pushes without replacing the test session itself.
.import_e2e_server <- function(sent) {
  function(input, output, session) {
    proxy <- new.env(parent = emptyenv())
    proxy$sendInputMessage <- function(id, msg) {
      sent[[id]] <- msg$value
      invisible(NULL)
    }
    proxy$userData <- session$userData
    export_menu_server(input, output, proxy,
                       provenance = shiny::reactive(list()), seed = 1L)
  }
}

# UI-69: selecting a file stages it; pressing Run applies it and starts the
# pipeline. These tests exercise the applying half, so they do both.
.import_and_run <- function(session, path, n = 1L) {
  session$setInputs(import_config_file = list(
    datapath = path, name = "configuration.json", size = 20L,
    type = "application/json"))
  session$setInputs(import_run_config = n)
}

.import_file <- function(inputs) {
  # Plain tempfile: withr::local_tempfile() would delete the file when this
  # helper returns, before the import handler reads it.
  f <- tempfile(fileext = ".json")
  jsonlite::write_json(list(wiseapp_config_version = 1L, inputs = inputs),
                       f, auto_unbox = TRUE, pretty = TRUE)
  f
}

test_that("a deferred setting is applied once its control appears (UI-52)", {
  sent <- new.env(parent = emptyenv())
  testServer(.import_e2e_server(sent), {
    session$setInputs(`dummy_existing` = 1L)  # live before the import
    f <- .import_file(list(late_ctrl = "restored"))
    .import_and_run(session, f)

    # UI-72: the dialog no longer counts settings at the user - the pipeline
    # stage list is the feedback. The deferred queue is still the contract,
    # so assert on that instead of on prose.
    expect_true("late_ctrl" %in% session$userData$wise_import_state$pending$ids)
    expect_false("late_ctrl" %in% ls(sent))

    # The mock session flushes synchronously when the control appears, so the
    # retry observer sees it immediately in this environment.
    session$setInputs(`late_ctrl` = "placeholder")
    expect_equal(sent$late_ctrl, "restored")
    expect_match(session$userData$wise_import_status$text,
                 "All settings restored", fixed = TRUE)
  })
})

test_that("the heartbeat applies deferred settings without any input change", {
  sent <- new.env(parent = emptyenv())
  testServer(.import_e2e_server(sent), {
    skip_if_not(is.function(session$elapse),
                "MockShinySession$elapse() unavailable")
    f <- .import_file(list(late_ctrl = "restored"))
    .import_and_run(session, f)
    # Let the heartbeat run while the control is still absent. The pending
    # state remains active until the control appears in a later flush.
    session$elapse(1500)
    expect_false("late_ctrl" %in% ls(sent))
    expect_true("late_ctrl" %in% session$userData$wise_import_state$pending$ids)

    session$setInputs(`late_ctrl` = "placeholder")
    expect_equal(sent$late_ctrl, "restored")
  })
})

test_that("deferred settings that never appear are abandoned audibly", {
  sent <- new.env(parent = emptyenv())
  testServer(.import_e2e_server(sent), {
    session$setInputs(`dummy_existing` = 1L)
    f <- .import_file(list(never_ctrl = "x"))
    .import_and_run(session, f)
    expect_true("never_ctrl" %in% session$userData$wise_import_state$pending$ids)

    # Expire the retry window; the next retry pass gives up, audibly.
    session$userData$wise_import_state$pending$deadline <- Sys.time() - 1
    session$setInputs(`dummy_existing` = 2L)
    st <- session$userData$wise_import_status
    expect_equal(st$class, "alert-warning")
    # UI-72: the message says what to do about it, not just that it happened.
    expect_match(st$text, "could not be restored", fixed = TRUE)
    expect_match(st$text, "never_ctrl", fixed = TRUE)
    expect_match(st$text, "Overview", fixed = TRUE)
    expect_false("never_ctrl" %in% ls(sent))
    expect_null(session$userData$wise_import_state$pending)
  })
})


test_that("the archive writer falls back when no system zip is present", {
  skip_if_not_installed("zip")
  d <- withr::local_tempdir()
  writeLines("a,b\n1,2", file.path(d, "x.csv"))
  zf <- withr::local_tempfile(fileext = ".zip")

  # The zip package is the primary writer (PERF-38): hiding the system
  # binary must not matter to it.
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

test_that("list-column numbers keep full double precision in the CSV (UI-55)", {
  # format()'s 7-significant-digit default silently rounded specification
  # values (bin breaks, polynomial coefficients) on their way to the CSV.
  df <- data.frame(name = "tx")
  df$polynomial <- list(c(1.23456789012345, 9.87654321098765))
  flat <- .export_flatten_df(df)
  vals <- as.numeric(strsplit(flat$polynomial, "; ")[[1]])
  expect_equal(vals, c(1.23456789012345, 9.87654321098765), tolerance = 1e-13)
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


# ---- UI-70: what must never enter a configuration --------------------------

test_that("DataTables' own UI state is never exported", {
  # DT registers its controls as Shiny inputs under auto-numbered ids. The
  # number comes from render order, so the same table is Table_0 in one
  # session and Table_3 in the next - the value could not be restored to the
  # control it came from even in principle.
  for (id in c("DataTables_Table_0_length", "DataTables_Table_1_length",
               "DataTables_Table_3_length")) {
    expect_false(.export_keep_input(id), info = id)
  }
  expect_false(.export_keep_input("tbl_cell_edit"))
  expect_false(.export_keep_input("grid_state_change"))
})

test_that("real settings with similar names are still exported", {
  for (id in c("outcome", "currency", "step1-model-model_type",
               "step3-sp-targeting", "step1-weather-numBins")) {
    expect_true(.export_keep_input(id), info = id)
  }
})

test_that("action buttons are excluded by type, not by name", {
  testServer(function(input, output, session) {}, {
    btn <- function(n) structure(as.integer(n),
                                 class = c("shinyActionButtonValue", "integer"))
    session$setInputs(
      `step1-model-model_type` = "Linear regression",
      outcome                  = "welfare",
      outcome_stats_btn        = btn(3),   # names vary; the class does not
      survey_stats             = btn(1),
      some_future_button       = btn(2)
    )
    cfg <- wise_config_snapshot(input)
    # Shiny's button binding ignores `value`, so a button can never be
    # restored - carrying it only fills the deferred queue until it gives up.
    expect_setequal(names(cfg$inputs),
                    c("step1-model-model_type", "outcome"))
  })
})

test_that("a button-free config still round-trips its real settings", {
  testServer(function(input, output, session) {}, {
    session$setInputs(`step1-model-covariates` = "Lasso",
                      `step2-sim-climate` = c("ssp2_4_5", "ssp5_8_5"))
    cfg <- wise_config_snapshot(input)
    expect_equal(cfg$inputs$`step1-model-covariates`, "Lasso")
    expect_equal(cfg$inputs$`step2-sim-climate`, c("ssp2_4_5", "ssp5_8_5"))
  })
})
