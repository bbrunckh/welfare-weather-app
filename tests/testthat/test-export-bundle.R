# ============================================================================ #
# tests/testthat/test-export-bundle.R                                          #
# UI-48: an analysis must be exportable as a self-describing bundle -          #
#        configuration, tables, figures, manifest and README - and importable  #
#        again.                                                                #
# ============================================================================ #

library(testthat)
library(shiny)

item <- function(key, kind = "table", step = 1L, fun = NULL, label = key,
                 description = "Description.", width = 6, height = 4) {
  list(key = key, label = label, step = as.integer(step), kind = kind,
       fun = fun %||% function() data.frame(a = 1:2, b = c("x", "y")),
       description = description, width = width, height = height)
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
  shiny::testServer(function(input, output, session) {
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
  zf <- withr::local_tempfile(fileext = ".zip")
  items <- list(item("first"), item("never_run", fun = function() NULL))
  before <- .Random.seed
  wise_export_bundle(zf, items, config = NULL)
  expect_identical(.Random.seed, before)
})

test_that("an artefact that errors is skipped with a warning, not fatal", {
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

# ---- echarts figure PNG export (guidelines §7) -------------------------------

test_that("an echarts figure renders to PNG through the headless browser", {
  has_chrome <- tryCatch({
    ci <- chromote::Chromote$new()
    ci$close()
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(has_chrome, "no headless Chrome available")

  widget <- echarts4r::e_charts(data.frame(x = 1:3, y = c(1, 4, 2)), x) |>
    echarts4r::e_bar(y)
  path <- withr::local_tempfile(fileext = ".png")
  unlink(path)
  res <- .export_write_item(
    item("ech", kind = "figure", width = 6, height = 4,
         fun = function() widget),
    dirname(path), basename(path)
  )
  expect_equal(res$status, "ok")
  expect_true(file.exists(path))
  expect_gt(file.size(path), 1000)
})

test_that("a req() throw inside a registered artefact leaves no failure note", {
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
  fig <- item("plot", kind = "figure",
              fun = function() echarts4r::e_charts(mtcars, wt) |>
                echarts4r::e_scatter(mpg))
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
  expect_match(md, "Welfare", fixed = TRUE)
  expect_match(md, "Run signature", fixed = TRUE)
  # CR-SEC-05: only the source type is recorded - neither its location nor
  # the means of reading it.
  expect_false(grepl("https://adb-1.example.net", md, fixed = TRUE))
  expect_false(grepl("/Volumes/cat", md, fixed = TRUE))
  expect_false(grepl("TOP-SECRET-VALUE", md, fixed = TRUE))
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

test_that("DataTables UI state and legacy button ids are excluded", {
  expect_false(all(.export_keep_input(c(
    "DataTables_Table_0_length",
    "DataTables_Table_1_search",
    "tbl_cell_edit",
    "grid_state_change",
    "survey_stats",
    "weather_stats",
    "outcome_stats_btn"
  ))))
  expect_true(all(.export_keep_input(c(
    "outcome",
    "step1-model-model_type",
    "step3-sp-targeting"
  ))))
})

test_that("action-button values are filtered by class during snapshot", {
  testServer(function(input, output, session) {}, {
    button <- structure(3L,
                        class = c("shinyActionButtonValue", "integer"))
    session$setInputs(
      `step1-model-model_type` = "Linear regression",
      outcome = "welfare",
      arbitrary_button = button
    )
    cfg <- wise_config_snapshot(input)
    expect_true(all(c("step1-model-model_type", "outcome") %in%
                    names(cfg$inputs)))
    expect_false("arbitrary_button" %in% names(cfg$inputs))
  })
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
  # CR-SEC-05: source identity (type, hosts, buckets, paths) is not carried
  # either; other steps' settings are.
  expect_false(any(.export_keep_input(c(
    "overview-connection_type", "overview-db_workspace",
    "overview-db_volume_path", "overview-s3_bucket", "overview-s3_prefix",
    "overview-s3_region", "overview-gcs_bucket", "overview-hf_repo",
    "overview-hf_subdir", "overview-local_path"))))
  expect_true(all(.export_keep_input(c("step1-model-model_type", "sim-fut_period_1"))))
})

test_that("config import drops connection settings and says so (CR-SEC-05)", {
  sent <- character(0)
  session <- list(
    input = list(),
    sendInputMessage = function(id, message) sent <<- c(sent, id)
  )
  cfg <- list(inputs = list(
    "overview-connection_type" = "databricks",
    "overview-db_workspace" = "https://evil.cloud.databricks.com",
    "overview-local_path" = "/etc",
    "step1-model-model_type" = "Linear regression"
  ))
  res <- wise_config_apply(cfg, session)
  expect_identical(sent, "step1-model-model_type")
  expect_identical(res$applied, "step1-model-model_type")
  expect_length(res$pending, 0L)

  n <- .import_dropped_connection(cfg)
  expect_identical(n, 3L)
  text <- .import_status_text(res, n)
  expect_match(text, "Applied 1 setting(s).", fixed = TRUE)
  expect_match(text, "3 data-source setting(s) in the file were not imported", fixed = TRUE)
  expect_false(grepl("not imported", .import_status_text(res, 0L)))
  expect_match(.import_status_text(list(applied = "a", pending = c("b", "c")), 0L),
    "Applied 1 setting(s); 2 more", fixed = TRUE)
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
    expect_false(grepl("my-bucket", js, fixed = TRUE))
    expect_false(any(grepl("^overview-", names(cfg$inputs))))
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
    # CR-SEC-05: no connection field is pushed, not even the bucket.
    expect_length(res$applied, 0L)
    expect_length(sent, 0L)
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

test_that("applying legacy configurations filters transient ids", {
  sent <- list()
  fake <- list(sendInputMessage = function(id, msg) {
    sent[[id]] <<- msg$value
    invisible(NULL)
  })
  cfg <- list(inputs = list(
    real_setting = "yes",
    DataTables_Table_0_length = 25,
    old_run_btn = 4L
  ))
  res <- wise_config_apply(cfg, fake,
                           existing = names(cfg$inputs))
  expect_equal(res$applied, "real_setting")
  expect_equal(sent$real_setting, "yes")
  expect_false(any(c("DataTables_Table_0_length", "old_run_btn") %in%
                   names(sent)))
})

test_that("applying an empty config is a no-op", {
  res <- wise_config_apply(list(inputs = list()), list(), character(0))
  expect_length(res$applied, 0L)
  expect_length(res$pending, 0L)
})

test_that("configuration validation rejects non-object JSON and malformed inputs", {
  bad_empty <- .import_validate(list())
  expect_false(bad_empty$ok)
  bad_scalar <- .import_validate(1)
  expect_false(bad_scalar$ok)
  bad_inputs <- .import_validate(list(inputs = "not-an-object"))
  expect_false(bad_inputs$ok)
})

test_that("input application keeps failed controls pending", {
  fake <- list(sendInputMessage = function(id, msg) stop("unavailable"))
  res <- wise_config_apply(list(inputs = list(setting = "value")), fake,
                           existing = "setting")
  expect_equal(res$failed, "setting")
  expect_equal(res$pending, "setting")
  expect_length(res$applied, 0L)
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
                       provenance = shiny::reactive(list()), seed = 1L,
                       run_triggers = list(), step_results = list())
  }
}

test_that("configuration exports retain the session analysis seed", {
  testServer(function(input, output, session) {
    export_menu_server(input, output, session, seed = 123L)
  }, {
    session$userData$wise_analysis_seed <- 907L
    file <- output$export_config
    cfg <- jsonlite::read_json(file, simplifyVector = FALSE)
    expect_equal(cfg$random_seed, 907L)
    expect_equal(cfg$wiseapp_config_version, 1L)
  })
})

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
    session$setInputs(import_config_file = list(
      datapath = f, name = "configuration.json", size = 20L,
      type = "application/json"))

    st <- session$userData$wise_import_status
    expect_equal(st$class, "alert-success")
    expect_match(st$text, "1 more will be applied", fixed = TRUE)
    expect_false("late_ctrl" %in% ls(sent))

    # The mock session flushes synchronously when the control appears, so the
    # retry observer sees it immediately in this environment.
    session$setInputs(`late_ctrl` = "placeholder")
    expect_equal(sent$late_ctrl, "restored")
    expect_match(session$userData$wise_import_status$text,
                 "All deferred settings restored")
  })
})

test_that("the heartbeat applies deferred settings without any input change", {
  sent <- new.env(parent = emptyenv())
  testServer(.import_e2e_server(sent), {
    skip_if_not(is.function(session$elapse),
                "MockShinySession$elapse() unavailable")
    f <- .import_file(list(late_ctrl = "restored"))
    session$setInputs(import_config_file = list(
      datapath = f, name = "configuration.json", size = 20L,
      type = "application/json"))
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
    session$setInputs(import_config_file = list(
      datapath = f, name = "configuration.json", size = 20L,
      type = "application/json"))
    # Expire the retry window; the next retry pass gives up, audibly.
    session$userData$wise_import_state$pending$deadline <- Sys.time() - 1
    session$setInputs(`dummy_existing` = 2L)
    st <- session$userData$wise_import_status
    expect_equal(st$class, "alert-warning")
    expect_match(st$text, "Gave up on 1 setting")
    expect_false("never_ctrl" %in% ls(sent))
    expect_null(session$userData$wise_import_state$pending)
  })
})


test_that("the archive writer falls back when no system zip is present", {
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
  items <- list(
    item("good_fig", kind = "figure",
         fun = function() echarts4r::e_charts(mtcars, wt) |>
           echarts4r::e_scatter(mpg)),
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

test_that("echarts PNG export removes its temporary page and _files dir (R2-SEC-05)", {
  seen <- NULL
  local_mocked_bindings(saveWidget = function(widget, file, ...) {
    seen <<- file
    writeLines("<html></html>", file)
    libdir <- paste0(tools::file_path_sans_ext(file), "_files")
    dir.create(libdir)
    writeLines("x", file.path(libdir, "dep.js"))
  }, .package = "htmlwidgets")
  fail_shot <- FALSE
  local_mocked_bindings(webshot = function(url, file, ...) {
    if (fail_shot) stop("browser crashed")
    writeLines("png", file)
  }, .package = "webshot2")
  out <- withr::local_tempfile(fileext = ".png")
  item <- list(width = 4, height = 3)
  for (fail_shot in c(FALSE, TRUE)) {
    res <- .export_write_echarts(list(x = list(opts = list())), out, item)
    expect_identical(res$status, if (fail_shot) "error" else "ok")
    expect_false(file.exists(seen))
    expect_false(dir.exists(paste0(tools::file_path_sans_ext(seen), "_files")))
  }
})
