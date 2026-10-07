# ============================================================================ #
# tests/testthat/test-fct_run_simulation.R                                     #
# REACT-12 / TEST-01: direct tests of fct_run_simulation()'s per-key failure   #
# ledger - fail fast on historical or whole-group failure, publish partial     #
# results with ledger + provenance counts otherwise. Weather loading and the   #
# per-key pipeline are injected (weather_fn / pipeline_fn) so no network or    #
# model fit is needed.                                                         #
# ============================================================================ #

library(testthat)

test_that("cached weather join preserves inner_join expansion and ordering", {
  survey <- data.frame(
    code = c("T", "T", "T"), year = c("2020", "2020", "2021"),
    survname = "S", loc_id = c("a", "a", "b"), int_month = c(1L, 1L, 2L),
    hhid = 1:3, stringsAsFactors = FALSE
  )
  weather <- data.frame(
    code = c("T", "T", "T"), year = c(2020L, 2020L, 2020L),
    survname = "S", loc_id = c("a", "a", "z"), int_month = c(1L, 1L, 2L),
    timestamp = as.POSIXct(c("2020-01-01", "2020-01-02", "2020-01-03"),
                           tz = "UTC"), temp = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  expected <- weather |>
    .add_sim_timestamp_fields() |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(survey,
                      by = c("code", "year", "survname", "loc_id", "int_month"),
                      relationship = "many-to-many") |>
    dplyr::mutate(year = as.factor(year))
  actual <- join_weather_survey_cached(weather, build_weather_join_cache(survey))
  expect_identical(actual, expected)
})

make_ledger_svy <- function(n = 60L) {
  data.frame(
    hhid     = seq_len(n),
    year     = 2020L,
    code     = "TST",
    survname = "SRV",
    loc_id   = sprintf("loc%02d", seq_len(n)),
    welfare  = stats::rnorm(n, 10, 2),
    weight   = 1,
    temp     = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

make_ledger_weather_result <- function(with_ssp5 = TRUE) {
  wh <- data.frame(
    code = "TST", year = 2030L, survname = "SRV", loc_id = "loc01",
    temp = 1, timestamp = as.POSIXct("2030-06-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  hist_wh <- wh
  hist_wh$year <- 2020L
  hist_wh$timestamp <- as.POSIXct("2020-06-01", tz = "UTC")

  out <- list(
    historical                        = hist_wh,
    "ssp2_4_5_2030_2040_ensemble_mean" = wh,
    "ssp2_4_5_2030_2040_ensemble_hi"   = wh
  )
  if (with_ssp5) {
    out[["ssp5_8_5_2030_2040_ensemble_mean"]] <- wh
  }
  out
}

make_ledger_pipeline_fn <- function() {
  function(weather_raw, ...) {
    if (isTRUE(attr(weather_raw, "fail")))
      stop("injected pipeline failure")
    list(sim_year = 2030L, y_point = 1:10, weight = rep(1, 10),
         weather_raw = weather_raw)
  }
}

run_ledger_sim <- function(weather_result,
                           pipeline_fn = make_ledger_pipeline_fn(),
                           weather_fn = function(...) weather_result, ...) {
  fct_run_simulation(
    sw                  = data.frame(name = "temp", stringsAsFactors = FALSE),
    so                  = data.frame(name = "welfare", type = "numeric",
                                     transform = "log", label = "Welfare",
                                     stringsAsFactors = FALSE),
    svy                 = make_ledger_svy(),
    ss                  = NULL,
    mf                  = list(fit3 = NULL, engine = "fixest",
                                train_data = make_ledger_svy(),
                                weather_terms = "temp"),
    cp                  = list(type = "local", path = tempdir()),
    fp_list             = list(c("2030-01-01", "2040-12-31")),
    ssps                = c("ssp2_4_5", "ssp5_8_5"),
    residuals           = "none",
    skip_coef_draws     = TRUE,
    sim_dates           = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL,
    stored_breaks       = NULL,
    weather_fn          = weather_fn,
    pipeline_fn         = pipeline_fn,
    ...
  )
}

mark_fail <- function(weather_result, keys) {
  for (k in keys) attr(weather_result[[k]], "fail") <- TRUE
  weather_result
}

test_that("all keys succeeding returns an empty ledger and full provenance", {
  res <- suppressWarnings(run_ledger_sim(make_ledger_weather_result()))

  expect_length(res$failures, 0L)
  expect_identical(res$n_keys_ok, res$n_keys)
  # Two requested groups (SSP2 x {mean,hi}, SSP5 x {mean}) -> two scenarios
  expect_setequal(names(res$new_scenarios),
                  c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"))
  expect_identical(res$new_scenarios[["SSP2-4.5 / 2030-2040"]]$n_models, 2L)
  expect_identical(
    res$new_scenarios[["SSP2-4.5 / 2030-2040"]]$n_models_requested, 2L)
  expect_identical(
    res$new_scenarios[["SSP5-8.5 / 2030-2040"]]$n_models_requested, 1L)
})

test_that("historical key failure fails the run with no results published", {
  wr <- make_ledger_weather_result()
  attr(wr$historical, "fail") <- TRUE

  expect_error(
    res <- suppressWarnings(run_ledger_sim(wr)),
    regexp = "Historical simulation failed"
  )
})

test_that("whole-group failure fails the run naming the group", {
  wr <- make_ledger_weather_result(with_ssp5 = TRUE)
  attr(wr[["ssp5_8_5_2030_2040_ensemble_mean"]], "fail") <- TRUE

  expect_error(
    suppressWarnings(run_ledger_sim(wr)),
    regexp = "All ensemble members failed for: SSP5-8.5 / 2030-2040"
  )
})

test_that("partial member failure publishes results with the ledger", {
  wr <- make_ledger_weather_result(with_ssp5 = TRUE)
  attr(wr[["ssp2_4_5_2030_2040_ensemble_hi"]], "fail") <- TRUE

  res <- suppressWarnings(run_ledger_sim(wr))

  # Ledger carries the failed key and its error message
  expect_length(res$failures, 1L)
  expect_identical(res$failures[[1L]]$key, "ssp2_4_5_2030_2040_ensemble_hi")
  expect_match(res$failures[[1L]]$error, "injected pipeline failure",
               fixed = TRUE)
  expect_false(res$failures[[1L]]$is_hist)
  expect_identical(res$failures[[1L]]$gk, "ssp2_4_5_2030_2040")

  # Provenance: scenario survives with 1 of 2 requested members
  s2 <- res$new_scenarios[["SSP2-4.5 / 2030-2040"]]
  expect_identical(s2$n_models, 1L)
  expect_identical(s2$n_models_requested, 2L)
  expect_identical(res$n_keys_ok, 3L)
  expect_identical(res$n_keys, 4L)

  # The successful member is still there under its member_type
  expect_true("ensemble_mean" %in% names(s2$pipelines))

  # The untouched group is intact
  expect_identical(
    res$new_scenarios[["SSP5-8.5 / 2030-2040"]]$n_models, 1L)
})

test_that("multiple partial failures across groups all reach the ledger", {
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  # ssp2 group has mean + hi; add one more member so single failures stay partial
  wr[["ssp2_4_5_2030_2040_ensemble_lo"]] <- wr[["ssp2_4_5_2030_2040_ensemble_mean"]]
  attr(wr[["ssp2_4_5_2030_2040_ensemble_hi"]], "fail") <- TRUE
  attr(wr[["ssp2_4_5_2030_2040_ensemble_lo"]], "fail") <- TRUE

  res <- suppressWarnings(run_ledger_sim(wr))

  expect_length(res$failures, 2L)
  expect_identical(res$n_keys_ok, res$n_keys - 2L)
  s2 <- res$new_scenarios[["SSP2-4.5 / 2030-2040"]]
  expect_identical(s2$n_models, 1L)
  expect_identical(s2$n_models_requested, 3L)
  expect_true("ensemble_mean" %in% names(s2$pipelines))
})

test_that("survey projection preserves model inputs, types, row order, and duplicates", {
  svy <- data.frame(
    hhid = c("hh2", "hh1", "hh1"), code = "TST", year = 2020L,
    survname = "SRV", loc_id = "loc01", int_month = c(2L, 1L, 1L),
    welfare = c(2, 1, 1), temp = 0,
    group = factor(c("b", "a", "a"), levels = c("b", "a", "unused")),
    control = c(3, 2, 2), weight = c(2, 1, 1), unused = letters[1:3],
    stringsAsFactors = FALSE
  )
  mf <- list(
    fit3 = stats::lm(welfare ~ temp * group + control, data = svy),
    formulas = list(formula3 = welfare ~ temp * group + control),
    weather_terms = "temp", interaction_terms = "temp:group",
    fe_terms = character(0)
  )
  projected <- .step2_survey_projection(
    svy, mf, data.frame(name = "temp"), data.frame(name = "welfare"),
    id_col = "hhid", weight_cols = "weight"
  )
  expect_identical(names(projected), c(
    "hhid", "code", "year", "survname", "loc_id", "int_month",
    "group", "control", "weight"
  ))
  expect_identical(projected$hhid, svy$hhid)
  expect_identical(projected$group, svy$group)
  expect_identical(projected$int_month, svy$int_month)
  expect_identical(projected$year, rep("2020", 3L))
})

test_that("survey projection falls back when formula or metadata is incomplete", {
  svy <- data.frame(
    hhid = "hh1", code = "TST", year = 2020L, survname = "SRV",
    loc_id = "loc01", int_month = 1L, welfare = 1, temp = 2,
    moderator = "urban", control = 3, fe = "district", weight = 1,
    unused = "retain", stringsAsFactors = FALSE
  )
  complete <- list(
    formulas = list(formula3 = welfare ~ temp * moderator + control + fe),
    weather_terms = "temp", interaction_terms = "temp:moderator", fe_terms = "fe"
  )
  expected <- transform(
    svy[, setdiff(names(svy), c("temp", "welfare")), drop = FALSE],
    year = as.character(year)
  )
  cases <- list(
    missing_formula3 = within(complete, formulas <- list()),
    missing_weather_metadata = complete[names(complete) != "weather_terms"],
    missing_interaction_metadata = complete[names(complete) != "interaction_terms"],
    missing_fe_metadata = complete[names(complete) != "fe_terms"],
    missing_formula_covariate = within(complete, formulas <- list(
      formula3 = welfare ~ temp * moderator + absent_control + fe)),
    missing_moderator = within(complete, interaction_terms <- "temp:absent_mod"),
    missing_fixed_effect = within(complete, fe_terms <- "absent_fe"),
    mismatched_weather = within(complete, weather_terms <- "absent_weather")
  )
  for (case_name in names(cases)) {
    out <- .step2_survey_projection(
      svy, cases[[case_name]], data.frame(name = "temp"),
      data.frame(name = "welfare"), id_col = "hhid", weight_cols = "weight"
    )
    expect_identical(out, expected, info = case_name)
  }
})

test_that("survey projection falls back when required ID or weight is absent", {
  svy <- data.frame(
    code = "TST", year = 2020L, survname = "SRV", loc_id = "loc01",
    int_month = 1L, welfare = 1, temp = 2, control = 3, unused = "retain",
    stringsAsFactors = FALSE
  )
  mf <- list(formulas = list(formula3 = welfare ~ temp + control),
             weather_terms = "temp", interaction_terms = character(0),
             fe_terms = character(0))
  expected <- transform(
    svy[, setdiff(names(svy), c("temp", "welfare")), drop = FALSE],
    year = as.character(year)
  )
  expect_identical(.step2_survey_projection(
    svy, mf, data.frame(name = "temp"), data.frame(name = "welfare"),
    id_col = "hhid"), expected)
  expect_identical(.step2_survey_projection(
    svy, mf, data.frame(name = "temp"), data.frame(name = "welfare"),
    weight_cols = "weight"), expected)
})

test_that("weather retrieval receives selected survey metadata unchanged", {
  captured <- NULL
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  svy <- make_ledger_svy()
  ss <- data.frame(code = c("TST", "TST"), year = c(2020L, 2020L),
                   survname = c("SRV", "SRV_ALT"), source = c("src", "src_alt"),
                   stringsAsFactors = FALSE)
  suppressWarnings(fct_run_simulation(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
                    stringsAsFactors = FALSE), svy = svy, ss = ss,
    mf = list(fit3 = NULL, engine = "fixest", train_data = svy,
              weather_terms = "temp"), cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")), ssps = "ssp2_4_5",
    residuals = "none", skip_coef_draws = TRUE,
    sim_dates = c("2020-01-01", "2020-12-31"), perturbation_method = NULL,
    stored_breaks = NULL,
    weather_fn = function(selected_surveys, ...) {
      captured <<- selected_surveys
      wr
    }, pipeline_fn = make_ledger_pipeline_fn()
  ))
  expect_identical(captured, ss)
  expect_identical(captured$survname, c("SRV", "SRV_ALT"))
  expect_identical(captured$source, c("src", "src_alt"))
})

test_that("weather thread mode is forwarded to the weather loader", {
  captured <- NULL
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  suppressWarnings(fct_run_simulation(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
                    stringsAsFactors = FALSE),
    svy = make_ledger_svy(),
    ss = data.frame(code = "TST", year = 2020L, survname = "SRV", source = "src"),
    mf = list(fit3 = NULL, engine = "fixest", train_data = make_ledger_svy(),
              weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(), ssps = character(0), residuals = "none",
    skip_coef_draws = TRUE, sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL, stored_breaks = NULL, weather_threads = "2",
    weather_fn = function(..., weather_threads) {
      captured <<- weather_threads
      wr
    }, pipeline_fn = make_ledger_pipeline_fn()
  ))
  expect_identical(captured, "2")
})

test_that("historical preview is emitted before future keys and stays bounded", {
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  order <- character(0)
  previews <- list()
  res <- suppressWarnings(run_ledger_sim(
    wr,
    preview_fn = function(preview) {
      order <<- c(order, "preview")
      previews[[length(previews) + 1L]] <<- preview
    },
    pipeline_fn = function(weather_raw, ...) {
      order <<- c(order, if (as.integer(format(weather_raw$timestamp[[1]], "%Y")) == 2020L) {
        "historical"
      } else {
        "future"
      })
      list(
        y_point = c(1, 2), F_loading = NULL,
        sim_year = c(2030L, 2030L), weight = c(1, 2),
        weather_raw = weather_raw
      )
    }
  ))

  expect_identical(order[1:3], c("historical", "preview", "future"))
  expect_length(previews, 1L)
  preview <- previews[[1L]]
  expect_identical(names(preview$data), c("sim_year", "value", "uncertainty"))
  expect_identical(preview$data$sim_year, 2030L)
  expect_identical(preview$metadata$method, "mean")
  expect_identical(preview$metadata$weighted, TRUE)
  expect_identical(preview$metadata$transform, "log")
  expect_false(any(c("y_point", "F_loading", "weight") %in% names(preview$data)))
  expected <- aggregate_pipeline_tables_multi(
    pipelines = res$hist_sim_result$pipeline,
    methods = "mean",
    weighted = TRUE,
    residuals = "none",
    is_log = TRUE,
    band_q = c(lo = 0.10, hi = 0.90),
    skip_coef = TRUE,
    bandwidth_p0 = 0.05,
    seed = WISEAPP_DEFAULT_SEED,
    model_ids = "Historical",
    scenario = "Historical",
    shared_context = res$hist_sim_result$shared_context
  )[["mean"]]
  expect_equal(preview$data$value, expected$value)
  expect_equal(preview$data$uncertainty, sqrt(pmax(expected$var_within, 0)))
  expect_identical(res$n_keys_ok, res$n_keys)
})

test_that("preview callback failures do not fail valid history", {
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  res <- suppressWarnings(run_ledger_sim(
    wr,
    preview_fn = function(preview) stop("preview transport failed"),
    pipeline_fn = make_ledger_pipeline_fn()
  ))
  expect_length(res$failures, 0L)
  expect_true(is.list(res$hist_sim_result))
})

test_that("cooperative cancellation propagates rather than entering failure ledger", {
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  cancellation <- structure(
    list(message = "cancelled", call = NULL),
    class = c("wiseapp_step2_cancelled", "error", "condition")
  )
  checkpoints <- list()
  expect_error(
    run_ledger_sim(
      wr,
      checkpoint_fn = function(event) {
        checkpoints[[length(checkpoints) + 1L]] <<- event
        if (identical(event$stage, "historical_ready")) stop(cancellation)
      }
    ),
    class = "wiseapp_step2_cancelled"
  )
  expect_true(any(vapply(checkpoints, function(event) {
    identical(event$stage, "historical_ready")
  }, logical(1))))
})

# Consumer-style weather_fn: emits each key through weather_consumer with
# get_weather()-style metadata (member_index / period_members).
make_consumer_weather_fn <- function(weather_result, with_metadata = TRUE) {
  function(..., weather_consumer) {
    gks <- vapply(names(weather_result), function(k) {
      if (identical(k, "historical")) NA_character_ else .key_group(k)$gk
    }, character(1))
    for (k in names(weather_result)) {
      meta <- list(order = 1L)
      if (with_metadata && !is.na(gks[[k]])) {
        siblings <- names(weather_result)[!is.na(gks) & gks == gks[[k]]]
        meta$member_index <- match(k, siblings)
        meta$period_members <- length(siblings)
      }
      weather_consumer(k, weather_result[[k]], meta)
    }
    list()
  }
}

collect_group_events <- function(wr, with_metadata, expect_error = FALSE) {
  events <- list()
  run <- function() {
    suppressWarnings(run_ledger_sim(
      wr,
      weather_fn = make_consumer_weather_fn(wr, with_metadata),
      checkpoint_fn = function(event) events[[length(events) + 1L]] <<- event
    ))
  }
  if (expect_error) expect_error(run(), "All ensemble members failed") else run()
  events
}

test_that("group_completed fires once per group, in order, with metadata", {
  events <- collect_group_events(make_ledger_weather_result(), TRUE)
  done <- Filter(function(e) identical(e$stage, "group_completed"), events)
  expect_length(done, 2L)
  expect_identical(
    vapply(done, `[[`, character(1), "label"),
    c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
  )
  expect_identical(vapply(done, `[[`, integer(1), "groups_done"), 1:2)
  expect_identical(vapply(done, `[[`, integer(1), "groups_total"), c(2L, 2L))
  expect_identical(vapply(done, `[[`, integer(1), "n_models"), c(2L, 1L))
  expect_identical(vapply(done, `[[`, integer(1), "n_models_requested"), c(2L, 1L))
  # Metadata trigger: the first group completes before the SSP5 member starts.
  stages <- vapply(events, `[[`, character(1), "stage")
  starts <- which(stages == "pipeline_started" &
    vapply(events, function(e) identical(e$current_label, "SSP5-8.5 / 2030-2040"), logical(1)))
  expect_lt(which(stages == "group_completed")[[1L]], starts[[1L]])
  # Member events carry determinate progress fields.
  member <- Filter(function(e) identical(e$current_label, "SSP2-4.5 / 2030-2040") &&
    identical(e$stage, "pipeline_started"), events)
  expect_identical(vapply(member, `[[`, integer(1), "member_index"), 1:2)
  expect_true(all(vapply(member, `[[`, integer(1), "period_members") == 2L))
  expect_true(all(vapply(events, function(e) !is.null(e$groups_total), logical(1))))
})

test_that("group_completed fires once per group without consumer metadata", {
  wr <- make_ledger_weather_result()
  for (events in list(
    collect_group_events(wr, FALSE),
    {
      ev <- list()
      suppressWarnings(run_ledger_sim(
        wr, checkpoint_fn = function(event) ev[[length(ev) + 1L]] <<- event
      ))
      ev
    }
  )) {
    done <- Filter(function(e) identical(e$stage, "group_completed"), events)
    expect_identical(
      vapply(done, `[[`, character(1), "label"),
      c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
    )
    expect_identical(vapply(done, `[[`, integer(1), "groups_done"), 1:2)
    expect_identical(vapply(done, `[[`, integer(1), "n_models"), c(2L, 1L))
  }
})

test_that("a group whose members all fail still completes once, then the run errors", {
  wr <- mark_fail(make_ledger_weather_result(), "ssp5_8_5_2030_2040_ensemble_mean")
  for (with_metadata in c(TRUE, FALSE)) {
    events <- collect_group_events(wr, with_metadata, expect_error = TRUE)
    done <- Filter(function(e) identical(e$stage, "group_completed"), events)
    expect_length(done, 2L)
    expect_identical(done[[2L]]$label, "SSP5-8.5 / 2030-2040")
    expect_identical(done[[2L]]$n_models, 0L)
    expect_identical(done[[2L]]$n_models_requested, 1L)
    expect_identical(done[[2L]]$groups_done, 2L)
  }
})

test_that("profiling helpers are called without defensive exists() guards (CR-CQ-05)", {
  src <- testthat::test_path("..", "..", "R", c(
    "fct_run_simulation.R", "fct_rif_sim.R", "fct_simulations.R",
    "fct_policy_decompose.R"
  ))
  skip_if(!all(file.exists(src)), "R/ source tree not available (installed package)")
  text <- unlist(lapply(src, readLines, warn = FALSE))
  expect_false(any(grepl(
    "exists\\(\"\\.(wx_process_tree_rss_bytes|prediction_profile_record)\"", text)))
  expect_false(any(grepl(
    "exists\\(\"(weighted_baseline_deciles|baseline_weight_column)\"", text)))
  expect_true(is.function(.wx_process_tree_rss_bytes))
  expect_true(is.function(.prediction_profile_record))
})
