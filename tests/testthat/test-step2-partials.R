# ============================================================================ #
# tests/testthat/test-step2-partials.R                                         #
# Phase 3 of review/step2_progressive_results_plan.md: the worker emits, for   #
# the historical key and for each completed (SSP x period) group, the same     #
# aggregated table the Results tab computes on the main process.               #
# ============================================================================ #

library(testthat)

# Fixture: a real lm fit (so coefficient draws and original residuals are
# available) with injected weather and pipelines. Pipelines carry the shape
# run_sim_pipeline() returns: sim_year, y_point, weight (optional), F_loading,
# id_vec, weather_raw.
make_partial_weather_result <- function(with_ssp5 = TRUE) {
  wh <- data.frame(
    code = "TST", year = 2030L, survname = "SRV", loc_id = "loc01",
    temp = 1, timestamp = as.POSIXct("2030-06-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  hist_wh <- wh
  hist_wh$year <- 2020L
  hist_wh$timestamp <- as.POSIXct("2020-06-01", tz = "UTC")
  out <- list(
    historical = hist_wh,
    "ssp2_4_5_2030_2040_ensemble_mean" = wh,
    "ssp2_4_5_2030_2040_ensemble_hi" = wh
  )
  if (with_ssp5) out[["ssp5_8_5_2030_2040_ensemble_mean"]] <- wh
  out
}

make_partial_svy <- function(n = 40L, weighted = TRUE) {
  set.seed(11)
  svy <- data.frame(
    hhid = seq_len(n), year = 2020L, code = "TST", survname = "SRV",
    loc_id = sprintf("loc%02d", seq_len(n)),
    welfare = stats::rnorm(n, 1, 0.5), temp = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  if (weighted) svy$weight <- rep(c(1, 2, 3, 4), length.out = n)
  svy
}

make_partial_pipeline_fn <- function(svy, weighted = TRUE) {
  n <- nrow(svy)
  calls <- 0L
  function(weather_raw, ...) {
    calls <<- calls + 1L
    is_hist <- as.integer(format(weather_raw$timestamp[[1L]], "%Y")) == 2020L
    yrs <- if (is_hist) 2020:2021 else 2030:2031
    set.seed(100L + calls)
    out <- list(
      sim_year = rep(yrs, each = n),
      y_point = stats::rnorm(2L * n, 1 + 0.1 * calls, 0.5),
      F_loading = matrix(stats::rnorm(4L * n, 0, 0.05), ncol = 2L),
      id_vec = rep(svy$hhid, 2L),
      weather_raw = weather_raw
    )
    if (weighted) out$weight <- rep(svy$weight, 2L)
    out
  }
}

run_partial_sim <- function(weighted = TRUE, partial_fn = NULL,
                            weather_result = make_partial_weather_result(),
                            display = NULL, pipeline_fn = NULL,
                            residuals = "original", skip_coef_draws = FALSE) {
  svy <- make_partial_svy(weighted = weighted)
  fct_run_simulation(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
                    label = "Welfare", stringsAsFactors = FALSE),
    svy = svy, ss = NULL,
    mf = list(fit3 = fixest::feols(welfare ~ temp, data = svy),
              engine = "fixest", train_data = svy, weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")),
    ssps = c("ssp2_4_5", "ssp5_8_5"),
    residuals = residuals, skip_coef_draws = skip_coef_draws,
    sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL, stored_breaks = NULL,
    weather_fn = function(...) weather_result,
    pipeline_fn = pipeline_fn %||% make_partial_pipeline_fn(svy, weighted),
    partial_fn = partial_fn, display = display
  )
}

collect_partials <- function(...) {
  got <- list()
  res <- suppressWarnings(suppressMessages(run_partial_sim(
    partial_fn = function(p) got[[length(got) + 1L]] <<- p, ...
  )))
  list(partials = got, result = res)
}

test_that("partials arrive as historical then each group in completion order", {
  run <- collect_partials()
  p <- run$partials
  expect_length(p, 3L)
  expect_identical(
    vapply(p, `[[`, character(1), "kind"),
    c("historical", "scenario", "scenario")
  )
  expect_identical(
    vapply(p, `[[`, character(1), "label"),
    c("Historical", "SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
  )
  expect_identical(vapply(p, `[[`, integer(1), "ordinal"), 0:2)
  expect_identical(vapply(p, `[[`, integer(1), "groups_total"), rep(2L, 3L))
  expect_identical(vapply(p, `[[`, integer(1), "n_models"), c(1L, 2L, 1L))
  expect_identical(
    vapply(p, `[[`, integer(1), "n_models_requested"), c(1L, 2L, 1L)
  )
  for (x in p) {
    expect_identical(x$schema, 1L)
    expect_s3_class(x$table, "tbl_df")
    expect_true(all(c("sim_year", "value", "value_all_sd", "F_agg_all") %in%
      names(x$table)))
    expect_identical(x$display$method, "mean")
    expect_identical(x$display$weight_key, "weighted")
    expect_identical(x$display$pov_line, 3)
    expect_identical(x$display$bandwidth_p0, 0.05)
    expect_identical(x$display$residuals, "original")
    expect_true(x$display$is_log)
    expect_true(x$has_weights)
    expect_true(x$has_draws)
    expect_identical(x$so$name, "welfare")
  }
  expect_identical(p[[1L]]$table$sim_year, 2020:2021)
  # The streamed scenario table carries one model entry per member.
  expect_length(p[[2L]]$table$value_all[[1L]], 2L)
  expect_identical(run$result$n_keys_ok, run$result$n_keys)
})

test_that("display settings resolve with module fallbacks", {
  so <- data.frame(name = "welfare", type = "numeric", transform = "log",
                   povline = 4.2, stringsAsFactors = FALSE)
  d <- .step2_resolve_display(NULL, so, "none", TRUE)
  expect_identical(d$method, "mean")
  expect_identical(d$pov_line, 4.2)
  expect_identical(d$bandwidth_p0, 0.05)
  d <- .step2_resolve_display(
    list(method = "gini", pov_line = 5, bandwidth_p0 = 0.1), so, "none", TRUE
  )
  expect_identical(d$method, "gini")
  expect_identical(d$pov_line, 5)
  expect_identical(d$bandwidth_p0, 0.1)
  # Unsupported method for the outcome falls back to "mean".
  so_bin <- data.frame(name = "employed", type = "logical",
                       stringsAsFactors = FALSE)
  expect_identical(
    .step2_resolve_display(list(method = "gini"), so_bin, "none", TRUE)$method,
    "mean"
  )
  expect_identical(
    .step2_resolve_display(list(pov_line = -1), data.frame(
      name = "welfare", type = "numeric"), "none", TRUE)$pov_line,
    3
  )
})

test_that("a failing partial_fn does not fail the run or the result", {
  res <- suppressWarnings(suppressMessages(run_partial_sim(
    partial_fn = function(p) stop("partial transport failed")
  )))
  expect_identical(res$n_keys_ok, res$n_keys)
  expect_setequal(names(res$new_scenarios),
                  c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"))
})

test_that("a cancellation raised inside partial_fn propagates", {
  cancel <- structure(
    class = c("wiseapp_step2_cancelled", "error", "condition"),
    list(message = "cancelled", call = NULL)
  )
  for (at in c("Historical", "SSP2-4.5 / 2030-2040")) {
    expect_error(
      suppressWarnings(suppressMessages(run_partial_sim(
        partial_fn = function(p) if (identical(p$label, at)) stop(cancel)
      ))),
      class = "wiseapp_step2_cancelled"
    )
  }
})

test_that("a group with no successful member emits nothing and the run fails", {
  wr <- make_partial_weather_result()
  attr(wr[["ssp5_8_5_2030_2040_ensemble_mean"]], "fail") <- TRUE
  svy <- make_partial_svy()
  base_fn <- make_partial_pipeline_fn(svy)
  fn <- function(weather_raw, ...) {
    if (isTRUE(attr(weather_raw, "fail"))) stop("injected pipeline failure")
    base_fn(weather_raw, ...)
  }
  got <- character(0)
  expect_error(
    suppressWarnings(suppressMessages(run_partial_sim(
      partial_fn = function(p) got <<- c(got, p$label),
      weather_result = wr, pipeline_fn = fn
    ))),
    "All ensemble members failed"
  )
  expect_identical(got, c("Historical", "SSP2-4.5 / 2030-2040"))
})

test_that("a partially failed group reports n_models < n_models_requested", {
  wr <- make_partial_weather_result()
  attr(wr[["ssp2_4_5_2030_2040_ensemble_hi"]], "fail") <- TRUE
  svy <- make_partial_svy()
  base_fn <- make_partial_pipeline_fn(svy)
  fn <- function(weather_raw, ...) {
    if (isTRUE(attr(weather_raw, "fail"))) stop("injected pipeline failure")
    base_fn(weather_raw, ...)
  }
  run <- collect_partials(weather_result = wr, pipeline_fn = fn)
  g1 <- run$partials[[2L]]
  expect_identical(g1$n_models, 1L)
  expect_identical(g1$n_models_requested, 2L)
})

test_that("preview output is unchanged when partial_fn is also supplied", {
  wr <- make_partial_weather_result(with_ssp5 = FALSE)
  svy <- make_partial_svy()
  previews <- list()
  res <- suppressWarnings(suppressMessages(fct_run_simulation(
    sw = data.frame(name = "temp"), so = data.frame(
      name = "welfare", type = "numeric", transform = "log"),
    svy = svy, ss = NULL,
    mf = list(fit3 = fixest::feols(welfare ~ temp, data = svy), engine = "fixest",
              train_data = svy, weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")), ssps = "ssp2_4_5",
    residuals = "original", skip_coef_draws = FALSE,
    sim_dates = c("2020-01-01", "2020-12-31"), perturbation_method = NULL,
    stored_breaks = NULL, weather_fn = function(...) wr,
    pipeline_fn = make_partial_pipeline_fn(svy),
    preview_fn = function(p) previews[[length(previews) + 1L]] <<- p
  )))
  both <- NULL
  suppressWarnings(suppressMessages(fct_run_simulation(
    sw = data.frame(name = "temp"), so = data.frame(
      name = "welfare", type = "numeric", transform = "log"),
    svy = svy, ss = NULL,
    mf = list(fit3 = fixest::feols(welfare ~ temp, data = svy), engine = "fixest",
              train_data = svy, weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")), ssps = "ssp2_4_5",
    residuals = "original", skip_coef_draws = FALSE,
    sim_dates = c("2020-01-01", "2020-12-31"), perturbation_method = NULL,
    stored_breaks = NULL, weather_fn = function(...) wr,
    pipeline_fn = make_partial_pipeline_fn(svy),
    preview_fn = function(p) both <<- p, partial_fn = function(p) NULL
  )))
  expect_length(previews, 1L)
  expect_identical(previews[[1L]], both)
})

# ---- PARITY with the Results module ----------------------------------------

# Drive the real module with the final result of the same run and return the
# table it displays for the method (hist or scenario label).
module_displayed_table <- function(result, method, pl_in, bw_in,
                                   which = "Historical") {
  out <- NULL
  mth <- method
  lbl <- which
  # Unrelated downstream outputs warn on these minimal fixtures; only the
  # aggregation tables matter here.
  suppressWarnings(shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id = "results",
      hist_sim = shiny::reactiveVal(result$hist_sim_result),
      saved_scenarios = shiny::reactiveVal(result$new_scenarios),
      selected_hist = shiny::reactiveVal(NULL),
      tabset_id = "step2_output_tabs",
      residuals = shiny::reactive(result$hist_sim_result$residuals),
      skip_coef_draws = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(
        cmp_agg_method = mth, pov_line = pl_in, bandwidth_p0 = bw_in
      )
      session$elapse(500)
      session$flushReact()
      out <<- if (identical(lbl, "Historical")) {
        hist_agg_rv()[[weight_key()]][[mth]]
      } else {
        scenario_agg_rv()[[lbl]][[weight_key()]][[mth]]
      }
    }
  ))
  out
}

test_that("partial tables are identical to the module's displayed tables", {
  skip_if_not_installed("shiny")
  for (weighted in c(TRUE, FALSE)) {
    for (method in c("mean", "headcount_ratio", "gini", "prosperity_gap")) {
      info <- paste0(if (weighted) "weighted/" else "unweighted/", method)
      run <- collect_partials(
        weighted = weighted,
        display = list(method = method, pov_line = 2.5, bandwidth_p0 = 0.05)
      )
      expect_length(run$partials, 3L)
      for (p in run$partials) {
        expect_identical(p$display$method, method, info = info)
        expect_identical(
          p$display$weight_key, if (weighted) "weighted" else "unweighted",
          info = info
        )
        shown <- module_displayed_table(
          run$result, method, 2.5, 0.05, which = p$label
        )
        expect_gt(nrow(p$table), 0L)
        expect_true(all(is.finite(p$table$value)), info = info)
        expect_false(is.null(p$table$F_agg_all[[1L]]), info = info)
        expect_identical(p$table, shown, info = paste(info, p$label))
      }
    }
  }
})

# Same as module_displayed_table() for several methods in one session:
# returns list(method = list(label = table)) in committed mode.
module_displayed_tables <- function(result, methods, pl_in, bw_in, labels) {
  out <- list()
  suppressWarnings(shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id = "results",
      hist_sim = shiny::reactiveVal(result$hist_sim_result),
      saved_scenarios = shiny::reactiveVal(result$new_scenarios),
      selected_hist = shiny::reactiveVal(NULL),
      tabset_id = "step2_output_tabs",
      residuals = shiny::reactive(result$hist_sim_result$residuals),
      skip_coef_draws = shiny::reactive(FALSE)
    ),
    {
      for (m in methods) {
        session$setInputs(
          cmp_agg_method = m, pov_line = pl_in, bandwidth_p0 = bw_in
        )
        session$elapse(500)
        session$flushReact()
        out[[m]] <<- stats::setNames(lapply(labels, function(lbl) {
          if (identical(lbl, "Historical")) {
            hist_agg_rv()[[weight_key()]][[m]]
          } else {
            scenario_agg_rv()[[lbl]][[weight_key()]][[m]]
          }
        }), labels)
      }
    }
  ))
  out
}

test_that("every streamed method's table is identical to the module's", {
  skip_if_not_installed("shiny")
  for (weighted in c(TRUE, FALSE)) {
    for (display_method in c("mean", "prosperity_gap")) {
      info <- paste0(if (weighted) "weighted/" else "unweighted/", display_method)
      run <- collect_partials(
        weighted = weighted,
        display = list(method = display_method, pov_line = 2.5, bandwidth_p0 = 0.05)
      )
      expect_length(run$partials, 3L)
      all_methods <- unname(hist_aggregate_choices("numeric", "welfare"))
      expected <- if (identical(display_method, "prosperity_gap")) {
        all_methods
      } else {
        setdiff(all_methods, "prosperity_gap")
      }
      labels <- vapply(run$partials, `[[`, character(1), "label")
      shown <- module_displayed_tables(run$result, expected, 2.5, 0.05, labels)
      for (p in run$partials) {
        expect_setequal(names(p$tables), expected)
        expect_setequal(p$display$methods, expected)
        expect_identical(p$table, p$tables[[display_method]], info = info)
        for (m in expected) {
          expect_identical(
            p$tables[[m]], shown[[m]][[p$label]],
            info = paste(info, m, p$label)
          )
        }
      }
    }
  }
})

test_that("parity holds with point estimates only and no residuals", {
  skip_if_not_installed("shiny")
  run <- collect_partials(
    display = list(method = "mean"), residuals = "none", skip_coef_draws = TRUE
  )
  expect_false(run$partials[[1L]]$has_draws)
  for (p in run$partials) {
    out <- NULL
    lbl <- p$label
    suppressWarnings(shiny::testServer(
      mod_2_02_results_server,
      args = list(
        id = "results",
        hist_sim = shiny::reactiveVal(run$result$hist_sim_result),
        saved_scenarios = shiny::reactiveVal(run$result$new_scenarios),
        selected_hist = shiny::reactiveVal(NULL),
        tabset_id = "step2_output_tabs",
        residuals = shiny::reactive("none"),
        skip_coef_draws = shiny::reactive(TRUE)
      ),
      {
        session$setInputs(cmp_agg_method = "mean")
        session$elapse(500)
        session$flushReact()
        out <<- if (identical(lbl, "Historical")) {
          hist_agg_rv()[[weight_key()]]$mean
        } else {
          scenario_agg_rv()[[lbl]][[weight_key()]]$mean
        }
      }
    ))
    expect_identical(p$table, out, info = p$label)
  }
})

# ---- suite vs single-method fast path --------------------------------------

test_that("single-method fast path equals the suite table for every method", {
  svy <- make_partial_svy()
  pipe <- make_partial_pipeline_fn(svy)(
    data.frame(timestamp = as.POSIXct("2020-06-01", tz = "UTC"))
  )
  pipe$weather_raw <- NULL
  ctx <- step2_shared_context(
    train_aug = dplyr::mutate(
      svy, .resid = svy$welfare - stats::fitted(stats::lm(welfare ~ temp, svy))
    )[, c("hhid", ".resid")],
    id_col = "hhid", residuals = "original"
  )
  so <- data.frame(name = "welfare", type = "numeric", transform = "log")
  methods <- unname(hist_aggregate_choices("numeric", "welfare"))
  timing <- c(suite = 0, single = 0)
  for (m in methods) {
    t0 <- proc.time()[["elapsed"]]
    a <- step2_display_aggregation(pipe, m, so, "original", FALSE, 3, 0.05,
      ctx, historical = TRUE, suite = TRUE)
    t1 <- proc.time()[["elapsed"]]
    b <- step2_display_aggregation(pipe, m, so, "original", FALSE, 3, 0.05,
      ctx, historical = TRUE, suite = FALSE)
    t2 <- proc.time()[["elapsed"]]
    timing <- timing + c(t1 - t0, t2 - t1)
    expect_identical(a$table, b$table, info = m)
  }
  message(sprintf("suite %.2fs vs single %.2fs (all methods, fixture)",
                  timing[["suite"]], timing[["single"]]))
})
