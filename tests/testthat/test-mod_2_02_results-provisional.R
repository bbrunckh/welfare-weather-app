# ============================================================================ #
# tests/testthat/test-mod_2_02_results-provisional.R                           #
# Phase 5 of review/step2_progressive_results_plan.md: the Results module reads #
# every display consumer through results_source(), which is either the         #
# committed run or a provisional live run built from worker partials           #
# (review/step2_live_run_contract.md).                                         #
# ============================================================================ #

library(testthat)
library(shiny)

# ---- fixtures (minimal copy of the builders in test-step2-partials.R) -------

.pv_weather <- function() {
  wh <- data.frame(
    code = "TST", year = 2030L, survname = "SRV", loc_id = "loc01",
    temp = 1, timestamp = as.POSIXct("2030-06-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  hist_wh <- wh
  hist_wh$year <- 2020L
  hist_wh$timestamp <- as.POSIXct("2020-06-01", tz = "UTC")
  list(
    historical = hist_wh,
    "ssp2_4_5_2030_2040_ensemble_mean" = wh,
    "ssp2_4_5_2030_2040_ensemble_hi" = wh,
    "ssp5_8_5_2030_2040_ensemble_mean" = wh
  )
}

.pv_svy <- function(n = 40L) {
  set.seed(11)
  data.frame(
    hhid = seq_len(n), year = 2020L, code = "TST", survname = "SRV",
    loc_id = sprintf("loc%02d", seq_len(n)),
    welfare = stats::rnorm(n, 1, 0.5), temp = stats::rnorm(n),
    weight = rep(c(1, 2, 3, 4), length.out = n),
    stringsAsFactors = FALSE
  )
}

.pv_pipeline_fn <- function(svy) {
  n <- nrow(svy)
  calls <- 0L
  function(weather_raw, ...) {
    calls <<- calls + 1L
    is_hist <- as.integer(format(weather_raw$timestamp[[1L]], "%Y")) == 2020L
    yrs <- if (is_hist) 2020:2021 else 2030:2031
    set.seed(100L + calls)
    list(
      sim_year = rep(yrs, each = n),
      y_point = stats::rnorm(2L * n, 1 + 0.1 * calls, 0.5),
      F_loading = matrix(stats::rnorm(4L * n, 0, 0.05), ncol = 2L),
      id_vec = rep(svy$hhid, 2L),
      weather_raw = weather_raw,
      weight = rep(svy$weight, 2L)
    )
  }
}

.pv_run <- function() {
  svy <- .pv_svy()
  got <- list()
  res <- suppressWarnings(suppressMessages(fct_run_simulation(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
                    label = "Welfare", stringsAsFactors = FALSE),
    svy = svy, ss = NULL,
    mf = list(fit3 = fixest::feols(welfare ~ temp, data = svy),
              engine = "fixest", train_data = svy, weather_terms = "temp"),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")),
    ssps = c("ssp2_4_5", "ssp5_8_5"),
    residuals = "original", skip_coef_draws = FALSE,
    sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL, stored_breaks = NULL,
    weather_fn = function(...) .pv_weather(),
    pipeline_fn = .pv_pipeline_fn(svy),
    partial_fn = function(p) got[[length(got) + 1L]] <<- p,
    display = list(method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
  )))
  list(partials = got, result = res)
}

# live_run value per the contract with the first `n_landed` partials landed.
.pv_live <- function(run, n_landed, generation = 1L) {
  p <- run$partials
  scen <- p[-1L]
  labels <- vapply(scen, `[[`, character(1), "label")
  landed <- scen[seq_len(max(0L, n_landed - 1L))]
  list(
    generation = generation, status = "running",
    groups_total = length(scen), groups_done = length(landed),
    scenario_labels = labels,
    display = list(method = "mean", pov_line = 3, bandwidth_p0 = 0.05),
    partials = list(
      historical = if (n_landed >= 1L) p[[1L]] else NULL,
      scenarios = stats::setNames(
        landed, vapply(landed, `[[`, character(1), "label")
      )
    )
  )
}

.pv_args <- function(live_run, hist_sim = reactiveVal(NULL),
                     saved = reactiveVal(list()), ...) {
  list(
    id = "results", hist_sim = hist_sim, saved_scenarios = saved,
    selected_hist = reactiveVal(NULL), tabset_id = "step2_output_tabs",
    live_run = live_run, ...
  )
}

.pv_settle <- function(session) {
  session$elapse(500)
  session$flushReact()
}

run <- .pv_run()

test_that("the partial-results fixture produces three partials", {
  expect_length(run$partials, 3L)
})

frame_tables <- function(frame) {
  lapply(frame$.entries, function(e) e$table)
}

# ---- tests ------------------------------------------------------------------

test_that("provisional mode shows historical and landed scenarios only", {
  live <- reactiveVal(.pv_live(run, 2L))
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)

    src <- results_source()
    expect_identical(src$mode, "provisional")
    expect_identical(src$scenario_names, "SSP2-4.5 / 2030-2040")
    expect_identical(src$pending_names, "SSP5-8.5 / 2030-2040")
    # Every streamed method is available; prosperity_gap is not (not captured).
    expect_true(all(c("mean", "median", "gini") %in% src$methods_available))
    expect_false("prosperity_gap" %in% src$methods_available)
    expect_identical(selected_scenario_names(), "SSP2-4.5 / 2030-2040")
    expect_identical(weight_key(), "weighted")
    expect_true(has_draws())

    frame <- derived_results_frame_rv()
    expect_setequal(names(frame$.entries), c("Historical", "SSP2-4.5 / 2030-2040"))
    expect_identical(frame$.entries$Historical$table, run$partials[[1L]]$table)

    # Display consumers resolve from the partials.
    expect_gt(nrow(pointrange_bands_rv()), 0L)
    expect_gt(nrow(annual_distribution_curves_rv()), 0L)
    expect_gt(nrow(exceedance_curves_rv()), 0L)
    expect_false(is.null(variance_breakdown_rv()))
    expect_false(is.null(threshold_table_rv()))
    expect_false(is.null(headline_cards_data_rv()))
    expect_match(output$provisional_banner$html, "1 of 2 scenarios ready", fixed = TRUE)

    # A second partial lands: the source grows, nothing else changes.
    live(.pv_live(run, 3L))
    .pv_settle(session)
    expect_identical(
      results_source()$scenario_names,
      c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
    )
    expect_length(results_source()$pending_names, 0L)
    expect_length(derived_results_frame_rv()$.entries, 3L)
  }))
})

test_that("provisional frame tables equal the committed ones after adoption", {
  live <- reactiveVal(.pv_live(run, 3L))
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved,
    residuals = reactive(run$result$hist_sim_result$residuals)), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    prov <- frame_tables(derived_results_frame_rv())
    expect_identical(results_source()$mode, "provisional")

    # Adoption: committed state set, live run cleared.
    hist_sim(run$result$hist_sim_result)
    saved(run$result$new_scenarios)
    live(NULL)
    .pv_settle(session)
    expect_identical(results_source()$mode, "committed")
    comm <- frame_tables(derived_results_frame_rv())
    expect_identical(prov, comm)
    expect_identical(results_tab_added(), TRUE)
  }))
})

test_that("provisional poverty line and bandwidth ignore the controls; method follows them", {
  live <- reactiveVal(.pv_live(run, 2L))
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(
      cmp_agg_method = "gini", pov_line = 9, bandwidth_p0 = 0.5
    )
    .pv_settle(session)
    # Intentional change: the method follows the control among streamed ones.
    expect_identical(.selected_method(), "gini")
    expect_identical(pov_line_val(), 3)
    expect_identical(bandwidth_p0(), 0.05)
    expect_identical(
      derived_results_frame_rv()$.entries$Historical$table,
      run$partials[[1L]]$tables$gini
    )
    # An unavailable method falls back to the captured one.
    session$setInputs(cmp_agg_method = "prosperity_gap")
    .pv_settle(session)
    expect_identical(.selected_method(), "mean")
    expect_identical(
      derived_results_frame_rv()$.entries$Historical$table,
      run$partials[[1L]]$table
    )
  }))
})

test_that("method switching in provisional mode equals committed after adoption", {
  live <- reactiveVal(.pv_live(run, 3L))
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved,
    residuals = reactive(run$result$hist_sim_result$residuals)), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    methods <- c("median", "gini", "headcount_ratio")
    prov <- list()
    for (m in methods) {
      session$setInputs(cmp_agg_method = m)
      .pv_settle(session)
      expect_identical(.selected_method(), m)
      prov[[m]] <- frame_tables(derived_results_frame_rv())
      expect_setequal(
        names(prov[[m]]),
        c("Historical", run$partials[[2L]]$label, run$partials[[3L]]$label)
      )
    }
    expect_false(identical(prov$median, prov$gini))

    hist_sim(run$result$hist_sim_result)
    saved(run$result$new_scenarios)
    live(NULL)
    .pv_settle(session)
    expect_identical(results_source()$mode, "committed")
    for (m in methods) {
      session$setInputs(cmp_agg_method = m)
      .pv_settle(session)
      expect_identical(frame_tables(derived_results_frame_rv()), prov[[m]])
    }
  }))
})

test_that("committed-only outputs are suppressed while provisional", {
  live <- reactiveVal(.pv_live(run, 3L))
  hist_sim <- reactiveVal(run$result$hist_sim_result)
  saved <- reactiveVal(run$result$new_scenarios)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    expect_identical(results_source()$mode, "provisional")
    expect_error(incidence_data_rv(), class = "shiny.silent.error")
    # Never provisional data: nothing committed was displayed yet -> NULL.
    expect_null(session$returned$timeseries_curves())

    live(NULL)
    .pv_settle(session)
    expect_identical(results_source()$mode, "committed")
    expect_false(is.null(session$returned$timeseries_curves()))
  }))
})

test_that("display settings are written in committed mode only", {
  live <- reactiveVal(.pv_live(run, 2L))
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  out <- reactiveVal(NULL)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved,
    display_settings_out = out), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 2, bandwidth_p0 = 0.1)
    .pv_settle(session)
    expect_null(out())

    hist_sim(run$result$hist_sim_result)
    saved(run$result$new_scenarios)
    live(NULL)
    .pv_settle(session)
    expect_identical(
      out(), list(method = "mean", pov_line = 2, bandwidth_p0 = 0.1)
    )
  }))
})

test_that("cancelled live run reverts to the committed run or removes the tab", {
  # With a committed run present (committed first, then a new run streams).
  live <- reactiveVal(NULL)
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    hist_sim(run$result$hist_sim_result)
    saved(run$result$new_scenarios)
    .pv_settle(session)
    expect_true(results_tab_added())
    live(.pv_live(run, 2L))
    .pv_settle(session)
    expect_identical(results_source()$mode, "provisional")
    expect_true(results_tab_added())
    live(NULL)
    .pv_settle(session)
    expect_true(results_tab_added())
    expect_identical(results_source()$mode, "committed")
    expect_setequal(
      names(derived_results_frame_rv()$.entries),
      c("Historical", names(run$result$new_scenarios))
    )
  }))

  # Without a committed run: tab appears with the historical partial, then goes.
  live <- reactiveVal(NULL)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    .pv_settle(session)
    expect_false(results_tab_added())
    live(.pv_live(run, 1L))
    .pv_settle(session)
    expect_true(results_tab_added())
    expect_identical(results_source()$mode, "provisional")
    expect_identical(selected_scenario_names(), character(0))
    live(NULL)
    .pv_settle(session)
    expect_false(results_tab_added())
    expect_null(results_source())
  }))
})

# ---- Phase 7: committed cache seeded from adopted partials -------------------

.pv_adopted <- function(sig = list(run = "a"), drop_scenarios = character(0),
                        edit = identity) {
  p <- .pv_live(run, 3L)$partials
  p$scenarios <- p$scenarios[setdiff(names(p$scenarios), drop_scenarios)]
  p <- edit(p)
  list(dependency_signature = sig, partials = p)
}

.pv_committed_hist <- function(sig = list(run = "a")) {
  hs <- run$result$hist_sim_result
  hs$.sig <- sig
  hs
}

# Runs a committed render and returns the displayed tables plus the number of
# aggregation calls made after adoption (the eager observer runs inside it).
.pv_committed_render <- function(adopted, skip = FALSE, sig = list(run = "a"),
                                 switch_methods = character(0)) {
  calls <- 0L
  orig_multi <- aggregate_pipeline_tables_multi
  orig_one <- aggregate_pipeline_table
  testthat::local_mocked_bindings(
    aggregate_pipeline_tables_multi = function(...) {
      calls <<- calls + 1L
      orig_multi(...)
    },
    aggregate_pipeline_table = function(...) {
      calls <<- calls + 1L
      orig_one(...)
    }
  )
  out <- list()
  live <- reactiveVal(.pv_live(run, 3L))
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  ap <- reactiveVal(NULL)
  suppressWarnings(testServer(
    mod_2_02_results_server,
    args = .pv_args(live, hist_sim, saved,
      residuals = reactive(run$result$hist_sim_result$residuals),
      skip_coef_draws = reactive(skip),
      adopted_partials = ap
    ), {
      session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
      .pv_settle(session)
      calls <<- 0L
      # Adoption commit: all three set before the next flush.
      hist_sim(.pv_committed_hist(sig))
      saved(run$result$new_scenarios)
      ap(adopted)
      live(NULL)
      session$flushReact()
      agg_workspace()
      .pv_settle(session)
      out$tables <<- frame_tables(derived_results_frame_rv())
      out$switched <<- list()
      for (m in switch_methods) {
        session$setInputs(cmp_agg_method = m)
        .pv_settle(session)
        out$switched[[m]] <<- frame_tables(derived_results_frame_rv())
      }
      out$cache <<- aggregation_cache()
    }
  ))
  out$calls <- calls
  out
}

test_that("adopted partials seed the committed cache: no re-aggregation, same tables", {
  seeded <- .pv_committed_render(.pv_adopted())
  unseeded <- .pv_committed_render(NULL)

  expect_identical(seeded$tables, unseeded$tables)
  expect_gt(unseeded$calls, 0L)
  expect_identical(seeded$calls, 0L)
  expect_gte(seeded$cache$hits, 2L)
})

test_that("all streamed methods are seeded: switching methods after adoption re-aggregates nothing", {
  methods <- c("median", "gini", "headcount_ratio", "gap", "total")
  seeded <- .pv_committed_render(.pv_adopted(), switch_methods = methods)
  unseeded <- .pv_committed_render(NULL, switch_methods = methods)
  expect_identical(seeded$switched, unseeded$switched)
  expect_identical(seeded$tables, unseeded$tables)
  expect_gt(unseeded$calls, 0L)
  expect_identical(seeded$calls, 0L)
  # The seeded methods fit: nothing was evicted by the 8-entry default.
  expect_length(seeded$cache$evictions, 0L)
  expect_gt(seeded$cache$max_entries, 8L)
})

test_that("seeding is skipped on any mismatch and results stay correct", {
  ref <- .pv_committed_render(NULL)

  # Different dependency signature.
  r <- .pv_committed_render(.pv_adopted(sig = list(run = "other")))
  expect_gt(r$calls, 0L)
  expect_identical(r$tables, ref$tables)

  # skip_coef differs from the one the partial was computed with.
  r <- .pv_committed_render(.pv_adopted(), skip = TRUE)
  expect_gt(r$calls, 0L)

  # residuals differ.
  r <- .pv_committed_render(.pv_adopted(edit = function(p) {
    p$historical$display$residuals <- "normal"
    p
  }))
  expect_gt(r$calls, 0L)
  expect_identical(r$tables, ref$tables)

  # method differs from the displayed (mean) one: "mean" is aggregated normally.
  r <- .pv_committed_render(.pv_adopted(edit = function(p) {
    p$historical$display$method <- "gini"
    p
  }))
  expect_gt(r$calls, 0L)
  expect_identical(r$tables, ref$tables)
})

test_that("a scenario without a partial stays lazy inside the seeded value", {
  missing_lbl <- "SSP5-8.5 / 2030-2040"
  r <- .pv_committed_render(.pv_adopted(drop_scenarios = missing_lbl))
  ref <- .pv_committed_render(NULL)
  expect_identical(r$tables, ref$tables)
  # Only the missing scenario's suite was aggregated.
  expect_identical(r$calls, 1L)
})


# ---- Phase 6: provisional UI -------------------------------------------------

test_that("Diagnostics timeseries keeps the last committed value while streaming", {
  live <- reactiveVal(NULL)
  hist_sim <- reactiveVal(run$result$hist_sim_result)
  saved <- reactiveVal(run$result$new_scenarios)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    committed <- session$returned$timeseries_curves()
    expect_false(is.null(committed$tbl))

    live(.pv_live(run, 2L))
    .pv_settle(session)
    expect_identical(results_source()$mode, "provisional")
    expect_identical(session$returned$timeseries_curves(), committed)
    live(.pv_live(run, 3L))
    .pv_settle(session)
    expect_identical(session$returned$timeseries_curves(), committed)

    live(NULL)
    .pv_settle(session)
    expect_identical(session$returned$timeseries_curves(), committed)
  }))
})

test_that("exports are skipped with a note while a run streams", {
  item <- list(
    key = "k", step = 2L, kind = "table", stale = NULL,
    fun = function() {
      stop(structure(
        class = c("wise_export_skip", "error", "condition"),
        list(message = "run in progress", call = NULL)
      ))
    }
  )
  res <- .export_write_item(item, tempdir(), "x.csv")
  expect_identical(res$status, "skipped")
  expect_match(res$note, "run in progress")

  live <- reactiveVal(.pv_live(run, 2L))
  hist_sim <- reactiveVal(run$result$hist_sim_result)
  saved <- reactiveVal(run$result$new_scenarios)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    items <- wise_export_items(session)
    expect_gt(length(items), 0L)
    err <- tryCatch(items[[1L]]$fun(), error = function(e) e)
    expect_s3_class(err, "wise_export_skip")
    live(NULL)
    .pv_settle(session)
    expect_false(inherits(
      tryCatch(items[[1L]]$fun(), error = function(e) e), "wise_export_skip"
    ))
  }))
})

test_that("controls are disabled when provisional and re-enabled on every exit", {
  pill <- list()
  inp <- list()
  local_mocked_bindings(
    update_pill_toggle_disabled = function(session, inputId, disabled = FALSE, tooltip = NULL) {
      pill[[length(pill) + 1L]] <<- list(id = inputId, disabled = disabled, tooltip = tooltip)
    },
    update_input_disabled = function(session, inputId, disabled = FALSE, tooltip = NULL) {
      inp[[length(inp) + 1L]] <<- list(id = inputId, disabled = disabled, tooltip = tooltip)
    }
  )
  selected <- character(0)
  local_mocked_bindings(
    updateRadioButtons = function(session, inputId, ..., selected = NULL) {
      selected <<- c(selected, paste(inputId, selected))
    },
    .package = "shiny"
  )
  last <- function(x) x[[length(x)]]
  live <- reactiveVal(NULL)
  hist_sim <- reactiveVal(NULL)
  saved <- reactiveVal(list())
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    hist_sim(run$result$hist_sim_result)
    saved(run$result$new_scenarios)
    .pv_settle(session)
    expect_true(results_tab_added())
    # Committed: enabled.
    expect_false(last(pill)$disabled)
    expect_false(last(inp)$disabled)

    # Enter provisional.
    n_selected <- length(selected)
    live(.pv_live(run, 2L))
    .pv_settle(session)
    expect_identical(last(pill)$id, "cmp_agg_method")
    expect_false("mean" %in% last(pill)$disabled)
    # Only the method that was not streamed is disabled.
    expect_identical(as.character(last(pill)$disabled), "prosperity_gap")
    expect_identical(last(pill)$tooltip, "Available when the simulation completes")
    expect_identical(last(inp)$id, "pov_line")
    expect_true(last(inp)$disabled)
    # The current selection is available, so it is not forced to the captured one.
    expect_length(selected, n_selected)

    # Each exit path ends provisional mode and re-enables: adoption/cancel/
    # stale/failure all clear live_run, which is what the gate keys on.
    for (i in 1:2) {
      live(NULL)
      .pv_settle(session)
      expect_false(last(pill)$disabled)
      expect_false(last(inp)$disabled)
      live(.pv_live(run, 3L))
      .pv_settle(session)
      expect_true(last(inp)$disabled)
    }
  }))
})

test_that("controls are re-sent when the pane is rebuilt while provisional", {
  n_calls <- 0L
  local_mocked_bindings(
    update_pill_toggle_disabled = function(...) n_calls <<- n_calls + 1L,
    update_input_disabled = function(...) invisible(NULL)
  )
  live <- reactiveVal(NULL)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    live(.pv_live(run, 2L))
    .pv_settle(session)
    before <- n_calls
    expect_gt(before, 0L)
    # A new run with a different outcome rebuilds the pane (insertUI).
    lv <- .pv_live(run, 2L, generation = 2L)
    lv$partials$historical$so$name <- "other"
    live(lv)
    .pv_settle(session)
    expect_gt(n_calls, before)
  }))
})

test_that("banner shows progress, the unlock note, and hides when committed", {
  live <- reactiveVal(.pv_live(run, 2L))
  hist_sim <- reactiveVal(run$result$hist_sim_result)
  saved <- reactiveVal(run$result$new_scenarios)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live, hist_sim, saved), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    html <- output$provisional_banner$html
    expect_match(html, "1 of 2 scenarios ready", fixed = TRUE)
    expect_match(html, "progress-bar", fixed = TRUE)
    expect_match(html, "The poverty line unlocks when the run finishes.", fixed = TRUE)
    expect_match(html, "Prosperity gap is also unavailable", fixed = TRUE)
    expect_match(html, "Step 3 uses the last completed run", fixed = TRUE)
    expect_match(output$headline_cards_ui$html, "Based on 1 of 2 scenarios", fixed = TRUE)

    live(NULL)
    .pv_settle(session)
    expect_null(output$provisional_banner$html)
    expect_no_match(output$headline_cards_ui$html, "Based on", fixed = TRUE)
  }))
})

test_that("threshold table gets placeholder rows for pending scenarios", {
  df <- data.frame(
    `Scenario / Period` = c("Historical", "SSP2-4.5 / 2030-2040"),
    Estimate = c("Central (P50)", "Central (P50)"),
    Expected = c(1.5, 1.6), check.names = FALSE
  )
  out <- .append_pending_threshold_rows(df, c("SSP5-8.5 / 2030-2040", "SSP2-4.5 / 2030-2040"))
  expect_identical(nrow(out), 3L)
  expect_identical(out[["Scenario / Period"]][[3L]], "SSP5-8.5 / 2030-2040")
  expect_identical(out$Estimate[[3L]], "computing...")
  expect_true(is.na(out$Expected[[3L]]))
  expect_identical(.append_pending_threshold_rows(df, character(0)), df)

  live <- reactiveVal(.pv_live(run, 2L))
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    expect_match(jsonlite::toJSON(output$summary_threshold_table, auto_unbox = TRUE),
      "computing...", fixed = TRUE)
  }))
})

.pc_annual_tbl <- function(scens) {
  do.call(rbind, lapply(scens, function(s) {
    data.frame(
      scenario = s, model_id = paste0("m", 1:3), sim_year = rep(2020:2022, 3),
      value = seq(5, 6, length.out = 9) + match(s, scens),
      is_historical = s == "Historical", stringsAsFactors = FALSE
    )
  }))
}

test_that("annual distribution pending categories are fixed, labelled and empty", {
  scens <- c("Historical", "SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
  full <- echart_annual_distribution(.pc_annual_tbl(scens), "Mean")
  part <- echart_annual_distribution(
    .pc_annual_tbl(scens[1:2]), "Mean", pending = scens[3]
  )
  mean_y <- function(ch) {
    s <- Filter(function(x) identical(x$name, "Mean All"), ch$x$opts$series)[[1L]]
    stats::setNames(
      vapply(s$data, function(d) d$value[[2L]], numeric(1)),
      vapply(s$data, function(d) d$scenario, character(1))
    )
  }
  # Landed rows keep the position they will have once everything has landed.
  expect_identical(mean_y(part), mean_y(full)[scens[1:2]])
  expect_identical(part$x$opts$yAxis$max, full$x$opts$yAxis$max)
  fmt <- as.character(part$x$opts$yAxis$axisLabel$formatter)
  expect_match(fmt, "(computing)", fixed = TRUE)
  expect_match(fmt, "SSP5-8.5", fixed = TRUE)
  expect_false(is.null(part$x$opts$yAxis$axisLabel$rich$pending))
  # No data for the pending scenario.
  draws <- Filter(function(x) identical(x$name, "Draws"), part$x$opts$series)[[1L]]
  expect_false(any(vapply(draws$data, function(d) d$scenario == scens[3], logical(1))))
  # Pending label for an already-landed scenario is ignored.
  expect_identical(
    echart_annual_distribution(.pc_annual_tbl(scens), "Mean", pending = scens[3])$x$opts,
    full$x$opts
  )
})

test_that("Step 3 annual distribution output is unchanged by the default pending", {
  tbl <- .pc_annual_tbl(c("Historical", "SSP2-4.5 / 2030-2040"))
  tbl$source <- rep(c("Baseline", "Policy"), length.out = nrow(tbl))
  expect_identical(
    echart_step3_annual_distribution(tbl, "x", "violin")$x$opts,
    echart_step3_annual_distribution(tbl, "x", "violin", pending = character(0))$x$opts
  )
  expect_identical(
    echart_step3_annual_distribution(tbl, "x", "boxplot")$x$opts,
    echart_annual_distribution(tbl, "x", "boxplot")$x$opts
  )
})

test_that("adverse dot slots stay fixed as pending scenarios land", {
  scens <- c("SSP2-4.5 / 2030-2040", "SSP3-7.0 / 2030-2040", "SSP5-8.5 / 2030-2040")
  rp_lv <- rev(c("Expected", "Adverse 1-in-5"))
  mk <- function(s) {
    data.frame(
      scenario = s, Estimate = "Central (P50)",
      rp_label = factor(rp_lv, levels = rp_lv),
      value = c(12, 13) + match(s, scens), is_historical = FALSE,
      intermod_lo = 11, intermod_hi = 15, stringsAsFactors = FALSE
    )
  }
  mk_hist <- function() {
    h <- mk(scens[1L])
    h$scenario <- "Historical"
    h$is_historical <- TRUE
    h
  }
  tbl_of <- function(ss) rbind(mk_hist(), do.call(rbind, lapply(ss, mk)))
  ys <- function(ch) {
    dots <- Filter(function(s) identical(s$type, "scatter") && length(s$data) == 2L &&
      !grepl("computing", s$name), ch$x$opts$series)
    stats::setNames(lapply(dots, function(s) {
      vapply(s$data, function(d) d$value[[2L]], numeric(1))
    }), vapply(dots, `[[`, character(1), "name"))
  }
  full <- echart_step2_adverse_dot(tbl_of(scens), "x", pending = character(0))
  one <- echart_step2_adverse_dot(tbl_of(scens[1]), "x", pending = scens[2:3])
  two <- echart_step2_adverse_dot(tbl_of(scens[1:2]), "x", pending = scens[3])
  # The first scenario's slot never moves while the others are pending.
  expect_identical(ys(one)[[2L]], ys(two)[[2L]])
  expect_identical(ys(two)[[2L]], ys(full)[[2L]])
  expect_identical(ys(two)[[3L]], ys(full)[[3L]])
  # Pending scenarios appear as muted "(computing)" labels with no points.
  labs <- unlist(lapply(one$x$opts$series, function(s) {
    vapply(s$data %||% list(), function(d) {
      if (is.list(d)) d$label$formatter %||% NA_character_ else NA_character_
    }, character(1))
  }))
  expect_true(all(paste0(scens[2:3], " (computing)") %in% labs))
  # Default pending leaves the original dodge untouched.
  expect_identical(
    echart_step2_adverse_dot(tbl_of(scens), "x")$x$opts, full$x$opts
  )
})

test_that("pill_toggle renders disabled choices and the server helpers send messages", {
  html <- as.character(pill_toggle(
    "m", c(One = "1", Two = "2", Three = "3"), selected = "1",
    disabled = c("1", "2"), disabled_tooltip = "later"
  ))
  # The selected pill is never disabled; the others carry the class + tooltip.
  expect_equal(lengths(regmatches(html, gregexpr("pill-disabled", html))), 1L)
  expect_match(html, 'title="later"', fixed = TRUE)
  expect_match(html, 'value="2" disabled="disabled"', fixed = TRUE)
  expect_no_match(as.character(pill_toggle("m", c(a = "a", b = "b"))), "disabled", fixed = TRUE)

  msgs <- list()
  fake <- list(
    ns = function(x) paste0("mod-", x),
    sendCustomMessage = function(type, message) {
      msgs[[length(msgs) + 1L]] <<- list(type = type, message = message)
    }
  )
  update_pill_toggle_disabled(fake, "pills", c("b", "c"), tooltip = "tip")
  update_pill_toggle_disabled(fake, "pills", FALSE)
  update_input_disabled(fake, "num", TRUE, tooltip = "tip")
  expect_identical(msgs[[1L]]$type, "wise_set_disabled")
  expect_identical(msgs[[1L]]$message$id, "mod-pills")
  expect_identical(as.character(msgs[[1L]]$message$values), c("b", "c"))
  expect_false(msgs[[2L]]$message$all)
  expect_length(msgs[[2L]]$message$values, 0L)
  expect_identical(msgs[[3L]]$message$kind, "input")
  expect_true(msgs[[3L]]$message$all)
})

test_that("progress ticks do not invalidate the display source; partials do", {
  live <- reactiveVal(.pv_live(run, 2L))
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    src <- results_source()
    frame <- derived_results_frame_rv()

    # A progress-only update (elapsed, member counters) must not recompute
    # the source: its closures would otherwise be recreated, breaking identity.
    tick <- live()
    tick$elapsed <- 42
    tick$member_index <- 3L
    tick$period_members <- 16L
    tick$current_label <- "SSP5-8.5 / 2030-2040"
    live(tick)
    .pv_settle(session)
    expect_identical(results_source(), src)
    expect_identical(derived_results_frame_rv(), frame)

    # A landed partial does recompute.
    live(.pv_live(run, 3L))
    .pv_settle(session)
    expect_false(identical(results_source(), src))
  }))
})

test_that("streaming renders keep the chart instance only for data-only updates", {
  live <- reactiveVal(.pv_live(run, 2L))
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none",
      pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    ch <- echarts4r::e_charts(data.frame(x = 1:2, y = 1:2), x)
    # First render of a run: full redraw.
    expect_true(.smooth_echart(ch, "t")$x$dispose)
    # Same run, same controls, still pending: keep the instance.
    expect_false(.smooth_echart(ch, "t")$x$dispose)
    # A display control change forces a redraw.
    session$setInputs(cmp_deviation = "mean")
    expect_true(.smooth_echart(ch, "t")$x$dispose)
    expect_false(.smooth_echart(ch, "t")$x$dispose)
    # The last pending scenario landing forces a redraw (series removal).
    live(.pv_live(run, 3L))
    .pv_settle(session)
    expect_true(.smooth_echart(ch, "t")$x$dispose)
    # Committed mode always redraws.
    live(NULL)
    .pv_settle(session)
    expect_true(.smooth_echart(ch, "t")$x$dispose)
  }))
})

test_that("historical-only live run renders charts when scenarios is an unnamed empty list", {
  # mod_2_01 starts a run with partials$scenarios = list() (NULL names); the
  # source must still report zero landed scenarios, not "no scenario data".
  lr <- .pv_live(run, 1L)
  lr$partials$scenarios <- list()
  live <- reactiveVal(lr)
  suppressWarnings(testServer(mod_2_02_results_server, args = .pv_args(live), {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none",
      pov_line = 3, bandwidth_p0 = 0.05)
    .pv_settle(session)
    expect_identical(results_source()$scenario_names, character(0))
    expect_length(results_source()$pending_names, 2L)
    expect_false(is.null(derived_results_frame_rv()))
    expect_gt(nrow(annual_distribution_curves_rv()), 0L)
    expect_false(is.null(annual_distribution_chart()))
    expect_false(is.null(adverse_dot_chart()))
    expect_false(is.null(exceedance_chart()))
  }))
})
