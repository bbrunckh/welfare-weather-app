# ============================================================================ #
# tests/testthat/test-mod_2_02_results-characterization.R                      #
# Characterisation snapshots for the committed-mode Results reactives. They    #
# pin the pre-Phase-5 behaviour (review/step2_progressive_results_plan.md,     #
# section 6) so the results_source() refactor is provably value-neutral.       #
#                                                                              #
# Robustness: each reactive value is normalised to plain lists (env objects,   #
# attributes, digests and tags dropped), numerics are rounded to 10            #
# significant digits, and the result is stored as JSON (style = "json2") and   #
# compared with a small tolerance. Unlike style = "serialize", this does not   #
# depend on the R serialization version or on floating-point noise in the      #
# last bits, so snapshots survive platform and R upgrades.                     #
# ============================================================================ #

library(testthat)
library(shiny)

.char_pipe <- function(seed, n_year, years, centre, weighted = TRUE) {
  set.seed(seed)
  n <- 6L * n_year
  list(
    sim_year  = rep(years, each = 6L),
    y_point   = log(centre) + rnorm(n, 0, 0.25),
    weight    = if (weighted) rep(c(1, 2, 3), length.out = n) else NULL,
    F_loading = matrix(rnorm(3L * n) * 0.02, nrow = n, ncol = 3L),
    train_aug = NULL, id_vec = NULL, id_col = NULL
  )
}

.char_hist_sim <- function() {
  list(
    so          = list(type = "numeric", name = "welfare", transform = "log"),
    residuals   = "none",
    has_weights = TRUE,
    pipeline    = .char_pipe(11L, 22L, 2000:2021, 4)
  )
}

.char_saved <- function() {
  so <- list(type = "numeric", name = "welfare", transform = "log")
  mk <- function(base_seed, centres) {
    pipes <- lapply(seq_along(centres), function(i) {
      .char_pipe(base_seed + i, 21L, 2030:2050, centres[[i]])
    })
    names(pipes) <- paste0("model_", seq_along(centres))
    list(so = so, pipelines = pipes)
  }
  list(
    "SSP2-4.5 / 2030-2050" = mk(100L, c(3.6, 3.9, 4.2)),
    "SSP5-8.5 / 2030-2050" = mk(200L, c(3.0, 3.3))
  )
}

# Plain-data view of a reactive value: no environments, functions, attributes
# (other than names), or htmltools tags; numerics rounded to 10 sig. digits.
.char_norm <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.environment(x) || is.function(x)) return("<dropped>")
  if (inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list") ||
      inherits(x, "html")) {
    return(as.character(x))
  }
  if (is.data.frame(x)) {
    out <- lapply(as.list(x), .char_norm)
    return(c(list(.nrow = nrow(x)), out))
  }
  if (is.matrix(x)) {
    return(list(dim = dim(x), data = .char_norm(as.vector(x))))
  }
  if (is.factor(x)) return(as.character(x))
  if (is.list(x)) {
    out <- lapply(x, .char_norm)
    if (is.null(names(x))) names(out) <- NULL
    return(out)
  }
  if (is.double(x)) return(signif(as.vector(x), 10))
  if (is.atomic(x)) return(as.vector(x))
  "<unsupported>"
}

# Pretty JSON lines of the normalised value. Numbers are already rounded to 10
# significant digits, so the text is deterministic and diffs line by line.
.char_json <- function(x) {
  strsplit(
    as.character(jsonlite::toJSON(
      .char_norm(x), digits = NA, pretty = TRUE, auto_unbox = TRUE,
      null = "null", na = "string"
    )),
    "\n", fixed = TRUE
  )[[1L]]
}

.char_frame_view <- function(frame) {
  entries <- lapply(frame$.entries, function(e) {
    list(
      scenario = e$scenario,
      is_historical = e$is_historical,
      table = .char_norm(e$table),
      matrix = .char_norm(e$matrix)
    )
  })
  list(
    method = frame$method, deviation = frame$deviation, weight = frame$weight,
    entries = entries
  )
}

.char_snapshot_config <- function(method, deviation, ensemble_band = "none") {
  testServer(
    mod_2_02_results_server,
    args = list(
      id = "results",
      hist_sim = reactiveVal(.char_hist_sim()),
      saved_scenarios = reactiveVal(.char_saved()),
      selected_hist = reactiveVal(NULL),
      tabset_id = "step2_output_tabs"
    ),
    {
      session$setInputs(
        cmp_agg_method = method, cmp_deviation = deviation,
        ensemble_band = ensemble_band, uncertainty_band = "p10_p90",
        pov_line = 3, bandwidth_p0 = 0.05
      )
      session$elapse(500)
      session$flushReact()

      frame <- derived_results_frame_rv()
      expect_identical(frame$method, method)
      expect_setequal(
        names(frame$.entries),
        c("Historical", names(.char_saved()))
      )

      expect_snapshot_value(.char_json(.char_frame_view(frame)), style = "json2")
      expect_snapshot_value(.char_json(pointrange_bands_rv()), style = "json2")
      expect_snapshot_value(.char_json(headline_bands_rv()), style = "json2")
      expect_snapshot_value(.char_json(timeseries_curves_rv()), style = "json2")
      expect_snapshot_value(.char_json(annual_distribution_curves_rv()), style = "json2")
      expect_snapshot_value(.char_json(variance_breakdown_rv()), style = "json2")
      expect_snapshot_value(.char_json(exceedance_curves_rv()), style = "json2")
      expect_snapshot_value(.char_json(threshold_table_rv()), style = "json2")
      expect_snapshot_value(.char_json(headline_cards_data_rv()), style = "json2")
    }
  )
}

test_that("characterisation: mean, outcome level (committed mode)", {
  skip_if_not_installed("shiny")
  .char_snapshot_config("mean", "none")
})

test_that("characterisation: mean, difference from historical mean", {
  skip_if_not_installed("shiny")
  .char_snapshot_config("mean", "mean", ensemble_band = "minmax")
})

test_that("characterisation: headcount ratio, difference from historical mean", {
  skip_if_not_installed("shiny")
  .char_snapshot_config("headcount_ratio", "mean", ensemble_band = "p10_p90")
})
