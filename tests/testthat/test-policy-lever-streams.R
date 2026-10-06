library(testthat)

# R2-BUG-12: each policy lever draws from its own RNG stream.

make_stream_fixture <- function(n = 2000L, seed = 3L) {
  set.seed(seed)
  data.frame(
    welfare = stats::rlnorm(n, log(3), 0.5),
    weight = stats::runif(n, 0.5, 2),
    hhsize = sample(1:6, n, replace = TRUE),
    electricity = stats::rbinom(n, 1, 0.4),
    internet = stats::rbinom(n, 1, 0.3)
  )
}

sp_cfg <- list(
  budget_mode = "transfer_first", transfer_amount_usd = 100,
  transfer_n_payments = 1, targeting = "exante_poor",
  targeting_threshold = 30, inclusion_error_pct = 10,
  exclusion_error_pct = 10
)

test_that("SP recipients do not depend on unrelated levers", {
  svy <- make_stream_fixture()
  sp_only <- apply_policy_to_svy(svy, sp = sp_cfg, seed = 7L)
  with_infra <- apply_policy_to_svy(
    svy, sp = sp_cfg, seed = 7L,
    infra = list(elec_access_change_pct = 30)
  )
  expect_false(identical(with_infra$electricity, svy$electricity))
  expect_identical(with_infra[[SP_TRANSFER_COL]], sp_only[[SP_TRANSFER_COL]])
})

test_that("one lever's draws do not depend on another lever", {
  svy <- make_stream_fixture()
  elec_only <- apply_policy_to_svy(
    svy, infra = list(elec_access_change_pct = 30), seed = 7L
  )
  with_digital <- apply_policy_to_svy(
    svy, infra = list(elec_access_change_pct = 30),
    digital = list(internet_access_change_pct = 20), seed = 7L
  )
  expect_identical(with_digital$electricity, elec_only$electricity)
})

test_that("SP preview matches the run when other levers are enabled", {
  svy <- make_stream_fixture()
  run <- apply_policy_to_svy(
    svy, sp = sp_cfg, seed = 7L,
    infra = list(elec_access_change_pct = 30)
  )
  preview <- .sp_scenario_reach(svy, sp_cfg, analysis_unit = "hh", seed = 7L)
  expect_identical(preview$n_rows, sum(run[[SP_TRANSFER_COL]] > 0))
  expect_equal(
    preview$transfer_total,
    .sp_transfer_totals(run, "hh")$total
  )
})
