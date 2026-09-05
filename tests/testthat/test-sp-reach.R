# ============================================================================ #
# tests/testthat/test-sp-reach.R                                               #
# UI-32: pre-run feedback on who a social-protection scenario reaches and what #
#        it costs. The preview must be the number the run produces, not a      #
#        separate approximation of it.                                         #
# ============================================================================ #

library(testthat)

make_svy <- function(n = 600, seed = 7, weights = TRUE) {
  set.seed(seed)
  df <- data.frame(
    welfare = rlnorm(n, 0.6, 0.7),
    hhsize  = sample(1:8, n, replace = TRUE)
  )
  if (weights) df$weight <- runif(n, 50, 150)
  df
}

base_sp <- function(...) {
  utils::modifyList(
    list(budget_mode = "transfer_first", targeting = "exante_poor",
         targeting_threshold = 25, inclusion_error_pct = 15,
         exclusion_error_pct = 5, transfer_amount_usd = 40,
         transfer_n_payments = 6L),
    list(...)
  )
}


test_that("the preview reproduces the run's eligibility and cost exactly", {
  svy <- make_svy()
  sp  <- base_sp()

  preview <- .sp_scenario_reach(svy, sp, "hh")
  mod <- apply_policy_to_svy(svy, sp = sp, analysis_unit = "hh",
                             seed = WISEAPP_DEFAULT_SEED)

  v  <- mod[[SP_TRANSFER_COL]]
  ok <- is.finite(v) & is.finite(svy$weight)

  # Same eligible population...
  expect_equal(preview$n_pop, sum(svy$weight[ok & v > 0]))
  # ...and the same annual cost the diagnostics tab reports.
  expect_equal(preview$transfer_total,
               sum(v[ok] * svy$weight[ok] * svy$hhsize[ok]) * 365)
})

test_that("the preview is stable across calls despite the error draws", {
  svy <- make_svy()
  sp  <- base_sp()
  # Inclusion/exclusion errors are sampled; a preview that flickered on every
  # reactive tick would be unusable.
  expect_identical(.sp_scenario_reach(svy, sp, "hh"),
                   .sp_scenario_reach(svy, sp, "hh"))
})

test_that("universal targeting reaches the whole population", {
  svy <- make_svy()
  r <- .sp_scenario_reach(svy, base_sp(targeting = "universal"), "hh")
  expect_equal(r$n_rows, nrow(svy))
  expect_equal(r$share_pct, 100)
  expect_equal(r$n_pop, sum(svy$weight))
})

test_that("a tighter cutoff reaches fewer units", {
  svy <- make_svy()
  wide   <- .sp_scenario_reach(svy, base_sp(targeting_threshold = 50), "hh")
  narrow <- .sp_scenario_reach(svy, base_sp(targeting_threshold = 10), "hh")
  expect_gt(wide$share_pct, narrow$share_pct)
})

test_that("targeting errors move the eligible count in the expected direction", {
  svy <- make_svy()
  clean <- .sp_scenario_reach(
    svy, base_sp(inclusion_error_pct = 0, exclusion_error_pct = 0), "hh")
  # With no leakage the eligible share is the cutoff itself.
  expect_equal(round(clean$share_pct), 25, tolerance = 3)

  leaky <- .sp_scenario_reach(
    svy, base_sp(inclusion_error_pct = 30, exclusion_error_pct = 0), "hh")
  expect_gt(leaky$n_rows, clean$n_rows)
})

test_that("budget-first mode derives the per-unit transfer from the budget", {
  svy <- make_svy()
  r <- .sp_scenario_reach(
    svy, base_sp(budget_mode = "budget_first", budget_fixed = 1e6), "hh")
  expect_true(r$budget_first)
  expect_equal(r$transfer_total, 1e6)
  # Per-unit is the budget spread over the weighted eligible population.
  expect_equal(r$transfer_per_unit, 1e6 / r$n_pop)
})

test_that("an unweighted survey reports sample counts and says so", {
  svy <- make_svy(weights = FALSE)
  r <- .sp_scenario_reach(svy, base_sp(targeting = "universal"), "hh")
  expect_false(r$weighted)
  expect_equal(r$n_pop, nrow(svy))
})

test_that("a zero transfer costs nothing", {
  svy <- make_svy()
  r <- .sp_scenario_reach(svy, base_sp(transfer_amount_usd = 0), "hh")
  expect_equal(r$transfer_total, 0)
  expect_equal(r$transfer_per_unit, 0)
  # Eligibility is still reported - the targeting is configured, the money is not.
  expect_gt(r$n_rows, 0)
})

test_that("the reach preview degrades instead of erroring", {
  expect_null(.sp_scenario_reach(NULL, base_sp(), "hh"))
  expect_null(.sp_scenario_reach(make_svy(), NULL, "hh"))
  expect_null(.sp_scenario_reach(data.frame(), base_sp(), "hh"))
  # No welfare column: eligibility cannot be determined.
  expect_null(.sp_scenario_reach(data.frame(x = 1:5), base_sp(), "hh"))
})


# ---- Displayed-figure formatting (UI-32) ------------------------------------

test_that("fmt_num rounds to one decimal with thousands separators", {
  expect_equal(fmt_num(1234.567), "1,234.6")
  expect_equal(fmt_num(0), "0.0")
  expect_equal(fmt_num(-2.04), "-2.0")
  # Rounds, does not truncate.
  expect_equal(fmt_num(1.25, digits = 1), "1.2")   # banker's rounding in R
  expect_equal(fmt_num(1.26, digits = 1), "1.3")
})

test_that("fmt_num applies prefix and suffix and handles non-finite input", {
  expect_equal(fmt_num(12345.67, prefix = "$"), "$12,345.7")
  expect_equal(fmt_num(26.19, suffix = "%"), "26.2%")
  expect_equal(fmt_num(c(NA, NaN, Inf)), rep("—", 3))
  expect_equal(fmt_num(NA, na = "n/a"), "n/a")
})

test_that("fmt_count renders whole units with separators", {
  expect_equal(fmt_count(98765.4), "98,765")
  expect_equal(fmt_count(0), "0")
  expect_equal(fmt_count(NA), "—")
})

test_that("formatting is vectorised", {
  expect_equal(fmt_num(c(1.44, 2.55)), c("1.4", "2.5"))
  expect_length(fmt_count(c(1, 2, 3)), 3L)
})


# ---- UI-32 regression: one calculation, one survey frame --------------------
#
# The sidebar preview reported a population-level total well above the
# diagnostics tab's. Two causes, both now closed:
#
#   1. The preview estimated over the full multi-round `survey_weather()`,
#      while the run (and therefore diagnostics) uses `hist_sim()$svy` - the
#      single baseline round Step 2 filtered to. That inflated the recipient
#      count and shifted the welfare quantile defining "ex-ante poor".
#   2. The two re-derived the transfer arithmetic independently.

multiround_svy <- function(n = 500) {
  set.seed(11)
  mk <- function(yr) data.frame(
    year = yr, welfare = rlnorm(n, 0.6, 0.7),
    hhsize = sample(1:8, n, replace = TRUE), weight = runif(n, 50, 150))
  rbind(mk(2010L), mk(2015L))
}

test_that("the preview total equals the diagnostics tab total", {
  full     <- multiround_svy()
  baseline <- full[full$year == 2015L, ]
  sp <- base_sp(targeting_threshold = 20)

  # Diagnostics computes from the applied policy frame.
  pol  <- apply_policy_to_svy(baseline, sp = sp, analysis_unit = "hh")
  diag <- .sp_transfer_totals(pol, "hh")

  # The sidebar preview, given the frame the run uses.
  prev <- .sp_scenario_reach(baseline, sp, "hh")

  expect_equal(prev$transfer_total,    diag$total)
  expect_equal(prev$transfer_per_unit, diag$per_unit)
})

test_that("estimating over every survey round overstates the cost", {
  full     <- multiround_svy()
  baseline <- full[full$year == 2015L, ]
  sp <- base_sp(targeting_threshold = 20)

  on_baseline <- .sp_scenario_reach(baseline, sp, "hh")
  on_full     <- .sp_scenario_reach(full, sp, "hh")

  # This is the bug's signature: the total roughly doubles with the extra
  # round while the per-recipient amount is unchanged - which is why the
  # report was "the total is wrong, per unit looks fine".
  expect_gt(on_full$transfer_total, on_baseline$transfer_total * 1.5)
  expect_equal(on_full$transfer_per_unit, on_baseline$transfer_per_unit)
})


# ---- .sp_transfer_totals ----------------------------------------------------

test_that("transfer totals undo the per-capita scaling applied to welfare", {
  svy <- make_svy(200)
  sp  <- base_sp(targeting = "universal", transfer_amount_usd = 10,
                 transfer_n_payments = 12L)
  pol <- apply_policy_to_svy(svy, sp = sp, analysis_unit = "hh")
  t   <- .sp_transfer_totals(pol, "hh")

  # Everyone is eligible, so the annual cost is the annual per-household
  # amount times the weighted population.
  expect_equal(t$per_unit, 120)
  expect_equal(t$total, 120 * sum(svy$weight))
  expect_equal(t$n_recipients, nrow(svy))
})

test_that("a non-positive household size cannot poison the total", {
  svy <- make_svy(100)
  svy$hhsize[1:5] <- c(0, NA, -1, NA, 0)
  sp  <- base_sp(targeting = "universal")
  pol <- apply_policy_to_svy(svy, sp = sp, analysis_unit = "hh")
  t   <- .sp_transfer_totals(pol, "hh")

  # apply_policy_to_svy() guards hhsize the same way; totalling must too,
  # rather than propagating NA through the whole sum.
  expect_true(is.finite(t$total))
  expect_gt(t$total, 0)
})

test_that("transfer totals degrade rather than erroring", {
  expect_equal(.sp_transfer_totals(NULL)$total, 0)
  expect_equal(.sp_transfer_totals(data.frame(a = 1))$total, 0)
  # A frame with a transfer column of all zeros costs nothing.
  df <- data.frame(weight = 1:5, hhsize = 1:5)
  df[[SP_TRANSFER_COL]] <- 0
  expect_equal(.sp_transfer_totals(df, "hh")$total, 0)
  expect_equal(.sp_transfer_totals(df, "hh")$n_recipients, 0L)
})

test_that("totals work without survey weights and say so", {
  svy <- make_svy(100, weights = FALSE)
  pol <- apply_policy_to_svy(svy, sp = base_sp(targeting = "universal"),
                             analysis_unit = "hh")
  t <- .sp_transfer_totals(pol, "hh")
  expect_false(t$weighted)
  expect_equal(t$n_recipients_weighted, nrow(svy))
  expect_gt(t$total, 0)
})
