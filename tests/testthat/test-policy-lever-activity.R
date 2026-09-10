# ============================================================================ #
# tests/testthat/test-policy-lever-activity.R                                  #
# UI-64: the Step 3 summary card counted *selected* policy domains, so a       #
#        domain the user never touched reported as configured. The count and   #
#        the Policies row now key off the same predicates                      #
#        `apply_policy_to_svy()` uses to decide whether to touch the survey.   #
# ============================================================================ #

library(testthat)

card_text <- function(...) {
  gsub("[[:space:]]+", " ",
       gsub("<[^>]+>", " ", as.character(policy_summary_card(...))))
}

sp_off <- list(budget_mode = "transfer_first", transfer_amount_usd = 0,
               transfer_n_payments = 6L, targeting = "exante_poor")
sp_on  <- utils::modifyList(sp_off, list(transfer_amount_usd = 50))


# ---- Predicates -------------------------------------------------------------

test_that("a configured target with no money is not a change", {
  # Targeting alone moves nobody's welfare.
  expect_false(has_sp_change(sp_off))
  expect_true(has_sp_change(sp_on))
})

test_that("budget-first social protection keys off the budget", {
  bf <- list(budget_mode = "budget_first", budget_fixed = 0,
             transfer_amount_usd = 999)
  # In budget-first mode the per-payment amount is derived, not set.
  expect_false(has_sp_change(bf))
  expect_true(has_sp_change(utils::modifyList(bf, list(budget_fixed = 1e6))))
})

test_that("a transfer with zero payments a year spends nothing", {
  expect_false(has_sp_change(utils::modifyList(
    sp_on, list(transfer_n_payments = 0))))
})

test_that("access levers count as moved when shifted or set to universal", {
  expect_false(has_infra_change(list(elec_access_change_pct = 0,
                                     elec_universal = FALSE)))
  expect_true(has_infra_change(list(elec_access_change_pct = 10)))
  expect_true(has_infra_change(list(elec_universal = TRUE)))
  # A negative shift is still a shift.
  expect_true(has_infra_change(list(water_access_change_pct = -5)))
  # The health lever has its own "max" mode with no percentage.
  expect_true(has_infra_change(list(health_mode = "max")))
})

test_that("each domain reads only its own fields", {
  expect_true(has_digital_change(list(internet_access_change_pct = 5)))
  expect_false(has_digital_change(list(elec_access_change_pct = 5)))
  expect_true(has_labor_change(list(sector_services = 3)))
  expect_false(has_labor_change(list(employment_change_pp = 0,
                                     sector_manufacturing = 0,
                                     sector_services = 0)))
  expect_true(has_education_change(list(secondary_universal = TRUE)))
  expect_false(has_education_change(list(primary_access_change_pct = 0)))
})

test_that("absent, empty and non-numeric scenarios are not changes", {
  for (f in list(has_infra_change, has_digital_change,
                 has_labor_change, has_education_change, has_sp_change)) {
    expect_false(f(NULL))
    expect_false(f(list()))
  }
  # NA is not a value the user set.
  expect_false(has_labor_change(list(employment_change_pp = NA_real_)))
  expect_false(has_infra_change(list(elec_access_change_pct = "10")))
})


# ---- Card ------------------------------------------------------------------

test_that("selecting a policy in Step 1 does not count as configuring it", {
  txt <- card_text(selected_policies = "A", sp_scenario = sp_off,
                   infra_scenario = list(elec_access_change_pct = 0))
  expect_match(txt, "0 policies", fixed = TRUE)
  expect_match(txt, "no lever has been changed yet", fixed = TRUE)
  expect_match(txt, "None", fixed = TRUE)
})

test_that("the count follows the levers that actually moved", {
  one <- card_text(selected_policies = "A", sp_scenario = sp_on)
  expect_match(one, "1 policy", fixed = TRUE)
  expect_match(one, "Social protection", fixed = TRUE)

  two <- card_text(selected_policies = "A", sp_scenario = sp_on,
                   infra_scenario = list(elec_universal = TRUE))
  expect_match(two, "2 policies", fixed = TRUE)
  expect_match(two, "Infrastructure", fixed = TRUE)
})

test_that("every domain can appear in the Policies row", {
  txt <- card_text(
    sp_scenario        = sp_on,
    infra_scenario     = list(elec_universal = TRUE),
    digital_scenario   = list(internet_access_change_pct = 10),
    labor_scenario     = list(employment_change_pp = 2),
    education_scenario = list(primary_universal = TRUE))
  expect_match(txt, "5 policies", fixed = TRUE)
  for (d in c("Social protection", "Infrastructure", "Digital inclusion",
              "Labor market", "Education")) {
    expect_match(txt, d, fixed = TRUE)
  }
})

test_that("the Step 1 policy selection is reported as the model interaction", {
  # It is a specification choice, not a lever that was moved - so it belongs
  # with the model, not in the configured-policy count.
  txt <- card_text(selected_policies = "A", sp_scenario = sp_off)
  expect_match(txt, "Policy interaction", fixed = TRUE)
  expect_match(txt, "0 policies", fixed = TRUE)
})

test_that("the card accepts reactives as well as plain lists", {
  txt <- card_text(sp_scenario = function() sp_on,
                   infra_scenario = function() list(elec_universal = TRUE))
  expect_match(txt, "2 policies", fixed = TRUE)
})


# ---- The card and the simulation must agree --------------------------------

test_that("a scenario the card calls unconfigured leaves the survey untouched", {
  set.seed(3)
  svy <- data.frame(welfare = runif(50, 1, 5), hhsize = sample(1:6, 50, TRUE),
                    electricity = rep(0, 50), weight = runif(50, 50, 150))
  mod <- apply_policy_to_svy(svy, sp = sp_off,
                             infra = list(elec_access_change_pct = 0),
                             analysis_unit = "hh")
  expect_false(.scenario_has_effect(svy, mod))
  expect_match(card_text(sp_scenario = sp_off,
                         infra_scenario = list(elec_access_change_pct = 0)),
               "0 policies", fixed = TRUE)
})

test_that("a scenario the card counts does change the survey", {
  set.seed(3)
  svy <- data.frame(welfare = runif(50, 1, 5), hhsize = sample(1:6, 50, TRUE),
                    weight = runif(50, 50, 150))
  mod <- apply_policy_to_svy(svy, sp = sp_on, analysis_unit = "hh")
  expect_true(.scenario_has_effect(svy, mod))
  expect_match(card_text(sp_scenario = sp_on), "1 policy", fixed = TRUE)
})
