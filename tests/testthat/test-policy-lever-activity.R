# Policy activity predicates and Step 3 summary consistency.

library(testthat)

card_text <- function(...) {
  gsub("[[:space:]]+", " ",
       gsub("<[^>]+>", " ", as.character(policy_summary_card(...))))
}

sp_off <- list(
  budget_mode = "transfer_first",
  transfer_amount_usd = 0,
  transfer_n_payments = 6L,
  targeting = "exante_poor"
)

sp_on <- utils::modifyList(sp_off, list(transfer_amount_usd = 50))

test_that("default and absent policy scenarios are inactive", {
  for (f in list(has_infra_change, has_digital_change,
                 has_labor_change, has_education_change, has_sp_change)) {
    expect_false(f(NULL))
    expect_false(f(list()))
  }
  expect_false(has_sp_change(sp_off))
  expect_true(has_sp_change(sp_on))
})

test_that("social protection activity respects budget mode and payments", {
  budget_first <- list(
    budget_mode = "budget_first",
    budget_fixed = 0,
    transfer_amount_usd = 999
  )
  expect_false(has_sp_change(budget_first))
  expect_true(has_sp_change(utils::modifyList(
    budget_first, list(budget_fixed = 1000000)
  )))
  expect_false(has_sp_change(utils::modifyList(
    sp_on, list(transfer_n_payments = 0)
  )))
})

test_that("access predicates recognise universal and negative changes", {
  expect_true(has_infra_change(list(elec_access_change_pct = 10)))
  expect_true(has_infra_change(list(water_access_change_pct = -5)))
  expect_true(has_infra_change(list(elec_universal = TRUE)))
  expect_true(has_infra_change(list(health_mode = "max")))
  expect_false(has_infra_change(list(elec_access_change_pct = "10")))
  expect_true(has_digital_change(list(internet_access_change_pct = 5)))
  expect_true(has_education_change(list(secondary_universal = TRUE)))
})

test_that("labor activity reads only labor fields", {
  expect_true(has_labor_change(list(sector_services = 3)))
  # Sector targets are NULL until moved from the observed share (R2-BUG-05).
  expect_false(has_labor_change(list(
    employment_change_pp = 0,
    sector_manufacturing = NULL,
    sector_services = NULL
  )))
  expect_false(has_labor_change(list(elec_access_change_pct = 5)))
  expect_false(has_labor_change(list(employment_change_pp = NA_real_)))
})

test_that("policy summary counts changed domains, not selected interactions", {
  text <- card_text(
    selected_policies = "A",
    sp_scenario = sp_off,
    infra_scenario = list(elec_access_change_pct = 0)
  )
  expect_match(text, "0 policies", fixed = TRUE)
  expect_match(text, "no lever has been changed yet", fixed = TRUE)
  expect_match(text, "Energy / Electricity access", fixed = TRUE)
  expect_match(text, "None", fixed = TRUE)
})

test_that("policy summary lists every active domain", {
  text <- card_text(
    sp_scenario = sp_on,
    infra_scenario = list(elec_universal = TRUE),
    digital_scenario = list(internet_access_change_pct = 10),
    labor_scenario = list(employment_change_pp = 2),
    education_scenario = list(primary_universal = TRUE)
  )
  expect_match(text, "5 policies", fixed = TRUE)
  for (domain in c("Social protection", "Infrastructure",
                   "Digital inclusion", "Labor market", "Education")) {
    expect_match(text, domain, fixed = TRUE)
  }
})

test_that("a zero scenario leaves the survey unchanged and remains inactive", {
  set.seed(3)
  svy <- data.frame(
    welfare = runif(50, 1, 5),
    hhsize = sample(1:6, 50, TRUE),
    electricity = rep(0, 50),
    weight = runif(50, 50, 150)
  )
  modified <- apply_policy_to_svy(
    svy,
    sp = sp_off,
    infra = list(elec_access_change_pct = 0),
    analysis_unit = "hh"
  )
  expect_false(.scenario_has_effect(svy, modified))
  expect_match(card_text(
    sp_scenario = sp_off,
    infra_scenario = list(elec_access_change_pct = 0)
  ), "0 policies", fixed = TRUE)
})

test_that("a changed scenario changes the survey and is active", {
  set.seed(3)
  svy <- data.frame(
    welfare = runif(50, 1, 5),
    hhsize = sample(1:6, 50, TRUE),
    electricity = rep(0, 50),
    weight = runif(50, 50, 150)
  )
  modified <- apply_policy_to_svy(
    svy,
    sp = sp_on,
    analysis_unit = "hh"
  )
  expect_true(.scenario_has_effect(svy, modified))
  expect_match(card_text(sp_scenario = sp_on), "1 policy", fixed = TRUE)
})


test_that("the Step 3 summary shows the SP amount in the entry currency", {
  # R2-BUG-04: an LCU amount must not be labelled with a dollar sign.
  lcu <- utils::modifyList(sp_on, list(transfer_amount_usd = 1200, currency = "LCU"))
  expect_match(card_text(sp_scenario = lcu), "LCU 1,200/payment", fixed = TRUE)
  expect_no_match(card_text(sp_scenario = lcu), "$1,200", fixed = TRUE)

  ppp <- utils::modifyList(sp_on, list(transfer_amount_usd = 1200, currency = "PPP"))
  expect_match(card_text(sp_scenario = ppp), "$1,200/payment", fixed = TRUE)
  # Scenarios that predate the field stay in dollars.
  expect_match(card_text(sp_scenario = utils::modifyList(sp_on, list(
    transfer_amount_usd = 1200))), "$1,200/payment", fixed = TRUE)
})
