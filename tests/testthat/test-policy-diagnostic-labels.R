test_that("policy diagnostic charts use metadata labels without changing raw keys", {
  variable_list <- data.frame(
    name = c("electricity", "welfare"),
    label = c("Access to electricity", "Household welfare"),
    stringsAsFactors = FALSE
  )
  outcome <- data.frame(
    name = "welfare", label = "Welfare ($ per day, 2021 PPP)",
    stringsAsFactors = FALSE
  )

  expect_identical(
    .policy_diagnostic_label("electricity", variable_list),
    "Access to electricity"
  )
  expect_identical(
    .policy_diagnostic_label("welfare", variable_list, outcome),
    "Welfare ($ per day, 2021 PPP)"
  )
  expect_identical(
    .policy_diagnostic_label(SP_TRANSFER_COL, variable_list, outcome),
    "Social protection transfer ($ per day)"
  )
  expect_identical(paste0("hist_", "electricity"), "hist_electricity")
  expect_identical(
    paste0("policy_before_after_", "electricity"),
    "policy_before_after_electricity"
  )
})
