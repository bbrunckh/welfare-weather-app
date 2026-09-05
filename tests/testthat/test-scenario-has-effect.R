# A social-protection-only scenario is a valid policy run. The cash transfer
# is deliberately excluded from .compute_policy_deltas() (it enters through
# welfare, not as a covariate), so "no covariate deltas" must never be read as
# "nothing was configured".

svy_fixture <- function(n = 20) {
  set.seed(11)
  data.frame(
    welfare     = runif(n, 1, 5),
    hhsize      = sample(1:6, n, replace = TRUE),
    electricity = rep(0, n),
    urban       = rep(c(0, 1), length.out = n)
  )
}

test_that("an untouched scenario has no effect", {
  svy <- svy_fixture()
  expect_false(.scenario_has_effect(svy, svy))
})

test_that("a social-protection transfer alone counts as an effect", {
  svy <- svy_fixture()
  policy <- svy
  policy[[SP_TRANSFER_COL]] <- c(rep(0.5, 10), rep(0, 10))

  expect_true(.scenario_has_effect(svy, policy))

  # And it is invisible to the covariate deltas - which is exactly why the
  # transfer needs its own check here.
  deltas <- .compute_policy_deltas(svy, policy, "welfare", character(0))
  expect_length(deltas, 0L)
})

test_that("a zero transfer is not an effect", {
  svy <- svy_fixture()
  policy <- svy
  policy[[SP_TRANSFER_COL]] <- rep(0, nrow(svy))
  expect_false(.scenario_has_effect(svy, policy))
})

test_that("a covariate lever alone counts as an effect", {
  svy <- svy_fixture()
  policy <- svy
  policy$electricity <- rep(1, nrow(svy))
  expect_true(.scenario_has_effect(svy, policy))
})

test_that("non-numeric column changes are detected", {
  svy <- svy_fixture()
  svy$region <- rep(c("a", "b"), length.out = nrow(svy))
  policy <- svy
  policy$region <- rep("a", nrow(svy))
  expect_true(.scenario_has_effect(svy, policy))
})

test_that("floating-point noise below tolerance is not an effect", {
  svy <- svy_fixture()
  policy <- svy
  policy$welfare <- policy$welfare + 1e-14
  expect_false(.scenario_has_effect(svy, policy))
})

test_that("missing frames are reported as no effect rather than erroring", {
  svy <- svy_fixture()
  expect_false(.scenario_has_effect(NULL, svy))
  expect_false(.scenario_has_effect(svy, NULL))
  expect_false(.scenario_has_effect(NULL, NULL))
})
