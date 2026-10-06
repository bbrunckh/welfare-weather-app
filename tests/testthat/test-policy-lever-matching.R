library(testthat)

# CR-BUG-08: lever gating and coefficient term maps use one exact matcher.

make_piped_fixture <- function(n = 200L, seed = 8L) {
  set.seed(seed)
  data.frame(
    welfare = exp(stats::rnorm(n, log(3), 0.25)),
    temp = stats::rnorm(n, 25, 2),
    piped = stats::rbinom(n, 1, 0.3),
    piped_to_prem = stats::rbinom(n, 1, 0.4)
  )
}

test_that("lever matcher is exact on variable and term components", {
  expect_false(.lever_in_model("piped", "piped_to_prem"))
  expect_false(.lever_in_model("piped", "temp:piped_to_prem"))
  expect_true(.lever_in_model("piped_to_prem", "piped_to_prem"))
  expect_true(.lever_in_model("electricity", "temp:electricity"))
  expect_true(.lever_in_model("Electricity", "electricity"))
  expect_identical(
    .lever_in_model(c("piped", "piped_to_prem"), c("x", "piped_to_prem")),
    c(FALSE, TRUE)
  )

  terms <- c("temp", "piped_to_prem", "temp:piped_to_prem")
  expect_identical(.policy_lever_terms("piped", terms), character(0))
  expect_identical(
    .policy_lever_terms("piped", terms, partner = "temp"),
    character(0)
  )
})

test_that("factor-level and interaction term names still resolve", {
  f <- factor(c("0", "1", "1"))
  terms <- c("(Intercept)", "temp", "piped1", "piped_to_prem",
             "temp:piped1", "piped_to_prem:temp")
  expect_identical(.policy_lever_terms("piped", terms, f), "piped1")
  expect_identical(
    .policy_lever_terms("piped", terms, f, partner = "temp"),
    "temp:piped1"
  )
  expect_identical(
    .policy_lever_terms("piped_to_prem", terms, partner = "temp"),
    "piped_to_prem:temp"
  )
  # Binned weather keeps its prefix match on the weather side only.
  expect_identical(
    .policy_lever_terms("piped", "temp[20,25):piped", partner = "temp"),
    "temp[20,25):piped"
  )
  expect_identical(.policy_lever_terms("flag", "flagTRUE", c(TRUE, FALSE)),
                   "flagTRUE")
})

test_that("piped lever is unavailable when only piped_to_prem is modelled", {
  svy <- make_piped_fixture()
  infra <- list(piped_access_change_pct = 50)
  out <- apply_policy_to_svy(svy, infra = infra,
                             model_vars = "piped_to_prem", seed = 1L)
  expect_identical(out$piped, svy$piped)
  expect_identical(out$piped_to_prem, svy$piped_to_prem)
  expect_identical(
    .policy_candidate_cols(infra = infra, model_vars = "piped_to_prem"),
    character(0)
  )
})

test_that("piped main effect is zero when the model only has piped_to_prem", {
  svy <- make_piped_fixture()
  fit <- stats::lm(log(welfare) ~ temp + piped_to_prem + temp:piped_to_prem,
                   data = svy)
  policy <- svy
  policy$piped <- 1L
  policy[[SP_TRANSFER_COL]] <- 0
  r <- suppressWarnings(decompose_policy_effect(
    svy, policy,
    list(engine = "fixest", fit3 = fit, weather_terms = "temp",
         train_data = svy),
    list(name = "welfare", transform = "log")
  ))
  expect_equal(max(abs(r$delta_main)), 0)
  expect_equal(max(abs(r$delta_res2)), 0)
})

test_that("factor lever terms feed the main and interaction channels", {
  svy <- make_piped_fixture()
  svy$piped <- factor(svy$piped, levels = c(0, 1))
  fit <- stats::lm(log(welfare) ~ temp + piped + temp:piped, data = svy)
  policy <- svy
  policy$piped <- factor(rep(1, nrow(svy)), levels = c(0, 1))
  policy[[SP_TRANSFER_COL]] <- 0
  r <- decompose_policy_effect(
    svy, policy,
    list(engine = "fixest", fit3 = fit, weather_terms = "temp",
         train_data = svy),
    list(name = "welfare", transform = "log")
  )
  delta <- as.numeric(as.character(policy$piped)) -
    as.numeric(as.character(svy$piped))
  expect_equal(r$delta_main, unname(stats::coef(fit)[["piped1"]] * delta))
  expect_true(any(abs(r$delta_res2) > 0))
})
