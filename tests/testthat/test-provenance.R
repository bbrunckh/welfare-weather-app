# ============================================================================ #
# tests/testthat/test-provenance.R                                             #
# UI-49: an exported file must state what produced it - source, model         #
#        specification, seed, run signature - read from the result's own       #
#        stored metadata rather than from live inputs. There is deliberately   #
#        no on-screen banner; the record travels with the export.              #
# ============================================================================ #

library(testthat)

fake_fit <- function(...) {
  utils::modifyList(
    list(
      engine = "fixest",
      .sig  = list(step = "fit", survey_version = 3L,
                   outcome = list(name = "welfare", label = "Welfare"),
                   weather = list(name = "tx", label = "Max temp")),
      .snap = list(
        outcome        = data.frame(name = "welfare", label = "Welfare pc"),
        weather        = data.frame(name = "tx", label = "Max temperature"),
        survey_weather = data.frame(a = 1:250),
        model = list(type = "Linear regression", engine = "fixest",
                     interactions = "urban", fixedeffects = c("year", "gaul1"),
                     hh_covariates = "hhsize", ind_covariates = character(0),
                     firm_covariates = character(0), area_covariates = character(0),
                     covariate_selection = "User-defined",
                     cluster = "loc_id_panel")
      )
    ),
    list(...)
  )
}

db_params <- function() list(
  type          = "databricks",
  workspace     = "https://adb-999.example.net",
  volume_path   = "/Volumes/cat/sch/vol",
  client_id     = "abcd-1234",
  client_secret = "TOP-SECRET-VALUE",
  token         = "ANOTHER-SECRET"
)


# ---- Credential redaction ---------------------------------------------------

test_that("source identity is recorded and credentials never are", {
  src <- .provenance_source(db_params())

  expect_equal(src$type, "databricks")
  expect_equal(src$workspace, "https://adb-999.example.net")
  expect_equal(src$volume_path, "/Volumes/cat/sch/vol")

  flat <- paste(unlist(src), collapse = " ")
  expect_false(grepl("TOP-SECRET-VALUE", flat, fixed = TRUE))
  expect_false(grepl("ANOTHER-SECRET", flat, fixed = TRUE))
  expect_false(grepl("abcd-1234", flat, fixed = TRUE))
  # Their presence is still disclosed - three fields were set.
  expect_match(src$credentials, "3 field")
})

test_that("redaction is case-insensitive and covers common key names", {
  src <- .provenance_source(list(
    type = "s3", bucket = "b",
    AWS_SECRET_ACCESS_KEY = "x", Password = "y", apiToken = "z"))
  expect_equal(src$bucket, "b")
  expect_false(any(c("x", "y", "z") %in% unlist(src)))
})

test_that("an absent source is reported rather than fabricated", {
  expect_equal(.provenance_source(NULL)$type, "unknown")
  expect_equal(.provenance_source(list())$type, "unknown")
})


# ---- Run signature ----------------------------------------------------------

test_that("the run signature is stable for equal inputs and differs otherwise", {
  a <- list(step = "fit", outcome = "welfare", weather = "tx")
  b <- list(step = "fit", outcome = "welfare", weather = "tx")
  c2 <- list(step = "fit", outcome = "welfare", weather = "pr")

  expect_equal(.provenance_digest(a), .provenance_digest(b))
  expect_false(identical(.provenance_digest(a), .provenance_digest(c2)))
  expect_match(.provenance_digest(a), "^[0-9a-f]{8}$")
  expect_true(is.na(.provenance_digest(NULL)))
})


# ---- Specification line -----------------------------------------------------

test_that("the specification line names every part of the model", {
  spec <- .provenance_model_spec(fake_fit()$.snap$model)
  for (bit in c("Linear regression", "fixest", "urban", "year", "hhsize",
                "User-defined", "loc_id_panel")) {
    expect_match(spec, bit, fixed = TRUE)
  }
})

test_that("a covariate-free specification says so rather than going silent", {
  spec <- .provenance_model_spec(list(type = "Linear regression",
                                      interactions = "urban"))
  expect_match(spec, "covariates: none", fixed = TRUE)
})

test_that("the specification line uses variable labels when given a lookup", {
  spec <- .provenance_model_spec(
    list(type = "Linear regression", interactions = "urban"),
    label_fun = function(x) if (identical(x, "urban")) "Urban area" else x)
  expect_match(spec, "Urban area", fixed = TRUE)
})


# ---- Provenance record ------------------------------------------------------

test_that("a step that has not run has no provenance", {
  expect_null(wise_provenance(1L, NULL))
})

test_that("the record is built from stored metadata, not live inputs", {
  p <- wise_provenance(1L, fake_fit(), connection_params = db_params(),
                       seed = 42L)

  # Labels come from the fit-time snapshot (.snap), which is the point: the
  # sidebar may since have moved on.
  expect_equal(p$outcome, "Welfare pc")
  expect_equal(p$weather, "Max temperature")
  expect_equal(p$n_observations, 250L)
  expect_equal(p$random_seed, 42L)
  expect_equal(p$engine, "fixest")
  expect_equal(p$survey_version, 3L)
  expect_equal(p$step_label, "Step 1 - Model welfare")
  expect_match(p$run_signature, "^[0-9a-f]{8}$")
  expect_match(p$model_spec, "Linear regression", fixed = TRUE)
  expect_equal(p$source$type, "databricks")
})

test_that("specification fallbacks travel with the record", {
  fit <- fake_fit(fallbacks = list(
    list(kind = "model_family", requested = "logistic", used = "linear",
         reason = "separation")))
  p <- wise_provenance(1L, fit)
  expect_length(p$fallbacks, 1L)
  expect_match(p$fallbacks, "requested logistic, fitted linear", fixed = TRUE)
})

test_that("extra step-specific fields are appended", {
  p <- wise_provenance(2L, fake_fit(),
                       extra = list(scenarios = "SSP2-4.5, SSP5-8.5"))
  expect_equal(p$scenarios, "SSP2-4.5, SSP5-8.5")
  expect_equal(p$step_label, "Step 2 - Climate scenarios")
})


# ---- Safe reading -----------------------------------------------------------

test_that("connection params are read without blocking on an unmet req()", {
  expect_null(read_connection_params(function() shiny::req(FALSE)))
  expect_null(read_connection_params(NULL))
  expect_equal(read_connection_params(function() list(type = "local"))$type,
               "local")
  # Redaction is not this function's job - wise_provenance() owns it, so the
  # raw value comes back untouched.
  expect_equal(read_connection_params(function() db_params())$client_secret,
               "TOP-SECRET-VALUE")
})
