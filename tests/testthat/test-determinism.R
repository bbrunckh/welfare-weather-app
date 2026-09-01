library(testthat)

make_lasso_fixture <- function(n = 180L) {
  set.seed(71)
  signal <- stats::rnorm(n)
  data.frame(
    welfare = 2 + 2 * signal + 0.2 * stats::rnorm(n),
    temp = stats::rnorm(n),
    signal = signal,
    noise = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

lasso_variable_list <- function() {
  data.frame(
    name = c("signal", "noise"),
    ind = 0L,
    hh = 1L,
    area = 0L,
    firm = 0L,
    outcome = 0L,
    stringsAsFactors = FALSE
  )
}

make_policy_fixture <- function(n = 120L) {
  data.frame(
    hhid = seq_len(n),
    welfare = seq(1, 6, length.out = n),
    hhsize = rep(4, n),
    electricity = rep(c(0L, 1L), length.out = n),
    internet = rep(c(0L, 0L, 1L), length.out = n),
    educ_com1_hh = rep(c(0L, 1L, 0L, 1L), length.out = n),
    employed = rep(c(1L, 0L, 0L), length.out = n),
    selfemployed = rep(c(0L, 1L, 0L), length.out = n),
    unemployed = rep(c(0L, 0L, 1L), length.out = n),
    agriculture = rep(c(1L, 0L, 0L), length.out = n),
    industry = rep(c(0L, 1L, 0L), length.out = n),
    services = rep(c(0L, 0L, 1L), length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("seed derivation and keyed fallbacks have stable golden values", {
  expect_identical(wise_seed(123L, "policy"), 19985857L)
  expect_identical(wise_seed(123L, "residual", 2030L), 52011994L)
  expect_identical(
    deterministic_values_by_key(
      c(-2, -1, 0, 1, 2, 3),
      c(99L, 100L),
      wise_seed(123L, "original-unmatched")
    ),
    c(0, -1)
  )
})

test_that("deterministic collection is invariant to source row order", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")

  dir <- withr::local_tempdir()
  x <- data.frame(
    code = c("B", "A", "A", "A"),
    year = c(2020L, 2021L, 2020L, 2020L),
    loc_id = c("z", "a", "b", "a"),
    hhid = c(4L, 3L, 2L, 1L),
    value = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(x, file.path(dir, "a.parquet"))
  arrow::write_parquet(x[c(3, 1, 4, 2), ], file.path(dir, "b.parquet"))

  cp <- list(type = "local", path = dir)
  a <- load_data("a.parquet", cp, collect = TRUE)
  b <- load_data("b.parquet", cp, collect = TRUE)

  expect_identical(a, b)
  expect_identical(a$hhid, c(1L, 2L, 3L, 4L))
})

test_that("household rows remain contiguous when person IDs are present", {
  x <- data.frame(
    code = "A",
    year = 2020L,
    survname = "SRV",
    loc_id = "loc",
    hhid = c(2L, 1L, 2L, 1L),
    pid = c(1L, 2L, 2L, 1L)
  )
  out <- collect_deterministic(x)
  expect_identical(out$hhid, c(1L, 1L, 2L, 2L))
  expect_identical(out$pid, c(1L, 2L, 1L, 2L))
})

test_that("loc_panel labels are stable across row order", {
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  h3 <- data.frame(
    code = "TST",
    year = 2020L,
    survname = "SRV",
    loc_id = c("a", "b", "c", "d"),
    h3 = c("x", "x", "y", "z"),
    pop_2020 = 1,
    stringsAsFactors = FALSE
  )
  a <- suppressWarnings(loc_panel(
    dplyr::copy_to(con, h3, temporary = TRUE),
    id_col = loc_id, h3_col = h3, weight_col = pop_2020,
    group_cols = c("code", "year", "survname")
  ))
  b <- suppressWarnings(loc_panel(
    dplyr::copy_to(con, h3[c(4, 2, 1, 3), ], temporary = TRUE),
    id_col = loc_id, h3_col = h3, weight_col = pop_2020,
    group_cols = c("code", "year", "survname")
  ))

  expect_identical(
    collect_deterministic(a, "loc_id"),
    collect_deterministic(b, "loc_id")
  )
  panels <- collect_deterministic(a, "loc_id")
  expect_identical(panels$loc_id_panel, c(1L, 1L, 2L, 3L))
})

test_that("weather thread pin is restored after validation errors", {
  skip_if_not_installed("duckdb")

  con <- .duck_con()
  before <- DBI::dbGetQuery(
    con, "SELECT current_setting('threads') AS threads"
  )$threads
  expect_error(
    get_weather(
      survey_data = data.frame(code = "TST"),
      selected_surveys = data.frame(
        code = "TST", year = 2020L, survname = "SRV", source = "lsms"
      ),
      selected_weather = data.frame(
        name = "tx", ref_start = 1L, ref_end = 3L,
        temporalAgg = "Mean", transformation = "None"
      ),
      dates = as.Date("2020-01-01"),
      connection_params = list(type = "local", path = tempdir()),
      ssp = "ssp2_4_5"
    ),
    "future_period is required"
  )
  after <- DBI::dbGetQuery(
    con, "SELECT current_setting('threads') AS threads"
  )$threads
  expect_identical(after, before)
})

test_that("Lasso is deterministic and restores caller RNG", {
  df <- make_lasso_fixture()
  args <- list(
    df = df,
    selected_outcome = list(name = "welfare", type = "numeric"),
    weather_vars = "temp",
    valid_vl = lasso_variable_list(),
    mi_m = 2L,
    nfolds = 5L,
    use_mice = FALSE
  )

  set.seed(901)
  before <- .Random.seed
  a <- do.call(run_lasso_selection, args)
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- do.call(run_lasso_selection, args)

  expect_identical(a, b)
})

test_that("MICE Lasso path is deterministic and restores caller RNG", {
  skip_if_not_installed("mice")

  df <- make_lasso_fixture(120L)
  df$noise[seq(5L, 25L, by = 5L)] <- NA_real_
  args <- list(
    df = df,
    selected_outcome = list(name = "welfare", type = "numeric"),
    weather_vars = "temp",
    valid_vl = lasso_variable_list(),
    mi_m = 1L,
    mi_maxit = 1L,
    mi_method = "norm.predict",
    nfolds = 3L,
    use_mice = TRUE,
    use_parallel = FALSE
  )

  set.seed(906)
  before <- .Random.seed
  a <- do.call(run_lasso_selection, args)
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- do.call(run_lasso_selection, args)

  expect_identical(a, b)
})

test_that("parallel MICE Lasso is deterministic and restores caller RNG", {
  skip_if_not_installed("mice")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  df <- make_lasso_fixture(150L)
  df$noise[seq(5L, 25L, by = 5L)] <- NA_real_
  args <- list(
    df = df,
    selected_outcome = list(name = "welfare", type = "numeric"),
    weather_vars = "temp",
    valid_vl = lasso_variable_list(),
    mi_m = 2L,
    mi_maxit = 1L,
    mi_method = "pmm",
    nfolds = 3L,
    use_mice = TRUE,
    use_parallel = TRUE,
    n_workers = 1L,
    parallel_min_n = 1L
  )

  set.seed(907)
  before <- .Random.seed
  a <- do.call(run_lasso_selection, args)
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- do.call(run_lasso_selection, args)

  expect_identical(a, b)
})

test_that("Step 1 fixest fits are identical across repeated runs", {
  skip_if_not_installed("fixest")

  df <- make_lasso_fixture(160L)
  df$loc_id_panel <- rep(letters[1:8], length.out = nrow(df))
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type = "Linear regression",
    engine = "fixest",
    hh_covariates = c("signal", "noise"),
    cluster = "loc_id_panel"
  )

  a <- fit_model(df, so, sw, sm)
  stats::runif(10)
  b <- fit_model(df, so, sw, sm)

  expect_identical(stats::coef(a$fit3), stats::coef(b$fit3))
  expect_identical(stats::vcov(a$fit3), stats::vcov(b$fit3))
})

test_that("Step 2 simulation pipeline is identical across repeated runs", {
  skip_if_not_installed("fixest")

  df <- make_lasso_fixture(120L)
  df$code <- "TST"
  df$year <- 2020L
  df$survname <- "SRV"
  df$loc_id <- rep(c("a", "b"), each = 60L)
  df$int_month <- rep(1:12, length.out = nrow(df))
  df$hhid <- seq_len(nrow(df))
  so <- list(name = "welfare", type = "numeric", transform = "none")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type = "Linear regression",
    engine = "fixest",
    hh_covariates = c("signal", "noise")
  )
  mf <- fit_model(df, so, sw, sm)
  model <- fixest::feols(welfare ~ temp + signal + noise, data = mf$train_data)
  weather <- expand.grid(
    loc_id = c("a", "b"),
    timestamp = as.Date(sprintf("2030-%02d-01", 1:12)),
    stringsAsFactors = FALSE
  )
  weather$code <- "TST"
  weather$year <- 2020L
  weather$survname <- "SRV"
  weather$temp <- seq(-1, 1, length.out = nrow(weather))

  args <- list(
    weather_raw = weather,
    svy = df,
    sw = sw,
    so = so,
    model = model,
    residuals = "original",
    train_data = mf$train_data,
    engine = mf$engine,
    chol_obj = NULL
  )
  a <- do.call(run_sim_pipeline, args)
  stats::runif(10)
  b <- do.call(run_sim_pipeline, args)

  expect_identical(a$y_point, b$y_point)
  expect_identical(a$sim_year, b$sim_year)
  expect_identical(a$id_vec, b$id_vec)
  expect_identical(a$train_aug$.resid, b$train_aug$.resid)
})

test_that("policy assignment is deterministic and restores caller RNG", {
  svy <- make_policy_fixture()
  args <- list(
    svy = svy,
    infra = list(elec_universal = FALSE, elec_access_change_pct = 30),
    digital = list(internet_universal = FALSE, internet_access_change_pct = 25,
                   mobile_universal = FALSE, mobile_access_change_pct = 0),
    education = list(primary_universal = FALSE, primary_access_change_pct = 20,
                     secondary_universal = FALSE, secondary_access_change_pct = 0,
                     postsec_universal = FALSE, postsec_access_change_pct = 0),
    labor = list(employment_change_pp = 10, sector_manufacturing = 40,
                 sector_services = 40),
    sp = list(budget_mode = "transfer_first", transfer_n_payments = 1,
              transfer_amount_usd = 100, targeting = "exante_poor",
              targeting_threshold = 30, inclusion_error_pct = 10,
              exclusion_error_pct = 10),
    seed = 123L
  )

  set.seed(902)
  before <- .Random.seed
  a <- do.call(apply_policy_to_svy, args)
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- do.call(apply_policy_to_svy, args)
  args$seed <- 124L
  c <- do.call(apply_policy_to_svy, args)

  expect_identical(a, b)
  expect_false(identical(a, c))
})

test_that("Step 3 policy delta pipeline is identical across repeated runs", {
  fx <- make_policy_fixture(90L)
  fx$temp <- seq(20, 30, length.out = nrow(fx))
  fit <- stats::lm(log(welfare) ~ temp + electricity + temp:electricity,
                   data = fx)
  mf <- list(
    engine = "fixest",
    fit3 = fit,
    weather_terms = "temp",
    train_data = fx
  )
  so <- list(name = "welfare", transform = "log")
  svy_policy <- apply_policy_to_svy(
    fx,
    infra = list(elec_universal = FALSE, elec_access_change_pct = 30),
    seed = 123L
  )
  pipe <- list(
    y_point = stats::predict(fit, newdata = fx),
    F_loading = NULL,
    sim_year = rep(2030L, nrow(fx)),
    weight = NULL,
    id_vec = fx$hhid,
    id_col = "hhid",
    svy_row_id = seq_len(nrow(fx)),
    train_aug = transform(fx, .resid = stats::residuals(fit))
  )
  hist <- list(pipeline = pipe, weather_raw = data.frame(temp = mean(fx$temp)),
               so = so, svy = fx)

  run <- function() {
    apply_policy_delta_to_baseline(
      svy_baseline = fx,
      svy_policy = svy_policy,
      model_fit = mf,
      so = so,
      hist_sim_baseline = hist,
      skip_coef = TRUE
    )
  }
  a <- run()
  stats::runif(10)
  b <- run()

  expect_identical(a, b)
  expect_false(identical(a$hist_sim$pipeline$y_point, pipe$y_point))
})

test_that("residual draws are deterministic and restore caller RNG", {
  train <- data.frame(hhid = 1:6, .resid = c(-2, -1, 0, 1, 2, 3))
  ids <- c(1L, 99L, 99L, 100L)

  set.seed(903)
  before <- .Random.seed
  original_a <- draw_residuals_vec("original", train, length(ids), ids, "hhid")
  normal_a <- draw_residuals_vec("normal", train, length(ids), seed = 123L)
  resample_a <- draw_residuals_vec("resample", train, length(ids), seed = 123L)
  expect_identical(.Random.seed, before)

  stats::runif(10)
  original_b <- draw_residuals_vec("original", train, length(ids), ids, "hhid")
  normal_b <- draw_residuals_vec("normal", train, length(ids), seed = 123L)
  resample_b <- draw_residuals_vec("resample", train, length(ids), seed = 123L)

  expect_identical(original_a, original_b)
  expect_identical(original_a[2], original_a[3])
  expect_identical(normal_a, normal_b)
  expect_identical(resample_a, resample_b)
})

test_that("predict_outcome uses deterministic residual fallbacks", {
  train <- data.frame(
    hhid = 1:30,
    x = seq(-1, 1, length.out = 30),
    y = 2 + seq(-1, 1, length.out = 30) + rep(c(-0.2, 0.2), 15)
  )
  model <- stats::lm(y ~ x, data = train)
  newdata <- data.frame(hhid = c(1L, 99L, 99L), x = c(-1, 0, 1))

  set.seed(905)
  before <- .Random.seed
  a <- expect_warning(
    predict_outcome(model, newdata, residuals = "original", id = "hhid",
                    train_data = train, seed = 123L),
    "filling deterministically"
  )
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- expect_warning(
    predict_outcome(model, newdata, residuals = "original", id = "hhid",
                    train_data = train, seed = 123L),
    "filling deterministically"
  )

  expect_identical(a$.residual, b$.residual)
  expect_identical(a$.residual[2], a$.residual[3])
})

test_that("per-year residual streams are independent of aggregation method", {
  pipe <- list(
    y_point = rep(log(c(2, 3, 4)), 2),
    F_loading = NULL,
    sim_year = rep(c(2030L, 2031L), each = 3L),
    weight = NULL,
    id_vec = rep(1:3, 2),
    id_col = "hhid",
    train_aug = data.frame(hhid = 1:3, .resid = c(-0.2, 0.1, 0.3))
  )
  mean_a <- aggregate_pipeline_per_year(
    pipe, method = "mean", residuals = "resample", seed = 123L
  )
  total_a <- aggregate_pipeline_per_year(
    pipe, method = "total", residuals = "resample", seed = 123L
  )

  expect_equal(
    vapply(total_a, `[[`, numeric(1), "value"),
    vapply(mean_a, `[[`, numeric(1), "value") * 3
  )
})

test_that("direct Step 2 and Step 3 aggregation use the same yearly stream", {
  pipe <- list(
    y_point = log(c(2, 3, 4)),
    F_loading = NULL,
    sim_year = rep(2030L, 3L),
    weight = NULL,
    id_vec = 1:3,
    id_col = "hhid",
    train_aug = data.frame(hhid = 1:3, .resid = c(-0.2, 0.1, 0.3))
  )
  direct <- aggregate_with_uncertainty_delta(
    y_point = pipe$y_point,
    F_loading = NULL,
    method = "mean",
    residuals = "resample",
    train_aug = pipe$train_aug,
    id_vec = pipe$id_vec,
    id_col = pipe$id_col,
    seed = wise_seed(WISEAPP_DEFAULT_SEED, "residual", 2030L)
  )
  per_year <- aggregate_pipeline_per_year(
    pipe,
    method = "mean",
    residuals = "resample",
    seed = WISEAPP_DEFAULT_SEED
  )[[1L]]

  expect_identical(direct, per_year[names(direct)])
})

test_that("ranger engine is deterministic and single-threaded", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("ranger")

  df <- make_lasso_fixture(120L)
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type = "Linear regression",
    engine = "ranger",
    hh_covariates = c("signal", "noise")
  )

  set.seed(904)
  before <- .Random.seed
  a <- fit_model(df, so, sw, sm)
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- fit_model(df, so, sw, sm)

  expect_identical(
    as.numeric(stats::predict(a$fit3, new_data = df)$.pred),
    as.numeric(stats::predict(b$fit3, new_data = df)$.pred)
  )
  expect_identical(
    rlang::quo_get_expr(a$fit3$spec$method$fit$args$num.threads),
    1L
  )
})

test_that("xgboost engine is deterministic when installed", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("xgboost")

  df <- make_lasso_fixture(120L)
  so <- list(name = "welfare", type = "numeric")
  sw <- data.frame(name = "temp", cont_binned = "Continuous",
                   stringsAsFactors = FALSE)
  sm <- build_selected_model(
    model_type = "Linear regression",
    engine = "xgboost",
    hh_covariates = c("signal", "noise")
  )

  a <- fit_model(df, so, sw, sm)
  stats::runif(10)
  b <- fit_model(df, so, sw, sm)
  expect_identical(
    as.numeric(stats::predict(a$fit3, new_data = df)$.pred),
    as.numeric(stats::predict(b$fit3, new_data = df)$.pred)
  )
  expect_identical(
    rlang::quo_get_expr(a$fit3$spec$method$fit$args$nthread),
    1L
  )
})
