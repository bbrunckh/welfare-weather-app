test_policy_exposure_fixture <- function() {
  survey <- data.frame(
    hhid = c("h1", "h2", "h3"),
    code = "TST",
    year = c(2020L, 2020L, 2020L),
    survname = c("wave_a", "wave_a", "wave_b"),
    loc_id = "loc1",
    int_month = 1L,
    welfare = c(1, 2, 3),
    weight = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  weather <- data.frame(
    code = "TST",
    year = 2020L,
    survname = c("wave_a", "wave_a", "wave_b"),
    loc_id = "loc1",
    timestamp = as.POSIXct(
      c("2030-01-01", "2030-01-01", "2031-01-01"), tz = "UTC"
    ),
    exposure = factor(c("dry", "wet", "dry"), levels = c("dry", "wet")),
    stringsAsFactors = FALSE
  )
  train <- data.frame(
    welfare = c(1, 2, 3, 4),
    exposure = factor(c("dry", "wet", "dry", "wet"),
      levels = c("dry", "wet")
    )
  )
  prepared <- survey |>
    dplyr::mutate(year = as.character(year)) |>
    dplyr::select(-welfare)
  cache <- build_weather_join_cache(dplyr::mutate(
    prepared, .svy_row_id = seq_len(nrow(prepared))
  ))
  list(survey = survey, weather = weather, train = train,
       prepared = prepared, cache = cache)
}

run_policy_exposure_fixture <- function(cached) {
  fixture <- test_policy_exposure_fixture()
  suppressWarnings(run_sim_pipeline(
    weather_raw = fixture$weather,
    svy = fixture$survey,
    sw = data.frame(name = "exposure"),
    so = list(name = "welfare", type = "numeric", transform = "none"),
    model = stats::lm(welfare ~ exposure, data = fixture$train),
    residuals = "none",
    train_data = fixture$train,
    engine = "lm",
    svy_prepared = if (cached) fixture$prepared else NULL,
    weather_join_cache = if (cached) fixture$cache else NULL
  ))
}

test_that("inline and cached joins retain exact exposure-to-prediction mapping", {
  inline <- run_policy_exposure_fixture(FALSE)
  cached <- run_policy_exposure_fixture(TRUE)

  expect_true(inline$weather_exposure$available)
  expect_true(cached$weather_exposure$available)
  expect_identical(inline$weather_exposure$status, "ok")
  expect_identical(inline$weather_exposure, cached$weather_exposure)
  mapping <- inline$weather_exposure
  expect_identical(nrow(mapping$table), 3L)
  expect_identical(mapping$table$timestamp[1], mapping$table$timestamp[2])
  expect_identical(mapping$table$exposure, factor(
    c("dry", "wet", "dry"), levels = c("dry", "wet")
  ))
  expect_identical(mapping$table$survname, c("wave_a", "wave_a", "wave_b"))
  expect_identical(mapping$row_index, c(1L, 1L, 2L, 2L, 3L))
  expect_identical(mapping$prediction_row_id, seq_len(5L))
  expect_identical(mapping$svy_row_id, c(1L, 2L, 1L, 2L, 3L))
  expect_identical(mapping$sim_year, c(2030L, 2030L, 2030L, 2030L, 2031L))
  expect_identical(mapping$weight, c(1, 2, 1, 2, 3))
  expect_null(mapping$id_vec)
  expect_identical(inline$y_point, cached$y_point)
  fixture <- test_policy_exposure_fixture()
  survey <- fixture$survey
  survey$exposure <- factor("dry", levels = c("dry", "wet"))
  policy <- survey
  policy[[SP_TRANSFER_COL]] <- 0.5
  context <- .build_decomposition_context(survey, policy,
    list(engine = "fixest", fit3 = stats::lm(welfare ~ exposure, fixture$train),
      weather_terms = "exposure", train_data = fixture$train),
    list(name = "welfare", transform = "none"), skip_coef = TRUE,
    run_identity = "mapping-run")
  prepared <- .prepare_policy_annual_channels(context, "mapping-run")
  annual <- .policy_annual_channels(cached, prepared, "mapping-run")
  expect_equal(annual$delta_total, rep(0.5, length(cached$y_point)))
  expect_equal(annual$delta_total,
    .policy_annual_channels_reference(inline, context, "mapping-run")$delta_total)
})

test_that("compact Step 2 pipelines retain exposure mapping vectors and table", {
  pipe <- run_policy_exposure_fixture(FALSE)
  compact <- .compact_pipeline(pipe)

  expect_identical(compact$weather_exposure, pipe$weather_exposure)
  expect_identical(compact$weather_exposure$row_index, c(1L, 1L, 2L, 2L, 3L))
})

test_that("lost prediction identity is explicitly unavailable", {
  table <- .policy_exposure_table(data.frame(
    code = "TST", year = 2020L, survname = "wave_a", loc_id = "loc1",
    timestamp = as.POSIXct("2030-01-01", tz = "UTC"), exposure = "dry"
  ))
  mapping <- .policy_exposure_mapping(
    out = data.frame(.svy_row_id = 1L), table = table, n_joined = 1L,
    sim_year = 2030L, weight = 1, id_vec = "h1", id_col = "hhid"
  )

  expect_false(mapping$available)
  expect_identical(mapping$status, "unavailable")
  expect_match(mapping$reason, "lost weather or survey row identity")
})

test_that("mapping follows prediction filtering and reordering, not position", {
  table <- .policy_exposure_table(data.frame(
    code = "TST", year = 2020L, survname = "wave_a", loc_id = "loc1",
    timestamp = as.POSIXct(c("2030-01-01", "2031-01-01"), tz = "UTC"),
    exposure = factor(c("dry", "wet"), levels = c("dry", "wet"))
  ))
  out <- data.frame(
    .policy_exposure_id = c(2L, 1L),
    .policy_prediction_row_id = c(3L, 1L),
    .svy_row_id = c(7L, 4L)
  )
  mapping <- .policy_exposure_mapping(
    out = out, table = table, n_joined = 3L, sim_year = c(2031L, 2030L),
    weight = c(2, 1), id_vec = c("b", "a"), id_col = "hhid"
  )

  expect_true(mapping$available)
  expect_identical(mapping$row_index, c(2L, 1L))
  expect_identical(mapping$prediction_row_id, c(3L, 1L))
  expect_identical(mapping$svy_row_id, c(7L, 4L))
  for (field in c(".policy_exposure_id", ".policy_prediction_row_id", ".svy_row_id")) {
    bad <- out
    bad[[field]][1] <- 1.5
    invalid <- .policy_exposure_mapping(bad, table, 3L, c(2031L, 2030L),
      c(2, 1), c("b", "a"), "hhid")
    expect_identical(invalid$status, "unavailable")
    expect_match(invalid$reason, "nonintegral")
  }
})
