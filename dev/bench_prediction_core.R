# Opt-in Item 5 characterization benchmark.
options(golem.app.prod = FALSE)
pkgload::load_all(quiet = TRUE)

set.seed(205)
profile <- function(expr) {
  Sys.setenv(WISEAPP_PREDICTION_PROFILE = "1")
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(value = value, elapsed_seconds = proc.time()[["elapsed"]] - started,
       profile = attr(value, "prediction_profile"))
}

n_train <- 1000L
n_svy <- 800L
taus <- seq(0.1, 0.9, by = 0.1)
train <- data.frame(
  welfare = rnorm(n_train, 10, 2), temp = rnorm(n_train), rain = rnorm(n_train),
  loc = factor(sample(letters[1:8], n_train, TRUE)),
  year = factor(sample(2010:2018, n_train, TRUE))
)
svy <- train[seq_len(n_svy), , drop = FALSE]
svy$code <- "TST"; svy$year_num <- 2020L; svy$year <- 2020L
svy$survname <- "SRV"; svy$loc_id <- as.character(svy$loc)
svy$int_month <- sample.int(12L, n_svy, TRUE)
svy$hhid <- seq_len(n_svy)
weather <- data.frame(
  code = "TST", year = 2020L, survname = "SRV", loc_id = as.character(svy$loc),
  timestamp = as.POSIXct(sprintf("2030-%02d-01", svy$int_month), tz = "UTC"),
  temp = svy$temp + 0.4, rain = svy$rain + 0.2
)
sw <- data.frame(name = c("temp", "rain"), stringsAsFactors = FALSE)
so <- list(name = "welfare", type = "numeric", transform = "none")
model <- fixest::feols(welfare ~ temp + rain | loc + year, data = train, warn = FALSE)
chol_obj <- compute_chol_vcov(model)
prepped <- svy |>
  dplyr::mutate(year = as.character(year)) |>
  dplyr::select(-dplyr::any_of(c(sw$name, so$name))) |>
  dplyr::mutate(.svy_row_id = seq_len(dplyr::n()))
join_cache <- build_weather_join_cache(prepped)
common <- list(
  weather_raw = weather, svy = svy, sw = sw, so = so, model = model,
  residuals = "none", train_data = train, engine = "fixest", chol_obj = chol_obj,
  svy_prepared = prepped
)

ols_inline <- profile(do.call(run_sim_pipeline, common))
ols_cached <- profile(do.call(run_sim_pipeline, c(common, list(weather_join_cache = join_cache))))

rif_cols <- c("temp", "rain")
rif_cols_train <- train
for (i in seq_along(taus)) {
  rif_cols_train[[paste0("rif_", formatC(taus[[i]] * 100, format = "d"))]] <-
    compute_rif(train$welfare, taus[[i]])
}
fit_multi <- fixest::feols(
  stats::as.formula(paste0("c(", paste(names(rif_cols_train)[grepl("^rif_", names(rif_cols_train))], collapse = ","), ") ~ temp + rain | loc + year")),
  data = rif_cols_train, warn = FALSE
)
rif_common <- list(
  weather_raw = weather, svy = svy, sw = sw, so = so,
  model = NULL, residuals = "none", train_data = rif_cols_train,
  engine = "rif", chol_obj = NULL, fit_multi = fit_multi,
  taus = taus, weather_cols = rif_cols, svy_prepared = prepped,
  direct_rif_predictions = TRUE,
  direct_rif_metadata = build_direct_rif_metadata(fit_multi),
  direct_rif_baseline_cache = new.env(parent = emptyenv()),
  precomputed_ecdf_train = stats::ecdf(rif_cols_train[[so$name]])
)
rif_direct <- profile(do.call(run_sim_pipeline, rif_common))
rif_fallback_args <- rif_common
rif_fallback_args$direct_rif_predictions <- FALSE
rif_fallback_args$direct_rif_metadata <- NULL
rif_fallback_args$direct_rif_baseline_cache <- NULL
rif_fallback <- profile(do.call(run_sim_pipeline, rif_fallback_args))

summary <- data.frame(
  path = c("ols_inline", "ols_cached", "rif_direct", "rif_fallback"),
  elapsed_seconds = c(ols_inline$elapsed_seconds, ols_cached$elapsed_seconds,
                      rif_direct$elapsed_seconds, rif_fallback$elapsed_seconds),
  result_hash = vapply(list(ols_inline$value, ols_cached$value,
                            rif_direct$value, rif_fallback$value), function(x) {
    digest::digest(x[intersect(
      c("y_point", "F_loading", "sim_year", "weight", "id_vec",
        "id_col", "svy_row_id", "n_pre_join"), names(x)
    )], algo = "sha256")
  }, character(1L)),
  stringsAsFactors = FALSE
)
summary$parity_reference <- c(
  summary$result_hash[[1L]], summary$result_hash[[1L]],
  summary$result_hash[[3L]], summary$result_hash[[3L]]
)
summary$parity <- summary$result_hash == summary$parity_reference
profile_runs <- list(ols_inline, ols_cached, rif_direct, rif_fallback)
profiles <- do.call(rbind, lapply(seq_along(profile_runs), function(i) {
  x <- profile_runs[[i]]
  p <- x$profile
  if (is.null(p)) return(NULL)
  p$path <- summary$path[[i]]
  p
}))
dir.create("dev/outputs/prediction-core", recursive = TRUE, showWarnings = FALSE)
write.csv(summary, "dev/outputs/prediction-core/summary.csv", row.names = FALSE)
write.csv(profiles, "dev/outputs/prediction-core/profile.csv", row.names = FALSE)
print(summary)
print(profiles)
