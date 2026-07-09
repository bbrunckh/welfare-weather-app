# Test: get_weather() determinism — same inputs must produce identical outputs.
# Runs each case twice and checks identical().

devtools::load_all()

connection_params <- list(
  type = "local",
  path = "/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents"
)

selected_surveys <- data.frame(
  code = "GNB", year = 2021, survname = "EHCVM", source = "gmd"
)

survey_data <- load_data(
  "microdata/hh/GNB/GNB_2021_EHCVM_GMD_hh.parquet",
  connection_params, collect = TRUE
)

dates <- sort(unique(survey_data$timestamp[!is.na(survey_data$timestamp)]))

# Helper: run a call twice and report
check_determinism <- function(label, call_fn) {
  cat(sprintf("%-50s", paste0(label, " ... ")))
  a <- call_fn()
  b <- call_fn()
  if (identical(a, b)) {
    cat("PASS\n")
  } else {
    cat("FAIL\n")
    for (nm in union(names(a), names(b))) {
      if (!identical(a[[nm]], b[[nm]])) {
        cat("  slice differs:", nm, "\n")
        diffs <- which(!mapply(identical, a[[nm]], b[[nm]]))
        if (length(diffs) > 0) cat("    columns:", names(a[[nm]])[diffs], "\n")
      }
    }
  }
  invisible(identical(a, b))
}

results <- c()

# 1. Single variable, continuous, no transformation
results["continuous"] <- check_determinism("1. Continuous, no transform", function() {
  get_weather(
    survey_data      = survey_data,
    selected_surveys = selected_surveys,
    selected_weather = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "None",
      stringsAsFactors = FALSE
    ),
    dates = dates, connection_params = connection_params
  )
})

# 2. Standardised anomaly transformation
results["anomaly"] <- check_determinism("2. Standardised anomaly", function() {
  get_weather(
    survey_data      = survey_data,
    selected_surveys = selected_surveys,
    selected_weather = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "Standardized anomaly",
      stringsAsFactors = FALSE
    ),
    dates = dates, connection_params = connection_params
  )
})

# 3. Two variables, mixed transformations
results["mixed_vars"] <- check_determinism("3. Two vars, mixed transforms", function() {
  get_weather(
    survey_data      = survey_data,
    selected_surveys = selected_surveys,
    selected_weather = data.frame(
      name        = c("tx", "r"),
      ref_start   = c(1L, 1L), ref_end = c(3L, 3L),
      temporalAgg = c("Mean", "Sum"),
      transformation = c("Deviation from mean", "None"),
      stringsAsFactors = FALSE
    ),
    dates = dates, connection_params = connection_params
  )
})

# 4. Equal-frequency binning
results["bin_eqfreq"] <- check_determinism("4. Equal-frequency binning", function() {
  get_weather(
    survey_data      = survey_data,
    selected_surveys = selected_surveys,
    selected_weather = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "None",
      cont_binned = "Binned", num_bins = 3L,
      binning_method = "Equal frequency",
      stringsAsFactors = FALSE
    ),
    dates = dates, connection_params = connection_params
  )
})

# 5. K-means binning
results["bin_kmeans"] <- check_determinism("5. K-means binning", function() {
  get_weather(
    survey_data      = survey_data,
    selected_surveys = selected_surveys,
    selected_weather = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "None",
      cont_binned = "Binned", num_bins = 3L,
      binning_method = "K-means",
      stringsAsFactors = FALSE
    ),
    dates = dates, connection_params = connection_params
  )
})

# 6. SSP climate scenario
dates_sim <- with(
  expand.grid(
    int_month = unique(survey_data$int_month),
    int_year  = 1990:2024
  ),
  as.Date(paste(int_year, int_month, "01", sep = "-"))
)

results["ssp"] <- check_determinism("6. SSP scenario (ssp2_4_5)", function() {
  get_weather(
    survey_data         = survey_data,
    selected_surveys    = selected_surveys,
    selected_weather    = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "None",
      stringsAsFactors = FALSE
    ),
    dates               = dates_sim,
    connection_params   = connection_params,
    ssp                 = "ssp2_4_5",
    future_period       = c("2045-01-01", "2055-12-31"),
    perturbation_method = c(tx = "additive")
  )
})

# 7. SSP + binning
results["ssp_binned"] <- check_determinism("7. SSP + equal-freq binning", function() {
  get_weather(
    survey_data         = survey_data,
    selected_surveys    = selected_surveys,
    selected_weather    = data.frame(
      name = "tx", ref_start = 1L, ref_end = 3L,
      temporalAgg = "Mean", transformation = "None",
      cont_binned = "Binned", num_bins = 3L,
      binning_method = "Equal frequency",
      stringsAsFactors = FALSE
    ),
    dates               = dates_sim,
    connection_params   = connection_params,
    ssp                 = "ssp2_4_5",
    future_period       = c("2045-01-01", "2055-12-31"),
    perturbation_method = c(tx = "additive")
  )
})

cat("\n========================================\n")
cat(sprintf("Results: %d/%d passed\n", sum(results), length(results)))
if (all(results)) cat("All determinism checks passed.\n") else cat("FAILURES detected.\n")