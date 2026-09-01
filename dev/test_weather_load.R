
# TEST WEATHER DATA LOADING AND PROCESSING

# source functions for testing
source("R/fct_load_data.R")
source("R/fct_get_weather.R")

# WEATHER FOR SURVEY DATES ONLY (STEP 1)

# inputs

  # connection params for local testing (update local path as needed) 
  connection_params <- list(type = "local", path = "/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents")

  # selected surveys
  selected_surveys <- data.frame(
    code     = "GNB",
    year     = 2021,
    survname = "EHCVM",
    source   = "gmd"
  )

  # survey data
  survey_data <- load_data(
    "microdata/hh/GNB/GNB_2021_EHCVM_GMD_hh.parquet",
    connection_params,
    collect = TRUE)

  # dates — unique survey timestamps
  dates <- survey_data |>
    dplyr::filter(!is.na(timestamp)) |>
    dplyr::pull(timestamp) |>
    unique() |>
    sort()

# 1. Single variable — Mean, no transformation, continuous
res1 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name = "tx", ref_start = 1L, ref_end = 3L,
    temporalAgg = "Mean", transformation = "None",
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
# result: named list with one entry "historical"
# res1$historical — data frame with columns code, year, survname, loc_id, timestamp, tx
str(res1$historical)
# check date range
range(res1$historical$timestamp)
# check tx range
range(res1$historical$tx, na.rm = TRUE)

# 2. Single variable — Mean, standardised anomaly, continuous
res2 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name = "tx", ref_start = 1L, ref_end = 3L,
    temporalAgg = "Mean", transformation = "Standardized anomaly",
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
str(res2$historical)
range(res2$historical$timestamp)
range(res1$historical$tx, na.rm = TRUE)

# 3. Single variable — Sum aggregation (precipitation)
res3 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name = "r", ref_start = 1L, ref_end = 3L,
    temporalAgg = "Sum", transformation = "None",
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
str(res3$historical)
range(res3$historical$timestamp)
range(res3$historical$r, na.rm = TRUE)

# 4. Two variables, mixed configurations (tx: Deviation from mean, r: Sum, None)
res4 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name        = c("tx", "r"),
    ref_start   = c(1L,  1L),
    ref_end     = c(3L,  3L),
    temporalAgg = c("Mean", "Sum"),
    transformation = c("Deviation from mean", "None"),
    
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
str(res4$historical)
range(res4$historical$timestamp)
range(res4$historical$tx, na.rm = TRUE)

# ---- WEATHER FOR SIMULATIONS — historical + SSPs (STEP 2) ------------------

# additional inputs

  # simulation date range — all survey months across historical period
  start_year <- 1990
  end_year   <- 2024

  dates_sim <- with(
    expand.grid(
      int_month = unique(survey_data$int_month),
      int_year  = start_year:end_year
    ),
    as.Date(paste(int_year, int_month, "01", sep = "-"))
  )

  # SSPs to run
  ssps <- c("ssp2_4_5", "ssp5_8_5")

  # future period(s) to perturb — single or list for multiple
  future_period <- c("2045-01-01", "2055-12-31")
  future_periods_multi <- list(
    c("2035-01-01", "2045-12-31"),
    c("2045-01-01", "2055-12-31")
  )

  # perturbation method: additive for temperature, multiplicative for precipitation
  perturbation_method <- c(tx = "additive", r = "multiplicative")

# 5. Historical simulation only — two variables, no SSP
res5 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name        = c("tx", "r"),
    ref_start   = c(1L,  1L),
    ref_end     = c(3L,  3L),
    temporalAgg = c("Mean", "Sum"),
    transformation = c("None", "None"),
    stringsAsFactors = FALSE
  ),
  dates             = dates_sim,
  connection_params = connection_params
)
# result: list with $historical only — all simulation dates
str(res5$historical)
nrow(res5$historical)
range(res5$historical$timestamp)

# 6. Single SSP, single period, all models — two variables
res6 <- get_weather(
  survey_data         = survey_data,
  selected_surveys    = selected_surveys,
  selected_weather    = data.frame(
    name        = c("tx", "r"),
    ref_start   = c(1L,  1L),
    ref_end     = c(3L,  3L),
    temporalAgg = c("Mean", "Sum"),
    transformation = c("None", "None"),
    stringsAsFactors = FALSE
  ),
  dates               = dates_sim,
  connection_params   = connection_params,
  ssp                 = "ssp2_4_5",
  future_period       = future_period,
  perturbation_method = perturbation_method
)
# result: $historical + ssp2_4_5_2045_2055_<model> entries
names(res6)
str(res6[[2]])
range(res6[[2]]$timestamp)

# 7. Two SSPs, single period, all models
res7 <- get_weather(
  survey_data         = survey_data,
  selected_surveys    = selected_surveys,
  selected_weather    = data.frame(
    name        = c("tx", "r"),
    ref_start   = c(1L,  1L),
    ref_end     = c(3L,  3L),
    temporalAgg = c("Mean", "Sum"),
    transformation = c("None", "None"),
    stringsAsFactors = FALSE
  ),
  dates               = dates_sim,
  connection_params   = connection_params,
  ssp                 = ssps,
  future_period       = future_period,
  perturbation_method = perturbation_method
)
# result: $historical + <ssp>_2045_2055_<model> per SSP
names(res7)

# 8. Single SSP, single period — single variable
res8 <- get_weather(
  survey_data         = survey_data,
  selected_surveys    = selected_surveys,
  selected_weather    = data.frame(
    name           = "tx",
    ref_start      = 1L,
    ref_end        = 3L,
    temporalAgg    = "Mean",
    transformation = "None",
    stringsAsFactors = FALSE
  ),
  dates               = dates_sim,
  connection_params   = connection_params,
  ssp                 = "ssp2_4_5",
  future_period       = future_period,
  perturbation_method = c(tx = "additive")
)
# result: $historical + ssp2_4_5_2045_2055_<model> entries
names(res8)
length(res8)

# 9. Single SSP, multiple periods — tests multi-period support
res9 <- get_weather(
  survey_data         = survey_data,
  selected_surveys    = selected_surveys,
  selected_weather    = data.frame(
    name           = "tx",
    ref_start      = 1L,
    ref_end        = 3L,
    temporalAgg    = "Mean",
    transformation = "None",
    stringsAsFactors = FALSE
  ),
  dates               = dates_sim,
  connection_params   = connection_params,
  ssp                 = "ssp2_4_5",
  future_period       = future_periods_multi,
  perturbation_method = c(tx = "additive")
)
# result: $historical + ssp2_4_5_2035_2045_<model> + ssp2_4_5_2045_2055_<model>
names(res9)
length(res9)


# Test: K-means binning
res_km <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name           = "tx",
    ref_start      = 1L,
    ref_end        = 3L,
    temporalAgg    = "Mean",
    transformation = "None",
    cont_binned    = "Binned",
    num_bins       = 3L,
    binning_method = "K-means",
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
cat("class(tx):", class(res_km$historical$tx), "\n")
cat("levels:", levels(res_km$historical$tx), "\n")
table(res_km$historical$tx, useNA = "ifany")


# Test: mixed — one variable binned, one continuous
res_bin_mixed <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name           = c("tx", "r"),
    ref_start      = c(1L, 1L),
    ref_end        = c(3L, 3L),
    temporalAgg    = c("Mean", "Sum"),
    transformation = c("None", "None"),
    cont_binned    = c("Binned", "Continuous"),
    num_bins       = c(4L, NA_integer_),
    binning_method = c("Equal width", NA_character_),
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
cat("class(tx):", class(res_bin_mixed$historical$tx), "— levels:", levels(res_bin_mixed$historical$tx), "\n")
cat("class(r):", class(res_bin_mixed$historical$r), "\n")
cat("r range:", range(res_bin_mixed$historical$r, na.rm = TRUE), "\n")


# Test: binning + single SSP — bins should be computed from unperturbed historical
res_bin_ssp <- get_weather(
  survey_data         = survey_data,
  selected_surveys    = selected_surveys,
  selected_weather    = data.frame(
    name           = "tx",
    ref_start      = 1L,
    ref_end        = 3L,
    temporalAgg    = "Mean",
    transformation = "None",
    cont_binned    = "Binned",
    num_bins       = 3L,
    binning_method = "Equal frequency",
    stringsAsFactors = FALSE
  ),
  dates               = dates_sim,
  connection_params   = connection_params,
  ssp                 = "ssp2_4_5",
  future_period       = future_period,
  perturbation_method = c(tx = "additive")
)

cat("Slices:", names(res_bin_ssp), "\n")
cat("Historical class(tx):", class(res_bin_ssp$historical$tx), "\n")
cat("Historical levels:", levels(res_bin_ssp$historical$tx), "\n")

# Check SSP slice — should use same breaks, values may shift toward warmer bins
ssp_slice <- res_bin_ssp[[2]]
cat("SSP slice class(tx):", class(ssp_slice$tx), "\n")
cat("SSP slice levels:", levels(ssp_slice$tx), "\n")
cat("\nHistorical bin counts:\n")
table(res_bin_ssp$historical$tx, useNA = "ifany")
cat("\nSSP slice bin counts:\n")
table(ssp_slice$tx, useNA = "ifany")


# Test: binning with equal frequency, single variable, historical only
res_bin1 <- get_weather(
  survey_data      = survey_data,
  selected_surveys = selected_surveys,
  selected_weather = data.frame(
    name           = "tx",
    ref_start      = 1L,
    ref_end        = 3L,
    temporalAgg    = "Mean",
    transformation = "None",
    cont_binned    = "Binned",
    num_bins       = 3L,
    binning_method = "Equal frequency",
    stringsAsFactors = FALSE
  ),
  dates             = dates,
  connection_params = connection_params
)
cat("class(tx):", class(res_bin1$historical$tx), "\n")
cat("levels:", levels(res_bin1$historical$tx), "\n")
table(res_bin1$historical$tx, useNA = "ifany")
