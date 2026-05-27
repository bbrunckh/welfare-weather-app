OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")

POLICY_VARS <- c("electricity", "imp_wat_san_rec", "ttime_health", "urban")

# keep fit3 t_1to3m 
coefs <- readr::read_csv(paste0(OUT_DIR, "/model_fit/model_coefficients.csv")) |>
  filter(model == "fit3", weather == "t_1to3m_binn_None") |>
  filter(grepl("Inf\\]", term), is.na(interaction))

readr::write_csv(coefs, paste0(OUT_DIR, "/model_fit/coefficients_t_top_bin.csv"))

# keep fit3 t_1to3m 
coefs <- readr::read_csv(paste0(OUT_DIR, "/model_fit/model_coefficients.csv")) |>
  filter(model == "fit3", weather == "t_1to3m_binn_None") |>
  filter(grepl("Inf\\]", term) | interaction == term, !is.na(interaction)) |>
  filter(interaction %in% POLICY_VARS)

readr::write_csv(coefs, paste0(OUT_DIR, "/model_fit/coefficients_t_top_bin_interactions.csv"))
