#' 1_modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom bsplus bs_accordion bs_append
#' @importFrom waiter autoWaiter spin_2 transparent
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),

    sidebarLayout(

      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title   = "1 Sample",
            content = tagList(
              mod_1_01_sample_ui(ns("sample")),
              mod_1_02_surveystats_ui(ns("surveystats"))
            )
          ) |>
          bs_append(
            title   = "2 Outcome",
            content = tagList(
              mod_1_03_outcome_ui(ns("outcome"))
            )
          ) |>
          bs_append(
            title   = "3 Weather",
            content = tagList(
              mod_1_04_weather_ui(ns("weather")),
              mod_1_05_weatherstats_ui(ns("weatherstats"))
            )
          ) |>
          bs_append(
            title   = "4 Model",
            content = tagList(
              mod_1_06_model_ui(ns("model")),
              mod_1_07_results_ui(ns("results"))
            )
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step1_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Outputs will appear here after you load data and click the relevant buttons in the sidebar."),
            includeMarkdown(system.file("app/www/equation.md", package = "wiseapp"))
          )
        )
      )
    )
  )
}

#' 1_modelling Server Functions
#'
#' @param id Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param survey_list Reactive tibble of survey metadata from mod_0_overview.
#' @param variable_list Reactive tibble of variable metadata from mod_0_overview.
#' @param cpi_ppp Reactive tibble of CPI/PPP conversion factors from mod_0_overview.
#' @param pov_lines Reactive tibble of 2021 PPP poverty lines from mod_0_overview.
#'
#' @noRd
mod_1_modelling_server <- function(
    id,
    connection_params,
    survey_list,
    variable_list,
    cpi_ppp,
    pov_lines
) {
  moduleServer(id, function(input, output, session) {

    # ---- 1. Sample selection ------------------------------------------------

    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      connection_params = connection_params,
      survey_list       = survey_list,
      variable_list     = variable_list
    )

    # ---- 2. Survey stats (adds tab after button click) -----------------

    mod_1_02_surveystats_api <- mod_1_02_surveystats_server(
      "surveystats",
      connection_params = connection_params,
      variable_list    = variable_list,
      cpi_ppp          = cpi_ppp,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = NULL,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 3. Outcome selection -----------------------------------------------

    mod_1_03_outcome_api <- mod_1_03_outcome_server(
      "outcome",
      variable_list = variable_list,
      survey_data   = mod_1_02_surveystats_api$survey_data
    )

    # ---- 4. Weather selection and configuration -----------------------------

    mod_1_04_weather_api <- mod_1_04_weather_server(
      "weather",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      survey_data      = mod_1_02_surveystats_api$survey_data
    )

    # ---- 5. Weather stats (adds tab after button click) ---------------------

    mod_1_05_weatherstats_api <- mod_1_05_weatherstats_server(
      "weatherstats",
      connection_params = connection_params,
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_data      = mod_1_02_surveystats_api$survey_data,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 6. Model specification ---------------------------------------------

    mod_1_06_model_api <- mod_1_06_model_server(
      "model",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather   = mod_1_05_weatherstats_api$survey_weather
    )

    # ---- 7. Results (adds tab after model run) ------------------------------

    mod_1_07_results_api <- mod_1_07_results_server(
      "results",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather   = mod_1_05_weatherstats_api$survey_weather,
      selected_model   = mod_1_06_model_api$selected_model,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 8. Model fit diagnostics (adds tab alongside results) --------------

    mod_1_08_modelfit_server(
      "modelfit",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      model_fit        = mod_1_07_results_api$model_fit,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # Convenience reactive: "final model" (your convention is [[3]])
    # final_model <- reactive({
    #   fits <- mod_1_07_results_api$model_fit()
    #   if (is.null(fits) || !length(fits)) return(NULL)
    #   fits[[3]] %||% fits[[length(fits)]]
    # })

    
# ---- Step 1 -> Step 2 explicit exports (hardening) ----

# Survey data file IDs used by Step 2 to derive weather pin names.
# Prefer explicit columns if present; otherwise fall back to code_year.
survey_data_files <- reactive({
  ss <- mod_1_01_sample_api$selected_surveys()
  if (is.null(ss) || !nrow(ss)) return(character(0))

  for (nm in c("pin_id", "file_id", "pin", "file", "id")) {
    if (nm %in% names(ss)) {
      v <- unique(as.character(ss[[nm]]))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) return(v)
    }
  }

  if (all(c("code","year") %in% names(ss))) return(unique(paste0(ss$code, "_", ss$year)))
  if ("code" %in% names(ss)) return(unique(as.character(ss$code)))

  character(0)
})

# Minimal Step1Spec exported to Step 2/3: outcome + hazard choices + survey files.
# This is intentionally small; Step 2 still has shims for backward-compatibility.
step1_spec <- reactive({
  so <- mod_1_03_outcome_api$selected_outcome()
  wx <- mod_1_04_weather_api$selected_weather()
  files <- survey_data_files()

  povline <- NA_real_
  units <- NA_character_
  transform <- NA_character_
  if (!is.null(so) && is.data.frame(so) && nrow(so)) {
    if ("povline" %in% names(so)) povline <- suppressWarnings(as.numeric(so$povline[[1]]))
    if ("units" %in% names(so)) units <- as.character(so$units[[1]])
    if ("transform" %in% names(so)) transform <- as.character(so$transform[[1]])
  }

  haz_vars <- character(0)
  haz_spec <- tibble::tibble()
  if (!is.null(wx) && is.data.frame(wx) && nrow(wx) && "name" %in% names(wx)) {
    haz_vars <- paste0("haz_", as.character(wx$name))
    haz_vars <- haz_vars[!is.na(haz_vars) & nzchar(haz_vars)]
    haz_vars <- unique(haz_vars)

    # Step 2 hazard spec schema (names used by Phase B hazard construction)
    haz_spec <- dplyr::transmute(
      wx,
      varname         = as.character(.data$name),
      ref_start       = as.numeric(.data$ref_start),
      ref_end         = as.numeric(.data$ref_end),
      temporalAgg     = as.character(.data$temporalAgg),
      varConstruction = as.character(.data$transformation),
      contOrBinned    = as.character(.data$cont_binned)
    )

    # Optional fields (not required by Phase B, but useful for UI/diagnostics)
    if ("polynomial" %in% names(wx)) haz_spec$polynomial <- as.integer(wx$polynomial)
    if ("num_bins" %in% names(wx)) haz_spec$num_bins <- suppressWarnings(as.integer(wx$num_bins))
    if ("binning_method" %in% names(wx)) haz_spec$binning_method <- as.character(wx$binning_method)
  }

  list(
    spec_version     = "v2026-02-22-passA",
    outcome          = so,
    povline_level    = povline,
    units            = units,
    transform        = transform,
    hazard           = list(selected = wx, haz_spec = haz_spec, haz_vars = haz_vars),
    survey_data_files = files
  )
})



# Convenience wrappers for the Step 2 v0 contract (prefer Step 1-native exports over Step 2 shims)
haz_spec <- reactive({
  spec <- step1_spec()
  hs <- tryCatch(spec$hazard$haz_spec, error = function(e) NULL)
  if (is.null(hs)) tibble::tibble() else hs
})

haz_vars <- reactive({
  spec <- step1_spec()
  hv <- tryCatch(spec$hazard$haz_vars, error = function(e) NULL)
  if (is.null(hv)) character(0) else hv
})

final_model <- reactive({
  mf <- mod_1_07_results_api$model_fit()
  if (is.null(mf)) return(NULL)

  # unwrap common list-of-fits pattern
  if (is.list(mf) && any(c("fit3","fit2","fit1") %in% names(mf))) {
    for (nm in c("fit3","fit2","fit1")) {
      if (!is.null(mf[[nm]])) { mf <- mf[[nm]]; break }
    }
  }

  if (requireNamespace("parsnip", quietly = TRUE)) {
    eng <- tryCatch(parsnip::extract_fit_engine(mf), error = function(e) NULL)
    if (!is.null(eng)) return(eng)
  }
  if (is.list(mf) && !is.null(mf$fit)) return(mf$fit)

  mf
})

unit_frame <- reactive({
  sd <- mod_1_02_surveystats_api$survey_data()
  if (is.null(sd) || !nrow(sd)) return(NULL)

  # Standardize a unit_id column (Step 2 uses this as the stable panel key)
  if (!"unit_id" %in% names(sd)) {
    unit_cands <- c("hhid","hh_key","household_id","hh_id","case_id","case_uuid","uuid")
    unit_col <- unit_cands[unit_cands %in% names(sd)][1]
    if (!is.na(unit_col) && nzchar(unit_col)) sd$unit_id <- as.character(sd[[unit_col]]) else sd$unit_id <- as.character(seq_len(nrow(sd)))
  } else {
    sd$unit_id <- as.character(sd$unit_id)
  }

  mod <- final_model()
  predictors <- character(0)
  if (!is.null(mod)) {
    f <- tryCatch(stats::formula(mod), error = function(e) NULL)
    if (!is.null(f)) {
      vars <- all.vars(f)
      resp <- as.character(f[[2]])
      predictors <- setdiff(vars, resp)
    }
  }
  predictors <- setdiff(predictors, grep("^haz_", predictors, value = TRUE))
  predictors <- setdiff(predictors, c("timestamp","svy_timestamp","month","sim_year","loc_id","location_id"))

  wt_cands <- c("weight","wgt","hh_weight","final_weight","sampling_weight")
  wt_col <- wt_cands[wt_cands %in% names(sd)][1]

  keep <- unique(c("unit_id", predictors, wt_col))
  keep <- intersect(keep, names(sd))

  uf <- sd[, keep, drop = FALSE]
  uf <- dplyr::distinct(uf, .data$unit_id, .keep_all = TRUE)

  haz_cols <- grep("^haz_", names(uf), value = TRUE)
  if (length(haz_cols)) uf[haz_cols] <- NULL

  uf
})
# NEW: export Step 1 API for Step 2 / Step 3
    list(
      # Nested APIs (for direct access if needed)
      sample_api      = mod_1_01_sample_api,
      surveystats_api = mod_1_02_surveystats_api,
      outcome_api     = mod_1_03_outcome_api,
      weather_api     = mod_1_04_weather_api,
      model_api       = mod_1_06_model_api,
      results_api     = mod_1_07_results_api,

      # Flattened selections (what Step 2 / Step 3 will use)
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      selected_model   = mod_1_06_model_api$selected_model,

      survey_data       = mod_1_02_surveystats_api$survey_data,
      survey_h3         = mod_1_02_surveystats_api$survey_h3,
      survey_geo        = mod_1_02_surveystats_api$survey_geo,
      survey_weather    = mod_1_05_weatherstats_api$survey_weather,
      survey_data_files  = survey_data_files,
      step1_spec         = step1_spec,

      # Step 2 v0 contract exports (avoid reliance on Step 2 shims)
      haz_spec           = haz_spec,
      haz_vars           = haz_vars,
      unit_frame         = unit_frame,
      final_model        = final_model,

      model_fit         = mod_1_07_results_api$model_fit
      # final_model       = final_model
    )
  })
}