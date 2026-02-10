# ACTIVE FILE: utils_mod_2_helpers.R
# VERSION_TAG: step2_contracts_v2026_02_10

# ------------------------------------------------------------------------------
# Debug flag
# ------------------------------------------------------------------------------
wise_debug_enabled <- function() {
  isTRUE(getOption("wise.debug", FALSE))
}

# ------------------------------------------------------------------------------
# Small safe helpers
# ------------------------------------------------------------------------------
safe_nrow <- function(x) {
  tryCatch(if (is.data.frame(x)) nrow(x) else NA_integer_, error = function(e) NA_integer_)
}

safe_n_distinct <- function(x) {
  tryCatch(dplyr::n_distinct(x), error = function(e) NA_integer_)
}

# ------------------------------------------------------------------------------
# Contract checks: Step 1 -> Step 2
# ------------------------------------------------------------------------------

# Returns list(ok, msg, missing, not_fun)
check_step1_contract_step2 <- function(step1, require_weather_api = TRUE) {
  if (is.null(step1)) {
    return(list(
      ok = FALSE,
      msg = "FAIL | step1 is NULL",
      missing = character(),
      not_fun = character()
    ))
  }

  needed <- c("survey_weather", "haz_spec", "haz_vars", "final_model", "survey_data_files")
  missing <- needed[!needed %in% names(step1)]

  present <- needed[needed %in% names(step1)]
  not_fun <- present[!vapply(step1[present], is.function, logical(1))]

  # Optional nested dependency used by historical hazard build
  has_weather_api <- TRUE
  if (require_weather_api) {
    has_weather_api <- !is.null(step1$weather_api) &&
      is.list(step1$weather_api) &&
      "survey_h3" %in% names(step1$weather_api) &&
      is.function(step1$weather_api$survey_h3)
  }

  ok <- length(missing) == 0 && length(not_fun) == 0 && has_weather_api

  msg <- paste0(
    if (ok) "OK" else "FAIL",
    " | missing: ", if (length(missing)) paste(missing, collapse = ", ") else "<none>",
    " | not functions: ", if (length(not_fun)) paste(not_fun, collapse = ", ") else "<none>",
    if (require_weather_api) paste0(" | weather_api$survey_h3: ", if (has_weather_api) "OK" else "MISSING") else ""
  )

  list(ok = ok, msg = msg, missing = missing, not_fun = not_fun)
}

# Convenience wrapper that errors (useful in compute helpers)
assert_step1_contract_step2 <- function(step1, require_weather_api = TRUE) {
  ck <- check_step1_contract_step2(step1, require_weather_api = require_weather_api)
  if (!isTRUE(ck$ok)) {
    rlang::abort(paste0("Step 1 -> Step 2 contract failed: ", ck$msg))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Contract checks: board/pins
# ------------------------------------------------------------------------------

check_pin_board <- function(board) {
  if (is.null(board)) {
    return(list(ok = FALSE, msg = "FAIL | board is NULL (not passed from app_server)"))
  }
  # pins boards are R6-ish; don't over-validate, just sanity check class and basic call
  ok <- TRUE
  msg <- "OK"
  tryCatch({
    cls <- class(board)
    msg <- paste0("OK | board class: ", paste(cls, collapse = ", "))
  }, error = function(e) {
    ok <<- FALSE
    msg <<- paste0("FAIL | board object error: ", conditionMessage(e))
  })
  list(ok = ok, msg = msg)
}

# ------------------------------------------------------------------------------
# Diagnostic utilities (keep “merge sanity” out of main module code)
# ------------------------------------------------------------------------------

check_required_cols <- function(df, cols, context = "") {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(ok = FALSE, msg = paste0("FAIL | not a data.frame | ", context)))
  }
  missing <- setdiff(cols, names(df))
  ok <- length(missing) == 0
  msg <- paste0(
    if (ok) "OK" else "FAIL",
    " | missing cols: ", if (length(missing)) paste(missing, collapse = ", ") else "<none>",
    if (nzchar(context)) paste0(" | ", context) else ""
  )
  list(ok = ok, msg = msg, missing = missing)
}

check_unique_key <- function(df, keys, context = "", n_show = 20) {
  ck <- check_required_cols(df, keys, context = context)
  if (!isTRUE(ck$ok)) return(ck)

  dup <- df |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") |>
    dplyr::filter(.data$n > 1) |>
    dplyr::arrange(dplyr::desc(.data$n))

  ok <- nrow(dup) == 0
  msg <- paste0(
    if (ok) "OK" else "FAIL",
    " | duplicate key rows: ", nrow(dup),
    if (nzchar(context)) paste0(" | ", context) else ""
  )

  examples <- if (!ok) utils::head(dup, n_show) else NULL
  list(ok = ok, msg = msg, examples = examples)
}

# Compare rowcounts pre/post join and coverage of non-missing joined columns
check_join_coverage <- function(before_df, after_df, key_cols, joined_cols, context = "", n_show = 10) {
  ck1 <- check_required_cols(before_df, key_cols, context = paste0(context, " | before"))
  ck2 <- check_required_cols(after_df,  key_cols, context = paste0(context, " | after"))
  if (!isTRUE(ck1$ok) || !isTRUE(ck2$ok)) {
    return(list(ok = FALSE, msg = paste(ck1$msg, ck2$msg, sep = " || ")))
  }

  n_before <- safe_nrow(before_df)
  n_after  <- safe_nrow(after_df)

  ck3 <- check_required_cols(after_df, joined_cols, context = paste0(context, " | joined"))
  if (!isTRUE(ck3$ok)) return(list(ok = FALSE, msg = ck3$msg))

  cov <- after_df |>
    dplyr::summarise(dplyr::across(dplyr::all_of(joined_cols), ~ mean(!is.na(.x)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "col", values_to = "share_non_missing") |>
    dplyr::arrange(.data$share_non_missing)

  ok <- TRUE
  msg <- paste0(
    "OK | rows before: ", n_before,
    " | rows after: ", n_after,
    if (nzchar(context)) paste0(" | ", context) else ""
  )

  list(ok = ok, msg = msg, coverage = cov, coverage_head = utils::head(cov, n_show))
}

summarize_missingness <- function(df, cols, by = NULL, n_show = 15) {
  ck <- check_required_cols(df, cols, context = "summarize_missingness")
  if (!isTRUE(ck$ok)) return(NULL)

  if (is.null(by)) {
    out <- df |>
      dplyr::summarise(dplyr::across(dplyr::all_of(cols), ~ mean(is.na(.x)))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "col", values_to = "share_na") |>
      dplyr::arrange(dplyr::desc(.data$share_na))
    return(utils::head(out, n_show))
  }

  # grouped missingness
  ck_by <- check_required_cols(df, by, context = "summarize_missingness(by=...)")
  if (!isTRUE(ck_by$ok)) return(NULL)

  out <- df |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), ~ mean(is.na(.x))),
      .by = dplyr::all_of(by)
    )
  out
}

# ------------------------------------------------------------------------------
# Small status formatter (optional)
# ------------------------------------------------------------------------------
format_step2_status <- function(step1, board = NULL) {
  ck1 <- check_step1_contract_step2(step1, require_weather_api = FALSE)
  ckB <- check_pin_board(board)

  has_model <- tryCatch(!is.null(step1$final_model()), error = function(e) FALSE)
  n_sw <- tryCatch(nrow(step1$survey_weather()), error = function(e) NA_integer_)
  n_haz <- tryCatch(length(step1$haz_vars()), error = function(e) NA_integer_)

  paste0(
    "[Contract] ", ck1$msg, "\n",
    "[Board] ", ckB$msg, "\n",
    "Model available: ", if (has_model) "YES" else "NO", " | ",
    "survey_weather rows: ", n_sw, " | ",
    "haz vars: ", n_haz
  )
}
