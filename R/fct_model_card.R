# Model card helpers ----
# Pure functions translating a `build_selected_model()` spec into a concise     #
# model-card equation for the sidebar, Results, and Model fit tabs.              #
# Stateless and testable without Shiny.                                        #


# Labels ----

#' Badge text for the model card: model type and fitting engine
#'
#' @param selected_model Named list from `build_selected_model()`.
#'
#' @return e.g. `"Linear regression \u00B7 OLS"`, `"Logistic regression
#'   \u00B7 logit"`, `"Random forest \u00B7 ranger"`.
#'
#' @export
model_badge <- function(selected_model) {
  type <- as.character(selected_model$type[1] %||% "")
  engine <- as.character(selected_model$engine[1] %||% "fixest")
  eng <- switch(engine,
    fixest = switch(type,
      "Linear regression"   = "OLS",
      "Logistic regression" = "logit",
      engine
    ),
    engine
  )
  paste0(type, " \u00B7 ", eng)
}

#' Plain-language clustering phrase for the formula tail
#'
#' @param cluster Scalar character cluster column name, or empty/NULL for
#'   unclustered.
#'
#' @return e.g. `"clustered by location panel"` or `"unclustered"`.
#'
#' @export
model_cluster_phrase <- function(cluster) {
  cl <- as.character(cluster[1])
  if (length(cl) == 0 || is.na(cl) || !nzchar(cl)) {
    return("unclustered")
  }
  paste0("clustered by ", switch(cl,
    loc_id_panel = "location panel",
    loc_id       = "location",
    cl
  ))
}

#' Count of model covariates by role
#'
#' @param selected_model Named list from `build_selected_model()`.
#'
#' @return Named integer vector (household, area, individual, firm) with only
#'   non-zero entries.
#'
#' @export
model_covariate_counts <- function(selected_model) {
  n <- vapply(
    selected_model[c(
      "hh_covariates", "area_covariates", "ind_covariates",
      "firm_covariates"
    )],
    function(x) length(unlist(x)), integer(1)
  )
  n <- n[n > 0L]
  names(n) <- c("household", "area", "individual", "firm")[seq_along(n)]
  n[n > 0]
}

#' Total number of covariates in the model spec
#' @noRd
model_covariate_total <- function(selected_model) {
  sum(model_covariate_counts(selected_model))
}

#' Compact covariate metadata for the model-card badge
#'
#' @param selected_model Named list from `build_selected_model()`.
#' @return The covariate selection method, e.g. `"Lasso"`.
#' @export
model_covariate_badge <- function(selected_model) {
  method <- as.character(selected_model$covariate_selection[1] %||% "User-defined")
  method
}


# Card assembly ----

#' Assemble the concise "Selected model" card equation
#'
#' The equation uses blue pills for selected values. The model type is shown at
#' the start of the equation and covariate metadata lives in the card header.
#'
#' @param selected_model Named list from `build_selected_model()`.
#' @param label_fun Optional function mapping variable names to display
#'   labels.
#' @param outcome_label Optional outcome label (left-hand side).
#' @param weather_labels Optional character vector of weather variable labels
#'   ("Monthly ..." prefixes are stripped).
#' @param include_model_type Logical; include the model type at the start of the
#'   equation. Set to `FALSE` when it is already used as the card title.
#'
#' @return A list containing one pre-built row tag for `selection_summary_card()`.
#'
#' @export
model_card_rows <- function(selected_model, label_fun = NULL,
                            outcome_label = NULL,
                            weather_labels = character(0),
                            include_model_type = TRUE) {
  sm <- selected_model

  to_lab <- function(nms) {
    nms <- unlist(nms)
    if (!length(nms)) {
      return(character(0))
    }
    if (is.null(label_fun)) {
      return(as.character(nms))
    }
    vapply(nms, function(v) {
      l <- label_fun(v)
      if (length(l) > 0 && !is.na(l[1]) && nzchar(l[1])) l[1] else v
    }, character(1), USE.NAMES = FALSE)
  }

  wx <- wise_label_short(weather_labels)
  wx <- wx[!is.na(wx) & nzchar(wx)]
  mods <- to_lab(sm$interactions)
  n_cov <- model_covariate_total(sm)
  n_fe <- length(unlist(sm$fixedeffects))
  pill <- function(value) shiny::tags$span(class = "selection-card-pill", value)
  op <- function(value) shiny::tags$span(class = "selection-card-op", value)
  append_values <- function(kids, values, separator = "+") {
    for (i in seq_along(values)) {
      if (i > 1L) kids <- c(kids, list(op(separator)))
      kids <- c(kids, list(pill(values[[i]])))
    }
    kids
  }

  outcome <- if (is.null(outcome_label) || !nzchar(outcome_label)) {
    "Selected outcome"
  } else {
    outcome_label
  }
  if (!length(wx)) wx <- "Selected weather"

  equation <- list()
  if (isTRUE(include_model_type)) {
    equation <- c(equation, list(shiny::tags$span(
      class = "model-summary-equation-type selection-card-name",
      as.character(sm$type[1] %||% "Selected model")
    )))
  }
  equation <- c(equation, list(pill(outcome), op("~")))
  equation <- append_values(equation, wx)
  if (length(mods)) {
    equation <- c(equation, list(op("\u00D7")))
    equation <- append_values(equation, mods, separator = "\u00D7")
  }
  if (n_cov > 0L) {
    equation <- c(equation, list(op("+"), pill(
      paste(n_cov, if (n_cov == 1L) "covariate" else "covariates")
    )))
  }
  if (n_fe > 0L) {
    equation <- c(equation, list(op("+"), pill(paste(n_fe, "FEs"))))
  }

  list(shiny::tags$div(
    class = "selection-card-row model-summary-row model-summary-equation",
    do.call(htmltools::tagList, equation)
  ))
}
