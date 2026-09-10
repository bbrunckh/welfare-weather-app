# ============================================================================ #
# Pure functions translating a `build_selected_model()` spec into the labelled #
# rows of the "Selected model" card (Results / Model fit tabs): outcome,       #
# weather terms, covariates, fixed effects, standard errors.                   #
# Stateless and testable without Shiny.                                        #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Labels                                                                        #
# ---------------------------------------------------------------------------- #

#' Badge text for the model card: model type and fitting engine
#'
#' @param selected_model Named list from `build_selected_model()`.
#'
#' @return e.g. `"Linear regression \u00B7 OLS"`, `"Logistic regression
#'   \u00B7 logit"`, `"Random forest \u00B7 ranger"`.
#'
#' @export
model_badge <- function(selected_model) {
  type   <- as.character(selected_model$type[1] %||% "")
  engine <- as.character(selected_model$engine[1] %||% "fixest")
  eng <- switch(engine,
    fixest = switch(type,
      "Linear regression"   = "OLS",
      "Logistic regression" = "logit",
      engine),
    engine
  )
  paste0(type, " \u00B7 ", eng)
}

#' Plain-language clustering phrase for the standard-errors row
#'
#' @param cluster Scalar character cluster column name, or empty/NULL for
#'   unclustered.
#'
#' @return e.g. `"clustered by location panel"` or `"unclustered"`.
#'
#' @export
model_cluster_phrase <- function(cluster) {
  cl <- as.character(cluster[1])
  if (length(cl) == 0 || is.na(cl) || !nzchar(cl)) return("unclustered")
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
    selected_model[c("hh_covariates", "area_covariates", "ind_covariates",
                     "firm_covariates")],
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


# ---------------------------------------------------------------------------- #
# Card assembly                                                                 #
# ---------------------------------------------------------------------------- #

#' Assemble the "Selected model" card rows
#'
#' UI-56: this was a single formula line -
#' `outcome ~ weather + N covariates | year FE | clustered` - which packed the
#' whole specification into one dense string. The fixed-effect segment in
#' particular was unreadable: a middle `|` section whose only clue was a
#' trailing " FE". It now emits the same labelled `name` / `sub` / `pills`
#' rows the Sample, Outcome and Weather cards use, so every Step 1 card reads
#' the same way and each part of the model names itself.
#'
#' Model type and engine remain the card's badge row.
#'
#' @param selected_model Named list from `build_selected_model()`.
#' @param label_fun Optional function mapping variable names to display
#'   labels.
#' @param outcome_label Optional outcome label.
#' @param weather_labels Optional character vector of weather variable labels
#'   ("Monthly ..." prefixes are stripped).
#'
#' @return A list of row specs for `selection_summary_card()`.
#'
#' @export
model_card_rows <- function(selected_model, label_fun = NULL,
                            outcome_label = NULL,
                            weather_labels = character(0)) {
  sm <- selected_model

  to_lab <- function(nms) {
    nms <- unlist(nms)
    if (!length(nms)) return(character(0))
    if (is.null(label_fun)) return(as.character(nms))
    vapply(nms, function(v) {
      l <- label_fun(v)
      if (length(l) > 0 && !is.na(l[1]) && nzchar(l[1])) l[1] else v
    }, character(1), USE.NAMES = FALSE)
  }

  wx  <- sub("^Monthly\\s+", "", as.character(weather_labels))
  wx  <- wx[!is.na(wx) & nzchar(wx)]
  mods <- to_lab(sm$interactions)
  mode <- as.character(sm$interaction_mode[1] %||% "pairwise")
  fe   <- to_lab(sm$fixedeffects)
  counts <- model_covariate_counts(sm)
  n_cov  <- sum(counts)

  # Weather terms: crossed with the moderators when interactions are set.
  # Saturated mode crosses each weather with the full moderator set; pairwise
  # mode generates one term per weather x moderator pair.
  wx_terms <- if (length(mods) && length(wx)) {
    if (identical(mode, "saturated")) {
      vapply(wx, function(w) paste0(w, " \u00D7 ", paste(mods, collapse = " \u00D7 ")),
             character(1))
    } else {
      as.vector(outer(wx, mods, function(a, b) paste0(a, " \u00D7 ", b)))
    }
  } else {
    wx
  }
  wx_terms <- as.character(wx_terms)

  rows <- list()

  # UI-66: label first, like every other row. This read
  # "Poor (welfare < poverty line) outcome" - the value bolded and the label
  # trailing it - which inverted the order of the rows around it.
  rows[[length(rows) + 1L]] <- list(
    name  = "Outcome",
    sub   = NULL,
    pills = if (!is.null(outcome_label) && nzchar(outcome_label)) outcome_label
  )

  rows[[length(rows) + 1L]] <- list(
    name  = if (length(wx_terms)) "Weather terms" else "No weather terms",
    sub   = if (length(mods)) {
      paste0("interacted with ", paste(mods, collapse = ", "))
    },
    pills = if (length(wx_terms)) wx_terms
  )

  # UI-67: interaction moderators are covariates. `build_formulas()` puts
  # `terms$interactions_main` on the right-hand side of models 2 and 3, so a
  # variable locked in by a policy scenario is estimated as a main effect
  # exactly like a chosen control - and the card said "No covariates" while
  # the model was fitting one. They are counted here and named in the pills,
  # so it is still clear which came from the policy lock rather than from a
  # covariate selection.
  #
  # Spelled out by role: "7 covariates" said nothing about what they were.
  mod_labels <- unique(mods)
  n_total <- n_cov + length(mod_labels)
  rows[[length(rows) + 1L]] <- list(
    name  = if (n_total > 0) {
      paste0(n_total, " covariate", if (n_total != 1L) "s")
    } else "No covariates",
    sub   = if (n_cov > 0) {
      as.character(sm$covariate_selection[1] %||% "User-defined")
    },
    pills = c(
      if (n_cov > 0) paste0(unname(counts), " ", names(counts)),
      if (length(mod_labels)) paste0(mod_labels, " (moderator)")
    )
  )

  # The row the formula line hid: named, and explicit when there are none.
  rows[[length(rows) + 1L]] <- list(
    name  = if (length(fe)) "Fixed effects" else "No fixed effects",
    sub   = if (length(fe)) "absorbed",
    pills = if (length(fe)) fe
  )

  rows[[length(rows) + 1L]] <- list(
    name  = "Standard errors",
    sub   = model_cluster_phrase(sm$cluster),
    pills = NULL
  )

  rows
}
