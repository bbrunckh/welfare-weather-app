# =========================================================================== #
# Run provenance: one immutable record of what produced a set of results.     #
#                                                                             #
# UI-49. An exported table carried no record of what produced it - not        #
# the data source, the model specification, the random seed, or which         #
# run it came from - so a CSV could not be traced back to the analysis        #
# that generated it.                                                          #
#                                                                             #
# Every step already stores an immutable snapshot with its result:            #
# `.snap` (fit-time labels and settings, INT-05) and `.sig` (the run          #
# signature the stale banners compare against, INT-08).                       #
# `wise_provenance()` reads those, never the live inputs, so the record       #
# describes the run that produced the results rather than whatever is         #
# selected when it is read. It is written into the export bundle's            #
# metadata (see `wise_export_readme()`), which is what makes an               #
# exported file traceable to a specific run.                                  #
#                                                                             #
# There is deliberately no on-screen provenance banner: the same              #
# information travels with the export, and a permanent panel above            #
# every result surface cost more space than it earned.                        #
# =========================================================================== #


#' Strip credentials from a connection-parameter list
#'
#' Provenance must record *which* source the data came from without ever
#' carrying the means to read it: these records are written to disk and shared.
#' Anything key-shaped is replaced with a presence marker.
#'
#' @param params Connection parameter list from `mod_0_overview_server()`.
#' @return A named list safe to serialise.
#' @noRd
.provenance_source <- function(params) {
  if (is.null(params) || !length(params)) {
    return(list(type = "unknown"))
  }
  secret_like <- "secret|key|token|password|credential|client_id|tenant"
  keep <- names(params)[!grepl(secret_like, names(params), ignore.case = TRUE)]
  out <- lapply(params[keep], function(v) {
    if (is.null(v)) return(NULL)
    if (is.atomic(v) && length(v) <= 8) v else paste0("<", class(v)[1], ">")
  })
  out <- Filter(Negate(is.null), out)
  # Record that credentials were supplied, never what they were.
  redacted <- setdiff(names(params), keep)
  if (length(redacted)) {
    out$credentials <- paste0("redacted (", length(redacted), " field(s) set)")
  }
  out
}

#' Short, stable digest of a run signature
#'
#' The full signature is a nested list of every input a run consumed; a hash of
#' it is what makes two runs comparable at a glance ("these tables came from
#' run a1b2c3d4") without reproducing the whole structure in a banner.
#'
#' @param sig A run-signature list, or NULL.
#' @return An 8-character hex string, or NA when no signature is available.
#' @noRd
.provenance_digest <- function(sig) {
  if (is.null(sig)) return(NA_character_)
  tryCatch(
    substr(digest::digest(.sig_plain(sig), algo = "xxhash64"), 1L, 8L),
    error = function(e) NA_character_
  )
}

#' Describe a model specification in one line
#'
#' @param sm A `build_selected_model()` list.
#' @param label_fun Optional function mapping variable names to labels.
#' @return A single string, or NA when no specification is available.
#' @noRd
.provenance_model_spec <- function(sm, label_fun = identity) {
  if (is.null(sm) || !length(sm)) return(NA_character_)
  lab <- function(x) {
    if (!length(x)) return(character(0))
    vapply(x, function(v) {
      l <- tryCatch(label_fun(v), error = function(e) v)
      if (length(l) == 1 && !is.na(l) && nzchar(l)) l else v
    }, character(1))
  }
  covs <- unique(c(sm$ind_covariates, sm$hh_covariates,
                   sm$firm_covariates, sm$area_covariates))
  parts <- c(
    sm$type %||% NA_character_,
    if (length(sm$engine)) paste0("engine: ", sm$engine),
    if (length(sm$interactions))
      paste0("interaction: ", paste(lab(sm$interactions), collapse = ", ")),
    if (length(sm$fixedeffects))
      paste0("fixed effects: ", paste(lab(sm$fixedeffects), collapse = ", ")),
    paste0("covariates: ",
           if (length(covs)) paste(lab(covs), collapse = ", ") else "none"),
    if (length(sm$covariate_selection))
      paste0("selection: ", sm$covariate_selection),
    if (length(sm$cluster))
      paste0("clustered SEs: ", paste(sm$cluster, collapse = ", "))
  )
  paste(Filter(function(x) !is.na(x) && nzchar(x), parts), collapse = " · ")
}

#' Build an immutable provenance record for one step's results
#'
#' @param step   Integer step number.
#' @param result The stored result object (carries `.sig` and, for Step 1,
#'   `.snap`). NULL when the step has not run.
#' @param connection_params Connection parameters for the data source.
#' @param seed   Base random seed.
#' @param extra  Named list of step-specific fields to append.
#' @param label_fun Optional variable-name to label mapping.
#'
#' @return A named list, or NULL when the step has produced no results.
#' @noRd
wise_provenance <- function(step, result, connection_params = NULL,
                            seed = WISEAPP_DEFAULT_SEED, extra = list(),
                            label_fun = identity) {
  if (is.null(result)) return(NULL)

  snap <- result$.snap %||% list()
  sig  <- result$.sig

  outcome <- snap$outcome %||% sig$outcome %||% NULL
  weather <- snap$weather %||% sig$weather %||% NULL

  one_line <- function(x, field = "label") {
    if (is.null(x)) return(NA_character_)
    v <- if (is.data.frame(x)) x[[field]] else x[[field]] %||% x
    if (is.null(v) || !length(v)) return(NA_character_)
    paste(as.character(v), collapse = ", ")
  }

  rec <- list(
    step           = as.integer(step),
    step_label     = .export_step_label(step),
    run_signature  = .provenance_digest(sig),
    random_seed    = as.integer(seed),
    app_version    = tryCatch(as.character(golem::get_golem_version()),
                              error = function(e) NA_character_),
    source         = .provenance_source(connection_params),
    survey_version = sig$survey_version %||% NA,
    outcome        = one_line(outcome),
    weather        = one_line(weather),
    model_spec     = .provenance_model_spec(
      snap$model %||% sig$model %||% result$selected_model, label_fun
    ),
    engine         = result$engine %||% NA_character_,
    n_observations = tryCatch({
      sw <- snap$survey_weather
      if (is.null(sw)) NA_integer_ else nrow(sw)
    }, error = function(e) NA_integer_)
  )

  # Record any specification fallback the fitter applied (REACT-14): a result
  # whose fitted specification differs from the requested one must say so
  # wherever it is described, exports included.
  fb <- result$fallbacks %||% list()
  if (length(fb)) {
    rec$fallbacks <- vapply(fb, function(x) {
      sprintf("%s: requested %s, fitted %s (%s)",
              x$kind %||% "spec", x$requested %||% "?", x$used %||% "?",
              x$reason %||% "")
    }, character(1))
  }

  c(rec, extra)
}

#' Read connection parameters without blocking on them
#'
#' `connection_params` req()s internally before a source is chosen, and
#' provenance must degrade rather than error. Redaction is deliberately *not*
#' done here - `wise_provenance()` owns that, so there is exactly one place
#' where credentials are stripped.
#'
#' @param cp_reactive Reactive returning the connection parameter list.
#' @return The parameter list, or NULL.
#' @noRd
read_connection_params <- function(cp_reactive) {
  tryCatch(
    if (is.function(cp_reactive)) cp_reactive() else cp_reactive,
    error = function(e) NULL
  )
}
