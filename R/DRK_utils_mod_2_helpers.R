#DRK Version 20260220 1900


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

  needed <- c("step1_spec", "survey_weather", "survey_data", "unit_frame", "haz_spec", "haz_vars", "final_model", "selected_outcome", "survey_data_files")
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

check_step1_contract_step2_content <- function(step1, require_weather_api = TRUE) {
  # First, structural
  ck <- check_step1_contract_step2(step1, require_weather_api = require_weather_api)
  if (!isTRUE(ck$ok)) return(ck)

  problems <- character(0)

    # Unit-level base for Step 2 Phase C: prefer survey_weather(), else survey_data()
  sw <- tryCatch(step1$survey_weather(), error = function(e) e)
  base_nm <- "survey_weather"
  if (inherits(sw, "error") || is.null(sw) || !nrow(sw)) {
    sw <- tryCatch(step1$survey_data(), error = function(e) e)
    base_nm <- "survey_data"
  }

  if (inherits(sw, "error") || is.null(sw) || !nrow(sw)) {
    problems <- c(problems, "survey_weather() and survey_data() both returned NULL/empty or errored")
  } else {
    need <- c("loc_id", "timestamp")
    miss <- setdiff(need, names(sw))
    if (length(miss)) problems <- c(problems, paste0(base_nm, " missing: ", paste(miss, collapse = ", ")))

    unit_cands <- c("unit_id","hhid","hh_key","household_id","hh_id","case_id","case_uuid","uuid")
    if (!any(unit_cands %in% names(sw))) {
      problems <- c(problems, paste0(base_nm, " missing unit id column (one of: ", paste(unit_cands, collapse = ", "), ")"))
    }
  }

# unit_frame must have unique unit_id
  uf <- tryCatch(step1$unit_frame(), error = function(e) e)
  if (inherits(uf, "error") || is.null(uf) || !nrow(uf)) {
    problems <- c(problems, "unit_frame() returned NULL/empty or errored")
  } else {
    if (!"unit_id" %in% names(uf)) problems <- c(problems, "unit_frame missing unit_id")
    if (anyDuplicated(uf$unit_id)) problems <- c(problems, "unit_frame has duplicate unit_id values")
    # No hazards should be in unit_frame
    if (length(grep("^haz_", names(uf), value=TRUE))) problems <- c(problems, "unit_frame contains haz_* columns (should not)")
  }

  # haz_spec must have expected columns
  hs <- tryCatch(step1$haz_spec(), error = function(e) e)
  if (inherits(hs, "error") || is.null(hs) || !nrow(hs)) {
    problems <- c(problems, "haz_spec() returned NULL/empty or errored")
  } else {
    need <- c("varname","ref_start","ref_end","temporalAgg","varConstruction","contOrBinned")
    miss <- setdiff(need, names(hs))
    if (length(miss)) problems <- c(problems, paste0("haz_spec missing: ", paste(miss, collapse=", ")))
  }

  # final_model must be lm (or unwrap-able to lm by adapter)
  fm <- tryCatch(step1$final_model(), error = function(e) e)
  if (inherits(fm, "error") || is.null(fm)) {
    problems <- c(problems, "final_model() returned NULL/errored")
  } else if (!inherits(fm, "lm")) {
    problems <- c(problems, paste0("final_model is not lm (class=", paste(class(fm), collapse=", "), ")"))
  }

  ok <- length(problems) == 0
  msg <- if (ok) "OK" else paste0("FAIL | ", paste(problems, collapse=" || "))

  list(ok = ok, msg = msg, missing = ck$missing, not_fun = ck$not_fun, problems = problems)
}

# ------------------------------------------------------------------------------
# Step 1 -> Step 2 contract shim + phase-specific accessors (v0)
# ------------------------------------------------------------------------------
# These helpers serve two purposes:
# 1) A *shim* layer so Step 2 can tolerate small Step 1 refactors by mapping exports.
# 2) Centralized, phase-specific validation so Phase code stays clean and readable.

# Shim: adapt a Step 1 API object into the Step 2 v0 contract.
# For now this is mostly pass-through, but it is the single place to update once
# Module 1 exports change.
adapt_step1_to_step2_v0 <- function(step1_raw, require_weather_api = TRUE) {
  if (is.null(step1_raw)) return(NULL)

  # Allow caller to pass a reactive/function that returns the Step 1 exports list
  if (is.function(step1_raw) && !is.list(step1_raw)) {
    step1_raw_val <- tryCatch(step1_raw(), error = function(e) NULL)
    if (is.list(step1_raw_val)) step1_raw <- step1_raw_val
  }

  # Try common nesting patterns (e.g., step1$api, step1$exports)
  candidates <- list(
    step1_raw,
    step1_raw$api,
    step1_raw$exports,
    step1_raw$step1
  )
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]

  for (cand in candidates) {
    cand2 <- augment_step1_for_step2_v0(cand, require_weather_api = require_weather_api)
    chk <- check_step1_contract_step2(cand2, require_weather_api = require_weather_api)
    if (isTRUE(chk$ok)) return(cand2)
  }

  # Fall back: return augmented original; a later assert will error with details.
  augment_step1_for_step2_v0(step1_raw, require_weather_api = require_weather_api)
}

# Augment Step 1 exports to satisfy the Step 2 v0 contract.
# This keeps Step 2 resilient to Step 1 refactors. Prefer Step 1-native exports
# when available, otherwise derive reasonable fallbacks.
augment_step1_for_step2_v0 <- function(step1_obj, require_weather_api = TRUE) {
  if (is.null(step1_obj) || !is.list(step1_obj)) return(step1_obj)

  # ---- helpers ----
add_unit_id <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

  if ("unit_id" %in% names(df)) {
    df$unit_id <- as.character(df$unit_id)
    miss <- is.na(df$unit_id) | !nzchar(df$unit_id)
    if (any(miss)) df$unit_id[miss] <- paste0("tmp_", which(miss))
    return(df)
  }

  unit_cands <- c("hhid","hh_key","household_id","hh_id","case_id","case_uuid","uuid")
  unit_col <- unit_cands[unit_cands %in% names(df)][1]

  if (is.na(unit_col) || !nzchar(unit_col)) {
    df$unit_id <- paste0("tmp_", seq_len(nrow(df)))
  } else {
    df$unit_id <- as.character(df[[unit_col]])
    miss <- is.na(df$unit_id) | !nzchar(df$unit_id)
    if (any(miss)) df$unit_id[miss] <- paste0("tmp_", which(miss))
  }

  df
}

  # Wrap a non-function export (e.g., already computed data.frame) as a thunk.
  wrap_value_as_fun <- function(x) {
    force(x)
    function() x
  }

  # Normalize Step 1 exports: allow either reactives (functions) or already-realized values.
  normalize_export_fun <- function(obj, nm) {
    if (!nm %in% names(obj)) return(obj)
    if (!is.function(obj[[nm]])) obj[[nm]] <- wrap_value_as_fun(obj[[nm]])
    obj
  }

  extract_engine_model <- function(model_fit_obj) {
    # Works for parsnip model_fit objects and simple lists holding $fit
    if (is.null(model_fit_obj)) return(NULL)

    # If it's a list with fit3/fit2/fit1 (your new Module 1 pattern)
    if (is.list(model_fit_obj) && any(c("fit3","fit2","fit1") %in% names(model_fit_obj))) {
      for (nm in c("fit3","fit2","fit1")) {
        if (!is.null(model_fit_obj[[nm]])) {
          model_fit_obj <- model_fit_obj[[nm]]
          break
        }
      }
    }

    # parsnip model_fit typically supports extract_fit_engine()
    if (requireNamespace("parsnip", quietly = TRUE)) {
      eng <- tryCatch(parsnip::extract_fit_engine(model_fit_obj), error = function(e) NULL)
      if (!is.null(eng)) return(eng)
    }

    # fallback: common slot
    if (is.list(model_fit_obj) && !is.null(model_fit_obj$fit)) return(model_fit_obj$fit)

    # already an engine object?
    model_fit_obj
  }
  # If Step 1 provides some exports as values instead of functions, wrap them.
  for (nm in c(
    "survey_weather", "unit_frame", "haz_spec", "haz_vars", "final_model",
    "survey_data_files", "survey_data", "selected_weather", "selected_surveys",
    "model_fit", "survey_h3", "selected_outcome"
  )) {
    step1_obj <- normalize_export_fun(step1_obj, nm)
  }


  # ---- wrap/augment core exports ----

  # (A) Ensure survey_weather() returns unit_id (Step 2 expects unit_id for panel creation)
if ("survey_weather" %in% names(step1_obj) && is.function(step1_obj$survey_weather)) {
  if (!isTRUE(attr(step1_obj$survey_weather, "step2_wrapped"))) {
    .orig_sw <- step1_obj$survey_weather
    f <- function() add_unit_id(.orig_sw())
    attr(f, "step2_wrapped") <- TRUE
    step1_obj$survey_weather <- f
  }
}

  # (B) Prefer survey_data() for unit_frame() if available (clean separation: covariates/weights live here)
  if (!("unit_frame" %in% names(step1_obj)) || !is.function(step1_obj$unit_frame)) {
    if ("survey_data" %in% names(step1_obj) && is.function(step1_obj$survey_data)) {
      step1_obj$unit_frame <- function() {
        sd <- add_unit_id(step1_obj$survey_data())
        if (is.null(sd) || !nrow(sd)) stop("unit_frame: step1$survey_data() is NULL/empty.")

        # Determine predictors from the fitted model if available
        mod <- tryCatch(step1_obj$final_model(), error = function(e) NULL)
        predictors <- character(0)
        if (!is.null(mod)) {
          f <- tryCatch(stats::formula(mod), error = function(e) NULL)
          if (!is.null(f)) {
            vars <- all.vars(f)
            resp <- as.character(f[[2]])
            predictors <- setdiff(vars, resp)
          }
        }
        # No hazards inside unit_frame
        predictors <- setdiff(predictors, grep("^haz_", predictors, value = TRUE))
        # Never treat time/loc keys as predictors here
        predictors <- setdiff(predictors, c("timestamp","svy_timestamp","month","sim_year","loc_id","location_id"))

        wt_col <- tryCatch(detect_weight_col(sd, varlist = NULL), error = function(e) NULL)

        keep <- unique(c("unit_id", predictors, wt_col))
        keep <- intersect(keep, names(sd))

        uf <- sd[, keep, drop = FALSE]
        uf <- dplyr::distinct(uf, .data$unit_id, .keep_all = TRUE)

        # hard-remove haz_* if present for any reason
        haz_cols <- grep("^haz_", names(uf), value = TRUE)
        if (length(haz_cols)) uf[haz_cols] <- NULL

        uf
      }

    } else if ("survey_weather" %in% names(step1_obj) && is.function(step1_obj$survey_weather)) {
      # your existing fallback (kept), but now survey_weather is wrapped to include unit_id
      step1_obj$unit_frame <- function() {
        sw <- step1_obj$survey_weather()
        if (is.null(sw) || !nrow(sw)) stop("unit_frame fallback: step1$survey_weather() is NULL/empty.")

        mod <- tryCatch(step1_obj$final_model(), error = function(e) NULL)
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

        wt_col <- tryCatch(detect_weight_col(sw, varlist = NULL), error = function(e) NULL)
        keep <- unique(c("unit_id", predictors, wt_col))
        keep <- intersect(keep, names(sw))

        uf <- sw[, keep, drop = FALSE]
        uf <- dplyr::distinct(uf, .data$unit_id, .keep_all = TRUE)

        haz_cols <- grep("^haz_", names(uf), value = TRUE)
        if (length(haz_cols)) uf[haz_cols] <- NULL

        uf
      }
    } else {
      step1_obj$unit_frame <- function() stop("step1$unit_frame() missing and cannot be derived (survey_data/survey_weather unavailable).")
    }
  }



  # (C0) Prefer Step 1-native `step1_spec()` if available (single-source boundary object)
  if ("step1_spec" %in% names(step1_obj) && is.function(step1_obj$step1_spec)) {

    if (!("survey_data_files" %in% names(step1_obj)) || !is.function(step1_obj$survey_data_files)) {
      step1_obj$survey_data_files <- function() {
        spec <- step1_obj$step1_spec()
        files <- tryCatch(spec$survey_data_files, error = function(e) character(0))
        files <- as.character(files)
        files <- files[!is.na(files) & nzchar(files)]
        unique(files)
      }
    }

    if (!("haz_spec" %in% names(step1_obj)) || !is.function(step1_obj$haz_spec)) {
      step1_obj$haz_spec <- function() {
        spec <- step1_obj$step1_spec()
        hs <- tryCatch(spec$hazard$haz_spec, error = function(e) NULL)
        if (is.null(hs) || !is.data.frame(hs) || !nrow(hs)) {
          # fallback: derive from selected_weather()
          w <- tryCatch(step1_obj$selected_weather(), error = function(e) NULL)
          if (is.null(w) || !is.data.frame(w) || !nrow(w)) stop("haz_spec: step1_spec and selected_weather are NULL/empty.")
          hs <- dplyr::transmute(
            w,
            varname         = .data$name,
            ref_start       = as.numeric(.data$ref_start),
            ref_end         = as.numeric(.data$ref_end),
            temporalAgg     = as.character(.data$temporalAgg),
            varConstruction = as.character(.data$transformation),
            contOrBinned    = as.character(.data$cont_binned)
          )
        }
        hs
      }
    }

    if (!("haz_vars" %in% names(step1_obj)) || !is.function(step1_obj$haz_vars)) {
      step1_obj$haz_vars <- function() {
        spec <- step1_obj$step1_spec()
        hv <- tryCatch(spec$hazard$haz_vars, error = function(e) NULL)
        if (is.null(hv) || !length(hv)) {
          hs <- tryCatch(step1_obj$haz_spec(), error = function(e) NULL)
          if (is.null(hs) || !nrow(hs) || !"varname" %in% names(hs)) return(character(0))
          hv <- paste0("haz_", as.character(hs$varname))
        }
        unique(as.character(hv))
      }
    }

    if (!("selected_outcome" %in% names(step1_obj)) || !is.function(step1_obj$selected_outcome)) {
      step1_obj$selected_outcome <- function() {
        spec <- step1_obj$step1_spec()
        out <- tryCatch(spec$outcome, error = function(e) NULL)
        out
      }
    }
  }

  # (C) haz_spec() from selected_weather() (new Module 1 pattern)
  if (!("haz_spec" %in% names(step1_obj)) || !is.function(step1_obj$haz_spec)) {
    if ("selected_weather" %in% names(step1_obj) && is.function(step1_obj$selected_weather)) {
      step1_obj$haz_spec <- function() {
        w <- step1_obj$selected_weather()
        if (is.null(w) || !nrow(w)) stop("haz_spec: step1$selected_weather() is NULL/empty.")

        # Map Module 1 naming -> Step 2 naming
        out <- dplyr::transmute(
          w,
          varname         = .data$name,
          ref_start       = as.numeric(.data$ref_start),
          ref_end         = as.numeric(.data$ref_end),
          temporalAgg     = as.character(.data$temporalAgg),
          varConstruction = as.character(.data$transformation),
          contOrBinned    = as.character(.data$cont_binned)
        )
        out
      }
    }
  }

  # (D) haz_vars() from haz_spec()
  if (!("haz_vars" %in% names(step1_obj)) || !is.function(step1_obj$haz_vars)) {
    if ("haz_spec" %in% names(step1_obj) && is.function(step1_obj$haz_spec)) {
      step1_obj$haz_vars <- function() {
        hs <- step1_obj$haz_spec()
        if (is.null(hs) || !nrow(hs) || !"varname" %in% names(hs)) return(character(0))
        unique(paste0("haz_", as.character(hs$varname)))
      }
    }
  }

  # (E) final_model() from model_fit() (unwrap parsnip -> lm/glm engine)
  if (!("final_model" %in% names(step1_obj)) || !is.function(step1_obj$final_model)) {
    if ("model_fit" %in% names(step1_obj) && is.function(step1_obj$model_fit)) {
      step1_obj$final_model <- function() {
        mf <- step1_obj$model_fit()
        eng <- extract_engine_model(mf)
        if (is.null(eng)) stop("final_model: could not extract engine model from step1$model_fit().")
        eng
      }
    }
  }

  # (F) survey_data_files(): derive stable IDs from selected_surveys()
  if (!("survey_data_files" %in% names(step1_obj)) || !is.function(step1_obj$survey_data_files)) {
    if ("selected_surveys" %in% names(step1_obj) && is.function(step1_obj$selected_surveys)) {
      step1_obj$survey_data_files <- function() {
        ss <- step1_obj$selected_surveys()
        if (is.null(ss) || !nrow(ss)) return(character(0))

        if (all(c("code", "year") %in% names(ss))) return(unique(paste0(ss$code, "_", ss$year)))
        if ("code" %in% names(ss)) return(unique(as.character(ss$code)))
        if ("fpath" %in% names(ss)) return(unique(as.character(ss$fpath)))

        character(0)
      }
    }
  }

  # (G) weather_api wrapper: Step 2 Phase B expects weather_api$survey_h3()
  if (require_weather_api) {
    if (is.null(step1_obj$weather_api) || !is.list(step1_obj$weather_api) ||
        is.null(step1_obj$weather_api$survey_h3) || !is.function(step1_obj$weather_api$survey_h3)) {
      if ("survey_h3" %in% names(step1_obj) && is.function(step1_obj$survey_h3)) {
        step1_obj$weather_api <- list(
          survey_h3 = function() {
            df <- step1_obj$survey_h3()
            if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

            # Module 1 currently uses column name `h3` for the res-6 H3 index.
            # Step 2 expects `h3_6` (because weather pins store `h3_6`).
            if (!("h3_6" %in% names(df))) {
              if ("h3" %in% names(df)) {
                df$h3_6 <- df$h3
              } else if ("h3_07" %in% names(df)) {
                df$h3_6 <- df$h3_07
              } else {
                # last resort: pick the first h3-like column
                h3_like <- grep("^h3(_|$)", names(df), value = TRUE)
                if (length(h3_like)) df$h3_6 <- df[[h3_like[[1]]]]
              }
            }

            df$h3_6 <- as.character(df$h3_6)
            df
          }
        )
      }
    }
  }

  step1_obj
}

assert_has_cols <- function(df, cols, label = "data") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

assert_has_any_col <- function(df, cols, label = "data") {
  present <- intersect(cols, names(df))
  if (!length(present)) {
    stop(label, " is missing all candidate columns: ", paste(cols, collapse = ", "))
  }
  invisible(present[[1]])
}


# Phase B: inputs needed to build hazards (h3 -> loc + hazard construction)
step1_inputs_for_phase_b <- function(step1) {
  step1 <- adapt_step1_to_step2_v0(step1, require_weather_api = TRUE)
  assert_step1_contract_step2(step1, require_weather_api = TRUE)

  files <- step1$survey_data_files()
  if (is.null(files) || !length(files)) stop("step1$survey_data_files() returned 0 files.")

  survey_h3 <- step1$weather_api$survey_h3()
  if (is.null(survey_h3) || !nrow(survey_h3)) stop("step1$weather_api$survey_h3() is NULL/empty.")
  assert_has_cols(survey_h3, c("loc_id", "h3_6"), "step1$weather_api$survey_h3()")

  haz_spec <- step1$haz_spec()
  if (is.null(haz_spec) || !nrow(haz_spec)) stop("step1$haz_spec() is NULL/empty.")
  assert_has_cols(
    haz_spec,
    c("varname", "ref_start", "ref_end", "temporalAgg", "varConstruction", "contOrBinned"),
    "step1$haz_spec()"
  )

  list(step1 = step1, files = files, survey_h3 = survey_h3, haz_spec = haz_spec)
}

# Phase C: inputs needed to build simulation panel (survey_weather + unit_frame + model)
step1_inputs_for_phase_c <- function(step1) {
  step1 <- adapt_step1_to_step2_v0(step1, require_weather_api = TRUE)
  assert_step1_contract_step2(step1, require_weather_api = TRUE)

  sw <- step1$survey_weather()

  # Fallback: Phase C can use survey_data() as the unit-level base; hazards come from Phase B.
  if (is.null(sw) || !nrow(sw)) {
    sd <- tryCatch(step1$survey_data(), error = function(e) NULL)
    if (is.null(sd) || !nrow(sd)) stop("Phase C: both step1$survey_weather() and step1$survey_data() are NULL/empty.")
    sw <- sd
  }

  unit_cands <- c("unit_id","hhid","hh_key","household_id","hh_id","case_id","case_uuid","uuid")
  assert_has_any_col(sw, unit_cands, "step1$survey_weather() (unit id)")

  uf <- step1$unit_frame()
  if (is.null(uf) || !nrow(uf)) stop("step1$unit_frame() is NULL/empty.")
  assert_has_any_col(uf, unit_cands, "step1$unit_frame() (unit id)")

  mod <- step1$final_model()
  if (is.null(mod)) stop("step1$final_model() is NULL. Run Module 1 model selection first (or fix wiring).")

  list(step1 = step1, survey_weather = sw, unit_frame = uf, model = mod)
}

# Phase D: inputs needed to predict (currently lm-only)
step1_inputs_for_phase_d <- function(step1) {
  step1 <- adapt_step1_to_step2_v0(step1, require_weather_api = TRUE)
  assert_step1_contract_step2(step1, require_weather_api = TRUE)

  mod <- step1$final_model()
  if (is.null(mod)) stop("step1$final_model() is NULL. Run Module 1 model selection first (or fix wiring).")

  # Step 2 currently supports lm() only
  if (!inherits(mod, "lm")) {
    stop("Phase D currently supports only lm() models. Found: ", paste(class(mod), collapse = ", "))
  }

  list(
    step1 = step1,
    final_model = mod,
    model_kind = "lm"
  )
}

# Phase D: the Step 2 pipeline currently assumes lm() (not logistic / etc.)
step1_final_model_lm <- function(step1) {
  step1 <- adapt_step1_to_step2_v0(step1, require_weather_api = TRUE)
  assert_step1_contract_step2(step1, require_weather_api = TRUE)

  mod <- step1$final_model()
  if (is.null(mod)) stop("step1$final_model() is NULL. Run Module 1 model selection first (or fix wiring).")
  if (!inherits(mod, "lm")) stop("final_model must be an lm() object (current Step 2 only supports lm()).")

  mod
}


# ------------------------------------------------------------------------------
# Contract checks: board/pins
# ------------------------------------------------------------------------------

# Try to infer the local directory backing a pins::board_local() board.
# pins versions differ: some store `$path`, others `$cache`, etc.
board_local_path <- function(board) {
  if (is.null(board)) return(NULL)

  for (nm in c("path", "cache", "dir", "root")) {
    val <- tryCatch(board[[nm]], error = function(e) NULL)
    if (is.character(val) && length(val) == 1 && nzchar(val) && dir.exists(val)) return(val)
  }

  for (nm in c("path", "cache", "dir", "root")) {
    val <- tryCatch(attr(board, nm), error = function(e) NULL)
    if (is.character(val) && length(val) == 1 && nzchar(val) && dir.exists(val)) return(val)
  }

  NULL
}

check_pin_board <- function(board) {
  if (is.null(board)) {
    return(list(ok = FALSE, msg = "FAIL | board is NULL (not passed from app_server)"))
  }
  # pins boards are R6-ish; don't over-validate, just sanity check class and basic call
  ok <- TRUE
  msg <- "OK"
  tryCatch({
    cls <- class(board)
    bd <- board_local_path(board)
    msg <- paste0("OK | board class: ", paste(cls, collapse = ", "), if (!is.null(bd)) paste0(" | path: ", bd) else "")
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

# Helper: concise summary of the Step 1 spec (the boundary contract)
format_step1_spec_summary <- function(step1) {
  spec <- tryCatch(step1$step1_spec(), error = function(e) NULL)
  if (is.null(spec) || !is.list(spec)) return("Spec: <missing>")

  # outcome
  out <- spec$outcome
  out_name <- NA_character_
  out_units <- NA_character_
  out_transform <- NA_character_
  pl <- spec$povline_level %||% NA_real_

  if (is.data.frame(out) && nrow(out)) {
    if ("name" %in% names(out)) out_name <- as.character(out$name[[1]])
    if ("units" %in% names(out)) out_units <- as.character(out$units[[1]])
    if ("transform" %in% names(out)) out_transform <- as.character(out$transform[[1]])
    if ("povline" %in% names(out)) {
      pl2 <- suppressWarnings(as.numeric(out$povline[[1]]))
      if (is.finite(pl2)) pl <- pl2
    }
  }

  # hazard vars
  hv <- tryCatch(spec$hazard$haz_vars, error = function(e) character(0))
  hv <- as.character(hv)
  hv <- hv[!is.na(hv) & nzchar(hv)]
  hv <- unique(hv)
  haz_txt <- if (length(hv)) paste(hv, collapse = ",") else "<none>"

  files <- tryCatch(spec$survey_data_files, error = function(e) character(0))
  files <- as.character(files)
  files <- files[!is.na(files) & nzchar(files)]
  files <- unique(files)
  files_txt <- if (length(files)) paste(head(files, 6), collapse = ",") else "<none>"

  paste0(
    "Spec: outcome=", ifelse(is.na(out_name) || !nzchar(out_name), "<NA>", out_name),
    " | units=", ifelse(is.na(out_units) || !nzchar(out_units), "<NA>", out_units),
    " | transform=", ifelse(is.na(out_transform) || !nzchar(out_transform), "<NA>", out_transform),
    " | povline=", ifelse(is.finite(pl), format(pl, trim = TRUE), "<NA>"),
    " | haz=", haz_txt,
    " | files=", files_txt
  )
}

# ------------------------------------------------------------------------------
# Small status formatter (optional)
# ------------------------------------------------------------------------------
format_step2_status <- function(step1, board = NULL) {
  ck1 <- check_step1_contract_step2(step1, require_weather_api = FALSE)
  ck2 <- check_step1_contract_step2_content(step1, require_weather_api = FALSE)
  ckB <- check_pin_board(board)

  has_model <- tryCatch(!is.null(step1$final_model()), error = function(e) FALSE)
  n_sw <- tryCatch(nrow(step1$survey_weather()), error = function(e) NA_integer_)
  n_sd <- tryCatch(nrow(step1$survey_data()), error = function(e) NA_integer_)
  n_uf <- tryCatch(nrow(step1$unit_frame()), error = function(e) NA_integer_)
  n_haz <- tryCatch(length(step1$haz_vars()), error = function(e) NA_integer_)

  paste0(
    "[Contract] ", ck1$msg, "\n",
    "[Content] ", ck2$msg, "\n",
    "[Board] ", ckB$msg, "\n",
    format_step1_spec_summary(step1), "\n",
    "Model available: ", if (has_model) "YES" else "NO", " | ",
    "survey_weather rows: ", n_sw, " | ",
    "survey_data rows: ", n_sd, " | ",
    "unit_frame rows: ", n_uf, " | ",
    "haz vars: ", n_haz
  )
}


# -------------------------------------------------------------------------
# Smoke tests: lightweight regression snapshot for Step 2 wiring
# -------------------------------------------------------------------------
smoke_step2_snapshot <- function(step1, board = NULL, run = NULL) {
  # Step1 shim
  step1_s2 <- tryCatch(adapt_step1_to_step2_v0(step1, require_weather_api = TRUE), error = function(e) NULL)

  ck_struct <- if (is.null(step1_s2)) list(ok = FALSE, msg = "FAIL | adapt_step1_to_step2_v0 failed") else
    check_step1_contract_step2(step1_s2, require_weather_api = TRUE)

  ck_content <- if (is.null(step1_s2)) list(ok = FALSE, msg = "FAIL | adapt_step1_to_step2_v0 failed") else
    check_step1_contract_step2_content(step1_s2, require_weather_api = TRUE)

  ck_board <- check_pin_board(board)

  out <- data.frame(
    check = c("step1_structure", "step1_content", "board", "step1_spec"),
    ok    = c(isTRUE(ck_struct$ok), isTRUE(ck_content$ok), isTRUE(ck_board$ok), TRUE),
    msg   = c(ck_struct$msg, ck_content$msg, ck_board$msg, format_step1_spec_summary(step1_s2 %||% step1)),
    stringsAsFactors = FALSE
  )

  # Phase artifacts (if a run bundle is available)
  if (!is.null(run) && is.list(run) && !is.null(run$checks) && is.list(run$checks)) {
    for (nm in c("phase_b", "phase_c", "phase_d", "phase_e")) {
      ck <- run$checks[[nm]]
      if (is.null(ck)) next
      ok <- isTRUE(ck$ok)
      msg <- if (!is.null(ck$msg)) ck$msg else ""
      out <- rbind(out, data.frame(
        check = paste0(nm, "_artifact"),
        ok = ok,
        msg = msg,
        stringsAsFactors = FALSE
      ))
    }
  }

  out
}





#Pulled from prior version: Phase B
# Derive weather pin names from survey pin IDs (mirrors Step 1 approach).
#derive_weather_pin_names <- function(survey_pin_ids) {
#  if (!length(survey_pin_ids)) return(character(0))
#  unique(paste0(substr(survey_pin_ids, 1, nchar(survey_pin_ids) - 4), "weather"))
#}
# Keep this consistent with mod_1_04_weather.R to avoid surprises.
derive_weather_pin_names <- function(survey_pin_ids) {
  ids <- unique(stats::na.omit(as.character(survey_pin_ids)))
  if (!length(ids)) return(character(0))

  # If ids look like "GNB_2021", strip trailing year and append "_weather"
  base <- sub("_[0-9]{4}$", "", ids)
  cand <- unique(paste0(base, "_weather"))

  # Also keep the older conversions if they happen to apply
  cand <- unique(c(
    cand,
    sub("/data$", "/weather", ids),
    sub("_data$", "_weather", ids),
    ifelse(grepl("data$", ids), paste0(substr(ids, 1, nchar(ids) - 4), "weather"), NA_character_)
  ))
  cand <- cand[grepl("weather", cand)]
  unique(stats::na.omit(cand))
}
# Build H3-level hazards from wide weather data using haz_spec.
# Expects: weather_df has h3_6, timestamp (Date), and one column per weather var.
build_h3_hazards <- function(weather_df, haz_spec) {
  if (is.null(weather_df) || !nrow(weather_df)) return(NULL)
  if (is.null(haz_spec) || !nrow(haz_spec)) return(NULL)

  out <- NULL

  for (i in haz_spec$varname) {
    spec_i <- haz_spec[haz_spec$varname == i, , drop = FALSE]
    ref_start <- as.numeric(spec_i$ref_start[[1]])
    ref_end   <- as.numeric(spec_i$ref_end[[1]])
    temporal_agg <- as.character(spec_i$temporalAgg[[1]])
    transformation <- as.character(spec_i$varConstruction[[1]])
    cont_binned <- as.character(spec_i$contOrBinned[[1]])

    w <- weather_df |>
      dplyr::group_by(.data$h3_6) |>
      dplyr::arrange(.data$timestamp, .by_group = TRUE)

    # Lags
    if (is.finite(ref_start) && is.finite(ref_end)) {
      for (l in seq(ref_start, ref_end)) {
        colname <- paste0(i, "_", l)
        w <- w |>
          dplyr::mutate(
            !!rlang::sym(colname) := dplyr::lag(.data[[i]], n = l, order_by = .data$timestamp)
          )
      }
    }

    # Drop leading rows without full lag support
    if (is.finite(ref_start) && is.finite(ref_end)) {
      end_col <- paste0(i, "_", max(ref_start, ref_end))
      if (end_col %in% names(w)) {
        w <- w |>
          dplyr::filter(!is.na(.data[[end_col]])) |>
          dplyr::ungroup()
      } else {
        w <- w |> dplyr::ungroup()
      }
    } else {
      w <- w |> dplyr::ungroup()
    }

    # Temporal aggregation across lag columns
    lag_prefix <- paste0(i, "_")
    if (temporal_agg == "Mean") {
      w <- w |> dplyr::mutate(haz = rowMeans(dplyr::across(dplyr::starts_with(lag_prefix))))
    } else if (temporal_agg == "Median") {
      w <- w |>
        dplyr::mutate(haz = apply(dplyr::select(., dplyr::starts_with(lag_prefix)), 1, stats::median, na.rm = TRUE))
    } else if (temporal_agg == "Min") {
      w <- w |>
        dplyr::mutate(haz = do.call(pmin, c(dplyr::across(dplyr::starts_with(lag_prefix)), na.rm = TRUE)))
    } else if (temporal_agg == "Max") {
      w <- w |>
        dplyr::mutate(haz = do.call(pmax, c(dplyr::across(dplyr::starts_with(lag_prefix)), na.rm = TRUE)))
    } else if (temporal_agg == "Sum") {
      w <- w |> dplyr::mutate(haz = rowSums(dplyr::across(dplyr::starts_with(lag_prefix))))
    } else {
      w <- w |> dplyr::mutate(haz = NA_real_)
    }

    # Transformation (match Step 1 logic: skip for spi6/spei6 and for "None")
    if (!is.na(transformation) &&
        !(transformation == "None" || i %in% c("spi6", "spei6")) &&
        "haz" %in% names(w)) {

      w <- w |>
        dplyr::mutate(
          year  = lubridate::year(.data$timestamp),
          month = lubridate::month(.data$timestamp)
        )

      climate_ref <- w |>
        dplyr::filter(.data$year >= 1991 & .data$year <= 2020) |>
        dplyr::summarise(
          mean = mean(.data$haz, na.rm = TRUE),
          sd   = stats::sd(.data$haz, na.rm = TRUE),
          .by = c(.data$h3_6, .data$month)
        )

      if (transformation == "Deviation from mean") {
        w <- w |>
          dplyr::left_join(climate_ref, by = c("h3_6", "month")) |>
          dplyr::mutate(haz = .data$haz - .data$mean)
      } else if (transformation == "Standardized anomaly") {
        w <- w |>
          dplyr::left_join(climate_ref, by = c("h3_6", "month")) |>
          dplyr::mutate(haz = (.data$haz - .data$mean) / .data$sd)
      }
    }

    if (!is.na(cont_binned) && cont_binned == "Binned") {
      # Not implemented yet (consistent with Step 1)
    }

    w <- w |>
      dplyr::select(.data$h3_6, .data$timestamp, haz = .data$haz) |>
      dplyr::rename(!!paste0("haz_", i) := .data$haz) |>
      dplyr::arrange(.data$h3_6, .data$timestamp)

    out <- if (is.null(out)) w else dplyr::full_join(out, w, by = c("h3_6", "timestamp"))
  }

  out
}

# Aggregate H3 hazards to loc_id for SIMULATION (Step 2):
# produces one row per (code, loc_id, timestamp), NOT per survey year/survname.
aggregate_h3_to_loc <- function(survey_h3, h3_haz) {
  if (is.null(survey_h3) || !nrow(survey_h3)) return(NULL)
  if (is.null(h3_haz) || !nrow(h3_haz)) return(NULL)

  # We only need an H3->loc mapping (optionally with weights)
  keep <- intersect(c("code", "h3_6", "loc_id", "pop_2020"), names(survey_h3))
  if (!all(c("h3_6", "loc_id") %in% keep)) return(NULL)

  map <- survey_h3 |>
    dplyr::select(dplyr::all_of(keep)) |>
    dplyr::mutate(loc_id = as.character(.data$loc_id)) |>
    dplyr::distinct()

  # Collapse any duplicates in the mapping (common if survey_h3 is at HH-row level)
  if ("pop_2020" %in% names(map)) {
    by_map <- intersect(c("code", "h3_6", "loc_id"), names(map))
    map <- map |>
      dplyr::summarise(pop_2020 = max(.data$pop_2020, na.rm = TRUE), .by = dplyr::all_of(by_map))
  } else {
    map <- map |>
      dplyr::mutate(pop_2020 = 1)
  }

  # Join hazards by h3 only (hazards already have timestamp)
  data <- dplyr::left_join(map, h3_haz, by = "h3_6")

  haz_cols <- grep("^haz_", names(data), value = TRUE)
  if (!length(haz_cols)) return(NULL)

  by_cols <- intersect(c("code", "loc_id", "timestamp"), names(data))
  if (!all(c("loc_id", "timestamp") %in% by_cols)) return(NULL)

  # Weighted aggregation to loc-month
  out <- data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(haz_cols),
        ~ sum(.x * .data$pop_2020, na.rm = TRUE) / sum(.data$pop_2020, na.rm = TRUE)
      ),
      .by = dplyr::all_of(by_cols)
    )

  out |>
    dplyr::mutate(loc_id = as.character(.data$loc_id))
}

infer_model_kind <- function(mod) {
  if (inherits(mod, "glm") &&
      !is.null(mod$family) &&
      identical(mod$family$family, "binomial")) {
    return("glm_binomial")
  }
  if (inherits(mod, "lm")) return("lm")
  "other"
}

infer_pred_scale_lm <- function(mod, pred_sample = NULL) {
  resp <- tryCatch(as.character(stats::formula(mod))[2], error = function(e) "")


  # If formula explicitly logs/ln()'s the response, it's log-scale.
  if (grepl("(^ln_|^log_|\\blog\\s*\\(|\\bln\\s*\\()", resp, ignore.case = TRUE)) {
    return("log")
  }


  # If we can inspect fitted response values, use simple heuristics.
  y <- tryCatch(stats::model.response(stats::model.frame(mod)), error = function(e) NULL)
  if (!is.null(y) && is.numeric(y)) {
    if (any(y < 0, na.rm = TRUE)) return("log")
    if (stats::quantile(y, 0.99, na.rm = TRUE) > 100) return("level")
  }


  # Optional: look at prediction magnitudes
  if (!is.null(pred_sample) && is.numeric(pred_sample)) {
    if (any(pred_sample < 0, na.rm = TRUE)) return("log")
    if (stats::quantile(pred_sample, 0.99, na.rm = TRUE) > 100) return("level")
  }


  # Default
  "level"
}

detect_weight_col <- function(df, varlist = NULL) {
  # allow varlist override
  if (!is.null(varlist) && is.list(varlist) && "wt_hh" %in% names(varlist)) {
    if (is.character(varlist$wt_hh) && varlist$wt_hh %in% names(df)) return(varlist$wt_hh)
  }
  cands <- c("hh_wt", "hh_weight", "weight", "wgt", "pweight", "final_weight")
  cands[cands %in% names(df)][1] |> (\(x) if (is.na(x) || !nzchar(x)) NULL else x)()
}

wmean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Coerce poverty lines to a named numeric vector on the MODEL SCALE for lm()
# Supports:
# - named numeric vector (assumed already on model scale)
# - data.frame with columns: name + value (+ optional scale="log"/"level")
coerce_pov_lines_modelscale <- function(pov_lines,
                                        pred_scale = c("log", "level"),
                                        ppp_year = NULL) {
  pred_scale <- match.arg(pred_scale)

  if (is.null(pov_lines)) return(NULL)

  # allow reactive/function
  if (is.function(pov_lines)) pov_lines <- pov_lines()

  # numeric vector
  if (is.numeric(pov_lines)) {
    v <- as.numeric(pov_lines)
    nm <- names(pov_lines)
    if (is.null(nm) || any(!nzchar(nm))) nm <- paste0("pline_", seq_along(v))
    names(v) <- nm
    return(v)
  }

  # list -> named numeric
  if (is.list(pov_lines) && !is.data.frame(pov_lines)) {
    v <- unlist(pov_lines, recursive = TRUE, use.names = TRUE)
    if (is.numeric(v) && length(v) > 0) {
      if (is.null(names(v))) names(v) <- paste0("pline_", seq_along(v))
      v2 <- as.numeric(v)
      names(v2) <- names(v)
      return(v2)
    }
  }

  # data.frame formats
  if (is.data.frame(pov_lines)) {
    df <- pov_lines


    # --- Format A: (ppp_year, ln) where `ln` is LEVEL poverty line in USD PPP (often $/day) ---
    if (all(c("ppp_year", "ln") %in% names(df))) {
      if (!is.null(ppp_year)) df <- dplyr::filter(df, .data$ppp_year == !!ppp_year)
      if (!nrow(df)) return(NULL)


      pl_level <- as.numeric(df$ln)
      vals <- if (pred_scale == "log") log(pl_level) else pl_level


      nm <- if ("name" %in% names(df)) as.character(df$name) else paste0("pline_", seq_along(vals))
      names(vals) <- nm
      vals2 <- as.numeric(vals)
      names(vals2) <- names(vals)
      return(vals2)
    }


    # --- Format B: generic (name, value[, scale]) ---
    if (all(c("name", "value") %in% names(df))) {
      vals <- as.numeric(df$value)
      names(vals) <- as.character(df$name)


      if ("scale" %in% names(df)) {
        from <- as.character(df$scale)
        if (pred_scale == "log") {
          vals[from == "level"] <- log(vals[from == "level"])
        } else {
          vals[from == "log"] <- exp(vals[from == "log"])
        }
      }
      vals2 <- as.numeric(vals)
      names(vals2) <- names(vals)
      return(vals2)
    }


    rlang::abort("pov_lines data.frame must have either (ppp_year, ln) or (name, value[, scale]).")
  }

  rlang::abort("Unsupported pov_lines format. Use named numeric vector, list, or data.frame(...).")
}
# ------------------------------------------------------------------------------
# Functional core extraction (Phase B–E)
#
# Goal: keep Shiny modules thin by moving the “real work” into reusable helpers.
# These functions are intentionally side-effect free EXCEPT for Phase B, which
# downloads pins to disk.
# ------------------------------------------------------------------------------

# Phase B (Historical): download + read weather pins, build hazards, aggregate to loc-month
phase_b_historical_core <- function(step1, board, hist_years, pin_name_prefix = NULL, verbose = FALSE) {
  inp <- step1_inputs_for_phase_b(step1)
  step1 <- inp$step1

  if (is.null(board)) stop("Phase B requires a pins board (board is NULL)")

  stopifnot(is.numeric(hist_years), length(hist_years) == 2)
  year_start <- as.integer(hist_years[[1]])
  year_end   <- as.integer(hist_years[[2]])
  if (!is.finite(year_start) || !is.finite(year_end) || year_start > year_end) {
    stop("hist_years must be numeric length-2 with start <= end")
  }

  files <- inp$files
  if (wise_debug_enabled()) {
    message("DEBUG Phase B: step1$survey_data_files() (head): ", paste(head(files, 20), collapse = " | "))
  }

  # Prefer explicit weather pins if Step 1 lists them
  weather_file_ids <- grep("/weather", files, value = TRUE)
  if (!length(weather_file_ids)) weather_file_ids <- grep("weather", files, value = TRUE)

  # Fallback: derive weather pin IDs from survey pin IDs
  if (!length(weather_file_ids)) {
    weather_file_ids <- derive_weather_pin_names(files)
  }
  if (wise_debug_enabled()) {
    message("DEBUG Phase B: derived weather pin ids (head): ", paste(head(weather_file_ids, 20), collapse = " | "))
  }

  if (!length(weather_file_ids)) {
    stop(
      "Phase B: could not find or derive weather pins from step1$survey_data_files(). ",
      "First entries were: ", paste(head(files, 10), collapse = ", ")
    )
  }

  # Avoid double-prefixing if Step 1 already returns fully-qualified pin names
  if (!is.null(pin_name_prefix) && nzchar(pin_name_prefix) &&
      !all(startsWith(weather_file_ids, pin_name_prefix))) {
    weather_pin_names <- paste0(pin_name_prefix, weather_file_ids)
  } else {
    weather_pin_names <- weather_file_ids
  }
  if (wise_debug_enabled()) {
    message("DEBUG Phase B: weather_pin_names (head): ", paste(head(weather_pin_names, 20), collapse = " | "))
  }
# --- Phase B: read weather strictly via the passed pins board ---
board_dir <- board_local_path(board)
if (is.null(board_dir)) {
  stop(
    "Phase B: could not resolve pins board path from `board`. ",
    "Create the board in app_server as pins::board_folder(file.path(data_dir, 'pins')) and pass it into Module 2."
  )
}
data_dir <- dirname(board_dir)

# Only keep candidates that are weather-like
weather_pin_names <- unique(weather_pin_names[grepl("weather", weather_pin_names)])
if (!length(weather_pin_names)) stop("Phase B: no weather-like pin names after filtering.")

avail <- tryCatch(pins::pin_list(board), error = function(e) character(0))

# Bootstrap missing pins from <data_dir>/<pin>.parquet (one-time) then re-read via pins
missing_pins <- setdiff(weather_pin_names, avail)
if (length(missing_pins)) {
  if (wise_debug_enabled()) {
    message("DEBUG Phase B: missing pins on board: ", paste(missing_pins, collapse = ", "))
  }
  for (pn in missing_pins) {
    pn_base <- basename(pn)
    fp <- file.path(data_dir, paste0(pn_base, ".parquet"))
    if (!file.exists(fp)) next

    if (wise_debug_enabled()) message("DEBUG Phase B: bootstrapping pin ", pn, " from ", fp)

    x <- tryCatch(read_parquet_duckdb(fp), error = function(e) NULL)
    if (is.null(x) || !is.data.frame(x) || !nrow(x)) next

    # Prefer parquet pin if supported; fall back to default
    tryCatch(
      pins::pin_write(board, x = x, name = pn, type = "parquet"),
      error = function(e) pins::pin_write(board, x = x, name = pn)
    )
  }
  avail <- tryCatch(pins::pin_list(board), error = function(e) character(0))
}

present_pins <- intersect(weather_pin_names, avail)
if (!length(present_pins)) {
  stop(
    "Phase B: none of the derived weather pins exist on the board after bootstrap. ",
    "Candidates: ", paste(weather_pin_names, collapse = ", "),
    " | board path: ", board_dir
  )
}

# Read pins (bind rows across selected surveys)
weather_list <- lapply(present_pins, function(pn) {
  tryCatch(pins::pin_read(board, pn), error = function(e) NULL)
})
weather_list <- weather_list[!vapply(weather_list, function(x) is.null(x) || !is.data.frame(x) || !nrow(x), logical(1))]

if (!length(weather_list)) stop("Phase B: pin_read() returned 0 rows across all weather pins.")
weather <- dplyr::bind_rows(weather_list)

  # ---- Standardize weather schema (Module 1 variants) ----
  # Expect an H3 column at res 6 named h3_6, but newer Module 1 may emit `h3`.
  if (!"h3_6" %in% names(weather)) {
    h3_cands <- c("h3", "h3_id", "h3_index", "h3index", "h3_cell", "h3_cell_id",
                 grep("^h3(_|$)", names(weather), value = TRUE))
    h3_cand <- h3_cands[h3_cands %in% names(weather)][1]
    if (!is.na(h3_cand) && nzchar(h3_cand)) {
      weather <- dplyr::rename(weather, h3_6 = !!rlang::sym(h3_cand))
    }
  }
  # Timestamp naming variants
  if (!"timestamp" %in% names(weather)) {
    ts_cands <- c("date", "time", "t", "month", "ym", "svy_timestamp")
    ts_cand <- ts_cands[ts_cands %in% names(weather)][1]
    if (!is.na(ts_cand) && nzchar(ts_cand)) {
      weather <- dplyr::rename(weather, timestamp = !!rlang::sym(ts_cand))
    }
  }
  # Coerce types (helps lubridate + joins)
  if ("h3_6" %in% names(weather)) weather$h3_6 <- as.character(weather$h3_6)
  if ("timestamp" %in% names(weather) && !inherits(weather$timestamp, "Date")) {
    weather$timestamp <- tryCatch(as.Date(weather$timestamp), error = function(e) weather$timestamp)
  }

  # Expect Step 1 schema: h3_6 + timestamp (post-standardization)
  req_cols <- c("h3_6", "timestamp")
  miss <- setdiff(req_cols, names(weather))
  if (length(miss)) stop("Weather data missing required columns: ", paste(miss, collapse = ", "))

  weather_data_sim <- weather |>
    dplyr::mutate(
      sim_year = lubridate::year(.data$timestamp),
      month    = lubridate::month(.data$timestamp)
    ) |>
    dplyr::filter(.data$sim_year >= year_start, .data$sim_year <= year_end)

  if (!nrow(weather_data_sim)) {
    stop("No weather rows after filtering to year window ", year_start, "-", year_end)
  }

  haz_spec <- inp$haz_spec
  survey_h3 <- inp$survey_h3

  h3_haz <- build_h3_hazards(weather_data_sim, haz_spec)
  haz_cols <- grep("^haz_", names(h3_haz), value = TRUE)
  if (!length(haz_cols)) stop("No haz_* columns produced by build_h3_hazards().")

  loc_haz <- aggregate_h3_to_loc(survey_h3 = survey_h3, h3_haz = h3_haz)
  if (is.null(loc_haz) || !nrow(loc_haz)) stop("aggregate_h3_to_loc() produced 0 rows")

  if (verbose) {
    message("[Phase B core] years=", year_start, "-", year_end,
            " | loc_month rows=", nrow(loc_haz),
            " | haz cols=", paste(haz_cols, collapse = ", "))
  }

  list(
    haz_spec        = haz_spec,
    sim_year_range  = c(year_start, year_end),
    weather_pins    = weather_pin_names,
    weather_data_sim = weather_data_sim,
    loc_weather_sim = loc_haz,
    haz_cols        = haz_cols
  )
}



# --- Poverty line selection (from Step 1) ---
# Pull the active poverty line configured in Module 1 and coerce it to model scale (log vs level).
get_active_poverty_line <- function(step1) {
  so <- tryCatch(step1$selected_outcome(), error = function(e) NULL)

  if (wise_debug_enabled()) {
    message("DEBUG step1$selected_outcome():")
    print(so)
  }

  if (is.null(so)) stop("Phase E: step1$selected_outcome() is NULL")

  # ---- pull poverty line in LEVEL space from Step 1 ----
  pline_level <- NA_real_
  if (is.data.frame(so) && nrow(so) && "povline" %in% names(so)) {
    pline_level <- suppressWarnings(as.numeric(so$povline[[1]]))
  } else if (!is.null(so$povline)) {
    pline_level <- suppressWarnings(as.numeric(so$povline))
  }
  if (is.null(pline_level) || !is.finite(pline_level) || pline_level <= 0) {
    stop("Phase E: poverty line (step1$selected_outcome()$povline) is missing/invalid")
  }

  # ---- determine prediction scale (MODEL space) ----
  # Prefer Step 1's declared transform, because heuristic inference can fail when log(y) is always positive.
  so_transform <- NA_character_
  if (is.data.frame(so) && nrow(so) && "transform" %in% names(so)) {
    so_transform <- as.character(so$transform[[1]])
  } else if (!is.null(so$transform)) {
    so_transform <- as.character(so$transform)
  }

  mod <- tryCatch(step1$final_model(), error = function(e) NULL)
  if (is.null(mod)) stop("Phase E: step1$final_model() is NULL")

  pred_scale <- NA_character_
  if (!is.na(so_transform) && nzchar(so_transform)) {
    pred_scale <- if (tolower(so_transform) %in% c("log", "ln")) "log" else "level"
  } else {
    pred_scale <- infer_pred_scale_lm(mod)
  }

  pline_model <- if (identical(pred_scale, "log")) log(pline_level) else pline_level

  # ---- label / units ----
  so_units <- NA_character_
  if (is.data.frame(so) && nrow(so) && "units" %in% names(so)) {
    so_units <- as.character(so$units[[1]])
  } else if (!is.null(so$units)) {
    so_units <- as.character(so$units)
  }

  label <- if (!is.na(so_units) && toupper(so_units) == "LCU") {
    paste0(format(as.numeric(pline_level), trim = TRUE), " LCU/day")
  } else {
    paste0("$", format(as.numeric(pline_level), trim = TRUE), "/day (2021 PPP)")
  }

  list(
    level = as.numeric(pline_level),
    model = as.numeric(pline_model),
    pred_scale = pred_scale,
    units = so_units,
    label = label
  )
}


# Phase C (Historical): build simulation panel
phase_c_historical_core <- function(step1, built_haz) {

  #Helper to run later
  fix_joined_haz_cols <- function(df, haz_cols) {
    for (hc in haz_cols) {
      if (!(hc %in% names(df))) {
        y <- paste0(hc, ".y")
        x <- paste0(hc, ".x")
        if (y %in% names(df)) {
          df[[hc]] <- df[[y]]
          df[[y]] <- NULL
        }
        if (x %in% names(df)) {
          df[[x]] <- NULL
        }
      }
    }
    df
  }


  inp <- step1_inputs_for_phase_c(step1)

  sw <- inp$survey_weather
  if (is.null(sw) || !nrow(sw)) stop("step1$survey_weather() is NULL/empty")

  if (is.null(built_haz) || !is.list(built_haz)) stop("built_haz is NULL/invalid")
  loc_haz <- built_haz$loc_weather_sim
  if (is.null(loc_haz) || !nrow(loc_haz)) stop("built_haz$loc_weather_sim is NULL/empty")

  haz_cols <- grep("^haz_", names(loc_haz), value = TRUE)
  if (!length(haz_cols)) stop("loc_weather_sim has no haz_* columns")

  # ---- helper: pick the first existing column name ----
  pick_col <- function(df, cands, what) {
    nm <- cands[cands %in% names(df)][1]
    if (is.na(nm) || !nzchar(nm)) {
      stop("Phase C: required column for ", what, " not found. Tried: ",
           paste(cands, collapse = ", "), call. = FALSE)
    }
    nm
  }

  # Keys (support both loc_id and location_id, and standardize)
  loc_col_haz <- pick_col(loc_haz, c("loc_id", "location_id"), "location id in loc_weather_sim")
  ts_col_haz  <- pick_col(loc_haz,  c("timestamp", "sim_timestamp"), "timestamp in loc_weather_sim")

  loc_col_sw  <- pick_col(sw, c("loc_id", "location_id"), "location id in survey_weather")
  ts_col_sw   <- pick_col(sw, c("timestamp", "svy_timestamp"), "timestamp in survey_weather")

  # Unit id: accept common candidates (HH / person / firm)
  unit_candidates <- c("unit_id","hhid","hh_key","household_id","hh_id","case_id","case_uuid","uuid")
  unit_col_sw <- unit_candidates[unit_candidates %in% names(sw)][1]

  # Fallback if none exist: create a temporary stable id
  if (is.na(unit_col_sw) || !nzchar(unit_col_sw)) {
    sw$.unit_tmp_id <- seq_len(nrow(sw))
    unit_col_sw <- ".unit_tmp_id"
  }

  # loc-month hazards table with explicit join keys (derive sim_year/month from timestamp)
  loc_haz2 <- loc_haz |>
    dplyr::transmute(
      loc_id   = as.character(.data[[loc_col_haz]]),
      sim_year = lubridate::year(.data[[ts_col_haz]]),
      month    = lubridate::month(.data[[ts_col_haz]]),
      dplyr::across(dplyr::all_of(haz_cols))
    ) |>
    dplyr::group_by(.data$loc_id, .data$sim_year, .data$month) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(haz_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Identify predictors expected by model (so we keep them in the panel)
  mod <- inp$model
  f <- stats::formula(mod)
  vars <- all.vars(f)
  response <- as.character(f[[2]])
  predictors <- setdiff(vars, response)

  haz_pred    <- grep("^haz_", predictors, value = TRUE)
  nonhaz_pred <- setdiff(predictors, haz_pred)


  # Ensure required predictors are present on unit_frame() (preferred) before we build the panel
    uf <- inp$unit_frame
    if (is.null(uf) || !nrow(uf)) stop("Phase C: step1$unit_frame() is NULL/empty.")

    unit_col_uf <- unit_candidates[unit_candidates %in% names(uf)][1]
    if (is.na(unit_col_uf) || !nzchar(unit_col_uf)) {
      stop("Phase C: step1$unit_frame() is missing a unit id column. Tried: ", paste(unit_candidates, collapse = ", "))
    }

    uf2 <- uf |>
      dplyr::mutate(unit_id = as.character(.data[[unit_col_uf]])) |>
      dplyr::distinct(.data$unit_id, .keep_all = TRUE)

    missing_in_uf <- setdiff(nonhaz_pred, names(uf2))
    if (length(missing_in_uf)) {
      stop(
        "Phase C: step1$unit_frame() is missing model predictors: ",
        paste(missing_in_uf, collapse = ", "),
        ". Ensure Module 1 exports predictors in unit_frame()."
      )
    }

    wt_col <- tryCatch(detect_weight_col(uf2, varlist = NULL), error = function(e) NULL)
    uf_keep <- unique(c("unit_id", nonhaz_pred, wt_col))
    uf_keep <- intersect(uf_keep, names(uf2))
    uf2 <- uf2 |>
      dplyr::select(dplyr::all_of(uf_keep))

    # Collapse survey_weather to one record per (unit_id, loc_id, month) using the latest timestamp.
    sw2 <- sw |>
      dplyr::mutate(
        unit_id       = as.character(.data[[unit_col_sw]]),
        loc_id        = as.character(.data[[loc_col_sw]]),
        month         = lubridate::month(.data[[ts_col_sw]]),
        svy_timestamp = .data[[ts_col_sw]]
      ) |>
      dplyr::select(.data$unit_id, .data$loc_id, .data$month, .data$svy_timestamp) |>
      dplyr::group_by(.data$unit_id, .data$loc_id, .data$month) |>
      dplyr::slice_max(.data$svy_timestamp, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::left_join(uf2, by = "unit_id")
  if (wise_debug_enabled()) message("DEBUG Phase C: unit_col_sw=", unit_col_sw,
            " | unit_col_uf=", unit_col_uf,
            " | sw2 rows=", nrow(sw2),
            " | predictors(nonhaz)=", length(nonhaz_pred))

  yrs <- built_haz$sim_year_range
  if (is.null(yrs) || length(yrs) != 2) stop("built_haz$sim_year_range missing/invalid")
  yrs <- seq(as.integer(yrs[[1]]), as.integer(yrs[[2]]))

  panel <- tidyr::crossing(sw2, sim_year = yrs)
  if (wise_debug_enabled()) message("DEBUG Phase C: after crossing rows=", nrow(panel),
          " | years=", min(yrs), "-", max(yrs))

  # sanity: loc_haz2 must be unique on join keys
  dup_haz <- loc_haz2 |>
    dplyr::count(.data$loc_id, .data$sim_year, .data$month, name = "n") |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup_haz) > 0) stop("Phase C: loc_haz2 still has duplicate (loc_id, sim_year, month) keys.")

  panel <- panel |>
    dplyr::left_join(loc_haz2, by = c("loc_id", "sim_year", "month"))
  if (wise_debug_enabled()) message("DEBUG Phase C: after haz join rows=", nrow(panel))
  if (wise_debug_enabled()) message("DEBUG Phase C: haz cols present: ", paste(haz_cols, collapse = ", "))


  panel <- fix_joined_haz_cols(panel, haz_cols)

  missing_haz <- setdiff(haz_cols, names(panel))
  if (length(missing_haz)) {
    stop("Phase C: hazards missing after join: ", paste(missing_haz, collapse = ", "))
  }

  any_na_haz <- mean(!stats::complete.cases(panel[, haz_cols, drop = FALSE]))

  list(
    sim_panel = panel,
    diag = list(
      rows = nrow(panel),
      haz_cols = haz_cols,
      haz_na_rate = any_na_haz
    )
  )
}

# Phase D (Historical): predict welfare
phase_d_historical_core <- function(step1, sim_panel) {
  inp <- step1_inputs_for_phase_d(step1)
  final_model <- inp$final_model
  model_kind  <- inp$model_kind

  if (!identical(model_kind, "lm")) {
    stop("Phase D core currently supports only lm() models. Found model_kind=", model_kind)
  }
  if (is.null(sim_panel) || !nrow(sim_panel)) stop("sim_panel is NULL/empty")

  # Identify predictors expected by model
  f <- stats::formula(final_model)
  vars <- all.vars(f)
  if (!length(vars)) stop("final_model has empty formula vars")

  response <- as.character(f[[2]])
  predictors <- setdiff(vars, response)

  missing_pred <- setdiff(predictors, names(sim_panel))
  if (length(missing_pred)) {
    stop("sim_panel missing predictors required by model: ", paste(missing_pred, collapse = ", "))
  }

  # Coerce factor predictors to model training levels (prevents silent NA/mismatch)
  sp <- sim_panel
  xlevels <- final_model$xlevels
  if (length(xlevels)) {
    for (nm in names(xlevels)) {
      if (nm %in% names(sp)) {
        sp[[nm]] <- factor(sp[[nm]], levels = xlevels[[nm]])
      }
    }
  }

  pred <- tryCatch(stats::predict(final_model, newdata = sp), error = function(e) e)
  if (inherits(pred, "error")) stop("predict() failed: ", pred$message)

  pp <- sp
  pp$pred <- as.numeric(pred)

  na_pred <- is.na(pp$pred)
  na_rate <- mean(na_pred)

  list(
    pred_panel = pp,
    diag = list(
      rows = nrow(pp),
      na_preds = sum(na_pred),
      na_rate = na_rate,
      predictors = predictors
    )
  )
}

# Phase E (Historical): compute poverty (FGT0 only), single poverty line
phase_e_historical_core <- function(step1, pred_panel, pline_value = 3.0, pline_level = NULL, pred_scale = NULL) {
  model_kind <- "lm"  # (you’re enforcing lm-only now)

  # Prefer an explicit weight column if Step 1 provides it, otherwise auto-detect from pred_panel
  weight_col <- tryCatch(step1$weight_col(), error = function(e) NULL)
  if (is.null(weight_col) || !nzchar(weight_col) || !(weight_col %in% names(pred_panel))) {
    weight_col <- detect_weight_col(pred_panel, varlist = NULL)
  }

  use_weights <- !is.null(weight_col) && nzchar(weight_col) && (weight_col %in% names(pred_panel))

  # Determine prediction scale (MODEL space) if not provided
  if (is.null(pred_scale) || !nzchar(pred_scale)) {
    so <- tryCatch(step1$selected_outcome(), error = function(e) NULL)
    so_transform <- NA_character_
    if (is.data.frame(so) && nrow(so) && "transform" %in% names(so)) {
      so_transform <- as.character(so$transform[[1]])
    } else if (!is.null(so$transform)) {
      so_transform <- as.character(so$transform)
    }
    if (!is.na(so_transform) && nzchar(so_transform)) {
      pred_scale <- if (tolower(so_transform) %in% c("log", "ln")) "log" else "level"
    } else {
      mod <- tryCatch(step1$final_model(), error = function(e) NULL)
      pred_scale <- if (is.null(mod)) "level" else infer_pred_scale_lm(mod)
    }
  }

  # If poverty line in LEVEL space not provided, derive from model-space pline_value when possible
  if (is.null(pline_level) || !is.finite(pline_level) || pline_level <= 0) {
    pline_level <- if (identical(pred_scale, "log")) exp(pline_value) else pline_value
  }

  df <- pred_panel |>
    dplyr::mutate(
      poor = as.numeric(.data$pred < pline_value),
      pred_level = if (identical(pred_scale, "log")) exp(.data$pred) else .data$pred
    )

  # FGT components (computed in LEVEL space)
  # gap = ((z - y)/z) * I(y < z); severity = gap^2
  df <- df |>
    dplyr::mutate(
      gap = pmax((pline_level - .data$pred_level) / pline_level, 0),
      fgt1 = .data$gap,
      fgt2 = .data$gap * .data$gap
    )

  # Overall FGT0/1/2
  if (use_weights) {
    w <- df[[weight_col]]
    fgt0 <- wmean_safe(df$poor, w)
    fgt1 <- wmean_safe(df$fgt1, w)
    fgt2 <- wmean_safe(df$fgt2, w)
  } else {
    fgt0 <- mean(df$poor, na.rm = TRUE)
    fgt1 <- mean(df$fgt1, na.rm = TRUE)
    fgt2 <- mean(df$fgt2, na.rm = TRUE)
  }

  overall <- data.frame(
    pline = pline_value,          # model-space (used internally)
    pline_level = pline_level,    # level-space (for interpretation)
    pov_rate = fgt0,              # backward-compatible name
    fgt0 = fgt0,
    fgt1 = fgt1,
    fgt2 = fgt2,
    stringsAsFactors = FALSE
  )

  # By simulated year
  if (!"sim_year" %in% names(df)) stop("pred_panel missing sim_year")

  by_year <- df |>
    dplyr::group_by(.data$sim_year) |>
    dplyr::summarise(
      pov_rate = { if (use_weights) wmean_safe(.data$poor, .data[[weight_col]]) else mean(.data$poor, na.rm = TRUE) },
      fgt0     = { if (use_weights) wmean_safe(.data$poor, .data[[weight_col]]) else mean(.data$poor, na.rm = TRUE) },
      fgt1     = { if (use_weights) wmean_safe(.data$fgt1, .data[[weight_col]]) else mean(.data$fgt1, na.rm = TRUE) },
      fgt2     = { if (use_weights) wmean_safe(.data$fgt2, .data[[weight_col]]) else mean(.data$fgt2, na.rm = TRUE) },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pline = pline_value,
      pline_level = pline_level
    )

  # Coverage (weighted if available, else unweighted share)
  cov_w <- tryCatch({
    ok <- is.finite(df$pred)
    if (use_weights) {
      w <- df[[weight_col]]
      sum(w[ok], na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      mean(ok)
    }
  }, error = function(e) NA_real_)

  list(
    overall = overall,
    by_year = by_year,
    meta = list(
      model_kind = model_kind,
      coverage_weight_overall = cov_w,
      pov_lines = c(pline_value),
      pline_value = pline_value,         # model-space
      pline_level = pline_level,         # level-space
      pred_scale = pred_scale,
      weight_col = if (use_weights) weight_col else "<unweighted>"
    )
  )
}

# One-shot runner: B -> C -> D -> E
run_historical_pipeline_core <- function(step1, board, hist_years, pline_value = 3.0, pin_name_prefix = NULL, progress = NULL) {
  if (is.null(progress)) progress <- function(...) NULL

  progress("Phase B: hazards")
  b <- phase_b_historical_core(step1 = step1, board = board, hist_years = hist_years, pin_name_prefix = pin_name_prefix)

  progress("Phase C: sim panel")
  c <- phase_c_historical_core(step1 = step1, built_haz = b)

  progress("Phase D: predict")
  d <- phase_d_historical_core(step1 = step1, sim_panel = c$sim_panel)

  progress("Phase E: poverty")
  e <- phase_e_historical_core(step1 = step1, pred_panel = d$pred_panel, pline_value = pline_value)

  list(
    phase_b = b,
    phase_c = c,
    phase_d = d,
    phase_e = e
  )
}

# ------------------------------------------------------------------------------
# Artifact validators (Phase B–E)
# ------------------------------------------------------------------------------
# These are lightweight “schema + sanity” checks that run after each phase.
# They return a uniform list(ok, msg, details) and are intended to be stored in
# run$checks so diagnostics can be shown without re-computing phase cores.

mk_check <- function(ok, msg, details = NULL) {
  list(ok = isTRUE(ok), msg = as.character(msg), details = details)
}

validate_phase_b_artifact <- function(res_b) {
  if (is.null(res_b) || !is.list(res_b)) {
    return(mk_check(FALSE, "FAIL | Phase B artifact is NULL/invalid"))
  }
  lw <- res_b$loc_weather_sim
  if (is.null(lw) || !is.data.frame(lw) || !nrow(lw)) {
    return(mk_check(FALSE, "FAIL | Phase B loc_weather_sim is NULL/empty"))
  }
  haz_cols <- grep("^haz_", names(lw), value = TRUE)
  if (!length(haz_cols)) {
    return(mk_check(FALSE, "FAIL | Phase B loc_weather_sim has 0 haz_* columns"))
  }

  # Key sanity
  has_loc <- "loc_id" %in% names(lw)
  has_ts  <- "timestamp" %in% names(lw)
  if (!has_loc || !has_ts) {
    return(mk_check(FALSE, paste0(
      "FAIL | Phase B loc_weather_sim missing cols: ",
      paste(setdiff(c("loc_id", "timestamp"), names(lw)), collapse = ", ")
    )))
  }

  # Duplicate key rate after collapsing timestamp -> (sim_year, month)
  dup_n <- NA_integer_
  tryCatch({
    tmp <- lw |>
      dplyr::mutate(
        sim_year = lubridate::year(.data$timestamp),
        month    = lubridate::month(.data$timestamp)
      ) |>
      dplyr::count(.data$loc_id, .data$sim_year, .data$month, name = "n") |>
      dplyr::filter(.data$n > 1)
    dup_n <- nrow(tmp)
  }, error = function(e) NULL)

  msg <- paste0(
    "OK | rows=", nrow(lw),
    " | haz_cols=", length(haz_cols),
    if (!is.na(dup_n)) paste0(" | dup(loc_id,sim_year,month)=", dup_n) else ""
  )

  mk_check(TRUE, msg, details = list(
    rows = nrow(lw),
    haz_cols = haz_cols,
    dup_loc_year_month = dup_n,
    years = res_b$sim_year_range
  ))
}

validate_phase_c_artifact <- function(res_c, model = NULL) {
  if (is.null(res_c) || !is.list(res_c) || is.null(res_c$sim_panel)) {
    return(mk_check(FALSE, "FAIL | Phase C artifact missing sim_panel"))
  }
  sp <- res_c$sim_panel
  if (!is.data.frame(sp) || !nrow(sp)) {
    return(mk_check(FALSE, "FAIL | Phase C sim_panel is NULL/empty"))
  }

  req <- c("unit_id", "loc_id", "sim_year", "month")
  miss <- setdiff(req, names(sp))
  if (length(miss)) {
    return(mk_check(FALSE, paste0("FAIL | Phase C sim_panel missing: ", paste(miss, collapse = ", "))))
  }

  haz_cols <- grep("^haz_", names(sp), value = TRUE)
  haz_na <- NA_real_
  if (length(haz_cols)) {
    haz_na <- mean(is.na(sp[[haz_cols[[1]]]]))
  }

  # Model predictor coverage (non-hazard predictors)
  miss_pred <- character()
  n_pred <- NA_integer_
  if (!is.null(model)) {
    f <- tryCatch(stats::formula(model), error = function(e) NULL)
    if (!is.null(f)) {
      vars <- all.vars(f)
      resp <- as.character(f[[2]])
      preds <- setdiff(vars, resp)
      preds <- setdiff(preds, grep("^haz_", preds, value = TRUE))
      n_pred <- length(preds)
      miss_pred <- setdiff(preds, names(sp))
    }
  }

  ok <- length(miss_pred) == 0
  msg <- paste0(
    if (ok) "OK" else "FAIL",
    " | rows=", nrow(sp),
    " | haz_cols=", length(haz_cols),
    if (!is.na(haz_na)) paste0(" | haz_na(first)=", round(haz_na, 4)) else "",
    if (!is.na(n_pred)) paste0(" | model_preds=", n_pred) else "",
    if (length(miss_pred)) paste0(" | missing_preds=", paste(miss_pred, collapse = ", ")) else ""
  )

  mk_check(ok, msg, details = list(
    rows = nrow(sp),
    haz_cols = haz_cols,
    haz_na_first = haz_na,
    missing_model_preds = miss_pred
  ))
}

validate_phase_d_artifact <- function(res_d) {
  if (is.null(res_d) || !is.list(res_d) || is.null(res_d$pred_panel)) {
    return(mk_check(FALSE, "FAIL | Phase D artifact missing pred_panel"))
  }
  pp <- res_d$pred_panel
  if (!is.data.frame(pp) || !nrow(pp) || !"pred" %in% names(pp)) {
    return(mk_check(FALSE, "FAIL | Phase D pred_panel is NULL/empty or missing pred"))
  }

  na_rate <- mean(is.na(pp$pred))
  wcol <- detect_weight_col(pp, varlist = NULL)
  wtxt <- if (is.null(wcol)) "<unweighted>" else wcol

  mk_check(TRUE, paste0(
    "OK | rows=", nrow(pp),
    " | na_pred=", round(na_rate, 4),
    " | weight=", wtxt
  ), details = list(
    rows = nrow(pp),
    na_rate = na_rate,
    weight_col = wtxt
  ))
}

validate_phase_e_artifact <- function(res_e) {
  if (is.null(res_e) || !is.list(res_e)) {
    return(mk_check(FALSE, "FAIL | Phase E artifact is NULL/invalid"))
  }
  if (is.null(res_e$overall) || !is.data.frame(res_e$overall) || !nrow(res_e$overall)) {
    return(mk_check(FALSE, "FAIL | Phase E overall table missing/empty"))
  }
  if (is.null(res_e$by_year) || !is.data.frame(res_e$by_year) || !nrow(res_e$by_year)) {
    return(mk_check(FALSE, "FAIL | Phase E by_year table missing/empty"))
  }

  # Require FGT columns (D1 correctness layer)
  need_cols_overall <- c("pov_rate", "fgt0", "fgt1", "fgt2")
  miss_overall <- setdiff(need_cols_overall, names(res_e$overall))
  if (length(miss_overall)) {
    return(mk_check(FALSE, paste0("FAIL | Phase E overall missing cols: ", paste(miss_overall, collapse = ", "))))
  }

  need_cols_by_year <- c("sim_year", "pov_rate", "fgt0", "fgt1", "fgt2")
  miss_by_year <- setdiff(need_cols_by_year, names(res_e$by_year))
  if (length(miss_by_year)) {
    return(mk_check(FALSE, paste0("FAIL | Phase E by_year missing cols: ", paste(miss_by_year, collapse = ", "))))
  }

  mk_check(TRUE, paste0(
    "OK | overall_rows=", nrow(res_e$overall),
    " | by_year_rows=", nrow(res_e$by_year),
    " | fgt_cols=3"
  ), details = list(
    overall_rows = nrow(res_e$overall),
    by_year_rows = nrow(res_e$by_year),
    fgt_cols = c("fgt0", "fgt1", "fgt2")
  ))
}

format_checks_short <- function(checks) {
  if (is.null(checks) || !is.list(checks) || !length(checks)) return("No checks yet.")
  parts <- lapply(names(checks), function(nm) {
    ch <- checks[[nm]]
    if (is.null(ch)) return(NULL)
    paste0(nm, "=", if (isTRUE(ch$ok)) "OK" else "FAIL")
  })
  parts <- Filter(Negate(is.null), parts)
  paste(parts, collapse = " | ")
}

# ------------------------------------------------------------------------------
# Minimal ScenarioSpec helpers
# ------------------------------------------------------------------------------
# A ScenarioSpec is a small named list stored under run$meta$scenario_spec.

make_scenario_spec <- function(type,
                               id = NULL,
                               label = NULL,
                               hazard_source = NULL,
                               years = NULL,
                               ...) {
  type <- as.character(type)
  if (is.null(id) || !nzchar(id)) {
    id <- paste0(type, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  if (is.null(label) || !nzchar(label)) label <- type

  spec <- list(
    type = type,
    id = as.character(id),
    label = as.character(label),
    hazard_source = if (is.null(hazard_source)) NULL else as.character(hazard_source),
    years = years,
    created_at = Sys.time()
  )

  extra <- list(...)
  if (length(extra)) spec <- c(spec, extra)
  spec
}

format_scenario_spec_short <- function(spec) {
  if (is.null(spec) || !is.list(spec) || is.null(spec$type)) return("(no scenario)")
  yrs <- spec$years
  yrs_txt <- if (!is.null(yrs) && length(yrs) == 2 && all(is.finite(as.numeric(yrs)))) {
    paste0(yrs[[1]], "-", yrs[[2]])
  } else {
    "(years ? )"
  }
  hz <- if (!is.null(spec$hazard_source) && nzchar(spec$hazard_source)) spec$hazard_source else "(hazard ? )"
  paste0(spec$type, " | years=", yrs_txt, " | hazard=", hz)
}
