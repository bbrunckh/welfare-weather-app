# Policy scenario discovery and diagnostics ----
# Pure functions for policy scenario variable discovery and diagnostics.
# Stateless and testable without Shiny.


#' Internal column name holding the per-household SP cash-transfer amount
#'
#' Set by \code{apply_policy_to_svy()}, read by \code{run_sim_pipeline()} (which
#' adds it to predicted welfare on the level scale), the diagnostics module,
#' and the decomposition. The leading-dot prefix marks it as an internal column
#' and the suffix makes it unlikely to collide with a user-supplied variable.
#' Treat this constant as the single source of truth across the codebase.
#' @export
SP_TRANSFER_COL <- ".wiseapp_sp_transfer"


# Policy activity predicates ----

# Keep the summary card and policy application on the same definition of an
# active lever. Selecting a policy domain alone is not an intervention.
.lever_moved <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x != 0
}

.access_moved <- function(universal, change_pct) {
  isTRUE(universal) || .lever_moved(change_pct)
}

# Sector sliders carry a target share only when the user changed it from the
# observed share; NULL means "unchanged", and 0 is a valid target.
.sector_target_set <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

# Exact lever/term matching (CR-BUG-08) ----

#' Match a policy lever variable against model variable or coefficient names
#'
#' The single matcher shared by the lever UI gating, `apply_policy_to_svy()`
#' gating and the decomposition term maps. Each name is split on `:` and a
#' component matches when it equals `var`, or `var` followed by one of its
#' factor levels (`values` is the survey column, used only to derive the
#' levels of factor, character or logical columns). Matching is exact and
#' case-insensitive, so `piped` never matches `piped_to_prem`.
#'
#' @param var Lever variable name (scalar).
#' @param terms Character vector of model variable or coefficient names.
#' @param values Optional survey column for `var`.
#' @param partner Optional weather variable. When NULL, returns the main-effect
#'   terms of `var` (no `:`), exact name first. When supplied, returns the
#'   two-way interaction terms whose other component starts with `partner`.
#' @param any_component When TRUE, returns every name in which `var` appears
#'   as a component (main effect or interaction); used for lever gating.
#' @return Character vector of matching names.
#' @keywords internal
.policy_lever_terms <- function(var, terms, values = NULL, partner = NULL,
                                any_component = FALSE) {
  terms <- as.character(terms %||% character(0))
  if (!length(terms) || is.null(var) || !nzchar(var)) {
    return(character(0))
  }
  lvls <- if (is.factor(values)) {
    levels(values)
  } else if (is.character(values)) {
    unique(values[!is.na(values)])
  } else if (is.logical(values)) {
    "TRUE"
  } else {
    character(0)
  }
  keys <- tolower(unique(c(var, paste0(var, lvls))))
  parts <- strsplit(tolower(terms), ":", fixed = TRUE)
  hit <- vapply(parts, function(p) {
    if (isTRUE(any_component)) {
      return(any(p %in% keys))
    }
    if (is.null(partner)) {
      return(length(p) == 1L && p %in% keys)
    }
    length(p) == 2L && sum(p %in% keys) == 1L &&
      startsWith(p[!p %in% keys], tolower(partner))
  }, logical(1))
  out <- terms[hit]
  # Prefer the exact variable name over level-suffixed terms.
  out[order(tolower(out) != tolower(var))]
}

#' Is a lever variable referenced by the Step 1 model?
#' @param var Lever variable name(s).
#' @param model_vars Model variable or term names.
#' @return Logical vector, one per `var`.
#' @keywords internal
.lever_in_model <- function(var, model_vars) {
  vapply(var, function(v) {
    length(.policy_lever_terms(v, model_vars, any_component = TRUE)) > 0L
  }, logical(1), USE.NAMES = FALSE)
}

#' Has the infrastructure scenario been changed from its defaults?
#' @param infra Scenario list from `mod_3_02_infra_server()`.
#' @return Scalar logical.
#' @export
has_infra_change <- function(infra) {
  if (!is.list(infra)) {
    return(FALSE)
  }
  .access_moved(infra$elec_universal, infra$elec_access_change_pct) ||
    .access_moved(infra$water_universal, infra$water_access_change_pct) ||
    .access_moved(
      infra$sanitation_universal,
      infra$sanitation_access_change_pct
    ) ||
    .lever_moved(infra$health_travel_pct) ||
    identical(infra$health_mode, "max") ||
    .access_moved(infra$piped_universal, infra$piped_access_change_pct) ||
    .access_moved(
      infra$piped_to_prem_universal,
      infra$piped_to_prem_access_change_pct
    ) ||
    .access_moved(
      infra$imp_wat_san_universal,
      infra$imp_wat_san_access_change_pct
    )
}

#' Has the digital inclusion scenario been changed from its defaults?
#' @param digital Scenario list from `mod_3_03_digital_server()`.
#' @return Scalar logical.
#' @export
has_digital_change <- function(digital) {
  if (!is.list(digital)) {
    return(FALSE)
  }
  .access_moved(
    digital$internet_universal,
    digital$internet_access_change_pct
  ) ||
    .access_moved(
      digital$mobile_universal,
      digital$mobile_access_change_pct
    )
}

#' Has the education scenario been changed from its defaults?
#' @param education Scenario list from `mod_3_05_education_server()`.
#' @return Scalar logical.
#' @export
has_education_change <- function(education) {
  if (!is.list(education)) {
    return(FALSE)
  }
  .access_moved(
    education$primary_universal,
    education$primary_access_change_pct
  ) ||
    .access_moved(
      education$secondary_universal,
      education$secondary_access_change_pct
    ) ||
    .access_moved(
      education$postsec_universal,
      education$postsec_access_change_pct
    )
}

#' Has the labor market scenario been changed from its defaults?
#' @param labor Scenario list from `mod_3_04_labor_server()`.
#' @return Scalar logical.
#' @export
has_labor_change <- function(labor) {
  if (!is.list(labor)) {
    return(FALSE)
  }
  .lever_moved(labor$employment_change_pp) ||
    .sector_target_set(labor$sector_manufacturing) ||
    .sector_target_set(labor$sector_services)
}

#' Observed sector shares of the working population
#'
#' Initial values for the labour lever's sector sliders, so an untouched
#' slider means "no change". Uses survey `weight` when present (as the SP
#' reach preview does), otherwise row counts.
#' @param svy Survey frame with `employed`, `selfemployed`, `agriculture`,
#'   `industry` and `services`.
#' @return Named numeric vector (`industry`, `services`, `agriculture`) of
#'   percentages, or NULL when the shares cannot be computed.
#' @keywords internal
.labor_sector_shares <- function(svy) {
  need <- c("employed", "selfemployed", "agriculture", "industry", "services")
  if (!is.data.frame(svy) || !all(need %in% names(svy))) {
    return(NULL)
  }
  working <- (svy$employed == 1L | svy$selfemployed == 1L) &
    !is.na(svy$employed) & !is.na(svy$selfemployed)
  w <- if ("weight" %in% names(svy)) {
    suppressWarnings(as.numeric(svy$weight))
  } else {
    rep(1, nrow(svy))
  }
  w[!is.finite(w) | w < 0] <- 0
  total <- sum(w[working])
  if (!any(working) || total <= 0) {
    return(NULL)
  }
  vapply(c("industry", "services", "agriculture"), function(sec) {
    100 * sum(w[working & svy[[sec]] == 1L], na.rm = TRUE) / total
  }, numeric(1))
}

#' Has social protection been configured to spend money?
#' @param sp Scenario list from `mod_3_01_sp_server()`.
#' @return Scalar logical.
#' @export
has_sp_change <- function(sp) {
  if (!is.list(sp)) {
    return(FALSE)
  }
  if (identical(sp$budget_mode %||% "transfer_first", "budget_first")) {
    .lever_moved(sp$budget_fixed)
  } else {
    .lever_moved(sp$transfer_amount_usd) &&
      .lever_moved(sp$transfer_n_payments)
  }
}


#' Known survey columns that policy levers can modify
#'
#' Keep changed-column scans bounded to the columns the Step 3 policy
#' application can actually mutate. Social protection is handled separately
#' through `SP_TRANSFER_COL`; diagnostics optionally include the outcome after
#' applying that transfer to a local copy.
#'
#' @param infra,digital,labor,education Optional policy scenario lists. When
#'   none are supplied, all known policy covariates are returned.
#' @param model_vars Optional model terms used to apply the same gating as
#'   `apply_policy_to_svy()`.
#' @param outcome Optional outcome column to include (diagnostics only).
#' @return Character vector in stable policy-definition order.
#' @keywords internal
.policy_candidate_cols <- function(infra = NULL, digital = NULL, labor = NULL,
                                   education = NULL, model_vars = NULL,
                                   outcome = NULL) {
  definition_vars <- unlist(
    lapply(POLICY_DEFINITIONS, function(def) def$vars %||% character()),
    use.names = FALSE
  )
  labor_vars <- c(
    "employed", "selfemployed", "unemployed",
    "agriculture", "industry", "services"
  )
  all_candidates <- unique(c(definition_vars, labor_vars))

  scenarios_supplied <- !all(vapply(
    list(infra, digital, labor, education), is.null, logical(1)
  ))
  if (!scenarios_supplied) {
    return(unique(c(all_candidates, outcome)))
  }

  changed <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x)) {
      return(FALSE)
    }
    if (is.logical(x)) isTRUE(x) else x != 0
  }
  candidates <- character()

  if (!is.null(infra)) {
    if (isTRUE(infra$elec_universal) || changed(infra$elec_access_change_pct)) {
      candidates <- c(candidates, "electricity")
    }
    if (isTRUE(infra$water_universal) || changed(infra$water_access_change_pct)) {
      candidates <- c(candidates, "imp_wat_rec")
    }
    if (isTRUE(infra$sanitation_universal) ||
      changed(infra$sanitation_access_change_pct)) {
      candidates <- c(candidates, "imp_san_rec")
    }
    if (changed(infra$health_travel_pct) ||
      identical(infra$health_mode, "max")) {
      candidates <- c(candidates, "ttime_health")
    }
    if (isTRUE(infra$piped_universal) || changed(infra$piped_access_change_pct)) {
      candidates <- c(candidates, "piped")
    }
    if (isTRUE(infra$piped_to_prem_universal) ||
      changed(infra$piped_to_prem_access_change_pct)) {
      candidates <- c(candidates, "piped_to_prem")
    }
    if (isTRUE(infra$imp_wat_san_universal) ||
      changed(infra$imp_wat_san_access_change_pct)) {
      candidates <- c(candidates, "imp_wat_san_rec")
    }
  }
  if (!is.null(digital)) {
    if (isTRUE(digital$internet_universal) ||
      changed(digital$internet_access_change_pct)) {
      candidates <- c(candidates, "internet")
    }
    if (isTRUE(digital$mobile_universal) ||
      changed(digital$mobile_access_change_pct)) {
      candidates <- c(candidates, "cellphone")
    }
  }
  if (!is.null(education)) {
    if (isTRUE(education$primary_universal) ||
      changed(education$primary_access_change_pct)) {
      candidates <- c(candidates, "educ_com1_hh")
    }
    if (isTRUE(education$secondary_universal) ||
      changed(education$secondary_access_change_pct)) {
      candidates <- c(candidates, "educ_com2_hh")
    }
    if (isTRUE(education$postsec_universal) ||
      changed(education$postsec_access_change_pct)) {
      candidates <- c(candidates, "educ_com3_hh")
    }
  }
  if (!is.null(labor)) {
    if (changed(labor$employment_change_pp)) {
      candidates <- c(candidates, "employed", "selfemployed", "unemployed")
    }
    if (.sector_target_set(labor$sector_manufacturing) ||
      .sector_target_set(labor$sector_services)) {
      candidates <- c(
        candidates, "employed", "selfemployed",
        "agriculture", "industry", "services"
      )
    }
  }

  candidates <- unique(candidates)
  if (!is.null(model_vars)) {
    candidates <- candidates[.lever_in_model(candidates, model_vars)]
  }
  unique(c(candidates, outcome))
}


#' Population-level cost of an applied social-protection transfer
#'
#' The single implementation of the transfer arithmetic the Step 3 diagnostics
#' tab reports. Reads the transfer column `apply_policy_to_svy()` wrote, so it
#' cannot drift from what was actually applied.
#'
#' `SP_TRANSFER_COL` holds a *daily* amount, already divided by household size
#' where welfare is per-capita (analysis unit "hh"). With survey weights, each
#' household row's weight represents people, so multiplying the per-capita
#' transfer by that weight gives the population cost. Without weights, each row
#' represents one household and `hhsize` restores its household-level amount.
#'
#' @param svy_policy    Survey frame after `apply_policy_to_svy()`.
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#'
#' @return A named list: `total` (annual population cost), `per_unit` (annual
#'   amount per recipient), `n_recipients` (sample rows receiving a transfer),
#'   `n_recipients_weighted` (people represented),
#'   `n_households_weighted` (recipient households represented in household
#'   mode) and `weighted`
#'   (whether survey weights were used).
#' @keywords internal
.sp_transfer_totals <- function(svy_policy, analysis_unit = "hh") {
  if (is.null(svy_policy) || !is.data.frame(svy_policy) ||
    !SP_TRANSFER_COL %in% names(svy_policy)) {
    return(.sp_transfer_totals_values(NULL, svy_policy, analysis_unit))
  }
  .sp_transfer_totals_values(
    svy_policy[[SP_TRANSFER_COL]], svy_policy, analysis_unit
  )
}

.sp_transfer_totals_values <- function(v, svy, analysis_unit = "hh") {
  zero <- list(
    total = 0, per_unit = 0, n_recipients = 0L,
    n_recipients_weighted = 0, weighted = FALSE
  )
  if (is.null(svy) || !is.data.frame(svy) || is.null(v) ||
    length(v) != nrow(svy)) {
    return(zero)
  }

  v <- suppressWarnings(as.numeric(v))

  has_w <- "weight" %in% names(svy)
  w <- if (has_w) {
    suppressWarnings(as.numeric(svy$weight))
  } else {
    rep(1, nrow(svy))
  }

  # In household mode, row weights represent population; divide by household
  # size to recover recipient-household equivalents for cost arithmetic.
  hh <- if (identical(analysis_unit, "hh") && "hhsize" %in% names(svy)) {
    h <- suppressWarnings(as.numeric(svy$hhsize))
    h[!is.finite(h) | h <= 0] <- 1
    h
  } else {
    rep(1, nrow(svy))
  }

  ok <- is.finite(v) & is.finite(w)
  if (!any(ok)) {
    return(zero)
  }

  cost_scale <- if (identical(analysis_unit, "hh") && !has_w) {
    hh
  } else {
    rep(1, nrow(svy))
  }
  total <- sum(v[ok] * w[ok] * cost_scale[ok]) * 365

  elig <- ok & v > 0
  unit_weight <- if (identical(analysis_unit, "hh") && has_w) w / hh else w
  per_unit <- if (any(elig) && sum(unit_weight[elig]) > 0) {
    (sum(v[elig] * w[elig] * cost_scale[elig]) /
       sum(unit_weight[elig])) * 365
  } else {
    0
  }

  list(
    total                 = total,
    per_unit              = per_unit,
    n_recipients          = sum(elig),
    n_recipients_weighted = if (any(elig)) sum(w[elig]) else 0,
    n_households_weighted = if (identical(analysis_unit, "hh")) {
      if (any(elig)) sum(unit_weight[elig]) else 0
    } else {
      NA_real_
    },
    weighted              = has_w
  )
}

# Build the transfer vector from an eligibility draw. Both the preview and
# apply_policy_to_svy() use this arithmetic; only the latter writes the vector
# onto the survey as SP_TRANSFER_COL.
.sp_transfer_values <- function(svy, sp, analysis_unit, eligible) {
  if (length(eligible) != nrow(svy) || !"welfare" %in% names(svy)) {
    return(NULL)
  }

  hhsize_scale <- if (identical(analysis_unit, "hh") && "hhsize" %in% names(svy)) {
    hs <- suppressWarnings(as.numeric(svy$hhsize))
    hs[!is.finite(hs) | hs <= 0] <- 1
    hs
  } else {
    rep_len(1, nrow(svy))
  }

  if (sp$budget_mode == "transfer_first") {
    daily_transfer <- (sp$transfer_amount_usd * sp$transfer_n_payments) / 365
    return(ifelse(eligible, daily_transfer / hhsize_scale, 0))
  }

  if (sp$budget_mode == "budget_first") {
    n_eligible <- sum(eligible)
    if (n_eligible <= 0) return(NULL)
    w_elig <- if ("weight" %in% names(svy)) {
      w <- suppressWarnings(as.numeric(svy$weight[eligible]))
      valid <- is.finite(w) & w > 0
      if (identical(analysis_unit, "hh")) {
        hs <- hhsize_scale[eligible]
        sum(w[valid] / hs[valid], na.rm = TRUE)
      } else {
        sum(w[valid], na.rm = TRUE)
      }
    } else {
      0
    }
    divisor <- if (w_elig > 0) w_elig else n_eligible
    daily_transfer <- (sp$budget_fixed / divisor) / 365
    return(ifelse(eligible, daily_transfer / hhsize_scale, 0))
  }

  NULL
}


#' Reach and cost of a social-protection scenario, before it is run
#'
#' UI-32: the targeting controls used to give no feedback until a full policy
#' simulation had run, so picking a cutoff was guesswork. This answers "who
#' does this reach, and what does it cost?" directly from the survey and the
#' scenario.
#'
#' The numbers use the same seeded eligibility function and transfer
#' arithmetic as the run, without materializing a policy copy of the survey.
#' `.sp_transfer_totals()` remains the single implementation of the arithmetic
#' the diagnostics tab reports for the run's `.wiseapp_sp_transfer` column.
#'
#' The caller must pass the survey frame the *run* will use - Step 2 may have
#' filtered `survey_weather()` down to one baseline round, and totals computed
#' over every round instead would overstate both the eligible population and
#' the cost.
#'
#' @param svy Survey frame the policy run will use (`hist_sim()$svy`, falling
#'   back to `survey_weather()`). Needs `welfare`; `weight` and `hhsize` are
#'   used when present.
#' @param sp  Scenario list from `mod_3_01_sp_server()`.
#' @param analysis_unit `"hh"`, `"ind"` or `"firm"`.
#' @param seed Base seed; must match the one passed to `apply_policy_to_svy()`.
#'
#' @return A named list, or NULL when the survey cannot support the estimate:
#'   \describe{
#'     \item{n_rows}{Sample rows receiving a transfer.}
#'     \item{n_total}{Sample rows in the survey.}
#'     \item{n_pop}{Person-weighted recipient count (sample count when the
#'       survey carries no weights).}
#'     \item{n_recipient_units}{Recipient count, using weight / household size
#'       when household rows are person weighted.}
#'     \item{share_pct}{Weighted recipient share of the population, 0-100.}
#'     \item{weighted}{TRUE when survey weights were used.}
#'     \item{transfer_per_unit}{Annual transfer per recipient.}
#'     \item{transfer_total}{Annual population-level cost.}
#'     \item{budget_first}{TRUE when the per-unit amount was derived from a
#'       fixed budget rather than set directly.}
#'   }
#' @keywords internal
.sp_scenario_reach <- function(svy, sp, analysis_unit = "hh",
                               seed = WISEAPP_DEFAULT_SEED) {
  if (is.null(svy) || is.null(sp) || !is.data.frame(svy) || nrow(svy) == 0) {
    return(NULL)
  }
  if (!"welfare" %in% names(svy)) {
    return(NULL)
  }

  # The run and preview share one eligibility draw and transfer calculation;
  # the preview needs only this vector and the columns used by its totals.
  values <- tryCatch(
    withr::with_seed(wise_seed(seed, "policy"), {
      eligible <- .determine_sp_eligibility(svy, sp)
      transfer <- .sp_transfer_values(svy, sp, analysis_unit, eligible)
      list(eligible = eligible, transfer = transfer)
    }),
    error = function(e) NULL
  )
  if (is.null(values) || is.null(values$transfer) ||
    length(values$eligible) != nrow(svy)) {
    return(NULL)
  }
  eligible <- values$eligible
  totals <- .sp_transfer_totals_values(values$transfer, svy, analysis_unit)

  # Eligibility is reported separately from receipt: a scenario with a zero
  # transfer still targets a population, and saying "0 eligible" there would
  # be misleading.
  eligible[is.na(eligible)] <- FALSE

  w <- if ("weight" %in% names(svy)) {
    suppressWarnings(as.numeric(svy$weight))
  } else {
    rep(1, nrow(svy))
  }
  w[!is.finite(w) | w < 0] <- 0
  weighted <- "weight" %in% names(svy) && sum(w) > 0
  if (!weighted) w <- rep(1, nrow(svy))

  w_elig <- sum(w[eligible])
  w_total <- sum(w)
  hhsize <- if (identical(analysis_unit, "hh") && "hhsize" %in% names(svy)) {
    hs <- suppressWarnings(as.numeric(svy$hhsize))
    hs[!is.finite(hs) | hs <= 0] <- 1
    hs
  } else {
    rep(1, nrow(svy))
  }
  household_weights <- if (identical(analysis_unit, "hh") && weighted) {
    w / hhsize
  } else {
    w
  }

  list(
    n_rows = sum(eligible),
    n_total = nrow(svy),
    n_pop = w_elig,
    n_recipient_units = sum(household_weights[eligible]),
    n_households_total = sum(household_weights),
    share_pct = if (w_total > 0) 100 * w_elig / w_total else NA_real_,
    weighted = weighted,
    transfer_per_unit = totals$per_unit,
    transfer_total = totals$total,
    budget_first = identical(
      sp$budget_mode %||% "transfer_first",
      "budget_first"
    )
  )
}


#' Does a policy scenario actually change the survey?
#'
#' TRUE when \code{apply_policy_to_svy()} moved anything at all - a covariate
#' lever, or a non-zero social-protection transfer. Unlike
#' \code{.compute_policy_deltas()} (which deliberately excludes
#' \code{SP_TRANSFER_COL}, since the transfer enters through welfare rather
#' than as a covariate), an SP-only scenario counts as an effect here.
#'
#' Used to warn - never to block - when every lever sits at its zero default,
#' so a run that will reproduce the baseline says so instead of looking like
#' a simulation that failed to start.
#'
#' @param svy_baseline Baseline survey-weather data frame.
#' @param svy_policy   Policy-adjusted survey-weather data frame.
#' @return Scalar logical.
#' @keywords internal
.scenario_has_effect <- function(svy_baseline, svy_policy,
                                 candidates = NULL) {
  if (is.null(svy_baseline) || is.null(svy_policy)) {
    return(FALSE)
  }

  # Social protection: any non-zero transfer is an effect on its own.
  sp <- svy_policy[[SP_TRANSFER_COL]]
  if (!is.null(sp)) {
    sp <- suppressWarnings(as.numeric(sp))
    if (any(is.finite(sp) & abs(sp) > 1e-10)) {
      return(TRUE)
    }
  }

  # Covariate levers: only columns apply_policy_to_svy() can mutate. Survey
  # frames are wide, while this set is small and fixed by the policy schema.
  shared <- intersect(names(svy_baseline), names(svy_policy))
  if (!is.null(candidates)) shared <- shared[shared %in% candidates]
  for (col in shared) {
    b <- svy_baseline[[col]]
    p <- svy_policy[[col]]
    if (is.factor(b)) b <- as.character(b)
    if (is.factor(p)) p <- as.character(p)
    if (is.numeric(b) && is.numeric(p)) {
      if (any(abs(p - b) > 1e-10, na.rm = TRUE)) {
        return(TRUE)
      }
    } else if (!identical(b, p)) {
      return(TRUE)
    }
  }
  FALSE
}


#' Build the immutable diagnostics snapshot for one successful policy run
#'
#' Computes changed-variable and numeric summaries once, before the policy run
#' publishes any reactive state. The caller stores the returned ordinary R
#' object in one reactive value so renderers and exports observe the same run.
#'
#' @param svy_baseline Baseline survey frame.
#' @param svy_policy Policy-adjusted survey frame.
#' @param outcome Outcome column whose post-transfer values are diagnosed.
#' @param analysis_unit Analysis unit used for transfer totals.
#' @param candidates Known columns that the active policy configuration can
#'   modify, plus `outcome` when transfer-adjusted welfare should be shown.
#' @param sp Social-protection scenario used to snapshot eligibility before
#'   targeting errors.
#' @return A diagnostics snapshot, or NULL when either frame is unavailable.
#' @keywords internal
.policy_diagnostics_snapshot <- function(svy_baseline, svy_policy,
                                         outcome = "welfare",
                                         analysis_unit = "hh",
                                         candidates = .policy_candidate_cols(
                                           outcome = outcome
                                         ),
                                         sp = NULL) {
  if (is.null(svy_baseline) || is.null(svy_policy)) {
    return(NULL)
  }

  policy_diag <- svy_policy
  if (SP_TRANSFER_COL %in% names(policy_diag) &&
    outcome %in% names(policy_diag)) {
    policy_diag[[outcome]] <- policy_diag[[outcome]] +
      policy_diag[[SP_TRANSFER_COL]]
  }

  totals <- .sp_transfer_totals(policy_diag, analysis_unit)
  vars <- detect_manipulated_vars(
    svy_baseline, policy_diag,
    candidates = candidates
  )

  input_summary <- if (length(vars) > 0L) {
    policy_input_diagnostics(svy_baseline, policy_diag, vars = vars)
  } else {
    NULL
  }
  baseline_values <- lapply(vars, function(v) svy_baseline[[v]])
  policy_values <- lapply(vars, function(v) policy_diag[[v]])
  names(baseline_values) <- names(policy_values) <- vars
  changed_counts <- .policy_changed_counts(svy_baseline, policy_diag, vars)
  eligibility <- tryCatch(
    if (!is.null(sp)) {
      .determine_sp_eligibility(svy_baseline, sp, apply_errors = FALSE)
    } else {
      NULL
    },
    error = function(e) NULL
  )

  list(
    status = if (length(vars) == 0L) "no_change" else NULL,
    manipulated_vars = vars,
    baseline_values = baseline_values,
    policy_values = policy_values,
    changed_counts = changed_counts,
    transfer_sum = totals$total,
    transfer_pp = totals$per_unit,
    transfer_households = totals$n_households_weighted,
    input_summary = input_summary,
    analysis_unit = analysis_unit,
    treatment_matrix = policy_treatment_matrix(
      svy_baseline, policy_diag,
      eligibility = eligibility,
      analysis_unit = analysis_unit
    ),
    component_matrix = policy_component_matrix(
      svy_baseline, policy_diag,
      analysis_unit = analysis_unit,
      candidates = unique(c(candidates, SP_TRANSFER_COL))
    )
  )
}


# Policy scenario candidate discovery ----

#' Identify candidate variables in the variable list matching given patterns
#'
#' @param variable_list A data frame with columns \code{name}, \code{label},
#'   and role flags \code{ind}, \code{hh}, \code{firm}, \code{area}.
#' @param patterns Character vector of regex patterns matched (case-insensitive)
#'   against the \code{name} column.
#'
#' @return A data frame with columns \code{name}, \code{label}, \code{level},
#'   or \code{NULL} if no matches.
#' @export
policy_candidate_info <- function(variable_list, patterns) {
  if (is.null(variable_list) || nrow(variable_list) == 0) {
    return(NULL)
  }
  nm <- variable_list$name
  if (is.null(nm)) {
    return(NULL)
  }
  mask <- Reduce(`|`, lapply(patterns, function(p) grepl(p, nm, ignore.case = TRUE)))
  if (!any(mask)) {
    return(NULL)
  }
  df <- variable_list[mask, , drop = FALSE]
  level <- vapply(seq_len(nrow(df)), function(i) {
    for (lvl in c("ind", "hh", "firm", "area")) {
      v <- df[[lvl]][i]
      if (!is.null(v) && !is.na(v) && v == 1L) {
        return(switch(lvl,
          ind = "Individual",
          hh = "Household",
          firm = "Firm",
          area = "Area"
        ))
      }
    }
    ""
  }, character(1))
  data.frame(
    name = df$name,
    label = if (!is.null(df$label)) df$label else df$name,
    level = level,
    stringsAsFactors = FALSE
  )
}

#' Build a placeholder tag listing selectable candidate variables
#'
#' @param category_label Human-readable category name (e.g. "infrastructure").
#' @param candidate_df Data frame from \code{policy_candidate_info()}.
#'
#' @return A Shiny tag.
#' @export
policy_placeholder_tag <- function(category_label, candidate_df) {
  msg_head <- paste0(
    "None of the relevant variables for ", category_label,
    " have been selected in the Step 1 model, so they cannot be manipulated here. ",
    "Go back to Step 1 to include them in the model."
  )
  if (is.null(candidate_df) || nrow(candidate_df) == 0) {
    return(shiny::div(
      class = "alert alert-info",
      msg_head, shiny::tags$br(),
      shiny::tags$em("No candidate variables were found in the variable list for this category.")
    ))
  }
  items <- lapply(seq_len(nrow(candidate_df)), function(i) {
    lvl <- candidate_df$level[i]
    shiny::tags$li(paste0(
      candidate_df$label[i],
      if (nzchar(lvl)) paste0(" (", lvl, ")") else ""
    ))
  })
  shiny::div(
    class = "alert alert-info",
    msg_head, shiny::tags$br(),
    shiny::tags$strong("Candidates available to select:"),
    do.call(shiny::tags$ul, items)
  )
}


# Binary access flip helper ----
#
# Treats `change_pct` as the share of currently-without-access observations to
# flip to access (positive) or share of with-access to flip to no-access
# (negative).  `universal = TRUE` sets everyone to 1.

.apply_binary_access <- function(x, universal, change_pct) {
  if (isTRUE(universal)) {
    return(ifelse(is.na(x), NA_integer_, 1L))
  }
  if (is.null(change_pct) || is.na(change_pct) || change_pct == 0) {
    return(x)
  }

  x_out <- as.integer(x)
  if (change_pct > 0) {
    idx0 <- which(x_out == 0L & !is.na(x_out))
    n_flip <- round(length(idx0) * change_pct / 100)
    if (n_flip > 0 && length(idx0) > 0) {
      flip <- idx0[sample.int(length(idx0), min(n_flip, length(idx0)))]
      x_out[flip] <- 1L
    }
  } else {
    idx1 <- which(x_out == 1L & !is.na(x_out))
    n_flip <- round(length(idx1) * abs(change_pct) / 100)
    if (n_flip > 0 && length(idx1) > 0) {
      flip <- idx1[sample.int(length(idx1), min(n_flip, length(idx1)))]
      x_out[flip] <- 0L
    }
  }
  x_out
}

.apply_health_travel <- function(x, mode, pct, max_min) {
  if (identical(mode, "pct")) {
    if (is.null(pct) || is.na(pct) || pct == 0) {
      return(x)
    }
    x * (1 + pct / 100)
  } else if (identical(mode, "max")) {
    if (is.null(max_min) || is.na(max_min)) {
      return(x)
    }
    pmin(x, max_min)
  } else {
    x
  }
}

.determine_sp_eligibility <- function(svy, sp, apply_errors = TRUE) {
  n <- nrow(svy)
  targeting <- sp$targeting %||% "exante_poor"

  if (targeting == "universal") {
    eligible <- rep(TRUE, n)
  } else if (targeting == "exante_poor") {
    q <- stats::quantile(svy$welfare,
      (sp$targeting_threshold %||% 20) / 100,
      na.rm = TRUE
    )
    eligible <- !is.na(svy$welfare) & svy$welfare <= q
  } else if (targeting == "pmt") {
    pmt_var <- sp$pmt_variable
    pmt_cut <- as.numeric(sp$pmt_cutoff %||% NA)
    if (is.null(pmt_var) || is.na(pmt_var) || is.na(pmt_cut) ||
      !(pmt_var %in% names(svy))) {
      eligible <- rep(FALSE, n)
    } else {
      col <- svy[[pmt_var]]
      uniq <- sort(unique(col[!is.na(col)]))
      if (length(uniq) == 2 && all(uniq %in% c(0, 1))) {
        # Binary: match the selected value exactly
        eligible <- !is.na(col) & col == pmt_cut
      } else {
        # Continuous: include those at or below the cutoff
        eligible <- !is.na(col) & col <= pmt_cut
      }
    }
  } else {
    eligible <- rep(FALSE, n)
  }

  # Inclusion / exclusion errors (not applied for universal). Diagnostics can
  # request the ideal targeting rule by leaving these errors unapplied.
  if (isTRUE(apply_errors) && targeting != "universal") {
    incl_rate <- (sp$inclusion_error_pct %||% 0) / 100
    excl_rate <- (sp$exclusion_error_pct %||% 0) / 100
    non_elig <- which(!eligible)
    elig <- which(eligible)
    if (length(non_elig) > 0 && incl_rate > 0) {
      n_flip <- round(length(non_elig) * incl_rate)
      eligible[non_elig[sample.int(length(non_elig), min(n_flip, length(non_elig)))]] <-
        TRUE
    }
    if (length(elig) > 0 && excl_rate > 0) {
      n_flip <- round(length(elig) * excl_rate)
      eligible[elig[sample.int(length(elig), min(n_flip, length(elig)))]] <- FALSE
    }
  }

  eligible
}


#' Apply Policy Scenario Adjustments to Survey Covariates
#'
#' Modifies selected covariates in the survey-weather frame to reflect the
#' user-defined policy scenarios from the Step 3 sidebar.
#'
#' @param svy     A data frame of merged survey-weather data (from
#'                \code{mod_1_modelling_server()$survey_weather()}).
#' @param infra   Named list returned by \code{mod_3_02_infra_server()}.
#' @param sp      Named list returned by \code{mod_3_01_sp_server()}.
#' @param digital Named list returned by \code{mod_3_03_digital_server()}.
#' @param labor   Named list returned by \code{mod_3_04_labor_server()}.
#' @param education Named list returned by \code{mod_3_05_education_server()}.
#' @param model_vars Character vector of variable / term names in the current
#'   Step 1 model (covariates and interactions). When supplied, covariate
#'   levers only mutate columns that appear in the model - a variable dropped
#'   from Step 1 leaves the survey untouched. When \code{NULL} (the default)
#'   no gating is applied (legacy behaviour).
#' @param seed Integer seed for stochastic policy assignment.
#' @param analysis_unit Character. Analysis unit of the survey frame:
#'   `"hh"` scales social-protection transfers by household size when an
#'   `hhsize` column is present; `"pc"` treats rows as per-capita
#'   observations. Default `"hh"`.
#'
#' @return The modified survey data frame.
#' @export
apply_policy_to_svy <- function(svy,
                                infra = NULL,
                                sp = NULL,
                                digital = NULL,
                                labor = NULL,
                                education = NULL,
                                model_vars = NULL,
                                analysis_unit = "hh",
                                seed = WISEAPP_DEFAULT_SEED) {
  if (is.null(svy)) {
    return(svy)
  }
  withr::with_seed(wise_seed(seed, "policy"), {
    cols <- names(svy)

    # Columns eligible for covariate-lever manipulation. A lever whose variable
    # has been removed from the Step 1 model (its UI control is hidden) must not
    # mutate the survey, otherwise the diagnostics tab would surface phantom
    # "manipulated" variables that the model - and hence the Results and
    # Decomposition tabs - ignore. Matching is shared with the lever modules'
    # show_* logic (exact match via .lever_in_model()). SP transfers act on
    # `welfare` (the outcome, not a covariate) and are intentionally not gated.
    lever_cols <- if (is.null(model_vars)) {
      cols
    } else {
      cols[.lever_in_model(cols, model_vars)]
    }

    # Infrastructure: only apply if user has specified non-zero changes
    if (!is.null(infra)) {
      if (has_infra_change(infra)) {
        if ("electricity" %in% lever_cols) {
          svy$electricity <- .apply_binary_access(
            svy$electricity,
            infra$elec_universal,
            infra$elec_access_change_pct
          )
        }
        if ("imp_wat_rec" %in% lever_cols) {
          svy$imp_wat_rec <- .apply_binary_access(
            svy$imp_wat_rec,
            infra$water_universal,
            infra$water_access_change_pct
          )
        }
        if ("imp_san_rec" %in% lever_cols) {
          svy$imp_san_rec <- .apply_binary_access(
            svy$imp_san_rec,
            infra$sanitation_universal,
            infra$sanitation_access_change_pct
          )
        }
        if ("piped" %in% lever_cols) {
          svy$piped <- .apply_binary_access(
            svy$piped,
            infra$piped_universal,
            infra$piped_access_change_pct
          )
        }
        if ("piped_to_prem" %in% lever_cols) {
          svy$piped_to_prem <- .apply_binary_access(
            svy$piped_to_prem,
            infra$piped_to_prem_universal,
            infra$piped_to_prem_access_change_pct
          )
        }
        if ("imp_wat_san_rec" %in% lever_cols) {
          svy$imp_wat_san_rec <- .apply_binary_access(
            svy$imp_wat_san_rec,
            infra$imp_wat_san_universal,
            infra$imp_wat_san_access_change_pct
          )
        }
        if ("ttime_health" %in% lever_cols) {
          svy$ttime_health <- .apply_health_travel(
            svy$ttime_health,
            infra$health_mode,
            infra$health_travel_pct,
            infra$health_travel_max
          )
        }
      }
    }

    # Digital inclusion: only apply if user has specified non-zero changes
    if (!is.null(digital)) {
      if (has_digital_change(digital)) {
        if ("internet" %in% lever_cols) {
          svy$internet <- .apply_binary_access(
            svy$internet,
            digital$internet_universal,
            digital$internet_access_change_pct
          )
        }
        if ("cellphone" %in% lever_cols) {
          svy$cellphone <- .apply_binary_access(
            svy$cellphone,
            digital$mobile_universal,
            digital$mobile_access_change_pct
          )
        }
      }
    }

    # Education: only apply if user has specified non-zero changes
    if (!is.null(education)) {
      if (has_education_change(education)) {
        if ("educ_com1_hh" %in% lever_cols) {
          svy$educ_com1_hh <- .apply_binary_access(
            svy$educ_com1_hh,
            education$primary_universal,
            education$primary_access_change_pct
          )
        }
        if ("educ_com2_hh" %in% lever_cols) {
          svy$educ_com2_hh <- .apply_binary_access(
            svy$educ_com2_hh,
            education$secondary_universal,
            education$secondary_access_change_pct
          )
        }
        if ("educ_com3_hh" %in% lever_cols) {
          svy$educ_com3_hh <- .apply_binary_access(
            svy$educ_com3_hh,
            education$postsec_universal,
            education$postsec_access_change_pct
          )
        }
      }
    }

    # Labour market: only apply if user has specified non-zero changes
    if (!is.null(labor)) {
      if (has_labor_change(labor)) {
        # Employment rate change (percentage points): unemployed -> employed/selfemployed
        # Requires all three employment status columns to be present
        emp_change <- (labor$employment_change_pp %||% 0) / 100
        if (emp_change != 0 && all(c("employed", "selfemployed", "unemployed") %in%
          lever_cols)) {
          # Find unemployed individuals and current ratio of employed/selfemployed
          unemp_idx <- which(svy$unemployed == 1L & !is.na(svy$unemployed))
          employed_idx <- which(svy$employed == 1L & !is.na(svy$employed))
          selfemp_idx <- which(svy$selfemployed == 1L & !is.na(svy$selfemployed))

          # emp_change is the target shift in employment rate (as a fraction of
          # total N), so multiply by nrow(svy) to get the number of workers to
          # flip - giving a true percentage-point change in employment rate.
          n_total <- nrow(svy)

          if (emp_change > 0 && length(unemp_idx) > 0) {
            # Calculate ratio of employed vs selfemployed among currently employed
            n_employed <- length(employed_idx)
            n_selfemp <- length(selfemp_idx)
            total_employed <- n_employed + n_selfemp
            ratio_employed <- if (total_employed > 0) {
              n_employed / total_employed
            } else {
              0.5
            }

            n_flip <- min(round(n_total * emp_change), length(unemp_idx))
            if (n_flip > 0) {
              flip_idx <- unemp_idx[sample.int(length(unemp_idx), n_flip)]
              n_to_employed <- round(length(flip_idx) * ratio_employed)
              n_to_selfemp <- length(flip_idx) - n_to_employed

              if (n_to_employed > 0) {
                flip_employed <- flip_idx[seq_len(n_to_employed)]
                svy$unemployed[flip_employed] <- 0L
                svy$employed[flip_employed] <- 1L
              }
              if (n_to_selfemp > 0) {
                flip_selfemp <- flip_idx[seq(n_to_employed + 1, length(flip_idx))]
                svy$unemployed[flip_selfemp] <- 0L
                svy$selfemployed[flip_selfemp] <- 1L
              }
            }
          } else if (emp_change < 0 && length(c(employed_idx, selfemp_idx)) > 0) {
            # Decrease employment: flip some employed/selfemployed to unemployed
            employed_all <- c(employed_idx, selfemp_idx)
            n_flip <- min(round(n_total * abs(emp_change)), length(employed_all))
            if (n_flip > 0) {
              flip_idx <- employed_all[sample.int(length(employed_all), n_flip)]
              svy$employed[flip_idx] <- 0L
              svy$selfemployed[flip_idx] <- 0L
              svy$unemployed[flip_idx] <- 1L
            }
          }
        }

        # Sectoral composition: minimize reallocation to achieve target percentages
        # Only move workers from sectors exceeding their target. Runs only when
        # a sector target was actually changed (R2-BUG-05); an unset target
        # keeps that sector's current count.
        sector_set <- .sector_target_set(labor$sector_manufacturing) ||
          .sector_target_set(labor$sector_services)
        if (sector_set && all(c("employed", "selfemployed", "agriculture", "industry", "services") %in% lever_cols)) {
          working <- (svy$employed == 1L | svy$selfemployed == 1L) &
            !is.na(svy$employed) & !is.na(svy$selfemployed)

          if (any(working)) {
            working_idx <- which(working)
            n_working <- length(working_idx)

            # Count current sector distribution (within working population)
            n_curr_agri <- sum(svy$agriculture[working_idx] == 1L, na.rm = TRUE)
            n_curr_ind <- sum(svy$industry[working_idx] == 1L, na.rm = TRUE)
            n_curr_serv <- sum(svy$services[working_idx] == 1L, na.rm = TRUE)

            # Target counts: manufacturing and services chosen by user (an
            # unset target keeps the current count), agriculture is the
            # residual. Clamp so targets cannot exceed 100% combined (which
            # would make target_agri negative and corrupt the reallocation).
            n_target_ind <- if (.sector_target_set(labor$sector_manufacturing)) {
              round(n_working * min(max(labor$sector_manufacturing, 0) / 100, 1))
            } else {
              n_curr_ind
            }
            n_target_ind <- min(n_target_ind, n_working)
            n_target_serv <- if (.sector_target_set(labor$sector_services)) {
              round(n_working * max(labor$sector_services, 0) / 100)
            } else {
              n_curr_serv
            }
            n_target_serv <- min(n_target_serv, n_working - n_target_ind)
            n_target_agri <- n_working - n_target_ind - n_target_serv

            # Surplus = current exceeds target; deficit = current below target
            surplus_agri <- max(0L, n_curr_agri - n_target_agri)
            surplus_ind <- max(0L, n_curr_ind - n_target_ind)
            surplus_serv <- max(0L, n_curr_serv - n_target_serv)

            deficit_agri <- max(0L, n_target_agri - n_curr_agri)
            deficit_ind <- max(0L, n_target_ind - n_curr_ind)
            deficit_serv <- max(0L, n_target_serv - n_curr_serv)

            # Build list of workers to reallocate and their target sectors
            # Priority: reallocate from agriculture, then industry, then services
            reallocations <- data.frame(
              worker = integer(0),
              from_sector = character(0),
              to_sector = character(0),
              stringsAsFactors = FALSE
            )

            # Agriculture -> (deficit sectors)
            if (surplus_agri > 0) {
              agri_workers <- which(working & svy$agriculture == 1L)
              if (length(agri_workers) > 0) {
                n_to_move <- min(surplus_agri, length(agri_workers))
                candidates <- agri_workers[sample.int(length(agri_workers), n_to_move)]
                n_to_ind <- if (deficit_ind > 0) min(n_to_move, deficit_ind) else 0L
                n_to_serv <- if (deficit_serv > 0) max(0L, n_to_move - n_to_ind) else 0L
                targets <- c(
                  rep("industry", n_to_ind),
                  rep("services", n_to_serv)
                )
                if (length(targets) > 0 && length(targets) == length(candidates)) {
                  reallocations <- rbind(
                    reallocations,
                    data.frame(
                      worker = candidates[seq_along(targets)],
                      from_sector = "agriculture",
                      to_sector = targets,
                      stringsAsFactors = FALSE
                    )
                  )
                  deficit_ind <- max(0, deficit_ind - sum(targets == "industry"))
                  deficit_serv <- max(0, deficit_serv - sum(targets == "services"))
                }
              }
            }

            # Industry -> (deficit sectors)
            if (surplus_ind > 0 && (deficit_agri > 0 || deficit_serv > 0)) {
              ind_workers <- which(working & svy$industry == 1L)
              if (length(ind_workers) > 0) {
                n_to_move <- min(
                  surplus_ind, length(ind_workers),
                  deficit_agri + deficit_serv
                )
                candidates <- ind_workers[sample.int(length(ind_workers), n_to_move)]
                n_to_agri <- if (deficit_agri > 0) {
                  min(n_to_move, deficit_agri)
                } else {
                  0L
                }
                n_to_serv <- if (deficit_serv > 0) {
                  max(0L, n_to_move - n_to_agri)
                } else {
                  0L
                }
                targets <- c(
                  rep("agriculture", n_to_agri),
                  rep("services", n_to_serv)
                )
                if (length(targets) > 0 && length(targets) == length(candidates)) {
                  reallocations <- rbind(
                    reallocations,
                    data.frame(
                      worker = candidates[seq_along(targets)],
                      from_sector = "industry",
                      to_sector = targets,
                      stringsAsFactors = FALSE
                    )
                  )
                  deficit_agri <- max(0, deficit_agri - sum(targets == "agriculture"))
                  deficit_serv <- max(0, deficit_serv - sum(targets == "services"))
                }
              }
            }

            # Services -> (deficit sectors)
            if (surplus_serv > 0 && (deficit_agri > 0 || deficit_ind > 0)) {
              serv_workers <- which(working & svy$services == 1L)
              if (length(serv_workers) > 0) {
                n_to_move <- min(
                  surplus_serv, length(serv_workers),
                  deficit_agri + deficit_ind
                )
                candidates <- serv_workers[sample.int(length(serv_workers), n_to_move)]
                n_to_agri <- if (deficit_agri > 0) {
                  min(n_to_move, deficit_agri)
                } else {
                  0L
                }
                n_to_ind <- if (deficit_ind > 0) {
                  max(0L, n_to_move - n_to_agri)
                } else {
                  0L
                }
                targets <- c(
                  rep("agriculture", n_to_agri),
                  rep("industry", n_to_ind)
                )
                if (length(targets) > 0 && length(targets) == length(candidates)) {
                  reallocations <- rbind(
                    reallocations,
                    data.frame(
                      worker = candidates[seq_along(targets)],
                      from_sector = "services",
                      to_sector = targets,
                      stringsAsFactors = FALSE
                    )
                  )
                }
              }
            }

            # Execute reallocations: clear all sector flags for movers then assign
            # target sector. Vectorised over sector to avoid a row-by-row loop.
            if (nrow(reallocations) > 0) {
              movers <- reallocations$worker
              svy$agriculture[movers] <- 0L
              svy$industry[movers] <- 0L
              svy$services[movers] <- 0L
              for (sec in c("agriculture", "industry", "services")) {
                targets <- reallocations$worker[reallocations$to_sector == sec]
                if (length(targets) > 0 && sec %in% names(svy)) {
                  svy[[sec]][targets] <- 1L
                }
              }
            }
          }
        }
      }
    }

    # Social protection: add per-recipient transfer as SP_TRANSFER_COL column.
    # welfare is the regression outcome (not a covariate), so we tag the
    # transfer amount here; run_sim_pipeline() (fct_simulations.R) reads the
    # column post-prediction and adds it to y_point on the level scale
    # (re-logged when so$transform == "log") so the boost flows into
    # aggregate_with_uncertainty_delta() and matches the decomposition's delta_sp.
    #
    # When analysis_unit == "hh" each row is a household and the SP "recipient"
    # is the household, but welfare is per-capita - so the per-household
    # transfer must be divided by hhsize to match scale. When analysis_unit ==
    # "ind" the SP transfer is already per-individual and applies to every
    # eligible (individual) row as-is.
    if (!is.null(sp) && "welfare" %in% cols) {
      eligible <- .determine_sp_eligibility(svy, sp)
      transfer <- .sp_transfer_values(svy, sp, analysis_unit, eligible)
      if (!is.null(transfer)) {
        svy[[SP_TRANSFER_COL]] <- transfer
      }
    }

    svy
  })
}


# Policy-by-delta derivation ----

#' Apply a per-household policy delta to the baseline Step 2 pipelines
#'
#' Module 3 historically built its policy arm by calling
#' \code{resimulate_with_svy()} - a full re-prediction against the policy-
#' adjusted survey. That works, but it (a) duplicates compute for every
#' CMIP6 member, and (b) historically caused the Results pane to disagree with
#' the Decomposition pane when baseline and policy aggregation used independent
#' residual draws. Residual streams are now deterministic, but deriving the
#' policy arm from the baseline still guarantees paired household-level values
#' and avoids that duplicate prediction work.
#'
#' This helper prepares invariant central channels once and evaluates resilience
#' at the exact Step 2 weather exposure retained for each prediction row:
#'   \itemize{
#'     \item \code{y_point_policy = y_point_baseline + main + repositioning + interaction}
#'     \item All other pipeline slots (\code{F_loading}, \code{train_aug},
#'           \code{id_vec}, \code{weather_raw}, \code{sim_year},
#'           \code{weight}) are passed through unchanged. Because
#'           \code{aggregate_pipeline_per_year()} sees the same \code{id_vec}
#'           and \code{train_aug} for both arms, residual draws line up
#'           household-for-household, so a zero delta produces a zero visual
#'           effect by construction.
#'   }
#'
#' Limitations of this v1:
#'   \itemize{
#'     \item \code{F_loading} is not updated to reflect the policy-modified
#'           design matrix. SE bands therefore reflect baseline-X coefficient
#'           uncertainty, not policy-X. For the central value this is exact;
#'           for SE bands it's a small approximation that is exact when no
#'           covariate moves and acceptable for typical policy modifications.
#'           Recomputing \code{F_loading} per pipeline is a follow-up.
#'     \item Exact retained prediction-row exposure mapping is required. Invalid
#'           mappings fail the entire run; no period-mean fallback is used.
#'   }
#'
#' @param svy_baseline           Baseline survey-weather data frame.
#' @param svy_policy             Policy-adjusted survey-weather data frame.
#' @param model_fit              Step 1 model-fit list.
#' @param so                     Selected-outcome metadata.
#' @param hist_sim_baseline      Step 2 \code{hist_sim} list (verbatim).
#' @param saved_scenarios_baseline Step 2 named \code{saved_scenarios} list.
#' @param skip_coef              Retained for API compatibility. The active
#'   correction path computes central values only, so this value does not
#'   affect the result.
#' @param deltas                 Optional pre-computed covariate deltas from
#'   \code{.compute_policy_deltas()} - weather-independent, so computed once
#'   here and reused for every pipeline/scenario instead of per call
#'   (PERF-22). Computed internally when NULL.
#' @param F_hat                  Optional pre-built ecdf of the training
#'   outcome (RIF path; see \code{.compute_rif_channels()}). Computed
#'   internally when NULL.
#' @param decomp_context Optional immutable run-owned decomposition context.
#' @param run_identity Run identity required when a context is supplied.
#' @param annual_channels Optional prepared annual source from that context.
#' @param chunk_size Maximum prediction rows evaluated in each central block.
#'
#' @return Named list with \code{hist_sim} and \code{saved_scenarios} on the
#'   Step 2 schema, prepared \code{annual_channels}, compact future
#'   \code{decomp_scenarios}, and \code{correction_version}. Invalid inputs error.
#' @export
apply_policy_delta_to_baseline <- function(svy_baseline,
                                           svy_policy,
                                           model_fit,
                                           so,
                                           hist_sim_baseline,
                                           saved_scenarios_baseline = list(),
                                           skip_coef = FALSE,
                                           deltas = NULL,
                                           F_hat = NULL,
                                           decomp_context = NULL,
                                           run_identity = NULL,
                                           annual_channels = NULL,
                                           chunk_size = 100000L) {
  if (is.null(svy_baseline) || is.null(svy_policy) ||
    is.null(model_fit) || is.null(so) ||
    is.null(hist_sim_baseline)) {
    return(NULL)
  }
  if (!is.null(decomp_context) && is.null(run_identity)) {
    stop("Current run identity is required.", call. = FALSE)
  }

  if (length(chunk_size) != 1L || !is.numeric(chunk_size) ||
    !is.finite(chunk_size) || chunk_size < 1 || chunk_size != as.integer(chunk_size)) {
    stop("Invalid annual correction chunk size.", call. = FALSE)
  }
  run_identity <- run_identity %||% "standalone-annual-policy"
  if (!model_fit$engine %in% c("rif", "fixest")) {
    stop("Annual policy correction requires RIF or linear fixest.", call. = FALSE)
  }
  decomp_context <- decomp_context %||% .build_decomposition_context(
    svy_baseline, svy_policy, model_fit, so, deltas = deltas,
    skip_coef = TRUE, F_hat = F_hat, run_identity = run_identity
  )
  .validate_run_decomposition_context(decomp_context, run_identity)
  annual_channels <- annual_channels %||% .prepare_policy_annual_channels(decomp_context, run_identity)
  if (!is.environment(annual_channels) || !environmentIsLocked(annual_channels) ||
    !identical(annual_channels$status, "ok")) {
    stop("Annual policy correction unavailable: ", annual_channels$reason %||% "invalid source", call. = FALSE)
  }
  if (!identical(annual_channels$context, decomp_context)) {
    stop("Annual policy source/context mismatch.", call. = FALSE)
  }
  hist_result <- .apply_policy_annual_pipeline(
    hist_sim_baseline$pipeline, annual_channels, run_identity,
    scenario = hist_sim_baseline$hist_label %||% "Historical", member = "Historical",
    chunk_size = chunk_size, owner = hist_sim_baseline
  )
  hist_sim_new <- hist_sim_baseline
  hist_sim_new$pipeline <- hist_result$pipeline
  parts <- list(hist_result$compact)
  saved_scenarios_new <- lapply(seq_along(saved_scenarios_baseline), function(i) {
    s <- saved_scenarios_baseline[[i]]
    if (is.null(s) || is.null(s$pipelines)) {
      stop("Missing saved scenario prediction pipelines.", call. = FALSE)
    }
    # Members of one scenario share timestamps: rebuild exposure tables with
    # one cached timestamp decomposition per scenario.
    exposure_cache <- new.env(parent = emptyenv())
    pipes_new <- lapply(seq_along(s$pipelines), function(j) {
      result <- .apply_policy_annual_pipeline(
        s$pipelines[[j]], annual_channels, run_identity,
        scenario = names(saved_scenarios_baseline)[i] %||% paste0("Scenario ", i),
        member = names(s$pipelines)[j] %||% paste0("Member ", j),
        year_range = s$year_range %||% c(NA_integer_, NA_integer_),
        chunk_size = chunk_size, owner = s, exposure_cache = exposure_cache
      )
      parts[[length(parts) + 1L]] <<- result$compact
      result$pipeline
    })
    names(pipes_new) <- names(s$pipelines)
    s$pipelines <- pipes_new
    s$policy_correction_version <- annual_channels$correction_version
    s
  })
  names(saved_scenarios_new) <- names(saved_scenarios_baseline)
  hist_sim_new$policy_correction_version <- annual_channels$correction_version

  list(hist_sim = hist_sim_new, saved_scenarios = saved_scenarios_new,
    annual_channels = annual_channels,
    decomp_scenarios = .bind_compact_future_decompositions(parts, model_fit$engine,
      identical(model_fit$engine, "rif")),
    correction_version = annual_channels$correction_version)
}
