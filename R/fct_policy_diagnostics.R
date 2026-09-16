# ============================================================================ #
# Step 3 construction, treatment assignment, and covariate support diagnostics.
# ============================================================================ #

policy_treatment_matrix <- function(baseline_svy, policy_svy,
                                    eligibility = NULL, weight_col = "weight") {
  if (is.null(baseline_svy) || is.null(policy_svy) ||
      nrow(baseline_svy) != nrow(policy_svy)) return(data.frame())
  b <- if (!is.null(eligibility)) as.logical(eligibility) else {
    if ("sp_eligible" %in% names(baseline_svy)) as.logical(baseline_svy$sp_eligible) else rep(FALSE, nrow(baseline_svy))
  }
  p <- if (SP_TRANSFER_COL %in% names(policy_svy)) policy_svy[[SP_TRANSFER_COL]] > 0 else {
    if ("sp_eligible" %in% names(policy_svy)) as.logical(policy_svy$sp_eligible) else rep(FALSE, nrow(policy_svy))
  }
  b[is.na(b)] <- FALSE; p[is.na(p)] <- FALSE
  w <- if (weight_col %in% names(baseline_svy)) as.numeric(baseline_svy[[weight_col]]) else rep(1, nrow(baseline_svy))
  w[!is.finite(w) | w < 0] <- 0
  status <- interaction(b, p, drop = TRUE, sep = "_")
  labels <- c(
    `FALSE_FALSE` = "Not eligible, not treated",
    `FALSE_TRUE` = "Inclusion error: not eligible, treated",
    `TRUE_FALSE` = "Exclusion error: eligible, not treated",
    `TRUE_TRUE` = "Eligible and treated"
  )
  dplyr::bind_rows(lapply(names(labels), function(k) {
    ok <- as.character(status) == k
    data.frame(
      status = labels[[k]],
      eligible_baseline = startsWith(k, "TRUE"),
      treated_policy = endsWith(k, "TRUE"),
      n = sum(ok), weighted_n = sum(w[ok]),
      weighted_share = if (sum(w) > 0) sum(w[ok]) / sum(w) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

# Unit-level masks shared by the diagnostics coverage table and the Step 3
# Results headline card, so "population represented" cannot drift between the
# two surfaces.

.policy_sp_mask <- function(policy_svy) {
  if (SP_TRANSFER_COL %in% names(policy_svy))
    is.finite(as.numeric(policy_svy[[SP_TRANSFER_COL]])) & as.numeric(policy_svy[[SP_TRANSFER_COL]]) > 0
  else rep(FALSE, nrow(policy_svy))
}

.policy_other_mask <- function(baseline_svy, policy_svy, weight_col = "weight") {
  changed <- detect_manipulated_vars(baseline_svy, policy_svy)
  non_sp_vars <- setdiff(changed, c("welfare", SP_TRANSFER_COL, weight_col, "sim_year", "year"))
  if (!length(non_sp_vars)) return(rep(FALSE, nrow(policy_svy)))
  changed_mask <- function(v) {
    b <- baseline_svy[[v]]; p <- policy_svy[[v]]
    (!is.na(b) & !is.na(p) & b != p) | (is.na(b) != is.na(p))
  }
  Reduce("|", lapply(non_sp_vars, changed_mask))
}

#' Units touched by any implemented policy
#'
#' Logical mask over \code{policy_svy} rows: TRUE where the unit receives a
#' positive social-protection transfer or has any covariate changed by another
#' policy lever. The same union the Diagnostics tab's coverage table reports.
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#' @param weight_col   Weight column excluded from the covariate comparison.
#' @return Logical vector of length \code{nrow(policy_svy)}.
#' @keywords internal
policy_reach_mask <- function(baseline_svy, policy_svy, weight_col = "weight") {
  .policy_sp_mask(policy_svy) | .policy_other_mask(baseline_svy, policy_svy, weight_col)
}

policy_component_matrix <- function(baseline_svy, policy_svy,
                                    weight_col = "weight", analysis_unit = "hh",
                                    candidates = NULL) {
  if (is.null(baseline_svy) || is.null(policy_svy) ||
      nrow(baseline_svy) != nrow(policy_svy)) return(data.frame())
  w <- if (weight_col %in% names(baseline_svy)) as.numeric(baseline_svy[[weight_col]]) else rep(1, nrow(baseline_svy))
  w[!is.finite(w) | w < 0] <- 0
  total_w <- sum(w)
  changed <- detect_manipulated_vars(
    baseline_svy, policy_svy, candidates = candidates
  )
  non_sp_vars <- setdiff(changed, c("welfare", SP_TRANSFER_COL, weight_col, "sim_year", "year"))
  changed_mask <- function(v) {
    b <- baseline_svy[[v]]; p <- policy_svy[[v]]
    (!is.na(b) & !is.na(p) & b != p) | (is.na(b) != is.na(p))
  }
  sp_mask <- .policy_sp_mask(policy_svy)
  other_mask <- if (length(non_sp_vars)) Reduce("|", lapply(non_sp_vars, changed_mask)) else rep(FALSE, nrow(policy_svy))
  rows <- list()
  add_row <- function(component, mask, cost = NA_real_) {
    rows[[length(rows) + 1L]] <<- data.frame(
      component = component, n_affected = sum(mask, na.rm = TRUE),
      weighted_affected = sum(w[mask], na.rm = TRUE),
      population_share = if (total_w > 0) sum(w[mask], na.rm = TRUE) / total_w else NA_real_,
      realized_cost = cost, stringsAsFactors = FALSE
    )
  }
  add_row("Social protection", sp_mask,
          if (SP_TRANSFER_COL %in% names(policy_svy)) .sp_transfer_totals(policy_svy, analysis_unit)$total else NA_real_)
  for (v in non_sp_vars) add_row(.policy_display_name(v), changed_mask(v))
  if (length(non_sp_vars) > 1L) add_row("Other policy levers (combined)", other_mask)
  if (any(sp_mask & other_mask)) add_row("Touched by both policy types", sp_mask & other_mask)
  dplyr::bind_rows(rows)
}

policy_covariate_support <- function(training, policy, vars = NULL,
                                      rare_share = 0.01) {
  if (is.null(training) || is.null(policy) ||
      !is.data.frame(training) || !is.data.frame(policy)) {
    return(data.frame())
  }
  vars <- vars %||% intersect(names(training), names(policy))
  # A caller may provide the Step 1 variable list, which can contain a
  # variable dropped from either the training or policy frame. Only shared
  # columns can be compared safely.
  vars <- intersect(vars, intersect(names(training), names(policy)))
  vars <- setdiff(vars, c("welfare", "weight", "sim_year", "year"))
  dplyr::bind_rows(lapply(vars, function(v) {
    tr <- training[[v]]; po <- policy[[v]]
    if (is.numeric(tr) && is.numeric(po)) {
      finite_tr <- is.finite(tr)
      lo <- if (any(finite_tr)) min(tr[finite_tr]) else NA_real_
      hi <- if (any(finite_tr)) max(tr[finite_tr]) else NA_real_
      out <- if (is.finite(lo) && is.finite(hi)) po < lo | po > hi else rep(FALSE, length(po))
      data.frame(variable = v, type = "numeric", training_lo = lo,
                 training_hi = hi, policy_outside_n = sum(out, na.rm = TRUE),
                 policy_outside_share = if (length(out)) mean(out, na.rm = TRUE) else NA_real_,
                 rare_or_absent = FALSE, warning = any(out, na.rm = TRUE),
                 stringsAsFactors = FALSE)
    } else {
      trc <- as.character(tr); poc <- as.character(po)
      freq <- prop.table(table(trc, useNA = "no"))
      absent <- !(poc %in% names(freq))
      rare <- !absent & vapply(poc, function(x) {
        # Use an integer match before indexing. This avoids `[[` errors for
        # missing or unusual category values in policy-adjusted frames.
        idx <- match(x, names(freq), nomatch = 0L)
        if (idx == 0L) return(FALSE)
        as.numeric(freq[[idx]]) < rare_share
      }, logical(1L))
      data.frame(variable = v, type = "categorical", training_lo = NA,
                 training_hi = NA, policy_outside_n = 0,
                 policy_outside_share = 0,
                 rare_or_absent = any(absent | rare, na.rm = TRUE),
                 warning = any(absent | rare, na.rm = TRUE),
                 stringsAsFactors = FALSE)
    }
  }))
}

policy_construction_summary <- function(baseline_svy, policy_svy, sp = NULL,
                                        analysis_unit = "hh", seed = WISEAPP_DEFAULT_SEED) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(data.frame())
  changed <- detect_manipulated_vars(baseline_svy, policy_svy)
  transfer <- if (SP_TRANSFER_COL %in% names(policy_svy)) policy_svy[[SP_TRANSFER_COL]] else rep(0, nrow(policy_svy))

  changed_mask <- if (length(changed) > 0L) {
    Reduce("|", lapply(changed, function(v) {
      b_col <- baseline_svy[[v]]
      p_col <- policy_svy[[v]]
      (!is.na(b_col) & !is.na(p_col) & b_col != p_col) | (is.na(b_col) != is.na(p_col))
    }))
  } else {
    rep(FALSE, nrow(baseline_svy))
  }

  w <- if ("weight" %in% names(baseline_svy)) baseline_svy$weight else rep(1, nrow(baseline_svy))
  total_w <- sum(w, na.rm = TRUE)
  w_share <- if (total_w > 0) sum(changed_mask * w, na.rm = TRUE) / total_w else 0

  data.frame(
    item = c("Active manipulated variables", "Changed row count", "Weighted changed share",
             "Realized transfer total"),
    value = c(
      paste(changed, collapse = ", "),
      sum(changed_mask, na.rm = TRUE),
      w_share,
      .sp_transfer_totals(policy_svy, analysis_unit)$total
    ), stringsAsFactors = FALSE
  )
}
