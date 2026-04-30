# ============================================================================ #
# Pure functions for policy scenario variable discovery and diagnostics.
# Stateless and testable without Shiny.
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Policy scenario: candidate variable discovery & placeholder UI               #
# ---------------------------------------------------------------------------- #

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
  if (is.null(variable_list) || nrow(variable_list) == 0) return(NULL)
  nm <- variable_list$name
  if (is.null(nm)) return(NULL)
  mask <- Reduce(`|`, lapply(patterns, function(p) grepl(p, nm, ignore.case = TRUE)))
  if (!any(mask)) return(NULL)
  df <- variable_list[mask, , drop = FALSE]
  level <- vapply(seq_len(nrow(df)), function(i) {
    for (lvl in c("ind", "hh", "firm", "area")) {
      v <- df[[lvl]][i]
      if (!is.null(v) && !is.na(v) && v == 1L) {
        return(switch(lvl, ind = "Individual", hh = "Household",
                      firm = "Firm", area = "Area"))
      }
    }
    ""
  }, character(1))
  data.frame(
    name  = df$name,
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
    return(shiny::div(class = "alert alert-info",
      msg_head, shiny::tags$br(),
      shiny::tags$em("No candidate variables were found in the variable list for this category.")))
  }
  items <- lapply(seq_len(nrow(candidate_df)), function(i) {
    lvl <- candidate_df$level[i]
    shiny::tags$li(paste0(
      candidate_df$label[i],
      if (nzchar(lvl)) paste0(" (", lvl, ")") else ""
    ))
  })
  shiny::div(class = "alert alert-info",
    msg_head, shiny::tags$br(),
    shiny::tags$strong("Candidates available to select:"),
    do.call(shiny::tags$ul, items))
}


# ---------------------------------------------------------------------------- #
# Binary access flip helper                                                    #
# ---------------------------------------------------------------------------- #
#
# Treats `change_pct` as the share of currently-without-access observations to
# flip to access (positive) or share of with-access to flip to no-access
# (negative).  `universal = TRUE` sets everyone to 1.

.apply_binary_access <- function(x, universal, change_pct) {
  if (isTRUE(universal)) {
    return(ifelse(is.na(x), NA_integer_, 1L))
  }
  if (is.null(change_pct) || is.na(change_pct) || change_pct == 0) return(x)

  x_out <- as.integer(x)
  if (change_pct > 0) {
    idx0 <- which(x_out == 0L & !is.na(x_out))
    n_flip <- round(length(idx0) * change_pct / 100)
    if (n_flip > 0 && length(idx0) > 0) {
      flip <- sample(idx0, min(n_flip, length(idx0)))
      x_out[flip] <- 1L
    }
  } else {
    idx1 <- which(x_out == 1L & !is.na(x_out))
    n_flip <- round(length(idx1) * abs(change_pct) / 100)
    if (n_flip > 0 && length(idx1) > 0) {
      flip <- sample(idx1, min(n_flip, length(idx1)))
      x_out[flip] <- 0L
    }
  }
  x_out
}

.apply_health_travel <- function(x, mode, pct, max_min) {
  if (identical(mode, "pct")) {
    if (is.null(pct) || is.na(pct) || pct == 0) return(x)
    x * (1 + pct / 100)
  } else if (identical(mode, "max")) {
    if (is.null(max_min) || is.na(max_min)) return(x)
    pmin(x, max_min)
  } else {
    x
  }
}

.determine_sp_eligibility <- function(svy, sp) {
  n         <- nrow(svy)
  targeting <- sp$targeting %||% "exante_poor"

  if (targeting == "universal") {
    eligible <- rep(TRUE, n)
  } else if (targeting == "exante_poor") {
    q        <- quantile(svy$welfare,
                        (sp$targeting_threshold %||% 20) / 100,
                        na.rm = TRUE)
    eligible <- !is.na(svy$welfare) & svy$welfare <= q
  } else if (targeting == "pmt") {
    pmt_var <- sp$pmt_variable
    pmt_cut <- as.numeric(sp$pmt_cutoff %||% NA)
    if (is.null(pmt_var) || is.na(pmt_var) || is.na(pmt_cut) ||
        !(pmt_var %in% names(svy))) {
      eligible <- rep(FALSE, n)
    } else {
      col      <- svy[[pmt_var]]
      uniq     <- sort(unique(col[!is.na(col)]))
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

  # Inclusion / exclusion errors (not applied for universal)
  if (targeting != "universal") {
    incl_rate <- (sp$inclusion_error_pct %||% 0) / 100
    excl_rate <- (sp$exclusion_error_pct %||% 0) / 100
    non_elig  <- which(!eligible)
    elig      <- which(eligible)
    if (length(non_elig) > 0 && incl_rate > 0) {
      n_flip <- round(length(non_elig) * incl_rate)
      eligible[sample(non_elig, min(n_flip, length(non_elig)))] <-
        TRUE
    }
    if (length(elig) > 0 && excl_rate > 0) {
      n_flip <- round(length(elig) * excl_rate)
      eligible[sample(elig, min(n_flip, length(elig)))] <- FALSE
    }
  }

  eligible
}


#' Apply Policy Scenario Adjustments to Survey Covariates
#'
#' Modifies selected covariates in the survey-weather frame to reflect the
#' user-defined policy scenarios from the Step 3 sidebar. Only the
#' infrastructure scenario is currently wired; social-protection, digital, and
#' labor scenarios are accepted for future extension.
#'
#' @param svy     A data frame of merged survey-weather data (from
#'                \code{mod_1_modelling_server()$survey_weather()}).
#' @param infra   Named list returned by \code{mod_3_02_infra_server()}.
#' @param sp      Named list returned by \code{mod_3_01_sp_server()}.
#' @param digital Named list returned by \code{mod_3_03_digital_server()}.
#' @param labor   Named list returned by \code{mod_3_04_labor_server()}.
#'
#' @return The modified survey data frame.
#' @export
apply_policy_to_svy <- function(svy,
                                infra   = NULL,
                                sp      = NULL,
                                digital = NULL,
                                labor   = NULL) {
  if (is.null(svy)) return(svy)
  cols <- names(svy)

  # Infrastructure: only apply if user has specified non-zero changes
  if (!is.null(infra)) {
    # Check if any infrastructure policy is actually active (non-zero or universal)
    has_infra_change <- (isTRUE(infra$elec_universal) ||
                         (!is.null(infra$elec_access_change_pct) && infra$elec_access_change_pct != 0)) ||
                        (isTRUE(infra$water_universal) ||
                         (!is.null(infra$water_access_change_pct) && infra$water_access_change_pct != 0)) ||
                        (isTRUE(infra$sanitation_universal) ||
                         (!is.null(infra$sanitation_access_change_pct) && infra$sanitation_access_change_pct != 0)) ||
                        (!is.null(infra$health_travel_pct) && infra$health_travel_pct != 0)

    if (has_infra_change) {
      if ("electricity" %in% cols) {
        svy$electricity <- .apply_binary_access(
          svy$electricity,
          infra$elec_universal,
          infra$elec_access_change_pct
        )
      }
      if ("imp_wat_rec" %in% cols) {
        svy$imp_wat_rec <- .apply_binary_access(
          svy$imp_wat_rec,
          infra$water_universal,
          infra$water_access_change_pct
        )
      }
      if ("imp_san_rec" %in% cols) {
        svy$imp_san_rec <- .apply_binary_access(
          svy$imp_san_rec,
          infra$sanitation_universal,
          infra$sanitation_access_change_pct
        )
      }
      if ("ttime_health" %in% cols) {
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
    has_digital_change <- (isTRUE(digital$internet_universal) ||
                           (!is.null(digital$internet_access_change_pct) && digital$internet_access_change_pct != 0)) ||
                          (isTRUE(digital$mobile_universal) ||
                           (!is.null(digital$mobile_access_change_pct) && digital$mobile_access_change_pct != 0))

    if (has_digital_change) {
      if ("internet" %in% cols) {
        svy$internet <- .apply_binary_access(
          svy$internet,
          digital$internet_universal,
          digital$internet_access_change_pct
        )
      }
      if ("cellphone" %in% cols) {
        svy$cellphone <- .apply_binary_access(
          svy$cellphone,
          digital$mobile_universal,
          digital$mobile_access_change_pct
        )
      }
    }
  }

  # Labour market: only apply if user has specified non-zero changes
  if (!is.null(labor)) {
    has_labor_change <- (!is.null(labor$employment_change_pp) && labor$employment_change_pp != 0) ||
                        (!is.null(labor$sector_manufacturing) && labor$sector_manufacturing != 0) ||
                        (!is.null(labor$sector_services) && labor$sector_services != 0)

    if (has_labor_change) {
      # Employment rate change (percentage points): unemployed → employed/selfemployed
      # Requires all three employment status columns to be present
      emp_change <- (labor$employment_change_pp %||% 0) / 100
      if (emp_change != 0 && all(c("employed", "selfemployed", "unemployed") %in%
          cols)) {
      # Find unemployed individuals and current ratio of employed/selfemployed
      unemp_idx <- which(svy$unemployed == 1L & !is.na(svy$unemployed))
      employed_idx <- which(svy$employed == 1L & !is.na(svy$employed))
      selfemp_idx <- which(svy$selfemployed == 1L & !is.na(svy$selfemployed))

      if (emp_change > 0 && length(unemp_idx) > 0) {
        # Calculate ratio of employed vs selfemployed among currently employed
        n_employed <- length(employed_idx)
        n_selfemp <- length(selfemp_idx)
        total_employed <- n_employed + n_selfemp
        ratio_employed <- if (total_employed > 0) n_employed / total_employed
                          else 0.5

        # Number of unemployed to flip to employed/selfemployed
        n_flip <- round(length(unemp_idx) * emp_change)
        if (n_flip > 0) {
          flip_idx <- sample(unemp_idx, min(n_flip, length(unemp_idx)))
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
        n_flip <- round(length(employed_all) * abs(emp_change))
        if (n_flip > 0) {
          flip_idx <- sample(employed_all, min(n_flip, length(employed_all)))
          svy$employed[flip_idx] <- 0L
          svy$selfemployed[flip_idx] <- 0L
          svy$unemployed[flip_idx] <- 1L
        }
      }
    }

    # Sectoral composition: minimize reallocation to achieve target percentages
    # Only move workers from sectors exceeding their target
    if (all(c("employed", "selfemployed", "agriculture", "industry", "services") %in% cols)) {
      working <- (svy$employed == 1L | svy$selfemployed == 1L) &
                 !is.na(svy$employed) & !is.na(svy$selfemployed)

      if (any(working)) {
        working_idx <- which(working)
        n_working <- length(working_idx)

        # Target percentages: manufacturing and services chosen by user,
        # agriculture is the residual
        target_ind <- (labor$sector_manufacturing %||% 0) / 100
        target_serv <- (labor$sector_services %||% 0) / 100
        target_agri <- 1.0 - target_ind - target_serv

        # Convert targets to row counts
        n_target_ind <- round(n_working * target_ind)
        n_target_serv <- round(n_working * target_serv)
        n_target_agri <- n_working - n_target_ind - n_target_serv

        # Count current sector distribution (within working population)
        n_curr_agri <- sum(svy$agriculture[working_idx] == 1L, na.rm = TRUE)
        n_curr_ind <- sum(svy$industry[working_idx] == 1L, na.rm = TRUE)
        n_curr_serv <- sum(svy$services[working_idx] == 1L, na.rm = TRUE)

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

        # Agriculture → (deficit sectors)
        if (surplus_agri > 0) {
          agri_workers <- which(working & svy$agriculture == 1L)
          if (length(agri_workers) > 0) {
            n_to_move <- min(surplus_agri, length(agri_workers))
            candidates <- sample(agri_workers, n_to_move)
            n_to_ind <- if (deficit_ind > 0) min(n_to_move, deficit_ind) else 0L
            n_to_serv <- if (deficit_serv > 0) max(0L, n_to_move - n_to_ind) else 0L
            targets <- c(
              rep("industry", n_to_ind),
              rep("services", n_to_serv)
            )
            if (length(targets) > 0 && length(targets) == length(candidates)) {
              reallocations <- rbind(reallocations,
                data.frame(worker = candidates[seq_along(targets)],
                          from_sector = "agriculture",
                          to_sector = targets,
                          stringsAsFactors = FALSE))
              deficit_ind <- max(0, deficit_ind - sum(targets == "industry"))
              deficit_serv <- max(0, deficit_serv - sum(targets == "services"))
            }
          }
        }

        # Industry → (deficit sectors)
        if (surplus_ind > 0 && (deficit_agri > 0 || deficit_serv > 0)) {
          ind_workers <- which(working & svy$industry == 1L)
          if (length(ind_workers) > 0) {
            n_to_move <- min(surplus_ind, length(ind_workers),
                            deficit_agri + deficit_serv)
            candidates <- sample(ind_workers, n_to_move)
            n_to_agri <- if (deficit_agri > 0) min(n_to_move, deficit_agri)
                         else 0L
            n_to_serv <- if (deficit_serv > 0) max(0L, n_to_move - n_to_agri)
                         else 0L
            targets <- c(
              rep("agriculture", n_to_agri),
              rep("services", n_to_serv)
            )
            if (length(targets) > 0 && length(targets) == length(candidates)) {
              reallocations <- rbind(reallocations,
                data.frame(worker = candidates[seq_along(targets)],
                          from_sector = "industry",
                          to_sector = targets,
                          stringsAsFactors = FALSE))
              deficit_agri <- max(0, deficit_agri - sum(targets == "agriculture"))
              deficit_serv <- max(0, deficit_serv - sum(targets == "services"))
            }
          }
        }

        # Services → (deficit sectors)
        if (surplus_serv > 0 && (deficit_agri > 0 || deficit_ind > 0)) {
          serv_workers <- which(working & svy$services == 1L)
          if (length(serv_workers) > 0) {
            n_to_move <- min(surplus_serv, length(serv_workers),
                            deficit_agri + deficit_ind)
            candidates <- sample(serv_workers, n_to_move)
            n_to_agri <- if (deficit_agri > 0) min(n_to_move, deficit_agri)
                         else 0L
            n_to_ind <- if (deficit_ind > 0) max(0L, n_to_move - n_to_agri)
                        else 0L
            targets <- c(
              rep("agriculture", n_to_agri),
              rep("industry", n_to_ind)
            )
            if (length(targets) > 0 && length(targets) == length(candidates)) {
              reallocations <- rbind(reallocations,
                data.frame(worker = candidates[seq_along(targets)],
                          from_sector = "services",
                          to_sector = targets,
                          stringsAsFactors = FALSE))
            }
          }
        }

        # Execute reallocations: each worker moves from one sector to exactly one sector
        for (i in seq_len(nrow(reallocations))) {
          worker <- reallocations$worker[i]
          to_sec <- reallocations$to_sector[i]

          # Remove from all sectors
          svy$agriculture[worker] <- 0L
          svy$industry[worker] <- 0L
          svy$services[worker] <- 0L

          # Assign to target sector
          if (to_sec %in% names(svy)) {
            svy[[to_sec]][worker] <- 1L
          }
        }
      }
    }
    }
  }

  # Social protection: add per-household transfer as ._sp_transfer column.
  # welfare is the regression outcome (not a covariate), so we tag the
  # transfer amount here; it is applied to predictions post-prediction
  # in run_sim_pipeline() (fct_simulations.R).
  if (!is.null(sp) && "welfare" %in% cols) {
    if (sp$budget_mode == "transfer_first" && isTRUE(sp$transfer_amount_usd != 0)) {
      # If transfer-first budget mode is selected, apply the transfer to the
      # survey immediately so it is included in the predictions and thus the
      # targeting can be based on post-transfer welfare. The transfer amount is
      # annualized and converted to a daily amount for this purpose, since the
      # model is based on daily welfare.
      n_pay     <- sp$transfer_n_payments %||% 6
      if (!is.finite(n_pay)) n_pay <- 6
      annual_hh <- sp$transfer_amount_usd * n_pay
      daily_hh  <- annual_hh / 365
      eligible  <- .determine_sp_eligibility(svy, sp)
      if (length(eligible) == nrow(svy)) {
        svy[["._sp_transfer"]] <- ifelse(eligible, daily_hh, 0)
      }
    }
    else if (sp$budget_mode == "budget_first" && isTRUE(sp$budget_fixed > 0)) {
      # If budget-first mode is selected, we cannot apply a fixed transfer amount
      # upfront since the actual transfer per household will depend on how many
      # are eligible under the targeting and thus how many payments can be made
      # within the fixed budget. Instead, we calculate the transfer amount that
      # would be applied if the entire fixed budget were distributed evenly among
      # all eligible households, and tag that as ._sp_transfer for diagnostics
      # purposes. The actual transfer applied post-prediction will be adjusted
      # pro-rata based on the number of eligible households in each simulation.
      eligible <- .determine_sp_eligibility(svy, sp)
      n_eligible <- sum(eligible)
      if (n_eligible > 0) {
        annual_hh <- sp$budget_fixed / n_eligible
        daily_hh  <- annual_hh / 365
        svy[["._sp_transfer"]] <- ifelse(eligible, daily_hh, 0)
      }
    }
  }

  svy
}


#' Detect columns that differ between the baseline and policy-adjusted frames
#'
#' Returns the names of columns whose values differ between
#' \code{baseline_svy} and \code{policy_svy}. Used by the Step 3 diagnostics
#' table to surface any variable a user manipulation has touched —
#' covariates, interaction variables, or outcomes alike.
#'
#' Comparison rules:
#' \itemize{
#'   \item Numeric columns are compared with tolerance via
#'     \code{isTRUE(all.equal(..., check.attributes = FALSE))}.
#'   \item Other columns are compared with \code{identical()}.
#' }
#'
#' Rows must match across the two frames; if \code{nrow()} differs the
#' function returns the union of column names instead (since values can no
#' longer be compared element-wise).
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#'
#' @return Character vector of column names that changed.
#' @export
detect_manipulated_vars <- function(baseline_svy, policy_svy) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(character(0))
  shared <- intersect(names(baseline_svy), names(policy_svy))
  if (length(shared) == 0) return(character(0))
  if (nrow(baseline_svy) != nrow(policy_svy)) {
    return(setdiff(union(names(baseline_svy), names(policy_svy)), character(0)))
  }
  changed <- vapply(shared, function(v) {
    xb <- baseline_svy[[v]]
    xp <- policy_svy[[v]]
    if (is.numeric(xb) && is.numeric(xp)) {
      !isTRUE(all.equal(xb, xp, check.attributes = FALSE))
    } else {
      !identical(xb, xp)
    }
  }, logical(1))
  shared[changed]
}


#' Build a Diagnostics Summary for Policy-Adjusted Inputs
#'
#' Computes mean / sd / n_nonNA for each covariate in both the baseline and
#' policy-adjusted survey frames, so the Step 3 Results tab can display
#' what changed.
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#' @param vars         Character vector of variable names to summarise. If
#'   \code{NULL}, uses the intersection of the two frames' numeric cols.
#'
#' @return A tibble with columns \code{variable}, \code{mean_baseline},
#'   \code{mean_policy}, \code{delta_mean}, \code{sd_baseline},
#'   \code{sd_policy}, \code{n_nonNA}.
#' @export
policy_input_diagnostics <- function(baseline_svy, policy_svy, vars = NULL) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(NULL)

  if (is.null(vars)) {
    num_b <- names(baseline_svy)[vapply(baseline_svy, is.numeric, logical(1))]
    num_p <- names(policy_svy)[vapply(policy_svy, is.numeric, logical(1))]
    vars  <- intersect(num_b, num_p)
    # Drop obvious non-covariate keys
    vars  <- setdiff(vars, c("loc_id", "int_year", "int_month", "sim_year"))
  }

  vars <- vars[vars %in% names(baseline_svy) & vars %in% names(policy_svy)]

  if (length(vars) == 0) return(NULL)

  rows <- lapply(vars, function(v) {
    xb <- suppressWarnings(as.numeric(baseline_svy[[v]]))
    xp <- suppressWarnings(as.numeric(policy_svy[[v]]))
    data.frame(
      variable       = v,
      mean_baseline  = mean(xb, na.rm = TRUE),
      mean_policy    = mean(xp, na.rm = TRUE),
      delta_mean     = mean(xp, na.rm = TRUE) - mean(xb, na.rm = TRUE),
      sd_baseline    = stats::sd(xb, na.rm = TRUE),
      sd_policy      = stats::sd(xp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
