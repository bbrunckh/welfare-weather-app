# ============================================================================ #
# Pure functions for policy scenario variable discovery and diagnostics.
# Stateless and testable without Shiny.
# ============================================================================ #


#' Internal column name holding the per-household SP cash-transfer amount
#'
#' Set by \code{apply_policy_to_svy()}, read by \code{run_sim_pipeline()} (which
#' adds it to predicted welfare on the level scale), the diagnostics module,
#' and the decomposition. The leading-dot prefix marks it as an internal column
#' and the suffix makes it unlikely to collide with a user-supplied variable.
#' Treat this constant as the single source of truth across the codebase.
#' @export
SP_TRANSFER_COL <- ".wiseapp_sp_transfer"


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
                        (!is.null(infra$health_travel_pct) && infra$health_travel_pct != 0) ||
                        (isTRUE(infra$piped_universal) ||
                         (!is.null(infra$piped_access_change_pct) && infra$piped_access_change_pct != 0)) ||
                        (isTRUE(infra$piped_to_prem_universal) ||
                         (!is.null(infra$piped_to_prem_access_change_pct) && infra$piped_to_prem_access_change_pct != 0)) ||
                        (isTRUE(infra$imp_wat_san_universal) ||
                         (!is.null(infra$imp_wat_san_access_change_pct) && infra$imp_wat_san_access_change_pct != 0))


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
      if ("piped" %in% cols) {
        svy$piped <- .apply_binary_access(
          svy$piped,
          infra$piped_universal,
          infra$piped_access_change_pct
        )
      }
      if ("piped_to_prem" %in% cols) {
        svy$piped_to_prem <- .apply_binary_access(
          svy$piped_to_prem,
          infra$piped_to_prem_universal,
          infra$piped_to_prem_access_change_pct
        )
      }
      if ("imp_wat_san_rec" %in% cols) {
        svy$imp_wat_san_rec <- .apply_binary_access(
          svy$imp_wat_san_rec,
          infra$imp_wat_san_universal,
          infra$imp_wat_san_access_change_pct
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

      # emp_change is the target shift in employment rate (as a fraction of
      # total N), so multiply by nrow(svy) to get the number of workers to
      # flip — giving a true percentage-point change in employment rate.
      n_total <- nrow(svy)

      if (emp_change > 0 && length(unemp_idx) > 0) {
        # Calculate ratio of employed vs selfemployed among currently employed
        n_employed <- length(employed_idx)
        n_selfemp <- length(selfemp_idx)
        total_employed <- n_employed + n_selfemp
        ratio_employed <- if (total_employed > 0) n_employed / total_employed
                          else 0.5

        n_flip <- min(round(n_total * emp_change), length(unemp_idx))
        if (n_flip > 0) {
          flip_idx <- sample(unemp_idx, n_flip)
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
          flip_idx <- sample(employed_all, n_flip)
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
        # agriculture is the residual. Clamp so targets cannot exceed 100%
        # combined (which would make target_agri negative and corrupt the
        # reallocation logic).
        target_ind  <- min((labor$sector_manufacturing %||% 0) / 100, 1)
        target_serv <- min((labor$sector_services %||% 0) / 100, 1 - target_ind)
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

        # Execute reallocations: clear all sector flags for movers then assign
        # target sector. Vectorised over sector to avoid a row-by-row loop.
        if (nrow(reallocations) > 0) {
          movers <- reallocations$worker
          svy$agriculture[movers] <- 0L
          svy$industry[movers]    <- 0L
          svy$services[movers]    <- 0L
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

  # Social protection: add per-household transfer as SP_TRANSFER_COL column.
  # welfare is the regression outcome (not a covariate), so we tag the
  # transfer amount here; run_sim_pipeline() (fct_simulations.R) reads the
  # column post-prediction and adds it to y_point on the level scale
  # (re-logged when so$transform == "log") so the boost flows into
  # aggregate_with_uncertainty_delta() and matches the decomposition's δ_sp.
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
        svy[[SP_TRANSFER_COL]] <- ifelse(eligible, daily_hh, 0)
      }
    }
    else if (sp$budget_mode == "budget_first" && isTRUE(sp$budget_fixed > 0)) {
      # Distribute the fixed budget across eligible households. Eligibility is
      # determined once here using the baseline survey; SP_TRANSFER_COL holds
      # the resulting daily per-household amount.
      #
      # When survey weights are present, divide the budget by the weighted
      # count of eligible households so that the population-level total
      # sum(transfer * weight) equals budget_fixed. Without weight adjustment
      # the realised budget would scale with mean(weight) on eligible HHs.
      eligible <- .determine_sp_eligibility(svy, sp)
      n_eligible <- sum(eligible)
      if (n_eligible > 0) {
        weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                           names(svy), value = TRUE,
                           ignore.case = TRUE)[1]
        w_elig <- if (!is.na(weight_col)) {
          w <- svy[[weight_col]][eligible]
          sum(w[is.finite(w) & w > 0], na.rm = TRUE)
        } else 0
        divisor <- if (w_elig > 0) w_elig else n_eligible
        annual_hh <- sp$budget_fixed / divisor
        daily_hh  <- annual_hh / 365
        svy[[SP_TRANSFER_COL]] <- ifelse(eligible, daily_hh, 0)
      }
    }
  }

  svy
}


#' Re-run the Step 2 simulation pipeline against an alternative survey frame
#'
#' Reuses the model fit, residual draw spec, coefficient draws, and weather
#' data captured in the Step 2 \code{hist_sim} object to re-produce a Step 2-
#' shaped \code{hist_sim} / \code{saved_scenarios} pair using \code{svy} as
#' the input population. For saved scenarios, only the representative weather
#' frame stored in each entry is re-simulated (n_models = 1 in the output).
#'
#' @param svy   Survey-weather data frame (baseline or policy-adjusted).
#' @param sw    Selected-weather metadata (from Step 1).
#' @param so    Selected-outcome metadata (from Step 1).
#' @param mf    Model-fit list (from Step 1) with \code{$fit3}, \code{$engine},
#'   \code{$train_data}, and (for RIF) \code{$taus}, \code{$weather_terms}.
#' @param hist_sim_baseline      Step 2 hist_sim list.
#' @param saved_scenarios_baseline Step 2 named scenario list.
#'
#' @return A named list with elements \code{hist_sim} and
#'   \code{saved_scenarios} matching the Step 2 schema, or \code{NULL} on
#'   failure.
#' @export
resimulate_with_svy <- function(svy, sw, so, mf,
                                hist_sim_baseline,
                                saved_scenarios_baseline = list(),
                                svy_baseline = NULL) {
  if (is.null(svy) || is.null(mf) || is.null(hist_sim_baseline) ||
      is.null(so)) return(NULL)

  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                     names(svy), value = TRUE,
                     ignore.case = TRUE)[1]
  if (is.na(weight_col %||% NA)) weight_col <- NULL

  pov_line   <- hist_sim_baseline$pov_line
  residuals  <- hist_sim_baseline$residuals %||%
                hist_sim_baseline$pipeline$residuals %||% "none"
  # Canonical Cholesky-factor key is $chol_obj (matches Step 2 hist_sim and
  # the primary run_sim_pipeline parameter). Older Mod 3 outputs stored it
  # under $chol_Sigma; read that as a fallback so existing in-memory state
  # keeps working, but emit only $chol_obj on output.
  chol_obj <- hist_sim_baseline$chol_obj %||% hist_sim_baseline$chol_Sigma

  # Rebuild the additive-decomposition active mask for the policy arm. Step 2
  # attached a weather-only mask; the policy survey adds modified columns
  # (e.g. electricity, internet, employed) that must also stay active.
  # Diff svy (post-policy) against svy_baseline (the pre-policy survey) so
  # the detected modifications are exactly what apply_policy_to_svy()
  # flipped — robust to baseline/training mismatch.
  if (!is.null(chol_obj)) {
    is_rif_shape <- is.list(chol_obj) && !("L" %in% names(chol_obj))
    if (is_rif_shape) attr(chol_obj, "active_mask") <- NULL
    else              chol_obj$active_mask         <- NULL
    chol_obj <- attach_active_mask(
      chol_obj                            = chol_obj,
      svy_modified                        = svy,
      svy_reference                       = svy_baseline,
      train_data                          = mf$train_data,
      weather_terms                       = mf$weather_terms,
      outcome_col                         = so$name,
      residuals                           = residuals,
      propagate_all_covariate_uncertainty =
        isTRUE(hist_sim_baseline$propagate_all_covariate_uncertainty)
    )
  }

  is_rif       <- identical(mf$engine, "rif")
  fit_multi    <- if (is_rif) mf$fit3 else NULL
  rif_taus     <- if (is_rif) mf$taus else NULL
  rif_weather  <- if (is_rif) mf$weather_terms else NULL
  rif_grid     <- if (is_rif) mf$rif_grid else NULL
  model        <- if (is_rif) extract_rif_median(mf$fit3, mf$engine) else mf$fit3

  run_one <- function(weather_raw, slim) {
    tryCatch(
      run_sim_pipeline(
        weather_raw  = weather_raw,
        svy          = svy,
        sw           = sw,
        so           = so,
        model        = model,
        residuals    = residuals,
        train_data   = mf$train_data,
        engine       = mf$engine,
        chol_obj     = chol_obj,
        slim         = slim,
        fit_multi    = fit_multi,
        taus         = rif_taus,
        weather_cols = rif_weather,
        svy_baseline = svy_baseline,
        rif_grid     = rif_grid
      ),
      error = function(e) {
        warning("[resimulate_with_svy] run_sim_pipeline failed: ",
                conditionMessage(e))
        NULL
      }
    )
  }

  hist_out <- run_one(hist_sim_baseline$weather_raw, slim = FALSE)
  if (is.null(hist_out)) return(NULL)

  # Emit the same schema Mod 2's run_full_simulation() produces, so the Mod 3
  # Results pane (and any downstream consumer) can read baseline and policy
  # objects through one code path. hist_sim nests its single run under
  # $pipeline; each saved scenario carries a named $pipelines list, one entry
  # per CMIP6 ensemble member.
  hist_sim_new <- list(
    pipeline       = hist_out,
    chol_obj       = chol_obj,
    so             = so,
    has_weights    = !is.null(hist_out$weight),
    weather_raw    = hist_out$weather_raw,
    train_data     = mf$train_data,
    residuals      = residuals,
    pov_line       = hist_sim_baseline$pov_line,
    yr_range       = hist_sim_baseline$yr_range,
    S              = hist_sim_baseline$S %||% 200L
  )

  saved_scenarios_new <- lapply(seq_along(saved_scenarios_baseline), function(i) {
    s <- saved_scenarios_baseline[[i]]
    # Re-simulate once per CMIP6 ensemble member using that member's own
    # `weather_raw`. Each pipeline in `s$pipelines` was produced by
    # run_sim_pipeline() in fct_run_simulation.R and carries its own
    # `weather_raw`; iterating here is what gives Module 3 a genuine
    # inter-model spread to display downstream.
    pipes <- s$pipelines
    if (is.null(pipes) || length(pipes) == 0L) {
      # Fallback to the representative weather (older saved-scenario schema).
      # Log so mixed-version deployments aren't silently downgraded to a
      # single-member spread without the user noticing.
      sc_label <- names(saved_scenarios_baseline)[[i]] %||% paste0("scenario_", i)
      message("[resimulate_with_svy] Scenario '", sc_label,
              "' has no $pipelines; falling back to representative weather ",
              "(single model — inter-model spread will not be available).")
      out <- run_one(s$weather_raw, slim = TRUE)
      if (is.null(out)) return(NULL)
      pipes_new <- list(single = out)
    } else {
      member_names <- names(pipes) %||% paste0("model_", seq_along(pipes))
      pipes_new <- lapply(seq_along(pipes), function(i) {
        wr <- pipes[[i]]$weather_raw %||% s$weather_raw
        run_one(wr, slim = TRUE)
      })
      names(pipes_new) <- member_names
      pipes_new <- Filter(Negate(is.null), pipes_new)
      if (length(pipes_new) == 0L) return(NULL)
    }

    list(
      pipelines   = pipes_new,
      weather_raw = s$weather_raw,
      chol_obj    = chol_obj,
      so          = so,
      year_range  = s$year_range,
      n_models    = length(pipes_new)
    )
  })
  names(saved_scenarios_new) <- names(saved_scenarios_baseline)
  saved_scenarios_new <- saved_scenarios_new[
    !vapply(saved_scenarios_new, is.null, logical(1))
  ]

  list(hist_sim = hist_sim_new, saved_scenarios = saved_scenarios_new)
}


# ---------------------------------------------------------------------------- #
# Policy-by-delta: derive policy pipelines from baseline + analytic per-HH delta
# ---------------------------------------------------------------------------- #

#' Apply a per-household policy delta to the baseline Step 2 pipelines
#'
#' Module 3 historically built its policy arm by calling
#' \code{resimulate_with_svy()} — a full re-prediction against the policy-
#' adjusted survey. That works, but it (a) duplicates compute for every
#' CMIP6 member, and (b) causes the Results pane to disagree with the
#' Decomposition pane whenever residual drawing has any stochastic component
#' (e.g. \code{residuals = "original"} with any unmatched households): the
#' baseline and policy arms call \code{aggregate_pipeline_per_year()}
#' independently and get independent residual draws, so a no-op policy can
#' produce a non-zero visual gap.
#'
#' This helper takes the closed-form per-household delta the Decomposition
#' pane already produces (\code{decompose_policy_effect()$delta_total}) and
#' applies it directly to the cached baseline pipelines:
#'   \itemize{
#'     \item \code{y_point_policy = y_point_baseline + delta[svy_row_id]}
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
#'     \item The delta is computed once per pipeline using that pipeline's
#'           full \code{weather_raw} (period-mean hazard), matching the
#'           Decomposition Summary table. Per-\code{sim_year} delta variation
#'           inside a scenario is not yet modelled — also a follow-up.
#'   }
#'
#' @param svy_baseline           Baseline survey-weather data frame.
#' @param svy_policy             Policy-adjusted survey-weather data frame.
#' @param model_fit              Step 1 model-fit list.
#' @param so                     Selected-outcome metadata.
#' @param hist_sim_baseline      Step 2 \code{hist_sim} list (verbatim).
#' @param saved_scenarios_baseline Step 2 named \code{saved_scenarios} list.
#' @param skip_coef              Logical. Forwarded to
#'   \code{decompose_policy_effect()} — when TRUE, per-channel SEs are zeroed
#'   but \code{delta_total} is still computed.
#'
#' @return Named list with \code{hist_sim} and \code{saved_scenarios} on the
#'   Step 2 schema, or \code{NULL} on failure.
#' @export
apply_policy_delta_to_baseline <- function(svy_baseline,
                                            svy_policy,
                                            model_fit,
                                            so,
                                            hist_sim_baseline,
                                            saved_scenarios_baseline = list(),
                                            skip_coef = FALSE) {
  if (is.null(svy_baseline) || is.null(svy_policy) ||
      is.null(model_fit) || is.null(so) ||
      is.null(hist_sim_baseline)) return(NULL)

  # Per-HH delta_total for a given weather panel. Returns NULL when the
  # decomposition is unavailable (engine outside {rif, fixest}, missing
  # model bits, etc.) so the caller can fall back to resimulate_with_svy().
  delta_for <- function(weather_raw) {
    decomp <- tryCatch(
      decompose_policy_effect(
        svy_baseline = svy_baseline,
        svy_policy   = svy_policy,
        model_fit    = model_fit,
        so           = so,
        weather_raw  = weather_raw,
        skip_coef    = skip_coef
      ),
      error = function(e) {
        warning("[apply_policy_delta_to_baseline] decompose_policy_effect() ",
                "failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(decomp) || !"delta_total" %in% names(decomp)) return(NULL)
    decomp$delta_total
  }

  apply_to_pipeline <- function(pipe, weather_raw_for_delta) {
    if (is.null(pipe) || is.null(pipe$y_point)) return(pipe)
    delta_hh <- delta_for(weather_raw_for_delta)
    if (is.null(delta_hh)) return(pipe)

    # Broadcast delta_hh (length nrow(svy_baseline)) onto the expanded
    # (HH x year) pipeline rows via svy_row_id. When svy_row_id is missing
    # (older pipeline format pre-`.svy_row_id` tagging), fall back to
    # broadcasting by id_vec if a matching id column is on the baseline
    # survey, else skip the policy correction with a warning.
    sri <- pipe$svy_row_id
    if (is.null(sri) || length(sri) != length(pipe$y_point)) {
      id_col <- pipe$id_col
      if (!is.null(id_col) && !is.null(pipe$id_vec) &&
          id_col %in% names(svy_baseline)) {
        lookup <- match(pipe$id_vec, svy_baseline[[id_col]])
        delta_per_row <- delta_hh[lookup]
        delta_per_row[is.na(delta_per_row)] <- 0
      } else {
        warning("[apply_policy_delta_to_baseline] pipeline lacks svy_row_id ",
                "and no usable id_col fallback; policy arm will equal ",
                "baseline for this pipeline.")
        return(pipe)
      }
    } else {
      delta_per_row <- delta_hh[sri]
      delta_per_row[is.na(delta_per_row)] <- 0
    }

    pipe$y_point <- pipe$y_point + delta_per_row
    pipe
  }

  hist_pipeline_new <- apply_to_pipeline(
    hist_sim_baseline$pipeline,
    hist_sim_baseline$weather_raw %||% hist_sim_baseline$pipeline$weather_raw
  )

  hist_sim_new <- hist_sim_baseline
  hist_sim_new$pipeline <- hist_pipeline_new

  saved_scenarios_new <- lapply(saved_scenarios_baseline, function(s) {
    if (is.null(s) || is.null(s$pipelines)) return(s)
    pipes_new <- lapply(s$pipelines, function(pipe) {
      apply_to_pipeline(pipe, pipe$weather_raw %||% s$weather_raw)
    })
    names(pipes_new) <- names(s$pipelines)
    s$pipelines <- pipes_new
    s
  })
  names(saved_scenarios_new) <- names(saved_scenarios_baseline)

  list(hist_sim = hist_sim_new, saved_scenarios = saved_scenarios_new)
}
