# ============================================================================ #
# Pure functions used by mod_3_05_policy_sim.
# Stateless and testable without Shiny.
#
# Two main helpers:
#   apply_policy_to_svy()   -- modifies survey_weather covariates to reflect
#                              infra / sp / digital / labor scenario inputs.
#   run_policy_pipeline()   -- re-runs the prediction pipeline over the
#                              historical + saved future scenarios stored in
#                              Step 2, using the policy-modified survey frame.
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

  if (!is.null(infra)) {
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

  # sp / digital / labor scenarios: infrastructure for future wiring.
  svy
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
      n_nonNA        = sum(!is.na(xb)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Dodged Point-Range Chart Comparing Baseline vs Policy Scenarios
#'
#' Plots baseline and policy-adjusted outcomes side-by-side for each scenario
#' (Historical + each SSP × period), using a dodged position and distinct
#' colours (baseline = grey, policy = SSP colour).
#'
#' @param baseline_series Named list of \code{aggregate_sim_preds()} outputs for
#'   baseline scenarios (historical + each future). Names must include the SSP
#'   and year-range tokens so they can be parsed.
#' @param policy_series   Same structure, for policy-adjusted scenarios.
#' @param hist_agg        Baseline historical aggregate (dashed reference line).
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_shape_manual labs theme_minimal theme
#'   element_blank element_text position_dodge
#' @export
plot_policy_comparison <- function(baseline_series, policy_series, hist_agg,
                                   group_order = "scenario_x_year") {
  summarise_vals <- .summarise_vals

  build_rows <- function(series_list, variant) {
    if (length(series_list) == 0) return(NULL)
    rows <- lapply(names(series_list), function(nm) {
      s <- summarise_vals(series_list[[nm]]$out$value)
      if (is.null(s)) return(NULL)
      is_hist   <- is.na(.normalise_ssp(nm))
      ssp_key   <- if (is_hist) "Historical" else .normalise_ssp(nm)
      yr        <- if (is_hist) "" else (.parse_year(nm) %||% "")
      ssp_short_map <- c("SSP2-4.5" = "SSP2", "SSP3-7.0" = "SSP3", "SSP5-8.5" = "SSP5")
      ssp_short <- if (is_hist) "Hist" else (ssp_short_map[ssp_key] %||% ssp_key)
      s$pt_key    <- if (is_hist) "Historical" else paste0(ssp_short, "\n", yr)
      s$variant   <- variant
      s$ssp_key   <- ssp_key
      s
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  df <- dplyr::bind_rows(
    build_rows(baseline_series, "Baseline"),
    build_rows(policy_series,   "Policy")
  )

  if (is.null(df) || nrow(df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run the policy simulation to see results."))
  }

  # Order x-axis: Historical first, then by group_order
  ssp_order <- c("Historical", "SSP2", "SSP3", "SSP5")
  df$ssp_short <- sub("\n.*$", "", df$pt_key)
  df$yr        <- sub("^[^\n]*\n?", "",  df$pt_key)
  lvl_order <- if (identical(group_order, "year_x_scenario")) {
    unique(df$pt_key[order(
      df$ssp_short != "Historical",  # keep Historical first
      df$yr,
      match(df$ssp_short, ssp_order)
    )])
  } else {
    unique(df$pt_key[order(
      match(df$ssp_short, ssp_order), df$yr
    )])
  }
  df$pt_key <- factor(df$pt_key, levels = lvl_order)
  df$variant <- factor(df$variant, levels = c("Baseline", "Policy"))

  pal <- c("Baseline" = "#808080", "Policy" = "#c0392b")

  hist_mean <- summarise_vals(hist_agg$out$value)$mean

  pd <- ggplot2::position_dodge(width = 0.6)

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$pt_key, colour = .data$variant, group = .data$variant
  )) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
      linewidth = 0.5, position = pd
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lo90, ymax = .data$hi90),
      linewidth = 2.0, position = pd
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$mean),
      shape = 21, fill = "white", stroke = 1.4, size = 3, position = pd
    ) +
    ggplot2::geom_hline(
      yintercept = hist_mean, linetype = "dashed",
      colour = "#808080", linewidth = 0.55
    ) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::labs(x = NULL, y = hist_agg$x_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = "top"
    )
}


#' Run the Prediction Pipeline over Step 2 Scenarios with Modified Covariates
#'
#' Reuses the weather already simulated in Step 2 (stored in \code{hist_sim}
#' and \code{saved_scenarios}) but re-runs predictions against a policy-
#' adjusted survey frame. Future scenarios preserve per-ensemble-model
#' grouping via the \code{model} column attached to each scenario's
#' \code{weather_raw} by Step 2.
#'
#' @param hist_sim         List with \code{weather_raw}, \code{preds}, \code{so},
#'                         \code{n_pre_join} (as stored by Step 2).
#' @param saved_scenarios  Named list; each entry has \code{weather_raw},
#'                         \code{so}, \code{year_range}, \code{n_models}.
#' @param svy_mod          Policy-adjusted survey-weather data frame.
#' @param sw               Selected weather metadata.
#' @param so               Selected outcome metadata.
#' @param model            Fitted model object (\code{mf$fit3}).
#' @param residuals        Character; residual method.
#' @param train_data       Training data frame (\code{mf$train_data}).
#' @param engine           Character; model engine.
#'
#' @return List with \code{hist_sim} (same shape as Step 2) and
#'   \code{saved_scenarios} (named list with same shape as Step 2 entries).
#' @export
run_policy_pipeline <- function(hist_sim,
                                saved_scenarios,
                                svy_mod,
                                sw,
                                so,
                                model,
                                residuals,
                                train_data,
                                engine) {

  # ---- Historical ---------------------------------------------------------
  hist_result <- run_sim_pipeline(
    weather_raw = hist_sim$weather_raw,
    svy         = svy_mod,
    sw          = sw,
    so          = so,
    model       = model,
    residuals   = residuals,
    train_data  = train_data,
    engine      = engine,
    slim        = FALSE
  )

  hist_out <- list(
    preds       = hist_result$preds,
    so          = so,
    n_pre_join  = hist_result$n_pre_join,
    weather_raw = hist_result$weather_raw,
    train_data  = train_data
  )

  # ---- Future scenarios ---------------------------------------------------
  new_scenarios <- list()
  for (nm in names(saved_scenarios)) {
    s  <- saved_scenarios[[nm]]
    wr <- s$weather_raw

    if ("model" %in% names(wr)) {
      model_names <- unique(wr$model)
      preds_parts   <- list()
      weather_parts <- list()

      for (mn in model_names) {
        wrm <- wr[wr$model == mn, , drop = FALSE]
        # prepare_hist_weather would carry `model` through but also join on it;
        # drop to avoid key clash and re-tag after prediction.
        wrm$model <- NULL

        out <- tryCatch(
          run_sim_pipeline(
            weather_raw = wrm,
            svy         = svy_mod,
            sw          = sw,
            so          = so,
            model       = model,
            residuals   = residuals,
            train_data  = train_data,
            engine      = engine,
            slim        = TRUE
          ),
          error = function(e) {
            warning("[run_policy_pipeline] scenario '", nm, "' model '", mn,
                    "' failed: ", conditionMessage(e))
            NULL
          }
        )
        if (is.null(out)) next

        out$preds$model       <- mn
        out$weather_raw$model <- mn
        preds_parts[[mn]]     <- out$preds
        weather_parts[[mn]]   <- out$weather_raw
      }

      if (length(preds_parts) == 0L) next

      new_scenarios[[nm]] <- list(
        preds       = dplyr::bind_rows(preds_parts),
        weather_raw = dplyr::bind_rows(weather_parts),
        so          = so,
        year_range  = s$year_range,
        n_models    = length(preds_parts)
      )
    } else {
      # Pre-model-tag scenario: single pass, fall back to sim_year-only CI.
      out <- tryCatch(
        run_sim_pipeline(
          weather_raw = wr,
          svy         = svy_mod,
          sw          = sw,
          so          = so,
          model       = model,
          residuals   = residuals,
          train_data  = train_data,
          engine      = engine,
          slim        = TRUE
        ),
        error = function(e) {
          warning("[run_policy_pipeline] scenario '", nm, "' failed: ",
                  conditionMessage(e))
          NULL
        }
      )
      if (is.null(out)) next
      new_scenarios[[nm]] <- list(
        preds       = out$preds,
        weather_raw = out$weather_raw,
        so          = so,
        year_range  = s$year_range,
        n_models    = s$n_models %||% 1L
      )
    }
  }

  list(
    hist_sim        = hist_out,
    saved_scenarios = new_scenarios
  )
}
