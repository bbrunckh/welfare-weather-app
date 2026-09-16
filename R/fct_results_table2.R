# Step 1 focused regression table ----
# Step 1 focused regression table (T2). Results-first `.wise-table` of the      #
# weather + interaction coefficients of the full specification (3), with a      #
# per-row translation on the outcome's reporting scale. Complements             #
# make_regtable() - the full AER-style table stays the expandable               #
# "all coefficients" view.                                                      #
#                                                                               #
# One tidy row derivation (.t2_focused_rows) feeds both the HTML renderer and   #
# the export data frame, so the two cannot diverge. Translations mirror the     #
# pct/pp/level formatting rules of step1_fmt_effect() / .s1_fmt_scaled() in     #
# fct_step1_headline.R without calling those private helpers.                   #


# Small helpers ----

.t2_esc <- function(x) gsub("([][{}().+*^$|?\\\\])", "\\\\\\1", x)

# Significance stars from a p-value (same thresholds as make_regtable()).
.t2_stars <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
    ifelse(p < 0.01,  "**",
    ifelse(p < 0.05,  "*",
    ifelse(p < 0.1,   "\u2020", "")))))
}

# Format a model-scale number on the outcome's reporting scale (mirrors
# .s1_fmt_scaled() / step1_fmt_effect() without the CI):
#   "pct"   -> percent change from log points
#   "pp"    -> percentage points
#   "level" -> raw outcome units
.t2_scale_fmt <- function(est, scale) {
  switch(scale,
    pct = sprintf("%+.1f%%", 100 * (exp(est) - 1)),
    pp  = sprintf("%+.1f pp", 100 * est),
    sprintf("%+.3f", est))
}

# Readable "a-b" label for a bin coefficient: strips the "^var[(]" prefix and
# the trailing "[)]", splits on "," and joins with an en dash. Falls back to
# the raw term when the shape does not match.
.t2_bin_label <- function(term, var) {
  inner <- sub(paste0("^", .t2_esc(var), "[\\[\\(]"), "", term)
  if (endsWith(inner, "]") || endsWith(inner, ")")) {
    inner <- substr(inner, 1, nchar(inner) - 1)
  }
  parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    paste0(trimws(parts[1]), " \u2013 ", trimws(parts[2]))
  } else {
    term
  }
}

# Weather variable a coefficient belongs to (exact / I(var^k) / bin /
# var:moderator shapes). Falls back to the first weather variable. Poly terms
# allow fixest's double-wrapped "I(I(var^2))" spelling.
.t2_poly_pat <- function(var) paste0("^I\\(I?\\(", .t2_esc(var), "\\^")

.t2_row_var <- function(term, weather_terms) {
  for (v in weather_terms) {
    e <- .t2_esc(v)
    if (grepl(.t2_poly_pat(v), term)) return(v)
    if (grepl(paste0("^", e, "($|:|[\\[\\(])"), term)) return(v)
  }
  weather_terms[1]
}

# SD of one weather variable from the caller-supplied (possibly named) vector.
.t2_sd_for <- function(sd_x, var) {
  if (is.null(sd_x) || !length(sd_x)) return(NULL)
  v <- if (var %in% names(sd_x)) sd_x[[var]] else sd_x[[1]]
  if (!is.finite(v) || v <= 0) NULL else v
}

# Label one raw term part: bin-shaped parts get the "a-b" range, everything
# else goes through the label lookup.
.t2_part_label <- function(part, weather_terms, label_fun) {
  for (v in weather_terms) {
    if (grepl(paste0("^", .t2_esc(v), "[\\[\\(]"), part)) {
      return(.t2_bin_label(part, v))
    }
  }
  lab <- tryCatch(label_fun(part), error = function(e) part)
  if (is.null(lab) || is.na(lab) || !nzchar(lab)) part else lab
}

# Readable row label: interaction terms are split on ":" and labelled per
# part (so bin interactions read "80 - 120 x Urban"), bin terms get the range,
# everything else goes through coef_label().
.t2_var_label <- function(term, weather_terms, label_fun = identity) {
  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    labs <- vapply(parts, .t2_part_label, character(1),
                   weather_terms = weather_terms, label_fun = label_fun)
    return(paste(labs, collapse = " \u00d7 "))
  }
  .t2_part_label(term, weather_terms, label_fun)
}

# Order weather-related coefficients: exact main term(s) first, then
# polynomial I(var^k) terms, then bins (sorted by numeric lower bound);
# interactions last, in the caller's interaction_terms order when possible.
# Interaction terms (containing ":") never enter the main-term shapes, so
# bin interactions are not duplicated as bins.
.t2_ordered_terms <- function(coef_names, weather_terms, interaction_terms = character(0)) {
  inter <- coef_names[grepl(":", coef_names, fixed = TRUE)]
  main_pool <- setdiff(coef_names, inter)
  main <- character(0)
  for (v in weather_terms) {
    e <- .t2_esc(v)
    main <- c(main, main_pool[main_pool == v])
    main <- c(main, grep(.t2_poly_pat(v), main_pool, value = TRUE))
    bins <- grep(paste0("^", e, "[\\[\\(]"), main_pool, value = TRUE)
    if (length(bins) > 1) {
      lo <- suppressWarnings(
        as.numeric(sub(paste0("^", e, "[\\[\\(]([^,]+),.*"), "\\1", bins)))
      bins <- bins[order(lo)]
    }
    main <- c(main, bins)
  }
  main <- unique(c(main, setdiff(main_pool, main)))
  if (length(inter) && length(interaction_terms)) {
    canon <- function(t) paste(sort(strsplit(t, ":", fixed = TRUE)[[1]]), collapse = ":")
    key <- vapply(inter, function(t) {
      idx <- which(vapply(interaction_terms, function(p) identical(canon(p), canon(t)), logical(1)))
      if (length(idx)) idx[1] else length(interaction_terms) + 1L
    }, integer(1))
    inter <- inter[order(key)]
  }
  list(main = main, inter = inter)
}

# Uniform coeftable extraction (term / estimate / std.error / p.value).
.t2_fit_coefs <- function(fit) {
  ct <- tryCatch(.fixest_coeftable(fit), error = function(e) NULL)
  if (is.null(ct) || nrow(ct) == 0 || is.null(rownames(ct))) return(NULL)
  pv <- if (ncol(ct) >= 4) ct[, 4] else ct[, ncol(ct)]
  data.frame(
    term      = rownames(ct),
    estimate  = suppressWarnings(as.numeric(ct[, 1])),
    std.error = suppressWarnings(as.numeric(ct[, 2])),
    p.value   = suppressWarnings(as.numeric(pv)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# Shared row derivation ----

# Tidy rows behind both the focused table and its data-frame export.
# fixest: one row per weather/interaction term of fit3.
# rif:    one row per (term, tau) of model 3; Translation only at tau = 0.5.
.t2_focused_rows <- function(fit3, weather_terms, interaction_terms, label_fun = identity,
                             engine = "fixest", is_logistic = FALSE, is_lpm = FALSE,
                             is_log_outcome = TRUE, rif_grid = NULL,
                             mf = NULL, scenarios_list = NULL, sd_x = NULL) {
  weather_terms <- if (is.null(weather_terms)) character(0) else as.character(weather_terms)
  weather_terms <- weather_terms[nzchar(weather_terms)]
  if (!length(weather_terms)) return(NULL)

  # Reporting scale (mirrors .s1_scale()).
  scale <- if (isTRUE(is_logistic) || isTRUE(is_lpm)) "pp" else
           if (isTRUE(is_log_outcome)) "pct" else "level"

  # scenarios_list: named list of step1_scenarios() results keyed by weather
  # variable (quadratic-inclusive +1 SD contrasts, one translation path).
  eta <- if (length(scenarios_list)) scenarios_list[[1]]$profile_eta else NULL
  if (is.null(eta) || !is.finite(eta)) eta <- NULL

  # Which weather variables enter with polynomial terms? Their interaction
  # slope differences are weather-level-dependent (see .translate).
  ct3_terms <- tryCatch(rownames(.fixest_coeftable(fit3)),
                        error = function(e) character(0))
  has_poly <- stats::setNames(
    vapply(weather_terms, function(v) any(grepl(.t2_poly_pat(v), ct3_terms)),
           logical(1)),
    weather_terms
  )

  # Fallback: derive per-variable SDs from the estimation data (numeric only).
  if ((is.null(sd_x) || !length(sd_x)) && !is.null(mf$train_data)) {
    sd_x <- vapply(weather_terms, function(v) {
      col <- mf$train_data[[v]]
      if (is.null(col) || !is.numeric(col)) return(NA_real_)
      x <- col[is.finite(col)]
      if (length(x) > 1) stats::sd(x) else NA_real_
    }, numeric(1))
  }

  # Per-row translation on the reporting scale; NA renders as "-".
  .translate <- function(term, est) {
    v <- .t2_row_var(term, weather_terms)
    s <- .t2_sd_for(sd_x, v)
    is_inter <- grepl(":", term, fixed = TRUE)
    is_poly  <- grepl("^I\\(", term)
    is_bin   <- grepl(paste0("^", .t2_esc(v), "[\\[\\(]"), term)
    if (is_inter) {
      # Polynomial terms and polynomial-x-moderator interactions have
      # weather-level-dependent slope differences (the polynomial row is part
      # of the same contrast) - no single number; the moderated effect plot
      # carries them.
      if (is_poly || isTRUE(has_poly[v])) return("\u2014")
      if (isTRUE(is_logistic) || is.null(s) || !is.finite(est * s)) {
        return(NA_character_)
      }
      return(paste0("slope difference: ", .t2_scale_fmt(est * s, scale)))
    }
    if (is_poly) return("\u2014")
    if (is_bin) {
      if (isTRUE(is_logistic)) {
        if (is.null(eta) || !is.finite(est)) return(NA_character_)
        return(sprintf("%+.1f pp", 100 * (plogis(eta + est) - plogis(eta))))
      }
      if (!is.finite(est)) return(NA_character_)
      return(.t2_scale_fmt(est, scale))
    }
    # Main linear term: prefer the scenario translation - for polynomial
    # specifications the +1 SD contrast runs along the whole polynomial (the
    # same number the At a glance card shows), not beta * SD of the linear
    # term alone. Falls back to the single-coefficient contrast.
    scn <- scenarios_list[[v]]
    s1 <- if (!is.null(scn) && length(scn$scenarios)) {
      if (identical(engine, "rif")) {
        hit <- Filter(function(x) is.finite(x$tau) && abs(x$tau - 0.5) < 1e-9,
                      scn$scenarios)
        if (length(hit)) hit[[1]] else NULL
      } else {
        scn$scenarios[[1]]
      }
    } else NULL
    if (!is.null(s1) && is.finite(s1$estimate) && is.finite(s1$se) && s1$se > 0) {
      fmt <- step1_fmt_effect(s1$estimate, s1$se, scale, ci = s1$ci)
      return(fmt$value)
    }
    if (isTRUE(is_logistic)) {
      if (is.null(eta) || is.null(s) || !is.finite(est * s)) return(NA_character_)
      return(sprintf("%+.1f pp", 100 * (plogis(eta + est * s) - plogis(eta))))
    }
    if (is.null(s) || !is.finite(est * s)) return(NA_character_)
    .t2_scale_fmt(est * s, scale)
  }

  # --- RIF: one row per (term, tau) of model 3 -------------------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    grid3 <- rif_grid[rif_grid$model == 3L, , drop = FALSE]
    if (!nrow(grid3)) return(NULL)
    wpat <- paste0("\\b(", paste(weather_terms, collapse = "|"), ")\\b")
    keep <- unique(as.character(grid3$term))
    keep <- keep[grepl(wpat, keep)]
    ord  <- .t2_ordered_terms(keep, weather_terms, interaction_terms)
    terms_vec <- c(ord$main, ord$inter)
    if (!length(terms_vec)) return(NULL)
    taus <- sort(unique(grid3$tau))

    row_fn <- function(tm, group) {
      sub <- grid3[as.character(grid3$term) == tm, , drop = FALSE]
      if (!nrow(sub)) return(NULL)
      do.call(rbind, lapply(taus, function(tau) {
        r <- sub[abs(sub$tau - tau) < 1e-9, , drop = FALSE]
        if (!nrow(r)) return(NULL)
        est <- if ("estimate" %in% names(r)) suppressWarnings(as.numeric(r$estimate[1])) else NA_real_
        se  <- if ("std.error" %in% names(r)) suppressWarnings(as.numeric(r$std.error[1])) else NA_real_
        pv  <- if ("p.value" %in% names(r)) suppressWarnings(as.numeric(r$p.value[1])) else NA_real_
        lo  <- if ("conf.low" %in% names(r)) suppressWarnings(as.numeric(r$conf.low[1])) else est - 1.96 * se
        hi  <- if ("conf.high" %in% names(r)) suppressWarnings(as.numeric(r$conf.high[1])) else est + 1.96 * se
        data.frame(
          Variable = .t2_var_label(tm, weather_terms, label_fun),
          Group    = group,
          Term     = tm,
          Tau      = tau,
          Effect   = est,
          CI_low   = lo,
          CI_high  = hi,
          SE       = se,
          p        = pv,
          Translation = if (abs(tau - 0.5) < 1e-9) .translate(tm, est) else NA_character_,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }))
    }
    rows <- do.call(rbind, c(
      lapply(ord$main, row_fn, group = "Weather effects"),
      lapply(ord$inter, row_fn, group = "Interactions")))
    if (is.null(rows) || !nrow(rows)) return(NULL)
    return(rows)
  }

  # --- fixest: one row per weather/interaction term of fit3 ------------------
  cf <- .t2_fit_coefs(fit3)
  if (is.null(cf)) return(NULL)
  keep <- tryCatch(weather_coef_names(fit3, weather_terms), error = function(e) character(0))
  keep <- as.character(keep)
  keep <- keep[keep %in% cf$term]
  if (!length(keep)) return(NULL)
  cf <- cf[match(keep, cf$term), , drop = FALSE]
  ord <- .t2_ordered_terms(cf$term, weather_terms, interaction_terms)

  row_fn <- function(tm, group) {
    r <- cf[cf$term == tm, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    est <- r$estimate[1]
    se  <- r$std.error[1]
    pv  <- r$p.value[1]
    data.frame(
      Variable = .t2_var_label(tm, weather_terms, label_fun),
      Group    = group,
      Term     = tm,
      Effect   = est,
      CI_low   = est - 1.96 * se,
      CI_high  = est + 1.96 * se,
      SE       = se,
      p        = pv,
      Translation = .translate(tm, est),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  rows <- do.call(rbind, c(
    lapply(ord$main, row_fn, group = "Weather effects"),
    lapply(ord$inter, row_fn, group = "Interactions")))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rows
}


# HTML renderer (over the shared tidy rows) ----

.t2_render_focused <- function(rows, engine, is_logistic, is_lpm, subheader, footnotes) {
  .f3 <- function(x) if (length(x) && is.finite(x)) formatC(x, format = "f", digits = 3) else ""
  .pci <- function(p) {
    if (!length(p) || is.na(p) || !is.finite(p)) return("")
    if (p < 0.001) "<0.001" else formatC(p, format = "f", digits = 3)
  }
  .trans <- function(x) if (!is.na(x) && nzchar(x)) x else "-"
  .stars_cell <- function(est, p) paste0(.f3(est), .t2_stars(p))

  if (identical(engine, "rif") && "Tau" %in% names(rows)) {
    # Pivot: rows = terms, columns = tau quantiles + one translated column.
    taus <- sort(unique(rows$Tau))
    has_bins <- any(grepl("[\\[\\(]", rows$Term))
    head_cells <- c(list("Variable"),
                    lapply(taus, function(t) sprintf("\u03c4 = %.1f", t)),
                    list(if (has_bins) "Translated effect (\u03c4 = 0.5)" else
                      "Per +1 SD (\u03c4 = 0.5)"))
    ncol_t <- length(taus) + 2L
    body <- list()
    prev <- NA_character_
    for (tm in unique(rows$Term)) {
      tr <- rows[rows$Term == tm, , drop = FALSE]
      group <- tr$Group[1]
      if (!identical(group, prev)) {
        body[[length(body) + 1L]] <- htmltools::tags$tr(class = "group",
                                             htmltools::tags$td(colspan = ncol_t, group))
        prev <- group
      }
      cls <- if (identical(group, "Weather effects")) "hi" else NULL
      est_cells <- lapply(taus, function(t) {
        r <- tr[abs(tr$Tau - t) < 1e-9, , drop = FALSE]
        if (!nrow(r)) return(htmltools::tags$td(class = "num", ""))
        htmltools::tags$td(class = "num", .stars_cell(r$Effect[1], r$p[1]))
      })
      se_cells <- lapply(taus, function(t) {
        r <- tr[abs(tr$Tau - t) < 1e-9, , drop = FALSE]
        if (!nrow(r)) return(htmltools::tags$td(class = "num", ""))
        htmltools::tags$td(class = "num", if (is.finite(r$SE[1])) paste0("(", .f3(r$SE[1]), ")") else "")
      })
      r50 <- tr[abs(tr$Tau - 0.5) < 1e-9, , drop = FALSE]
      trans <- if (nrow(r50)) .trans(r50$Translation[1]) else "-"
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = cls,
        htmltools::tags$td(tr$Variable[1]), est_cells, htmltools::tags$td(class = "num", trans))
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = "se",
        htmltools::tags$td(""), se_cells, htmltools::tags$td(class = "num", ""))
    }
  } else {
    # Binned terms translate as bin-vs-reference contrasts, not per-+1-SD
    # effects, so the mixed case carries a neutral header and the footnote
    # explains the per-term translation.
    has_bins <- any(grepl("[\\[\\(]", rows$Term))
    trans_header <- if (has_bins) "Translated effect"
                    else if (isTRUE(is_logistic)) "pp effect per +1 SD"
                    else if (isTRUE(is_lpm)) "pp per +1 SD"
                    else "Per +1 SD"
    head_cells <- list("Variable", "Effect", "95% CI", "SE", "p", trans_header)
    ncol_t <- length(head_cells)
    body <- list()
    prev <- NA_character_
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      if (!identical(r$Group, prev)) {
        body[[length(body) + 1L]] <- htmltools::tags$tr(class = "group",
                                             htmltools::tags$td(colspan = ncol_t, r$Group))
        prev <- r$Group
      }
      cls <- if (identical(r$Group, "Weather effects")) "hi" else NULL
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = cls,
        htmltools::tags$td(r$Variable),
        htmltools::tags$td(class = "num", .stars_cell(r$Effect, r$p)),
        htmltools::tags$td(class = "num", paste0(.f3(r$CI_low), " \u2013 ", .f3(r$CI_high))),
        htmltools::tags$td(class = "num", .f3(r$SE)),
        htmltools::tags$td(class = "num", .pci(r$p)),
        htmltools::tags$td(class = "num", .trans(r$Translation)))
    }
  }

  for (f in footnotes) {
    if (!is.null(f) && !is.na(f) && nzchar(f)) {
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = "t2-note", htmltools::tags$td(colspan = ncol_t, f))
    }
  }

  tbl <- htmltools::tags$table(class = "wise-table",
                    htmltools::tags$thead(htmltools::tags$tr(lapply(head_cells, htmltools::tags$th))),
                    htmltools::tags$tbody(body))
  htmltools::HTML(as.character(htmltools::tags$div(
    if (!is.null(subheader) && !is.na(subheader) && nzchar(subheader)) {
      htmltools::tags$p(class = "wise-subheader", subheader)
    },
    tbl
  )))
}


# Public entry points ----

#' Focused regression table for the Step 1 results tab (T2)
#'
#' Renders a results-first `.wise-table` of the weather and interaction
#' coefficients of the full specification (3): one row per term with the
#' estimate + significance stars, 95% CI, SE, p-value and a translated
#' per-+1-SD effect on the outcome's reporting scale. Weather rows are
#' highlighted; interactions follow in their own group. For the RIF engine a
#' pivot over quantiles (tau 0.1-0.9) is rendered instead, with a final
#' "Per +1 SD (tau = 0.5)" column. The full AER-style table
#' (\code{\link{make_regtable}}) remains the expandable "all coefficients" view.
#'
#' Translations mirror the formatting contract of \code{step1_fmt_effect()}:
#' exact main terms translate \code{est * SD} (percent change for log
#' outcomes, percentage points for probability outcomes), logit main terms use
#' the reference-profile linear predictor, interactions report the slope
#' difference, bins are reported vs the omitted reference, and polynomial rows
#' show an em dash (nonlinear, see the curve).
#'
#' @param fit3              Native fixest model of specification (3) (fixest_multi for RIF).
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key ("fixest" or "rif").
#' @param is_logistic       TRUE for logistic fits (Effect stays log-odds; translations in pp).
#' @param is_lpm            TRUE for linear models on a binary outcome (translations in pp).
#' @param is_log_outcome    TRUE when the outcome is log-transformed (translations in %).
#' @param rif_grid          Tidy RIF beta grid (from \code{fit_model()$rif_grid}).
#' @param mf                Named list returned by \code{fit_model()}; used to derive
#'   weather SDs from \code{mf$train_data} when \code{sd_x} is not supplied.
#' @param scenarios_list    Result of \code{step1_scenarios()}; supplies
#'   \code{profile_eta} for logit pp translations.
#' @param sd_x              Named numeric vector of sample SDs per weather variable.
#' @param subheader         Optional sub-header line above the table.
#' @param footnotes         Character vector of footnote lines under the table.
#'
#' @return `htmltools::HTML` (`.wise-table`), or NULL when no rows can be built.
#'
#' @export
make_regtable_focused <- function(fit3, weather_terms, interaction_terms, label_fun = identity,
                                  engine = "fixest", is_logistic = FALSE, is_lpm = FALSE,
                                  is_log_outcome = TRUE, rif_grid = NULL,
                                  mf = NULL, scenarios_list = NULL, sd_x = NULL,
                                  subheader = NULL, footnotes = character(0)) {
  tryCatch({
    rows <- .t2_focused_rows(fit3, weather_terms, interaction_terms, label_fun = label_fun,
                             engine = engine, is_logistic = is_logistic, is_lpm = is_lpm,
                             is_log_outcome = is_log_outcome, rif_grid = rif_grid,
                             mf = mf, scenarios_list = scenarios_list, sd_x = sd_x)
    if (is.null(rows) || !nrow(rows)) return(NULL)
    .t2_render_focused(rows, engine = engine, is_logistic = is_logistic, is_lpm = is_lpm,
                       subheader = subheader, footnotes = footnotes)
  }, error = function(e) {
    htmltools::tags$p(paste("Focused table error:", conditionMessage(e)))
  })
}


#' Compact specification-comparison table for the Step 1 results tab
#'
#' Renders a `.wise-table` comparing the weather coefficients of the three
#' progressive specifications - Variable | (1) No FE | (2) FE |
#' (3) FE + Controls - with the same grouping, ordering, labels and
#' estimate + stars / (SE) formatting as the focused table. Cells are empty
#' when a term is absent from a specification. Returns NULL for the RIF
#' engine (the caller hides the panel).
#'
#' @param fit1,fit2,fit3    Native fixest model objects (specifications 1-3).
#' @param weather_terms     Character vector of base weather variable names.
#' @param interaction_terms Character vector of interaction term strings.
#' @param label_fun         Function mapping variable names to readable labels.
#' @param engine            Scalar character engine key.
#' @param rif_grid          Unused; kept for call-site symmetry.
#'
#' @return `htmltools::HTML` (`.wise-table`), or NULL on the RIF engine or
#'   when no rows can be built.
#'
#' @export
make_regtable_specs <- function(fit1, fit2, fit3, weather_terms, interaction_terms,
                                label_fun = identity, engine = "fixest", rif_grid = NULL,
                                has_controls = TRUE) {
  if (identical(engine, "rif")) return(NULL)
  tryCatch({
    weather_terms <- if (is.null(weather_terms)) character(0) else as.character(weather_terms)
    weather_terms <- weather_terms[nzchar(weather_terms)]
    if (!length(weather_terms)) return(NULL)

    lab3 <- if (isTRUE(has_controls)) "(3) FE + Controls" else "(3) FE (no controls selected)"
    specs <- list("(1) No FE" = fit1, "(2) FE" = fit2)
    specs[[lab3]] <- fit3
    coefs <- lapply(specs, .t2_fit_coefs)
    cf3 <- coefs[[lab3]]
    if (is.null(cf3)) return(NULL)
    keep <- tryCatch(weather_coef_names(fit3, weather_terms), error = function(e) character(0))
    keep <- as.character(keep)
    keep <- keep[keep %in% cf3$term]
    if (!length(keep)) return(NULL)
    ord <- .t2_ordered_terms(keep, weather_terms, interaction_terms)
    terms_vec <- c(ord$main, ord$inter)
    if (!length(terms_vec)) return(NULL)
    groups <- ifelse(grepl(":", terms_vec, fixed = TRUE), "Interactions", "Weather effects")

    body <- list()
    prev <- NA_character_
    for (i in seq_along(terms_vec)) {
      tm <- terms_vec[i]
      if (!identical(groups[i], prev)) {
        body[[length(body) + 1L]] <- htmltools::tags$tr(class = "group", htmltools::tags$td(colspan = 4L, groups[i]))
        prev <- groups[i]
      }
      cells <- lapply(coefs, function(cf) {
        r <- if (is.null(cf)) NULL else cf[cf$term == tm, , drop = FALSE]
        if (is.null(r) || !nrow(r)) {
          list(est = "", se = "")
        } else {
          list(
            est = paste0(
              if (is.finite(r$estimate[1])) formatC(r$estimate[1], format = "f", digits = 3) else "",
              .t2_stars(r$p.value[1])),
            se  = if (is.finite(r$std.error[1])) {
              paste0("(", formatC(r$std.error[1], format = "f", digits = 3), ")")
            } else "")
        }
      })
      cls <- if (identical(groups[i], "Weather effects")) "hi" else NULL
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = cls,
        htmltools::tags$td(.t2_var_label(tm, weather_terms, label_fun)),
        lapply(cells, function(x) htmltools::tags$td(class = "num", x$est)))
      body[[length(body) + 1L]] <- htmltools::tags$tr(class = "se", htmltools::tags$td(""),
        lapply(cells, function(x) htmltools::tags$td(class = "num", x$se)))
    }

    tbl <- htmltools::tags$table(class = "wise-table",
                      htmltools::tags$thead(htmltools::tags$tr(lapply(c("Variable", names(specs)), htmltools::tags$th))),
                      htmltools::tags$tbody(body))
    htmltools::HTML(as.character(tbl))
  }, error = function(e) NULL)
}


#' Tidy data frame behind the focused regression table (export bundle / CSV)
#'
#' One row per weather/interaction term of the full specification (3) with
#' model-scale numbers (\code{Effect}, \code{CI_low}, \code{CI_high},
#' \code{SE}, \code{p} are unformatted numerics; only \code{Translation} is a
#' formatted string), plus the group and raw term for traceability. For the
#' RIF engine, one row per (term, tau) with a \code{Tau} column and
#' \code{Translation} only at tau = 0.5.
#'
#' @inheritParams make_regtable_focused
#'
#' @return A data frame (Variable, Group, Term, Effect, CI_low, CI_high, SE, p,
#'   Translation; + Tau for RIF), or NULL on failure.
#'
#' @export
make_regtable_focused_df <- function(fit3, weather_terms, interaction_terms, label_fun = identity,
                                     engine = "fixest", is_logistic = FALSE, is_lpm = FALSE,
                                     is_log_outcome = TRUE, rif_grid = NULL,
                                     mf = NULL, scenarios_list = NULL, sd_x = NULL) {
  tryCatch(
    .t2_focused_rows(fit3, weather_terms, interaction_terms, label_fun = label_fun,
                     engine = engine, is_logistic = is_logistic, is_lpm = is_lpm,
                     is_log_outcome = is_log_outcome, rif_grid = rif_grid,
                     mf = mf, scenarios_list = scenarios_list, sd_x = sd_x),
    error = function(e) NULL
  )
}
