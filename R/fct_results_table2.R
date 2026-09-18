# Step 1 regression tables (reactable, guidelines §6) ----
# Results-first table of the weather + interaction coefficients of the full     #
# specification (3), with a per-row translation on the outcome's reporting      #
# scale; a compact three-specification comparison; and the full AER-style       #
# "all coefficients" view built from make_regtable_df(). All three render as    #
# `reactable` widgets with the client-side CSV button in the module UI.         #
#                                                                               #
# One tidy row derivation (.t2_focused_rows) feeds both the on-screen renderer  #
# and the export data frame, so the two cannot diverge. Translations mirror the #
# pct/pp/level formatting rules of step1_fmt_effect() / .s1_fmt_scaled() in     #
# fct_step1_headline.R without calling those private helpers.                   #


# Small helpers ----

.t2_esc <- function(x) gsub("([][{}().+*^$|?\\\\])", "\\\\\\1", x)

# Significance stars from a p-value (same thresholds as make_regtable()).
.t2_stars <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*",
          ifelse(p < 0.1, "\u2020", "")
        )
      )
    )
  )
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
    sprintf("%+.3f", est)
  )
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
    if (grepl(.t2_poly_pat(v), term)) {
      return(v)
    }
    if (grepl(paste0("^", e, "($|:|[\\[\\(])"), term)) {
      return(v)
    }
  }
  weather_terms[1]
}

# SD of one weather variable from the caller-supplied (possibly named) vector.
.t2_sd_for <- function(sd_x, var) {
  if (is.null(sd_x) || !length(sd_x)) {
    return(NULL)
  }
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
      weather_terms = weather_terms, label_fun = label_fun
    )
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
        as.numeric(sub(paste0("^", e, "[\\[\\(]([^,]+),.*"), "\\1", bins))
      )
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
  if (is.null(ct) || nrow(ct) == 0 || is.null(rownames(ct))) {
    return(NULL)
  }
  pv <- if (ncol(ct) >= 4) ct[, 4] else ct[, ncol(ct)]
  data.frame(
    term = rownames(ct),
    estimate = suppressWarnings(as.numeric(ct[, 1])),
    std.error = suppressWarnings(as.numeric(ct[, 2])),
    p.value = suppressWarnings(as.numeric(pv)),
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
  if (!length(weather_terms)) {
    return(NULL)
  }

  # Reporting scale (mirrors .s1_scale()).
  scale <- if (isTRUE(is_logistic) || isTRUE(is_lpm)) "pp" else if (isTRUE(is_log_outcome)) "pct" else "level"

  # scenarios_list: named list of step1_scenarios() results keyed by weather
  # variable (quadratic-inclusive +1 SD contrasts, one translation path).
  eta <- if (length(scenarios_list)) scenarios_list[[1]]$profile_eta else NULL
  if (is.null(eta) || !is.finite(eta)) eta <- NULL

  # Which weather variables enter with polynomial terms? Their interaction
  # slope differences are weather-level-dependent (see .translate).
  ct3_terms <- tryCatch(rownames(.fixest_coeftable(fit3)),
    error = function(e) character(0)
  )
  has_poly <- stats::setNames(
    vapply(
      weather_terms, function(v) any(grepl(.t2_poly_pat(v), ct3_terms)),
      logical(1)
    ),
    weather_terms
  )

  # Fallback: derive per-variable SDs from the estimation data (numeric only).
  if ((is.null(sd_x) || !length(sd_x)) && !is.null(mf$train_data)) {
    sd_x <- vapply(weather_terms, function(v) {
      col <- mf$train_data[[v]]
      if (is.null(col) || !is.numeric(col)) {
        return(NA_real_)
      }
      x <- col[is.finite(col)]
      if (length(x) > 1) stats::sd(x) else NA_real_
    }, numeric(1))
  }

  # Per-row translation on the reporting scale; NA renders as "-".
  .translate <- function(term, est) {
    v <- .t2_row_var(term, weather_terms)
    s <- .t2_sd_for(sd_x, v)
    is_inter <- grepl(":", term, fixed = TRUE)
    is_poly <- grepl("^I\\(", term)
    is_bin <- grepl(paste0("^", .t2_esc(v), "[\\[\\(]"), term)
    if (is_inter) {
      # Polynomial terms and polynomial-x-moderator interactions have
      # weather-level-dependent slope differences (the polynomial row is part
      # of the same contrast) - no single number; the moderated effect plot
      # carries them.
      if (is_poly || isTRUE(has_poly[v])) {
        return("\u2014")
      }
      if (isTRUE(is_logistic) || is.null(s) || !is.finite(est * s)) {
        return(NA_character_)
      }
      return(paste0("slope difference: ", .t2_scale_fmt(est * s, scale)))
    }
    if (is_poly) {
      return("\u2014")
    }
    if (is_bin) {
      if (isTRUE(is_logistic)) {
        if (is.null(eta) || !is.finite(est)) {
          return(NA_character_)
        }
        return(sprintf("%+.1f pp", 100 * (plogis(eta + est) - plogis(eta))))
      }
      if (!is.finite(est)) {
        return(NA_character_)
      }
      return(.t2_scale_fmt(est, scale))
    }
    # Main linear term: prefer the scenario translation - for polynomial
    # specifications the +1 SD contrast runs along the whole polynomial (the
    # same number the At a glance card shows), not beta * SD of the linear
    # term alone. Falls back to the single-coefficient contrast.
    scn <- scenarios_list[[v]]
    s1 <- if (!is.null(scn) && length(scn$scenarios)) {
      if (identical(engine, "rif")) {
        hit <- Filter(
          function(x) is.finite(x$tau) && abs(x$tau - 0.5) < 1e-9,
          scn$scenarios
        )
        if (length(hit)) hit[[1]] else NULL
      } else {
        scn$scenarios[[1]]
      }
    } else {
      NULL
    }
    if (!is.null(s1) && is.finite(s1$estimate) && is.finite(s1$se) && s1$se > 0) {
      fmt <- step1_fmt_effect(s1$estimate, s1$se, scale, ci = s1$ci)
      return(fmt$value)
    }
    if (isTRUE(is_logistic)) {
      if (is.null(eta) || is.null(s) || !is.finite(est * s)) {
        return(NA_character_)
      }
      return(sprintf("%+.1f pp", 100 * (plogis(eta + est * s) - plogis(eta))))
    }
    if (is.null(s) || !is.finite(est * s)) {
      return(NA_character_)
    }
    .t2_scale_fmt(est * s, scale)
  }

  # --- RIF: one row per (term, tau) of model 3 -------------------------------
  if (identical(engine, "rif") && !is.null(rif_grid)) {
    grid3 <- rif_grid[rif_grid$model == 3L, , drop = FALSE]
    if (!nrow(grid3)) {
      return(NULL)
    }
    wpat <- paste0("\\b(", paste(weather_terms, collapse = "|"), ")\\b")
    keep <- unique(as.character(grid3$term))
    keep <- keep[grepl(wpat, keep)]
    ord <- .t2_ordered_terms(keep, weather_terms, interaction_terms)
    terms_vec <- c(ord$main, ord$inter)
    if (!length(terms_vec)) {
      return(NULL)
    }
    taus <- sort(unique(grid3$tau))

    row_fn <- function(tm, group) {
      sub <- grid3[as.character(grid3$term) == tm, , drop = FALSE]
      if (!nrow(sub)) {
        return(NULL)
      }
      do.call(rbind, lapply(taus, function(tau) {
        r <- sub[abs(sub$tau - tau) < 1e-9, , drop = FALSE]
        if (!nrow(r)) {
          return(NULL)
        }
        est <- if ("estimate" %in% names(r)) suppressWarnings(as.numeric(r$estimate[1])) else NA_real_
        se <- if ("std.error" %in% names(r)) suppressWarnings(as.numeric(r$std.error[1])) else NA_real_
        pv <- if ("p.value" %in% names(r)) suppressWarnings(as.numeric(r$p.value[1])) else NA_real_
        lo <- if ("conf.low" %in% names(r)) suppressWarnings(as.numeric(r$conf.low[1])) else est - 1.96 * se
        hi <- if ("conf.high" %in% names(r)) suppressWarnings(as.numeric(r$conf.high[1])) else est + 1.96 * se
        data.frame(
          Variable = .t2_var_label(tm, weather_terms, label_fun),
          Group = group,
          Term = tm,
          Tau = tau,
          Effect = est,
          CI_low = lo,
          CI_high = hi,
          SE = se,
          p = pv,
          Translation = if (abs(tau - 0.5) < 1e-9) .translate(tm, est) else NA_character_,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }))
    }
    rows <- do.call(rbind, c(
      lapply(ord$main, row_fn, group = "Weather effects"),
      lapply(ord$inter, row_fn, group = "Interactions")
    ))
    if (is.null(rows) || !nrow(rows)) {
      return(NULL)
    }
    return(rows)
  }

  # --- fixest: one row per weather/interaction term of fit3 ------------------
  cf <- .t2_fit_coefs(fit3)
  if (is.null(cf)) {
    return(NULL)
  }
  keep <- tryCatch(weather_coef_names(fit3, weather_terms), error = function(e) character(0))
  keep <- as.character(keep)
  keep <- keep[keep %in% cf$term]
  if (!length(keep)) {
    return(NULL)
  }
  cf <- cf[match(keep, cf$term), , drop = FALSE]
  ord <- .t2_ordered_terms(cf$term, weather_terms, interaction_terms)

  row_fn <- function(tm, group) {
    r <- cf[cf$term == tm, , drop = FALSE]
    if (!nrow(r)) {
      return(NULL)
    }
    est <- r$estimate[1]
    se <- r$std.error[1]
    pv <- r$p.value[1]
    data.frame(
      Variable = .t2_var_label(tm, weather_terms, label_fun),
      Group = group,
      Term = tm,
      Effect = est,
      CI_low = est - 1.96 * se,
      CI_high = est + 1.96 * se,
      SE = se,
      p = pv,
      Translation = .translate(tm, est),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  rows <- do.call(rbind, c(
    lapply(ord$main, row_fn, group = "Weather effects"),
    lapply(ord$inter, row_fn, group = "Interactions")
  ))
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  rows
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
#' @inheritParams make_regtable_focused_df
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
    .t2_focused_rows(fit3, weather_terms, interaction_terms,
      label_fun = label_fun,
      engine = engine, is_logistic = is_logistic, is_lpm = is_lpm,
      is_log_outcome = is_log_outcome, rif_grid = rif_grid,
      mf = mf, scenarios_list = scenarios_list, sd_x = sd_x
    ),
    error = function(e) NULL
  )
}


# Reactable renderers (guidelines §6) ----

# Shared reactable plumbing for the Step 1 regression tables: compact rows,
# deliberate (unsortable) order, group separator rows carried in a hidden
# `.t2grp` column that rowClass turns into styled separator rows. Weather
# rows get the same tint the old .wise-table "hi" rows had.
.t2_reactable_row_class <- function() {
  htmlwidgets::JS(paste0(
    "function(rowInfo) {",
    "  var g = rowInfo.row['.t2grp'];",
    "  if (g === 'sep') return 't2-group-row';",
    "  if (g === 'wx') return 't2-wx-row';",
    "  return '';",
    "}"
  ))
}

.t2_reactable_cols <- function(df, left_cols = "Variable") {
  cols <- lapply(names(df), function(nm) {
    if (nm == ".t2grp" || nm == "Group") {
      return(reactable::colDef(show = FALSE))
    }
    reactable::colDef(
      align = if (nm %in% left_cols) "left" else "right",
      na = "",
      minWidth = if (nm %in% left_cols) 200 else 90
    )
  })
  stats::setNames(cols, names(df))
}

.t2_reactable_core <- function(df) {
  reactable::reactable(
    df,
    columns = .t2_reactable_cols(df),
    rowClass = .t2_reactable_row_class(),
    compact = TRUE,
    sortable = FALSE,
    searchable = FALSE,
    pagination = FALSE,
    highlight = FALSE,
    showSortIcon = FALSE
  )
}

# Inject one separator row per group (Group label in the Variable column,
# blanks elsewhere). Rows keep the caller's deliberate order. All display
# columns are preformatted strings, so a character matrix is a safe carrier.
.t2_with_group_rows <- function(df, group_col = "Group") {
  grp <- as.character(df[[group_col]])
  n <- nrow(df)
  sep_at <- c(1L, which(grp[-1] != grp[-n]) + 1L)
  total <- n + length(sep_at)
  m <- matrix("", nrow = total, ncol = ncol(df) + 1L,
              dimnames = list(NULL, c(names(df), ".t2grp")))
  out_i <- 0L
  di <- 0L
  pending <- sep_at
  while (out_i < total) {
    if (length(pending) && di < n && di + 1L == pending[1]) {
      out_i <- out_i + 1L
      m[out_i, 1L] <- grp[di + 1L]
      m[out_i, ".t2grp"] <- "sep"
      pending <- pending[-1L]
    }
    di <- di + 1L
    out_i <- out_i + 1L
    m[out_i, seq_len(ncol(df))] <- as.character(unlist(df[di, , drop = TRUE], use.names = FALSE))
    m[out_i, ".t2grp"] <- if (identical(grp[di], "Weather effects")) "wx" else ""
  }
  as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE)
}

# Small note-only table for the empty/error states (mirrors the other §6
# tables' "Note" rows).
.t2_reactable_note <- function(msg) {
  reactable::reactable(
    data.frame(Note = msg),
    columns = list(Note = reactable::colDef(align = "left")),
    compact = TRUE,
    sortable = FALSE,
    searchable = FALSE,
    pagination = FALSE
  )
}

# Format helpers shared with the old HTML renderer (kept identical), but
# vectorized over whole columns.
.t2_fmt_num3 <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x), formatC(x, format = "f", digits = 3), "")
}
.t2_fmt_p <- function(p) {
  p <- as.numeric(p)
  ifelse(
    is.finite(p),
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)),
    ""
  )
}
.t2_fmt_ci <- function(lo, hi) paste0(.t2_fmt_num3(lo), " \u2013 ", .t2_fmt_num3(hi))

#' Reactable focused regression table for the Step 1 results tab (guidelines §6)
#'
#' Renders the shared `.t2_focused_rows()` tidy rows as a reactable widget:
#' one row per weather/interaction term with estimate + significance stars,
#' 95% CI, SE, p-value and the translated per-+1-SD effect. Weather rows are
#' tinted; interactions follow under a separator row. For the RIF engine the
#' tau 0.1-0.9 grid is pivoted wide with a final translated-effect column.
#'
#' @param rows   Tidy rows from `.t2_focused_rows()` / `make_regtable_focused_df()`.
#' @param engine,is_logistic,is_lpm  Only affect the translation column header
#'   (and the RIF pivot), mirroring the old `.wise-table` renderer.
#'
#' @return A `reactable` widget, or a note widget when `rows` is empty.
#' @noRd
make_regtable_focused_reactable <- function(rows, engine = "fixest",
                                             is_logistic = FALSE, is_lpm = FALSE) {
  if (is.null(rows) || !nrow(rows)) {
    return(.t2_reactable_note("Focused estimates unavailable for this fit."))
  }

  if (identical(engine, "rif") && "Tau" %in% names(rows)) {
    # Pivot: rows = terms, columns = tau quantiles + one translated column.
    taus <- sort(unique(rows$Tau))
    has_bins <- any(grepl("[\\[\\(]", rows$Term))
    trans_nm <- if (has_bins) {
      "Translated effect (\u03c4 = 0.5)"
    } else {
      "Per +1 SD (\u03c4 = 0.5)"
    }
    tau_nms <- sprintf("\u03c4 = %.1f", taus)
    col_nms <- c("Variable", tau_nms, trans_nm, ".t2grp")
    body <- list()
    prev <- NA_character_
    for (tm in unique(rows$Term)) {
      tr <- rows[rows$Term == tm, , drop = FALSE]
      group <- tr$Group[1]
      if (!identical(group, prev)) {
        sep <- rep("", length(col_nms))
        sep[1] <- group
        sep[length(sep)] <- "sep"
        body <- c(body, list(sep))
        prev <- group
      }
      cells <- vapply(taus, function(t) {
        r <- tr[abs(tr$Tau - t) < 1e-9, , drop = FALSE]
        if (!nrow(r)) return("")
        paste0(.t2_fmt_num3(r$Effect[1]), .t2_stars(r$p[1]), " (", .t2_fmt_num3(r$SE[1]), ")")
      }, character(1))
      r50 <- tr[abs(tr$Tau - 0.5) < 1e-9, , drop = FALSE]
      trans <- if (nrow(r50) && !is.na(r50$Translation[1]) && nzchar(r50$Translation[1])) {
        r50$Translation[1]
      } else {
        "-"
      }
      row <- c(tr$Variable[1], cells, trans,
        if (identical(group, "Weather effects")) "wx" else ""
      )
      body <- c(body, list(row))
    }
    m <- do.call(rbind, body)
    colnames(m) <- col_nms
    out <- as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE)
    return(.t2_reactable_core(out))
  }

  # fixest: one row per term, group separator rows between groups.
  has_bins <- any(grepl("[\\[\\(]", rows$Term))
  trans_nm <- if (has_bins) {
    "Translated effect"
  } else if (isTRUE(is_logistic)) {
    "pp effect per +1 SD"
  } else if (isTRUE(is_lpm)) {
    "pp per +1 SD"
  } else {
    "Per +1 SD"
  }
  df <- data.frame(
    Variable = rows$Variable,
    Effect = paste0(.t2_fmt_num3(rows$Effect), .t2_stars(rows$p)),
    `95% CI` = .t2_fmt_ci(rows$CI_low, rows$CI_high),
    SE = .t2_fmt_num3(rows$SE),
    p = .t2_fmt_p(rows$p),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  df[[trans_nm]] <- ifelse(!is.na(rows$Translation) & nzchar(rows$Translation), rows$Translation, "-")
  df$Variable <- as.character(df$Variable)
  df$Group <- as.character(rows$Group)
  .t2_reactable_core(.t2_with_group_rows(df))
}

#' Reactable specification-comparison table for the Step 1 results tab
#'
#' Compares the weather coefficients of the three progressive specifications -
#' Variable | (1) No FE | (2) FE | (3) FE + Controls - with the same grouping,
#' ordering, labels and estimate + stars / (SE) formatting as the focused
#' table. Cells are blank when a term is absent from a specification. Returns
#' NULL for the RIF engine (the caller hides the panel).
#'
#' @inheritParams make_regtable_specs
#'
#' @return A `reactable` widget, or NULL on the RIF engine / when no rows can
#'   be built.
#' @noRd
make_regtable_specs_reactable <- function(fit1, fit2, fit3, weather_terms,
                                           interaction_terms, label_fun = identity,
                                           engine = "fixest", rif_grid = NULL,
                                           has_controls = TRUE) {
  if (identical(engine, "rif")) {
    return(NULL)
  }
  tryCatch(
    {
      weather_terms <- if (is.null(weather_terms)) character(0) else as.character(weather_terms)
      weather_terms <- weather_terms[nzchar(weather_terms)]
      if (!length(weather_terms)) {
        return(NULL)
      }
      lab3 <- if (isTRUE(has_controls)) "(3) FE + Controls" else "(3) FE (no controls selected)"
      specs <- list("(1) No FE" = fit1, "(2) FE" = fit2)
      specs[[lab3]] <- fit3
      coefs <- lapply(specs, .t2_fit_coefs)
      cf3 <- coefs[[lab3]]
      if (is.null(cf3)) {
        return(NULL)
      }
      keep <- tryCatch(weather_coef_names(fit3, weather_terms), error = function(e) character(0))
      keep <- as.character(keep)
      keep <- keep[keep %in% cf3$term]
      if (!length(keep)) {
        return(NULL)
      }
      ord <- .t2_ordered_terms(keep, weather_terms, interaction_terms)
      terms_vec <- c(ord$main, ord$inter)
      if (!length(terms_vec)) {
        return(NULL)
      }
      groups <- ifelse(grepl(":", terms_vec, fixed = TRUE), "Interactions", "Weather effects")

      cell <- function(cf, tm) {
        r <- if (is.null(cf)) NULL else cf[cf$term == tm, , drop = FALSE]
        if (is.null(r) || !nrow(r)) {
          return("")
        }
        paste0(
          if (is.finite(r$estimate[1])) formatC(r$estimate[1], format = "f", digits = 3) else "",
          .t2_stars(r$p.value[1]),
          if (is.finite(r$std.error[1])) paste0(" (", formatC(r$std.error[1], format = "f", digits = 3), ")") else ""
        )
      }
      spec_cols <- lapply(seq_along(specs), function(j) {
        vapply(terms_vec, function(tm) cell(coefs[[j]], tm), character(1))
      })
      df <- data.frame(
        Variable = vapply(terms_vec, .t2_var_label, character(1),
          weather_terms = weather_terms, label_fun = label_fun
        ),
        spec_cols,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      names(df) <- c("Variable", names(specs))
      df$Group <- as.character(groups)
      .t2_reactable_core(.t2_with_group_rows(df))
    },
    error = function(e) NULL
  )
}

#' Reactable view of the full AER-style coefficient table (guidelines §6)
#'
#' Renders the long tidy data frame from `make_regtable_df()` (one row per
#' specification and term, both engines) as a client-side sortable/searchable
#' reactable - the expandable "all coefficients" view.
#'
#' @param df Tidy rows from `make_regtable_df()`, or NULL.
#'
#' @return A `reactable` widget, or a note widget when `df` is empty.
#' @noRd
make_regtable_df_reactable <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(.t2_reactable_note("No coefficients available for this fit."))
  }
  cols <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (is.numeric(x) && identical(nm, "Observations")) {
      reactable::colDef(format = reactable::colFormat(digits = 0), na = "")
    } else if (is.numeric(x)) {
      reactable::colDef(format = reactable::colFormat(digits = 3), na = "")
    } else {
      reactable::colDef(na = "", minWidth = if (identical(nm, "Variable")) 200 else 90)
    }
  })
  names(cols) <- names(df)
  reactable::reactable(
    df,
    columns = cols,
    compact = TRUE,
    sortable = TRUE,
    searchable = TRUE,
    defaultPageSize = 15,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(15, 25, 50, 100)
  )
}
