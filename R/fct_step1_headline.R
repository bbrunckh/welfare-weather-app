# Step 1 headline helpers ----
# Pure functions: a fit_model() result plus its fit-time snapshot -> card       #
# values. One translation path for every configuration, so cards, figures and   #
# the table cannot diverge:                                                     #
#   outcome   : log-continuous / binary-LPM / binary-logit                      #
#   engine    : fixest / rif / tree-based (degraded cards)                      #
#   weather   : continuous / polynomial / binned (omitted reference bin)        #
#   interact. : none / moderator (pairwise or saturated)                        #
# All values are computed from the fitted run only (fit snapshot, INT-05).      #


# Small helpers ----

.s1_engine <- function(mf) {
  e <- tolower(mf$engine %||% "fixest")
  if (e %in% c("ranger", "xgboost")) "ml" else e
}

.s1_esc <- function(x) gsub("([][{}().+*^$|?\\\\])", "\\\\\\1", x)

# Scale of the reported effect: "pct" (% change), "pp" (percentage points),
# or "level" (raw outcome units).
.s1_scale <- function(mf, snap) {
  if (is_logistic_fit(mf)) {
    return("pp")
  }
  if (identical(tolower(as.character(snap$outcome$type[1])), "logical")) {
    return("pp")
  }
  if (identical(as.character(snap$outcome$transform[1]), "log")) {
    return("pct")
  }
  "level"
}

.s1_weather_label <- function(snap, var, label_fun = identity) {
  lab <- NULL
  w <- snap$weather
  if (!is.null(w) && !is.null(w$name) && var %in% as.character(w$name)) {
    lab <- as.character(w$label[w$name == var][1])
  }
  if (is.null(lab) || is.na(lab) || !nzchar(lab)) {
    lab <- tryCatch(label_fun(var), error = function(e) var)
  }
  wise_label_short(lab)
}

.s1_cont_binned <- function(snap, var) {
  w <- snap$weather
  if (!is.null(w) && !is.null(w$name) && var %in% as.character(w$name)) {
    cb <- as.character(w$cont_binned[w$name == var][1])
    if (!is.na(cb) && nzchar(cb)) {
      return(cb)
    }
  }
  "Continuous"
}

.s1_x_stats <- function(td, var) {
  if (is.null(td) || !var %in% names(td)) {
    return(NULL)
  }
  x <- suppressWarnings(as.numeric(td[[var]]))
  x <- x[is.finite(x)]
  if (length(x) < 10) {
    return(NULL)
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(m) || !is.finite(s) || s <= 0) {
    return(NULL)
  }
  list(mean = m, sd = s)
}

# First moderator variable interacting with `var`, from the fit snapshot.
.s1_modx_var <- function(mf, var) {
  its <- mf$interaction_terms %||% character(0)
  if (!length(its)) {
    return(NULL)
  }
  pat <- paste0("(\\b|^)", .s1_esc(var), "(\\b|\\[|\\^)")
  mt <- its[grepl(pat, its)]
  if (!length(mt)) {
    return(NULL)
  }
  parts <- strsplit(mt[1], ":", fixed = TRUE)[[1]]
  cand <- parts[!grepl(pat, parts)]
  if (!length(cand) || is.na(cand[1]) || !nzchar(cand[1])) {
    return(NULL)
  }
  cand[1]
}

.s1_modx_label <- function(mf, snap, modx_var, label_fun = identity) {
  lab <- tryCatch(label_fun(modx_var), error = function(e) modx_var)
  if (is.null(lab) || is.na(lab) || !nzchar(lab)) lab <- modx_var
  lab
}

# Moderator evaluation values: all levels when <= 5 unique values (0/1
# moderators), otherwise mean +/- 1 SD. Returns list(value, label).
.s1_modx_levels <- function(td, modx_var) {
  if (is.null(modx_var) || is.null(td) || !modx_var %in% names(td)) {
    return(list(list(value = 0, label = NULL)))
  }
  mx <- suppressWarnings(as.numeric(td[[modx_var]]))
  mx <- mx[is.finite(mx)]
  if (!length(mx)) {
    return(list(list(value = 0, label = NULL)))
  }
  u <- sort(unique(mx))
  if (length(u) <= 5) {
    # Binary 0/1 moderators get yes/no level labels instead of raw codes.
    is_bin <- length(u) == 2 && all(u %in% c(0, 1))
    lapply(u, function(v) {
      list(
        value = v,
        label = if (is_bin) (if (v == 1) "yes" else "no") else as.character(v)
      )
    })
  } else {
    m <- mean(mx)
    s <- stats::sd(mx)
    lapply(c(m - s, m, m + s), function(v) list(value = v, label = sprintf("%.2f", v)))
  }
}

.s1_bin_lower <- function(bin_cols, pred_var) {
  s <- sub(paste0("^", .s1_esc(pred_var), "[\\[\\(]"), "", bin_cols)
  suppressWarnings(as.numeric(sub("^([^,]+),.*", "\\1", s)))
}

.s1_bin_contrast_label <- function(snap, var, top_col) {
  inner <- sub(paste0("^", .s1_esc(var), "[\\[\\(]"), "", top_col)
  if (endsWith(inner, "]") || endsWith(inner, ")")) {
    inner <- substr(inner, 1, nchar(inner) - 1)
  }
  parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
  rng <- if (length(parts) == 2) {
    paste0(trimws(parts[1]), "\u2013", trimws(parts[2]))
  } else {
    inner
  }
  ref <- tryCatch(get_first_bin_label(snap$survey_weather, var),
    error = function(e) NA_character_
  )
  paste0(
    rng, " bin vs reference bin",
    if (!is.na(ref) && nzchar(ref)) paste0(" (", ref, ")") else ""
  )
}

# Format an estimate/SE pair in the outcome's reporting scale.
.s1_fmt_scaled <- function(est, se, scale, digits = 1, ci = NULL) {
  f <- switch(scale,
    pct = function(v) sprintf("%+.*f%%", digits, 100 * (exp(v) - 1)),
    pp  = function(v) sprintf("%+.*f pp", digits, 100 * v),
    function(v) sprintf("%+.*f", digits, v)
  )
  lo <- est - 1.96 * se
  hi <- est + 1.96 * se
  # Precomputed transformed interval (logit pp): carried by the scenario
  # instead of the symmetric delta-method interval, which understates
  # uncertainty on a nonlinear scale.
  if (length(ci) == 2 && all(is.finite(ci))) {
    lo <- min(ci)
    hi <- max(ci)
  }
  list(
    value = f(est), lo_txt = f(lo), hi_txt = f(hi),
    spans_zero = (lo < 0 && hi > 0)
  )
}


# Design-matrix rows ----
# counterfactual point. Mirrors the manual-prediction logic of                   #
# make_weather_effect_plot() so cards and figures share one construction.        #

# Polynomial term matcher: fixest double-wraps I() terms in coefficient
# names ("I(I(x^2))"); accept the plain "I(x^2)" spelling too.
.s1_is_poly_term <- function(term, var, k) {
  grepl(paste0("^I\\((?:I\\()?", .s1_esc(var), "\\^", k, "\\)\\)?$"), term)
}

.s1_poly_col <- function(cols, var, k) {
  hit <- grep(paste0("^I\\((?:I\\()?", .s1_esc(var), "\\^", k, "\\)\\)?$"),
    cols,
    value = TRUE
  )
  if (length(hit)) hit[1] else NA_character_
}

.s1_part_value <- function(part, pred_var, x, active_bin, row) {
  if (part == pred_var) {
    return(x)
  }
  if (.s1_is_poly_term(part, pred_var, 2)) {
    return(x^2)
  }
  if (.s1_is_poly_term(part, pred_var, 3)) {
    return(x^3)
  }
  if (grepl(paste0("^", .s1_esc(pred_var), "[\\[\\(]"), part)) {
    return(as.numeric(identical(part, active_bin)))
  }
  if (part %in% names(row)) {
    return(row[[part]])
  }
  NA_real_
}

.s1_design_row <- function(mm, pred_var, x = 0, modx_var = NULL, modx_value = 0,
                           active_bin = NULL, base_row = NULL) {
  cols <- names(mm)
  if (!is.null(base_row)) {
    # Counterfactual profile of a real household (used for binary outcomes,
    # where an average-covariate hybrid can sit at an unrealistic baseline
    # probability). Unknown columns default to 0.
    row <- rep(0, length(cols))
    names(row) <- cols
    common <- intersect(cols, names(base_row))
    row[common] <- base_row[common]
  } else {
    row <- vapply(seq_along(mm), function(j) {
      col <- mm[[j]]
      if (is.numeric(col)) {
        m <- mean(col[is.finite(col)])
        if (is.finite(m)) m else 0
      } else {
        tb <- sort(table(col), decreasing = TRUE)
        if (!length(tb)) {
          return(0)
        }
        v <- suppressWarnings(as.numeric(names(tb)[1]))
        if (is.finite(v)) v else 0
      }
    }, numeric(1))
    names(row) <- cols
  }

  if ("(Intercept)" %in% cols) row[["(Intercept)"]] <- 1

  p_esc <- .s1_esc(pred_var)
  if (pred_var %in% cols) row[[pred_var]] <- x
  for (k in 2:3) {
    nm <- .s1_poly_col(cols, pred_var, k)
    if (!is.na(nm)) row[[nm]] <- x^k
  }

  bin_cols <- grep(paste0("^", p_esc, "[\\[\\(]"), cols, value = TRUE)
  if (length(bin_cols)) {
    row[bin_cols] <- 0
    if (!is.null(active_bin) && active_bin %in% bin_cols) row[[active_bin]] <- 1
  }

  if (!is.null(modx_var) && nzchar(modx_var)) {
    m_esc <- .s1_esc(modx_var)
    mod_cols <- grep(paste0("^", m_esc), cols, value = TRUE)
    if (modx_var %in% cols) {
      row[[modx_var]] <- modx_value
      sib <- setdiff(mod_cols, modx_var)
      if (length(sib)) row[sib] <- 0
    } else if (length(mod_cols)) {
      hit <- grep(paste0("^", m_esc, modx_value, "$"), mod_cols, value = TRUE)
      row[mod_cols] <- 0
      if (length(hit)) row[[hit[1]]] <- 1
    }
  }

  for (nm in grep(":", cols, value = TRUE, fixed = TRUE)) {
    parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
    vals <- vapply(parts, .s1_part_value, numeric(1),
      pred_var = pred_var,
      x = x, active_bin = active_bin, row = row
    )
    if (!anyNA(vals)) row[[nm]] <- prod(vals)
  }
  row
}


# Effect scenarios ----

# fixest (feols / feglm): one contrast per moderator level. Each scenario
# carries the model-scale contrast (w = design row difference), its SE from the
# full vcov, and - for logit - the pieces needed to convert to probabilities.
.s1_fixest_scenarios <- function(mf, snap, var) {
  tryCatch(
    {
      fit <- extract_native_fit(mf$fit3, mf$engine)
      mm <- resolve_model_matrix(fit)
      if (is.null(mm)) {
        return(NULL)
      }
      beta <- stats::coef(fit)
      V <- .fixest_vcov(fit)
      if (is.null(beta) || is.null(V)) {
        return(NULL)
      }

      td <- mf$train_data
      binned <- identical(.s1_cont_binned(snap, var), "Binned")
      xd <- .s1_x_stats(td, var)
      if (!binned && is.null(xd)) {
        return(NULL)
      }

      modx_var <- .s1_modx_var(mf, var)
      modx_vals <- .s1_modx_levels(td, modx_var)

      bin_cols <- grep(paste0("^", .s1_esc(var), "[\\[\\(]"), names(mm), value = TRUE)
      top_bin <- if (length(bin_cols)) bin_cols[which.max(.s1_bin_lower(bin_cols, var))] else NULL
      if (binned && is.null(top_bin)) {
        return(NULL)
      }

      contrast_label <- if (binned) {
        .s1_bin_contrast_label(snap, var, top_bin)
      } else {
        paste0("+1 SD of ", .s1_weather_label(snap, var))
      }

      is_logit <- is_logistic_fit(mf)

      # Binary outcomes are evaluated at a real household whose fitted risk is
      # the sample median, not at the average-covariate hybrid: with strongly
      # separating logit fits the hybrid profile can sit at a probability of
      # ~0, where the pp effect saturates to zero.
      is_binary <- identical(tolower(as.character(snap$outcome$type[1])), "logical")
      profile_note <- "sample-average household"
      base_row <- NULL
      xb_lo_binary <- NULL
      if (is_binary) {
        f <- tryCatch(stats::fitted(fit), error = function(e) NULL)
        if (!is.null(f) && length(f) == nrow(mm) && any(is.finite(f))) {
          i_med <- which.min(abs(f - stats::median(f, na.rm = TRUE)))
          base_row <- stats::setNames(as.numeric(mm[i_med, ]), names(mm))
          profile_note <- "median-risk household"
          # Baseline linear predictor of that household, fixed effects included:
          # fitted = plogis(X beta + FE_i), so qlogis(fitted) is exact, and the
          # fixed effects cancel in the +1 SD contrast (same household).
          if (is_logit && is.finite(f[i_med]) && f[i_med] > 0 && f[i_med] < 1) {
            xb_lo_binary <- stats::qlogis(f[i_med])
          }
        }
      }
      # Counterfactual baselines for moderator levels: shift only the moderator
      # (and its interactions) away from the reference household's own values,
      # keeping the household's weather level.
      modx_shift_beta <- function(mv) {
        if (is.null(xb_lo_binary)) {
          return(NULL)
        }
        r_hh <- if (!is.null(base_row) && var %in% names(base_row)) base_row[[var]] else 0
        mv_hh <- if (!is.null(modx_var) && !is.null(base_row) &&
          modx_var %in% names(base_row)) {
          base_row[[modx_var]]
        } else {
          0
        }
        row_mv <- .s1_design_row(mm, var,
          x = r_hh, modx_var = modx_var,
          modx_value = mv, base_row = base_row
        )
        row_hh <- .s1_design_row(mm, var,
          x = r_hh, modx_var = modx_var,
          modx_value = mv_hh, base_row = base_row
        )
        d <- row_mv - row_hh
        cc <- intersect(names(d), names(beta))
        sum(d[cc] * beta[cc])
      }

      sc <- lapply(modx_vals, function(mv) {
        if (binned) {
          r_hi <- .s1_design_row(mm, var,
            x = 0, modx_var = modx_var,
            modx_value = mv$value, active_bin = top_bin,
            base_row = base_row
          )
          r_lo <- .s1_design_row(mm, var,
            x = 0, modx_var = modx_var,
            modx_value = mv$value, active_bin = NULL,
            base_row = base_row
          )
        } else {
          r_hi <- .s1_design_row(mm, var,
            x = xd$mean + xd$sd, modx_var = modx_var,
            modx_value = mv$value, base_row = base_row
          )
          r_lo <- .s1_design_row(mm, var,
            x = xd$mean, modx_var = modx_var,
            modx_value = mv$value, base_row = base_row
          )
        }
        w <- r_hi - r_lo
        common <- intersect(names(w), names(beta))
        if (!length(common)) {
          return(NULL)
        }
        wc <- w[common]
        est <- sum(wc * beta[common])
        se <- tryCatch(
          sqrt(max(drop(t(wc) %*% V[common, common, drop = FALSE] %*% wc), 0)),
          error = function(e) NA_real_
        )
        list(
          value = mv$value, label = mv$label,
          estimate = est, se = se,
          xb_lo = if (!is.null(xb_lo_binary)) {
            xb_lo_binary + (modx_shift_beta(mv$value) %||% 0)
          } else {
            sum(r_lo[common] * beta[common])
          },
          w = wc, cols = common, V = V, is_logit = is_logit
        )
      })
      sc <- Filter(Negate(is.null), sc)
      if (!length(sc)) {
        return(NULL)
      }
      attr(sc, "contrast_label") <- contrast_label
      attr(sc, "profile_note") <- profile_note
      sc
    },
    error = function(e) NULL
  )
}

# Convert a fixest scenario to probability-scale (pp) for logistic fits.
.s1_to_pp <- function(s) {
  if (is.null(s) || !isTRUE(s$is_logit) || !is.finite(s$estimate)) {
    return(s)
  }
  p_lo <- stats::plogis(s$xb_lo)
  p_hi <- stats::plogis(s$xb_lo + s$estimate)
  grad <- p_hi * (1 - p_hi) * s$w - p_lo * (1 - p_lo) * s$w
  se <- tryCatch(
    sqrt(max(drop(t(grad) %*% s$V[s$cols, s$cols, drop = FALSE] %*% grad), 0)),
    error = function(e) NA_real_
  )
  # Confidence interval: transform the link-scale contrast endpoints through
  # the same profile map. plogis is monotone, so the interval inherits the
  # link interval's containment of zero - a symmetric delta-method pp interval
  # can exclude zero (or be far too narrow) even when the coefficient CI
  # includes it, because the logistic curve saturates away from p = 0.5.
  ci <- NULL
  se_link <- tryCatch(
    sqrt(max(drop(t(s$w) %*% s$V[s$cols, s$cols, drop = FALSE] %*% s$w), 0)),
    error = function(e) NA_real_
  )
  if (is.finite(se_link) && se_link > 0) {
    pp_at <- function(v) stats::plogis(s$xb_lo + v) - p_lo
    ci <- sort(c(
      pp_at(s$estimate - 1.96 * se_link),
      pp_at(s$estimate + 1.96 * se_link)
    ))
  }
  list(
    value = s$value, label = s$label, estimate = p_hi - p_lo, se = se,
    ci = ci, w = s$w, cols = s$cols, V = s$V, is_logit = TRUE
  )
}

# RIF: term weights for the same contrast, evaluated analytically from the
# term names (main / polynomial / bin / interaction). SEs combine per-tau
# standard errors without cross-term covariance - the same approximation the
# RIF effect plot discloses in its caption.
.s1_rif_scenarios <- function(mf, snap, var, taus = c(0.1, 0.5, 0.9)) {
  tryCatch(
    {
      g3 <- mf$rif_grid
      if (is.null(g3)) {
        return(NULL)
      }
      g3 <- g3[g3$model == 3L, , drop = FALSE]
      if (!nrow(g3)) {
        return(NULL)
      }

      pat <- paste0("(\\b|^)", .s1_esc(var), "(\\b|\\[|\\^)")
      tt <- unique(as.character(g3$term[grepl(pat, g3$term)]))
      if (!length(tt)) {
        return(NULL)
      }

      td <- mf$train_data
      binned <- identical(.s1_cont_binned(snap, var), "Binned")
      xd <- .s1_x_stats(td, var)
      modx_var <- .s1_modx_var(mf, var)
      modx_vals <- .s1_modx_levels(td, modx_var)

      bin_cols <- tt[grepl(paste0("^", .s1_esc(var), "[\\[\\(]"), tt)]
      top_bin <- if (length(bin_cols)) bin_cols[which.max(.s1_bin_lower(bin_cols, var))] else NULL
      if (binned && is.null(top_bin)) {
        return(NULL)
      }
      if (!binned && is.null(xd)) {
        return(NULL)
      }

      contrast_label <- if (binned) {
        .s1_bin_contrast_label(snap, var, top_bin)
      } else {
        paste0("+1 SD of ", .s1_weather_label(snap, var))
      }

      term_value <- function(t, x, mv, bin) {
        parts <- strsplit(t, ":", fixed = TRUE)[[1]]
        pv <- vapply(parts, function(p) {
          if (p == var) {
            return(x)
          }
          if (.s1_is_poly_term(p, var, 2)) {
            return(x^2)
          }
          if (.s1_is_poly_term(p, var, 3)) {
            return(x^3)
          }
          if (grepl(paste0("^", .s1_esc(var), "[\\[\\(]"), p)) {
            return(as.numeric(identical(p, bin)))
          }
          if (!is.null(modx_var) && p == modx_var) {
            return(mv)
          }
          NA_real_
        }, numeric(1))
        if (anyNA(pv)) NA_real_ else prod(pv)
      }

      out <- lapply(modx_vals, function(mv) {
        lapply(taus, function(tau) {
          gt <- g3[abs(g3$tau - tau) < 1e-9, c("term", "estimate", "std.error"),
            drop = FALSE
          ]
          if (!nrow(gt)) {
            return(NULL)
          }
          x_hi <- if (binned) 0 else xd$mean + xd$sd
          x_lo <- if (binned) 0 else xd$mean
          w <- vapply(tt, function(t) {
            vh <- term_value(t, x_hi, mv$value, if (binned) top_bin else NULL)
            vl <- term_value(t, x_lo, mv$value, NULL)
            if (anyNA(c(vh, vl))) NA_real_ else vh - vl
          }, numeric(1))
          ok <- !is.na(w)
          if (!any(ok)) {
            return(NULL)
          }
          m <- match(tt[ok], gt$term)
          keep <- !is.na(m)
          if (!any(keep)) {
            return(NULL)
          }
          wt <- w[ok][keep]
          list(
            tau = tau, value = mv$value, label = mv$label,
            estimate = sum(wt * gt$estimate[m[keep]]),
            se = sqrt(sum((wt * gt$std.error[m[keep]])^2))
          )
        })
      })
      out <- lapply(out, function(x) Filter(Negate(is.null), x))
      if (!length(out) || !length(out[[1]])) {
        return(NULL)
      }
      attr(out, "contrast_label") <- contrast_label
      out
    },
    error = function(e) NULL
  )
}

.s1_scen_at <- function(scens, tau) {
  if (is.null(scens) || !length(scens)) {
    return(NULL)
  }
  first <- scens[[1]]
  if (!is.list(first) || !length(first)) {
    return(NULL)
  }
  hit <- Filter(
    function(s) !is.null(s) && is.finite(s$tau) && abs(s$tau - tau) < 1e-9,
    first
  )
  if (!length(hit)) {
    return(NULL)
  }
  hit[[1]]
}


# RIF heterogeneity screen ----

# Approximate Wald screen for differences in the weather effect across the
# quantile grid (tau = 0.1 ... 0.9). Restacks the RIF responses (already stored
# on fit_model()'s train data) and fits one feols with tau-specific weather
# columns, keeping control and fixed-effect slopes tau-invariant. Household
# clustering captures the cross-quantile covariance, so the test is internally
# consistent with the stacked fit; the tau-invariant controls make it an
# approximation of the exact quantile-by-quantile comparison. Returns a p-value
# or NULL when the screen cannot be run (falls back to no p-value on the card).
step1_rif_heterogeneity_p <- function(mf, snap, var) {
  tryCatch(
    {
      if (!identical(.s1_engine(mf), "rif")) {
        return(NULL)
      }
      td <- mf$train_data
      if (is.null(td)) {
        return(NULL)
      }
      rif_cols <- grep("^rif_[0-9]+$", names(td), value = TRUE)
      K <- length(rif_cols)
      if (K < 3) {
        return(NULL)
      }

      med <- extract_rif_median(mf$fit3, "rif")
      mm <- resolve_model_matrix(med)
      if (is.null(mm)) {
        return(NULL)
      }
      pat <- paste0("(\\b|^)", .s1_esc(var), "(\\b|\\[|\\^)")
      vcols <- grep(pat, names(mm), value = TRUE)
      if (!length(vcols) || length(vcols) > 8) {
        return(NULL)
      }

      idx <- med$obs
      if (is.null(idx) || !length(idx)) idx <- seq_len(nrow(td))
      if (length(idx) != nrow(mm)) {
        return(NULL)
      }
      tds <- td[idx, , drop = FALSE]
      N <- nrow(tds)
      if (N * K > 250000) {
        return(NULL)
      }

      covs <- unique(unlist(snap$model[c(
        "hh_covariates", "ind_covariates",
        "area_covariates", "firm_covariates"
      )]))
      covs <- unique(c(covs, .s1_modx_var(mf, var)))
      covs <- covs[!is.na(covs) & nzchar(covs) & covs %in% names(tds)]
      fe <- (mf$fe_terms %||% character(0))
      fe <- fe[!is.na(fe) & nzchar(fe) & fe %in% names(tds)]

      # Keep only the columns the stacked fit needs; configuration columns that
      # ride along on train_data (e.g. the polynomial list-column) would break
      # fixest's model frame.
      keep <- unique(c(covs, fe))
      keep <- keep[vapply(keep, function(v) !is.list(tds[[v]]), logical(1))]
      base_cols <- tds[, intersect(keep, names(tds)), drop = FALSE]

      vnames <- paste0(
        ".v", rep(seq_len(K), each = length(vcols)), "_",
        rep(seq_along(vcols), times = K)
      )
      M <- as.matrix(mm[, vcols, drop = FALSE])
      stack <- do.call(rbind, lapply(seq_len(K), function(k) {
        d <- base_cols
        d$.rif_y <- tds[[rif_cols[k]]]
        d$.tau <- k
        for (v in vnames) d[[v]] <- 0
        for (j in seq_along(vcols)) d[[paste0(".v", k, "_", j)]] <- M[, j]
        d
      }))
      stack$.tau_f <- factor(stack$.tau)

      rhs <- c(".tau_f", covs, vnames)
      fml <- stats::as.formula(paste(
        "`.rif_y` ~ 0 +", paste(rhs, collapse = " + "),
        if (length(fe)) paste0(" | ", paste(fe, collapse = " + ")) else ""
      ))
      fit <- suppressWarnings(fixest::feols(fml, data = stack, warn = FALSE))
      b <- stats::coef(fit)
      V <- .fixest_vcov(fit)
      if (is.null(b) || is.null(V)) {
        return(NULL)
      }

      # H0: the tau-specific effect of each design column is flat across taus.
      R <- NULL
      for (j in seq_along(vcols)) {
        nms <- paste0(".v", seq_len(K), "_", j)
        if (!all(nms %in% names(b))) next
        Rj <- matrix(0, nrow = K - 1L, ncol = length(b), dimnames = list(NULL, names(b)))
        for (k in 2:K) {
          Rj[k - 1L, nms[k]] <- 1
          Rj[k - 1L, nms[1]] <- -1
        }
        R <- rbind(R, Rj)
      }
      if (is.null(R) || !nrow(R)) {
        return(NULL)
      }
      Rb <- as.numeric(R %*% b[colnames(R)])
      Vsub <- V[colnames(R), colnames(R), drop = FALSE]
      stat <- tryCatch(
        as.numeric(t(Rb) %*% solve(R %*% Vsub %*% t(R)) %*% Rb),
        error = function(e) NA_real_
      )
      if (!is.finite(stat) || stat < 0) {
        return(NULL)
      }
      stats::pchisq(stat, df = nrow(R), lower.tail = FALSE)
    },
    error = function(e) NULL
  )
}


# Public scenario / formatting contract ----

#' Effect scenarios for one weather variable (public wrapper)
#'
#' Returns the per-moderator-level contrasts behind the headline cards, on the
#' outcome's reporting scale (logit scenarios already converted to probability
#' differences). Consumed by the focused regression table and the effect-plot
#' annotation so figures, cards and table share one set of numbers.
#'
#' @param mf   Named list returned by `fit_model()` (with `.snap` attached).
#' @param snap The fit-time snapshot (`mf$.snap`).
#' @param var  Weather variable name.
#'
#' @return List with elements `engine`, `scale` ("pct"/"pp"/"level"),
#'   `contrast_label`, `profile_note` (may be NULL), and `scenarios` - a list
#'   per moderator level of lists(value, label, estimate, se), or NULL when the
#'   configuration cannot be translated.
#'
#' @export
step1_scenarios <- function(mf, snap, var) {
  engine <- .s1_engine(mf)
  scale <- .s1_scale(mf, snap)
  if (engine == "ml") {
    return(NULL)
  }
  if (engine == "rif") {
    rs <- .s1_rif_scenarios(mf, snap, var, taus = 0.5)
    if (!length(rs)) {
      return(NULL)
    }
    sc <- lapply(rs, function(x) if (length(x)) x[[1]] else NULL)
    sc <- Filter(Negate(is.null), sc)
    if (!length(sc)) {
      return(NULL)
    }
    return(list(
      engine = engine, scale = scale,
      contrast_label = attr(rs, "contrast_label"),
      profile_note = NULL, scenarios = sc
    ))
  }
  sc <- .s1_fixest_scenarios(mf, snap, var)
  if (!length(sc)) {
    return(NULL)
  }
  # Read attributes before any lapply/Filter - they drop list attributes.
  clab <- attr(sc, "contrast_label")
  pnote <- attr(sc, "profile_note")
  # Baseline linear predictor of the reference profile (fixed effects
  # included) for logit fits - used for per-term pp translations.
  profile_eta <- if (is_logistic_fit(mf) && !is.null(sc[[1]]$xb_lo)) sc[[1]]$xb_lo else NULL
  if (is_logistic_fit(mf)) sc <- lapply(sc, .s1_to_pp)
  sc <- Filter(Negate(is.null), sc)
  if (!length(sc)) {
    return(NULL)
  }
  list(
    engine = engine, scale = scale,
    contrast_label = clab, profile_note = pnote,
    profile_eta = profile_eta, scenarios = sc
  )
}

#' Format an effect estimate and SE in the outcome's reporting scale
#'
#' Wrapper over the internal formatter so the table and plot annotations use
#' exactly the card formatting: "pct" -> % change from log points, "pp" ->
#' percentage points, "level" -> raw units.
#'
#' @param est,se Numeric estimate and standard error (model scale, except
#'   logit scenarios which arrive on the probability scale).
#' @param scale One of "pct", "pp", "level".
#' @param digits Significant decimals (default 1).
#' @param ci Optional length-2 precomputed interval in the reporting scale
#'   (logit scenarios carry the endpoint-transformed pp interval).
#'
#' @return List(value, lo_txt, hi_txt, spans_zero).
#'
#' @export
step1_fmt_effect <- function(est, se, scale, digits = 1, ci = NULL) {
  .s1_fmt_scaled(est, se, scale, digits = digits, ci = ci)
}


# Cards ----

.s1_effect_card <- function(mf, snap, var, engine, scale, label_fun,
                            scenario = NULL, scenario_cached = FALSE) {
  varlab <- .s1_weather_label(snap, var, label_fun)
  lab <- paste0("Effect of ", varlab)
  blank <- function(note) {
    list(
      label = lab, value = "Unavailable",
      note = note, class = "neutral"
    )
  }
  if (isTRUE(scenario_cached)) {
    if (is.null(scenario) || !length(scenario$scenarios)) {
      return(blank("the effect could not be translated for this configuration"))
    }
    s <- scenario$scenarios[[1]]
    clab <- scenario$contrast_label
    pnote <- scenario$profile_note
  } else if (engine == "rif") {
    rs <- .s1_rif_scenarios(mf, snap, var, taus = 0.5)
    s <- .s1_scen_at(rs, 0.5)
    clab <- attr(rs, "contrast_label")
    pnote <- NULL
  } else {
    sc <- .s1_fixest_scenarios(mf, snap, var)
    if (!length(sc)) {
      return(blank("the effect could not be translated for this configuration"))
    }
    s <- if (is_logistic_fit(mf)) .s1_to_pp(sc[[1]]) else sc[[1]]
    clab <- attr(sc, "contrast_label")
    pnote <- attr(sc, "profile_note")
  }
  if (is.null(s) || !is.finite(s$estimate) || is.null(s$se) ||
    !is.finite(s$se) || s$se <= 0) {
    return(blank("the effect could not be translated for this configuration"))
  }
  fmt <- .s1_fmt_scaled(s$estimate, s$se, scale, ci = s$ci)
  # Binned weather: the headline is always the top bin vs the omitted lowest
  # bin, so the bold line can say just that; the exact cutpoints stay in the
  # popover (and on the effect plot's axis labels / caption).
  binned <- identical(.s1_cont_binned(snap, var), "Binned")
  contrast_line <- if (binned) "highest vs lowest bin" else clab
  ci_line <- paste0("(95% CI: ", fmt$lo_txt, " to ", fmt$hi_txt, ")")
  info_bits <- c(
    "Translated effect of the fitted model for the contrast shown, in the outcome's units.",
    if (binned) paste0("Bin contrast: ", clab, ".") else NULL,
    if (engine == "rif") {
      "For RIF models this is the median-quantile (\u03c4 = 0.5) effect."
    } else {
      NULL
    },
    if (!is.null(pnote) && nzchar(pnote)) {
      paste0(
        "Evaluated for the sample-average household (all other ",
        "variables held at sample means)."
      )
    } else {
      NULL
    },
    paste0(
      "The same number feeds the effect plot and the focused table ",
      "below. Estimates are associations in this survey population ",
      "\u2014 the relationship Steps 2\u20133 apply to simulated weather ",
      "and policies, not causal weather impacts."
    )
  )
  list(
    label = lab, value = fmt$value,
    note = paste(ci_line, contrast_line, sep = " \u00b7 "),
    note_html = shiny::tagList(
      shiny::tags$div(ci_line),
      shiny::tags$div(style = "font-weight: 600;", contrast_line)
    ),
    info = paste(info_bits, collapse = " ")
  )
}

.s1_who_card <- function(mf, snap, var, engine, scale, label_fun,
                         scenario = NULL, scenario_cached = FALSE,
                         rif_scenarios = NULL, rif_scenarios_cached = FALSE,
                         rif_p = NULL, rif_p_cached = FALSE) {
  bits_val <- character(0)
  note_parts <- character(0) # plain text (CSV export)
  html_parts <- list() # display: p-value line + bold comparison line
  info_bits <- character(0)
  rif_p_line <- NULL
  rif_cmp_line <- NULL

  # Distribution sensitivity (RIF only)
  if (engine == "rif") {
    sc <- if (isTRUE(rif_scenarios_cached)) {
      rif_scenarios
    } else {
      .s1_rif_scenarios(mf, snap, var, taus = c(0.1, 0.9))
    }
    s1 <- .s1_scen_at(sc, 0.1)
    s9 <- .s1_scen_at(sc, 0.9)
    if (!is.null(s1) && !is.null(s9) &&
      all(is.finite(c(s1$estimate, s9$estimate, s1$se, s9$se))) &&
      s1$se > 0 && s9$se > 0) {
      f1 <- .s1_fmt_scaled(s1$estimate, s1$se, scale)
      f9 <- .s1_fmt_scaled(s9$estimate, s9$se, scale)
      bits_val <- c(bits_val, paste0(f1$value, " vs ", f9$value))
      cmp_line <- "poorest 10% vs richest 10%"
      rif_cmp_line <- cmp_line
      note_parts <- c(note_parts, cmp_line)
      info_bits <- c(info_bits, paste0(
        "Compares the translated effect between the poorest 10% and the ",
        "richest 10% of households (RIF quantile estimates; values are ",
        "log-point approximations of % changes)."
      ))
      p <- if (isTRUE(rif_p_cached)) {
        rif_p
      } else {
        tryCatch(step1_rif_heterogeneity_p(mf, snap, var), error = function(e) NULL)
      }
      if (!is.null(p) && is.finite(p)) {
        p_line <- if (p < 0.001) "RIF distribution p < 0.001" else sprintf("RIF distribution p = %.3f", p)
        rif_p_line <- p_line
        note_parts <- c(p_line, note_parts)
        info_bits <- c(info_bits, paste0(
          "The p-value screens whether the effect differs across the welfare ",
          "distribution."
        ))
      }
    }
  }

  # Moderator heterogeneity (any engine with interactions)
  modx_var <- .s1_modx_var(mf, var)
  if (!is.null(modx_var) && engine != "ml" && engine != "rif") {
    sc <- if (isTRUE(scenario_cached)) {
      scenario$scenarios
    } else {
      ss <- .s1_fixest_scenarios(mf, snap, var)
      if (!length(ss)) NULL else if (is_logistic_fit(mf)) lapply(ss, .s1_to_pp) else ss
    }
    sc <- Filter(function(s) {
      !is.null(s) && is.finite(s$estimate) &&
        is.finite(s$se) && s$se > 0
    }, sc)
    if (length(sc) >= 2) {
      fmts <- lapply(sc, function(s) {
        .s1_fmt_scaled(s$estimate, s$se, scale,
          ci = s$ci
        )
      })
      imax <- which.max(abs(vapply(sc, function(s) s$estimate, numeric(1))))
      i_rng <- paste0(fmts[[1]]$value, " vs ", fmts[[length(sc)]]$value)
      interaction_value_line <- i_rng
      ml <- .s1_modx_label(mf, snap, modx_var, label_fun)
      imax_lab <- sc[[imax]]$label %||% "extreme value"
      # Binary moderator levels are labelled yes/no; keep the separator
      # readable either way ("Urban: yes" / "Urban = 1.25").
      lvl_sep <- function(l) {
        if (identical(l, "yes") || identical(l, "no")) ": " else " = "
      }
      lvl1 <- sc[[1]]$label %||% ""
      lvlN <- sc[[length(sc)]]$label %||% ""
      cmp_line <- paste0(
        ml, lvl_sep(lvl1), lvl1, " vs ", ml,
        lvl_sep(lvlN), lvlN
      )
      interaction_cmp_line <- cmp_line
      note_parts <- c(note_parts, cmp_line)
      if (length(sc) > 2) {
        largest_line <- paste0("largest for ", ml, lvl_sep(imax_lab), imax_lab)
        note_parts <- c(note_parts, largest_line)
        html_parts <- c(html_parts, list(shiny::tags$div(largest_line)))
      }
      pdiff <- tryCatch(
        {
          if (engine == "rif") {
            z <- (sc[[1]]$estimate - sc[[length(sc)]]$estimate) /
              sqrt(sc[[1]]$se^2 + sc[[length(sc)]]$se^2)
            2 * stats::pnorm(-abs(z))
          } else {
            # Between-level contrast: difference of the two level estimates with
            # an exact SE from the full vcov (w_last - w_first).
            wd <- (sc[[length(sc)]]$w - sc[[1]]$w)[sc[[1]]$cols]
            estd <- sc[[length(sc)]]$estimate - sc[[1]]$estimate
            V <- sc[[1]]$V
            se <- tryCatch(
              sqrt(max(drop(t(wd) %*% V[sc[[1]]$cols, sc[[1]]$cols, drop = FALSE] %*% wd), 0)),
              error = function(e) NA_real_
            )
            if (!is.finite(se) || se <= 0) {
              return(NA_real_)
            }
            2 * stats::pnorm(-abs(estd / se))
          }
        },
        error = function(e) NULL
      )
      if (!is.null(pdiff) && is.finite(pdiff)) {
        p_line <- if (pdiff < 0.001) "Interaction p < 0.001" else sprintf("Interaction p = %.3f", pdiff)
        interaction_p_line <- p_line
        note_parts <- c(p_line, note_parts)
      }
      info_bits <- c(info_bits, paste0(
        "Compares the translated effect between levels of ", ml,
        " (interaction model). The p-value tests whether the difference ",
        "between the first and last level is statistically significant."
      ))
      # The interaction range only becomes the headline value when there is no
      # RIF distribution range to lead with.
      if (!length(bits_val)) bits_val <- c(bits_val, i_rng)
    }
  }

  if (!length(bits_val)) {
    extra <- if (is_logistic_fit(mf)) {
      " probability effects vary with baseline risk; add interactions or use the quantile (RIF) engine for explicit heterogeneity"
    } else {
      " add interactions or use the quantile (RIF) engine for heterogeneity"
    }
    return(list(
      label = "Who is most affected", value = "Uniform by design",
      note = paste0("This specification applies one weather effect to all households;", extra),
      class = "neutral"
    ))
  }

  if (engine == "rif") {
    html_parts <- list()
    if (!is.null(rif_p_line)) {
      html_parts <- c(html_parts, list(shiny::tags$div(
        paste0("(", rif_p_line, ")")
      )))
    }
    if (!is.null(rif_cmp_line)) {
      html_parts <- c(html_parts, list(shiny::tags$div(
        style = "font-weight: 600;", rif_cmp_line
      )))
    }
  }

  list(
    label = "Who is most affected", value = bits_val[1],
    note = paste(note_parts, collapse = " \u00b7 "),
    note_html = if (length(html_parts)) shiny::tagList(html_parts),
    info = paste(info_bits, collapse = " ")
  )
}

# Pick the coefficient that represents the headline effect of `var`:
# the exact main term, else the top bin, else the first matching term.
.s1_pick_term <- function(terms, var) {
  terms <- as.character(terms)
  hit <- terms[terms == var]
  if (length(hit)) {
    return(hit[1])
  }
  bins <- terms[grepl(paste0("^", .s1_esc(var), "[\\[\\(]"), terms)]
  if (length(bins)) {
    return(bins[which.max(.s1_bin_lower(bins, var))])
  }
  m <- terms[grepl(paste0("(\\b|^)", .s1_esc(var), "(\\b|\\[|\\^)"), terms)]
  if (length(m)) {
    return(m[1])
  }
  NA_character_
}

.s1_coef_at <- function(mf, var, spec) {
  if (identical(.s1_engine(mf), "rif")) {
    g <- mf$rif_grid
    if (is.null(g)) {
      return(NULL)
    }
    g <- g[g$model == spec & abs(g$tau - 0.5) < 1e-9, , drop = FALSE]
    if (!nrow(g)) {
      return(NULL)
    }
    t <- .s1_pick_term(unique(as.character(g$term)), var)
    if (is.na(t)) {
      return(NULL)
    }
    r <- g[g$term == t, , drop = FALSE]
    if (!nrow(r)) {
      return(NULL)
    }
    list(estimate = r$estimate[1], se = r$std.error[1], term = t)
  } else {
    fit <- switch(as.character(spec),
      "1" = mf$fit1,
      "2" = mf$fit2,
      mf$fit3
    )
    ct <- tryCatch(.fixest_coeftable(extract_native_fit(fit, mf$engine)),
      error = function(e) NULL
    )
    if (is.null(ct) || !nrow(ct)) {
      return(NULL)
    }
    t <- .s1_pick_term(rownames(ct), var)
    if (is.na(t) || !t %in% rownames(ct)) {
      return(NULL)
    }
    list(estimate = ct[t, 1], se = ct[t, 2], term = t)
  }
}

.s1_stability_card <- function(mf, snap, var, engine) {
  lab <- "Spec robustness"
  if (engine == "ml") {
    return(list(
      label = lab, value = "\u2014",
      note = "specification comparison is not available for tree-based models",
      class = "neutral"
    ))
  }
  b1 <- .s1_coef_at(mf, var, 1L)
  b3 <- .s1_coef_at(mf, var, 3L)
  if (is.null(b1) || is.null(b3) || !is.finite(b1$estimate) ||
    !is.finite(b3$estimate) || !is.finite(b1$se) || !is.finite(b3$se)) {
    return(list(
      label = lab, value = "\u2014",
      note = "stability across specifications could not be assessed",
      class = "neutral"
    ))
  }
  sign_agree <- sign(b1$estimate) == sign(b3$estimate) && sign(b1$estimate) != 0
  ci_overlap <- abs(b1$estimate - b3$estimate) <=
    1.96 * sqrt(b1$se^2 + b3$se^2)
  # The spec-comparison table is hidden for RIF, so the verdict points at the
  # full coefficient table instead and names the quantile it describes.
  rif_provenance <- if (identical(engine, "rif")) {
    " The RIF coefficient comparison is at the median quantile (\u03c4 = 0.5); full coefficients are in the table below."
  } else {
    ""
  }
  stab_info <- paste0(
    "Compares the weather coefficient across the three nested specifications ",
    "shown in the table below: (1) weather only, (2) + fixed effects, ",
    "(3) + controls. Stable: same sign and overlapping 95% confidence ",
    "intervals; Sensitive: sign holds but the magnitude moves; Unstable: ",
    "sign changes \u2014 interpret with caution."
  )
  if (sign_agree && ci_overlap) {
    list(
      label = lab, value = "Stable",
      note = "same sign and overlapping 95% CIs across specifications",
      info = paste0(stab_info, rif_provenance)
    )
  } else if (sign_agree) {
    list(
      label = lab, value = "Sensitive",
      note = "sign consistent, but the magnitude changes across specifications",
      info = paste0(stab_info, rif_provenance),
      class = "neutral"
    )
  } else {
    list(
      label = lab, value = "Unstable",
      note = "sign changes across specifications \u2014 interpret with caution",
      info = paste0(stab_info, rif_provenance),
      class = "neutral"
    )
  }
}

.s1_fit_card <- function(mf, snap, engine) {
  fit3n <- if (engine == "rif") {
    extract_rif_median(mf$fit3, "rif")
  } else {
    extract_native_fit(mf$fit3, mf$engine)
  }
  N <- tryCatch(as.integer(stats::nobs(fit3n)), error = function(e) NA_integer_)
  # Same R² the full coefficient table reports (within R², FE contribution
  # excluded) - a different metric here used to contradict the table.
  fmt_r2 <- function(w) {
    if (!is.finite(w)) {
      "\u2014"
    } else if (w < 0.005) {
      "<0.01"
    } else {
      sprintf("%.2f", w)
    }
  }
  stat <- tryCatch(
    {
      if (engine == "ml") {
        "tree-based model"
      } else if (engine == "rif") {
        w <- fixest::r2(fit3n, "wr2")
        if (!is.finite(w)) w <- fixest::r2(fit3n, "r2")
        paste0("Within R\u00b2 (\u03c4 = 0.5) ", fmt_r2(w))
      } else if (is_logistic_fit(mf)) {
        sprintf("McFadden R\u00b2 %s", fmt_r2(fixest::r2(fit3n, "pr2")))
      } else {
        w <- fixest::r2(fit3n, "wr2")
        if (!is.finite(w)) w <- fixest::r2(fit3n, "r2")
        paste0("Within R\u00b2 ", fmt_r2(w))
      }
    },
    error = function(e) ""
  )
  list(
    label = "Sample & fit",
    value = if (is.finite(N)) formatC(N, format = "d", big.mark = ",") else "\u2014",
    note = paste(
      c(
        if (is.finite(N)) "observations" else NULL,
        if (nzchar(stat)) stat else NULL
      ),
      collapse = " \u00b7 "
    ),
    info = paste0(
      "Observations used in estimation and the reported fit statistic: ",
      "Within R\u00b2 (fixed-effect contribution excluded) for linear and ",
      "RIF models, McFadden R\u00b2 for binary outcomes \u2014 the same ",
      "statistics the Model fit tab reports. Values refer to the full ",
      "specification (fixed effects and controls)."
    ),
    class = "neutral"
  )
}

.s1_cards_ml <- function(mf, snap, var, label_fun) {
  varlab <- .s1_weather_label(snap, var, label_fun)
  list(
    list(
      label = paste0("Effect of ", varlab), value = "ML model",
      note = paste0(
        "tree-based fit: no coefficients or standard errors; ",
        "see the effect plot for a prediction sweep"
      ),
      class = "neutral"
    ),
    list(
      label = "Who is most affected", value = "Not identified",
      note = "tree-based models do not expose per-covariate effects",
      class = "neutral"
    ),
    list(
      label = "Spec robustness", value = "\u2014",
      note = "specification comparison is not available for tree-based models",
      class = "neutral"
    ),
    .s1_fit_card(mf, snap, "ml")
  )
}

.s1_cards_model <- function(mf, snap, var, engine, label_fun,
                            scenario = NULL, scenario_cached = FALSE,
                            rif_scenarios = NULL,
                            rif_scenarios_cached = FALSE,
                            rif_p = NULL, rif_p_cached = FALSE) {
  scale <- .s1_scale(mf, snap)
  list(
    .s1_effect_card(mf, snap, var, engine, scale, label_fun,
      scenario = scenario, scenario_cached = scenario_cached
    ),
    .s1_who_card(
      mf, snap, var, engine, scale, label_fun,
      scenario = scenario, scenario_cached = scenario_cached,
      rif_scenarios = rif_scenarios,
      rif_scenarios_cached = rif_scenarios_cached,
      rif_p = rif_p, rif_p_cached = rif_p_cached
    ),
    .s1_stability_card(mf, snap, var, engine),
    .s1_fit_card(mf, snap, engine)
  )
}


# Public entry points ----

#' Build the "At a glance" card rows for the Step 1 results tab
#'
#' Returns one row per weather variable, each holding four cards:
#' effect, who is most affected, specification robustness, and sample/fit.
#' Every value is computed from the fitted run (fit-time snapshot), never from
#' live sidebar selections.
#'
#' @param mf        Named list returned by `fit_model()` (with `.snap` attached).
#' @param snap      The fit-time snapshot (`mf$.snap`).
#' @param label_fun Function mapping variable names to readable labels.
#' @param scenarios_list Optional named list of precomputed
#'   \code{step1_scenarios()} results, keyed by weather variable.
#' @param rif_scenarios Optional named list of precomputed tail RIF scenarios,
#'   keyed by weather variable.
#' @param rif_heterogeneity Optional named list of precomputed RIF
#'   heterogeneity p-values, keyed by weather variable.
#'
#' @return List with `rows`, each row a list(var_label, cards), or NULL.
#'
#' @export
step1_headline_cards <- function(mf, snap, label_fun = identity,
                                 scenarios_list = NULL,
                                 rif_scenarios = NULL,
                                 rif_heterogeneity = NULL) {
  tryCatch(
    {
      if (is.null(mf) || is.null(snap)) {
        return(NULL)
      }
      engine <- .s1_engine(mf)
      vars <- mf$weather_terms %||% character(0)
      if (!length(vars)) {
        return(NULL)
      }
      rows <- lapply(vars, function(var) {
        cards <- if (engine == "ml") {
          .s1_cards_ml(mf, snap, var, label_fun)
        } else {
          .s1_cards_model(
            mf, snap, var, engine, label_fun,
            scenario = scenarios_list[[var]],
            scenario_cached = var %in% names(scenarios_list),
            rif_scenarios = rif_scenarios[[var]],
            rif_scenarios_cached = var %in% names(rif_scenarios),
            rif_p = rif_heterogeneity[[var]],
            rif_p_cached = var %in% names(rif_heterogeneity)
          )
        }
        list(var_label = .s1_weather_label(snap, var, label_fun), cards = cards)
      })
      list(rows = rows)
    },
    error = function(e) NULL
  )
}

#' Tidy data frame behind the headline cards (export bundle / CSV)
#'
#' @param result Optional precomputed result from \code{step1_headline_cards()}.
#'   When supplied, \code{mf}, \code{snap}, and \code{label_fun} are not used.
#'
#' @return A data frame with columns weather_variable, card, value, detail,
#'   or NULL when no cards could be built.
#'
#' @export
step1_headline_table <- function(mf = NULL, snap = NULL, label_fun = identity,
                                 result = NULL) {
  res <- if (missing(result)) {
    step1_headline_cards(mf, snap, label_fun = label_fun)
  } else {
    result
  }
  if (is.null(res)) {
    return(NULL)
  }
  do.call(rbind, lapply(res$rows, function(r) {
    data.frame(
      weather_variable = r$var_label,
      card = vapply(r$cards, function(c) c$label %||% "", character(1)),
      value = vapply(r$cards, function(c) c$value %||% "", character(1)),
      detail = vapply(r$cards, function(c) c$note %||% "", character(1)),
      stringsAsFactors = FALSE
    )
  }))
}
