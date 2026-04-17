# ============================================================================ #
# Pure functions for Unconditional Quantile Regression (RIF).                  #
#                                                                              #
# Implements the Recentered Influence Function approach of                     #
# Firpo, Fortin & Lemieux (2009) for estimating distributional impacts.        #
#                                                                              #
# Used by:                                                                     #
#   Module 1 — fct_fit_model.R (compute_rif, build_rif_grid)                   #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# RIF computation                                                               #
# ---------------------------------------------------------------------------- #

#' Compute the Recentered Influence Function for a given quantile
#'
#' Transforms the outcome \code{y} into its RIF representation at quantile
#' \code{tau}, following Firpo, Fortin & Lemieux (2009). The resulting vector
#' can be used as a dependent variable in OLS to estimate unconditional
#' quantile partial effects.
#'
#' @param y   Numeric vector (the outcome).
#' @param tau Scalar in (0, 1) specifying the quantile.
#' @param bw  Optional bandwidth for kernel density estimation. When
#'   \code{NULL} (default), uses \code{stats::bw.SJ()}.
#'
#' @return Numeric vector of RIF values, same length as \code{y}.
#'
#' @export
compute_rif <- function(y, tau, bw = NULL) {
  y_obs <- y[!is.na(y)]
  q_tau <- stats::quantile(y_obs, probs = tau, names = FALSE)

  bw_use <- if (is.null(bw)) stats::bw.SJ(y_obs) else bw
  dens   <- stats::density(y_obs, bw = bw_use, n = 512)
  f_q    <- stats::approx(dens$x, dens$y, xout = q_tau)$y

  if (is.na(f_q) || f_q <= 0) {
    warning(sprintf("Density near zero at quantile %.2f; RIF may be unstable.", tau))
    f_q <- max(f_q, 1e-6)
  }

  q_tau + (tau - as.numeric(y <= q_tau)) / f_q
}


# ---------------------------------------------------------------------------- #
# Grid construction                                                             #
# ---------------------------------------------------------------------------- #

#' Build a tidy data frame of RIF regression coefficients ("beta curves")
#'
#' Extracts coefficients from a \code{fixest_multi} object (stacked feols
#' result) and arranges them into a long-format grid with one row per
#' (quantile x term) combination.
#'
#' @param fits_multi A \code{fixest_multi} object returned by
#'   \code{fixest::feols()} with a stacked LHS.
#' @param taus Numeric vector of quantile values corresponding to each
#'   element in \code{fits_multi}.
#' @param model_id Integer model identifier (1 = weather only, 2 = + FE,
#'   3 = + FE + controls).
#'
#' @return A data frame with columns: \code{tau}, \code{term},
#'   \code{estimate}, \code{std.error}, \code{conf.low}, \code{conf.high},
#'   \code{model}.
#'
#' @export
build_rif_grid <- function(fits_multi, taus, model_id) {
  purrr::map_dfr(seq_along(taus), function(i) {
    tbl       <- broom::tidy(fits_multi[[i]], conf.int = TRUE)
    tbl$tau   <- taus[i]
    tbl$model <- model_id
    tbl
  })
}
