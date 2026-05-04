# ---------------------------------------------------------------------------- #
# Predict outcomes                                                               #
# ---------------------------------------------------------------------------- #

#' Pre-compute Training Residuals for Caching Across Multiple Prediction Calls
#'
#' Extracts residuals (and optionally the id column) from a fitted model +
#' training data once, so callers iterating over many scenarios can pass the
#' result to `predict_outcome()` via `precomputed_train_resid` instead of
#' recomputing it every call.
#'
#' @param model      Fitted model (parsnip, fixest, or bare lm/glm).
#' @param train_data Data frame used at fit time.
#' @param engine     Character engine identifier (`"fixest"`, `"lm_glm"`, etc.).
#' @param id         Optional character column name to retain for id-matched
#'                   residual draws (`residuals = "original"`).
#'
#' @return A data frame with a `.resid` column and (if `id` is supplied and
#'   present in `train_data`) the id column. Returns `NULL` on failure.
#' @export
precompute_train_resid <- function(model, train_data, engine = NULL, id = NULL) {
  is_fixest <- inherits(model, "fixest") ||
    (!is.null(engine) && identical(engine, "fixest"))
  is_fixest_logistic <- is_fixest && inherits(model, "feglm")
  is_parsnip <- !is_fixest && inherits(model, "model_fit")

  resid_vec <- tryCatch({
    if (is_fixest) {
      resid_type <- if (is_fixest_logistic) "deviance" else "response"
      as.numeric(stats::residuals(model, type = resid_type))
    } else if (is_parsnip) {
      aug <- broom::augment(model, new_data = train_data)
      if (".resid" %in% names(aug) && length(stats::na.omit(aug$.resid)) > 0) {
        as.numeric(aug$.resid)
      } else {
        fitted_col <- grep("^\\.pred", names(aug), value = TRUE)
        if (length(fitted_col) == 0L) return(NULL)
        fitted_vals <- as.numeric(aug[[fitted_col[1L]]])
        fitted_vals - mean(fitted_vals, na.rm = TRUE)
      }
    } else {
      aug <- broom::augment(model, data = train_data)
      if (".resid" %in% names(aug)) as.numeric(aug$.resid) else NULL
    }
  }, error = function(e) {
    warning("[precompute_train_resid] failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(resid_vec)) return(NULL)

  # Return a data frame so the id column travels with residuals for matched draws
  out <- data.frame(.resid = resid_vec)
  if (!is.null(id) && id %in% names(train_data)) {
    out[[id]] <- train_data[[id]]
  }
  out
}

#' Predict outcomes                                                               #
# ---------------------------------------------------------------------------- #

#' Predict an Outcome from a Fitted Model with Optional Residual Simulation
#'
#' Generates predictions from a model fit produced by \code{fit_model()}, with
#' flexible options for incorporating residual uncertainty around the fitted
#' values. Handles three families of model objects transparently:
#' \itemize{
#'   \item \pkg{parsnip} \code{model_fit} objects backed by \code{lm} (linear)
#'     or \code{glm} (logistic) — produced when \code{engine == "lm_glm"}.
#'   \item \code{fixest} objects from \code{feols()} (linear) or
#'     \code{feglm()} (logistic) — produced when \code{engine == "fixest"}.
#'   \item Bare \code{lm} / \code{glm} objects (for ad-hoc use outside
#'     \code{fit_model()}).
#' }
#'
#' @section Residual types and logistic models:
#' For logistic models the function preserves a meaningful \code{.resid} column
#' regardless of engine:
#' \itemize{
#'   \item \strong{parsnip/glm}: \code{broom::augment} returns deviance
#'     residuals. \code{.resid} is passed through directly.
#'   \item \strong{fixest/feglm}: deviance residuals are extracted via
#'     \code{residuals(model, type = "deviance")} and stored in
#'     \code{train_aug$.resid}.
#' }
#' When \code{residuals != "none"}, these residuals are used for simulation.
#' Note that adding residuals to a logistic fitted probability is only
#' meaningful on the response/probability scale and is primarily useful for
#' bootstrap-style welfare simulations; interpret with care.
#'
#' @section fixest prediction:
#' \code{fixest} models do not implement the standard \code{predict()} S3
#' method in a way that \code{broom::augment} can dispatch, so this function
#' calls \code{predict(fixest_fit, newdata = newdata, type = "response")}
#' directly. For \code{feols} this returns fitted values on the response scale;
#' for \code{feglm(family = binomial)} this returns predicted probabilities
#' P(Y = 1). Fixed effects not present in \code{newdata} are silently dropped
#' by \code{fixest::predict.fixest}; ensure \code{newdata} contains all FE
#' columns for reliable predictions.
#'
#' @param model A fitted model: a \pkg{parsnip} \code{model_fit}, a
#'   \code{fixest} object (\code{feols}/\code{feglm}), or a bare \code{lm}/
#'   \code{glm} object.
#' @param newdata A data frame of new observations to predict on.
#' @param type Character. Passed to \code{broom::augment()} for bare
#'   \code{lm}/\code{glm} objects. Common values: \code{"response"} (default),
#'   \code{"link"}. Ignored for parsnip and fixest objects.
#' @param residuals Character. One of \code{"none"}, \code{"original"},
#'   \code{"normal"}, \code{"resample"}. Controls how residuals are added to
#'   fitted values.
#'   \describe{
#'     \item{\code{"none"}}{No residual added. \code{.residual} is \code{NA}.}
#'     \item{\code{"original"}}{Residuals from training data, matched by
#'       \code{id} column or recycled by position.}
#'     \item{\code{"normal"}}{Draws from \eqn{N(0, \hat\sigma)} where
#'       \eqn{\hat\sigma} is the SD of training residuals.}
#'     \item{\code{"resample"}}{Bootstrap sample from training residuals.}
#'   }
#' @param id Optional character. Column name used to match training residuals
#'   to rows in \code{newdata} when \code{residuals = "original"}. If
#'   \code{NULL}, residuals are recycled by row position.
#' @param outcome Character. Name for the predicted outcome column in the
#'   returned data frame. Defaults to \code{"predicted"}.
#' @param train_data Optional data frame. The training data used to fit
#'   \code{model}. Required when \code{residuals != "none"}. If \code{model}
#'   was produced by \code{fit_model()}, pass \code{model_result$train_data}
#'   here. For fixest models this is mandatory (fixest objects do not store
#'   the training frame).
#' @param engine Character. One of \code{"lm_glm"} or \code{"fixest"}.
#'   If \code{model} was produced by \code{fit_model()}, pass
#'   \code{model_result$engine}. Auto-detected from the model class if
#'   \code{NULL} (default).
#'   \code{NULL} (default) uses the standard prediction path unchanged.
#'
#' @return The \code{newdata} data frame (tibble) with additional columns:
#'   \describe{
#'     \item{\code{.fitted}}{Fitted values / predicted probabilities on the
#'       response scale.}
#'     \item{\code{.residual}}{The residual draw used (\code{NA} if
#'       \code{residuals = "none"}).}
#'     \item{\code{<outcome>}}{The final predicted value:
#'       \code{.fitted + .residual} (\code{.fitted} only when
#'       \code{residuals = "none"}).}
#'   }
#'
#' @examples
#' library(dplyr)
#' set.seed(42)
#' train_data <- data.frame(
#'   id      = 1:1000,
#'   welfare = exp(rnorm(1000, mean = log(3.5), sd = 0.8)),
#'   age     = rnorm(1000, mean = 35, sd = 10),
#'   educ    = rnorm(1000, mean = 8,  sd = 3)
#' )
#' model <- lm(log(welfare) ~ age + educ, data = train_data)
#' new_data <- data.frame(id = 1:500, age = rnorm(500), educ = rnorm(500))
#'
#' # Original residuals matched by id
#' predict_outcome(model, new_data, residuals = "original",
#'                 id = "id", train_data = train_data)
#'
#' # Parametric residuals
#' predict_outcome(model, new_data, residuals = "normal", train_data = train_data)
#'
#' @importFrom broom augment
#' @importFrom dplyr mutate left_join select
#' @importFrom rlang sym .data
#' @importFrom stats rnorm sd na.omit residuals predict
#' @export
predict_outcome <- function(model,
                            newdata,
                            type       = "response",
                            residuals  = c("none", "original", "normal", "resample"),
                            id         = NULL,
                            outcome    = "predicted",
                            train_data = NULL,
                            engine     = NULL,
                            precomputed_train_resid = NULL) {

  if (length(residuals) == 0) stop("`residuals` must be a single string; got length 0.")
  if (length(residuals) > 1)  residuals <- residuals[[1]]
  residuals <- match.arg(residuals)

  # ---------------------------------------------------------------------------
  # 1. Detect model class
  # ---------------------------------------------------------------------------
  # Priority: explicit `engine` argument > class-based detection.
  # This mirrors the `engine` field returned by fit_model().

  is_fixest  <- inherits(model, "fixest") ||
                (!is.null(engine) && identical(engine, "fixest"))
  is_parsnip <- !is_fixest && inherits(model, "model_fit")

  # For parsnip: distinguish linear vs logistic to pick the right augment column
  is_parsnip_logistic <- is_parsnip &&
    inherits(model$spec, c("logistic_reg", "multinom_reg"))

  # For fixest: distinguish feglm (logistic) vs feols (linear)
  is_fixest_logistic <- is_fixest &&
    inherits(model, "feglm")

  # ---------------------------------------------------------------------------
  # 2. Compute fitted values on newdata  → stored in .fitted
  # ---------------------------------------------------------------------------

  if (is_fixest) {

    # fixest does not have a broom::augment method that accepts new data cleanly.
    # Use predict.fixest directly:
    #   feols  → numeric fitted values
    #   feglm  → P(Y=1) when type = "response" (the default)
    preds_vec <- tryCatch(
      stats::predict(model, newdata = newdata, type = "response"),
      error = function(e) stop(sprintf("fixest predict failed: %s", conditionMessage(e)))
)
    # fixest::predict() silently drops rows where FE levels are absent from
    # training data. Trim newdata to match preds_vec length before mutating,
    # so that preds, X_nonFE (from model.matrix), and resid_draw are all
    # sized consistently downstream.
    predicted_rows <- attr(preds_vec, "rowids") %||% seq_along(preds_vec)
    preds <- dplyr::mutate(newdata[predicted_rows, ], .fitted = preds_vec)

  } else if (is_parsnip) {

    preds <- broom::augment(model, new_data = newdata)

    # Resolve the fitted-value column produced by augment
    fitted_col <- if (is_parsnip_logistic) ".pred_1" else ".pred"

    if (!fitted_col %in% names(preds)) {
      candidates <- c(
        grep("^\\.pred", names(preds), value = TRUE),
        if (".fitted" %in% names(preds)) ".fitted"
      )
      if (length(candidates) == 0L) stop("No fitted-value column found in augment output.")
      warning(sprintf(
        "predict_outcome: expected '%s' not in augment output; using '%s' instead.",
        fitted_col, candidates[[1L]]
      ))
      fitted_col <- candidates[[1L]]
    }

    # Normalise to .fitted for a consistent downstream interface
    if (!identical(fitted_col, ".fitted")) {
      preds <- dplyr::mutate(preds, .fitted = .data[[fitted_col]])
    }

  } else {
    # Bare lm / glm
    preds <- broom::augment(model, newdata = newdata, type.predict = type)
    if (!".fitted" %in% names(preds)) stop("No .fitted column in augment output.")
  }

  # ---------------------------------------------------------------------------
  # 3. Obtain training-data augmentation for residual simulation
  # ---------------------------------------------------------------------------

  train_aug <- NULL

  if (residuals != "none") {

    # Fast path: caller pre-computed residuals — use directly as train_aug
    if (!is.null(precomputed_train_resid)) {
      train_aug <- precomputed_train_resid  # already a data frame with .resid (+id)
    } else {

      if (is.null(train_data))
        stop("`train_data` must be supplied when `residuals != 'none'`.")

      if (is_fixest) {

      # fixest stores residuals internally; extract them and bind to train_data.
      # For feols:  residuals() returns OLS residuals (observed - fitted).
      # For feglm:  residuals(, type = "deviance") are the canonical residuals
      #             used for diagnostic purposes.  For simulation we keep them
      #             so the caller can choose how to use them.
      resid_type <- if (is_fixest_logistic) "deviance" else "response"
      resid_vec  <- tryCatch(
        stats::residuals(model, type = resid_type),
        error = function(e) {
          warning(sprintf(
            "Could not extract '%s' residuals from fixest model (%s); ",
            resid_type, conditionMessage(e),
            "falling back to response residuals."
          ))
          stats::residuals(model, type = "response")
        }
      )

      fitted_train <- tryCatch(
        stats::predict(model, newdata = train_data, type = "response"),
        error = function(e) stop(sprintf(
          "fixest predict on train_data failed: %s", conditionMessage(e)
        ))
      )

      train_aug <- dplyr::mutate(
        train_data,
        .fitted = as.numeric(fitted_train),
        .resid  = as.numeric(resid_vec)
      )

    } else if (is_parsnip) {

      train_aug <- broom::augment(model, new_data = train_data)

      # Normalise fitted column name
      fitted_col_tr <- if (is_parsnip_logistic) ".pred_1" else ".pred"
      if (!fitted_col_tr %in% names(train_aug)) {
        candidates <- grep("^\\.pred", names(train_aug), value = TRUE)
        if (length(candidates) > 0) fitted_col_tr <- candidates[[1L]]
        else if (".fitted" %in% names(train_aug)) fitted_col_tr <- ".fitted"
        else stop("No fitted-value column in training augment output.")
      }
      if (!identical(fitted_col_tr, ".fitted")) {
        train_aug <- dplyr::mutate(train_aug, .fitted = .data[[fitted_col_tr]])
      }

      # Ensure .resid is present and meaningful.
      # For parsnip logistic (glm engine), broom adds deviance .resid when
      # the outcome column is present in the augmented data.
      has_resid <- ".resid" %in% names(train_aug) &&
        length(stats::na.omit(train_aug$.resid)) > 0

      if (!has_resid) {
        # Try to compute response residual = observed - predicted probability.
        
        # The outcome column is in train_data but may not have survived augment.
        outcome_col <- intersect(names(train_data), names(train_aug))
        outcome_col <- outcome_col[outcome_col %in% names(train_data)]
        
        # Pick the column that differs from newdata (i.e. the outcome)
        outcome_col <- setdiff(outcome_col, names(newdata))
        outcome_col <- outcome_col[outcome_col %in% names(train_aug)]

        if (length(outcome_col) == 1L) {
          obs <- as.numeric(train_aug[[outcome_col]])
          train_aug <- dplyr::mutate(train_aug, .resid = obs - .data$.fitted)
        } else {
          warning(
            "`.resid` unavailable and outcome column could not be identified; ",
            "using centred fitted values as proxy residuals."
          )
          train_aug <- dplyr::mutate(
            train_aug, .resid = .data$.fitted - mean(.data$.fitted, na.rm = TRUE)
          )
        }
      }

    } else {
      # Bare lm / glm
      train_aug <- broom::augment(model, data = train_data)
      if (!".resid" %in% names(train_aug) ||
          length(stats::na.omit(train_aug$.resid)) == 0) {
        f <- train_aug$.fitted %||% 0
        train_aug <- dplyr::mutate(train_aug, .resid = f - mean(f, na.rm = TRUE))
      }
      if (!".fitted" %in% names(train_aug)) {
        train_aug <- dplyr::mutate(train_aug, .fitted = 0)
      }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Draw residuals
  # ---------------------------------------------------------------------------

  resid_draw <- switch(residuals,

    none = 0,

    original = {
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0)
        stop("No training residuals available for `residuals = 'original'`.")

      if (!is.null(id)) {
        if (!id %in% names(train_aug)) stop("`id` column not found in training augmentation.")
        if (!id %in% names(preds))     stop("`id` column not found in `newdata`.")

        train_resid_df <- dplyr::select(train_aug, !!rlang::sym(id), .resid)
        joined    <- dplyr::left_join(preds, train_resid_df, by = id)
        resid_vec <- joined$.resid[seq_len(nrow(preds))]
        missing   <- is.na(resid_vec)
        if (any(missing)) {
          warning("Some IDs in `newdata` not in training data; filling by resampling.")
          resid_vec[missing] <- sample(train_resid, size = sum(missing), replace = TRUE)
        }
        resid_vec
      } else {
        if (nrow(preds) %% length(train_resid) != 0)
          warning("`residuals = 'original'`: repeating training residuals ",
                  "(length not an integer multiple).")
        rep(train_resid, length.out = nrow(preds))
      }
    },

    normal = {
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0)
        stop("No training residuals available for `residuals = 'normal'`.")
      # Use nrow(preds) not nrow(newdata): fixest::predict() silently drops rows
      # with FE levels absent from training data, so preds may have fewer rows
      # than newdata. Sizing resid_draw to nrow(newdata) causes a mutate() error.
      stats::rnorm(nrow(preds), mean = 0, sd = stats::sd(train_resid, na.rm = TRUE))
    },

    resample = {
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0)
        stop("No training residuals available for `residuals = 'resample'`.")
      # Same reason as normal above — use nrow(preds).
      sample(train_resid, size = nrow(preds), replace = TRUE)
    }
  )

  # ---------------------------------------------------------------------------
  # 5. Assemble output
  # ---------------------------------------------------------------------------

  preds |>
    dplyr::mutate(
      .residual            = if (residuals == "none") NA_real_ else resid_draw,
      !!rlang::sym(outcome) := .data$.fitted + resid_draw
    )
}

# ----------------------------------------------------------------------------
