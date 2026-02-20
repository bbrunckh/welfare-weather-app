#' Predict an Outcome from a Fitted Model with Optional Residual Simulation
#'
#' Generates predictions from a fitted model object using new data, with
#' flexible options for incorporating residual uncertainty around the fitted
#' values. Useful for simulation workflows where predictions reflect both
#' model uncertainty and unexplained variance.
#'
#' @param model A fitted model object supported by `broom::augment()`,
#'   such as those produced by `lm()`, `glm()`, or `parsnip`.
#' @param newdata A data frame of new observations to predict on.
#' @param type A character string passed to `type.predict` in
#'   `broom::augment()`. Common values are `"response"` (default)
#'   and `"link"`.
#' @param residuals A character string specifying how residuals are added to
#'   fitted values. One of `"none"`, `"original"`, `"normal"`, `"empirical"`.
#'   See details in the help above for behaviour of each option.
#' @param id An optional character string naming an identifier column used to
#'   match original residuals from the training data to rows in `newdata`.
#'   If `NULL` (default), residuals are matched by row position or repeated
#'   to fill `newdata` as described above.
#' @param outcome A character string giving the name to assign to the predicted
#'   outcome column in the returned data frame. Defaults to `"predicted"`.
#' @param train_data Optional data.frame with the original training data used
#'   to fit `model`. When supplied, `broom::augment(model, data = train_data)`
#'   is used to obtain training residuals and any `id` column that was not
#'   included in the model frame.
#'
#' @return The `newdata` data frame (tibble) with additional columns `.fitted`,
#'   `.residual` (if `residuals != "none"`), and the predicted outcome column
#'   named by `outcome`.
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
#' # Use original training residuals matched by id by supplying train_data
#' predict_outcome(model, new_data, residuals = "original", id = "id", train_data = train_data)
#'
#' # Or avoid needing train_data / id by using parametric residuals
#' predict_outcome(model, new_data, residuals = "normal")
#'
#' @importFrom broom augment
#' @importFrom dplyr mutate left_join select
#' @importFrom stats rnorm sd
#' @export
predict_outcome <- function(model,
                            newdata,
                            type      = "response",
                            residuals = c("none", "original", "normal", "empirical"),
                            id        = NULL,
                            outcome   = "predicted",
                            train_data = NULL) {

  residuals <- match.arg(residuals)

  # Predictions on newdata (uses broom to get .fitted)
  preds <- broom::augment(model, newdata = newdata, type.predict = type)

  # When residuals are requested, obtain training augmentation;
  # prefer train_data (if supplied) so id/.resid are available.
  train_aug <- if (residuals != "none") {
    if (!is.null(train_data)) broom::augment(model, data = train_data)
    else broom::augment(model)
  } else NULL

  resid_draw <- switch(residuals,
    none = 0,
    original = {
      if (is.null(train_aug)) {
        stop("No training augmentation available for `residuals = 'original'`.")
      }
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0) {
        stop("No training residuals available to use with `residuals = 'original'`.")
      }

      if (!is.null(id)) {
        # Ensure id present in both training augmentation and preds/newdata
        if (!id %in% names(train_aug)) {
          stop("`id` column not found in training data (needed to match original residuals). Supply `train_data` containing the id or include id in the model frame.")
        }
        if (!id %in% names(preds)) {
          stop("`id` column not found in `newdata` (needed to match original residuals).")
        }

        train_resid_df <- train_aug |> dplyr::select(!!rlang::sym(id), .resid)
        joined <- dplyr::left_join(preds, train_resid_df, by = id)
        resid_vec <- joined$.resid

        missing <- is.na(resid_vec)
        if (any(missing)) {
          warning("Some IDs in `newdata` were not present in the training data; filling missing residuals by resampling training residuals.")
          resid_vec[missing] <- sample(train_resid, size = sum(missing), replace = TRUE)
        }

        resid_vec
      } else {
        # repeat training residuals to match newdata rows
        if (nrow(newdata) != length(train_resid)) {
          if (nrow(newdata) %% length(train_resid) != 0) {
            warning("`residuals = 'original'`: repeating training residuals to match newdata (length not an integer multiple).")
          }
        }
        rep(train_resid, length.out = nrow(newdata))
      }
    },
    normal = {
      if (is.null(train_aug)) {
        stop("No training augmentation available to estimate residual SD for `residuals = 'normal'`.")
      }
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0) {
        stop("No training residuals available to use with `residuals = 'normal'`.")
      }
      rnorm(nrow(newdata), mean = 0, sd = sd(train_resid, na.rm = TRUE))
    },
    empirical = {
      if (is.null(train_aug)) {
        stop("No training augmentation available to resample residuals for `residuals = 'empirical'`.")
      }
      train_resid <- train_aug$.resid
      if (length(train_resid) == 0) {
        stop("No training residuals available to use with `residuals = 'empirical'`.")
      }
      sample(train_resid, size = nrow(newdata), replace = TRUE)
    }
  )

  preds |>
    dplyr::mutate(
      .residual = if (residuals == "none") NA_real_ else resid_draw,
      !!rlang::sym(outcome) := .fitted + resid_draw
    )
}

#' Aggregate a Predicted Outcome Across Observations Within Groups
#'
#' Aggregates a predicted individual-level outcome across observations within
#' each group (e.g. simulation year), producing a single summary statistic per
#' group. This is typically used to collapse individual-level predictions from
#' a simulation into group-level indicators before plotting.
#'
#' For binary outcomes the aggregate is always the population share with a value
#' of 1 (i.e. the mean). For continuous outcomes a range of aggregates are
#' available, including poverty and inequality measures.
#'
#' @param df A data frame containing individual-level predictions.
#' @param outcome A character string giving the name of the outcome column in
#'   `df`
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `sim_year`.
#' @param type Either `continuous` (default) or `binary`. For
#'   binary outcomes the aggregate is always the population share with outcome
#'   equal to 1, and the `aggregate` and `pov_line` arguments are
#'   ignored.
#' @param aggregate The summary statistic to compute when `type =
#'   "continuous`. One of `mean` (default), `sum`,
#'   `median`, `headcount_ratio` (population share with outcome
#'   below `pov_line`), `gap` (average normalised shortfall below
#'   `pov_line` across the whole population, i.e. the Foster-Greer-Thorbecke
#'   P1 index), or `gini` (Gini coefficient). Ignored when
#'   `type = "binary`.
#' @param pov_line A numeric poverty line required when `aggregate` is
#'   `headcount_ratio` or `gap`. Ignored otherwise.
#' @param weights An optional character string giving the name of a survey
#'   weight column in `df`. If `NULL` (default) all observations are
#'   weighted equally.
#'
#' @return A tibble with one row per group containing the grouping column
#'   and a column named `value` holding the computed aggregate.
#'
#' @examples
#' library(dplyr)
#'
#' # Simulate 50 simulation years, 1000 individuals each
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' # Headcount poverty rate at $3.00/day
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' head(pov_by_year)
#'
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @export
aggregate_outcome <- function(df,
                              outcome,
                              group     = "sim_year",
                              type      = c("continuous", "binary"),
                              aggregate = c("mean", "sum", "median",
                                            "headcount_ratio", "gap", "gini"),
                              pov_line  = NULL,
                              weights   = NULL) {

  type      <- match.arg(type)
  aggregate <- match.arg(aggregate)

  gini_coef <- function(x, w) {
    ord    <- order(x)
    x      <- x[ord]
    w      <- if (is.null(w)) rep(1, length(x)) else w[ord]
    w      <- w / sum(w)
    lorenz <- cumsum(w * x) / sum(w * x)
    lorenz <- c(0, lorenz)
    cx     <- c(0, cumsum(w))
    B      <- sum(diff(cx) * (lorenz[-length(lorenz)] + lorenz[-1]) / 2)
    1 - 2 * B
  }

  compute <- function(x, w) {
    if (type == "binary") {
      if (is.null(w)) mean(as.numeric(x), na.rm = TRUE)
      else sum(as.numeric(x) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      switch(aggregate,
        mean            = if (is.null(w)) mean(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
        sum             = if (is.null(w)) sum(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE),
        median          = if (is.null(w)) median(x, na.rm = TRUE) else {
                            ord  <- order(x); x <- x[ord]; w <- w[ord]
                            cumw <- cumsum(w) / sum(w)
                            x[which(cumw >= 0.5)[1]]
                          },
        headcount_ratio = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for headcount_ratio")
          poor <- as.numeric(x < pov_line)
          if (is.null(w)) mean(poor, na.rm = TRUE)
          else sum(poor * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gap             = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for gap")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) mean(shortfall, na.rm = TRUE)
          else sum(shortfall * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gini            = gini_coef(x, w)
      )
    }
  }

  df |>
    group_by(!!sym(group)) |>
    summarise(
      value = compute(
        x = .data[[outcome]],
        w = if (!is.null(weights)) .data[[weights]] else NULL
      ),
      .groups = "drop"
    )
}

#' Express Aggregate Values as Deviation from a Central Tendency
#'
#' Takes the output of `aggregate_outcome()` and expresses each group's
#' value as a deviation from either the mean or median across all groups.
#' Optionally flips the sign of the result for loss-framed outcomes, where a
#' positive deviation is undesirable (e.g. poverty rates, deficits).
#'
#' @param df A data frame with at least a grouping column and a `value`
#'   column, as returned by `aggregate_outcome()`.
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `"sim_year"`.
#' @param centre A character string, either `"mean"` (default) or
#'   `"median"`, specifying the central tendency to deviate from.
#' @param loss Logical. If `TRUE`, the sign of the deviation is flipped so
#'   that positive values represent outcomes worse than the centre (e.g. higher
#'   poverty than expected). Defaults to `FALSE`.
#'
#' @return A tibble with the same columns as `df` and `value`
#'   replaced by the deviation from the chosen centre, optionally sign-flipped.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' # Deviation from mean, welfare framing (higher welfare = good)
#' deviation_from_centre(pov_by_year)
#'
#' # Deviation from median, loss framing (higher poverty = bad)
#' deviation_from_centre(pov_by_year, centre = "median", loss = TRUE)
#'
#' @importFrom dplyr mutate
#' @export
deviation_from_centre <- function(df,
                                  group  = "sim_year",
                                  centre = c("mean", "median"),
                                  loss   = FALSE) {

  centre <- match.arg(centre)

  ref <- switch(centre,
    mean   = mean(df$value,   na.rm = TRUE),
    median = median(df$value, na.rm = TRUE)
  )

  sign <- if (loss) -1 else 1

  df |>
    mutate(value = sign * (value - ref))
}

#' Plot Exceedance Probability Curve
#'
#' Creates an exceedance probability curve (1 - ECDF) for one or more variables,
#' with the probability axis on the y-axis and the variable axis on the x-axis
#' (via `coord_flip`). Useful for visualizing the probability that a
#' variable exceeds a given threshold. When multiple variables are provided,
#' each is drawn in a distinct color with a legend.
#'
#' @param df A data frame containing the variables to plot.
#' @param variables A character vector of column names in `df` to plot.
#'   Each variable will be drawn as a separate exceedance curve.
#' @param x_label A character string for the axis label of the plotted variable
#'   (displayed on the y-axis after `coord_flip`).
#' @param labels An optional character vector of labels corresponding to each
#'   entry in `variables`, used in the legend. Must be the same length as
#'   `variables`. If `NULL` (default), the column names are used.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate poverty rate changes across 500 scenarios
#' set.seed(42)
#' sim_data <- data.frame(
#'   pov300d_base   = rnorm(500, mean = 0.02, sd = 0.05),
#'   pov300d_policy = rnorm(500, mean = -0.01, sd = 0.04)
#' )
#'
#' # Single variable (no legend)
#' plot_exceedance(sim_data, "pov300d_base", "Change in $3.00 poverty rate (pp.)")
#'
#' # Multiple variables with custom labels
#' plot_exceedance(
#'   sim_data,
#'   variables = c("pov300d_base", "pov300d_policy"),
#'   x_label   = "Change in $3.00 poverty rate (pp.)",
#'   labels    = c("Baseline", "Policy")
#' )
#'
#' @importFrom ggplot2 ggplot aes stat_ecdf geom_vline labs theme_minimal
#'   coord_flip after_stat scale_color_brewer
#' @importFrom dplyr bind_rows mutate
#' @export
plot_exceedance <- function(df, variables, x_label, labels = NULL) {

  if (is.null(labels)) labels <- variables
  stopifnot(length(labels) == length(variables))

  # Reshape to long format so ggplot can map color to group
  long_df <- do.call(bind_rows, lapply(seq_along(variables), function(i) {
    data.frame(
      value = df[[variables[i]]],
      group = labels[i]
    )
  }))

  # Preserve label order in the legend
  long_df$group <- factor(long_df$group, levels = labels)

  ggplot(long_df, aes(x = value, color = group)) +
    stat_ecdf(geom = "point", aes(y = 1 - after_stat(y)), alpha = 0.5) +
    stat_ecdf(geom = "line",  aes(y = 1 - after_stat(y)), linewidth = 1) +
    geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
    labs(
      x     = x_label,
      y     = "Annual Exceedance Probability (P(X > x))",
      color = NULL
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    coord_flip()
}

# # EXAMPLE LINKING ALL FUNCTIONS TOGETHER IN A SIMULATION PIPELINE
# #
# # Fit the model on a training set that contains an `id` column so we can
# # match original residuals to simulated individuals across years.
# library(dplyr)
# library(ggplot2)
# library(broom)

# set.seed(123)
# n_years <- 100
# n_obs   <- 1000

# # 1. Training data with one row per individual (id)
# train_data <- data.frame(
#   id      = 1:n_obs,
#   welfare = exp(rnorm(n_obs, mean = log(3.50), sd = 0.8)),
#   age     = rnorm(n_obs, mean = 35, sd = 10),
#   educ    = rnorm(n_obs, mean = 8,  sd = 3)
# )

# model <- lm(log(welfare) ~ age + educ, data = train_data)

# # 2. Simulate new population data across n_years where the same individuals
# #    (ids 1:n_obs) are repeated each simulation year
# sim_base <- data.frame(
#   sim_year = rep(1:n_years, each = n_obs),
#   id       = rep(1:n_obs, times = n_years),
#   age      = rnorm(n_years * n_obs, mean = 35, sd = 10),
#   educ     = rnorm(n_years * n_obs, mean = 8,  sd = 3)
# )

# sim_policy <- sim_base |>
#   mutate(educ = educ + rnorm(n(), mean = 1, sd = 0.5))  # policy raises education

# # 3. Predict welfare for each scenario, matching original residuals by id
# #    (rows with ids not found in the training set would be filled by resampling)
# pred_base <- predict_outcome(
#   model     = model,
#   newdata   = sim_base,
#   type      = "response",
#   residuals = "original",
#   outcome   = "welfare",
#   id        = "id",
#   train_data = train_data
# ) |>
#   mutate(welfare = exp(welfare))   # back-transform from log scale

# pred_policy <- predict_outcome(
#   model     = model,
#   newdata   = sim_policy,
#   type      = "response",
#   residuals = "original",
#   outcome   = "welfare",
#   id        = "id",
#   train_data = train_data
# ) |>
#   mutate(welfare = exp(welfare))

# # 4. Aggregate to poverty headcount per simulation year
# pov_base <- aggregate_outcome(
#   df        = pred_base,
#   outcome   = "welfare",
#   group     = "sim_year",
#   type      = "continuous",
#   aggregate = "headcount_ratio",
#   pov_line  = 3.00
# )

# pov_policy <- aggregate_outcome(
#   df        = pred_policy,
#   outcome   = "welfare",
#   group     = "sim_year",
#   type      = "continuous",
#   aggregate = "headcount_ratio",
#   pov_line  = 3.00
# )

# # 5. Express each year as deviation from the cross-year mean (loss = TRUE so
# #    positive values mean "worse than expected")
# pov_base_d   <- deviation_from_centre(pov_base,   centre = "mean", loss = TRUE)
# pov_policy_d <- deviation_from_centre(pov_policy, centre = "mean", loss = TRUE)

# # 6. Combine into a single wide data frame for plotting
# plot_data <- data.frame(
#   baseline = pov_base_d$value,
#   policy   = pov_policy_d$value
# )

# # 7. Plot exceedance curves for both scenarios
# plot_exceedance(
#   df        = plot_data,
#   variables = c("baseline", "policy"),
#   x_label   = "Change in $3.00 poverty rate relative to mean (pp.)",
#   labels    = c("Baseline", "Education policy")
# )