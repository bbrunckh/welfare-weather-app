# ============================================================================ #
# fct_h3_check.R                                                               #
# Data-quality checks on the h3 <-> survey-location population mapping that    #
# get_weather() uses to aggregate gridded weather to survey locations.         #
# Pure functions, no Shiny, no database.                                       #
# ============================================================================ #


#' Check the population mapping behind the weather aggregation
#'
#' `get_weather()` collapses gridded weather to survey locations with a
#' population-weighted mean, taking the weights from the `pop_2020` column of
#' each survey's h3 mapping file. This summarises that file so the weights can
#' be checked before they are trusted, and so a regenerated file can be
#' compared against the one it replaces.
#'
#' Two things are worth watching:
#'
#' * **Sub-cell rows.** The mapping is finer-grained than the weather grid:
#'   several rows can share one `loc_id` x `h3` pair, one per populated
#'   sub-cell of that cell. `max_rows_per_cell` reports that multiplicity. It
#'   is not a fault in itself, but it means the rows have to be *summed* to
#'   form a cell's weight and must never be de-duplicated — sub-cell
#'   populations are small integers that frequently tie, so dropping
#'   "duplicates" deletes real population. `n_tied_rows` and `pop_tied` measure
#'   exactly how much would be lost that way.
#' * **National coverage.** The mapping only covers cells that contain a survey
#'   location, so `pop_mapped` falls short of the country's population — the
#'   difference is the part of the country the survey never reached, which is
#'   also the part a national-exposure comparison would need. Pass
#'   `reference_pop` to quantify the gap.
#'
#' `pop_mapped` is built per cell as the largest population seen for it from
#' any single location, then summed over cells. A cell that straddles several
#' locations appears once per location, so summing the file directly would
#' count it many times over; taking the richest single view of each cell
#' counts it once.
#'
#' @param h3_map A data frame with columns `code`, `year`, `survname`,
#'   `loc_id`, `h3` and `pop_2020`, or a list of such data frames. May hold
#'   several surveys at once; results are reported per survey.
#' @param reference_pop Optional named numeric vector of national populations
#'   keyed by `code`, e.g. `c(TGO = 8443000, GNB = 1968000)`. Adds
#'   `pop_reference` and `pop_coverage` columns.
#'
#' @return A data frame with one row per survey and columns:
#'   `code`, `year`, `survname`, `n_rows`, `n_cells`, `n_locations`,
#'   `max_rows_per_cell`, `n_tied_rows`, `pop_tied`, `pop_mapped`, and — when
#'   `reference_pop` is supplied — `pop_reference` and `pop_coverage` (a
#'   share of 1).
#'
#' @export
check_h3_population <- function(h3_map, reference_pop = NULL) {
  if (is.data.frame(h3_map)) h3_map <- list(h3_map)
  if (!is.list(h3_map) || length(h3_map) == 0) {
    stop("`h3_map` must be a data frame or a non-empty list of data frames.")
  }

  df <- dplyr::bind_rows(lapply(h3_map, as.data.frame))

  required <- c("code", "year", "survname", "loc_id", "h3", "pop_2020")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("`h3_map` is missing required column(s): ",
         paste(missing, collapse = ", "), ".")
  }
  if (nrow(df) == 0) stop("`h3_map` has no rows.")

  # Parquet writes these as decimals; everything below is arithmetic.
  df$pop_2020 <- as.numeric(df$pop_2020)

  keys  <- unique(df[, c("code", "year", "survname")])
  keys  <- keys[order(keys$code, keys$year, keys$survname), , drop = FALSE]

  out <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    k   <- keys[i, ]
    sub <- df[df$code == k$code & df$year == k$year &
                df$survname == k$survname, , drop = FALSE]
    cbind(k, .check_h3_population_one(sub), row.names = NULL)
  }))

  if (!is.null(reference_pop)) {
    out$pop_reference <- unname(reference_pop[as.character(out$code)])
    out$pop_coverage  <- out$pop_mapped / out$pop_reference
  }

  out
}


# One survey's worth of the summary above. `df` is already restricted to a
# single code / year / survname.
#' @noRd
.check_h3_population_one <- function(df) {
  # Rows sharing a location, a cell and a population: what a de-duplication
  # would throw away. Counted as "all but one of each tied group".
  tied <- df |>
    dplyr::count(.data$loc_id, .data$h3, .data$pop_2020, name = "k") |>
    dplyr::filter(.data$k > 1L)

  # A cell's population as seen from one location, then the richest such view
  # of each cell — a cell straddling several locations is otherwise counted
  # once per location it touches.
  per_cell <- df |>
    dplyr::group_by(.data$loc_id, .data$h3) |>
    dplyr::summarise(pop = sum(.data$pop_2020, na.rm = TRUE),
                     rows = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$h3) |>
    dplyr::summarise(pop = max(.data$pop, na.rm = TRUE),
                     rows = max(.data$rows), .groups = "drop")

  data.frame(
    n_rows            = nrow(df),
    n_cells           = dplyr::n_distinct(df$h3),
    n_locations       = dplyr::n_distinct(df$loc_id),
    max_rows_per_cell = if (nrow(per_cell) == 0) 0L else max(per_cell$rows),
    n_tied_rows       = as.integer(sum(tied$k - 1L)),
    pop_tied          = sum(tied$pop_2020 * (tied$k - 1L)),
    pop_mapped        = sum(per_cell$pop, na.rm = TRUE),
    stringsAsFactors  = FALSE
  )
}
