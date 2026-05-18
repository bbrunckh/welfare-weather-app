#' Group locations into panel IDs based on H3 cell overlap
#'
#' @description
#' Assigns a panel group ID to each location using connected components of a
#' graph where two locations are linked when their H3 cell overlap meets or
#' exceeds \code{threshold} in \emph{both} directions (mutual overlap).
#' Overlap can be unweighted (cell counts) or weighted by a numeric column.
#' Transitivity applies: if A–B and B–C both pass, all three are grouped.
#'
#' @param data A data frame or lazy frame with at least \code{id_col} and
#'   \code{h3_col} columns. Lazy frames are collected after all computation.
#' @param id_col Unquoted column name of the location IDs (e.g. \code{loc_id}).
#' @param h3_col Unquoted column name of the H3 cell identifiers.
#' @param threshold Numeric in \code{(0, 1]}. Minimum overlap fraction required
#'   in \emph{both} directions for two locations to be linked. Default
#'   \code{0.5}.
#' @param weight_col Optional unquoted name of a numeric column used as weights.
#'   If \code{NULL} (default), each H3 cell has equal weight of 1.
#' @param group_cols Optional character vector of column names that jointly
#'   scope \code{id_col} uniqueness (e.g. \code{c("code", "year", "survname")}
#'   when \code{loc_id} is only unique within a survey). A composite key is
#'   created internally so panel IDs are globally unique across groups. The
#'   returned tibble includes the group columns alongside \code{loc_id} and
#'   \code{loc_id_panel}.
#'
#' @return A tibble with one row per location and columns:
#'   \describe{
#'     \item{group columns}{Only present when \code{group_cols} is supplied.}
#'     \item{\code{loc_id}}{Original location identifier (same type as input).}
#'     \item{\code{loc_id_panel}}{Integer group ID. Isolated locations each
#'       receive a unique ID.}
#'   }
#'
#' @details
#' For each candidate pair (X, Y):
#' \deqn{\text{overlap}_X = \frac{\sum_{c \in X \cap Y} w_{X,c}}{\sum_{c \in X} w_{X,c}}}
#' and symmetrically for Y. The pair is linked when
#' \eqn{\min(\text{overlap}_X, \text{overlap}_Y) \geq \text{threshold}}.
#' Groups are then derived via
#' \code{\link[igraph:components]{igraph::components}} on the resulting
#' undirected graph; all locations appear in the output even if isolated.
#'
#' @seealso \code{\link[igraph:components]{igraph::components}},
#'   \code{\link[igraph:graph_from_data_frame]{igraph::graph_from_data_frame}}
#'
#' @examples
#' library(dplyr)
#'
#' survey <- tibble::tribble(
#'   ~loc_id, ~h3_7,  ~population,
#'   1L,      "a",    100,
#'   1L,      "b",    200,
#'   2L,      "b",    150,
#'   2L,      "c",    50,
#'   3L,      "d",    300
#' )
#'
#' loc_panel(survey, id_col = loc_id, h3_col = h3_7)
#' loc_panel(survey, id_col = loc_id, h3_col = h3_7, weight_col = population)
#'
#' # Multi-survey: loc_id only unique within (code, year, survname)
#' survey_multi <- dplyr::bind_rows(
#'   dplyr::mutate(survey, code = "GNB", year = 2021L, survname = "EHCVM"),
#'   dplyr::mutate(survey, code = "SEN", year = 2021L, survname = "EHCVM")
#' )
#' loc_panel(survey_multi, id_col = loc_id, h3_col = h3_7,
#'           group_cols = c("code", "year", "survname"))
#'
#' @importFrom dplyr select distinct mutate summarise inner_join left_join
#'   filter rename
#' @importFrom tibble tibble
#' @importFrom igraph graph_from_data_frame add_vertices components
#'
#' @export
loc_panel <- function(data,
                      id_col,
                      h3_col,
                      threshold  = 0.5,
                      weight_col = NULL,
                      group_cols = NULL) {

  wt_quo     <- rlang::enquo(weight_col)
  has_groups <- !is.null(group_cols) && length(group_cols) > 0

  # --- 0. If group_cols supplied, stamp a composite loc_id ------------------
  # paste(!!!syms(...), ...) is translatable to SQL by DuckDB/dbplyr.
  if (has_groups) {
    actual_cols  <- colnames(data)
    missing_cols <- setdiff(group_cols, actual_cols)
    if (length(missing_cols) > 0) {
      stop("group_cols not found in data: ", paste(missing_cols, collapse = ", "))
    }
    grp_syms <- rlang::syms(group_cols)

    # Restore key: maps composite string back to (group_cols, original loc_id)
    restore_key <- data |>
      dplyr::select(dplyr::all_of(group_cols), loc_id_orig = {{ id_col }}) |>
      dplyr::distinct() |>
      dplyr::mutate(loc_id_composite = paste(!!!grp_syms, loc_id_orig, sep = "__")) |>
      dplyr::collect()

    data   <- data |>
      dplyr::mutate(.loc_id_composite = paste(!!!grp_syms, {{ id_col }}, sep = "__"))
    id_sym <- rlang::sym(".loc_id_composite")
  } else {
    id_sym <- rlang::ensym(id_col)
  }

  # --- 1. Build a distinct (id, h3, weight) base table -----------------------
  if (!rlang::quo_is_null(wt_quo)) {
    base <- data |>
      dplyr::select(loc_id = !!id_sym,
                    h3     = {{ h3_col }},
                    weight = {{ weight_col }}) |>
      dplyr::summarise(weight = sum(weight, na.rm = TRUE),
                       .by = c(loc_id, h3))
  } else {
    base <- data |>
      dplyr::select(loc_id = !!id_sym,
                    h3     = {{ h3_col }}) |>
      dplyr::distinct() |>
      dplyr::mutate(weight = 1L)
  }

  # --- 2. Total weight per location ------------------------------------------
  totals <- base |>
    dplyr::summarise(total = sum(weight), .by = loc_id)

  # --- 3. Candidate pairs: loc_ids sharing ≥1 h3 cell -----------------------
  pairs <- dplyr::inner_join(base, base,
                             by           = "h3",
                             suffix       = c("_x", "_y"),
                             relationship = "many-to-many") |>
    dplyr::filter(loc_id_x != loc_id_y)

  # --- 4. Overlap in both directions (all still lazy / in DuckDB) ------------
  edges <- pairs |>
    dplyr::summarise(
      shared_x = sum(weight_x),
      shared_y = sum(weight_y),
      .by = c(loc_id_x, loc_id_y)
    ) |>
    dplyr::left_join(totals, by = c("loc_id_x" = "loc_id")) |>
    dplyr::rename(total_x = total) |>
    dplyr::left_join(totals, by = c("loc_id_y" = "loc_id")) |>
    dplyr::rename(total_y = total) |>
    dplyr::mutate(
      overlap_x = shared_x / total_x,
      overlap_y = shared_y / total_y
    ) |>
    dplyr::filter(pmin(overlap_x, overlap_y) >= threshold) |>
    dplyr::select(from = loc_id_x, to = loc_id_y)

  # --- 5. Collect — single trip to DuckDB for both edges and all IDs ---------
  edges_df <- dplyr::collect(edges)
  ids_df   <- totals |>
    dplyr::select(loc_id) |>
    dplyr::collect()

  all_ids_chr <- as.character(ids_df$loc_id)

  # --- 6. Build graph, find connected components -----------------------------
  g <- igraph::graph_from_data_frame(
    d        = edges_df,
    directed = FALSE,
    vertices = data.frame(name = all_ids_chr)
  )

  comp <- igraph::components(g)

  # --- 7. Map components back to (composite) loc_id -------------------------
  panel_map <- tibble::tibble(
    loc_id_composite = names(comp$membership),
    loc_id_panel     = unname(comp$membership)
  )

  # --- 8. Restore original loc_id (and group cols if supplied) ---------------
  if (has_groups) {
    panel_map |>
      dplyr::left_join(restore_key, by = "loc_id_composite") |>
      dplyr::select(dplyr::all_of(group_cols),
                    loc_id = loc_id_orig,
                    loc_id_panel)
  } else {
    panel_map |>
      dplyr::left_join(
        dplyr::mutate(ids_df, loc_id_composite = as.character(loc_id)),
        by = "loc_id_composite"
      ) |>
      dplyr::select(loc_id, loc_id_panel)
  }
}