# ============================================================================ #
# globals.R                                                                    #
# Package-wide import declarations and data-masking bindings.                  #
# ============================================================================ #

#' Package-wide imports and dynamically created column names
#'
#' Base/stats helpers used unqualified across the package, plus column names
#' created dynamically inside dplyr/dbplyr data-masking pipelines (these are
#' invisible to static code checks and DuckDB SQL translation handles the
#' spatial/h3 functions directly in the database).
#'
#' @importFrom graphics plot.new title
#' @importFrom stats coef complete.cases model.matrix reorder setNames vcov
#' @importFrom utils combn head
#' @importFrom rlang .data
#' @name wise-package-globals
#' @keywords internal
NULL

utils::globalVariables(c(
  ".resid", "Estimate", "Group", "Proportion", "Value", "bin_index",
  "channel", "count", "countryyear", "decile", "economy", "est",
  "estimate", "conf.high", "conf.low", "fname", "geom",
  "h3_cell_to_boundary_wkt", "h3_weather", "label", "label_wrap",
  "loc_id", "loc_id_orig", "loc_id_panel", "loc_id_x", "loc_id_y",
  "model", "model_label", "modx", "modx_label", "month", "month_num",
  "n_obs", "name", "overlap_x", "overlap_y", "pct", "period_lbl",
  "plot.new", "pop_2020", "prop", "ref_mean", "ref_sd", "scenario", "se",
  "shared_x", "shared_y", "ssp", "st_asgeojson", "st_geomfromtext",
  "st_union_agg", "survname", "tau", "timestamp", "title", "total",
  "total_x", "total_y", "value", "value_p50", "variable",
  "variable_label", "weight", "weight_x", "weight_y", "welfare", "year",
  ":="
))
