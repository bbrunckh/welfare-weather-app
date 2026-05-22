# =============================================================================
# batch/R/aaa_load.R
#
# Loads packages used only in batch scripts (not part of the main Shiny app).
# Named aaa_load.R so it sorts first and runs before other batch/R/ files.
# Sourced automatically via the list.files("batch/R", ...) call at the top of
# each batch script. Install with:
#   install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "collapse", "arrow"))
# =============================================================================

.batch_pkgs <- c(
  sf                 = "sf",
  rnaturalearth      = "rnaturalearth",
  rnaturalearthdata  = "rnaturalearthdata",
  collapse           = "collapse",
  arrow              = "arrow"
)

.missing <- names(Filter(Negate(requireNamespace), .batch_pkgs))
if (length(.missing) > 0) {
  stop(
    "Missing batch-only package(s): ", paste(.missing, collapse = ", "), "\n",
    "Install with: install.packages(c(",
    paste0('"', .missing, '"', collapse = ", "), "))",
    call. = FALSE
  )
}

library(sf,                warn.conflicts = FALSE)
library(rnaturalearth,     warn.conflicts = FALSE)
library(rnaturalearthdata, warn.conflicts = FALSE)
library(collapse,         warn.conflicts = FALSE) 
library(arrow,            warn.conflicts = FALSE)

# s2 spherical geometry causes spurious topology errors on natural earth data
sf::sf_use_s2(FALSE)
