# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv()
## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
golem::add_positconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----

## In command line.
rsconnect::deployApp(
	appName = desc::desc_get_field("Package"),
	appTitle = desc::desc_get_field("Package"),
	appFiles = c(
		# Add any additional files unique to your app here.
		"R/",
		"inst/",
		"data/",
		"NAMESPACE",
		"DESCRIPTION",
		"app.R"
	),
	appId = rsconnect::deployments(".")$appID,
	lint = FALSE,
	forceUpdate = TRUE
)

## Posit Connect, git-backed ----
## Connect pulls straight from this GitHub repo (poll or "Update Now") rather
## than a push-button rsconnect::deployApp() bundle. That path requires a
## manifest.json committed at the repo root (app.R runs via pkgload::load_all(),
## not library(), since wiseapp is never installed into Connect's library).
## Re-run this and commit the diff whenever dependencies or the shipped file
## set change:
rsconnect::writeManifest(
  appDir = ".",
  appFiles = c("app.R", "R", "inst", "man", "DESCRIPTION", "NAMESPACE"),
  appPrimaryDoc = "app.R"
)

## IMPORTANT (updated 2026-09-03, Leaflet removal): re-run writeManifest()
## whenever dependencies or the shipped file set change, and commit the diff.
## The dependency scan no longer includes sf at all: it only entered the
## manifest as a leaflet dependency, and leaflet left DESCRIPTION Imports
## when the Leaflet fallback was removed (the maps are MapLibre-only, with
## vendored inst/app/www/vendor/ assets that ship inside inst/). The strip
## below stays as a harmless safety net for stale manifests.
##
## GOTCHA (2026-09-03): if a stale wiseapp INSTALL is present in the local
## library (from the Leaflet era), the scan resolves `wiseapp` from the
## installed DESCRIPTION -- re-importing leaflet/sf/raster/terra into the
## manifest even though the source DESCRIPTION is clean. Uninstall it first
## (remove.packages("wiseapp")); the app runs via pkgload::load_all() both
## locally and on Connect, so an install is never needed.
## SUGGESTS ARE SWEPT (2026-09-03): writeManifest() includes every package in
## the app DESCRIPTION's Suggests field, plus their dependency closures. Keep
## Suggests to runtime-optional features only (model backends parsnip /
## ranger / xgboost live in Imports now); dev/test-only packages (covr,
## testthat, arrow, bit64, spelling) were dropped so they leave the manifest.
m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
m$packages[["sf"]] <- NULL
jsonlite::write_json(m, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
## Verify before committing:
##   jq -r '.packages | has("sf")' manifest.json           ->  false
##   jq -r '.packages | has("leaflet")' manifest.json      ->  false
##   jq -r '.packages | has("mapgl")' manifest.json        ->  false
##   jq -r '.packages | has("brand.yml")' manifest.json    ->  true
##
## If the Connect admins later install the GDAL runtime (libgdal.so.36 +
## proj/geos), sf may re-enter the manifest through any future dependency
## that Imports it -- re-check the strip then. The 2026-08-28 mapgl/deck.gl
## experiment history is in the review report §10.1.
##
## NOTE: keep brand.yml in DESCRIPTION Imports. The static dependency scan
## cannot see it (bslib loads it dynamically for bs_theme(brand = ...)), so a
## regenerated manifest silently drops it when it sits in Suggests -- and the
## app aborts on Connect via rlang::check_installed("brand.yml"). See review
## report §10.1.
