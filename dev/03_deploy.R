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

## IMPORTANT (2026-09-02, MapLibre rollback): after every writeManifest() run,
## re-strip "sf" from manifest.json. The Connect host cannot build sf
## (GDAL 3.10.2 headers exist but the runtime libgdal.so.36 is missing, so
## configure fails; see the 2026-09-02 deploy incident). It is safe to drop sf
## because nothing in the executed app loads it:
##   - leaflet lists sf (>= 0.9-6) in Imports, but its NAMESPACE only
##     registers S3 methods for sf classes (no import(sf)), so the package
##     loads fine without sf installed;
##   - the mapgl/vector-basemap experiment was rolled back to leaflet for
##     exactly this reason: mapgl hard-Imports sf/terra/geojsonsf in both
##     DESCRIPTION and NAMESPACE (import(sf)/import(terra)/import(geojsonsf)),
##     which made the sf strip fatal for mapgl -- and per-feature styles are
##     available on leaflet via feature.properties.style anyway, so the
##     one-layer-per-map rendering survives the rollback.
##   - batch/ scripts do use sf but batch/ is excluded from appFiles.
## geojsonsf, classInt and the other mapgl-only deps drop out of the dep scan
## automatically once mapgl leaves DESCRIPTION Imports. terra STAYS: raster
## (a leaflet dependency) requires it and terra builds without GDAL.
m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
stopifnot(!is.null(m$packages[["sf"]]))
m$packages[["sf"]] <- NULL
jsonlite::write_json(m, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
## Verify before committing:
##   jq -r '.packages | has("sf")' manifest.json          ->  false
##   jq -r '.packages | has("mapgl")' manifest.json       ->  false
##   jq -r '.packages | has("brand.yml")' manifest.json   ->  true
##
## If the Connect admins later install the GDAL runtime (libgdal.so.36 +
## proj/geos) or we ever re-attempt a mapgl/deck.gl migration, the sf entry
## must come back -- and mapgl additionally needs terra and geojsonsf, with
## no possibility of stripping (see the 2026-08-28 review report history).
##
## NOTE: keep brand.yml in DESCRIPTION Imports. The static dependency scan
## cannot see it (bslib loads it dynamically for bs_theme(brand = ...)), so a
## regenerated manifest silently drops it when it sits in Suggests -- and the
## app aborts on Connect via rlang::check_installed("brand.yml"). See review
## report §10.1.
