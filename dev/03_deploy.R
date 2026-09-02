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

## IMPORTANT: the line above WILL re-add "sf" to manifest.json. {leaflet}
## hard-Imports sf (>= 0.9-6) on CRAN -- a real transitive dependency, not
## something .rscignore can hide -- and the Connect host cannot build sf. The
## app itself never touches {sf} at runtime (only batch/, which is excluded
## from appFiles, uses it), so sf is never actually loaded; Connect's install
## step just needs to not attempt it. After every writeManifest() run,
## re-strip it:
m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
m$packages[["sf"]] <- NULL
jsonlite::write_json(m, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
## Verify before committing: grep -c '"sf":' manifest.json  ->  should be 0
##
## NOTE: keep brand.yml in DESCRIPTION Imports. The static dependency scan
## cannot see it (bslib loads it dynamically for bs_theme(brand = ...)), so a
## regenerated manifest silently drops it when it sits in Suggests -- and the
## app aborts on Connect via rlang::check_installed("brand.yml"). See review
## report §10.1.
