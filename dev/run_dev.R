# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Comment this if you don't want the app to automatically open in the browser
# Set to a function that opens the system browser directly (bypassing the
# editor's viewer/callback, which has been mis-serving a static HTML
# snapshot instead of the live app URL).
options(shiny.launch.browser = function(url) {
  message("Open this URL manually if it doesn't launch: ", url)
  utils::browseURL(url, browser = "/usr/bin/open")
})

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
# golem::document_and_reload() #DRK Note - why did this not work for me?
devtools::document()
devtools::load_all()

# Run the application
#options(wise.debug = T)
run_app()
