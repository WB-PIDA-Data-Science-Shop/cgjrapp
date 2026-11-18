# This file's presence stops shiny::runApp() from auto-sourcing the R/
# directory when Connect launches inst/app/cgjrapp_app.R (which loads the
# package itself with pkgload::load_all()). Without it, Connect warns
#   "Loading R/ subdirectory for Shiny application, but this directory
#   appears to contain an R package."
# and sources every file in R/ a second time. See ?shiny::loadSupport.
