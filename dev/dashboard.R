# dev/dashboard.R — benchmark dashboard harness.
#
#   Rscript dev/dashboard.R      # launches the app on a random port
#
# Everything selection-related is hardcoded HERE, at the top of this dev
# script — never inside the module or the utils_family / plotting functions
# (build plan). Slice 3 replaces these with mod_country_selection.

pkgload::load_all(".", quiet = TRUE)
library(shiny)

# --- hardcoded dev selection ------------------------------------------------
DEV_BASE        <- "GHA"
DEV_COMPARATORS <- cgjrdata::cgjr_ctf |>
  dplyr::filter(unit_level == "country", unit_code != DEV_BASE) |>
  dplyr::distinct(unit_code) |>
  dplyr::arrange(unit_code) |>
  dplyr::pull(unit_code) |>
  head(40)
DEV_THRESHOLD  <- "Default"
DEV_YEAR_RANGE <- c(2013L, 2024L)

# Slice 1: the whole taxonomy, no leaf filter. Narrow this for fast iteration,
# e.g. dplyr::filter(cgjrdata::cgjr_taxonomy, cluster == "core_governance_functions")
DEV_TAXONOMY <- cgjrdata::cgjr_taxonomy

ui <- bslib::page_fillable(
  title = "CGJR benchmark dashboard (dev)",
  bslib::card(
    bslib::card_header(
      sprintf("Base: %s  |  %d comparators  |  %s thresholds  |  %d leaves",
              DEV_BASE, length(DEV_COMPARATORS), DEV_THRESHOLD, nrow(DEV_TAXONOMY))
    ),
    mod_benchmark_dashboard_ui("bench", taxonomy = DEV_TAXONOMY)
  )
)

server <- function(input, output, session) {
  mod_benchmark_dashboard_server(
    "bench",
    taxonomy         = DEV_TAXONOMY,
    base_unit        = reactive(DEV_BASE),
    comparison_units = reactive(DEV_COMPARATORS),
    threshold        = reactive(DEV_THRESHOLD),
    year_range       = reactive(DEV_YEAR_RANGE)
  )
}

shinyApp(ui, server, options = list(launch.browser = interactive()))
