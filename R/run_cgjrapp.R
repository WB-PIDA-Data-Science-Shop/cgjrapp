#' Launch the CGJR Shiny application
#'
#' Starts the Country Jobs and Growth Report institutional benchmarking
#' dashboard.
#'
#' Tabs: Home (Slice 6, carried over); Benchmarking - the nested
#' `cgjr_taxonomy` accordion of live leaf cards ([mod_benchmark_dashboard_ui()],
#' Slices 0-2); Data Download ([mod_data_ui()], Slice 5); AI Report
#' ([mod_report_ui()], Slice 5). The sidebar selection is
#' [mod_country_selection_ui()] (Slice 3): base unit + comparator picker over
#' every `cgjr_ctf` unit, Apply gate, save/load.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @export
run_cgjrapp <- function(...) {

  app_theme <- bslib::bs_theme(
    bootswatch   = "litera",
    base_font    = bslib::font_google("Source Sans Pro"),
    heading_font = bslib::font_google("Fira Sans"),
    navbar_bg    = "#FFFFFF"
  )

  default_base <- "GHA"

  ui <- bslib::page_navbar(
    id = "pagenavbar", title = "CGJR", theme = app_theme,
    navbar_options = bslib::navbar_options(underline = TRUE),
    sidebar = bslib::sidebar(
      id = "main_sidebar", title = "Selection", width = 320,
      mod_country_selection_ui("selection", default_base = default_base)
    ),

    mod_welcome_ui("welcome"),

    bslib::nav_panel(
      title = "Benchmarking", value = "benchmarking",
      icon = shiny::icon("chart-simple"),
      mod_benchmark_dashboard_ui("bench", taxonomy = cgjrdata::cgjr_taxonomy)
    ),

    mod_data_ui("data"),
    mod_report_ui("report")
  )

  server <- function(input, output, session) {
    shiny::addResourcePath("assets", system.file("www", package = "cgjrapp"))
    thematic::thematic_shiny(font = "auto")
    install_benchmark_cache()

    mod_welcome_server("welcome")

    sel <- mod_country_selection_server("selection", default_base = default_base)

    shiny::observeEvent(input$pagenavbar, {
      bslib::sidebar_toggle("main_sidebar",
                            open = !identical(input$pagenavbar, "home"))
    }, ignoreInit = TRUE)

    mod_benchmark_dashboard_server(
      "bench",
      taxonomy         = cgjrdata::cgjr_taxonomy,
      base_unit        = sel$base_unit,
      comparison_units = sel$comparison_units,
      threshold        = sel$threshold,
      year_range       = sel$year_range
    )

    mod_data_server("data",
      base_unit = sel$base_unit, comparison_units = sel$comparison_units,
      year_range = sel$year_range
    )

    mod_report_server("report",
      base_unit = sel$base_unit, comparison_units = sel$comparison_units,
      threshold = sel$threshold, year_range = sel$year_range
    )
  }

  shiny::shinyApp(ui, server, ...)
}

#' Install the app-level cache the benchmark dashboard reads
#'
#' `mod_benchmark_dashboard_server()` wraps its per-selection computations in
#' [shiny::bindCache()], which resolves its store from
#' `shiny::getShinyOption("cache")`. This sets that to an LRU-evicted
#' in-memory cache shared across sessions, capped so a long-running deployment
#' can't grow without bound. Called from `run_cgjrapp()`'s server.
#'
#' @param max_mb Cache ceiling in MiB.
#' @return The cache object, invisibly.
#' @export
install_benchmark_cache <- function(max_mb = 250) {
  cache <- cachem::cache_mem(max_size = max_mb * 1024^2, evict = "lru")
  shiny::shinyOptions(cache = cache)
  invisible(cache)
}
