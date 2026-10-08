#' Launch the CGJR Shiny application
#'
#' Starts the Country Jobs and Growth Report institutional benchmarking
#' dashboard.
#'
#' The shell is a port of `cliarappak`'s `app_ui()`: a [bs4Dash::dashboardPage()]
#' with the same header, the same 350px light sidebar menu, and `cliarappak`'s
#' `styles.css` (shipped verbatim as `inst/www/styles.css`; cgjrapp's own
#' additions live in `inst/www/cgjr.css`), so fonts, sizes and colours match
#' CLIAR's. Sidebar items, in `cliarappak`'s order: Home
#' ([mod_welcome_ui()]); Country Selection ([mod_country_selection_ui()]) - base
#' unit + comparator picker, Apply gate, save/load; Benchmarking - the nested
#' `cgjr_taxonomy` accordion of live leaf cards
#' ([mod_benchmark_dashboard_ui()]); Data ([mod_data_ui()]); AI Report
#' ([mod_report_ui()]); then the Methodology & User Guide, Terms of use and
#' Disclaimers, FAQ and Contact Us placeholders ([mod_placeholder_ui()]) and a
#' Source code link.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @export
run_cgjrapp <- function(...) {

  shiny::addResourcePath("assets", system.file("www", package = "cgjrapp"))

  default_base <- "GHA"

  ui <- shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "assets/styles.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "assets/cgjr.css")
    ),
    bs4Dash::dashboardPage(
      freshTheme = fresh::create_theme(fresh::bs4dash_layout(sidebar_width = "350px")),

      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(title = "GOVCGJR Benchmarking Dashboard"),
        status = "white",
        border = TRUE,
        sidebarIcon = shiny::icon("bars"),
        controlbarIcon = shiny::icon("th"),
        fixed = FALSE
      ),

      bs4Dash::dashboardSidebar(
        status = "info",
        skin = "light",
        elevation = 5,
        bs4Dash::sidebarMenu(
          id = "sidebar",
          bs4Dash::menuItem("Home", tabName = "home", icon = shiny::icon("home")),
          bs4Dash::menuItem("Country Selection", tabName = "selection",
                            icon = shiny::icon("flag")),
          bs4Dash::menuItem("Benchmarking", tabName = "benchmarking",
                            icon = shiny::icon("sort-amount-up")),
          bs4Dash::menuItem("Data", tabName = "data", icon = shiny::icon("table")),
          bs4Dash::menuItem("AI Report", tabName = "report", icon = shiny::icon("robot")),
          bs4Dash::menuItem("Methodology & User Guide", tabName = "methodology_ug",
                            icon = shiny::icon("book")),
          bs4Dash::menuItem("Terms of use and Disclaimers", tabName = "terms",
                            icon = shiny::icon("handshake")),
          bs4Dash::menuItem("FAQ", tabName = "faq", icon = shiny::icon("question")),
          bs4Dash::menuItem("Contact Us", tabName = "contact",
                            icon = shiny::icon("comments", lib = "font-awesome")),
          bs4Dash::menuItem("Source code",
                            icon = shiny::icon("github", lib = "font-awesome"),
                            href = "https://github.com/WB-PIDA-Data-Science-Shop/cgjrapp")
        )
      ),

      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem("home", mod_welcome_ui("welcome")),
          bs4Dash::tabItem("selection",
            mod_country_selection_ui("selection", default_base = default_base)),
          bs4Dash::tabItem("benchmarking",
            mod_benchmark_dashboard_ui("bench", taxonomy = cgjrdata::cgjr_taxonomy)),
          bs4Dash::tabItem("data", mod_data_ui("data")),
          bs4Dash::tabItem("report", mod_report_ui("report")),
          bs4Dash::tabItem("methodology_ug",
            mod_placeholder_ui("methodology_ug", "Methodology & User Guide")),
          bs4Dash::tabItem("terms",
            mod_placeholder_ui("terms", "Terms of use and Disclaimers")),
          bs4Dash::tabItem("faq", mod_placeholder_ui("faq", "FAQ")),
          bs4Dash::tabItem("contact", mod_placeholder_ui("contact", "Contact Us"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    thematic::thematic_shiny(font = "auto")
    install_benchmark_cache()

    mod_welcome_server("welcome")

    sel <- mod_country_selection_server("selection", default_base = default_base)

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
