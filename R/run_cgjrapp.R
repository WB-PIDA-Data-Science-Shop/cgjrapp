
#' Launch the CGJR Shiny application
#'
#' Starts the Growth and Jobs Institutional Review dashboard.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @export
run_cgjrapp <- function(...) {

  shiny::addResourcePath("assets", system.file("www", package = "cgjrapp"))
  thematic::thematic_shiny(font = "auto")

  # ── Theme ──────────────────────────────────────────────────────────────────
  app_theme <- bslib::bs_theme(
    bootswatch   = "litera",
    base_font    = bslib::font_google("Source Sans Pro"),
    code_font    = bslib::font_google("Source Sans Pro"),
    heading_font = bslib::font_google("Fira Sans"),
    navbar_bg    = "#FFFFFF"
  ) |>
    bslib::bs_add_rules(
      readLines(system.file("www", "styles.css", package = "cgjrapp"))
    )

  # ── Sidebar contents ────────────────────────────────────────────────────────
  app_sidebar <- bslib::sidebar(
    id    = "main_sidebar",
    title = "Filters",
    width = 280,

    shiny::selectInput(
      inputId  = "primary_iso",
      label    = "Country",
      choices  = get_country_choices(),
      selected = "GHA"
    ),

    shiny::selectizeInput(
      inputId  = "peer_isos",
      label    = "Peer Countries",
      choices  = get_country_choices(),
      selected = NULL,
      multiple = TRUE,
      options  = list(placeholder = "Select peer countries...", maxItems = 6L)
    ),

    shiny::hr(),

    shiny::selectizeInput(
      inputId  = "region_codes",
      label    = "Regions",
      choices  = get_region_choices(),
      selected = NULL,
      multiple = TRUE,
      options  = list(placeholder = "Select regions...")
    ),

    shiny::selectizeInput(
      inputId  = "income_groups",
      label    = "Income Groups",
      choices  = get_income_choices(),
      selected = NULL,
      multiple = TRUE,
      options  = list(placeholder = "Select income groups...")
    ),

    shiny::checkboxInput(
      inputId = "show_members",
      label   = "Show individual countries",
      value   = FALSE
    ),

    shiny::sliderInput(
      inputId = "year_range",
      label   = "Year Range",
      min     = 2013L,
      max     = 2024L,
      value   = c(2013L, 2024L),
      step    = 1L,
      sep     = ""
    ),

    shiny::hr(),

    shiny::selectInput(
      inputId  = "threshold_mode",
      label    = "Benchmarking Thresholds",
      choices  = list(
        Relative = list(
          "Quartiles (annual 25th / 50th)" = "rel_quartile",
          "Terciles  (annual 33rd / 67th)" = "rel_tercile"
        ),
        Absolute = list(
          "Quartiles (0.25 / 0.50)" = "abs_quartile",
          "Terciles  (0.33 / 0.67)" = "abs_tercile"
        )
      ),
      selected = "rel_quartile"
    )
  )

  # ── UI ──────────────────────────────────────────────────────────────────────
  ui <- bslib::page_navbar(
    title    = "govcgjr",
    fillable = FALSE,
    theme    = app_theme,
    navbar_options = bslib::navbar_options(underline = TRUE),
    padding  = "20px",

    # Tab 1 -- Welcome / Home (no sidebar)
    mod_welcome_ui("welcome"),

    # Tab 2 -- Overview (sidebar embedded inside this tab)
    mod_overview_ui("overview", sidebar = app_sidebar)
  )

  # ── Server ──────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    # Shared reactive wrappers — passed by reference to all modules
    r_primary_iso    <- shiny::reactive(input$primary_iso)
    r_peer_isos      <- shiny::reactive(input$peer_isos %||% character(0))
    r_region_codes   <- shiny::reactive(input$region_codes %||% character(0))
    r_income_groups  <- shiny::reactive(input$income_groups %||% character(0))
    r_year_range     <- shiny::reactive(input$year_range)
    r_threshold_mode <- shiny::reactive(input$threshold_mode)
    r_show_members   <- shiny::reactive(isTRUE(input$show_members))

    mod_welcome_server("welcome")

    mod_overview_server(
      id             = "overview",
      primary_iso    = r_primary_iso,
      peer_isos      = r_peer_isos,
      region_codes   = r_region_codes,
      income_groups  = r_income_groups,
      year_range     = r_year_range,
      threshold_mode = r_threshold_mode,
      show_members   = r_show_members
    )
  }

  shiny::shinyApp(ui, server, ...)
}