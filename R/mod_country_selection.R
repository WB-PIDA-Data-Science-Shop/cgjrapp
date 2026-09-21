# ---
# Country / comparator selection page.
#
# Port of cliarappak::mod_benchmark.R's selection UI/logic (not its plots) -
# a base unit, a comparator set, threshold and year-range controls, an
# "Apply selection" gate, and save/load of the selection - as its own nav
# page (between Home and Benchmarking), card-laid-out like the first half of
# cliarappak's benchmarking page (base unit + save/load up top, a "Comparator
# countries" card, a "Benchmarking options" card), rather than cliarappak's
# always-visible sidebar. Left out deliberately: cliarappak's per-family
# picker (cgjrapp's Benchmarking tab already shows the whole taxonomy at
# once, so there's nothing to pick between), custom comparison groups, the
# separate "groups" shortcut + toggle-revealed country checklist widgets,
# guided tour, and rank-instead-of-value/best-to-worst toggles - none of
# which exist anywhere else in cgjrapp either.
#
# Two pieces ARE now cliarappak parity (2026-09-21), not a departure:
#   - Base unit is multi-select, restricted to actual countries - never a
#     region/income-group aggregate row - matching cliarappak's `country`
#     picker exactly.
#   - Comparators stay a single combined picker over countries/regions/
#     income groups (a deliberate simplification of cliarappak's two-widget
#     "groups" + checkbox-list UI), but picking a region or income group is
#     purely a shortcut: .expand_to_countries() immediately swaps it for its
#     member country codes, so a region is NEVER passed to the methodology
#     layer as a single atomic comparator - only real countries are, exactly
#     as cliarappak's `observeEvent(input$groups, ...)` does for its
#     `countries` checkbox list.
#
# Returns a "bench"-style list of Apply-gated reactives that
# mod_benchmark_dashboard_server() consumes.
# ---

# grouped picker choices: names = unit_name, values = unit_code
.unit_choices <- function(ctf = cgjrdata::cgjr_ctf) {
  u <- dplyr::distinct(ctf, .data$unit_level, .data$unit_code, .data$unit_name)
  mk <- function(lvl, drop = character(0)) {
    x <- u[u$unit_level == lvl & !u$unit_code %in% drop, ]
    x <- x[order(x$unit_name), ]
    stats::setNames(x$unit_code, x$unit_name)
  }
  list(
    Countries       = mk("country", drop = c("AFE", "AFW")),  # WB aggregates leak in as "countries"
    Regions         = mk("region"),
    `Income groups` = mk("income_group")
  )
}

# a sensible default comparator set: the base country's WB region-mates
.default_comparators <- function(base_iso, wb = cgjrdata::wbcountries) {
  reg <- wb$region_code[wb$country_code == base_iso]
  if (length(reg) == 0L || is.na(reg[[1]])) return(character(0))
  setdiff(wb$country_code[!is.na(wb$region_code) & wb$region_code == reg[[1]]], base_iso)
}

# Expand any region / income-group unit_codes in `codes` into their member
# country codes; country codes pass through unchanged. A group is always a
# shortcut, never a comparator unit in its own right (cliarappak parity: its
# `groups` picker only ever pre-checks `countries`, and `def_quantiles()` is
# always fed individual countries). Selecting several groups unions their
# members, matching cliarappak's own "union of the groups" semantics.
#
# Region unit_codes in cgjr_ctf equal wbcountries$region_code verbatim
# (e.g. "AFE"); income-group unit_codes are a slug ("high_income") that only
# matches wbcountries$income_group via the display label ("High income"), so
# income groups are matched on unit_name, not unit_code.
.expand_to_countries <- function(codes, ctf = cgjrdata::cgjr_ctf,
                                 wb = cgjrdata::wbcountries) {
  codes <- unique(codes)
  if (length(codes) == 0L) return(character(0))

  u <- dplyr::distinct(ctf, .data$unit_level, .data$unit_code, .data$unit_name)
  u <- u[u$unit_code %in% codes, ]

  # AFE/AFW leak into cgjr_ctf as BOTH a region row and a country row (the
  # same "WB aggregates leak in as countries" issue .unit_choices() already
  # drops from its Countries list) - always resolve them as regions here.
  country_codes <- u$unit_code[u$unit_level == "country" & !u$unit_code %in% c("AFE", "AFW")]
  region_codes  <- u$unit_code[u$unit_level == "region"]
  income_labels <- u$unit_name[u$unit_level == "income_group"]

  unique(c(
    country_codes,
    wb$country_code[!is.na(wb$region_code) & wb$region_code %in% region_codes],
    wb$country_code[!is.na(wb$income_group) & wb$income_group %in% income_labels]
  ))
}

.SELECTION_FIELDS <- c("base", "comparators", "threshold", "year_range")
.YEAR_MIN <- 2013L
.YEAR_MAX <- 2024L

#' Country / comparator selection page UI
#'
#' @param id Module id.
#' @param default_base Character `unit_code` to pre-select as the base unit.
#' @param ctf Long CTF tibble the picker choices are built from.
#' @return A [bslib::nav_panel()].
#' @export
mod_country_selection_ui <- function(id, default_base = "GHA",
                                     ctf = cgjrdata::cgjr_ctf) {
  ns <- shiny::NS(id)
  choices <- .unit_choices(ctf)

  bslib::nav_panel(
    title = "Country Selection", icon = shiny::icon("flag"), value = "selection",

    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::selectizeInput(
        ns("base"), "Base unit(s)",
        choices = choices$Countries, selected = default_base, multiple = TRUE,
        options = list(placeholder = "One or more countries")
      ),
      shiny::tags$div(
        class = "d-flex align-items-end h-100 gap-2 pb-3",
        shiny::downloadButton(ns("save_sel"), "Save",
                              class = "btn-sm btn-outline-secondary"),
        shiny::tags$div(
          class = "flex-grow-1",
          shiny::fileInput(ns("load_sel"), NULL, accept = ".rds",
                           buttonLabel = "Load...", placeholder = "")
        )
      )
    ),

    bslib::card(
      bslib::card_header("Comparator countries"),
      shiny::selectizeInput(
        ns("comparators"), "Comparators",
        choices = choices, selected = .default_comparators(default_base),
        multiple = TRUE, width = "100%",
        options = list(placeholder = "Countries, regions, income groups")
      ),
      shiny::helpText(
        "Picking a region or income group adds its member countries as ",
        "individual comparators - the group itself is never used as a ",
        "single comparator unit."
      ),
      shiny::helpText(shiny::textOutput(ns("comp_count"), inline = TRUE))
    ),

    bslib::card(
      bslib::card_header("Benchmarking options"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput(
          ns("threshold"), "Benchmarking thresholds",
          choices = c("Default (25th / 50th pctile)" = "Default",
                      "Terciles (33rd / 66th)"       = "Terciles")
        ),
        shiny::sliderInput(
          ns("year_range"), "Year range (dynamic view, even years only)",
          min = .YEAR_MIN, max = .YEAR_MAX, value = c(.YEAR_MIN, .YEAR_MAX),
          step = 1L, sep = ""
        )
      )
    ),

    shiny::uiOutput(ns("apply_ui"))
  )
}

#' Country / comparator selection module server
#'
#' @param id Module id (must match the UI).
#' @param default_base Character `unit_code` - must match the UI's
#'   `default_base`; seeds the pre-Apply state so the dashboard renders
#'   immediately.
#' @return A named list of reactives: `base_unit` (character vector of
#'   country `unit_code`s - one or more, never a region/income-group
#'   aggregate), `comparison_units` (character vector of country
#'   `unit_code`s, base removed, any region/income group already expanded to
#'   its members), `threshold`, `year_range`, and `applied` (the Apply click
#'   count). `base_unit` ... `year_range` hold the default selection until
#'   "Apply" is clicked, then update only on each subsequent click.
#' @export
mod_country_selection_server <- function(id, default_base = "GHA") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Regions / income groups picked as comparators are shortcuts, never
    # comparator units themselves - .expand_to_countries() swaps them for
    # their member countries wherever the comparator set is actually
    # consumed (comps_live() below), so correctness never depends on the
    # widget round-trip. The observer just keeps the widget's own display in
    # sync for the user (cliarappak's `groups` -> `countries` sync); it's a
    # UX nicety only - shiny::testServer()'s MockShinySession has no
    # sendInputMessage(), so update*Input() calls are inert there, which is
    # exactly why the real expansion can't live behind this observer alone.
    shiny::observeEvent(input$comparators, {
      sel      <- input$comparators %||% character(0)
      expanded <- setdiff(.expand_to_countries(sel), input$base %||% character(0))
      if (!setequal(expanded, sel)) {
        shiny::updateSelectizeInput(session, "comparators", selected = expanded)
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    comps_live <- shiny::reactive(
      setdiff(.expand_to_countries(input$comparators %||% character(0)),
              input$base %||% character(0))
    )
    base_ok <- shiny::reactive(length(input$base) >= 1L && all(nzchar(input$base)))
    ready   <- shiny::reactive(base_ok() && length(comps_live()) >= 2L)

    output$comp_count <- shiny::renderText({
      n <- length(comps_live())
      if (n == 0L) "No comparators selected."
      else if (n < 10L) sprintf("%d comparator%s - rankings steadier with 10+.",
                                n, if (n == 1L) "" else "s")
      else sprintf("%d comparators.", n)
    })

    output$apply_ui <- shiny::renderUI({
      if (ready()) {
        shiny::actionButton(ns("apply"), "Apply selection",
                            class = "btn-primary btn-lg", icon = shiny::icon("check"))
      } else {
        shiny::actionButton(ns("apply"),
                            "Pick a base unit and 2+ comparators",
                            class = "btn-secondary btn-lg", disabled = TRUE)
      }
    })

    # Applied state: seeded with the default selection, then overwritten only
    # when "Apply" is clicked. (A reactiveValues + observeEvent, rather than
    # eventReactive, so the gate holds identically under shiny::testServer.)
    applied_state <- shiny::reactiveValues(
      base_unit        = default_base,
      comparison_units = .default_comparators(default_base),
      threshold        = "Default",
      year_range       = c(.YEAR_MIN, .YEAR_MAX)
    )
    shiny::observeEvent(input$apply, {
      applied_state$base_unit        <- input$base
      applied_state$comparison_units <- comps_live()
      applied_state$threshold        <- input$threshold %||% "Default"
      applied_state$year_range       <- input$year_range
    }, ignoreInit = TRUE)

    # --- save / load ---
    output$save_sel <- shiny::downloadHandler(
      filename = function() "cgjr_selection.rds",
      content = function(file) {
        saveRDS(list(
          base = input$base, comparators = input$comparators,
          threshold = input$threshold, year_range = input$year_range
        ), file)
      }
    )

    shiny::observeEvent(input$load_sel, {
      f <- input$load_sel
      shiny::req(f)
      s <- tryCatch(readRDS(f$datapath), error = function(e) NULL)
      if (!is.list(s) || !all(.SELECTION_FIELDS %in% names(s))) {
        shiny::showNotification("Not a valid CGJR selection file.", type = "error")
        return()
      }
      shiny::updateSelectizeInput(session, "base", selected = s$base)
      shiny::updateSelectizeInput(session, "comparators", selected = s$comparators)
      shiny::updateSelectInput(session, "threshold", selected = s$threshold)
      shiny::updateSliderInput(session, "year_range", value = s$year_range)
    })

    list(
      base_unit        = shiny::reactive(applied_state$base_unit),
      comparison_units = shiny::reactive(applied_state$comparison_units),
      threshold        = shiny::reactive(applied_state$threshold),
      year_range       = shiny::reactive(applied_state$year_range),
      applied          = shiny::reactive(input$apply)
    )
  })
}
