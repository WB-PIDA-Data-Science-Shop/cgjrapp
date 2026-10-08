# ---
# Country / comparator selection page.
#
# Port of cliarappak::mod_benchmark.R's selection UI/logic (not its plots) -
# a base unit, a comparator set, threshold and year-range controls, an
# "Apply selection" gate, and save/load of the selection - as its own sidebar
# page (between Home and Benchmarking), laid out like the first half of
# cliarappak's benchmarking page (base country + save/load up top, a
# "Comparator countries" bs4Card, a "Benchmarking options" bs4Card). Left out deliberately: cliarappak's per-family
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

# ISO codes of real countries. cgjr_ctf tags 17 World Bank aggregates (AFE,
# AFW, EAS, HIC, NAC, ...) as unit_level == "country"; cliaretl::wb_country_list
# lists each aggregate under `group_code`, so its distinct `country_code`s are
# actual countries only.
.country_codes <- function(wb_list = cliaretl::wb_country_list) {
  unique(wb_list$country_code)
}

# grouped picker choices: names = unit_name, values = unit_code
.unit_choices <- function(ctf = cgjrdata::cgjr_ctf) {
  u <- dplyr::distinct(ctf, .data$unit_level, .data$unit_code, .data$unit_name)
  real <- .country_codes()
  mk <- function(lvl) {
    x <- u[u$unit_level == lvl & (lvl != "country" | u$unit_code %in% real), ]
    x <- x[order(x$unit_name), ]
    stats::setNames(x$unit_code, x$unit_name)
  }
  list(
    Countries       = mk("country"),
    Regions         = mk("region"),
    `Income groups` = mk("income_group")
  )
}

# Picker labels with a flag in front of each country (cliarappak parity: its
# `flags_with_countries`, flagcdn.com images keyed on the ISO2 code). Built
# from our ISO3 `unit_code`s rather than by matching names. Codes with no
# ISO2 flag (e.g. CHI, Channel Islands) and non-country units (regions,
# income groups) get the plain name. Returns HTML strings for
# shinyWidgets::pickerInput()'s `choicesOpt$content`, in `choices` order.
.flag_content <- function(choices) {
  codes <- unname(choices)
  labels <- htmltools::htmlEscape(names(choices))
  iso2 <- countrycode::countrycode(
    codes, "iso3c", "iso2c", warn = FALSE,
    custom_match = c(XKX = "XK", CHI = NA_character_)
  )
  ifelse(
    is.na(iso2),
    labels,
    sprintf('<img src="https://flagcdn.com/w20/%s.png" alt="%s"> %s',
            tolower(iso2), iso2, labels)
  )
}

# shinyWidgets::pickerInput() options shared by both pickers (cliarappak's)
.PICKER_OPTIONS <- list(`actions-box` = TRUE, `live-search` = TRUE)

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

  # WB aggregates (AFE, NAC, HIC, ...) leak into cgjr_ctf as "country" rows;
  # only codes in .country_codes() are real countries. AFE/AFW/NAC also have a
  # genuine region row, which is resolved below.
  country_codes <- u$unit_code[u$unit_level == "country" & u$unit_code %in% .country_codes()]
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
#' @return A `shiny::tagList` - the content of the `"selection"`
#'   [bs4Dash::tabItem()].
#' @export
mod_country_selection_ui <- function(id, default_base = "GHA",
                                     ctf = cgjrdata::cgjr_ctf) {
  ns <- shiny::NS(id)
  choices <- .unit_choices(ctf)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        style = "padding-left: 24px",
        shinyWidgets::pickerInput(
          ns("base"),
          shiny::tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
          choices = choices$Countries,
          choicesOpt = list(content = .flag_content(choices$Countries)),
          selected = default_base, multiple = TRUE, width = "100%",
          options = c(.PICKER_OPTIONS, list(title = "One or more countries"))
        )
      ),
      shiny::column(
        width = 4,
        shiny::tags$span("Selection of Countries",
                         style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
        shiny::tags$div(
          class = "d-flex align-items-start",
          shiny::downloadButton(ns("save_sel"), "Save",
                                class = "save_inputs_btn mr-2"),
          shiny::fileInput(ns("load_sel"), NULL, accept = ".rds",
                           buttonLabel = "Load...", placeholder = "")
        )
      )
    ),

    shiny::fluidRow(style = "height: 5px;"),

    bs4Dash::bs4Card(
      title = "Comparator countries",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      shinyWidgets::pickerInput(
        ns("comparators"), "Comparators",
        choices = choices,
        # content follows the flattened choices: countries, regions, income groups
        choicesOpt = list(content = .flag_content(unlist(unname(choices)))),
        selected = .default_comparators(default_base),
        multiple = TRUE, width = "100%",
        options = c(.PICKER_OPTIONS, list(
          title = "Countries, regions, income groups",
          `selected-text-format` = "count > 6"
        ))
      ),
      shiny::helpText(
        "Picking a region or income group adds its member countries as ",
        "individual comparators - the group itself is never used as a ",
        "single comparator unit."
      ),
      shiny::helpText(shiny::textOutput(ns("comp_count"), inline = TRUE))
    ),

    bs4Dash::bs4Card(
      title = "Benchmarking options",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            ns("threshold"), "Benchmarking thresholds",
            choices = c("Default (25th / 50th pctile)" = "Default",
                        "Terciles (33rd / 66th)"       = "Terciles")
          )
        ),
        shiny::column(
          width = 6,
          shiny::sliderInput(
            ns("year_range"), "Year range (dynamic view, even years only)",
            min = .YEAR_MIN, max = .YEAR_MAX, value = c(.YEAR_MIN, .YEAR_MAX),
            step = 1L, sep = ""
          )
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
        shinyWidgets::updatePickerInput(session, "comparators", selected = expanded)
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
        # bs4Dash's actionButton(), not shiny's: shiny's always adds
        # btn-default, which overrides btn-primary under AdminLTE
        bs4Dash::actionButton(ns("apply"), "Apply selection",
                              status = "primary", size = "lg",
                              icon = shiny::icon("check"))
      } else {
        bs4Dash::actionButton(ns("apply"),
                              "Pick a base unit and 2+ comparators",
                              status = "secondary", size = "lg", disabled = TRUE)
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
      shinyWidgets::updatePickerInput(session, "base", selected = s$base)
      shinyWidgets::updatePickerInput(session, "comparators", selected = s$comparators)
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
