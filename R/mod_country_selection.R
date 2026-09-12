# ---
# Country / comparator selection module.
#
# Port of cliarappak::mod_benchmark.R's selection UI/logic (not its plots):
# a base unit, a comparator set, threshold and year-range controls, an
# "Apply selection" gate, and save/load of the selection. Unlike cliarappak,
# countries / regions / income groups are equal citizens in one picker over
# distinct(cgjr_ctf, unit_level, unit_code, unit_name).
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

.SELECTION_FIELDS <- c("base", "comparators", "threshold", "year_range")
.YEAR_MIN <- 2013L
.YEAR_MAX <- 2024L

#' Country / comparator selection module UI
#'
#' @param id Module id.
#' @param default_base Character `unit_code` to pre-select as the base unit.
#' @param ctf Long CTF tibble the picker choices are built from.
#' @return A [shiny::tagList] intended for a [bslib::sidebar()].
#' @export
mod_country_selection_ui <- function(id, default_base = "GHA",
                                     ctf = cgjrdata::cgjr_ctf) {
  ns <- shiny::NS(id)
  choices <- .unit_choices(ctf)

  shiny::tagList(
    shiny::selectizeInput(
      ns("base"), "Base unit",
      choices = choices, selected = default_base,
      options = list(placeholder = "A country, region or income group")
    ),
    shiny::selectizeInput(
      ns("comparators"), "Comparators",
      choices = choices, selected = .default_comparators(default_base),
      multiple = TRUE,
      options = list(placeholder = "Countries, regions, income groups")
    ),
    shiny::helpText(shiny::textOutput(ns("comp_count"), inline = TRUE)),
    shiny::selectInput(
      ns("threshold"), "Benchmarking thresholds",
      choices = c("Default (25th / 50th pctile)" = "Default",
                  "Terciles (33rd / 66th)"       = "Terciles")
    ),
    shiny::sliderInput(
      ns("year_range"), "Year range (dynamic view, even years only)",
      min = .YEAR_MIN, max = .YEAR_MAX, value = c(.YEAR_MIN, .YEAR_MAX),
      step = 1L, sep = ""
    ),
    shiny::uiOutput(ns("apply_ui")),
    shiny::hr(),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      shiny::downloadButton(ns("save_sel"), "Save",
                            class = "btn-sm btn-outline-secondary"),
      shiny::tags$div(
        class = "flex-grow-1",
        shiny::fileInput(ns("load_sel"), NULL, accept = ".rds",
                         buttonLabel = "Load...", placeholder = "")
      )
    )
  )
}

#' Country / comparator selection module server
#'
#' @param id Module id (must match the UI).
#' @param default_base Character `unit_code` - must match the UI's
#'   `default_base`; seeds the pre-Apply state so the dashboard renders
#'   immediately.
#' @return A named list of reactives: `base_unit` (scalar `unit_code`),
#'   `comparison_units` (character vector, base removed), `threshold`,
#'   `year_range`, and `applied` (the Apply click count). `base_unit` ...
#'   `year_range` hold the default selection until "Apply" is clicked, then
#'   update only on each subsequent click.
#' @export
mod_country_selection_server <- function(id, default_base = "GHA") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    comps_live <- shiny::reactive(
      setdiff(input$comparators %||% character(0), input$base %||% character(0))
    )
    base_ok <- shiny::reactive(!is.null(input$base) && nzchar(input$base))
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
                            class = "btn-primary w-100", icon = shiny::icon("check"))
      } else {
        shiny::actionButton(ns("apply"),
                            "Pick a base unit and 2+ comparators",
                            class = "btn-secondary w-100", disabled = TRUE)
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
