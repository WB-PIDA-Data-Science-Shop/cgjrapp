# mod_data.R -- Data Download tab.
#
# Four sub-tabs, all driven by the sidebar selection:
#   Scores      -- the LIVE per-selection composites + the cgjr_scores
#                  precomputed reference, shown side by side and labelled as
#                  different things (build plan, constraint 6).
#   CTF         -- indicator-grain closeness-to-frontier for the selection.
#   Raw values  -- native-scale provider values for the base country.
#   Catalogue   -- the full indicator crosswalk / methodology reference.

.dt <- function(df, filename) {
  DT::datatable(
    df, rownames = FALSE, extensions = "Buttons", filter = "top",
    options = list(
      dom = "Bfrtip",
      buttons = list(
        list(extend = "csv", filename = filename),
        list(extend = "excel", filename = filename),
        "copy"
      ),
      pageLength = 25, scrollX = TRUE
    )
  )
}

.ctf_type_picker <- function(ns) {
  shiny::radioButtons(
    ns("ctf_type"), NULL,
    choices = c("Static (latest snapshot)" = "static",
                "Dynamic (over time, even years)" = "dynamic"),
    selected = "static", inline = TRUE
  )
}

#' Data Download tab UI
#'
#' @param id Module id.
#' @return A [bslib::nav_panel()].
#' @export
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Data", icon = shiny::icon("download"), value = "data",

    shiny::tags$p(
      class = "text-muted",
      "Every table reflects the applied sidebar selection. Use the CSV / Excel",
      " buttons on each table to download."
    ),

    bslib::navset_card_pill(
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("star-half-stroke"), " Scores"),
        .ctf_type_picker(ns),
        bslib::card(
          bslib::card_header("Live composite scores (what the dashboard shows)"),
          shiny::tags$p(class = "text-muted small",
            "One row per leaf: the mean of its indicators' CTF, screened for",
            " missing / flat indicators within THIS comparison group."),
          DT::DTOutput(ns("tbl_live"))
        ),
        bslib::card(
          bslib::card_header("Precomputed reference - cgjrdata::cgjr_scores"),
          shiny::tags$p(class = "text-muted small",
            shiny::tags$strong("Not the interactive numbers. "),
            "A fixed rollup with median cross-country aggregation and no",
            " selection-dependent screening - it will not match the live",
            " scores for a non-default selection, by design."),
          DT::DTOutput(ns("tbl_reference"))
        )
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("sliders"), " CTF indicators"),
        .ctf_type_picker(ns),
        DT::DTOutput(ns("tbl_ctf"))
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("database"), " Raw values"),
        shiny::tags$p(class = "text-muted small",
          "Native-scale provider values for the base country (1990+).",
          " Raw data is country-grain only - regions / income groups are not",
          " shown here."),
        DT::DTOutput(ns("tbl_raw"))
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("book"), " Indicator catalogue"),
        DT::DTOutput(ns("tbl_catalogue"))
      )
    )
  )
}

#' Data Download tab server
#'
#' @param id Module id.
#' @param base_unit,comparison_units,year_range Reactives from
#'   [mod_country_selection_server()].
#' @return Invisibly `NULL`.
#' @export
mod_data_server <- function(id, base_unit, comparison_units, year_range) {
  shiny::moduleServer(id, function(input, output, session) {
    ct <- shiny::reactive(input$ctf_type %||% "static")

    output$tbl_live <- DT::renderDT({
      shiny::validate(shiny::need(length(comparison_units()) >= 2L,
        "Select a base unit and at least 2 comparators, then Apply."))
      .dt(export_live_scores(base_unit(), comparison_units(), ct(), year_range()),
          "cgjr_live_scores")
    })

    output$tbl_reference <- DT::renderDT({
      .dt(export_reference_scores(base_unit(), comparison_units(), ct(),
                                  year_range()),
          "cgjr_scores_reference")
    })

    output$tbl_ctf <- DT::renderDT({
      .dt(export_ctf_table(base_unit(), comparison_units(), ct(), year_range()),
          "cgjr_ctf_indicators")
    })

    output$tbl_raw <- DT::renderDT({
      .dt(export_raw_table(base_unit(), comparison_units(), year_range()),
          "cgjr_raw_values")
    })

    output$tbl_catalogue <- DT::renderDT(
      .dt(export_indicator_catalogue(), "cgjr_indicator_catalogue")
    )

    invisible(NULL)
  })
}
