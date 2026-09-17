# mod_data.R -- Data Download tab.
#
# Three sub-tabs, all driven by the sidebar selection:
#   CTF         -- indicator-grain closeness-to-frontier for the selection.
#   Raw values  -- native-scale provider values for the base country.
#   Catalogue   -- the full indicator crosswalk / methodology reference.
#
# No composite-score table here - cliarappak's benchmarking tab doesn't
# surface one either; the live composite is computed only where cgjrapp
# actually needs a number to work with (the AI Report prompt, via
# export_live_scores()), never displayed as its own table.

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
