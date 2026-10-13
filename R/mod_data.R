# mod_data.R -- Data Download tab.
#
# Three sub-tabs, all driven by the applied Country Selection:
#   CTF         -- indicator-grain closeness-to-frontier for the selection.
#   Raw values  -- native-scale provider values for the base country.
#   Catalogue   -- the full indicator crosswalk / methodology reference.
#
# No composite-score table here - cliarappak's benchmarking tab doesn't
# surface one either; the AI Report works from the evidence pack
# (build_evidence()), never from a composite-score table.

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
#' @return A `shiny::tagList` - the content of the `"data"`
#'   [bs4Dash::tabItem()].
#' @export
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bs4Dash::bs4Card(
      title = "Data download",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,

      shiny::tags$p(
        class = "text-muted",
        "Every table reflects the applied Country Selection. Use the CSV / Excel",
        " buttons on each table to download."
      ),

      # bs4Dash's tabsetPanel(), not shiny's: without a bslib theme shiny's
      # emits Bootstrap 3 nav markup, which Bootstrap 4 doesn't style
      bs4Dash::tabsetPanel(
        type = "pills",
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("sliders"), " CTF indicators"),
          .ctf_type_picker(ns),
          DT::DTOutput(ns("tbl_ctf"))
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("database"), " Raw values"),
          shiny::tags$p(class = "text-muted small",
            "Native-scale provider values for the base country (1990+).",
            " Raw data is country-grain only - regions / income groups are not",
            " shown here."),
          DT::DTOutput(ns("tbl_raw"))
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("book"), " Indicator catalogue"),
          DT::DTOutput(ns("tbl_catalogue"))
        )
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
