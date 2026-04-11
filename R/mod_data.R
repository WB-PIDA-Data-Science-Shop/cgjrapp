# mod_data.R -- Data download tab
# Three sub-tabs: Scores | CTF Indicators | Raw Data
# All data-driven; adding clusters/subclusters/indicators to cgjrdata
# automatically appears here with zero code changes.

# Shared DT options factory
.dt_options <- function(filename) {
  list(
    dom        = "Bfrtip",
    buttons    = list(
      list(extend = "csv",   filename = filename),
      list(extend = "excel", filename = filename),
      "copy"
    ),
    pageLength = 25,
    scrollX    = TRUE
  )
}

# -- UI -----------------------------------------------------------------------

#' Data download tab UI
#'
#' Generates a [bslib::nav_panel()] with three sub-tabs:
#' \describe{
#'   \item{Scores}{Cluster and subcluster CTF score averages.}
#'   \item{CTF Indicators}{Rescaled 0-1 indicator values.}
#'   \item{Raw Data}{Original source-scale indicator values.}
#' }
#' All tables are driven by the shared sidebar filters and include
#' CSV / Excel / Copy download buttons.
#'
#' @param id Module namespace ID.
#' @return A [bslib::nav_panel()].
#' @export
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Data",
    icon  = shiny::icon("download"),
    value = "data",

    shiny::tags$h5(
      class = "mb-3",
      style = "font-weight: 600;",
      "Data Download"
    ),

    shiny::tags$p(
      class = "text-muted mb-4",
      "All tables reflect the current sidebar filter selections. ",
      "Use the CSV or Excel buttons to download the data."
    ),

    bslib::navset_card_pill(

      # ── Sub-tab 1: Scores ──────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("star-half-stroke"), " Scores"),
        shiny::tags$p(
          class = "text-muted mt-2 mb-3",
          style = "font-size: 0.875rem;",
          "Cluster and subcluster CTF score averages for the selected",
          " country, peers, and benchmark groups."
        ),
        DT::DTOutput(ns("tbl_scores"))
      ),

      # ── Sub-tab 2: CTF Indicators ──────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("sliders"), " CTF Indicators"),
        shiny::tags$p(
          class = "text-muted mt-2 mb-3",
          style = "font-size: 0.875rem;",
          "Indicator values rescaled to the 0-1 CTF scale, grouped by",
          " cluster and subcluster."
        ),
        DT::DTOutput(ns("tbl_ctf"))
      ),

      # ── Sub-tab 3: Raw Data ────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("database"), " Raw Data"),
        shiny::tags$p(
          class = "text-muted mt-2 mb-3",
          style = "font-size: 0.875rem;",
          "Original source-scale indicator values (native units) from",
          " 2013 onwards, grouped by cluster and subcluster."
        ),
        DT::DTOutput(ns("tbl_raw"))
      )
    )
  )
}

# -- Server -------------------------------------------------------------------

#' Data download tab server
#'
#' Registers [DT::renderDT()] outputs for the three data sub-tabs.
#' All outputs are lazy (rendered only when the sub-tab is first visited).
#'
#' @param id Module namespace ID.
#' @param primary_iso Reactive string — ISO3 of the focal country.
#' @param peer_isos Reactive character vector — peer ISO3 codes.
#' @param region_codes Reactive character vector — selected region codes.
#' @param income_groups Reactive character vector — selected income groups.
#' @param year_range Reactive integer vector of length 2.
#'
#' @export
mod_data_server <- function(id, primary_iso, peer_isos,
                             region_codes, income_groups, year_range) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Scores ────────────────────────────────────────────────────────────────
    output$tbl_scores <- DT::renderDT({
      df <- get_scores_table(
        primary_iso   = primary_iso(),
        peer_isos     = peer_isos(),
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )

      # Human-readable score column names from metadata
      score_cols   <- setdiff(names(df), c("Entity", "Year"))
      label_map    <- get_indicator_label_map(score_cols)
      display_names <- c(Entity = "Entity", Year = "Year",
                         stats::setNames(label_map, score_cols))
      colnames(df) <- display_names[names(df)]

      DT::datatable(
        df,
        rownames   = FALSE,
        extensions = "Buttons",
        options    = .dt_options("cgjr_scores")
      )
    })

    # ── CTF Indicators ────────────────────────────────────────────────────────
    output$tbl_ctf <- DT::renderDT({
      df <- get_ctf_table(
        primary_iso   = primary_iso(),
        peer_isos     = peer_isos(),
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )

      DT::datatable(
        df,
        rownames   = FALSE,
        extensions = "Buttons",
        options    = .dt_options("cgjr_ctf_indicators")
      )
    })

    # ── Raw Data ──────────────────────────────────────────────────────────────
    output$tbl_raw <- DT::renderDT({
      df <- get_raw_table(
        primary_iso   = primary_iso(),
        peer_isos     = peer_isos(),
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )

      DT::datatable(
        df,
        rownames   = FALSE,
        extensions = "Buttons",
        options    = .dt_options("cgjr_raw_data")
      )
    })
  })
}
