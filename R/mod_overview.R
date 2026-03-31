# mod_overview.R -- Tab 2: Overview
# Institutional Dimensions of the CGJR -- tabbed cluster score benchmark plots.
# Sidebar is passed in from run_cgjrapp() so it only appears on data tabs.

# Cluster score columns in institutional_averages_tbl, in display order
OVERVIEW_CLUSTER_VARS <- c(
  "Institutional Environment"  = "institutional_environment_score",
  "Political Institutions"     = "political_institutions_score",
  "Center of Government"       = "center_of_government_score",
  "Sectors / Service Delivery" = "sectors_service_delivery_score"
)

# Cluster label -> colour mapping
CLUSTER_COLOURS <- c(
  "Institutional Environment"  = "#1a4f8a",
  "Political Institutions"     = "#5b9e6e",
  "Center of Government"       = "#e07b39",
  "Sectors / Service Delivery" = "#7b5ea7"
)

# -- UI ------------------------------------------------------------------

#' Overview tab UI
#'
#' @param id Module namespace ID.
#' @param sidebar A [bslib::sidebar()] object to embed in this tab.
#' @return A [bslib::nav_panel()] containing the overview dashboard.
#' @export
mod_overview_ui <- function(id, sidebar) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Overview",
    icon  = shiny::icon("chart-line"),
    value = "overview",

    bslib::layout_sidebar(
      sidebar = sidebar,

      # Tabbed cluster score plots -- one tab per cluster
      shiny::tags$h5(
        class = "mb-3",
        style = "font-weight:600;",
        "Institutional Dimensions of the CGJR"
      ),
      bslib::navset_card_pill(
        !!!unname(purrr::imap(
          OVERVIEW_CLUSTER_VARS,
          function(var, label) {
            bslib::nav_panel(
              title = label,
              plotly::plotlyOutput(ns(paste0("plot_", var)), height = "380px")
            )
          }
        ))
      )
    )
  )
}

# -- Server --------------------------------------------------------------

#' Overview tab server
#'
#' @param id Module namespace ID.
#' @param primary_iso Reactive string -- ISO3 of the focal country.
#' @param peer_isos Reactive character vector -- peer ISO3 codes.
#' @param region_codes Reactive character vector -- selected region codes.
#' @param income_groups Reactive character vector -- selected income groups.
#' @param year_range Reactive integer vector of length 2.
#' @param threshold_mode Reactive string -- one of `"abs_quartile"`,
#'   `"abs_tercile"`, `"rel_quartile"`, `"rel_tercile"`.
#' @export
mod_overview_server <- function(id, primary_iso, peer_isos,
                                region_codes, income_groups,
                                year_range, threshold_mode) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive: filtered country + peer rows from institutional_averages_tbl
    overview_data <- shiny::reactive({
      filter_country_data(
        data        = cgjrdata::institutional_averages_tbl,
        primary_iso = primary_iso(),
        peer_isos   = peer_isos(),
        year_range  = year_range()
      )
    })

    # Helper: build aggregate rows for a given cluster score column
    agg_rows_for <- function(cluster_key, var_name) {
      agg <- get_aggregate_data(
        cluster       = cluster_key,
        subcluster    = names(cgjrdata::ctfdata_list[[cluster_key]])[[1]],
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )
      if (nrow(agg) == 0L) return(agg)
      names(agg)[names(agg) == "score"] <- var_name
      agg
    }

    # Helper: render one cluster plot
    # data     = filtered tibble to plot (country + peers + aggregates).
    #            NULL → built internally from overview_data() + agg_rows_for().
    # ref_data = full global tibble for computing relative thresholds.
    #            NULL → defaults to institutional_averages_tbl (overview standard).
    render_cluster_plot <- function(y_var, cluster_key,
                                    data     = NULL,
                                    ref_data = NULL) {
      if (is.null(data)) {
        agg_data <- agg_rows_for(cluster_key, y_var)
        data     <- dplyr::bind_rows(overview_data(), agg_data)
      }

      if (is.null(ref_data)) {
        ref_data <- cgjrdata::institutional_averages_tbl
      }

      mode <- threshold_mode()

      thresholds <- if (startsWith(mode, "rel")) {
        probs <- if (mode == "rel_tercile") c(1/3, 2/3) else c(0.25, 0.50)
        get_annual_thresholds(
          data       = ref_data,
          y_var      = y_var,
          year_range = year_range(),
          probs      = probs
        )
      } else {
        NULL
      }

      p <- plot_cgjr_master(
        data           = data,
        y_var          = y_var,
        primary_iso    = primary_iso(),
        year_range     = year_range(),
        threshold_mode = mode,
        thresholds     = thresholds
      )

      pl <- plotly::ggplotly(p, tooltip = "text")

      # Fix plotly legend names: strip "(<label>, 1)" artefacts produced when
      # ggplotly lifts discrete fill/shape scales into trace names.
      pl$x$data <- lapply(pl$x$data, function(trace) {
        if (!is.null(trace$name)) {
          trace$name <- gsub("^\\((.+),\\s*\\d+\\)$", "\\1", trace$name)
        }
        trace
      })

      pl |> plotly::layout(legend = list(orientation = "h", y = -0.2))
    }

    # Outputs
    purrr::iwalk(OVERVIEW_CLUSTER_VARS, function(var, label) {
      cluster_key <- names(cgjrdata::ctfdata_list)[
        which(OVERVIEW_CLUSTER_VARS == var)
      ]
      local({
        v  <- var
        ck <- cluster_key
        out_id <- paste0("plot_", v)
        output[[out_id]] <- plotly::renderPlotly({
          render_cluster_plot(v, cluster_key = ck)
        })
        # Render even when the tab is not the active pill
        shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
      })
    })
  })
}
