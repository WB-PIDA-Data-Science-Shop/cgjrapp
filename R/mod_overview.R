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

# Internal helper: build popover content for a cluster tab title.
# Lists each subcluster and its indicator count, data-driven from metadata_tbl.
.cluster_info_popover <- function(cluster_label) {
  sub_counts <- cgjrdata::metadata_tbl |>
    dplyr::filter(cluster == cluster_label) |>
    dplyr::count(subcluster, name = "n") |>
    dplyr::arrange(subcluster)

  total_n <- sum(sub_counts$n)

  rows <- purrr::pmap(sub_counts, function(subcluster, n) {
    shiny::tags$tr(
      shiny::tags$td(subcluster),
      shiny::tags$td(
        style = "text-align: right; padding-left: 1rem;",
        n, " indicators"
      )
    )
  })

  content <- shiny::tagList(
    shiny::tags$p(
      "This score is a weighted average of ",
      shiny::tags$strong(total_n, " indicators"),
      " across ", shiny::tags$strong(nrow(sub_counts), " subclusters:"),
    ),
    shiny::tags$table(
      class = "table table-sm table-borderless mb-0",
      shiny::tags$tbody(!!!rows)
    )
  )

  bslib::popover(
    trigger = shiny::icon(
      "circle-info",
      style = "font-size: 0.75rem; color: #888;",
      class = "ms-1"
    ),
    title     = paste0(cluster_label, " Score"),
    content,
    placement = "bottom"
  )
}

#' Overview tab UI
#'
#' @param id Module namespace ID.
#' @return A [bslib::nav_panel()] containing the overview dashboard.
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Overview",
    icon  = shiny::icon("chart-line"),
    value = "overview",

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
              title = shiny::tagList(
                label,
                .cluster_info_popover(label)
              ),
              plotly::plotlyOutput(ns(paste0("plot_", var)), height = "380px")
            )
          }
        ))
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
#' @param show_members Reactive logical -- whether to show individual member
#'   country dots behind group benchmark medians.
#' @export
mod_overview_server <- function(id, primary_iso, peer_isos,
                                region_codes, income_groups,
                                year_range, threshold_mode,
                                show_members) {
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

    # Helper: member-country data for a given cluster / score var
    member_data_for <- function(cluster_key, var_name) {
      has_groups <- length(region_codes()) > 0L || length(income_groups()) > 0L
      if (!isTRUE(show_members()) || !has_groups) return(NULL)
      get_cluster_member_data(
        cluster       = cluster_key,
        subcluster    = names(cgjrdata::ctfdata_list[[cluster_key]])[[1]],
        score_var     = var_name,
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )
    }

    # Helper: render one cluster plot
    # data     = filtered tibble to plot (country + peers + aggregates).
    #            NULL → built internally from overview_data() + agg_rows_for().
    # ref_data = full global tibble for computing relative thresholds.
    #            NULL → defaults to institutional_averages_tbl (overview standard).
    render_cluster_plot <- function(y_var, cluster_key,
                                    data        = NULL,
                                    ref_data    = NULL,
                                    member_data = NULL) {
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
        thresholds     = thresholds,
        show_members   = isTRUE(show_members()),
        member_data    = member_data
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
        v   <- var
        ck  <- cluster_key
        plot_id <- paste0("plot_", v)

        output[[plot_id]] <- plotly::renderPlotly({
          render_cluster_plot(v, cluster_key = ck,
                              member_data = member_data_for(ck, v))
        }) |>
          shiny::bindCache(
            primary_iso(), peer_isos(), region_codes(), income_groups(),
            year_range(), threshold_mode(), show_members(), ck
          )
      })
    })
  })
}
