# mod_cluster_factory.R -- Per-cluster detail tabs
# Each cluster (e.g. "institutional_environment") gets its own nav_panel with
# one pill tab per subcluster. Each subcluster tab shows:
#   - a selectInput to choose between the subcluster score and individual
#     indicator columns
#   - a plotlyOutput rendering a CLIAR-style benchmark evolution plot
#
# Sidebar (with filters + threshold selector) is passed in from run_cgjrapp()
# and shared across all data tabs.

# Human-readable display names for each cluster
CLUSTER_DISPLAY_NAMES <- c(
  institutional_environment  = "Institutional Environment",
  political_institutions     = "Political Institutions",
  center_of_government       = "Center of Government",
  sectors_service_delivery   = "Sectors / Service Delivery"
)

# Human-readable display names for each subcluster
SUBCLUSTER_DISPLAY_NAMES <- c(
  degree_of_integrity                   = "Degree of Integrity",
  transparency_and_accountability       = "Transparency & Accountability",
  justice_and_rule_of_law               = "Justice & Rule of Law",
  social_cohesion_norms_and_cooperation = "Social Cohesion, Norms & Cooperation",
  political_institutions                = "Political Institutions",
  public_financial_management           = "Public Financial Management",
  public_sector_hrm                     = "Public Sector HRM",
  digital_and_data                      = "Digital & Data",
  business_environment                  = "Business Environment",
  service_delivery                      = "Service Delivery",
  soe_corporate_governance              = "SOE Corporate Governance",
  labor_and_social_protection           = "Labor & Social Protection",
  energy_and_environment                = "Energy & Environment"
)

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Get the display name for a cluster key
#'
#' @param cluster_key Character. Internal key from `names(cgjrdata::ctfdata_list)`.
#' @return A single character string — the human-readable display name.
#'   Falls back to `cluster_key` if not found.
#' @keywords internal
.cluster_label <- function(cluster_key) {
  label <- CLUSTER_DISPLAY_NAMES[cluster_key]
  if (is.na(label)) cluster_key else unname(label)
}

#' Get the display name for a subcluster key
#'
#' @param subcluster_key Character. Internal key, e.g. `"degree_of_integrity"`.
#' @return A single character string — the human-readable display name.
#'   Falls back to `subcluster_key` if not found.
#' @keywords internal
.subcluster_label <- function(subcluster_key) {
  label <- SUBCLUSTER_DISPLAY_NAMES[subcluster_key]
  if (is.na(label)) subcluster_key else unname(label)
}

#' Build indicator choices for a subcluster selectInput
#'
#' Prepends the subcluster aggregate score as the first (default) choice,
#' followed by all individual indicator columns returned by
#' [get_indicator_choices()].
#'
#' @param subcluster_tbl A tibble from `cgjrdata::ctfdata_list[[c]][[s]]`.
#' @param subcluster_key Character. The subcluster key (used as the score
#'   label in the display name).
#' @return A named character vector suitable for [shiny::selectInput()].
#' @keywords internal
.build_indicator_choices <- function(subcluster_tbl, subcluster_key) {
  score_choice  <- stats::setNames("score", paste0(.subcluster_label(subcluster_key), " (Score)"))
  ind_choices   <- get_indicator_choices(subcluster_tbl)
  c(score_choice, ind_choices)
}

# ── UI ────────────────────────────────────────────────────────────────────────

#' Cluster detail tab UI
#'
#' Produces a [bslib::nav_panel()] for one governance cluster, containing a
#' [bslib::layout_sidebar()] with the shared sidebar and a
#' [bslib::navset_card_pill()] of per-subcluster tabs.
#'
#' Each subcluster tab contains a [shiny::selectInput()] for choosing the
#' indicator to plot and a [plotly::plotlyOutput()] for the benchmark chart.
#'
#' @param id Module namespace ID.
#' @param cluster_key Character. One of `names(cgjrdata::ctfdata_list)`.
#' @param sidebar A [bslib::sidebar()] object to embed. Passed in from
#'   `run_cgjrapp()` so the sidebar is shared across all tabs.
#'
#' @return A [bslib::nav_panel()] object.
#' @export
mod_cluster_ui <- function(id, cluster_key, sidebar) {
  ns <- shiny::NS(id)

  cluster_label    <- .cluster_label(cluster_key)
  subcluster_keys  <- names(cgjrdata::ctfdata_list[[cluster_key]])

  bslib::nav_panel(
    title = cluster_label,
    value = cluster_key,

    bslib::layout_sidebar(
      sidebar = sidebar,

      shiny::tags$h5(
        class = "mb-3",
        style = "font-weight:600;",
        cluster_label
      ),

      bslib::navset_card_pill(
        !!!unname(purrr::map(subcluster_keys, function(sub_key) {
          sub_label    <- .subcluster_label(sub_key)
          sub_tbl      <- cgjrdata::ctfdata_list[[cluster_key]][[sub_key]]
          ind_choices  <- .build_indicator_choices(sub_tbl, sub_key)
          plot_id      <- paste0("plot_", cluster_key, "_", sub_key)
          select_id    <- paste0("ind_", cluster_key, "_", sub_key)

          bslib::nav_panel(
            title = sub_label,
            shiny::div(
              class = "mb-2",
              shiny::selectInput(
                inputId  = ns(select_id),
                label    = "Indicator",
                choices  = ind_choices,
                selected = "score",
                width    = "100%"
              )
            ),
            plotly::plotlyOutput(ns(plot_id), height = "380px")
          )
        }))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

#' Cluster detail tab server
#'
#' Handles the server logic for one governance cluster tab. For each
#' subcluster, registers a [plotly::renderPlotly()] output that:
#'
#' 1. Filters country + peer rows via [filter_country_data()].
#' 2. Retrieves aggregate rows via [get_aggregate_data()].
#' 3. Computes annual thresholds via [get_annual_thresholds()] when a relative
#'    threshold mode is selected.
#' 4. Renders via [plot_cgjr_master()].
#'
#' @param id Module namespace ID. Must match the `id` used in [mod_cluster_ui()].
#' @param cluster_key Character. One of `names(cgjrdata::ctfdata_list)`.
#' @param primary_iso Reactive string — ISO3 of the focal country.
#' @param peer_isos Reactive character vector — peer ISO3 codes.
#' @param region_codes Reactive character vector — selected region codes.
#' @param income_groups Reactive character vector — selected income groups.
#' @param year_range Reactive integer vector of length 2.
#' @param zone_type Reactive string — one of `"abs_default"`, `"abs_tercile"`,
#'   `"rel_quartile"`, `"rel_tercile"`.
#'
#' @return Invoked for side effects (registers Shiny outputs).
#' @export
mod_cluster_server <- function(id, cluster_key,
                               primary_iso, peer_isos,
                               region_codes, income_groups,
                               year_range, zone_type) {
  shiny::moduleServer(id, function(input, output, session) {

    subcluster_keys <- names(cgjrdata::ctfdata_list[[cluster_key]])

    purrr::walk(subcluster_keys, function(sub_key) {
      local({
        sk       <- sub_key
        plot_id  <- paste0("plot_", cluster_key, "_", sk)
        sel_id   <- paste0("ind_",  cluster_key, "_", sk)

        output[[plot_id]] <- plotly::renderPlotly({

          y_var    <- input[[sel_id]] %||% "score"
          sub_tbl  <- cgjrdata::ctfdata_list[[cluster_key]][[sk]]

          # Filtered country + peer rows for this subcluster
          country_data <- filter_country_data(
            data        = sub_tbl,
            primary_iso = primary_iso(),
            peer_isos   = peer_isos(),
            year_range  = year_range()
          )

          # Aggregate region / income rows
          agg_data <- get_aggregate_data(
            cluster       = cluster_key,
            subcluster    = sk,
            region_codes  = region_codes(),
            income_groups = income_groups(),
            year_range    = year_range()
          )

          # Rename aggregate 'score' to y_var if plotting an indicator column
          # so that bind_rows produces a consistent y_var column.
          if (y_var != "score" && nrow(agg_data) > 0L) {
            agg_data$score <- NA_real_
          }

          combined <- dplyr::bind_rows(country_data, agg_data)

          mode <- zone_type()

          # For relative modes: compute annual thresholds from the full global
          # subcluster dataset (all countries) for the selected y_var.
          # For indicator columns, use the subcluster tibble itself as the
          # global reference so the distribution is indicator-specific.
          thresholds <- if (mode %in% c("rel_quartile", "rel_tercile")) {
            probs <- if (mode == "rel_tercile") c(1/3, 2/3) else c(0.25, 0.50)
            ref_data <- if (y_var == "score") {
              sub_tbl
            } else {
              sub_tbl
            }
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
            data           = combined,
            y_var          = y_var,
            primary_iso    = primary_iso(),
            year_range     = year_range(),
            threshold_mode = mode,
            thresholds     = thresholds
          )

          pl <- plotly::ggplotly(p, tooltip = "text")

          # Fix plotly legend names: strip "(label, 1)" artefacts
          pl$x$data <- lapply(pl$x$data, function(trace) {
            if (!is.null(trace$name)) {
              trace$name <- gsub("^\\((.+),\\s*\\d+\\)$", "\\1", trace$name)
            }
            trace
          })

          pl |> plotly::layout(legend = list(orientation = "h", y = -0.2))
        })

        # Render even when the pill tab is not the active one
        shiny::outputOptions(output, plot_id, suspendWhenHidden = FALSE)
      })
    })
  })
}
