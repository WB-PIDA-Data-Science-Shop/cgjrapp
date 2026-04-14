# mod_detail.R -- Cluster detail tab
# One instance per cluster in ctfdata_list.  The UI and server are both
# fully data-driven: adding a new cluster/subcluster/indicator to cgjrdata
# automatically produces new tabs and plots with zero code changes here.

# Columns that are never plotted as indicators in a subcluster tibble
.DETAIL_SKIP_COLS <- c(
  "country_code", "country_name", "year",
  "var_count", "nonna_count"
)

# Fallback colour when a cluster_key is not in CLUSTER_COLOURS
.DETAIL_FALLBACK_COLOUR <- "#555555"

# -- UI -----------------------------------------------------------------------

#' Detail tab UI (one per cluster)
#'
#' Generates a [bslib::nav_panel()] containing a [bslib::navset_card_pill()]
#' of subcluster sub-tabs.  Each sub-tab shows stacked indicator plots and a
#' single wide-format data table.  Subclusters with no indicators show a
#' placeholder message.
#'
#' The UI is fully data-driven from `cgjrdata::ctfdata_list[[cluster_key]]`:
#' no hardcoding of subcluster names or indicator lists.
#'
#' @param id Module namespace ID -- must match the `id` passed to
#'   [mod_detail_server()].
#' @param cluster_key Character scalar.  One of `names(cgjrdata::ctfdata_list)`,
#'   e.g. `"institutional_environment"`.
#'
#' @return A [bslib::nav_panel()].
#' @export
mod_detail_ui <- function(id, cluster_key) {
  ns           <- shiny::NS(id)
  cluster      <- cgjrdata::ctfdata_list[[cluster_key]]
  cluster_title <- key_to_title(cluster_key)

  # One sub-tab per subcluster
  subcluster_tabs <- purrr::imap(cluster, function(sub_tbl, sub_key) {
    sub_title  <- key_to_title(sub_key)
    # Indicators = plotted columns excluding score (shown separately full-width)
    indicators <- setdiff(names(sub_tbl), c(.DETAIL_SKIP_COLS, "score"))

    has_score      <- "score" %in% names(sub_tbl)

    if (!has_score && length(indicators) == 0L) {
      # ── Placeholder for empty subclusters ──────────────────────────────────
      bslib::nav_panel(
        title = sub_title,
        shiny::div(
          class = "text-muted p-4",
          style = "font-style: italic;",
          "Data and Plots forthcoming"
        )
      )
    } else {
      # ── Full-width subcluster composite score plot ───────────────────────
      score_output_id <- ns(paste0("plot_", cluster_key, "__", sub_key, "__score"))
      n_indicators    <- length(indicators)

      score_popover_content <- shiny::tagList(
        shiny::tags$p(
          "This score is a weighted average of ",
          shiny::tags$strong(n_indicators, " indicators"),
          " in the ", shiny::tags$strong(sub_title), " subcluster."
        )
      )

      score_plot_ui <- shiny::div(
        class = "mb-4",
        shiny::tags$h6(
          class = "text-muted mb-1",
          style = "font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em;",
          sub_title, " - Composite Score",
          bslib::popover(
            trigger = shiny::icon(
              "circle-info",
              style = "font-size: 0.75rem; color: #888;",
              class = "ms-1"
            ),
            title   = paste0(sub_title, " Score"),
            score_popover_content,
            placement = "right"
          )
        ),
        plotly::plotlyOutput(score_output_id, height = "300px")
      )

      # ── Indicator plots: 2-column grid ────────────────────────────────────
      plot_outputs <- purrr::map(indicators, function(ind) {
        output_id <- ns(paste0("plot_", cluster_key, "__", sub_key, "__", ind))
        meta      <- get_indicator_metadata(ind)
        label     <- if (!is.null(meta)) meta$var_name else ind

        # Build popover content from metadata if available
        info_btn <- if (!is.null(meta)) {
          popover_content <- shiny::tagList(
            shiny::tags$p(
              shiny::tags$strong("Code: "),
              shiny::tags$code(meta$variable)
            ),
            shiny::tags$p(
              shiny::tags$strong("Source: "),
              meta$source
            ),
            shiny::tags$p(meta$description_short)
          )
          bslib::popover(
            trigger = shiny::icon(
              "circle-info",
              style = "font-size: 0.75rem; color: #888; margin-left: 4px;",
              class = "ms-1"
            ),
            title   = meta$var_name,
            popover_content,
            placement = "right"
          )
        } else {
          NULL
        }

        shiny::div(
          shiny::tags$h6(
            class = "text-muted mb-1",
            style = "font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em;",
            label,
            info_btn
          ),
          plotly::plotlyOutput(output_id, height = "300px")
        )
      })

      bslib::nav_panel(
        title = sub_title,
        shiny::div(
          class = "pt-3",
          score_plot_ui,
          bslib::layout_column_wrap(
            width  = 1 / 2,
            gap    = "1rem",
            !!!plot_outputs
          )
        )
      )
    }
  })

  bslib::nav_panel(
    title = cluster_title,
    value = cluster_key,
    shiny::tags$h5(
      class = "mb-3",
      style = "font-weight: 600;",
      cluster_title
    ),
    bslib::navset_card_pill(!!!unname(subcluster_tabs))
  )
}

# -- Server -------------------------------------------------------------------

#' Detail tab server (one per cluster)
#'
#' Registers [plotly::renderPlotly()] outputs for every indicator in every
#' subcluster, and a [DT::renderDT()] output for each subcluster's data table.
#' All outputs are fully data-driven from `cgjrdata::ctfdata_list[[cluster_key]]`.
#'
#' @param id Module namespace ID.
#' @param cluster_key Character scalar -- key into `cgjrdata::ctfdata_list`.
#' @param primary_iso Reactive string -- ISO3 of the focal country.
#' @param peer_isos Reactive character vector -- peer ISO3 codes.
#' @param region_codes Reactive character vector -- selected region codes.
#' @param income_groups Reactive character vector -- selected income groups.
#' @param year_range Reactive integer vector of length 2.
#' @param threshold_mode Reactive string -- one of `"abs_quartile"`,
#'   `"abs_tercile"`, `"rel_quartile"`, `"rel_tercile"`.
#' @param show_members Reactive logical -- overlay individual country dots.
#'
#' @export
mod_detail_server <- function(id, cluster_key,
                              primary_iso, peer_isos,
                              region_codes, income_groups,
                              year_range, threshold_mode,
                              show_members) {
  shiny::moduleServer(id, function(input, output, session) {

    cluster <- cgjrdata::ctfdata_list[[cluster_key]]

    # ── Shared helper: aggregate rows for a subcluster ─────────────────────
    agg_rows_for <- function(sub_key) {
      get_aggregate_data(
        cluster       = cluster_key,
        subcluster    = sub_key,
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )
    }

    # ── Shared helper: member-country data for a subcluster / indicator ─────
    member_data_for <- function(sub_key, ind) {
      has_groups <- length(region_codes()) > 0L || length(income_groups()) > 0L
      if (!isTRUE(show_members()) || !has_groups) return(NULL)
      get_cluster_member_data(
        cluster       = cluster_key,
        subcluster    = sub_key,
        score_var     = ind,
        region_codes  = region_codes(),
        income_groups = income_groups(),
        year_range    = year_range()
      )
    }

    # ── Shared helper: render one indicator plot ────────────────────────────
    # agg_data is passed in from the per-subcluster reactive memo so it is
    # only computed once per subcluster, not once per indicator.
    render_indicator_plot <- function(sub_key, ind, agg_data = NULL) {
      sub_tbl <- cgjrdata::ctfdata_list[[cluster_key]][[sub_key]]

      country_data <- filter_country_data(
        data        = sub_tbl,
        primary_iso = primary_iso(),
        peer_isos   = peer_isos(),
        year_range  = year_range()
      )

      if (is.null(agg_data)) agg_data <- agg_rows_for(sub_key)
      # Rename agg `score` to ind so plot_cgjr_master finds y_var
      if (nrow(agg_data) > 0 && "score" %in% names(agg_data)) {
        agg_data <- dplyr::rename(agg_data, !!ind := score)
      }

      plot_data <- dplyr::bind_rows(country_data, agg_data)

      mode <- threshold_mode()
      thresholds <- if (startsWith(mode, "rel")) {
        probs <- if (mode == "rel_tercile") c(1 / 3, 2 / 3) else c(0.25, 0.50)
        get_annual_thresholds(
          data       = sub_tbl,
          y_var      = ind,
          year_range = year_range(),
          probs      = probs
        )
      } else {
        NULL
      }

      p <- plot_cgjr_master(
        data           = plot_data,
        y_var          = ind,
        primary_iso    = primary_iso(),
        year_range     = year_range(),
        threshold_mode = mode,
        thresholds     = thresholds,
        show_members   = isTRUE(show_members()),
        member_data    = member_data_for(sub_key, ind)
      )

      pl <- plotly::ggplotly(p, tooltip = "text")

      pl$x$data <- lapply(pl$x$data, function(trace) {
        if (!is.null(trace$name)) {
          trace$name <- gsub("^\\((.+),\\s*\\d+\\)$", "\\1", trace$name)
        }
        trace
      })

      pl |> plotly::layout(legend = list(orientation = "h", y = -0.2))
    }

    # ── Register outputs for every subcluster / indicator ───────────────────
    purrr::iwalk(cluster, function(sub_tbl, sub_key) {
      # Indicators = all plotted columns except score (handled separately)
      indicators <- setdiff(names(sub_tbl), c(.DETAIL_SKIP_COLS, "score"))

      # Shared reactive memo: aggregate rows computed once per subcluster,
      # not once per indicator.  Invalidates only when filter inputs change.
      local({
        sk <- sub_key
        r_agg <- shiny::reactive({
          agg_rows_for(sk)
        })

        # -- Score plot: full-width composite ----------------------------------
        local({
          oid <- paste0("plot_", cluster_key, "__", sk, "__score")
          output[[oid]] <- plotly::renderPlotly({
            render_indicator_plot(sk, "score", r_agg())
          }) |>
            shiny::bindCache(
              primary_iso(), peer_isos(), region_codes(), income_groups(),
              year_range(), threshold_mode(), show_members(),
              cluster_key, sk, "score"
            )
        })

        # -- Indicator plots: one per indicator --------------------------------
        purrr::walk(indicators, function(ind) {
          local({
            iv  <- ind
            oid <- paste0("plot_", cluster_key, "__", sk, "__", iv)

            output[[oid]] <- plotly::renderPlotly({
              render_indicator_plot(sk, iv, r_agg())
            }) |>
              shiny::bindCache(
                primary_iso(), peer_isos(), region_codes(), income_groups(),
                year_range(), threshold_mode(), show_members(),
                cluster_key, sk, iv
              )
          })
        })
      })
    })
  })
}

# ── Internal helpers ─────────────────────────────────────────────────────────

# Return the human-readable label for a single indicator code,
# falling back to the code itself if not in metadata.
.indicator_display_label <- function(ind) {
  meta <- cgjrdata::metadata_tbl |>
    dplyr::filter(variable == ind) |>
    dplyr::pull(var_name)
  if (length(meta) > 0L && !is.na(meta[[1L]])) meta[[1L]] else ind
}
