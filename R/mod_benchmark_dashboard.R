# ---
# Benchmark dashboard module.
#
# One generic renderer that walks `cgjr_taxonomy` and draws a nested accordion:
#
#   cluster (cluster_num order)
#      - subcluster (subcluster_num order)
#           - leaf card                     when sub_subcluster is NA
#           - sub_subcluster (num order)    when it isn't (only PFM today)
#                - leaf card
#
# A "leaf card" is the same fragment everywhere: a static/dynamic toggle, the
# live benchmarking plot, the live composite score, and a notes line. It is
# drawn once per finest-grain node - `coalesce(sub_subcluster, subcluster)`.
#
# Nothing here hardcodes a cluster / subcluster / leaf name, count or depth.
# The walk is a fixed three-step descent because `cgjr_taxonomy` is a
# fixed-width 3-level schema (build plan, constraint 1); a genuine 4th level
# would need a schema change there and a rewrite here.
# ---

# resolve_leaf() equivalent - the finest-grain key / label for a taxonomy row
.leaf_key   <- function(taxonomy) dplyr::coalesce(taxonomy$sub_subcluster, taxonomy$subcluster)
.leaf_label <- function(taxonomy) dplyr::coalesce(taxonomy$sub_subcluster_name, taxonomy$subcluster_name)

# The card body rendered at every finest-grain node.
.leaf_card_ui <- function(ns, key, label) {
  shiny::tagList(
    shiny::radioButtons(
      ns(paste0("ctf_type_", key)),
      label = NULL,
      choices = c("Static (latest snapshot)" = "static",
                  "Dynamic (over time)" = "dynamic"),
      selected = "static",
      inline = TRUE
    ),
    bslib::layout_columns(
      col_widths = c(9, 3),
      plotly::plotlyOutput(ns(paste0("plot_", key)), height = "560px"),
      bslib::value_box(
        title = "Composite score",
        value = shiny::textOutput(ns(paste0("score_", key))),
        showcase = bsicons::bs_icon("bullseye"),
        theme = "primary"
      )
    ),
    shiny::htmlOutput(ns(paste0("note_", key)))
  )
}

# iterate a distinct/ordered tibble one row at a time (row is a 1-row tibble)
.by_row <- function(tbl, f) purrr::map(seq_len(nrow(tbl)), function(i) f(tbl[i, ]))

# One subcluster panel: a leaf card, or (branching) an inner accordion of
# sub_subcluster leaf cards.
.subcluster_panel <- function(ns, taxonomy, sub_key, sub_name) {
  rows <- taxonomy[taxonomy$subcluster == sub_key, ]

  if (all(is.na(rows$sub_subcluster))) {
    return(bslib::accordion_panel(
      title = sub_name, value = sub_key,
      .leaf_card_ui(ns, key = sub_key, label = sub_name)
    ))
  }

  ssub <- rows[order(rows$sub_subcluster_num),
               c("sub_subcluster", "sub_subcluster_name")]
  ssub <- ssub[!duplicated(ssub$sub_subcluster), ]
  inner <- .by_row(ssub, function(r) {
    bslib::accordion_panel(
      title = r$sub_subcluster_name, value = r$sub_subcluster,
      .leaf_card_ui(ns, key = r$sub_subcluster, label = r$sub_subcluster_name)
    )
  })
  bslib::accordion_panel(
    title = sub_name, value = sub_key,
    bslib::accordion(id = ns(paste0("acc_", sub_key)), !!!inner)
  )
}

#' Benchmark dashboard module UI
#'
#' @param id Module id.
#' @param taxonomy `cgjrdata::cgjr_taxonomy` (or any subset with the same
#'   columns). The accordion structure is generated entirely from its
#'   `cluster_num` / `subcluster_num` / `sub_subcluster_num` ordering and
#'   `*_name` labels.
#' @return A nested `bslib::accordion`.
#' @export
mod_benchmark_dashboard_ui <- function(id, taxonomy = cgjrdata::cgjr_taxonomy) {
  ns <- shiny::NS(id)
  taxonomy <- as.data.frame(taxonomy)

  clusters <- taxonomy[order(taxonomy$cluster_num),
                       c("cluster", "cluster_name")]
  clusters <- clusters[!duplicated(clusters$cluster), ]

  cluster_panels <- .by_row(clusters, function(cl) {
    subs <- taxonomy[taxonomy$cluster == cl$cluster,
                     c("subcluster", "subcluster_num", "subcluster_name")]
    subs <- subs[order(subs$subcluster_num), ]
    subs <- subs[!duplicated(subs$subcluster), ]

    sub_panels <- .by_row(subs, function(s) {
      .subcluster_panel(ns, taxonomy, s$subcluster, s$subcluster_name)
    })

    bslib::accordion_panel(
      title = cl$cluster_name, value = cl$cluster,
      bslib::accordion(id = ns(paste0("acc_", cl$cluster)), !!!sub_panels)
    )
  })

  bslib::accordion(
    id = ns("acc"), open = clusters$cluster[[1]], !!!cluster_panels
  )
}

#' Benchmark dashboard module server
#'
#' Leaf-agnostic: it renders one card per finest-grain taxonomy node
#' (`coalesce(sub_subcluster, subcluster)`), regardless of how the UI nests
#' them.
#'
#' @param id Module id (must match the UI).
#' @param taxonomy Same taxonomy passed to [mod_benchmark_dashboard_ui()].
#' @param base_unit reactive() -> character scalar `unit_code` (focal unit).
#' @param comparison_units reactive() -> character vector of comparison
#'   `unit_code`s.
#' @param threshold reactive() -> `"Default"` or `"Terciles"`.
#' @param year_range reactive() -> integer vector of length 2 (dynamic only).
#' @return Invisibly `NULL`.
#' @export
mod_benchmark_dashboard_server <- function(id, taxonomy,
                                           base_unit, comparison_units,
                                           threshold, year_range) {
  shiny::moduleServer(id, function(input, output, session) {
    keys   <- .leaf_key(taxonomy)
    labels <- stats::setNames(.leaf_label(taxonomy), keys)

    # percent-rank / family averaging need at least a small comparison group
    enough_comparators <- shiny::reactive(length(comparison_units()) >= 2L)

    for (key in keys) local({
      leaf  <- key
      label <- labels[[leaf]]
      ct_in <- paste0("ctf_type_", leaf)

      # Everything a card shows is a pure function of this signature - it keys
      # every bindCache() below. (Same idea as the old mod_detail.R.)
      cache_key <- shiny::reactive(list(
        leaf,
        input[[ct_in]] %||% "static",
        base_unit(),
        sort(comparison_units()),
        threshold(),
        year_range()
      ))

      sel <- shiny::reactive({
        ctf_type <- input[[ct_in]] %||% "static"
        units <- unique(c(base_unit(), comparison_units()))
        yr <- if (ctf_type == "dynamic") year_range() else NULL
        list(
          ctf_type = ctf_type,
          # dynamic benchmark uses even years only (cliarappak parity, 2026-09-10)
          data = filter_unit_data(
            leaf, ctf_type, units,
            year_range = yr, even_years = identical(ctf_type, "dynamic")
          )
        )
      })

      dq <- shiny::reactive({
        s <- sel()
        if (nrow(s$data) == 0L || !enough_comparators()) return(NULL)
        fn <- if (s$ctf_type == "dynamic") def_quantiles_dyn else def_quantiles
        fn(s$data, base_unit(), comparison_units(), threshold())
      }) |>
        shiny::bindCache(cache_key())

      # live composite for the focal unit; latest year in the dynamic view
      fam <- shiny::reactive({
        s <- sel()
        if (nrow(s$data) == 0L || !enough_comparators()) return(NULL)
        f <- compute_family_average_app(
          s$data, base_unit(), comparison_units(), type = s$ctf_type
        ) |>
          dplyr::filter(.data$unit_code %in% base_unit())
        if (s$ctf_type == "dynamic") {
          f <- dplyr::slice_max(f, .data$year, n = 1L, with_ties = FALSE)
        }
        f
      }) |>
        shiny::bindCache(cache_key())

      output[[paste0("plot_", leaf)]] <- plotly::renderPlotly({
        s <- sel()
        shiny::validate(
          shiny::need(enough_comparators(),
                      "Pick a base unit and at least 2 comparators, then Apply."),
          shiny::need(nrow(s$data) > 0L,
                      sprintf("No data available for %s (%s).", label, s$ctf_type))
        )
        plot_fn <- if (s$ctf_type == "dynamic") static_plot_dyn else static_plot
        as_interactive_plot(
          plot_fn(dq(), base_unit(), tab_name = label, threshold = threshold(),
                  dots = TRUE)
        )
      }) |>
        shiny::bindCache(cache_key())

      output[[paste0("score_", leaf)]] <- shiny::renderText({
        f <- fam()
        if (is.null(f) || nrow(f) == 0L || is.na(f$score[[1]])) "n/a"
        else sprintf("%.3f", f$score[[1]])
      })

      output[[paste0("note_", leaf)]] <- shiny::renderUI({
        s <- sel()
        if (nrow(s$data) == 0L) return(NULL)
        f <- fam()
        n_ind <- dplyr::n_distinct(s$data$variable)
        base_line <- sprintf(
          "%d indicator%s; %d comparator%s; %s thresholds.",
          n_ind, if (n_ind == 1L) "" else "s",
          length(comparison_units()),
          if (length(comparison_units()) == 1L) "" else "s",
          threshold()
        )
        # Composite wiped by the missing-data screen (common in the dynamic
        # view for small leaves - see build-plan Slice 1 / option A).
        if (s$ctf_type == "dynamic" &&
            (is.null(f) || nrow(f) == 0L || is.na(f$score[[1]]))) {
          shiny::tags$p(
            class = "text-muted small",
            base_line,
            shiny::tags$br(),
            sprintf(
              paste0("Composite unavailable for the trend view: every ",
                     "indicator has a coverage gap for %s over %d-%d. ",
                     "Switch to Static for a composite score."),
              base_unit(), year_range()[[1]], year_range()[[2]]
            )
          )
        } else {
          shiny::tags$p(class = "text-muted small", base_line)
        }
      })
    })

    invisible(NULL)
  })
}
