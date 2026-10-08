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
# live benchmarking plot, a notes line, and (static view only) a "Show notes"
# switch that reveals which indicators were excluded and why - a port of
# cliarappak's plot_notes_function() / "Show notes" switch. No composite
# score - cliarappak's benchmarking plots don't show one either, only the
# plot. Drawn once per finest-grain node - `coalesce(sub_subcluster,
# subcluster)`.
#
# After the clusters, an "Overall" item holds the same card for every
# .overall_charts() entry (R/utils_overall.R): a Clusters chart, one chart per
# cluster of its subcluster averages, and one per branching subcluster of its
# sub-subcluster averages - a port of cliarappak's Overview benchmark.
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
  ct_id    <- paste0("ctf_type_", key)
  notes_id <- paste0("show_notes_", key)
  shiny::tagList(
    shiny::radioButtons(
      ns(ct_id),
      label = NULL,
      choices = c("Static (latest snapshot)" = "static",
                  "Dynamic (over time)" = "dynamic"),
      selected = "static",
      inline = TRUE
    ),
    # fixed-height scroll window (cliarappak parity); the plot inside is sized
    # by benchmark_plot_height() and scrolls when it is taller than the window
    shiny::tags$div(
      class = "cgjr-plot-window",
      plotly::plotlyOutput(ns(paste0("plot_", key)),
                           height = paste0(.PLOT_WINDOW_PX, "px"))
    ),
    shiny::htmlOutput(ns(paste0("note_", key))),
    # "Show notes" toggle, static view only - cliarappak parity: its
    # plot_notes_function() returns no notes for the dynamic view either.
    shiny::conditionalPanel(
      sprintf("input.%s == 'static'", ct_id), ns = ns,
      shinyWidgets::materialSwitch(ns(notes_id), "Show notes",
                                   value = FALSE, status = "success"),
      shiny::conditionalPanel(
        sprintf("input.%s == true", notes_id), ns = ns,
        shiny::htmlOutput(ns(paste0("plot_notes_", key)))
      )
    )
  )
}

# iterate a distinct/ordered tibble one row at a time (row is a 1-row tibble)
.by_row <- function(tbl, f) purrr::map(seq_len(nrow(tbl)), function(i) f(tbl[i, ]))

# One subcluster panel: a leaf card, or (branching) an inner accordion of
# sub_subcluster leaf cards.
.subcluster_panel <- function(ns, taxonomy, sub_key, sub_name) {
  rows <- taxonomy[taxonomy$subcluster == sub_key, ]

  if (all(is.na(rows$sub_subcluster))) {
    return(bs4Dash::accordionItem(
      title = sub_name,
      .leaf_card_ui(ns, key = sub_key, label = sub_name)
    ))
  }

  ssub <- rows[order(rows$sub_subcluster_num),
               c("sub_subcluster", "sub_subcluster_name")]
  ssub <- ssub[!duplicated(ssub$sub_subcluster), ]
  inner <- .by_row(ssub, function(r) {
    bs4Dash::accordionItem(
      title = r$sub_subcluster_name,
      .leaf_card_ui(ns, key = r$sub_subcluster, label = r$sub_subcluster_name)
    )
  })
  bs4Dash::accordionItem(
    title = sub_name,
    bs4Dash::accordion(id = ns(paste0("acc_", sub_key)), .list = inner)
  )
}

#' Benchmark dashboard module UI
#'
#' @param id Module id.
#' @param taxonomy `cgjrdata::cgjr_taxonomy` (or any subset with the same
#'   columns). The accordion structure is generated entirely from its
#'   `cluster_num` / `subcluster_num` / `sub_subcluster_num` ordering and
#'   `*_name` labels.
#' @return A nested [bs4Dash::accordion()] - one item per cluster, then the
#'   Overall item (cluster / subcluster average charts, see
#'   [family_scores()]) - the content of the `"benchmarking"`
#'   [bs4Dash::tabItem()].
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

    bs4Dash::accordionItem(
      title = cl$cluster_name,
      # the first cluster starts open, as before
      collapsed = cl$cluster != clusters$cluster[[1]],
      bs4Dash::accordion(id = ns(paste0("acc_", cl$cluster)), .list = sub_panels)
    )
  })

  # Overall: one card per .overall_charts() entry, the Clusters chart open
  charts <- .overall_charts(taxonomy)
  overall_panels <- lapply(seq_along(charts), function(i) {
    ch <- charts[[i]]
    bs4Dash::accordionItem(
      title = ch$label, collapsed = i != 1L,
      .leaf_card_ui(ns, key = ch$key, label = ch$label)
    )
  })
  overall <- bs4Dash::accordionItem(
    title = "Overall",
    bs4Dash::accordion(id = ns("acc_overall"), .list = overall_panels)
  )

  bs4Dash::accordion(id = ns("acc"), .list = c(cluster_panels, list(overall)))
}

# The per-card server logic shared by leaf cards and Overall cards. `card` is
# a list: `key`, `label`, `leaves` (the cgjr_ctf slice), `quantiles(data,
# ctf_type)` -> def_quantiles()-shaped rows, `notes(data)` -> "Show notes" HTML,
# `count(data)` -> the leading phrase of the notes line, `row_order` (NULL, or
# a fixed top-to-bottom row order). `sel_in` holds the selection reactives.
.card_server <- function(input, output, card, sel_in) {
  key   <- card$key
  label <- card$label
  ct_in <- paste0("ctf_type_", key)
  base_unit <- sel_in$base_unit
  comparison_units <- sel_in$comparison_units
  threshold <- sel_in$threshold
  year_range <- sel_in$year_range
  enough_comparators <- sel_in$enough_comparators

  # Everything a card shows is a pure function of this signature - it keys
  # every bindCache() below. (Same idea as the old mod_detail.R.)
  cache_key <- shiny::reactive(list(
    key,
    input[[ct_in]] %||% "static",
    sort(base_unit()),
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
        card$leaves, ctf_type, units,
        year_range = yr, even_years = identical(ctf_type, "dynamic")
      )
    )
  })

  dq <- shiny::reactive({
    s <- sel()
    if (nrow(s$data) == 0L || !enough_comparators()) return(NULL)
    card$quantiles(s$data, s$ctf_type)
  }) |>
    shiny::bindCache(cache_key())

  output[[paste0("plot_", key)]] <- plotly::renderPlotly({
    s <- sel()
    shiny::validate(
      shiny::need(enough_comparators(),
                  "Pick a base unit and at least 2 comparators, then Apply."),
      shiny::need(nrow(s$data) > 0L,
                  sprintf("No data available for %s (%s).", label, s$ctf_type)),
      # cliarappak parity: a trend line doesn't generalize to multiple
      # base units the way the static view's per-unit shape does.
      shiny::need(s$ctf_type != "dynamic" || length(base_unit()) == 1L,
                  "Dynamic view is available only when a single base unit is selected.")
    )
    plot_fn <- if (s$ctf_type == "dynamic") static_plot_dyn else static_plot
    as_interactive_plot(
      plot_fn(dq(), base_unit(), tab_name = label, threshold = threshold(),
              dots = TRUE, row_order = card$row_order),
      height   = benchmark_plot_height(dq(), base_unit(), s$ctf_type),
      filename = paste0("cgjr_", key, "_", s$ctf_type)
    )
  }) |>
    shiny::bindCache(cache_key())

  output[[paste0("note_", key)]] <- shiny::renderUI({
    s <- sel()
    if (nrow(s$data) == 0L) return(NULL)
    base_line <- sprintf(
      "%s; %d comparator%s; %s thresholds.",
      card$count(s$data),
      length(comparison_units()),
      if (length(comparison_units()) == 1L) "" else "s",
      threshold()
    )
    shiny::tags$p(class = "text-muted small", base_line)
  })

  # "Show notes" panel - static view only (see .leaf_card_ui()).
  output[[paste0("plot_notes_", key)]] <- shiny::renderUI({
    s <- sel()
    if (s$ctf_type != "static" || nrow(s$data) == 0L || !enough_comparators()) {
      return(NULL)
    }
    card$notes(s$data)
  }) |>
    shiny::bindCache(cache_key())

  invisible(NULL)
}

.plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1L) "" else "s")

#' Benchmark dashboard module server
#'
#' Renders one card per finest-grain taxonomy node
#' (`coalesce(sub_subcluster, subcluster)`), regardless of how the UI nests
#' them, plus one per Overall chart: cluster / subcluster / sub-subcluster
#' averages ([family_scores()], a port of `cliarappak`'s Overview), each
#' benchmarked by [def_quantiles()] / [def_quantiles_dyn()] like an indicator
#' and drawn in fixed taxonomy order.
#'
#' @param id Module id (must match the UI).
#' @param taxonomy Same taxonomy passed to [mod_benchmark_dashboard_ui()].
#' @param base_unit reactive() -> character vector of focal `unit_code`(s).
#'   The dynamic view is restricted to exactly one (cliarappak parity); the
#'   static view supports several, each drawn with its own point shape.
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
    sel_in <- list(
      base_unit = base_unit, comparison_units = comparison_units,
      threshold = threshold, year_range = year_range,
      # percent-rank / family averaging need at least a small comparison group
      enough_comparators = shiny::reactive(length(comparison_units()) >= 2L)
    )
    quantile_fn <- function(ctf_type) {
      if (ctf_type == "dynamic") def_quantiles_dyn else def_quantiles
    }

    # leaf cards: indicators benchmarked directly
    keys   <- .leaf_key(taxonomy)
    labels <- stats::setNames(.leaf_label(taxonomy), keys)
    for (key in unique(keys)) local({
      leaf <- key
      .card_server(input, output, sel_in = sel_in, card = list(
        key = leaf, label = labels[[leaf]], leaves = leaf, row_order = NULL,
        quantiles = function(data, ctf_type) {
          quantile_fn(ctf_type)(data, base_unit(), comparison_units(), threshold())
        },
        notes = function(data) {
          plot_notes(base_unit(), comparison_units(), data,
                     excluded_indicators(data, base_unit(), comparison_units()))
        },
        count = function(data) .plural(dplyr::n_distinct(data$variable), "indicator")
      ))
    })

    # Overall cards: cluster / subcluster averages, benchmarked like indicators
    for (chart in .overall_charts(taxonomy)) local({
      ch <- chart
      word <- c(cluster = "cluster", subcluster = "subcluster",
                sub_subcluster = "sub-subcluster")[[ch$row_col]]
      .card_server(input, output, sel_in = sel_in, card = list(
        key = ch$key, label = ch$label, row_order = unname(ch$rows),
        leaves = .overall_leaves(ch, taxonomy),
        quantiles = function(data, ctf_type) {
          scores <- family_scores(data, base_unit(), comparison_units(),
                                  ch$row_col, ch$rows, type = ctf_type)
          quantile_fn(ctf_type)(scores, base_unit(), comparison_units(), threshold())
        },
        notes = function(data) {
          overall_notes(base_unit(), comparison_units(), data,
                        overall_excluded(data, base_unit(), comparison_units(),
                                         ch$row_col, ch$rows))
        },
        count = function(data) {
          rows <- intersect(names(ch$rows), unique(data[[ch$row_col]]))
          sprintf("%s (%s)", .plural(length(rows), word),
                  .plural(dplyr::n_distinct(data$variable), "indicator"))
        }
      ))
    })

    invisible(NULL)
  })
}
