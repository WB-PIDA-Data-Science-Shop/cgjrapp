# ---
# Overall benchmarking: cluster / subcluster / sub-subcluster averages.
#
# Port of cliarappak's "Overview" benchmark (mod_benchmark.R's
# data_family() / data_family_dyn() reactives) with CGJR's taxonomy levels
# standing in for CLIAR's institutional families:
#
#   1. family_scores() - port of cliarappak::family_data() / family_data_dyn():
#      screen the indicators, then average each unit's CTF over the screened
#      indicators of every row (a cluster, subcluster or sub-subcluster). The
#      literal CLIAR rule: a plain mean over ALL of the row's indicators, so a
#      subcluster with many indicators weighs more in its cluster's score (not
#      cgjrdata's equal-weight cgjr_scores rollup).
#   2. def_quantiles() / def_quantiles_dyn() on those scores, exactly as
#      cliarappak feeds family_data() into def_quantiles() - each row is then
#      ranked, banded and plotted like an indicator.
#
# The chart list comes from cgjr_taxonomy - no cluster / subcluster name is
# hardcoded: one "Clusters" chart, one chart per cluster (rows = its
# subclusters), and one per subcluster that branches further (rows = its
# sub-subclusters; only Public Financial Management today).
# ---

# Prefix keeping Overall card ids clear of leaf ids (`plot_<key>` etc.).
.OVERALL_PREFIX <- "overall__"

#' The Overall charts for a taxonomy
#'
#' @param taxonomy `cgjrdata::cgjr_taxonomy` or a subset with the same columns.
#' @return A list of chart specs, in display order. Each is a list with `key`
#'   (card id), `label` (accordion / plot title), `row_col` (the `cgjr_ctf`
#'   column whose values are the chart's rows), `parent_col` / `parent` (the
#'   `cgjr_ctf` slice the chart covers; `NULL` for the Clusters chart) and
#'   `rows` (named character vector, row key -> display name, in taxonomy
#'   order).
#' @noRd
.overall_charts <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  first <- function(df, key, num) {
    df <- df[order(df[[num]]), ]
    df[!duplicated(df[[key]]), ]
  }
  named <- function(df, key, name) stats::setNames(df[[name]], df[[key]])

  clusters <- first(tx, "cluster", "cluster_num")
  charts <- list(list(
    key = paste0(.OVERALL_PREFIX, "clusters"), label = "Clusters",
    row_col = "cluster", parent_col = NULL, parent = NULL,
    rows = named(clusters, "cluster", "cluster_name")
  ))

  for (i in seq_len(nrow(clusters))) {
    cl <- clusters[i, ]
    subs <- first(tx[tx$cluster == cl$cluster, ], "subcluster", "subcluster_num")
    charts[[length(charts) + 1L]] <- list(
      key = paste0(.OVERALL_PREFIX, cl$cluster), label = cl$cluster_name,
      row_col = "subcluster", parent_col = "cluster", parent = cl$cluster,
      rows = named(subs, "subcluster", "subcluster_name")
    )
    for (j in seq_len(nrow(subs))) {
      sb <- subs[j, ]
      ssub <- tx[tx$subcluster == sb$subcluster & !is.na(tx$sub_subcluster), ]
      if (nrow(ssub) == 0L) next
      ssub <- first(ssub, "sub_subcluster", "sub_subcluster_num")
      charts[[length(charts) + 1L]] <- list(
        key = paste0(.OVERALL_PREFIX, sb$subcluster), label = sb$subcluster_name,
        row_col = "sub_subcluster", parent_col = "subcluster",
        parent = sb$subcluster,
        rows = named(ssub, "sub_subcluster", "sub_subcluster_name")
      )
    }
  }
  charts
}

# Leaf keys a chart's slice covers (every leaf for the Clusters chart).
.overall_leaves <- function(chart, taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  if (!is.null(chart$parent_col)) tx <- tx[tx[[chart$parent_col]] == chart$parent, ]
  unique(dplyr::coalesce(tx$sub_subcluster, tx$subcluster))
}

# The indicator screen of cliarappak::family_data() (static: base unit has ANY
# NA, or flat across base + comparison units) / family_data_dyn() (dynamic:
# base unit 100% NA; no low-variance screen). Returns dropped `variable`s.
.family_screen <- function(dat, base_unit, type) {
  if (type == "static") {
    union(.missing_for_base(dat, base_unit, rule = "any"), .low_variance(dat))
  } else {
    .missing_for_base(dat, base_unit, rule = "all")
  }
}

#' Cluster / subcluster average scores for a selection
#'
#' Long-shape port of `cliarappak::family_data()` (static) and
#' `family_data_dyn()` (dynamic), with a CGJR taxonomy level in place of
#' CLIAR's institutional family. Screens the indicators the way those
#' functions do, then averages each unit's CTF over the surviving indicators
#' of every row - a plain mean over all of the row's indicators (`NaN`, a row
#' with nothing observed, becomes `NA`). The result has the shape
#' [def_quantiles()] / [def_quantiles_dyn()] take, so each row is then
#' benchmarked like an indicator, as `cliarappak` does for its Overview.
#'
#' Screens: static drops indicators the base unit has *any* `NA` for, and
#' indicators flat (25th == 75th percentile) across the base + comparison
#' units; dynamic drops only indicators the base unit is *100%* `NA` for.
#'
#' @inheritParams def_quantiles
#' @param ctf_long Long CTF slice ([filter_unit_data()]) covering every row's
#'   indicators.
#' @param row_col The `ctf_long` column whose values are the rows:
#'   `"cluster"`, `"subcluster"` or `"sub_subcluster"`.
#' @param rows Named character vector, row key -> display name; becomes the
#'   `indicator` column. Rows not in `rows` are dropped.
#' @param type `"static"` or `"dynamic"`.
#'
#' @return A long tibble - `unit_code`, `unit_name`, (`year`,) `variable` (the
#'   row key), `indicator` (its display name) and `ctf` (the average) - one row
#'   per unit x row (x year).
#' @seealso [def_quantiles()], [overall_excluded()]
#' @export
family_scores <- function(ctf_long, base_unit, comparison_units, row_col, rows,
                          type = c("static", "dynamic")) {
  type <- match.arg(type)
  keep_units <- unique(c(base_unit, comparison_units))

  dat <- dplyr::filter(ctf_long, .data$unit_code %in% keep_units,
                       .data[[row_col]] %in% names(rows))
  dat <- dplyr::filter(dat, !.data$variable %in% .family_screen(dat, base_unit, type))

  dat |>
    dplyr::group_by(
      .data$unit_code, .data$unit_name,
      dplyr::across(dplyr::any_of(if (type == "dynamic") "year")),
      variable = .data[[row_col]]
    ) |>
    dplyr::summarise(ctf = mean(.data$ctf, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      ctf = ifelse(is.nan(.data$ctf), NA_real_, .data$ctf),
      indicator = unname(rows[.data$variable])
    )
}

#' Indicators left out of each row of a static Overall chart
#'
#' The indicators [family_scores()]'s static screen drops, grouped by the row
#' they would have counted towards - the "Show notes" text of an Overall card.
#'
#' @inheritParams family_scores
#' @return A tibble, one row per chart row that lost indicators: `row` (display
#'   name), `n_total` (the row's indicators), `excluded` (list of dropped
#'   indicator display names). Rows in taxonomy order.
#' @export
overall_excluded <- function(ctf_long, base_unit, comparison_units, row_col, rows) {
  keep_units <- unique(c(base_unit, comparison_units))
  dat <- dplyr::filter(ctf_long, .data$unit_code %in% keep_units,
                       .data[[row_col]] %in% names(rows))
  dropped <- .family_screen(dat, base_unit, "static")

  ind <- dplyr::distinct(dat, row = .data[[row_col]], .data$variable, .data$indicator)
  out <- ind |>
    dplyr::group_by(.data$row) |>
    dplyr::summarise(
      n_total  = dplyr::n_distinct(.data$variable),
      excluded = list(sort(unique(.data$indicator[.data$variable %in% dropped]))),
      .groups = "drop"
    ) |>
    dplyr::filter(lengths(.data$excluded) > 0L)
  out <- out[order(match(out$row, names(rows))), ]
  out$row <- unname(rows[out$row])
  out
}

#' Build the "Show notes" text for a static Overall chart
#'
#' Same opening line as [plot_notes()] (who is compared to whom), then the
#' excluded indicators grouped by the row they belong to, e.g.
#' "Institutional Environment: 3 of 21 indicators not considered (...)".
#'
#' @inheritParams plot_notes
#' @param excluded Output of [overall_excluded()].
#' @return An [shiny::HTML()] string.
#' @seealso [overall_excluded()], [plot_notes()]
#' @export
overall_notes <- function(base_unit, comparison_units, ctf_long, excluded) {
  txt <- plot_notes(base_unit, comparison_units, ctf_long, character(0))
  if (nrow(excluded) == 0L) return(txt)
  lines <- sprintf(
    "<b>%s</b>: %d of %d indicator%s not considered (%s).",
    htmltools::htmlEscape(excluded$row), lengths(excluded$excluded),
    excluded$n_total, ifelse(excluded$n_total == 1L, "", "s"),
    vapply(excluded$excluded,
           function(x) htmltools::htmlEscape(paste(x, collapse = ", ")),
           character(1))
  )
  shiny::HTML(paste0(
    txt,
    "<br><br>Indicators are not considered when the base unit has no",
    " information for them, or because of low variance:<br>",
    paste(lines, collapse = "<br>")
  ))
}
