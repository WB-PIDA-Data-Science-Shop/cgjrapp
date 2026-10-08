# ---
# AI Report, Phase 1: the evidence pack.
#
# Everything the report (and, later, the LLM narrative and chat) may state
# about a selection, computed here in R by the dashboard's own, parity-tested
# functions - never by the model:
#
#   * Overall: cluster / subcluster / sub-subcluster averages
#     (family_scores() -> def_quantiles() / def_quantiles_dyn()), the same
#     numbers the Benchmarking tab's Overall charts draw;
#   * every indicator of every leaf (def_quantiles() / def_quantiles_dyn()),
#     as the leaf cards draw them;
#   * dynamic change in CTF and rank (.change_table(), the chart labels' code);
#   * excluded indicators and not-yet-measured areas.
#
# Each statement is a "fact" with a stable id (E1, E2, ...). The LLM will only
# be allowed to cite these ids, and its numbers are checked against them.
# ---

.flat_status <- function(x) gsub("\\s*\\n\\s*", " ", x)

# One fact row per base-unit row of a def_quantiles() result (static).
.static_facts <- function(dq, base_unit, level, cluster, parent, leaf = NA_character_) {
  if (is.null(dq) || nrow(dq) == 0L) return(NULL)
  n_ranked <- dplyr::count(dq, .data$variable, name = "n_ranked")
  comp_med <- dq |>
    dplyr::filter(!.data$unit_code %in% base_unit) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(comp_median = stats::median(.data$dtf, na.rm = TRUE),
                     .groups = "drop")
  dq |>
    dplyr::filter(.data$unit_code %in% base_unit) |>
    dplyr::left_join(n_ranked, by = "variable") |>
    dplyr::left_join(comp_med, by = "variable") |>
    dplyr::transmute(
      view = "static", level = level, cluster = cluster, parent = parent,
      leaf = leaf, name = as.character(.data$indicator),
      ctf = .data$dtf, status = .flat_status(.data$status),
      rank = .data$nrank, n_ranked = .data$n_ranked,
      comp_median = .data$comp_median
    )
}

# One fact row per indicator / row of a def_quantiles_dyn() result: the
# change between the base unit's first and last year.
.dynamic_facts <- function(dq, base_unit, level, cluster, parent, leaf = NA_character_) {
  if (is.null(dq) || nrow(dq) == 0L) return(NULL)
  base_df <- dplyr::filter(dq, .data$unit_code %in% base_unit)
  keep <- .trend_indicators(base_df)
  if (length(keep) == 0L) return(NULL)
  ch <- .change_table(dq, dplyr::filter(base_df, .data$indicator %in% keep))
  dplyr::transmute(
    ch,
    view = "dynamic", level = level, cluster = cluster, parent = parent,
    leaf = leaf, name = as.character(.data$indicator),
    year_first = .data$year_first, year_last = .data$year_last,
    delta = .data$delta,
    rank_first = .data$rank_first, n_first = .data$n_first,
    rank_last = .data$rank_last, n_last = .data$n_last
  )
}

# The deterministic sentence for each fact - what the register, the Word
# tables and (later) the LLM prompt all show.
.fact_text <- function(f, base_name) {
  lvl <- c(cluster = "cluster average", subcluster = "subcluster average",
           sub_subcluster = "sub-subcluster average", indicator = "indicator")
  what <- sprintf("%s (%s)", f$name, lvl[f$level])
  ifelse(
    f$view == "static",
    sprintf("%s for %s: CTF %.3f; %s; rank %d of %d; comparator median %.3f.",
            what, base_name, f$ctf, f$status, f$rank, f$n_ranked, f$comp_median),
    sprintf("%s for %s, %d-%d: change in CTF %+.3f; rank %s.",
            what, base_name, f$year_first, f$year_last, f$delta,
            ifelse(f$rank_first == f$rank_last,
                   sprintf("unchanged at %d", f$rank_last),
                   sprintf("from %d of %d to %d of %d",
                           f$rank_first, f$n_first, f$rank_last, f$n_last)))
  )
}

#' Build the evidence pack for an AI Report
#'
#' Computes, with the dashboard's own functions, everything a report on the
#' selection may state: the Overall cluster / subcluster averages and every
#' leaf's indicators (static: CTF, Weak / Emerging / Strong band, rank,
#' comparator median; dynamic: change in CTF and rank), the indicators each
#' screen excluded, and the areas with no indicators yet. Each statement is a
#' numbered fact (`E1`, `E2`, ...) - the only figures a report may cite.
#'
#' @param base_unit Character scalar - one focal `unit_code` (a narrative
#'   needs a single focal unit).
#' @param comparison_units Character vector of comparison `unit_code`s.
#' @param threshold `"Default"` or `"Terciles"`.
#' @param year_range Integer length-2 vector for the dynamic view.
#' @param clusters Cluster keys to report on (`NULL` = all). The Overview
#'   always covers every cluster.
#' @param taxonomy `cgjrdata::cgjr_taxonomy`.
#'
#' @return A list: `meta` (selection), `facts` (tibble: `id`, `view`,
#'   `level`, `cluster`, `parent`, `leaf`, `name`, the numbers, `text`),
#'   `excluded` (tibble: `cluster`, `leaf`, `indicator`), `gaps` (tibble:
#'   `cluster`, `leaf`, `name` - areas with no indicators for the base unit),
#'   `charts` (list of `list(key, title, view, dq, row_order)` to draw).
#' @seealso [priority_areas()], [evidence_report_docx()]
#' @export
build_evidence <- function(base_unit, comparison_units,
                           threshold = c("Default", "Terciles"),
                           year_range = c(2013L, 2024L), clusters = NULL,
                           taxonomy = cgjrdata::cgjr_taxonomy) {
  threshold <- match.arg(threshold)
  stopifnot(
    "build_evidence() needs exactly one base unit" = length(base_unit) == 1L,
    "build_evidence() needs at least 2 comparison units" =
      length(setdiff(comparison_units, base_unit)) >= 2L
  )
  comparison_units <- setdiff(comparison_units, base_unit)
  tx <- as.data.frame(taxonomy)
  all_clusters <- unique(tx$cluster[order(tx$cluster_num)])
  clusters <- if (is.null(clusters)) all_clusters else intersect(all_clusters, clusters)

  units <- c(base_unit, comparison_units)
  st  <- filter_unit_data(NULL, "static", units)
  dyn <- filter_unit_data(NULL, "dynamic", units, year_range = year_range,
                          even_years = TRUE)
  base_name <- dplyr::coalesce(
    st$unit_name[match(base_unit, st$unit_code)],
    dyn$unit_name[match(base_unit, dyn$unit_code)],
    base_unit
  )

  facts <- list(); charts <- list(); excluded <- list()
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)

  # Overall charts: the Clusters one always, cluster / branching ones if selected
  for (ch in .overall_charts(taxonomy)) {
    in_scope <- is.null(ch$parent_col) ||
      (ch$parent_col == "cluster" && ch$parent %in% clusters) ||
      (ch$parent_col == "subcluster" &&
         unique(tx$cluster[tx$subcluster == ch$parent]) %in% clusters)
    if (!in_scope) next
    leaves <- .overall_leaves(ch, taxonomy)
    cl <- if (is.null(ch$parent_col)) NA_character_ else
      if (ch$parent_col == "cluster") ch$parent else
        unique(tx$cluster[tx$subcluster == ch$parent])
    level <- ch$row_col
    for (view in c("static", "dynamic")) {
      dat <- if (view == "static") st else dyn
      dat <- dat[dat$leaf %in% leaves, ]
      if (nrow(dat) == 0L) next
      qfn <- if (view == "static") def_quantiles else def_quantiles_dyn
      dq <- safe(qfn(family_scores(dat, base_unit, comparison_units, ch$row_col,
                                   ch$rows, type = view),
                     base_unit, comparison_units, threshold))
      if (is.null(dq) || nrow(dq) == 0L) next
      ffn <- if (view == "static") .static_facts else .dynamic_facts
      facts[[length(facts) + 1L]] <- ffn(dq, base_unit, level, cl, ch$label)
      charts[[length(charts) + 1L]] <- list(
        key = ch$key, title = ch$label, view = view, dq = dq,
        row_order = unname(ch$rows), cluster = cl, leaf = NA_character_
      )
    }
  }

  # Leaves of the selected clusters: indicator-level facts and charts
  leaf_tbl <- tx[tx$cluster %in% clusters, ]
  leaf_tbl <- leaf_tbl[order(leaf_tbl$cluster_num, leaf_tbl$subcluster_num,
                             dplyr::coalesce(leaf_tbl$sub_subcluster_num, 0L)), ]
  leaf_tbl$leaf <- .leaf_key(leaf_tbl)
  leaf_tbl$leaf_name <- .leaf_label(leaf_tbl)
  leaf_tbl <- leaf_tbl[!duplicated(leaf_tbl$leaf), ]

  gaps <- list()
  for (i in seq_len(nrow(leaf_tbl))) {
    lf <- leaf_tbl[i, ]
    s_dat <- st[st$leaf == lf$leaf, ]
    d_dat <- dyn[dyn$leaf == lf$leaf, ]
    s_dq <- if (nrow(s_dat)) safe(def_quantiles(s_dat, base_unit, comparison_units, threshold))
    d_dq <- if (nrow(d_dat)) safe(def_quantiles_dyn(d_dat, base_unit, comparison_units, threshold))
    if (is.null(s_dq) || nrow(s_dq) == 0L) {
      gaps[[length(gaps) + 1L]] <- data.frame(cluster = lf$cluster, leaf = lf$leaf,
                                              name = lf$leaf_name)
      next
    }
    facts[[length(facts) + 1L]] <- .static_facts(s_dq, base_unit, "indicator",
                                                  lf$cluster, lf$leaf_name, lf$leaf)
    facts[[length(facts) + 1L]] <- .dynamic_facts(d_dq, base_unit, "indicator",
                                                   lf$cluster, lf$leaf_name, lf$leaf)
    charts[[length(charts) + 1L]] <- list(
      key = lf$leaf, title = lf$leaf_name, view = "static", dq = s_dq,
      row_order = NULL, cluster = lf$cluster, leaf = lf$leaf
    )
    ex <- excluded_indicators(s_dat, base_unit, comparison_units)
    if (length(ex)) {
      excluded[[length(excluded) + 1L]] <- data.frame(cluster = lf$cluster,
                                                      leaf = lf$leaf_name,
                                                      indicator = ex)
    }
  }

  facts <- dplyr::bind_rows(facts)
  facts$id <- paste0("E", seq_len(nrow(facts)))
  facts$text <- .fact_text(facts, base_name)
  facts <- dplyr::relocate(facts, "id")

  list(
    meta = list(
      base_unit = base_unit, base_name = base_name,
      comparison_units = comparison_units,
      comparison_names = dplyr::coalesce(
        st$unit_name[match(comparison_units, st$unit_code)], comparison_units),
      threshold = threshold, year_range = as.integer(range(year_range)),
      clusters = clusters,
      cluster_names = stats::setNames(
        tx$cluster_name[match(clusters, tx$cluster)], clusters),
      generated = Sys.Date()
    ),
    facts = facts,
    # typed even when empty, so callers can always read the columns
    excluded = dplyr::bind_rows(
      data.frame(cluster = character(), leaf = character(), indicator = character()),
      excluded),
    gaps = dplyr::bind_rows(
      data.frame(cluster = character(), leaf = character(), name = character()),
      gaps),
    charts = charts
  )
}

#' Priority areas from an evidence pack
#'
#' Ranks the finest averaged rows - subclusters, and sub-subclusters where a
#' subcluster branches - by band (Weak, then Emerging, then Strong) and then by
#' distance from the frontier (lowest CTF first). Data-driven: this, not the
#' LLM, decides which weaknesses a report leads with.
#'
#' @param evidence Output of [build_evidence()].
#' @return The static fact rows, ordered most to least pressing, with `gap`
#'   (`ctf - comp_median`, the distance from the comparison group).
#' @export
priority_areas <- function(evidence) {
  f <- evidence$facts
  f <- f[f$view == "static" & f$level %in% c("subcluster", "sub_subcluster"), ]
  # a branching subcluster is represented by its sub-subclusters
  branching <- unique(f$parent[f$level == "sub_subcluster"])
  f <- f[!(f$level == "subcluster" & f$name %in% branching), ]
  band <- ifelse(startsWith(f$status, "Weak"), 1L,
                 ifelse(startsWith(f$status, "Emerging"), 2L, 3L))
  f$gap <- f$ctf - f$comp_median
  f[order(band, f$ctf), ]
}
