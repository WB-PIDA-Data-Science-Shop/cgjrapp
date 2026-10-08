# ---
# AI Report, Phase 1: the Word report built from an evidence pack.
#
# Charts are the dashboard's own static_plot() / static_plot_dyn() figures
# (drawn from the evidence pack's stored def_quantiles() results, so they are
# the exact data behind every number in the tables), tables are the fact
# rows, and an appendix lists every fact with its id. With `narrative = NULL`
# this is the data-only report; Phase 3 slots LLM text into the same layout.
# ---

# Inches for a chart: grow with its rows, capped at a page.
.chart_inches <- function(chart, base_unit) {
  base <- chart$dq[chart$dq$unit_code %in% base_unit, ]
  if (chart$view == "static") {
    n <- dplyr::n_distinct(base$indicator)
    min(8.5, 1.8 + 0.32 * n)
  } else {
    n <- length(.trend_indicators(base))
    min(8.5, 1.6 + 2.3 * ceiling(n / 2))
  }
}

.chart_png <- function(chart, meta, dir) {
  fn <- if (chart$view == "static") static_plot else static_plot_dyn
  p <- fn(chart$dq, meta$base_unit, tab_name = chart$title,
          threshold = meta$threshold, dots = TRUE, row_order = chart$row_order)
  path <- file.path(dir, sprintf("%s_%s.png", chart$key, chart$view))
  h <- .chart_inches(chart, meta$base_unit)
  # dynamic facet titles carry the change-in-CTF/rank lines, too long for
  # two 3-inch columns: draw on a wider canvas and let Word scale it to the
  # page, which shrinks the text in proportion
  w <- if (chart$view == "dynamic") 9 else 6.5
  ggplot2::ggsave(path, p, width = w, height = h * w / 6.5, dpi = 150, bg = "white")
  list(path = path, height = h)
}

.static_table <- function(f) {
  data.frame(
    Area = f$name, CTF = sprintf("%.3f", f$ctf), Band = f$status,
    Rank = sprintf("%d of %d", f$rank, f$n_ranked),
    `Comparator median` = sprintf("%.3f", f$comp_median),
    Evidence = f$id, check.names = FALSE
  )
}

.dynamic_table <- function(f) {
  data.frame(
    Area = f$name, Years = sprintf("%d-%d", f$year_first, f$year_last),
    `Change in CTF` = sprintf("%+.3f", f$delta),
    `Rank (first -> last)` = sprintf("%d of %d -> %d of %d",
                                     f$rank_first, f$n_first, f$rank_last, f$n_last),
    Evidence = f$id, check.names = FALSE
  )
}

.add_table <- function(doc, df) {
  if (nrow(df) == 0L) return(doc)
  officer::body_add_table(doc, df, style = "table_template") |>
    officer::body_add_par("", style = "Normal")
}

.add_chart <- function(doc, chart, meta, dir) {
  img <- tryCatch(.chart_png(chart, meta, dir), error = function(e) NULL)
  if (is.null(img)) return(doc)
  officer::body_add_img(doc, img$path, width = 6.5, height = img$height)
}

# Markdown narrative (Phase 3) -> paragraphs; NULL / "" adds nothing.
.add_narrative <- function(doc, text) {
  if (is.null(text) || !nzchar(text)) return(doc)
  for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    t <- trimws(line)
    if (!nzchar(t)) next
    doc <- if (startsWith(t, "### ")) {
      officer::body_add_par(doc, sub("^### ", "", t), style = "heading 3")
    } else {
      officer::body_add_par(doc, gsub("^[-*+]\\s+", "", t), style = "Normal")
    }
  }
  doc
}

# Chart, then its table(s), for one chart key + view.
.add_chart_block <- function(doc, ev, key, view, level_filter, dir) {
  ch <- Filter(function(c) c$key == key && c$view == view, ev$charts)
  if (length(ch) == 0L) return(doc)
  ch <- ch[[1]]
  f <- ev$facts[ev$facts$view == view & level_filter(ev$facts), ]
  doc <- .add_chart(doc, ch, ev$meta, dir)
  .add_table(doc, if (view == "static") .static_table(f) else .dynamic_table(f))
}

#' Build the AI Report Word document from an evidence pack
#'
#' Overview (the Overall Clusters chart and table, the dynamic change, and the
#' data-driven priority areas), then one chapter per selected cluster: its
#' subcluster-average chart and table, then each leaf's indicator chart,
#' indicator table, dynamic changes and excluded indicators, and the areas not
#' yet measured. An appendix lists every fact with its evidence id.
#'
#' @param evidence Output of [build_evidence()].
#' @param narrative Optional named list of Markdown text, keyed `"overview"`,
#'   a cluster key, or a leaf key, inserted under that heading (Phase 3: the
#'   LLM's text). `NULL` gives the data-only report.
#' @return An `officer` `rdocx`; write it with `print(doc, target = path)`.
#' @seealso [build_evidence()], [priority_areas()]
#' @export
evidence_report_docx <- function(evidence, narrative = NULL) {
  ev <- evidence; m <- ev$meta; f <- ev$facts
  dir <- tempfile("cgjr_charts_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  nar <- function(key) if (is.null(narrative)) NULL else narrative[[key]]

  doc <- officer::read_docx() |>
    officer::body_add_par(paste0("Governance and Public Institutions: ", m$base_name),
                          style = "heading 1") |>
    officer::body_add_par(
      paste0(if (is.null(narrative)) "Data report" else "AI-assisted draft",
             " - ", format(m$generated, "%B %d, %Y")),
      style = "heading 2") |>
    officer::body_add_par(sprintf(
      "Selection: %s compared to %d units (%s). Thresholds: %s. Dynamic view: even years %d-%d.",
      m$base_name, length(m$comparison_units),
      paste(m$comparison_names, collapse = ", "), m$threshold,
      m$year_range[[1]], m$year_range[[2]]), style = "Normal") |>
    officer::body_add_par(paste0(
      "Every figure in this document was computed by the GOVCGJR Benchmarking ",
      "Dashboard for the selection above; evidence ids (E1, E2, ...) refer to ",
      "the register in the appendix.",
      if (is.null(narrative)) " This version contains no AI-generated text." else
        " Narrative text was drafted by a language model from these figures and must be reviewed before use."
    ), style = "Normal")

  # Overview
  doc <- officer::body_add_par(doc, "Overview", style = "heading 1")
  doc <- .add_narrative(doc, nar("overview"))
  is_cluster <- function(x) x$level == "cluster"
  doc <- .add_chart_block(doc, ev, "overall__clusters", "static", is_cluster, dir)
  doc <- .add_chart_block(doc, ev, "overall__clusters", "dynamic", is_cluster, dir)
  pa <- priority_areas(ev)
  pa <- pa[pa$cluster %in% m$clusters, ]
  if (nrow(pa)) {
    doc <- officer::body_add_par(doc, "Priority areas (lowest band, then furthest from the frontier)",
                                 style = "heading 3")
    doc <- .add_table(doc, transform(.static_table(pa),
                                     `Gap to comparators` = sprintf("%+.3f", pa$gap),
                                     check.names = FALSE))
  }

  # One chapter per selected cluster
  for (cl in m$clusters) {
    doc <- officer::body_add_par(doc, m$cluster_names[[cl]], style = "heading 1")
    doc <- .add_narrative(doc, nar(cl))
    in_cl <- function(x) x$level == "subcluster" & x$cluster %in% cl
    doc <- .add_chart_block(doc, ev, paste0(.OVERALL_PREFIX, cl), "static", in_cl, dir)
    doc <- .add_chart_block(doc, ev, paste0(.OVERALL_PREFIX, cl), "dynamic", in_cl, dir)

    # branching subclusters' own charts (e.g. Public Financial Management)
    for (ch in Filter(function(c) c$cluster %in% cl && is.na(c$leaf) &&
                        !c$key %in% paste0(.OVERALL_PREFIX, c("clusters", cl)),
                      ev$charts)) {
      in_br <- function(x) x$level == "sub_subcluster" & x$parent == ch$title
      doc <- .add_chart_block(doc, ev, ch$key, ch$view, in_br, dir)
    }

    for (ch in Filter(function(c) c$cluster %in% cl && !is.na(c$leaf), ev$charts)) {
      doc <- officer::body_add_par(doc, ch$title, style = "heading 2")
      doc <- .add_narrative(doc, nar(ch$leaf))
      doc <- .add_chart(doc, ch, m, dir)
      lf <- f[f$level == "indicator" & f$leaf %in% ch$leaf, ]
      doc <- .add_table(doc, .static_table(lf[lf$view == "static", ]))
      dy <- lf[lf$view == "dynamic", ]
      if (nrow(dy)) {
        doc <- officer::body_add_par(doc, "Change over time", style = "heading 3")
        doc <- .add_table(doc, .dynamic_table(dy))
      }
      ex <- ev$excluded$indicator[ev$excluded$leaf %in% ch$title]
      if (length(ex)) {
        doc <- officer::body_add_par(doc, paste0(
          "Not considered (no data for the base unit, or low variance): ",
          paste(ex, collapse = "; "), "."), style = "Normal")
      }
    }

    gaps <- ev$gaps$name[ev$gaps$cluster %in% cl]
    if (length(gaps)) {
      doc <- officer::body_add_par(doc, paste0(
        "Not yet measured (no indicators available): ",
        paste(gaps, collapse = "; "), "."), style = "Normal")
    }
  }

  # Appendix: the evidence register
  doc <- officer::body_add_par(doc, "Appendix: evidence register", style = "heading 1")
  .add_table(doc, data.frame(Evidence = f$id, Statement = f$text))
}
