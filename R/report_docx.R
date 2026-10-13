# ---
# AI Report, Phase 1: the Word report built from an evidence pack.
#
# Charts are the dashboard's own static_plot() / static_plot_dyn() figures
# (drawn from the evidence pack's stored def_quantiles() results, so they are
# the exact data behind every number in the tables), tables are the fact
# rows, and an appendix lists every fact with its id. With `narrative = NULL`
# this is the data-only report; the AI Report (report_ai.R) slots the LLM's
# text into the same layout, with its sources and verification appended.
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

# The ggplot for an evidence-pack chart - the dashboard's own plot functions,
# shared by the Word and PowerPoint reports. `title = FALSE` leaves the title
# to the slide (cliarappak's PPT report does the same).
.chart_ggplot <- function(chart, meta, title = TRUE) {
  fn <- if (chart$view == "static") static_plot else static_plot_dyn
  fn(chart$dq, meta$base_unit, tab_name = if (title) chart$title,
     threshold = meta$threshold, dots = TRUE, row_order = chart$row_order)
}

.chart_png <- function(chart, meta, dir) {
  p <- .chart_ggplot(chart, meta)
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

# Markdown narrative (AI Report) -> paragraphs: "##"/"###"/"####" headings,
# "- " bullets kept as bullets, emphasis markers dropped; NULL / "" adds nothing.
.add_narrative <- function(doc, text) {
  if (is.null(text) || !nzchar(text)) return(doc)
  for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    t <- trimws(gsub("\\*\\*|__", "", line))
    if (!nzchar(t)) next
    doc <- if (grepl("^#{3,4} ", t)) {
      officer::body_add_par(doc, sub("^#+ ", "", t), style = "heading 3")
    } else if (startsWith(t, "## ")) {
      officer::body_add_par(doc, sub("^## ", "", t), style = "heading 2")
    } else if (grepl("^[-*+] ", t)) {
      officer::body_add_par(doc, paste0("\u2022 ", sub("^[-*+] ", "", t)), style = "Normal")
    } else {
      officer::body_add_par(doc, t, style = "Normal")
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
#' With an AI narrative ([parse_narrative()]), each section's text goes under
#' its heading; `"prioritization"` follows the priority-areas table,
#' `"recommendations"` and a data-limitations note close the chapter, and the
#' appendices add the guidance passages cited and the automated verification.
#'
#' @param evidence Output of [build_evidence()].
#' @param narrative Optional named list of Markdown text, keyed `"overview"`,
#'   `"prioritization"`, a cluster key, a leaf key, or `"recommendations"`.
#'   `NULL` gives the data-only report.
#' @param leaves Optional leaf keys: report only these sections (and skip the
#'   Overview and cluster-average charts) - the AI Report's one-section scope.
#' @param ai Optional list for the AI-assisted version: `guidance` (output of
#'   [load_guidance()]), `cited` (data frame `id`, `section`, `title`, `page`)
#'   and `verification` (output of [verify_text()]).
#' @return An `officer` `rdocx`; write it with `print(doc, target = path)`.
#' @seealso [build_evidence()], [priority_areas()]
#' @export
evidence_report_docx <- function(evidence, narrative = NULL, leaves = NULL, ai = NULL) {
  ev <- evidence; m <- ev$meta; f <- ev$facts
  dir <- tempfile("cgjr_charts_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  nar <- function(key) if (is.null(narrative)) NULL else narrative[[key]]
  show_overview <- is.null(leaves) && (is.null(narrative) || !is.null(nar("overview")))
  in_scope <- function(leaf) is.null(leaves) || leaf %in% leaves

  doc <- officer::read_docx()
  if (identical(ai$guidance$source, "real")) {
    doc <- officer::body_add_par(doc, "OFFICIAL USE ONLY", style = "Normal")
  }
  doc <- doc |>
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
      if (is.null(narrative)) " This version contains no AI-generated text." else paste0(
        " Narrative text was drafted by a language model from these figures and from ",
        ai$guidance$label %||% "the CGJR guidance note",
        "; guidance passage ids (G1.2-3, ...) are listed in the appendix. It must be ",
        "reviewed before use.")
    ), style = "Normal")

  # Overview
  if (show_overview) {
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
    if (!is.null(nar("prioritization"))) {
      doc <- officer::body_add_par(doc, "Priorities for this chapter", style = "heading 2")
      doc <- .add_narrative(doc, nar("prioritization"))
    }
  }

  # One chapter per selected cluster
  for (cl in m$clusters) {
    cl_leaves <- unique(c(ev$gaps$leaf[ev$gaps$cluster %in% cl],
                          vapply(Filter(function(c) c$cluster %in% cl && !is.na(c$leaf), ev$charts),
                                 `[[`, character(1), "leaf")))
    if (!is.null(leaves) && !any(cl_leaves %in% leaves)) next
    doc <- officer::body_add_par(doc, m$cluster_names[[cl]], style = "heading 1")
    doc <- .add_narrative(doc, nar(cl))
    if (is.null(leaves)) {
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
    }

    for (ch in Filter(function(c) c$cluster %in% cl && !is.na(c$leaf) && in_scope(c$leaf),
                      ev$charts)) {
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

    # areas with no indicators: no chart, but the AI narrative still covers them
    g <- ev$gaps[ev$gaps$cluster %in% cl & vapply(ev$gaps$leaf, in_scope, logical(1)), ]
    for (j in seq_len(nrow(g))) {
      if (is.null(nar(g$leaf[[j]]))) next
      doc <- officer::body_add_par(doc, g$name[[j]], style = "heading 2")
      doc <- .add_narrative(doc, nar(g$leaf[[j]]))
    }
    if (nrow(g)) {
      doc <- officer::body_add_par(doc, paste0(
        "Not yet measured (no indicators available): ",
        paste(g$name, collapse = "; "), "."), style = "Normal")
    }
  }

  if (!is.null(narrative)) {
    if (!is.null(nar("recommendations"))) {
      doc <- officer::body_add_par(doc, "Possible recommendations", style = "heading 1")
      doc <- .add_narrative(doc, nar("recommendations"))
    }
    doc <- officer::body_add_par(doc, "Data limitations", style = "heading 2") |>
      officer::body_add_par(paste(
        "The figures in this chapter are the dashboard's closeness-to-frontier benchmarking",
        "of publicly available institutional indicators. They are a starting point: their",
        "quality depends on each source's coverage, definitions and measurement, and the",
        "analysis should be complemented with country-specific data and consultations with",
        "the country team and government counterparts."), style = "Normal")
  }

  # Appendices
  doc <- officer::body_add_par(doc, "Appendix: evidence register", style = "heading 1")
  doc <- .add_table(doc, data.frame(Evidence = f$id, Statement = f$text))
  if (!is.null(ai$cited) && nrow(ai$cited)) {
    doc <- officer::body_add_par(doc, "Appendix: guidance passages cited", style = "heading 1") |>
      officer::body_add_par(paste0("Source: ", ai$guidance$label, "."), style = "Normal")
    doc <- .add_table(doc, data.frame(Id = ai$cited$id, Section = ai$cited$section,
                                      Title = ai$cited$title, Page = ai$cited$page))
  }
  if (!is.null(ai$verification)) {
    v <- ai$verification
    doc <- officer::body_add_par(doc, "Appendix: automated verification", style = "heading 1") |>
      officer::body_add_par(paste(
        "Every citation and figure in the narrative was checked by the dashboard against the",
        "evidence register and the guidance passages.", verification_summary(v)), style = "Normal")
    if (nrow(v$issues)) {
      doc <- .add_table(doc, data.frame(Section = v$issues$section, Issue = v$issues$detail,
                                        Sentence = v$issues$sentence))
    }
  }
  doc
}
