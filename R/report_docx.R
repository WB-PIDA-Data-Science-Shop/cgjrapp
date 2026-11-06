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
# "- " bullets kept as bullets, **bold** lead sentences kept bold (other
# emphasis in headings dropped); NULL / "" adds nothing.
.add_narrative <- function(doc, text) {
  if (is.null(text) || !nzchar(text)) return(doc)
  for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    t <- trimws(gsub("__", "", line))
    if (!nzchar(t)) next
    doc <- if (grepl("^#{3,4} ", t)) {
      officer::body_add_par(doc, gsub("\\*\\*", "", sub("^#+ ", "", t)), style = "heading 3")
    } else if (startsWith(t, "## ")) {
      officer::body_add_par(doc, gsub("\\*\\*", "", sub("^## ", "", t)), style = "heading 2")
    } else if (grepl("^[-*+] ", t)) {
      .add_md_par(doc, paste0("\u2022 ", sub("^[-*+] ", "", t)))
    } else {
      .add_md_par(doc, t)
    }
  }
  doc
}

# One Markdown paragraph -> a Word paragraph: **text** in bold (a CGJR
# paragraph's lead sentence), *text* in italics (titles in a reference
# entry); an escaped \* or \_ is the character itself.
.add_md_par <- function(doc, md) {
  runs <- list()
  bold <- strsplit(md, "**", fixed = TRUE)[[1]]  # every second piece is bold
  for (i in seq_along(bold)) {
    italic <- strsplit(bold[[i]], "(?<!\\\\)\\*", perl = TRUE)[[1]]  # split at unescaped *
    for (j in seq_along(italic)) {
      if (!nzchar(italic[[j]])) next
      runs[[length(runs) + 1L]] <- officer::ftext(gsub("\\\\([\\\\*_])", "\\1", italic[[j]]),
        officer::fp_text_lite(bold = i %% 2L == 0L, italic = j %% 2L == 0L))
    }
  }
  officer::body_add_fpar(doc, do.call(officer::fpar, runs), style = "Normal")
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

#' Build the data report Word document from an evidence pack
#'
#' Overview (the Overall Clusters chart and table, the dynamic change, and the
#' data-driven priority areas), then one chapter per selected cluster: its
#' subcluster-average chart and table, then each leaf's indicator chart,
#' indicator table, dynamic changes and excluded indicators, and the areas not
#' yet measured. An appendix lists every fact with its evidence id. No AI
#' text: the AI Report's own document is [ai_report_docx()].
#'
#' @param evidence Output of [build_evidence()].
#' @return An `officer` `rdocx`; write it with `print(doc, target = path)`.
#' @seealso [build_evidence()], [priority_areas()], [ai_report_docx()]
#' @export
evidence_report_docx <- function(evidence) {
  ev <- evidence; m <- ev$meta; f <- ev$facts
  dir <- tempfile("cgjr_charts_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  doc <- officer::read_docx() |>
    officer::body_add_par(paste0("Governance and Public Institutions: ", m$base_name),
                          style = "heading 1") |>
    officer::body_add_par(paste0("Data report - ", format(m$generated, "%B %d, %Y")),
                          style = "heading 2") |>
    officer::body_add_par(sprintf(
      "Selection: %s compared to %d units (%s). Thresholds: %s. Dynamic view: even years %d-%d.",
      m$base_name, length(m$comparison_units),
      paste(m$comparison_names, collapse = ", "), m$threshold,
      m$year_range[[1]], m$year_range[[2]]), style = "Normal") |>
    officer::body_add_par(paste0(
      "Every figure in this document was computed by the GOVCGJR Benchmarking ",
      "Dashboard for the selection above; evidence ids (E1, E2, ...) refer to ",
      "the register in the appendix. This version contains no AI-generated text."),
      style = "Normal")

  # Overview
  doc <- officer::body_add_par(doc, "Overview", style = "heading 1")
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

    g <- ev$gaps[ev$gaps$cluster %in% cl, ]
    if (nrow(g)) {
      doc <- officer::body_add_par(doc, paste0(
        "Not yet measured (no indicators available): ",
        paste(g$name, collapse = "; "), "."), style = "Normal")
    }
  }

  # Appendix
  doc <- officer::body_add_par(doc, "Appendix: evidence register", style = "heading 1")
  .add_table(doc, data.frame(Evidence = f$id, Statement = f$text))
}

#' Build the AI Report's Word document
#'
#' The report as the AI Report tab previews it: its sections in order, each
#' under its heading and followed by the dashboard's chart(s) for it; research
#' cited author-date, "(World Bank 2020, 10)", with a References list; the
#' narrative format's questions under "Questions for further analysis". It is
#' written for readers: the dashboard and guidance ids (`[E3]`, `[G1.2-1]`) are
#' taken out, and there are no data tables or appendices (the data report,
#' [evidence_report_docx()], has those). The checks behind every citation are
#' in the app.
#'
#' @param evidence Output of [build_evidence()] the report was written from.
#' @param sections The plan's sections, in display order: a data frame `key`,
#'   `title` (`plan_ai_report()$sections`).
#' @param narrative Output of [parse_narrative()]: Markdown by section key. A
#'   section with no text (a run that stopped part-way) is left out.
#' @param research The research pack the report cites, or `NULL`.
#' @param guidance Output of [load_guidance()]; with the real guidance note the
#'   document is marked Official Use Only.
#' @return An `officer` `rdocx`; write it with `print(doc, target = path)`.
#' @seealso [plan_ai_report()], [parse_narrative()]
#' @export
ai_report_docx <- function(evidence, sections, narrative, research = NULL, guidance = NULL) {
  ev <- evidence; m <- ev$meta
  dir <- tempfile("cgjr_charts_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  report <- paste(unlist(narrative), collapse = "\n")
  # the reader's text: research author-date (lettered across the whole
  # report), and no dashboard or guidance ids
  text_of <- function(key) {
    text <- narrative[[key]]
    if (!is.null(research)) text <- .render_citations(text, research, report)
    .strip_ids(text)
  }

  doc <- officer::read_docx()
  if (identical(guidance$source, "real")) {
    doc <- officer::body_add_par(doc, "OFFICIAL USE ONLY", style = "Normal")
  }
  doc <- doc |>
    officer::body_add_par(paste0("Governance and Public Institutions: ", m$base_name),
                          style = "heading 1") |>
    officer::body_add_par(sprintf(
      "AI-assisted draft, %s: %s compared with %d countries. To be reviewed before use.",
      format(m$generated, "%B %d, %Y"), m$base_name, length(m$comparison_units)), style = "Normal")

  annexed <- FALSE
  for (i in seq_len(nrow(sections))) {
    key <- sections$key[[i]]
    if (is.null(narrative[[key]])) next
    question <- startsWith(key, "questions_")
    if (question && !annexed) {
      doc <- officer::body_add_par(doc, "Questions for further analysis", style = "heading 1")
      annexed <- TRUE
    }
    level <- if (!question && key %in% c("overview", "recommendations", m$clusters)) "heading 1"
             else "heading 2"
    doc <- officer::body_add_par(doc, sections$title[[i]], style = level)
    doc <- .add_narrative(doc, text_of(key))
    for (ch in .section_charts(ev, key)) doc <- .add_chart(doc, ch, m, dir)
  }

  refs <- if (!is.null(research)) .reference_list(report, research)
  if (length(refs)) {
    doc <- officer::body_add_par(doc, "References", style = "heading 1")
    for (entry in refs) doc <- .add_md_par(doc, entry)
  }
  doc
}

# Charts shown under a report section (the dashboard's own), in the preview
# and in the Word document: the overview and each cluster have a static and an
# over-time chart; a section has its static chart only (the dashboard draws no
# over-time chart for it).
.section_charts <- function(ev, key) {
  want <- if (key == "overview") "overall__clusters" else
    if (key %in% ev$meta$clusters) paste0(.OVERALL_PREFIX, key) else key
  Filter(function(c) c$key %in% want, ev$charts)
}
