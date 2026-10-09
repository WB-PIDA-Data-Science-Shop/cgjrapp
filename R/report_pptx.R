# ---
# PowerPoint benchmarking report built from an evidence pack.
#
# Port of cliarappak's "PPT report" (mod_reports.R): its World Bank template
# (inst/www/govcgjr_template.pptx, derived from CLIAR_template.pptx by
# dev/build_pptx_template.R), its slide geometry, and its charts inserted as
# editable vector graphics with rvg::dml(). The charts are the dashboard's own
# static_plot() / static_plot_dyn() figures from the evidence pack, the same
# ones the Word report draws.
# ---

.PPT_LAYOUT <- "Title and Content"
.PPT_MASTER <- "Custom Design"
# content area of the "Title and Content" layout (inches), above its footer bar
.PPT_BODY <- list(left = 0.92, top = 1.35, width = 11.5, height = 4.9)

.ppt_text <- function(text, size, color = "black", bold = FALSE) {
  officer::fpar(officer::ftext(text, officer::fp_text(font.size = size, color = color,
                                                       bold = bold)))
}

.ppt_slide <- function(ppt, title) {
  officer::add_slide(ppt, layout = .PPT_LAYOUT, master = .PPT_MASTER) |>
    officer::ph_with(title, location = officer::ph_location_type(type = "title"))
}

.ppt_chart_slide <- function(ppt, chart, meta, title) {
  p <- tryCatch(.chart_ggplot(chart, meta, title = FALSE), error = function(e) NULL)
  if (is.null(p)) return(ppt)
  .ppt_slide(ppt, title) |>
    officer::ph_with(rvg::dml(ggobj = p), location = do.call(officer::ph_location, .PPT_BODY))
}

.ppt_table_slide <- function(ppt, df, title) {
  if (nrow(df) == 0L) return(ppt)
  .ppt_slide(ppt, title) |>
    officer::ph_with(df, location = do.call(officer::ph_location, .PPT_BODY))
}

.ppt_bullets_slide <- function(ppt, title, items) {
  items <- items[nzchar(items)]
  if (length(items) == 0L) return(ppt)
  .ppt_slide(ppt, title) |>
    officer::ph_with(
      officer::unordered_list(
        str_list = items, level_list = rep(1L, length(items)),
        style = officer::fp_text(font.size = 12)
      ),
      location = do.call(officer::ph_location, .PPT_BODY)
    )
}

.find_chart <- function(ev, key, view) {
  ch <- Filter(function(c) c$key == key && c$view == view, ev$charts)
  if (length(ch)) ch[[1]] else NULL
}

#' Build the PowerPoint benchmarking report from an evidence pack
#'
#' Port of `cliarappak`'s PPT report onto the GOVCGJR template: a title slide,
#' the base-vs-comparison slide, the Overall Clusters charts (static and over
#' time), the priority areas, then for each selected cluster its subcluster
#' averages (and any branching subcluster's) and one slide per leaf, closing
#' with the excluded indicators and the areas not yet measured. Charts are
#' editable vector graphics ([rvg::dml()]), as in `cliarappak`.
#'
#' @param evidence Output of [build_evidence()].
#' @return An `officer` `rpptx`; write it with `print(ppt, target = path)`.
#' @seealso [build_evidence()], [evidence_report_docx()]
#' @export
evidence_report_pptx <- function(evidence) {
  ev <- evidence; m <- ev$meta
  ppt <- officer::read_pptx(system.file("www", "govcgjr_template.pptx",
                                        package = "cgjrapp"))

  # title slide: white on the template's navy band, under the banner
  ppt <- officer::on_slide(ppt, 1L) |>
    officer::ph_with(
      .ppt_text(paste0("Governance and Public Institutions: ", m$base_name),
                28, color = "white", bold = TRUE),
      location = officer::ph_location(left = 0.6, top = 3.75, width = 12, height = 0.6)) |>
    officer::ph_with(
      .ppt_text(paste0("Benchmarking report - ", format(m$generated, "%B %d, %Y")),
                18, color = "white"),
      location = officer::ph_location(left = 0.6, top = 4.3, width = 12, height = 0.45))

  # base vs comparison (cliarappak's slide 8)
  ppt <- .ppt_slide(ppt, "Base country vs comparison countries") |>
    officer::ph_with(
      officer::block_list(
        .ppt_text(paste0("Base country: ", m$base_name), 20),
        .ppt_text(paste0("Comparison countries: ",
                         paste(m$comparison_names, collapse = ", ")), 16),
        .ppt_text(sprintf("Thresholds: %s. Dynamic view: even years %d-%d.",
                          m$threshold, m$year_range[[1]], m$year_range[[2]]), 16)
      ),
      location = do.call(officer::ph_location, .PPT_BODY))

  # Overview
  ch <- .find_chart(ev, "overall__clusters", "static")
  if (!is.null(ch)) ppt <- .ppt_chart_slide(ppt, ch, m, "Clusters: overview")
  ch <- .find_chart(ev, "overall__clusters", "dynamic")
  if (!is.null(ch)) ppt <- .ppt_chart_slide(ppt, ch, m, "Clusters: change over time")
  pa <- priority_areas(ev)
  pa <- pa[pa$cluster %in% m$clusters, ]
  ppt <- .ppt_table_slide(ppt, .static_table(utils::head(pa, 10L)),
                          "Priority areas (lowest band, then furthest from the frontier)")

  # One section per selected cluster
  for (cl in m$clusters) {
    nm <- m$cluster_names[[cl]]
    key <- paste0(.OVERALL_PREFIX, cl)
    for (view in c("static", "dynamic")) {
      ch <- .find_chart(ev, key, view)
      if (!is.null(ch)) {
        ppt <- .ppt_chart_slide(ppt, ch, m, paste0(
          nm, if (view == "static") ": subclusters" else ": change over time"))
      }
    }
    # branching subclusters (e.g. Public Financial Management)
    for (ch in Filter(function(c) c$cluster %in% cl && is.na(c$leaf) &&
                        !c$key %in% paste0(.OVERALL_PREFIX, c("clusters", cl)),
                      ev$charts)) {
      ppt <- .ppt_chart_slide(ppt, ch, m, paste0(
        ch$title, if (ch$view == "static") "" else ": change over time"))
    }
    for (ch in Filter(function(c) c$cluster %in% cl && !is.na(c$leaf), ev$charts)) {
      ppt <- .ppt_chart_slide(ppt, ch, m, ch$title)
    }
  }

  # Closing: what the charts leave out
  ex <- ev$excluded[ev$excluded$cluster %in% m$clusters, ]
  ex_items <- vapply(split(ex$indicator, factor(ex$leaf, unique(ex$leaf))),
                     function(x) paste(x, collapse = "; "), character(1))
  gaps <- ev$gaps$name[ev$gaps$cluster %in% m$clusters]
  ppt <- .ppt_bullets_slide(ppt, "Notes", c(
    if (length(ex_items))
      paste0("Not considered (no data for the base country, or low variance) - ",
             names(ex_items), ": ", ex_items),
    if (length(gaps))
      paste0("Not yet measured (no indicators available): ",
             paste(gaps, collapse = "; "))
  ))
  ppt
}
