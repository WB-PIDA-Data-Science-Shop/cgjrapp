# ---
# Benchmarking plots - port of cliarappak's static_plot() / static_plot_dyn()
# (R/fct_plots.R) onto def_quantiles() / def_quantiles_dyn() output.
#
# Trimmed to what the CGJR leaf card needs: a single base unit, closeness-to-
# frontier on the x-axis (never the rank view), optional hollow comparison
# dots. The zone-band colours, the 0/cutoff1/cutoff2/1 segment construction,
# the status palette and the dashed "frontier" line at x = 1 are carried over
# unchanged so the figure reads identically to CLIAR's.
# ---

# Zone band segment fills (cliarappak fct_plots.R geom_segment colours)
.ZONE_FILLS <- c(weak = "#e47a81", emerging = "#ffd966", strong = "#8ec18e")

# Focal-unit point fill by status (cliarappak fct_plots.R `colors`)
.STATUS_FILLS <- c(weak = "#D2222D", emerging = "#FFBF00", strong = "#238823")

.wrap_label <- function(x, width = 40) {
  vapply(
    x,
    function(s) paste(strwrap(s, width = width), collapse = "\n"),
    character(1),
    USE.NAMES = FALSE
  )
}

.status_palette <- function(cutoff) {
  labs <- .status_labels(cutoff)
  stats::setNames(unname(.STATUS_FILLS[names(labs)]), unname(labs))
}

.empty_plot <- function(msg = "No data available for this indicator set.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg,
                      size = 4.5, colour = "#888888") +
    ggplot2::theme_void()
}

# Indicators the dynamic plot can draw: those where the focal unit has data in
# at least two years (a trend needs two points).
.trend_indicators <- function(base_df) {
  base_df |>
    dplyr::group_by(.data$indicator) |>
    dplyr::filter(dplyr::n_distinct(.data$year) > 1L) |>
    dplyr::pull(.data$indicator) |>
    unique()
}

# Dynamic facet titles - port of cliarappak::static_plot_dyn()'s `new_labels`:
# the indicator name, then the focal unit's change in CTF and in rank between
# its earliest and latest year. Unlike cliarappak, each "out of N" counts the
# units ranked in the SAME year as the rank it qualifies (cliarappak takes N
# from the indicator's earliest/latest year across all countries, which is a
# different year whenever the focal unit's own series starts later).
# .change_table() returns the numbers (one row per indicator: first/last year,
# delta, ranks and N); .change_labels() formats them as a named character
# vector, indicator -> label. The AI Report's evidence pack reuses
# .change_table(), so the report and the charts can't disagree.
.change_table <- function(dq, base_df) {
  n_ranked <- dplyr::count(dq, .data$indicator, .data$year, name = "n")
  base_df |>
    dplyr::group_by(.data$indicator) |>
    dplyr::filter(.data$year %in% range(.data$year)) |>
    dplyr::arrange(.data$year, .by_group = TRUE) |>
    dplyr::left_join(n_ranked, by = c("indicator", "year")) |>
    dplyr::summarise(
      year_first = dplyr::first(.data$year), year_last = dplyr::last(.data$year),
      delta      = round(dplyr::last(.data$dtf) - dplyr::first(.data$dtf), 3),
      rank_first = dplyr::first(.data$nrank), n_first = dplyr::first(.data$n),
      rank_last  = dplyr::last(.data$nrank),  n_last  = dplyr::last(.data$n),
      .groups = "drop"
    )
}

.change_labels <- function(dq, base_df) {
  ends <- .change_table(dq, base_df)
  rank_line <- ifelse(
    ends$rank_first != ends$rank_last,
    sprintf("(Change in Percentile Rank: from %d out of %d to %d out of %d)",
            ends$rank_first, ends$n_first, ends$rank_last, ends$n_last),
    "(No significant change in Percentile Rank)"
  )
  stats::setNames(
    paste0(.wrap_label(as.character(ends$indicator), 60),
           "\n\n(Change in CTF: ", ends$delta, ")\n", rank_line),
    ends$indicator
  )
}

# Pixel height of a benchmarking plot, grown with its indicator count so every
# indicator row (static) / facet row (dynamic) keeps the same size whatever the
# leaf - the card shows it in a fixed scroll window (.PLOT_WINDOW_PX), the way
# cliarappak's benchmarking page does. Never shorter than that window.
.PLOT_WINDOW_PX <- 600L
.plot_height <- function(n_indicators, ctf_type = c("static", "dynamic")) {
  ctf_type <- match.arg(ctf_type)
  h <- if (ctf_type == "static") {
    160L + 45L * n_indicators
  } else {
    160L + 330L * ceiling(n_indicators / 2)   # two facet columns
  }
  as.integer(max(.PLOT_WINDOW_PX, h))
}

#' Pixel height for a leaf's benchmarking plot
#'
#' Counts the indicators the plot will actually draw - every indicator with
#' focal-unit data (static), or those with at least two years of it
#' (dynamic) - and sizes the plot so each keeps a fixed amount of room.
#'
#' @param dq Output of [def_quantiles()] / [def_quantiles_dyn()].
#' @param base_unit Character vector of focal `unit_code`(s).
#' @param ctf_type `"static"` or `"dynamic"`.
#' @return Integer pixel height, at least 600.
#' @export
benchmark_plot_height <- function(dq, base_unit, ctf_type = c("static", "dynamic")) {
  ctf_type <- match.arg(ctf_type)
  if (is.null(dq) || nrow(dq) == 0L) return(.PLOT_WINDOW_PX)
  base_df <- dplyr::filter(dq, .data$unit_code %in% base_unit)
  n <- if (ctf_type == "static") {
    dplyr::n_distinct(base_df$indicator)
  } else {
    length(.trend_indicators(base_df))
  }
  .plot_height(n, ctf_type)
}

#' Static benchmarking dot plot for one leaf
#'
#' Port of `cliarappak::static_plot()` for the closeness-to-frontier view: one
#' row per indicator, coloured focal-unit point at its `dtf`, three shaded
#' zone bands (Weak / Emerging / Strong) whose breakpoints are the selection's
#' own percentiles, and a dashed line at the frontier (`x = 1`).
#'
#' @param dq Output of [def_quantiles()].
#' @param base_unit Character vector. The focal `unit_code`(s) - when more
#'   than one, each is drawn with its own point shape (cliarappak parity:
#'   `fill` stays reserved for Weak/Emerging/Strong `status`, `shape` carries
#'   the base unit identity), matching a single legend for each aesthetic.
#' @param tab_name Optional plot title (the leaf's display name).
#' @param threshold `"Default"` or `"Terciles"` - must match the value passed
#'   to [def_quantiles()].
#' @param dots Logical. Overlay hollow comparison-unit points.
#' @param note Optional caption string.
#' @param row_order Optional character vector of `indicator` values in the
#'   order to draw them, top to bottom (the Overall charts' fixed taxonomy
#'   order, as cliarappak's Overview uses its family order). `NULL` orders by
#'   the base unit's value.
#'
#' @return A `ggplot`. Wrap with [as_interactive_plot()] for Shiny.
#' @seealso [static_plot_dyn()], [def_quantiles()]
#' @export
static_plot <- function(dq, base_unit, tab_name = NULL,
                        threshold = c("Default", "Terciles"),
                        dots = FALSE, note = NULL, row_order = NULL) {
  cutoff <- .family_cutoffs(threshold)
  if (is.null(dq) || nrow(dq) == 0L) return(.empty_plot())

  pal <- .status_palette(cutoff)
  base_df <- dplyr::filter(dq, .data$unit_code %in% base_unit)
  if (nrow(base_df) == 0L) return(.empty_plot())

  # Ordered by mean dtf across all selected base units - reduces to the
  # original single-unit ordering when only one is selected.
  order_lvls <- base_df |>
    dplyr::group_by(.data$indicator) |>
    dplyr::summarise(.dtf_mean = mean(.data$dtf, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$.dtf_mean) |>
    dplyr::pull(.data$indicator)
  # a fixed order instead; ggplot draws the first factor level at the bottom
  if (!is.null(row_order)) order_lvls <- rev(intersect(row_order, order_lvls))
  dq <- dplyr::mutate(
    dq,
    indicator = factor(.data$indicator, levels = order_lvls),
    status    = factor(.data$status, levels = unname(.status_labels(cutoff)))
  )
  base_df <- dplyr::filter(dq, .data$unit_code %in% base_unit) |>
    dplyr::mutate(
      text = paste0(
        "Indicator: ", .data$indicator,
        "<br>Closeness to frontier: ", round(.data$dtf, 3),
        "<br>Rank: ", .data$nrank
      )
    )
  seg_df <- dplyr::distinct(
    dq, .data$indicator, .data$q_cutoff1, .data$q_cutoff2
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(y = .data$indicator, yend = .data$indicator,
                   x = 0, xend = .data$q_cutoff1),
      colour = .ZONE_FILLS[["weak"]], linewidth = 2, alpha = 0.3
    ) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(y = .data$indicator, yend = .data$indicator,
                   x = .data$q_cutoff1, xend = .data$q_cutoff2),
      colour = .ZONE_FILLS[["emerging"]], linewidth = 2, alpha = 0.3
    ) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(y = .data$indicator, yend = .data$indicator,
                   x = .data$q_cutoff2, xend = 1),
      colour = .ZONE_FILLS[["strong"]], linewidth = 2, alpha = 0.3
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "#888888", linewidth = 0.6)

  if (isTRUE(dots)) {
    comp_df <- dplyr::filter(dq, !.data$unit_code %in% base_unit)
    if (nrow(comp_df) > 0L) {
      p <- p + suppressWarnings(ggplot2::geom_point(
        data = comp_df,
        ggplot2::aes(y = .data$indicator, x = .data$dtf,
                     text = paste0(.data$unit_name, ": ", round(.data$dtf, 3))),
        shape = 21, size = 2, colour = "gray30", fill = "white", alpha = 0.5
      ))
    }
  }

  n_base <- dplyr::n_distinct(base_df$unit_code)
  if (n_base > 1L) {
    p <- p +
      suppressWarnings(ggplot2::geom_point(
        data = base_df,
        ggplot2::aes(y = .data$indicator, x = .data$dtf, shape = .data$unit_name,
                     fill = .data$status, text = .data$text),
        size = 3, colour = "gray0"
      )) +
      ggplot2::scale_shape_manual(values = 21:25, name = NULL) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21)))
  } else {
    p <- p +
      suppressWarnings(ggplot2::geom_point(
        data = base_df,
        ggplot2::aes(y = .data$indicator, x = .data$dtf,
                     fill = .data$status, text = .data$text),
        shape = 21, size = 3, colour = "gray0"
      ))
  }

  p +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE, name = NULL) +
    ggplot2::scale_y_discrete(labels = function(x) .wrap_label(x, 40)) +
    ggplot2::scale_x_continuous(limits = c(0, NA)) +
    ggplot2::labs(
      title = tab_name, x = "Closeness to frontier", y = NULL, caption = note
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Dynamic (year-by-year) benchmarking plot for one leaf
#'
#' Port of `cliarappak::static_plot_dyn()`: one facet per indicator, `year` on
#' the x-axis, the focal unit's `dtf` as a status-coloured line + points, with
#' the per-year zone bands drawn as vertical segments.
#'
#' @inheritParams static_plot
#' @param dq Output of [def_quantiles_dyn()].
#'
#' @return A `ggplot`.
#' @seealso [static_plot()], [def_quantiles_dyn()]
#' @export
static_plot_dyn <- function(dq, base_unit, tab_name = NULL,
                            threshold = c("Default", "Terciles"),
                            dots = FALSE, note = NULL, row_order = NULL) {
  cutoff <- .family_cutoffs(threshold)
  if (is.null(dq) || nrow(dq) == 0L) return(.empty_plot())
  # facets follow factor level order
  if (!is.null(row_order)) {
    dq$indicator <- factor(dq$indicator,
                           levels = intersect(row_order, unique(dq$indicator)))
  }

  pal <- .status_palette(cutoff)
  base_df <- dq |>
    dplyr::filter(.data$unit_code %in% base_unit) |>
    dplyr::mutate(
      status   = factor(.data$status, levels = unname(.status_labels(cutoff))),
      year_chr = as.character(.data$year),
      text = paste0(
        "Indicator: ", .data$indicator,
        "<br>Year: ", .data$year,
        "<br>Closeness to frontier: ", round(.data$dtf, 3),
        "<br>Rank: ", .data$nrank
      )
    )
  if (nrow(base_df) == 0L) return(.empty_plot())

  keep_ind <- .trend_indicators(base_df)
  base_df <- dplyr::filter(base_df, .data$indicator %in% keep_ind)
  if (nrow(base_df) == 0L) return(.empty_plot("Not enough years of data to show a trend."))

  seg_df <- dplyr::distinct(
    base_df, .data$indicator, .data$year_chr, .data$q_cutoff1, .data$q_cutoff2
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = .data$year_chr, xend = .data$year_chr,
                   y = 0, yend = .data$q_cutoff1),
      colour = .ZONE_FILLS[["weak"]], linewidth = 2, alpha = 0.3
    ) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = .data$year_chr, xend = .data$year_chr,
                   y = .data$q_cutoff1, yend = .data$q_cutoff2),
      colour = .ZONE_FILLS[["emerging"]], linewidth = 2, alpha = 0.3
    ) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = .data$year_chr, xend = .data$year_chr,
                   y = .data$q_cutoff2, yend = 1),
      colour = .ZONE_FILLS[["strong"]], linewidth = 2, alpha = 0.3
    )

  if (isTRUE(dots)) {
    comp_df <- dq |>
      dplyr::filter(!.data$unit_code %in% base_unit,
                    .data$indicator %in% keep_ind) |>
      dplyr::mutate(year_chr = as.character(.data$year))
    if (nrow(comp_df) > 0L) {
      p <- p + suppressWarnings(ggplot2::geom_point(
        data = comp_df,
        ggplot2::aes(x = .data$year_chr, y = .data$dtf,
                     text = paste0(.data$unit_name, " (", .data$year, "): ",
                                   round(.data$dtf, 3))),
        shape = 21, size = 1.6, colour = "gray30", fill = "white", alpha = 0.4
      ))
    }
  }

  p +
    ggplot2::geom_line(
      data = base_df,
      ggplot2::aes(x = .data$year_chr, y = .data$dtf, group = 1),
      colour = "gray30"
    ) +
    suppressWarnings(ggplot2::geom_point(
      data = base_df,
      ggplot2::aes(x = .data$year_chr, y = .data$dtf,
                   fill = .data$status, text = .data$text),
      shape = 21, size = 2, colour = "gray0"
    )) +
    # cliarappak parity: two columns, free scales, change-in-CTF/rank titles
    ggplot2::facet_wrap(ggplot2::vars(.data$indicator), ncol = 2, scales = "free",
                        labeller = ggplot2::labeller(
                          indicator = .change_labels(dq, base_df))) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE, name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = tab_name, x = NULL, y = "Closeness to frontier",
                  caption = note) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      panel.spacing.x = ggplot2::unit(1, "lines"),
      panel.spacing.y = ggplot2::unit(3, "lines")
    )
}

# cliarappak's plotly_remove_buttons: leaves Download-as-PNG, zoom and reset
.PLOTLY_REMOVE_BUTTONS <- c(
  "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "lasso2d", "select2d",
  "toggleSpikelines", "hoverClosest3d", "hoverClosestCartesian",
  "hoverCompareCartesian"
)

# The title row and, under it, the legend row are placed in pixels from the
# top of the plot (title at the very top, legend LEGEND_TOP_PX down), inside a
# fixed top margin - so neither can run into the other, nor into the dynamic
# plot's facet titles, whatever the plot's height or title length. Dynamic
# plots get extra margin for their tall (4-5 line) change-in-CTF/rank facet
# titles, which ggplotly draws above each panel.
.LEGEND_TOP_PX     <- 45L
.TOP_MARGIN_PX     <- 120L
.FACET_TITLE_PX    <- 75L

# ggplotly names a trace mapped to two aesthetics "(Ghana,Emerging ...)"; turn
# that into "Ghana, Emerging ..." (cliarappak's interactive_plot() does the
# same clean-up of ggplotly's legend entries).
.tidy_legend_names <- function(w) {
  w$x$data <- lapply(w$x$data, function(tr) {
    nm <- tr$name
    if (is.character(nm) && length(nm) == 1L && grepl("^\\(.*\\)$", nm)) {
      tr$name <- sub(",", ", ", substr(nm, 2L, nchar(nm) - 1L), fixed = TRUE)
    }
    tr
  })
  w
}

#' Convert a benchmarking ggplot to an interactive plotly
#'
#' Lays the title and the Weak / Emerging / Strong legend out on separate rows
#' in a fixed-height top margin (title on top, legend under it, left-aligned)
#' so a long title can't run into the legend, and keeps cliarappak's modebar:
#' Download-as-PNG (at the plot's full height, so a tall scrolling plot exports
#' whole), zoom and reset.
#'
#' @param p A `ggplot` from [static_plot()] / [static_plot_dyn()].
#' @param height Pixel height, or `NULL` to let plotly decide.
#' @param filename Base name (no extension) for the PNG download.
#' @return A `plotly` htmlwidget.
#' @export
as_interactive_plot <- function(p, height = NULL, filename = "cgjr_benchmark") {
  title <- p$labels$title
  faceted <- !inherits(p$facet, "FacetNull")
  # legend's top edge in container coordinates; without a known height, fall
  # back to just above the plotting area
  legend_y <- if (is.null(height)) {
    list(y = 1, yref = "paper", yanchor = "bottom")
  } else {
    list(y = 1 - .LEGEND_TOP_PX / height, yref = "container", yanchor = "top")
  }
  plotly::ggplotly(p, tooltip = "text", height = height) |>
    .tidy_legend_names() |>
    plotly::layout(
      title = list(
        text = if (is.null(title)) "" else paste0("<b>", title, "</b>"),
        x = 0, xref = "paper", xanchor = "left",
        y = 1, yref = "container", yanchor = "top", pad = list(t = 12)
      ),
      legend = c(list(orientation = "h", x = 0, xanchor = "left"), legend_y),
      margin = list(t = .TOP_MARGIN_PX + if (faceted) .FACET_TITLE_PX else 0L)
    ) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = .PLOTLY_REMOVE_BUTTONS,
      toImageButtonOptions = list(
        filename = filename, format = "png",
        width = 1100, height = height %||% 1000
      )
    )
}

#' Build the "Show notes" text for a static benchmarking chart
#'
#' Port of `cliarappak::plot_notes_function()` for the static view only -
#' cliarappak shows no notes for the dynamic view either, so there is no
#' `_dyn` counterpart.
#'
#' @param base_unit,comparison_units Character `unit_code`(s) - the same
#'   selection passed to [def_quantiles()].
#' @param ctf_long The same long CTF slice passed to [def_quantiles()]; used
#'   only to resolve `unit_code` to `unit_name` for display.
#' @param excluded Character vector of excluded indicator display names, from
#'   [excluded_indicators()].
#'
#' @return An [shiny::HTML()] string.
#' @seealso [excluded_indicators()], [static_plot()]
#' @export
plot_notes <- function(base_unit, comparison_units, ctf_long, excluded) {
  lookup <- dplyr::distinct(ctf_long, .data$unit_code, .data$unit_name)
  name_of <- function(codes) {
    nm <- lookup$unit_name[match(codes, lookup$unit_code)]
    dplyr::coalesce(nm, codes)   # fall back to the code if unresolved
  }

  txt <- sprintf(
    "Notes:<br><br>%s compared to %s.",
    paste(name_of(base_unit), collapse = ", "),
    paste(name_of(comparison_units), collapse = ", ")
  )
  if (length(excluded) > 0L) {
    txt <- paste0(
      txt,
      "<br><br>The following indicators are not considered because the base",
      " unit has no information for them, or because of low variance: ",
      paste(excluded, collapse = ", "), "."
    )
  }
  shiny::HTML(txt)
}
