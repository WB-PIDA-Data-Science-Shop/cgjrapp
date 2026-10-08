# plot_notes() -- the "Show notes" text (port of cliarappak::plot_notes_function()).

.notes_ctf <- tibble::tribble(
  ~unit_code, ~unit_name,
  "GHA",      "Ghana",
  "NGA",      "Nigeria",
  "KEN",      "Kenya"
)

test_that("plot_notes states who is compared to whom, no exclusion sentence when nothing dropped", {
  html <- as.character(plot_notes("GHA", c("NGA", "KEN"), .notes_ctf, character(0)))
  expect_match(html, "Ghana compared to Nigeria, Kenya", fixed = TRUE)
  expect_no_match(html, "not considered")
})

test_that("plot_notes lists excluded indicators when there are any", {
  html <- as.character(plot_notes("GHA", c("NGA", "KEN"), .notes_ctf,
                                  c("Indicator A", "Indicator B")))
  expect_match(html, "not considered", fixed = TRUE)
  expect_match(html, "Indicator A, Indicator B", fixed = TRUE)
})

test_that("plot_notes falls back to the raw code for an unresolved unit_code", {
  html <- as.character(plot_notes("ZZZ", "NGA", .notes_ctf, character(0)))
  expect_match(html, "ZZZ compared to Nigeria", fixed = TRUE)
})

test_that("plot_notes handles multiple base units", {
  html <- as.character(plot_notes(c("GHA", "KEN"), "NGA", .notes_ctf, character(0)))
  expect_match(html, "Ghana, Kenya compared to Nigeria", fixed = TRUE)
})

# static_plot() -- multi-base-unit shape aesthetic (cliarappak parity).
# GHA/KEN/NGA/SEN/CIV are the well-populated countries already relied on
# elsewhere in this suite (test-mod_benchmark_dashboard.R, test-utils_family.R)
# for justice_and_rule_of_law's static data - unlike an alphabetical slice of
# cgjr_ctf, which turns up small territories with too little coverage to
# survive def_quantiles()'s missing-data screen.

test_that("static_plot draws one shape per base unit when multiple are selected", {
  base  <- c("GHA", "KEN")
  comps <- c("NGA", "SEN", "CIV")

  slice <- filter_unit_data("justice_and_rule_of_law", "static", c(base, comps))
  dq <- def_quantiles(slice, base, comps, "Default")
  expect_gt(nrow(dq), 0L)
  p  <- static_plot(dq, base, tab_name = "Justice and Rule of Law")

  base_layer <- p$layers[[length(p$layers)]]
  expect_true("shape" %in% names(base_layer$mapping))
  expect_true(any(vapply(p$scales$scales,
                        function(s) "shape" %in% s$aesthetics, logical(1))))
})

test_that("static_plot keeps a fixed point shape for a single base unit (no shape legend)", {
  base  <- "GHA"
  comps <- c("NGA", "SEN", "CIV")

  slice <- filter_unit_data("justice_and_rule_of_law", "static", c(base, comps))
  dq <- def_quantiles(slice, base, comps, "Default")
  expect_gt(nrow(dq), 0L)
  p  <- static_plot(dq, base, tab_name = "Justice and Rule of Law")

  base_layer <- p$layers[[length(p$layers)]]
  expect_false("shape" %in% names(base_layer$mapping))
  expect_identical(base_layer$aes_params$shape, 21)
})

# --- dynamic change labels, plot sizing, plotly layout ------------------------

# GHA has indicator A in 2014 (rank 3 of 3), 2016 and 2018 (rank 1 of 4);
# indicator B holds rank 2 throughout.
.dyn_dq <- tibble::tribble(
  ~unit_code, ~indicator, ~year, ~dtf, ~nrank,
  "GHA", "A", 2014, 0.40, 3L,
  "NGA", "A", 2014, 0.60, 1L,
  "KEN", "A", 2014, 0.50, 2L,
  "GHA", "A", 2016, 0.45, 2L,
  "NGA", "A", 2016, 0.30, 3L,
  "GHA", "A", 2018, 0.52, 1L,
  "NGA", "A", 2018, 0.30, 2L,
  "KEN", "A", 2018, 0.20, 3L,
  "SEN", "A", 2018, 0.10, 4L,
  "GHA", "B", 2014, 0.70, 2L,
  "NGA", "B", 2014, 0.90, 1L,
  "GHA", "B", 2018, 0.65, 2L,
  "NGA", "B", 2018, 0.95, 1L
)

test_that(".change_labels states CTF change and rank change, N counted per year", {
  base_df <- dplyr::filter(.dyn_dq, unit_code == "GHA")
  lab <- cgjrapp:::.change_labels(.dyn_dq, base_df)
  expect_named(lab, c("A", "B"))
  expect_identical(
    lab[["A"]],
    "A\n\n(Change in CTF: 0.12)\n(Change in Percentile Rank: from 3 out of 3 to 1 out of 4)"
  )
  expect_identical(
    lab[["B"]],
    "B\n\n(Change in CTF: -0.05)\n(No significant change in Percentile Rank)"
  )
})

test_that("plot height grows with indicator count, never below the 600px window", {
  expect_identical(cgjrapp:::.plot_height(3, "static"), 600L)
  expect_identical(cgjrapp:::.plot_height(24, "static"), 160L + 45L * 24L)
  expect_identical(cgjrapp:::.plot_height(1, "dynamic"), 600L)
  expect_identical(cgjrapp:::.plot_height(21, "dynamic"), 160L + 330L * 11L)  # 2 columns
  expect_identical(benchmark_plot_height(NULL, "GHA", "static"), 600L)
  # dynamic counts only indicators with >= 2 years for the focal unit
  one_year <- dplyr::filter(.dyn_dq, !(indicator == "B" & year == 2018))
  expect_identical(benchmark_plot_height(.dyn_dq, "GHA", "dynamic"),
                   cgjrapp:::.plot_height(2, "dynamic"))
  expect_identical(benchmark_plot_height(one_year, "GHA", "dynamic"),
                   cgjrapp:::.plot_height(1, "dynamic"))
})

test_that("as_interactive_plot keeps title and legend on separate rows, downloadable", {
  base  <- "GHA"
  comps <- c("NGA", "SEN", "CIV")
  slice <- filter_unit_data("justice_and_rule_of_law", "static", c(base, comps))
  dq <- def_quantiles(slice, base, comps, "Default")
  w <- as_interactive_plot(static_plot(dq, base, tab_name = "Justice and Rule of Law"),
                           height = 900, filename = "cgjr_justice_static")
  lay <- plotly::plotly_build(w)$x$layout
  cfg <- w$x$config

  # title pinned to the top, legend pinned below it - both in pixels
  expect_identical(lay$title$text, "<b>Justice and Rule of Law</b>")
  expect_identical(lay$title$yref, "container")
  expect_identical(lay$title$yanchor, "top")
  expect_identical(lay$legend$yref, "container")
  expect_identical(lay$legend$yanchor, "top")
  expect_equal(lay$legend$y, 1 - cgjrapp:::.LEGEND_TOP_PX / 900)
  # the plotting area starts below the legend row
  expect_gt(lay$margin$t, cgjrapp:::.LEGEND_TOP_PX)

  # cliarappak's modebar: PNG download at the plot's full height
  expect_false(isFALSE(cfg$displayModeBar))
  expect_identical(cfg$toImageButtonOptions$filename, "cgjr_justice_static")
  expect_equal(cfg$toImageButtonOptions$height, 900)
  expect_true(all(c("pan2d", "lasso2d", "select2d") %in% cfg$modeBarButtonsToRemove))
  expect_false("toImage" %in% cfg$modeBarButtonsToRemove)
})

test_that("dynamic plot: two facet columns, extra top margin for facet titles", {
  p <- static_plot_dyn(.dyn_dq |> dplyr::mutate(
         unit_name = unit_code, q_cutoff1 = 0.25, q_cutoff2 = 0.5,
         status = "Strong\n(top 50%)"),
       "GHA", tab_name = "Leaf")
  expect_identical(p$facet$params$ncol, 2)
  w <- as_interactive_plot(p, height = 1000)
  expect_equal(plotly::plotly_build(w)$x$layout$margin$t,
               cgjrapp:::.TOP_MARGIN_PX + cgjrapp:::.FACET_TITLE_PX)
})

test_that("multi-base legend entries are tidied like cliarappak's", {
  base  <- c("GHA", "KEN")
  comps <- c("NGA", "SEN", "CIV")
  slice <- filter_unit_data("justice_and_rule_of_law", "static", c(base, comps))
  dq <- def_quantiles(slice, base, comps, "Default")
  w <- as_interactive_plot(static_plot(dq, base), height = 600)
  nms <- unlist(lapply(w$x$data, `[[`, "name"))
  nms <- nms[nzchar(nms)]
  expect_true(length(nms) > 0L)
  expect_false(any(startsWith(nms, "(")))
  expect_true(any(startsWith(nms, "Ghana, ")))
})
