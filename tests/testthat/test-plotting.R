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
