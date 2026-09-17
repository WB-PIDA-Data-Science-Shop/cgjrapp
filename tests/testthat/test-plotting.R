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
