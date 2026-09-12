# -- .fmt_score() / .fmt_score_row() ---

test_that(".fmt_score formats to percent, N/A for NA", {
  expect_equal(.fmt_score(0.5),    "50%")
  expect_equal(.fmt_score(0.756),  "75.6%")
  expect_equal(.fmt_score(NA_real_), "N/A")
})

test_that(".fmt_score_row joins a named vector", {
  r <- .fmt_score_row(c("Overall" = 0.6, "Integrity" = NA_real_))
  expect_match(r, "Overall: 60%")
  expect_match(r, "Integrity: N/A")
  expect_match(r, "/", fixed = TRUE)
})

# -- .taxonomy_outline() ---

test_that(".taxonomy_outline is generated from cgjr_taxonomy, not hardcoded", {
  o <- cgjrapp:::.taxonomy_outline()
  tx <- cgjrdata::cgjr_taxonomy
  # cluster names appear as upper-case headers; the rest verbatim
  for (nm in unique(tx$cluster_name)) expect_match(o, toupper(nm), fixed = TRUE)
  for (nm in unique(c(tx$subcluster_name,
                      stats::na.omit(tx$sub_subcluster_name)))) {
    expect_match(o, nm, fixed = TRUE)
  }
  first <- toupper(tx$cluster_name[tx$cluster_num == min(tx$cluster_num)][[1]])
  last  <- toupper(tx$cluster_name[tx$cluster_num == max(tx$cluster_num)][[1]])
  expect_lt(regexpr(first, o, fixed = TRUE), regexpr(last, o, fixed = TRUE))
})

# -- .summarise_scores() ---

.mk_live <- function() {
  tibble::tibble(
    Cluster = c("A", "A", "B"),
    Leaf    = c("Integrity", "Justice", "PFM"),
    Composite = c(0.4, 0.62, NA_real_),
    `Indicators used`     = c(5L, 16L, 0L),
    `Indicators observed` = c(5L, 16L, 0L)
  )
}

test_that(".summarise_scores groups by cluster and marks empty leaves", {
  s <- cgjrapp:::.summarise_scores(.mk_live())
  expect_match(s, "**A**", fixed = TRUE)
  expect_match(s, "Integrity: 40%", fixed = TRUE)
  expect_match(s, "Justice: 62%", fixed = TRUE)
  expect_match(s, "PFM: N/A (no eligible indicators)", fixed = TRUE)
})

test_that(".summarise_scores handles an empty table", {
  expect_match(cgjrapp:::.summarise_scores(.mk_live()[0, ]), "No scores available")
})

# -- .cgjr_system_prompt() ---

test_that(".cgjr_system_prompt names the four current clusters, not CLIAR's", {
  sp <- cgjrapp:::.cgjr_system_prompt()
  expect_gt(nchar(sp), 500L)
  for (cn in unique(cgjrdata::cgjr_taxonomy$cluster_name)) {
    expect_match(sp, toupper(cn), fixed = TRUE)
  }
})

test_that(".cgjr_system_prompt states the CLIAR departures and the empty leaves", {
  sp <- cgjrapp:::.cgjr_system_prompt()
  expect_match(sp, "departures from the CLIAR", fixed = TRUE)
  expect_match(sp, "Justice and Rule of Law", fixed = TRUE)
  expect_match(sp, "merges", fixed = TRUE)
  expect_match(sp, "not yet measured", fixed = TRUE)
  # and that the composite is selection-dependent, not precomputed
  expect_match(sp, "depends on the comparison group", fixed = TRUE)
})

# -- build_cgjr_prompt() ---

test_that("build_cgjr_prompt returns list(system, user) with the key facts", {
  p <- build_cgjr_prompt(
    base_name = "Ghana", base_unit = "GHA",
    scores_tbl = .mk_live(), year_range = c(2014L, 2022L),
    ctf_type = "dynamic", comparators = c("Nigeria", "Kenya")
  )
  expect_named(p, c("system", "user"))
  expect_match(p$user, "Ghana (GHA)", fixed = TRUE)
  expect_match(p$user, "even years 2014-2022", fixed = TRUE)
  expect_match(p$user, "Nigeria, Kenya", fixed = TRUE)
  expect_match(p$user, "Justice: 62%", fixed = TRUE)
})

test_that("build_cgjr_prompt says so when there are no comparators", {
  p <- build_cgjr_prompt("Ghana", "GHA", .mk_live(), c(2013L, 2024L))
  expect_match(p$user, "No comparators selected", fixed = TRUE)
})

# -- format_report_docx() ---

test_that("format_report_docx builds an rdocx and writes a file", {
  tmp <- withr::local_tempfile(fileext = ".docx")
  doc <- format_report_docx("## Section\n\nParagraph.\n- item", "Testland")
  expect_s3_class(doc, "rdocx")
  expect_no_error(print(doc, target = tmp))
  expect_gt(file.size(tmp), 0L)
  combined <- paste(officer::docx_summary(doc)$text, collapse = " ")
  expect_match(combined, "Testland", fixed = TRUE)
})

test_that("format_report_docx uses the provided date and handles empty text", {
  doc <- format_report_docx("text", "Ghana", report_date = "2025-01-15")
  expect_match(paste(officer::docx_summary(doc)$text, collapse = " "),
               "January 15, 2025", fixed = TRUE)
  expect_s3_class(format_report_docx("", "Ghana"), "rdocx")
})
