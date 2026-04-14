# ── .fmt_score() ──────────────────────────────────────────────────────────────

test_that(".fmt_score() formats numeric to percentage", {
  expect_equal(.fmt_score(0.5),   "50%")
  expect_equal(.fmt_score(0),     "0%")
  expect_equal(.fmt_score(1),     "100%")
  expect_equal(.fmt_score(0.756), "75.6%")
})

test_that(".fmt_score() returns 'N/A' for NA", {
  expect_equal(.fmt_score(NA_real_), "N/A")
})

# ── .fmt_score_row() ──────────────────────────────────────────────────────────

test_that(".fmt_score_row() formats a named vector as a slash-separated string", {
  scores <- c("Overall Score" = 0.6, "Integrity" = 0.4)
  result <- .fmt_score_row(scores)
  expect_true(grepl("Overall Score: 60%",  result))
  expect_true(grepl("Integrity: 40%",      result))
  expect_true(grepl("/",                   result))
})

test_that(".fmt_score_row() handles NA values gracefully", {
  scores <- c("A" = NA_real_, "B" = 0.3)
  result <- .fmt_score_row(scores)
  expect_true(grepl("N/A", result))
  expect_true(grepl("30%", result))
})

# ── .summarise_scores() ───────────────────────────────────────────────────────

.make_scores_tbl <- function() {
  tibble::tibble(
    Entity                           = c("Ghana", "Nigeria", "Ghana"),
    Year                             = c(2023L, 2023L, 2022L),
    overall_score                    = c(0.5, 0.4, 0.45),
    institutional_environment_score  = c(0.6, 0.5, 0.55),
    political_institutions_score     = c(0.4, 0.35, 0.38),
    center_of_government_score       = c(0.5, 0.45, 0.48),
    sectors_service_delivery_score   = c(0.55, 0.42, 0.50)
  )
}

test_that(".summarise_scores() uses the most recent year per entity", {
  tbl    <- .make_scores_tbl()
  result <- .summarise_scores(tbl, "Ghana")
  # Ghana's latest year is 2023; should appear once per entity line
  lines <- strsplit(result, "\n", fixed = TRUE)[[1]]
  ghana_line <- grep("Ghana", lines, value = TRUE)
  expect_length(ghana_line, 1L)
  expect_true(grepl("2023", ghana_line))
})

test_that(".summarise_scores() returns fallback string when no score cols present", {
  tbl <- tibble::tibble(Entity = "X", Year = 2023L, other_col = 1)
  result <- .summarise_scores(tbl, "X")
  expect_equal(result, "(No scores data available.)")
})

test_that(".summarise_scores() includes all entities", {
  tbl    <- .make_scores_tbl()
  result <- .summarise_scores(tbl, "Ghana")
  expect_true(grepl("Ghana",   result))
  expect_true(grepl("Nigeria", result))
})

# ── .cgjr_system_prompt() ────────────────────────────────────────────────────

test_that(".cgjr_system_prompt() returns a non-empty string", {
  sp <- .cgjr_system_prompt()
  expect_type(sp, "character")
  expect_true(nchar(sp) > 100L)
})

test_that(".cgjr_system_prompt() references all four CGJR thematic areas", {
  sp <- .cgjr_system_prompt()
  expect_true(grepl("INSTITUTIONAL ENVIRONMENT", sp, fixed = TRUE))
  expect_true(grepl("POLITICAL INSTITUTIONS",    sp, fixed = TRUE))
  expect_true(grepl("CENTER OF GOVERNMENT",      sp, fixed = TRUE))
  expect_true(grepl("SECTORS",                   sp, fixed = TRUE))
})

# ── build_cgjr_prompt() ───────────────────────────────────────────────────────

test_that("build_cgjr_prompt() returns a character scalar", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt(
    primary_iso  = "GHA",
    primary_name = "Ghana",
    scores_tbl   = tbl,
    year_range   = c(2013L, 2023L)
  )
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("build_cgjr_prompt() includes country name and ISO", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt("GHA", "Ghana", tbl, c(2013L, 2023L))
  expect_true(grepl("Ghana", result, fixed = TRUE))
  expect_true(grepl("GHA",   result, fixed = TRUE))
})

test_that("build_cgjr_prompt() includes year range", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt("GHA", "Ghana", tbl, c(2015L, 2022L))
  expect_true(grepl("2015", result, fixed = TRUE))
  expect_true(grepl("2022", result, fixed = TRUE))
})

test_that("build_cgjr_prompt() lists peer countries when provided", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt(
    "GHA", "Ghana", tbl, c(2013L, 2023L),
    peer_isos = c("NGA", "KEN")
  )
  expect_true(grepl("NGA", result, fixed = TRUE))
  expect_true(grepl("KEN", result, fixed = TRUE))
})

test_that("build_cgjr_prompt() says no comparators when none provided", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt("GHA", "Ghana", tbl, c(2013L, 2023L))
  expect_true(grepl("No comparators selected", result, fixed = TRUE))
})

test_that("build_cgjr_prompt() lists regions and income groups when provided", {
  tbl    <- .make_scores_tbl()
  result <- build_cgjr_prompt(
    "GHA", "Ghana", tbl, c(2013L, 2023L),
    region_codes  = "SSA",
    income_groups = "Lower middle income"
  )
  expect_true(grepl("SSA",                  result, fixed = TRUE))
  expect_true(grepl("Lower middle income",  result, fixed = TRUE))
})

# ── format_report_docx() ─────────────────────────────────────────────────────

test_that("format_report_docx() returns an rdocx object", {
  doc <- format_report_docx("## Intro\n\nSome text.", "Ghana")
  expect_s3_class(doc, "rdocx")
})

test_that("format_report_docx() writes to a file without error", {
  tmp  <- withr::local_tempfile(fileext = ".docx")
  doc  <- format_report_docx("## Section\n\nParagraph.", "Ghana")
  expect_no_error(print(doc, target = tmp))
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that("format_report_docx() handles empty report text", {
  doc <- format_report_docx("", "Ghana")
  expect_s3_class(doc, "rdocx")
})

test_that("format_report_docx() handles all heading levels", {
  md  <- "# H1\n## H2\n### H3\nNormal paragraph.\n- List item"
  doc <- format_report_docx(md, "Ghana")
  expect_s3_class(doc, "rdocx")
})

test_that("format_report_docx() includes country name in document content", {
  doc     <- format_report_docx("Some text.", "Testland")
  summary <- officer::docx_summary(doc)
  combined <- paste(summary$text, collapse = " ")
  expect_true(grepl("Testland", combined, fixed = TRUE))
})

test_that("format_report_docx() uses provided date", {
  doc     <- format_report_docx("text", "Ghana", report_date = "2025-01-15")
  summary <- officer::docx_summary(doc)
  combined <- paste(summary$text, collapse = " ")
  expect_true(grepl("January 15, 2025", combined, fixed = TRUE))
})
