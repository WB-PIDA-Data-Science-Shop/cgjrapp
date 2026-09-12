# Slice 5 - AI Report tab module (no live LLM call).

COMPS <- c("NGA", "KEN", "SEN", "CIV", "BEN")

test_that("mod_report_ui renders the controls", {
  html <- as.character(mod_report_ui("report"))
  for (p in c("report-generate", "report-download_docx", "report-status_badge",
              "report-report_output")) {
    expect_match(html, p, fixed = TRUE)
  }
})

test_that("server: idle state, then a thin selection is rejected cleanly", {
  shiny::testServer(
    mod_report_server,
    args = list(
      base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive("NGA"),   # only 1
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      expect_true(grepl("Click 'Generate report'",
                        paste(as.character(output$report_output), collapse = " ")))
      session$setInputs(generate = 1)
      expect_match(r_error(), "at least 2 comparators")
      expect_false(r_gen())
    }
  )
})

test_that("server: unreachable LLM endpoint surfaces a clear error", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "http://127.0.0.1:59999/v1")
  shiny::testServer(
    mod_report_server,
    args = list(
      base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(generate = 1)
      expect_match(r_error(), "Cannot reach the LLM endpoint")
      expect_false(r_gen())
    }
  )
})

test_that("the report prompt is buildable from a real selection", {
  # what the server feeds the LLM
  scores <- export_live_scores("GHA", COMPS, "static")
  p <- build_cgjr_prompt(
    base_name = "Ghana", base_unit = "GHA",
    scores_tbl = scores, year_range = c(2013L, 2024L),
    ctf_type = "static", comparators = c("Nigeria", "Kenya")
  )
  expect_named(p, c("system", "user"))
  expect_match(p$user, "Justice and Rule of Law", fixed = TRUE)
  expect_match(p$user, "Ghana (GHA)", fixed = TRUE)
})
