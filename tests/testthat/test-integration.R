# End-to-end: the selection module's default output drives the dashboard.

test_that("run_cgjrapp() assembles a shiny app", {
  app <- run_cgjrapp()
  expect_s3_class(app, "shiny.appobj")
})

test_that("nav order is Home, Country Selection, Benchmarking, Data, AI Report - no sidebar", {
  # page_navbar()'s ui is built inside run_cgjrapp() and only reachable via
  # the assembled shiny.appobj's internal http handler, so check the source
  # directly: no sidebar argument, and the nav modules appear in this order.
  src <- paste(deparse(body(run_cgjrapp)), collapse = "\n")
  expect_no_match(src, "sidebar", fixed = TRUE)

  pos <- vapply(
    c('mod_welcome_ui("welcome")', 'mod_country_selection_ui("selection"',
      'title = "Benchmarking"', 'mod_data_ui("data")', 'mod_report_ui("report")'),
    function(needle) regexpr(needle, src, fixed = TRUE)[[1]],
    numeric(1)
  )
  expect_true(all(pos > 0))
  expect_true(all(diff(pos) > 0))   # strictly increasing == that exact order
})

test_that("default selection (GHA vs region-mates) renders a real leaf card", {
  # what the selection module hands the dashboard on load
  base  <- "GHA"
  comps <- cgjrapp:::.default_comparators(base)
  expect_gt(length(comps), 9L)   # a real, >=10 comparator default

  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = cgjrdata::cgjr_taxonomy,
      base_unit        = shiny::reactive(base),
      comparison_units = shiny::reactive(comps),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      expect_match(output$`plot_justice_and_rule_of_law`, "plotly")
    }
  )
})
