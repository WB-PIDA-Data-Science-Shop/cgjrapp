# End-to-end: the selection module's default output drives the dashboard.

test_that("run_cgjrapp() assembles a shiny app", {
  app <- run_cgjrapp()
  expect_s3_class(app, "shiny.appobj")
})

test_that("sidebar menu mirrors cliarappak: same shell, GOVCGJR brand, tabs in order", {
  # the bs4Dash ui is built inside run_cgjrapp() and only reachable via the
  # assembled shiny.appobj's internal http handler, so check the source.
  src <- paste(deparse(body(run_cgjrapp)), collapse = "
")
  expect_match(src, "bs4Dash::dashboardPage", fixed = TRUE)
  expect_match(src, 'sidebar_width = "350px"', fixed = TRUE)
  expect_match(src, '"GOVCGJR Benchmarking Dashboard"', fixed = TRUE)
  expect_no_match(src, "page_navbar", fixed = TRUE)

  in_order <- function(needles) {
    pos <- vapply(needles, function(n) regexpr(n, src, fixed = TRUE)[[1]], numeric(1))
    expect_true(all(pos > 0))
    expect_true(all(diff(pos) > 0))   # strictly increasing == that exact order
  }
  # sidebar items
  in_order(c('"Home"', '"Country Selection"', '"Benchmarking"', '"Data"',
             '"AI Report"', '"Methodology & User Guide"',
             '"Terms of use and Disclaimers"', '"FAQ"', '"Contact Us"',
             '"Source code"'))
  # tab bodies
  in_order(c('mod_welcome_ui("welcome")', 'mod_country_selection_ui("selection"',
             'mod_benchmark_dashboard_ui("bench"', 'mod_data_ui("data")',
             'mod_report_ui("report")'))
})

test_that("the app ships cliarappak's styles.css", {
  css <- readLines(system.file("www", "styles.css", package = "cgjrapp"))
  expect_true(any(grepl("#051f3f", css, fixed = TRUE)))
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
