# End-to-end: the selection module's default output drives the dashboard.

test_that("run_cgjrapp() assembles a shiny app", {
  app <- run_cgjrapp()
  expect_s3_class(app, "shiny.appobj")
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
      expect_match(output$score_justice_and_rule_of_law, "^0\\.[0-9]{3}$")
      expect_match(output$`plot_justice_and_rule_of_law`, "plotly")

      # the composite equals a direct call to the ported family average
      slice <- filter_unit_data("justice_and_rule_of_law", "static",
                                unique(c(base, comps)))
      fam <- compute_family_average_app(slice, base, comps, type = "static")
      expect_equal(
        as.numeric(output$score_justice_and_rule_of_law),
        round(fam$score[fam$unit_code == base], 3),
        tolerance = 1e-9
      )
    }
  )
})
