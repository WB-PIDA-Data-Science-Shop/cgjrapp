# Slice 5 - Data Download tab module.

COMPS <- c("NGA", "KEN", "SEN", "CIV", "BEN")

test_that("mod_data_ui has the four labelled sub-tabs", {
  html <- as.character(mod_data_ui("data"))
  for (piece in c("data-tbl_live", "data-tbl_reference", "data-tbl_ctf",
                  "data-tbl_raw", "data-tbl_catalogue", "data-ctf_type")) {
    expect_match(html, piece, fixed = TRUE)
  }
  # the two Scores tables are explicitly distinguished
  expect_match(html, "Live composite scores", fixed = TRUE)
  expect_match(html, "Not the interactive numbers", fixed = TRUE)
})

test_that("server renders every table for a real selection", {
  shiny::testServer(
    mod_data_server,
    args = list(
      base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type = "static")
      for (out in c("tbl_live", "tbl_reference", "tbl_ctf", "tbl_raw",
                    "tbl_catalogue")) {
        payload <- output[[out]]
        expect_type(payload, "character")          # DT JSON
        expect_gt(nchar(payload), 100L)
      }
    }
  )
})

test_that("server: live scores table guards a thin selection", {
  shiny::testServer(
    mod_data_server,
    args = list(
      base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive("NGA"),   # only 1
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type = "static")
      expect_error(output$tbl_live, "at least 2 comparators")
      # the reference table doesn't need a comparison group
      expect_type(output$tbl_reference, "character")
    }
  )
})
