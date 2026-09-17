# Slice 5 - Data Download tab module.

COMPS <- c("NGA", "KEN", "SEN", "CIV", "BEN")

test_that("mod_data_ui has the three labelled sub-tabs", {
  html <- as.character(mod_data_ui("data"))
  for (piece in c("data-tbl_ctf", "data-tbl_raw", "data-tbl_catalogue",
                  "data-ctf_type")) {
    expect_match(html, piece, fixed = TRUE)
  }
  # no Scores sub-tab - cliarappak's benchmarking tab doesn't show one either
  expect_no_match(html, "data-tbl_live", fixed = TRUE)
  expect_no_match(html, "data-tbl_reference", fixed = TRUE)
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
      for (out in c("tbl_ctf", "tbl_raw", "tbl_catalogue")) {
        payload <- output[[out]]
        expect_type(payload, "character")          # DT JSON
        expect_gt(nchar(payload), 100L)
      }
    }
  )
})
