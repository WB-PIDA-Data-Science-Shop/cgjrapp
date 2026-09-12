# Slice 3 - the comparator selection module.

test_that(".unit_choices groups every cgjr_ctf unit by level", {
  ch <- cgjrapp:::.unit_choices()
  expect_named(ch, c("Countries", "Regions", "Income groups"))
  expect_true("GHA" %in% unname(ch$Countries))
  expect_setequal(unname(ch$Regions),
                  c("AFE", "AFW", "EAP", "ECA", "LAC", "MENAAP", "NAC", "SAR"))
  expect_length(ch[["Income groups"]], 4L)
  # the AFE/AFW WB-aggregate "country" rows are dropped from Countries
  expect_false(any(c("AFE", "AFW") %in% unname(ch$Countries)))
})

test_that(".default_comparators returns the base country's region-mates", {
  comps <- cgjrapp:::.default_comparators("GHA")
  expect_gt(length(comps), 5L)
  expect_false("GHA" %in% comps)
  wb <- cgjrdata::wbcountries
  gha_region  <- wb$region_code[wb$country_code == "GHA"]
  comp_region <- wb$region_code[match(comps, wb$country_code)]
  expect_true(all(comp_region == gha_region))
})

test_that("UI renders the pickers, gate and save/load", {
  html <- as.character(mod_country_selection_ui("selection"))
  for (piece in c("selection-base", "selection-comparators", "selection-threshold",
                  "selection-year_range", "selection-apply_ui",
                  "selection-save_sel", "selection-load_sel")) {
    expect_match(html, piece, fixed = TRUE)
  }
})

test_that("applied state holds the default selection before any Apply click", {
  shiny::testServer(mod_country_selection_server, args = list(default_base = "GHA"), {
    session$setInputs(base = "KEN", comparators = c("NGA", "SEN", "CIV"),
                      threshold = "Terciles", year_range = c(2016L, 2020L))
    sel <- session$returned
    # changing the pickers does NOT move the applied state
    expect_identical(sel$base_unit(), "GHA")
    expect_setequal(sel$comparison_units(), cgjrapp:::.default_comparators("GHA"))
    expect_identical(sel$threshold(), "Default")
    expect_equal(sel$year_range(), c(2013L, 2024L))
  })
})

test_that("Apply commits the current pickers; base is removed from comparators", {
  shiny::testServer(mod_country_selection_server, {
    session$setInputs(base = "GHA", comparators = c("GHA", "NGA", "SEN"),
                      threshold = "Terciles", year_range = c(2014L, 2022L))
    session$setInputs(apply = 1)
    sel <- session$returned
    expect_identical(sel$base_unit(), "GHA")
    expect_setequal(sel$comparison_units(), c("NGA", "SEN"))
    expect_identical(sel$threshold(), "Terciles")
    expect_equal(sel$year_range(), c(2014L, 2022L))
  })
})

test_that("changes after Apply require another Apply to take effect", {
  shiny::testServer(mod_country_selection_server, {
    session$setInputs(base = "GHA", comparators = c("NGA", "SEN"),
                      threshold = "Default", year_range = c(2013L, 2024L), apply = 1)
    session$setInputs(threshold = "Terciles", year_range = c(2014L, 2022L))
    expect_identical(session$returned$threshold(), "Default")   # not yet re-applied
    session$setInputs(apply = 2)
    expect_identical(session$returned$threshold(), "Terciles")
    expect_equal(session$returned$year_range(), c(2014L, 2022L))
  })
})

test_that("comp_count message scales with the comparator count", {
  shiny::testServer(mod_country_selection_server, {
    session$setInputs(base = "GHA", comparators = c("NGA", "SEN"))
    expect_match(output$comp_count, "steadier with 10")
    session$setInputs(comparators = character(0))
    expect_match(output$comp_count, "No comparators")
  })
})

test_that("an invalid load file is rejected, not fatal", {
  shiny::testServer(mod_country_selection_server, {
    tmp <- withr::local_tempfile(fileext = ".rds")
    saveRDS(list(not = "a selection"), tmp)
    session$setInputs(load_sel = list(datapath = tmp, name = "bad.rds"))
    expect_no_error(session$flushReact())
  })
})

test_that("a valid load file is accepted without error", {
  shiny::testServer(mod_country_selection_server, {
    tmp <- withr::local_tempfile(fileext = ".rds")
    saveRDS(list(base = "KEN", comparators = c("UGA", "TZA"),
                 threshold = "Terciles", year_range = c(2016L, 2020L)), tmp)
    session$setInputs(load_sel = list(datapath = tmp, name = "sel.rds"))
    expect_no_error(session$flushReact())
  })
})
