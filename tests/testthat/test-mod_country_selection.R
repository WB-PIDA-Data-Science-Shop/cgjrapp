# Slice 3 - the comparator selection module.

test_that(".unit_choices groups every cgjr_ctf unit by level", {
  ch <- cgjrapp:::.unit_choices()
  expect_named(ch, c("Countries", "Regions", "Income groups"))
  expect_true("GHA" %in% unname(ch$Countries))
  expect_setequal(unname(ch$Regions),
                  c("AFE", "AFW", "EAP", "ECA", "LAC", "MENAAP", "NAC", "SAR"))
  expect_length(ch[["Income groups"]], 4L)
  # every WB-aggregate "country" row (AFE, NAC, EAS, HIC, OED, ...) is dropped
  # from Countries - only real countries remain
  aggs <- unique(cliaretl::wb_country_list$group_code)
  expect_false(any(aggs %in% unname(ch$Countries)))
  expect_true(all(unname(ch$Countries) %in% cliaretl::wb_country_list$country_code))
  expect_length(ch$Countries, 216L)
})

test_that(".expand_to_countries drops leaked aggregate 'country' rows", {
  got <- cgjrapp:::.expand_to_countries(c("HIC", "EAS", "OED"))
  expect_length(got, 0L)
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

test_that(".expand_to_countries passes countries through unchanged", {
  expect_setequal(cgjrapp:::.expand_to_countries(c("GHA", "NGA")), c("GHA", "NGA"))
})

test_that(".expand_to_countries swaps a region code for its member countries", {
  wb <- cgjrdata::wbcountries
  expected <- wb$country_code[!is.na(wb$region_code) & wb$region_code == "AFE"]
  got <- cgjrapp:::.expand_to_countries("AFE")
  expect_gt(length(got), 5L)
  expect_setequal(got, expected)
  expect_false("AFE" %in% got)   # the region code itself is never kept
})

test_that(".expand_to_countries swaps an income-group slug for its member countries (matched on label, not slug)", {
  wb <- cgjrdata::wbcountries
  expected <- wb$country_code[!is.na(wb$income_group) & wb$income_group == "High income"]
  got <- cgjrapp:::.expand_to_countries("high_income")
  expect_gt(length(got), 5L)
  expect_setequal(got, expected)
})

test_that(".expand_to_countries unions a mix of countries, a region and an income group", {
  got <- cgjrapp:::.expand_to_countries(c("GHA", "AFE", "high_income"))
  wb <- cgjrdata::wbcountries
  expect_true("GHA" %in% got)
  expect_true(all(wb$country_code[!is.na(wb$region_code) & wb$region_code == "AFE"] %in% got))
  expect_true(all(wb$country_code[!is.na(wb$income_group) & wb$income_group == "High income"] %in% got))
})

test_that("picking a region as a comparator expands it to member countries, not a single unit", {
  # comps_live()/comparison_units() re-derive the expansion straight from
  # input$comparators (see mod_country_selection.R), so this holds even
  # though testServer's MockShinySession can't verify the cosmetic
  # updateSelectizeInput() widget-display sync (it has no sendInputMessage()).
  shiny::testServer(mod_country_selection_server, args = list(default_base = "GHA"), {
    session$setInputs(base = "GHA", comparators = "AFE")
    session$setInputs(apply = 1)
    got <- session$returned$comparison_units()
    expect_false("AFE" %in% got)
    wb <- cgjrdata::wbcountries
    expect_setequal(
      got,
      setdiff(wb$country_code[!is.na(wb$region_code) & wb$region_code == "AFE"], "GHA")
    )
  })
})

test_that("the base unit picker can hold more than one country", {
  shiny::testServer(mod_country_selection_server, args = list(default_base = "GHA"), {
    session$setInputs(base = c("GHA", "KEN"), comparators = c("NGA", "SEN", "CIV"))
    session$setInputs(apply = 1)
    expect_setequal(session$returned$base_unit(), c("GHA", "KEN"))
    expect_setequal(session$returned$comparison_units(), c("NGA", "SEN", "CIV"))
  })
})

test_that("UI renders the pickers, gate and save/load", {
  html <- as.character(mod_country_selection_ui("selection"))
  for (piece in c("selection-base", "selection-comparators", "selection-threshold",
                  "selection-year_range", "selection-apply_ui",
                  "selection-save_sel", "selection-load_sel")) {
    expect_match(html, piece, fixed = TRUE)
  }
})

test_that("UI is card-laid-out like cliarappak's selection UI", {
  html <- as.character(mod_country_selection_ui("selection"))
  expect_match(html, "Base country:", fixed = TRUE)
  expect_match(html, "Selection of Countries", fixed = TRUE)
  expect_match(html, "card-success", fixed = TRUE)
  expect_match(html, "Comparator countries", fixed = TRUE)
  expect_match(html, "Benchmarking options", fixed = TRUE)
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

test_that(".flag_content prefixes each country with its flagcdn flag", {
  out <- cgjrapp:::.flag_content(c(Ghana = "GHA", Kosovo = "XKX",
                                   `Channel Islands` = "CHI",
                                   `High income` = "high_income"))
  expect_identical(out[[1]], '<img src="https://flagcdn.com/w20/gh.png" alt="GH"> Ghana')
  expect_match(out[[2]], "flagcdn.com/w20/xk.png", fixed = TRUE)
  expect_identical(out[[3]], "Channel Islands")   # no ISO2 flag
  expect_identical(out[[4]], "High income")       # groups: plain name
  # names are HTML-escaped
  expect_match(cgjrapp:::.flag_content(c(`A & B` = "GHA")), "A &amp; B", fixed = TRUE)
})

test_that("pickers are cliarappak's pickerInput with flags, search and select-all", {
  html <- as.character(mod_country_selection_ui("selection"))
  expect_match(html, "selectpicker", fixed = TRUE)
  expect_match(html, "flagcdn.com/w20/gh.png", fixed = TRUE)
  expect_match(html, 'data-live-search="true"', fixed = TRUE)
  expect_match(html, 'data-actions-box="true"', fixed = TRUE)
})

# --- Downloads card: Word / PPT reports on the APPLIED selection --------------

test_that("UI has the Downloads card with the cluster picker", {
  html <- as.character(mod_country_selection_ui("selection"))
  expect_match(html, "Downloads", fixed = TRUE)
  expect_match(html, "selection-report_clusters", fixed = TRUE)
  expect_match(html, "selection-downloads_ui", fixed = TRUE)
  for (nm in unique(cgjrdata::cgjr_taxonomy$cluster_name)) {
    expect_match(html, nm, fixed = TRUE)
  }
})

test_that("report buttons follow the applied selection, not the live pickers", {
  shiny::testServer(mod_country_selection_server, args = list(default_base = "GHA"), {
    session$setInputs(report_clusters = "context")
    # the default applied selection (GHA vs region-mates) is reportable
    ui <- paste(as.character(output$downloads_ui), collapse = " ")
    expect_match(ui, "-report_docx\"", fixed = TRUE)
    expect_match(ui, "-report_pptx\"", fixed = TRUE)

    # changing the pickers without Apply changes nothing
    session$setInputs(base = c("GHA", "NGA"))
    expect_match(paste(as.character(output$downloads_ui), collapse = " "),
                 "-report_docx\"", fixed = TRUE)

    # applying two base countries disables the buttons, with a reason
    session$setInputs(comparators = c("SEN", "CIV", "BEN"), threshold = "Default",
                      year_range = c(2013L, 2024L), apply = 1)
    ui <- paste(as.character(output$downloads_ui), collapse = " ")
    expect_match(ui, "exactly one base country", fixed = TRUE)
    expect_no_match(ui, "-report_docx\"", fixed = TRUE)

    session$setInputs(base = "GHA", apply = 2, report_clusters = character(0))
    expect_match(paste(as.character(output$downloads_ui), collapse = " "),
                 "at least one cluster", fixed = TRUE)
  })
})

test_that("Word download writes a report for the applied selection", {
  shiny::testServer(mod_country_selection_server, args = list(default_base = "GHA"), {
    session$setInputs(report_clusters = "institutional_environment")
    path <- output$report_docx
    expect_true(file.exists(path))
    s <- officer::docx_summary(officer::read_docx(path))
    expect_true("Institutional Environment" %in% s$text)
    expect_false("Context" %in% s$text[s$style_name == "heading 1"])
  })
})
