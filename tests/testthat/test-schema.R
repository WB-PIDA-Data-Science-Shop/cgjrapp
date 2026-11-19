# What cgjrapp assumes about the cgjrdata objects it is built against.
# cgjrdata's own tests check every value against cliaretl; these pin the
# shape the app's code relies on.

test_that("cgjrdata ships the five objects the app uses", {
  expect_true(requireNamespace("cgjrdata", quietly = TRUE))
  items <- data(package = "cgjrdata")$results[, "Item"]
  expect_setequal(items, c("cgjr_taxonomy", "cgjr_crosswalk", "cgjr_ctf",
                           "cgjr_raw", "wbcountries"))
})

test_that("cgjr_taxonomy is a 14-row, 3-level fixed-width schema", {
  tx <- cgjrdata::cgjr_taxonomy
  expect_equal(nrow(tx), 14L)
  expect_named(tx, c(
    "cluster", "cluster_num", "cluster_name",
    "subcluster", "subcluster_num", "subcluster_name",
    "sub_subcluster", "sub_subcluster_num", "sub_subcluster_name"
  ))
  expect_equal(dplyr::n_distinct(tx$cluster), 4L)
  expect_equal(dplyr::n_distinct(tx$subcluster), 11L)
  # only Public Financial Management branches to a sub_subcluster level
  branching <- tx |>
    dplyr::filter(!is.na(sub_subcluster)) |>
    dplyr::distinct(subcluster) |>
    dplyr::pull(subcluster)
  expect_identical(branching, "public_financial_management")
})

test_that("cgjr_ctf has the long indicator-grain contract, real countries only", {
  ctf <- cgjrdata::cgjr_ctf
  expect_named(ctf, c(
    "unit_level", "unit_code", "unit_name", "year", "ctf_type",
    "cluster", "subcluster", "sub_subcluster", "leaf",
    "indicator", "variable", "ctf"
  ))
  expect_setequal(unique(ctf$ctf_type), c("dynamic", "static"))
  # regions and income groups are built from wbcountries members, never rows
  expect_identical(unique(ctf$unit_level), "country")
  expect_true(all(ctf$unit_code %in% cgjrdata::wbcountries$country_code))
  expect_false(anyNA(ctf$unit_name))
  # static rows carry no year; dynamic rows all do
  expect_true(all(is.na(ctf$year[ctf$ctf_type == "static"])))
  expect_true(all(!is.na(ctf$year[ctf$ctf_type == "dynamic"])))
})

test_that("empty (leaf, ctf_type) pairs are absent from cgjr_ctf, not scaffolded", {
  # budget_cycle_and_fiscal_planning: 24 static-eligible indicators, 0 dynamic
  ctf <- cgjrdata::cgjr_ctf
  expect_equal(
    nrow(dplyr::filter(ctf, .data$leaf == "budget_cycle_and_fiscal_planning",
                       .data$ctf_type == "dynamic")),
    0L
  )
  expect_gt(
    nrow(dplyr::filter(ctf, .data$leaf == "budget_cycle_and_fiscal_planning",
                       .data$ctf_type == "static")),
    0L
  )
  # the other three PFM sub-subclusters are empty in both
  expect_equal(
    nrow(dplyr::filter(ctf, .data$leaf == "domestic_revenue_mobilization")),
    0L
  )
})

test_that("cgjr_raw holds observed values for real countries only", {
  raw <- cgjrdata::cgjr_raw
  expect_true(all(raw$unit_code %in% cgjrdata::wbcountries$country_code))
  expect_false(anyNA(raw$unit_name))
  expect_false(anyNA(raw$value))
})

test_that("wbcountries has the expected columns", {
  expect_true(all(
    c("country_code", "economy", "income_group",
      "lending_category", "region_code", "region") %in%
      names(cgjrdata::wbcountries)
  ))
})

test_that("cgjr_crosswalk carries what the indicator catalogue shows", {
  cw <- cgjrdata::cgjr_crosswalk
  expect_true(all(
    c("leaf", "indicator_num", "indicator", "source", "variable",
      "description_short", "cluster_num", "cluster_name",
      "subcluster_num", "subcluster_name",
      "sub_subcluster_num", "sub_subcluster_name",
      "in_dynamic", "in_static") %in% names(cw)
  ))
})
