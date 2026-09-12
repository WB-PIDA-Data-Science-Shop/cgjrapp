# Contract tests for the tidy `cgjrdata` objects the app is built against
# (the rewrite-tidy-tibble build: cgjr_taxonomy / cgjr_crosswalk / cgjr_ctf /
# cgjr_scores / cgjr_raw / wbcountries). Rewritten from the old list-based
# schema when cgjrapp moved to the tidy dependency.

test_that("cgjrdata is installed and ships the six tidy objects", {
  expect_true(requireNamespace("cgjrdata", quietly = TRUE))
  items <- data(package = "cgjrdata")$results[, "Item"]
  expect_true(all(
    c("cgjr_taxonomy", "cgjr_crosswalk", "cgjr_ctf", "cgjr_scores",
      "cgjr_raw", "wbcountries") %in% items
  ))
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

test_that("cgjr_ctf has the long indicator-grain contract", {
  ctf <- cgjrdata::cgjr_ctf
  expect_named(ctf, c(
    "unit_level", "unit_code", "unit_name", "year", "ctf_type",
    "cluster", "subcluster", "sub_subcluster", "leaf",
    "indicator", "variable", "ctf", "n_inputs", "n_inputs_obs"
  ))
  expect_setequal(unique(ctf$ctf_type), c("dynamic", "static"))
  expect_setequal(unique(ctf$unit_level), c("country", "region", "income_group"))
  # static rows carry no year; dynamic rows all do
  expect_true(all(is.na(ctf$year[ctf$ctf_type == "static"])))
  expect_true(all(!is.na(ctf$year[ctf$ctf_type == "dynamic"])))
})

test_that("cgjr_ctf country rows are an exact slice of cliaretl (never recomputed)", {
  skip_if_not_installed("cliaretl")
  jv <- cgjrdata::cgjr_crosswalk |>
    dplyr::filter(.data$leaf == "justice_and_rule_of_law", .data$static_eligible) |>
    dplyr::pull(.data$variable)
  cgjr_ken <- cgjrdata::cgjr_ctf |>
    dplyr::filter(.data$leaf == "justice_and_rule_of_law",
                  .data$ctf_type == "static", .data$unit_code == "KEN")
  cliaretl_ken <- dplyr::filter(cliaretl::closeness_to_frontier_static,
                                .data$country_code == "KEN")
  for (v in jv) {
    expect_equal(
      cgjr_ken$ctf[cgjr_ken$variable == v],
      cliaretl_ken[[v]],
      tolerance = 1e-12,
      label = v
    )
  }
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

test_that("every taxonomy leaf appears in cgjr_scores' finest-grain filter", {
  scores <- cgjrdata::cgjr_scores
  tx <- cgjrdata::cgjr_taxonomy
  leaves <- dplyr::coalesce(tx$sub_subcluster, tx$subcluster)

  branching <- unique(
    scores$subcluster[scores$node_level == "sub_subcluster"]
  )
  finest <- scores |>
    dplyr::filter(
      .data$node_level %in% c("subcluster", "sub_subcluster"),
      !(.data$node_level == "subcluster" & .data$subcluster %in% branching)
    )
  expect_true(all(leaves %in% finest$node))
})

test_that("wbcountries has the expected columns", {
  expect_true(all(
    c("country_code", "economy", "income_group",
      "lending_category", "region_code", "region") %in%
      names(cgjrdata::wbcountries)
  ))
})

test_that("cgjr_crosswalk carries taxonomy keys and cliaretl eligibility flags", {
  cw <- cgjrdata::cgjr_crosswalk
  expect_true(all(
    c("leaf", "indicator", "variable", "source",
      "in_cliaretl", "in_dynamic_panel", "in_static_panel",
      "dynamic_eligible", "static_eligible", "cliaretl_status") %in% names(cw)
  ))
  expect_setequal(
    unique(cw$cliaretl_status),
    c("resolved", "unresolved")
  )
})
