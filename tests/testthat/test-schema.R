test_that("cgjrdata is installed and loads", {
  expect_true(requireNamespace("cgjrdata", quietly = TRUE))
})

test_that("institutional_averages_tbl has expected columns", {
  expected_cols <- c(
    "country_code", "country_name", "year",
    "institutional_environment_score",
    "political_institutions_score",
    "center_of_government_score",
    "sectors_service_delivery_score",
    "overall_score"
  )
  actual_cols <- names(cgjrdata::institutional_averages_tbl)
  expect_true(
    all(expected_cols %in% actual_cols),
    label = paste(
      "Missing columns:",
      paste(setdiff(expected_cols, actual_cols), collapse = ", ")
    )
  )
})

test_that("institutional_averages_tbl covers expected year range", {
  years <- cgjrdata::institutional_averages_tbl$year
  expect_lte(min(years, na.rm = TRUE), 2013L)
  expect_gte(max(years, na.rm = TRUE), 2024L)
})

test_that("ctfdata_list has exactly 4 clusters", {
  expect_length(cgjrdata::ctfdata_list, 4L)
})

test_that("ctfdata_list has expected cluster names", {
  expected <- c(
    "institutional_environment",
    "political_institutions",
    "center_of_government",
    "sectors_service_delivery"
  )
  expect_identical(names(cgjrdata::ctfdata_list), expected)
})

test_that("ctfdata_list subclusters match expected counts per cluster", {
  expected_counts <- c(
    institutional_environment    = 4L,
    political_institutions       = 1L,
    center_of_government         = 3L,
    sectors_service_delivery     = 5L
  )
  actual_counts <- purrr::map_int(cgjrdata::ctfdata_list, length)
  expect_identical(actual_counts, expected_counts)
})

test_that("ctfdata_list subclusters have required base columns", {
  required <- c("country_code", "country_name", "year", "score", "var_count", "nonna_count")
  purrr::iwalk(cgjrdata::ctfdata_list, function(cluster, cluster_name) {
    purrr::iwalk(cluster, function(tbl, subcluster_name) {
      missing <- setdiff(required, names(tbl))
      expect_true(
        length(missing) == 0L,
        label = paste0("Missing in ", cluster_name, "/", subcluster_name, ": ",
                       paste(missing, collapse = ", "))
      )
    })
  })
})

test_that("regionctf_list has same cluster/subcluster structure as ctfdata_list", {
  expect_identical(names(cgjrdata::regionctf_list), names(cgjrdata::ctfdata_list))
  expect_identical(
    purrr::map(cgjrdata::regionctf_list, names),
    purrr::map(cgjrdata::ctfdata_list, names)
  )
})

test_that("regionctf_list subclusters have region_code, region, year, score columns", {
  required <- c("region_code", "region", "year", "score")
  purrr::iwalk(cgjrdata::regionctf_list, function(cluster, cluster_name) {
    purrr::iwalk(cluster, function(tbl, subcluster_name) {
      missing <- setdiff(required, names(tbl))
      expect_true(
        length(missing) == 0L,
        label = paste0("Missing in regionctf_list ", cluster_name, "/",
                       subcluster_name, ": ", paste(missing, collapse = ", "))
      )
    })
  })
})

test_that("regionctf_list contains expected 8 region codes", {
  expected_codes <- c("AFE", "AFW", "EAP", "ECA", "LAC", "MENAAP", "NAC", "SAR")
  actual_codes <- cgjrdata::regionctf_list[[1]][[1]] |>
    dplyr::distinct(region_code) |>
    dplyr::pull(region_code) |>
    sort()
  expect_identical(actual_codes, sort(expected_codes))
})

test_that("incomectf_list has same cluster/subcluster structure as ctfdata_list", {
  expect_identical(names(cgjrdata::incomectf_list), names(cgjrdata::ctfdata_list))
})

test_that("incomectf_list subclusters have income_group, year, score columns", {
  required <- c("income_group", "year", "score")
  purrr::iwalk(cgjrdata::incomectf_list, function(cluster, cluster_name) {
    purrr::iwalk(cluster, function(tbl, subcluster_name) {
      missing <- setdiff(required, names(tbl))
      expect_true(
        length(missing) == 0L,
        label = paste0("Missing in incomectf_list ", cluster_name, "/",
                       subcluster_name, ": ", paste(missing, collapse = ", "))
      )
    })
  })
})

test_that("incomectf_list contains expected 4 income groups", {
  expected_groups <- c(
    "High income", "Low income", "Lower middle income", "Upper middle income"
  )
  actual_groups <- cgjrdata::incomectf_list[[1]][[1]] |>
    dplyr::distinct(income_group) |>
    dplyr::pull(income_group) |>
    sort()
  expect_identical(actual_groups, sort(expected_groups))
})

test_that("wbcountries has expected columns", {
  expected_cols <- c(
    "country_code", "economy", "income_group",
    "lending_category", "region_code", "region"
  )
  missing <- setdiff(expected_cols, names(cgjrdata::wbcountries))
  expect_true(
    length(missing) == 0L,
    label = paste("Missing wbcountries cols:", paste(missing, collapse = ", "))
  )
})

test_that("metadata_tbl has expected columns", {
  expected_cols <- c(
    "var_name", "variable", "description", "description_short",
    "source", "cluster", "cluster_num", "subcluster", "subcluster_num"
  )
  missing <- setdiff(expected_cols, names(cgjrdata::metadata_tbl))
  expect_true(
    length(missing) == 0L,
    label = paste("Missing metadata_tbl cols:", paste(missing, collapse = ", "))
  )
})

test_that("metadata_tbl has exactly 4 distinct clusters", {
  n_clusters <- cgjrdata::metadata_tbl |>
    dplyr::distinct(cluster) |>
    nrow()
  expect_equal(n_clusters, 4L)
})

test_that("metadata_tbl has exactly 13 distinct subclusters", {
  n_subclusters <- cgjrdata::metadata_tbl |>
    dplyr::distinct(subcluster) |>
    nrow()
  expect_equal(n_subclusters, 13L)
})
