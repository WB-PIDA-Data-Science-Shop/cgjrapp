# tests/testthat/test-utils_data.R

# ── Fixtures ──────────────────────────────────────────────────────────────────

# Minimal synthetic tibble mimicking institutional_averages_tbl / ctfdata_list
make_country_tbl <- function() {
  tibble::tibble(
    country_code = c("KEN", "GHA", "NGA", "KEN", "GHA", "NGA"),
    country_name = c("Kenya", "Ghana", "Nigeria", "Kenya", "Ghana", "Nigeria"),
    year         = c(2018L, 2018L, 2018L, 2020L, 2020L, 2020L),
    score        = c(0.40, 0.50, 0.30, 0.42, 0.52, 0.32),
    var_count    = rep(4L, 6),
    nonna_count  = rep(4L, 6),
    vdem_corr    = c(0.3, 0.6, 0.2, 0.31, 0.61, 0.21),
    wjp_rl       = c(0.5, 0.4, 0.4, 0.51, 0.41, 0.41)
  )
}

# ── filter_country_data() ─────────────────────────────────────────────────────

test_that("filter_country_data returns rows for primary_iso only when no peers", {
  result <- filter_country_data(make_country_tbl(), "KEN", character(0), c(2018L, 2020L))
  expect_true(all(result$country_code == "KEN"))
  expect_equal(nrow(result), 2L)
})

test_that("filter_country_data returns rows for primary and peers", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("GHA"), c(2018L, 2020L))
  expect_true(all(c("KEN", "GHA") %in% result$country_code))
  expect_false("NGA" %in% result$country_code)
})

test_that("filter_country_data assigns country_type == 'primary' to primary_iso", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("GHA"), c(2018L, 2020L))
  ken_types <- result$country_type[result$country_code == "KEN"]
  expect_true(all(ken_types == "primary"))
})

test_that("filter_country_data assigns country_type == 'peer' to peers", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("GHA"), c(2018L, 2020L))
  gha_types <- result$country_type[result$country_code == "GHA"]
  expect_true(all(gha_types == "peer"))
})

test_that("filter_country_data adds country_type column", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("GHA"), c(2018L, 2020L))
  expect_true("country_type" %in% names(result))
})

test_that("filter_country_data year range is inclusive on both ends", {
  result <- filter_country_data(make_country_tbl(), "KEN", character(0), c(2018L, 2020L))
  expect_true(2018L %in% result$year)
  expect_true(2020L %in% result$year)
})

test_that("filter_country_data single year range returns only that year", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("GHA"), c(2018L, 2018L))
  expect_true(all(result$year == 2018L))
})

test_that("filter_country_data returns zero rows gracefully when country not in data", {
  result <- filter_country_data(make_country_tbl(), "ZZZ", character(0), c(2018L, 2020L))
  expect_equal(nrow(result), 0L)
  expect_true("country_type" %in% names(result))
})

test_that("filter_country_data returns zero rows gracefully when year range has no coverage", {
  result <- filter_country_data(make_country_tbl(), "KEN", character(0), c(2000L, 2001L))
  expect_equal(nrow(result), 0L)
})

test_that("filter_country_data returns tibble even when empty", {
  result <- filter_country_data(make_country_tbl(), "ZZZ", character(0), c(2000L, 2001L))
  expect_s3_class(result, "tbl_df")
})

test_that("filter_country_data no duplicate rows when primary_iso also in peer_isos", {
  result <- filter_country_data(make_country_tbl(), "KEN", c("KEN", "GHA"), c(2018L, 2020L))
  ken_rows <- result[result$country_code == "KEN", ]
  source_ken_rows <- sum(make_country_tbl()$country_code == "KEN")
  expect_lte(nrow(ken_rows), source_ken_rows)
})

test_that("filter_country_data errors informatively when country_code column missing", {
  bad <- tibble::tibble(iso = "KEN", year = 2018L, score = 0.5)
  expect_error(
    filter_country_data(bad, "KEN", character(0), c(2018L, 2020L)),
    regexp = "country_code"
  )
})

test_that("filter_country_data errors informatively when year column missing", {
  bad <- tibble::tibble(country_code = "KEN", score = 0.5)
  expect_error(
    filter_country_data(bad, "KEN", character(0), c(2018L, 2020L)),
    regexp = "year"
  )
})

# ── get_aggregate_data() ──────────────────────────────────────────────────────

# These tests use real cgjrdata — confirmed cluster/subcluster keys
CLUSTER    <- "institutional_environment"
SUBCLUSTER <- "degree_of_integrity"
YEAR_RANGE <- c(2015L, 2020L)

test_that("get_aggregate_data returns rows for requested region_codes", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP", "ECA"),
                               character(0), YEAR_RANGE)
  expect_true(all(result$group_code %in% c("EAP", "ECA")))
  expect_gt(nrow(result), 0L)
})

test_that("get_aggregate_data returns rows for requested income_groups", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, character(0),
                               c("High income"), YEAR_RANGE)
  expect_true(all(result$group_code == "High income"))
  expect_gt(nrow(result), 0L)
})

test_that("get_aggregate_data returns both region and income rows when both requested", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP"),
                               c("High income"), YEAR_RANGE)
  expect_true("region" %in% result$country_type)
  expect_true("income" %in% result$country_type)
})

test_that("get_aggregate_data country_type is 'region' for region rows", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP"),
                               character(0), YEAR_RANGE)
  expect_true(all(result$country_type == "region"))
})

test_that("get_aggregate_data country_type is 'income' for income rows", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, character(0),
                               c("Low income"), YEAR_RANGE)
  expect_true(all(result$country_type == "income"))
})

test_that("get_aggregate_data returns empty tibble when both inputs are empty", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, character(0),
                               character(0), YEAR_RANGE)
  expect_equal(nrow(result), 0L)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("group_label", "group_code", "year", "score", "country_type")
                  %in% names(result)))
})

test_that("get_aggregate_data year range filtering is applied correctly", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP"),
                               character(0), c(2018L, 2018L))
  expect_true(all(result$year == 2018L))
})

test_that("get_aggregate_data returns correct group_label for regions", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP"),
                               character(0), YEAR_RANGE)
  expect_true(all(result$group_label == "East Asia & Pacific"))
})

test_that("get_aggregate_data has required output columns", {
  result <- get_aggregate_data(CLUSTER, SUBCLUSTER, c("EAP"),
                               c("High income"), YEAR_RANGE)
  expected_cols <- c("group_label", "group_code", "year", "score", "country_type")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_aggregate_data works across all cluster/subcluster combinations", {
  purrr::iwalk(cgjrdata::ctfdata_list, function(cluster_tbl, cl) {
    purrr::iwalk(cluster_tbl, function(sub_tbl, sc) {
      result <- tryCatch(
        get_aggregate_data(cl, sc, c("EAP"), c("High income"), YEAR_RANGE),
        error = function(e) {
          testthat::fail(paste0("Error in ", cl, "/", sc, ": ", conditionMessage(e)))
        }
      )
      expect_s3_class(result, "tbl_df")
    })
  })
})

# ── get_annual_thresholds() ─────────────────────────────────────────────────

test_that("get_annual_thresholds returns one row per year in year_range", {
  result <- get_annual_thresholds(make_country_tbl(), "score",
                                  c(2018L, 2020L), probs = c(0.25, 0.50))
  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$year), c(2018L, 2020L))
})

test_that("get_annual_thresholds returns columns year, q1, q2", {
  result <- get_annual_thresholds(make_country_tbl(), "score",
                                  c(2018L, 2020L))
  expect_true(all(c("year", "q1", "q2") %in% names(result)))
})

test_that("get_annual_thresholds q1 < q2 for all rows", {
  result <- get_annual_thresholds(make_country_tbl(), "score",
                                  c(2018L, 2020L), probs = c(0.25, 0.75))
  expect_true(all(result$q1 < result$q2))
})

test_that("get_annual_thresholds respects year_range filter", {
  result <- get_annual_thresholds(make_country_tbl(), "score",
                                  c(2018L, 2018L))
  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 2018L)
})

test_that("get_annual_thresholds works with tercile probs", {
  result <- get_annual_thresholds(make_country_tbl(), "score",
                                  c(2018L, 2020L), probs = c(1/3, 2/3))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$q1 < result$q2))
})

test_that("get_annual_thresholds errors when y_var not in data", {
  expect_error(
    get_annual_thresholds(make_country_tbl(), "nonexistent", c(2018L, 2020L)),
    regexp = "y_var"
  )
})

test_that("get_annual_thresholds errors on invalid probs", {
  expect_error(
    get_annual_thresholds(make_country_tbl(), "score",
                          c(2018L, 2020L), probs = c(0.75, 0.25)),
    regexp = "probs"
  )
  expect_error(
    get_annual_thresholds(make_country_tbl(), "score",
                          c(2018L, 2020L), probs = c(0.5)),
    regexp = "probs"
  )
})

test_that("get_annual_thresholds works on real cgjrdata", {
  result <- get_annual_thresholds(
    data       = cgjrdata::institutional_averages_tbl,
    y_var      = "institutional_environment_score",
    year_range = c(2015L, 2020L),
    probs      = c(0.25, 0.50)
  )
  expect_equal(nrow(result), 6L)
  expect_true(all(result$q1 < result$q2))
})

# ── get_cluster_member_data() ─────────────────────────────────────────────────

test_that("get_cluster_member_data returns empty tibble when no groups selected", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = character(0),
    income_groups = character(0),
    year_range    = YEAR_RANGE
  )
  expect_equal(nrow(result), 0L)
  expect_true(all(c("country_code", "country_name", "year", "score",
                    "group_label", "group_code", "country_type") %in% names(result)))
})

test_that("get_cluster_member_data returns rows for selected region", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = c("AFE"),
    income_groups = character(0),
    year_range    = YEAR_RANGE
  )
  expect_gt(nrow(result), 0L)
  expect_true(all(result$country_type == "region"))
  expect_true(all(result$year >= YEAR_RANGE[[1]] & result$year <= YEAR_RANGE[[2]]))
})

test_that("get_cluster_member_data returns rows for selected income group", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = character(0),
    income_groups = c("Low income"),
    year_range    = YEAR_RANGE
  )
  expect_gt(nrow(result), 0L)
  expect_true(all(result$country_type == "income"))
  expect_true(all(result$group_label == "Low income"))
})

test_that("get_cluster_member_data combines region and income rows", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = c("AFE"),
    income_groups = c("Low income"),
    year_range    = YEAR_RANGE
  )
  expect_true("region" %in% result$country_type)
  expect_true("income" %in% result$country_type)
})

test_that("get_cluster_member_data output contains required columns", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = c("AFE"),
    income_groups = character(0),
    year_range    = YEAR_RANGE
  )
  expected_cols <- c("country_code", "country_name", "year", "score",
                     "group_label", "group_code", "country_type")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_cluster_member_data respects year_range filter", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = c("AFE"),
    income_groups = character(0),
    year_range    = c(2018L, 2018L)
  )
  expect_true(all(result$year == 2018L))
})

test_that("get_cluster_member_data has no NA scores", {
  result <- get_cluster_member_data(
    cluster       = CLUSTER,
    subcluster    = SUBCLUSTER,
    score_var     = "score",
    region_codes  = c("AFE"),
    income_groups = character(0),
    year_range    = YEAR_RANGE
  )
  expect_false(anyNA(result$score))
})

# ── get_indicator_choices() ───────────────────────────────────────────────────

test_that("get_indicator_choices excludes standard structural columns", {
  tbl <- make_country_tbl()
  result <- get_indicator_choices(tbl)
  excluded <- c("country_code", "country_name", "year", "score", "var_count", "nonna_count")
  expect_false(any(excluded %in% result))
})

test_that("get_indicator_choices returns indicator columns", {
  tbl <- make_country_tbl()
  result <- get_indicator_choices(tbl)
  expect_true("vdem_corr" %in% result)
  expect_true("wjp_rl" %in% result)
})

test_that("get_indicator_choices returns a named character vector", {
  tbl <- make_country_tbl()
  result <- get_indicator_choices(tbl)
  expect_type(result, "character")
  expect_identical(names(result), unname(result))
})

test_that("get_indicator_choices returns character(0) when no indicator columns present", {
  tbl <- tibble::tibble(
    country_code = "KEN", country_name = "Kenya", year = 2018L,
    score = 0.4, var_count = 2L, nonna_count = 2L
  )
  result <- get_indicator_choices(tbl)
  expect_length(result, 0L)
})

test_that("get_indicator_choices works on real ctfdata_list subcluster", {
  tbl <- cgjrdata::ctfdata_list[[CLUSTER]][[SUBCLUSTER]]
  result <- get_indicator_choices(tbl)
  expect_gt(length(result), 0L)
  expect_false(any(c("score", "var_count", "nonna_count") %in% result))
})

# ── get_country_choices() ─────────────────────────────────────────────────────

test_that("get_country_choices returns a named character vector", {
  result <- get_country_choices()
  expect_type(result, "character")
  expect_false(is.null(names(result)))
})

test_that("get_country_choices values are ISO3 codes", {
  result <- get_country_choices()
  # All values should be 3-character strings
  expect_true(all(nchar(unname(result)) == 3L))
})

test_that("get_country_choices is sorted alphabetically by country name", {
  result <- get_country_choices()
  expect_identical(names(result), sort(names(result)))
})

test_that("get_country_choices returns non-zero entries", {
  result <- get_country_choices()
  expect_gt(length(result), 0L)
})

# ── get_region_choices() ──────────────────────────────────────────────────────

test_that("get_region_choices returns a named character vector", {
  result <- get_region_choices()
  expect_type(result, "character")
  expect_false(is.null(names(result)))
})

test_that("get_region_choices returns exactly 8 entries", {
  result <- get_region_choices()
  expect_length(result, 8L)
})

test_that("get_region_choices contains expected region codes", {
  result <- get_region_choices()
  expected <- c("AFE", "AFW", "EAP", "ECA", "LAC", "MENAAP", "NAC", "SAR")
  expect_true(all(expected %in% unname(result)))
})

# ── get_income_choices() ──────────────────────────────────────────────────────

test_that("get_income_choices returns a named character vector", {
  result <- get_income_choices()
  expect_type(result, "character")
  expect_false(is.null(names(result)))
})

test_that("get_income_choices returns exactly 4 entries (no NA)", {
  result <- get_income_choices()
  expect_length(result, 4L)
  expect_false(any(is.na(result)))
})

test_that("get_income_choices contains expected income groups", {
  result <- get_income_choices()
  expected <- c("High income", "Low income", "Lower middle income", "Upper middle income")
  expect_true(all(expected %in% unname(result)))
})

test_that("get_region_choices values are all valid region codes in regionctf_list", {
  # The codes returned by get_region_choices() must be valid keys in regionctf_list
  region_codes <- unname(get_region_choices())
  valid_codes  <- cgjrdata::regionctf_list[[1]][[1]] |>
    dplyr::distinct(region_code) |>
    dplyr::pull(region_code)
  expect_true(all(region_codes %in% valid_codes))
})
