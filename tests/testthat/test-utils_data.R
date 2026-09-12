# tests/testthat/test-utils_data.R
#
# The overview/detail-era helpers this file used to cover (filter_country_data,
# get_aggregate_data, get_cluster_member_data, get_annual_thresholds,
# get_indicator_choices) were removed with mod_overview / mod_detail in
# build-plan Slice 2. What's left here are the sidebar picker helpers.

# -- get_country_choices() ---

test_that("get_country_choices returns an ISO3-valued, name-sorted vector", {
  result <- get_country_choices()
  expect_type(result, "character")
  expect_gt(length(result), 0L)
  expect_true(all(nchar(unname(result)) == 3L))
  expect_identical(names(result), sort(names(result)))
})

test_that("get_country_choices codes resolve in cgjr_ctf", {
  codes <- unname(get_country_choices())
  ctf_codes <- unique(cgjrdata::cgjr_ctf$unit_code[cgjrdata::cgjr_ctf$unit_level == "country"])
  # most WB economies are covered; assert the bulk overlap rather than 100%
  expect_gt(mean(codes %in% ctf_codes), 0.8)
})

# -- get_region_choices() ---

test_that("get_region_choices returns the 8 WB region codes", {
  result <- get_region_choices()
  expect_type(result, "character")
  expect_length(result, 8L)
  expect_setequal(
    unname(result),
    c("AFE", "AFW", "EAP", "ECA", "LAC", "MENAAP", "NAC", "SAR")
  )
})

test_that("get_region_choices codes match the region rows in cgjr_ctf", {
  region_codes <- unname(get_region_choices())
  ctf_regions  <- unique(cgjrdata::cgjr_ctf$unit_code[cgjrdata::cgjr_ctf$unit_level == "region"])
  expect_setequal(region_codes, ctf_regions)
})

# -- get_income_choices() ---

test_that("get_income_choices returns the 4 WB income groups, no NA", {
  result <- get_income_choices()
  expect_type(result, "character")
  expect_length(result, 4L)
  expect_false(anyNA(result))
  expect_setequal(
    unname(result),
    c("High income", "Low income", "Lower middle income", "Upper middle income")
  )
})

# -- key_to_title() ---

test_that("key_to_title turns snake_case taxonomy keys into titles", {
  expect_equal(key_to_title("justice_and_rule_of_law"), "Justice and Rule of Law")
  expect_equal(key_to_title("degree_of_integrity"), "Degree of Integrity")
  expect_equal(key_to_title("institutional_environment"), "Institutional Environment")
})
