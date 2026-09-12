test_that("filter_unit_data slices one leaf / ctf_type / unit set", {
  out <- filter_unit_data("justice_and_rule_of_law", "static", c("KEN", "GHA"))
  expect_s3_class(out, "tbl_df")
  expect_setequal(unique(out$leaf), "justice_and_rule_of_law")
  expect_setequal(unique(out$ctf_type), "static")
  expect_setequal(unique(out$unit_code), c("KEN", "GHA"))
  expect_true(all(is.na(out$year)))                # static carries no year
  expect_named(out, names(cgjrdata::cgjr_ctf))
})

test_that("filter_unit_data never recomputes ctf - values match cgjr_ctf", {
  out <- filter_unit_data("justice_and_rule_of_law", "dynamic", "KEN")
  ref <- dplyr::filter(
    cgjrdata::cgjr_ctf,
    leaf == "justice_and_rule_of_law", ctf_type == "dynamic", unit_code == "KEN"
  )
  expect_identical(
    dplyr::arrange(out, year, variable),
    dplyr::arrange(ref, year, variable)
  )
})

test_that("year_range bounds dynamic rows and is ignored for static", {
  dyn <- filter_unit_data("justice_and_rule_of_law", "dynamic", "KEN",
                          year_range = c(2016L, 2019L))
  expect_equal(range(dyn$year), c(2016L, 2019L))

  stat <- filter_unit_data("justice_and_rule_of_law", "static", "KEN",
                           year_range = c(2016L, 2019L))
  expect_gt(nrow(stat), 0L)
  expect_true(all(is.na(stat$year)))
})

test_that("even_years keeps only even years for the dynamic view (cliarappak parity)", {
  ev <- filter_unit_data("justice_and_rule_of_law", "dynamic", "KEN",
                         year_range = c(2013L, 2024L), even_years = TRUE)
  expect_setequal(unique(ev$year), c(2014L, 2016L, 2018L, 2020L, 2022L, 2024L))

  # even_years is a no-op for static
  st <- filter_unit_data("justice_and_rule_of_law", "static", "KEN",
                         even_years = TRUE)
  expect_gt(nrow(st), 0L)

  # and a strict subset of the full-year slice
  full <- filter_unit_data("justice_and_rule_of_law", "dynamic", "KEN",
                           year_range = c(2013L, 2024L))
  expect_lt(nrow(ev), nrow(full))
})

test_that("an empty (leaf, ctf_type) returns zero rows, not an error", {
  out <- filter_unit_data("budget_cycle_and_fiscal_planning", "dynamic",
                          c("KEN", "GHA"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, names(cgjrdata::cgjr_ctf))

  # ... but the same leaf is populated for static
  expect_gt(
    nrow(filter_unit_data("budget_cycle_and_fiscal_planning", "static", "KEN")),
    0L
  )
})

test_that("regions and income groups are addressable by unit_code", {
  out <- filter_unit_data("justice_and_rule_of_law", "static",
                          c("EAP", "high_income"))
  expect_setequal(unique(out$unit_code), c("EAP", "high_income"))
  expect_setequal(unique(out$unit_level), c("region", "income_group"))
})

test_that("KNOWN cgjrdata quirk: AFE/AFW exist at BOTH country and region level", {
  # cgjrdata's tidy cgjr_ctf carries "Africa Eastern and Southern" / "...Western
  # and Central" as unit_level == "country" rows in addition to the real region
  # rows, so unit_code is not unique across unit_level. filter_unit_data() by
  # unit_code alone therefore returns both. Flagged for the cgjrdata team;
  # Slice 3's unit picker will need to disambiguate on (unit_level, unit_code).
  out <- filter_unit_data("justice_and_rule_of_law", "static", "AFE")
  expect_setequal(unique(out$unit_level), c("country", "region"))
})

test_that("leaf accepts a vector - several leaves in one call", {
  leaves <- c("justice_and_rule_of_law", "degree_of_integrity")
  out <- filter_unit_data(leaves, "static", c("KEN", "GHA"))
  expect_setequal(unique(out$leaf), leaves)
  expect_equal(
    nrow(out),
    nrow(filter_unit_data("justice_and_rule_of_law", "static", c("KEN", "GHA"))) +
      nrow(filter_unit_data("degree_of_integrity", "static", c("KEN", "GHA")))
  )
})

test_that("leaf = NULL returns every leaf present in cgjr_ctf", {
  out <- filter_unit_data(NULL, "static", "KEN")
  ref <- dplyr::filter(cgjrdata::cgjr_ctf, ctf_type == "static", unit_code == "KEN")
  expect_identical(nrow(out), nrow(ref))
  expect_setequal(
    unique(out$leaf),
    unique(dplyr::filter(cgjrdata::cgjr_ctf, ctf_type == "static")$leaf)
  )
})

test_that("leaf must be NULL or character, never NA", {
  expect_error(filter_unit_data(NA_character_, "static", "KEN"), "NULL or a character")
  expect_error(filter_unit_data(1L, "static", "KEN"), "NULL or a character")
})
