# Slice 5 - Data Download table builders.

BASE  <- "GHA"
COMPS <- c("NGA", "KEN", "SEN", "CIV", "BEN")

test_that("export_ctf_table: long, taxonomy-named, one row per unit x indicator", {
  d <- export_ctf_table(BASE, COMPS, "static")
  expect_setequal(unique(d$Unit),
                  cgjrdata::cgjr_ctf$unit_name[match(c(BASE, COMPS),
                                                     cgjrdata::cgjr_ctf$unit_code)])
  expect_true(all(c("Cluster", "Subcluster", "Indicator", "Variable", "CTF") %in% names(d)))
  expect_true(all(is.na(d$Year)))                 # static
  # values are a pass-through of cgjr_ctf, not recomputed
  one <- dplyr::filter(cgjrdata::cgjr_ctf, unit_code == "GHA", ctf_type == "static",
                       variable == "wjp_rol_2_2")$ctf
  expect_equal(d$CTF[d$Unit == "Ghana" & d$Variable == "wjp_rol_2_2"],
               round(one, 4))
})

test_that("export_ctf_table dynamic view is even years only", {
  d <- export_ctf_table(BASE, COMPS, "dynamic", year_range = c(2013L, 2024L))
  expect_setequal(unique(d$Year), c(2014L, 2016L, 2018L, 2020L, 2022L, 2024L))
})

test_that("export_raw_table is country-grain and ignores non-country comparators", {
  d <- export_raw_table(BASE, c("EAP", "high_income"), year_range = c(2015L, 2020L))
  expect_setequal(unique(d$Country), "Ghana")     # regions/income dropped
  expect_true(all(d$Year >= 2015L & d$Year <= 2020L))
  expect_true(all(c("Cluster", "Indicator", "Value") %in% names(d)))
})

test_that("export_indicator_catalogue is the full crosswalk, display-tidied", {
  d <- export_indicator_catalogue()
  expect_equal(nrow(d), nrow(cgjrdata::cgjr_crosswalk))
  expect_named(d, c("Cluster", "Subcluster", "Sub-subcluster", "Indicator",
                    "Source", "Variable", "Description"))
  cw <- cgjrdata::cgjr_crosswalk
  expect_identical(d$Description[!is.na(d$Description)][1],
                   cw$description_short[!is.na(cw$description_short)][1])
})
