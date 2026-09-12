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

test_that("export_live_scores has one row per taxonomy leaf, empty leaves scaffolded", {
  d <- export_live_scores(BASE, COMPS, "static")
  tx <- cgjrdata::cgjr_taxonomy
  leaf_names <- dplyr::coalesce(tx$sub_subcluster_name, tx$subcluster_name)
  expect_setequal(d$Leaf, leaf_names)
  # domestic_revenue_mobilization has no indicators -> NA composite, 0 used
  drm <- d[d$Leaf == "Domestic Revenue Mobilization", ]
  expect_true(is.na(drm$Composite))
  expect_identical(drm$`Indicators used`, 0L)
  # a populated leaf matches a direct compute_family_average_app call
  slice <- filter_unit_data("justice_and_rule_of_law", "static", c(BASE, COMPS))
  fam <- compute_family_average_app(slice, BASE, COMPS, type = "static")
  expect_equal(d$Composite[d$Leaf == "Justice and Rule of Law"],
               round(fam$score[fam$unit_code == BASE], 4))
})

test_that("export_reference_scores is cgjr_scores filtered, never the live numbers", {
  d <- export_reference_scores(BASE, COMPS, "static")
  expect_true(all(c("Node level", "Node", "Score") %in% names(d)))
  expect_setequal(unique(d$`Node level`),
                  c("overall", "cluster", "subcluster", "sub_subcluster"))
  # it is a slice of cgjr_scores, not a recomputation
  ref_overall <- cgjrdata::cgjr_scores |>
    dplyr::filter(unit_code == "GHA", ctf_type == "static", node_level == "overall")
  expect_equal(d$Score[d$Unit == "Ghana" & d$`Node level` == "overall"],
               round(ref_overall$score, 4))
})

test_that("export_indicator_catalogue is the full crosswalk, display-tidied", {
  d <- export_indicator_catalogue()
  expect_equal(nrow(d), nrow(cgjrdata::cgjr_crosswalk))
  expect_true(all(c("Cluster", "Indicator", "Source", "Variable",
                    "Dynamic-eligible", "Static-eligible", "Status") %in% names(d)))
})
