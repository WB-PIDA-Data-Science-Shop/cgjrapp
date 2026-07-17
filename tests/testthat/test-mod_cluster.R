# test-mod_cluster.R
# Tests for mod_cluster_ui() and mod_cluster_server()

# ── Helpers ───────────────────────────────────────────────────────────────────

test_sidebar <- bslib::sidebar("Test sidebar")

# Minimal reactive wrappers matching what run_cgjrapp() passes
make_cluster_reactives <- function(
    primary_iso   = "GHA",
    peer_isos     = character(0),
    region_codes  = character(0),
    income_groups = character(0),
    year_range    = c(2013L, 2024L),
    zone_type     = "abs_default"
) {
  list(
    primary_iso   = shiny::reactive(primary_iso),
    peer_isos     = shiny::reactive(peer_isos),
    region_codes  = shiny::reactive(region_codes),
    income_groups = shiny::reactive(income_groups),
    year_range    = shiny::reactive(year_range),
    zone_type     = shiny::reactive(zone_type)
  )
}

# Pick a single stable cluster + subcluster for detailed tests
CLUSTER_KEY    <- "institutional_environment"
SUBCLUSTER_KEY <- "degree_of_integrity"

# ── CLUSTER_DISPLAY_NAMES ─────────────────────────────────────────────────────

test_that("CLUSTER_DISPLAY_NAMES covers all clusters in ctfdata_list", {
  for (ck in names(cgjrdata::ctfdata_list)) {
    expect_true(
      ck %in% names(CLUSTER_DISPLAY_NAMES),
      label = paste0("'", ck, "' not in CLUSTER_DISPLAY_NAMES")
    )
  }
})

test_that("CLUSTER_DISPLAY_NAMES values are non-empty strings", {
  purrr::walk(CLUSTER_DISPLAY_NAMES, function(nm) {
    expect_true(nchar(nm) > 0L)
  })
})

# ── SUBCLUSTER_DISPLAY_NAMES ──────────────────────────────────────────────────

test_that("SUBCLUSTER_DISPLAY_NAMES covers all subclusters in ctfdata_list", {
  all_sub_keys <- unlist(purrr::map(cgjrdata::ctfdata_list, names))
  for (sk in all_sub_keys) {
    expect_true(
      sk %in% names(SUBCLUSTER_DISPLAY_NAMES),
      label = paste0("'", sk, "' not in SUBCLUSTER_DISPLAY_NAMES")
    )
  }
})

test_that("SUBCLUSTER_DISPLAY_NAMES values are non-empty strings", {
  purrr::walk(SUBCLUSTER_DISPLAY_NAMES, function(nm) {
    expect_true(nchar(nm) > 0L)
  })
})

# ── .cluster_label() ──────────────────────────────────────────────────────────

test_that(".cluster_label() returns display name for known key", {
  label <- .cluster_label("institutional_environment")
  expect_equal(label, "Institutional Environment")
})

test_that(".cluster_label() falls back to key for unknown key", {
  label <- .cluster_label("unknown_cluster_xyz")
  expect_equal(label, "unknown_cluster_xyz")
})

# ── .subcluster_label() ───────────────────────────────────────────────────────

test_that(".subcluster_label() returns display name for known key", {
  label <- .subcluster_label("degree_of_integrity")
  expect_equal(label, "Degree of Integrity")
})

test_that(".subcluster_label() falls back to key for unknown key", {
  label <- .subcluster_label("some_unknown_sub")
  expect_equal(label, "some_unknown_sub")
})

# ── .build_indicator_choices() ────────────────────────────────────────────────

test_that(".build_indicator_choices() first choice is 'score'", {
  sub_tbl <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  choices <- .build_indicator_choices(sub_tbl, SUBCLUSTER_KEY)
  expect_equal(choices[[1L]], "score")
})

test_that(".build_indicator_choices() includes indicator columns beyond score", {
  sub_tbl <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  choices <- .build_indicator_choices(sub_tbl, SUBCLUSTER_KEY)
  expect_true(length(choices) > 1L)
})

test_that(".build_indicator_choices() values match get_indicator_choices() + 'score'", {
  sub_tbl   <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  ind_cols  <- get_indicator_choices(sub_tbl)
  built     <- .build_indicator_choices(sub_tbl, SUBCLUSTER_KEY)
  expected  <- c("score", unname(ind_cols))
  expect_equal(unname(built), expected)
})

test_that(".build_indicator_choices() names are non-empty", {
  sub_tbl <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  choices <- .build_indicator_choices(sub_tbl, SUBCLUSTER_KEY)
  expect_true(all(nchar(names(choices)) > 0L))
})

# ── mod_cluster_ui() — basic structure ───────────────────────────────────────

test_that("mod_cluster_ui() returns a shiny.tag object", {
  ui <- mod_cluster_ui(CLUSTER_KEY, cluster_key = CLUSTER_KEY, sidebar = test_sidebar)
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_cluster_ui() data-value equals cluster_key", {
  ui <- mod_cluster_ui(CLUSTER_KEY, cluster_key = CLUSTER_KEY, sidebar = test_sidebar)
  expect_equal(ui$attribs$`data-value`, CLUSTER_KEY)
})

test_that("mod_cluster_ui() contains a nav_panel per subcluster", {
  ui   <- mod_cluster_ui(CLUSTER_KEY, cluster_key = CLUSTER_KEY, sidebar = test_sidebar)
  html <- as.character(ui)
  sub_keys <- names(cgjrdata::ctfdata_list[[CLUSTER_KEY]])
  for (sk in sub_keys) {
    # Each subcluster plot output ID is embedded in the HTML
    expect_true(
      grepl(paste0("plot_", CLUSTER_KEY, "_", sk), html),
      label = paste0("plotlyOutput for subcluster '", sk, "' not found")
    )
  }
})

test_that("mod_cluster_ui() contains a selectInput per subcluster", {
  ui   <- mod_cluster_ui(CLUSTER_KEY, cluster_key = CLUSTER_KEY, sidebar = test_sidebar)
  html <- as.character(ui)
  sub_keys <- names(cgjrdata::ctfdata_list[[CLUSTER_KEY]])
  for (sk in sub_keys) {
    expect_true(
      grepl(paste0("ind_", CLUSTER_KEY, "_", sk), html),
      label = paste0("selectInput for subcluster '", sk, "' not found")
    )
  }
})

test_that("mod_cluster_ui() renders for all 4 clusters without error", {
  for (ck in names(cgjrdata::ctfdata_list)) {
    expect_no_error(
      mod_cluster_ui(ck, cluster_key = ck, sidebar = test_sidebar)
    )
  }
})

# ── mod_cluster_server() — basic smoke tests ──────────────────────────────────

test_that("mod_cluster_server runs without error for abs_default", {
  r <- make_cluster_reactives(primary_iso = "GHA", zone_type = "abs_default")
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server runs without error for abs_tercile", {
  r <- make_cluster_reactives(primary_iso = "GHA", zone_type = "abs_tercile")
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server runs without error for rel_quartile", {
  r <- make_cluster_reactives(primary_iso = "GHA", zone_type = "rel_quartile")
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server runs without error for rel_tercile", {
  r <- make_cluster_reactives(primary_iso = "GHA", zone_type = "rel_tercile")
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server handles peers correctly", {
  r <- make_cluster_reactives(
    primary_iso = "GHA",
    peer_isos   = c("KEN", "SEN")
  )
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server with peers errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server handles region benchmarks", {
  r <- make_cluster_reactives(
    primary_iso  = "GHA",
    region_codes = c("AFW", "SSA")
  )
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server with regions errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server handles income group benchmarks", {
  r <- make_cluster_reactives(
    primary_iso   = "GHA",
    income_groups = c("Lower middle income")
  )
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server with income errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server handles narrow year_range", {
  r <- make_cluster_reactives(
    primary_iso = "GHA",
    year_range  = c(2018L, 2020L)
  )
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server with narrow year range errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server handles year_range that yields no data", {
  r <- make_cluster_reactives(
    primary_iso = "GHA",
    year_range  = c(2099L, 2099L)
  )
  result <- tryCatch(
    shiny::testServer(
      mod_cluster_server,
      args = c(list(cluster_key = CLUSTER_KEY), r),
      expr = { expect_true(TRUE) }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_cluster_server empty year range errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_cluster_server runs for all 4 clusters", {
  for (ck in names(cgjrdata::ctfdata_list)) {
    r <- make_cluster_reactives(primary_iso = "GHA")
    result <- tryCatch(
      shiny::testServer(
        mod_cluster_server,
        args = c(list(cluster_key = ck), r),
        expr = { expect_true(TRUE) }
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      testthat::fail(paste("mod_cluster_server failed for cluster:", ck,
                           "—", conditionMessage(result)))
    }
  }
  expect_true(TRUE)
})

# ── Integration: plot_cgjr_master via subcluster data ─────────────────────────

test_that("plot_cgjr_master works with subcluster score data", {
  sub_tbl <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  country_data <- filter_country_data(
    data        = sub_tbl,
    primary_iso = "GHA",
    peer_isos   = character(0),
    year_range  = c(2013L, 2024L)
  )
  p <- plot_cgjr_master(
    data           = country_data,
    y_var          = "score",
    primary_iso    = "GHA",
    year_range     = c(2013L, 2024L),
    threshold_mode = "abs_default"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_cgjr_master works with subcluster indicator data", {
  sub_tbl <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  ind_col <- get_indicator_choices(sub_tbl)[[1L]]
  country_data <- filter_country_data(
    data        = sub_tbl,
    primary_iso = "GHA",
    peer_isos   = character(0),
    year_range  = c(2013L, 2024L)
  )
  p <- plot_cgjr_master(
    data           = country_data,
    y_var          = ind_col,
    primary_iso    = "GHA",
    year_range     = c(2013L, 2024L),
    threshold_mode = "abs_default"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_cgjr_master works with rel_tercile thresholds for subcluster score", {
  sub_tbl    <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  year_range <- c(2013L, 2024L)
  thresholds <- get_annual_thresholds(sub_tbl, "score", year_range, probs = c(1/3, 2/3))
  country_data <- filter_country_data(
    data        = sub_tbl,
    primary_iso = "GHA",
    peer_isos   = character(0),
    year_range  = year_range
  )
  p <- plot_cgjr_master(
    data           = country_data,
    y_var          = "score",
    primary_iso    = "GHA",
    year_range     = year_range,
    threshold_mode = "rel_tercile",
    thresholds     = thresholds
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_cgjr_master works with rel_quartile thresholds for indicator column", {
  sub_tbl    <- cgjrdata::ctfdata_list[[CLUSTER_KEY]][[SUBCLUSTER_KEY]]
  ind_col    <- get_indicator_choices(sub_tbl)[[1L]]
  year_range <- c(2013L, 2024L)
  thresholds <- get_annual_thresholds(sub_tbl, ind_col, year_range, probs = c(0.25, 0.50))
  country_data <- filter_country_data(
    data        = sub_tbl,
    primary_iso = "GHA",
    peer_isos   = character(0),
    year_range  = year_range
  )
  p <- plot_cgjr_master(
    data           = country_data,
    y_var          = ind_col,
    primary_iso    = "GHA",
    year_range     = year_range,
    threshold_mode = "rel_quartile",
    thresholds     = thresholds
  )
  expect_s3_class(p, "ggplot")
})

# ── run_cgjrapp.R integration: cluster modules appear in UI ───────────────────

test_that("run_cgjrapp UI contains cluster tab values for all clusters", {
  # Build the full UI to make sure !!!purrr::map() works
  # We only check that no error is thrown and key cluster values are present
  result <- tryCatch(
    {
      ui_html <- as.character(bslib::page_navbar(
        !!!purrr::map(
          names(cgjrdata::ctfdata_list),
          function(ck) mod_cluster_ui(ck, cluster_key = ck, sidebar = test_sidebar)
        )
      ))
      for (ck in names(cgjrdata::ctfdata_list)) {
        expect_true(
          grepl(ck, ui_html),
          label = paste0("cluster key '", ck, "' not found in assembled UI HTML")
        )
      }
    },
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("cluster UI assembly errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})
