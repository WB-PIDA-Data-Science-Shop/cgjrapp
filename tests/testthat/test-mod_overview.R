# test-mod_overview.R
# Tests for mod_overview_ui() and mod_overview_server()
# Uses shiny::testServer() for server-side reactive logic.

# ── Helpers ──────────────────────────────────────────────────────────────────

# Minimal sidebar for UI tests
test_sidebar <- bslib::sidebar("Test sidebar")

# Minimal reactive wrappers that mimic what run_cgjrapp() passes to the module
make_reactives <- function(
    primary_iso   = "GHA",
    peer_isos     = character(0),
    region_codes  = character(0),
    income_groups = character(0),
    year_range    = c(2013L, 2024L),
    threshold_mode = "abs_quartile"
) {
  list(
    primary_iso    = shiny::reactive(primary_iso),
    peer_isos      = shiny::reactive(peer_isos),
    region_codes   = shiny::reactive(region_codes),
    income_groups  = shiny::reactive(income_groups),
    year_range     = shiny::reactive(year_range),
    threshold_mode = shiny::reactive(threshold_mode)
  )
}

# ── UI structure tests ────────────────────────────────────────────────────────

test_that("mod_overview_ui returns a shiny.tag object", {
  ui <- mod_overview_ui("test", sidebar = test_sidebar)
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_overview_ui includes a nav_panel with title 'Overview'", {
  ui <- mod_overview_ui("test", sidebar = test_sidebar)
  expect_equal(ui$attribs$`data-value`, "overview")
})

test_that("mod_overview_ui does NOT contain an overall score plot", {
  ui   <- mod_overview_ui("test", sidebar = test_sidebar)
  html <- as.character(ui)
  expect_false(grepl("plot_overall", html))
})

test_that("mod_overview_ui contains plotlyOutput for all 4 cluster scores", {
  ui  <- mod_overview_ui("test", sidebar = test_sidebar)
  html <- as.character(ui)
  for (var in OVERVIEW_CLUSTER_VARS) {
    expect_true(
      grepl(var, html),
      label = paste0("plotlyOutput for '", var, "' not found in UI")
    )
  }
})

test_that("OVERVIEW_CLUSTER_VARS has exactly 4 entries", {
  expect_length(OVERVIEW_CLUSTER_VARS, 4L)
})

test_that("OVERVIEW_CLUSTER_VARS values match columns in institutional_averages_tbl", {
  tbl_cols <- names(cgjrdata::institutional_averages_tbl)
  for (var in OVERVIEW_CLUSTER_VARS) {
    expect_true(
      var %in% tbl_cols,
      label = paste0("'", var, "' not a column in institutional_averages_tbl")
    )
  }
})

test_that("mod_overview_ui does NOT contain threshold_mode input (it lives in sidebar)", {
  ui   <- mod_overview_ui("test", sidebar = test_sidebar)
  html <- as.character(ui)
  expect_false(grepl("threshold_mode", html))
})

# ── Server reactive tests ─────────────────────────────────────────────────────

test_that("mod_overview_server runs without error for a valid country", {
  r <- make_reactives(primary_iso = "GHA")
  result <- tryCatch(
    shiny::testServer(
      mod_overview_server,
      args = r,
      expr = {
        # Just trigger the reactive — no error expected
        data <- overview_data()
        expect_s3_class(data, "data.frame")
      }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("mod_overview_server errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("overview_data() contains country_type column", {
  r <- make_reactives(primary_iso = "GHA")
  shiny::testServer(
    mod_overview_server,
    args = r,
    expr = {
      data <- overview_data()
      expect_true("country_type" %in% names(data))
    }
  )
})

test_that("overview_data() marks focal country as 'primary'", {
  r <- make_reactives(primary_iso = "GHA", peer_isos = character(0))
  shiny::testServer(
    mod_overview_server,
    args = r,
    expr = {
      data <- overview_data()
      primary_rows <- data[data$country_type == "primary", ]
      expect_true(nrow(primary_rows) > 0L)
    }
  )
})

test_that("overview_data() includes peer rows when peers provided", {
  r <- make_reactives(
    primary_iso = "GHA",
    peer_isos   = c("KEN", "SEN")
  )
  shiny::testServer(
    mod_overview_server,
    args = r,
    expr = {
      data <- overview_data()
      peer_rows <- data[data$country_type == "peer", ]
      expect_true(nrow(peer_rows) > 0L)
    }
  )
})

test_that("overview_data() respects year_range filter", {
  r <- make_reactives(
    primary_iso = "GHA",
    year_range  = c(2018L, 2020L)
  )
  shiny::testServer(
    mod_overview_server,
    args = r,
    expr = {
      data <- overview_data()
      expect_true(all(data$year >= 2018L))
      expect_true(all(data$year <= 2020L))
    }
  )
})

test_that("overview_data() returns no rows when year_range excludes all data", {
  r <- make_reactives(
    primary_iso = "GHA",
    year_range  = c(2099L, 2099L)
  )
  shiny::testServer(
    mod_overview_server,
    args = r,
    expr = {
      data <- overview_data()
      expect_equal(nrow(data), 0L)
    }
  )
})

test_that("mod_overview_server works with region benchmarks", {
  r <- make_reactives(
    primary_iso  = "GHA",
    region_codes = c("AFW", "SSA")
  )
  result <- tryCatch(
    shiny::testServer(
      mod_overview_server,
      args = r,
      expr = {
        data <- overview_data()
        expect_s3_class(data, "data.frame")
      }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("region benchmarks errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})

test_that("mod_overview_server works with income group benchmarks", {
  r <- make_reactives(
    primary_iso   = "GHA",
    income_groups = c("Lower middle income")
  )
  result <- tryCatch(
    shiny::testServer(
      mod_overview_server,
      args = r,
      expr = {
        data <- overview_data()
        expect_s3_class(data, "data.frame")
      }
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    testthat::fail(paste("income benchmarks errored:", conditionMessage(result)))
  }
  expect_true(TRUE)
})
