# tests/testthat/test-utils_plotting.R

# ── Fixtures ──────────────────────────────────────────────────────────────────

# Mimics combined output of filter_country_data() + get_aggregate_data()
make_plot_data <- function(years = 2015:2020) {
  n <- length(years)
  tibble::tibble(
    country_code  = c(rep("KEN", n), rep("GHA", n), rep(NA_character_, n), rep(NA_character_, n)),
    country_name  = c(rep("Kenya", n), rep("Ghana", n), rep(NA_character_, n), rep(NA_character_, n)),
    group_label   = c(rep(NA_character_, n), rep(NA_character_, n), rep("East Asia & Pacific", n), rep("High income", n)),
    group_code    = c(rep(NA_character_, n), rep(NA_character_, n), rep("EAP", n), rep("High income", n)),
    year          = rep(years, times = 4),
    score         = runif(n * 4, 0.2, 0.8),
    country_type  = c(rep("primary", n), rep("peer", n), rep("region", n), rep("income", n))
  )
}

make_primary_only <- function(years = 2015:2020) {
  n <- length(years)
  tibble::tibble(
    country_code = rep("KEN", n),
    country_name = rep("Kenya", n),
    group_label  = rep(NA_character_, n),
    group_code   = rep(NA_character_, n),
    year         = years,
    score        = runif(n, 0.3, 0.7),
    country_type = rep("primary", n)
  )
}

YEAR_RANGE <- c(2015L, 2020L)

# ── get_capacity_zones() ──────────────────────────────────────────────────────

test_that("get_capacity_zones returns a list of length 3", {
  result <- get_capacity_zones()
  expect_type(result, "list")
  expect_length(result, 3L)
})

test_that("get_capacity_zones elements are ggplot layers", {
  result <- get_capacity_zones()
  purrr::walk(result, \(layer) expect_true(inherits(layer, "LayerInstance")))
})

# ── plot_cgjr_master() — return type ─────────────────────────────────────────

test_that("plot_cgjr_master returns a ggplot object", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cgjr_master returns ggplot not plotly", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  expect_false(inherits(p, "plotly"))
})

# ── plot_cgjr_master() — input validation ────────────────────────────────────

test_that("plot_cgjr_master errors when y_var not in data", {
  expect_error(
    plot_cgjr_master(make_plot_data(), "nonexistent", "KEN", YEAR_RANGE),
    regexp = "y_var"
  )
})

# ── plot_cgjr_master() — empty data ──────────────────────────────────────────

test_that("plot_cgjr_master returns ggplot for empty data", {
  empty <- make_plot_data()[0, ]
  p <- plot_cgjr_master(empty, "score", "KEN", YEAR_RANGE)
  expect_s3_class(p, "ggplot")
})

# ── plot_cgjr_master() — layer checks ────────────────────────────────────────

test_that("plot contains GeomPoint layer", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  layer_classes <- purrr::map_chr(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("plot contains GeomLine layer", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  layer_classes <- purrr::map_chr(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
})

test_that("plot contains GeomRect zone layer (CLIAR-style vertical bands)", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  layer_classes <- purrr::map_chr(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomRect" %in% layer_classes)
  expect_equal(sum(layer_classes == "GeomRect"), 1L)
})

test_that("fill scale always contains all three zone levels (drop = FALSE)", {
  withr::with_options(list(), {
    thematic::thematic_off()
    # Use primary-only data so no peer points are present — all 3 zones must
    # still appear in the legend
    p     <- plot_cgjr_master(make_primary_only(), "score", "KEN", YEAR_RANGE)
    built <- ggplot2::ggplot_build(p)
    fill_scale <- p$scales$get_scales("fill")
    expect_false(isTRUE(fill_scale$drop))
    expect_equal(length(fill_scale$limits), 3L)
  })
})

test_that("x-axis is continuous (numeric years)", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  expect_true(inherits(p$scales$get_scales("x"), "ScaleContinuousPosition"))
})

# ── plot_cgjr_master() — y-axis scale ────────────────────────────────────────

test_that("y-axis is fixed to 0-1", {
  withr::with_options(list(), {
    thematic::thematic_off()
    p     <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
    built <- ggplot2::ggplot_build(p)
    y_lim <- built$layout$panel_params[[1]]$y.range
    expect_equal(y_lim[[1]], 0, tolerance = 0.01)
    expect_equal(y_lim[[2]], 1, tolerance = 0.01)
  })
})

# ── plot_cgjr_master() — robustness ──────────────────────────────────────────

test_that("does not error with only primary country", {
  expect_no_error(
    plot_cgjr_master(make_primary_only(), "score", "KEN", YEAR_RANGE)
  )
})

test_that("does not error when data contains only aggregate rows", {
  agg_only <- make_plot_data() |>
    dplyr::filter(country_type %in% c("region", "income"))
  expect_no_error(
    plot_cgjr_master(agg_only, "score", "KEN", YEAR_RANGE)
  )
})

# ── plot_cgjr_master() — CLIAR-style evolution behaviour ─────────────────────

test_that("CAPACITY_ZONES has 3 rows with correct zone labels", {
  expect_equal(nrow(CAPACITY_ZONES), 3L)
  expect_equal(CAPACITY_ZONES$label, c("Weak", "Emerging", "Strong"))
})

test_that("CAPACITY_ZONES cutoffs are at 0.25 and 0.50", {
  expect_equal(CAPACITY_ZONES$ymax[[1]], 0.25)
  expect_equal(CAPACITY_ZONES$ymax[[2]], 0.50)
  expect_equal(CAPACITY_ZONES$ymax[[3]], 1.00)
})

test_that("CAPACITY_ZONES fill colours match CLIAR palette", {
  expect_equal(CAPACITY_ZONES$fill, c("#e47a81", "#ffd966", "#8ec18e"))
})

# ── .get_zone_params() ────────────────────────────────────────────────────────

test_that(".get_zone_params('abs_quartile') returns correct cutoffs", {
  zp <- cgjrapp:::.get_zone_params("abs_quartile")
  expect_equal(zp$q1, 0.25)
  expect_equal(zp$q2, 0.50)
  expect_equal(zp$breaks, c(0, 0.25, 0.50, 1))
})

test_that(".get_zone_params('abs_tercile') returns correct cutoffs", {
  zp <- cgjrapp:::.get_zone_params("abs_tercile")
  expect_equal(zp$q1, 1/3)
  expect_equal(zp$q2, 2/3)
  expect_equal(zp$breaks, c(0, 1/3, 2/3, 1))
})

test_that(".get_zone_params labels are named weak/emerging/strong", {
  for (tm in c("abs_quartile", "abs_tercile")) {
    zp <- cgjrapp:::.get_zone_params(tm)
    expect_named(zp$labels, c("weak", "emerging", "strong"))
  }
})

test_that(".get_zone_params fill_values names match labels", {
  for (tm in c("abs_quartile", "abs_tercile")) {
    zp <- cgjrapp:::.get_zone_params(tm)
    expect_equal(names(zp$fill_values), unname(zp$labels))
  }
})

test_that(".get_zone_params errors on unknown threshold_mode", {
  expect_error(cgjrapp:::.get_zone_params("unknown"))
})

# ── .get_rel_zone_params() ────────────────────────────────────────────────────

test_that(".get_rel_zone_params returns labels and fill_values", {
  for (tm in c("rel_quartile", "rel_tercile")) {
    zp <- cgjrapp:::.get_rel_zone_params(tm)
    expect_named(zp$labels, c("weak", "emerging", "strong"))
    expect_length(zp$fill_values, 3L)
  }
})

test_that(".get_rel_zone_params errors on unknown mode", {
  expect_error(cgjrapp:::.get_rel_zone_params("abs_quartile"))
})

test_that("primary-only plot has shape-21 point (status-filled circle)", {
  set.seed(1)
  d <- make_primary_only() |> dplyr::mutate(score = 0.7)
  p <- plot_cgjr_master(d, "score", "KEN", YEAR_RANGE)
  point_layers <- Filter(\(l) inherits(l$geom, "GeomPoint"), p$layers)
  shapes <- purrr::map_int(point_layers, \(l) {
    s <- l$aes_params$shape %||% NA_integer_
    as.integer(s)
  })
  expect_true(21L %in% shapes)
})

test_that("plot has exactly 1 GeomRect zone layer", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  layer_classes <- purrr::map_chr(p$layers, \(l) class(l$geom)[1])
  expect_equal(sum(layer_classes == "GeomRect"), 1L)
})

test_that("plot with benchmarks contains GeomPoint and GeomLine", {
  p <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE)
  layer_classes <- purrr::map_chr(p$layers, \(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})

# ── threshold_mode argument ───────────────────────────────────────────────────

test_that("plot_cgjr_master accepts threshold_mode = 'abs_quartile'", {
  expect_no_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "abs_quartile")
  )
})

test_that("plot_cgjr_master accepts threshold_mode = 'abs_tercile'", {
  expect_no_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "abs_tercile")
  )
})

test_that("plot_cgjr_master errors on invalid threshold_mode", {
  expect_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "bogus")
  )
})

test_that("plot_cgjr_master errors for rel mode when thresholds is NULL", {
  expect_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "rel_quartile",
                     thresholds = NULL),
    regexp = "thresholds"
  )
})

test_that("plot_cgjr_master accepts rel_quartile with valid thresholds tibble", {
  thr <- tibble::tibble(year = 2015:2020, q1 = 0.25, q2 = 0.50)
  expect_no_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "rel_quartile",
                     thresholds = thr)
  )
})

test_that("plot_cgjr_master accepts rel_tercile with valid thresholds tibble", {
  thr <- tibble::tibble(year = 2015:2020, q1 = 1/3, q2 = 2/3)
  expect_no_error(
    plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                     threshold_mode = "rel_tercile",
                     thresholds = thr)
  )
})

test_that("abs_tercile y-axis breaks are at 0, 1/3, 2/3, 1", {
  withr::with_options(list(), {
    thematic::thematic_off()
    p     <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                              threshold_mode = "abs_tercile")
    built <- ggplot2::ggplot_build(p)
    breaks <- built$layout$panel_params[[1]]$y$breaks
    expect_equal(breaks, c(0, 1/3, 2/3, 1), tolerance = 0.001)
  })
})

test_that("abs_quartile y-axis breaks are at 0, 0.25, 0.50, 1", {
  withr::with_options(list(), {
    thematic::thematic_off()
    p     <- plot_cgjr_master(make_plot_data(), "score", "KEN", YEAR_RANGE,
                              threshold_mode = "abs_quartile")
    built <- ggplot2::ggplot_build(p)
    breaks <- built$layout$panel_params[[1]]$y$breaks
    expect_equal(breaks, c(0, 0.25, 0.50, 1), tolerance = 0.001)
  })
})

# ── plot_cgjr_master() — works on real cgjrdata ──────────────────────────────

test_that("plot_cgjr_master works end-to-end with real cgjrdata", {
  country_data <- filter_country_data(
    cgjrdata::ctfdata_list$institutional_environment$degree_of_integrity,
    primary_iso = "KEN",
    peer_isos   = c("GHA", "NGA"),
    year_range  = c(2015L, 2024L)
  )
  agg_data <- get_aggregate_data(
    cluster       = "institutional_environment",
    subcluster    = "degree_of_integrity",
    region_codes  = c("EAP"),
    income_groups = c("High income"),
    year_range    = c(2015L, 2024L)
  )
  agg_data <- agg_data |>
    dplyr::mutate(country_code = group_code, country_name = group_label)

  combined <- dplyr::bind_rows(country_data, agg_data)
  expect_no_error(
    plot_cgjr_master(combined, "score", "KEN", c(2015L, 2024L))
  )
})
