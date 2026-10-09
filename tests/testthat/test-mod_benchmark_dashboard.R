taxo_one <- dplyr::filter(
  cgjrdata::cgjr_taxonomy,
  dplyr::coalesce(sub_subcluster, subcluster) == "justice_and_rule_of_law"
)

COMPS <- cgjrdata::cgjr_ctf |>
  dplyr::filter(.data$unit_level == "country", .data$unit_code != "GHA") |>
  dplyr::distinct(.data$unit_code) |>
  dplyr::arrange(.data$unit_code) |>
  dplyr::pull(.data$unit_code) |>
  head(30)

test_that("mod_benchmark_dashboard_ui builds an accordion for the given taxonomy", {
  ui <- mod_benchmark_dashboard_ui("bench", taxonomy = taxo_one)
  expect_s3_class(ui, "shiny.tag")
  html <- as.character(ui)
  expect_match(html, "Justice and Rule of Law", ignore.case = TRUE)
  expect_match(html, "bench-plot_justice_and_rule_of_law")
})

test_that("leaf card has a 'Show notes' switch, no composite-score value_box", {
  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = taxo_one))
  expect_match(html, 'id="bench-show_notes_justice_and_rule_of_law"', fixed = TRUE)
  expect_match(html, "Show notes", fixed = TRUE)
  expect_match(html, "material-switch", fixed = TRUE)   # cliarappak's switch
  # the plot sits in the fixed-height scroll window
  expect_match(html, "cgjr-plot-window", fixed = TRUE)
  expect_match(html, "height:600px", fixed = TRUE)
  expect_no_match(html, "bslib-value-box")
  expect_no_match(html, "Composite score")
})

test_that("server renders a real static leaf card: plot + note", {
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = taxo_one,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")

      # renderPlotly() serialises to a JSON string under testServer
      p <- output$`plot_justice_and_rule_of_law`
      expect_type(p, "character")
      expect_gt(nchar(p), 100L)
      expect_match(p, "plotly")

      note <- paste(as.character(output$note_justice_and_rule_of_law), collapse = " ")
      expect_match(note, "indicators?;.*comparators?;.*Default thresholds")
    }
  )
})

test_that("Show notes panel: static view lists the units and any excluded indicators", {
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = taxo_one,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static",
                        show_notes_justice_and_rule_of_law = TRUE)

      notes_html <- paste(as.character(output$plot_notes_justice_and_rule_of_law),
                          collapse = " ")
      expect_match(notes_html, "Ghana compared to", fixed = TRUE)

      # matches a direct call to excluded_indicators() for the same selection
      slice <- filter_unit_data("justice_and_rule_of_law", "static",
                                unique(c("GHA", COMPS)))
      expected <- excluded_indicators(slice, "GHA", COMPS)
      if (length(expected) > 0L) {
        expect_match(notes_html, "not considered", fixed = TRUE)
        for (nm in expected) expect_match(notes_html, nm, fixed = TRUE)
      } else {
        expect_no_match(notes_html, "not considered")
      }
    }
  )
})

test_that("Show notes panel is empty in the dynamic view (cliarappak parity)", {
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = taxo_one,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "dynamic")
      expect_null(output$plot_notes_justice_and_rule_of_law)
    }
  )
})

test_that("server switches to the dynamic view without error", {
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = taxo_one,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Terciles"),
      year_range       = shiny::reactive(c(2015L, 2022L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "dynamic")
      expect_type(output$`plot_justice_and_rule_of_law`, "character")
    }
  )
})

test_that("an empty (leaf, ctf_type) card renders the no-data message, not an error", {
  taxo_pfm <- dplyr::filter(
    cgjrdata::cgjr_taxonomy,
    dplyr::coalesce(sub_subcluster, subcluster) == "budget_cycle_and_fiscal_planning"
  )
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = taxo_pfm,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_budget_cycle_and_fiscal_planning = "dynamic")
      expect_error(output$`plot_budget_cycle_and_fiscal_planning`, "No data available")
    }
  )
})

# --- Slice 1: the renderer walks the whole taxonomy ---

test_that("UI renders one leaf card per taxonomy row for the full cgjr_taxonomy", {
  tx <- cgjrdata::cgjr_taxonomy
  keys <- dplyr::coalesce(tx$sub_subcluster, tx$subcluster)
  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))
  for (k in keys) expect_match(html, paste0("bench-plot_", k), fixed = TRUE)
  expect_equal(length(keys), 14L)   # 10 two-level leaves + 4 PFM sub-subclusters
})

# --- Slice 2: the nested cluster -> subcluster -> sub_subcluster accordion ----

test_that("accordion nests: cluster panels contain a subcluster accordion", {
  tx <- cgjrdata::cgjr_taxonomy
  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))

  # one inner accordion per cluster, ordered by cluster_num
  clusters <- tx[order(tx$cluster_num), ]
  clusters <- clusters$cluster[!duplicated(clusters$cluster)]
  for (cl in clusters) expect_match(html, paste0("bench-acc_", cl), fixed = TRUE)

  # every cluster / subcluster / sub_subcluster *name* is shown as a title
  for (nm in unique(c(tx$cluster_name, tx$subcluster_name,
                      stats::na.omit(tx$sub_subcluster_name)))) {
    expect_match(html, nm, fixed = TRUE)
  }

  # panel order follows cluster_num: first cluster's name precedes the last's
  first_nm <- clusters_name <- tx$cluster_name[tx$cluster == clusters[[1]]][[1]]
  last_nm  <- tx$cluster_name[tx$cluster == clusters[[length(clusters)]]][[1]]
  expect_lt(regexpr(first_nm, html, fixed = TRUE),
            regexpr(last_nm, html, fixed = TRUE))
})

test_that("PFM (the one branching subcluster) gets a further sub_subcluster accordion", {
  tx <- cgjrdata::cgjr_taxonomy
  branching <- tx$subcluster[!is.na(tx$sub_subcluster)]
  branching <- unique(branching)
  expect_identical(branching, "public_financial_management")   # fixture check

  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))
  # an inner accordion keyed on the branching subcluster
  expect_match(html, "bench-acc_public_financial_management", fixed = TRUE)
  # its 4 sub_subclusters each get a leaf card
  ssubs <- tx$sub_subcluster[tx$subcluster == "public_financial_management"]
  expect_length(ssubs, 4L)
  for (ss in ssubs) expect_match(html, paste0("bench-plot_", ss), fixed = TRUE)

  # a NON-branching subcluster renders its leaf card directly (no inner acc)
  expect_match(html, "bench-plot_degree_of_integrity", fixed = TRUE)
  expect_no_match(html, "bench-acc_degree_of_integrity", fixed = TRUE)
})

test_that("a fully-empty sub_subcluster (domestic_revenue_mobilization) still gets a card", {
  tx <- cgjrdata::cgjr_taxonomy
  drm <- "domestic_revenue_mobilization"
  # explicit fixture: it has zero rows in cgjr_ctf for both ctf_types
  expect_equal(nrow(dplyr::filter(cgjrdata::cgjr_ctf, .data$leaf == drm)), 0L)

  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))
  expect_match(html, paste0("bench-plot_", drm), fixed = TRUE)
  expect_match(html, paste0("bench-ctf_type_", drm), fixed = TRUE)

  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_domestic_revenue_mobilization = "dynamic")
      expect_error(output$`plot_domestic_revenue_mobilization`, "No data available")
    }
  )
})

test_that("fewer than 2 comparators: card asks for a selection, doesn't error", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive("NGA"),          # only 1
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      expect_error(output$`plot_justice_and_rule_of_law`, "at least 2 comparators")
    }
  )
})

test_that("dynamic view rejects a multi-unit base selection (cliarappak parity)", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive(c("GHA", "KEN")),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "dynamic")
      expect_error(output$`plot_justice_and_rule_of_law`,
                  "single base unit")

      # the static view for the same leaf is unaffected
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      expect_type(output$`plot_justice_and_rule_of_law`, "character")
    }
  )
})

test_that("dynamic view uses even years only (cliarappak parity)", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "dynamic")
      j <- output$`plot_justice_and_rule_of_law`   # plotly JSON
      # x-axis year ticks: even years present, odd years absent
      expect_true(grepl('"2014"', j, fixed = TRUE))
      expect_true(grepl('"2018"', j, fixed = TRUE))
      expect_false(grepl('"2013"', j, fixed = TRUE))
      expect_false(grepl('"2017"', j, fixed = TRUE))
    }
  )
})

# --- Slice 4: bindCache on the per-selection computation ---

test_that("caching is transparent: toggle ctf_type and back -> identical output", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      p1 <- output$`plot_justice_and_rule_of_law`

      session$setInputs(ctf_type_justice_and_rule_of_law = "dynamic")
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      expect_identical(output$`plot_justice_and_rule_of_law`, p1)
    }
  )
})

test_that("cache key invalidates: changing threshold changes the plot", {
  tx <- cgjrdata::cgjr_taxonomy
  th <- shiny::reactiveVal("Default")
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold = th,
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_justice_and_rule_of_law = "static")
      p_default <- output$`plot_justice_and_rule_of_law`
      th("Terciles")
      session$flushReact()
      p_terciles <- output$`plot_justice_and_rule_of_law`
      expect_false(identical(p_default, p_terciles))   # different cache entry
      expect_match(p_terciles, "33%")                   # tercile band labels
    }
  )
})

test_that("install_benchmark_cache sets an LRU app cache bindCache can find", {
  old <- shiny::getShinyOption("cache")
  withr::defer(shiny::shinyOptions(cache = old))
  cache <- install_benchmark_cache(max_mb = 8)
  expect_s3_class(cache, "cachem")
  expect_identical(shiny::getShinyOption("cache"), cache)
})

test_that("a leaf with a base-unit coverage gap in the dynamic view still renders", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy = tx, base_unit = shiny::reactive("KEN"),
      comparison_units = shiny::reactive(COMPS),
      threshold = shiny::reactive("Default"),
      year_range = shiny::reactive(c(2013L, 2024L))
    ),
    {
      # service_delivery: KEN has a year gap in every indicator (screen wipes
      # the composite in compute_family_average_app(), but the plot itself
      # still renders - indicator dots survive the 100%-NA rule).
      session$setInputs(ctf_type_service_delivery = "dynamic")
      expect_type(output$`plot_service_delivery`, "character")
    }
  )
})

test_that("server handles the full taxonomy: populated leaves plot, empty ones show a message", {
  tx <- cgjrdata::cgjr_taxonomy
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = tx,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(COMPS),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(
        ctf_type_justice_and_rule_of_law     = "static",  # populated
        ctf_type_domestic_revenue_mobilization = "static" # empty in both types
      )
      expect_type(output$`plot_justice_and_rule_of_law`, "character")

      expect_error(output$`plot_domestic_revenue_mobilization`, "No data available")
    }
  )
})

# --- Overall: cluster / subcluster averages ------------------------------------

test_that("UI: Overall item comes after the clusters, one card per Overall chart", {
  tx <- cgjrdata::cgjr_taxonomy
  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))
  expect_match(html, "bench-acc_overall", fixed = TRUE)
  last_cluster <- tx$cluster_name[which.max(tx$cluster_num)]
  expect_lt(max(gregexpr(last_cluster, html, fixed = TRUE)[[1]][1]),
            regexpr(">Overall<", html, fixed = TRUE))
  for (ch in cgjrapp:::.overall_charts(tx)) {
    expect_match(html, paste0("bench-plot_", ch$key), fixed = TRUE)
    expect_match(html, paste0("bench-ctf_type_", ch$key), fixed = TRUE)
  }
})

test_that("UI: a branching subcluster nests inside its cluster in Overall", {
  tx <- cgjrdata::cgjr_taxonomy
  html <- as.character(mod_benchmark_dashboard_ui("bench", taxonomy = tx))
  n_clusters <- length(unique(tx$cluster))

  # Overall's own items: Clusters + one per cluster - PFM is not among them
  top <- gregexpr('data-parent="#bench-acc_overall"', html, fixed = TRUE)[[1]]
  expect_length(top, n_clusters + 1L)

  # PFM's card sits in the nested accordion of its cluster's item, before the
  # next cluster's chart
  for (b in unique(tx$subcluster[!is.na(tx$sub_subcluster)])) {
    parent <- unique(tx$cluster[tx$subcluster == b])
    nested <- regexpr(sprintf('id="bench-acc_overall__%s"', parent), html, fixed = TRUE)
    card   <- regexpr(paste0("bench-plot_overall__", b), html, fixed = TRUE)
    expect_gt(nested, regexpr(paste0("bench-plot_overall__", parent), html, fixed = TRUE))
    expect_gt(card, nested)
    expect_match(substr(html, nested, card),
                 sprintf('data-parent="#bench-acc_overall__%s"', parent), fixed = TRUE)
    nxt <- tx$cluster[tx$cluster_num == unique(tx$cluster_num[tx$cluster == parent]) + 1L]
    if (length(nxt)) {
      expect_lt(card, regexpr(paste0("bench-plot_overall__", nxt[[1]]), html, fixed = TRUE))
    }
  }
})

test_that("server renders the Overall Clusters card from family_scores() + def_quantiles()", {
  tx <- cgjrdata::cgjr_taxonomy
  comps <- cgjrapp:::.default_comparators("GHA")
  shiny::testServer(
    mod_benchmark_dashboard_server,
    args = list(
      taxonomy         = tx,
      base_unit        = shiny::reactive("GHA"),
      comparison_units = shiny::reactive(comps),
      threshold        = shiny::reactive("Default"),
      year_range       = shiny::reactive(c(2013L, 2024L))
    ),
    {
      session$setInputs(ctf_type_overall__clusters = "static",
                        show_notes_overall__clusters = TRUE)
      p <- output$plot_overall__clusters
      expect_match(p, "plotly")
      # every cluster is a row of the chart
      for (nm in unique(tx$cluster_name)) expect_match(p, nm, fixed = TRUE)

      note <- paste(as.character(output$note_overall__clusters), collapse = " ")
      expect_match(note, "4 clusters \\(\\d+ indicators\\);")

      notes_html <- paste(as.character(output$plot_notes_overall__clusters), collapse = " ")
      expect_match(notes_html, "Ghana compared to", fixed = TRUE)
    }
  )
})

test_that("Overall plots keep taxonomy order, not value order", {
  ch <- cgjrapp:::.overall_charts()[[1]]
  comps <- cgjrapp:::.default_comparators("GHA")
  slice <- filter_unit_data(cgjrapp:::.overall_leaves(ch), "static", c("GHA", comps))
  dq <- def_quantiles(family_scores(slice, "GHA", comps, ch$row_col, ch$rows),
                      "GHA", comps, "Default")
  p <- static_plot(dq, "GHA", row_order = unname(ch$rows))
  # ggplot draws the first level at the bottom: levels are the reversed order
  expect_identical(levels(p$layers[[1]]$data$indicator), rev(unname(ch$rows)))
})
