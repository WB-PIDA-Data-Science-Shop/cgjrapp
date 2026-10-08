# Overall benchmarking: family_scores() (port of cliarappak::family_data() /
# family_data_dyn()), overall_excluded() / overall_notes(), .overall_charts().

# --- parity: family_scores() == cliarappak's family_data() ------------------

# A multi-leaf slice (every leaf of one cluster) with each indicator's
# family_var set to its subcluster - CGJR's "family" for the cluster chart.
overall_fixture <- function(want_cluster, want_type, base = "KEN", n_comp = 30L) {
  slice <- cgjrdata::cgjr_ctf |>
    dplyr::filter(.data$cluster == .env$want_cluster,
                  .data$ctf_type == .env$want_type,
                  .data$unit_level == "country",
                  # real countries only, not the WB aggregates in cgjr_ctf
                  .data$unit_code %in% cgjrapp:::.country_codes())
  codes <- sort(unique(slice$unit_code))
  comps <- setdiff(codes, base)[seq_len(n_comp)]
  slice <- dplyr::filter(slice, .data$unit_code %in% c(base, comps))
  name_of <- tibble::deframe(dplyr::distinct(slice, .data$unit_code, .data$unit_name))
  rows <- slice |>
    dplyr::distinct(.data$subcluster) |>
    dplyr::pull(.data$subcluster)
  list(
    slice = slice, base = base, comps = comps,
    base_name = unname(name_of[base]), comp_names = unname(name_of[comps]),
    rows = stats::setNames(rows, rows),
    variable_names = slice |>
      dplyr::distinct(.data$variable, .data$indicator, .data$subcluster) |>
      dplyr::transmute(.data$variable, var_name = .data$indicator,
                       family_var = .data$subcluster)
  )
}

# cgjr_slice_to_wide() leaves its placeholder id columns all-NA. family_data_dyn()
# screens *every* all-NA column of the base unit's rows, ids included, which
# would drop those ids and shift its positional `pivot_longer(cols = 5:ncol)`
# onto the first indicators. cliarappak's real ids are always filled, so fill
# them here too.
filled_wide <- function(slice, type) {
  w <- cgjr_slice_to_wide(slice, type)
  w$income_group  <- "n/a"
  w$country_group <- 0
  if ("region" %in% names(w)) w$region <- "n/a"
  w
}

# cliarappak's wide family table -> long (unit_name, [year,] variable, ctf)
ref_long <- function(wide, fams, dynamic) {
  wide |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of(c("country_name", if (dynamic) "year")),
                  dplyr::any_of(fams)) |>
    tidyr::pivot_longer(dplyr::any_of(fams), names_to = "variable", values_to = "ref")
}

for (cl in c("institutional_environment", "core_governance_functions", "context")) {
  test_that(sprintf("family_scores static parity vs cliarappak::family_data [%s]", cl), {
    ref <- load_cliarappak_ref()
    fx  <- overall_fixture(cl, "static")

    theirs <- ref$family_data(filled_wide(fx$slice, "static"), fx$base_name,
                              fx$variable_names, fx$comp_names) |>
      ref_long(names(fx$rows), dynamic = FALSE)
    ours <- family_scores(fx$slice, fx$base, fx$comps, "subcluster", fx$rows, "static")

    both <- dplyr::inner_join(ours, theirs,
                              by = c(unit_name = "country_name", "variable"))
    expect_gt(nrow(both), 0L)
    expect_identical(nrow(both), nrow(ours))
    expect_equal(both$ctf, both$ref, tolerance = 1e-12)
  })

  test_that(sprintf("family_scores dynamic parity vs cliarappak::family_data_dyn [%s]", cl), {
    ref <- load_cliarappak_ref()
    fx  <- overall_fixture(cl, "dynamic")

    theirs <- ref$family_data_dyn(filled_wide(fx$slice, "dynamic"), fx$base_name,
                                  fx$variable_names) |>
      ref_long(names(fx$rows), dynamic = TRUE)
    ours <- family_scores(fx$slice, fx$base, fx$comps, "subcluster", fx$rows, "dynamic")

    both <- dplyr::inner_join(ours, theirs,
                              by = c(unit_name = "country_name", "year", "variable"))
    expect_gt(nrow(both), 0L)
    expect_identical(nrow(both), nrow(ours))
    expect_equal(both$ctf, both$ref, tolerance = 1e-12)
  })
}

# --- family_scores() on a fixed input -----------------------------------------

.fs_long <- tibble::tribble(
  ~unit_code, ~unit_name, ~cluster, ~variable, ~indicator, ~ctf,
  "GHA", "Ghana",   "c1", "v1", "V1", 0.2,
  "GHA", "Ghana",   "c1", "v2", "V2", 0.6,
  "GHA", "Ghana",   "c1", "v3", "V3", 0.9,
  "GHA", "Ghana",   "c2", "v4", "V4", NA,     # base missing -> screened out
  "GHA", "Ghana",   "c2", "v5", "V5", 0.4,
  "NGA", "Nigeria", "c1", "v1", "V1", 0.1,
  "NGA", "Nigeria", "c1", "v2", "V2", 0.3,
  "NGA", "Nigeria", "c1", "v3", "V3", 0.9,
  "NGA", "Nigeria", "c2", "v4", "V4", 0.8,
  "NGA", "Nigeria", "c2", "v5", "V5", NA,
  "KEN", "Kenya",   "c1", "v1", "V1", 0.5,
  "KEN", "Kenya",   "c1", "v2", "V2", 0.7,
  "KEN", "Kenya",   "c1", "v3", "V3", 0.9,    # v3 flat across units -> screened
  "KEN", "Kenya",   "c2", "v4", "V4", 0.3,
  "KEN", "Kenya",   "c2", "v5", "V5", 0.2
)
.fs_rows <- c(c1 = "Cluster One", c2 = "Cluster Two")

test_that("family_scores: plain mean over each row's screened indicators", {
  fs <- family_scores(.fs_long, "GHA", c("NGA", "KEN"), "cluster", .fs_rows, "static")
  get <- function(u, r) fs$ctf[fs$unit_code == u & fs$variable == r]
  expect_equal(get("GHA", "c1"), mean(c(0.2, 0.6)))   # v3 dropped (flat)
  expect_equal(get("NGA", "c1"), mean(c(0.1, 0.3)))
  expect_equal(get("GHA", "c2"), 0.4)                 # v4 dropped (base NA)
  expect_true(is.na(get("NGA", "c2")))                # nothing observed -> NA, not NaN
  expect_setequal(fs$indicator, unname(.fs_rows))
})

test_that("overall_excluded groups dropped indicators by row; overall_notes lists them", {
  ex <- overall_excluded(.fs_long, "GHA", c("NGA", "KEN"), "cluster", .fs_rows)
  expect_identical(ex$row, c("Cluster One", "Cluster Two"))
  expect_identical(ex$n_total, c(3L, 2L))
  expect_identical(ex$excluded, list("V3", "V4"))

  html <- as.character(overall_notes("GHA", c("NGA", "KEN"), .fs_long, ex))
  expect_match(html, "Ghana compared to Nigeria, Kenya.", fixed = TRUE)
  expect_match(html, "<b>Cluster One</b>: 1 of 3 indicators not considered (V3).", fixed = TRUE)
  expect_match(html, "<b>Cluster Two</b>: 1 of 2 indicators not considered (V4).", fixed = TRUE)
})

# --- .overall_charts(): data-driven from the taxonomy -------------------------

test_that(".overall_charts: Clusters, then each cluster, branching subclusters after", {
  tx <- cgjrdata::cgjr_taxonomy
  ch <- cgjrapp:::.overall_charts(tx)
  keys <- vapply(ch, `[[`, "", "key")
  clusters <- unique(tx$cluster[order(tx$cluster_num)])

  expect_identical(keys[[1]], "overall__clusters")
  expect_identical(names(ch[[1]]$rows), clusters)
  # every cluster has a subcluster chart, in cluster order
  expect_identical(intersect(sub("^overall__", "", keys), clusters), clusters)
  # each subcluster with sub-subclusters gets its own chart right after its cluster's
  branching <- unique(tx$subcluster[!is.na(tx$sub_subcluster)])
  for (b in branching) {
    parent <- unique(tx$cluster[tx$subcluster == b])
    expect_identical(match(paste0("overall__", b), keys),
                     match(paste0("overall__", parent), keys) + 1L)
    rr <- ch[[match(paste0("overall__", b), keys)]]
    expect_identical(rr$row_col, "sub_subcluster")
    expect_setequal(names(rr$rows), unique(tx$sub_subcluster[tx$subcluster == b]))
  }
})
