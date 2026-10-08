# ---
# Data Download tab - table builders over the tidy cgjrdata objects.
#
# All of these are filters + a taxonomy-name join, keyed on the same
# bench-style selection the dashboard uses (base_unit / comparison_units /
# ctf_type / year_range). The dynamic view uses even years only, matching the
# dashboard (cliarappak parity).
#
# export_live_scores() is the per-selection composite: screened + averaged for
# THIS comparison group. Not displayed as its own table anywhere (cliarappak's
# benchmarking tab doesn't show one either) - it exists to feed the AI Report
# prompt (mod_report.R / utils_report.R).
# ---

.taxonomy_name_cols <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  dplyr::select(
    as.data.frame(taxonomy),
    "cluster", "cluster_num", "cluster_name",
    "subcluster", "subcluster_num", "subcluster_name",
    "sub_subcluster", "sub_subcluster_num", "sub_subcluster_name"
  )
}

# join cluster/subcluster/sub_subcluster display names + ordering onto a table
# that carries the snake keys (cgjr_ctf / cgjr_raw shape)
.with_taxonomy_names <- function(df, taxonomy = cgjrdata::cgjr_taxonomy) {
  dplyr::left_join(
    df, .taxonomy_name_cols(taxonomy),
    by = c("cluster", "subcluster", "sub_subcluster")
  )
}

#' Indicator-grain CTF export for the current selection
#'
#' @param base_unit,comparison_units Character `unit_code`s.
#' @param ctf_type `"static"` or `"dynamic"`.
#' @param year_range Optional length-2 integer vector (dynamic only).
#' @param ctf,taxonomy Data overrides for tests.
#' @return A tibble ready for [DT::datatable()] / CSV, one row per
#'   unit x indicator (x year for dynamic).
#' @export
export_ctf_table <- function(base_unit, comparison_units,
                             ctf_type = c("static", "dynamic"),
                             year_range = NULL,
                             ctf = cgjrdata::cgjr_ctf,
                             taxonomy = cgjrdata::cgjr_taxonomy) {
  ctf_type <- match.arg(ctf_type)
  units <- unique(c(base_unit, comparison_units))
  filter_unit_data(NULL, ctf_type, units, year_range = year_range,
                   even_years = identical(ctf_type, "dynamic"), ctf = ctf) |>
    .with_taxonomy_names(taxonomy) |>
    dplyr::arrange(.data$cluster_num, .data$subcluster_num,
                   .data$sub_subcluster_num, .data$indicator,
                   .data$unit_name, .data$year) |>
    dplyr::transmute(
      Unit             = .data$unit_name,
      `Unit type`      = .data$unit_level,
      Year             = .data$year,
      Cluster          = .data$cluster_name,
      Subcluster       = .data$subcluster_name,
      `Sub-subcluster` = .data$sub_subcluster_name,
      Indicator        = .data$indicator,
      Variable         = .data$variable,
      CTF              = round(.data$ctf, 4)
    )
}

#' Raw source-value export for the base country
#'
#' `cgjr_raw` is country-grain only and on each provider's native scale, so
#' this ignores `comparison_units` that are regions / income groups and never
#' aggregates.
#'
#' @inheritParams export_ctf_table
#' @param raw,taxonomy Data overrides for tests.
#' @return A tibble, one row per country x indicator x year.
#' @export
export_raw_table <- function(base_unit, comparison_units = character(0),
                             year_range = NULL,
                             raw = cgjrdata::cgjr_raw,
                             taxonomy = cgjrdata::cgjr_taxonomy) {
  units <- unique(c(base_unit, comparison_units))
  # cgjr_raw is country-grain; it also carries phantom region-code rows
  # (unit_level "country", NA unit_name, NA value) - drop those.
  out <- dplyr::filter(raw, .data$unit_code %in% units,
                       .data$unit_level == "country", !is.na(.data$unit_name))
  if (!is.null(year_range)) {
    rng <- range(year_range, na.rm = TRUE)
    out <- dplyr::filter(out, .data$year >= rng[[1]], .data$year <= rng[[2]])
  }
  out |>
    .with_taxonomy_names(taxonomy) |>
    dplyr::arrange(.data$cluster_num, .data$subcluster_num,
                   .data$sub_subcluster_num, .data$indicator,
                   .data$unit_name, .data$year) |>
    dplyr::transmute(
      Country          = .data$unit_name,
      Year             = .data$year,
      Cluster          = .data$cluster_name,
      Subcluster       = .data$subcluster_name,
      `Sub-subcluster` = .data$sub_subcluster_name,
      Indicator        = .data$indicator,
      Variable         = .data$variable,
      Value            = round(.data$value, 4)
    )
}

#' Live per-selection composite scores (feeds the AI Report prompt)
#'
#' One row per leaf (per year for the dynamic view): the screened + averaged
#' composite for the base unit, from [compute_family_average_app()]. Not
#' displayed as its own table anywhere in the app - `mod_report.R` is the only
#' caller, using it to build the AI Report's system prompt.
#'
#' @inheritParams export_ctf_table
#' @return A tibble with `Cluster`, `Leaf`, `Year` (dynamic), `Composite`,
#'   `Indicators used`, `Indicators observed`.
#' @export
export_live_scores <- function(base_unit, comparison_units,
                               ctf_type = c("static", "dynamic"),
                               year_range = NULL,
                               ctf = cgjrdata::cgjr_ctf,
                               taxonomy = cgjrdata::cgjr_taxonomy) {
  ctf_type <- match.arg(ctf_type)
  units <- unique(c(base_unit, comparison_units))
  slice <- filter_unit_data(NULL, ctf_type, units, year_range = year_range,
                            even_years = identical(ctf_type, "dynamic"), ctf = ctf)
  fam <- compute_family_average_app(
    slice, base_unit, comparison_units, type = ctf_type
  ) |>
    dplyr::filter(.data$unit_code %in% base_unit)

  tx <- as.data.frame(taxonomy)
  tx$leaf <- dplyr::coalesce(tx$sub_subcluster, tx$subcluster)
  tx$leaf_name <- dplyr::coalesce(tx$sub_subcluster_name, tx$subcluster_name)
  tx$leaf_ord <- dplyr::coalesce(tx$sub_subcluster_num, 0) + tx$subcluster_num * 100 +
    tx$cluster_num * 10000

  # scaffold every leaf so a leaf the screen wiped still shows (blank composite)
  out <- dplyr::left_join(
    tx[, c("leaf", "leaf_name", "cluster_name", "leaf_ord")],
    fam, by = "leaf"
  ) |>
    dplyr::arrange(.data$leaf_ord)

  cols <- c(
    Cluster                = "cluster_name",
    Leaf                   = "leaf_name",
    if ("year" %in% names(out)) c(Year = "year"),
    Composite              = "score",
    `Indicators used`      = "n_inputs",
    `Indicators observed`  = "n_inputs_obs"
  )
  out <- dplyr::select(out, dplyr::all_of(cols))
  out$Composite <- round(out$Composite, 4)
  # a leaf with no eligible indicators is scaffolded in with 0, not NA
  out$`Indicators used`     <- dplyr::coalesce(out$`Indicators used`, 0L)
  out$`Indicators observed` <- dplyr::coalesce(out$`Indicators observed`, 0L)
  out
}

#' The indicator catalogue (`cgjrdata::cgjr_crosswalk`, tidied for display)
#'
#' Every crosswalk row - taxonomy placement, provider, `cliaretl` variable
#' code, and the variable's description - for the Data Download tab and as
#' methodology reference.
#'
#' @param crosswalk Data override for tests.
#' @return A tibble, one row per indicator x leaf.
#' @export
export_indicator_catalogue <- function(crosswalk = cgjrdata::cgjr_crosswalk) {
  crosswalk |>
    dplyr::arrange(.data$cluster_num, .data$subcluster_num,
                   .data$sub_subcluster_num, .data$indicator_num) |>
    dplyr::transmute(
      Cluster          = .data$cluster_name,
      Subcluster       = .data$subcluster_name,
      `Sub-subcluster` = .data$sub_subcluster_name,
      Indicator        = .data$indicator,
      Source           = .data$source,
      Variable         = .data$variable,
      Description      = .data$description_short
    )
}
