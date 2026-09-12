# ---
# Live per-selection benchmarking methodology.
#
# A deliberate port of cliarappak's def_quantiles() / def_quantiles_dyn() /
# compute_family_average_app() (R/fct_quantiles.R, R/fct_family.R) onto the
# long `cgjrdata::cgjr_ctf` shape, instead of cliarappak's wide
# `closeness_to_frontier_*` tables. The maths is unchanged; only the data
# reshaping differs. tests/testthat/test-utils_family.R feeds a fixed input to
# both cliarappak's own functions and these and asserts the outputs match.
#
# The four screening rules, carried over verbatim from cliarappak:
#   * def_quantiles()            missing = base unit has ANY NA for the indicator
#   * def_quantiles_dyn()        missing = base unit is 100% NA for the indicator
#   * compute_family_average_app missing = base unit has ANY NA, pooled across
#                                the whole selection window (static AND dynamic)
#   * low variance = 25th and 75th percentile equal across the base + comparison
#                    units (indicators whose code contains "_avg" are exempt;
#                    cgjr_ctf carries no such columns, so this is a no-op here
#                    but kept for parity)
# ---

.family_cutoffs <- function(threshold = c("Default", "Terciles")) {
  threshold <- match.arg(threshold)
  if (threshold == "Terciles") c(33, 66) else c(25, 50)
}

.status_labels <- function(cutoff) {
  c(
    weak     = paste0("Weak\n(bottom ", cutoff[[1]], "%)"),
    emerging = paste0("Emerging\n(", cutoff[[1]], "% - ", cutoff[[2]], "%)"),
    strong   = paste0("Strong\n(top ", 100 - cutoff[[2]], "%)")
  )
}

# Indicators the base unit(s) have no usable data for within `ctf_long`.
#   rule = "any"  -> flagged if any base row is NA          (static / family avg)
#   rule = "all"  -> flagged only if every base row is NA   (dynamic indicators)
# An indicator that has comparison rows but no base row at all is always flagged.
.missing_for_base <- function(ctf_long, base_unit, rule = c("any", "all")) {
  rule <- match.arg(rule)
  base_rows <- dplyr::filter(ctf_long, .data$unit_code %in% base_unit)

  flagged <- base_rows |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      bad = if (rule == "any") any(is.na(.data$ctf)) else all(is.na(.data$ctf)),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$bad) |>
    dplyr::pull(.data$variable)

  absent <- setdiff(unique(ctf_long$variable), unique(base_rows$variable))
  unique(c(flagged, absent))
}

# Indicators flat across the base + comparison units (25th pctile == 75th).
.low_variance <- function(ctf_long) {
  ctf_long |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      q25 = stats::quantile(.data$ctf, 0.25, na.rm = TRUE, names = FALSE),
      q75 = stats::quantile(.data$ctf, 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$q25), !is.na(.data$q75), .data$q25 == .data$q75) |>
    dplyr::pull(.data$variable) |>
    (\(v) v[!grepl("_avg", v)])()
}

.quantile_core <- function(ctf_long, group_cols, cutoff) {
  labs <- .status_labels(cutoff)
  ctf_long |>
    dplyr::filter(!is.na(.data$ctf)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      dtt       = dplyr::percent_rank(.data$ctf),
      q_lv_25   = stats::quantile(.data$ctf, 0.25, names = FALSE),
      q_lv_75   = stats::quantile(.data$ctf, 0.75, names = FALSE),
      q_cutoff1 = stats::quantile(.data$ctf, cutoff[[1]] / 100, names = FALSE),
      q_cutoff2 = stats::quantile(.data$ctf, cutoff[[2]] / 100, names = FALSE),
      status = dplyr::case_when(
        .data$dtt <= cutoff[[1]] / 100 ~ labs[["weak"]],
        .data$dtt >  cutoff[[1]] / 100 & .data$dtt <= cutoff[[2]] / 100 ~ labs[["emerging"]],
        .data$dtt >  cutoff[[2]] / 100 ~ labs[["strong"]]
      ),
      nrank = dplyr::min_rank(-.data$ctf)
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(dtf = "ctf")
}

#' Classify indicators into Weak / Emerging / Strong within a comparison group
#'
#' Long-shape port of `cliarappak::def_quantiles()` (`def_quantiles_dyn()` for
#' the year-by-year variant). Given a base unit and a comparison set, ranks
#' every indicator *within that selection only* and labels each unit's value.
#'
#' Steps (identical to cliarappak):
#' 1. Drop indicators the base unit has no data for - `def_quantiles()` treats
#'    *any* `NA` as missing; `def_quantiles_dyn()` requires *100%* missing.
#' 2. Restrict to base + comparison units and the surviving indicators.
#' 3. Per indicator (per indicator-year for the dynamic variant): `dtt` =
#'    [dplyr::percent_rank()] of `ctf`; `q_lv_25` / `q_lv_75`; the two
#'    `q_cutoff*` percentiles; the `status` label; `nrank` =
#'    `dplyr::min_rank(-ctf)`. `ctf` is renamed `dtf`.
#' 4. Drop indicators flat across the selection (25th == 75th percentile for
#'    the base unit).
#'
#' @param ctf_long Long CTF tibble as returned by [filter_unit_data()] - one
#'   row per `unit_code` x `variable` (x `year` for the dynamic variant), with
#'   an `indicator` display label and a numeric `ctf` column.
#' @param base_unit Character vector of base `unit_code`(s) being benchmarked.
#' @param comparison_units Character vector of comparison `unit_code`s.
#' @param threshold `"Default"` (cut points at the 25th / 50th percentile) or
#'   `"Terciles"` (33rd / 66th).
#'
#' @return A long tibble, one row per unit-indicator (unit-indicator-year for
#'   `def_quantiles_dyn()`), with `dtf`, `dtt`, `q_lv_25`, `q_lv_75`,
#'   `q_cutoff1`, `q_cutoff2`, `status`, `nrank` added and the identifier /
#'   taxonomy columns of `ctf_long` carried through.
#'
#' @seealso [compute_family_average_app()], [static_plot()], [static_plot_dyn()]
#' @export
def_quantiles <- function(ctf_long, base_unit, comparison_units,
                          threshold = c("Default", "Terciles")) {
  cutoff <- .family_cutoffs(threshold)
  keep_units <- unique(c(base_unit, comparison_units))

  dat <- dplyr::filter(ctf_long, .data$unit_code %in% keep_units)
  drop_missing <- .missing_for_base(dat, base_unit, rule = "any")
  dat <- dplyr::filter(dat, !.data$variable %in% drop_missing)

  q <- .quantile_core(dat, group_cols = "variable", cutoff = cutoff)

  lv <- q |>
    dplyr::filter(.data$unit_code %in% base_unit, .data$q_lv_25 == .data$q_lv_75) |>
    dplyr::pull(.data$variable)
  lv <- lv[!grepl("_avg", lv)]
  dplyr::filter(q, !.data$variable %in% lv)
}

#' @rdname def_quantiles
#' @export
def_quantiles_dyn <- function(ctf_long, base_unit, comparison_units,
                              threshold = c("Default", "Terciles")) {
  cutoff <- .family_cutoffs(threshold)
  keep_units <- unique(c(base_unit, comparison_units))

  dat <- dplyr::filter(ctf_long, .data$unit_code %in% keep_units)
  drop_missing <- .missing_for_base(dat, base_unit, rule = "all")
  dat <- dplyr::filter(dat, !.data$variable %in% drop_missing)

  q <- .quantile_core(dat, group_cols = c("variable", "year"), cutoff = cutoff)

  # cliarappak drops a low-variance row-wise here (per unit-indicator-year),
  # not by indicator - replicate that exactly.
  q |>
    dplyr::mutate(
      .todrop = .data$unit_code %in% base_unit & .data$q_lv_25 == .data$q_lv_75
    ) |>
    dplyr::filter(!.data$.todrop) |>
    dplyr::select(-".todrop")
}

#' Family / composite closeness-to-frontier score for a selection
#'
#' Long-shape port of `cliarappak::compute_family_average_app()`. Applies the
#' app's missing-data and low-variance screens, then averages the surviving
#' indicators' `ctf` per unit (per unit-year when `type == "dynamic"`).
#'
#' The missing-data screen is `cliarappak`'s: an indicator is dropped when the
#' base unit has **any** `NA` for it, pooled across the whole selection window -
#' the same rule for static and dynamic (this is looser than
#' [def_quantiles_dyn()]'s 100%-missing rule; the difference is inherited from
#' `cliarappak` and asserted in the parity test).
#'
#' @inheritParams def_quantiles
#' @param type `"static"` (one score per unit) or `"dynamic"` (one per
#'   unit-year).
#'
#' @return A tibble with `leaf` (when `ctf_long` carries it), `unit_code` (and
#'   `year` when `type == "dynamic"`), `score` (the mean, `NaN` collapsed to
#'   `NA`), `n_inputs` (indicators that survived the screens) and
#'   `n_inputs_obs` (of those, how many were observed for the unit). One row
#'   per `leaf` x unit \[x year\] - pass a multi-leaf `ctf_long` to score the
#'   whole taxonomy in one call.
#'
#' @seealso [def_quantiles()]
#' @export
compute_family_average_app <- function(ctf_long, base_unit, comparison_units,
                                       type = c("static", "dynamic")) {
  type <- match.arg(type)
  keep_units <- unique(c(base_unit, comparison_units))

  dat <- dplyr::filter(ctf_long, .data$unit_code %in% keep_units)
  drop_vars <- union(
    .missing_for_base(dat, base_unit, rule = "any"),
    .low_variance(dat)
  )
  dat <- dplyr::filter(dat, !.data$variable %in% drop_vars)

  # Group by leaf so a whole-taxonomy slice yields one composite per leaf
  # (cliarappak groups its wide table by `family_var` for the same reason).
  # `leaf` is constant within a single-leaf slice, so this is a no-op there.
  group_cols <- c(
    if ("leaf" %in% names(dat)) "leaf",
    "unit_code",
    if (type == "dynamic") "year"
  )

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      score        = mean(.data$ctf, na.rm = TRUE),
      n_inputs     = dplyr::n_distinct(.data$variable),
      n_inputs_obs = sum(!is.na(.data$ctf)),
      .groups = "drop"
    ) |>
    dplyr::mutate(score = ifelse(is.nan(.data$score), NA_real_, .data$score))
}
