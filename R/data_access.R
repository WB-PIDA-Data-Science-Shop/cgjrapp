#' Slice indicator-level CTF rows out of `cgjr_ctf`
#'
#' The single entry point the benchmarking dashboard uses to pull
#' indicator-grain closeness-to-frontier values. It only ever **filters** the
#' long `cgjrdata::cgjr_ctf` tibble - it never recomputes a CTF value (the
#' upstream `cgjrdata` / `cliaretl` pipeline owns that; see the CGJR build
#' plan, non-negotiable constraint 4).
#'
#' `leaf` is an argument, never baked into the function body. Pass one key for
#' a single leaf card, a vector for several, or `NULL` for every leaf in
#' `cgjr_ctf` (the Slice 1 generalisation - the downstream methodology
#' functions key on `leaf` themselves, so a whole-taxonomy slice rolls up
#' correctly).
#'
#' Emptiness is **not** an error. An empty `(leaf, ctf_type)` pair - e.g.
#' `budget_cycle_and_fiscal_planning` for `ctf_type == "dynamic"`, which has no
#' dynamic-eligible indicators - contributes no rows. The caller renders "no
#' data available" for that case (build plan constraint 3).
#'
#' @param leaf `NULL` (every leaf) or a character vector of leaf keys
#'   (`dplyr::coalesce(sub_subcluster, subcluster)` from `cgjr_taxonomy`).
#' @param ctf_type One of `"dynamic"` or `"static"`.
#' @param units Character vector of `unit_code`s (ISO3 for countries, WB
#'   `region_code` such as `"AFE"`, or an income-group slug such as
#'   `"high_income"`). Countries, regions and income groups are equal citizens.
#' @param year_range Optional integer vector; its `range()` bounds the `year`
#'   column. Ignored when `ctf_type == "static"` (static rows carry `year = NA`).
#' @param even_years Logical. When `TRUE` (and `ctf_type == "dynamic"`), keep
#'   only even years - `cliarappak`'s rule for the dynamic benchmark, adopted
#'   for `cgjrapp` (decision, 2026-09-10). Ignored for static.
#' @param ctf The long CTF tibble to filter. Defaults to `cgjrdata::cgjr_ctf`;
#'   overridable for tests.
#'
#' @return A tibble with the columns of `cgjr_ctf`
#'   (`unit_level`, `unit_code`, `unit_name`, `year`, `ctf_type`, `cluster`,
#'   `subcluster`, `sub_subcluster`, `leaf`, `indicator`, `variable`, `ctf`,
#'   `n_inputs`, `n_inputs_obs`), filtered to the requested slice. Row order is
#'   preserved from `cgjr_ctf`.
#'
#' @seealso [def_quantiles()], [compute_family_average_app()]
#' @export
filter_unit_data <- function(leaf = NULL,
                             ctf_type = c("dynamic", "static"),
                             units,
                             year_range = NULL,
                             even_years = FALSE,
                             ctf = cgjrdata::cgjr_ctf) {
  ctf_type <- match.arg(ctf_type)
  stopifnot(
    "`leaf` must be NULL or a character vector" =
      is.null(leaf) || (is.character(leaf) && !anyNA(leaf)),
    "`units` must be a character vector" = is.character(units)
  )

  out <- dplyr::filter(
    ctf,
    .data$ctf_type == !!ctf_type,
    .data$unit_code %in% !!units
  )

  if (!is.null(leaf)) {
    out <- dplyr::filter(out, .data$leaf %in% !!leaf)
  }

  if (ctf_type == "dynamic") {
    if (!is.null(year_range)) {
      rng <- range(year_range, na.rm = TRUE)
      out <- dplyr::filter(
        out, !is.na(.data$year),
        .data$year >= rng[[1]], .data$year <= rng[[2]]
      )
    }
    if (isTRUE(even_years)) {
      out <- dplyr::filter(out, .data$year %% 2L == 0L)
    }
  }

  out
}
