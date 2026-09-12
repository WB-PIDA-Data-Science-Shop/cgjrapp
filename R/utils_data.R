# utils_data.R -- sidebar picker helpers.
#
# The Data Download / Report table builders live in R/data_exports.R now
# (build-plan Slice 5) -- they replaced the pre-tidy get_*_table /
# get_indicator_* helpers this file used to carry.

# -- Sidebar choice helpers ---

#' Country choices for the base-unit selector
#'
#' Named character vector of every country in [cgjrdata::wbcountries], sorted
#' by name, for [shiny::selectInput()].
#'
#' @return Names are country names, values are ISO3 codes.
#' @export
get_country_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::distinct(country_code = .data$country_code, country_name = .data$economy) |>
    dplyr::arrange(.data$country_name)
  stats::setNames(choices$country_code, choices$country_name)
}

#' WB region choices for the comparator selector
#'
#' @return Named character vector: names are region names, values are region
#'   codes (e.g. `"EAP"`).
#' @export
get_region_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::filter(!is.na(.data$region_code), !is.na(.data$region)) |>
    dplyr::distinct(.data$region_code, .data$region) |>
    dplyr::arrange(.data$region)
  stats::setNames(choices$region_code, choices$region)
}

#' WB income-group choices for the comparator selector
#'
#' @return Named character vector: names and values are the income-group
#'   labels (e.g. `"High income"`).
#' @export
get_income_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::filter(!is.na(.data$income_group)) |>
    dplyr::distinct(.data$income_group) |>
    dplyr::arrange(.data$income_group) |>
    dplyr::pull(.data$income_group)
  stats::setNames(choices, choices)
}

#' Convert a snake_case taxonomy key to a display title
#'
#' Underscores to spaces, then title case. Used for readable node labels
#' without hardcoding display strings.
#'
#' @param key Character vector, e.g. `"institutional_environment"`.
#' @return Character vector, e.g. `"Institutional Environment"`.
#' @export
key_to_title <- function(key) {
  tools::toTitleCase(gsub("_", " ", key))
}
