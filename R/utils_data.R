#' @keywords internal
SUBCLUSTER_EXCLUDE_COLS <- c(
  "country_code", "country_name", "year",
  "score", "var_count", "nonna_count"
)

# ── Country data filtering ─────────────────────────────────────────────────────

#' Filter country-level CTF or raw data
#'
#' Subsets a country-keyed tibble (e.g. from [cgjrdata::ctfdata_list] or
#' [cgjrdata::institutional_averages_tbl]) to the focal country and any peers,
#' within a specified year range. Adds a `country_type` column to distinguish
#' the primary country from peers.
#'
#' @param data A tibble with at least `country_code` (character) and `year`
#'   (numeric/integer) columns.
#' @param primary_iso A single ISO3 character code for the focal country.
#' @param peer_isos A character vector of ISO3 codes for peer countries. May be
#'   empty (`character(0)`).
#' @param year_range An integer or numeric vector of length 2:
#'   `c(min_year, max_year)`. Both bounds are inclusive.
#'
#' @return A filtered tibble containing only rows matching the selected
#'   countries and year range, with an added `country_type` column:
#'   `"primary"` for `primary_iso`, `"peer"` for all others. Returns a
#'   zero-row tibble (with the same columns) if no rows match.
#'
#' @export
filter_country_data <- function(data, primary_iso, peer_isos, year_range) {
  if (!"country_code" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain a {.field country_code} column.")
  }
  if (!"year" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain a {.field year} column.")
  }

  all_isos <- unique(c(primary_iso, peer_isos))

  data |>
    dplyr::filter(
      country_code %in% all_isos,
      year >= year_range[[1]],
      year <= year_range[[2]]
    ) |>
    dplyr::mutate(
      country_type = dplyr::if_else(country_code == primary_iso, "primary", "peer")
    )
}

# ── Aggregate data retrieval ───────────────────────────────────────────────────

#' Retrieve and combine regional and income-group aggregate CTF data
#'
#' Pulls precomputed CTF averages from [cgjrdata::regionctf_list] and
#' [cgjrdata::incomectf_list] for a given cluster/subcluster, filters by
#' selected codes and year range, and returns a normalised tibble ready to
#' be bound with country-level data for plotting.
#'
#' @param cluster Character. A top-level key from `names(cgjrdata::ctfdata_list)`,
#'   e.g. `"institutional_environment"`.
#' @param subcluster Character. A second-level key from
#'   `names(cgjrdata::ctfdata_list[[cluster]])`.
#' @param region_codes Character vector of `region_code` values to include
#'   (e.g. `c("EAP", "SSA")`). Use `character(0)` to include none.
#' @param income_groups Character vector of `income_group` values to include
#'   (e.g. `c("High income")`). Use `character(0)` to include none.
#' @param year_range An integer or numeric vector of length 2:
#'   `c(min_year, max_year)`. Both bounds are inclusive.
#'
#' @return A tibble with columns `group_label`, `group_code`, `year`, `score`,
#'   `country_type`. Returns a zero-row tibble with those columns if both
#'   `region_codes` and `income_groups` are empty or yield no rows.
#'
#' @export
get_aggregate_data <- function(cluster, subcluster, region_codes,
                               income_groups, year_range) {
  empty_result <- tibble::tibble(
    group_label  = character(),
    group_code   = character(),
    year         = integer(),
    score        = double(),
    country_type = character()
  )

  region_rows <- if (length(region_codes) > 0) {
    cgjrdata::regionctf_list[[cluster]][[subcluster]] |>
      dplyr::filter(
        region_code %in% region_codes,
        year >= year_range[[1]],
        year <= year_range[[2]]
      ) |>
      dplyr::transmute(
        group_label  = region,
        group_code   = region_code,
        year,
        score,
        country_type = "region"
      )
  } else {
    empty_result
  }

  income_rows <- if (length(income_groups) > 0) {
    cgjrdata::incomectf_list[[cluster]][[subcluster]] |>
      dplyr::filter(
        income_group %in% income_groups,
        year >= year_range[[1]],
        year <= year_range[[2]]
      ) |>
      dplyr::transmute(
        group_label  = income_group,
        group_code   = income_group,
        year,
        score,
        country_type = "income"
      )
  } else {
    empty_result
  }

  dplyr::bind_rows(region_rows, income_rows)
}

# ── Individual member-country data ────────────────────────────────────────────

#' Get individual country rows belonging to selected benchmark groups
#'
#' Returns country-level rows from a subcluster tibble for every country that
#' belongs to one of the selected `region_codes` or `income_groups`, filtered
#' to the given `year_range`. Intended for overlaying individual dots behind
#' group-median benchmark lines.
#'
#' @param cluster Character. Top-level key from `names(cgjrdata::ctfdata_list)`.
#' @param subcluster Character. Second-level key.
#' @param score_var Character. Name of the score column to keep. For overview
#'   cluster-score plots pass the cluster score column name (e.g.
#'   `"institutional_environment_score"`); for subcluster indicator plots pass
#'   the indicator column name.
#' @param region_codes Character vector of region codes. Use `character(0)` to
#'   include none.
#' @param income_groups Character vector of income group labels. Use
#'   `character(0)` to include none.
#' @param year_range Integer vector of length 2: `c(min_year, max_year)`.
#'
#' @return A tibble with columns `country_code`, `country_name`, `year`,
#'   `score` (renamed from `score_var`), `group_label`, `group_code`,
#'   `country_type` (`"region"` or `"income"`). Returns a zero-row tibble with
#'   those columns when no groups are selected or no rows match.
#'
#' @export
get_cluster_member_data <- function(cluster, subcluster, score_var,
                                    region_codes, income_groups,
                                    year_range) {
  empty_result <- tibble::tibble(
    country_code = character(),
    country_name = character(),
    year         = integer(),
    score        = double(),
    group_label  = character(),
    group_code   = character(),
    country_type = character()
  )

  if (length(region_codes) == 0L && length(income_groups) == 0L) {
    return(empty_result)
  }

  # Country-level subcluster data (has country_code, country_name, year, score_var)
  sub_tbl <- cgjrdata::ctfdata_list[[cluster]][[subcluster]]

  if (!score_var %in% names(sub_tbl)) {
    # score_var may be a cluster-level column not present in subcluster tibble;
    # fall back to the generic "score" column if available
    if ("score" %in% names(sub_tbl)) {
      score_var <- "score"
    } else {
      cli::cli_abort(
        "{.arg score_var} ({.val {score_var}}) is not a column in the \\
        subcluster tibble and no {.field score} fallback exists."
      )
    }
  }

  # WB country metadata for group membership
  wbc <- cgjrdata::wbcountries |>
    dplyr::select(country_code, region_code, region, income_group)

  # Filter subcluster data to year range
  sub_filtered <- sub_tbl |>
    dplyr::filter(year >= year_range[[1]], year <= year_range[[2]]) |>
    dplyr::select(dplyr::all_of(c("country_code", "country_name", "year", score_var))) |>
    dplyr::rename(score = dplyr::all_of(score_var)) |>
    dplyr::left_join(wbc, by = "country_code")

  region_rows <- if (length(region_codes) > 0L) {
    sub_filtered |>
      dplyr::filter(region_code %in% region_codes) |>
      dplyr::transmute(
        country_code,
        country_name,
        year,
        score,
        group_label  = region,
        group_code   = region_code,
        country_type = "region"
      )
  } else {
    empty_result
  }

  income_rows <- if (length(income_groups) > 0L) {
    sub_filtered |>
      dplyr::filter(income_group %in% income_groups) |>
      dplyr::transmute(
        country_code,
        country_name,
        year,
        score,
        group_label  = income_group,
        group_code   = income_group,
        country_type = "income"
      )
  } else {
    empty_result
  }

  dplyr::bind_rows(region_rows, income_rows) |>
    tidyr::drop_na(score)
}

# ── Distributional threshold helpers ──────────────────────────────────────────

#' Compute annual quantile thresholds from a reference dataset
#'
#' For each year in `year_range`, computes two quantile cutoffs (`q1`, `q2`)
#' from `y_var` across all rows (all countries), ignoring `NA`s. Returns a
#' one-row-per-year tibble that can be joined to plot data so zone boundaries
#' vary year-by-year.
#'
#' @param data A tibble containing at least `year` (numeric) and the column
#'   named by `y_var`. Pass the **unfiltered** global reference dataset —
#'   e.g. `cgjrdata::institutional_averages_tbl` for cluster-score plots, or
#'   `cgjrdata::ctfdata_list[[cluster]][[subcluster]]` for indicator plots.
#' @param y_var Character. Name of the score/indicator column to quantile.
#' @param year_range Integer vector of length 2: `c(min_year, max_year)`.
#' @param probs Numeric vector of length 2: lower and upper quantile
#'   probabilities. Defaults to `c(0.25, 0.50)` (quartile-based).
#'
#' @return A tibble with columns `year` (numeric), `q1` (lower threshold),
#'   `q2` (upper threshold). One row per year in `year_range`.
#'
#' @export
get_annual_thresholds <- function(data, y_var, year_range,
                                  probs = c(0.25, 0.50)) {
  if (!y_var %in% names(data)) {
    cli::cli_abort(
      "{.arg y_var} ({.val {y_var}}) is not a column in {.arg data}."
    )
  }
  if (length(probs) != 2L || any(probs < 0) || any(probs > 1) ||
      probs[[1]] >= probs[[2]]) {
    cli::cli_abort(
      "{.arg probs} must be a length-2 vector with 0 <= probs[1] < probs[2] <= 1."
    )
  }

  data |>
    dplyr::filter(
      year >= year_range[[1]],
      year <= year_range[[2]]
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      q1 = stats::quantile(.data[[y_var]], probs = probs[[1]], na.rm = TRUE),
      q2 = stats::quantile(.data[[y_var]], probs = probs[[2]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(year)
}

# ── Sidebar choice helpers ─────────────────────────────────────────────────────

#' Get country choices for the primary country selector
#'
#' Returns a named character vector of all countries in
#' [cgjrdata::institutional_averages_tbl], sorted alphabetically by country
#' name. Suitable for use in [shiny::selectInput()].
#'
#' @return A named character vector: names are country names, values are ISO3
#'   codes.
#'
#' @export
get_country_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::rename(country_name = economy) |>
    dplyr::distinct(country_code, country_name) |>
    dplyr::arrange(country_name)
  stats::setNames(choices$country_code, choices$country_name)
}

#' Get region choices for the benchmark groups selector
#'
#' Returns a named character vector of World Bank regions derived from
#' [cgjrdata::wbcountries], suitable for use in [shiny::selectizeInput()].
#'
#' @return A named character vector: names are full region names, values are
#'   region codes (e.g. `"EAP"`).
#'
#' @export
get_region_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::filter(!is.na(region_code), !is.na(region)) |>
    dplyr::distinct(region_code, region) |>
    dplyr::arrange(region)
  stats::setNames(choices$region_code, choices$region)
}

#' Get income group choices for the benchmark groups selector
#'
#' Returns a named character vector of World Bank income groups derived from
#' [cgjrdata::wbcountries], suitable for use in [shiny::selectizeInput()].
#'
#' @return A named character vector: both names and values are the income group
#'   label strings (e.g. `"High income"`).
#'
#' @export
get_income_choices <- function() {
  choices <- cgjrdata::wbcountries |>
    dplyr::filter(!is.na(income_group)) |>
    dplyr::distinct(income_group) |>
    dplyr::arrange(income_group) |>
    dplyr::pull(income_group)
  stats::setNames(choices, choices)
}

#' Get indicator column choices for a subcluster
#'
#' Returns a named character vector of indicator column names from a subcluster
#' tibble, excluding the standard structural columns. Suitable for use in
#' [shiny::selectInput()].
#'
#' @param subcluster_tbl A tibble from `cgjrdata::ctfdata_list[[cluster]][[subcluster]]`.
#'
#' @return A named character vector of indicator column names. Returns
#'   `character(0)` if no indicator columns are present.
#'
#' @export
get_indicator_choices <- function(subcluster_tbl) {
  indicators <- setdiff(names(subcluster_tbl), SUBCLUSTER_EXCLUDE_COLS)
  stats::setNames(indicators, indicators)
}

#' Get a label map from indicator variable codes to human-readable names
#'
#' Looks up `cgjrdata::metadata_tbl` to map variable codes (column names in
#' subcluster tibbles) to their human-readable `var_name` labels. Variables
#' not found in metadata fall back to the raw code. Intended for use as DT
#' column headers in the detail module.
#'
#' @param indicators Character vector of indicator variable codes, e.g.
#'   `c("wjp_rol_2", "vdem_core_v2x_pubcorr")`.
#'
#' @return A named character vector where names are the raw codes and values
#'   are human-readable labels (from `metadata_tbl$var_name`, falling back to
#'   the code itself when not found).
#'
#' @export
get_indicator_label_map <- function(indicators) {
  if (length(indicators) == 0L) return(character(0))
  meta <- cgjrdata::metadata_tbl |>
    dplyr::select(variable, var_name) |>
    dplyr::distinct(variable, .keep_all = TRUE)
  labels <- stats::setNames(indicators, indicators)
  matched <- match(indicators, meta$variable)
  found <- !is.na(matched)
  labels[found] <- meta$var_name[matched[found]]
  labels
}

#' Get metadata for a single indicator code
#'
#' Returns a named list with fields `var_name`, `variable`, `cluster`,
#' `subcluster`, `description_short`, and `source` for the given indicator
#' code. Falls back gracefully when the indicator is not in
#' [cgjrdata::metadata_tbl].
#'
#' @param ind Character scalar — indicator column name, e.g. `"wjp_rol_2"`.
#' @return A named list, or `NULL` if `ind` is not found in metadata.
#' @export
get_indicator_metadata <- function(ind) {
  row <- cgjrdata::metadata_tbl |>
    dplyr::filter(variable == ind) |>
    dplyr::select(var_name, variable, cluster, subcluster,
                  description_short, source) |>
    dplyr::slice(1L)
  if (nrow(row) == 0L) return(NULL)
  as.list(row)
}

# ── Data-page table helpers ───────────────────────────────────────────────────

#' Build the Scores table for the Data page
#'
#' Returns a wide tibble of cluster and subcluster CTF score averages for the
#' selected entities (primary country, peers, regions, income groups), filtered
#' to `year_range`. Each score column retains its original name; the caller can
#' rename using [get_indicator_label_map()] if desired.
#'
#' @param primary_iso Character scalar — ISO3 of the focal country.
#' @param peer_isos Character vector — peer ISO3 codes.
#' @param region_codes Character vector — selected region codes.
#' @param income_groups Character vector — selected income group labels.
#' @param year_range Integer vector of length 2: `c(min_year, max_year)`.
#'
#' @return A tibble with columns `Entity`, `Year`, then one column per score
#'   variable in [cgjrdata::institutional_averages_tbl].
#'
#' @export
get_scores_table <- function(primary_iso, peer_isos,
                             region_codes, income_groups, year_range) {
  score_cols <- setdiff(
    names(cgjrdata::institutional_averages_tbl),
    c("country_code", "country_name", "year")
  )

  country_rows <- filter_country_data(
    data        = cgjrdata::institutional_averages_tbl,
    primary_iso = primary_iso,
    peer_isos   = peer_isos,
    year_range  = year_range
  ) |>
    dplyr::mutate(Entity = country_name) |>
    dplyr::select(Entity, Year = year, dplyr::all_of(score_cols))

  # First subcluster of first cluster as a proxy for agg score lookup
  first_cluster    <- names(cgjrdata::ctfdata_list)[[1]]
  first_subcluster <- names(cgjrdata::ctfdata_list[[first_cluster]])[[1]]

  agg_rows <- get_aggregate_data(
    cluster       = first_cluster,
    subcluster    = first_subcluster,
    region_codes  = region_codes,
    income_groups = income_groups,
    year_range    = year_range
  )

  if (nrow(agg_rows) == 0L) {
    return(dplyr::arrange(country_rows, Entity, Year))
  }

  # For aggregate entities, pull their scores from institutional_averages_tbl
  # (which doesn't have region/income rows) — so we reconstruct from
  # the per-cluster agg data by looping all clusters.
  all_cluster_agg <- purrr::map(
    names(cgjrdata::ctfdata_list),
    function(ck) {
      sub_key <- names(cgjrdata::ctfdata_list[[ck]])[[1]]
      agg <- get_aggregate_data(
        cluster       = ck,
        subcluster    = sub_key,
        region_codes  = region_codes,
        income_groups = income_groups,
        year_range    = year_range
      )
      if (nrow(agg) == 0L) return(NULL)
      score_col <- paste0(ck, "_score")
      agg |>
        dplyr::select(group_label, year, score) |>
        dplyr::rename(Entity = group_label, Year = year, !!score_col := score)
    }
  ) |>
    purrr::compact()

  if (length(all_cluster_agg) == 0L) {
    return(dplyr::arrange(country_rows, Entity, Year))
  }

  agg_wide <- purrr::reduce(all_cluster_agg, dplyr::full_join,
                             by = c("Entity", "Year"))

  # Add any score_cols not covered by cluster aggregates as NA
  missing_cols <- setdiff(score_cols, names(agg_wide))
  for (col in missing_cols) agg_wide[[col]] <- NA_real_

  agg_wide <- dplyr::select(agg_wide, Entity, Year, dplyr::all_of(score_cols))

  dplyr::bind_rows(country_rows, agg_wide) |>
    dplyr::arrange(Entity, Year) |>
    dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 3)))
}

#' Build the CTF Indicators table for the Data page
#'
#' Returns a tibble with one row per entity × year × subcluster, and one
#' column per indicator (rescaled 0–1 CTF values). Column names are
#' human-readable labels from [cgjrdata::metadata_tbl]. Aggregate rows
#' (region/income) are sourced from `regionctf_list` / `incomectf_list`.
#'
#' @inheritParams get_scores_table
#'
#' @return A tibble with columns `Entity`, `Year`, `Cluster`, `Subcluster`,
#'   then one column per indicator.
#'
#' @export
get_ctf_table <- function(primary_iso, peer_isos,
                          region_codes, income_groups, year_range) {
  .build_indicator_table(
    country_list  = cgjrdata::ctfdata_list,
    region_list   = cgjrdata::regionctf_list,
    income_list   = cgjrdata::incomectf_list,
    primary_iso   = primary_iso,
    peer_isos     = peer_isos,
    region_codes  = region_codes,
    income_groups = income_groups,
    year_range    = year_range
  )
}

#' Build the Raw Data table for the Data page
#'
#' Returns the same structure as [get_ctf_table()] but using
#' `rawdata_list` / `regionrawdata_list` / `incomerawdata_list` — the
#' original source-scale indicator values before CTF rescaling.
#'
#' @inheritParams get_scores_table
#'
#' @return A tibble with columns `Entity`, `Year`, `Cluster`, `Subcluster`,
#'   then one column per indicator (original source scale).
#'
#' @export
get_raw_table <- function(primary_iso, peer_isos,
                          region_codes, income_groups, year_range) {
  .build_indicator_table(
    country_list  = cgjrdata::rawdata_list,
    region_list   = cgjrdata::regionrawdata_list,
    income_list   = cgjrdata::incomerawdata_list,
    primary_iso   = primary_iso,
    peer_isos     = peer_isos,
    region_codes  = region_codes,
    income_groups = income_groups,
    year_range    = year_range
  )
}

# Internal: shared logic for get_ctf_table() and get_raw_table()
.build_indicator_table <- function(country_list, region_list, income_list,
                                   primary_iso, peer_isos,
                                   region_codes, income_groups, year_range) {
  skip <- c("country_code", "country_name", "year",
            "score", "var_count", "nonna_count")

  purrr::map_dfr(names(country_list), function(cluster_key) {
    cluster_label <- key_to_title(cluster_key)
    purrr::map_dfr(names(country_list[[cluster_key]]), function(sub_key) {
      sub_label  <- key_to_title(sub_key)
      ctry_tbl   <- country_list[[cluster_key]][[sub_key]]
      indicators <- setdiff(names(ctry_tbl), skip)

      if (length(indicators) == 0L) return(NULL)

      # Country / peer rows
      ctry_rows <- filter_country_data(
        data        = ctry_tbl,
        primary_iso = primary_iso,
        peer_isos   = peer_isos,
        year_range  = year_range
      ) |>
        dplyr::mutate(Entity = country_name) |>
        dplyr::select(Entity, Year = year, dplyr::all_of(indicators))

      # Region aggregate rows
      reg_rows <- if (length(region_codes) > 0L &&
                      !is.null(region_list[[cluster_key]][[sub_key]])) {
        reg_tbl   <- region_list[[cluster_key]][[sub_key]]
        reg_inds  <- intersect(indicators, names(reg_tbl))
        if (length(reg_inds) > 0L) {
          reg_tbl |>
            dplyr::filter(
              region_code %in% region_codes,
              year >= year_range[[1]],
              year <= year_range[[2]]
            ) |>
            dplyr::mutate(Entity = region) |>
            dplyr::select(Entity, Year = year, dplyr::all_of(reg_inds))
        } else NULL
      } else NULL

      # Income aggregate rows
      inc_rows <- if (length(income_groups) > 0L &&
                      !is.null(income_list[[cluster_key]][[sub_key]])) {
        inc_tbl  <- income_list[[cluster_key]][[sub_key]]
        inc_inds <- intersect(indicators, names(inc_tbl))
        if (length(inc_inds) > 0L) {
          inc_tbl |>
            dplyr::filter(
              income_group %in% income_groups,
              year >= year_range[[1]],
              year <= year_range[[2]]
            ) |>
            dplyr::mutate(Entity = income_group) |>
            dplyr::select(Entity, Year = year, dplyr::all_of(inc_inds))
        } else NULL
      } else NULL

      combined <- dplyr::bind_rows(ctry_rows, reg_rows, inc_rows)
      if (nrow(combined) == 0L) return(NULL)

      # Apply human-readable column names
      label_map <- get_indicator_label_map(indicators)
      combined <- dplyr::mutate(combined,
        Cluster    = cluster_label,
        Subcluster = sub_label,
        .before    = 1
      )
      # Rename indicator columns using label map
      ind_cols_present <- intersect(indicators, names(combined))
      names(combined)[names(combined) %in% ind_cols_present] <-
        label_map[names(combined)[names(combined) %in% ind_cols_present]]

      combined |>
        dplyr::arrange(Entity, Year) |>
        dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 3)))
    })
  })
}

#' Convert snake_case cluster or subcluster key to a display title
#'
#' Replaces underscores with spaces and applies title case. Used to generate
#' tab labels from `ctfdata_list` keys without hardcoding display strings.
#'
#' @param key Character scalar, e.g. `"institutional_environment"`.
#'
#' @return Character scalar, e.g. `"Institutional Environment"`.
#'
#' @export
key_to_title <- function(key) {
  tools::toTitleCase(gsub("_", " ", key))
}
