# Zone band definitions — CLIAR-style cutoffs at 0.25 / 0.50 (kept for
# get_capacity_zones() / get_zone_labels() backward compat and direct tests)
CAPACITY_ZONES <- tibble::tibble(
  ymin  = c(0,    0.25, 0.50),
  ymax  = c(0.25, 0.50, 1.00),
  label = c("Weak", "Emerging", "Strong"),
  fill  = c("#e47a81", "#ffd966", "#8ec18e")
)

#' Return zone thresholds and labels for absolute threshold modes
#'
#' @param threshold_mode One of `"abs_quartile"` (0.25 / 0.50) or
#'   `"abs_tercile"` (1/3 / 2/3).
#' @return A named list: `q1`, `q2`, `labels`, `breaks`, `break_labels`,
#'   `fill_values`.
#' @keywords internal
.get_zone_params <- function(threshold_mode = c("abs_quartile", "abs_tercile")) {
  threshold_mode <- match.arg(threshold_mode)
  if (threshold_mode == "abs_tercile") {
    q1 <- 1 / 3
    q2 <- 2 / 3
    labels <- c(
      weak     = "Weak (bottom 1/3)",
      emerging = "Emerging (1/3 - 2/3)",
      strong   = "Strong (top 1/3)"
    )
    breaks       <- c(0, 1/3, 2/3, 1)
    break_labels <- c("0", "0.33", "0.67", "1")
  } else {
    q1 <- 0.25
    q2 <- 0.50
    labels <- c(
      weak     = "Weak (bottom 25%)",
      emerging = "Emerging (25% - 50%)",
      strong   = "Strong (top 50%)"
    )
    breaks       <- c(0, 0.25, 0.50, 1)
    break_labels <- c("0", "0.25", "0.50", "1")
  }
  fill_values <- c("#D2222D", "#FFBF00", "#238823")
  names(fill_values) <- unname(labels)
  list(
    q1           = q1,
    q2           = q2,
    labels       = labels,
    breaks       = breaks,
    break_labels = break_labels,
    fill_values  = fill_values
  )
}

#' Return zone labels and colours for relative threshold modes
#'
#' For relative modes the actual q1/q2 values are computed annually from
#' reference data via [get_annual_thresholds()] and passed as a tibble —
#' this function only returns the display labels and colours.
#'
#' @param threshold_mode One of `"rel_quartile"` or `"rel_tercile"`.
#' @return A named list: `labels`, `fill_values`.
#'   No `q1`/`q2`/`breaks`/`break_labels` — those are derived from the data.
#' @keywords internal
.get_rel_zone_params <- function(threshold_mode = c("rel_quartile", "rel_tercile")) {
  threshold_mode <- match.arg(threshold_mode)
  if (threshold_mode == "rel_tercile") {
    labels <- c(
      weak     = "Weak (bottom 1/3)",
      emerging = "Emerging (1/3 - 2/3)",
      strong   = "Strong (top 1/3)"
    )
  } else {
    labels <- c(
      weak     = "Weak (bottom 25%)",
      emerging = "Emerging (25% - 50%)",
      strong   = "Strong (top 50%)"
    )
  }
  fill_values <- c("#D2222D", "#FFBF00", "#238823")
  names(fill_values) <- unname(labels)
  list(
    labels      = labels,
    fill_values = fill_values
  )
}

# Status colours for the focal country dot fill
STATUS_COLOURS <- c(
  Weak     = "#D2222D",
  Emerging = "#FFBF00",
  Strong   = "#238823"
)

# Colour palette for country_type encoding (benchmark groups)
COUNTRY_TYPE_COLOURS <- c(
  primary   = "#1a4f8a",   # WB dark blue — bold focal country
  peer      = "#7a7a7a",   # mid-grey — ad-hoc peers
  region    = "#e07b39",   # amber — regional averages
  income    = "#5b9e6e"    # green — income group averages
)

# Shapes for benchmark groups (hollow, CLIAR pattern)
BENCHMARK_SHAPES <- c(
  peer   = 22L,
  region = 23L,
  income = 24L
)

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Return capacity zone background layers for CTF plots
#'
#' Returns a list of three [ggplot2::geom_rect()] layers representing the
#' Weak (0–0.25), Emerging (0.25–0.50), and Strong (0.50–1.0) CTF capacity
#' zones using CLIAR-style colours. The list is intended to be spliced into a
#' `ggplot2` call with `+`.
#'
#' @return A list of three `ggplot2` layer objects (class `"Layer"`).
#'
#' @export
get_capacity_zones <- function() {
  purrr::pmap(
    CAPACITY_ZONES,
    function(ymin, ymax, label, fill) {
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = ymin, ymax = ymax,
        fill = fill, alpha = 0.35
      )
    }
  )
}

#' Return capacity zone label layers for CTF plots
#'
#' Returns three [ggplot2::annotate()] text layers labelling the capacity zones
#' on the right-hand side of the plot.
#'
#' @param x_pos Numeric. The x-position (year) at which to place the labels.
#'   Defaults to `Inf` (right edge).
#'
#' @return A list of three `ggplot2` annotation objects.
#'
#' @keywords internal
get_zone_labels <- function(x_pos = Inf) {
  purrr::pmap(
    CAPACITY_ZONES,
    function(ymin, ymax, label, fill) {
      ggplot2::annotate(
        "text",
        x = x_pos, y = (ymin + ymax) / 2,
        label = label,
        hjust = 1.1, vjust = 0.5,
        size = 3, colour = "#888888",
        fontface = "italic"
      )
    }
  )
}

# ── Master plot function ───────────────────────────────────────────────────────

#' Master CTF benchmarking plot
#'
#' Produces a CLIAR-style dynamic benchmark [ggplot2] visualisation.
#' x-axis = year (discrete character), y-axis = CTF score (0–1).
#' Zone bands are vertical [ggplot2::geom_segment()] per year tick.
#' The focal country is a status-filled circle ([ggplot2::geom_point()]
#' `shape=21`) connected by a line. Benchmark group medians are hollow
#' shapes (`22:25`) via [ggplot2::scale_shape_manual()].
#'
#' Returns a `ggplot` object — callers wrap with [plotly::ggplotly()] for
#' interactive use in Shiny.
#'
#' @param data A tibble produced by combining [filter_country_data()] and
#'   (optionally) [get_aggregate_data()]. Must contain columns: `year`
#'   (numeric), `country_type` (one of `"primary"`, `"peer"`, `"region"`,
#'   `"income"`), and the column named in `y_var`. Country rows need
#'   `country_code` and `country_name`; aggregate rows need `group_label`
#'   and `group_code`.
#' @param y_var Character. Name of the column to map to the y-axis.
#' @param primary_iso Character. ISO3 code of the focal country.
#' @param year_range Integer vector of length 2: `c(min, max)`.
#' @param threshold_mode Character. One of:
#'   * `"abs_quartile"` — fixed 0.25 / 0.50 cutoffs (default).
#'   * `"abs_tercile"`  — fixed 1/3 / 2/3 cutoffs.
#'   * `"rel_quartile"` — annual 25th / 50th percentile from `ref_data`.
#'   * `"rel_tercile"`  — annual 33rd / 67th percentile from `ref_data`.
#' @param thresholds A tibble from [get_annual_thresholds()] with columns
#'   `year`, `q1`, `q2`. Required for relative modes; `NULL` for absolute.
#' @param show_members Logical. When `TRUE` and `member_data` is supplied,
#'   overlays individual member-country dots behind benchmark group medians.
#'   Defaults to `FALSE`.
#' @param member_data A tibble from [get_cluster_member_data()] with columns
#'   `country_name`, `year`, `score`, `group_label`, `country_type`. Only
#'   used when `show_members = TRUE`.
#'
#' @return A `ggplot` object. Never `NULL`.
#'
#' @export
plot_cgjr_master <- function(data, y_var, primary_iso, year_range,
                             threshold_mode = c("abs_quartile", "abs_tercile",
                                               "rel_quartile", "rel_tercile"),
                             thresholds   = NULL,
                             show_members = FALSE,
                             member_data  = NULL) {
  threshold_mode <- match.arg(threshold_mode)

  if (threshold_mode %in% c("rel_quartile", "rel_tercile") &&
      is.null(thresholds)) {
    cli::cli_abort(
      "{.arg thresholds} must be supplied for {.val {threshold_mode}} mode. \
      Use {.fn get_annual_thresholds} to compute it."
    )
  }

  # --- Input validation -------------------------------------------------------
  if (!y_var %in% names(data)) {
    cli::cli_abort(
      "{.arg y_var} ({.val {y_var}}) is not a column in {.arg data}."
    )
  }

  # --- Guard against either label column being absent -------------------------
  if (!"country_name" %in% names(data)) data$country_name <- NA_character_
  if (!"group_label"  %in% names(data)) data$group_label  <- NA_character_

  # --- Derive unified display_label ------------------------------------------
  data <- data |>
    dplyr::mutate(
      display_label = dplyr::coalesce(country_name, group_label, country_type)
    )

  # --- Empty data guard -------------------------------------------------------
  all_years <- seq(year_range[[1]], year_range[[2]])

  if (nrow(data) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = all_years[ceiling(length(all_years) / 2)], y = 0.5,
          label = "No data available for the selected filters.",
          size = 4.5, colour = "#888888", hjust = 0.5
        ) +
        ggplot2::scale_x_continuous(
          limits = c(year_range[[1]] - 0.5, year_range[[2]] + 0.5),
          breaks = all_years, labels = as.character(all_years)
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        .cgjr_theme()
    )
  }

  # --- Build and return -------------------------------------------------------
  suppressWarnings(
    .plot_evolution(data, y_var, primary_iso, year_range,
                    threshold_mode, thresholds,
                    show_members = show_members,
                    member_data  = member_data)
  )
}

# ── Evolution view ─────────────────────────────────────────────────────────────

#' @keywords internal
.plot_evolution <- function(data, y_var, primary_iso, year_range,
                            threshold_mode = "abs_quartile",
                            thresholds   = NULL,
                            show_members = FALSE,
                            member_data  = NULL) {

  is_relative <- startsWith(threshold_mode, "rel")

  # ── Resolve zone parameters and attach q1/q2 as columns ────────────────────
  # Both abs and rel paths produce q1/q2 as real columns on `data` so all
  # downstream code (status classification, seg_data, aes()) is uniform.
  # Pre-computing ALL aes() columns avoids plotly's eval/cbind bug where
  # expressions like `yend = q1` fail when q1 is not a column name.
  if (is_relative) {
    zp <- .get_rel_zone_params(threshold_mode)
    # Join per-year thresholds; rows with no threshold data get NA q1/q2
    data <- data |>
      dplyr::left_join(
        thresholds |> dplyr::select(year, q1, q2),
        by = "year"
      )
  } else {
    zp <- .get_zone_params(threshold_mode)
    # Broadcast fixed scalars as uniform columns
    data <- data |> dplyr::mutate(q1 = zp$q1, q2 = zp$q2)
  }

  primary_data <- data |> dplyr::filter(country_type == "primary")
  bench_data   <- data |> dplyr::filter(country_type != "primary")

  # Restrict all layers to years where the primary country has non-missing data.
  # This avoids rendering zone bands, bench lines, or peer dots in years where
  # the primary country has no observation for this indicator.
  valid_years <- primary_data |>
    dplyr::filter(!is.na(.data[[y_var]])) |>
    dplyr::pull(year)

  primary_data <- primary_data |> dplyr::filter(year %in% valid_years)
  bench_data   <- bench_data   |> dplyr::filter(year %in% valid_years)
  data         <- data         |> dplyr::filter(year %in% valid_years)

  # Status classification — uses per-row q1/q2 (works for both modes)
  if (nrow(primary_data) > 0) {
    primary_data <- primary_data |>
      dplyr::mutate(
        status = dplyr::case_when(
          is.na(q1) | is.na(q2)     ~ zp$labels[["weak"]],
          .data[[y_var]] < q1       ~ zp$labels[["weak"]],
          .data[[y_var]] < q2       ~ zp$labels[["emerging"]],
          TRUE                      ~ zp$labels[["strong"]]
        )
      )
  }

  if (nrow(bench_data) > 0) {
    bench_data <- bench_data |>
      dplyr::mutate(
        bench_label = dplyr::coalesce(group_label, country_name)
      )
  }

  # ── Zone band data: one row per year × zone, all aes columns pre-computed ───
  # plotly's to_basic.GeomRect evals aes() via cbind — every aes() mapping must
  # reference a real column (no inline expressions). We also map `fill` to a
  # `zone` factor so that scale_fill_manual(drop=FALSE) always shows all three
  # legend entries, even when a zone has no primary-country points in it.
  zone_base <- data |>
    dplyr::distinct(year, q1, q2) |>
    tidyr::drop_na(year, q1, q2) |>
    dplyr::mutate(
      xmin   = year - 0.15,
      xmax   = year + 0.15,
      ymin_0 = 0,
      ymax_1 = 1
    )

  zone_levels <- unname(zp$labels)   # c(weak_label, emerging_label, strong_label)

  seg_data <- dplyr::bind_rows(
    dplyr::mutate(zone_base, zone = zone_levels[[1]], ymin = ymin_0, ymax = q1),
    dplyr::mutate(zone_base, zone = zone_levels[[2]], ymin = q1,     ymax = q2),
    dplyr::mutate(zone_base, zone = zone_levels[[3]], ymin = q2,     ymax = ymax_1)
  ) |>
    dplyr::mutate(zone = factor(zone, levels = zone_levels)) |>
    # Drop zero-height bands (degenerate distributions where q1 == q2).
    # The legend entries still appear via scale_fill_manual(drop = FALSE).
    dplyr::filter(ymax > ymin)

  # ── Base plot — geom_rect zone bands (numeric x-axis) ───────────────────────
  # Pass seg_data as the *primary* ggplot() dataset so plotly's to_basic.GeomRect
  # finds all aes() columns (xmin, xmax, ymin, ymax, zone) as real columns in
  # the inherited data frame, avoiding the cbind/eval "object not found" error.
  p <- ggplot2::ggplot(data = seg_data) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                             fill = zone),
      alpha   = 0.30,
      colour  = NA
    )

  # ── Member-country dots: individual countries behind group medians ───────────
  # Rendered only when show_members=TRUE and member_data has rows.
  # Small (size 1.5) semi-transparent circles, coloured by country_type,
  # interactive tooltip with country name + year + score.
  if (isTRUE(show_members) && !is.null(member_data) && nrow(member_data) > 0) {
    member_data <- member_data |>
      dplyr::filter(
        year >= year_range[[1]],
        year <= year_range[[2]]
      ) |>
      dplyr::mutate(
        member_text = paste0(
          "Country: ", country_name, "<br>",
          "Group: ",   group_label,  "<br>",
          "Year: ",    year,         "<br>",
          "CTF: ",     round(score,  3)
        )
      )

    if (nrow(member_data) > 0) {
      p <- p +
        ggplot2::geom_point(
          data    = member_data,
          mapping = ggplot2::aes(
            x    = year,
            y    = score,
            text = member_text
          ),
          shape  = 16,
          size   = 1.2,
          alpha  = 0.30,
          colour = "#555555"
        )
    }
  }

  # ── Benchmark group medians: hollow shapes, one shape per group ─────────────
  if (nrow(bench_data) > 0) {
    p <- p +
      ggplot2::geom_point(
        data    = bench_data,
        mapping = ggplot2::aes(
          x     = year,
          y     = .data[[y_var]],
          shape = bench_label,
          text  = paste0(
            "Group: ", bench_label, "<br>",
            "Year: ", year, "<br>",
            "Median CTF: ", round(.data[[y_var]], 3)
          )
        ),
        alpha  = 0.5,
        colour = "black",
        fill   = "white",
        size   = 2.5
      ) +
      ggplot2::scale_shape_manual(
        values = stats::setNames(
          22:25,
          utils::head(sort(unique(bench_data$bench_label)), 4)
        ),
        name = NULL
      )
  }

  # ── Primary country: status-filled circle (shape 21) + connecting line ──────
  if (nrow(primary_data) > 0) {
    p <- p +
      ggplot2::geom_line(
        data    = primary_data,
        mapping = ggplot2::aes(x = year, y = .data[[y_var]], group = 1),
        colour  = "gray30",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data    = primary_data,
        mapping = ggplot2::aes(
          x    = year,
          y    = .data[[y_var]],
          fill = status,
          text = paste0(
            "Country: ", country_name, "<br>",
            "Year: ", year, "<br>",
            "CTF: ", round(.data[[y_var]], 3)
          )
        ),
        shape  = 21,
        size   = 3,
        colour = "gray0"
      )
  }

  # ── Fill scale — covers both zone rects and primary-country points ──────────
  # drop = FALSE ensures all three zone levels always appear in the legend even
  # when a zone has no primary-country observations in it.
  p <- p +
    ggplot2::scale_fill_manual(
      values = zp$fill_values,
      limits = zone_levels,
      name   = NULL,
      drop   = FALSE,
      guide  = ggplot2::guide_legend(
        order        = 1,
        override.aes = list(shape = 21, size = 3)
      )
    )

  # y-axis breaks: use abs mode breaks when available; rel modes show 0 and 1
  y_breaks <- if (is_relative) c(0, 1) else zp$breaks
  y_labels <- if (is_relative) c("0", "1")  else zp$break_labels

  all_years <- seq(year_range[[1]], year_range[[2]])

  p +
    ggplot2::scale_x_continuous(
      limits = c(year_range[[1]] - 0.5, year_range[[2]] + 0.5),
      breaks = all_years,
      labels = as.character(all_years)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      breaks = y_breaks,
      labels = y_labels
    ) +
    ggplot2::labs(y = "CTF Score", x = NULL, fill = NULL, shape = NULL) +
    .cgjr_theme()
}

# ── Shared theme ───────────────────────────────────────────────────────────────

#' @keywords internal
.cgjr_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line.x       = ggplot2::element_line(colour = "#cccccc"),
      legend.position   = "bottom",
      legend.text       = ggplot2::element_text(size = 9),
      plot.margin       = ggplot2::margin(8, 16, 8, 8)
    )
}
