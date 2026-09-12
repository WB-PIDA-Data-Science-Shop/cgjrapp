# Numeric parity: the CGJR long-shape port of def_quantiles() /
# def_quantiles_dyn() / compute_family_average_app() must reproduce
# cliarappak's own functions cell-for-cell on a fixed input.
#
# Slice 1 widens this from justice_and_rule_of_law to every leaf whose
# indicator set the cgjrdata QA report reconciles 1:1 against a CLIAR
# family_var (qcheck/qa-report.md 3b), plus justice (a deliberate departure,
# but the port must still match cliarappak's function on identical inputs).

QA_LEAVES <- c(
  "degree_of_integrity",
  "transparency_and_accountability",
  "public_sector_hrm",
  "digital_and_data",
  "market_regulatory_institutions",
  "service_delivery"
)
PARITY_LEAVES <- c("justice_and_rule_of_law", QA_LEAVES)
BASE <- "KEN"
N_COMP <- 30L

fixture <- function(want_leaf, want_type) {
  slice <- cgjrdata::cgjr_ctf |>
    dplyr::filter(.data$leaf == .env$want_leaf,
                  .data$ctf_type == .env$want_type,
                  .data$unit_level == "country")
  codes <- slice |>
    dplyr::distinct(.data$unit_code) |>
    dplyr::arrange(.data$unit_code) |>
    dplyr::pull(.data$unit_code)
  comps <- setdiff(codes, BASE)[seq_len(N_COMP)]

  code_to_name <- slice |>
    dplyr::distinct(.data$unit_code, .data$unit_name) |>
    tibble::deframe()

  list(
    slice = slice,
    vars  = sort(unique(slice$variable)),
    comps = comps,
    base_name  = unname(code_to_name[BASE]),
    comp_names = unname(code_to_name[comps]),
    country_list = tibble::tibble(country_name = unname(code_to_name)),
    variable_names = slice |>
      dplyr::distinct(.data$variable, .data$indicator) |>
      dplyr::transmute(
        variable, var_name = .data$indicator,
        family_var = "vars_test", family_name = "Test Family"
      )
  )
}

for (leaf in PARITY_LEAVES) {
  for (ct in c("static", "dynamic")) {
    for (th in c("Default", "Terciles")) {

      test_that(sprintf("def_quantiles parity vs cliarappak [%s / %s / %s]",
                        leaf, ct, th), {
        ref <- load_cliarappak_ref()
        fx  <- fixture(leaf, ct)
        wide <- cgjr_slice_to_wide(fx$slice, type = ct)

        if (ct == "static") {
          ref_out <- ref$def_quantiles(
            wide, fx$base_name, fx$country_list, fx$comp_names,
            fx$vars, fx$variable_names, th
          )
          port <- def_quantiles(fx$slice, BASE, fx$comps, th)
          join_by <- c("country_name" = "unit_name", "variable" = "variable")
        } else {
          ref_out <- ref$def_quantiles_dyn(
            wide, fx$base_name, fx$country_list, fx$comp_names,
            fx$vars, fx$variable_names, th
          )
          port <- def_quantiles_dyn(fx$slice, BASE, fx$comps, th)
          join_by <- c("country_name" = "unit_name", "variable" = "variable",
                       "year" = "year")
        }

        ref_out <- tibble::as_tibble(ref_out) |>
          dplyr::select(dplyr::any_of(c("country_name", "variable", "year")),
                        dtf_ref = "dtf", dtt_ref = "dtt",
                        status_ref = "status", nrank_ref = "nrank")
        port <- dplyr::select(
          port,
          dplyr::any_of(c("unit_name", "variable", "year")),
          "dtf", "dtt", "status", "nrank"
        )

        skip_if(nrow(ref_out) == 0L, "base unit has no usable data for this leaf")

        cmp <- dplyr::inner_join(ref_out, port, by = join_by)

        expect_equal(nrow(cmp), nrow(ref_out))       # every ref row matched
        expect_equal(nrow(cmp), nrow(port))          # no extra port rows
        expect_equal(cmp$dtf, cmp$dtf_ref, tolerance = 1e-9)
        expect_equal(cmp$dtt, cmp$dtt_ref, tolerance = 1e-9)
        expect_identical(cmp$status, cmp$status_ref)
        expect_identical(cmp$nrank, cmp$nrank_ref)
      })

      test_that(sprintf("compute_family_average_app parity vs cliarappak [%s / %s / %s]",
                        leaf, ct, th), {
        ref <- load_cliarappak_ref()
        fx  <- fixture(leaf, ct)
        wide <- cgjr_slice_to_wide(fx$slice, type = ct)

        # cliarappak's function throws when its missing-data screen removes
        # every indicator (empty `vars` -> pivot_longer with no columns). Its
        # pooled any-NA rule wipes small leaves for any base unit with a year
        # gap. The port returns no row for that case, which is the agreement
        # we can assert; there is nothing else to compare.
        ref_fa <- tryCatch(
          ref$compute_family_average_app(
            wide, fx$vars, ct, fx$variable_names, fx$base_name, fx$comp_names
          ) |>
            tibble::as_tibble() |>
            dplyr::filter(.data$country_code == BASE),
          error = function(e) NULL
        )
        port_fa <- compute_family_average_app(fx$slice, BASE, fx$comps, type = ct) |>
          dplyr::filter(.data$unit_code == BASE)

        if (is.null(ref_fa) || nrow(ref_fa) == 0L ||
            !"vars_test_avg" %in% names(ref_fa) ||
            all(is.na(ref_fa$vars_test_avg))) {
          expect_equal(nrow(port_fa), 0L)
          skip("cliarappak's screen removed every indicator; port agrees (no composite)")
        }

        if (ct == "static") {
          expect_equal(nrow(ref_fa), 1L)
          expect_equal(nrow(port_fa), 1L)
          expect_equal(port_fa$score, ref_fa$vars_test_avg, tolerance = 1e-9)
        } else {
          cmp <- dplyr::inner_join(
            dplyr::select(ref_fa, "year", ref = "vars_test_avg"),
            dplyr::select(port_fa, "year", port = "score"),
            by = "year"
          ) |>
            dplyr::filter(!is.na(.data$ref), !is.na(.data$port))
          expect_gt(nrow(cmp), 0L)
          expect_equal(cmp$port, cmp$ref, tolerance = 1e-9)
        }
      })
    }
  }
}

# --- Slice 1: the functions are leaf-agnostic ---
# Handed a multi-leaf slice, each function must produce exactly what running it
# per leaf and stacking the results would (given the same unit selection).

.multi_leaf_units <- function(n = 40L) {
  others <- cgjrdata::cgjr_ctf |>
    dplyr::filter(.data$unit_level == "country", .data$unit_code != BASE) |>
    dplyr::distinct(.data$unit_code) |> dplyr::arrange(.data$unit_code) |>
    dplyr::pull(.data$unit_code)
  c(BASE, utils::head(others, n - 1L))
}

test_that("def_quantiles on a multi-leaf slice == per-leaf calls stacked", {
  leaves <- c("degree_of_integrity", "digital_and_data", "public_sector_hrm")
  units  <- .multi_leaf_units()
  comps  <- setdiff(units, BASE)

  multi <- filter_unit_data(leaves, "static", units)
  combined <- def_quantiles(multi, BASE, comps, "Default") |>
    dplyr::arrange(.data$leaf, .data$unit_code, .data$variable)

  per_leaf <- leaves |>
    lapply(function(l) {
      def_quantiles(filter_unit_data(l, "static", units), BASE, comps, "Default")
    }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$leaf, .data$unit_code, .data$variable)

  expect_equal(combined, per_leaf)
  expect_gt(nrow(combined), 0L)
  expect_setequal(unique(combined$leaf), leaves)
})

test_that("compute_family_average_app on a multi-leaf slice == per-leaf calls", {
  leaves <- c("degree_of_integrity", "digital_and_data", "public_sector_hrm")
  units  <- .multi_leaf_units()
  comps  <- setdiff(units, BASE)

  multi <- compute_family_average_app(
    filter_unit_data(leaves, "static", units), BASE, comps, type = "static"
  ) |> dplyr::arrange(.data$leaf, .data$unit_code)

  per_leaf <- leaves |>
    lapply(function(l) {
      compute_family_average_app(
        filter_unit_data(l, "static", units), BASE, comps, type = "static"
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$leaf, .data$unit_code)

  expect_equal(multi, per_leaf)
  expect_setequal(unique(multi$leaf), leaves)   # one composite per leaf, not pooled
  expect_gt(nrow(multi), 0L)
})

test_that("threshold argument is validated", {
  fx <- fixture("justice_and_rule_of_law", "static")
  expect_error(def_quantiles(fx$slice, BASE, fx$comps, "nonsense"))
})
