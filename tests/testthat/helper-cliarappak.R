# Locate the sibling cliarappak source checkout and load its reference
# methodology functions (fct_quantiles.R / fct_family.R) into a private env,
# so the parity test in test-utils_family.R checks the CGJR port against
# cliarappak's *own* code rather than a re-description of it.

# cliarappak's fct_quantiles.R / fct_family.R use unqualified dplyr / tidyr
# verbs, so those packages must be on the search path while they run.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

cliarappak_dir <- function() {
  env <- Sys.getenv("CLIARAPPAK_DIR", "")
  candidates <- c(
    if (nzchar(env)) env,
    testthat::test_path("..", "..", "..", "cliarappak"),
    "../../../cliarappak",
    "~/GitProjects/cliarappak"
  )
  for (d in candidates) {
    if (nzchar(d) && dir.exists(file.path(d, "R"))) {
      return(normalizePath(d))
    }
  }
  NA_character_
}

# Returns an environment with def_quantiles(), def_quantiles_dyn(),
# compute_family_average_app(), check_quantiles() from cliarappak, or skips.
load_cliarappak_ref <- function() {
  dir <- cliarappak_dir()
  testthat::skip_if(
    is.na(dir),
    "cliarappak source checkout not found (set CLIARAPPAK_DIR)"
  )
  testthat::skip_if_not_installed("cliaretl")

  # cliarappak's compute_family_average_app passes a bare vector to
  # cliaretl::compute_family_average -> tidyr::pivot_longer, tripping a
  # tidyselect "external vector" deprecation. It's upstream code; quiet it for
  # the calling test so the parity signal isn't buried.
  withr::local_options(lifecycle_verbosity = "quiet", .local_envir = parent.frame())

  ref <- new.env(parent = globalenv())
  sys.source(file.path(dir, "R", "fct_quantiles.R"), envir = ref)
  sys.source(file.path(dir, "R", "fct_family.R"), envir = ref)
  ref
}

# Build a cliarappak-shape wide CTF table from a long cgjr_ctf slice.
# First 5 columns are identifiers (cliarappak's def_quantiles does select(-(1:5))).
cgjr_slice_to_wide <- function(slice, type = c("static", "dynamic")) {
  type <- match.arg(type)
  base <- dplyr::transmute(
    slice,
    country_code  = .data$unit_code,
    country_name  = .data$unit_name,
    income_group  = NA_character_,
    region        = NA_character_,
    year          = .data$year,
    country_group = NA_real_,
    variable      = .data$variable,
    value         = .data$ctf
  )
  id_cols <- if (type == "dynamic") {
    base$region <- NULL
    c("country_code", "country_name", "income_group", "year", "country_group")
  } else {
    base$year <- NULL
    c("country_code", "country_name", "income_group", "region", "country_group")
  }
  base |>
    tidyr::pivot_wider(names_from = "variable", values_from = "value") |>
    dplyr::select(dplyr::all_of(id_cols), dplyr::everything())
}
