# Publication identity: Crossref's record for each DOI, and whether a source's
# metadata matches it. HTTP is mocked with recorded public responses; no network.

POLICY <- load_source_policy()
PRWP_DOI <- "10.1596/1813-9450-10390"
no_pause <- function(seconds) invisible()

test_that("Crossref's record for a DOI gives its title, year and split author names", {
  seen <- local_provider(research_fixture("crossref_prwp_10390.json"))
  cr <- cgjrapp:::.crossref_lookup(PRWP_DOI, POLICY, pause = no_pause)
  expect_match(seen()[[1]]$url, "^https://api\\.crossref\\.org/works/10\\.1596")
  expect_identical(cr$status, "found")
  expect_identical(cr$title, "Introducing E-Procurement in Bangladesh: The Promise of Efficiency and Openness")
  expect_identical(cr$year, 2023L)
  expect_identical(cr$authors[[1]]$family[1:2], c("Blum", "Datta"))
  expect_identical(cr$authors[[1]]$given[[1]], "Jürgen René")
  expect_identical(cr$publisher, "World Bank, Washington, DC")
})

test_that("a DOI Crossref does not know is not_found; an outage is not evidence against it", {
  local_provider(list(status = "error"), status = 404L)
  expect_identical(cgjrapp:::.crossref_lookup("10.9999/made-up", POLICY, pause = no_pause)$status,
                   "not_found")
  local_provider(list(status = "error"), status = 503L)
  expect_identical(cgjrapp:::.crossref_lookup(PRWP_DOI, POLICY, pause = no_pause)$status, "unavailable")
  local_provider(list(status = "error"), status = 429L)
  expect_identical(cgjrapp:::.crossref_lookup(PRWP_DOI, POLICY, pause = no_pause)$status, "rate_limited")
})

test_that("Crossref lookups go one at a time, spaced to its rate limit", {
  local_provider(research_fixture("crossref_prwp_10390.json"))
  waits <- numeric(0)
  cgjrapp:::.crossref_lookup(c(PRWP_DOI, PRWP_DOI, PRWP_DOI), POLICY,
                             pause = function(s) waits <<- c(waits, s))
  expect_identical(waits, c(0.2, 0.2))  # between requests, not before the first
})

# -- .verify_identity() ---

# Crossref's answer for the working paper, edited by `edit` (a function of
# the parsed record) - or a status with no record.
crossref_for <- function(edit = identity, status = 200L) {
  fx <- research_fixture("crossref_prwp_10390.json")
  fx$message <- edit(fx$message)
  local_provider(if (status == 200L) fx else list(status = "error"), status = status)
  cgjrapp:::.crossref_lookup(PRWP_DOI, POLICY, pause = no_pause)
}
# The paper as OpenAlex alone describes it: display names split on the last
# word, so flagged
openalex_record <- function() {
  r <- cgjrapp:::.as_sources(prwp_rows()[1, ])
  r$issues <- list("author_name_unsplit")
  r
}

test_that("a source matching its DOI's record is verified, with Crossref's author names", {
  v <- cgjrapp:::.verify_identity(openalex_record(), crossref_for())
  expect_identical(v$identity, "verified")
  expect_identical(v$authors[[1]]$given[[1]], "Jürgen René")
  expect_false("author_name_unsplit" %in% v$issues[[1]])
})

test_that("a real DOI that points to another publication is a conflict", {
  check <- function(edit) {
    v <- cgjrapp:::.verify_identity(openalex_record(), crossref_for(edit))
    c(v$identity, setdiff(v$issues[[1]], "author_name_unsplit"))
  }
  expect_identical(check(function(m) { m$title <- list("Tax Morale in Ghana"); m }),
                   c("conflict", "doi_title_mismatch"))
  expect_identical(check(function(m) { m$issued$`date-parts`[[1]][[1]] <- 2019L; m }),
                   c("conflict", "doi_year_mismatch"))
  expect_identical(check(function(m) { m$author[[1]]$family <- "Smith"; m }),
                   c("conflict", "doi_author_mismatch"))
  # a year apart is the same publication (online and print dates)
  expect_identical(check(function(m) { m$issued$`date-parts`[[1]][[1]] <- 2022L; m }), "verified")
  # the same title with an accent and different case is the same title
  expect_identical(check(function(m) { m$title <- list("INTRODUCING E-PROCUREMENT IN BANGLADÉSH: The Promise of Efficiency and Openness"); m }),
                   "verified")
})

test_that("title markup and stray spaces do not make a mismatch", {
  # as seen live (Public Administration Review, 10.1111/puar.13696): Crossref
  # keeps <scp> small-caps markup; OpenAlex strips it, leaving a stray space
  cr <- crossref_for(function(m) {
    m$title <- list("Introducing <scp>E‐Procurement</scp> in Bangladesh: The Promise of Efficiency and Openness")
    m
  })
  expect_identical(cr$title, "Introducing E‐Procurement in Bangladesh: The Promise of Efficiency and Openness")
  rec <- openalex_record()
  rec$title <- "Introducing E‐P rocurement in Bangladesh: The Promise of Efficiency and Openness"
  expect_identical(cgjrapp:::.verify_identity(rec, cr)$identity, "verified")
})

test_that("a DOI that does not exist is a conflict; an unanswered check leaves it unverified", {
  v <- cgjrapp:::.verify_identity(openalex_record(), crossref_for(status = 404L))
  expect_identical(v$identity, "conflict")
  expect_true("doi_not_found" %in% v$issues[[1]])
  u <- cgjrapp:::.verify_identity(openalex_record(), crossref_for(status = 503L))
  expect_identical(u$identity, "unverified")
  expect_true("identity_unchecked" %in% u$issues[[1]])
})

test_that("without a DOI, only the World Bank's own record counts as verified", {
  rows <- prwp_rows()
  rows$doi <- NA
  rows$wb_id[1] <- NA  # the OpenAlex row: no identifier Crossref could check
  r <- cgjrapp:::.as_sources(rows)
  v <- cgjrapp:::.verify_identity(r, cgjrapp:::.crossref_lookup(character(0), POLICY))
  expect_identical(v$identity, c("unverified", "verified"))
  expect_true("no_identifier" %in% v$issues[[1]])
})

test_that("providers that disagreed about a record leave it in conflict", {
  r <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows(oa_title = "Tax Morale in Ghana")))
  expect_identical(r$issues[[1]], "title_conflict")
  v <- cgjrapp:::.verify_identity(r, crossref_for())  # Crossref agrees with the World Bank
  expect_identical(v$identity, "conflict")
})

test_that("accents are folded even where the locale cannot transliterate", {
  expect_identical(cgjrapp:::.fold(c("Jürgen René", "Côte d'Ivoire", "plain")),
                   c("Jurgen Rene", "Cote d'Ivoire", "plain"))
  # a C locale makes iconv give "?" for letters: they are dropped, not kept as "?"
  testthat::local_mocked_bindings(iconv = function(x, ...) gsub("[^\\x01-\\x7F]", "?", x, perl = TRUE),
                                  .package = "base")
  expect_identical(cgjrapp:::.fold("Jürgen?"), "Jrgen?")
})
