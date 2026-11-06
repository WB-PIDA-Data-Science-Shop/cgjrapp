# Source records: normalising provider results, merging duplicates, stable
# S ids and CSL-JSON metadata. Synthetic records only; no network.

test_that("every way of writing a DOI gives the same key", {
  r <- cgjrapp:::.as_sources(provider_rows(
    "openalex", title = "A",
    doi = c("https://doi.org/10.1596/1813-9450-10390", "doi:10.1596/1813-9450-10390",
            " 10.1596/1813-9450-10390 ", "HTTPS://DX.DOI.ORG/10.1596/1813-9450-10390")))
  expect_identical(unique(r$doi), "10.1596/1813-9450-10390")
  expect_identical(unique(r$key), "doi:10.1596/1813-9450-10390")
})

test_that("without a DOI the key falls back to the World Bank id, then the OpenAlex id", {
  r <- cgjrapp:::.as_sources(provider_rows(
    c("wb_documents", "openalex"), title = c("A", "B"), doi = NA,
    wb_id = c("34034392", NA), openalex_id = c(NA, "https://openalex.org/W2128495427")))
  expect_identical(r$key, c("wb:34034392", "openalex:W2128495427"))
  expect_identical(r$openalex_id, c(NA, "W2128495427"))
})

test_that("an empty DOI or id counts as none, so it never joins unrelated records", {
  r <- cgjrapp:::.as_sources(provider_rows(
    "openalex", title = c("A", "B"), year = 2020L, doi = "", wb_id = " ", openalex_id = ""))
  expect_identical(r$doi, c(NA_character_, NA_character_))
  expect_match(r$key, "^title:")
  expect_identical(nrow(cgjrapp:::.dedupe_sources(r)), 2L)
})

test_that("with no identifier, the same title, year and first author give the same key", {
  r <- cgjrapp:::.as_sources(provider_rows(
    "mai_search", year = c(2023L, 2023L, 2022L),
    title = c("Introducing E-Procurement in Bangladesh:  The Promise of Efficiency and Openness",
              "introducing e-procurement in Bangladesh - the promise of efficiency and openness",
              "Introducing E-Procurement in Bangladesh: The Promise of Efficiency and Openness"),
    authors = list(people("Blum", "Jurgen Rene"), people("BLUM", "J. R."), people("Blum", "Jurgen Rene"))))
  expect_match(r$key, "^title:")
  expect_identical(r$key[[1]], r$key[[2]])
  expect_false(r$key[[1]] == r$key[[3]])  # another year: another publication
})

test_that("a record without a title is kept and flagged", {
  r <- cgjrapp:::.as_sources(provider_rows("mai_search", title = c("A", NA, "  "), year = 2020L))
  expect_identical(r$issues, list(character(0), "missing_title", "missing_title"))
})

test_that("records have every source column, whatever the provider gave", {
  r <- cgjrapp:::.as_sources(provider_rows("openalex", title = "A", journal = "World Development"))
  expect_true(all(names(cgjrapp:::.SOURCE_COLUMNS) %in% names(r)))
  expect_identical(r$providers, list("openalex"))
  expect_identical(r$journal, "World Development")
  empty <- cgjrapp:::.as_sources(provider_rows(character(0), title = character(0)))
  expect_identical(nrow(empty), 0L)
  expect_true(all(names(cgjrapp:::.SOURCE_COLUMNS) %in% names(empty)))
})

test_that("author names split into family and given names", {
  # the World Bank writes "Family, Given"
  expect_identical(cgjrapp:::.parse_names(c("Blum, Jurgen Rene", "Fazekas, Mihály"), "family_given"),
                   people(c("Blum", "Fazekas"), c("Jurgen Rene", "Mihály")))
  # OpenAlex gives display names: the last word is taken as the family name
  expect_identical(cgjrapp:::.parse_names(c("Ibne Mahmood", "Sushmita  Samaddar "), "display"),
                   people(c("Mahmood", "Samaddar"), c("Ibne", "Sushmita")))
})

# -- .dedupe_sources() ---

test_that("one publication found by two providers becomes one record, World Bank first", {
  r <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows()))
  expect_identical(nrow(r), 1L)
  expect_setequal(r$providers[[1]], c("openalex", "wb_documents"))
  expect_identical(r$title, PRWP_TITLE)                       # the World Bank's title
  expect_identical(r$authors[[1]]$given, "Jurgen Rene")       # and its author names
  expect_identical(r$abstract, "Abstract from OpenAlex.")     # a gap filled from OpenAlex
  expect_identical(c(r$wb_id, r$openalex_id), c("34034392", "W0000000001"))
  expect_identical(r$issues[[1]], character(0))
})

test_that("providers that disagree on a DOI's title or year flag a conflict", {
  title <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows(oa_title = "Tax Morale in Ghana")))
  expect_identical(title$issues[[1]], "title_conflict")
  year <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows(oa_year = 2019L)))
  expect_identical(year$issues[[1]], "year_conflict")
  expect_identical(year$year, 2023L)  # the World Bank's year is kept
})

test_that("a record without a DOI joins one with the same title, year and first author", {
  rows <- prwp_rows()
  rows$doi[rows$provider == "wb_documents"] <- NA
  r <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(rows))
  expect_identical(nrow(r), 1L)
  expect_identical(r$key, "doi:10.1596/1813-9450-10390")  # the strongest id of the two
  expect_identical(r$wb_id, "34034392")
})

test_that("a working paper and its later journal article stay two publications", {
  rows <- prwp_rows()
  # the article: another DOI, or no DOI but another year
  article <- rows[1, ]
  article$doi <- "10.1016/j.example.2025.100001"
  expect_identical(nrow(cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(rbind(rows, article)))), 2L)
  article$doi <- NA
  article$openalex_id <- "W0000000002"
  article$year <- 2025L
  expect_identical(nrow(cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(rbind(rows, article)))), 2L)
})

test_that("an mAI search lead only fills gaps in the record it matches", {
  wb <- prwp_rows()[2, ]
  lead <- provider_rows("mai_search", doi = NA, openalex_id = NA, wb_id = NA,
                        title = PRWP_TITLE, year = 2023L, kind = "report", abstract = NA,
                        url = "https://example.org/a-copy.pdf",
                        authors = list(people("Blum", "Jürgen")))
  r <- cgjrapp:::.dedupe_sources(rbind(cgjrapp:::.as_sources(lead), cgjrapp:::.as_sources(wb)))
  expect_identical(nrow(r), 1L)
  expect_identical(r$kind, "working_paper")                 # the World Bank's, not the lead's
  expect_identical(r$authors[[1]]$given, "Jurgen Rene")
  expect_identical(r$url, "https://example.org/a-copy.pdf")  # a gap the lead filled
  expect_setequal(r$providers[[1]], c("mai_search", "wb_documents"))
})

test_that("untitled records are never merged on their (empty) titles", {
  r <- cgjrapp:::.as_sources(provider_rows("mai_search", title = NA, year = 2020L,
                                           url = c("https://a.example", "https://b.example")))
  expect_identical(nrow(cgjrapp:::.dedupe_sources(r)), 2L)
})

# -- .number_sources() ---

test_that("a first run numbers sources S1, S2, ... in discovery order", {
  r <- cgjrapp:::.as_sources(provider_rows("openalex", title = c("B", "A", "C"),
                                           openalex_id = c("W3", "W1", "W2")))
  expect_identical(cgjrapp:::.number_sources(r)$id, c("S1", "S2", "S3"))
})

test_that("a later run keeps every earlier source's id and never reuses one", {
  first <- cgjrapp:::.number_sources(cgjrapp:::.as_sources(
    provider_rows("openalex", title = c("A", "B", "C"), openalex_id = c("W1", "W2", "W3"))))
  # W2 is gone, W4 is new, W3 now ranks first
  later <- cgjrapp:::.as_sources(provider_rows("openalex", title = c("C", "D", "A"),
                                               openalex_id = c("W3", "W4", "W1")))
  expect_identical(cgjrapp:::.number_sources(later, previous = first)$id, c("S3", "S4", "S1"))
})

test_that("a source keeps its id when a later run finds a stronger identifier for it", {
  # first run: the World Bank record alone (no DOI known); later: merged with
  # OpenAlex, which supplies the DOI - the key changes, the id must not
  wb_only <- prwp_rows()[2, ]
  wb_only$doi <- NA
  first <- cgjrapp:::.number_sources(cgjrapp:::.as_sources(wb_only))
  later <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows()))
  expect_false(identical(first$key, later$key))
  expect_identical(cgjrapp:::.number_sources(later, previous = first)$id, first$id)
})

# -- .sources_csl() ---

# The working paper as a complete World Bank record
prwp_record <- function() {
  r <- cgjrapp:::.dedupe_sources(cgjrapp:::.as_sources(prwp_rows()))
  r$institution <- "WBG"; r$collection <- "Policy Research Working Paper"; r$number <- "10390"
  r$publisher <- "World Bank"; r$place <- "Washington, DC"; r$date <- "2023-04-03"
  r$url <- "https://documents.worldbank.org/curated/en/099246304032338660"
  cgjrapp:::.number_sources(r)
}

test_that("a World Bank working paper becomes a CSL report in its series", {
  item <- cgjrapp:::.sources_csl(prwp_record())[[1]]
  expect_identical(item$id, "S1")
  expect_identical(item$type, "report")
  expect_identical(item$title, PRWP_TITLE)
  expect_identical(item$author, list(list(family = "Blum", given = "Jurgen Rene")))
  expect_identical(item$issued, list(`date-parts` = list(c(2023L, 4L, 3L))))
  expect_identical(item[c("collection-title", "number", "publisher", "publisher-place", "DOI", "URL")],
                   list(`collection-title` = "Policy Research Working Paper", number = "10390",
                        publisher = "World Bank", `publisher-place` = "Washington, DC",
                        DOI = "10.1596/1813-9450-10390",
                        URL = "https://documents.worldbank.org/curated/en/099246304032338660"))
})

test_that("an institution as author is one literal name", {
  r <- prwp_record()
  r$authors <- list(institution_author("World Bank"))
  expect_identical(cgjrapp:::.sources_csl(r)[[1]]$author, list(list(literal = "World Bank")))
})

test_that("a journal article carries its journal, volume, issue and pages", {
  r <- cgjrapp:::.number_sources(cgjrapp:::.as_sources(provider_rows(
    "crossref", doi = "10.1016/j.example.2025.100001", kind = "journal_article",
    title = "An Article", year = 2025L, journal = "World Development", volume = "120",
    issue = "3", pages = "117-123", authors = list(people("Doe", "Jane")))))
  item <- cgjrapp:::.sources_csl(r)[[1]]
  expect_identical(item$type, "article-journal")
  expect_identical(item[c("container-title", "volume", "issue", "page")],
                   list(`container-title` = "World Development", volume = "120",
                        issue = "3", page = "117-123"))
  expect_null(item$publisher)
})

test_that("an undated publication has no issued date; a year alone is a one-part date", {
  r <- prwp_record()
  r$date <- NA
  expect_identical(cgjrapp:::.sources_csl(r)[[1]]$issued, list(`date-parts` = list(2023L)))
  r$year <- NA_integer_
  expect_null(cgjrapp:::.sources_csl(r)[[1]]$issued)
})
