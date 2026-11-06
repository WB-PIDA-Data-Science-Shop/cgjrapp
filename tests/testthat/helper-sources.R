# Provider rows for the source-record tests. `...` gives scalar columns
# (recycled); `authors` and `issn` are list columns when given.
provider_rows <- function(provider, ..., authors = NULL, issn = NULL) {
  df <- data.frame(provider, ...)
  if (!is.null(authors)) df$authors <- authors
  if (!is.null(issn)) df$issn <- issn
  df
}

# An authors table: people as family and given names, or an institution as a
# literal name.
people <- function(family, given) data.frame(family, given, literal = NA_character_)
institution_author <- function(name) data.frame(family = NA_character_, given = NA_character_,
                                                literal = name)

# One public working paper as two providers describe it (OpenAlex id is synthetic)
PRWP_TITLE <- "Introducing E-Procurement in Bangladesh: The Promise of Efficiency and Openness"
prwp_rows <- function(oa_title = toupper(PRWP_TITLE), oa_year = 2023L) {
  rbind(
    provider_rows("openalex", doi = "https://doi.org/10.1596/1813-9450-10390",
                  openalex_id = "W0000000001", wb_id = NA, title = oa_title, year = oa_year,
                  kind = "working_paper", abstract = "Abstract from OpenAlex.",
                  authors = list(people("Blum", "J. R."))),
    provider_rows("wb_documents", doi = "10.1596/1813-9450-10390", openalex_id = NA,
                  wb_id = "34034392", title = PRWP_TITLE, year = 2023L, kind = "working_paper",
                  abstract = NA, authors = list(people("Blum", "Jurgen Rene"))))
}
