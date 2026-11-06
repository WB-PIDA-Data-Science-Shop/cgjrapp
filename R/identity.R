# ---
# Publication identity: is a source the publication its metadata says it is?
#
# A DOI is checked against Crossref, the DOI registry: the record behind the
# DOI must exist and carry the same title, year and first author as the
# source. Crossref corroborates *which* publication a source is - it says
# nothing about whether the publication supports a claim (that is checked
# against its text, later). Crossref allows anonymous clients 5 requests a
# second, one at a time, so lookups go one by one with a pause between.
# ---

.CROSSREF_API <- "https://api.crossref.org/works/"

# DOIs -> one row each: `status` "found", "not_found" (the DOI does not
# exist) or why Crossref gave nothing ("unavailable", "rate_limited",
# "malformed" - not evidence against the DOI), and Crossref's `title`,
# `year`, `authors` (family / given / literal), `publisher`, `journal`,
# `type`. CGJR_CROSSREF_MAILTO, if set, identifies us to Crossref.
.crossref_lookup <- function(dois, policy, pause = Sys.sleep) {
  if (length(dois) == 0L) return(data.frame(doi = character(0), status = character(0)))
  mailto <- Sys.getenv("CGJR_CROSSREF_MAILTO")
  rows <- lapply(seq_along(dois), function(i) {
    if (i > 1L) pause(0.2)  # 5 requests a second at most
    res <- .provider_get(paste0(.CROSSREF_API, curl::curl_escape(dois[[i]])), policy,
                         list(mailto = if (nzchar(mailto)) mailto))
    .crossref_row(dois[[i]], res)
  })
  .bind_rows(rows)
}

# Records + Crossref's answers for their DOIs -> records with `identity`
# ("verified", "conflict", "unverified") and the reasons added to `issues`.
# A verified DOI's Crossref author names replace the record's: Crossref
# splits family and given names, which Chicago sorts by.
.verify_identity <- function(records, crossref) {
  cr <- crossref[match(records$doi, crossref$doi), , drop = FALSE]
  found <- cr$status %in% "found"
  # what does not match between each record and its DOI's Crossref record
  mismatch <- lapply(seq_len(nrow(records)), function(i) {
    if (!found[[i]]) return(character(0))
    c(if (!.same_title(records$title[[i]], cr$title[[i]])) "doi_title_mismatch",
      # online and print dates can fall in neighbouring years
      if (isTRUE(abs(records$year[[i]] - cr$year[[i]]) > 1L)) "doi_year_mismatch",
      if (!.same_first_author(records$authors[[i]], cr$authors[[i]])) "doi_author_mismatch")
  })
  not_found <- cr$status %in% "not_found"
  unchecked <- !is.na(records$doi) & !found & !not_found
  # without a DOI, the publisher's own record (the World Bank's) is its
  # identity; any other record has nothing independent to check it against
  no_id <- is.na(records$doi) & is.na(records$wb_id)
  mismatch[not_found] <- list("doi_not_found")
  mismatch[unchecked] <- list("identity_unchecked")
  mismatch[no_id] <- list("no_identifier")
  # providers already disagreed when their records were merged
  merged_conflict <- vapply(records$issues, function(x) any(x %in% c("title_conflict", "year_conflict")),
                            logical(1))
  records$identity <- ifelse(unchecked | no_id, "unverified",
                             ifelse(lengths(mismatch) > 0L | merged_conflict, "conflict", "verified"))
  records$issues <- Map(union, records$issues, mismatch)
  verified <- records$identity == "verified" & found
  records$authors[verified] <- cr$authors[verified]
  records$issues[verified] <- lapply(records$issues[verified], setdiff, "author_name_unsplit")
  records
}

# Titles match when equal ignoring case, punctuation, accents and every space
# - indexes that strip a title's markup can leave a space inside a word
# ("p rivate-sector").
.same_title <- function(a, b) {
  squeeze <- function(x) gsub(" ", "", .norm_title(.fold(x)), fixed = TRUE)
  isTRUE(squeeze(a) == squeeze(b))
}

# First authors match on family name (or institution) ignoring case and
# accents; with no authors on either side there is nothing to contradict.
.same_first_author <- function(a, b) {
  first <- function(x) if (NROW(x)) tolower(.fold(dplyr::coalesce(x$family[[1]], x$literal[[1]])))
  fa <- first(a); fb <- first(b)
  is.null(fa) || is.null(fb) || isTRUE(fa == fb)
}

# Accents off (an accented "u" becomes "u", and so on) for comparisons. Transliteration
# depends on the locale: where it fails (NA, or "?" for a letter, as in a C
# locale on a server), the non-ASCII letters are dropped instead.
.fold <- function(x) {
  y <- suppressWarnings(iconv(x, "UTF-8", "ASCII//TRANSLIT"))
  # more "?" than the original had: letters were lost (a title may itself
  # ask a question)
  marks <- function(s) lengths(regmatches(s, gregexpr("?", s, fixed = TRUE)))
  failed <- is.na(y) | marks(y) > marks(x)
  y[failed] <- gsub("[^\\x01-\\x7F]", "", x[failed], perl = TRUE)
  y
}

.crossref_row <- function(doi, res) {
  m <- if (identical(res$status, "ok")) res$body$message
  row <- data.frame(doi = doi, status = if (is.null(m)) res$status else "found",
                    # Crossref titles can carry markup: "<scp>meta-analysis</scp>"
                    title = .squish(gsub("<[^>]+>", "", .scalar(m$title))),
                    year = as.integer(.scalar(m$issued$`date-parts`[[1]])),
                    publisher = .scalar(m$publisher), journal = .scalar(m$`container-title`),
                    type = .scalar(m$type))
  # people have family and given names; an organisation as author, a name
  row$authors <- list(data.frame(
    family = vapply(m$author, function(a) .scalar(a$family), ""),
    given = vapply(m$author, function(a) .scalar(a$given), ""),
    literal = vapply(m$author, function(a) .scalar(a$name), "")))
  row
}
