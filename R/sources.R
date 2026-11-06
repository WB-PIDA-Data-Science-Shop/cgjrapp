# ---
# Source records: one row per external publication, whichever provider found it.
#
# Discovery providers (World Bank Documents & Reports, Crossref, OpenAlex,
# mAI search) each return their own fields; .as_sources() puts them in one
# record shape and gives each a deterministic `key` from its strongest
# identifier - DOI, else World Bank document id, else OpenAlex id, else a
# fingerprint of title, year and first author.
# ---

# The record shape: every column with an empty prototype of its type. List
# columns hold several values per record: `authors` a data frame (family,
# given, literal), `issn` the journal's ISSNs, `providers` who found the
# record, `issues` machine-readable problems with it.
.SOURCE_COLUMNS <- list(
  key = character(0), doi = character(0), wb_id = character(0), openalex_id = character(0),
  kind = character(0), title = character(0), authors = list(), year = integer(0),
  date = character(0), institution = character(0), series = character(0),
  collection = character(0), number = character(0), project = character(0),
  publisher = character(0),
  place = character(0), journal = character(0), issn = list(),
  openalex_source_id = character(0), volume = character(0), issue = character(0),
  pages = character(0), url = character(0), pdf_url = character(0),
  text_url = character(0), abstract = character(0), language = character(0),
  is_retracted = logical(0), providers = list(), retrieved_at = character(0),
  issues = list())

# Provider rows (a data frame with a `provider` column and any record
# columns) -> records: missing columns filled with NA (empty for list
# columns), identifiers normalised, `key` and `issues` computed.
.as_sources <- function(df) {
  df$providers <- as.list(df$provider)
  df$provider <- NULL
  n <- nrow(df)
  for (col in setdiff(names(.SOURCE_COLUMNS), names(df))) {
    proto <- .SOURCE_COLUMNS[[col]]
    df[[col]] <- if (is.list(proto)) rep(list(character(0)), n) else proto[rep(NA_integer_, n)]
  }
  df$authors <- lapply(df$authors, function(a) if (NROW(a)) a else .no_authors())
  df$doi <- .normalize_doi(df$doi)
  df$wb_id <- .blank_na(df$wb_id)
  df$openalex_id <- .blank_na(sub("^https://openalex\\.org/", "", df$openalex_id))
  df$key <- .source_key(df)
  # keep what the provider flagged (e.g. "author_name_unsplit"), add our own
  df$issues <- Map(function(own, titled) union(as.character(own), if (!titled) "missing_title"),
                   df$issues, .has_title(df$title))
  df
}

.has_title <- function(x) !is.na(x) & nzchar(trimws(x))

# Providers send "" or " " for an unknown identifier; it must read as NA, or
# every record with a blank DOI would share the key "doi:".
.blank_na <- function(x) {
  x <- trimws(x)
  x[!nzchar(x)] <- NA
  x
}

# Person names -> an authors table. "family_given" is "Family, Given" (the
# World Bank's form). "display" is "Given Family" (OpenAlex's form): the last
# word is taken as the family name, which is wrong for some names, so a
# record whose names were parsed this way should carry "author_name_unsplit".
.parse_names <- function(x, format = c("family_given", "display")) {
  x <- trimws(gsub("\\s+", " ", x))
  if (match.arg(format) == "family_given") {
    family <- trimws(sub(",.*$", "", x))
    given <- trimws(sub("^[^,]*,?", "", x))
  } else {
    family <- sub("^.* ", "", x)
    given <- sub(" ?[^ ]*$", "", x)
  }
  given[!nzchar(given)] <- NA
  data.frame(family, given, literal = NA_character_)
}

.no_authors <- function() {
  data.frame(family = character(0), given = character(0), literal = character(0))
}

# The strongest identifier a record has, as "doi:...", "wb:..." or
# "openalex:..."; with none, "title:" and a fingerprint of the publication;
# untitled, "url:..."; with nothing at all, NA (a record never merged).
.source_key <- function(df) {
  f <- .id_forms(df)
  dplyr::coalesce(f$doi, f$wb, f$openalex, f$title, .prefixed("url:", df$url))
}

# Each identifier of each record in key form, strongest first; NA where a
# record lacks it.
.id_forms <- function(df) {
  list(doi = .prefixed("doi:", df$doi), wb = .prefixed("wb:", df$wb_id),
       openalex = .prefixed("openalex:", df$openalex_id),
       title = .prefixed("title:", .fingerprint(df)))
}

.prefixed <- function(prefix, x) {
  out <- paste0(prefix, x, recycle0 = TRUE)  # no records -> no keys, not "doi:"
  out[is.na(x)] <- NA
  out
}

# Title, year and first author's family name, ignoring case, punctuation and
# spacing - so one publication spelled two ways by two providers matches,
# while the same title in another year (a later edition) does not. NA for a
# record without a title.
.fingerprint <- function(df) {
  first <- vapply(df$authors, function(a) {
    if (NROW(a) == 0L) "" else tolower(dplyr::coalesce(a$family[[1]], a$literal[[1]]))
  }, character(1))
  fp <- vapply(paste(.norm_title(df$title), df$year, first, sep = "|"),
               function(x) substr(rlang::hash(x), 1L, 16L), character(1), USE.NAMES = FALSE)
  fp[!.has_title(df$title)] <- NA
  fp
}

.norm_title <- function(x) trimws(gsub("[^[:alnum:]]+", " ", tolower(x)))

# Whose metadata wins when providers describe the same publication: the
# publisher's own record first, then the DOI registry, then indexes, and an
# LLM search result last - it only ever fills gaps.
.PROVIDER_RANK <- c("wb_documents", "crossref", "openalex", "mai_search")

# One row per publication, in the order the first of its records appeared
# (discovery's ranking). Records are the same publication when they share a
# key, or the same title, year and first author while at least one of them
# has no DOI - two different DOIs are two publications (a working paper and
# its journal version, say), however alike their titles.
.dedupe_sources <- function(records) {
  n <- nrow(records)
  if (n == 0L) return(records)
  fp <- .fingerprint(records)
  group <- seq_len(n)
  for (i in seq_len(n)[-1L]) {
    earlier <- seq_len(i - 1L)
    # %in% TRUE: an NA key or fingerprint matches nothing
    same <- (records$key[earlier] == records$key[i]) %in% TRUE |
      ((fp[earlier] == fp[i]) %in% TRUE & (is.na(records$doi[earlier]) | is.na(records$doi[i])))
    if (any(same)) group[i] <- group[earlier[same][1L]]
  }
  do.call(rbind, lapply(unique(group), function(g) .merge_records(records[group == g, ])))
}

# Several records of one publication -> one. Each field comes from the
# best-ranked provider that has it; ISSNs, providers and issues are pooled.
.merge_records <- function(rows) {
  rows <- rows[order(match(vapply(rows$providers, `[[`, "", 1L), .PROVIDER_RANK)), ]
  out <- rows[1L, ]
  for (col in names(rows)) {
    v <- rows[[col]]
    # authors: the best-ranked non-empty table (which.max finds the first TRUE)
    out[[col]] <- if (col == "authors") list(v[[which.max(vapply(v, NROW, 1L) > 0L)]])
                  else if (is.list(v)) list(as.character(unique(unlist(v))))
                  else v[!is.na(v)][1L]
  }
  # providers disagreeing on what a DOI or id points to is a warning sign
  # (a wrong DOI, a mis-indexed record), kept for identity verification
  conflicts <- c(title_conflict = length(unique(stats::na.omit(.norm_title(rows$title)))) > 1L,
                 year_conflict = length(unique(stats::na.omit(rows$year))) > 1L)
  out$issues <- list(union(out$issues[[1]], names(conflicts)[conflicts]))
  out$key <- .source_key(out)
  out
}

# Stable ids S1, S2, ... A record found in `previous` (an earlier run's
# numbered records) by any of its identifiers keeps its id - even when this
# run found a stronger key for it; new records continue after the highest id
# used, so an id that dropped out is never given to another publication.
.number_sources <- function(records, previous = NULL) {
  id <- rep(NA_character_, nrow(records))
  if (!is.null(previous)) {
    known <- .aliases(previous)
    id <- vapply(.aliases(records), function(a) {
      hit <- which(vapply(known, function(k) any(a %in% k), logical(1)))
      if (length(hit)) previous$id[[hit[[1]]]] else NA_character_
    }, character(1))
  }
  start <- max(0L, as.integer(sub("^S", "", previous$id)))
  new <- is.na(id)
  id[new] <- paste0("S", start + seq_len(sum(new)))
  records$id <- id
  records
}

# Every identifier of each record, in key form ("doi:...", "wb:...",
# "openalex:...", "title:..."), as a list of character vectors.
.aliases <- function(records) {
  f <- .id_forms(records)
  lapply(seq_len(nrow(records)), function(i) {
    x <- vapply(f, `[[`, "", i)
    unname(x[!is.na(x)])
  })
}

# Numbered records -> CSL-JSON items (lists), the metadata a citation
# processor turns into author-date citations and the reference list. Fields
# a record lacks are left out rather than sent empty.
.sources_csl <- function(records) {
  lapply(seq_len(nrow(records)), function(i) {
    r <- records[i, ]
    item <- list(id = r$id, type = unname(.CSL_TYPE[r$kind]), title = r$title,
                 author = .csl_names(r$authors[[1]]), issued = .csl_date(r$date, r$year),
                 `container-title` = r$journal, volume = r$volume, issue = r$issue,
                 page = r$pages, `collection-title` = r$collection, number = r$number,
                 publisher = r$publisher, `publisher-place` = r$place, DOI = r$doi, URL = r$url)
    Filter(function(x) length(x) > 0L && !all(is.na(x)), item)
  })
}

# Working papers are CSL reports; the series name and number set them apart.
.CSL_TYPE <- c(journal_article = "article-journal", report = "report",
               working_paper = "report", book = "book", chapter = "chapter")

# An authors table -> CSL names: a person as family and given names, an
# institution (World Bank, IMF) as one literal name.
.csl_names <- function(authors) {
  lapply(seq_len(nrow(authors)), function(i) {
    a <- authors[i, ]
    if (!is.na(a$literal)) list(literal = a$literal)
    else Filter(Negate(is.na), list(family = a$family, given = a$given))
  })
}

# "2023-04-03" (or "2023-04", or only a year) -> CSL date-parts; NULL when
# the publication has no known date.
.csl_date <- function(date, year) {
  if (!is.na(date)) return(list(`date-parts` = list(as.integer(strsplit(date, "-")[[1]]))))
  if (!is.na(year)) return(list(`date-parts` = list(year)))
  NULL
}

# DOIs are case-insensitive and arrive as URLs, "doi:" strings or bare;
# all become the bare, lower-case form "10.xxxx/yyyy".
.normalize_doi <- function(x) {
  sub("^(https?://(dx\\.)?doi\\.org/|doi:)", "", .blank_na(tolower(x)))
}
