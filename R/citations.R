# ---
# Citations: research findings [S3.2] as author-date citations and the
# reference list, in the World Bank's style - the Editorial Style Guide 2020,
# which supplements Chicago (17th edition), with Chicago 18 where the guide
# is silent, and DOIs written as https://doi.org/ links.
#
# Written in R rather than as a CSL style rendered by pandoc: the Bank's
# rules differ from standard Chicago styles in several places, pandoc is not
# always present where the app runs, and the report cites only five kinds of
# publication. Output is Markdown (*italics* for titles and journals).
# ---

# The names an in-text citation gives: "Goldstein", "Scherer and Ross",
# "Garrett, Thomas, and Smith", and from four authors "Broadman et al." (the
# guide: "more than three authors"); an institution by its name.
.cite_names <- function(authors) {
  n <- dplyr::coalesce(authors$family, authors$literal)
  if (length(n) == 0L) return("")
  if (length(n) == 1L) return(n)
  if (length(n) == 2L) return(paste(n, collapse = " and "))
  if (length(n) == 3L) return(paste0(n[[1]], ", ", n[[2]], ", and ", n[[3]]))
  paste(n[[1]], "et al.")
}

# Each source's year as cited: "2019"; works by the same authors in the same
# year are lettered 2019a, 2019b ... in title order; no year is "n.d.".
.cite_years <- function(sources) {
  year <- ifelse(is.na(sources$year), "n.d.", as.character(sources$year))
  who <- paste(vapply(sources$authors, .cite_names, ""), year)
  letter <- stats::ave(seq_along(who), who, FUN = function(i) {
    if (length(i) == 1L) "" else letters[rank(tolower(sources$title[i]), ties.method = "first")]
  })
  paste0(year, letter)
}

# Text with each research citation - "[S1.1]", "[S1.1, S2.1]" - as an
# author-date citation: "(World Bank 2011, 37)", "(Debas 2019; Garrett,
# Thomas, and Smith 2019, 5)". Dashboard and guidance ids in the same
# brackets stay ("[E3] (World Bank 2011, 37)"); an id the pack does not hold
# is left as it is, for the checks to flag. `report` is the whole report
# when `text` is one part of it: year letters are worked out across the
# report, so every section and the reference list agree.
.render_citations <- function(text, pack, report = text) {
  s <- .cited_sources(report, pack)
  label <- stats::setNames(paste(vapply(s$authors, .cite_names, ""), .cite_years(s)), s$id)
  m <- gregexpr(.CITE_RE, text, perl = TRUE)
  regmatches(text, m) <- lapply(regmatches(text, m), function(groups) {
    vapply(groups, .cite_group, "", findings = pack$findings, label = label, USE.NAMES = FALSE)
  })
  text
}

# Text for readers: without the dashboard and guidance ids - "[E3]",
# "[G1.2-1]", "[E3, G1.2-1]" go, with the space before them. A bracket holding
# any other id (a research id the checks flagged as unknown) stays, so a
# flagged citation is not hidden.
.strip_ids <- function(text) {
  m <- gregexpr(paste0("\\s*", .CITE_RE), text, perl = TRUE)
  regmatches(text, m) <- lapply(regmatches(text, m), function(groups) {
    ids <- regmatches(groups, gregexpr(.ID_RE, groups, perl = TRUE))
    ifelse(vapply(ids, function(x) all(grepl("^[EG]", x)), logical(1)), "", groups)
  })
  text
}

# One bracket of ids -> its citation (see .render_citations()).
.cite_group <- function(group, findings, label) {
  ids <- regmatches(group, gregexpr(.ID_RE, group, perl = TRUE))[[1]]
  research <- ids[ids %in% findings$id]
  if (length(research) == 0L) return(group)
  f <- findings[match(research, findings$id), ]
  # one source: its label, then the pages of the findings cited from it
  parts <- vapply(unique(f$source_id), function(sid) {
    pages <- unique(stats::na.omit(f$page[f$source_id == sid]))
    paste(c(label[[sid]], if (length(pages)) paste(pages, collapse = ", ")), collapse = ", ")
  }, "")
  cite <- paste0("(", paste(sort(parts), collapse = "; "), ")")
  others <- setdiff(ids, research)
  if (length(others)) paste0("[", paste(others, collapse = ", "), "] ", cite) else cite
}

# One source (a row of records) as its reference-list entry; `year` is its
# cited year (.cite_years(): "2019a"). Following the guide's examples:
#   working paper  Names. 2018. "Title." Series 2725, World Bank, Washington, DC.
#   article        Names. 2016. "Title." *Journal* 24 (1): 77-96.
#   report, book   Names. 2018. *Title*. Series. Washington, DC: World Bank.
# then the DOI as an https://doi.org/ link, or else the URL.
.reference_entry <- function(record, year) {
  r <- record
  # outside text is written as it is: its * and _ never read as italics
  for (col in c("title", "journal", "collection", "publisher", "place")) r[[col]] <- .md_escape(r[[col]])
  # a title ending in a question or exclamation mark takes no period after it
  ends <- grepl("[?!]$", r$title)
  quoted <- paste0("\"", r$title, if (!ends) ".", "\"")
  body <- switch(r$kind,
    working_paper = paste0(quoted, " ", .join_given(c(.join_given(c(r$collection, r$number), " "),
                                                      r$publisher, r$place), ", "), "."),
    journal_article = paste0(quoted, " *", r$journal, "* ", r$volume,
                             if (!is.na(r$issue)) paste0(" (", r$issue, ")"),
                             if (!is.na(r$pages)) paste0(": ", .page_range(r$pages)), "."),
    paste0("*", r$title, "*", if (!ends) ".", if (!is.na(r$collection)) paste0(" ", r$collection, "."),
           " ", .join_given(c(r$place, r$publisher), ": "), "."))
  link <- if (!is.na(r$doi)) paste0(" https://doi.org/", r$doi, ".")
          else if (!is.na(r$url)) paste0(" ", r$url, ".")
  names <- .reference_names(r$authors[[1]])
  paste0(names, if (!endsWith(names, ".")) ".", " ", year, ". ", body, link)
}

# Text with Markdown's emphasis characters (and the backslash) escaped, so
# it shows as written; NA stays NA.
.md_escape <- function(x) gsub("([\\\\*_])", "\\\\\\1", x)

# Authors as a reference list gives them: every author, the first inverted -
# "Barth, James, Gerard Caprio Jr., and Ross Levine"; an institution by name.
.reference_names <- function(authors) {
  person <- function(i, inverted) {
    a <- authors[i, ]
    if (!is.na(a$literal)) return(a$literal)
    if (is.na(a$given)) return(a$family)
    if (inverted) paste0(a$family, ", ", a$given) else paste(a$given, a$family)
  }
  n <- nrow(authors)
  names <- vapply(seq_len(n), function(i) person(i, inverted = i == 1L), "")
  if (n <= 1L) return(paste(names, collapse = ""))
  paste0(paste(names[-n], collapse = ", "), ", and ", names[[n]])
}

# The parts that are given, joined by `sep`.
.join_given <- function(x, sep) paste(x[!is.na(x) & nzchar(x)], collapse = sep)

# Page ranges with an en dash, the second number shortened as Chicago does
# (and the guide's examples): 77-96, 225-35, 101-8, 1087-89, 1184-208; in
# full after a number under 100 or a multiple of 100 (100-104).
.page_range <- function(x) {
  vapply(x, function(r) {
    ab <- strsplit(r, paste0("[-", intToUtf8(0x2013), "]"))[[1]]
    if (length(ab) != 2L) return(r)
    a <- trimws(ab[[1]]); b <- trimws(ab[[2]])
    n <- suppressWarnings(as.integer(a))
    if (!is.na(n) && n >= 100L && n %% 100L != 0L && nchar(a) == nchar(b)) {
      same <- which(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])[1] - 1L  # shared leading digits
      if (!is.na(same)) {
        changed <- substring(b, same + 1L)
        # 101-109 keep only the changed digits; otherwise at least two
        b <- if (n %% 100L < 10L || nchar(changed) >= 2L) changed else substring(b, nchar(b) - 1L)
      }
    }
    paste0(a, intToUtf8(0x2013), b)
  }, "", USE.NAMES = FALSE)
}

# The reference list of a text: an entry for each source it cites, and only
# those, in alphabetical order (author, then year, then title).
.reference_list <- function(text, pack) {
  s <- .cited_sources(text, pack)
  if (nrow(s) == 0L) return(character(0))
  year <- .cite_years(s)
  entries <- vapply(seq_len(nrow(s)), function(i) .reference_entry(s[i, ], year[[i]]), "")
  first <- vapply(s$authors, function(a) tolower(dplyr::coalesce(a$family, a$literal)[1] %||% ""), "")
  entries[order(first, year, tolower(s$title))]
}

# The pack's sources that a text cites. Year letters (2019a, 2019b) are
# worked out among these only, so the text and its reference list agree.
.cited_sources <- function(text, pack) {
  cited <- unique(pack$findings$source_id[pack$findings$id %in% .cited_ids(text)])
  pack$sources[pack$sources$id %in% cited, ]
}
