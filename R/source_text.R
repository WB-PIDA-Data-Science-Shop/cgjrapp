# ---
# Reading sources: a publication's text, as passages with page numbers.
#
# A claim drawn from a publication must point to the passage it rests on,
# and a Chicago citation to its page - "(World Bank 2023, 37)" - so text is
# kept page by page. World Bank documents come as plain-text files split
# into pages by form feeds; other open publications as PDFs. Document text is
# data from outside: it is stored and searched, never acted on.
# ---

# One source (a row of records) -> the best text there is: the World Bank's
# plain-text file, else an open PDF (both fetched safely, .fetch_body()),
# else the abstract, else nothing. Returns a list: `access` ("full_text",
# "abstract_only" - claims may rest only on what the abstract says - or
# "metadata_only"), `method` ("wb_text", "pdf", "abstract"), `url` read,
# `passages` (.passages()), `fingerprint` (a hash of the passage texts),
# `fetched_at`, and `reason`: why any text that was tried could not be read.
.read_source <- function(record, policy, resolve = .resolve_host) {
  tried <- character(0)
  for (way in list(c(method = "wb_text", field = "text_url"), c(method = "pdf", field = "pdf_url"))) {
    url <- record[[way[["field"]]]]
    if (is.na(url)) next
    got <- .fetch_body(url, policy, resolve = resolve)
    pages <- if (identical(got$status, "ok")) .text_pages(got$body, got$content_type) else character(0)
    p <- .passages(pages)
    if (nrow(p)) return(.read_result("full_text", way[["method"]], url, p))
    tried <- c(tried, paste0(way[["method"]], ": ", if (identical(got$status, "ok")) "no text" else got$status))
  }
  if (!is.na(record$abstract) && nzchar(trimws(record$abstract))) {
    p <- data.frame(passage = 1L, file_page = NA_integer_, page = NA_character_,
                    text = .squish(record$abstract))
    return(.read_result("abstract_only", "abstract", NA_character_, p, tried))
  }
  .read_result("metadata_only", NA_character_, NA_character_, .passages(character(0)), tried)
}

.read_result <- function(access, method, url, passages, tried = character(0)) {
  list(access = access, method = method, url = url, passages = passages,
       fingerprint = rlang::hash(passages$text),
       fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       reason = if (length(tried)) paste(tried, collapse = "; ") else NA_character_)
}

# Reads a section's selected sources (cached: a document is downloaded once)
# and keeps each one's most relevant passages for the section. Returns
# list(sources, passages): `sources` with `access`, `access_method`,
# `access_reason`, `text_fingerprint` and `fetched_at` (NA for sources not
# selected); `passages` with the source's `key` and .passages()'s columns.
.read_sources <- function(sources, leaf, country, policy, cache = NULL, resolve = .resolve_host) {
  query <- paste(c(.section_label(leaf), policy$search$terms[[leaf]], country), collapse = " ")
  read <- lapply(seq_len(nrow(sources)), function(i) {
    if (!isTRUE(sources$selected[[i]])) return(NULL)
    r <- sources[i, ]
    # a read that hit a failure (a download down for now) is tried again; one
    # that fails outright leaves the source as metadata only, not citable for
    # claims, and the section's other sources are still read
    .cached(cache, list("text", r$key, r$text_url, r$pdf_url, r$abstract), function() {
      tryCatch(.read_source(r, policy, resolve), error = function(e) .read_result(
        "metadata_only", NA_character_, NA_character_, .passages(character(0)),
        paste("unreadable:", conditionMessage(e))))
    }, keep = function(x) is.na(x$reason))
  })
  field <- function(name) vapply(read, function(x) x[[name]] %||% NA_character_, character(1))
  sources$access <- field("access")
  sources$access_method <- field("method")
  sources$access_reason <- field("reason")
  sources$text_fingerprint <- field("fingerprint")
  sources$fetched_at <- field("fetched_at")
  kept <- lapply(which(!vapply(read, is.null, logical(1))), function(i) {
    x <- read[[i]]
    # an abstract is all there is: kept whole
    p <- if (x$access == "abstract_only") x$passages
         else .relevant_passages(x$passages, query, policy$limits$passages_per_source)
    cbind(key = rep(sources$key[[i]], nrow(p)), p)
  })
  none <- data.frame(key = character(0), passage = integer(0), file_page = integer(0),
                     page = character(0), text = character(0))
  list(sources = sources, passages = do.call(rbind, c(list(none), kept)))
}

# A source's passages ranked by the section's words (the BM25 keyword
# ranking the AI Chat uses for the guidance): the best `k` that share at
# least one word with `query`.
.relevant_passages <- function(passages, query, k) {
  if (nrow(passages) == 0L) return(passages)
  score <- .bm25(.tokens(passages$text), .tokens(query)[[1]])
  best <- order(-score)
  passages[utils::head(best[score[best] > 0], k), ]
}

# A downloaded document (raw bytes) -> its pages' text; character(0) for a
# content type that is not read here. Plain text is UTF-8 when its bytes are
# valid UTF-8, otherwise Windows-1252 (as some World Bank text files are).
.text_pages <- function(body, content_type) {
  if (identical(content_type, "application/pdf")) {
    return(.spell_ligatures(tryCatch(pdftools::pdf_text(body), error = function(e) character(0))))
  }
  if (!identical(content_type, "text/plain")) return(character(0))
  text <- rawToChar(body[body != as.raw(0)])  # NUL bytes (in old scanned reports) are no text
  text <- if (validUTF8(text)) enc2utf8(text) else iconv(text, "windows-1252", "UTF-8")
  .spell_ligatures(strsplit(gsub("\r\n?", "\n", text), "\f", fixed = TRUE)[[1]])
}

# Typeset text writes "fi" as one character (U+FB01) and so on: spelled out,
# words match a search and a quote matches its passage.
.spell_ligatures <- function(x) {
  for (l in names(.LIGATURES)) x <- gsub(l, .LIGATURES[[l]], x, fixed = TRUE)
  x
}
.LIGATURES <- stats::setNames(c("ff", "fi", "fl", "ffi", "ffl", "st", "st"),
                               intToUtf8(0xFB00:0xFB06, multiple = TRUE))  # U+FB00-U+FB06

# Pages -> passages of about 180 words: `passage` (1, 2, ...), `file_page`
# (position in the file), `page` (the printed page, or NA) and `text`. A
# passage never crosses a page break, so its page is exact for a citation;
# the line holding a page's printed number is left out of the text.
.passages <- function(pages) {
  printed <- .printed_pages(pages)
  chunks <- lapply(seq_along(pages), function(i) {
    lines <- strsplit(pages[[i]], "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (!is.na(printed[[i]])) lines <- lines[trimws(lines) != printed[[i]]]
    ch <- .chunk_block(lines, rep(i, length(lines)))
    data.frame(file_page = rep(i, length(ch$text)), text = ch$text)
  })
  p <- do.call(rbind, c(list(data.frame(file_page = integer(0), text = character(0))), chunks))
  data.frame(passage = seq_len(nrow(p)), file_page = p$file_page,
             page = printed[p$file_page], text = p$text)
}

# The page number printed on each page - Chicago cites the printed page, and
# file page 4 of a report is often its page 2 - or NA. A candidate is a
# lone number on a page's last line (a footer) or first line (a header); it
# is kept only where it fits the offset between file and printed pages that
# most numbered pages share (at least two, and at least half of them), so a
# stray number on a page is never taken for its page number.
.printed_pages <- function(pages) {
  number <- vapply(pages, function(p) {
    lines <- trimws(strsplit(p, "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines)]
    edge <- c(utils::tail(lines, 1L), utils::head(lines, 1L))  # footer first
    edge <- edge[grepl("^[0-9]{1,4}$", edge)]
    if (length(edge)) edge[[1]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  offset <- seq_along(pages) - suppressWarnings(as.integer(number))
  counts <- table(offset)
  if (!length(counts) || max(counts) < 2L || max(counts) < sum(counts) / 2) {
    return(rep(NA_character_, length(pages)))
  }
  ifelse(offset %in% as.integer(names(which.max(counts))), number, NA_character_)
}
