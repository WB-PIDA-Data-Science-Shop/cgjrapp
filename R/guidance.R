# ---
# The CGJR guidance note as citable passages.
#
# The AI Report and AI Chat follow the CGJR governance guidance note. The note
# is Official Use Only, so it is never committed or sent to a non-approved
# LLM: by default the app loads a synthetic STAND-IN with the same section
# structure (inst/guidance_standin/), and the real note is opt-in
# (CGJR_GUIDANCE_SOURCE = "real", for WBG mAI only) from a gitignored rds
# built by data-raw/guidance_passages.R.
#
# Both go through split_guidance(): a page-by-page text dump -> passages with
# stable ids (G1.2-3 = section 1.2, third passage), the section, its title and
# the page, plus the reference list. Every [G...] citation the LLM makes is
# checked against these ids (verify.R).
# ---

# Guidance section(s) behind each report section: taxonomy cluster and leaf
# keys, plus the chapter-level sections. Numbered codes are the note's own
# section numbers; unnumbered sections use the first three letters of their
# title (Overview -> OVE). Only numbers, never text from the note.
.GUIDANCE_MAP <- list(
  overview = c("OBJ", "WHY", "THE", "PRI"),
  institutional_environment = "1",
  degree_of_integrity = "1.1",
  transparency_and_accountability = "1.2",
  justice_and_rule_of_law = "1.3",
  core_governance_functions = "2",
  public_financial_management = "2.1",
  budget_cycle_and_fiscal_planning = "2.1.1",
  domestic_revenue_mobilization = "2.1.2",
  public_procurement = "2.1.3",
  public_investment_management = "2.1.4",
  public_sector_hrm = "2.2",
  digital_and_data = "2.3",
  beyond_core_governance_functions = "3",
  market_regulatory_institutions = "3.1",
  service_delivery = "3.2",
  soe_governance = "3.3",
  context = "4",
  political_institutions_and_social_cohesion = "4.1",
  social_cohesion_norms_and_cooperation = "4.2",
  recommendations = c("5", "BOX")
)

# Lines that are page furniture, not content: page numbers and running headers.
.GUIDANCE_DROP <- c("^\\s*\\d+\\s*$", "^\\s*Official Use Only\\s*$", "^\\s*STAND-IN\\b.*$")

.norm_title <- function(x) {
  x <- tolower(gsub("[^[:alnum:] ]", " ", x))
  trimws(gsub("\\s+", " ", x))
}

# "1.2. Transparency and Accountability" -> "1.2"; unnumbered -> first three
# letters of the title, upper case.
.section_code <- function(title) {
  num <- regmatches(title, regexpr("^\\d+(\\.\\d+)*", title))
  if (length(num) && nzchar(num)) return(num)
  toupper(substr(gsub("[^A-Za-z]", "", title), 1L, 3L))
}

# The contents page: "Title ........ 12" lines.
.parse_toc <- function(pages) {
  lines <- unlist(strsplit(pages, "\n", fixed = TRUE))
  toc <- grep("\\.{5,}", lines, value = TRUE)
  titles <- trimws(sub("\\s*\\.{5,}.*$", "", toc))
  titles <- gsub("\\s+", " ", titles)
  titles <- titles[nzchar(titles)]
  data.frame(title = titles, code = vapply(titles, .section_code, character(1)),
             norm = .norm_title(titles), stringsAsFactors = FALSE)
}

# Index of the contents title a line opens, or NA. Numbered titles must start
# the line with their number; a heading wrapped over two lines matches on its
# first line when that is most of the title.
.match_heading <- function(line, toc) {
  n <- .norm_title(line)
  if (!nzchar(n)) return(NA_integer_)
  hit <- which(toc$norm == n)
  if (!length(hit)) {
    numbered <- grepl("^\\d", toc$code)
    hit <- which(numbered & startsWith(toc$norm, n) & nchar(n) >= 0.6 * nchar(toc$norm))
  }
  if (length(hit)) hit[[1]] else NA_integer_
}

.table_kind <- function(caption) {
  c <- tolower(caption)
  if (grepl("jobs strategy pillars|critical assumption", c)) "pillars"
  else if (grepl("potential questions", c)) "questions"
  else if (grepl("recommendation", c)) "recommendations"
  else if (grepl("other chapters", c)) "chapter_links"
  else "table"
}

# Cut one block's text into passages of roughly 120-220 words, breaking at a
# bullet or a sentence end.
.chunk_block <- function(lines, pages, max_words = 180L) {
  out_text <- character(0); out_page <- integer(0)
  cur <- character(0); cur_page <- NA_integer_; words <- 0L
  flush <- function() {
    if (length(cur)) {
      out_text <<- c(out_text, trimws(gsub("\\s+", " ", paste(cur, collapse = " "))))
      out_page <<- c(out_page, cur_page)
    }
    cur <<- character(0); words <<- 0L; cur_page <<- NA_integer_
  }
  for (i in seq_along(lines)) {
    ln <- lines[[i]]
    is_break <- grepl("^\\s*[\u2022\\-]\\s", ln) || grepl("[.?!:]\\s*$", lines[max(i - 1L, 1L)])
    if (words >= max_words && is_break) flush()
    if (!length(cur)) cur_page <- pages[[i]]
    cur <- c(cur, ln)
    words <- words + length(strsplit(trimws(ln), "\\s+")[[1]])
  }
  flush()
  keep <- nzchar(out_text)
  list(text = out_text[keep], page = out_page[keep])
}

# Reference list -> first author (lower case, letters only) and year.
.parse_refs <- function(lines) {
  lines <- trimws(lines[nzchar(trimws(lines))])
  if (!length(lines)) return(data.frame(first_author = character(), year = character(),
                                        entry = character()))
  # an entry opens "Surname, I." or "Surname, Given," (or "World Bank ...")
  starts <- grepl("^[[:upper:]][[:alpha:]'\u2019 \\-]*,\\s+([[:upper:]]\\.|[[:upper:]][[:alpha:]]+,)",
                  lines) | grepl("^World Bank", lines)
  starts[[1]] <- TRUE
  entries <- vapply(split(lines, cumsum(starts)), paste, character(1), collapse = " ")
  entries <- gsub("\\s+", " ", entries)
  first <- sub("^([^,(]+?)\\s*[,(].*$", "\\1", entries)
  year <- vapply(entries, function(e) {
    m <- regmatches(e, regexpr("(19|20)\\d{2}", e)); if (length(m)) m else NA_character_
  }, character(1))
  data.frame(first_author = .ascii_lower(first), year = unname(year),
             entry = unname(entries), stringsAsFactors = FALSE)
}

.ascii_lower <- function(x) {
  tolower(gsub("[^A-Za-z]", "", iconv(x, to = "ASCII//TRANSLIT", sub = "")))
}

#' Split a guidance note into citable passages
#'
#' Turns a page-by-page text dump of the CGJR guidance note (or the stand-in)
#' into passages with stable ids. Sections are found from the note's own
#' contents page; after "Glossary of Data Sources" the numbered headings are
#' glossary subsections (`GLO1.1`), and the "References" list is parsed into
#' first author + year for the external-reference check.
#'
#' @param pages Character vector, one element per page.
#' @param drop Regexes for page-furniture lines to discard.
#' @return A list: `passages` (data frame: `id`, `section`, `title`, `kind`,
#'   `page`, `text`) and `refs` (data frame: `first_author`, `year`, `entry`).
#' @keywords internal
split_guidance <- function(pages, drop = .GUIDANCE_DROP) {
  toc <- .parse_toc(pages)
  if (nrow(toc) == 0L) stop("No contents page found in the guidance text.", call. = FALSE)

  recs <- list(); ref_lines <- character(0)
  sec <- NULL; mode <- "front"; kind <- "narrative"
  buf <- character(0); buf_pages <- integer(0)

  emit <- function() {
    if (!is.null(sec) && length(buf)) {
      ch <- .chunk_block(buf, buf_pages)
      if (length(ch$text)) {
        recs[[length(recs) + 1L]] <<- data.frame(
          section = sec$code, title = sec$title, kind = kind,
          page = ch$page, text = ch$text, stringsAsFactors = FALSE)
      }
    }
    buf <<- character(0); buf_pages <<- integer(0)
  }

  for (p in seq_along(pages)) {
    for (ln in strsplit(pages[[p]], "\n", fixed = TRUE)[[1]]) {
      if (grepl("\\.{5,}", ln) || !nzchar(trimws(ln))) next
      if (any(vapply(drop, grepl, logical(1), x = ln))) next
      h <- if (mode == "refs") NA_integer_ else .match_heading(ln, toc)
      if (!is.na(h)) {
        emit()
        code <- toc$code[[h]]
        title <- sub("^\\d+(\\.\\d+)*\\.?\\s+", "", toc$title[[h]])
        if (code == "GLO") { mode <- "glossary"; sec <- list(code = "GLO", title = title) }
        else if (code == "REF") { mode <- "refs"; sec <- NULL }
        else if (mode == "glossary") sec <- list(code = paste0("GLO", code),
                                                 title = paste("Glossary:", title))
        else { mode <- "body"; sec <- list(code = code, title = title) }
        kind <- if (mode == "glossary") "glossary" else if (code == "BOX") "box" else "narrative"
        next
      }
      if (mode == "refs") { ref_lines <- c(ref_lines, ln); next }
      if (mode != "glossary" && grepl("^\\s*Table \\d+\\.", ln)) {
        emit(); kind <- .table_kind(ln)
      }
      buf <- c(buf, trimws(ln)); buf_pages <- c(buf_pages, p)
    }
  }
  emit()

  passages <- dplyr::bind_rows(recs)
  if (nrow(passages)) {
    passages$id <- paste0("G", passages$section, "-",
                          stats::ave(seq_len(nrow(passages)), passages$section, FUN = seq_along))
    passages <- passages[, c("id", "section", "title", "kind", "page", "text")]
  }
  list(passages = passages, refs = .parse_refs(ref_lines))
}

#' Load the guidance corpus the AI features cite
#'
#' `CGJR_GUIDANCE_SOURCE` picks it: `"standin"` (default) is the synthetic
#' stand-in shipped with the package, safe for any LLM provider; `"real"` is
#' the Official Use Only note, for WBG mAI only, read from
#' `CGJR_GUIDANCE_PATH` or else `inst/docs/guidance_passages.rds` (built by
#' `data-raw/guidance_passages.R`, never committed).
#'
#' @param source `"standin"` or `"real"`; defaults to the env var.
#' The passages' saved embeddings (for the chat's retrieval) are attached when
#' their file exists next to them and still matches the passages:
#' `inst/guidance_standin/guidance_standin_embeddings.rds`, or
#' `<passages file>_embeddings.rds` for the real note. When they no longer
#' match, a message says so and retrieval uses keywords only.
#'
#' @return A list: `source`, `label` (for prompts and the UI), `passages`,
#'   `refs` (see [split_guidance()]) and `embeddings` (`NULL`, or a list with
#'   `model`, `fingerprint` and `vectors`).
#' @export
load_guidance <- function(source = Sys.getenv("CGJR_GUIDANCE_SOURCE", "standin")) {
  source <- match.arg(source, c("standin", "real"))
  if (source == "standin") {
    path <- system.file("guidance_standin", "guidance_standin.txt", package = "cgjrapp")
    txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    g <- split_guidance(strsplit(txt, "\n<<<PAGE>>>\n", fixed = TRUE)[[1]])
    g$label <- "STAND-IN guidance (synthetic text for testing, not the CGJR guidance note)"
    emb <- file.path(dirname(path), "guidance_standin_embeddings.rds")
  } else {
    path <- Sys.getenv("CGJR_GUIDANCE_PATH", "")
    if (!nzchar(path)) path <- system.file("docs", "guidance_passages.rds", package = "cgjrapp")
    if (!nzchar(path) || !file.exists(path)) {
      stop("CGJR_GUIDANCE_SOURCE is 'real' but the guidance passages file was not found. ",
           "Build it with data-raw/guidance_passages.R or set CGJR_GUIDANCE_PATH.",
           call. = FALSE)
    }
    g <- readRDS(path)
    g$label <- "CGJR Guidance Note for the Chapter on Governance and Public Institutions"
    emb <- sub("\\.rds$", "_embeddings.rds", path)
  }
  g$embeddings <- .read_embeddings(emb, g$passages)
  g$source <- source
  g
}

# Passages for a set of report-section keys (see .GUIDANCE_MAP), with the
# glossary entries of the same sections; a numbered code also takes its
# subsections ("2.1" -> "2.1.1" ...) only when `children = TRUE`.
.guidance_for <- function(guidance, keys, children = FALSE, glossary = TRUE) {
  codes <- unique(unlist(.GUIDANCE_MAP[keys], use.names = FALSE))
  s <- guidance$passages$section
  hit <- s %in% codes
  if (children) hit <- hit | Reduce(`|`, lapply(codes, function(cd) startsWith(s, paste0(cd, "."))), FALSE)
  if (glossary) {
    gl <- sub("^GLO", "", s)
    hit <- hit | (startsWith(s, "GLO") & (gl %in% codes |
      (children & Reduce(`|`, lapply(codes, function(cd) startsWith(gl, paste0(cd, "."))), FALSE))))
  }
  guidance$passages[hit, , drop = FALSE]
}

# The guidance's table of contents: one "- <section> <title>" line per section.
.guidance_contents <- function(passages) {
  s <- unique(passages[c("section", "title")])
  paste(sprintf("- %s %s", s$section, s$title), collapse = "\n")
}

# The prompt block for a set of passages.
.guidance_block <- function(passages) {
  if (is.null(passages) || nrow(passages) == 0L) return("(No guidance passages for this section.)")
  paste(sprintf("[%s] (section %s, %s, p.%d) %s", passages$id, passages$section,
                passages$title, passages$page, passages$text), collapse = "\n")
}
