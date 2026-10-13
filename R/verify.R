# ---
# Deterministic checks on LLM text (AI Report and AI Chat).
#
# The model is told to cite dashboard facts as [E12] and guidance passages as
# [G1.2-3]. These checks back the "verified" claim - they are R code, not the
# model grading itself:
#   * every cited id exists;
#   * every figure in a sentence is supported by what that sentence cites -
#     numerically for evidence facts (CTF, comparator median, change: to
#     +/-0.0005; "X of N" ranks exactly), or verbatim in a cited fact or
#     passage;
#   * a sentence with figures but no citation is flagged;
#   * an author-year reference not in the guidance's reference list is
#     flagged (the model may only name papers the guidance cites);
#   * expected report sections that never appear are flagged.
# Text under a "Beyond the guidance" heading (chat) is labelled unverified
# for the reader, so its figures are not flagged as uncited.
# ---

.ID_RE <- "E\\d+|G[A-Z]{0,3}[0-9.]*-\\d+"
.CITE_RE <- "\\[([^][]*?\\b(?:E\\d+|G[A-Z]{0,3}[0-9.]*-\\d+)\\b[^][]*?)\\]"
.MARKER_RE <- "^\\s*[`*_]*\\[\\[\\s*section\\s*:\\s*([A-Za-z0-9_]+)\\s*\\]\\][`*_]*\\s*$"
.BEYOND_RE <- "(?i)beyond the guidance"

.cited_ids <- function(x) {
  groups <- regmatches(x, gregexpr(.CITE_RE, x, perl = TRUE))[[1]]
  as.character(unique(unlist(regmatches(groups, gregexpr(.ID_RE, groups, perl = TRUE)))))
}

# Text -> one row per sentence, with the report section (marker) it sits in
# and whether it is in a "Beyond the guidance" block.
.sentences <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  out <- list(); section <- NA_character_; beyond <- FALSE
  for (ln in lines) {
    m <- regmatches(ln, regexec(.MARKER_RE, ln, perl = TRUE))[[1]]
    if (length(m)) { section <- m[[2]]; next }
    if (grepl(.BEYOND_RE, ln, perl = TRUE)) beyond <- TRUE
    t <- trimws(gsub("^\\s*(#+|[-*+\u2022]|\\d+\\.)\\s+", "", ln))
    if (!nzchar(t)) next
    s <- strsplit(t, "(?<=[.!?])\\s+(?=[A-Z\\[(\"\u201c*])", perl = TRUE)[[1]]
    # a citation that trails its sentence ("... 0.655. [E3]") belongs to it
    keep <- character(0)
    for (piece in s) {
      if (length(keep) && grepl("^\\[", piece)) keep[[length(keep)]] <- paste(keep[[length(keep)]], piece)
      else keep <- c(keep, piece)
    }
    out[[length(out) + 1L]] <- data.frame(section = section, beyond = beyond,
                                          sentence = keep, stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(data.frame(section = character(), beyond = logical(),
                              sentence = character()), out)
}

# Figures a sentence states: decimals, "X of N" ranks, percentages. Citations,
# section numbers ("2.1.1", "section 2.1") and page refs are not figures.
.figures <- function(s) {
  s <- gsub(.CITE_RE, " ", s, perl = TRUE)
  s <- gsub("(?i)\\b(section|sections|table|box|p\\.|page|\u00a7)\\s*[0-9.]+", " ", s, perl = TRUE)
  s <- gsub("\\b\\d+(\\.\\d+){2,}\\b", " ", s, perl = TRUE)
  dec <- regmatches(s, gregexpr("(?<![\\w.])[+\\-\u2212]?\\d*\\.\\d+(?![\\w.%]|\\.\\d)", s, perl = TRUE))[[1]]
  rank <- regmatches(s, gregexpr("\\b\\d+(?:st|nd|rd|th)?\\s+(?:of|out of)\\s+\\d+\\b", s, perl = TRUE))[[1]]
  pct <- regmatches(s, gregexpr("\\d+(?:\\.\\d+)?\\s?(?:%|percent\\b)", s, perl = TRUE))[[1]]
  list(dec = trimws(dec), rank = trimws(rank), pct = trimws(pct))
}

.num <- function(x) as.numeric(gsub("[\u2212]", "-", gsub("\\s", "", x)))

# Is one figure supported by the facts / passages a sentence cites?
.figure_ok <- function(fig, type, facts, texts) {
  if (any(grepl(fig, texts, fixed = TRUE))) return(TRUE)
  if (is.null(facts) || nrow(facts) == 0L) return(FALSE)
  pick <- function(col) if (col %in% names(facts)) facts[[col]] else NULL
  if (type == "dec") {
    v <- c(pick("ctf"), pick("comp_median"), pick("delta"))
    v <- v[!is.na(v)]
    x <- .num(fig)
    return(any(abs(v - x) <= 5e-4) || any(abs(abs(v) - abs(x)) <= 5e-4))
  }
  if (type == "rank") {
    n <- as.integer(regmatches(fig, gregexpr("\\d+", fig))[[1]])
    pairs <- rbind(cbind(pick("rank"), pick("n_ranked")),
                   cbind(pick("rank_first"), pick("n_first")),
                   cbind(pick("rank_last"), pick("n_last")))
    return(!is.null(pairs) && any(pairs[, 1] == n[[1]] & pairs[, 2] == n[[2]], na.rm = TRUE))
  }
  if (type == "pct") {
    x <- .num(sub("\\s?(%|percent)$", "", fig))
    v <- 100 * c(pick("ctf"), pick("comp_median"), pick("delta"))
    return(any(abs(abs(v[!is.na(v)]) - x) <= 0.05))
  }
  FALSE
}

# Author-year references: "(Barro 1997)", "(Evans and Rauch 1999; North 1990)",
# "Evans and Rauch (1999)", "Acemoglu et al. (2001)".
.author_years <- function(s, stop_words = character(0)) {
  found <- list()
  inner <- regmatches(s, gregexpr("\\(([^()]*?(?:19|20)\\d{2}[a-z]?[^()]*)\\)", s, perl = TRUE))[[1]]
  for (grp in inner) {
    for (piece in strsplit(gsub("^\\(|\\)$", "", grp), ";")[[1]]) {
      m <- regmatches(piece, regexec("^\\s*(?:e\\.g\\.,?\\s*|see\\s+)?([A-Z][a-z][[:alpha:]'\u2019\\-]*).*?((?:19|20)\\d{2})", piece, perl = TRUE))[[1]]
      if (length(m)) found[[length(found) + 1L]] <- c(m[[2]], m[[3]])
    }
  }
  narr <- regmatches(s, gregexpr("([A-Z][a-z][[:alpha:]'\u2019\\-]*)(?:\\s+et al\\.?|\\s+(?:and|&)\\s+[A-Z][[:alpha:]'\u2019\\-]+)?,?\\s+\\(((?:19|20)\\d{2})[a-z]?\\)", s, perl = TRUE))[[1]]
  for (n in narr) {
    m <- regmatches(n, regexec("^([A-Z][[:alpha:]'\u2019\\-]+).*?((?:19|20)\\d{2})", n, perl = TRUE))[[1]]
    found[[length(found) + 1L]] <- c(m[[2]], m[[3]])
  }
  if (!length(found)) return(data.frame(name = character(), year = character()))
  df <- unique(as.data.frame(do.call(rbind, found), stringsAsFactors = FALSE))
  names(df) <- c("name", "year")
  df[!tolower(df$name) %in% tolower(stop_words), , drop = FALSE]
}

.ref_known <- function(name, year, refs) {
  if (is.null(refs) || nrow(refs) == 0L) return(FALSE)
  a <- .ascii_lower(name)
  any(refs$year == year & (startsWith(refs$first_author, a) | grepl(a, refs$first_author, fixed = TRUE)))
}

.REF_STOP <- c("In", "Since", "By", "From", "Between", "Until", "As", "The", "Table", "Figure",
               "Section", "Box", "Report", month.name, month.abb)

#' Verify citations and figures in LLM text
#'
#' @param text Character scalar - the model's Markdown (report or chat reply).
#' @param evidence Output of [build_evidence()], or `NULL` (no selection).
#' @param guidance Output of [load_guidance()].
#' @param expected_sections Report section keys that must appear as
#'   `[[section:key]]` markers (`NULL` for chat).
#' @return A list: `issues` (data frame: `section`, `type`, `detail`,
#'   `sentence`; types `unknown_evidence_id`, `unknown_guidance_id`,
#'   `figure_not_supported`, `uncited_figure`, `external_reference`,
#'   `missing_section`) and `cited` (data frame: `id`, `source` -
#'   `"evidence"`/`"guidance"`, `found`).
#' @export
verify_text <- function(text, evidence = NULL, guidance = NULL,
                        expected_sections = NULL) {
  facts <- evidence$facts
  passages <- guidance$passages
  stop_words <- c(.REF_STOP, cgjrdata::cgjr_ctf$unit_name |> unique() |>
                    strsplit("\\s+") |> vapply(`[[`, character(1), 1L))
  sents <- .sentences(text)
  issues <- list(); cited <- list()
  add <- function(section, type, detail, sentence) {
    issues[[length(issues) + 1L]] <<- data.frame(section = section, type = type,
                                                 detail = detail, sentence = sentence)
  }

  for (i in seq_len(nrow(sents))) {
    s <- sents$sentence[[i]]; sec <- sents$section[[i]]
    ids <- .cited_ids(s)
    e_ids <- ids[startsWith(ids, "E")]; g_ids <- ids[startsWith(ids, "G")]
    e_ok <- !is.null(facts) & e_ids %in% facts$id
    g_ok <- !is.null(passages) & g_ids %in% passages$id
    if (length(ids)) cited[[length(cited) + 1L]] <- data.frame(
      id = c(e_ids, g_ids), source = rep(c("evidence", "guidance"), c(length(e_ids), length(g_ids))),
      found = c(e_ok, g_ok))
    for (id in e_ids[!e_ok]) add(sec, "unknown_evidence_id",
      if (is.null(facts)) sprintf("%s cited but no dashboard selection is loaded", id)
      else sprintf("%s is not in the evidence register", id), s)
    for (id in g_ids[!g_ok]) add(sec, "unknown_guidance_id",
      sprintf("%s is not a passage of the guidance", id), s)

    figs <- .figures(s)
    n_figs <- length(unlist(figs))
    if (n_figs && !length(ids)) {
      if (!sents$beyond[[i]]) add(sec, "uncited_figure",
        paste("Figure(s) with no citation:", paste(unlist(figs), collapse = ", ")), s)
    } else if (n_figs) {
      f <- if (length(e_ids[e_ok])) facts[facts$id %in% e_ids, , drop = FALSE]
      texts <- c(f$text, passages$text[passages$id %in% g_ids])
      for (type in names(figs)) for (fig in figs[[type]]) {
        if (!.figure_ok(fig, type, f, texts)) add(sec, "figure_not_supported",
          sprintf("%s is not in the cited source(s) %s", fig, paste(ids, collapse = ", ")), s)
      }
    }

    ay <- .author_years(s, stop_words)
    for (j in seq_len(nrow(ay))) {
      if (!.ref_known(ay$name[[j]], ay$year[[j]], guidance$refs)) add(sec, "external_reference",
        sprintf("%s (%s) is not in the guidance's reference list", ay$name[[j]], ay$year[[j]]), s)
    }
  }

  if (length(expected_sections)) {
    seen <- unique(sents$section[!is.na(sents$section)])
    for (k in setdiff(expected_sections, seen)) add(k, "missing_section",
      sprintf("Section '%s' was not written", k), NA_character_)
  }

  list(
    issues = dplyr::bind_rows(data.frame(section = character(), type = character(),
                                         detail = character(), sentence = character()), issues),
    cited = unique(dplyr::bind_rows(data.frame(id = character(), source = character(),
                                               found = logical()), cited))
  )
}

#' One-line summary of a verification result
#'
#' @param v Output of [verify_text()].
#' @return Character scalar, e.g. "14 citations checked, all found; no issues."
#' @keywords internal
verification_summary <- function(v) {
  n <- nrow(v$cited); bad <- sum(!v$cited$found)
  head <- if (n == 0L) "No citations." else if (bad == 0L)
    sprintf("%d citation%s checked, all found in the sources.", n, if (n == 1L) "" else "s")
  else sprintf("%d citation%s checked, %d not found.", n, if (n == 1L) "" else "s", bad)
  other <- v$issues[!v$issues$type %in% c("unknown_evidence_id", "unknown_guidance_id"), ]
  if (nrow(other) == 0L) return(paste(head, "No other issues."))
  labels <- c(figure_not_supported = "figure(s) not matching the cited source",
              uncited_figure = "uncited figure(s)",
              external_reference = "reference(s) not in the guidance",
              missing_section = "missing section(s)")
  tab <- table(other$type)
  paste(head, paste(sprintf("%d %s", tab, labels[names(tab)]), collapse = "; "), "flagged.")
}

# HTML from Markdown with each citation id as a badge whose tooltip is its
# source (the fact sentence, or the guidance section, page and passage).
.render_cited_html <- function(md, evidence = NULL, guidance = NULL) {
  html <- commonmark::markdown_html(md %||% "", extensions = TRUE)
  facts <- evidence$facts; passages <- guidance$passages
  badge <- function(id) {
    if (startsWith(id, "E")) {
      j <- match(id, facts$id)
      tip <- if (is.na(j)) "Not found in the evidence register" else facts$text[[j]]
      cls <- if (is.na(j)) "cgjr-cite cgjr-cite-bad" else "cgjr-cite cgjr-cite-e"
    } else {
      j <- match(id, passages$id)
      tip <- if (is.na(j)) "Not found in the guidance" else sprintf(
        "Guidance section %s (%s), p.%d: %s", passages$section[[j]], passages$title[[j]],
        passages$page[[j]], substr(passages$text[[j]], 1L, 400L))
      cls <- if (is.na(j)) "cgjr-cite cgjr-cite-bad" else "cgjr-cite cgjr-cite-g"
    }
    sprintf('<span class="%s" title="%s">%s</span>', cls, htmltools::htmlEscape(tip, TRUE), id)
  }
  m <- gregexpr(.CITE_RE, html, perl = TRUE)
  regmatches(html, m) <- list(vapply(regmatches(html, m)[[1]], function(g) {
    ids <- regmatches(g, gregexpr(.ID_RE, g, perl = TRUE))[[1]]
    paste(vapply(ids, badge, character(1)), collapse = " ")
  }, character(1)))
  html
}
