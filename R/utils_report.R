# utils_report.R
# LLM prompt construction + Word formatting for the AI institutional chapter.
#
# The scores handed to the model are the LIVE per-selection composites
# (export_live_scores()) - screened + averaged for the chosen comparison
# group - not cgjrdata::cgjr_scores. The system prompt says so, and states the
# deliberate CGJR-vs-CLIAR taxonomy departures, so the model doesn't imply a
# cleaner correspondence than exists (build plan, Slice 5).

# -- Score formatting helpers ---

.fmt_score <- function(x) {
  if (is.na(x)) return("N/A")
  paste0(round(x * 100, 1), "%")
}

.fmt_score_row <- function(scores_named) {
  parts <- purrr::imap_chr(scores_named, function(val, nm) {
    paste0(nm, ": ", .fmt_score(val))
  })
  paste(parts, collapse = " / ")
}

# -- Taxonomy outline (generated, never hardcoded) ---

.taxonomy_outline <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  tx <- tx[order(tx$cluster_num, tx$subcluster_num,
                 dplyr::coalesce(tx$sub_subcluster_num, 0L)), ]
  clusters <- unique(tx$cluster)

  lines <- character(0)
  for (i in seq_along(clusters)) {
    crows <- tx[tx$cluster == clusters[[i]], ]
    lines <- c(lines, sprintf("%d. %s", i, toupper(crows$cluster_name[[1]])))
    for (sc in unique(crows$subcluster)) {
      srows <- crows[crows$subcluster == sc, ]
      if (all(is.na(srows$sub_subcluster))) {
        lines <- c(lines, sprintf("   - %s", srows$subcluster_name[[1]]))
      } else {
        lines <- c(lines, sprintf("   - %s:", srows$subcluster_name[[1]]))
        lines <- c(lines, sprintf("       - %s", srows$sub_subcluster_name))
      }
    }
  }
  paste(lines, collapse = "\n")
}

# -- Scores summariser ---

#' Summarise the live composite scores into a compact prompt block
#'
#' @param scores_tbl A tibble from [export_live_scores()] (`Cluster`, `Leaf`,
#'   `Composite`, `Indicators used`, `Indicators observed`; optional `Year`).
#' @return Character scalar - one bullet per leaf, grouped by cluster.
#' @noRd
.summarise_scores <- function(scores_tbl) {
  if (is.null(scores_tbl) || nrow(scores_tbl) == 0L) {
    return("(No scores available for this selection.)")
  }
  # latest year per leaf when a Year column is present (dynamic export)
  if ("Year" %in% names(scores_tbl)) {
    scores_tbl <- scores_tbl |>
      dplyr::group_by(.data$Cluster, .data$Leaf) |>
      dplyr::slice_max(.data$Year, n = 1L, with_ties = FALSE) |>
      dplyr::ungroup()
  }
  parts <- character(0)
  for (cl in unique(scores_tbl$Cluster)) {
    rows <- scores_tbl[scores_tbl$Cluster == cl, ]
    parts <- c(parts, paste0("**", cl, "**"))
    for (j in seq_len(nrow(rows))) {
      used <- rows$`Indicators used`[[j]]
      obs  <- rows$`Indicators observed`[[j]]
      cov  <- if (is.na(used) || used == 0L) " (no eligible indicators)"
              else sprintf(" (%d of %d indicators observed)", obs, used)
      parts <- c(parts, sprintf("- %s: %s%s",
                                rows$Leaf[[j]], .fmt_score(rows$Composite[[j]]), cov))
    }
  }
  paste(parts, collapse = "\n")
}

# -- System prompt ---

.cgjr_system_prompt <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  paste0(
    "You are an expert World Bank economist specialising in governance and ",
    "public institutions in developing countries. You are drafting the ",
    "institutional chapter of a Country Jobs and Growth Report (CGJR) from ",
    "quantitative Closeness-to-Frontier (CTF) scores.\n\n",

    "CTF scores run 0 to 1, where 1 is the global frontier (best observed ",
    "performance across ~230 countries). A leaf's composite score is the ",
    "simple mean of its member indicators' CTF values, computed live for the ",
    "specific comparison group selected: indicators the base unit has no data ",
    "for, and indicators that are flat across the comparison group, are ",
    "dropped first. Because of that screening the composite depends on the ",
    "comparison group and will not equal any precomputed figure.\n\n",

    "The CGJR taxonomy has four clusters:\n\n",
    .taxonomy_outline(taxonomy), "\n\n",

    "Deliberate departures from the CLIAR benchmarking taxonomy (do not ",
    "describe these leaves as identical to a CLIAR indicator family):\n",
    "- Justice and Rule of Law uses 16 of the 17 CLIAR legal-institutions ",
    "indicators (it omits one criminal-adjudication item).\n",
    "- Political Institutions and Social Cohesion merges CLIAR's political and ",
    "social-cohesion families into one 21-indicator leaf.\n",
    "- Social Cohesion, Norms and Cooperation is a 5-indicator subset of ",
    "CLIAR's political family.\n",
    "Some leaves (parts of Public Financial Management, SOE Governance) have ",
    "no CTF-eligible indicators yet - treat these as 'not yet measured', not ",
    "as a score of zero.\n\n",

    "Writing guidelines:\n",
    "- Be concise and evidence-based; cite the CTF scores provided.\n",
    "- Work cluster by cluster, then call out the 2-3 leaves furthest from the ",
    "frontier and from the comparators as priority reform areas.\n",
    "- Note trends where multiple years are given.\n",
    "- Do NOT invent scores or indicators not in the data provided.\n",
    "- No preamble, disclaimers or meta-commentary - begin with the analysis.\n",
    "- Use Markdown headings (## clusters, ### leaves or themes).\n",
    "- Around 800-1200 words."
  )
}

# -- User prompt builder ---

#' Build the LLM prompt for the CGJR institutional chapter
#'
#' @param base_name Character - display name of the base unit.
#' @param base_unit Character - its `unit_code`.
#' @param scores_tbl A tibble from [export_live_scores()].
#' @param year_range Integer length-2 vector.
#' @param ctf_type `"static"` or `"dynamic"` - labels the analysis basis.
#' @param comparators Character vector of comparator `unit_name`s (or codes).
#' @param taxonomy Passed through to the system prompt.
#'
#' @return A named list `list(system=, user=)`.
#' @export
build_cgjr_prompt <- function(base_name, base_unit, scores_tbl, year_range,
                              ctf_type = "static", comparators = character(0),
                              taxonomy = cgjrdata::cgjr_taxonomy) {
  scores_block <- .summarise_scores(scores_tbl)

  comparators_block <- if (length(comparators) > 0L) {
    paste(comparators, collapse = ", ")
  } else {
    "No comparators selected."
  }

  basis <- if (identical(ctf_type, "dynamic")) {
    sprintf("Dynamic CTF panel, even years %d-%d.", year_range[[1]], year_range[[2]])
  } else {
    "Static CTF cross-section (latest available data)."
  }

  user_text <- paste0(
    "Write the governance and public institutions chapter of the Country Jobs ",
    "and Growth Report for ", base_name, " (", base_unit, ").\n\n",
    "Analysis basis: ", basis, "\n\n",
    "COMPARISON GROUP: ", comparators_block, "\n\n",
    "LIVE COMPOSITE CLOSENESS-TO-FRONTIER SCORES (this comparison group):\n",
    scores_block, "\n\n",
    "Using the CGJR framework and only the scores above, write a structured ",
    "analysis covering every cluster. Prioritise the leaves where ", base_name,
    " is furthest from the frontier and from its comparison group. End with ",
    "2-3 concrete reform priorities."
  )

  list(system = .cgjr_system_prompt(taxonomy), user = user_text)
}

# -- Word document formatter ---

#' Format the AI-generated report as a Word document
#'
#' Markdown (LLM output) -> a simple `officer` `rdocx`: `##`/`###`/`#` become
#' Heading 2/3/1, list markers are stripped, everything else is Normal.
#'
#' @param report_text Character scalar - Markdown text from the LLM.
#' @param country_name Character scalar - for the title.
#' @param report_date Date/character - subtitle date. Defaults to [Sys.Date()].
#' @return An `officer` `rdocx` object.
#' @export
format_report_docx <- function(report_text, country_name,
                               report_date = Sys.Date()) {
  date_str <- format(as.Date(report_date), "%B %d, %Y")

  doc <- officer::read_docx() |>
    officer::body_add_par(
      paste0("Governance and Public Institutions: ", country_name),
      style = "heading 1"
    ) |>
    officer::body_add_par(paste0("AI-generated draft -- ", date_str),
                          style = "heading 2") |>
    officer::body_add_par(
      paste0(
        "DISCLAIMER: This text was generated by a large language model from ",
        "Closeness-to-Frontier scores. It is a draft starting point only and ",
        "must be reviewed and verified before use in any official document."
      ),
      style = "Normal"
    ) |>
    officer::body_add_par("", style = "Normal")

  for (line in strsplit(report_text, "\n", fixed = TRUE)[[1]]) {
    trimmed <- trimws(line)
    if (nchar(trimmed) == 0L) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
    } else if (startsWith(trimmed, "### ")) {
      doc <- officer::body_add_par(doc, sub("^### ", "", trimmed), style = "heading 3")
    } else if (startsWith(trimmed, "## ")) {
      doc <- officer::body_add_par(doc, sub("^## ", "", trimmed), style = "heading 2")
    } else if (startsWith(trimmed, "# ")) {
      doc <- officer::body_add_par(doc, sub("^# ", "", trimmed), style = "heading 1")
    } else {
      doc <- officer::body_add_par(doc, gsub("^[-*+]\\s+", "", trimmed), style = "Normal")
    }
  }
  doc
}
