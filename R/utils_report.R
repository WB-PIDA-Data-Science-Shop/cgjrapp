# utils_report.R
# Helpers for building LLM prompts and formatting the AI-generated Word report.
# The prompt structure follows the "Guidance Note for the Chapter on Governance
# and Public Institutions in the Country Growth and Jobs Report" framework.

# ── Score formatting helpers ──────────────────────────────────────────────────

# Format a numeric score (0-1) as a percentage string for the prompt
.fmt_score <- function(x) {
  if (is.na(x)) return("N/A")
  paste0(round(x * 100, 1), "%")
}

# Summarise a named numeric vector as "Name: XX% / Name: XX% / ..." for one year
.fmt_score_row <- function(scores_named) {
  parts <- purrr::imap_chr(scores_named, function(val, nm) {
    paste0(nm, ": ", .fmt_score(val))
  })
  paste(parts, collapse = " / ")
}

# ── Scores data summariser ────────────────────────────────────────────────────

#' Summarise scores data into a compact text block for the LLM prompt
#'
#' @param scores_tbl Tibble returned by [get_scores_table()].
#' @param primary_name Character scalar -- display name of the focal country.
#' @param score_col_map Named character vector mapping raw score column names to
#'   human-readable labels. Defaults to a hard-coded map of the 4 cluster scores
#'   plus overall.
#' @return Character scalar.
#' @noRd
.summarise_scores <- function(scores_tbl, primary_name,
                              score_col_map = NULL) {
  if (is.null(score_col_map)) {
    score_col_map <- c(
      overall_score                    = "Overall Score",
      institutional_environment_score  = "Institutional Environment",
      political_institutions_score     = "Political Institutions",
      center_of_government_score       = "Center of Government",
      sectors_service_delivery_score   = "Sectors / Service Delivery"
    )
  }

  # Latest available year for each entity
  latest_tbl <- scores_tbl |>
    dplyr::group_by(Entity) |>
    dplyr::slice_max(Year, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  present_cols <- intersect(names(score_col_map), names(latest_tbl))
  if (length(present_cols) == 0L) return("(No scores data available.)")

  lines <- character(0)
  for (i in seq_len(nrow(latest_tbl))) {
    row    <- latest_tbl[i, ]
    entity <- row$Entity
    yr     <- row$Year
    vals   <- purrr::set_names(
      as.numeric(row[present_cols]),
      score_col_map[present_cols]
    )
    score_str <- .fmt_score_row(vals)
    lines <- c(lines, paste0("- ", entity, " (", yr, "): ", score_str))
  }

  paste(lines, collapse = "\n")
}

# ── System prompt ─────────────────────────────────────────────────────────────

.cgjr_system_prompt <- function() {
  paste0(
    "You are an expert World Bank economist specialising in governance and public ",
    "institutions in developing countries. You are helping write the institutional ",
    "chapter of a Country Growth and Jobs Report (CGJR) based on quantitative ",
    "Closeness-to-Frontier (CTF) scores.\n\n",
    "CTF scores range from 0 to 1, where 1 represents the global frontier ",
    "(best performance). All scores are relative benchmarks across ~170 countries ",
    "across multiple years of data.\n\n",
    "Your analysis must follow the CGJR Guidance Note framework, organised around ",
    "four thematic areas:\n\n",
    "1. INSTITUTIONAL ENVIRONMENT (Degree of Integrity; Transparency and ",
    "Accountability; Justice and Rule of Law; Social Cohesion and Cooperation)\n",
    "2. POLITICAL INSTITUTIONS (Electoral systems, checks and balances, power ",
    "sharing, accountability)\n",
    "3. CENTER OF GOVERNMENT (Public Financial Management; Public Sector HRM; ",
    "Digital and Data)\n",
    "4. SECTORS / SERVICE DELIVERY (Business Environment; Service Delivery; SOE ",
    "Governance; Labor and Social Protection; Energy and Environment)\n\n",
    "Writing guidelines:\n",
    "- Be concise and evidence-based; cite the CTF scores provided.\n",
    "- Identify 2-3 priority reform areas based on the data.\n",
    "- Note trends over time where scores are provided for multiple years.\n",
    "- Compare the focal country to its peers, regional, and income-group benchmarks.\n",
    "- Use plain language suitable for a mixed technical/policy audience.\n",
    "- Do NOT add any preamble, disclaimers, or meta-commentary about the report ",
    "itself. Begin directly with the analytical content.\n",
    "- Structure the output with clear section headings using Markdown ",
    "(## for main sections, ### for subsections).\n",
    "- Limit the report to approximately 800-1200 words."
  )
}

# ── User prompt builder ───────────────────────────────────────────────────────

#' Build the user-side LLM prompt for the CGJR institutional chapter
#'
#' Combines the scores summary, peer/benchmark comparisons, and instructions
#' into a structured prompt.
#'
#' @param primary_iso Character scalar -- ISO3 code of the focal country.
#' @param primary_name Character scalar -- display name of the focal country.
#' @param scores_tbl Tibble from [get_scores_table()].
#' @param year_range Integer vector of length 2: `c(min_year, max_year)`.
#' @param peer_isos Character vector of peer ISO3 codes (may be empty).
#' @param region_codes Character vector of selected region codes (may be empty).
#' @param income_groups Character vector of selected income group labels (may
#'   be empty).
#'
#' @return A named list with elements `system` (character) and `user` (character).
#' @export
build_cgjr_prompt <- function(primary_iso, primary_name, scores_tbl,
                               year_range,
                               peer_isos     = character(0),
                               region_codes  = character(0),
                               income_groups = character(0)) {

  scores_block <- .summarise_scores(scores_tbl, primary_name)

  # Build a brief description of comparators
  comparators <- c(
    if (length(peer_isos) > 0L)    paste("Peer countries:", paste(peer_isos,     collapse = ", ")),
    if (length(region_codes) > 0L) paste("Regions:",        paste(region_codes,  collapse = ", ")),
    if (length(income_groups) > 0L) paste("Income groups:", paste(income_groups, collapse = ", "))
  )
  comparators_block <- if (length(comparators) > 0L) {
    paste(comparators, collapse = "\n")
  } else {
    "No comparators selected."
  }

  year_block <- paste0("Analysis period: ", year_range[[1]], " to ", year_range[[2]])

  user_text <- paste0(
    "Please write the governance and public institutions chapter for the ",
    "Country Growth and Jobs Report for ", primary_name, " (", primary_iso, ").\n\n",
    year_block, "\n\n",
    "COMPARATORS:\n", comparators_block, "\n\n",
    "CLOSENESS-TO-FRONTIER SCORES (most recent available year per entity):\n",
    scores_block, "\n\n",
    "Using the CGJR framework and the scores above, write a structured analysis ",
    "covering all four thematic areas. Prioritise the areas where ", primary_name,
    " shows the largest gaps from the frontier and from its comparators. ",
    "Suggest 2-3 concrete reform priorities."
  )

  list(
    system = .cgjr_system_prompt(),
    user   = user_text
  )
}

# ── Word document formatter ───────────────────────────────────────────────────

#' Format the AI-generated report as a Word document
#'
#' Converts Markdown text (as produced by the LLM) into a simple Word document
#' using `officer`. Sections demarcated by `##` headings are rendered as
#' Heading 2; `###` headings as Heading 3; all other lines as Normal paragraphs.
#'
#' @param report_text Character scalar -- Markdown text from the LLM.
#' @param country_name Character scalar -- country name for the title.
#' @param report_date Date or character -- date for the subtitle. Defaults to
#'   [Sys.Date()].
#'
#' @return An `officer` `rdocx` object ready to be written with
#'   [officer::print.rdocx()] or passed to `print(doc, target = path)`.
#' @export
format_report_docx <- function(report_text, country_name,
                                report_date = Sys.Date()) {
  date_str <- format(as.Date(report_date), "%B %d, %Y")

  doc <- officer::read_docx() |>
    officer::body_add_par(
      paste0("Governance and Public Institutions: ", country_name),
      style = "heading 1"
    ) |>
    officer::body_add_par(
      paste0("AI-generated draft -- ", date_str),
      style = "heading 2"
    ) |>
    officer::body_add_par(
      paste0(
        "DISCLAIMER: This text was generated by a large language model ",
        "based on Closeness-to-Frontier scores. It is intended as a ",
        "draft starting point only and must be reviewed and verified ",
        "before use in any official document."
      ),
      style = "Normal"
    ) |>
    officer::body_add_par("", style = "Normal")

  # Parse Markdown lines into officer paragraphs
  lines <- strsplit(report_text, "\n", fixed = TRUE)[[1]]

  for (line in lines) {
    trimmed <- trimws(line)
    if (nchar(trimmed) == 0L) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
    } else if (startsWith(trimmed, "### ")) {
      doc <- officer::body_add_par(doc,
        sub("^### ", "", trimmed), style = "heading 3")
    } else if (startsWith(trimmed, "## ")) {
      doc <- officer::body_add_par(doc,
        sub("^## ", "", trimmed), style = "heading 2")
    } else if (startsWith(trimmed, "# ")) {
      doc <- officer::body_add_par(doc,
        sub("^# ", "", trimmed), style = "heading 1")
    } else {
      # Strip leading Markdown list markers for Word output
      clean <- gsub("^[-*+]\\s+", "", trimmed)
      doc   <- officer::body_add_par(doc, clean, style = "Normal")
    }
  }

  doc
}
