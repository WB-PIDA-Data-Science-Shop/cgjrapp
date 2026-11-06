# ---
# AI Report: an LLM-drafted chapter that follows the CGJR guidance note.
#
# The structure is the guidance's own - Overview (theory of change, where the
# country stands, prioritization), one chapter per cluster with, for every
# leaf, what the data show / why it matters for growth and jobs / questions
# for the country team, then possible recommendations. The model writes the
# text only: every figure comes from the evidence pack ([E..], report_evidence.R)
# and every guidance-derived claim cites a passage ([G..], guidance.R); the
# priority order is priority_areas()'s, not the model's; verify_text() checks
# the result. One LLM call per cluster (plus overview and recommendations),
# each carrying only that part of the guidance and evidence.
# ---

# -- Taxonomy outline (generated, never hardcoded) ---

.taxonomy_outline <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  tx <- tx[order(tx$cluster_num, tx$subcluster_num,
                 dplyr::coalesce(tx$sub_subcluster_num, 0L)), ]
  lines <- character(0)
  for (cl in unique(tx$cluster)) {
    crows <- tx[tx$cluster == cl, ]
    lines <- c(lines, sprintf("- %s", crows$cluster_name[[1]]))
    for (sc in unique(crows$subcluster)) {
      srows <- crows[crows$subcluster == sc, ]
      lines <- c(lines, sprintf("  - %s", srows$subcluster_name[[1]]))
      if (!all(is.na(srows$sub_subcluster))) {
        lines <- c(lines, sprintf("    - %s", srows$sub_subcluster_name))
      }
    }
  }
  paste(lines, collapse = "\n")
}

# -- Scopes ---

#' Scope choices for the AI Report
#'
#' @param taxonomy `cgjrdata::cgjr_taxonomy`.
#' @return A named list for [shiny::selectInput()]: the full chapter, each
#'   cluster (`"cluster:<key>"`) and each section (`"leaf:<key>"`).
#' @export
report_scope_choices <- function(taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  tx <- tx[order(tx$cluster_num, tx$subcluster_num,
                 dplyr::coalesce(tx$sub_subcluster_num, 0L)), ]
  tx$leaf <- .leaf_key(tx); tx$leaf_name <- .leaf_label(tx)
  cl <- unique(tx[, c("cluster", "cluster_name")])
  lf <- unique(tx[, c("leaf", "leaf_name")])
  list(
    "Whole report" = c("Full chapter" = "chapter"),
    "One cluster" = stats::setNames(paste0("cluster:", cl$cluster), cl$cluster_name),
    "One section" = stats::setNames(paste0("leaf:", lf$leaf), lf$leaf_name)
  )
}

.parse_scope <- function(scope, taxonomy = cgjrdata::cgjr_taxonomy) {
  if (identical(scope, "chapter")) return(list(type = "chapter", key = NULL, cluster = NULL))
  parts <- strsplit(scope, ":", fixed = TRUE)[[1]]
  stopifnot("Unknown report scope" = length(parts) == 2L && parts[[1]] %in% c("cluster", "leaf"))
  cluster <- if (parts[[1]] == "cluster") parts[[2]] else {
    tx <- as.data.frame(taxonomy)
    unique(tx$cluster[.leaf_key(tx) == parts[[2]]])
  }
  list(type = parts[[1]], key = parts[[2]], cluster = cluster)
}

# -- Prompt pieces ---

# Rule 8 of the system prompt: how a published CGJR writes - the spec, with
# the reports it was drawn from and the evidence for each rule, is
# docs/writing-style.md. Keep the two in step.
.STYLE_RULES <- paste0(
  "8. Write as a published CGJR does, for World Bank country teams:\n",
  "- Open each paragraph with one bold sentence stating its main point, then 2-4 sentences ",
  "of evidence, e.g. '**Recruitment is the weakest area of public sector HRM [E3].** Its CTF ",
  "of 0.306 ranks 16 of 22, below the comparators' median of 0.447 [E3].' Every sentence that ",
  "states a figure cites it in that sentence, even when the bold sentence cites the same ",
  "fact. Headings and bullet points take no bold.\n",
  "- Set each figure against the comparison group (its band, its rank, the comparators' ",
  "median) rather than stating it alone.\n",
  "- Be balanced: where the evidence shows a strength as well as a weakness, state the ",
  "strength first, then the weakness ('However, ...').\n",
  "- Keep sentences to about 25 words, rarely over 35; plain words; the active voice.\n",
  "- Spell out an abbreviation the first time, with it in parentheses: 'public financial ",
  "management (PFM)'. Write 'percent', not '%'; 'the government' in lower case; never ",
  "'we' or 'our'.\n",
  "- State what a reform could do, never what it will do: a reform 'could' or 'can' ",
  "improve an outcome.\n",
  "- Begin each recommendation with a verb ('Strengthen ...', 'Publish ...') and keep it ",
  "as specific as its GUIDANCE passage; give no timeframe the passage does not give.\n",
  "- A neutral, analytical register: no preamble, no closing remarks, no disclaimers, no ",
  "top-level headings.\n")

.report_system_prompt <- function(meta, guidance, research = FALSE) {
  research_source <- if (research) paste0(
    "- RESEARCH findings, cited as [S...] - findings from published research, each checked ",
    "against its source's text, with its claim type and whose experience it describes.\n")
  research_rules <- if (research) paste0(
    "\n10. Every statement drawn from published research must cite its RESEARCH finding, e.g. ",
    "[S3.2]. Name a study or its authors only as a RESEARCH finding gives them.\n",
    "11. Keep each finding's claim type: state a descriptive finding as an observation or ",
    "an association, never as a cause; use causal language only for a finding marked causal.\n",
    "12. Keep each finding's scope: a finding from another country or from several countries ",
    "is evidence from elsewhere - say so, and never present it as a fact about ", meta$base_name, ".\n",
    "13. Copy a finding's figures exactly as it states them. Dashboard figures [E] and ",
    "research figures [S] are different evidence: never present one as the other.")
  paste0(
    "You are drafting the Governance and Public Institutions chapter of a World Bank ",
    "Country Growth and Jobs Report (CGJR) for ", meta$base_name, ". Follow the CGJR ",
    "guidance note strictly: its structure, its theory of change and three jobs-strategy ",
    "pillars (Building the Foundation, Improving the Business Environment, Mobilizing ",
    "Private Capital), and its diagnostic questions. The guidance you are given is: ",
    guidance$label, ".\n\n",

    "You receive two kinds of sources, every item with an id:\n",
    "- GUIDANCE passages, cited as [G...] - excerpts of the guidance note.\n",
    "- EVIDENCE facts, cited as [E...] - figures computed by the GOVCGJR dashboard ",
    "for this selection.\n", research_source, "\n",

    "Rules:\n",
    "1. Every figure you state must be copied exactly as written in an EVIDENCE fact and ",
    "cited with its id, e.g. 'a CTF score of 0.655 [E3]'. Never compute, round, convert ",
    "to percentages, or state a figure that is not in the evidence.\n",
    "2. Every statement about why an area matters, how it links to growth and jobs, ",
    "which questions to ask, which data sources to use, or which reforms to consider ",
    "must come from a GUIDANCE passage and cite it, e.g. [G1.2-1].\n",
    "3. Cite only ids listed below. Combine ids in one bracket when needed: [E3, G1.2-1]. ",
    "List every id: never a range such as [E3-E6].\n",
    "4. Do not name any study, author, dataset, tool or institution that does not appear ",
    "in the passages given. Do not add facts about the country from your own knowledge.\n",
    "5. Areas listed as NOT YET MEASURED have no indicators: say so; never describe them ",
    "as weak or strong.\n",
    "6. CTF (closeness to frontier) runs from 0 to 1, where 1 is the best performance ",
    "observed globally. Each fact gives the band (Weak / Emerging / Strong, relative to ",
    "the comparison group), the rank within the base country plus its comparators, and the ",
    "comparators' median. Dynamic facts give the change in CTF and rank over the years shown.\n",
    "7. The dashboard's taxonomy (it differs slightly from CLIAR: Justice and Rule of Law ",
    "uses 16 of CLIAR's 17 legal indicators; Political Institutions and Social Cohesion ",
    "merges two CLIAR families):\n", .taxonomy_outline(), "\n",
    .STYLE_RULES,
    "9. For each section listed under OUTPUT, write its marker line exactly as given, on ",
    "its own line, then the section's text. Write nothing outside these sections.",
    research_rules
  )
}

.selection_block <- function(meta) {
  sprintf(paste0(
    "SELECTION: %s compared with %d countries (%s). Thresholds: %s. ",
    "Change over time: even years %d-%d."),
    meta$base_name, length(meta$comparison_units),
    paste(meta$comparison_names, collapse = ", "), meta$threshold,
    meta$year_range[[1]], meta$year_range[[2]])
}

.facts_lines <- function(f) {
  if (is.null(f) || nrow(f) == 0L) return("(none)")
  paste(sprintf("[%s] %s", f$id, f$text), collapse = "\n")
}

.priority_block <- function(ev, clusters, n = 6L) {
  pa <- priority_areas(ev)
  pa <- utils::head(pa[pa$cluster %in% clusters, ], n)
  if (nrow(pa) == 0L) return("(none)")
  paste(sprintf("%d. %s - %s, CTF %.3f, comparator median %.3f [%s]", seq_len(nrow(pa)),
                pa$name, pa$status, pa$ctf, pa$comp_median, pa$id), collapse = "\n")
}

.gaps_block <- function(ev, clusters) {
  g <- ev$gaps[ev$gaps$cluster %in% clusters, ]
  if (nrow(g) == 0L) "(none)" else paste("-", g$name, collapse = "\n")
}

.excluded_block <- function(ev, clusters) {
  x <- ev$excluded[ev$excluded$cluster %in% clusters, ]
  if (nrow(x) == 0L) return("(none)")
  paste(sprintf("- %s: %s", x$leaf, x$indicator), collapse = "\n")
}

.section_titles <- function(ev, taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  leaf <- unique(data.frame(key = .leaf_key(tx), title = .leaf_label(tx), cluster = tx$cluster))
  c(overview = "Overview", prioritization = "Priorities for this chapter",
    recommendations = "Possible recommendations",
    stats::setNames(unname(ev$meta$cluster_names), names(ev$meta$cluster_names)),
    stats::setNames(leaf$title, leaf$key))
}

.LEAF_TEMPLATE <- paste(
  "Under three subheadings written exactly as '#### What the data show',",
  "'#### Why it matters for growth and jobs' and '#### Questions for the country team':",
  "(a) 2-4 sentences on the strongest and weakest indicators - band, rank, comparator",
  "median, and change over time where given [E]; (b) 2-3 sentences linking the area to",
  "the jobs-strategy pillars as the guidance does [G]; (c) 3-5 bullet points ('- ') with",
  "the guidance's diagnostic questions that the dashboard data cannot answer, and the",
  "data sources or tools the guidance suggests for them [G]. About 200 words.")

.leaf_template <- function(ev, leaf_key) {
  if (leaf_key %in% ev$gaps$leaf) {
    paste("This area is NOT YET MEASURED. Under '#### What the data show' say in one",
          "sentence that the dashboard has no indicators for it yet; then write the other",
          "two subheadings as for any section:", .LEAF_TEMPLATE)
  } else .LEAF_TEMPLATE
}

# Leaves (in taxonomy order) of the clusters in an evidence pack.
.scope_leaves <- function(ev, taxonomy = cgjrdata::cgjr_taxonomy) {
  tx <- as.data.frame(taxonomy)
  tx <- tx[order(tx$cluster_num, tx$subcluster_num,
                 dplyr::coalesce(tx$sub_subcluster_num, 0L)), ]
  u <- unique(data.frame(cluster = tx$cluster, leaf = .leaf_key(tx)))
  u[u$cluster %in% ev$meta$clusters, ]
}

.call <- function(id, label, keys, templates, guidance_rows, evidence_text, ev, guidance,
                  research = FALSE) {
  titles <- .section_titles(ev)
  name <- ifelse(startsWith(keys, "questions_"),
                 paste("Questions:", titles[sub("^questions_", "", keys)]), titles[keys])
  out <- paste(sprintf("[[section:%s]]  (%s) %s", keys, name, templates), collapse = "\n")
  user <- paste0(
    .selection_block(ev$meta), "\n\n",
    "OUTPUT - write these sections, in this order, each opened by its marker line ",
    "exactly as shown (marker only, without the description in brackets):\n", out, "\n\n",
    "GUIDANCE PASSAGES:\n", .guidance_block(guidance_rows), "\n\n",
    evidence_text)
  list(id = id, label = label, keys = keys,
       prompt = list(system = .report_system_prompt(ev$meta, guidance, research), user = user))
}

# Where research belongs in a section that has findings: the country's own
# findings add context to what the data show, findings from elsewhere explain
# why the area matters - each kept apart from the dashboard's figures.
.LEAF_RESEARCH <- paste(
  "Where RESEARCH findings bear on this section, use them: findings about the country add",
  "context to (a), kept apart from the dashboard's figures; findings from elsewhere explain",
  "mechanisms in (b), said to come from elsewhere [S]. Use only findings that strengthen the text.")

# The narrative format, as a published CGJR reads: prose whose depth follows
# the area's priority (the guidance: "only a few" clusters will matter most),
# recommendations inside the section (the guidance: "each section of the
# chapter should ... derive recommendations relevant to its area"), and the
# guidance's diagnostic questions gathered in an annex.
.NARRATIVE_FULL <- paste(
  "A PRIORITY AREA: about 350-450 words in 3-5 paragraphs, each opening with a bold",
  "sentence (rule 8), no subheadings. Cover, in the",
  "order the evidence suggests: where the country stands - the strongest and weakest",
  "indicators, their band, rank, the comparators' median and change over time [E]; why the",
  "area matters for growth and jobs, through the jobs-strategy pillars and the literature",
  "the guidance gives [G]; and 1-2 recommendations for this area from the guidance's",
  "possible recommendations [G], worded for the country, each with the evidence that",
  "motivates it [E].")
.NARRATIVE_SHORT <- paste(
  "Not a priority area: one paragraph of 80-120 words, opening with a bold sentence, on",
  "where the country stands [E] and",
  "why the area matters for growth and jobs [G]. No recommendations.")
.NARRATIVE_GAP <- paste(
  "This area is NOT YET MEASURED: one paragraph of 2-3 sentences - the dashboard has no",
  "indicators for it yet, and why the area matters for growth and jobs [G]. Never describe",
  "it as weak or strong.")
.NARRATIVE_QUESTIONS <- paste(
  "The area's questions for further analysis: 2-4 bullet points ('- ') with the guidance's",
  "diagnostic questions that the dashboard data cannot answer, each with the data sources",
  "or tools the guidance suggests [G]. No other text.")
# Research's jobs in the narrative format (with the section's findings only).
.NARRATIVE_RESEARCH <- paste(
  "Where RESEARCH findings bear on this area, give them a job: findings about the country",
  "add context the indicators cannot show - say how old the evidence is; findings from",
  "elsewhere explain mechanisms or what reforms achieved, said to come from elsewhere [S].",
  "A recommendation not among the guidance's possible recommendations is allowed only when",
  "a RESEARCH finding supports it: cite the finding and say the recommendation comes from",
  "research. Use only findings that strengthen the text.")
.NARRATIVE_QUESTIONS_RESEARCH <- paste(
  "Where a RESEARCH finding bears on a question, sharpen the question with it [S].")

# One section's instructions in the narrative format.
.narrative_template <- function(ev, leaf_key, priority, research) {
  body <- if (leaf_key %in% ev$gaps$leaf) .NARRATIVE_GAP
          else if (priority) .NARRATIVE_FULL else .NARRATIVE_SHORT
  paste(c(body, if (research) .NARRATIVE_RESEARCH), collapse = " ")
}

# The sections a narrative report treats in depth: the scope's top `n`
# measured areas in the dashboard's priority order; a one-section report's
# own section.
.priority_leaves <- function(ev, leaves, sc, n = 3L) {
  if (sc$type == "leaf") return(sc$key)
  titles <- .section_titles(ev)
  keys <- names(titles)[match(priority_areas(ev)$name, titles)]
  utils::head(keys[keys %in% leaves], n)
}

#' Plan the LLM calls for an AI Report
#'
#' @param evidence Output of [build_evidence()], built for the scope's
#'   clusters (all of them for the full chapter).
#' @param guidance Output of [load_guidance()].
#' @param scope `"chapter"`, `"cluster:<key>"` or `"leaf:<key>"` (see
#'   [report_scope_choices()]).
#' @param research `NULL` (no research: the plan is the dashboard-and-guidance
#'   draft), or a research pack: a list whose `findings` data frame has `id`
#'   (e.g. `"S3.2"`), `leaf` and `line` (the finding as the writer sees it).
#'   Each section's call then carries its own findings, and the rules for
#'   citing them.
#' @param format `"structured"` (each section under three fixed subheadings:
#'   what the data show, why it matters, questions for the country team) or
#'   `"narrative"` (prose as a published CGJR reads: the scope's top three
#'   priority areas in depth with their own recommendations, other areas in a
#'   paragraph, a closing synthesis by pillar, and each section's questions
#'   under its own `questions_<leaf>` marker, for an annex).
#' @param focus Optional text from the team on what to emphasise; it reaches
#'   every call as an instruction, never as a source. `NULL` or blank: none.
#' @return A list: `scope`, `format`, `focus`, `sections` (data frame `key`,
#'   `title`, in display order; in the narrative format the `questions_`
#'   sections last) and `calls` (list of `list(id, label, keys, prompt =
#'   list(system, user))`).
#' @export
plan_ai_report <- function(evidence, guidance, scope = "chapter", research = NULL,
                           format = c("structured", "narrative"), focus = NULL) {
  format <- match.arg(format)
  narrative <- format == "narrative"
  ev <- evidence; sc <- .parse_scope(scope)
  f <- ev$facts
  leaves <- .scope_leaves(ev)
  if (sc$type == "leaf") leaves <- leaves[leaves$leaf == sc$key, ]
  priority <- if (narrative) .priority_leaves(ev, leaves$leaf, sc)
  calls <- list()

  if (sc$type == "chapter") {
    ev_text <- paste0(
      "EVIDENCE - cluster averages:\n", .facts_lines(f[f$level == "cluster", ]), "\n\n",
      "PRIORITY ORDER (data-driven; keep this order):\n", .priority_block(ev, ev$meta$clusters), "\n\n",
      "NOT YET MEASURED:\n", .gaps_block(ev, ev$meta$clusters))
    calls[[1]] <- .call("overview", "Overview", c("overview", "prioritization"), c(
      paste("Three paragraphs, about 300 words: (1) why governance and public institutions",
            "matter for growth and jobs, following the theory of change and its pillars [G];",
            "(2) where the country stands across the four clusters [E]; (3) how this chapter",
            "connects to the other chapters of the report [G]."),
      paste("Following the guidance on prioritization [G], 2-4 bullet points ('- ') on the",
            "areas to prioritise, in the PRIORITY ORDER given (do not reorder), each with the",
            "jobs-strategy pillar(s) it bears on [G] and its evidence [E]. About 180 words.")),
      .guidance_for(guidance, "overview", glossary = FALSE), ev_text, ev, guidance)
  }

  for (cl in unique(leaves$cluster)) {
    lv <- leaves$leaf[leaves$cluster == cl]
    cf <- f[f$cluster %in% cl & !is.na(f$cluster), ]
    blocks <- vapply(lv, function(k) paste0(
      "Section ", k, ":\n", .facts_lines(cf[cf$leaf %in% k, ])), character(1))
    ev_text <- paste0(
      "EVIDENCE - ", ev$meta$cluster_names[[cl]], ":\n",
      "Cluster and subcluster averages:\n",
      .facts_lines(rbind(f[f$level == "cluster" & f$name == ev$meta$cluster_names[[cl]], ],
                         cf[cf$level != "indicator", ])), "\n\n",
      "Indicators by section:\n", paste(blocks, collapse = "\n\n"), "\n\n",
      "NOT YET MEASURED:\n", .gaps_block(ev, cl), "\n\n",
      "EXCLUDED INDICATORS (no data for the base country, or no variation in the group):\n",
      .excluded_block(ev, cl))
    # the research findings of this call's sections only
    r <- research$findings[research$findings$leaf %in% lv, ]
    if (NROW(r)) ev_text <- paste0(ev_text, "\n\nRESEARCH FINDINGS:\n", paste(r$line, collapse = "\n"))
    intro <- if (sc$type != "leaf") paste("2-4 sentences introducing the cluster as the guidance",
                                          "frames it [G] and summarising its subcluster averages [E].")
    if (narrative) {
      keys <- c(if (sc$type != "leaf") cl, lv, paste0("questions_", lv))
      templates <- c(
        intro,
        vapply(lv, function(k) .narrative_template(ev, k, k %in% priority, k %in% r$leaf), ""),
        vapply(lv, function(k) paste(c(.NARRATIVE_QUESTIONS, if (k %in% r$leaf) .NARRATIVE_QUESTIONS_RESEARCH),
                                     collapse = " "), ""))
    } else {
      keys <- if (sc$type == "leaf") lv else c(cl, lv)
      templates <- c(
        intro,
        vapply(lv, function(k) paste(c(.leaf_template(ev, k), if (k %in% r$leaf) .LEAF_RESEARCH),
                                     collapse = " "), character(1)))
    }
    g_keys <- if (sc$type == "leaf") lv else c(cl, lv, if (cl == "core_governance_functions") "public_financial_management")
    g_rows <- .guidance_for(guidance, g_keys)
    # a priority area makes its own recommendations: the call needs the guidance's list
    if (any(lv %in% priority)) g_rows <- rbind(g_rows, .guidance_for(guidance, "recommendations", glossary = FALSE))
    calls[[length(calls) + 1L]] <- .call(
      cl, ev$meta$cluster_names[[cl]], keys, templates, g_rows, ev_text, ev, guidance,
      research = NROW(r) > 0L)
  }

  rec_clusters <- unique(leaves$cluster)
  rec_ev <- paste0(
    "PRIORITY ORDER (data-driven; address these first):\n", .priority_block(ev, rec_clusters), "\n\n",
    if (sc$type == "leaf") paste0("EVIDENCE - this section:\n",
                                  .facts_lines(f[f$leaf %in% leaves$leaf & f$view == "static", ]), "\n\n"),
    "NOT YET MEASURED:\n", .gaps_block(ev, rec_clusters))
  rec_template <- if (narrative) {
    paste("The chapter's synthesis of recommendations: 3-6 bullet points ('- '), the areas of",
          "the PRIORITY ORDER first [E]", if (sc$type != "chapter") "- only areas within this scope",
          "- each with the recommendation(s) the guidance offers for",
          "it [G], worded for the country, and the jobs-strategy pillar(s) it contributes to [G].",
          "Close with 2-3 sentences on sequencing - which reforms come first, and why - and the",
          "political economy of carrying them out [G].")
  } else {
    paste("3-6 bullet points ('- '). Use ONLY recommendations from the guidance's list of",
          "possible recommendations [G], choosing those that address the weakest areas in",
          "the PRIORITY ORDER [E]", if (sc$type != "chapter") "within this scope", ". For each,",
          "give the recommendation as the guidance words it, the pillar(s) it contributes to",
          "[G], and the evidence that motivates it [E]. Close with 1-2 sentences on political",
          "economy considerations for feasibility and sequencing [G].")
  }
  calls[[length(calls) + 1L]] <- .call(
    "recommendations", "Possible recommendations", "recommendations", rec_template,
    rbind(.guidance_for(guidance, "recommendations", glossary = FALSE),
          .guidance_for(guidance, "overview", glossary = FALSE)[
            .guidance_for(guidance, "overview", glossary = FALSE)$section == "THE", ]),
    rec_ev, ev, guidance)

  # the team's focus, in every call: an instruction, never a source
  focus <- trimws(focus %||% "")
  if (nzchar(focus)) calls <- lapply(calls, function(call) {
    call$prompt$user <- paste0(call$prompt$user, "\n\nFOCUS FOR THIS DRAFT - an instruction from ",
      "the country team, not a source: follow it in what you emphasise, but anything it ",
      "mentions still needs cited evidence.\n", focus)
    call
  })

  keys <- unlist(lapply(calls, `[[`, "keys"))
  # display order: overview / prioritization first, recommendations, then the
  # questions annex (each named after its section)
  q <- startsWith(keys, "questions_")
  keys <- c(keys[!q], keys[q])
  titles <- .section_titles(ev)
  list(scope = scope, format = format, focus = if (nzchar(focus)) focus, calls = calls,
       sections = data.frame(key = keys, title = unname(titles[sub("^questions_", "", keys)]),
                             stringsAsFactors = FALSE))
}

#' Split an AI Report into its sections
#'
#' @param text The model's output: sections opened by `[[section:<key>]]`
#'   lines (tolerant of surrounding whitespace, backticks and emphasis).
#' @return A named list of Markdown, keyed by section; text before the first
#'   marker is dropped.
#' @export
parse_narrative <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  out <- list(); key <- NULL
  for (ln in lines) {
    m <- regmatches(ln, regexec(.MARKER_RE, ln, perl = TRUE))[[1]]
    if (length(m)) { key <- m[[2]]; out[[key]] <- character(0); next }
    if (!is.null(key)) out[[key]] <- c(out[[key]], ln)
  }
  lapply(out, function(x) trimws(paste(x, collapse = "\n")))
}

#' Run the LLM calls of an AI Report plan
#'
#' Calls run one after another (free tiers limit requests per minute). A
#' transient error - rate limit, or the provider overloaded (HTTP 5xx) - is
#' retried with backoff (5, 15, then 30 seconds) before the run gives up.
#' Calls already in `done` are skipped, so a run that failed part-way can be
#' resumed.
#'
#' @param plan Output of [plan_ai_report()].
#' @param chat_factory Function `system_prompt -> Chat`; [llm_chat()] in the app.
#' @param on_progress Called as `on_progress(i, call, text)` while call `i`
#'   streams (and with `text = NULL` when it starts).
#' @param on_retry Called as `on_retry(i, call, n, delay, cnd)` before retry
#'   `n` of call `i`, which waits `delay` seconds after error `cnd`.
#' @param on_call_done Called as `on_call_done(i, call, text)` when call `i`
#'   has its complete text.
#' @param done Named list, call id -> text, of calls already completed.
#' @param delays Seconds to wait before each retry.
#' @return A promise for the full text: every call's output, in plan order.
#' @export
run_ai_report <- function(plan, chat_factory = llm_chat, on_progress = NULL,
                          on_retry = NULL, on_call_done = NULL, done = list(),
                          delays = .RETRY_DELAYS) {
  # a plain wrapper: defaults resolve in the namespace, and an error before
  # the first await comes back as a rejection, not a throw
  tryCatch(.run_ai_report_async(plan, chat_factory, on_progress, on_retry,
                                on_call_done, done, delays),
           error = function(e) promises::promise_reject(e))
}

.run_ai_report_async <- coro::async(function(plan, chat_factory, on_progress, on_retry,
                                             on_call_done, done, delays) {
  call_back <- function(f, ...) if (!is.null(f)) f(...)
  for (i in seq_along(plan$calls)) {
    call <- plan$calls[[i]]
    if (!is.null(done[[call$id]])) next
    call_back(on_progress, i, call, NULL)
    res <- await(.stream_retry(
      make_chat = function() chat_factory(call$prompt$system),
      prompt = call$prompt$user,
      on_chunk = function(acc) call_back(on_progress, i, call, acc),
      on_retry = function(n, delay, cnd) call_back(on_retry, i, call, n, delay, cnd),
      delays = delays))
    done[[call$id]] <- res
    call_back(on_call_done, i, call, res)
  }
  ids <- vapply(plan$calls, `[[`, character(1), "id")
  paste(unlist(done[ids]), collapse = "\n\n")
})

# The guidance passages a text cites, for the report's sources appendix.
.cited_passages <- function(text, guidance) {
  ids <- .cited_ids(text)
  p <- guidance$passages
  p <- p[p$id %in% ids, c("id", "section", "title", "page")]
  p[order(match(p$id, p$id)), ]
}
