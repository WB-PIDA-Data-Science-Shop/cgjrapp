# AI Report: scope choices, prompt plans, narrative parsing, a full run with a
# fake LLM, and the AI-assisted Word document. Stand-in guidance only.

AI_COMPS <- cgjrapp:::.default_comparators("GHA")
AI_EV <- build_evidence("GHA", AI_COMPS, "Default", c(2013L, 2024L))
AI_G <- load_guidance("standin")

test_that("scope choices cover the chapter, every cluster and every section", {
  ch <- report_scope_choices()
  expect_identical(unname(ch[["Whole report"]]), "chapter")
  expect_length(ch[["One cluster"]], 4L)
  expect_length(ch[["One section"]], 14L)
  expect_true("leaf:public_sector_hrm" %in% ch[["One section"]])
})

test_that("a full-chapter plan: overview, one call per cluster, recommendations", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter")
  expect_identical(vapply(p$calls, `[[`, character(1), "id"),
                   c("overview", AI_EV$meta$clusters, "recommendations"))
  expect_identical(p$sections$key[1:2], c("overview", "prioritization"))
  expect_identical(utils::tail(p$sections$key, 1), "recommendations")
  expect_true(all(c("degree_of_integrity", "social_cohesion_norms_and_cooperation") %in% p$sections$key))
  # every leaf of the taxonomy appears once
  expect_identical(anyDuplicated(p$sections$key), 0L)
})

test_that("each call carries only its own guidance and evidence, plus its markers", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter")
  inst <- p$calls[[which(vapply(p$calls, `[[`, "", "id") == "institutional_environment")]]
  u <- inst$prompt$user
  expect_match(u, "[[section:institutional_environment]]", fixed = TRUE)
  expect_match(u, "[[section:justice_and_rule_of_law]]", fixed = TRUE)
  expect_match(u, "[G1.1-1]", fixed = TRUE)
  expect_false(grepl("[G2.2-1]", u, fixed = TRUE))
  ie_ids <- AI_EV$facts$id[AI_EV$facts$cluster %in% "institutional_environment" &
                             AI_EV$facts$level == "indicator"]
  expect_true(all(vapply(sprintf("[%s]", ie_ids), grepl, logical(1), x = u, fixed = TRUE)))
  ctx_id <- AI_EV$facts$id[AI_EV$facts$cluster %in% "context" & AI_EV$facts$level == "indicator"][[1]]
  expect_false(grepl(sprintf("[%s]", ctx_id), u, fixed = TRUE))
  # the system prompt names the guidance and states the citation rules
  expect_match(inst$prompt$system, "STAND-IN")
  expect_match(inst$prompt$system, "Every figure you state must be copied exactly")
})

test_that("cluster and section scopes plan only what they need", {
  ev <- build_evidence("GHA", AI_COMPS, clusters = "core_governance_functions")
  pc <- plan_ai_report(ev, AI_G, "cluster:core_governance_functions")
  expect_identical(vapply(pc$calls, `[[`, "", "id"), c("core_governance_functions", "recommendations"))
  expect_match(pc$calls[[1]]$prompt$user, "[G2.1.3-1]", fixed = TRUE)
  pl <- plan_ai_report(ev, AI_G, "leaf:public_sector_hrm")
  expect_identical(pl$sections$key, c("public_sector_hrm", "recommendations"))
  expect_false(grepl("[[section:core_governance_functions]]", pl$calls[[1]]$prompt$user, fixed = TRUE))
  expect_false(grepl("[[section:digital_and_data]]", pl$calls[[1]]$prompt$user, fixed = TRUE))
})

test_that("the priority order in the prompt is priority_areas()'s", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter")
  first <- priority_areas(AI_EV)$id[[1]]
  expect_match(p$calls[[1]]$prompt$user, sprintf("PRIORITY ORDER[^\n]*\n1\\. [^\n]*\\[%s\\]", first))
})

test_that("parse_narrative splits on markers, tolerating formatting noise", {
  txt <- paste("preamble to drop", "[[section:overview]]", "Over [E1].",
               "  **[[section: degree_of_integrity ]]**  ", "#### What the data show", "Text.",
               "`[[section:recommendations]]`", "- Do X [G5-1].", sep = "\n")
  n <- parse_narrative(txt)
  expect_identical(names(n), c("overview", "degree_of_integrity", "recommendations"))
  expect_identical(n$overview, "Over [E1].")
  expect_match(n$degree_of_integrity, "^#### What the data show\nText\\.$")
})

test_that("run_ai_report streams every call in order and reports progress", {
  ev <- build_evidence("GHA", AI_COMPS, clusters = "context")
  p <- plan_ai_report(ev, AI_G, "cluster:context")
  f <- fake_chat_factory(list(c("[[section:context]]\nIntro ", "[GTHE-1]."),
                              c("[[section:recommendations]]\n- Do it [G5-1].")))
  seen <- list()
  out <- wait_for(run_ai_report(p, f, function(i, call, text) seen[[length(seen) + 1L]] <<- list(i, text)))
  expect_identical(attr(f, "calls")(), 2L)
  expect_match(out, "Intro [GTHE-1].", fixed = TRUE)
  expect_match(out, "Do it [G5-1].", fixed = TRUE)
  expect_identical(vapply(seen, `[[`, 1L, 1L), c(1L, 1L, 1L, 2L, 2L))
  expect_identical(attr(f, "prompts")()[[1]]$system, p$calls[[1]]$prompt$system)
})

GEMINI_503 <- paste0("\033[1m\033[22mHTTP 503 Service Unavailable.\n",
                     "i [ { \"error\": { \"code\": 503, \"message\": \"This model is currently ",
                     "experiencing high demand.\", \"status\": \"UNAVAILABLE\" } } ]")

test_that("run_ai_report retries transient errors with backoff, then carries on", {
  ev <- build_evidence("GHA", AI_COMPS, clusters = "context")
  p <- plan_ai_report(ev, AI_G, "cluster:context")
  f <- fake_chat_factory(list(simpleError(GEMINI_503), simpleError("HTTP 429 Too Many Requests"),
                              "[[section:context]]\nok", "[[section:recommendations]]\nrec"))
  retries <- list()
  out <- wait_for(run_ai_report(p, f, on_retry = function(i, call, n, delay, cnd)
    retries[[length(retries) + 1L]] <<- c(i, n), delays = c(0, 0, 0)))
  expect_identical(retries, list(c(1L, 1L), c(1L, 2L)))
  expect_identical(attr(f, "calls")(), 4L)
  expect_identical(out, "[[section:context]]\nok\n\n[[section:recommendations]]\nrec")
})

test_that("run_ai_report gives up after its retries, or at once on a permanent error", {
  ev <- build_evidence("GHA", AI_COMPS, clusters = "context")
  p <- plan_ai_report(ev, AI_G, "cluster:context")
  f <- fake_chat_factory(list(simpleError(GEMINI_503)))
  expect_error(wait_for(run_ai_report(p, f, delays = c(0, 0, 0))), "503")
  expect_identical(attr(f, "calls")(), 4L)   # first try + 3 retries
  f2 <- fake_chat_factory(list(simpleError("HTTP 400 boom")))
  expect_error(wait_for(run_ai_report(p, f2, delays = c(0, 0, 0))), "boom")
  expect_identical(attr(f2, "calls")(), 1L)
})

test_that("run_ai_report resumes: completed calls are skipped and kept in order", {
  ev <- build_evidence("GHA", AI_COMPS, clusters = "context")
  p <- plan_ai_report(ev, AI_G, "cluster:context")
  f <- fake_chat_factory(list("[[section:recommendations]]\nrec"))
  done_ids <- character(0)
  out <- wait_for(run_ai_report(p, f, on_call_done = function(i, call, text) done_ids <<- c(done_ids, call$id),
                                done = list(context = "[[section:context]]\nkept")))
  expect_identical(attr(f, "calls")(), 1L)
  expect_identical(done_ids, "recommendations")
  expect_identical(out, "[[section:context]]\nkept\n\n[[section:recommendations]]\nrec")
})

# -- research findings in the plan ---

# Two verified findings, one for a section in each of two clusters
AI_RESEARCH <- list(findings = data.frame(
  id = c("S1.1", "S2.1"), leaf = c("public_sector_hrm", "degree_of_integrity"),
  line = c("[S1.1] Who Serves the Poor? (World Bank, 2017), p. 9 - descriptive, about the country: A.",
           "[S2.1] Audits (Governance, 2023) - causal, about the country: B.")))

test_that("without research, no prompt mentions it", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter")
  prompts <- unlist(lapply(p$calls, `[[`, "prompt"))
  expect_false(any(grepl("RESEARCH", prompts, fixed = TRUE) | grepl("[S", prompts, fixed = TRUE)))
})

test_that("with research, the rules come in and each call gets only its own sections' findings", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter", research = AI_RESEARCH)
  call <- function(id) p$calls[[which(vapply(p$calls, `[[`, "", "id") == id)]]
  sys <- call("core_governance_functions")$prompt$system
  expect_match(sys, "RESEARCH findings, cited as [S...]", fixed = TRUE)
  expect_match(sys, "never as a cause", fixed = TRUE)
  expect_match(sys, "never present it as a fact about Ghana", fixed = TRUE)
  core <- call("core_governance_functions")$prompt$user
  expect_match(core, "RESEARCH FINDINGS:\n[S1.1]", fixed = TRUE)
  expect_false(grepl("[S2.1]", core, fixed = TRUE))
  expect_match(call("institutional_environment")$prompt$user, "[S2.1]", fixed = TRUE)
  # a cluster none of the findings is about gets none
  expect_false(grepl("RESEARCH FINDINGS", call("context")$prompt$user, fixed = TRUE))
})


# -- the narrative format ---

call_of <- function(p, id) p$calls[[which(vapply(p$calls, `[[`, "", "id") == id)]]
# a section's line in its call's OUTPUT list: "[[section:key]]  (Title) template"
section_line <- function(p, id, key) {
  u <- call_of(p, id)$prompt$user
  regmatches(u, regexpr(paste0("\\[\\[section:", key, "\\]\\][^\n]*"), u))
}
NARR <- plan_ai_report(AI_EV, AI_G, "chapter", format = "narrative")

test_that("the structured format is the default, with its three subheadings and no annex", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter")
  expect_identical(p$format, "structured")
  expect_match(section_line(p, "core_governance_functions", "public_sector_hrm"),
               "#### What the data show", fixed = TRUE)
  expect_false(any(startsWith(p$sections$key, "questions_")))
})

test_that("narrative: the top three priority areas get full depth, others a paragraph, gaps a line", {
  # Ghana's priority order: Degree of Integrity, Transparency and Accountability,
  # Public Sector HRM, then Justice and Rule of Law ...
  for (k in c("degree_of_integrity", "transparency_and_accountability")) {
    expect_match(section_line(NARR, "institutional_environment", k), "PRIORITY AREA", fixed = TRUE)
  }
  expect_match(section_line(NARR, "core_governance_functions", "public_sector_hrm"), "PRIORITY AREA", fixed = TRUE)
  short <- section_line(NARR, "institutional_environment", "justice_and_rule_of_law")
  expect_match(short, "one paragraph", fixed = TRUE)
  expect_false(grepl("PRIORITY AREA", short, fixed = TRUE))
  expect_match(section_line(NARR, "beyond_core_governance_functions", "soe_governance"),
               "NOT YET MEASURED", fixed = TRUE)
  expect_false(grepl("####", section_line(NARR, "core_governance_functions", "public_sector_hrm"), fixed = TRUE))
})

test_that("narrative: a one-section report treats its section as a priority", {
  p <- plan_ai_report(AI_EV, AI_G, "leaf:justice_and_rule_of_law", format = "narrative")
  expect_match(section_line(p, "institutional_environment", "justice_and_rule_of_law"), "PRIORITY AREA", fixed = TRUE)
})

test_that("narrative: a call with a priority area gets the guidance's possible recommendations", {
  expect_match(call_of(NARR, "institutional_environment")$prompt$user, "[G5-1]", fixed = TRUE)
  expect_false(grepl("[G5-1]", call_of(NARR, "context")$prompt$user, fixed = TRUE))  # no priority area
  structured <- plan_ai_report(AI_EV, AI_G, "chapter")
  expect_false(grepl("[G5-1]", call_of(structured, "institutional_environment")$prompt$user, fixed = TRUE))
})

test_that("narrative: every section's questions come under their own marker, gathered at the end", {
  leaves <- cgjrapp:::.scope_leaves(AI_EV)$leaf
  q <- paste0("questions_", leaves)
  expect_identical(utils::tail(NARR$sections$key, length(q)), q)
  expect_identical(NARR$sections$title[NARR$sections$key == "questions_public_sector_hrm"], "Public Sector HRM")
  expect_match(section_line(NARR, "core_governance_functions", "questions_public_sector_hrm"),
               "questions", fixed = TRUE)
})

test_that("narrative: the closing section is a synthesis by pillar, with sequencing", {
  line <- section_line(NARR, "recommendations", "recommendations")
  expect_match(line, "synthesis", fixed = TRUE)
  expect_match(line, "sequencing", fixed = TRUE)
})

test_that("a focus for the draft reaches every call as an instruction, never as a source", {
  p <- plan_ai_report(AI_EV, AI_G, "chapter", format = "narrative", focus = "  Emphasize the wage bill.  ")
  for (call in p$calls) {
    expect_match(call$prompt$user, "FOCUS FOR THIS DRAFT", fixed = TRUE)
    expect_match(call$prompt$user, "not a source", fixed = TRUE)
    expect_match(call$prompt$user, "Emphasize the wage bill.", fixed = TRUE)
  }
  for (f in list(NULL, "", "   ")) {
    p0 <- plan_ai_report(AI_EV, AI_G, "chapter", focus = f)
    expect_false(any(grepl("FOCUS", vapply(p0$calls, function(x) x$prompt$user, ""), fixed = TRUE)))
  }
})


test_that("narrative: a cluster's closing synthesis keeps to the cluster", {
  p <- plan_ai_report(AI_EV, AI_G, "cluster:core_governance_functions", format = "narrative")
  expect_match(section_line(p, "recommendations", "recommendations"), "within this scope", fixed = TRUE)
  expect_false(grepl("within this scope", section_line(NARR, "recommendations", "recommendations"), fixed = TRUE))
})

# -- the AI report in Word ---

# Two World Bank 2020 reports, one finding each
DOCX_PACK <- local({
  s <- cgjrapp:::.as_sources(provider_rows(
    "wb_documents", wb_id = c("1", "2"), title = c("Ghana Public Expenditure Review", "Wage Bill Study"),
    year = 2020L, kind = "report", publisher = "World Bank", place = "Washington, DC",
    authors = list(institution_author("World Bank"), institution_author("World Bank"))))
  f <- data.frame(leaf = "public_sector_hrm", key = c("wb:1", "wb:2"), finding_id = 1L, passage = 1L,
                  file_page = 12L, page = c("10", "3"),
                  quote = c("the wage bill takes 57 percent of revenue", "arrears doubled"),
                  finding = c("The wage bill takes 57 percent of revenue.", "Arrears doubled."),
                  claim_type = "descriptive", scope = "country")
  cgjrapp:::.research_pack(s, f)
})
DOCX_SECTIONS <- data.frame(key = c("public_sector_hrm", "recommendations", "questions_public_sector_hrm"),
                            title = c("Public Sector HRM", "Possible recommendations", "Public Sector HRM"))
ai_docx <- function(narrative, research = NULL, guidance = AI_G) {
  ai_report_docx(AI_EV, DOCX_SECTIONS, narrative, research = research, guidance = guidance)
}

test_that("the Word report reads like the report: no dashboard or guidance ids, no tables, no appendices", {
  doc <- ai_docx(list(public_sector_hrm = "Ghana scores 0.517 [E17]. Merit matters [G2.2-1, E17].",
                      recommendations = "- Strengthen recruitment [G5-1].",
                      questions_public_sector_hrm = "- Is pay competitive? [G2.2-3]"))
  s <- officer::docx_summary(doc)
  text <- s$text[nzchar(s$text)]
  expect_true(any(text == "Ghana scores 0.517. Merit matters."))
  expect_true(any(text == "• Strengthen recruitment."))
  expect_false(any(grepl("[E", text, fixed = TRUE) | grepl("[G", text, fixed = TRUE)))
  expect_false(any(s$content_type == "table cell"))
  expect_false(any(grepl("^Appendix|evidence register|guidance passage", text, ignore.case = TRUE)))
  # the report's sections in its order, the questions under their annex heading
  headings <- s$text[grepl("^heading", s$style_name)]
  expect_identical(headings[-1], c("Public Sector HRM", "Possible recommendations",
                                   "Questions for further analysis", "Public Sector HRM"))
  # the section's chart, as in the preview
  expect_match(as.character(officer::docx_body_xml(doc)), "pic:pic", fixed = TRUE)
})

test_that("a section the run did not write is left out of the Word report", {
  s <- officer::docx_summary(ai_docx(list(public_sector_hrm = "HRM-TEXT")))
  expect_false(any(s$text %in% c("Possible recommendations", "Questions for further analysis")))
})

test_that("the Word report cites research author-date and lists the references, titles in italics", {
  doc <- ai_docx(list(public_sector_hrm = "The wage bill is high [S1.1].",
                      recommendations = "- Review the wage bill [G5-1]."), research = DOCX_PACK)
  s <- officer::docx_summary(doc)
  expect_true(any(s$text == "The wage bill is high (World Bank 2020, 10)."))
  expect_identical(s$text[which(s$text == "References") + 1L],
                   "World Bank. 2020. Ghana Public Expenditure Review. Washington, DC: World Bank.")
  expect_false(any(grepl("Wage Bill Study", s$text)))  # not cited, not listed
  xml <- as.character(officer::docx_body_xml(doc))
  # the title is a run of its own, in italics
  expect_match(xml, "(?s)<w:i w:val=\"true\"/>(?:(?!</w:r>).)*>Ghana Public Expenditure Review</w:t>",
               perl = TRUE)
})

test_that("works lettered 2020a and 2020b keep their letters in every section of the Word report", {
  s <- officer::docx_summary(ai_docx(list(public_sector_hrm = "High [S1.1].",
                                          recommendations = "- Pay arrears [S2.1]."), research = DOCX_PACK))
  expect_true(any(s$text == "High (World Bank 2020a, 10)."))
  expect_true(any(s$text == "• Pay arrears (World Bank 2020b, 3)."))
})

test_that("a paragraph's bold lead sentence is bold in the Word report, the rest is not", {
  doc <- ai_docx(list(public_sector_hrm = "**Merit is weak.** Recruitment is ad hoc."))
  expect_true(any(officer::docx_summary(doc)$text == "Merit is weak. Recruitment is ad hoc."))
  xml <- as.character(officer::docx_body_xml(doc))
  expect_match(xml, "(?s)<w:b w:val=\"true\"/>(?:(?!</w:r>).)*>Merit is weak.</w:t>", perl = TRUE)
  expect_match(xml, "(?s)<w:b w:val=\"false\"/>(?:(?!</w:r>).)*> Recruitment is ad hoc.</w:t>", perl = TRUE)
})

test_that("the real guidance marks the Word report Official Use Only", {
  g <- AI_G; g$source <- "real"
  expect_identical(officer::docx_summary(ai_docx(list(public_sector_hrm = "x"), guidance = g))$text[[1]],
                   "OFFICIAL USE ONLY")
  expect_false(any(officer::docx_summary(ai_docx(list(public_sector_hrm = "x")))$text == "OFFICIAL USE ONLY"))
})
