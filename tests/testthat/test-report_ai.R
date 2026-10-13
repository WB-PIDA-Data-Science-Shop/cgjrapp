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

test_that("the AI-assisted Word report has the narrative, sources and verification", {
  e <- AI_EV$facts[AI_EV$facts$level == "cluster" & AI_EV$facts$view == "static", ][1, ]
  txt <- paste0("[[section:overview]]\nWhy it matters [GTHE-1]. Score ",
                sprintf("%.3f", e$ctf), " [", e$id, "].\n",
                "[[section:degree_of_integrity]]\n#### What the data show\nUNIQUE-LEAF-TEXT [G1.1-1].\n",
                "[[section:recommendations]]\n- UNIQUE-REC [G5-1].")
  n <- parse_narrative(txt)
  v <- verify_text(txt, AI_EV, AI_G)
  doc <- evidence_report_docx(AI_EV, n, ai = list(guidance = AI_G, cited = cgjrapp:::.cited_passages(txt, AI_G),
                                                  verification = v))
  s <- officer::docx_summary(doc)
  expect_true(any(grepl("UNIQUE-LEAF-TEXT", s$text)))
  expect_true(any(s$text == "Possible recommendations"))
  expect_true(any(grepl("^• UNIQUE-REC", s$text)))
  expect_true(any(s$text == "Appendix: guidance passages cited"))
  expect_true(any(s$text == "Appendix: automated verification"))
  expect_false(any(s$text == "OFFICIAL USE ONLY"))
})

test_that("a one-section Word report holds only that section", {
  doc <- evidence_report_docx(AI_EV, list(public_sector_hrm = "HRM-TEXT"), leaves = "public_sector_hrm")
  s <- officer::docx_summary(doc)
  expect_true(any(s$text == "HRM-TEXT"))
  expect_false(any(s$text == "Overview"))
  expect_false(any(s$text %in% c("Degree of Integrity", "Digital and Data")))
})

test_that("the real guidance marks the Word report Official Use Only", {
  g <- AI_G; g$source <- "real"
  doc <- evidence_report_docx(AI_EV, list(overview = "x"), ai = list(guidance = g))
  expect_identical(officer::docx_summary(doc)$text[[1]], "OFFICIAL USE ONLY")
})
