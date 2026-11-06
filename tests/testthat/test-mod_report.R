# AI Report tab module: report generation and chat with a fake LLM (no
# network), stand-in guidance only.

COMPS <- c("NGA", "KEN", "SEN", "CIV", "BEN")
MR_G <- load_guidance("standin")

report_args <- function(base = "GHA", comps = COMPS, factory = fake_chat_factory(list("x"))) {
  list(base_unit = shiny::reactive(base), comparison_units = shiny::reactive(comps),
       threshold = shiny::reactive("Default"), year_range = shiny::reactive(c(2013L, 2024L)),
       chat_factory = factory, guidance = MR_G)
}

# Pump the later loop (the LLM calls are async) until `done()` or timeout.
pump <- function(session, done, timeout = 20) {
  t0 <- Sys.time()
  while (!isTRUE(done()) && difftime(Sys.time(), t0, units = "secs") < timeout) {
    later::run_now(0.05); session$flushReact()
  }
  later::run_now(0.05); session$flushReact()  # outputs catch up with the last change
}

test_that("mod_report_ui renders the report and chat controls", {
  html <- as.character(mod_report_ui("report"))
  for (p in c("report-scope", "report-generate", "report-download_ui", "report-status",
              "report-preview", "report-chat", "report-chat_reset")) {
    expect_match(html, p, fixed = TRUE)
  }
  expect_match(html, "leaf:public_sector_hrm", fixed = TRUE)
})

test_that(".ai_selection: one base, region fallback for thin comparators, clean problems", {
  s <- cgjrapp:::.ai_selection("GHA", "NGA", "Default", c(2013L, 2024L))
  expect_null(s$problem)
  expect_identical(s$comps, cgjrapp:::.default_comparators("GHA"))
  expect_match(s$note, "region")
  expect_match(cgjrapp:::.ai_selection(c("GHA", "KEN"), COMPS, "Default", c(2013L, 2024L))$problem,
               "exactly one base country")
  expect_match(cgjrapp:::.ai_selection("AFE", character(0), "Default", c(2013L, 2024L))$problem,
               "at least 2 comparators")
})

test_that("server: a multi-unit base selection is rejected cleanly", {
  shiny::testServer(mod_report_server, args = report_args(base = c("GHA", "KEN")), {
    session$setInputs(scope = "chapter", generate = 1)
    expect_identical(r_status()$state, "error")
    expect_match(r_status()$msg, "exactly one base country")
  })
})

test_that("server: an unreachable or key-rejecting endpoint surfaces a clear error", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "http://127.0.0.1:59999/v1")
  shiny::testServer(mod_report_server, args = report_args(), {
    session$setInputs(scope = "chapter", generate = 1)
    expect_match(r_status()$msg, "Cannot reach the AI service")
  })
  httr2::local_mocked_responses(function(req) httr2::response(401))
  shiny::testServer(mod_report_server, args = report_args(), {
    session$setInputs(scope = "chapter", generate = 1)
    expect_match(r_status()$msg, "rejected the API key")
  })
})

test_that("server: a one-section report streams, is verified and downloads", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  ev <- build_evidence("GHA", COMPS, clusters = "core_governance_functions")
  f1 <- ev$facts[ev$facts$leaf %in% "public_sector_hrm" & ev$facts$view == "static", ][1, ]
  leaf_txt <- sprintf("[[section:public_sector_hrm]]\n#### What the data show\n%s scores %.3f [%s]. Merit matters [G2.2-1].",
                      "Ghana", f1$ctf, f1$id)
  f <- fake_chat_factory(list(c(substr(leaf_txt, 1, 40), substring(leaf_txt, 41)),
                              "[[section:recommendations]]\n- Professionalize recruitment [G5-1]."))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(scope = "leaf:public_sector_hrm", generate = 1)
    expect_identical(r_plan()$sections$key, c("public_sector_hrm", "recommendations"))
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_status()$state, "done")
    res <- r_result()
    expect_named(res$narrative, c("public_sector_hrm", "recommendations"))
    expect_identical(nrow(res$verification$issues), 0L)
    expect_setequal(res$cited$id, c("G2.2-1", "G5-1"))
    expect_match(as.character(output$sec_public_sector_hrm$html), "cgjr-cite-e", fixed = TRUE)
    path <- output$download_docx
    s <- officer::docx_summary(officer::read_docx(path))
    expect_true(any(grepl("Merit matters", s$text)))
    # the reader's version: no ids, no appendices
    expect_false(any(grepl("[E", s$text, fixed = TRUE) | grepl("[G", s$text, fixed = TRUE)))
    expect_false(any(grepl("^Appendix", s$text)))
    expect_false(any(s$text == "Overview"))
  })
})

test_that("server: an LLM failure mid-report keeps what was written", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list("[[section:context]]\nIntro [G4-1].", simpleError("HTTP 400 boom")))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(scope = "cluster:context", generate = 1)
    pump(session, function() identical(r_status()$state, "error"))
    expect_identical(r_status()$state, "error")
    expect_match(r_status()$msg, "boom")
    expect_true(r_status()$partial)
    expect_match(r_result()$narrative$context, "Intro")
  })
})

test_that("server: Resume finishes a failed run without rewriting completed parts", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list("[[section:context]]\nIntro [G4-1].", simpleError("HTTP 400 boom"),
                              "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(scope = "cluster:context", generate = 1)
    pump(session, function() identical(r_status()$state, "error"))
    expect_identical(r_status()$n_done, 1L)
    expect_identical(r_status()$n, 2L)
    expect_match(as.character(output$status$html), "-resume\"", fixed = TRUE)
    session$setInputs(resume = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_status()$state, "done")
    expect_identical(attr(f, "calls")(), 3L)          # the context part was not redone
    expect_named(r_result()$narrative, c("context", "recommendations"))
    # the fake LLM skipped the cluster's two leaf sections: flagged as missing
    iss <- r_result()$verification$issues
    expect_setequal(iss$section, c("political_institutions_and_social_cohesion",
                                   "social_cohesion_norms_and_cooperation"))
    expect_true(all(iss$type == "missing_section"))
  })
})

test_that("server: a chat turn gets the guidance passages for the question and the selection's evidence", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list(c("The guidance says integrity ", "matters [G1.1-1].")))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    # shinychat 0.5 sends list(text, attachments)
    session$setInputs(chat_user_input = list(text = "Why does integrity matter?", attachments = list()))
    pump(session, function() !chat_state$busy && attr(f, "calls")() == 1L)
    expect_identical(attr(f, "calls")(), 1L)
    user <- attr(f, "prompts")()[[1]]$user
    expect_match(user, "[G1.1-1]", fixed = TRUE)
    expect_match(user, "Why does integrity matter?", fixed = TRUE)
    sys <- attr(f, "prompts")()[[1]]$system
    expect_match(sys, "Beyond the guidance (general knowledge, not verified)", fixed = TRUE)
    expect_match(sys, "SELECTION: Ghana compared with 5 countries")
    expect_false(chat_state$busy)
  })
})

test_that("server: after a chat answer, the history keeps the question but not its passages", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list("Integrity matters [G1.1-1]."))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(chat_user_input = "Why does integrity matter?")
    pump(session, function() !chat_state$busy && attr(f, "calls")() == 1L)
    turns <- chat_state$chat$get_turns()
    expect_length(turns, 2L)
    expect_identical(ellmer::contents_text(turns[[1]]), "Why does integrity matter?")
    expect_identical(ellmer::contents_text(turns[[2]]), "Integrity matters [G1.1-1].")
  })
})

test_that("server: a follow-up question still gets the passages of the topic before it", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list("Integrity matters [G1.1-1]."))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(chat_user_input = "Why does integrity matter?")
    pump(session, function() !chat_state$busy && attr(f, "calls")() == 1L)
    session$setInputs(chat_user_input = "Tell me more about that.")
    pump(session, function() !chat_state$busy && attr(f, "calls")() == 2L)
    expect_match(attr(f, "prompts")()[[2]]$user, "[G1.1-1]", fixed = TRUE)
  })
})

test_that("server (mAI): the question is embedded, and a failed embedding falls back to keywords", {
  local_mai()
  g <- mini_guidance()
  g$embeddings <- list(vectors = rbind(`G1.1-1` = c(1, 0), `G2.2-1` = c(0, 1),
                                       `G2.1.3-1` = c(0.7, 0.7), `G2.3-1` = c(-1, 0)))
  embed_ok <- TRUE
  httr2::local_mocked_responses(function(req) {
    if (!grepl("/embeddings", req$url)) return(httr2::response(200))   # health check
    if (!embed_ok) return(httr2::response(500))
    httr2::response_json(body = list(data = list(list(index = 0L, embedding = list(1, 0.05)))))
  })
  f <- fake_chat_factory(list("Tenders [G2.1.3-1]."))
  args <- report_args(factory = f)
  args$guidance <- g
  shiny::testServer(mod_report_server, args = args, {
    session$setInputs(chat_user_input = "tenders")
    pump(session, function() !chat_state$busy && attr(f, "calls")() == 1L)
    # G1.1-1 shares no word with "tenders": only the embedding finds it
    expect_match(attr(f, "prompts")()[[1]]$user, "[G1.1-1]", fixed = TRUE)

    embed_ok <<- FALSE
    expect_message({
      session$setInputs(chat_user_input = "tenders again")
      pump(session, function() !chat_state$busy && attr(f, "calls")() == 2L)
    }, "embedding.*keyword")
    expect_match(attr(f, "prompts")()[[2]]$user, "[G2.1.3-1]", fixed = TRUE)
  })
})

test_that("chat input text is read from every shape shinychat sends", {
  f <- cgjrapp:::.chat_input_text
  expect_identical(f("hi"), "hi")
  expect_identical(f(list("hi")), "hi")                       # from the browser
  expect_identical(f(list(text = "hi", attachments = list())), "hi")
  expect_identical(f(NULL), "")
})

test_that("chat_reply_html badges citations and adds a verification footer", {
  r <- chat_reply_html("Integrity matters [G1.1-1]. A claim [G9-9].", NULL, MR_G)
  html <- as.character(r$html)
  expect_match(html, "cgjr-cite-g", fixed = TRUE)
  expect_match(html, "cgjr-cite-bad", fixed = TRUE)
  expect_match(html, "cgjr-verify-warn", fixed = TRUE)
  ok <- chat_reply_html("Integrity matters [G1.1-1].", NULL, MR_G)
  expect_match(as.character(ok$html), "Sources checked", fixed = TRUE)
})

# -- research in the AI Report ---

# A section's research, as .research_section() returns it: one World Bank
# source with one finding. `title` lets a test plant untrusted text.
research_result <- function(leaf, title = "Ghana Public Expenditure Review") {
  s <- cgjrapp:::.as_sources(provider_rows("wb_documents", wb_id = "1", title = title, year = 2020L,
                                           kind = "report", publisher = "World Bank",
                                           place = "Washington, DC",
                                           authors = list(institution_author("World Bank"))))
  s$leaf <- leaf; s$selected <- TRUE; s$citable <- TRUE; s$tier <- 1L; s$scope <- "country"
  s$relevance <- "high"; s$relevance_reason <- "On the wage bill"; s$access <- "full_text"
  s$reason <- "ok"
  f <- data.frame(leaf = leaf, key = "wb:1", finding_id = 1L, passage = 1L, file_page = 12L,
                  page = "10", quote = "the wage bill takes 57 percent of revenue",
                  finding = "The wage bill takes 57 percent of revenue.", claim_type = "descriptive",
                  scope = "country")
  list(leaf = leaf, sources = s, findings = f, dropped = f[0, c("key", "passage", "finding")],
       screening = "ok")
}

# A fake research function: records each call (leaf and policy); `fail`
# names sections whose research throws.
fake_research <- function(fail = character(0), title = "Ghana Public Expenditure Review", delay = 0) {
  calls <- list()
  fun <- function(leaf, ev, policy) {
    calls[[length(calls) + 1L]] <<- list(leaf = leaf, policy = policy, base = ev$meta$base_unit)
    if (leaf %in% fail) stop("HTTP 503 from a provider")
    if (delay == 0) return(research_result(leaf, title))
    # still running, like the background process: the result comes later
    promises::promise(function(resolve, reject) later::later(function() resolve(research_result(leaf, title)), delay))
  }
  attr(fun, "calls") <- function() calls
  fun
}

# The sections researched for `base`, in the order asked for
called <- function(rf, base = "GHA") {
  calls <- Filter(function(x) identical(x$base, base), attr(rf, "calls")())
  vapply(calls, `[[`, "", "leaf")
}

research_args <- function(factory, research) {
  utils::modifyList(report_args(factory = factory), list(research_fun = research))
}

HRM_TEXT <- "[[section:public_sector_hrm]]\n#### What the data show\nThe wage bill takes 57 percent of revenue [S1.1]."

test_that("the report tab offers research, its depth and a sources view", {
  html <- as.character(mod_report_ui("report"))
  for (p in c("report-use_research", "report-depth", "report-sources", "report-references")) {
    expect_match(html, p, fixed = TRUE)
  }
})

test_that("server: without research ticked, no research is done", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research()
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = FALSE, generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_length(attr(rf, "calls")(), 0L)
    expect_false(grepl("RESEARCH", r_plan()$calls[[1]]$prompt$user, fixed = TRUE))
  })
})

test_that("server: with research, each section is researched, then written with author-date citations", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research()
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(attr(rf, "calls")()[[1]]$leaf, "public_sector_hrm")  # the report's section first
    expect_match(r_plan()$calls[[1]]$prompt$user, "RESEARCH FINDINGS:\n[S1.1]", fixed = TRUE)
    expect_identical(nrow(r_result()$verification$issues), 0L)
    expect_match(as.character(output$sec_public_sector_hrm$html), "(World Bank 2020, 10)", fixed = TRUE)
    refs <- as.character(output$references$html)
    expect_match(refs, "World Bank. 2020. <em>Ghana Public Expenditure Review</em>.", fixed = TRUE)
    src <- as.character(output$sources$html)
    expect_match(src, "Ghana Public Expenditure Review", fixed = TRUE)
    expect_match(src, "the wage bill takes 57 percent of revenue", fixed = TRUE)
  })
})

test_that("server: fewer sources per section is passed on as smaller caps", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research()
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "5", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    lim <- attr(rf, "calls")()[[1]]$policy$limits
    expect_identical(c(lim$max_institutional_per_leaf, lim$max_peer_reviewed_per_leaf), c(3L, 2L))
  })
})

test_that("server: a section whose research fails is reported; the rest is written", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(fail = "social_cohesion_norms_and_cooperation")
  f <- fake_chat_factory(list("[[section:context]]\nIntro [G4-1].", "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "cluster:context", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_status()$state, "done")
    expect_identical(sum(vapply(attr(rf, "calls")(), `[[`, "", "leaf") ==
                           "social_cohesion_norms_and_cooperation"), 1L)
    expect_match(r_research()$social_cohesion_norms_and_cooperation$error, "503")
    expect_match(as.character(output$sources$html), "Research could not be completed", fixed = TRUE)
  })
})

test_that("server: Resume after a writing failure does not research again", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research()
  f <- fake_chat_factory(list(HRM_TEXT, simpleError("HTTP 400 boom"), "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "error"))
    session$setInputs(resume = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_status()$state, "done")
    expect_identical(anyDuplicated(called(rf)), 0L)  # no section researched twice
  })
})

test_that("server: with research, the Word download cites author-date and lists the references", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, fake_research()), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    s <- officer::docx_summary(officer::read_docx(output$download_docx))
    expect_true(any(s$text == "The wage bill takes 57 percent of revenue (World Bank 2020, 10)."))
    expect_true(any(s$text == "References"))
  })
})

test_that("server: two same-year works cited in different sections keep their letters in the preview", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- function(leaf, ev, policy) {
    r <- research_result(leaf)
    two <- research_result(leaf, title = "Wage Bill Study")
    two$sources$wb_id <- "2"; two$sources$key <- "wb:2"; two$findings$key <- "wb:2"
    r$sources <- rbind(r$sources, two$sources); r$findings <- rbind(r$findings, two$findings)
    r
  }
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [S2.1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_match(as.character(output$sec_public_sector_hrm$html), "(World Bank 2020a, 10)", fixed = TRUE)
    expect_match(as.character(output$sec_recommendations$html), "(World Bank 2020b, 10)", fixed = TRUE)
  })
})

test_that("server: the sources view says which sources were set aside, and why", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- function(leaf, ev, policy) {
    r <- research_result(leaf)
    aside <- cgjrapp:::.as_sources(provider_rows("openalex", openalex_id = "W9", title = "A Blog Post",
                                                 year = 2021L, kind = "other"))
    aside[setdiff(names(r$sources), names(aside))] <- NA
    aside$selected <- FALSE; aside$reason <- "type_excluded"
    r$sources <- rbind(r$sources, aside[names(r$sources)])
    r
  }
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    src <- as.character(output$sources$html)
    expect_match(src, "1 sources set aside", fixed = TRUE)
    expect_match(src, "A Blog Post\\s*<span[^>]*> - type excluded</span>")
  })
})

# -- excluding a source ---

REWRITTEN <- "[[section:public_sector_hrm]]\n#### What the data show\nMerit matters [G2.2-1]."
REC_TEXT <- "[[section:recommendations]]\n- Rec [G5-1]."
has_rewrite <- function(html) grepl("id=\"[^\"]*rewrite\"", html)

test_that("server: excluding a cited source rewrites only the parts that cite it", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list(HRM_TEXT, REC_TEXT, REWRITTEN))
  shiny::testServer(mod_report_server, args = research_args(f, fake_research()), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    session$setInputs(exclude = "wb:1")
    src <- as.character(output$sources$html)
    expect_match(src, "Include again", fixed = TRUE)
    expect_true(has_rewrite(src))
    session$setInputs(rewrite = 1)
    pump(session, function() identical(r_status()$state, "done") && !is.null(r_result()))
    expect_identical(attr(f, "calls")(), 3L)  # the recommendations were not rewritten
    expect_false(grepl("[S1.1]", attr(f, "prompts")()[[3]]$user, fixed = TRUE))
    res <- r_result()
    expect_identical(res$narrative$recommendations, "- Rec [G5-1].")
    expect_match(res$narrative$public_sector_hrm, "Merit matters", fixed = TRUE)
    expect_identical(nrow(res$verification$issues), 0L)
    expect_false(has_rewrite(as.character(output$sources$html)))
  })
})

test_that("server: excluding a source no part cites needs no rewrite; Include again restores it", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list(REWRITTEN, REC_TEXT))  # the report cites no research
  shiny::testServer(mod_report_server, args = research_args(f, fake_research()), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    session$setInputs(exclude = "wb:1")
    expect_false(has_rewrite(as.character(output$sources$html)))
    session$setInputs(include = "wb:1")
    src <- as.character(output$sources$html)
    expect_false(grepl("Include again", src, fixed = TRUE))
    expect_match(src, "Exclude", fixed = TRUE)
  })
})

test_that("server: an exclusion holds for the next report on that country, not on another", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  base <- shiny::reactiveVal("GHA")
  args <- research_args(fake_chat_factory(list(HRM_TEXT, REC_TEXT)), fake_research())
  args$base_unit <- base
  shiny::testServer(mod_report_server, args = args, {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    session$setInputs(exclude = "wb:1")
    session$setInputs(generate = 2)
    pump(session, function() !is.null(r_result()))
    expect_false(grepl("[S1.1]", r_plan()$calls[[1]]$prompt$user, fixed = TRUE))
    base("NGA")
    session$setInputs(generate = 3)
    pump(session, function() !is.null(r_result()))
    expect_match(r_plan()$calls[[1]]$prompt$user, "[S1.1]", fixed = TRUE)
  })
})

# -- research started early ---

N_SECTIONS <- length(report_scope_choices()[["One section"]])

test_that("server: ticking research for the applied country researches the chapter in the background", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(delay = 0.01)
  shiny::testServer(mod_report_server, args = research_args(fake_chat_factory(list("x")), rf), {
    session$setInputs(use_research = TRUE, depth = "10")
    pump(session, function() length(called(rf)) >= 1L, timeout = 5)
    expect_match(as.character(output$research_note$html), "in the background", fixed = TRUE)
    pump(session, function() length(called(rf)) == N_SECTIONS && research_task$status() != "running")
    expect_length(called(rf), N_SECTIONS)
    expect_identical(anyDuplicated(called(rf)), 0L)
    expect_identical(r_status()$state, "idle")  # nothing written until asked
  })
})

test_that("server: Generate after the background research is done researches nothing again", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(delay = 0.01)
  f <- fake_chat_factory(list(HRM_TEXT, REC_TEXT))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(use_research = TRUE, depth = "10")
    pump(session, function() research_task$status() == "success")
    session$setInputs(scope = "leaf:public_sector_hrm", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_length(called(rf), N_SECTIONS)
    expect_match(r_plan()$calls[[1]]$prompt$user, "RESEARCH FINDINGS:\n[S1.1]", fixed = TRUE)
  })
})

test_that("server: Generate while research runs goes first, and writes once its sections are done", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(delay = 0.05)
  f <- fake_chat_factory(list(REWRITTEN, REC_TEXT))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(use_research = TRUE, depth = "10")
    pump(session, function() length(called(rf)) >= 1L, timeout = 5)
    n0 <- length(called(rf))  # the section under way is finished first
    session$setInputs(scope = "leaf:digital_and_data", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(called(rf)[[n0 + 1L]], "digital_and_data")
    expect_lt(length(called(rf)), N_SECTIONS)              # written before the chapter's research ended
    pump(session, function() research_task$status() == "success")
    expect_identical(anyDuplicated(called(rf)), 0L)
  })
})

test_that("server: applying another country mid-run switches the research to it", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  base <- shiny::reactiveVal("GHA")
  rf <- fake_research(delay = 0.05)
  args <- research_args(fake_chat_factory(list("x")), rf)
  args$base_unit <- base
  shiny::testServer(mod_report_server, args = args, {
    session$setInputs(use_research = TRUE, depth = "10")
    pump(session, function() length(called(rf)) >= 1L, timeout = 5)
    base("NGA")
    pump(session, function() length(called(rf, "NGA")) == N_SECTIONS)
    expect_lt(length(called(rf)), N_SECTIONS)  # Ghana stopped after the section under way
    expect_length(called(rf, "NGA"), N_SECTIONS)
  })
})

test_that("server: a failure in the research run itself is reported, and Generate works again", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  broken <- function(leaf, ev, policy) list(leaf = leaf, sources = "not a table", findings = "nor this")
  f <- fake_chat_factory(list(REWRITTEN, REC_TEXT))
  shiny::testServer(mod_report_server, args = research_args(f, broken), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "error"), timeout = 5)
    expect_identical(r_status()$state, "error")
    expect_match(r_status()$msg, "research could not be completed", fixed = TRUE)
    session$setInputs(use_research = FALSE, generate = 2)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_status()$state, "done")
  })
})

test_that("server: with research unticked, applying a country starts no research", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  base <- shiny::reactiveVal("GHA")
  rf <- fake_research()
  args <- research_args(fake_chat_factory(list("x")), rf)
  args$base_unit <- base
  shiny::testServer(mod_report_server, args = args, {
    session$setInputs(use_research = FALSE, depth = "10")
    base("NGA")
    pump(session, function() FALSE, timeout = 0.5)
    expect_length(attr(rf, "calls")(), 0L)
  })
})

test_that("server: titles and quotes from outside are shown as text, never as HTML", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(title = "Review <script>alert(1)</script>")
  f <- fake_chat_factory(list(HRM_TEXT, "[[section:recommendations]]\n- Rec [G5-1]."))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_false(grepl("<script>", as.character(output$sources$html), fixed = TRUE))
    expect_false(grepl("<script>", as.character(output$references$html), fixed = TRUE))
  })
})

# -- the narrative format, a focus, and over-time charts ---

NARR_HRM <- paste("[[section:public_sector_hrm]]", "**Merit matters for delivery [G2.2-1].** More text.",
                  "[[section:questions_public_sector_hrm]]", "- Is pay competitive? [G2.2-3]", sep = "\n")

test_that("the report tab offers a writing format (structured by default) and a focus box", {
  html <- as.character(mod_report_ui("report"))
  expect_match(html, "report-format", fixed = TRUE)
  expect_match(html, "report-focus", fixed = TRUE)
  expect_match(html, "<option value=\"structured\" selected>", fixed = TRUE)
})

test_that("server: a narrative report with a focus gathers its questions at the end", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  f <- fake_chat_factory(list(NARR_HRM, REC_TEXT))
  shiny::testServer(mod_report_server, args = report_args(factory = f), {
    session$setInputs(scope = "leaf:public_sector_hrm", format = "narrative", focus = "Wage bill first",
                      generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    expect_identical(r_plan()$format, "narrative")
    expect_match(r_plan()$calls[[1]]$prompt$user, "Wage bill first", fixed = TRUE)
    expect_identical(nrow(r_result()$verification$issues), 0L)
    preview <- as.character(output$preview$html)
    expect_match(preview, "Questions for further analysis", fixed = TRUE)
    expect_match(as.character(output$sec_questions_public_sector_hrm$html), "Is pay competitive", fixed = TRUE)
  })
})

test_that("server: rewriting after an exclusion keeps the draft's format and focus", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  cites <- sub("More text.", "Pay is high [S1.1].", NARR_HRM, fixed = TRUE)
  f <- fake_chat_factory(list(cites, REC_TEXT, NARR_HRM))
  shiny::testServer(mod_report_server, args = research_args(f, fake_research()), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10",
                      format = "narrative", focus = "Wage bill first", generate = 1)
    pump(session, function() identical(r_status()$state, "done"))
    session$setInputs(exclude = "wb:1")
    session$setInputs(rewrite = 1)
    pump(session, function() identical(r_status()$state, "done") && !is.null(r_result()))
    expect_identical(attr(f, "calls")(), 3L)
    expect_identical(r_plan()$format, "narrative")
    expect_match(attr(f, "prompts")()[[3]]$user, "Wage bill first", fixed = TRUE)
  })
})

test_that("the overview and each cluster show their over-time chart beside the static one", {
  ev <- build_evidence("GHA", COMPS)
  views <- function(key) vapply(cgjrapp:::.section_charts(ev, key), `[[`, "", "view")
  expect_setequal(views("overview"), c("static", "dynamic"))
  expect_setequal(views("institutional_environment"), c("static", "dynamic"))
  expect_identical(views("public_sector_hrm"), "static")  # sections have no over-time chart
})

test_that("server: research that yields no checked findings still writes the report, and says so", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- function(leaf, ev, policy) { r <- research_result(leaf); r$findings <- r$findings[0, ]; r }
  f <- fake_chat_factory(list(REWRITTEN, REC_TEXT))
  shiny::testServer(mod_report_server, args = research_args(f, rf), {
    session$setInputs(scope = "leaf:public_sector_hrm", use_research = TRUE, depth = "10", generate = 1)
    pump(session, function() r_status()$state %in% c("done", "error"))
    expect_identical(r_status()$state, "done")
    expect_false(grepl("RESEARCH", r_plan()$calls[[1]]$prompt$user, fixed = TRUE))
    expect_match(as.character(output$references$html), "no findings", fixed = TRUE)
  })
})

test_that("server: the sources view shows only the sections of the scope selected", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(delay = 0.01)
  shiny::testServer(mod_report_server, args = research_args(fake_chat_factory(list("x")), rf), {
    session$setInputs(use_research = TRUE, depth = "10", scope = "leaf:public_sector_hrm")
    pump(session, function() research_task$status() == "success")
    src <- as.character(output$sources$html)
    expect_match(src, "<h5>Public Sector HRM</h5>", fixed = TRUE)
    expect_false(grepl("<h5>Degree of Integrity</h5>", src, fixed = TRUE))
    session$setInputs(scope = "cluster:institutional_environment")  # its sections are researched now
    pump(session, function() "degree_of_integrity" %in% called(rf) && research_task$status() == "success")
    src <- as.character(output$sources$html)
    expect_match(src, "<h5>Degree of Integrity</h5>", fixed = TRUE)
    expect_false(grepl("<h5>Public Sector HRM</h5>", src, fixed = TRUE))
  })
})

test_that("server: early research covers only the scope selected; a new scope replaces what is queued", {
  httr2::local_mocked_responses(function(req) httr2::response(200))
  rf <- fake_research(delay = 0.05)
  shiny::testServer(mod_report_server, args = research_args(fake_chat_factory(list("x")), rf), {
    session$setInputs(scope = "cluster:core_governance_functions", use_research = TRUE, depth = "10")
    pump(session, function() length(called(rf)) >= 1L, timeout = 5)
    session$setInputs(scope = "leaf:degree_of_integrity")
    pump(session, function() research_task$status() == "success")
    core <- cgjrapp:::.report_leaves(build_evidence("GHA", COMPS), "cluster:core_governance_functions")
    expect_true(called(rf)[[1]] %in% core)          # the section under way was finished
    expect_lt(length(called(rf)), length(core) + 1L)  # the rest of the old scope was dropped
    expect_identical(utils::tail(called(rf), 1), "degree_of_integrity")
    expect_false(any(c("social_cohesion_norms_and_cooperation", "soe_governance") %in% called(rf)))
  })
})
