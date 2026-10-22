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
    expect_true(any(s$text == "Appendix: automated verification"))
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
