# Live LLM smoke test: one report section and one chat turn against the
# configured provider (CGJR_LLM_* env vars). Skipped unless
# CGJR_LLM_LIVE_TEST=true, so CI and ordinary test runs never call out.
#
# Uses whichever guidance CGJR_GUIDANCE_SOURCE selects (default: the
# stand-in). Run the real, Official Use Only guidance only against an
# approved endpoint (WBG mAI).

skip_if_not(identical(Sys.getenv("CGJR_LLM_LIVE_TEST"), "true"),
            "set CGJR_LLM_LIVE_TEST=true to call the live LLM")

test_that("the live provider writes a verifiable one-section report", {
  expect_identical(check_llm_available(), "ok")
  g <- load_guidance()
  ev <- build_evidence("GHA", cgjrapp:::.default_comparators("GHA"),
                       clusters = "core_governance_functions")
  plan <- plan_ai_report(ev, g, "leaf:public_sector_hrm")
  text <- wait_for(run_ai_report(plan), timeout = 240)
  n <- parse_narrative(text)
  expect_true(all(c("public_sector_hrm", "recommendations") %in% names(n)))
  v <- verify_text(text, ev, g, plan$sections$key)
  message("Live report: ", verification_summary(v))
  expect_gt(nrow(v$cited), 0L)
  expect_false(any(v$issues$type %in% c("unknown_evidence_id", "unknown_guidance_id", "missing_section")))
})

test_that("the live provider answers a chat question with citations", {
  g <- load_guidance()
  chat <- llm_chat(cgjrapp:::.chat_system_prompt(g, NULL))
  text <- wait_for(cgjrapp:::.stream_text(chat, "What does the guidance ask teams to check about public procurement?"),
                   timeout = 120)
  v <- verify_text(text, NULL, g)
  message("Live chat: ", verification_summary(v))
  expect_gt(sum(v$cited$source == "guidance" & v$cited$found), 0L)
})
