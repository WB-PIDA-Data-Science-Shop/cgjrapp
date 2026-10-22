# AI Chat prompts: what the model is given once per conversation (the system
# prompt) and with each question. Hand-made passages; no network.

test_that("the system prompt lists the guidance's sections, not its passage texts", {
  sys <- .chat_system_prompt(mini_guidance(), NULL)
  expect_match(sys, "2.1.3 Public Procurement", fixed = TRUE)
  expect_match(sys, "1.1 Degree of Integrity", fixed = TRUE)
  expect_false(grepl("tenders are competitive", sys, fixed = TRUE))
  expect_false(grepl("[G2.1.3-1]", sys, fixed = TRUE))
})
