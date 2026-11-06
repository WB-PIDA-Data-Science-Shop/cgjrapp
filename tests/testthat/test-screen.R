# Relevance screening: an LLM rates each verified candidate's fit to a report
# section. The LLM is a stand-in (`ask` returns a canned reply); no network.

POLICY <- load_source_policy()

# Verified candidates, as discovery leaves them (only what screening reads)
candidates <- function(n = 3L) {
  data.frame(key = paste0("wb:", seq_len(n)), title = paste("Title", seq_len(n)),
             year = 2000L + seq_len(n), publisher = "World Bank",
             collection = "Policy Research Working Paper", journal = NA_character_,
             abstract = paste("Abstract", seq_len(n)), citable = TRUE)
}

test_that("the screening prompt names the section and country and lists each candidate", {
  c3 <- candidates()
  c3$abstract[[2]] <- strrep("x", 2000)
  p <- cgjrapp:::.screening_prompt(c3, "Public Sector HRM", c("civil service", "public employment"), "Ghana")
  expect_match(p$user, "SECTION: Public Sector HRM (civil service, public employment)", fixed = TRUE)
  expect_match(p$user, "COUNTRY: Ghana", fixed = TRUE)
  expect_match(p$user, "C1 | 2001 | World Bank, Policy Research Working Paper | Title 1", fixed = TRUE)
  expect_match(p$user, "C3 |", fixed = TRUE)
  # an abstract is cut to 600 characters
  expect_false(grepl(strrep("x", 601), p$user, fixed = TRUE))
  expect_match(p$system, "JSON")
})

test_that("a screening reply is read only for the ids sent and the three ratings", {
  reply <- paste0("Here you are:\n```json\n",
                  '[{"id": "C1", "relevance": "high", "reason": "Directly on point"},',
                  ' {"id": "C2", "relevance": "LOW", "reason": "Off topic"},',
                  ' {"id": "C3", "relevance": "maybe", "reason": "?"},',
                  ' {"id": "C9", "relevance": "high", "reason": "Not a candidate"}]\n```')
  r <- cgjrapp:::.parse_screening(reply, ids = c("C1", "C2", "C3"))
  expect_identical(r$id, c("C1", "C2"))
  expect_identical(r$relevance, c("high", "low"))
  expect_identical(r$reason, c("Directly on point", "Off topic"))
  # nothing readable: no ratings
  expect_identical(nrow(cgjrapp:::.parse_screening("Sorry, I cannot help.", "C1")), 0L)
})

test_that("screening rates each citable candidate; one the reply skips is unscreened", {
  s <- candidates()
  s$citable[[3]] <- FALSE  # not citable: never sent
  sent <- NULL
  ask <- function(system, prompt) {
    sent <<- prompt
    '[{"id": "C1", "relevance": "low", "reason": "Off topic"}]'
  }
  r <- cgjrapp:::.screen_relevance(s, "public_sector_hrm", "Ghana", POLICY, ask)
  expect_identical(r$status, "ok")
  expect_identical(r$sources$relevance, c("low", "unscreened", NA))
  expect_identical(r$sources$relevance_reason, c("Off topic", NA, NA))
  expect_match(sent, "Public Sector HRM (civil service, public employment)", fixed = TRUE)
  expect_false(grepl("Title 3", sent, fixed = TRUE))
})

test_that("at most the policy's number of candidates go in one screening call", {
  policy <- POLICY
  policy$limits$max_screened_per_leaf <- 2L
  sent <- NULL
  ask <- function(system, prompt) { sent <<- prompt; '[]' }
  r <- cgjrapp:::.screen_relevance(candidates(), "public_sector_hrm", "Ghana", policy, ask)
  expect_match(sent, "C2 |", fixed = TRUE)
  expect_false(grepl("C3 |", sent, fixed = TRUE))
})

test_that("a section screened before is not screened again", {
  cache <- cachem::cache_mem()
  calls <- 0L
  ask <- function(system, prompt) { calls <<- calls + 1L; '[{"id": "C1", "relevance": "high", "reason": "x"}]' }
  cgjrapp:::.screen_relevance(candidates(), "public_sector_hrm", "Ghana", POLICY, ask, cache)
  again <- cgjrapp:::.screen_relevance(candidates(), "public_sector_hrm", "Ghana", POLICY, ask, cache)
  expect_identical(calls, 1L)
  expect_identical(again$sources$relevance[[1]], "high")
})

test_that("a screening call that fails, or a reply with no ratings, leaves all unscreened", {
  s <- candidates()
  failing <- function(system, prompt) stop("HTTP 503 Service Unavailable")
  r <- cgjrapp:::.screen_relevance(s, "public_sector_hrm", "Ghana", POLICY, failing)
  expect_identical(r$status, "failed")
  expect_identical(r$sources$relevance, rep("unscreened", 3))
  unreadable <- function(system, prompt) "I'd rather not."
  expect_identical(cgjrapp:::.screen_relevance(s, "public_sector_hrm", "Ghana", POLICY, unreadable)$status,
                   "failed")
})
