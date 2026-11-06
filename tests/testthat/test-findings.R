# Findings: an LLM states what a source's passages establish, each finding
# with a verbatim quote that code checks against the passage. The LLM is a
# stand-in (`ask` returns a canned reply); no network.

POLICY <- load_source_policy()

# A source and two of its passages, as reading leaves them
SOURCE <- data.frame(key = "wb:1", title = "Who Serves the Poor? Surveying Civil Servants",
                     year = 2017L, publisher = "World Bank", journal = NA_character_,
                     collection = "Policy Research Working Paper", access = "full_text")
PASSAGES <- data.frame(
  key = "wb:1", passage = c(4L, 9L), file_page = c(11L, 16L), page = c("9", NA),
  text = c("Civil servants in Ghana report that 42 percent of their time is spent on routine tasks.",
           "Managers with more autonomy were associated with higher project completion rates."))

test_that("the findings prompt gives the section, the source and its passages with pages", {
  p <- cgjrapp:::.findings_prompt(SOURCE, PASSAGES, "Public Sector HRM", c("civil service"), "Ghana")
  expect_match(p$user, "SECTION: Public Sector HRM (civil service)", fixed = TRUE)
  expect_match(p$user, "COUNTRY: Ghana", fixed = TRUE)
  expect_match(p$user, "SOURCE: Who Serves the Poor? Surveying Civil Servants (World Bank, 2017)", fixed = TRUE)
  expect_match(p$user, "P1 (p. 9): Civil servants in Ghana", fixed = TRUE)
  expect_match(p$user, "P2 (p. -): Managers with more autonomy", fixed = TRUE)
  expect_match(p$system, "verbatim")
  expect_match(p$system, "JSON")
})

test_that("evidence from other countries is asked for too, not only findings about the country", {
  # without this, a country with no studies of its own (Benin) got no findings at all
  p <- cgjrapp:::.findings_prompt(SOURCE, PASSAGES, "Justice and Rule of Law", "judiciary", "Benin")
  expect_match(p$user, "COUNTRY: Benin (findings about other countries or several countries are wanted too",
               fixed = TRUE)
  intl <- cgjrapp:::.findings_prompt(SOURCE, PASSAGES, "Justice and Rule of Law", "judiciary", NULL)
  expect_match(intl$user, "COUNTRY: none - international research\n", fixed = TRUE)
})

# A reply with one finding per check: kept, then each reason to drop one
reply_of <- function(...) jsonlite::toJSON(list(...), auto_unbox = TRUE)
finding <- function(passage, quote, finding, claim_type = "descriptive", scope = "country") {
  list(passage = passage, quote = quote, finding = finding, claim_type = claim_type, scope = scope)
}

test_that("a finding is kept only when its quote is in its passage and its numbers in the quote", {
  reply <- reply_of(
    finding("P1", "report that 42 percent of their time is spent on routine tasks",
            "Ghanaian civil servants spend 42 percent of their time on routine tasks."),
    finding("P1", "half of their time", "Civil servants spend half their time on routine work."),
    finding("P1", "42 percent of their time", "They spend 42 percent, or 3 days a week, on routine tasks."),
    finding("P7", "anything", "A finding from a passage that was not sent."),
    finding("P2", "Managers with more autonomy", "Autonomy raises completion.", claim_type = "proven"))
  r <- cgjrapp:::.parse_findings(reply, PASSAGES)
  expect_identical(r$findings$passage, 4L)  # the source's own passage number
  expect_identical(r$findings$page, "9")
  expect_identical(r$findings$quote, "report that 42 percent of their time is spent on routine tasks")
  expect_identical(r$dropped$reason,
                   c("quote_not_found", "number_not_in_quote", "unknown_passage", "invalid_field"))
})

# -- .extract_findings() ---

# Two selected sources with passages, and a third not selected
SOURCES <- rbind(SOURCE, transform(SOURCE, key = "wb:2", title = "Second"),
                 transform(SOURCE, key = "wb:3", title = "Not selected"))
SOURCES$selected <- c(TRUE, TRUE, FALSE)
ALL_PASSAGES <- rbind(PASSAGES, transform(PASSAGES, key = "wb:2"), transform(PASSAGES, key = "wb:3"))
good <- function(n) do.call(reply_of, rep(list(finding(
  "P1", "42 percent of their time", "Civil servants spend 42 percent of their time on routine tasks.")), n))
extract <- function(ask, policy = POLICY, cache = NULL) {
  cgjrapp:::.extract_findings(SOURCES, ALL_PASSAGES, "public_sector_hrm", "Ghana", policy, ask, cache)
}

test_that("each selected source is asked once; findings are numbered within their source", {
  asked <- character(0)
  ask <- function(system, prompt) {
    asked <<- c(asked, regmatches(prompt, regexpr("SOURCE: [^(]+", prompt)))
    good(4L)  # one more than the policy allows per source
  }
  r <- extract(ask)
  expect_identical(trimws(asked), c("SOURCE: Who Serves the Poor? Surveying Civil Servants", "SOURCE: Second"))
  expect_identical(r$findings$key, rep(c("wb:1", "wb:2"), each = 3L))
  expect_identical(r$findings$finding_id, rep(1:3, 2L))
  expect_identical(r$status$status, c("ok", "ok"))
})

test_that("the section's findings stop at the policy's limit, best-ranked sources first", {
  policy <- POLICY
  policy$limits$max_findings_per_leaf <- 4L
  r <- extract(function(system, prompt) good(3L), policy)
  expect_identical(r$findings$key, c("wb:1", "wb:1", "wb:1", "wb:2"))
})

test_that("a source whose call fails has no findings; the others are unaffected", {
  ask <- function(system, prompt) if (grepl("SOURCE: Second", prompt)) stop("HTTP 503") else good(1L)
  r <- extract(ask)
  expect_identical(r$findings$key, "wb:1")
  expect_identical(r$status$status, c("ok", "failed"))
})

test_that("dropped findings are kept for the record, with their source and reason", {
  ask <- function(system, prompt) reply_of(finding("P1", "words not in the passage", "A claim."))
  r <- extract(ask)
  expect_identical(r$dropped$key, c("wb:1", "wb:2"))
  expect_identical(r$dropped$reason, c("quote_not_found", "quote_not_found"))
  expect_identical(r$status$dropped, c(1L, 1L))
})

test_that("findings from the same passages are not asked for again", {
  cache <- cachem::cache_mem()
  calls <- 0L
  ask <- function(system, prompt) { calls <<- calls + 1L; good(1L) }
  extract(ask, cache = cache)
  extract(ask, cache = cache)
  expect_identical(calls, 2L)  # two sources, once each
})

test_that("a quote may leave words out with an ellipsis, if every piece is in the passage in order", {
  # as seen live: the model quotes "A ... B", skipping text between
  passages <- PASSAGES
  passages$text[[1]] <- paste("Hiring takes place as part of the budget process.",
                              "MDAs submit plans to the ministry. Each service manages its own staff.")
  q <- function(quote) {
    nrow(cgjrapp:::.parse_findings(reply_of(finding("P1", quote, "Hiring follows the budget.")), passages)$findings)
  }
  expect_identical(q("Hiring takes place as part of the budget process. ... Each service manages its own staff."), 1L)
  expect_identical(q("Hiring takes place as part of the budget process. … Each service manages its own staff."), 1L)
  # pieces out of order, a piece not in the passage, or a piece too short to mean anything
  expect_identical(q("Each service manages its own staff. ... Hiring takes place as part of the budget"), 0L)
  expect_identical(q("Hiring takes place as part of the budget process. ... Every ministry hires centrally."), 0L)
  expect_identical(q("Hiring takes place ... the"), 0L)
})

test_that("a finding called causal whose quote reports an association is descriptive", {
  # as seen live: "managerial practices are associated with greater ...
  # corruption" was labelled causal
  r <- cgjrapp:::.parse_findings(reply_of(
    finding("P2", "Managers with more autonomy were associated with higher project completion rates",
            "Autonomy raises completion rates.", claim_type = "causal"),
    finding("P1", "report that 42 percent of their time is spent on routine tasks",
            "Civil servants spend 42 percent of their time on routine tasks.", claim_type = "causal")),
    PASSAGES)
  expect_identical(r$findings$claim_type, c("descriptive", "causal"))
})

test_that("a quote that mends a word the text split is still found", {
  # as read live: World Bank text files break words ("i s driven", "o f
  # teachers"); a quote that writes them whole is the same text
  passages <- PASSAGES
  passages$text[[1]] <- "In Ghana's case, wage bill i s driven by new hiring, particularly o f teachers."
  reply <- reply_of(finding("P1", "wage bill is driven by new hiring, particularly of teachers",
                            "Ghana's wage bill is driven by new hiring of teachers."))
  expect_identical(nrow(cgjrapp:::.parse_findings(reply, passages)$findings), 1L)
})

test_that("small differences in spacing, quote marks and dashes do not fail a quote", {
  passages <- PASSAGES
  passages$text[[2]] <- "Managers with “more” autonomy were associated with higher completion rates."
  reply <- reply_of(finding("P2", "Managers  with \"more\" autonomy were associated",
                            "More autonomy goes with higher completion.", scope = "cross_country"))
  expect_identical(nrow(cgjrapp:::.parse_findings(reply, passages)$findings), 1L)
})

test_that("no text from the guidance note ever reaches a findings prompt", {
  prompts <- character(0)
  ask <- function(system, prompt) { prompts <<- c(prompts, system, prompt); "[]" }
  extract(ask)
  expect_true(any(grepl("^SECTION: ", prompts)))  # the prompts were captured
  expect_identical(guidance_leaks(prompts), character(0))
})
