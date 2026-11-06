# Source policy: loading and checking inst/config/source_policy.yml, and
# classifying candidate sources into tiers. No network.

test_that("the bundled policy has five tiers and identified journals", {
  p <- load_source_policy()
  expect_identical(p$tiers$tier, 1:5)
  expect_gte(nrow(p$journals), 16L)
  expect_true(all(lengths(p$journals$issn) >= 1L))
  expect_match(p$journals$openalex_id, "^S[0-9]+$")
})

test_that("CGJR_SOURCE_POLICY points the app at another policy file", {
  path <- local_policy_file(function(y) { y$journals <- y$journals[1]; y })
  withr::local_envvar(CGJR_SOURCE_POLICY = path)
  expect_identical(nrow(load_source_policy()$journals), 1L)
})

test_that("a missing policy file is an error naming the path", {
  expect_error(load_source_policy("no/such/policy.yml"), "no/such/policy.yml")
})

test_that("an ISSN with a wrong check digit is an error naming the journal", {
  # 0304-3878 is valid: 0*8 + 3*7 + 0*6 + 4*5 + 3*4 + 8*3 + 7*2 = 91, and
  # (11 - 91 %% 11) %% 11 = 8; changing the last digit to 7 breaks it
  path <- local_policy_file(function(y) { y$journals[[1]]$issn[[1]] <- "0304-3877"; y })
  expect_error(load_source_policy(path), "jde.*0304-3877")
})

test_that("a journal listed twice, or two journals sharing an ISSN, is an error", {
  twice <- local_policy_file(function(y) { y$journals[[2]]$id <- "jde"; y })
  expect_error(load_source_policy(twice), "Duplicate journal id.*jde")
  # World Development given JDE's print ISSN: a match would be ambiguous
  shared <- local_policy_file(function(y) { y$journals[[2]]$issn[[1]] <- "0304-3878"; y })
  expect_error(load_source_policy(shared), "ISSN listed for more than one journal.*0304-3878")
})

test_that("an unknown status or tier, or a missing section, is an error", {
  status <- local_policy_file(function(y) { y$journals[[1]]$status <- "maybe"; y })
  expect_error(load_source_policy(status), "status.*jde.*maybe")
  tier <- local_policy_file(function(y) { y$institutions[[2]]$tier <- 7L; y })
  expect_error(load_source_policy(tier), "tier.*IMF")
  section <- local_policy_file(function(y) { y$series <- NULL; y })
  expect_error(load_source_policy(section), "missing.*series")
})

test_that("each research limit must be given as a positive whole number", {
  zero <- local_policy_file(function(y) { y$limits$max_findings_per_leaf <- 0L; y })
  expect_error(load_source_policy(zero), "max_findings_per_leaf")
  frac <- local_policy_file(function(y) { y$limits$request_timeout_s <- 2.5; y })
  expect_error(load_source_policy(frac), "request_timeout_s")
  gone <- local_policy_file(function(y) { y$limits$max_redirects <- NULL; y })
  expect_error(load_source_policy(gone), "max_redirects")
})

test_that("the policy carries search settings, with terms only for real report sections", {
  s <- load_source_policy()$search
  expect_identical(s$wb_document_groups,
                   c("Publications & Research", "Economic & Sector Work", "Country Focus"))
  expect_identical(s$terms$public_sector_hrm, c("civil service", "public employment"))
  typo <- local_policy_file(function(y) { y$search$terms$public_sector_hmr <- "x"; y })
  expect_error(load_source_policy(typo), "Unknown report section.*public_sector_hmr")
})

# -- classify_sources() ---

test_that("a World Bank report is a tier 1 source", {
  expect_identical(classify_one("report", institution = "WBG"),
                   list(tier = 1L, eligible = TRUE, reason = "ok"))
})

test_that("an IMF working paper is tier 2, flagged as not peer reviewed", {
  expect_identical(classify_one("working_paper", institution = "IMF"),
                   list(tier = 2L, eligible = TRUE, reason = "not_peer_reviewed"))
})

test_that("a listed journal is recognised by print ISSN, electronic ISSN or OpenAlex id", {
  # Journal of Development Economics: 0304-3878 (print), 1872-6089 (online)
  tier3 <- list(tier = 3L, eligible = TRUE, reason = "ok")
  expect_identical(classify_one("journal_article", issn = list("0304-3878")), tier3)
  expect_identical(classify_one("journal_article", issn = list("1872-6089")), tier3)
  expect_identical(classify_one("journal_article", openalex_source_id = "S101209419"), tier3)
})

test_that("a journal's name alone never makes it a listed journal", {
  # same title as a listed journal, but an ISSN the registry does not hold
  # (1234-5679 has a valid check digit, so only the registry can reject it)
  x <- candidates("journal_article", issn = list("1234-5679"))
  x$journal <- "Journal of Development Economics"
  r <- classify_sources(x)
  expect_identical(as.list(r[1, c("tier", "eligible", "reason")]),
                   list(tier = NA_integer_, eligible = FALSE, reason = "journal_not_in_registry"))
  expect_identical(r$journal, x$journal)
})

test_that("an article in a journal the policy excludes is not eligible", {
  policy <- load_source_policy(local_policy_file(function(y) {
    y$journals[[1]]$status <- "excluded"; y
  }))
  r <- classify_sources(candidates("journal_article", issn = list("0304-3878")), policy)
  expect_identical(as.list(r[1, c("tier", "eligible", "reason")]),
                   list(tier = 3L, eligible = FALSE, reason = "journal_excluded"))
})

test_that("a report from a publisher outside the policy is not eligible", {
  expect_identical(classify_one("report", institution = "Some Think Tank"),
                   list(tier = NA_integer_, eligible = FALSE, reason = "publisher_not_eligible"))
})

test_that("a paper in a listed working-paper series is tier 5, not peer reviewed", {
  expect_identical(classify_one("working_paper", series = "nber_wp"),
                   list(tier = 5L, eligible = TRUE, reason = "not_peer_reviewed"))
})

test_that("an institution's tier wins over its working-paper series", {
  # a World Bank paper also issued in a tier 5 series stays tier 1
  expect_identical(classify_one("working_paper", institution = "WBG", series = "nber_wp")$tier, 1L)
})

test_that("blogs, news and other kinds outside the policy are not eligible", {
  # even from a tier 1 publisher
  r <- classify_sources(candidates(c("blog", "news"), institution = "WBG"))
  expect_identical(r$eligible, c(FALSE, FALSE))
  expect_identical(r$reason, c("type_excluded", "type_excluded"))
})

test_that("a retracted article is never eligible, even in a listed journal", {
  expect_identical(classify_one("journal_article", issn = list("0304-3878"), is_retracted = TRUE),
                   list(tier = 3L, eligible = FALSE, reason = "retracted"))
})

test_that("a candidate of unknown kind is not eligible: its metadata is missing", {
  expect_identical(classify_one(NA_character_, institution = "WBG"),
                   list(tier = 1L, eligible = FALSE, reason = "missing_metadata"))
})

test_that("no candidates give an empty table with the classification columns", {
  r <- classify_sources(candidates(character(0)))
  expect_identical(nrow(r), 0L)
  expect_true(all(c("tier", "eligible", "reason") %in% names(r)))
})
