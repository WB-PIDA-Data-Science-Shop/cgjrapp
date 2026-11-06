# One report section's research, start to finish, and the pack of several
# sections. Providers are mocked (helper-discover.R) and the LLM is a
# stand-in; no network.

POLICY <- load_source_policy()
EV <- build_evidence("GHA", cgjrapp:::.default_comparators("GHA"), clusters = "core_governance_functions")

test_that("a section's research runs discovery, reading and findings, and reports each step", {
  local_research(crossref = list(`10.1596/1813-9450-10390` = research_fixture("crossref_prwp_10390.json")$message))
  ask <- function(system, prompt) "[]"  # the LLM rates nothing and finds nothing
  r <- cgjrapp:::.research_section("public_sector_hrm", EV, POLICY, ask = ask, cache = NULL,
                                   resolve = fake_dns())
  expect_identical(r$leaf, "public_sector_hrm")
  expect_true(all(c("key", "selected", "access", "tier", "relevance") %in% names(r$sources)))
  expect_identical(nrow(r$findings), 0L)
  expect_identical(r$screening, "failed")  # "[]" rates nothing
  expect_true(all(c("provider", "status") %in% names(r$providers)))
})

test_that("several sections' research makes one pack, a shared source numbered once", {
  s <- cgjrapp:::.as_sources(provider_rows("wb_documents", wb_id = c("1", "2"), title = c("A", "B"),
                                           year = 2020L, kind = "report"))
  f <- function(leaf, key) data.frame(leaf = leaf, key = key, finding_id = 1L, passage = 1L,
                                      file_page = 1L, page = NA_character_, quote = "q",
                                      finding = "f", claim_type = "descriptive", scope = "country")
  results <- list(public_sector_hrm = list(sources = s, findings = f("public_sector_hrm", "wb:1")),
                  digital_and_data = list(sources = s, findings = rbind(f("digital_and_data", "wb:1"),
                                                                        f("digital_and_data", "wb:2"))),
                  service_delivery = list(error = "HTTP 503"))  # a section whose research failed
  p <- cgjrapp:::.research_pack_of(results)
  expect_identical(p$findings$id, c("S1.1", "S1.2", "S2.1"))
  expect_identical(p$sources$id, c("S1", "S2"))
})

test_that("a background process runs the package's own code, also when run from source", {
  skip_on_cran()
  m <- cgjrapp:::.in_background(".report_leaves", EV, "leaf:public_sector_hrm")
  expect_identical(mirai::call_mirai(m)$data, "public_sector_hrm")
})

test_that("research with no checked findings, or no section researched, makes no pack", {
  s <- cgjrapp:::.as_sources(provider_rows("wb_documents", wb_id = "1", title = "A", year = 2020L, kind = "report"))
  none <- data.frame(leaf = character(0), key = character(0), finding_id = integer(0), passage = integer(0),
                     file_page = integer(0), page = character(0), quote = character(0),
                     finding = character(0), claim_type = character(0), scope = character(0))
  expect_null(cgjrapp:::.research_pack_of(list(justice_and_rule_of_law = list(sources = s, findings = none))))
  expect_null(cgjrapp:::.research_pack_of(list(justice_and_rule_of_law = list(error = "HTTP 503"))))
})

test_that("a scope's sections are its own, whatever evidence was built", {
  chapter <- build_evidence("GHA", cgjrapp:::.default_comparators("GHA"))
  expect_identical(cgjrapp:::.report_leaves(chapter, "cluster:context"),
                   c("political_institutions_and_social_cohesion", "social_cohesion_norms_and_cooperation"))
  expect_identical(cgjrapp:::.report_leaves(chapter, "leaf:public_sector_hrm"), "public_sector_hrm")
  expect_length(cgjrapp:::.report_leaves(chapter, "chapter"), 14L)
})
