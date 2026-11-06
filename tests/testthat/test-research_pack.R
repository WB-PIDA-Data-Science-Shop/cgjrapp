# The research pack: verified findings numbered as citable ids (S3.2 =
# source 3, finding 2) with what the report writer needs about each.
# Synthetic findings; no network.

pack_sources <- function() {
  s <- cgjrapp:::.as_sources(provider_rows(
    "wb_documents", wb_id = c("1", "2"), title = c("Who Serves the Poor?", "Pay Reform in Africa"),
    year = c(2017L, 1991L), publisher = "World Bank", kind = "report",
    authors = list(people("Rogger", "Daniel"), institution_author("World Bank"))))
  s
}
pack_findings <- function() {
  data.frame(leaf = c("public_sector_hrm", "public_sector_hrm", "public_sector_hrm", "service_delivery"),
             key = c("wb:1", "wb:1", "wb:2", "wb:1"), finding_id = c(1L, 2L, 1L, 1L),
             passage = 1L, file_page = 11L, page = c("9", NA, "3", "12"),
             quote = c("42 percent of their time", "managers have autonomy", "pay was cut", "q"),
             finding = c("Civil servants spend 42 percent of their time on routine tasks.",
                         "Managers have autonomy.", "Pay was cut.", "Services are uneven."),
             claim_type = c("descriptive", "descriptive", "policy", "descriptive"),
             scope = c("country", "country", "cross_country", "country"))
}

test_that("findings are numbered by source; a source in two sections keeps one number", {
  p <- cgjrapp:::.research_pack(pack_sources(), pack_findings())
  expect_identical(p$findings$id, c("S1.1", "S1.2", "S2.1", "S1.3"))
  expect_identical(p$sources$id, c("S1", "S2"))
  expect_identical(p$findings$leaf, pack_findings()$leaf)
})

test_that("each finding is described as the report writer will see it", {
  p <- cgjrapp:::.research_pack(pack_sources(), pack_findings())
  expect_identical(p$findings$line[[1]], paste0(
    "[S1.1] Who Serves the Poor? (World Bank, 2017), p. 9 - descriptive, about the country: ",
    "Civil servants spend 42 percent of their time on routine tasks. ",
    "Quote: \"42 percent of their time\""))
  expect_match(p$findings$line[[3]], "p. 3 - policy, from several countries: ", fixed = TRUE)
  expect_match(p$findings$line[[2]], "(World Bank, 2017) - descriptive", fixed = TRUE)  # no page known
})
