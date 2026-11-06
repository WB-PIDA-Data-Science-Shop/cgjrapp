# verify_text(): deterministic checks on LLM output. Synthetic facts and the
# stand-in guidance only.

.VF_EV <- list(facts = data.frame(
  id = c("E1", "E2", "E3"),
  view = c("static", "static", "dynamic"),
  ctf = c(0.655, 0.412, NA), comp_median = c(0.527, 0.480, NA),
  rank = c(3L, 6L, NA), n_ranked = c(7L, 7L, NA),
  delta = c(NA, NA, -0.089),
  rank_first = c(NA, NA, 1L), n_first = c(NA, NA, 7L),
  rank_last = c(NA, NA, 3L), n_last = c(NA, NA, 7L),
  text = c("Alpha (indicator) for Testland: CTF 0.655; Strong (top 50%); rank 3 of 7; comparator median 0.527.",
           "Beta (indicator) for Testland: CTF 0.412; Weak (bottom 25%); rank 6 of 7; comparator median 0.480.",
           "Gamma (indicator) for Testland, 2014-2024: change in CTF -0.089; rank from 1 of 7 to 3 of 7.")
))
.VF_G <- load_guidance("standin")

types <- function(v) v$issues$type

test_that("clean, fully cited text passes", {
  txt <- paste(
    "[[section:degree_of_integrity]]",
    "Testland scores 0.655 [E1], above the comparator median of 0.527 [E1], ranking 3 of 7 [E1].",
    "Its Gamma score fell by 0.089 [E3]. Integrity matters for investment [G1.1-1].",
    "Meritocratic recruitment matters (Evans and Rauch 1999) [G2.2-1].", sep = "\n")
  v <- verify_text(txt, .VF_EV, .VF_G, expected_sections = "degree_of_integrity")
  expect_identical(nrow(v$issues), 0L)
  expect_true(all(v$cited$found))
  expect_setequal(v$cited$id, c("E1", "E3", "G1.1-1", "G2.2-1"))
  expect_match(verification_summary(v), "all found.*No other issues")
})

test_that("unknown evidence and guidance ids are flagged", {
  v <- verify_text("Score 0.655 [E1, E99]. As noted [G9.9-1].", .VF_EV, .VF_G)
  expect_true(all(c("unknown_evidence_id", "unknown_guidance_id") %in% types(v)))
  expect_false(all(v$cited$found))
})

test_that("a wrong CTF, a rounded CTF and a wrong rank are flagged", {
  v <- verify_text("Testland scores 0.665 [E1]. It scores 0.66 [E1]. It ranks 2 of 7 [E1].", .VF_EV, .VF_G)
  expect_identical(sum(types(v) == "figure_not_supported"), 3L)
})

test_that("figures need a citation; trailing citations attach to their sentence", {
  v <- verify_text("Testland scores 0.655 in Alpha.", .VF_EV, .VF_G)
  expect_identical(types(v), "uncited_figure")
  v2 <- verify_text("Testland scores 0.655 in Alpha. [E1]", .VF_EV, .VF_G)
  expect_identical(nrow(v2$issues), 0L)
})

test_that("a bold lead sentence is checked on its own, apart from the sentence after it", {
  v <- verify_text("**Integrity is a strength [E1].** Testland scores 0.412 in Beta.", .VF_EV, .VF_G)
  expect_identical(types(v), "uncited_figure")
  v2 <- verify_text("**Testland scores 0.655 [E1].** Beta is weaker, at 0.412 [E2].", .VF_EV, .VF_G)
  expect_identical(nrow(v2$issues), 0L)
})

test_that("a percentage that matches the cited fact's text passes", {
  v <- verify_text("Alpha is in the top 50% band [E1].", .VF_EV, .VF_G)
  expect_identical(nrow(v$issues), 0L)
  # written out, as the CGJR style has it: the same figure
  v2 <- verify_text("Alpha is in the top 50 percent band [E1].", .VF_EV, .VF_G)
  expect_identical(nrow(v2$issues), 0L)
  v3 <- verify_text("Alpha is in the top 60 percent band [E1].", .VF_EV, .VF_G)
  expect_identical(types(v3), "figure_not_supported")
})

test_that("section numbers, page numbers and years are not figures", {
  v <- verify_text("As section 2.1.1 explains on p.6, budgets matter (2014-2024) [G2.1.1-1].",
                   .VF_EV, .VF_G)
  expect_identical(nrow(v$issues), 0L)
})

test_that("references outside the guidance's list are flagged, listed ones pass", {
  v <- verify_text("Courts matter (Smith 2020) [G1.3-1]. See also North (1990) [G4.1-1].",
                   .VF_EV, .VF_G)
  expect_identical(types(v), "external_reference")
  expect_match(v$issues$detail, "Smith")
})

test_that("missing report sections are flagged", {
  v <- verify_text("[[section:overview]]\nText [GTHE-1].", .VF_EV, .VF_G,
                   expected_sections = c("overview", "recommendations"))
  expect_identical(v$issues$section[v$issues$type == "missing_section"], "recommendations")
})

test_that("uncited figures in a 'Beyond the guidance' block are not flagged", {
  txt <- "Integrity matters [G1.1-1].\n\n**Beyond the guidance (general knowledge, not verified):** about 0.5 of firms report it."
  v <- verify_text(txt, .VF_EV, .VF_G)
  expect_identical(nrow(v$issues), 0L)
})

test_that("evidence citations with no selection loaded are flagged", {
  v <- verify_text("Score 0.655 [E1].", NULL, .VF_G)
  expect_match(v$issues$detail[[1]], "no dashboard selection")
})

test_that("citations render as badges with their source as tooltip", {
  html <- cgjrapp:::.render_cited_html("Score 0.655 [E1, G1.1-1] and [E42].", .VF_EV, .VF_G)
  expect_match(html, 'class="cgjr-cite cgjr-cite-e" title="Alpha \\(indicator\\)')
  expect_match(html, 'class="cgjr-cite cgjr-cite-g" title="Guidance section 1.1')
  expect_match(html, 'cgjr-cite-bad[^>]*>E42<')
  expect_false(grepl("\\[E1", html))
})

# -- research findings [S] ---

# A research pack as .research_pack() makes it: two findings of one source
.VF_R <- list(
  sources = data.frame(id = "S1", year = 2017L, authors = I(list(data.frame(
    family = c("Rasul", "Rogger"), given = c("Imran", "Daniel"), literal = NA_character_)))),
  findings = data.frame(
    id = c("S1.1", "S1.2"), claim_type = c("descriptive", "causal"),
    finding = c("Civil servants spend 42 percent of their time on routine tasks.",
                "Top-down audits cut in-party favoritism from 60 to 20 percent."),
    quote = c("42 percent of their time", "favoritism falls from 60 percent at baseline to 20 percent"),
    line = c("[S1.1] ...", "[S1.2] ...")))

test_that("research findings are cited and checked like the other sources", {
  txt <- paste("[[section:public_sector_hrm]]",
               "Civil servants in Testland spend 42 percent of their time on routine tasks [S1.1].",
               "Audits cut favoritism from 60 to 20 percent [S1.2].", sep = "\n")
  v <- verify_text(txt, .VF_EV, .VF_G, research = .VF_R)
  expect_identical(nrow(v$issues), 0L)
  expect_identical(v$cited$source, c("research", "research"))
  expect_true(all(v$cited$found))
})

test_that("an unknown research id, or research cited with none loaded, is flagged", {
  v <- verify_text("Routine tasks take time [S9.1].", .VF_EV, .VF_G, research = .VF_R)
  expect_identical(types(v), "unknown_research_id")
  expect_identical(types(verify_text("Routine tasks take time [S1.1].", .VF_EV, .VF_G)),
                   "unknown_research_id")
})

test_that("a figure not in the cited finding or its quote is flagged", {
  v <- verify_text("Civil servants spend 45 percent of their time on routine tasks [S1.1].",
                   .VF_EV, .VF_G, research = .VF_R)
  expect_identical(types(v), "figure_not_supported")
})

test_that("a research percentage written the other way ('42%' for '42 percent') is the same figure", {
  v <- verify_text("Civil servants spend 42% of their time on routine tasks [S1.1].",
                   .VF_EV, .VF_G, research = .VF_R)
  expect_identical(nrow(v$issues), 0L)
  v2 <- verify_text("Civil servants spend 43% of their time on routine tasks [S1.1].",
                    .VF_EV, .VF_G, research = .VF_R)
  expect_identical(types(v2), "figure_not_supported")
})

test_that("causal wording on a finding that is not causal is flagged", {
  bad <- verify_text("Routine work causes low productivity [S1.1].", .VF_EV, .VF_G, research = .VF_R)
  expect_identical(types(bad), "causal_claim_unsupported")
  # the causal finding may be stated as a cause
  ok <- verify_text("Audits led to less favoritism [S1.2].", .VF_EV, .VF_G, research = .VF_R)
  expect_identical(nrow(ok$issues), 0L)
})

test_that("causal wording seen live is caught: leading to, contributes to, increases", {
  # as written live from descriptive findings
  for (s in c("The wage bill is high, leading to low salaries [S1.1].",
              "Routine work contributes to low output [S1.1].",
              "Managerial practices increase corruption [S1.1].")) {
    expect_identical(types(verify_text(s, .VF_EV, .VF_G, research = .VF_R)), "causal_claim_unsupported",
                     label = s)
  }
  # a trend in the past tense is not a claim of cause
  expect_identical(nrow(verify_text("Routine work increased over the decade [S1.1].", .VF_EV, .VF_G,
                                    research = .VF_R)$issues), 0L)
})

test_that("evidence from elsewhere stated as a fact about the country is flagged", {
  # as written live: "Research from Ghana ..." citing a cross-country study
  r <- .VF_R
  r$findings$scope <- c("cross_country", "country")
  ev <- .VF_EV
  ev$meta <- list(base_name = "Testland")
  bad <- verify_text("Research from Testland shows routine work is common [S1.1].", ev, .VF_G, research = r)
  expect_identical(types(bad), "scope_misstated")
  # the country's own finding may name it; elsewhere-evidence that does not name it is fine
  expect_identical(nrow(verify_text("In Testland, audits led to less favoritism [S1.2].", ev, .VF_G,
                                    research = r)$issues), 0L)
  expect_identical(nrow(verify_text("Studies elsewhere find routine work is common [S1.1].", ev, .VF_G,
                                    research = r)$issues), 0L)
})

test_that("a research source's authors may be named; others still may not", {
  ok <- verify_text("Rasul and Rogger (2017) surveyed civil servants [S1.1].", .VF_EV, .VF_G, research = .VF_R)
  expect_identical(nrow(ok$issues), 0L)
  bad <- verify_text("Smith (2017) surveyed civil servants [S1.1].", .VF_EV, .VF_G, research = .VF_R)
  expect_identical(types(bad), "external_reference")
})

test_that("a research citation renders as a badge with its finding and quote", {
  html <- cgjrapp:::.render_cited_html("Routine tasks [S1.1].", .VF_EV, .VF_G, .VF_R)
  expect_match(html, 'class="cgjr-cite cgjr-cite-s" title="Research finding: Civil servants spend 42 percent', fixed = TRUE)
  expect_match(html, "Quote: &quot;42 percent of their time&quot;", fixed = TRUE)
})
