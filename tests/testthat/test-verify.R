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

test_that("a percentage that matches the cited fact's text passes", {
  v <- verify_text("Alpha is in the top 50% band [E1].", .VF_EV, .VF_G)
  expect_identical(nrow(v$issues), 0L)
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
