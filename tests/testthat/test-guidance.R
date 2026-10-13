# Guidance corpus: splitter, stand-in loader, section map. Uses only the
# synthetic stand-in and inline text - never the Official Use Only note.

test_that("split_guidance finds sections from the contents page, with stable ids", {
  pages <- c(
    "Contents\nIntro ........ 1\n1. Alpha Area ........ 1\n1.1. Beta Topic ........ 2\nGlossary of Data Sources ........ 3\nReferences ........ 3",
    paste("1\nOfficial Use Only\nIntro\nSome opening words.\n1. Alpha Area\nAlpha text.",
          "1.1. Beta Topic\nBeta text one.\nTable 2. Beta and the links to the Jobs Strategy Pillars",
          "Building the Foundation: x.", sep = "\n"),
    "Glossary of Data Sources\n1.1. Beta Topic\nA data source.\nReferences\nSmith, J. (2001). A paper. Journal, 1(2).\nDoe, A., & Roe, B. (1999). Another. Press."
  )
  g <- split_guidance(pages)
  p <- g$passages
  expect_setequal(p$section, c("INT", "1", "1.1", "GLO1.1"))
  expect_identical(p$id[p$section == "1.1"], c("G1.1-1", "G1.1-2"))
  expect_identical(p$kind[p$section == "1.1"], c("narrative", "pillars"))
  expect_identical(p$page[p$section == "GLO1.1"], 3L)
  expect_false(any(grepl("Official Use Only", p$text)))
  expect_identical(g$refs$first_author, c("smith", "doe"))
  expect_identical(g$refs$year, c("2001", "1999"))
})

test_that("a heading wrapped over two lines still opens its section", {
  pages <- c("2. Efficiency of core government functions ........ 1",
             "2. Efficiency of core government\nfunctions\nBody text.")
  p <- split_guidance(pages)$passages
  expect_identical(unique(p$section), "2")
})

test_that("the stand-in loads by default and covers every report section", {
  withr::local_envvar(CGJR_GUIDANCE_SOURCE = NA)
  g <- load_guidance()
  expect_identical(g$source, "standin")
  expect_match(g$label, "STAND-IN")
  needed <- unique(unlist(cgjrapp:::.GUIDANCE_MAP, use.names = FALSE))
  expect_length(setdiff(needed, g$passages$section), 0L)
  expect_true(all(c("acemoglu", "north") %in% g$refs$first_author))
})

test_that("the section map covers every taxonomy leaf and cluster", {
  tx <- as.data.frame(cgjrdata::cgjr_taxonomy)
  keys <- c(unique(cgjrapp:::.leaf_key(tx)), unique(tx$cluster))
  expect_length(setdiff(keys, names(cgjrapp:::.GUIDANCE_MAP)), 0L)
})

test_that("'real' guidance without the passages file fails with a clear message", {
  withr::local_envvar(CGJR_GUIDANCE_PATH = file.path(tempdir(), "nope.rds"))
  expect_error(load_guidance("real"), "guidance passages file was not found")
})

test_that(".guidance_for picks a cluster's sections, subsections and glossary", {
  g <- load_guidance("standin")
  p <- cgjrapp:::.guidance_for(g, "core_governance_functions", children = TRUE)
  expect_true(all(c("2", "2.1", "2.1.1", "2.2", "2.3", "GLO2.2") %in% p$section))
  expect_false(any(c("1.1", "3.1", "GLO1.1") %in% p$section))
  leaf <- cgjrapp:::.guidance_for(g, "public_sector_hrm")
  expect_setequal(leaf$section, c("2.2", "GLO2.2"))
})
