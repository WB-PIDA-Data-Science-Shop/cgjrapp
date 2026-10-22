# Guidance retrieval for the chat: keyword (BM25) and meaning (embeddings)
# rankings, fused. Hand-made passages (mini_guidance()); no network.

test_that("a question in a section's own words ranks that section's passage first", {
  r <- retrieve_guidance(mini_guidance(), "Are tenders competitive and contracts published?")
  expect_identical(r$id[1], "G2.1.3-1")
})

test_that("stop words, capitals and punctuation don't make a match", {
  expect_identical(nrow(retrieve_guidance(mini_guidance(), "What are the, and of the...?")), 0L)
  expect_identical(retrieve_guidance(mini_guidance(), "TENDERS?!")$id, "G2.1.3-1")
})

test_that("word endings don't stop a match: plurals, -ing, -ed", {
  expect_identical(retrieve_guidance(mini_guidance(), "declaration")$id, "G1.1-1")
  expect_identical(retrieve_guidance(mini_guidance(), "publishing")$id, "G2.1.3-1")
  expect_identical(retrieve_guidance(mini_guidance(), "a recruitments drive")$id, "G2.2-1")
  # a word ending in -ss keeps its s, so "processes" and "process" still meet
  g <- mini_guidance()
  g$passages$text[[4]] <- "Business process reengineering of registries."
  expect_identical(retrieve_guidance(g, "processes")$id, "G2.3-1")
})

test_that("a passage is found by its section title even when its text lacks the word", {
  expect_identical(retrieve_guidance(mini_guidance(), "procurement")$id, "G2.1.3-1")
})

test_that("a question nothing in the guidance matches gets no passages (keywords only)", {
  expect_identical(nrow(retrieve_guidance(mini_guidance(), "weather forecast for Paris")), 0L)
  expect_identical(nrow(retrieve_guidance(mini_guidance(), "")), 0L)
})

test_that("reciprocal rank fusion (k = 60) orders by summed 1 / (60 + rank)", {
  # A: 1/61 + 1/63 = 0.032266; B: 1/62 + 1/62 = 0.032258; D: 1/61; C: 1/63
  expect_identical(.rrf(list(c("A", "B", "C"), c("D", "B", "A"))), c("A", "B", "D", "C"))
})

test_that("with embeddings, a passage close in meaning is found though it shares no words", {
  g <- mini_guidance()
  # 2-d stand-in vectors: the query points at G1.1-1, which has no word in common with it
  g$embeddings <- list(vectors = rbind(`G1.1-1` = c(1, 0), `G2.2-1` = c(0, 1),
                                       `G2.1.3-1` = c(0.7, 0.7), `G2.3-1` = c(-1, 0)))
  r <- retrieve_guidance(g, "tenders", k = 2L, query_vec = c(1, 0.05))
  expect_identical(r$id, c("G2.1.3-1", "G1.1-1"))
  expect_identical(retrieve_guidance(g, "tenders", k = 2L)$id, "G2.1.3-1")
})

test_that("load_guidance() attaches saved embeddings only while they match the passages", {
  dir <- withr::local_tempdir()
  g <- mini_guidance()[c("passages", "refs")]
  path <- file.path(dir, "guidance_passages.rds")
  saveRDS(g, path)
  vec <- matrix(seq_len(8) / 8, 4, dimnames = list(g$passages$id, NULL))
  saveRDS(list(model = "text-embedding-3-small", fingerprint = guidance_fingerprint(g$passages),
               vectors = vec), file.path(dir, "guidance_passages_embeddings.rds"))
  withr::local_envvar(CGJR_GUIDANCE_PATH = path)
  expect_identical(load_guidance("real")$embeddings$vectors, vec)

  g$passages$text[[1]] <- "A rebuilt passage."
  saveRDS(g, path)
  expect_message(g2 <- load_guidance("real"), "embeddings.*out of date")
  expect_null(g2$embeddings)
})

test_that("no more than k passages come back, best first", {
  q <- "public procurement integrity data recruitment"
  r <- retrieve_guidance(mini_guidance(), q, k = 2L)
  expect_identical(nrow(r), 2L)
  expect_identical(r$id, retrieve_guidance(mini_guidance(), q)$id[1:2])
})
