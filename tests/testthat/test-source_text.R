# Reading sources: document text into pages and passages with page numbers.
# Text comes from recorded public documents or PDFs made on the spot; no network.

test_that("a World Bank text file splits into its pages", {
  pages <- cgjrapp:::.text_pages(wb_text_fixture(), "text/plain")
  expect_length(pages, 6L)
  expect_match(pages[[1]], "^Policy Research Working Paper")
  # file page 4 is the paper's printed page 2: its last line is "2" (padded)
  expect_identical(trimws(utils::tail(strsplit(trimws(pages[[4]]), "\n")[[1]], 1)), "2")
})

test_that("printed page numbers are read only where they fit the document's numbering", {
  pages <- c("Cover", "Abstract", "Contents",
             "1. INTRODUCTION\nText.\n   2", "More text.\n   3", "Table 7\nText ending on a number\n   12",
             "More.\n   5", "No number on this page")
  # file page - printed page is 2 throughout; page 6's "12" does not fit,
  # and pages without a number get none - never a guess
  expect_identical(cgjrapp:::.printed_pages(pages), c(NA, NA, NA, "2", "3", NA, "5", NA))
  # no consistent numbering at all: no printed pages
  expect_identical(cgjrapp:::.printed_pages(c("a\n7", "b\n1", "c\n40")), rep(NA_character_, 3))
})

test_that("pages become passages of about 180 words, each with its pages", {
  pages <- cgjrapp:::.text_pages(wb_text_fixture(), "text/plain")
  p <- cgjrapp:::.passages(pages)
  expect_identical(names(p), c("passage", "file_page", "page", "text"))
  expect_identical(p$passage, seq_len(nrow(p)))
  intro <- p[startsWith(p$text, "1. INTRODUCTION"), ]
  expect_identical(c(intro$file_page, as.integer(intro$page)), c(4L, 2L))
  words <- lengths(strsplit(p$text, " ", fixed = TRUE))
  expect_true(all(words > 0L & words < 300L))
  # the footer that carries the page number is not part of the text
  expect_false(any(grepl(" 2$", p$text[p$file_page == 4L & p$text != p$text[[1]]])))
})

# -- .read_source() ---

POLICY <- load_source_policy()
TXT <- "https://documents.worldbank.org/curated/en/1/text/doc.txt"
PDF_URL <- "https://journal.example.org/article.pdf"
record <- function(text_url = NA, pdf_url = NA, abstract = NA) {
  cgjrapp:::.as_sources(provider_rows("wb_documents", title = "A", text_url = text_url,
                                      pdf_url = pdf_url, abstract = abstract))
}
read <- function(r) cgjrapp:::.read_source(r, POLICY, resolve = fake_dns())

test_that("a World Bank text file is read in full, with its provenance", {
  local_routes(list(`https://documents.worldbank.org/curated/en/1/text/doc.txt` =
                      resp(200, "text/plain", body = wb_text_fixture())))
  s <- read(record(text_url = TXT))
  expect_identical(c(s$access, s$method, s$url), c("full_text", "wb_text", TXT))
  expect_gt(nrow(s$passages), 5L)
  expect_identical(s$fingerprint, rlang::hash(s$passages$text))
  expect_match(s$fetched_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("without a text file, an open PDF is read", {
  local_routes(list(`https://journal.example.org/article.pdf` =
                      resp(200, "application/pdf", body = make_pdf(c("One", "Two")))))
  s <- read(record(pdf_url = PDF_URL))
  expect_identical(c(s$access, s$method), c("full_text", "pdf"))
  expect_identical(s$passages$file_page, 1:2)
})

test_that("when no text can be had, the abstract stands in; without one, metadata only", {
  local_routes(list(`https://documents.worldbank.org/curated/en/1/text/doc.txt` = resp(404)))
  a <- read(record(text_url = TXT, abstract = "What the abstract says."))
  expect_identical(c(a$access, a$method), c("abstract_only", "abstract"))
  expect_identical(a$passages$text, "What the abstract says.")
  expect_identical(a$passages$page, NA_character_)
  expect_match(a$reason, "broken")  # why the text was not read
  m <- read(record())
  expect_identical(m$access, "metadata_only")
  expect_identical(nrow(m$passages), 0L)
})

test_that("an unsafe document link is never requested", {
  seen <- local_routes(list())
  s <- read(record(pdf_url = "https://10.0.0.1/secret.pdf", abstract = "Abstract."))
  expect_identical(s$access, "abstract_only")
  expect_length(seen(), 0L)
})

# -- choosing passages, and a section's sources ---

test_that("a source's passages are ranked by the section's words; unrelated ones are left out", {
  p <- data.frame(passage = 1:4, file_page = 1:4, page = NA_character_,
                  text = c("Civil service pay reform in Ghana", "Weather and crops",
                           "Civil service recruitment", "Pay"))
  r <- cgjrapp:::.relevant_passages(p, "civil service pay", k = 2L)
  expect_identical(r$passage, c(1L, 3L))
  expect_identical(nrow(cgjrapp:::.relevant_passages(p, "civil service pay", k = 10L)), 3L)
})

test_that("a section's selected sources are read; their best passages kept", {
  local_routes(list(`https://documents.worldbank.org/curated/en/1/text/doc.txt` =
                      resp(200, "text/plain", body = wb_text_fixture())))
  s <- rbind(record(text_url = TXT), record(abstract = "Procurement in Bangladesh."),
             record(text_url = "https://documents.worldbank.org/never/read.txt"))
  s$key <- c("wb:1", "wb:2", "wb:3")
  s$selected <- c(TRUE, TRUE, FALSE)
  r <- cgjrapp:::.read_sources(s, "public_procurement", "Bangladesh", POLICY, resolve = fake_dns())
  expect_identical(r$sources$access, c("full_text", "abstract_only", NA))
  expect_setequal(unique(r$passages$key), c("wb:1", "wb:2"))
  expect_lte(sum(r$passages$key == "wb:1"), POLICY$limits$passages_per_source)
  expect_true(all(grepl("procurement", r$passages$text, ignore.case = TRUE)))
})

test_that("a document read before is not downloaded again", {
  cache <- cachem::cache_mem()
  seen <- local_routes(list(`https://documents.worldbank.org/curated/en/1/text/doc.txt` =
                              resp(200, "text/plain", body = wb_text_fixture())))
  s <- record(text_url = TXT)
  s$key <- "wb:1"; s$selected <- TRUE
  cgjrapp:::.read_sources(s, "public_procurement", "Bangladesh", POLICY, cache, fake_dns())
  cgjrapp:::.read_sources(s, "public_procurement", "Bangladesh", POLICY, cache, fake_dns())
  expect_length(seen(), 1L)
})

test_that("typographic ligatures are spelled out, so words and quotes match", {
  # as read live from a World Bank text file: "deﬁning" with the "ﬁ" ligature
  expect_identical(cgjrapp:::.text_pages(charToRaw("deﬁning eﬀects ﬂows"), "text/plain"),
                   "defining effects flows")
})

test_that("NUL bytes in a text file (an old scanned World Bank report) are dropped", {
  bytes <- c(charToRaw("Public Expenditure"), as.raw(0), charToRaw(" Review"))
  expect_identical(cgjrapp:::.text_pages(bytes, "text/plain"), "Public Expenditure Review")
})

test_that("a source that fails to read is kept as metadata only; the section's other sources are read", {
  local_routes(list(`https://documents.worldbank.org/curated/en/1/text/doc.txt` =
                      resp(200, "text/plain", body = wb_text_fixture())))
  s <- rbind(record(text_url = TXT), record(text_url = "https://broken.example.org/doc.txt"))
  s$key <- c("wb:1", "wb:2"); s$selected <- TRUE
  dns <- fake_dns()
  resolve <- function(host) if (host == "broken.example.org") stop("resolver crashed") else dns(host)
  r <- cgjrapp:::.read_sources(s, "public_sector_hrm", "Ghana", POLICY, resolve = resolve)
  expect_identical(r$sources$access, c("full_text", "metadata_only"))
  expect_match(r$sources$access_reason[[2]], "resolver crashed", fixed = TRUE)
  expect_true(all(r$passages$key == "wb:1"))
})

test_that("text that is not UTF-8 is read as Windows-1252", {
  bytes <- c(charToRaw("J"), as.raw(0xfc), charToRaw("rgen"))  # "Jürgen" in Windows-1252
  expect_identical(cgjrapp:::.text_pages(bytes, "text/plain"), "Jürgen")
})

test_that("a PDF is read page by page; other content gives no pages", {
  pages <- cgjrapp:::.text_pages(make_pdf(c("First page text", "Second page text")), "application/pdf")
  expect_length(pages, 2L)
  expect_match(pages[[2]], "Second page text")
  expect_identical(cgjrapp:::.text_pages(charToRaw("<html></html>"), "text/html"), character(0))
})
