# Citations in the World Bank's author-date style (Editorial Style Guide 2020,
# on Chicago; Chicago 18 where the guide is silent). Expected strings are the
# guide's own examples. Synthetic records; no network.

# A source record for the citation tests
src <- function(kind = "report", title = "A Title", year = 2019L, authors = institution_author("World Bank"),
                ...) {
  cgjrapp:::.as_sources(provider_rows("wb_documents", kind = kind, title = title, year = year,
                                      authors = list(authors), ...))
}

test_that("in-text names follow the guide: one, two, three authors, then et al. from four", {
  names <- function(family) cgjrapp:::.cite_names(people(family, rep("X", length(family))))
  expect_identical(names("Goldstein"), "Goldstein")
  expect_identical(names(c("Scherer", "Ross")), "Scherer and Ross")
  expect_identical(names(c("Garrett", "Thomas", "Smith")), "Garrett, Thomas, and Smith")
  expect_identical(names(c("Broadman", "Anderson", "Claessens", "Ryterman")), "Broadman et al.")
  expect_identical(cgjrapp:::.cite_names(institution_author("World Bank")), "World Bank")
})

test_that("an author's works in one year are told apart as 2019a, 2019b, by title", {
  s <- rbind(src(title = "Zeta Report"), src(title = "Alpha Report"), src(title = "Other Year", year = 2020L))
  expect_identical(cgjrapp:::.cite_years(s), c("2019b", "2019a", "2020"))
  expect_identical(cgjrapp:::.cite_years(src(year = NA_integer_)), "n.d.")
})

# A pack of three sources: the World Bank (2011), Debas (2019) and Garrett,
# Thomas and Smith (2019); four findings
PACK <- local({
  s <- rbind(src(title = "Joint Review", year = 2011L),
             src(title = "Surgery", authors = people("Debas", "Haile")),
             src(title = "Costs", authors = people(c("Garrett", "Thomas", "Smith"), c("A", "B", "C"))))
  s$id <- c("S1", "S2", "S3")
  list(sources = s,
       findings = data.frame(id = c("S1.1", "S1.2", "S2.1", "S3.1"), source_id = c("S1", "S1", "S2", "S3"),
                             page = c("37", "41", NA, "5")))
})
render <- function(text) cgjrapp:::.render_citations(text, PACK)

test_that("a research finding is cited by author and date, with its page when known", {
  expect_identical(render("Wage bills are high [S1.1]."), "Wage bills are high (World Bank 2011, 37).")
  expect_identical(render("Surgery is costly [S2.1]."), "Surgery is costly (Debas 2019).")
  # two findings of one source: both pages
  expect_identical(render("Both [S1.1, S1.2]."), "Both (World Bank 2011, 37, 41).")
})

test_that("several sources make one citation, in alphabetical order", {
  expect_identical(render("Costs are high [S3.1, S2.1]."),
                   "Costs are high (Debas 2019; Garrett, Thomas, and Smith 2019, 5).")
})

EN <- intToUtf8(0x2013)  # en dash
entry <- function(r) cgjrapp:::.reference_entry(r, cgjrapp:::.cite_years(r))

test_that("a working paper is listed as the guide's example (16.28)", {
  r <- src(kind = "working_paper", title = "Bank Regulation and Supervision: What Works Best?", year = 2018L,
           authors = people(c("Barth", "Caprio Jr.", "Levine"), c("James", "Gerard", "Ross")),
           collection = "Policy Research Working Paper", number = "2725", publisher = "World Bank",
           place = "Washington, DC")
  expect_identical(entry(r), paste0(
    "Barth, James, Gerard Caprio Jr., and Ross Levine. 2018. \"Bank Regulation and Supervision: ",
    "What Works Best?\" Policy Research Working Paper 2725, World Bank, Washington, DC."))
})

test_that("a journal article is listed as the guide's example (16.25), with its DOI as a link", {
  r <- src(kind = "journal_article", title = paste0("Capital Flight from the People's Republic of China: 1984", EN, "94"),
           year = 2016L, authors = people("Gunter", "Frank R."), journal = "China Economic Review",
           volume = "24", issue = "1", pages = "77-96")
  expect_identical(entry(r), paste0(
    "Gunter, Frank R. 2016. \"Capital Flight from the People's Republic of China: 1984", EN, "94.\" ",
    "*China Economic Review* 24 (1): 77", EN, "96."))
  r$doi <- "10.1111/j.1467-6435.2007.00376.x"
  expect_match(entry(r), "77–96\\. https://doi\\.org/10\\.1111/j\\.1467-6435\\.2007\\.00376\\.x\\.$")
})

test_that("an institution's report is listed as the guide's example (16.18)", {
  r <- src(title = "World Bank Annual Report 2018", year = 2018L, publisher = "World Bank",
           place = "Washington, DC", url = "https://openknowledge.worldbank.org/handle/10986/30326")
  expect_identical(entry(r), paste0("World Bank. 2018. *World Bank Annual Report 2018*. Washington, DC: ",
                                    "World Bank. https://openknowledge.worldbank.org/handle/10986/30326."))
})

test_that("page ranges take an en dash and the guide's abbreviation", {
  pr <- function(x) cgjrapp:::.page_range(x)
  expect_identical(pr(c("77-96", "225-235", "359-383", "1184-1208", "101-108", "1087-1089",
                        "100-104", "498-532", "5")),
                   paste0(c("77", "225", "359", "1184", "101", "1087", "100", "498", "5"),
                          c(EN, EN, EN, EN, EN, EN, EN, EN, ""),
                          c("96", "35", "83", "208", "8", "89", "104", "532", "")))
})

test_that("the reference list holds the sources cited, alphabetically, each once", {
  refs <- cgjrapp:::.reference_list("Costs [S3.1]. Wage bills [S1.1, S1.2]. Surgery [S2.1].", PACK)
  expect_identical(substr(refs, 1, 12), c("Debas, Haile", "Garrett, A, ", "World Bank. "))
  # a source not cited is not listed
  expect_length(cgjrapp:::.reference_list("Only [S2.1].", PACK), 1L)
  expect_identical(cgjrapp:::.reference_list("Nothing cited.", PACK), character(0))
})

test_that("an author's two works of one year are lettered alike in the text and the list", {
  pack <- PACK
  pack$sources$authors[[2]] <- institution_author("World Bank")
  pack$sources$year[[2]] <- 2011L  # now two World Bank 2011 works: "Joint Review", "Surgery"
  text <- "Both [S1.1] and [S2.1]."
  expect_identical(cgjrapp:::.render_citations(text, pack),
                   "Both (World Bank 2011a, 37) and (World Bank 2011b).")
  expect_match(cgjrapp:::.reference_list(text, pack), "^World Bank\\. 2011[ab]\\. \\*(Joint Review|Surgery)\\*")
  expect_identical(substr(cgjrapp:::.reference_list(text, pack), 13, 17), c("2011a", "2011b"))
  # with only one of them cited, it has no letter
  expect_identical(cgjrapp:::.render_citations("One [S2.1].", pack), "One (World Bank 2011).")
})

test_that("one section of a report is lettered as the whole report is", {
  pack <- PACK
  pack$sources$authors[[2]] <- institution_author("World Bank")
  pack$sources$year[[2]] <- 2011L  # two World Bank 2011 works, cited in different sections
  report <- "[[section:a]]\nFirst [S1.1].\n[[section:b]]\nSecond [S2.1]."
  expect_identical(cgjrapp:::.render_citations("Second [S2.1].", pack, report = report),
                   "Second (World Bank 2011b).")
})

test_that("a title's asterisks and underscores are kept as written, not read as italics", {
  entry <- cgjrapp:::.reference_entry(src(title = "Jobs * Growth_2"), "2019")
  expect_match(commonmark::markdown_html(entry), "<em>Jobs * Growth_2</em>", fixed = TRUE)
})

test_that("dashboard and guidance ids stay; an unknown research id is left for the checks", {
  expect_identical(render("A score of 0.655 [E3, S1.1]."), "A score of 0.655 [E3] (World Bank 2011, 37).")
  expect_identical(render("Unknown [S9.9]."), "Unknown [S9.9].")
})

test_that("for readers, dashboard and guidance ids go; a flagged research id stays", {
  strip <- cgjrapp:::.strip_ids
  expect_identical(strip("A score of 0.655 [E3]. Merit matters [G1.2-1, E3]."),
                   "A score of 0.655. Merit matters.")
  expect_identical(strip("Pay is high [E3] (World Bank 2011, 37)."), "Pay is high (World Bank 2011, 37).")
  expect_identical(strip("Unknown [S9.9]."), "Unknown [S9.9].")
  expect_identical(strip("No citation here."), "No citation here.")
})
