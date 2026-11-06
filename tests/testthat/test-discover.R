# Research discovery: search queries from public inputs, and the provider
# adapters (World Bank Documents & Reports, OpenAlex). HTTP is mocked with
# recorded public responses (fixtures/research/); no network.

POLICY <- load_source_policy()
EV <- build_evidence("GHA", cgjrapp:::.default_comparators("GHA"), clusters = "core_governance_functions")
# A country query, as .research_queries() makes them
COUNTRY_Q <- data.frame(leaf = "public_procurement", scope = "country", country = "Bangladesh",
                        text = "Public Procurement public procurement")

test_that("a section gets a country query and an international one", {
  q <- cgjrapp:::.research_queries("public_sector_hrm", EV, POLICY)
  expect_identical(q$scope, c("country", "international"))
  expect_identical(q$country, c("Ghana", NA))
  # the country query is the section's search terms - short, so that with the
  # provider's country filter it still finds documents (live, 2026-10-30, a
  # query with indicator names added found none at any provider)
  expect_identical(q$text, c("civil service public employment",
                             "Public Sector HRM civil service public employment"))
})

test_that("an unmeasured section, or one without search terms, still gets queries", {
  expect_true("public_procurement" %in% EV$gaps$leaf)
  q <- cgjrapp:::.research_queries("public_procurement", EV, POLICY)
  expect_identical(q$text, c("public procurement", "Public Procurement public procurement"))
  # no terms: the section's label stands in for them
  policy <- POLICY
  policy$search$terms$public_sector_hrm <- NULL
  expect_identical(cgjrapp:::.research_queries("public_sector_hrm", EV, policy)$text,
                   c("Public Sector HRM", "Public Sector HRM"))
})

# -- .discover_sources() ---

# Crossref records for the two fixture publications: the working paper's is
# recorded; the article's is written from its OpenAlex record
CROSSREF <- list(
  `10.1596/1813-9450-10390` = research_fixture("crossref_prwp_10390.json")$message,
  `10.1016/j.worlddev.2017.09.018` = list(
    DOI = "10.1016/j.worlddev.2017.09.018", type = "journal-article",
    title = list(research_fixture("openalex_world_dev.json")$results[[1]]$display_name),
    author = list(list(given = "Sara", family = "Dewachter", sequence = "first")),
    issued = list(`date-parts` = list(list(2017L))), publisher = "Elsevier BV",
    `container-title` = list("World Development")))
discover <- function(evidence = EV, policy = POLICY, ...) {
  cgjrapp:::.discover_sources("public_sector_hrm", evidence, policy, pause = function(s) NULL, ...)
}

test_that("research for a section: found, merged, checked against the policy and Crossref, ranked", {
  local_research(crossref = CROSSREF)
  d <- discover()
  s <- d$sources
  expect_identical(s$key, c("doi:10.1596/1813-9450-10390", "doi:10.1016/j.worlddev.2017.09.018"))
  expect_identical(s$tier, c(1L, 3L))                   # the World Bank first
  expect_identical(s$reason, c("not_peer_reviewed", "ok"))  # a working paper is labelled so
  expect_identical(s$identity, c("verified", "verified"))
  expect_identical(s$scope, c("country", "country"))   # both came up for Ghana
  expect_identical(s$selected, c(TRUE, TRUE))
  expect_identical(s$leaf, rep("public_sector_hrm", 2))
  status <- vapply(split(d$providers$status, d$providers$provider), unique, "")
  expect_identical(status[c("wb_documents", "openalex", "mai_search")],
                   c(wb_documents = "ok", openalex = "ok", mai_search = "not_configured"))
})

test_that("with mAI, search leads add the World Bank records they point to", {
  local_mai()
  local_research(crossref = CROSSREF)
  d <- discover()
  ghana <- research_fixture("wb_guid_prwp_ghana_2018.json")$documents[[1]]$id
  expect_true(paste0("wb:", ghana) %in% d$sources$key)
  added <- d$sources[d$sources$key == paste0("wb:", ghana), ]
  expect_identical(c(added$tier, added$identity, added$citable), c("1", "verified", "TRUE"))
  expect_setequal(added$providers[[1]], c("wb_documents", "mai_search"))
  # every lead is accounted for, resolved or set aside with a reason
  expect_identical(nrow(d$leads), 10L)  # 5 per query, two queries
  expect_setequal(d$leads$status, c("resolved", "document_group_excluded", "not_in_wb_api", "unresolved"))
  expect_identical(unique(d$providers$status[d$providers$provider == "mai_search"]), "ok")
})

test_that("a lead pointing to a document the World Bank search found adds no duplicate", {
  local_mai()
  # the lead for guid 335361537384686708 now resolves to the working paper
  # the World Bank search also returns
  local_research(crossref = CROSSREF,
                 wb_guids = list(`335361537384686708` = "wb_prwp_10390.json"))
  s <- discover()$sources
  prwp <- s[s$key == "doi:10.1596/1813-9450-10390", ]
  expect_identical(nrow(prwp), 1L)
  expect_setequal(prwp$providers[[1]], c("wb_documents", "mai_search"))
})

test_that("screening reorders the selection; a source rated low is set aside", {
  local_research(crossref = CROSSREF)
  # before screening the working paper ranks first (C1), the article second (C2)
  ask <- function(system, prompt) {
    '[{"id": "C1", "relevance": "low", "reason": "Procurement, not HRM"},
      {"id": "C2", "relevance": "high", "reason": "Public service accountability"}]'
  }
  d <- discover(ask = ask)
  s <- d$sources
  expect_identical(d$screening, "ok")
  expect_identical(s$key, c("doi:10.1016/j.worlddev.2017.09.018", "doi:10.1596/1813-9450-10390"))
  expect_identical(s$relevance, c("high", "low"))
  expect_identical(s$selected, c(TRUE, FALSE))
  expect_true("screened_low" %in% s$issues[[2]])
  expect_identical(s$relevance_reason[[2]], "Procurement, not HRM")
})

test_that("among equally relevant sources, recent ones rank first", {
  fx <- research_fixture("wb_prwp_10390.json")
  doc <- function(id, date) {
    d <- fx$documents[[1]]
    d$id <- id; d$docdt <- date; d$dois <- NULL
    d$display_title <- paste("Report", id)
    d
  }
  fx$documents <- list(D1 = doc("1", "1991-01-01T00:00:00Z"), D2 = doc("2", "2020-01-01T00:00:00Z"))
  local_research(wb = fx, openalex = list(meta = list(count = 0L), results = list()))
  ask <- function(system, prompt) '[{"id": "C1", "relevance": "medium", "reason": "a"},
                                    {"id": "C2", "relevance": "medium", "reason": "b"}]'
  s <- discover(ask = ask)$sources
  expect_identical(s$year, c(2020L, 1991L))
})

test_that("without an LLM to ask, discovery is unscreened and says so", {
  local_research(crossref = CROSSREF)
  d <- discover()
  expect_identical(d$screening, "not_configured")
  expect_identical(d$sources$relevance, c("unscreened", "unscreened"))
})

test_that("a provider that is down leaves the others' results; all down leaves none", {
  local_research(wb = 503L, crossref = CROSSREF)
  d <- discover()
  expect_identical(d$sources$key, "doi:10.1016/j.worlddev.2017.09.018")
  expect_identical(unique(d$providers$status[d$providers$provider == "wb_documents"]), "unavailable")
  local_research(wb = 503L, openalex = 429L)
  none <- discover()
  expect_identical(nrow(none$sources), 0L)
  expect_true(all(c("key", "tier", "identity", "citable", "selected") %in% names(none$sources)))
  expect_setequal(none$providers$status, c("unavailable", "rate_limited", "not_configured"))
})

test_that("sources that cannot be cited are kept, with why, after those that can", {
  # Crossref knows the working paper but not the article's DOI
  local_research(crossref = CROSSREF["10.1596/1813-9450-10390"])
  s <- discover()$sources
  expect_identical(s$citable, c(TRUE, FALSE))
  expect_identical(s$identity[[2]], "conflict")
  expect_true("doi_not_found" %in% s$issues[[2]])
  expect_identical(s$selected, c(TRUE, FALSE))
})

test_that("one World Bank project's many outputs take one place: the latest", {
  # as seen live: six deliverables of one advisory project (P165191) filled
  # the institutional places
  fx <- research_fixture("wb_prwp_10390.json")
  out <- function(id, date, title) {
    d <- fx$documents[[1]]
    d$id <- id; d$docdt <- date; d$docna <- list(list(docna = title)); d$dois <- NULL
    d$projectid <- "P165191"; d$docty <- "Report"
    d
  }
  fx$documents <- list(D1 = out("1", "2019-05-01T00:00:00Z", "Output 1 - Deliverable 1.1"),
                       D2 = out("2", "2020-11-01T00:00:00Z", "Output 5 - Deliverable 5.6"))
  local_research(wb = fx, crossref = CROSSREF)
  s <- discover()$sources
  wb <- s[s$wb_id %in% c("1", "2"), ]
  expect_identical(wb$citable, c(TRUE, TRUE))
  expect_identical(wb$selected[wb$wb_id == "2"], TRUE)   # the latest output
  expect_identical(wb$selected[wb$wb_id == "1"], FALSE)
  expect_true("same_project" %in% wb$issues[[which(wb$wb_id == "1")]])
  expect_identical(wb$project, c("P165191", "P165191"))
})

test_that("peer-reviewed articles have their own places, so institutions cannot crowd them out", {
  # with mAI, the World Bank finds two working papers that rank above the
  # article; one place each: one working paper and the article are selected
  local_mai()
  local_research(crossref = CROSSREF)
  policy <- POLICY
  policy$limits$max_institutional_per_leaf <- 1L
  policy$limits$max_peer_reviewed_per_leaf <- 1L
  s <- discover(policy = policy)$sources
  s <- s[s$citable, ]
  expect_identical(s$tier, c(1L, 1L, 3L))
  expect_identical(s$selected, c(TRUE, FALSE, TRUE))
})

test_that("a region or income group as base gets only the international search", {
  region <- EV
  region$meta$base_unit <- "AFE"
  region$meta$base_name <- "Africa Eastern and Southern"
  seen <- local_research(crossref = CROSSREF)
  d <- discover(region)
  expect_identical(unique(d$providers$scope), "international")
  wb <- Filter(function(r) grepl("search.worldbank.org", r$url, fixed = TRUE), seen())
  expect_false(any(vapply(wb, function(r) !is.null(query_of(r)$count_exact), logical(1))))
  expect_identical(unique(d$sources$scope), "international")
})

test_that("no text from the guidance note ever reaches a search service", {
  # the stand-in guidance is loaded beside the run, as it is in the app
  g <- load_guidance("standin")
  local_mai()  # mAI search too: its queries go in a POST body
  seen <- local_research(crossref = CROSSREF)
  # the LLM's screening prompts are checked too
  prompts <- character(0)
  ask <- function(system, prompt) { prompts <<- c(prompts, system, prompt); "[]" }
  for (leaf in c("public_sector_hrm", "public_procurement", "budget_cycle_and_fiscal_planning")) {
    cgjrapp:::.discover_sources(leaf, EV, POLICY, pause = function(s) NULL, ask = ask)
  }
  # each request as text: its URL and its body (raw bytes, or a JSON body as
  # httr2 holds it, a list - serialised as it is sent)
  body_text <- function(d) {
    if (is.null(d)) "" else if (is.raw(d)) rawToChar(d) else jsonlite::toJSON(d, auto_unbox = TRUE)
  }
  sent <- tolower(c(vapply(seen(), function(r) paste(utils::URLdecode(r$url), body_text(r$body$data)), ""),
                    prompts))
  expect_true(any(grepl("^section: ", sent)))  # screening prompts were read
  expect_true(any(grepl("site:documents.worldbank.org", sent, fixed = TRUE)))  # mAI bodies were read
  expect_identical(guidance_leaks(sent, g), character(0))
  expect_gt(length(seen()), 0L)
})

test_that("a repeated run is answered from the cache; new settings search again", {
  cache <- cachem::cache_mem()
  seen <- local_research(crossref = CROSSREF)
  first <- discover(cache = cache)
  n <- length(seen())
  again <- discover(cache = cache)
  expect_length(seen(), n)  # no request at all: searches and DOI records cached
  expect_identical(again$sources$key, first$sources$key)
  policy <- POLICY
  policy$search$terms$public_sector_hrm <- c("civil service", "meritocracy")
  discover(policy = policy, cache = cache)
  expect_gt(length(seen()), n)  # other search terms: other queries
})

test_that("the research cache lives under CGJR_RESEARCH_CACHE and keeps the policy's days", {
  dir <- withr::local_tempdir()
  withr::local_envvar(CGJR_RESEARCH_CACHE = dir)
  info <- cgjrapp:::.research_cache(POLICY)$info()
  expect_identical(normalizePath(info$dir), normalizePath(file.path(dir, "research")))
  expect_identical(info$max_age, POLICY$limits$cache_days * 86400)
})

test_that("failed searches are not cached: the next run tries again", {
  cache <- cachem::cache_mem()
  seen <- local_research(wb = 503L, crossref = CROSSREF)
  discover(cache = cache)
  wb_requests <- function() sum(grepl("search.worldbank.org", vapply(seen(), `[[`, "", "url"), fixed = TRUE))
  n <- wb_requests()
  discover(cache = cache)
  expect_identical(wb_requests(), 2L * n)
})

# -- .search_mai() ---

test_that("mAI's Google search is asked with the token, limited to the repositories", {
  local_mai()
  seen <- local_provider(research_fixture("mai_google_search_ghana.json"))
  cgjrapp:::.search_mai(COUNTRY_Q, POLICY, n = 5L)
  req <- seen()[[1]]
  expect_identical(req$method, "POST")
  expect_identical(req$url, "https://azapimdev.worldbank.org/conversationalai/platform/google_search/")
  h <- httr2::req_get_headers(req, "reveal")
  expect_match(h$Authorization, "^Bearer ")
  expect_identical(h$`x-source-type`, "interactive")
  # only the document repositories, whose links resolve to records (live,
  # 2026-10-30, all institution domains gave pages that resolved to nothing)
  expect_identical(POLICY$search$mai_sites, c("documents.worldbank.org", "openknowledge.worldbank.org"))
  expect_identical(req$body$data$query,
                   "Bangladesh Public Procurement public procurement (site:documents.worldbank.org OR site:openknowledge.worldbank.org)")
})

test_that("mAI search results are leads: a link, and a cleaned title", {
  local_mai()
  local_provider(research_fixture("mai_google_search_ghana.json"))
  res <- cgjrapp:::.search_mai(COUNTRY_Q, POLICY, n = 5L)
  expect_identical(res$status, "ok")
  expect_null(res$rows)  # nothing is a source until it is resolved
  l <- res$leads
  expect_identical(nrow(l), 5L)
  expect_identical(l$url[[5]], "https://documents.worldbank.org/en/publication/documents-reports/documentdetail/335361537384686708")
  expect_identical(l$title[[2]], "Ghana's New Approach to Public Sector Reform")  # was &#x27;
  expect_identical(l$title_truncated, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("an mAI search the platform refuses is reported as unauthorized", {
  local_mai()
  local_provider(list(statusCode = 401L, message = "Unauthorized"), status = 401L)
  expect_identical(cgjrapp:::.search_mai(COUNTRY_Q, POLICY, n = 5L)$status, "unauthorized")
})

# -- .resolve_leads() ---

LEADS <- function() {
  local_mai()
  local_provider(research_fixture("mai_google_search_ghana.json"))
  cgjrapp:::.search_mai(COUNTRY_Q, POLICY, n = 5L)$leads
}

test_that("a search lead becomes a source only through the World Bank's own record", {
  leads <- LEADS()
  seen <- local_wb_guids()
  r <- cgjrapp:::.resolve_leads(leads, POLICY)
  expect_identical(r$leads$status, c("document_group_excluded",  # a project appraisal document
                                     "unresolved",               # an Open Knowledge Repository page
                                     "not_in_wb_api",            # an id the API does not know
                                     "unresolved",               # a news feature
                                     "resolved"))
  # looked up by id, one request per World Bank link
  expect_identical(vapply(seen(), function(q) query_of(q)$guid, ""),
                   c("758341540692513115", "848451468771579060", "335361537384686708"))
  s <- cgjrapp:::.as_sources(r$rows)
  expect_identical(nrow(s), 1L)
  expect_match(s$title, "^Management and Bureaucratic Effectiveness")  # the Bank's title, in full
  expect_identical(s$authors[[1]]$family, c("Rasul", "Rogger", "Williams"))  # its authors too
  expect_identical(s$wb_id, research_fixture("wb_guid_prwp_ghana_2018.json")$documents[[1]]$id)
  expect_setequal(s$providers[[1]], c("wb_documents", "mai_search"))
})

test_that("without mAI configured, its search is skipped, not failed", {
  seen <- local_provider(research_fixture("mai_google_search_ghana.json"))
  res <- cgjrapp:::.search_mai(COUNTRY_Q, POLICY, n = 5L)
  expect_identical(res$status, "not_configured")
  expect_length(seen(), 0L)
})

# -- .search_wb() ---

test_that("the World Bank search asks for research documents about the country", {
  seen <- local_provider(research_fixture("wb_prwp_10390.json"))
  cgjrapp:::.search_wb(COUNTRY_Q, POLICY, n = 10L)
  req <- seen()[[1]]
  expect_match(req$url, "^https://search\\.worldbank\\.org/api/v3/wds\\?")
  q <- query_of(req)
  expect_identical(q[c("format", "qterm", "rows", "count_exact")],
                   list(format = "json", qterm = "Public Procurement public procurement",
                        rows = "10", count_exact = "Bangladesh"))
  expect_identical(q$majdocty_exact, "Publications & Research^Economic & Sector Work^Country Focus")
  expect_identical(q$lang_exact, "English")
  # authors must be asked for as "authr" (the API returns them as "authors";
  # asked for as "authors", it leaves them out - found live, 2026-11-04)
  expect_true("authr" %in% strsplit(q$fl, ",", fixed = TRUE)[[1]])
  # an international query is not limited to one country
  intl <- COUNTRY_Q
  intl$scope <- "international"; intl$country <- NA
  cgjrapp:::.search_wb(intl, POLICY, n = 10L)
  expect_null(query_of(seen()[[2]])$count_exact)
})

test_that("a World Bank working paper record becomes a full source row", {
  local_provider(research_fixture("wb_prwp_10390.json"))
  res <- cgjrapp:::.search_wb(COUNTRY_Q, POLICY, n = 10L)
  expect_identical(res$status, "ok")
  r <- cgjrapp:::.as_sources(res$rows)
  expect_identical(nrow(r), 1L)
  expect_identical(r$title, "Introducing E-Procurement in Bangladesh: The Promise of Efficiency and Openness")
  expect_identical(r$authors[[1]]$family, c("Blum", "Datta", "Fazekas", "Samaddar", "Siddique"))
  expect_identical(r$authors[[1]]$given[[1]], "Jurgen Rene")
  expect_identical(r[c("kind", "collection", "number", "institution", "publisher", "place")],
                   data.frame(kind = "working_paper", collection = "Policy Research Working Paper",
                              number = "10390", institution = "WBG", publisher = "World Bank",
                              place = "Washington, DC"))
  expect_identical(c(r$wb_id, r$doi, r$date, r$language), c("34034392", "10.1596/1813-9450-10390",
                                                            "2023-04-03", "English"))
  expect_identical(r$year, 2023L)
  expect_identical(r$url, "https://documents.worldbank.org/curated/en/099246304032338660")
  expect_match(r$text_url, "^https://documents\\.worldbank\\.org/.*\\.txt$")
  expect_match(r$abstract, "^Governments around the world spend")
  expect_identical(r$providers[[1]], "wb_documents")
})

test_that("World Bank reports are reports, and an unsigned one has the Bank as author", {
  fx <- research_fixture("wb_prwp_10390.json")
  doc <- fx$documents[[1]]
  cem <- doc
  cem$docty <- "Country Economic Memorandum"; cem$authors <- NULL; cem$repnb <- "123456-GH"
  numbered <- doc
  numbered$id <- "1"; numbered$docty <- "Working Paper (Numbered Series)"; numbered$repnb <- "WDP 412"
  fx$documents <- list(D1 = cem, D2 = numbered)
  local_provider(fx)
  r <- cgjrapp:::.as_sources(cgjrapp:::.search_wb(COUNTRY_Q, POLICY, n = 10L)$rows)
  expect_identical(r$kind, c("report", "working_paper"))
  expect_identical(r$collection, c(NA, "Working Paper"))
  expect_identical(r$number, c(NA, "412"))
  expect_identical(r$authors[[1]]$literal, "World Bank")
})

# -- .search_openalex() ---

test_that("the OpenAlex search covers only the registry's journals", {
  seen <- local_provider(research_fixture("openalex_world_dev.json"))
  cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)
  req <- seen()[[1]]
  expect_match(req$url, "^https://api\\.openalex\\.org/works\\?")
  q <- query_of(req)
  # OpenAlex has no country filter for a work's subject: the name goes in the text
  expect_identical(q$search, "Bangladesh Public Procurement public procurement")
  expect_identical(q$filter, paste0("primary_location.source.id:",
                                    paste(POLICY$journals$openalex_id, collapse = "|"), ",type:article"))
  expect_identical(q$`per-page`, "10")
  expect_null(q$api_key)  # no key set: none sent (not the string "NA")
  withr::local_envvar(CGJR_OPENALEX_KEY = "k123")
  cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)
  expect_identical(query_of(seen()[[2]])$api_key, "k123")
  # a journal the policy excludes is not searched at all
  policy <- POLICY
  policy$journals$status[policy$journals$id == "jde"] <- "excluded"
  cgjrapp:::.search_openalex(COUNTRY_Q, policy, n = 10L)
  expect_no_match(query_of(seen()[[3]])$filter, POLICY$journals$openalex_id[POLICY$journals$id == "jde"])
})

test_that("OpenAlex failures are reported like any provider's", {
  local_provider(list(meta = list(count = 0L), results = list()))
  expect_identical(NROW(cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)$rows), 0L)
  local_provider("not json")
  expect_identical(cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)$status, "malformed")
  local_provider(list(error = "Rate limit exceeded"), status = 429L)
  expect_identical(cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)$status, "rate_limited")
})

test_that("an OpenAlex work becomes a journal-article source row", {
  local_provider(research_fixture("openalex_world_dev.json"))
  res <- cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)
  r <- cgjrapp:::.as_sources(res$rows)
  expect_identical(nrow(r), 1L)
  expect_identical(r$key, "doi:10.1016/j.worlddev.2017.09.018")
  expect_identical(r$openalex_id, sub("^https://openalex.org/", "",
                                      research_fixture("openalex_world_dev.json")$results[[1]]$id))
  expect_identical(r[c("kind", "journal", "openalex_source_id", "volume", "pages")],
                   data.frame(kind = "journal_article", journal = "World Development",
                              openalex_source_id = "S85457386", volume = "102", pages = "158-169"))
  expect_identical(r$issue, NA_character_)  # OpenAlex has none for this article
  expect_identical(r$issn[[1]], c("0305-750X", "1873-5991"))
  expect_identical(c(r$year, r$is_retracted), c(2017L, FALSE))
  # display names, split on the last word - flagged for checking
  expect_identical(r$authors[[1]]$family, c("Dewachter", "Holvoet", "Kuppens", "Molenaers"))
  expect_true("author_name_unsplit" %in% r$issues[[1]])
  expect_identical(r$abstract, NA_character_)  # OpenAlex holds no abstract for it
  # and the policy places it: a registry journal, tier 3
  expect_identical(classify_sources(r, POLICY)$tier, 3L)
})

test_that("an OpenAlex abstract is rebuilt in word order from its word index", {
  fx <- research_fixture("openalex_world_dev.json")
  fx$results[[1]]$abstract_inverted_index <- list(Water = list(0L), services = list(1L, 4L),
                                                  improve = list(3L), and = list(2L))
  local_provider(fx)
  r <- cgjrapp:::.search_openalex(COUNTRY_Q, POLICY, n = 10L)$rows
  expect_identical(r$abstract, "Water services and improve services")
})

test_that("a World Bank document's title is its own, not its volume's name", {
  # as seen live: for a multi-volume report the record's `docna` is the
  # volume ("Main report"); `display_title` is the document's, with the
  # Bank's " : " before a subtitle
  fx <- research_fixture("wb_prwp_10390.json")
  fx$documents[[1]]$docna <- list(list(docna = "Main report"))
  fx$documents[[1]]$display_title <- "Ghana - Joint review of public expenditure : Main report"
  local_provider(fx)
  r <- cgjrapp:::.as_sources(cgjrapp:::.search_wb(COUNTRY_Q, POLICY, n = 10L)$rows)
  expect_identical(r$title, "Ghana - Joint review of public expenditure: Main report")
})

test_that("a provider that fails says why and gives nothing, without stopping the run", {
  search <- function() cgjrapp:::.search_wb(COUNTRY_Q, POLICY, n = 10L)
  outcome <- function(res) list(status = res$status, rows = NROW(res$rows))
  local_provider(list(rows = 0, total = 0, documents = setNames(list(), character(0))))
  expect_identical(outcome(search()), list(status = "ok", rows = 0L))
  local_provider("<html>Service temporarily unavailable</html>")
  expect_identical(outcome(search()), list(status = "malformed", rows = 0L))
  local_provider(list(error = "slow down"), status = 429L)
  expect_identical(outcome(search()), list(status = "rate_limited", rows = 0L))
  local_provider(list(error = "down"), status = 503L)
  expect_identical(outcome(search()), list(status = "unavailable", rows = 0L))
  httr2::local_mocked_responses(function(req) stop("Timeout was reached"))
  expect_identical(outcome(search()), list(status = "unavailable", rows = 0L))
})
