# ---
# Research discovery: which publications to look at for a report section.
#
# Search queries are built from public inputs only - the section's taxonomy
# label, its search terms in the source policy and the country's name. The
# guidance note never reaches a query: it is Official Use Only, and a search
# service is a third party. .research_queries() takes no guidance argument for that
# reason.
# ---

# A report section (leaf key) -> its search queries: one about the country
# and one international, for mechanisms and policy evidence from elsewhere.
# The country query is only the section's search terms (its label when it
# has none): the country itself goes to each provider's country filter, and
# a longer query - one with indicator names added was tried - finds nothing.
.research_queries <- function(leaf, evidence, policy) {
  label <- .section_label(leaf)
  terms <- policy$search$terms[[leaf]]
  q <- data.frame(leaf = leaf, scope = c("country", "international"),
                  country = c(evidence$meta$base_name, NA),
                  text = c(paste(if (length(terms)) terms else label, collapse = " "),
                           paste(c(label, terms), collapse = " ")))
  # a region or income group is not a country the World Bank files
  # documents under: international research only
  if (!evidence$meta$base_unit %in% .country_codes()) q <- q[q$scope == "international", ]
  q
}

# Research for one report section: every query of the section to every
# provider, the results merged into one record per publication, placed
# under the source policy, and checked against Crossref. Returns a list:
#   sources   - records with `leaf`, `scope` ("country" when the country
#               query found them, else "international"), the policy's
#               `tier`/`eligible`/`reason`, `identity`, `citable` (eligible
#               and verified) and `selected` (citable, within the policy's
#               places for institutional and peer-reviewed sources), ranked: citable first, then country-specific, then by
#               tier, then in the order the providers ranked them;
#   providers - one row per provider and query: `provider`, `scope`,
#               `status`, `found`.
# Sources that are not citable stay in the result with their reasons, so a
# reviewer can see what was set aside and why.
.discover_sources <- function(leaf, evidence, policy, cache = NULL, pause = Sys.sleep,
                              ask = NULL) {
  runs <- .run_searches(.research_queries(leaf, evidence, policy), policy, cache)
  records <- runs$records
  # a publication is country-specific if the country query found it,
  # under any of its identifiers
  in_country <- unlist(.aliases(records[records$query_scope == "country", ]))
  s <- .dedupe_sources(records[setdiff(names(records), "query_scope")])
  s$scope <- ifelse(vapply(.aliases(s), function(a) any(a %in% in_country), logical(1)),
                    "country", "international")
  s <- classify_sources(s, policy)
  s <- .verify_identity(s, .crossref_cached(unique(stats::na.omit(s$doi)), policy, pause, cache))
  s$leaf <- rep(leaf, nrow(s))
  s$citable <- s$eligible & s$identity == "verified"
  # ranked once so the screener reads the best candidates first, then again
  # with its ratings; among equals, recent publications first
  s <- s[order(!s$citable, s$scope != "country", s$tier, seq_len(nrow(s))), ]
  country <- if (evidence$meta$base_unit %in% .country_codes()) evidence$meta$base_name
  screened <- .screen_relevance(s, leaf, country, policy, ask, cache)
  s <- screened$sources
  recent <- s$year >= as.integer(format(Sys.Date(), "%Y")) - policy$limits$prefer_recent_years
  s <- s[order(!s$citable, s$scope != "country", match(s$relevance, .RELEVANCE_ORDER), s$tier,
               !(recent %in% TRUE), seq_len(nrow(s))), ]
  low <- s$relevance %in% "low"
  s$issues[low] <- lapply(s$issues[low], union, "screened_low")
  # one World Bank project's many outputs (deliverables, volumes) take one
  # place: its latest document; the others stay citable, set aside
  repeat_of_project <- .repeat_of_project(s)
  s$issues[repeat_of_project] <- lapply(s$issues[repeat_of_project], union, "same_project")
  # peer-reviewed articles (tier 3) and institutional publications each fill
  # their own places, in rank order - otherwise the World Bank's documents,
  # ranked first, would leave no room for academic evidence
  candidate <- s$citable & !repeat_of_project & !low
  peer <- s$tier %in% 3L
  s$selected <- candidate &
    ifelse(peer, cumsum(candidate & peer) <= policy$limits$max_peer_reviewed_per_leaf,
           cumsum(candidate & !peer) <= policy$limits$max_institutional_per_leaf)
  rownames(s) <- NULL
  list(sources = s, providers = runs$providers, leads = runs$leads, screening = screened$status)
}

# TRUE for a citable source whose World Bank project already has a later
# citable document (ties: the better-ranked one is kept).
.repeat_of_project <- function(s) {
  in_project <- s$citable & !is.na(s$project)
  repeated <- logical(nrow(s))
  for (p in unique(s$project[in_project])) {
    rows <- which(in_project & s$project == p)
    # which.max: the first of the latest; with no years known, the best-ranked
    kept <- if (all(is.na(s$year[rows]))) rows[[1]] else rows[[which.max(s$year[rows])]]
    repeated[setdiff(rows, kept)] <- TRUE
  }
  repeated
}

# Every query to every provider, through the cache. Returns list(records,
# providers, leads): `records` the providers' rows as source records, with
# the `query_scope` that found each; `providers` a status row per search;
# `leads` every mAI search result with what became of it (.resolve_leads()).
.run_searches <- function(queries, policy, cache) {
  searchers <- list(wb_documents = .search_wb, openalex = .search_openalex,
                    mai_search = .search_mai)
  leads <- list(data.frame(url = character(0), title = character(0), title_truncated = logical(0),
                           status = character(0), scope = character(0)))
  n <- policy$limits$max_candidates_per_leaf
  journals <- policy$journals$openalex_id[policy$journals$status == "preferred"]
  # nothing found (every provider down, say) still gives every column
  found <- list(cbind(.as_sources(data.frame(provider = character(0), title = character(0))),
                      query_scope = character(0)))
  providers <- list()
  for (i in seq_len(nrow(queries))) {
    for (p in names(searchers)) {
      # the key holds everything the answer depends on: another query, limit,
      # search setting or journal list is another search
      key <- list("search", p, unname(as.list(queries[i, c("scope", "country", "text")])), n,
                  policy$search, journals)
      res <- .cached(cache, key, function() searchers[[p]](queries[i, ], policy, n),
                     keep = function(r) identical(r$status, "ok"))
      if (NROW(res$leads)) {
        resolved <- .resolve_leads(res$leads, policy, cache)
        res$rows <- resolved$rows
        leads[[length(leads) + 1L]] <- cbind(resolved$leads, scope = queries$scope[[i]])
      }
      providers[[length(providers) + 1L]] <- data.frame(
        provider = p, scope = queries$scope[[i]], status = res$status, found = NROW(res$rows))
      if (NROW(res$rows)) {
        found[[length(found) + 1L]] <- cbind(.as_sources(res$rows), query_scope = queries$scope[[i]])
      }
    }
  }
  list(records = do.call(rbind, found), providers = do.call(rbind, providers),
       leads = do.call(rbind, leads))
}

# A report section's title in the taxonomy ("public_sector_hrm" ->
# "Public Sector HRM").
.section_label <- function(leaf) {
  tx <- as.data.frame(cgjrdata::cgjr_taxonomy)
  unique(.leaf_label(tx)[.leaf_key(tx) == leaf])
}

# -- Cache ---

# The research cache: public metadata only (search results, DOI records -
# never guidance), on disk under CGJR_RESEARCH_CACHE or R's per-user cache
# directory, each entry kept for the policy's `cache_days`.
.research_cache <- function(policy) {
  dir <- Sys.getenv("CGJR_RESEARCH_CACHE", tools::R_user_dir("cgjrapp", "cache"))
  cachem::cache_disk(file.path(dir, "research"), max_age = policy$limits$cache_days * 86400)
}

# `compute()`'s value from `cache` under a key made from `key` (any R
# value), computing and storing it when absent - only if `keep(value)`, so
# a failure is retried next time rather than remembered. No cache: compute.
.cached <- function(cache, key, compute, keep = function(value) TRUE) {
  if (is.null(cache)) return(compute())
  k <- rlang::hash(key)
  hit <- cache$get(k)
  if (!cachem::is.key_missing(hit)) return(hit)
  value <- compute()
  if (keep(value)) cache$set(k, value)
  value
}

# .crossref_lookup() with each DOI's answer cached on its own, so a run asks
# Crossref only about DOIs it has not seen. "found" and "not_found" are
# answers; an outage is not, and is asked again.
.crossref_cached <- function(dois, policy, pause, cache) {
  if (is.null(cache)) return(.crossref_lookup(dois, policy, pause))
  keys <- lapply(dois, function(d) list("crossref", d))
  hits <- lapply(keys, function(k) cache$get(rlang::hash(k)))
  missing <- vapply(hits, cachem::is.key_missing, logical(1))
  fresh <- .crossref_lookup(dois[missing], policy, pause)
  for (j in seq_len(nrow(fresh))) {
    if (fresh$status[[j]] %in% c("found", "not_found")) {
      cache$set(rlang::hash(list("crossref", fresh$doi[[j]])), fresh[j, ])
    }
  }
  .bind_rows(c(hits[!missing], list(fresh)))
}

# -- mAI web search ---

# One Google search through WBG mAI (the platform's Google Search API), for
# leads only: each result is a pointer - a link - that .resolve_leads()
# turns into an authoritative record or sets aside; its title and snippet
# are never used as metadata. The query is limited with `site:` to the
# policy's `mai_sites`, document repositories whose links resolve to records.
# Needs mAI (CGJR_LLM_PROVIDER=mai): without it the search is skipped.
# Returns list(status, rows = NULL, leads): `leads` has `url`, `title`
# (HTML entities decoded) and `title_truncated`.
.search_mai <- function(query, policy, n) {
  if (.llm_provider() != "mai") return(list(status = "not_configured", rows = NULL, leads = NULL))
  sites <- paste0("site:", policy$search$mai_sites, collapse = " OR ")
  text <- paste(c(if (identical(query$scope, "country")) query$country, query$text), collapse = " ")
  token <- tryCatch(.mai_token(), error = function(e) NULL)
  if (is.null(token)) return(list(status = "unauthorized", rows = NULL, leads = NULL))
  res <- httr2::request(paste0(.mai_host(), "/conversationalai/platform/google_search/")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization = paste("Bearer", token), !!!as.list(.mai_headers()),
                       .redact = "Authorization") |>
    httr2::req_body_json(list(query = paste0(text, " (", sites, ")"))) |>
    .provider_perform(policy)
  hits <- if (identical(res$status, "ok")) res$body else list()
  titles <- vapply(hits, function(h) .unescape_html(.scalar(h$title)), "")
  list(status = res$status, rows = NULL,
       leads = data.frame(url = vapply(hits, function(h) .scalar(h$link), ""), title = titles,
                          # Google cuts long titles: "Transforming ... Ambitious ..."
                          title_truncated = grepl(paste0("(\\.\\.\\.|", intToUtf8(0x2026), ")\\s*$"), titles)))
}

# Search leads -> sources. A World Bank documents link carries the
# document's id, which the Bank's own API turns into its full record - kept
# only if it is in a document group the policy reads (project paperwork is
# not). Any other link has no authoritative record to resolve to here, and
# is set aside. Returns list(rows, leads): `rows` provider rows for the
# resolved leads (found by mAI search, described by the World Bank);
# `leads` with `status`: "resolved", "document_group_excluded",
# "not_in_wb_api", "unresolved", or why the API could not answer.
.resolve_leads <- function(leads, policy, cache = NULL) {
  guid <- .wb_guid(leads$url)
  leads$status <- ifelse(is.na(guid), "unresolved", NA_character_)
  rows <- list()
  for (i in which(!is.na(guid))) {
    res <- .cached(cache, list("wb_guid", guid[[i]]), function() {
      .provider_get(.WB_API, policy, list(format = "json", guid = guid[[i]],
                                          fl = paste(.WB_FIELDS, collapse = ",")))
    }, keep = function(r) identical(r$status, "ok"))
    docs <- res$body$documents[setdiff(names(res$body$documents), "facets")]
    leads$status[[i]] <-
      if (!identical(res$status, "ok")) res$status
      else if (length(docs) == 0L) "not_in_wb_api"
      else if (!docs[[1]]$majdocty %in% policy$search$wb_document_groups) "document_group_excluded"
      else "resolved"
    if (leads$status[[i]] == "resolved") {
      row <- .wb_row(docs[[1]])
      row$provider <- list(c("wb_documents", "mai_search"))  # the Bank's record ranks first
      rows[[length(rows) + 1L]] <- row
    }
  }
  list(rows = .bind_rows(rows), leads = leads)
}

# The document id in a World Bank documents link - ".../documentdetail/<id>"
# or ".../curated/en/<id>" - or NA for any other link.
.wb_guid <- function(url) {
  re <- "^https?://documents1?\\.worldbank\\.org/.*(?:documentdetail|/curated/[a-z]{2})/([0-9]+).*$"
  ifelse(grepl(re, url, perl = TRUE), sub(re, "\\1", url, perl = TRUE), NA_character_)
}

# "&#x27;", "&#39;", "&amp;" and the like -> the characters they stand for.
.unescape_html <- function(x) {
  for (re in c("&#x([0-9a-fA-F]+);", "&#([0-9]+);")) {
    m <- gregexpr(re, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(e) {
      code <- sub(re, "\\1", e, perl = TRUE)
      vapply(if (startsWith(re, "&#x")) strtoi(code, 16L) else as.integer(code), intToUtf8, "")
    })
  }
  for (e in names(.HTML_ENTITIES)) x <- gsub(e, .HTML_ENTITIES[[e]], x, fixed = TRUE)
  x
}
.HTML_ENTITIES <- c(`&quot;` = "\"", `&apos;` = "'", `&lt;` = "<", `&gt;` = ">",
                    `&nbsp;` = " ", `&amp;` = "&")  # &amp; last: "&amp;lt;" is "&lt;"

# -- World Bank Documents & Reports ---

.WB_API <- "https://search.worldbank.org/api/v3/wds"
# "authr" asks for the authors (returned as "authors"; asked for by that
# name, they are left out)
.WB_FIELDS <- c("id", "display_title", "docna", "docdt", "docty", "majdocty", "authr",
                "count", "lang", "repnb", "colti", "dois", "url", "pdfurl", "txturl", "abstracts",
                "projectid")

# One search of the World Bank's documents for a query (one row of
# .research_queries()): research document groups and languages from the
# policy, and - for a country query - only documents about that country.
# Returns list(status, rows): `status` is the provider's ("ok", or why it
# gave nothing), `rows` provider rows for .as_sources().
.search_wb <- function(query, policy, n) {
  res <- .provider_get(.WB_API, policy, list(
    format = "json", qterm = query$text, rows = n, fl = paste(.WB_FIELDS, collapse = ","),
    majdocty_exact = paste(policy$search$wb_document_groups, collapse = "^"),
    lang_exact = paste(policy$search$languages, collapse = "^"),
    count_exact = if (identical(query$scope, "country")) query$country))
  docs <- res$body$documents[setdiff(names(res$body$documents), "facets")]
  list(status = res$status, rows = .bind_rows(lapply(docs, .wb_row)))
}

# Document types published as working papers; every other type is a report.
.WB_WORKING_PAPERS <- c("Policy Research Working Paper", "Working Paper (Numbered Series)")

# One World Bank document record -> one provider row. The Bank publishes
# them all (Washington, DC); a document without named authors has the World
# Bank as its author, which Chicago cites as "(World Bank 2024)".
.wb_row <- function(d) {
  wp <- d$docty %in% .WB_WORKING_PAPERS
  author_names <- vapply(d$authors %||% list(), function(a) .scalar(a$author), "")
  row <- data.frame(
    provider = "wb_documents", wb_id = .scalar(d$id), doi = .scalar(d$dois),
    # the document's title ("Ghana - ... : Main report", the Bank's " : " made
    # ": "); `docna` is a volume's name, a fallback only
    title = gsub(" : ", ": ", .squish(.scalar(d$display_title %||% d$docna[[1]]$docna)), fixed = TRUE),
    kind = if (wp) "working_paper" else "report",
    year = as.integer(substr(.scalar(d$docdt), 1L, 4L)), date = substr(.scalar(d$docdt), 1L, 10L),
    institution = "WBG", publisher = "World Bank", place = "Washington, DC",
    collection = if (wp) sub(" \\(Numbered Series\\)$", "", d$docty) else NA_character_,
    # "WPS10390" -> "10390"
    number = if (wp) sub("^[^0-9]*", "", .scalar(d$repnb)) else NA_character_,
    project = .scalar(d$projectid),  # the lending or advisory project, e.g. "P165191"
    url = .https(.scalar(d$url)), pdf_url = .https(.scalar(d$pdfurl)),
    text_url = .https(.scalar(d$txturl)), abstract = .scalar(d$abstracts$`cdata!`),
    language = .scalar(d$lang))
  row$authors <- list(if (length(author_names)) .parse_names(author_names, "family_given")
                      else data.frame(family = NA_character_, given = NA_character_,
                                      literal = "World Bank"))
  row
}

# Provider rows from a list of one-row data frames; none -> zero rows.
.bind_rows <- function(rows) {
  if (length(rows) == 0L) return(data.frame(provider = character(0), title = character(0)))
  do.call(rbind, unname(rows))
}

.squish <- function(x) trimws(gsub("\\s+", " ", x))

# The Bank's own records still give http:// links to its https site.
.https <- function(x) sub("^http://", "https://", x)

# -- OpenAlex ---

.OPENALEX_API <- "https://api.openalex.org/works"
.OPENALEX_FIELDS <- c("id", "doi", "display_name", "publication_year", "publication_date",
                      "type", "authorships", "primary_location", "best_oa_location", "biblio",
                      "is_retracted", "abstract_inverted_index", "language")

# One search of OpenAlex for a query, limited to articles in the policy's
# preferred journals - the registry is applied in the query itself, so no
# credits go on journals the policy would reject. OpenAlex cannot filter a
# work by the country it studies, so a country query carries the country's
# name in its text. The API key (CGJR_OPENALEX_KEY, optional) raises the
# daily budget. Returns list(status, rows) as .search_wb() does.
.search_openalex <- function(query, policy, n) {
  j <- policy$journals
  key <- Sys.getenv("CGJR_OPENALEX_KEY")
  res <- .provider_get(.OPENALEX_API, policy, list(
    search = paste(c(if (identical(query$scope, "country")) query$country, query$text), collapse = " "),
    filter = paste0("primary_location.source.id:",
                    paste(j$openalex_id[j$status == "preferred"], collapse = "|"), ",type:article"),
    `per-page` = n, select = paste(.OPENALEX_FIELDS, collapse = ","),
    api_key = if (nzchar(key)) key))
  list(status = res$status, rows = .bind_rows(lapply(res$body$results, .openalex_row)))
}

# One OpenAlex work -> one provider row. OpenAlex gives authors only as
# display names ("Sara Dewachter"), so they are split on the last word and
# the row is flagged "author_name_unsplit" (Crossref's split names replace
# them where the DOI is known).
.openalex_row <- function(w) {
  src <- w$primary_location$source
  b <- w$biblio
  author_names <- vapply(w$authorships,
                         function(a) .scalar(a$author$display_name %||% a$raw_author_name), "")
  row <- data.frame(
    provider = "openalex", openalex_id = .scalar(w$id), doi = .scalar(w$doi),
    title = .squish(.scalar(w$display_name)), kind = "journal_article",
    year = as.integer(.scalar(w$publication_year)), date = .scalar(w$publication_date),
    journal = .scalar(src$display_name),
    openalex_source_id = sub("^https://openalex\\.org/", "", .scalar(src$id)),
    volume = .scalar(b$volume), issue = .scalar(b$issue),
    pages = if (is.null(b$last_page)) .scalar(b$first_page) else paste0(b$first_page, "-", b$last_page),
    url = .scalar(w$primary_location$landing_page_url), pdf_url = .scalar(w$best_oa_location$pdf_url),
    abstract = .openalex_abstract(w$abstract_inverted_index), language = .scalar(w$language),
    is_retracted = isTRUE(w$is_retracted))
  row$issn <- list(as.character(unlist(src$issn)))
  row$authors <- list(.parse_names(author_names, "display"))
  row$issues <- list(if (length(author_names)) "author_name_unsplit" else character(0))
  row
}

# OpenAlex stores an abstract as an index, word -> its positions; read back
# in position order.
.openalex_abstract <- function(index) {
  if (length(index) == 0L) return(NA_character_)
  words <- rep(names(index), lengths(index))
  paste(words[order(unlist(index))], collapse = " ")
}

# A JSON value as one string: NULL, an empty object or an empty array is NA.
.scalar <- function(x) if (length(x) == 0L) NA_character_ else as.character(x[[1]])

# A GET to a provider's search API (a fixed, trusted endpoint - not a URL
# from a search result, so not .check_url()'s job). A provider that fails
# never stops a research run: it reports why, and gives nothing.
# Returns list(status, body): body is the parsed JSON when status is "ok".
.provider_get <- function(url, policy, params) {
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    .provider_perform(policy)
}

# Sends a provider request (any method) with our User-Agent and the
# policy's timeout, and says how it went: list(status, body).
.provider_perform <- function(req, policy) {
  req <- req |>
    httr2::req_user_agent(.user_agent()) |>
    httr2::req_timeout(policy$limits$request_timeout_s) |>
    httr2::req_error(is_error = function(resp) FALSE)
  r <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(r)) return(list(status = "unavailable", body = NULL))  # timeout, no connection
  if (httr2::resp_status(r) %in% c(401L, 403L)) return(list(status = "unauthorized", body = NULL))
  if (httr2::resp_status(r) == 429L) return(list(status = "rate_limited", body = NULL))
  # a record that does not exist (e.g. a DOI no one registered)
  if (httr2::resp_status(r) == 404L) return(list(status = "not_found", body = NULL))
  if (httr2::resp_is_error(r)) return(list(status = "unavailable", body = NULL))
  body <- tryCatch(httr2::resp_body_json(r), error = function(e) NULL)
  if (is.null(body)) return(list(status = "malformed", body = NULL))
  list(status = "ok", body = body)
}
