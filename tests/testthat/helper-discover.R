# A recorded provider response (tests/testthat/fixtures/research/), parsed
# so a test can edit it before serving it.
research_fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", "research", name), simplifyVector = FALSE)
}

# Serve `body` (a list, sent as JSON; or a raw string) with `status` to every
# request; returns a function listing the requests made.
local_provider <- function(body, status = 200L, env = parent.frame()) {
  seen <- list()
  payload <- if (is.character(body)) charToRaw(body)
             else charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- req
    httr2::response(status, headers = list(`Content-Type` = "application/json"), body = payload)
  }, env = env)
  function() seen
}

# Mocked research providers for a whole discovery run, routed by host: the
# World Bank, OpenAlex and mAI searches serve `wb`, `openalex` and `mai`
# (lists, or a status code to fail with); a World Bank lookup by guid serves
# `wb_guids[[guid]]` (a fixture name) or nothing; Crossref serves
# `crossref[[doi]]` (a record list) or 404. Returns a function listing
# every request made.
local_research <- function(wb = research_fixture("wb_prwp_10390.json"),
                           openalex = research_fixture("openalex_world_dev.json"),
                           mai = research_fixture("mai_google_search_ghana.json"),
                           wb_guids = list(`335361537384686708` = "wb_guid_prwp_ghana_2018.json",
                                           `758341540692513115` = "wb_guid_pad_ghana_2018.json"),
                           crossref = list(), env = parent.frame()) {
  seen <- list()
  json <- function(x, status = 200L) {
    if (is.numeric(x)) return(httr2::response(x))
    httr2::response(status, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")))
  }
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- req
    host <- httr2::url_parse(req$url)$hostname
    guid <- httr2::url_parse(req$url)$query$guid
    if (host == "search.worldbank.org" && !is.null(guid)) {
      return(json(if (guid %in% names(wb_guids)) research_fixture(wb_guids[[guid]])
                  else list(rows = 0L, total = 0L, documents = setNames(list(), character(0)))))
    }
    if (host == "search.worldbank.org") return(json(wb))
    if (host == "azapimdev.worldbank.org") return(json(mai))
    if (host == "api.openalex.org") return(json(openalex))
    if (host == "api.crossref.org") {
      doi <- utils::URLdecode(sub("^https://api\\.crossref\\.org/works/", "", sub("\\?.*$", "", req$url)))
      rec <- crossref[[doi]]
      return(if (is.null(rec)) httr2::response(404L) else json(list(status = "ok", message = rec)))
    }
    stop("unexpected request: ", req$url)
  }, env = env)
  function() seen
}

# The World Bank API looking documents up by guid: `known` maps a guid to a
# recorded response; any other guid finds nothing. Returns the requests made.
local_wb_guids <- function(known = list(
                             `335361537384686708` = "wb_guid_prwp_ghana_2018.json",
                             `758341540692513115` = "wb_guid_pad_ghana_2018.json"),
                           env = parent.frame()) {
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- req
    guid <- httr2::url_parse(req$url)$query$guid
    body <- if (guid %in% names(known)) research_fixture(known[[guid]])
            else list(rows = 0L, total = 0L, documents = setNames(list(), character(0)))
    httr2::response(200L, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  }, env = env)
  function() seen
}

# The query parameters of a request, as a named list.
query_of <- function(req) httr2::url_parse(req$url)$query

# The runs of 6 words from any guidance passage that appear in `texts` (what
# was sent to a service or an LLM); character(0) when nothing leaked.
guidance_leaks <- function(texts, guidance = load_guidance("standin")) {
  clean <- function(x) gsub("[^a-z0-9 ]+", " ", tolower(x))
  shingles <- unique(unlist(lapply(clean(guidance$passages$text), function(p) {
    w <- strsplit(p, " +")[[1]]
    w <- w[nzchar(w)]
    if (length(w) < 6L) return(character(0))
    vapply(seq_len(length(w) - 5L), function(i) paste(w[i:(i + 5L)], collapse = " "), "")
  })))
  sent <- clean(texts)
  shingles[vapply(shingles, function(s) any(grepl(s, sent, fixed = TRUE)), logical(1))]
}
