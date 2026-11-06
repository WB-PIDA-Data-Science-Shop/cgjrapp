# A stand-in DNS for the fetch tests: `map` names the addresses each host
# resolves to; any other host resolves to one public documentation address
# (TEST-NET-3), and a host mapped to NULL does not resolve at all.
fake_dns <- function(map = list()) {
  function(host) if (host %in% names(map)) map[[host]] else "203.0.113.10"
}

# Mocked HTTP for a set of URLs: `routes` maps "METHOD url" (or just "url",
# any method) to a response, or to a function(req) giving one. Unrouted
# requests fail the test. Returns a function listing the requests made.
local_routes <- function(routes, env = parent.frame()) {
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- req
    r <- routes[[paste(req$method %||% "GET", req$url)]] %||% routes[[req$url]]
    if (is.null(r)) stop("unexpected request: ", req$method %||% "GET", " ", req$url)
    if (is.function(r)) r(req) else r
  }, env = env)
  function() seen
}

# A response with a status and, optionally, a content type, a redirect
# target and a body.
resp <- function(status, type = NULL, location = NULL, body = raw()) {
  headers <- Filter(Negate(is.null), list(`Content-Type` = type, Location = location))
  httr2::response(status, headers = headers, body = body)
}
