# ---
# Safe fetching of external publications.
#
# Every URL the research layer requests comes from outside: a provider's
# record, a redirect, or an LLM search result. Each is checked before any
# request is sent - https only, no IP literals, internal names or private
# addresses - so a planted link cannot reach the Bank's internal network
# (server-side request forgery).
# ---

# A URL -> list(ok, url, reason, host, ip): `url` is the form to request
# (http upgraded to https), `reason` says why a URL is refused, `ip` is the
# checked address the request must go to.
.check_url <- function(url, resolve = .resolve_host) {
  if (length(url) != 1L || is.na(url) || !nzchar(trimws(url))) return(.refuse("invalid_url"))
  # the scheme is read before parsing: a parser may reject (or misread)
  # "javascript:" and "data:" URLs before it can be asked what they are
  scheme <- tolower(regmatches(url, regexpr("^[A-Za-z][A-Za-z0-9+.-]*(?=:)", url, perl = TRUE)))
  if (length(scheme) == 0L) return(.refuse("invalid_url"))
  if (!scheme %in% c("http", "https")) return(.refuse("unsafe_scheme"))
  u <- tryCatch(httr2::url_parse(url), error = function(e) NULL)
  if (is.null(u) || is.null(u$hostname) || !nzchar(u$hostname)) return(.refuse("invalid_url"))
  # "localhost." is the same name as "localhost" in DNS
  host <- sub("\\.$", "", tolower(u$hostname))
  if (!is.null(u$username) || !is.null(u$password)) return(.refuse("credentials_in_url"))
  if (!is.null(u$port) && u$port != "443") return(.refuse("unusual_port"))
  if (.is_ip_literal(host)) return(.refuse("ip_literal"))
  if (.is_internal_name(host)) return(.refuse("internal_host"))
  # a public-looking name can still point inside the network
  ips <- resolve(host)
  if (length(ips) == 0L) return(.refuse("dns_failure"))
  if (any(.is_private_ip(ips))) return(.refuse("private_address"))
  u$scheme <- "https"
  list(ok = TRUE, url = httr2::url_build(u), reason = "ok", host = host, ip = ips[[1]])
}

.check_link <- function(url, policy, expected_domains = NULL, content_types = NULL,
                        resolve = .resolve_host) {
  hop <- .follow(url, policy, resolve, expected_domains, function(target) {
    r <- .request(target, "HEAD", policy)
    # some servers refuse or mishandle HEAD; ask for the first kilobyte instead
    if (!is.null(r) && httr2::resp_status(r) %in% c(403L, 405L, 501L)) {
      r <- .request(target, "GET", policy, headers = list(Range = "bytes=0-1023"))
    }
    r
  })
  r <- hop$response
  type <- if (is.null(r)) NA_character_ else httr2::resp_content_type(r)
  status <- hop$status
  if (identical(status, "ok") && !is.null(content_types) && !type %in% content_types) {
    status <- "unexpected_content"
  }
  list(status = status, reason = hop$reason %||% NA_character_, url = url,
       final_url = hop$final_url,
       http_status = if (is.null(r)) NA_integer_ else httr2::resp_status(r),
       content_type = type, checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# Downloads a document (for text extraction), following redirects as
# .check_link() does, with the policy's size limit. Returns list(status,
# final_url, content_type, body): `body` is raw, NULL unless status is "ok".
.fetch_body <- function(url, policy, expected_domains = NULL, resolve = .resolve_host) {
  max_bytes <- policy$limits$max_pdf_mb * 1024^2
  hop <- .follow(url, policy, resolve, expected_domains, function(target) {
    .request(target, "GET", policy, max_bytes = max_bytes)
  })
  r <- hop$response
  status <- hop$status
  if (identical(status, "ok")) {
    declared <- suppressWarnings(as.numeric(httr2::resp_header(r, "Content-Length", "0")))
    if (isTRUE(declared > max_bytes) || length(r$body) > max_bytes) status <- "too_large"
  }
  list(status = status, final_url = hop$final_url,
       content_type = if (is.null(r)) NA_character_ else httr2::resp_content_type(r),
       body = if (identical(status, "ok")) r$body)
}

# Requests `url` with `send(target)`, following redirects one hop at a time
# up to the policy's limit; every hop is checked by .check_url() before it is
# requested. A redirect that ends outside `expected_domains` (when given), or
# on a site's front page (a "soft 404": the document is gone), is
# "redirected_elsewhere". Returns list(status, final_url, response).
.follow <- function(url, policy, resolve, expected_domains, send) {
  current <- url
  for (i in 0:policy$limits$max_redirects) {
    target <- .check_url(current, resolve)
    if (!target$ok) {
      return(list(status = "unsafe", reason = target$reason, final_url = current, response = NULL))
    }
    r <- send(target)
    if (identical(r, "too_large")) return(list(status = "too_large", final_url = target$url, response = NULL))
    code <- if (is.null(r)) NA_integer_ else httr2::resp_status(r)
    location <- if (is.null(r)) NULL else httr2::resp_header(r, "Location")
    if (code %in% c(301L, 302L, 303L, 307L, 308L) && !is.null(location)) {
      current <- httr2::url_build(httr2::url_parse(location, base_url = target$url))
      next
    }
    status <- .status_of(code)
    if (i > 0L && identical(status, "ok")) {
      outside <- !is.null(expected_domains) && !.in_domains(target$host, expected_domains)
      front_page <- httr2::url_parse(target$url)$path %in% c("", "/")
      if (outside || front_page) status <- "redirected_elsewhere"
    }
    return(list(status = status, final_url = target$url, response = r))
  }
  list(status = "too_many_redirects", final_url = current, response = NULL)
}

# Is `host` one of `domains` or a subdomain of one ("documents1.worldbank.org"
# is in "worldbank.org"; "worldbank.org.example.com" is not)?
.in_domains <- function(host, domains) {
  any(host == domains | endsWith(host, paste0(".", domains)))
}

# What an HTTP status says about a link. No response at all (NA: timeout,
# connection refused), a rate limit or a server error is "unreachable" - a
# temporary state, never reported as a broken link.
.status_of <- function(code) {
  if (is.na(code) || code == 429L || code >= 500L) return("unreachable")
  if (code >= 200L && code < 300L) return("ok")
  if (code %in% c(401L, 402L, 403L)) return("restricted")
  "broken"  # any other failure: 404, 400, a redirect with nowhere to go
}

# One request to a URL that passed .check_url(). curl is told to connect to
# the address that was checked (CURLOPT_RESOLVE), so a second DNS answer
# cannot swap in an internal one; redirects are not followed here, but one
# hop at a time by the caller, each hop checked first. HTTP errors come back
# as responses; a network failure or timeout comes back as NULL, and curl
# stopping a download at `max_bytes` as the string "too_large".
.request <- function(target, method, policy, headers = list(), max_bytes = NULL) {
  ip <- if (grepl(":", target$ip)) paste0("[", target$ip, "]") else target$ip
  req <- httr2::request(target$url) |>
    httr2::req_method(method) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_user_agent(.user_agent()) |>
    httr2::req_options(followlocation = FALSE, resolve = paste0(target$host, ":443:", ip)) |>
    # curl aborts a transfer that would pass the limit
    httr2::req_options(maxfilesize = max_bytes) |>
    httr2::req_timeout(policy$limits$request_timeout_s) |>
    httr2::req_error(is_error = function(resp) FALSE)
  tryCatch(httr2::req_perform(req), error = function(e) {
    if (grepl("maximum file size", conditionMessage(e), ignore.case = TRUE)) "too_large"
  })
}

# Who is asking: many publishers refuse anonymous clients.
.user_agent <- function() {
  paste0("cgjrapp/", utils::packageVersion("cgjrapp"),
         " (+https://github.com/WB-PIDA-Data-Science-Shop/cgjrapp)")
}

.resolve_host <- function(host) curl::nslookup(host, error = FALSE, multiple = TRUE)

# Addresses that are not on the public internet: IPv4 "this network" (0/8),
# private (10/8, 172.16/12, 192.168/16), carrier-grade NAT (100.64/10),
# loopback (127/8), link-local and cloud metadata (169.254/16), multicast and
# reserved (224 and up); IPv6 unspecified, loopback, unique-local (fc00::/7)
# and link-local (fe80::/10). IPv4-mapped IPv6 is judged as its IPv4 part.
.is_private_ip <- function(ip) {
  ip <- sub("^::ffff:", "", tolower(ip))
  v4 <- grepl("^[0-9]+(\\.[0-9]+){3}$", ip)
  out <- logical(length(ip))
  if (any(v4)) {
    o <- do.call(rbind, lapply(strsplit(ip[v4], ".", fixed = TRUE), as.integer))
    a <- o[, 1]; b <- o[, 2]
    out[v4] <- a %in% c(0L, 10L, 127L) | a >= 224L | (a == 100L & b >= 64L & b <= 127L) |
      (a == 169L & b == 254L) | (a == 172L & b >= 16L & b <= 31L) | (a == 192L & b == 168L)
  }
  out[!v4] <- ip[!v4] %in% c("::", "::1") | grepl("^(f[cd]|fe[89ab])", ip[!v4])
  out
}

# An IP address written as the host: IPv6 (any colon), or IPv4 in dotted,
# integer, octal or hex form ("2130706433" and "0x7f.1" are 127.0.0.1 to
# curl). Links to publications use names, so any such host is refused.
.is_ip_literal <- function(host) {
  grepl(":", host) || startsWith(host, "[") ||
    grepl("^(0x[0-9a-f]+|[0-9]+)(\\.(0x[0-9a-f]+|[0-9]+))*$", host)
}

# Names that only mean something inside a network: single-label hosts
# ("intranet", "localhost") and the reserved local suffixes.
.is_internal_name <- function(host) {
  !grepl(".", host, fixed = TRUE) || grepl("\\.(localhost|local|internal|lan)$", host)
}

.refuse <- function(reason) list(ok = FALSE, url = NA_character_, reason = reason)
