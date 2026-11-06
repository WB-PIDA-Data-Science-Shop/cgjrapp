# Safe fetching: URL safety checks, link checks and size-limited downloads.
# HTTP is mocked and DNS is a stand-in (helper-fetch.R): no network.

test_that("an https link to a public host is safe as it is; http is upgraded", {
  ok <- cgjrapp:::.check_url("https://documents.worldbank.org/curated/en/1", fake_dns())
  expect_true(ok$ok)
  expect_identical(ok$url, "https://documents.worldbank.org/curated/en/1")
  up <- cgjrapp:::.check_url("http://documents.worldbank.org/curated/en/1", fake_dns())
  expect_true(up$ok)
  expect_identical(up$url, "https://documents.worldbank.org/curated/en/1")
})

# The reason .check_url() gives for each URL
refusal <- function(urls, dns = fake_dns()) {
  vapply(urls, function(u) cgjrapp:::.check_url(u, dns)$reason, character(1), USE.NAMES = FALSE)
}

test_that("only web links are fetched: other schemes are refused", {
  expect_identical(refusal(c("ftp://example.org/a.pdf", "file:///C:/secrets.txt",
                             "javascript:alert(1)", "data:text/html,<b>x</b>")),
                   rep("unsafe_scheme", 4))
})

test_that("empty and malformed URLs are refused as invalid", {
  expect_identical(refusal(c(NA, "", "not a url", "https://", "https://exa mple.org/")),
                   rep("invalid_url", 5))
})

test_that("raw IP addresses are refused, whatever they point to", {
  expect_identical(refusal(c("https://203.0.113.10/a.pdf", "https://[2001:db8::1]/a.pdf")),
                   rep("ip_literal", 2))
  # 127.0.0.1 written as one integer, and in hex: curl would accept both
  expect_identical(refusal(c("https://2130706433/", "https://0x7f.1/")), rep("ip_literal", 2))
})

test_that("a name that resolves to a private or loopback address is refused", {
  dns <- fake_dns(list(`intra.example.org` = "10.1.2.3", `mixed.example.org` = c("203.0.113.5", "192.168.0.7"),
                       `loop.example.org` = "127.0.0.1", `meta.example.org` = "169.254.169.254",
                       `v6.example.org` = "fd00::1", `v6loop.example.org` = "::1",
                       `cgnat.example.org` = "100.64.0.1", `mapped.example.org` = "::ffff:10.0.0.1",
                       `edge.example.org` = c("172.15.0.1", "172.32.0.1")))
  expect_identical(refusal(paste0("https://", c("intra", "mixed", "loop", "meta", "v6", "v6loop",
                                                "cgnat", "mapped"), ".example.org/"), dns),
                   rep("private_address", 8))
  # just outside 172.16.0.0/12: public
  expect_identical(refusal("https://edge.example.org/", dns), "ok")
})

# -- .check_link() ---

POLICY <- load_source_policy()
PDF <- "https://documents.worldbank.org/curated/en/1/pdf/report.pdf"
check_link <- function(url = PDF, ...) cgjrapp:::.check_link(url, POLICY, ..., resolve = fake_dns())

test_that("a working link is checked with HEAD, pinned to the address that was checked", {
  seen <- local_routes(list(`HEAD https://documents.worldbank.org/curated/en/1/pdf/report.pdf` =
                              resp(200, "application/pdf")))
  r <- check_link()
  expect_identical(r[c("status", "final_url", "http_status", "content_type")],
                   list(status = "ok", final_url = PDF, http_status = 200L,
                        content_type = "application/pdf"))
  expect_match(r$checked_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  req <- seen()[[1]]
  expect_identical(req$options$resolve, "documents.worldbank.org:443:203.0.113.10")
  expect_false(req$options$followlocation)
  expect_match(req$options$useragent, "^cgjrapp/")
})

test_that("an IPv6 address is pinned in curl's bracketed form", {
  seen <- local_routes(list(`https://documents.worldbank.org/a.pdf` = resp(200, "application/pdf")))
  cgjrapp:::.check_link("https://documents.worldbank.org/a.pdf", POLICY,
                        resolve = fake_dns(list(`documents.worldbank.org` = "2001:db8::10")))
  expect_identical(seen()[[1]]$options$resolve, "documents.worldbank.org:443:[2001:db8::10]")
})

test_that("a missing page is broken; a login or paywall is restricted", {
  local_routes(list(`https://documents.worldbank.org/a.pdf` = resp(404),
                    `https://documents.worldbank.org/b.pdf` = resp(410),
                    `https://documents.worldbank.org/c.pdf` = resp(401),
                    `https://documents.worldbank.org/d.pdf` = resp(402)))
  status <- vapply(paste0("https://documents.worldbank.org/", c("a", "b", "c", "d"), ".pdf"),
                   function(u) check_link(u)$status, character(1), USE.NAMES = FALSE)
  expect_identical(status, c("broken", "broken", "restricted", "restricted"))
})

test_that("rate limits, server errors and network failures are temporary, not broken", {
  local_routes(list(`https://documents.worldbank.org/a.pdf` = resp(429),
                    `https://documents.worldbank.org/b.pdf` = resp(503),
                    `https://documents.worldbank.org/c.pdf` = function(req) stop("Timeout was reached")))
  status <- vapply(paste0("https://documents.worldbank.org/", c("a", "b", "c"), ".pdf"),
                   function(u) check_link(u)$status, character(1), USE.NAMES = FALSE)
  expect_identical(status, rep("unreachable", 3))
})

test_that("any other failed response is a broken link, never a missing status", {
  local_routes(list(`https://documents.worldbank.org/a.pdf` = resp(400),
                    `https://documents.worldbank.org/b.pdf` = resp(418),
                    `https://documents.worldbank.org/c.pdf` = resp(302)))  # no Location
  status <- vapply(paste0("https://documents.worldbank.org/", c("a", "b", "c"), ".pdf"),
                   function(u) check_link(u)$status, character(1), USE.NAMES = FALSE)
  expect_identical(status, rep("broken", 3))
})

test_that("a server that refuses HEAD is asked again with GET", {
  seen <- local_routes(list(`HEAD https://documents.worldbank.org/a.pdf` = resp(405),
                            `GET https://documents.worldbank.org/a.pdf` = resp(200, "application/pdf"),
                            `HEAD https://documents.worldbank.org/b.pdf` = resp(403),
                            `GET https://documents.worldbank.org/b.pdf` = resp(403)))
  a <- check_link("https://documents.worldbank.org/a.pdf")
  expect_identical(c(a$status, a$http_status), c("ok", "200"))
  # 403 on HEAD can be a server quirk; restricted only when GET says so too
  expect_identical(check_link("https://documents.worldbank.org/b.pdf")$status, "restricted")
  expect_identical(vapply(seen(), function(r) r$method %||% "GET", ""), c("HEAD", "GET", "HEAD", "GET"))
})

test_that("redirects are followed hop by hop to the final address", {
  local_routes(list(
    `https://documents.worldbank.org/curated/en/1` =
      resp(301, location = "https://documents1.worldbank.org/curated/en/1/pdf/r.pdf"),
    `https://documents1.worldbank.org/curated/en/1/pdf/r.pdf` = resp(302, location = "/curated/en/1/r2.pdf"),
    `https://documents1.worldbank.org/curated/en/1/r2.pdf` = resp(200, "application/pdf")))
  r <- check_link("http://documents.worldbank.org/curated/en/1", expected_domains = "worldbank.org")
  expect_identical(r$status, "ok")
  expect_identical(r$final_url, "https://documents1.worldbank.org/curated/en/1/r2.pdf")
  expect_identical(r$url, "http://documents.worldbank.org/curated/en/1")  # as given
})

test_that("a redirect away from the expected site, or to its home page, is flagged", {
  local_routes(list(
    `https://documents.worldbank.org/a` = resp(301, location = "https://some-aggregator.example.com/a"),
    `https://some-aggregator.example.com/a` = resp(200, "text/html"),
    `https://documents.worldbank.org/b` = resp(302, location = "https://documents.worldbank.org/"),
    `https://documents.worldbank.org/` = resp(200, "text/html")))
  away <- check_link("https://documents.worldbank.org/a", expected_domains = "worldbank.org")
  expect_identical(away$status, "redirected_elsewhere")
  expect_identical(away$final_url, "https://some-aggregator.example.com/a")
  # a "soft 404": the document is gone and the site sends you to its front page
  expect_identical(check_link("https://documents.worldbank.org/b")$status, "redirected_elsewhere")
})

test_that("a redirect to an unsafe address is never followed", {
  seen <- local_routes(list(`https://documents.worldbank.org/a` =
                              resp(302, location = "https://169.254.169.254/latest/meta-data/")))
  r <- check_link("https://documents.worldbank.org/a")
  expect_identical(c(r$status, r$reason), c("unsafe", "ip_literal"))
  expect_length(seen(), 1L)  # the metadata address was never requested
  expect_identical(check_link("https://localhost/a")$status, "unsafe")
})

test_that("more redirects than the policy allows end the check", {
  # /r0 -> /r1 -> ... -> /r6 -> a document: 7 redirects, the policy allows 5
  page <- function(i) paste0("https://documents.worldbank.org/r", i)
  routes <- stats::setNames(lapply(0:6, function(i) resp(302, location = page(i + 1L))), page(0:6))
  routes[[page(7)]] <- resp(200, "application/pdf")
  seen <- local_routes(routes)
  expect_identical(check_link(page(0))$status, "too_many_redirects")
  expect_length(seen(), POLICY$limits$max_redirects + 1L)
})

test_that("a page of the wrong kind (HTML where a PDF was expected) is flagged", {
  local_routes(list(`https://documents.worldbank.org/a.pdf` = resp(200, "text/html; charset=utf-8"),
                    `https://documents.worldbank.org/b.pdf` = resp(200, "application/pdf")))
  expect_identical(check_link("https://documents.worldbank.org/a.pdf",
                              content_types = "application/pdf")$status, "unexpected_content")
  expect_identical(check_link("https://documents.worldbank.org/b.pdf",
                              content_types = c("application/pdf", "text/html"))$status, "ok")
})

# -- .fetch_body() ---

test_that("a document within the size limit is downloaded with a GET", {
  seen <- local_routes(list(`GET https://documents.worldbank.org/a.txt` =
                              resp(200, "text/plain", body = charToRaw("Public text."))))
  r <- cgjrapp:::.fetch_body("https://documents.worldbank.org/a.txt", POLICY, resolve = fake_dns())
  expect_identical(r$status, "ok")
  expect_identical(rawToChar(r$body), "Public text.")
  expect_identical(r$content_type, "text/plain")
  # curl is also told the limit, so it stops a transfer that runs past it
  expect_identical(seen()[[1]]$options$maxfilesize, POLICY$limits$max_pdf_mb * 1024^2)
})

test_that("a document larger than the limit is not kept, however its size is learned", {
  limit <- POLICY$limits$max_pdf_mb * 1024^2
  big <- httr2::response(200, headers = list(`Content-Type` = "application/pdf",
                                             `Content-Length` = format(limit + 1, scientific = FALSE)))
  local_routes(list(`https://documents.worldbank.org/declared.pdf` = big,
                    # no size declared, and the body itself is over the limit
                    `https://documents.worldbank.org/actual.pdf` =
                      resp(200, "application/pdf", body = raw(limit + 1)),
                    # curl stopping the transfer at the limit
                    `https://documents.worldbank.org/curl.pdf` = function(req) {
                      stop("Maximum file size exceeded")
                    }))
  fetch <- function(name) cgjrapp:::.fetch_body(paste0("https://documents.worldbank.org/", name),
                                                POLICY, resolve = fake_dns())
  for (name in c("declared.pdf", "actual.pdf", "curl.pdf")) {
    r <- fetch(name)
    expect_identical(r$status, "too_large", label = name)
    expect_null(r$body)
  }
})

test_that("a name that does not resolve is refused", {
  expect_identical(refusal("https://gone.example.org/", fake_dns(list(`gone.example.org` = NULL))),
                   "dns_failure")
})

test_that("internal host names are refused before any lookup", {
  expect_identical(refusal(c("https://localhost/x", "https://intranet/x", "https://files.local/x",
                             "https://wiki.corp.internal/x", "https://a.localhost/x")),
                   rep("internal_host", 5))
  # a trailing dot is the same name in DNS
  expect_identical(refusal(c("https://localhost./x", "https://files.local./x")),
                   rep("internal_host", 2))
})

test_that("a user name or password in the URL is refused", {
  expect_identical(refusal("https://user:secret@documents.worldbank.org/x"), "credentials_in_url")
})

test_that("only the standard https port is allowed", {
  expect_identical(refusal(c("https://example.org:8443/x", "http://example.org:80/x")),
                   rep("unusual_port", 2))
  expect_identical(refusal("https://example.org:443/x"), "ok")
})
