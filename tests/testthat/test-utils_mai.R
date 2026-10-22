# mAI provider: the requests llm_chat() sends, the desktop token cache and the
# endpoint check. The token helper is faked (local_mai()); no network.

test_that("mAI requests go to the DEV v2 deployment route by default", {
  local_mai()
  req <- capture_request(llm_chat("sys"))
  expect_identical(req$url, paste0(
    "https://azapimdev.worldbank.org/conversationalai/v2/openai/deployments/",
    "gpt-4.1-mini/chat/completions?api-version=2025-01-01-preview"))
})

test_that("CGJR_MAI_ENV picks the QA or PROD gateway, and anything else is an error", {
  local_mai(CGJR_MAI_ENV = "QA")
  expect_match(capture_request(llm_chat())$url, "^https://azapimqa\\.worldbank\\.org/conversationalai/v2/")
  withr::local_envvar(CGJR_MAI_ENV = "prod")
  expect_match(capture_request(llm_chat())$url, "^https://azapim\\.worldbank\\.org/conversationalai/v2/")
  withr::local_envvar(CGJR_MAI_ENV = "STAGING")
  expect_error(llm_chat(), "CGJR_MAI_ENV.*DEV, QA or PROD")
})

test_that("mAI requests carry the desktop token and the gateway's usage headers", {
  tok <- fake_jwt(sub = "me")
  local_mai(fetch = function(env) tok)
  h <- httr2::req_get_headers(capture_request(llm_chat()), "reveal")
  expect_identical(h$Authorization, paste("Bearer", tok))
  expect_identical(h$`x-source-type`, "interactive")
  expect_identical(h$`x-team-name`, "govcgjrapp")

  withr::local_envvar(CGJR_MAI_TEAM = "pida-poc")
  h <- httr2::req_get_headers(capture_request(llm_chat()), "reveal")
  expect_identical(h$`x-team-name`, "pida-poc")
})

test_that("mAI requests lift the gateway's 1,024-token output default", {
  local_mai()
  body <- capture_request(llm_chat())$body$data
  expect_gt(body$max_completion_tokens, 1024)
})

test_that("the desktop token is fetched once and reused while it is fresh", {
  fetches <- local_mai()
  capture_request(llm_chat())
  capture_request(llm_chat())
  expect_identical(fetches(), 1L)
})

test_that("the token is asked for the configured environment, and refetched when it changes", {
  asked <- character()
  local_mai(fetch = function(env) { asked <<- c(asked, env); fake_jwt() })
  capture_request(llm_chat())
  withr::local_envvar(CGJR_MAI_ENV = "QA")
  capture_request(llm_chat())
  expect_identical(asked, c("DEV", "QA"))
})

test_that("a token about to expire is fetched again", {
  fetches <- local_mai(fetch = function(env) fake_jwt(exp = as.numeric(Sys.time()) + 120))
  capture_request(llm_chat())
  capture_request(llm_chat())
  expect_gt(fetches(), 1L)
})

test_that("without CGJR_MAI_PYTHON_PROJECT the error says which variable to set", {
  withr::local_envvar(CGJR_LLM_PROVIDER = "mai", CGJR_MAI_PYTHON_PROJECT = NA)
  cgjrapp:::.mai_clear_token()
  withr::defer(cgjrapp:::.mai_clear_token())
  httr2::local_mocked_responses(function(req) httr2::response(400))
  expect_error(llm_chat()$chat("hi"), "CGJR_MAI_PYTHON_PROJECT")
})

test_that("the token helper's output is the token, fetched with uv's Python downloads off", {
  withr::local_envvar(CGJR_MAI_PYTHON_PROJECT = "C:/fake/mai-poc", CGJR_MAI_UV = local_fake_uv(),
                      UV_PYTHON_DOWNLOADS = "automatic")
  expect_identical(cgjrapp:::.mai_fetch_token("QA"), "tok.never.QA")
  expect_identical(Sys.getenv("UV_PYTHON_DOWNLOADS"), "automatic")
})

test_that("a token helper that fails gives a desktop sign-in message", {
  withr::local_envvar(CGJR_LLM_PROVIDER = "mai", CGJR_MAI_PYTHON_PROJECT = "C:/fake/mai-poc",
                      CGJR_MAI_UV = "no-such-uv-command")
  cgjrapp:::.mai_clear_token()
  withr::defer(cgjrapp:::.mai_clear_token())
  expect_error(llm_chat(), "Could not get a mAI desktop token.*VPN")
})

test_that("a 401 from mAI drops the cached token and retries the turn once", {
  fetches <- local_mai()
  cgjrapp:::.mai_token()
  e401 <- structure(list(message = "HTTP 401 Unauthorized.", call = NULL),
                    class = c("httr2_http_401", "httr2_http", "httr2_error", "error", "condition"))
  f <- fake_chat_factory(list(e401, c("fine")))
  expect_message(out <- wait_for(cgjrapp:::.stream_retry(f, "q", delays = 0)), "rejected the token")
  expect_identical(out, "fine")
  cgjrapp:::.mai_token()
  expect_identical(fetches(), 2L)

  f <- fake_chat_factory(list(e401, e401, c("never")))
  expect_message(expect_error(wait_for(cgjrapp:::.stream_retry(f, "q", delays = 0)), "401"),
                 "rejected the token")
  expect_identical(attr(f, "calls")(), 2L)
})

test_that("check_llm_available() asks the mAI health endpoint, with the token", {
  tok <- fake_jwt()
  local_mai(fetch = function(env) tok)
  seen <- NULL
  httr2::local_mocked_responses(function(req) { seen <<- req; httr2::response(200) })
  expect_identical(check_llm_available(), "ok")
  expect_identical(seen$url, "https://azapimdev.worldbank.org/conversationalai/platform/health/")
  expect_identical(httr2::req_get_headers(seen, "reveal")$Authorization, paste("Bearer", tok))

  httr2::local_mocked_responses(function(req) httr2::response(401))
  expect_identical(check_llm_available(), "auth")
  httr2::local_mocked_responses(function(req) httr2::response(403))
  expect_identical(check_llm_available(), "auth")
  httr2::local_mocked_responses(function(req) stop("Could not resolve host"))
  expect_identical(check_llm_available(), "unreachable")
})

test_that("check_llm_available() is 'auth' when no desktop token can be had", {
  local_mai(fetch = function(env) stop("Could not get a mAI desktop token."))
  httr2::local_mocked_responses(function(req) httr2::response(200))
  expect_message(st <- check_llm_available(), "Could not get a mAI desktop token")
  expect_identical(st, "auth")
})

test_that("under mAI, sign-in and connection problems point to desktop access, not an API key", {
  local_mai()
  auth <- suppressMessages(llm_error_message(simpleError("HTTP 401 Unauthorized")))
  expect_match(auth, "mAI desktop")
  expect_no_match(auth, "CGJR_LLM_API_KEY|CGJR_LLM_BASE_URL")
  expect_match(cgjrapp:::.llm_problem_message("auth"), "mAI desktop")
  down <- cgjrapp:::.llm_problem_message("unreachable")
  expect_match(down, "VPN")
  expect_no_match(down, "CGJR_LLM_API_KEY|CGJR_LLM_BASE_URL")
})

test_that("embed_texts() calls the v2 embeddings route and keeps the input order", {
  tok <- fake_jwt()
  local_mai(fetch = function(env) tok)
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    # the reply lists the vectors out of order; `index` says which input each is for
    httr2::response_json(body = list(data = list(
      list(index = 1L, embedding = list(0, 1)), list(index = 0L, embedding = list(1, 0)))))
  })
  v <- embed_texts(c("first text", "second text"))
  expect_identical(seen$url, paste0(
    "https://azapimdev.worldbank.org/conversationalai/v2/openai/deployments/",
    "text-embedding-3-small/embeddings?api-version=2024-10-01-preview"))
  h <- httr2::req_get_headers(seen, "reveal")
  expect_identical(h$Authorization, paste("Bearer", tok))
  expect_identical(h$`x-team-name`, "govcgjrapp")
  expect_identical(seen$body$data$input, list("first text", "second text"))
  expect_identical(v, rbind(c(1, 0), c(0, 1)))
})

test_that("only reasoning models (gpt-5, o-series) are sent a reasoning effort", {
  local_mai(CGJR_LLM_MODEL = "gpt-5.5")
  expect_false(is.null(capture_request(llm_chat())$body$data$reasoning_effort))
  withr::local_envvar(CGJR_LLM_MODEL = "o4-mini")
  expect_false(is.null(capture_request(llm_chat())$body$data$reasoning_effort))
  withr::local_envvar(CGJR_LLM_MODEL = "gpt-4.1-mini")
  expect_null(capture_request(llm_chat())$body$data$reasoning_effort)
})
