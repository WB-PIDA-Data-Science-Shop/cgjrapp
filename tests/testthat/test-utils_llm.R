# LLM layer: endpoint check (mocked HTTP), error messages, async streaming
# (fake chat). No network.

test_that("check_llm_available() reports ok, auth and unreachable", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "https://llm.example/v1", CGJR_LLM_API_KEY = "k")
  httr2::local_mocked_responses(function(req) httr2::response(200))
  expect_identical(check_llm_available(), "ok")
  httr2::local_mocked_responses(function(req) httr2::response(401))
  expect_identical(check_llm_available(), "auth")
  httr2::local_mocked_responses(function(req) httr2::response(404))
  expect_identical(check_llm_available(), "ok")
  # Gemini's answer to a bad key
  httr2::local_mocked_responses(function(req) httr2::response(
    400, body = charToRaw('{"error":{"message":"Please pass a valid API key"}}')))
  expect_identical(check_llm_available(), "auth")
  httr2::local_mocked_responses(function(req) httr2::response(400, body = charToRaw("bad request")))
  expect_identical(check_llm_available(), "ok")
})

test_that("check_llm_available() sends the key to {base}/models", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "https://llm.example/v1/", CGJR_LLM_API_KEY = "sekret")
  seen <- NULL
  httr2::local_mocked_responses(function(req) { seen <<- req; httr2::response(200) })
  check_llm_available()
  expect_identical(seen$url, "https://llm.example/v1/models")
  expect_identical(httr2::req_get_headers(seen, "reveal")$Authorization, "Bearer sekret")
})

test_that("check_llm_available() is 'unreachable' with no server", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "http://127.0.0.1:19999/v1")
  expect_identical(check_llm_available(), "unreachable")
})

test_that("llm_error_message() gives plain messages and logs the real one", {
  expect_message(m <- llm_error_message(simpleError("HTTP 429 Too Many Requests")), "LLM error")
  expect_match(m, "rate limit")
  expect_match(suppressMessages(llm_error_message(simpleError("HTTP 401 Unauthorized"))), "API key")
  expect_match(suppressMessages(llm_error_message(simpleError("HTTP 404 model not found"))), "CGJR_LLM_MODEL")
  expect_match(suppressMessages(llm_error_message(simpleError("Timeout was reached"))), "Cannot reach")
})

test_that("a provider overload (Gemini's 503) reads plainly and counts as transient", {
  e <- simpleError(paste0("\033[1m\033[22mHTTP 503 Service Unavailable.\n",
                          "i [ { \"error\": { \"code\": 503, \"message\": \"This model is ",
                          "currently experiencing high demand.\", \"status\": \"UNAVAILABLE\" } } ]"))
  m <- suppressMessages(llm_error_message(e))
  expect_match(m, "overloaded right now (HTTP 503)", fixed = TRUE)
  expect_false(grepl("\033", m, fixed = TRUE))
  expect_false(grepl("[1m", m, fixed = TRUE))
  expect_true(cgjrapp:::.is_transient(e))
  expect_true(cgjrapp:::.is_transient(simpleError("HTTP 429 Too Many Requests")))
  expect_false(cgjrapp:::.is_transient(simpleError("HTTP 400 Bad Request")))
  expect_false(cgjrapp:::.is_transient(simpleError("HTTP 401 Unauthorized")))
})

test_that("a used-up daily quota is not retried and says when it resets", {
  e <- simpleError(paste0(
    "HTTP 429 Too Many Requests.\n i [ { \"error\": { \"code\": 429, \"message\": \"You exceeded ",
    "your current quota ... limit: 20, model: gemini-3.8-flash\\nPlease retry in 4h32m18.42s.\", ",
    "\"quotaId\": \"GenerateRequestsPerDayPerProjectPerModel-FreeTier\" } } ]"))
  expect_false(cgjrapp:::.is_transient(e))
  m <- suppressMessages(llm_error_message(e))
  expect_match(m, "daily request quota")
  expect_match(m, "resets in about 4 h 32 min", fixed = TRUE)
  # a per-minute rate limit is still retried
  expect_true(cgjrapp:::.is_transient(simpleError("HTTP 429 Too Many Requests. Please retry in 37s.")))
})

test_that("llm_chat() builds an ellmer chat from the env vars", {
  withr::local_envvar(CGJR_LLM_BASE_URL = "https://llm.example/v1", CGJR_LLM_MODEL = "m1")
  ch <- llm_chat("sys")
  expect_s3_class(ch, "Chat")
  expect_identical(ch$get_model(), "m1")
  expect_identical(ch$get_system_prompt(), "sys")
})

test_that(".stream_text() accumulates chunks and reports progress", {
  f <- fake_chat_factory(list(c("Hello ", "world")))
  seen <- character(0)
  out <- wait_for(cgjrapp:::.stream_text(f("sys"), "hi", function(x) seen <<- c(seen, x)))
  expect_identical(out, "Hello world")
  expect_identical(seen, c("Hello ", "Hello world"))
})

test_that(".stream_text() rejects an empty reply and passes errors on", {
  expect_error(wait_for(cgjrapp:::.stream_text(fake_chat_factory(list(c("", " ")))("s"), "hi")),
               "empty reply")
  boom <- fake_chat_factory(list(simpleError("HTTP 429")))
  expect_error(wait_for(cgjrapp:::.stream_text(boom("s"), "hi")), "429")
})
