# utils_llm.R - provider-agnostic LLM access for the AI Report and AI Chat.
#
# Configuration is entirely via environment variables, set in ~/.Renviron
# (local development; never a committed file) or the Posit Connect
# environment panel (production):
#
#   CGJR_LLM_PROVIDER  - "mai" for WBG mAI (utils_mai.R); unset for any
#                        OpenAI-compatible endpoint, configured with:
#   CGJR_LLM_BASE_URL  - OpenAI-compatible API base URL
#                        (default: http://localhost:11434/v1, local Ollama)
#   CGJR_LLM_MODEL     - model name (default: llama3.2; gpt-4.1-mini on mAI)
#   CGJR_LLM_API_KEY   - API key (default: ollama)
#
# Groq is routed to ellmer::chat_groq() (it rejects OpenAI-only fields);
# Ollama and Google Gemini's OpenAI-compatible endpoint use
# ellmer::chat_openai_compatible(); mAI uses ellmer::chat_azure_openai() with
# a desktop token (CGJR_MAI_* variables, see utils_mai.R). Switching provider
# is an env-var change only.
#
# Which guidance the prompts carry is a separate switch,
# CGJR_GUIDANCE_SOURCE (guidance.R): the Official Use Only note only with an
# approved endpoint.

.llm_config <- function() {
  list(
    base_url = sub("/+$", "", Sys.getenv("CGJR_LLM_BASE_URL", "http://localhost:11434/v1")),
    model    = Sys.getenv("CGJR_LLM_MODEL", "llama3.2"),
    api_key  = Sys.getenv("CGJR_LLM_API_KEY", "ollama")
  )
}

.llm_provider <- function() tolower(Sys.getenv("CGJR_LLM_PROVIDER", ""))

#' Create a chat object for the configured LLM provider
#'
#' The provider comes from environment variables (see the top of
#' `R/utils_llm.R`). Under WBG mAI the chat calls the v2 deployment route of
#' `CGJR_MAI_ENV` (DEV, QA or PROD) with a cached desktop token, an
#' 8,000-token output cap and, for gpt-5 / o-series models, a low reasoning
#' effort. Creating it already fetches the token, so sign-in problems surface
#' here.
#'
#' @param system_prompt Character scalar or `NULL`.
#' @return An `ellmer::Chat`.
#' @seealso [check_llm_available()]
#' @export
llm_chat <- function(system_prompt = NULL) {
  if (.llm_provider() == "mai") return(.mai_chat(system_prompt))
  cfg <- .llm_config()
  if (grepl("groq\\.com", cfg$base_url, ignore.case = TRUE)) {
    ellmer::chat_groq(system_prompt = system_prompt, model = cfg$model,
                      credentials = function() cfg$api_key, echo = "none")
  } else {
    ellmer::chat_openai_compatible(base_url = cfg$base_url, model = cfg$model,
                                   system_prompt = system_prompt,
                                   credentials = function() cfg$api_key, echo = "none")
  }
}

#' Check whether the configured LLM endpoint is reachable and accepts the key
#'
#' A 5-second `GET {base_url}/models` with the API key. 401/403, or a 400
#' that complains about the API key (Gemini), mean the key was rejected; any
#' other HTTP response counts as reachable (some servers don't implement
#' `/models`). Under WBG mAI (`CGJR_LLM_PROVIDER = "mai"`) it instead gets a
#' desktop token and asks the gateway's health endpoint: no token, 401 or 403
#' is `"auth"` (the reason is logged).
#'
#' @return `"ok"`, `"auth"` (the key or sign-in was rejected) or
#'   `"unreachable"`.
#' @export
check_llm_available <- function() {
  if (.llm_provider() == "mai") return(.mai_check())
  cfg <- .llm_config()
  resp <- tryCatch(
    httr2::request(paste0(cfg$base_url, "/models")) |>
      httr2::req_auth_bearer_token(cfg$api_key) |>
      httr2::req_timeout(5) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return("unreachable")
  status <- httr2::resp_status(resp)
  # Gemini answers a bad key with 400 "Please pass a valid API key"
  bad_key <- status %in% c(401L, 403L) || (status == 400L && grepl(
    "api.?key", tryCatch(httr2::resp_body_string(resp), error = function(e) ""), ignore.case = TRUE))
  if (bad_key) "auth" else "ok"
}

# A short, plain message for an LLM failure; the real condition is logged.
.llm_problem_message <- function(status) {
  if (.llm_provider() == "mai") return(.mai_problem_message(status))
  switch(status,
    unreachable = paste("Cannot reach the AI service. Check your connection and",
                        "CGJR_LLM_BASE_URL in your .Renviron."),
    auth = "The AI service rejected the API key. Check CGJR_LLM_API_KEY in your .Renviron.",
    NULL)
}

#' Turn an LLM error into a message for the user
#'
#' @param cnd A condition from an ellmer / httr2 call.
#' @return Character scalar. The full condition is written to the log with
#'   [message()].
#' @keywords internal
llm_error_message <- function(cnd) {
  msg <- .plain_message(cnd)
  message("[cgjrapp] LLM error: ", msg)
  if (.is_rate_limit(cnd) && .is_quota_exhausted(cnd)) {
    reset <- .quota_reset(cnd)
    paste0("The AI service's daily request quota for this model is used up (free tiers allow ",
           "only a few requests per day; a full chapter takes 6).",
           if (!is.null(reset)) paste0(" It resets in about ", reset, "."),
           " Try again later, or set CGJR_LLM_MODEL to another model with quota left.")
  } else if (.is_overloaded(cnd)) {
    paste0("The AI service is overloaded right now (", .http_status(cnd) %||% "HTTP 503",
           ") and still was after several retries. This is usually temporary: wait a ",
           "minute or two and try again.")
  } else if (.is_rate_limit(cnd)) {
    "The AI service's rate limit was reached (free tiers allow only a few requests per minute). Wait a minute and try again."
  } else if (inherits(cnd, c("httr2_http_401", "httr2_http_403")) || grepl("\\b40[13]\\b|unauthori[sz]ed|api key", msg, ignore.case = TRUE)) {
    .llm_problem_message("auth")
  } else if (inherits(cnd, "httr2_http_404") || grepl("\\b404\\b|model.*not found", msg, ignore.case = TRUE)) {
    "The AI service does not know this model. Check CGJR_LLM_MODEL in your .Renviron."
  } else if (grepl("timeout|timed out|could not resolve|connect", msg, ignore.case = TRUE)) {
    .llm_problem_message("unreachable")
  } else {
    first <- strsplit(trimws(msg), "\n", fixed = TRUE)[[1]][[1]]
    paste("The AI service returned an error:", substr(gsub("\\s+", " ", first), 1L, 200L))
  }
}

# A condition's message without the terminal styling cli adds ("\033[1m").
.plain_message <- function(cnd) {
  gsub("\u001b\\[[0-9;]*m", "", conditionMessage(cnd))
}

.http_status <- function(cnd) {
  m <- regmatches(.plain_message(cnd), regexpr("HTTP \\d{3}", .plain_message(cnd)))
  if (length(m)) m else NULL
}

.is_rate_limit <- function(cnd) {
  inherits(cnd, "httr2_http_429") ||
    grepl("HTTP 429|rate.?limit|RESOURCE_EXHAUSTED", .plain_message(cnd), ignore.case = TRUE)
}

# 5xx: the provider is overloaded or briefly failing ("This model is
# currently experiencing high demand" - Gemini's 503).
.is_overloaded <- function(cnd) {
  inherits(cnd, c("httr2_http_500", "httr2_http_502", "httr2_http_503", "httr2_http_504")) ||
    grepl("HTTP 50[0234]|UNAVAILABLE|high demand|overloaded", .plain_message(cnd), ignore.case = TRUE)
}

# A daily (not per-minute) quota is used up - e.g. Gemini's free tier, 20
# requests per day per model ("GenerateRequestsPerDayPerProjectPerModel").
# Retrying cannot help until it resets.
.is_quota_exhausted <- function(cnd) {
  grepl("PerDay|per day|retry in \\d+h", .plain_message(cnd), ignore.case = TRUE)
}

# "Please retry in 4h32m18.4s" -> "4 h 32 min", or NULL.
.quota_reset <- function(cnd) {
  m <- regmatches(.plain_message(cnd), regexec("retry in (?:(\\d+)h)?(?:(\\d+)m)?", .plain_message(cnd), perl = TRUE))[[1]]
  if (!length(m) || !nzchar(paste0(m[[2]], m[[3]]))) return(NULL)
  trimws(paste(if (nzchar(m[[2]])) paste(m[[2]], "h"), if (nzchar(m[[3]])) paste(m[[3]], "min")))
}

# Errors worth retrying: they usually clear within seconds to a minute.
.is_transient <- function(cnd) {
  (.is_rate_limit(cnd) && !.is_quota_exhausted(cnd)) || .is_overloaded(cnd)
}

# Seconds to wait before retry n (1-4). The 60 s wait outlasts a per-minute
# token limit (mAI DEV: 25k tokens a minute for most models).
.RETRY_DELAYS <- c(5, 15, 30, 60)

# Stream a prompt, retrying transient errors with backoff. `make_chat()` gives
# the chat to use for each attempt; `on_retry(n, delay, cnd)` is told before
# each wait. A mAI 401 gets one immediate retry with a fresh token. A promise
# for the full text.
.stream_retry <- function(make_chat, prompt, on_chunk = NULL, on_retry = NULL,
                          delays = .RETRY_DELAYS) {
  refreshed <- FALSE
  attempt <- function(n) {
    p <- .stream_text(make_chat(), prompt, on_chunk)
    promises::catch(p, function(e) {
      if (!refreshed && .is_mai_token_rejected(e)) {
        refreshed <<- TRUE
        message("[cgjrapp] mAI rejected the token; fetching a new one and retrying")
        .mai_clear_token()
        return(attempt(n))
      }
      if (n > length(delays) || !.is_transient(e)) stop(e)
      message("[cgjrapp] transient LLM error, retry ", n, " in ", delays[[n]], "s: ",
              strsplit(.plain_message(e), "\n", fixed = TRUE)[[1]][[1]])
      if (!is.null(on_retry)) on_retry(n, delays[[n]], e)
      promises::then(.async_sleep(delays[[n]]), function(...) attempt(n + 1L))
    })
  }
  attempt(1L)
}

# Wait `secs` without blocking the session.
.async_sleep <- function(secs) {
  promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), secs))
}

# coro::async() builds the async functions below when the package is built;
# the code it generates calls coro and rlang.
#' @importFrom coro async
#' @importFrom rlang %||%
NULL

# Stream one prompt; `on_chunk(text_so_far)` after each chunk. A promise for
# the full text. Non-blocking: httr2's async requests run on the later loop.
# coro runs an async function eagerly up to its first await, so an error
# there would be thrown synchronously - always hand it back as a rejection.
.stream_text <- function(chat, prompt, on_chunk = NULL) {
  tryCatch(.stream_text_async(chat, prompt, on_chunk),
           error = function(e) promises::promise_reject(e))
}

.stream_text_async <- coro::async(function(chat, prompt, on_chunk = NULL) {
  acc <- ""
  for (chunk in await_each(chat$stream_async(prompt))) {
    acc <- paste0(acc, chunk)
    if (!is.null(on_chunk)) on_chunk(acc)
  }
  if (!nzchar(trimws(acc))) stop("The AI service returned an empty reply.", call. = FALSE)
  acc
})
