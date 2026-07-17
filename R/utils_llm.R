# utils_llm.R — Provider-agnostic LLM interface
#
# Functions:
#   stream_llm_response()  — stream LLM tokens into a Shiny reactiveVal
#   check_llm_available()  — test whether the configured endpoint is reachable
#
# Configuration is entirely via environment variables. Set these in .Renviron
# (local development) or the Posit Connect environment panel (production):
#
#   CGJR_LLM_BASE_URL  — API base URL  (default: http://localhost:11434/v1)
#   CGJR_LLM_MODEL     — Model name    (default: llama3.2)
#   CGJR_LLM_API_KEY   — Auth key      (default: ollama)
#
# Provider routing (automatic, based on CGJR_LLM_BASE_URL):
#   - api.groq.com       → ellmer::chat_groq()         (avoids service_tier field)
#   - everything else    → ellmer::chat_openai_compatible() (generic, no OAI extras)
#
# Switching between Ollama, Groq, or WBG mAI requires only updating these
# environment variables — zero code changes.


#' Stream an LLM response into a Shiny reactive value
#'
#' @title Stream LLM Response
#'
#' @description
#' Sends a prompt to an OpenAI-compatible LLM endpoint via `ellmer` and streams
#' the response token-by-token into a `shiny::reactiveVal()`. This provides
#' progressive rendering in the UI rather than a long wait for the full response.
#'
#' The LLM provider and model are resolved from environment variables at call
#' time, so no code changes are needed to switch providers.
#'
#' Internally uses `coro::loop()` and `coro::yield()` to iterate over the
#' generator returned by `ellmer`'s `$stream()` method.
#'
#' @param prompt A named list with character elements `system` and `user`, as
#'   returned by `build_cgjr_prompt()`.
#' @param reactive_val A `shiny::reactiveVal` that will be updated with the
#'   accumulated response text after each token. The value always contains the
#'   full text received so far.
#' @param on_complete Optional function called once streaming finishes. Receives
#'   the complete response text as its only argument. Default is `NULL`.
#'
#' @return Invisibly returns the full response text (character scalar) after
#'   streaming completes.
#'
#' @examples
#' \dontrun{
#' # In a Shiny server context:
#' report_text <- shiny::reactiveVal("")
#'
#' stream_llm_response(
#'   prompt       = build_cgjr_prompt("GHA", "Ghana", scores_tbl, c(2013L, 2024L)),
#'   reactive_val = report_text,
#'   on_complete  = function(text) message("Streaming complete: ", nchar(text), " chars")
#' )
#' }
#'
#' @importFrom ellmer chat_groq chat_openai_compatible
#' @importFrom coro loop
#'
#' @keywords internal
stream_llm_response <- function(prompt, reactive_val, on_complete = NULL) {

  base_url <- Sys.getenv("CGJR_LLM_BASE_URL", unset = "http://localhost:11434/v1")
  model    <- Sys.getenv("CGJR_LLM_MODEL",    unset = "llama3.2")
  api_key  <- Sys.getenv("CGJR_LLM_API_KEY",  unset = "ollama")

  # Route to provider-specific constructor to avoid sending fields that some
  # providers (e.g. Groq free tier) reject. chat_groq() omits service_tier;
  # chat_openai_compatible() is safe for Ollama and generic OpenAI-compat APIs.
  is_groq <- grepl("groq\\.com", base_url, ignore.case = TRUE)

  chat <- if (is_groq) {
    ellmer::chat_groq(
      credentials   = function() api_key,
      model         = model,
      system_prompt = prompt$system,
      echo          = "none"
    )
  } else {
    ellmer::chat_openai_compatible(
      base_url      = base_url,
      model         = model,
      credentials   = function() api_key,
      system_prompt = prompt$system,
      echo          = "none"
    )
  }

  # $stream() returns a coro_generator_instance.
  # coro::loop() takes a for-expression as its argument — the correct
  # consumer pattern for iterating coro generators outside generator bodies.
  gen <- chat$stream(prompt$user)

  accumulated <- ""
  coro::loop(for (token in gen) {
    accumulated <- paste0(accumulated, token)
    reactive_val(accumulated)
  })

  if (!is.null(on_complete)) {
    on_complete(accumulated)
  }

  invisible(accumulated)
}


#' Check whether the configured LLM endpoint is reachable
#'
#' @title Check LLM Availability
#'
#' @description
#' Performs a lightweight HTTP GET to the configured LLM base URL with a
#' 5-second timeout. Used before attempting report generation to surface a
#' clear, user-friendly error message if the endpoint is unreachable, rather
#' than silently failing during streaming.
#'
#' Returns `FALSE` for any error (connection refused, timeout, DNS failure,
#' HTTP error response), and `TRUE` only when the server responds at all —
#' even a 404 counts as "reachable" because it indicates the server is running.
#'
#' @return Logical scalar. `TRUE` if the endpoint responds within 5 seconds;
#'   `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' if (!check_llm_available()) {
#'   stop("LLM endpoint is not reachable. Check CGJR_LLM_BASE_URL.")
#' }
#' }
#'
#' @importFrom httr2 request req_timeout req_error req_perform
#'
#' @keywords internal
check_llm_available <- function() {
  base_url <- Sys.getenv("CGJR_LLM_BASE_URL", unset = "http://localhost:11434/v1")

  tryCatch({
    httr2::request(base_url) |>
      httr2::req_timeout(5) |>
      # Treat any HTTP status as "reachable" — we just want to know the server
      # is up; a 404 from Ollama is fine
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
}