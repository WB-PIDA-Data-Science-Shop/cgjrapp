# utils_llm.R
# LLM connectivity helpers for the AI-generated report feature.
# Uses the ellmer package for streaming completions and httr2 for connectivity
# checks. Provider config is read from environment variables at call time.

# ── Environment variable helpers ──────────────────────────────────────────────

.llm_base_url <- function() {
  url <- Sys.getenv("CGJR_LLM_BASE_URL", unset = "")
  if (nchar(url) == 0L) {
    cli::cli_abort(
      "Environment variable {.envvar CGJR_LLM_BASE_URL} is not set.",
      call = NULL
    )
  }
  url
}

.llm_model <- function() {
  model <- Sys.getenv("CGJR_LLM_MODEL", unset = "")
  if (nchar(model) == 0L) {
    cli::cli_abort(
      "Environment variable {.envvar CGJR_LLM_MODEL} is not set.",
      call = NULL
    )
  }
  model
}

.llm_api_key <- function() {
  key <- Sys.getenv("CGJR_LLM_API_KEY", unset = "")
  if (nchar(key) == 0L) {
    cli::cli_abort(
      "Environment variable {.envvar CGJR_LLM_API_KEY} is not set.",
      call = NULL
    )
  }
  key
}

# ── Availability check ────────────────────────────────────────────────────────

#' Check whether the configured LLM endpoint is reachable
#'
#' Attempts a lightweight HTTP GET to the LLM base URL. Returns `TRUE` if the
#' server responds (any status code) and `FALSE` on connection error. This is
#' used in the report module to give a graceful degradation message when the
#' API is not available.
#'
#' @return Logical scalar.
#' @export
check_llm_available <- function() {
  tryCatch({
    resp <- httr2::request(.llm_base_url()) |>
      httr2::req_timeout(5L) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
}

# ── Streaming completion ───────────────────────────────────────────────────────

#' Stream an LLM completion and update a Shiny reactive value
#'
#' Sends `prompt` to the configured LLM endpoint using `ellmer::chat_openai()`
#' (which is OpenAI-API-compatible and therefore works with Groq). Each token
#' chunk is appended to `reactive_val` so the UI can update progressively via
#' `shiny::invalidateLater()` or reactive binding. On completion, the optional
#' `on_complete` callback is called with the full response text.
#'
#' @param prompt Character scalar -- the full user-side prompt (system prompt
#'   should be baked in by the caller or passed via ellmer's `system` arg).
#' @param system_prompt Character scalar -- system prompt sent to the model.
#' @param reactive_val A `shiny::reactiveVal` that accumulates streamed tokens.
#' @param on_complete Optional zero-argument function called after streaming
#'   completes successfully.
#'
#' @return Called for side effects; returns `NULL` invisibly.
#' @export
stream_llm_response <- function(prompt, system_prompt,
                                reactive_val, on_complete = NULL) {
  base_url  <- .llm_base_url()
  model     <- .llm_model()
  api_key   <- .llm_api_key()

  chat <- ellmer::chat_openai(
    base_url      = base_url,
    model         = model,
    api_key       = api_key,
    system_prompt = system_prompt,
    service_tier  = "default"
  )

  # Stream tokens into the reactive value
  stream <- chat$stream(prompt)

  coro::loop(for (chunk in stream) {
    current <- reactive_val()
    reactive_val(paste0(current, chunk))
  })

  if (!is.null(on_complete)) on_complete()
  invisible(NULL)
}
