# Test doubles for the LLM: no network, ever.

# A stand-in for an ellmer Chat: $stream_async() yields canned chunks. `replies`
# is a list (one element per call, recycled), each a character vector of
# chunks, or a condition to raise instead. Like ellmer, a finished reply adds
# a user and an assistant turn to the history.
fake_chat_factory <- function(replies) {
  calls <- 0L
  prompts <- list()
  factory <- function(system_prompt = NULL) {
    turns <- list()
    add_turns <- function(prompt, reply) {
      turns[[length(turns) + 1L]] <<- ellmer::UserTurn(list(ellmer::ContentText(prompt)))
      turns[[length(turns) + 1L]] <<- ellmer::AssistantTurn(list(ellmer::ContentText(reply)))
    }
    list(
      stream_async = function(prompt) {
        calls <<- calls + 1L
        prompts[[calls]] <<- list(system = system_prompt, user = prompt)
        r <- replies[[(calls - 1L) %% length(replies) + 1L]]
        if (inherits(r, "condition")) {
          coro::async_generator(function() { stop(r); yield("") })()
        } else {
          # the turns go in just before the last chunk: code after the loop
          # would hand its value on as an extra chunk
          coro::async_generator(function(chunks) {
            for (i in seq_along(chunks)) {
              if (i == length(chunks)) add_turns(prompt, paste(chunks, collapse = ""))
              yield(chunks[[i]])
            }
          })(r)
        }
      },
      get_turns = function() turns,
      set_turns = function(x) turns <<- x,
      set_system_prompt = function(x) system_prompt <<- x
    )
  }
  attr(factory, "calls") <- function() calls
  attr(factory, "prompts") <- function() prompts
  factory
}

# A token shaped like a JWT whose payload carries `exp` (unix seconds) - all
# the mAI layer reads from it. Not signed; never sent anywhere.
fake_jwt <- function(exp = as.numeric(Sys.time()) + 3600, sub = "test") {
  b64url <- function(x) gsub("=+$", "", chartr("+/", "-_", jsonlite::base64_enc(x)))
  paste(b64url('{"alg":"none"}'),
        b64url(sprintf('{"exp":%d,"sub":"%s"}', as.integer(exp), sub)), "sig", sep = ".")
}

# Switch the LLM layer to mAI for one test, with the desktop token helper
# (an external Python process) replaced by `fetch(env)`. Returns a function giving
# the number of token fetches so far.
local_mai <- function(..., fetch = function(env) fake_jwt(), .env = parent.frame()) {
  vars <- utils::modifyList(
    list(CGJR_LLM_PROVIDER = "mai", CGJR_MAI_PYTHON_PROJECT = "C:/fake/mai-poc",
         CGJR_LLM_MODEL = NA, CGJR_MAI_ENV = NA, CGJR_MAI_TEAM = NA),
    list(...))
  withr::local_envvar(unlist(vars), .local_envir = .env)
  n <- 0L
  testthat::local_mocked_bindings(.mai_fetch_token = function(env) { n <<- n + 1L; fetch(env) },
                                  .env = .env)
  cgjrapp:::.mai_clear_token()
  withr::defer(cgjrapp:::.mai_clear_token(), envir = .env)
  function() n
}

# A stand-in for the `uv` command: prints "tok.<UV_PYTHON_DOWNLOADS>.<env>",
# where <env> is the last argument .mai_fetch_token() passes (the 7th).
local_fake_uv <- function(.env = parent.frame()) {
  if (.Platform$OS.type == "windows") {
    f <- withr::local_tempfile(fileext = ".bat", .local_envir = .env)
    writeLines(c("@echo off", "echo tok.%UV_PYTHON_DOWNLOADS%.%7"), f)
  } else {
    f <- withr::local_tempfile(.local_envir = .env)
    writeLines(c("#!/bin/sh", 'echo "tok.$UV_PYTHON_DOWNLOADS.$7"'), f)
    Sys.chmod(f, "755")
  }
  f
}

# The first request a chat sends, answered with a 400 so nothing else happens.
capture_request <- function(chat, prompt = "hi") {
  seen <- NULL
  httr2::with_mocked_responses(
    function(req) { seen <<- seen %||% req; httr2::response(400) },
    try(chat$chat(prompt), silent = TRUE))
  seen
}

# Run the later loop until a promise settles; return its value or rethrow.
wait_for <- function(p, timeout = 10) {
  state <- new.env(); state$done <- FALSE
  promises::then(p,
    onFulfilled = function(v) { state$value <- v; state$done <- TRUE },
    onRejected = function(e) { state$error <- e; state$done <- TRUE })
  t0 <- Sys.time()
  while (!state$done && difftime(Sys.time(), t0, units = "secs") < timeout) later::run_now(0.05)
  if (!state$done) stop("promise did not settle")
  if (!is.null(state$error)) stop(state$error)
  state$value
}
