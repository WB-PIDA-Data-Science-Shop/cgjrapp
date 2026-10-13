# Test doubles for the LLM: no network, ever.

# A stand-in for an ellmer Chat: $stream_async() yields canned chunks. `replies`
# is a list (one element per call, recycled), each a character vector of
# chunks, or a condition to raise instead.
fake_chat_factory <- function(replies) {
  calls <- 0L
  prompts <- list()
  factory <- function(system_prompt = NULL) {
    turns <- list()
    list(
      stream_async = function(prompt) {
        calls <<- calls + 1L
        prompts[[calls]] <<- list(system = system_prompt, user = prompt)
        r <- replies[[(calls - 1L) %% length(replies) + 1L]]
        turns[[length(turns) + 1L]] <<- prompt
        if (inherits(r, "condition")) {
          coro::async_generator(function() { stop(r); yield("") })()
        } else {
          coro::async_generator(function(chunks) for (ch in chunks) yield(ch))(r)
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
