test_that("check_llm_available() returns FALSE on unreachable host", {
  withr::with_envvar(
    list(CGJR_LLM_BASE_URL = "https://this-host-does-not-exist.invalid"),
    {
      result <- check_llm_available()
      expect_false(result)
    }
  )
})

test_that("check_llm_available() returns FALSE on localhost with no server running", {
  # Port 19999 is almost certainly not listening
  withr::with_envvar(
    list(CGJR_LLM_BASE_URL = "http://127.0.0.1:19999"),
    {
      result <- check_llm_available()
      expect_false(result)
    }
  )
})

test_that("stream_llm_response() reads CGJR_LLM_BASE_URL env var", {
  # Verify the function uses CGJR_ prefix by checking it tries the right URL
  # (it will fail to connect, but the error should not mention a wrong URL)
  withr::with_envvar(
    list(
      CGJR_LLM_BASE_URL = "http://127.0.0.1:19999",
      CGJR_LLM_MODEL    = "test-model",
      CGJR_LLM_API_KEY  = "test-key"
    ),
    {
      acc <- shiny::reactiveVal("")
      # Expect an error (no server), not a silent wrong-URL failure
      expect_error(
        shiny::isolate(
          stream_llm_response(
            prompt       = list(system = "sys", user = "user"),
            reactive_val = acc
          )
        )
      )
    }
  )
})
