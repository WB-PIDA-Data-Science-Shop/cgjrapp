test_that(".llm_base_url() errors when env var is unset", {
  withr::with_envvar(list(CGJR_LLM_BASE_URL = ""), {
    expect_error(.llm_base_url(), "CGJR_LLM_BASE_URL")
  })
})

test_that(".llm_base_url() returns value when env var is set", {
  withr::with_envvar(list(CGJR_LLM_BASE_URL = "https://example.com"), {
    expect_equal(.llm_base_url(), "https://example.com")
  })
})

test_that(".llm_model() errors when env var is unset", {
  withr::with_envvar(list(CGJR_LLM_MODEL = ""), {
    expect_error(.llm_model(), "CGJR_LLM_MODEL")
  })
})

test_that(".llm_model() returns value when env var is set", {
  withr::with_envvar(list(CGJR_LLM_MODEL = "llama-3.3-70b-versatile"), {
    expect_equal(.llm_model(), "llama-3.3-70b-versatile")
  })
})

test_that(".llm_api_key() errors when env var is unset", {
  withr::with_envvar(list(CGJR_LLM_API_KEY = ""), {
    expect_error(.llm_api_key(), "CGJR_LLM_API_KEY")
  })
})

test_that(".llm_api_key() returns value when env var is set", {
  withr::with_envvar(list(CGJR_LLM_API_KEY = "test-key-123"), {
    expect_equal(.llm_api_key(), "test-key-123")
  })
})

test_that("check_llm_available() returns FALSE on unreachable host", {
  withr::with_envvar(
    list(CGJR_LLM_BASE_URL = "https://this-host-does-not-exist.invalid"),
    {
      result <- check_llm_available()
      expect_false(result)
    }
  )
})

test_that("check_llm_available() returns FALSE when env var is unset", {
  withr::with_envvar(list(CGJR_LLM_BASE_URL = ""), {
    # .llm_base_url() will abort, which check_llm_available() should handle
    result <- check_llm_available()
    expect_false(result)
  })
})
