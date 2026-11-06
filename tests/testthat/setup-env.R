# Test runs ignore the developer's own CGJR_* settings.
#
# A developer's .Renviron may point the app at WBG mAI (CGJR_LLM_PROVIDER=mai)
# and at the Official Use Only guidance (CGJR_GUIDANCE_SOURCE=real). Left in
# place, the LLM tests would fetch a real desktop token, call mAI and print
# the token in their failure messages. So every CGJR_* variable is unset for
# the whole run, and each test sets what it needs with withr::local_envvar().
# The developer's values are kept in .developer_env for the opt-in live test
# (test-llm-live.R). CGJR_LLM_LIVE_TEST stays set: it is that test's switch.

# Filtered from the full environment: Sys.getenv(character(0)) would return
# every variable, not none.
.developer_env <- local({
  env <- Sys.getenv()
  keep <- startsWith(names(env), "CGJR_") & names(env) != "CGJR_LLM_LIVE_TEST"
  as.list(unclass(env)[keep])
})

# withr fails restoring an empty list at teardown, so skip when nothing is set
if (length(.developer_env)) {
  withr::local_envvar(lapply(.developer_env, function(x) NA),
                      .local_envir = testthat::teardown_env())
}
