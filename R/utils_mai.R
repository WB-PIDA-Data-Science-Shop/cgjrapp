# utils_mai.R - WBG mAI Factory as the LLM provider (CGJR_LLM_PROVIDER = "mai").
#
# Azure OpenAI behind the Bank's API gateway, on the v2 deployment route,
# authenticated with a desktop access token. Desktop access is for proof of
# concept and local development only: the deployed app will need application
# access (an ACN), which this file does not implement yet.
#
#   CGJR_MAI_ENV            - DEV (default), QA or PROD
#   CGJR_MAI_PYTHON_PROJECT - the uv project with itsai-platform installed;
#                             inst/mai/get_token.py runs there for the token
#   CGJR_MAI_UV             - the uv command (default: uv)
#   CGJR_MAI_TEAM           - x-team-name, which usage is billed to
#                             (default: govcgjrapp)
#   CGJR_LLM_MODEL          - deployment name (default: gpt-4.1-mini)

.MAI_HOSTS <- c(DEV = "https://azapimdev.worldbank.org", QA = "https://azapimqa.worldbank.org",
                PROD = "https://azapim.worldbank.org")

.mai_env <- function() {
  env <- toupper(Sys.getenv("CGJR_MAI_ENV", "DEV"))
  if (!env %in% names(.MAI_HOSTS)) {
    stop("CGJR_MAI_ENV must be DEV, QA or PROD (it is '", env, "').", call. = FALSE)
  }
  env
}

.mai_host <- function() .MAI_HOSTS[[.mai_env()]]

# Without an explicit cap the gateway stops replies at 1,024 tokens - too
# short for a report section.
.MAI_MAX_TOKENS <- 8000L

# gpt-5 and o-series models think before they answer; on a high effort the
# thinking can use the whole output budget and the reply comes back empty.
# Other models reject the field.
.is_reasoning_model <- function(model) grepl("^(gpt-5|o\\d)", model)

.mai_chat <- function(system_prompt = NULL) {
  model <- Sys.getenv("CGJR_LLM_MODEL", "gpt-4.1-mini")
  ellmer::chat_azure_openai(
    endpoint = paste0(.mai_host(), "/conversationalai/v2"),
    model = model, api_version = "2025-01-01-preview", system_prompt = system_prompt,
    params = ellmer::params(max_tokens = .MAI_MAX_TOKENS),
    api_args = if (.is_reasoning_model(model)) list(reasoning_effort = "low") else list(),
    credentials = function() list(Authorization = paste("Bearer", .mai_token())),
    api_headers = .mai_headers(), echo = "none")
}

#' Embed texts with WBG mAI
#'
#' Calls the v2 embeddings route of `CGJR_MAI_ENV` (`text-embedding-3-small`)
#' with the desktop token. Used for the chat's question and, by
#' `data-raw/guidance_embeddings.R`, for the guidance passages.
#'
#' @param texts Character vector.
#' @return A numeric matrix, one row (1,536 values) per text, in input order.
#' @keywords internal
embed_texts <- function(texts) {
  resp <- httr2::request(paste0(.mai_host(), "/conversationalai/v2/openai/deployments/",
                                "text-embedding-3-small/embeddings")) |>
    httr2::req_url_query(`api-version` = "2024-10-01-preview") |>
    httr2::req_auth_bearer_token(.mai_token()) |>
    httr2::req_headers(!!!.mai_headers()) |>
    httr2::req_body_json(list(input = as.list(texts))) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60) |>
    httr2::req_perform()
  data <- httr2::resp_body_json(resp)$data
  idx <- vapply(data, function(d) d$index, integer(1))
  vecs <- lapply(data, function(d) as.numeric(unlist(d$embedding)))
  do.call(rbind, vecs[order(idx)])
}

# The gateway's usage-tracking headers: interactive use, billed to the team.
.mai_headers <- function() {
  c(`x-source-type` = "interactive", `x-team-name` = Sys.getenv("CGJR_MAI_TEAM", "govcgjrapp"))
}

# check_llm_available() for mAI: a token can be had, and the gateway's health
# endpoint accepts it. The reason a token can't be had goes to the log.
.mai_check <- function() {
  tok <- tryCatch(list(host = .mai_host(), token = .mai_token()), error = function(e) {
    message("[cgjrapp] mAI: ", conditionMessage(e))
    NULL
  })
  if (is.null(tok)) return("auth")
  resp <- tryCatch(
    httr2::request(paste0(tok$host, "/conversationalai/platform/health/")) |>
      httr2::req_auth_bearer_token(tok$token) |>
      httr2::req_headers(!!!.mai_headers()) |>
      httr2::req_timeout(5) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return("unreachable")
  if (httr2::resp_status(resp) %in% c(401L, 403L)) "auth" else "ok"
}

# The desktop token for the configured environment, cached for the R session
# until 5 minutes before it expires: ellmer asks for credentials twice per
# request, and each fetch runs a Python process.
.mai_cache <- new.env(parent = emptyenv())

.mai_token <- function() {
  env <- .mai_env()
  if (!identical(.mai_cache$env, env) || as.numeric(Sys.time()) > .mai_cache$exp - 300) {
    tok <- .mai_fetch_token(env)
    .mai_cache$exp <- .jwt_exp(tok)
    .mai_cache$env <- env
    .mai_cache$value <- tok
  }
  .mai_cache$value
}

# A 401 under mAI: the cached token was revoked or expired early, which a
# fresh one fixes (for the other providers a 401 is a wrong key).
.is_mai_token_rejected <- function(cnd) {
  .llm_provider() == "mai" &&
    (inherits(cnd, "httr2_http_401") || grepl("HTTP 401", .plain_message(cnd)))
}

.mai_problem_message <- function(status) {
  switch(status,
    unreachable = paste("Cannot reach WBG mAI. Check that you are on the Bank VPN (US East)",
                        "and that CGJR_MAI_ENV is right."),
    auth = paste("WBG mAI did not accept the sign-in. Check your mAI desktop access:",
                 "CGJR_MAI_PYTHON_PROJECT, and that DesktopToken works there (see the R log)."),
    NULL)
}

# The `exp` claim (unix seconds) of a JWT; 0 when it can't be read, so the
# token is fetched again next time rather than trusted indefinitely.
.jwt_exp <- function(token) {
  part <- strsplit(token, ".", fixed = TRUE)[[1]][2]
  tryCatch({
    part <- chartr("-_", "+/", part)
    part <- paste0(part, strrep("=", (4 - nchar(part) %% 4) %% 4))
    as.numeric(jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(part)))$exp)
  }, error = function(e) 0)
}

.mai_clear_token <- function() {
  rm(list = ls(.mai_cache), envir = .mai_cache)
  invisible(NULL)
}

# Run inst/mai/get_token.py (itsai-platform's DesktopToken) for `env` in the
# uv project named by CGJR_MAI_PYTHON_PROJECT. uv must not download a Python:
# Bank machines block programs under AppData.
.mai_fetch_token <- function(env) {
  project <- Sys.getenv("CGJR_MAI_PYTHON_PROJECT", "")
  if (!nzchar(project)) {
    stop("CGJR_MAI_PYTHON_PROJECT is not set. Point it at the uv project where ",
         "itsai-platform is installed (mAI desktop access).", call. = FALSE)
  }
  old <- Sys.getenv("UV_PYTHON_DOWNLOADS", NA)
  Sys.setenv(UV_PYTHON_DOWNLOADS = "never")
  on.exit(if (is.na(old)) Sys.unsetenv("UV_PYTHON_DOWNLOADS") else Sys.setenv(UV_PYTHON_DOWNLOADS = old))
  script <- system.file("mai", "get_token.py", package = "cgjrapp")
  # a missing uv is an error on Windows, a non-zero status elsewhere
  out <- tryCatch(suppressWarnings(system2(
    Sys.getenv("CGJR_MAI_UV", "uv"),
    c("run", "--quiet", "--project", shQuote(project), "python", shQuote(script), env),
    stdout = TRUE, stderr = FALSE)), error = function(e) NULL)
  tok <- paste(out, collapse = "")
  if (is.null(out) || !is.null(attr(out, "status")) || !nzchar(tok)) {
    stop("Could not get a mAI desktop token. Check that you are on the Bank VPN and that ",
         "`uv run python -c \"from itsai.platform.authentication import DesktopToken; ",
         "DesktopToken().get_token()\"` works in ", project, ".", call. = FALSE)
  }
  tok
}
