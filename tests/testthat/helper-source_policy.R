# Policy files for tests: the bundled policy, edited by `edit` (a function of
# the parsed YAML list), written to a temporary file removed after the test.
local_policy_file <- function(edit = identity, env = parent.frame()) {
  y <- yaml::read_yaml(system.file("config", "source_policy.yml", package = "cgjrapp"))
  path <- withr::local_tempfile(fileext = ".yml", .local_envir = env)
  yaml::write_yaml(edit(y), path)
  path
}

# Candidate sources as classify_sources() takes them; arguments recycle, and
# `issn` is a list (one character vector per candidate).
candidates <- function(kind, institution = NA, series = NA, issn = NULL,
                       openalex_source_id = NA, is_retracted = FALSE) {
  n <- length(kind)
  df <- data.frame(kind, institution = rep_len(institution, n), series = rep_len(series, n),
                   openalex_source_id = rep_len(openalex_source_id, n),
                   is_retracted = rep_len(is_retracted, n))
  df$issn <- issn %||% rep(list(character(0)), n)
  df
}

# The classification of one candidate, as a list(tier, eligible, reason).
classify_one <- function(...) {
  as.list(classify_sources(candidates(...))[1, c("tier", "eligible", "reason")])
}
