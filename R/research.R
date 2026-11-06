# ---
# Research for a report: one section at a time, then one pack for all.
#
# .research_section() runs a section's whole pipeline - discovery and
# relevance screening (discover.R, screen.R), reading the selected sources
# (source_text.R), findings with checked quotes (findings.R). The app runs it
# in a background R process (mirai), one section after another, so the
# session stays responsive; .research_pack_of() then numbers the findings of
# all sections as citable ids for the report (findings.R).
# ---

# One section's research. `ask(system, prompt)` returns an LLM's reply
# (.llm_ask() in the app); the research cache (.research_cache()) means a
# section researched before costs almost nothing. Returns a list: `leaf`,
# `sources` (discovered, ranked and read), `findings` and `dropped` (as
# .extract_findings() gives them), `providers` (each search's status),
# `screening` (relevance screening's status) and `leads` (mAI search results
# and what became of them).
.research_section <- function(leaf, evidence, policy, ask = .llm_ask,
                              cache = .research_cache(policy), resolve = .resolve_host) {
  country <- if (evidence$meta$base_unit %in% .country_codes()) evidence$meta$base_name
  d <- .discover_sources(leaf, evidence, policy, cache, ask = ask)
  r <- .read_sources(d$sources, leaf, country, policy, cache, resolve)
  f <- .extract_findings(r$sources, r$passages, leaf, country, policy, ask, cache)
  list(leaf = leaf, sources = r$sources, findings = f$findings, dropped = f$dropped,
       providers = d$providers, screening = d$screening, leads = d$leads)
}

# The configured LLM asked one question - the `ask` the research steps use.
.llm_ask <- function(system, prompt) llm_chat(system)$chat(prompt, echo = "none")

# The research of several sections (a named list of .research_section()
# results; a section whose research failed is list(error = ...)) -> one
# research pack (.research_pack()): a source found for two sections is one
# source, numbered once. NULL when there is no checked finding to cite (none
# survived the checks, or no section was researched): the draft is then
# written from the dashboard and guidance alone.
.research_pack_of <- function(results) {
  ok <- Filter(function(r) is.null(r$error), results)
  findings <- do.call(rbind, lapply(ok, `[[`, "findings"))
  if (NROW(findings) == 0L) return(NULL)
  sources <- do.call(rbind, lapply(ok, `[[`, "sources"))
  rownames(sources) <- NULL
  rownames(findings) <- NULL
  .research_pack(sources, findings)
}

# The report sections a scope covers ("chapter", "cluster:<key>",
# "leaf:<key>"), in the report's order - the ones researched.
.report_leaves <- function(evidence, scope) {
  sc <- .parse_scope(scope)
  if (sc$type == "leaf") return(sc$key)
  leaves <- .scope_leaves(evidence)
  # the evidence may cover more than the scope (built for the whole chapter)
  if (sc$type == "cluster") leaves <- leaves[leaves$cluster == sc$cluster, ]
  leaves$leaf
}

# The source policy for a research depth chosen in the app: "10" sources per
# section as the policy sets them (6 institutional + 4 peer-reviewed), or
# "5", faster (3 + 2).
.research_policy <- function(depth = "10") {
  p <- load_source_policy()
  if (identical(depth, "5")) {
    p$limits$max_institutional_per_leaf <- 3L
    p$limits$max_peer_reviewed_per_leaf <- 2L
  }
  p
}

# Call one of the package's functions (named by `f`) with `...` in a
# background R process (mirai), so the app stays responsive. Returns the mirai.
# The process loads the installed package - or, when the app runs from source
# (devtools::load_all(), as in development), that same source.
.in_background <- function(f, ...) {
  dev <- isNamespaceLoaded("pkgload") && pkgload::is_dev_package("cgjrapp")
  src <- if (dev) getNamespaceInfo("cgjrapp", "path")  # the source directory
  mirai::mirai({
    if (!is.null(src)) pkgload::load_all(src, quiet = TRUE, helpers = FALSE)
    do.call(utils::getFromNamespace(f, "cgjrapp"), args)
  }, f = f, args = list(...), src = src)
}

# A research pack without the sources whose keys are `keys` (excluded by the
# user) or their findings. The other ids stay as they were, so text already
# written still cites the right findings.
.drop_sources <- function(pack, keys) {
  pack$sources <- pack$sources[!pack$sources$key %in% keys, , drop = FALSE]
  pack$findings <- pack$findings[!pack$findings$key %in% keys, , drop = FALSE]
  pack
}
