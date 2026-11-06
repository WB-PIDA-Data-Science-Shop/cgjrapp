# ---
# Source policy: which external publications the AI Report may cite.
#
# The policy lives in inst/config/source_policy.yml - five tiers (World Bank
# Group, other international organizations, peer-reviewed journals, official
# national sources, other credible research), the institutions and
# working-paper series behind them, a registry of preferred journals, and the
# per-section research limits. load_source_policy() reads and checks it;
# classify_sources() places each candidate publication in a tier and says,
# with a machine-readable reason, whether it may be cited.
#
# Journals are matched by ISSN or OpenAlex id, never by name: titles are not
# unique, and a lookalike title is exactly how an ineligible source would slip
# in. A source the policy cannot place is not eligible (fail closed).
# ---

#' Load the research source policy
#'
#' @param path A YAML policy file. Defaults to `CGJR_SOURCE_POLICY` if set,
#'   else the policy shipped in `inst/config/source_policy.yml`.
#' @return A list:
#'   * `tiers` - data frame `tier`, `name`, `use`;
#'   * `institutions` - data frame `code`, `name`, `tier`, `domains` (list
#'     column of official web domains);
#'   * `series` - working-paper series accepted in tier 5: `id`, `name`,
#'     `publisher`, `domains`;
#'   * `journals` - the journal registry: `id`, `title`, `openalex_id`, `issn`
#'     (list column), `publisher`, `area`, `status` (`"preferred"` or
#'     `"excluded"`);
#'   * `search` - how research is searched for: `wb_document_groups` (World
#'     Bank major document types searched), `languages`, `mai_sites` (the
#'     sites mAI web search is limited to), and `terms` (a named list, report
#'     section key -> search terms);
#'   * `limits` - named list of positive whole numbers (research effort per
#'     report section, network bounds).
#'
#'   Stops, naming the problem, if the file is missing or the policy is
#'   inconsistent: an invalid or shared ISSN, a duplicate journal, an unknown
#'   status or tier, search terms for an unknown report section, a missing
#'   section or limit.
#' @export
load_source_policy <- function(path = Sys.getenv("CGJR_SOURCE_POLICY")) {
  if (!nzchar(path)) path <- system.file("config", "source_policy.yml", package = "cgjrapp")
  if (!file.exists(path)) stop("Source policy file not found: ", path, call. = FALSE)
  y <- yaml::read_yaml(path)
  .stop_if_any(setdiff(.POLICY_SECTIONS, names(y)), "The source policy is missing")
  p <- list(tiers = .records_df(y$tiers),
            institutions = .records_df(y$institutions, list_cols = "domains"),
            series = .records_df(y$series, list_cols = "domains"),
            journals = .records_df(y$journals, list_cols = "issn"),
            search = list(wb_document_groups = as.character(y$search$wb_document_groups),
                          languages = as.character(y$search$languages),
                          mai_sites = as.character(y$search$mai_sites),
                          terms = lapply(y$search$terms, as.character)),
            limits = y$limits)
  .check_policy(p)
  p
}

.POLICY_SECTIONS <- c("tiers", "institutions", "series", "journals", "search", "limits")
.JOURNAL_STATUS <- c("preferred", "excluded")
.POLICY_LIMITS <- c("max_candidates_per_leaf", "max_institutional_per_leaf", "max_peer_reviewed_per_leaf",
                    "passages_per_source", "max_findings_per_source",
                    "max_findings_per_leaf", "request_timeout_s", "max_redirects", "max_pdf_mb",
                    "cache_days", "max_screened_per_leaf", "prefer_recent_years")
# The kinds of publication a source can be; anything else (blogs, news,
# social media, web pages) is never cited as research.
.SOURCE_KINDS <- c("journal_article", "report", "working_paper", "book", "chapter")
# Reasons that leave a source eligible; every other reason excludes it.
.ELIGIBLE_REASONS <- c("ok", "not_peer_reviewed")

# Stops on the first kind of problem in a policy, naming what is wrong, so a
# mistyped identifier never silently matches (or fails to match) a source.
.check_policy <- function(p) {
  j <- p$journals
  bad <- lapply(j$issn, function(x) x[!.issn_valid(x)])
  .stop_if_any(sprintf("%s (%s)", j$id, vapply(bad, toString, ""))[lengths(bad) > 0L],
               "Invalid ISSN in the source policy")
  .stop_if_any(sprintf("%s (%s)", j$id, j$status)[!j$status %in% .JOURNAL_STATUS],
               "Unknown journal status in the source policy")
  inst <- p$institutions
  .stop_if_any(sprintf("%s (%s)", inst$code, inst$tier)[!inst$tier %in% p$tiers$tier],
               "Unknown tier for an institution in the source policy")
  whole <- vapply(p$limits[.POLICY_LIMITS], function(v) {
    is.numeric(v) && length(v) == 1L && v >= 1 && v == round(v)
  }, logical(1))
  .stop_if_any(.POLICY_LIMITS[!whole],
               "Source policy limits must be positive whole numbers; check")
  .stop_if_any(setdiff(names(p$search$terms), .leaf_key(as.data.frame(cgjrdata::cgjr_taxonomy))),
               "Unknown report section in the source policy's search terms")
  .stop_if_any(.duplicates(j$id), "Duplicate journal id in the source policy")
  # an ISSN under two journals would make matching an article ambiguous
  .stop_if_any(.duplicates(unlist(lapply(j$issn, unique))),
               "ISSN listed for more than one journal in the source policy")
}

.duplicates <- function(x) unique(x[duplicated(x)])

.stop_if_any <- function(found, msg) {
  if (length(found)) stop(msg, ": ", toString(found), call. = FALSE)
}

# An ISSN is "NNNN-NNNC": the check digit C makes the weighted sum of all
# eight digits (weights 8 down to 1) a multiple of 11; C = X stands for 10.
.issn_valid <- function(x) {
  ok <- grepl("^[0-9]{4}-[0-9]{3}[0-9X]$", x)
  ok[ok] <- vapply(strsplit(sub("-", "", x[ok]), ""), function(d) {
    d[d == "X"] <- "10"
    sum(as.integer(d) * 8:1) %% 11 == 0
  }, logical(1))
  ok
}

#' Classify candidate sources under the source policy
#'
#' Places each candidate publication in a tier of the policy and decides
#' whether the AI Report may cite it. Fails closed: a candidate the policy
#' cannot place is not eligible.
#'
#' @param candidates A data frame, one row per candidate, with columns:
#'   * `kind` - `"journal_article"`, `"report"`, `"working_paper"`, `"book"`
#'     or `"chapter"`; any other value is excluded, `NA` is missing metadata;
#'   * `institution` - publishing institution, as a policy institution `code`
#'     (e.g. `"WBG"`), or `NA`;
#'   * `series` - working-paper series, as a policy series `id` (e.g.
#'     `"nber_wp"`), or `NA`;
#'   * `issn` - list column, the ISSNs of the journal an article appeared in;
#'   * `openalex_source_id` - that journal's OpenAlex source id, or `NA`;
#'   * `is_retracted` - logical.
#'
#'   Other columns pass through unchanged. A journal's name is never used to
#'   identify it.
#' @param policy Output of [load_source_policy()].
#' @return `candidates` with three columns added:
#'   * `tier` - integer 1-5, `NA` when the policy cannot place the source.
#'     An institution's tier takes precedence over a working-paper series;
#'     articles are tier 3 only in a registry journal.
#'   * `eligible` - logical, `TRUE` when the source may be cited.
#'   * `reason` - why: `"ok"`; `"not_peer_reviewed"` (eligible, but a working
#'     paper or tier 5 series and never to be presented as peer reviewed);
#'     or, for an ineligible source, `"publisher_not_eligible"`,
#'     `"journal_not_in_registry"`, `"journal_excluded"`, `"type_excluded"`,
#'     `"missing_metadata"` or `"retracted"`.
#' @export
classify_sources <- function(candidates, policy = load_source_policy()) {
  inst <- policy$institutions
  tier <- inst$tier[match(candidates$institution, inst$code)]
  tier[is.na(tier) & candidates$series %in% policy$series$id] <- 5L
  article <- candidates$kind %in% "journal_article"
  journal <- .match_journal(candidates$issn, candidates$openalex_source_id, policy$journals)
  tier[article] <- NA_integer_
  tier[article & !is.na(journal)] <- 3L

  # From least to most serious: a later rule overrides an earlier one, so a
  # retracted article reads "retracted" whatever else is true of it.
  reason <- ifelse(candidates$kind %in% "working_paper", "not_peer_reviewed", "ok")
  reason[!article & is.na(tier)] <- "publisher_not_eligible"
  reason[article & is.na(journal)] <- "journal_not_in_registry"
  reason[article & policy$journals$status[journal] %in% "excluded"] <- "journal_excluded"
  reason[!candidates$kind %in% .SOURCE_KINDS] <- "type_excluded"
  reason[is.na(candidates$kind)] <- "missing_metadata"
  reason[candidates$is_retracted %in% TRUE] <- "retracted"

  candidates$tier <- tier
  candidates$eligible <- reason %in% .ELIGIBLE_REASONS
  candidates$reason <- reason
  candidates
}

# Row of `journals` each candidate was published in: the first registry row
# among its ISSNs, else by its OpenAlex source id; NA when neither is listed.
.match_journal <- function(issn, openalex_id, journals) {
  row_of_issn <- stats::setNames(rep(seq_len(nrow(journals)), lengths(journals$issn)),
                                 unlist(journals$issn))
  by_issn <- vapply(issn, function(x) unname(stats::na.omit(row_of_issn[x]))[1], integer(1))
  ifelse(is.na(by_issn), match(openalex_id, journals$openalex_id), by_issn)
}

# YAML records (a list of named lists) -> a data frame, one row per record.
# Fields named in `list_cols` hold several values and become list columns.
.records_df <- function(records, list_cols = character(0)) {
  df <- data.frame(row.names = seq_along(records))
  for (f in unique(unlist(lapply(records, names)))) {
    v <- lapply(records, `[[`, f)
    df[[f]] <- if (f %in% list_cols) lapply(v, as.character) else unlist(lapply(v, `%||%`, NA))
  }
  df
}
