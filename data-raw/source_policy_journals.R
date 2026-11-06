# Look up the preferred journals of the source policy in OpenAlex, so that
# their identifiers in inst/config/source_policy.yml come from a scholarly
# index rather than being typed in by hand:
#
#   openalex_id - the OpenAlex source id (S...), the journal's stable record
#   issn        - every ISSN OpenAlex lists for it (print and electronic)
#
# The script prints one YAML block per journal for review; paste the checked
# blocks under `journals:` in the policy file. A title that does not match
# exactly, or a source that is not a journal, is printed as a comment for a
# person to resolve - it never becomes an entry on its own.
#
# Sends only journal titles to the public OpenAlex API (no key needed; set
# CGJR_OPENALEX_KEY for a larger daily budget).
#
# Run from the package root:  source("data-raw/source_policy_journals.R")

journals <- data.frame(
  id = c("jde", "world_dev", "jeg", "edcc",
         "aer", "qje", "jpe", "restud", "ecta", "aej_applied", "jpube", "restat",
         "jleo", "governance", "par", "jpart",
         "wber", "wbro"),
  title = c("Journal of Development Economics", "World Development",
            "Journal of Economic Growth", "Economic Development and Cultural Change",
            "American Economic Review", "Quarterly Journal of Economics",
            "Journal of Political Economy", "Review of Economic Studies", "Econometrica",
            "American Economic Journal: Applied Economics", "Journal of Public Economics",
            "Review of Economics and Statistics",
            "Journal of Law, Economics, and Organization", "Governance",
            "Public Administration Review",
            "Journal of Public Administration Research and Theory",
            "World Bank Economic Review", "World Bank Research Observer"),
  area = c(rep("development_economics", 4), rep("economics_public_policy", 8),
           rep("institutions_governance", 4), rep("development_economics", 2)))

# "The Quarterly Journal..." and "Quarterly Journal..." name the same journal;
# case, punctuation and a leading article are ignored when comparing titles.
norm <- function(x) gsub("^the ", "", gsub("[^a-z0-9]+", " ", tolower(trimws(x))))

lookup <- function(title) {
  key <- Sys.getenv("CGJR_OPENALEX_KEY")
  req <- httr2::request("https://api.openalex.org/sources")
  # an unset key is left out (NULL), not sent as "NA", which OpenAlex rejects
  req <- httr2::req_url_query(req, search = title, `per-page` = 5,
                              api_key = if (nzchar(key)) key)
  req <- httr2::req_timeout(req, 20)
  Sys.sleep(0.2)  # stay well under the keyless 10 requests a second
  httr2::resp_body_json(httr2::req_perform(req))$results
}

for (i in seq_len(nrow(journals))) {
  j <- journals[i, ]
  hits <- Filter(function(s) identical(norm(s$display_name), norm(j$title)) &&
                   identical(s$type, "journal"), lookup(j$title))
  if (length(hits) != 1L) {
    cat(sprintf("# %s: %d exact journal matches for '%s' - resolve by hand\n\n",
                j$id, length(hits), j$title))
    next
  }
  s <- hits[[1]]
  cat(sprintf(paste0("  - id: %s\n    title: \"%s\"\n    openalex_id: %s\n",
                     "    issn: [%s]\n    publisher: \"%s\"\n    area: %s\n",
                     "    status: preferred\n\n"),
              j$id, j$title, sub("^https://openalex.org/", "", s$id),
              paste(unlist(s$issn), collapse = ", "),
              s$host_organization_name %||% "", j$area))
}
