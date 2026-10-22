# A tiny hand-made guidance corpus for the retrieval tests: four sections,
# one passage each, with distinct vocabulary. Not text from any guidance note.
mini_guidance <- function() {
  list(
    passages = data.frame(
      id = c("G1.1-1", "G2.2-1", "G2.1.3-1", "G2.3-1"),
      section = c("1.1", "2.2", "2.1.3", "2.3"),
      title = c("Degree of Integrity", "Public Sector Human Resource Management",
                "Public Procurement", "Digital and Data"),
      kind = "narrative", page = 1:4,
      text = c("Corruption risks, asset declarations and conflict of interest rules.",
               "Merit-based recruitment and pay compression in the civil service.",
               "Teams should check whether tenders are competitive and contracts are published.",
               "Interoperable registries and data protection laws."),
      stringsAsFactors = FALSE),
    refs = data.frame(first_author = character(), year = character(), entry = character()),
    label = "mini test guidance", source = "standin")
}
