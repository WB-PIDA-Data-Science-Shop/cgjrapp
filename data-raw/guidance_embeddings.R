# Build the passage embeddings the AI Chat's retrieval uses (R/retrieve.R):
#
#   inst/guidance_standin/guidance_standin_embeddings.rds  - the stand-in (committed)
#   inst/docs/guidance_passages_embeddings.rds             - the real note: Official
#     Use Only derived data, so it lives in the gitignored inst/docs/ next to the
#     passages, and is built only when those exist
#
# Each file carries a fingerprint of the passages it was computed for;
# load_guidance() ignores it once the passages change, so re-run this after
# rebuilding guidance_passages.rds (data-raw/guidance_passages.R).
#
# Needs WBG mAI desktop access (CGJR_LLM_PROVIDER=mai, CGJR_MAI_PYTHON_PROJECT;
# see R/utils_mai.R). The real note's ~30k tokens are more than mAI DEV takes
# in a minute (25k), so passages go in batches of 20 with a pause between.
#
# Run from the package root:  source("data-raw/guidance_embeddings.R")

pkgload::load_all(quiet = TRUE)
stopifnot("Set CGJR_LLM_PROVIDER=mai (embeddings come from WBG mAI)" = .llm_provider() == "mai")

build_embeddings <- function(source, out, batch = 20L, pause = 20) {
  p <- load_guidance(source)$passages
  texts <- paste(p$title, p$text, sep = ". ")
  batches <- split(seq_along(texts), ceiling(seq_along(texts) / batch))
  vecs <- lapply(seq_along(batches), function(i) {
    if (i > 1L) Sys.sleep(pause)
    message(source, ": batch ", i, " of ", length(batches))
    embed_texts(texts[batches[[i]]])
  })
  v <- do.call(rbind, vecs)
  rownames(v) <- p$id
  saveRDS(list(model = "text-embedding-3-small", fingerprint = guidance_fingerprint(p),
               vectors = v, built = Sys.Date()), out)
  message(source, ": ", nrow(v), " passages x ", ncol(v), " dimensions -> ", out)
}

build_embeddings("standin", "inst/guidance_standin/guidance_standin_embeddings.rds")
if (file.exists("inst/docs/guidance_passages.rds")) {
  build_embeddings("real", "inst/docs/guidance_passages_embeddings.rds")
}
