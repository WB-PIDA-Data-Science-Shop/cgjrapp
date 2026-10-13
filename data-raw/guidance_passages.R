# Build inst/docs/guidance_passages.rds from the CGJR guidance note PDF.
#
# The note is Official Use Only: the PDF and the rds both live in inst/docs/,
# which is gitignored (and .Rbuildignored). The app only reads the rds when
# CGJR_GUIDANCE_SOURCE = "real" -- set that only with an approved LLM
# endpoint (WBG mAI), never with a free/public provider.
#
# Run from the package root:  source("data-raw/guidance_passages.R")
# Needs pdftools (Suggests).

pkgload::load_all(quiet = TRUE)

pdf <- list.files("inst/docs", pattern = "\\.pdf$", full.names = TRUE)
stopifnot("Put the guidance note PDF in inst/docs/" = length(pdf) == 1L)

g <- split_guidance(pdftools::pdf_text(pdf))

# Every section the report needs must be present.
needed <- unique(unlist(.GUIDANCE_MAP, use.names = FALSE))
missing <- setdiff(needed, unique(g$passages$section))
if (length(missing)) warning("Sections with no passages: ", paste(missing, collapse = ", "))

saveRDS(g, file.path("inst/docs", "guidance_passages.rds"))
message(nrow(g$passages), " passages in ", length(unique(g$passages$section)),
        " sections; ", nrow(g$refs), " references.")
