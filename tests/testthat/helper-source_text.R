# A PDF made on the spot, one page per element of `pages` (each drawn as a
# single line of text), returned as raw bytes - as a download would be.
make_pdf <- function(pages) {
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  for (p in pages) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, p)
  }
  grDevices::dev.off()
  readBin(f, "raw", file.size(f))
}

# The first six pages of a public World Bank working paper's text file (PRWP
# 10390), as raw bytes; pages are separated by form feeds.
wb_text_fixture <- function() {
  f <- test_path("fixtures", "research", "wb_prwp_10390_pages1-6.txt")
  readBin(f, "raw", file.size(f))
}
