# dev/build_pptx_template.R - derive inst/www/govcgjr_template.pptx from
# cliarappak's inst/app/www/CLIAR_template.pptx.
#
#   CLIARAPPAK_DIR=/path/to/cliarappak Rscript dev/build_pptx_template.R
#
# Keeps CLIAR's master, layouts and World Bank Governance logo; swaps the CLIAR
# banner (ppt/media/image1.png, used by the title slide and its layout) for the
# GOVCGJR banner (inst/www/cgjr_logo.jpg, padded with its own background colour
# to CLIAR's aspect ratio so it isn't stretched); and drops slides 2-9 (CLIAR's
# "What is CLIAR" / methodology / pre-filled selection slides) - the report
# code adds every content slide itself. Run once; the output is committed.
#
# Needs the png and jpeg packages (dev-only, not package dependencies).

cliar_dir <- Sys.getenv("CLIARAPPAK_DIR")
stopifnot("set CLIARAPPAK_DIR to a cliarappak checkout" = dir.exists(cliar_dir))
src <- file.path(cliar_dir, "inst", "app", "www", "CLIAR_template.pptx")
out <- file.path("inst", "www", "govcgjr_template.pptx")

work <- tempfile("pptx_"); dir.create(work)
utils::unzip(src, exdir = work)

# GOVCGJR banner -> PNG at CLIAR's banner aspect ratio
cliar_banner <- png::readPNG(file.path(work, "ppt", "media", "image1.png"))
target_ratio <- dim(cliar_banner)[2] / dim(cliar_banner)[1]
logo <- jpeg::readJPEG(file.path("inst", "www", "cgjr_logo.jpg"))
h <- dim(logo)[1]; w <- dim(logo)[2]
new_w <- round(h * target_ratio)
bg <- logo[1, 1, ]                                # the banner's own background
padded <- array(rep(bg, each = h * new_w), dim = c(h, new_w, 3))
left <- (new_w - w) %/% 2
padded[, left + seq_len(w), ] <- logo
png::writePNG(padded, file.path(work, "ppt", "media", "image1.png"))

# re-zip (officer's own zip dependency), then drop slides 2-9 with officer
swapped <- tempfile(fileext = ".pptx")
old <- setwd(work)
zip::zip(swapped, files = list.files(".", recursive = TRUE, all.files = TRUE),
         mode = "mirror")
setwd(old)

ppt <- officer::read_pptx(swapped)
for (i in rev(seq_len(length(ppt))[-1])) ppt <- officer::remove_slide(ppt, index = i)
print(ppt, target = out)
message("Wrote ", out, " (", length(ppt), " slide)")
