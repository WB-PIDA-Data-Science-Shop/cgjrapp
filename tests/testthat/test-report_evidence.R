# AI Report, Phase 1: evidence pack, priority areas, data-only Word report.

EV_COMPS <- cgjrapp:::.default_comparators("GHA")
EV <- build_evidence("GHA", EV_COMPS, "Default", c(2013L, 2024L),
                     clusters = "institutional_environment")

test_that("build_evidence needs one base unit and 2+ comparators", {
  expect_error(build_evidence(c("GHA", "NGA"), EV_COMPS), "exactly one base unit")
  expect_error(build_evidence("GHA", c("GHA", "NGA")), "at least 2 comparison units")
})

test_that("facts carry unique, sequential evidence ids and a statement each", {
  f <- EV$facts
  expect_identical(f$id, paste0("E", seq_len(nrow(f))))
  expect_false(anyNA(f$text))
  expect_true(all(c("static", "dynamic") %in% f$view))
})

test_that("Overview covers every cluster; leaf facts only the selected clusters", {
  f <- EV$facts
  all_cl <- unique(cgjrdata::cgjr_taxonomy$cluster_name)
  expect_setequal(f$name[f$level == "cluster" & f$view == "static"], all_cl)
  expect_true(all(f$cluster[f$level != "cluster"] == "institutional_environment"))
})

test_that("cluster facts are exactly family_scores() -> def_quantiles()", {
  ch <- cgjrapp:::.overall_charts()[[1]]
  dat <- filter_unit_data(cgjrapp:::.overall_leaves(ch), "static", c("GHA", EV_COMPS))
  dq <- def_quantiles(family_scores(dat, "GHA", EV_COMPS, ch$row_col, ch$rows),
                      "GHA", EV_COMPS, "Default")
  ghana <- dq[dq$unit_code == "GHA", ]
  f <- EV$facts[EV$facts$level == "cluster" & EV$facts$view == "static", ]
  f <- f[match(ghana$indicator, f$name), ]
  expect_equal(f$ctf, ghana$dtf)
  expect_identical(f$rank, ghana$nrank)
  expect_match(f$text[[1]], "^.+ \\(cluster average\\) for Ghana: CTF \\d\\.\\d{3}; ")
})

test_that("dynamic facts reuse the chart labels' change table", {
  f <- EV$facts[EV$facts$view == "dynamic" & EV$facts$level == "cluster", ]
  expect_true(all(f$year_first < f$year_last))
  expect_true(all(f$year_first >= 2013L & f$year_last <= 2024L))
  expect_match(f$text, "change in CTF [+-]\\d\\.\\d{3}; rank (unchanged at|from)")
})

test_that("areas with no indicators are listed as gaps, not scored", {
  ev <- build_evidence("GHA", EV_COMPS, clusters = "core_governance_functions")
  # PFM sub-subclusters with no indicators in cgjr_ctf
  measured <- unique(cgjrdata::cgjr_ctf$leaf)
  tx <- cgjrdata::cgjr_taxonomy
  unmeasured <- setdiff(
    dplyr::coalesce(tx$sub_subcluster, tx$subcluster)[tx$cluster == "core_governance_functions"],
    measured)
  expect_true(all(unmeasured %in% ev$gaps$leaf))
  expect_false(any(ev$facts$leaf %in% unmeasured))
})

test_that("priority_areas: band first, then lowest CTF; branching subclusters split", {
  ev <- build_evidence("GHA", EV_COMPS)
  pa <- priority_areas(ev)
  band <- match(sub(" .*", "", pa$status), c("Weak", "Emerging", "Strong"))
  expect_false(is.unsorted(band))
  for (b in unique(band)) expect_false(is.unsorted(pa$ctf[band == b]))
  # PFM shows as its sub-subclusters, not as one subcluster row
  expect_false("Public Financial Management" %in% pa$name)
  expect_true(any(pa$level == "sub_subcluster"))
})

test_that("evidence_report_docx: headings, charts and the full evidence register", {
  doc <- evidence_report_docx(EV)
  path <- withr::local_tempfile(fileext = ".docx")
  print(doc, target = path)
  expect_gt(file.size(path), 10000)

  s <- officer::docx_summary(officer::read_docx(path))
  heads <- s$text[s$style_name %in% c("heading 1", "heading 2")]
  expect_true(all(c("Overview", "Institutional Environment", "Degree of Integrity",
                    "Appendix: evidence register") %in% heads))
  expect_false("Context" %in% heads)                   # not a selected cluster
  expect_true(any(grepl("no AI-generated text", s$text, fixed = TRUE)))
  # every fact id appears in the register
  expect_true(all(EV$facts$id %in% s$text[s$content_type == "table cell"]))

  media <- utils::unzip(path, list = TRUE)$Name
  expect_gt(sum(grepl("^word/media/.*\\.png$", media)), 3L)
})

test_that("narrative text, when given, lands under its heading", {
  doc <- evidence_report_docx(EV, narrative = list(
    overview = "Ghana leads its region on most clusters.",
    degree_of_integrity = "### What the data shows\nIntegrity is the weakest area [E9]."
  ))
  s <- officer::docx_summary(doc)
  i_over <- match("Overview", s$text)
  i_text <- match("Ghana leads its region on most clusters.", s$text)
  expect_true(!is.na(i_text) && i_text > i_over)
  expect_true("What the data shows" %in% s$text[s$style_name == "heading 3"])
  expect_true(any(grepl("AI-assisted draft", s$text, fixed = TRUE)))
})

test_that("evidence_report_pptx: template title slide, editable charts, one slide per leaf", {
  ppt <- evidence_report_pptx(EV)
  path <- withr::local_tempfile(fileext = ".pptx")
  print(ppt, target = path)
  s <- officer::pptx_summary(officer::read_pptx(path))
  txt <- s$text[!is.na(s$text)]

  expect_true("Governance and Public Institutions: Ghana" %in% txt)
  expect_true("Base country vs comparison countries" %in% txt)
  expect_true(all(c("Clusters: overview", "Clusters: change over time",
                    "Institutional Environment: subclusters") %in% txt))
  for (leaf in c("Degree of Integrity", "Transparency and Accountability",
                 "Justice and Rule of Law")) {
    expect_true(leaf %in% txt)
  }
  expect_false(any(grepl("CLIAR", txt, fixed = TRUE)))   # template rebranded
  # rvg charts are native shapes: their axis text is real slide text
  expect_true("Closeness to frontier" %in% txt)
  # no slide spills past the template's layouts
  expect_identical(unique(officer::layout_summary(ppt)$master), "Custom Design")
})
