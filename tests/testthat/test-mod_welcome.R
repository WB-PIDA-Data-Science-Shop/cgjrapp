# test-mod_welcome.R
# Tests for mod_welcome_ui(), mod_welcome_server(), and the %||% operator.

# -- %||% operator ---

test_that("%||% returns left side when not NULL", {
  expect_equal("a" %||% "b", "a")
})

test_that("%||% returns right side when left is NULL", {
  expect_equal(NULL %||% "b", "b")
})

test_that("%||% works with character(0) (not NULL, so returns left)", {
  expect_equal(character(0) %||% "b", character(0))
})

test_that("%||% works with a length-2 vector without error", {
  expect_no_error(c("x", "y") %||% "b")
  expect_equal(c("x", "y") %||% "b", c("x", "y"))
})

test_that("%||% works with a length-10 vector without error", {
  v <- letters[1:10]
  expect_no_error(v %||% "fallback")
  expect_equal(v %||% "fallback", v)
})

# -- UI structure tests ---

test_that("mod_welcome_ui is cliarappak's home card: navy, logo banner, GOVCGJR text", {
  html <- as.character(mod_welcome_ui("test"))
  expect_match(html, "card-navy", fixed = TRUE)
  expect_match(html, "assets/cgjr_logo.jpg", fixed = TRUE)
  expect_match(html, "Welcome to the GOVCGJR Benchmarking Dashboard!", fixed = TRUE)
  expect_match(html, "<h3>How to use this dashboard</h3>", fixed = TRUE)
  for (tab in c("Country Selection", "Benchmarking", "Data", "AI Report")) {
    expect_match(html, paste0("<strong>", tab, "</strong>"), fixed = TRUE)
  }
})

test_that("mod_placeholder_ui is a titled navy card", {
  html <- as.character(mod_placeholder_ui("faq", "FAQ"))
  expect_match(html, "card-navy", fixed = TRUE)
  expect_match(html, "FAQ", fixed = TRUE)
})

# -- Server tests ---

test_that("mod_welcome_server runs without error", {
  expect_no_error(
    shiny::testServer(mod_welcome_server, expr = {
      expect_true(TRUE)
    })
  )
})
