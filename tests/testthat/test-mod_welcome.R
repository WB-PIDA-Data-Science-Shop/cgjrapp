# test-mod_welcome.R
# Tests for mod_welcome_ui(), mod_welcome_server(), and the %||% operator.

# ── %||% operator ────────────────────────────────────────────────────────────

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

# ── UI structure tests ────────────────────────────────────────────────────────

test_that("mod_welcome_ui returns a shiny.tag object", {
  ui <- mod_welcome_ui("test")
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_welcome_ui contains a nav_panel with value 'home'", {
  ui <- mod_welcome_ui("test")
  expect_equal(ui$attribs$`data-value`, "home")
})

# ── Server tests ──────────────────────────────────────────────────────────────

test_that("mod_welcome_server runs without error", {
  expect_no_error(
    shiny::testServer(mod_welcome_server, expr = {
      expect_true(TRUE)
    })
  )
})
