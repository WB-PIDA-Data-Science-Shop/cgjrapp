# mod_welcome.R -- Tab 1: Home / Landing page
# Static landing page matching the cpiaapp pattern:
# logo in card_header, welcome title + markdown in card_body.
# No sidebar, no data.

# -- UI ------------------------------------------------------------------

#' Home tab UI
#'
#' @param id Module namespace ID.
#' @return A [bslib::nav_panel()] containing the landing page.
#' @export
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Home",
    icon  = shiny::icon("home"),
    value = "home",

    bslib::card(
      bslib::card_header(
        shiny::tags$img(src = "assets/cgjr_logo.jpg", width = "80%")
      ),
      bslib::card_body(
        shiny::tags$br(),
        shiny::markdown(
          readLines(
            system.file("markdown", "cgjr_home.md", package = "cgjrapp")
          )
        )
      )
    )
  )
}

#' Home tab server
#'
#' @param id Module namespace ID.
#' @export
mod_welcome_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # No reactive logic needed -- content is static
  })
}

# Operator alias for internal use (base R doesn't export %||%)
`%||%` <- function(a, b) if (!is.null(a)) a else b
