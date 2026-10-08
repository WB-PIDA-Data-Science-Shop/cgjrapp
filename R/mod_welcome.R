# mod_welcome.R -- Home / landing page.
# Port of cliarappak::mod_home_ui(): one full-width navy bs4Card with the logo
# banner as its title, then the welcome text. The text lives in
# inst/markdown/cgjr_home.md so it can be edited without touching R.
# No data.

# -- UI ---

#' Home tab UI
#'
#' @param id Module namespace ID.
#' @return A `shiny::tagList` holding the landing-page card - the content of
#'   the `"home"` [bs4Dash::tabItem()].
#' @export
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bs4Dash::bs4Card(
      width = 12,
      status = "navy",
      solidHeader = TRUE,
      title = shiny::tags$span(
        shiny::tags$img(src = "assets/cgjr_logo.jpg", width = "80%")
      ),

      shiny::tags$br(),
      shiny::markdown(
        readLines(
          system.file("markdown", "cgjr_home.md", package = "cgjrapp"),
          encoding = "UTF-8"
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
