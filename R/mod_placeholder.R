# mod_placeholder.R -- stand-in page for sidebar items whose content hasn't
# been written yet (Methodology & User Guide, Terms of use and Disclaimers,
# FAQ, Contact Us). Same card as the Home page (cliarappak's navy bs4Card) so
# the item looks finished; replace with a real module when the content lands.

#' Placeholder page UI
#'
#' @param id Module namespace ID.
#' @param title Card title - the page's sidebar label.
#' @return A `shiny::tagList` holding one full-width card - the content of a
#'   [bs4Dash::tabItem()].
#' @export
mod_placeholder_ui <- function(id, title) {
  shiny::tagList(
    bs4Dash::bs4Card(
      width = 12,
      status = "navy",
      solidHeader = TRUE,
      title = title,
      shiny::tags$p(class = "text-muted", "Content coming soon.")
    )
  )
}
