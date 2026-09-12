# mod_report.R -- AI-generated institutional chapter.
#
# Builds a prompt from the LIVE per-selection composite scores
# (export_live_scores()), streams an LLM draft token-by-token, and offers it
# as a Word download. Provider config is env-only (utils_llm.R).

#' AI Report tab UI
#'
#' @param id Module id.
#' @return A [bslib::nav_panel()].
#' @export
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "AI Report", icon = shiny::icon("robot"), value = "report",

    bslib::card(
      bslib::card_header(
        shiny::tagList(
          "AI-generated institutional chapter",
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            paste0("A draft governance chapter for the CGJR, written by a ",
                   "large language model from the live CTF composite scores. ",
                   "Review and edit before any official use.")
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          width = 9,
          shiny::actionButton(ns("generate"), "Generate report",
                              icon = shiny::icon("wand-magic-sparkles"),
                              class = "btn-primary"),
          shiny::downloadButton(ns("download_docx"), "Download Word",
                                icon = shiny::icon("file-word"),
                                style = "margin-left: 8px;")
        ),
        shiny::column(width = 3, shiny::uiOutput(ns("status_badge")))
      ),

      shiny::hr(),

      bslib::card(
        class = "border-warning bg-warning-subtle mb-3",
        bslib::card_body(shiny::tags$small(
          shiny::tags$strong("Disclaimer: "),
          "LLM-generated from CTF scores. A draft starting point only; verify ",
          "before use in any official document."
        ))
      ),

      shiny::uiOutput(ns("report_output"))
    )
  )
}

#' AI Report tab server
#'
#' @param id Module id.
#' @param base_unit,comparison_units,threshold,year_range Reactives from
#'   [mod_country_selection_server()].
#' @return Invisibly `NULL`.
#' @export
mod_report_server <- function(id, base_unit, comparison_units, threshold,
                              year_range) {
  shiny::moduleServer(id, function(input, output, session) {

    r_text  <- shiny::reactiveVal("")
    r_gen   <- shiny::reactiveVal(FALSE)
    r_done  <- shiny::reactiveVal(FALSE)
    r_error <- shiny::reactiveVal(NULL)

    unit_name <- function(code) {
      nm <- cgjrdata::cgjr_ctf$unit_name[match(code, cgjrdata::cgjr_ctf$unit_code)]
      if (is.na(nm)) code else nm
    }

    output$status_badge <- shiny::renderUI({
      if (isTRUE(r_gen())) {
        shiny::tags$span(class = "badge bg-secondary", "Generating...")
      } else if (!is.null(r_error())) {
        shiny::tags$span(class = "badge bg-danger", "Error")
      } else if (isTRUE(r_done()) && nchar(r_text()) > 0L) {
        shiny::tags$span(class = "badge bg-success", "Ready")
      } else {
        shiny::tags$span(class = "badge bg-light text-dark", "Not generated")
      }
    })

    output$report_output <- shiny::renderUI({
      if (!is.null(r_error())) {
        bslib::card(class = "border-danger", bslib::card_body(
          shiny::tags$p(class = "text-danger",
                        shiny::tags$strong("Error: "), r_error())
        ))
      } else if (nchar(r_text()) == 0L) {
        shiny::tags$p(class = "text-muted",
          "Click 'Generate report' for a draft chapter on the selected base unit.")
      } else {
        shiny::tagList(
          shiny::HTML(commonmark::markdown_html(r_text())),
          if (isTRUE(r_gen())) {
            shiny::tags$span(class = "spinner-grow spinner-grow-sm text-secondary ms-2",
                             role = "status")
          }
        )
      }
    })

    shiny::observeEvent(input$generate, {
      shiny::req(base_unit())
      r_text(""); r_gen(TRUE); r_done(FALSE); r_error(NULL)

      base <- base_unit()
      comps <- comparison_units()
      base_nm <- unit_name(base)

      if (length(comps) < 2L) {
        r_error("Select a base unit and at least 2 comparators, then Apply.")
        r_gen(FALSE); return()
      }
      if (!check_llm_available()) {
        r_error(paste0("Cannot reach the LLM endpoint. Check CGJR_LLM_BASE_URL, ",
                       "CGJR_LLM_MODEL and CGJR_LLM_API_KEY in your .Renviron."))
        r_gen(FALSE); return()
      }

      scores_tbl <- tryCatch(
        export_live_scores(base, comps, "static", year_range()),
        error = function(e) NULL
      )
      if (is.null(scores_tbl) || nrow(scores_tbl) == 0L) {
        r_error("Could not compute scores for this selection.")
        r_gen(FALSE); return()
      }

      prompt <- tryCatch(
        build_cgjr_prompt(
          base_name   = base_nm, base_unit = base,
          scores_tbl  = scores_tbl, year_range = year_range(),
          ctf_type    = "static",
          comparators = vapply(comps, unit_name, character(1))
        ),
        error = function(e) NULL
      )
      if (is.null(prompt)) {
        r_error("Failed to build the report prompt."); r_gen(FALSE); return()
      }

      shiny::withProgress(
        message = paste0("Generating report for ", base_nm, "..."), value = 0,
        tryCatch(
          stream_llm_response(
            prompt = prompt, reactive_val = r_text,
            on_complete = function(text) { r_gen(FALSE); r_done(TRUE) }
          ),
          error = function(e) { r_error(conditionMessage(e)); r_gen(FALSE) }
        )
      )
    })

    output$download_docx <- shiny::downloadHandler(
      filename = function() {
        paste0("CGJR_report_", base_unit(), "_",
               format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        shiny::req(nchar(r_text()) > 0L)
        doc <- format_report_docx(r_text(), unit_name(base_unit()))
        print(doc, target = file)
      }
    )

    invisible(NULL)
  })
}
