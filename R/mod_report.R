# mod_report.R -- the AI Report tab: an LLM-drafted chapter that follows the
# CGJR guidance note (report_ai.R), and an AI Chat over the guidance and the
# dashboard data (chat_ai.R). Both cite their sources and are checked by
# verify_text(). LLM calls are asynchronous (ExtendedTask / promises), so the
# session and the Shiny worker stay responsive. Provider config is env-only
# (utils_llm.R); which guidance is used is CGJR_GUIDANCE_SOURCE (guidance.R).

#' AI Report tab UI
#'
#' @param id Module id.
#' @return A `shiny::tagList` - the content of the `"report"`
#'   [bs4Dash::tabItem()].
#' @export
mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bs4Dash::bs4Card(
      title = shiny::tagList(
        "AI Report and Chat ",
        shiny::tags$span(
          shiny::icon("circle-info"),
          title = paste0("A draft governance chapter that follows the CGJR guidance note, ",
                         "written by a large language model from the dashboard's figures, ",
                         "and a chat that answers questions from the guidance and the data. ",
                         "Every citation is checked automatically; review before any official use.")
        )
      ),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,

      shiny::uiOutput(ns("context")),

      # bs4Dash's tabsetPanel(), not shiny's (see mod_data.R)
      bs4Dash::tabsetPanel(
        id = ns("tabs"), type = "pills",
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("file-lines"), " Write report"),
          value = "write",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::selectInput(ns("scope"), "What to write",
                                 choices = report_scope_choices(), selected = "chapter",
                                 width = "100%")
            ),
            shiny::column(
              width = 7, style = "padding-top: 30px;",
              bs4Dash::actionButton(ns("generate"), "Generate AI report",
                                    icon = shiny::icon("wand-magic-sparkles"),
                                    status = "primary"),
              shiny::uiOutput(ns("download_ui"), inline = TRUE)
            )
          ),
          shiny::uiOutput(ns("status")),
          shiny::uiOutput(ns("verification")),
          shiny::uiOutput(ns("preview"))
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("comments"), " Chat"),
          value = "chat",
          shiny::br(),
          shiny::tags$p(
            class = "text-muted small",
            "Ask about the guidance note or the applied selection. Answers cite ",
            shiny::tags$span(class = "cgjr-cite cgjr-cite-g", "G"), " guidance passages and ",
            shiny::tags$span(class = "cgjr-cite cgjr-cite-e", "E"), " dashboard figures ",
            "(hover a citation to see its source); anything else is labelled ",
            shiny::tags$em("Beyond the guidance (not verified)"), ". Each reply is checked ",
            "automatically."
          ),
          shiny::div(
            class = "cgjr-chat",
            # no file uploads: they would be a new path for documents to
            # reach the LLM provider
            shinychat::chat_ui(
              ns("chat"), height = "560px", fill = FALSE, allow_attachments = FALSE,
              placeholder = "e.g. What does the guidance say to look at for public procurement?"
            )
          ),
          bs4Dash::actionButton(ns("chat_reset"), "New conversation",
                                icon = shiny::icon("rotate-left"), size = "sm",
                                style = "margin-top: 8px;")
        )
      ),

      shiny::hr(),
      bs4Dash::bs4Callout(
        title = "Disclaimer", status = "warning", width = 12,
        shiny::tags$small(
          "Text drafted by a large language model from the dashboard's figures and the ",
          "CGJR guidance. Citations and figures are checked automatically, but the text is ",
          "a starting point only: review it before use in any official document."
        )
      )
    )
  )
}

# The selection the AI features work on: exactly one base unit and its
# comparators - the applied ones, or the base country's region when fewer
# than 2 were applied.
.ai_selection <- function(base, comps, threshold, year_range) {
  if (length(base) != 1L) {
    return(list(problem = paste("The AI features need exactly one base country - select one",
                                "on Country Selection and Apply.")))
  }
  comps <- setdiff(comps, base); note <- NULL
  if (length(comps) < 2L) {
    comps <- .default_comparators(base)
    note <- sprintf(paste("Fewer than 2 comparators are applied, so the %d other countries of",
                          "the base country's region are used."), length(comps))
  }
  if (length(comps) < 2L) {
    return(list(problem = "Select a base country and at least 2 comparators on Country Selection, then Apply."))
  }
  list(base = base, comps = comps, threshold = threshold, year_range = year_range,
       note = note, key = paste(c(base, sort(comps), threshold, year_range), collapse = "|"))
}

# Charts shown under a report section in the preview (the dashboard's own).
.section_charts <- function(ev, key) {
  want <- if (key == "overview") "overall__clusters" else
    if (key %in% ev$meta$clusters) paste0(.OVERALL_PREFIX, key) else key
  Filter(function(c) c$key %in% want && c$view == "static", ev$charts)
}

#' AI Report tab server
#'
#' @param id Module id.
#' @param base_unit,comparison_units,threshold,year_range Reactives from
#'   [mod_country_selection_server()]. The AI features need a single base
#'   country; with fewer than 2 comparators its region is used.
#' @param chat_factory Function `system_prompt -> Chat` (default [llm_chat()]);
#'   a seam for tests.
#' @param guidance Output of [load_guidance()] (default: loaded per
#'   `CGJR_GUIDANCE_SOURCE`).
#' @return Invisibly `NULL`.
#' @export
mod_report_server <- function(id, base_unit, comparison_units, threshold, year_range,
                              chat_factory = llm_chat, guidance = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    g <- guidance %||% tryCatch(load_guidance(), error = function(e) e)
    g_problem <- if (inherits(g, "error")) conditionMessage(g)

    sel <- shiny::reactive(.ai_selection(base_unit(), comparison_units(), threshold(),
                                         year_range()))

    output$context <- shiny::renderUI({
      s <- sel()
      src <- if (is.null(g_problem)) g$label else g_problem
      shiny::tags$p(
        class = "small text-muted",
        shiny::icon("book"), " Guidance: ", shiny::tags$strong(src), shiny::tags$br(),
        shiny::icon("globe"), " Selection: ",
        if (!is.null(s$problem)) shiny::tags$span(class = "text-danger", s$problem) else
          sprintf("%s compared with %d countries (%s thresholds, %d-%d). %s",
                  .unit_name(s$base), length(s$comps), s$threshold,
                  s$year_range[[1]], s$year_range[[2]], s$note %||% "")
      )
    })

    # ---- Report ----------------------------------------------------------
    r_plan   <- shiny::reactiveVal(NULL)   # plan_ai_report() result
    r_ev     <- shiny::reactiveVal(NULL)   # the evidence the plan was built on
    r_text   <- shiny::reactiveVal(list()) # call id -> text so far
    r_done   <- shiny::reactiveVal(list()) # call id -> complete text (for resume)
    r_status <- shiny::reactiveVal(list(state = "idle"))
    r_result <- shiny::reactiveVal(NULL)   # narrative, verification, cited

    finish <- function(text, plan, ev, error = NULL) {
      v <- verify_text(text, ev, g, expected_sections = if (is.null(error)) plan$sections$key)
      r_result(list(narrative = parse_narrative(text), verification = v,
                    cited = .cited_passages(text, g), text = text, scope = plan$scope,
                    base = ev$meta$base_unit))
      n_done <- length(shiny::isolate(r_done()))
      r_status(if (is.null(error)) list(state = "done") else list(
        state = "error", msg = error, partial = nzchar(text),
        n_done = n_done, n = length(plan$calls)))
    }

    task <- shiny::ExtendedTask$new(function(plan, ev, done) {
      last <- 0
      running <- function(i, call, note = NULL) r_status(list(
        state = "running", step = i, n = length(plan$calls), label = call$label, note = note))
      on_progress <- function(i, call, text) {
        if (is.null(text)) return(running(i, call))
        now <- as.numeric(Sys.time())
        if (now - last > 0.3) {
          last <<- now
          cur <- shiny::isolate(r_text()); cur[[call$id]] <- text; r_text(cur)
        }
      }
      on_retry <- function(i, call, n, delay, cnd) {
        why <- if (.is_rate_limit(cnd)) "hit its rate limit" else
          sprintf("is busy (%s)", .http_status(cnd) %||% "temporary error")
        running(i, call, sprintf("The AI service %s - retrying in %d seconds (retry %d of %d).",
                                 why, round(delay), n, length(.RETRY_DELAYS)))
      }
      on_call_done <- function(i, call, text) {
        d <- shiny::isolate(r_done()); d[[call$id]] <- text; r_done(d)
        cur <- shiny::isolate(r_text()); cur[[call$id]] <- text; r_text(cur)
      }
      p <- run_ai_report(plan, chat_factory, on_progress, on_retry, on_call_done, done = done)
      promises::then(p,
        onFulfilled = function(text) finish(text, plan, ev),
        onRejected = function(e) {
          # what was written: the completed parts, then the part that failed
          ids <- vapply(plan$calls, `[[`, character(1), "id")
          cur <- shiny::isolate(r_text())
          kept <- paste(unlist(cur[intersect(ids, names(cur))]), collapse = "\n\n")
          finish(kept, plan, ev, error = llm_error_message(e))
        })
    })

    start_llm <- function() {
      fail <- function(msg) { r_status(list(state = "error", msg = msg)); FALSE }
      if (!is.null(g_problem)) return(fail(g_problem))
      st <- check_llm_available()
      if (st != "ok") return(fail(.llm_problem_message(st)))
      TRUE
    }

    shiny::observeEvent(input$generate, {
      if (task$status() == "running") return()
      s <- sel()
      if (!is.null(s$problem)) return(r_status(list(state = "error", msg = s$problem)))
      if (!start_llm()) return()
      sc <- .parse_scope(input$scope)
      ev <- tryCatch(
        shiny::withProgress(message = "Computing the dashboard evidence...", value = 0.5,
          build_evidence(s$base, s$comps, s$threshold, s$year_range, clusters = sc$cluster)),
        error = function(e) e)
      if (inherits(ev, "error")) return(r_status(list(
        state = "error", msg = paste("Could not compute the evidence:", conditionMessage(ev)))))
      plan <- plan_ai_report(ev, g, input$scope)
      r_ev(ev); r_plan(plan); r_text(list()); r_done(list()); r_result(NULL)
      r_status(list(state = "running", step = 1L, n = length(plan$calls), label = plan$calls[[1]]$label))
      task$invoke(plan, ev, list())
    })

    # Resume a failed run: keep the completed parts, redo the rest.
    shiny::observeEvent(input$resume, {
      plan <- r_plan(); ev <- r_ev()
      if (task$status() == "running" || is.null(plan)) return()
      if (!start_llm()) return()
      done <- r_done()
      r_text(done); r_result(NULL)
      task$invoke(plan, ev, done)
    })

    narrative <- shiny::reactive(parse_narrative(paste(unlist(r_text()), collapse = "\n\n")))

    output$status <- shiny::renderUI({
      st <- r_status()
      switch(st$state,
        idle = shiny::tags$p(class = "text-muted",
          "Choose what to write and click 'Generate AI report'. The text follows the guidance",
          "note's structure and cites the dashboard's figures; the dashboard's own charts are",
          "shown with each section."),
        running = shiny::tags$div(class = "alert alert-light",
          shiny::tags$span(class = "spinner-border spinner-border-sm text-primary mr-2", role = "status"),
          sprintf("Writing part %d of %d: %s ...", st$step, st$n, st$label),
          if (!is.null(st$note)) shiny::tags$div(class = "small text-muted mt-1", st$note)),
        done = NULL,
        error = bs4Dash::bs4Callout(title = "The AI report could not be completed", status = "danger",
          width = 12, shiny::tags$p(st$msg),
          if (isTRUE(st$partial)) shiny::tags$p(class = "small", sprintf(
            "%d of %d parts were completed; what was written is kept below and can be downloaded.",
            st$n_done %||% 0L, st$n %||% 0L)),
          if (isTRUE((st$n_done %||% 0L) < (st$n %||% 0L)) && !is.null(r_plan()))
            bs4Dash::actionButton(ns("resume"), "Resume: write the remaining parts",
                                  icon = shiny::icon("play"), status = "primary", size = "sm"))
      )
    })

    output$verification <- shiny::renderUI({
      res <- r_result(); shiny::req(res)
      v <- res$verification
      ok <- nrow(v$issues) == 0L
      bs4Dash::bs4Callout(
        title = if (ok) "Automated check: all citations and figures verified" else
          "Automated check: review the flagged sentences",
        status = if (ok) "success" else "warning", width = 12,
        shiny::tags$p(verification_summary(v)),
        if (!ok) shiny::tags$ul(lapply(utils::head(seq_len(nrow(v$issues)), 15), function(i)
          shiny::tags$li(shiny::tags$strong(v$issues$detail[[i]]),
                         if (!is.na(v$issues$sentence[[i]])) shiny::tags$span(class = "text-muted",
                           paste0(" - \"", substr(v$issues$sentence[[i]], 1L, 160L), "\""))))),
        if (nrow(res$cited)) shiny::tags$details(
          shiny::tags$summary(sprintf("%d guidance passages cited", nrow(res$cited))),
          shiny::tags$ul(lapply(seq_len(nrow(res$cited)), function(i) shiny::tags$li(
            sprintf("%s - section %s, %s, p.%d", res$cited$id[[i]], res$cited$section[[i]],
                    res$cited$title[[i]], res$cited$page[[i]])))))
      )
    })

    output$download_ui <- shiny::renderUI({
      res <- r_result()
      if (is.null(res)) return(shiny::tags$button(
        type = "button", class = "btn btn-default", disabled = NA, style = "margin-left: 8px;",
        shiny::icon("file-word"), " Download Word"))
      shiny::downloadButton(ns("download_docx"), "Download Word", icon = shiny::icon("file-word"),
                            style = "margin-left: 8px;")
    })

    output$download_docx <- shiny::downloadHandler(
      filename = function() {
        res <- r_result()
        paste0("GOVCGJR_AI_report_", res$base, "_", gsub("[^A-Za-z0-9]+", "_", res$scope), "_",
               format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        res <- r_result(); shiny::req(res)
        sc <- .parse_scope(res$scope)
        shiny::withProgress(message = "Compiling the Word report...", value = 0.3, {
          doc <- evidence_report_docx(
            r_ev(), res$narrative, leaves = if (sc$type == "leaf") sc$key,
            ai = list(guidance = g, cited = res$cited, verification = res$verification))
          print(doc, target = file)
        })
      }
    )

    # Preview skeleton: one block per planned section - its text (updated as
    # it streams) and the dashboard's chart(s) for it.
    output$preview <- shiny::renderUI({
      plan <- r_plan(); ev <- r_ev(); shiny::req(plan, ev)
      shiny::tagList(lapply(seq_len(nrow(plan$sections)), function(i) {
        key <- plan$sections$key[[i]]
        level <- if (key %in% c("overview", "recommendations", ev$meta$clusters)) "h4" else "h5"
        charts <- .section_charts(ev, key)
        shiny::div(
          class = "cgjr-ai-section",
          shiny::tags[[level]](plan$sections$title[[i]]),
          shiny::uiOutput(ns(paste0("sec_", key))),
          lapply(seq_along(charts), function(j) shiny::plotOutput(
            ns(sprintf("plot_%s_%d", key, j)),
            height = sprintf("%dpx", round(96 * .chart_inches(charts[[j]], ev$meta$base_unit)))))
        )
      }))
    })

    shiny::observeEvent(r_plan(), {
      plan <- r_plan(); ev <- r_ev()
      for (key in plan$sections$key) local({
        k <- key
        output[[paste0("sec_", k)]] <- shiny::renderUI({
          txt <- narrative()[[k]]
          if (is.null(txt)) {
            st <- r_status()$state
            return(shiny::tags$p(class = "text-muted small",
              if (st == "running") "Waiting for the AI..." else "Not written."))
          }
          shiny::HTML(.render_cited_html(txt, ev, g))
        })
        charts <- .section_charts(ev, k)
        for (j in seq_along(charts)) local({
          ch <- charts[[j]]
          output[[sprintf("plot_%s_%d", k, j)]] <- shiny::renderPlot(.chart_ggplot(ch, ev$meta), res = 96)
        })
      })
    })

    # ---- Chat ------------------------------------------------------------
    chat_state <- new.env()
    chat_state$chat <- NULL; chat_state$key <- NULL; chat_state$ev <- NULL
    chat_state$report <- NULL; chat_state$busy <- FALSE

    chat_say <- function(text) shinychat::chat_append("chat", text)

    shiny::observeEvent(input$chat_reset, {
      chat_state$chat <- NULL; chat_state$key <- NULL
      shinychat::chat_clear("chat")
    })

    shiny::observeEvent(input$chat_user_input, {
      msg <- .chat_input_text(input$chat_user_input)
      # shinychat keeps its input locked until it gets a reply, so always answer
      if (!nzchar(trimws(msg))) return(chat_say("_Type a question first._"))
      if (chat_state$busy) return(chat_say("_Still answering the previous question - please wait._"))
      if (!is.null(g_problem)) return(chat_say(paste("**The chat is unavailable.**", g_problem)))
      st <- check_llm_available()
      if (st != "ok") return(chat_say(paste("**The chat is unavailable.**", .llm_problem_message(st))))

      s <- sel()
      key <- s$key %||% "none"
      res <- r_result()
      report <- if (!is.null(res) && identical(res$base, s$base)) res$text
      if (!identical(key, chat_state$key)) {
        if (!is.null(chat_state$key)) chat_say(paste(
          "_The selection changed, so this conversation now uses the new selection's data;",
          "earlier answers referred to the previous one._"))
        chat_state$ev <- if (is.null(s$problem)) tryCatch(
          shiny::withProgress(message = "Loading the dashboard evidence for the chat...", value = 0.5,
            build_evidence(s$base, s$comps, s$threshold, s$year_range)),
          error = function(e) NULL)
        prompt <- .chat_system_prompt(g, chat_state$ev, report)
        if (is.null(chat_state$chat)) chat_state$chat <- chat_factory(prompt)
        else chat_state$chat$set_system_prompt(prompt)
        chat_state$key <- key; chat_state$report <- report
      } else if (!identical(report, chat_state$report)) {
        chat_state$chat$set_system_prompt(.chat_system_prompt(g, chat_state$ev, report))
        chat_state$report <- report
      }
      # keep the last 10 exchanges: cost, and stale context
      turns <- chat_state$chat$get_turns()
      if (length(turns) > 20L) chat_state$chat$set_turns(utils::tail(turns, 20L))

      chat_state$busy <- TRUE
      p <- .chat_turn(chat_state$chat, msg, "chat", chat_state$ev, g, session)
      promises::finally(p, function() chat_state$busy <- FALSE)
      invisible(NULL)
    })

    invisible(NULL)
  })
}

# shinychat's input value: a string, or a list - list(text =, attachments =)
# or, from the browser, an unnamed list("...").
.chat_input_text <- function(x) {
  if (is.list(x)) x <- x[["text"]] %||% Filter(is.character, x)
  paste(unlist(x), collapse = "\n")
}

.unit_name <- function(code) {
  nm <- cgjrdata::cgjr_ctf$unit_name[match(code, cgjrdata::cgjr_ctf$unit_code)]
  if (is.na(nm)) code else nm
}
