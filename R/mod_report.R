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
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::checkboxInput(ns("use_research"), "Use published research", value = FALSE),
              shiny::tags$p(class = "small text-muted",
                "Searches World Bank documents, the listed journals and mAI search, checks and reads",
                "the sources, and cites their findings. About 1-2 minutes per section the first",
                "time; much faster once a country's research is cached."),
              shiny::uiOutput(ns("research_note"))
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(ns("depth"), "Sources per section",
                                 choices = c("10 (standard)" = "10", "5 (faster)" = "5"),
                                 selected = "10", width = "100%")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::selectInput(ns("format"), "Writing format",
                                 choices = c("Structured (fixed headings)" = "structured",
                                             "Narrative (CGJR prose, priorities in depth)" = "narrative"),
                                 selected = "structured", width = "100%")
            ),
            shiny::column(
              width = 7,
              shiny::textAreaInput(ns("focus"), "Focus for this draft (optional)", rows = 2, width = "100%",
                                   placeholder = "e.g. The team is preparing a DPF: emphasise the wage bill.")
            )
          ),
          shiny::uiOutput(ns("status")),
          shiny::uiOutput(ns("verification")),
          shiny::uiOutput(ns("preview")),
          shiny::uiOutput(ns("references"))
        ),
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("book-open"), " Research sources"),
          value = "sources",
          shiny::br(),
          shiny::uiOutput(ns("sources"))
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

#' AI Report tab server
#'
#' Writes the AI Report (and runs its chat) for the applied selection. With
#' "Use published research" ticked, the selected scope's sections are
#' researched one after another in the background (a new selection replaces
#' what is still queued), kept per country and depth for the session; a
#' report waits only for its own sections, which go to the front. The Research sources tab lists what was found and used;
#' excluding a source there (for that country, for the session) offers to
#' rewrite just the parts of the report that cite it.
#'
#' @param id Module id.
#' @param base_unit,comparison_units,threshold,year_range Reactives from
#'   [mod_country_selection_server()]. The AI features need a single base
#'   country; with fewer than 2 comparators its region is used.
#' @param chat_factory Function `system_prompt -> Chat` (default [llm_chat()]);
#'   a seam for tests.
#' @param guidance Output of [load_guidance()] (default: loaded per
#'   `CGJR_GUIDANCE_SOURCE`).
#' @param research_fun Function `(leaf, evidence, policy)` researching one
#'   report section: a value or a promise of it. The default runs the
#'   research in a background R process (mirai), so the session stays
#'   responsive; a seam for tests.
#' @return Invisibly `NULL`.
#' @export
mod_report_server <- function(id, base_unit, comparison_units, threshold, year_range,
                              chat_factory = llm_chat, guidance = NULL, research_fun = NULL) {
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
    r_pack <- shiny::reactiveVal(NULL)        # the research pack the plan cites
    r_pack_all <- shiny::reactiveVal(NULL)    # ... before the user's exclusions
    r_no_findings <- shiny::reactiveVal(FALSE)  # researched, but no finding to cite
    r_excluded <- shiny::reactiveVal(list())  # country -> source keys the user excluded
    excluded_for <- function(base) shiny::isolate(r_excluded())[[base]] %||% character(0)

    finish <- function(text, plan, ev, error = NULL) {
      v <- verify_text(text, ev, g, expected_sections = if (is.null(error)) plan$sections$key,
                       research = shiny::isolate(r_pack()))
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

    # Plan the report (with the research pack, if any) and start writing it.
    # `opts`: the writing format and the team's focus, as chosen at Generate.
    start_writing <- function(ev, scope, pack = NULL, opts = list()) {
      plan <- plan_ai_report(ev, g, scope, research = pack, format = opts$format %||% "structured",
                             focus = opts$focus)
      r_pack(pack); r_ev(ev); r_plan(plan); r_text(list()); r_done(list()); r_result(NULL)
      r_status(list(state = "running", step = 1L, n = length(plan$calls), label = plan$calls[[1]]$label))
      task$invoke(plan, ev, list())
    }

    # ---- Research ---------------------------------------------------------
    # Research is kept per country and depth ("GHA 10"): section -> its
    # .research_section() result, or list(error = ) if it failed. One run at
    # a time works through `run$queue`, each section in the background (by
    # default): a report's sections go to the front, and another country
    # replaces the queue - the run stops after the section under way.
    research_fun <- research_fun %||% function(leaf, ev, policy) {
      .in_background(".research_section", leaf, ev, policy)
    }
    r_store <- shiny::reactiveVal(list())
    r_shown <- shiny::reactiveVal(NULL)    # the research in view: list(key, base, ev)
    r_research <- shiny::reactive(r_store()[[r_shown()$key %||% ""]] %||% list())
    r_waiting <- shiny::reactiveVal(NULL)  # a report waiting for research: list(ev, scope, key, leaves)
    r_note <- shiny::reactiveVal(NULL)     # what the background run is doing
    run <- new.env()                       # the research wanted: key, ev, policy, queue, current
    run$queue <- character(0)

    research_task <- shiny::ExtendedTask$new(function() {
      key <- run$key; ev <- run$ev; policy <- run$policy
      titles <- .section_titles(ev)
      step <- function() {
        run$current <- NULL
        if (!identical(run$key, key) || length(run$queue) == 0L) {
          r_note(NULL)
          return(promises::promise_resolve(NULL))
        }
        leaf <- run$queue[[1]]
        run$queue <- run$queue[-1]; run$current <- leaf
        r_note(sprintf("Researching %s in the background - %d more section(s) to go.",
                       titles[[leaf]], length(run$queue)))
        w <- shiny::isolate(r_waiting())
        if (!is.null(w)) r_status(list(state = "researching", label = titles[[leaf]],
          done = sum(w$leaves %in% names(shiny::isolate(r_store())[[w$key]])), n = length(w$leaves)))
        p <- tryCatch({
          v <- research_fun(leaf, ev, policy)
          if (promises::is.promising(v)) promises::as.promise(v) else promises::promise_resolve(v)
        }, error = function(e) promises::promise_reject(e))
        p <- promises::catch(p, function(e) list(leaf = leaf, error = conditionMessage(e)))
        promises::then(p, function(res) {
          store <- shiny::isolate(r_store()); store[[key]][[leaf]] <- res; r_store(store)
          write_if_ready()
          step()
        })
      }
      step()
    })
    # a run stopped for another country: start the new country's research
    shiny::observeEvent(research_task$status(), {
      if (research_task$status() != "running" && length(run$queue)) research_task$invoke()
    })

    # Ask for research on `leaves` for the country of `ev` at `depth`: the
    # sections not yet researched (or whose research failed) are queued - at
    # the front if `first` (a report's own), else in place of what was queued.
    want_research <- function(ev, depth, leaves, first = FALSE) {
      key <- paste(ev$meta$base_unit, depth)
      under_way <- if (identical(run$key, key)) run$current  # this country's section in progress
      if (!identical(run$key, key)) {
        run$key <- key; run$ev <- ev; run$policy <- .research_policy(depth); run$queue <- character(0)
      }
      store <- shiny::isolate(r_store())
      researched <- function(l) !is.null(store[[key]][[l]]) && is.null(store[[key]][[l]]$error)
      todo <- Filter(function(l) !researched(l) && !identical(l, under_way), leaves)
      store[[key]][todo] <- NULL; r_store(store)  # failed before: tried again
      # a report's sections go first; otherwise the selection's sections
      # replace what was queued (keeping what a waiting report still needs)
      run$queue <- if (first) union(todo, run$queue)
                   else union(intersect(run$queue, shiny::isolate(r_waiting())$leaves), todo)
      r_shown(list(key = key, base = ev$meta$base_unit, ev = ev))
      if (research_task$status() != "running" && length(run$queue)) research_task$invoke()
    }

    # A report waiting for research: once all its sections are researched,
    # write it from them, without the sources excluded. A failure here is
    # reported, and frees Generate.
    write_if_ready <- function() {
      w <- shiny::isolate(r_waiting())
      if (is.null(w)) return()
      have <- shiny::isolate(r_store())[[w$key]]
      if (!all(w$leaves %in% names(have))) return()
      r_waiting(NULL)
      tryCatch({
        pack <- .research_pack_of(have[w$leaves])
        r_pack_all(pack)
        r_no_findings(is.null(pack))  # researched, but nothing checked to cite
        if (!is.null(pack)) pack <- .drop_sources(pack, excluded_for(w$ev$meta$base_unit))
        start_writing(w$ev, w$scope, pack, w$opts)
      }, error = function(e) r_status(list(
        state = "error", msg = paste("The research could not be completed:", conditionMessage(e)))))
    }

    # Exclude a source (or include it again) for this country, for the rest
    # of the session; the Research sources tab sends its key.
    set_excluded <- function(key, exclude) {
      base <- r_shown()$base
      ex <- r_excluded()
      ex[[base]] <- if (exclude) union(ex[[base]], key) else setdiff(ex[[base]], key)
      r_excluded(ex)
    }
    shiny::observeEvent(input$exclude, set_excluded(input$exclude, TRUE))
    shiny::observeEvent(input$include, set_excluded(input$include, FALSE))

    # The finished parts (call ids) whose text cites a finding of an excluded source.
    stale_parts <- shiny::reactive({
      pack <- r_pack(); ev <- r_ev(); shiny::req(pack, ev)
      ex <- r_excluded()[[ev$meta$base_unit]]
      gone <- pack$findings$id[pack$findings$key %in% ex]
      done <- r_done()
      names(done)[vapply(done, function(text) any(.cited_ids(text) %in% gone), NA)]
    })

    # Rewrite those parts without the excluded sources; the rest is kept
    # (the Resume path: run_ai_report() skips the parts already done).
    shiny::observeEvent(input$rewrite, {
      plan <- r_plan(); ev <- r_ev(); stale <- stale_parts()
      if (task$status() == "running" || !is.null(r_waiting()) || length(stale) == 0L) return()
      if (!start_llm()) return()
      pack <- .drop_sources(r_pack_all(), excluded_for(ev$meta$base_unit))
      plan <- plan_ai_report(ev, g, plan$scope, research = pack, format = plan$format, focus = plan$focus)
      done <- r_done()[setdiff(names(r_done()), stale)]
      r_pack(pack); r_plan(plan); r_done(done); r_text(done); r_result(NULL)
      task$invoke(plan, ev, done)
    })

    shiny::observeEvent(input$generate, {
      if (task$status() == "running" || !is.null(r_waiting())) return()
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
      opts <- list(format = input$format %||% "structured", focus = input$focus)
      r_no_findings(FALSE)
      if (!isTRUE(input$use_research)) return(start_writing(ev, input$scope, opts = opts))
      # research first (what is not researched yet): the plan is made once
      # the findings are in
      leaves <- .report_leaves(ev, input$scope)
      r_ev(ev); r_plan(NULL); r_pack(NULL); r_result(NULL)
      r_waiting(list(ev = ev, scope = input$scope, key = paste(ev$meta$base_unit, input$depth),
                     leaves = leaves, opts = opts))
      want_research(ev, input$depth, leaves, first = TRUE)
      write_if_ready()  # all researched already
    }, priority = 1)  # before the background research below, when both are triggered at once

    # Research ticked: research the selected scope's sections for the applied
    # country in the background, so a report can be written from them without
    # waiting; a new selection replaces what is still queued.
    shiny::observeEvent(list(sel(), input$use_research, input$depth, input$scope), {
      s <- sel()
      if (!isTRUE(input$use_research) || !is.null(s$problem) || check_llm_available() != "ok") return()
      ev <- tryCatch(build_evidence(s$base, s$comps, s$threshold, s$year_range), error = function(e) NULL)
      if (!is.null(ev)) want_research(ev, input$depth %||% "10", .report_leaves(ev, input$scope %||% "chapter"))
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
        researching = shiny::tags$div(class = "alert alert-light",
          shiny::tags$span(class = "spinner-border spinner-border-sm text-primary mr-2", role = "status"),
          sprintf("Researching for the report (%d of %d sections done): %s ...", st$done, st$n, st$label),
          shiny::tags$div(class = "small text-muted mt-1",
            "Searching, checking, reading and extracting findings - about 1-2 minutes per section",
            "the first time. The report is written once all sections are researched.")),
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
        shiny::withProgress(message = "Compiling the Word report...", value = 0.3, {
          doc <- ai_report_docx(r_ev(), r_plan()$sections, res$narrative, research = r_pack(),
                                guidance = g)
          print(doc, target = file)
        })
      }
    )

    # Preview skeleton: one block per planned section - its text (updated as
    # it streams) and the dashboard's chart(s) for it.
    output$preview <- shiny::renderUI({
      plan <- r_plan(); ev <- r_ev(); shiny::req(plan, ev)
      # the narrative format's questions, gathered at the end under one heading
      first_question <- match(TRUE, startsWith(plan$sections$key, "questions_"))
      shiny::tagList(lapply(seq_len(nrow(plan$sections)), function(i) {
        key <- plan$sections$key[[i]]
        level <- if (key %in% c("overview", "recommendations", ev$meta$clusters)) "h4" else "h5"
        charts <- .section_charts(ev, key)
        shiny::div(
          class = "cgjr-ai-section",
          if (identical(i, first_question)) shiny::tags$h4("Questions for further analysis"),
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
          pack <- r_pack()
          # [S1.1] -> (World Bank 2011, 37), year letters as across the whole report
          if (!is.null(pack)) txt <- .render_citations(txt, pack, paste(unlist(r_text()), collapse = "\n"))
          shiny::HTML(.render_cited_html(txt, ev, g, pack))
        })
        charts <- .section_charts(ev, k)
        for (j in seq_along(charts)) local({
          ch <- charts[[j]]
          output[[sprintf("plot_%s_%d", k, j)]] <- shiny::renderPlot(.chart_ggplot(ch, ev$meta), res = 96)
        })
      })
    })

    output$research_note <- shiny::renderUI({
      note <- r_note(); shiny::req(note)
      shiny::tags$p(class = "small text-info", shiny::icon("spinner", class = "fa-spin"), " ", note)
    })

    # The reference list of the research the report cites.
    output$references <- shiny::renderUI({
      pack <- r_pack(); res <- r_result(); shiny::req(res)
      if (is.null(pack)) {
        if (!isTRUE(r_no_findings())) return(NULL)
        return(shiny::tags$p(class = "small text-muted",
          "Published research was searched, but no findings could be checked against their",
          "sources, so this draft rests on the dashboard and the guidance alone. The Research",
          "sources tab shows what was found, and why it was not used."))
      }
      refs <- .reference_list(res$text, pack)
      if (length(refs) == 0L) return(NULL)
      # entries carry outside text (titles): escaped, then Markdown for italics
      shiny::div(class = "cgjr-references", shiny::tags$h4("References"),
                 shiny::HTML(commonmark::markdown_html(paste(htmltools::htmlEscape(refs), collapse = "\n\n"))))
    })

    output$sources <- shiny::renderUI({
      res <- r_research()
      if (length(res) == 0L) return(shiny::tags$p(class = "text-muted",
        "Tick 'Use published research' and generate a report: the sources found, checked and",
        "read for each section are listed here, with the findings drawn from them."))
      shown <- r_shown()
      titles <- .section_titles(shown$ev)
      excluded <- r_excluded()[[shown$base]] %||% character(0)
      stale <- if (!is.null(r_pack())) stale_parts()
      shiny::tagList(
        if (length(stale) && task$status() != "running") bs4Dash::bs4Callout(
          title = "Excluded sources are cited in the report", status = "warning", width = 12,
          shiny::tags$p(sprintf(
            "%d part(s) of the report cite a source you excluded. Rewrite them without it; the rest is kept.",
            length(stale))),
          bs4Dash::actionButton(ns("rewrite"), sprintf("Rewrite %d part(s)", length(stale)),
                                status = "primary", icon = shiny::icon("pen"))),
        # only the sections of the scope selected (research may cover the chapter)
        lapply(intersect(.report_leaves(shown$ev, input$scope %||% "chapter"), names(res)),
               function(leaf) .sources_panel(titles[[leaf]], res[[leaf]], excluded, ns))
      )
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

# One section's research for the Research sources tab: the selected sources
# with why they were chosen and what was drawn from them, then the ones set
# aside and why. `excluded` are the keys of the sources the user excluded;
# `ns` the module's namespace, for the Exclude buttons. Every title, reason
# and quote is outside text: built with htmltools tags, which escape it.
.sources_panel <- function(title, res, excluded = character(0), ns = shiny::NS(NULL)) {
  if (!is.null(res$error)) return(shiny::div(
    class = "cgjr-sources", shiny::tags$h5(title),
    shiny::tags$p(class = "text-danger", "Research could not be completed: ", res$error)))
  s <- res$sources
  used <- s[s$selected %in% TRUE, , drop = FALSE]
  aside <- s[!s$selected %in% TRUE, , drop = FALSE]
  # why a source was set aside: its eligibility reason and any issues, in words
  why <- vapply(seq_len(nrow(aside)), function(i) {
    paste(gsub("_", " ", c(aside$reason[[i]], aside$issues[[i]])), collapse = ", ")
  }, "")
  shiny::div(
    class = "cgjr-sources", shiny::tags$h5(title),
    shiny::tags$p(class = "small text-muted", sprintf(
      "%d sources used, %d set aside; %d findings with checked quotes.",
      nrow(used), nrow(aside), NROW(res$findings))),
    shiny::tags$ol(lapply(seq_len(nrow(used)), function(i) {
      .source_item(used[i, ], res$findings[res$findings$key == used$key[[i]], , drop = FALSE],
                   used$key[[i]] %in% excluded, ns)
    })),
    if (nrow(aside)) shiny::tags$details(
      shiny::tags$summary(sprintf("%d sources set aside", nrow(aside))),
      shiny::tags$ul(lapply(seq_len(nrow(aside)), function(i) shiny::tags$li(
        aside$title[[i]], shiny::tags$span(class = "small text-muted", paste(" -", why[[i]]))))))
  )
}

# A selected source (one row) and its findings, as a list item: the title
# (linked when the link is https), year, tier, relevance and how it was read,
# the screener's reason, then each finding with its quote and page; and a
# button to exclude the source, or include it again.
.source_item <- function(r, findings, excluded, ns) {
  quote <- function(j) {
    page <- if (is.na(findings$page[[j]])) "" else paste0(" (p. ", findings$page[[j]], ")")
    shiny::tags$em(class = "small", paste0("\"", findings$quote[[j]], "\"", page))
  }
  # the button sends the source's key (JSON: quotes in it stay inside the string)
  input <- if (excluded) "include" else "exclude"
  button <- shiny::tags$button(
    type = "button", class = "btn btn-xs btn-outline-secondary ml-2",
    onclick = sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'})",
                      jsonlite::toJSON(ns(input), auto_unbox = TRUE),
                      jsonlite::toJSON(r$key, auto_unbox = TRUE)),
    if (excluded) "Include again" else "Exclude")
  shiny::tags$li(
    class = if (excluded) "text-muted",
    if (excluded) shiny::tags$strong("Excluded: "),
    if (grepl("^https://", r$url)) {  # FALSE for a missing (NA) link
      shiny::tags$a(r$title, href = r$url, target = "_blank")
    } else r$title,
    sprintf(" (%s) - tier %s, %s relevance; %s", r$year, r$tier, r$relevance, gsub("_", " ", r$access)),
    button,
    if (!is.na(r$relevance_reason)) shiny::tags$div(class = "small text-muted", r$relevance_reason),
    if (nrow(findings)) shiny::tags$ul(lapply(seq_len(nrow(findings)), function(j) {
      shiny::tags$li(findings$finding[[j]], shiny::tags$br(), quote(j))
    }))
  )
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
