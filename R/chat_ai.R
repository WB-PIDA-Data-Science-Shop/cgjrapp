# ---
# AI Chat: questions answered from the CGJR guidance and the dashboard data.
#
# The system prompt carries the rules, the guidance's sections and reference
# list, and - when a single base country is applied - the evidence pack of the
# selection (ids [E..]). Each question comes with the guidance passages that
# bear on it (ids [G..], retrieve.R); once answered, the history keeps the
# question without them. It must cite every sourced statement, keep anything
# else in a labelled "Beyond the guidance (general knowledge, not verified)"
# block, say plainly when the sources don't answer a question, and push back
# on claims they don't support. Each reply is then checked by verify_text()
# and shown with its citations as badges and a verification footer.
# ---

.chat_system_prompt <- function(guidance, evidence = NULL, report = NULL) {
  refs <- guidance$refs
  ev_block <- if (is.null(evidence)) {
    "No dashboard selection is loaded (the user has not applied a single base country), so there are no [E] facts. Say so if asked about country data."
  } else {
    paste0(.selection_block(evidence$meta), "\n", .facts_lines(evidence$facts))
  }
  paste0(
    "You are the GOVCGJR assistant. You help World Bank country teams prepare the ",
    "Governance and Public Institutions chapter of a Country Growth and Jobs Report ",
    "(CGJR), answering questions from two sources:\n",
    "- GUIDANCE passages, cited [G...], from: ", guidance$label, ". The passages that ",
    "bear on each question come with the question; the guidance's sections are listed below.\n",
    "- EVIDENCE facts, cited [E...], computed by the GOVCGJR dashboard for the ",
    "current selection.\n\n",

    "Rules:\n",
    "1. Cite every statement that comes from the sources with its id(s) in square brackets ",
    "at the end of the sentence, e.g. [G2.2-1] or [E14, G2.2-3]. Cite only ids you have been ",
    "given in this conversation.\n",
    "2. Make the source of each statement explicit in the wording: 'The guidance ",
    "recommends ... [G..]' versus 'The dashboard shows ... [E..]'.\n",
    "3. Copy figures exactly as written in the evidence facts. Never compute, round or ",
    "convert them.\n",
    "4. Anything that is not in the sources goes at the very end, under the heading ",
    "'**Beyond the guidance (general knowledge, not verified):**', kept short. Do not name ",
    "studies, authors, URLs, datasets or statistics there unless the study is in the ",
    "guidance's reference list below.\n",
    "5. If the sources do not answer the question, say 'I can't find this in the guidance ",
    "or the dashboard data.' and, where useful, point to the guidance section or the data ",
    "sources it suggests. Never guess or invent.\n",
    "6. If the user states something the sources contradict or do not support, say so ",
    "politely and cite what the sources do say.\n",
    "7. The dashboard's composite figures (CTF, closeness to frontier) run 0-1, 1 being the ",
    "best performance observed globally; bands and ranks are relative to the comparison group.\n",
    "8. Be concise: short paragraphs or bullet points, Markdown.\n\n",

    "GUIDANCE REFERENCE LIST (the only studies you may name):\n",
    if (nrow(refs)) paste("-", refs$entry, collapse = "\n") else "(none)", "\n\n",
    "GUIDANCE SECTIONS (point to these when the passages you were given don't answer):\n",
    .guidance_contents(guidance$passages), "\n\n",
    "EVIDENCE:\n", ev_block,
    if (!is.null(report) && nzchar(report)) paste0(
      "\n\nCURRENT AI REPORT DRAFT (written earlier by you from the sources above; when you ",
      "use it, cite the [E]/[G] ids it cites, not the draft):\n", report)
  )
}

#' Render a chat reply with verified citations
#'
#' @param text The model's reply (Markdown).
#' @param evidence,guidance As for [verify_text()].
#' @return A list: `html` (an [htmltools::HTML()] string - the reply with each
#'   citation as a badge whose tooltip is its source, then a verification
#'   footer) and `verification` (output of [verify_text()]).
#' @keywords internal
chat_reply_html <- function(text, evidence, guidance) {
  v <- verify_text(text, evidence, guidance)
  ok <- nrow(v$issues) == 0L
  issues <- if (ok) "" else paste0(
    "<ul>", paste(sprintf("<li>%s</li>", htmltools::htmlEscape(utils::head(v$issues$detail, 5))),
                  collapse = ""), "</ul>")
  footer <- sprintf(
    '<div class="cgjr-verify %s"><strong>%s</strong> %s%s</div>',
    if (ok) "cgjr-verify-ok" else "cgjr-verify-warn",
    if (ok) "&#10003; Sources checked." else "&#9888; Check before use.",
    htmltools::htmlEscape(verification_summary(v)), issues)
  list(html = htmltools::HTML(paste0(.render_cited_html(text, evidence, guidance), footer)),
       verification = v)
}

# The message the model gets for one question: the guidance passages that
# bear on it, then the question.
.chat_question_message <- function(question, passages) {
  paste0("GUIDANCE PASSAGES FOR THIS QUESTION:\n", .guidance_block(passages),
         "\n\nQUESTION:\n", question)
}

# The passages for a question: keywords, plus meaning when the guidance has
# saved embeddings and mAI can embed the question. A failed embedding call
# falls back to keywords only, with a note in the log.
.chat_passages <- function(guidance, query) {
  qv <- NULL
  if (!is.null(guidance$embeddings) && .llm_provider() == "mai") {
    qv <- tryCatch(embed_texts(query)[1, ], error = function(e) {
      message("[cgjrapp] question embedding failed, using keyword matching only: ",
              strsplit(.plain_message(e), "\n", fixed = TRUE)[[1]][[1]])
      NULL
    })
  }
  retrieve_guidance(guidance, query, query_vec = qv)
}

.user_turns <- function(turns) which(vapply(turns, function(t) identical(t@role, "user"), logical(1)))

# The previous question in the chat history ("" if none).
.last_question <- function(chat) {
  turns <- chat$get_turns()
  users <- .user_turns(turns)
  if (length(users)) ellmer::contents_text(turns[[max(users)]]) else ""
}

# Once answered, the last question stays in the history without its passages,
# so later turns don't carry them again (~5k tokens each).
.forget_passages <- function(chat, question) {
  turns <- chat$get_turns()
  users <- .user_turns(turns)
  if (!length(users)) return(invisible())
  turns[[max(users)]] <- ellmer::UserTurn(list(ellmer::ContentText(question)))
  chat$set_turns(turns)
  invisible()
}

# One chat turn: stream the reply into the shinychat UI, then replace it with
# the verified, badge-rendered version. Transient provider errors are retried
# with backoff, with a note in the reply meanwhile. A promise for the
# verification.
.chat_turn <- function(chat, prompt, chat_id, evidence, guidance,
                       session = shiny::getDefaultReactiveDomain(),
                       delays = .RETRY_DELAYS) {
  send <- function(content, chunk, operation = "append") {
    shinychat::chat_append_message(chat_id, list(role = "assistant", content = content),
                                   chunk = chunk, operation = operation, session = session)
  }
  send("", "start")
  sent <- 0L
  # search with the previous question too, so a follow-up ("tell me more")
  # keeps the topic's passages
  query <- paste(.last_question(chat), prompt)
  p <- .stream_retry(
    make_chat = function() chat,
    prompt = .chat_question_message(prompt, .chat_passages(guidance, query)),
    on_chunk = function(acc) {
      if (sent == 0L && nzchar(acc)) send("", TRUE, "replace")  # drop a retry note
      delta <- substr(acc, sent + 1L, nchar(acc))
      sent <<- nchar(acc)
      if (nzchar(delta)) send(delta, TRUE)
    },
    on_retry = function(n, delay, cnd) {
      sent <<- 0L
      send(sprintf("_The AI service is busy (%s) - retrying in %d seconds..._",
                   .http_status(cnd) %||% "temporary error", round(delay)), TRUE, "replace")
    },
    delays = delays)
  promises::then(p,
    onFulfilled = function(text) {
      .forget_passages(chat, prompt)
      r <- chat_reply_html(text, evidence, guidance)
      send(r$html, "end", "replace")
      r$verification
    },
    onRejected = function(e) {
      send(paste0("**The assistant couldn't answer.** ", llm_error_message(e)), "end", "replace")
      NULL
    })
}
