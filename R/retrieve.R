# ---
# Guidance retrieval for the AI Chat: the passages that bear on a question.
#
# Sending the whole guidance note with every question costs ~34k tokens, more
# than mAI DEV allows in a minute. Instead each question gets the 12 passages
# that rank best by keywords (BM25) and - when the guidance has saved
# embeddings (data-raw/guidance_embeddings.R) and mAI can embed the question -
# by meaning, the two rankings fused by reciprocal rank fusion.
# ---

# Common English words that say nothing about a passage's topic.
.STOP_WORDS <- c(
  "a", "about", "after", "all", "also", "an", "and", "any", "are", "as", "at", "be",
  "been", "being", "but", "by", "can", "could", "did", "do", "does", "for", "from",
  "had", "has", "have", "how", "i", "if", "in", "into", "is", "it", "its", "may",
  "me", "more", "most", "my", "no", "not", "of", "on", "or", "other", "our", "should",
  "so", "such", "than", "that", "the", "their", "them", "then", "there", "these",
  "they", "this", "those", "to", "under", "was", "we", "were", "what", "when",
  "where", "which", "while", "who", "why", "will", "with", "would", "you", "your")

# Lower-case content words of each string, each cut to a rough stem.
.tokens <- function(x) {
  w <- strsplit(tolower(x), "[^[:alnum:]]+")
  lapply(w, function(v) .stem(v[nzchar(v) & !v %in% .STOP_WORDS]))
}

# Drop one common ending so that word forms meet: "declarations" ->
# "declaration", "publishing" / "published" -> "publish", "rules" / "rule" ->
# "rul". At least three letters stay; "-ss" keeps its s ("process").
.stem <- function(w) {
  done <- logical(length(w))
  for (suf in c("ing", "ed", "es", "s", "e")) {
    hit <- !done & endsWith(w, suf) & nchar(w) - nchar(suf) >= 3L &
      !(suf == "s" & endsWith(w, "ss"))
    w[hit] <- substr(w[hit], 1L, nchar(w[hit]) - nchar(suf))
    done <- done | hit
  }
  w
}

# Okapi BM25 score of each document (a list of token vectors) for the query.
.bm25 <- function(docs, query, k1 = 1.2, b = 0.75) {
  q <- unique(query)
  n <- length(docs)
  len <- lengths(docs)
  df <- vapply(q, function(t) sum(vapply(docs, function(d) t %in% d, logical(1))), numeric(1))
  idf <- log(1 + (n - df + 0.5) / (df + 0.5))
  vapply(seq_len(n), function(i) {
    tf <- vapply(q, function(t) sum(docs[[i]] == t), numeric(1))
    sum(idf * tf * (k1 + 1) / (tf + k1 * (1 - b + b * len[[i]] / mean(len))))
  }, numeric(1))
}

# Reciprocal rank fusion: each ranking (a vector of ids, best first) gives an
# id 1 / (k + rank); the ids come back by their summed score.
#' Fingerprint of a set of guidance passages
#'
#' A hash of the passage ids and texts. It is saved with the passages'
#' embeddings, and [load_guidance()] uses the embeddings only while it still
#' matches, so vectors computed for other passages are never used.
#'
#' @param passages The `passages` data frame of [load_guidance()].
#' @return Character scalar.
#' @keywords internal
guidance_fingerprint <- function(passages) rlang::hash(passages[c("id", "text")])

# The saved passage embeddings in `file`, or NULL when there are none or they
# were computed for other passages (logged).
.read_embeddings <- function(file, passages) {
  if (!file.exists(file)) return(NULL)
  e <- readRDS(file)
  if (!identical(e$fingerprint, guidance_fingerprint(passages))) {
    message("[cgjrapp] guidance embeddings in ", basename(file), " are out of date ",
            "(the passages changed); using keyword matching only. Rebuild them with ",
            "data-raw/guidance_embeddings.R.")
    return(NULL)
  }
  e
}

.rrf <- function(rankings, k = 60) {
  ids <- unlist(rankings, use.names = FALSE)
  if (!length(ids)) return(character(0))
  rank <- unlist(lapply(rankings, seq_along), use.names = FALSE)
  score <- tapply(1 / (k + rank), ids, sum)
  names(sort(score, decreasing = TRUE))
}

#' Find the guidance passages that bear on a question
#'
#' Ranks the passages by keywords (BM25 over each passage's section title and
#' text, with stop words dropped and simple word endings stripped) and, when
#' `query_vec` is given and the guidance carries saved embeddings, by cosine
#' similarity of meaning. The top `candidates` of each ranking are fused by
#' reciprocal rank fusion. With keywords only, a passage must share at least
#' one content word with the question; a question nothing matches gets no
#' passages.
#'
#' @param guidance Output of [load_guidance()]; `guidance$embeddings$vectors`
#'   (a matrix, one row per passage id) is used when present.
#' @param query Character scalar: the question (with the previous one, for
#'   follow-ups).
#' @param k Maximum number of passages returned.
#' @param query_vec Numeric vector: the question's embedding, or `NULL` for
#'   keywords only.
#' @param candidates How many passages of each ranking enter the fusion.
#' @return Rows of `guidance$passages`, best first; at most `k`.
#' @keywords internal
retrieve_guidance <- function(guidance, query, k = 12L, query_vec = NULL, candidates = 30L) {
  p <- guidance$passages
  score <- .bm25(.tokens(paste(p$title, p$text)), .tokens(query)[[1]])
  by_words <- p$id[order(-score)][sort(score, decreasing = TRUE) > 0]
  rankings <- list(utils::head(by_words, candidates))
  v <- guidance$embeddings$vectors
  if (!is.null(v) && !is.null(query_vec)) {
    sim <- drop(v %*% query_vec) / (sqrt(rowSums(v^2)) * sqrt(sum(query_vec^2)))
    rankings <- c(rankings, list(utils::head(rownames(v)[order(-sim)], candidates)))
  }
  ids <- .rrf(rankings)
  ids <- utils::head(ids[ids %in% p$id], k)
  p[match(ids, p$id), , drop = FALSE]
}
