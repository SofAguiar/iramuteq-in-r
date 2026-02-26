#' Build a sparse UCE×term matrix
#'
#' Accepts either:
#' - a tibble/data.frame with columns: uce_id + text_uce (will be cleaned + tokenized)
#' - a long table with columns: uce_id + term (+ n optional)
#'
#' If `term_source = "lemma"`, you must provide `lexique` (from read_lexique()).
#'
#' @param uce_tokens tibble/data.frame with UCE content or long token table
#' @param weighting 'count' or 'binary'
#' @param term_source 'token' or 'lemma' (requires lexique)
#' @param lexique lexique tibble/data.frame used for lemmatization (required if term_source="lemma")
#' @param drop_pos POS tags to drop when term_source="lemma" (default: c("sw"))
#' @param keep_pos POS tags to keep (NULL means keep everything except drop_pos)
#' @param lowercase passed to clean_text() when text_uce is provided
#' @param remove_numbers passed to clean_text()
#' @param remove_punct passed to clean_text()
#' @param min_term_len minimum token/lemma length to keep
#' @return A Matrix::dgCMatrix with rownames = uce_id and colnames = terms
#' @export
build_uce_term_matrix <- function(uce_tokens,
                                  weighting = c("count", "binary"),
                                  term_source = c("token", "lemma"),
                                  lexique = NULL,
                                  drop_pos = c("sw"),
                                  keep_pos = NULL,
                                  lowercase = TRUE,
                                  remove_numbers = FALSE,
                                  remove_punct = TRUE,
                                  min_term_len = 1) {
  weighting <- match.arg(weighting)
  term_source <- match.arg(term_source)
  stopifnot(is.numeric(min_term_len), length(min_term_len) == 1, min_term_len >= 1)
  
  if (term_source == "lemma" && is.null(lexique)) {
    stop("term_source = 'lemma' requires `lexique` (use read_lexique()).")
  }
  
  # ---- Case A: input is UCE table with raw text (uce_id + text_uce) ----
  if (is.data.frame(uce_tokens) && all(c("uce_id", "text_uce") %in% names(uce_tokens))) {
    df <- tibble::as_tibble(uce_tokens[, c("uce_id", "text_uce")])
    df$uce_id <- as.integer(df$uce_id)
    df$text_uce <- as.character(df$text_uce)
    
    all_uce_ids <- df$uce_id
    
    cleaned <- clean_text(
      df$text_uce,
      lowercase = lowercase,
      remove_numbers = remove_numbers,
      remove_punct = remove_punct
    )
    
    toks_list <- tokenize_text(cleaned, keep_hyphen = TRUE)
    
    if (term_source == "token") {
      long <- data.frame(
        uce_id = rep(df$uce_id, times = lengths(toks_list)),
        term   = unlist(toks_list, use.names = FALSE),
        n      = 1L,
        stringsAsFactors = FALSE
      )
    } else {
      # Lemma mode: lemmatize per-UCE but keep uce_id association
      uce_id_rep <- rep(df$uce_id, times = lengths(toks_list))
      lem <- lemmatize_tokens(toks_list, lexique)
      
      # Align lemmatized rows with uce_id_rep
      if (nrow(lem) != length(uce_id_rep)) {
        stop("Internal error: token/lemma alignment mismatch.")
      }
      
      # POS filtering
      pos <- as.character(lem$pos)
      keep <- rep(TRUE, length(pos))
      
      if (!is.null(drop_pos) && length(drop_pos) > 0) {
        keep <- keep & !(pos %in% drop_pos)
      }
      if (!is.null(keep_pos) && length(keep_pos) > 0) {
        keep <- keep & (pos %in% keep_pos)
      }
      
      long <- data.frame(
        uce_id = uce_id_rep[keep],
        term   = as.character(lem$lemma)[keep],
        n      = 1L,
        stringsAsFactors = FALSE
      )
    }
    
    # ---- Case B: input is already in long format (uce_id + term [+ n]) ----
  } else if (is.data.frame(uce_tokens) && all(c("uce_id", "term") %in% names(uce_tokens))) {
    long <- as.data.frame(uce_tokens, stringsAsFactors = FALSE)
    long$uce_id <- as.integer(long$uce_id)
    long$term <- as.character(long$term)
    if (!("n" %in% names(long))) long$n <- 1L
    all_uce_ids <- sort(unique(long$uce_id))
    
  } else {
    stop("Unsupported input. Provide (uce_id + text_uce) or (uce_id + term [+ n]).")
  }
  
  # ---- Filtering ----
  long <- long[!is.na(long$term) & nzchar(long$term), , drop = FALSE]
  if (min_term_len > 1) long <- long[nchar(long$term) >= min_term_len, , drop = FALSE]
  
  # If nothing remains, return an empty matrix with correct number of rows
  if (nrow(long) == 0) {
    return(Matrix::sparseMatrix(
      i = integer(), j = integer(), x = numeric(),
      dims = c(length(all_uce_ids), 0),
      dimnames = list(as.character(all_uce_ids), character(0))
    ))
  }
  
  # ---- Aggregate counts: n ~ uce_id + term ----
  long$n <- as.integer(long$n)
  agg <- stats::aggregate(n ~ uce_id + term, data = long, FUN = sum)
  agg$uce_id <- as.integer(agg$uce_id)
  agg$term <- as.character(agg$term)
  agg$n <- as.numeric(agg$n)
  
  if (weighting == "binary") {
    agg$n <- 1
  }
  
  # ---- Build sparse matrix ----
  uce_levels <- sort(unique(all_uce_ids))
  term_levels <- sort(unique(agg$term))
  
  i <- match(agg$uce_id, uce_levels)
  j <- match(agg$term, term_levels)
  x <- agg$n
  
  M <- Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(length(uce_levels), length(term_levels)),
    dimnames = list(as.character(uce_levels), term_levels)
  )
  
  methods::as(M, "dgCMatrix")
}

#' Build a lexical table (groups×terms) by summing UCE rows within groups
#'
#' @param uce_term_matrix dgCMatrix (UCE×terms)
#' @param groups factor/character vector aligned to UCE rows (same length as nrow(matrix))
#' @return A dgCMatrix with rows = groups and cols = terms
#' @export
build_lexical_table <- function(uce_term_matrix, groups) {
  stopifnot(inherits(uce_term_matrix, "Matrix"))
  stopifnot(length(groups) == nrow(uce_term_matrix))
  
  g <- as.factor(groups)
  
  # Incidence matrix: UCE×group
  G <- Matrix::sparseMatrix(
    i = seq_along(g),
    j = as.integer(g),
    x = 1,
    dims = c(length(g), nlevels(g)),
    dimnames = list(rownames(uce_term_matrix), levels(g))
  )
  
  LT <- Matrix::t(G) %*% uce_term_matrix
  methods::as(LT, "dgCMatrix")
}