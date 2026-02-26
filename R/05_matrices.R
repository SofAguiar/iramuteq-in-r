#' Build a sparse UCE×term matrix
#'
#' Accepts either:
#' - a tibble/data.frame with columns: uce_id + text_uce (will be cleaned + tokenized)
#' - a long table with columns: uce_id + term (+ n optional)
#'
#' @param uce_tokens tibble/data.frame with UCE content or long token table
#' @param weighting 'count' or 'binary'
#' @param lowercase passed to clean_text() when text_uce is provided
#' @param remove_numbers passed to clean_text()
#' @param remove_punct passed to clean_text()
#' @param min_term_len minimum token length to keep
#' @return A Matrix::dgCMatrix with rownames = uce_id and colnames = terms
#' @export
build_uce_term_matrix <- function(uce_tokens,
                                  weighting = c("count", "binary"),
                                  lowercase = TRUE,
                                  remove_numbers = FALSE,
                                  remove_punct = TRUE,
                                  min_term_len = 1) {
  weighting <- match.arg(weighting)
  stopifnot(is.numeric(min_term_len), length(min_term_len) == 1, min_term_len >= 1)
  
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
    
    long <- data.frame(
      uce_id = rep(df$uce_id, times = lengths(toks_list)),
      term   = unlist(toks_list, use.names = FALSE),
      n      = 1L,
      stringsAsFactors = FALSE
    )
    
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
  
  # Apply weighting
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
  
  # groups×terms
  LT <- Matrix::t(G) %*% uce_term_matrix
  methods::as(LT, "dgCMatrix")
}