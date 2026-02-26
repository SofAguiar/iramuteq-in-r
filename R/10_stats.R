#' Compute corpus statistics (term frequencies, hapax, density)
#'
#' @param uce_term_matrix dgCMatrix
#' @param terms optional term vector (defaults to colnames(matrix))
#' @return list with totals, term_freq, hapax_terms, summary
#' @export
corpus_stats <- function(uce_term_matrix, terms = colnames(uce_term_matrix)) {
  stopifnot(inherits(uce_term_matrix, "Matrix"))
  
  tf <- Matrix::colSums(uce_term_matrix)
  tf <- as.numeric(tf)
  names(tf) <- terms
  
  total_occurrences <- sum(tf)
  n_uce <- nrow(uce_term_matrix)
  n_terms <- ncol(uce_term_matrix)
  
  hapax <- names(tf)[tf == 1]
  
  out <- list(
    totals = list(
      n_uce = n_uce,
      n_terms = n_terms,
      total_occurrences = total_occurrences
    ),
    term_freq = sort(tf, decreasing = TRUE),
    hapax_terms = hapax,
    summary = data.frame(
      metric = c("n_uce", "n_terms", "total_occurrences", "n_hapax"),
      value = c(n_uce, n_terms, total_occurrences, length(hapax)),
      stringsAsFactors = FALSE
    )
  )
  
  out
}