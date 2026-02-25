#' Build sparse term matrix from UCE tokens/lemmas
#'
#' @param uce_tokens data.table/tibble mapping uce_id -> terms
#' @param weighting character, e.g., 'count' or 'binary'
#' @return A Matrix::dgCMatrix
#' @export
build_uce_term_matrix <- function(uce_tokens, weighting = c("count", "binary")) {
  stop("TODO: implement build_uce_term_matrix()")
}

#' Build lexical tables by grouping variables (etoiles)
#'
#' @param uce_term_matrix dgCMatrix
#' @param groups factor/character vector aligned to UCE rows
#' @return list with table + metadata
#' @export
build_lexical_table <- function(uce_term_matrix, groups) {
  stop("TODO: implement build_lexical_table()")
}
