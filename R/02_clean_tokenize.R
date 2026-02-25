#' Clean text with configurable options
#'
#' @param x character vector
#' @param lowercase logical
#' @param remove_numbers logical
#' @param remove_punct logical
#' @return character vector
#' @export
clean_text <- function(x, lowercase = TRUE, remove_numbers = FALSE, remove_punct = TRUE) {
  stop("TODO: implement clean_text()")
}

#' Tokenize text
#'
#' @param x character vector
#' @param keep_hyphen logical
#' @return list of character vectors (tokens)
#' @export
tokenize_text <- function(x, keep_hyphen = TRUE) {
  stop("TODO: implement tokenize_text()")
}
