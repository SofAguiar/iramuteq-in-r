#' Read IRaMuTeQ lexique file (form -> lemma, pos)
#'
#' @param file Path to lexique_*.txt
#' @param encoding Encoding
#' @return data.table with columns: form, lemma, pos, extra...
#' @export
read_lexique <- function(file, encoding = "UTF-8") {
  stop("TODO: implement read_lexique()")
}

#' Lemmatize tokens using lexique
#'
#' @param tokens list of token vectors
#' @param lexique data.table from read_lexique()
#' @return data.table with at least: token, lemma, pos, is_known
#' @export
lemmatize_tokens <- function(tokens, lexique) {
  stop("TODO: implement lemmatize_tokens()")
}
