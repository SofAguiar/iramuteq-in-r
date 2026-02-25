#' Read an IRaMuTeQ-style corpus file (UCI/UCE with etoiles)
#'
#' @param file Path to corpus text file.
#' @param encoding Encoding (e.g., "UTF-8", "latin1").
#' @param ucimark How UCI is marked. Usually "****" lines. Alternative modes can be supported.
#' @return A tibble with at least: uci_id, raw_meta, text.
#' @export
read_corpus_iramuteq <- function(file, encoding = "UTF-8", ucimark = "****") {
  stop("TODO: implement read_corpus_iramuteq()")
}
