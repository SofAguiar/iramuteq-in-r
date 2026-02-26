#' Segment cleaned text into UCEs
#'
#' @param text character vector (one per UCI or paragraph)
#' @param method segmentation method: 'char' or 'token'
#' @param uce_size target segment size (chars or tokens, depending on method)
#' @param douce logical, whether to try splitting at punctuation boundaries
#' @return tibble with columns: uce_id, uci_id, text_uce
#' @export
segment_uce <- function(text, method = c("char", "token"), uce_size = 40, douce = TRUE) {
  stop("TODO: implement segment_uce()")
}
