#' Build similitude graph (co-occurrence / similarity)
#'
#' @param uce_term_matrix dgCMatrix
#' @param min_freq minimum term freq
#' @param measure similarity measure (e.g., 'jaccard')
#' @return igraph object + edge/vertex data
#' @export
similitude_graph <- function(uce_term_matrix, min_freq = 5, measure = c("jaccard")) {
  stop("TODO: implement similitude_graph()")
}
