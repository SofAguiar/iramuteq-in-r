#' Compute specificities (hypergeometric or chi2) and correspondence analysis (CA)
#'
#' @param lexical_table matrix/data.frame (groups x terms)
#' @param method 'hypergeo' or 'chi2'
#' @param nd number of CA dimensions
#' @return list with specificities + CA results
#' @export
specificities_ca <- function(lexical_table, method = c("hypergeo", "chi2"), nd = 3) {
  stop("TODO: implement specificities_ca()")
}
