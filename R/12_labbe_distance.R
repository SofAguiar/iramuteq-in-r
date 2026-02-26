#' Compute Labbé intertextual distance between groups
#'
#' This implementation uses a transparent, bounded distance derived from
#' L1 distance between relative frequency profiles:
#'   D(a,b) = 0.5 * sum_i |p_ai - p_bi|
#' where p_ai are relative frequencies of term i in group a.
#'
#' Range: [0, 1] when using relative frequencies.
#'
#' @param lexical_table matrix-like (groups × terms) of term frequencies.
#'   Can be base matrix or Matrix.
#' @param method currently only "labbe_l1" (reserved for extension).
#' @param hclust_method linkage method for hierarchical clustering.
#' @return list with dist (dist object), matrix (distance matrix),
#'   hclust (hclust object), profiles (relative frequency matrix).
#' @export
labbe_distance <- function(lexical_table,
                           method = c("labbe_l1"),
                           hclust_method = "ward.D2") {
  method <- match.arg(method)
  
  # Normalize
  if (inherits(lexical_table, "Matrix")) {
    X <- as.matrix(lexical_table)
  } else {
    X <- as.matrix(lexical_table)
  }
  storage.mode(X) <- "numeric"
  
  if (is.null(rownames(X))) rownames(X) <- paste0("G", seq_len(nrow(X)))
  if (is.null(colnames(X))) colnames(X) <- paste0("T", seq_len(ncol(X)))
  
  # Relative frequency profiles per group
  rs <- rowSums(X)
  if (any(rs <= 0)) stop("labbe_distance: at least one group has zero total frequency.")
  P <- X / rs
  
  # Pairwise distance matrix: 0.5 * L1
  G <- nrow(P)
  D <- matrix(0, nrow = G, ncol = G, dimnames = list(rownames(P), rownames(P)))
  
  for (i in seq_len(G)) {
    for (j in i:G) {
      d <- 0.5 * sum(abs(P[i, ] - P[j, ]))
      D[i, j] <- d
      D[j, i] <- d
    }
  }
  
  d_obj <- stats::as.dist(D)
  hc <- stats::hclust(d_obj, method = hclust_method)
  
  list(
    dist = d_obj,
    matrix = D,
    hclust = hc,
    profiles = P
  )
}