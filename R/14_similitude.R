#' Build a similitude graph from an UCE×term matrix
#'
#' The graph is undirected over terms. Edges connect terms that co-occur within
#' the same UCE at least min_cooc times. Optionally, edge weights can be Jaccard.
#'
#' @param uce_term_matrix dgCMatrix (UCE×terms).
#' @param min_freq minimum global term frequency to keep a term.
#' @param min_cooc minimum co-occurrence count to keep an edge.
#' @param weight "cooc" or "jaccard".
#' @param top_terms optionally keep only top N terms by frequency (NULL = keep all).
#' @return list with igraph graph, term_freq, cooc_matrix (dense matrix for kept terms).
#' @export
similitude_graph <- function(uce_term_matrix,
                             min_freq = 5,
                             min_cooc = 2,
                             weight = c("cooc", "jaccard"),
                             top_terms = NULL) {
  weight <- match.arg(weight)
  stopifnot(inherits(uce_term_matrix, "Matrix"))
  
  M <- methods::as(uce_term_matrix, "dgCMatrix")
  
  tf <- as.numeric(Matrix::colSums(M))
  names(tf) <- colnames(M)
  
  keep <- tf >= min_freq
  if (!any(keep)) stop("similitude_graph: no terms left after min_freq filtering.")
  
  # Optionally top N
  if (!is.null(top_terms)) {
    ord <- order(tf, decreasing = TRUE)
    top_idx <- ord[seq_len(min(top_terms, length(ord)))]
    keep2 <- rep(FALSE, length(tf))
    keep2[top_idx] <- TRUE
    keep <- keep & keep2
    if (!any(keep)) stop("similitude_graph: no terms left after top_terms filtering.")
  }
  
  M2 <- M[, keep, drop = FALSE]
  tf2 <- as.numeric(Matrix::colSums(M2))
  terms <- colnames(M2)
  
  # Binarize presence/absence
  B <- M2
  B@x <- rep(1, length(B@x))
  
  # Co-occurrence counts (term×term)
  C <- as.matrix(Matrix::crossprod(B))
  diag(C) <- 0
  
  # Build edges
  idx <- which(C >= min_cooc, arr.ind = TRUE)
  idx <- idx[idx[,1] < idx[,2], , drop = FALSE]  # upper triangle only
  
  if (nrow(idx) == 0) {
    g <- igraph::make_empty_graph(n = length(terms), directed = FALSE)
    g <- igraph::set_vertex_attr(g, "name", value = terms)
    g <- igraph::set_vertex_attr(g, "freq", value = tf2)
    return(list(graph = g, term_freq = tf2, cooc_matrix = C))
  }
  
  cooc <- C[idx]
  if (weight == "jaccard") {
    # J = cooc / (f_i + f_j - cooc)
    fi <- tf2[idx[,1]]
    fj <- tf2[idx[,2]]
    w <- cooc / pmax(fi + fj - cooc, 1e-12)
  } else {
    w <- cooc
  }
  
  edges <- data.frame(
    from = terms[idx[,1]],
    to = terms[idx[,2]],
    weight = as.numeric(w),
    cooc = as.numeric(cooc),
    stringsAsFactors = FALSE
  )
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = terms))
  g <- igraph::set_vertex_attr(g, "freq", value = tf2[igraph::V(g)$name])
  
  list(
    graph = g,
    term_freq = tf2,
    cooc_matrix = C
  )
}