#' Reinert / CHD-like classification (transparent, R-only)
#'
#' This is a deterministic, auditable divisive clustering inspired by CHD workflows:
#' - Start with all UCEs in one class
#' - Repeatedly split one class into two using the first CA dimension on the class submatrix
#' - Choose the split that maximizes chi-square association gain
#'
#' This is a pragmatic R-only baseline that can be replaced later by a closer port.
#'
#' @param uce_term_matrix dgCMatrix (UCE×terms) with non-negative counts.
#' @param min_term_freq minimum global frequency to keep a term.
#' @param max_classes maximum number of classes to produce.
#' @param min_class_size minimum UCEs per class.
#' @param seed RNG seed (used only for tie-breaking).
#' @return list with:
#'   - classes: integer vector of class labels (length = nrow(matrix))
#'   - class_sizes: table of sizes
#'   - splits: split log
#'   - top_terms: per-class top terms by signed chi2 residual vs rest
#' @export
reinert_chd <- function(uce_term_matrix,
                        min_term_freq = 5,
                        max_classes = 6,
                        min_class_size = 20,
                        seed = 123) {
  stopifnot(inherits(uce_term_matrix, "Matrix"))
  M <- methods::as(uce_term_matrix, "dgCMatrix")
  if (any(M@x < 0)) stop("reinert_chd: counts must be non-negative.")
  
  set.seed(seed)
  
  # Filter terms by global frequency
  tf <- Matrix::colSums(M)
  keep <- as.numeric(tf) >= min_term_freq
  M <- M[, keep, drop = FALSE]
  if (ncol(M) == 0) stop("reinert_chd: no terms left after filtering. Lower min_term_freq.")
  
  n_uce <- nrow(M)
  classes <- rep(1L, n_uce)
  next_label <- 2L
  split_log <- list()
  
  # Helper: chi-square association for a 2×V table
  chisq_gain <- function(A, B) {
    # A and B are term count vectors for each side
    T2 <- rbind(A, B)
    rs <- rowSums(T2)
    cs <- colSums(T2)
    N <- sum(cs)
    E <- outer(rs, cs, FUN = function(a, b) a * b / N)
    E <- pmax(E, .Machine$double.eps)
    sum((T2 - E)^2 / E)
  }
  
  # Helper: CA first dimension row scores for a UCE×term contingency table
  ca_first_dim <- function(X) {
    Xd <- as.matrix(X)
    storage.mode(Xd) <- "numeric"
    N <- sum(Xd)
    if (N <= 0) return(rep(0, nrow(Xd)))
    
    P <- Xd / N
    r <- rowSums(P)
    c <- colSums(P)
    
    keep_r <- r > 0
    keep_c <- c > 0
    P2 <- P[keep_r, keep_c, drop = FALSE]
    r2 <- r[keep_r]
    c2 <- c[keep_c]
    
    rc <- outer(r2, c2)
    Dr_inv_sqrt <- diag(1 / sqrt(r2), nrow = length(r2))
    Dc_inv_sqrt <- diag(1 / sqrt(c2), nrow = length(c2))
    S <- Dr_inv_sqrt %*% (P2 - rc) %*% Dc_inv_sqrt
    
    sv <- svd(S)
    if (length(sv$d) == 0) return(rep(0, nrow(P)))
    
    # Row principal coord for dim1: Dr^{-1/2} U d
    u <- sv$u[, 1]
    d1 <- sv$d[1]
    f1_small <- (1 / sqrt(r2)) * u * d1
    
    # Map back to all rows (including any zero-mass rows)
    f1 <- rep(0, nrow(P))
    f1[which(keep_r)] <- f1_small
    f1
  }
  
  # Main loop: split until max_classes reached or no valid split
  while (length(unique(classes)) < max_classes) {
    current_labels <- sort(unique(classes))
    
    best <- list(gain = -Inf)
    
    for (lab in current_labels) {
      idx <- which(classes == lab)
      if (length(idx) < 2 * min_class_size) next
      
      subM <- M[idx, , drop = FALSE]
      
      # Row scores from CA dim1
      scores <- ca_first_dim(subM)
      
      # Split by median score
      med <- stats::median(scores, na.rm = TRUE)
      left <- idx[scores <= med]
      right <- idx[scores > med]
      
      # Enforce min size
      if (length(left) < min_class_size || length(right) < min_class_size) next
      
      # Compute gain using term totals in each side
      A <- as.numeric(Matrix::colSums(M[left, , drop = FALSE]))
      B <- as.numeric(Matrix::colSums(M[right, , drop = FALSE]))
      gain <- chisq_gain(A, B)
      
      # Tie-breaker: prefer splitting larger class
      if (gain > best$gain + 1e-12 ||
          (abs(gain - best$gain) <= 1e-12 && length(idx) > best$class_size)) {
        best <- list(
          gain = gain,
          lab = lab,
          left = left,
          right = right,
          class_size = length(idx),
          median = med
        )
      }
    }
    
    if (!is.finite(best$gain)) break
    
    # Apply best split: keep old label for left, assign new label to right
    classes[best$right] <- next_label
    
    split_log[[length(split_log) + 1]] <- list(
      split_from = best$lab,
      split_to = next_label,
      gain = best$gain,
      left_size = length(best$left),
      right_size = length(best$right)
    )
    
    next_label <- next_label + 1L
  }
  
  # Per-class top terms by signed standardized residual vs rest
  top_terms <- .reinert_top_terms(M, classes, top_n = 30)
  
  list(
    classes = classes,
    class_sizes = as.data.frame(table(classes), stringsAsFactors = FALSE),
    splits = split_log,
    top_terms = top_terms
  )
}

.reinert_top_terms <- function(M, classes, top_n = 30) {
  labs <- sort(unique(classes))
  terms <- colnames(M)
  out <- list()
  
  for (lab in labs) {
    in_idx <- which(classes == lab)
    out_idx <- which(classes != lab)
    A <- as.numeric(Matrix::colSums(M[in_idx, , drop = FALSE]))
    B <- as.numeric(Matrix::colSums(M[out_idx, , drop = FALSE]))
    T2 <- rbind(A, B)
    rs <- rowSums(T2)
    cs <- colSums(T2)
    N <- sum(cs)
    E <- outer(rs, cs, FUN = function(a, b) a * b / N)
    E <- pmax(E, .Machine$double.eps)
    Z <- (T2 - E) / sqrt(E)
    
    # Signed score for class side (row 1)
    score <- Z[1, ]
    ord <- order(abs(score), decreasing = TRUE)
    sel <- ord[seq_len(min(top_n, length(ord)))]
    
    out[[as.character(lab)]] <- tibble::tibble(
      class = lab,
      term = terms[sel],
      score = as.numeric(score[sel]),
      observed = as.numeric(A[sel]),
      expected = as.numeric(E[1, sel]),
      direction = ifelse(score[sel] >= 0, "over", "under")
    )
  }
  
  dplyr::bind_rows(out)
}