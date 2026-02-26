#' Compute specificities and Correspondence Analysis (CA)
#'
#' This function takes a contingency table (groups × terms), typically produced by
#' build_lexical_table(), and computes:
#' - Specificities by either:
#'   - "hypergeo": hypergeometric tail probability (signed score = ± -log10(p))
#'   - "chi2": Pearson standardized residuals (signed)
#' - Correspondence Analysis (CA) using an SVD-based implementation (no external packages).
#'
#' Notes on hypergeometric specificities:
#' - Let k = observed count of a term in a group
#' - Let K = corpus-wide count of the term
#' - Let n = total tokens in the group
#' - Let N = total tokens in the corpus
#' - Over-representation p-value: P(X >= k) via phyper(k-1, K, N-K, n, lower.tail=FALSE)
#' - Under-representation p-value: P(X <= k) via phyper(k,   K, N-K, n, lower.tail=TRUE)
#' - Score = sign(k - expected) * -log10(p)
#'
#' @param lexical_table matrix-like contingency table (groups × terms).
#'   Can be a base matrix or a Matrix::dgCMatrix.
#' @param method "hypergeo" or "chi2".
#' @param nd number of CA dimensions to return.
#' @param min_term_total minimum total frequency for a term to be included.
#' @param min_group_total minimum total frequency for a group to be included.
#' @param top_n number of top terms to keep per group in the returned summary tables.
#' @param eps numeric floor to avoid log(0).
#' @return A list with:
#'   - specificities_long: tibble (group, term, observed, expected, score, p_value, direction)
#'   - specificities_top:  tibble with top_n terms per group by |score|
#'   - ca: list (eigenvalues, rowcoord, colcoord, row_masses, col_masses)
#'   - table: filtered contingency table used for computations
#' @export
specificities_ca <- function(lexical_table,
                             method = c("hypergeo", "chi2"),
                             nd = 3,
                             min_term_total = 1,
                             min_group_total = 1,
                             top_n = 30,
                             eps = 1e-300) {
  method <- match.arg(method)
  stopifnot(is.numeric(nd), nd >= 1)
  stopifnot(is.numeric(min_term_total), min_term_total >= 1)
  stopifnot(is.numeric(min_group_total), min_group_total >= 1)
  stopifnot(is.numeric(top_n), top_n >= 1)
  stopifnot(is.numeric(eps), eps > 0)
  
  # ---- Normalize input to a numeric matrix-like object ----
  if (inherits(lexical_table, "Matrix")) {
    # Ensure numeric sparse matrix safely (storage.mode() does NOT work for Matrix S4 objects)
    X <- methods::as(lexical_table, "dgCMatrix")
    if (!is.numeric(X@x)) X@x <- as.numeric(X@x)
  } else {
    X <- as.matrix(lexical_table)
    storage.mode(X) <- "numeric"
  }
  
  # Ensure row/col names
  if (is.null(rownames(X))) rownames(X) <- paste0("G", seq_len(nrow(X)))
  if (is.null(colnames(X))) colnames(X) <- paste0("T", seq_len(ncol(X)))
  
  # ---- Filter empty/rare rows/cols ----
  row_tot <- as.numeric(Matrix::rowSums(X))
  col_tot <- as.numeric(Matrix::colSums(X))
  
  keep_rows <- row_tot >= min_group_total
  keep_cols <- col_tot >= min_term_total
  
  X <- X[keep_rows, keep_cols, drop = FALSE]
  
  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("After filtering, lexical_table has no rows/cols. Relax min_term_total/min_group_total.")
  }
  
  row_tot <- as.numeric(Matrix::rowSums(X))
  col_tot <- as.numeric(Matrix::colSums(X))
  N <- sum(col_tot)
  
  # ---- Expected counts under independence ----
  # E[i,j] = row_tot[i] * col_tot[j] / N
  E <- outer(row_tot, col_tot, FUN = function(a, b) a * b / N)
  dimnames(E) <- dimnames(X)
  
  # ---- Specificities ----
  groups <- rownames(X)
  terms <- colnames(X)
  
  if (method == "chi2") {
    # Pearson standardized residuals: (X - E) / sqrt(E)
    # Avoid division by zero:
    E_safe <- pmax(E, .Machine$double.eps)
    Z <- (as.matrix(X) - E) / sqrt(E_safe)
    
    # Build long table
    spec_long <- tibble::tibble(
      group = rep(groups, times = length(terms)),
      term = rep(terms, each = length(groups)),
      observed = as.numeric(as.matrix(X)),
      expected = as.numeric(E),
      score = as.numeric(Z),
      p_value = NA_real_,
      direction = ifelse(as.numeric(Z) >= 0, "over", "under")
    )
    
  } else {
    # Hypergeometric tail p-values (per group)
    spec_list <- vector("list", length(groups))
    
    for (i in seq_along(groups)) {
      g <- groups[i]
      k <- as.numeric(as.matrix(X[i, ]))     # observed counts in group
      n <- row_tot[i]                        # group size (tokens)
      K <- col_tot                           # term totals
      expected <- E[i, ]
      
      # Direction based on (k - expected)
      over_dir <- k >= expected
      
      # Over-representation p: P(X >= k)
      q_over <- k - 1
      p_over <- stats::phyper(q_over, K, N - K, n, lower.tail = FALSE)
      
      # Under-representation p: P(X <= k)
      p_under <- stats::phyper(k, K, N - K, n, lower.tail = TRUE)
      
      p <- ifelse(over_dir, p_over, p_under)
      
      # Numeric safety
      p <- pmax(p, eps)
      score <- ifelse(over_dir, 1, -1) * (-log10(p))
      
      spec_list[[i]] <- tibble::tibble(
        group = g,
        term = terms,
        observed = k,
        expected = as.numeric(expected),
        score = as.numeric(score),
        p_value = as.numeric(p),
        direction = ifelse(over_dir, "over", "under")
      )
    }
    
    spec_long <- dplyr::bind_rows(spec_list)
  }
  
  # Top terms per group by |score|
  spec_top <- spec_long |>
    dplyr::group_by(group) |>
    dplyr::arrange(dplyr::desc(abs(score)), .by_group = TRUE) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::ungroup()
  
  # ---- Correspondence Analysis (CA) ----
  ca <- .ca_svd(X, nd = nd)
  
  list(
    specificities_long = spec_long,
    specificities_top = spec_top,
    ca = ca,
    table = X
  )
}

# ---- Internal: CA via SVD ----
.ca_svd <- function(X, nd = 3) {
  # X: groups×terms contingency (Matrix or base matrix)
  
  if (inherits(X, "Matrix")) {
    Xd <- as.matrix(X)
  } else {
    Xd <- X
  }
  
  Xd <- as.matrix(Xd)
  storage.mode(Xd) <- "numeric"
  
  N <- sum(Xd)
  if (N <= 0) stop("CA: contingency table has zero total.")
  
  P <- Xd / N
  r <- rowSums(P)
  c <- colSums(P)
  
  # Remove zero-mass rows/cols (should already be filtered, but keep safe)
  keep_r <- r > 0
  keep_c <- c > 0
  
  P <- P[keep_r, keep_c, drop = FALSE]
  r <- r[keep_r]
  c <- c[keep_c]
  
  # Centered and standardized residual matrix:
  # S = Dr^{-1/2} (P - r c^T) Dc^{-1/2}
  rc <- outer(r, c)
  Dr_inv_sqrt <- diag(1 / sqrt(r), nrow = length(r))
  Dc_inv_sqrt <- diag(1 / sqrt(c), nrow = length(c))
  
  S <- Dr_inv_sqrt %*% (P - rc) %*% Dc_inv_sqrt
  
  sv <- svd(S)
  d <- sv$d
  U <- sv$u
  V <- sv$v
  
  # Eigenvalues (inertia) are d^2
  eig <- d^2
  
  nd_eff <- min(nd, length(d))
  
  # Principal coordinates:
  # Rows: F = Dr^{-1/2} U D
  # Cols: G = Dc^{-1/2} V D
  Dm <- diag(d[seq_len(nd_eff)], nrow = nd_eff)
  
  F <- Dr_inv_sqrt %*% U[, seq_len(nd_eff), drop = FALSE] %*% Dm
  G <- Dc_inv_sqrt %*% V[, seq_len(nd_eff), drop = FALSE] %*% Dm
  
  rownames(F) <- rownames(P)
  rownames(G) <- colnames(P)
  colnames(F) <- paste0("Dim", seq_len(nd_eff))
  colnames(G) <- paste0("Dim", seq_len(nd_eff))
  
  list(
    eigenvalues = eig[seq_len(nd_eff)],
    rowcoord = F,
    colcoord = G,
    row_masses = r,
    col_masses = c
  )
}