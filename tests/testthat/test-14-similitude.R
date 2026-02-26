test_that("similitude_graph builds an igraph graph", {
  X <- matrix(
    c(1,1,0,
      1,0,1,
      0,1,1,
      1,1,1),
    nrow = 4, byrow = TRUE
  )
  colnames(X) <- c("a","b","c")
  M <- Matrix::Matrix(X, sparse = TRUE)
  
  res <- similitude_graph(M, min_freq = 1, min_cooc = 1, weight = "cooc")
  expect_true(inherits(res$graph, "igraph"))
  expect_true(igraph::vcount(res$graph) == 3)
  expect_true(igraph::ecount(res$graph) >= 1)
})