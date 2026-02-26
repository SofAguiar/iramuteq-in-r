test_that("reinert_chd returns class labels with reasonable sizes", {
  # Small synthetic UCE×term matrix
  X <- matrix(
    c(3,0,0,  2,0,0,  3,0,0,   # cluster 1
      0,3,0,  0,2,0,  0,3,0,   # cluster 2
      0,0,3,  0,0,2,  0,0,3),  # cluster 3
    nrow = 9, byrow = TRUE
  )
  colnames(X) <- c("t1","t2","t3")
  M <- Matrix::Matrix(X, sparse = TRUE)
  
  res <- reinert_chd(M, min_term_freq = 1, max_classes = 3, min_class_size = 2, seed = 1)
  
  expect_true(is.list(res))
  expect_equal(length(res$classes), nrow(M))
  expect_true(length(unique(res$classes)) >= 2)
  expect_true(all(table(res$classes) >= 2))
  expect_true(nrow(res$top_terms) > 0)
})