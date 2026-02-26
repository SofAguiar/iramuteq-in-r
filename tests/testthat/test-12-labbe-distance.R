test_that("labbe_distance returns a valid distance and clustering", {
  X <- matrix(
    c(10, 0, 5,
      2,  8, 1,
      5,  5, 5),
    nrow = 3, byrow = TRUE
  )
  rownames(X) <- c("A", "B", "C")
  colnames(X) <- c("t1", "t2", "t3")
  
  res <- labbe_distance(X)
  
  expect_true(inherits(res$dist, "dist"))
  expect_true(is.matrix(res$matrix))
  
  # diag() returns a named vector; compare values only
  expect_equal(unname(diag(res$matrix)), c(0, 0, 0))
  
  expect_true(inherits(res$hclust, "hclust"))
  expect_true(all(res$matrix >= 0 & res$matrix <= 1))
})