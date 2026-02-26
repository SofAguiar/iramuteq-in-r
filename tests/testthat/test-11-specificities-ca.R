test_that("specificities_ca returns specificities and CA outputs", {
  # Small deterministic contingency table (2 groups × 3 terms)
  X <- matrix(
    c(10, 0, 5,
      2,  8, 1),
    nrow = 2, byrow = TRUE
  )
  rownames(X) <- c("A", "B")
  colnames(X) <- c("t1", "t2", "t3")
  
  res_h <- specificities_ca(X, method = "hypergeo", nd = 2, top_n = 2)
  expect_true(is.list(res_h))
  expect_true(all(c("specificities_long", "specificities_top", "ca", "table") %in% names(res_h)))
  expect_true(nrow(res_h$specificities_long) == 2 * 3)
  expect_true(length(res_h$ca$eigenvalues) == 2)
  
  res_c <- specificities_ca(X, method = "chi2", nd = 2, top_n = 2)
  expect_true(nrow(res_c$specificities_long) == 2 * 3)
  expect_true(all(!is.na(res_c$specificities_long$score)))
})