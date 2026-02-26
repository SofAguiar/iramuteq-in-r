test_that("read_corpus_iramuteq parses UCIs and etoile variables", {
  # When installed, files under inst/extdata become available under extdata/
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  
  # Fallback for dev/source workflows
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  
  expect_equal(nrow(corpus), 2)
  expect_true(all(c("uci_id", "text", "raw_meta") %in% names(corpus)))
  
  expect_true("meta_id" %in% names(corpus))
  expect_true("meta_gender" %in% names(corpus))
  expect_true("meta_group" %in% names(corpus))
  
  expect_equal(corpus$meta_id, c("001", "002"))
  expect_equal(corpus$meta_gender, c("F", "M"))
  expect_equal(corpus$meta_group, c("A", "B"))
})