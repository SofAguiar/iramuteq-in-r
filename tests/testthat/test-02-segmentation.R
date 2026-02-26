test_that("segment_uce works in char mode and preserves uci_id", {
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  uces <- segment_uce(corpus, method = "char", uce_size = 30, douce = TRUE)
  
  expect_true(nrow(uces) >= 2)
  expect_true(all(c("uce_id", "uci_id", "uce_index", "text_uce") %in% names(uces)))
  expect_true(all(uces$uci_id %in% corpus$uci_id))
  expect_true(all(nchar(uces$text_uce) > 0))
})

test_that("segment_uce works in token mode", {
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  uces <- segment_uce(corpus, method = "token", uce_size = 5, douce = FALSE)
  
  expect_true(nrow(uces) >= 2)
  expect_true(all(nchar(uces$text_uce) > 0))
})