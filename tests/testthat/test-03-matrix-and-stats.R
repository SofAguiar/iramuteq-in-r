test_that("build_uce_term_matrix builds a consistent sparse matrix", {
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  uces <- segment_uce(corpus, method = "char", uce_size = 40, douce = TRUE)
  
  M <- build_uce_term_matrix(uces, weighting = "count", remove_punct = TRUE, lowercase = TRUE)
  expect_true(inherits(M, "dgCMatrix"))
  expect_equal(nrow(M), nrow(uces))
  expect_true(ncol(M) > 0)
  
  # Token count consistency check (same cleaning/tokenization settings)
  cleaned <- clean_text(uces$text_uce, lowercase = TRUE, remove_numbers = FALSE, remove_punct = TRUE)
  toks <- tokenize_text(cleaned)
  expected_total <- sum(lengths(toks))
  
  expect_equal(as.numeric(sum(M)), expected_total)
  
  Mb <- build_uce_term_matrix(uces, weighting = "binary", remove_punct = TRUE, lowercase = TRUE)
  expect_true(all(Mb@x %in% 1))
  expect_true(sum(Mb) <= sum(M))
})

test_that("corpus_stats returns hapax and totals", {
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  uces <- segment_uce(corpus, method = "char", uce_size = 40, douce = TRUE)
  M <- build_uce_term_matrix(uces, weighting = "count", remove_punct = TRUE, lowercase = TRUE)
  
  st <- corpus_stats(M)
  expect_true(is.list(st))
  expect_true("term_freq" %in% names(st))
  expect_true("hapax_terms" %in% names(st))
  expect_true(st$totals$total_occurrences >= 1)
})