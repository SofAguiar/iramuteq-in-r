test_that("build_uce_term_matrix supports lemma source with lexique", {
  # corpus
  p <- system.file("extdata", "sample_corpus.txt", package = "iramuteqinr")
  if (identical(p, "")) p <- file.path("inst", "extdata", "sample_corpus.txt")
  expect_true(file.exists(p))
  
  corpus <- read_corpus_iramuteq(p)
  uces <- segment_uce(corpus, method = "char", uce_size = 40, douce = TRUE)
  
  # lexique
  lx <- system.file("dictionaries", "lexique_en.txt", package = "iramuteqinr")
  if (identical(lx, "")) lx <- file.path("inst", "dictionaries", "lexique_en.txt")
  expect_true(file.exists(lx))
  
  lex <- read_lexique(file = lx)
  
  M_lem <- build_uce_term_matrix(
    uces,
    term_source = "lemma",
    lexique = lex,
    drop_pos = c("sw"),
    weighting = "count"
  )
  
  expect_true(inherits(M_lem, "dgCMatrix"))
  expect_equal(nrow(M_lem), nrow(uces))
  expect_true(ncol(M_lem) > 0)
  
  # Dropping stopwords should reduce (or keep) total count vs tokens (never increase)
  M_tok <- build_uce_term_matrix(uces, term_source = "token", weighting = "count")
  expect_true(sum(M_lem) <= sum(M_tok))
})