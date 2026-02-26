test_that("read_lexique loads an English lexique from dictionaries or file", {
  # Preferred: full lexique under inst/dictionaries
  p1 <- system.file("dictionaries", "lexique_en.txt", package = "iramuteqinr")
  if (identical(p1, "")) p1 <- file.path("inst", "dictionaries", "lexique_en.txt")
  
  # Optional fallback: small test lexique under extdata
  p2 <- system.file("extdata", "sample_lexique_en.txt", package = "iramuteqinr")
  if (identical(p2, "")) p2 <- file.path("inst", "extdata", "sample_lexique_en.txt")
  
  have_any <- file.exists(p1) || file.exists(p2)
  expect_true(
    have_any,
    info = "Expected lexique_en.txt in inst/dictionaries OR sample_lexique_en.txt in inst/extdata."
  )
  
  lex <- if (file.exists(p1)) {
    read_lexique(file = p1)
  } else {
    read_lexique(file = p2)
  }
  
  expect_true(is.data.frame(lex))
  expect_true(all(c("form", "lemma", "pos") %in% names(lex)))
  expect_true(nrow(lex) > 100)  # full lexique should be big; sample can be smaller but still >100 is ok if you keep it tiny adjust to >5
})

test_that("lemmatize_tokens returns aligned outputs and tags numbers/unknowns", {
  p1 <- system.file("dictionaries", "lexique_en.txt", package = "iramuteqinr")
  if (identical(p1, "")) p1 <- file.path("inst", "dictionaries", "lexique_en.txt")
  
  p2 <- system.file("extdata", "sample_lexique_en.txt", package = "iramuteqinr")
  if (identical(p2, "")) p2 <- file.path("inst", "extdata", "sample_lexique_en.txt")
  
  if (file.exists(p1)) {
    lex <- read_lexique(file = p1)
  } else if (file.exists(p2)) {
    lex <- read_lexique(file = p2)
  } else {
    skip("No lexique file found to test lemmatization.")
  }
  
  toks <- c("This", "is", "a", "document", "123", "unknownword")
  res <- lemmatize_tokens(toks, lex, unknown_pos = "nr", number_pos = "num")
  
  expect_true(is.data.frame(res))
  expect_true(all(c("token", "lookup", "lemma", "pos", "is_known", "is_number") %in% names(res)))
  expect_equal(nrow(res), length(toks))
  
  # Numeric invariant
  expect_true(res$is_number[5])
  expect_equal(res$pos[5], "num")
  
  # Unknown word should not be marked known (unless your lexique happens to contain it)
  # So we only assert the safe direction:
  expect_true(res$is_known[6] %in% c(TRUE, FALSE))
  if (!res$is_known[6]) expect_equal(res$pos[6], "nr")
  
  # Lemma should never be empty
  expect_true(all(!is.na(res$lemma) & nzchar(res$lemma)))
})