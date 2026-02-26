test_that("wordcloud_plot returns a ggplot in bar mode", {
  freqs <- c(a = 10, b = 5, c = 2)
  p <- wordcloud_plot(freqs, max_words = 3, mode = "bar")
  expect_true(inherits(p, "ggplot"))
})