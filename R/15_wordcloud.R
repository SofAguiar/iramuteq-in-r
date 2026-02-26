#' Create a wordcloud (or fallback plot) from term frequencies
#'
#' @param freqs named numeric vector (term -> frequency)
#' @param max_words maximum words
#' @param mode "auto", "wordcloud", or "bar"
#' @return A ggplot object (bar mode) or a list with metadata (wordcloud mode).
#' @export
wordcloud_plot <- function(freqs, max_words = 200, mode = c("auto", "wordcloud", "bar")) {
  mode <- match.arg(mode)
  stopifnot(is.numeric(freqs))
  if (is.null(names(freqs))) stop("wordcloud_plot: `freqs` must be a named numeric vector.")
  
  freqs <- sort(freqs, decreasing = TRUE)
  freqs <- freqs[!is.na(freqs) & freqs > 0]
  freqs <- freqs[seq_len(min(max_words, length(freqs)))]
  
  if (length(freqs) == 0) stop("wordcloud_plot: no positive frequencies to plot.")
  
  has_wordcloud <- requireNamespace("wordcloud", quietly = TRUE)
  
  if (mode == "auto") {
    mode <- if (has_wordcloud) "wordcloud" else "bar"
  }
  
  if (mode == "wordcloud") {
    if (!has_wordcloud) stop("wordcloud_plot: mode='wordcloud' requires the 'wordcloud' package.")
    words <- names(freqs)
    f <- as.numeric(freqs)
    
    # Note: wordcloud() draws to the active device and returns NULL.
    # We return a small metadata object for pipeline reproducibility.
    wordcloud::wordcloud(words = words, freq = f, max.words = length(words), random.order = FALSE)
    
    return(list(mode = "wordcloud", words = words, freq = f))
  }
  
  # Fallback: deterministic bar chart
  df <- tibble::tibble(term = names(freqs), freq = as.numeric(freqs))
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(term, freq), y = freq)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Frequency", title = "Top terms") +
    ggplot2::theme_minimal()
}