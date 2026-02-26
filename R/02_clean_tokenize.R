#' Clean text with configurable options
#'
#' This function is intentionally conservative: it normalizes whitespace,
#' optionally lowercases, optionally removes numbers, and optionally removes
#' punctuation (keeping word characters and spaces).
#'
#' @param x character vector
#' @param lowercase logical
#' @param remove_numbers logical
#' @param remove_punct logical
#' @return character vector
#' @export
clean_text <- function(x,
                       lowercase = TRUE,
                       remove_numbers = FALSE,
                       remove_punct = TRUE) {
  stopifnot(is.character(x))
  
  y <- x
  
  # Normalize line endings and trim
  y <- gsub("\r\n|\r", "\n", y)
  y <- stringi::stri_trim_both(y)
  
  if (lowercase) {
    y <- stringi::stri_trans_tolower(y)
  }
  
  if (remove_numbers) {
    y <- gsub("[0-9]+", " ", y)
  }
  
  if (remove_punct) {
    # Keep letters/digits/underscore and whitespace. Replace the rest with space.
    # Note: this is not language-specific; refine later if needed.
    y <- gsub("[^\\p{L}\\p{N}_\\s]+", " ", y, perl = TRUE)
  }
  
  # Collapse repeated whitespace
  y <- gsub("[ \t]+", " ", y)
  y <- gsub("\n{3,}", "\n\n", y)
  y <- stringi::stri_trim_both(y)
  
  y
}

#' Tokenize text
#'
#' Tokenization is whitespace-based by default. If keep_hyphen is TRUE,
#' hyphenated forms remain intact; otherwise hyphens are treated as separators.
#'
#' @param x character vector
#' @param keep_hyphen logical
#' @return list of character vectors (tokens)
#' @export
tokenize_text <- function(x, keep_hyphen = TRUE) {
  stopifnot(is.character(x))
  
  y <- x
  if (!keep_hyphen) {
    y <- gsub("-", " ", y, fixed = TRUE)
  }
  
  # Split on any whitespace
  toks <- stringi::stri_split_regex(y, "\\s+", omit_empty = TRUE)
  
  toks
}