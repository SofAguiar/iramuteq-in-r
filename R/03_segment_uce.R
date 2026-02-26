#' Segment cleaned text into UCEs
#'
#' Supports two segmentation modes:
#' - method = "char": uses character counts and optionally prefers punctuation boundaries (douce).
#' - method = "token": uses token counts; douce currently has limited effect unless punctuation is preserved.
#'
#' Input can be either:
#' - a tibble/data.frame with columns (uci_id, text)
#' - or a character vector (one element per UCI), in which case uci_id is sequential
#'
#' @param text tibble/data.frame with uci_id + text, or a character vector
#' @param method segmentation method: 'char' or 'token'
#' @param uce_size target segment size (chars or tokens, depending on method)
#' @param douce logical, whether to try splitting at punctuation boundaries
#' @return tibble with columns: uce_id, uci_id, uce_index, text_uce
#' @export
segment_uce <- function(text,
                        method = c("char", "token"),
                        uce_size = 40,
                        douce = TRUE) {
  method <- match.arg(method)
  stopifnot(is.numeric(uce_size), length(uce_size) == 1, uce_size > 0)
  
  # Normalize input
  if (is.data.frame(text)) {
    if (!all(c("uci_id", "text") %in% names(text))) {
      stop("If `text` is a data.frame/tibble, it must contain columns: uci_id, text")
    }
    df <- tibble::as_tibble(text[, c("uci_id", "text")])
    df$uci_id <- as.integer(df$uci_id)
    df$text <- as.character(df$text)
  } else {
    stopifnot(is.character(text))
    df <- tibble::tibble(
      uci_id = seq_along(text),
      text = as.character(text)
    )
  }
  
  # Helper: choose a cut position for char mode
  choose_cut_pos <- function(s, start, target_len, douce) {
    n <- nchar(s)
    if (start > n) return(NA_integer_)
    
    target <- min(n, start + target_len - 1)
    if (!douce) return(target)
    
    # Search window around target
    w <- max(5L, floor(target_len * 0.30))
    lo <- max(start + 5L, target - w)
    hi <- min(n, target + w)
    
    window <- substr(s, lo, hi)
    
    # Prefer strong boundaries first
    # Note: indices returned are within 'window'
    m <- gregexpr("[\\.!\\?;:\\n]", window, perl = TRUE)[[1]]
    if (length(m) == 1 && m[1] == -1) {
      # fallback: any whitespace boundary
      m <- gregexpr("\\s", window, perl = TRUE)[[1]]
    }
    
    if (length(m) == 1 && m[1] == -1) {
      return(target)
    }
    
    # Convert to absolute positions
    candidates <- lo + m - 1L
    
    # Prefer the last candidate <= target, otherwise first > target
    left <- candidates[candidates <= target]
    if (length(left) > 0) return(max(left))
    
    right <- candidates[candidates > target]
    if (length(right) > 0) return(min(right))
    
    target
  }
  
  segments <- list()
  uce_global <- 0L
  
  for (i in seq_len(nrow(df))) {
    uci <- df$uci_id[i]
    s <- df$text[i]
    if (!nzchar(s)) next
    
    if (method == "char") {
      # Work on raw string, but normalize excessive spaces
      s2 <- gsub("[ \t]+", " ", s)
      s2 <- stringi::stri_trim_both(s2)
      
      start <- 1L
      idx <- 0L
      n <- nchar(s2)
      
      while (start <= n) {
        cut <- choose_cut_pos(s2, start, target_len = as.integer(uce_size), douce = douce)
        if (is.na(cut)) break
        
        chunk <- substr(s2, start, cut)
        chunk <- stringi::stri_trim_both(chunk)
        
        # Move start forward (avoid infinite loops)
        next_start <- cut + 1L
        if (next_start <= start) next_start <- start + as.integer(uce_size)
        
        # If chunk too small (e.g., trailing punctuation), skip
        if (nzchar(chunk)) {
          idx <- idx + 1L
          uce_global <- uce_global + 1L
          segments[[length(segments) + 1L]] <- list(
            uce_id = uce_global,
            uci_id = uci,
            uce_index = idx,
            text_uce = chunk
          )
        }
        
        start <- next_start
      }
    } else {
      # token-based segmentation
      toks <- tokenize_text(s, keep_hyphen = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      if (length(toks) == 0) next
      
      idx <- 0L
      pos <- 1L
      while (pos <= length(toks)) {
        end <- min(length(toks), pos + as.integer(uce_size) - 1L)
        chunk_toks <- toks[pos:end]
        chunk <- paste(chunk_toks, collapse = " ")
        chunk <- stringi::stri_trim_both(chunk)
        
        idx <- idx + 1L
        uce_global <- uce_global + 1L
        segments[[length(segments) + 1L]] <- list(
          uce_id = uce_global,
          uci_id = uci,
          uce_index = idx,
          text_uce = chunk
        )
        
        pos <- end + 1L
      }
    }
  }
  
  if (length(segments) == 0) {
    return(tibble::tibble(uce_id = integer(), uci_id = integer(), uce_index = integer(), text_uce = character()))
  }
  
  tibble::as_tibble(data.frame(
    uce_id = vapply(segments, `[[`, integer(1), "uce_id"),
    uci_id = vapply(segments, `[[`, integer(1), "uci_id"),
    uce_index = vapply(segments, `[[`, integer(1), "uce_index"),
    text_uce = vapply(segments, `[[`, character(1), "text_uce"),
    stringsAsFactors = FALSE
  ))
}