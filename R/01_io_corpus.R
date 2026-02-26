#' Read an IRaMuTeQ-style corpus file (UCI blocks with *etoile variables)
#'
#' This parser supports the most common IRaMuTeQ corpus format where each
#' text (UCI) begins with a header line starting with "****", followed by
#' optional metadata tokens beginning with "*".
#'
#' Metadata token formats supported:
#' - *key=value        -> key = value
#' - *key_value        -> key = value (split at first "_")
#' - *flag             -> flag = TRUE
#'
#' @param file Path to corpus text file.
#' @param encoding Text encoding (e.g., "UTF-8", "latin1").
#' @param ucimark UCI header marker. Use "****" (default) or "int" for
#'   the alternative IRaMuTeQ mode where a line starts with 4 digits then "*".
#' @param meta_prefix Prefix for metadata columns in the returned tibble.
#' @param keep_newlines Whether to preserve newlines inside each UCI text.
#' @return A tibble with columns:
#'   - uci_id (integer)
#'   - text (character)
#'   - raw_meta (character)
#'   - meta_* columns (parsed metadata)
#' @export
read_corpus_iramuteq <- function(file,
                                 encoding = "UTF-8",
                                 ucimark = "****",
                                 meta_prefix = "meta_",
                                 keep_newlines = TRUE) {
  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file))
  }
  
  lines <- readLines(con = file, encoding = encoding, warn = FALSE)
  # Normalize CRLF/CR to LF and remove BOM if present
  lines <- sub("\r$", "", lines)
  if (length(lines) > 0) {
    lines[1] <- sub("^\uFEFF", "", lines[1])
  }
  
  detect_header <- function(line) {
    x <- trimws(line)
    if (identical(ucimark, "****")) {
      return(startsWith(x, "****"))
    }
    if (identical(ucimark, "int")) {
      # IRaMuTeQ alternative: first 4 chars digits and 5th char "*"
      if (nchar(x) < 5) return(FALSE)
      a <- substr(x, 1, 4)
      b <- substr(x, 5, 5)
      return(grepl("^[0-9]{4}$", a) && b == "*")
    }
    # Fallback: treat ucimark as a literal prefix
    startsWith(x, ucimark)
  }
  
  parse_meta_tokens <- function(header_line) {
    x <- trimws(header_line)
    
    # Strip the marker part
    if (identical(ucimark, "****")) {
      x <- sub("^\\*\\*\\*\\*\\s*", "", x)
    } else if (identical(ucimark, "int")) {
      x <- sub("^[0-9]{4}\\*\\s*", "", x)
    } else {
      x <- sub(paste0("^", gsub("([\\\\.^$|()\\[\\]{}*+?])", "\\\\\\1", ucimark), "\\s*"), "", x)
    }
    
    if (!nzchar(x)) return(list())
    
    toks <- unlist(strsplit(x, "\\s+"))
    toks <- toks[nzchar(toks)]
    toks <- toks[startsWith(toks, "*")]
    if (length(toks) == 0) return(list())
    
    out <- list()
    
    for (t in toks) {
      t <- substring(t, 2) # drop leading "*"
      if (!nzchar(t)) next
      
      key <- NULL
      val <- NULL
      
      if (grepl("=", t, fixed = TRUE)) {
        parts <- strsplit(t, "=", fixed = TRUE)[[1]]
        key <- parts[1]
        val <- paste(parts[-1], collapse = "=")
        if (!nzchar(val)) val <- NA_character_
      } else if (grepl("_", t, fixed = TRUE)) {
        parts <- strsplit(t, "_", fixed = TRUE)[[1]]
        key <- parts[1]
        val <- paste(parts[-1], collapse = "_")
        if (!nzchar(val)) val <- NA_character_
      } else {
        key <- t
        val <- TRUE
      }
      
      key <- tolower(key)
      key <- gsub("[^a-z0-9]+", "_", key)
      key <- gsub("^_+|_+$", "", key)
      key <- make.names(key, unique = FALSE)
      
      # If repeated, concatenate values with ";"
      if (!is.null(out[[key]])) {
        out[[key]] <- paste(out[[key]], val, sep = ";")
      } else {
        out[[key]] <- val
      }
    }
    
    out
  }
  
  records <- list()
  current_meta_line <- NULL
  current_text <- character()
  
  push_record <- function(meta_line, text_lines) {
    txt <- if (keep_newlines) paste(text_lines, collapse = "\n") else paste(text_lines, collapse = " ")
    records[[length(records) + 1]] <<- list(
      raw_meta = if (!is.null(meta_line)) meta_line else "",
      meta = if (!is.null(meta_line)) parse_meta_tokens(meta_line) else list(),
      text = txt
    )
  }
  
  # If file has no headers at all, treat whole file as one UCI
  if (!any(vapply(lines, detect_header, logical(1)))) {
    push_record(meta_line = "", text_lines = lines)
  } else {
    for (ln in lines) {
      if (detect_header(ln)) {
        if (!is.null(current_meta_line)) {
          push_record(current_meta_line, current_text)
        }
        current_meta_line <- ln
        current_text <- character()
      } else {
        # Keep content lines exactly; trimming is handled downstream if needed
        current_text <- c(current_text, ln)
      }
    }
    # Push last record
    if (!is.null(current_meta_line)) {
      push_record(current_meta_line, current_text)
    }
  }
  
  # Build tibble with meta columns
  all_keys <- unique(unlist(lapply(records, function(r) names(r$meta))))
  all_keys <- all_keys[!is.na(all_keys)]
  
  df <- data.frame(
    uci_id = seq_along(records),
    text = vapply(records, `[[`, character(1), "text"),
    raw_meta = vapply(records, `[[`, character(1), "raw_meta"),
    stringsAsFactors = FALSE
  )
  
  for (k in all_keys) {
    df[[paste0(meta_prefix, k)]] <- vapply(
      records,
      function(r) {
        if (!is.null(r$meta[[k]])) as.character(r$meta[[k]]) else NA_character_
      },
      character(1)
    )
  }
  
  tibble::as_tibble(df)
}