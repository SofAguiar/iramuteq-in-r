#' Read an IRaMuTeQ lexique file (form -> lemma, POS/gram type)
#'
#' IRaMuTeQ lexique files are commonly tab-separated with at least 3 columns:
#'   form <TAB> lemma <TAB> pos
#'
#' Some lexiques may contain extra columns; this reader keeps the first three.
#'
#' When `file` is NULL, the function tries:
#'  1) installed package path: system.file("dictionaries", ...)
#'  2) development path: inst/dictionaries/...
#'
#' @param file Path to lexique_*.txt. If NULL, resolved from `language`.
#' @param language Language code used to select lexique_<language>.txt when file is NULL.
#' @param encoding Text encoding (e.g., "UTF-8", "latin1").
#' @param normalize_form If TRUE, lowercases and trims the `form` column.
#' @return A tibble with columns: form, lemma, pos
#' @export
read_lexique <- function(file = NULL,
                         language = "en",
                         encoding = "UTF-8",
                         normalize_form = TRUE) {
  
  if (is.null(file)) {
    candidate <- paste0("lexique_", language, ".txt")
    
    # 1) Installed package path
    file <- system.file("dictionaries", candidate, package = "iramuteqinr")
    
    # 2) Dev fallback (load_all() / working from repo)
    if (identical(file, "")) {
      file <- file.path("inst", "dictionaries", candidate)
    }
    
    if (!file.exists(file)) {
      stop(
        sprintf(
          "Lexique not found for language='%s'. Provide `file=...`.\nTried: dictionaries/%s and inst/dictionaries/%s",
          language, candidate, candidate
        )
      )
    }
  }
  
  if (!file.exists(file)) {
    stop(sprintf("Lexique file not found: %s", file))
  }
  
  # Robust TSV reading:
  # - Keep at least 3 columns
  # - Allow extra columns (ignored)
  raw <- readr::read_delim(
    file,
    delim = "\t",
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = encoding),
    progress = FALSE,
    show_col_types = FALSE,
    quote = ""
  )
  
  if (ncol(raw) < 3) {
    stop(
      sprintf(
        "Invalid lexique format: expected at least 3 tab-separated columns (form, lemma, pos). Found %d column(s).",
        ncol(raw)
      )
    )
  }
  
  lex <- raw[, 1:3]
  names(lex) <- c("form", "lemma", "pos")
  
  # Basic cleanup
  lex$form  <- stringi::stri_trim_both(lex$form)
  lex$lemma <- stringi::stri_trim_both(lex$lemma)
  lex$pos   <- stringi::stri_trim_both(lex$pos)
  
  if (normalize_form) {
    lex$form <- stringi::stri_trans_tolower(lex$form)
  }
  
  # Drop empty forms
  lex <- lex[!is.na(lex$form) & nzchar(lex$form), , drop = FALSE]
  
  # Ensure unique mapping per form (keep first occurrence)
  lex <- lex[!duplicated(lex$form), , drop = FALSE]
  
  tibble::as_tibble(lex)
}

#' Lemmatize tokens using an IRaMuTeQ lexique mapping
#'
#' @param tokens A character vector OR a list of character vectors (tokens).
#' @param lexique A tibble/data.frame with columns: form, lemma, pos.
#' @param unknown_pos POS tag for unknown tokens (default: "nr", IRaMuTeQ-like).
#' @param number_pos POS tag for numeric tokens (default: "num").
#' @param treat_numbers If TRUE, numeric-looking tokens are tagged as number_pos.
#' @param normalize_tokens If TRUE, lowercases tokens before lookup.
#' @return A tibble with columns: token, lookup, lemma, pos, is_known, is_number
#' @export
lemmatize_tokens <- function(tokens,
                             lexique,
                             unknown_pos = "nr",
                             number_pos = "num",
                             treat_numbers = TRUE,
                             normalize_tokens = TRUE) {
  
  if (is.list(tokens)) {
    tok <- unlist(tokens, use.names = FALSE)
  } else {
    tok <- tokens
  }
  
  tok <- as.character(tok)
  tok <- tok[!is.na(tok)]
  tok <- stringi::stri_trim_both(tok)
  tok <- tok[nzchar(tok)]
  
  if (!is.data.frame(lexique) || !all(c("form", "lemma", "pos") %in% names(lexique))) {
    stop("`lexique` must be a data.frame/tibble with columns: form, lemma, pos (use read_lexique()).")
  }
  
  lookup <- tok
  if (normalize_tokens) {
    lookup <- stringi::stri_trans_tolower(lookup)
  }
  
  # Build named lookup vectors (transparent + fast)
  form <- as.character(lexique$form)
  lemma_map <- stats::setNames(as.character(lexique$lemma), form)
  pos_map   <- stats::setNames(as.character(lexique$pos), form)
  
  is_num <- rep(FALSE, length(lookup))
  if (treat_numbers) {
    is_num <- grepl("^[-+]?[0-9]+([\\.,][0-9]+)?$", lookup)
  }
  
  is_known <- lookup %in% names(lemma_map)
  
  lemma <- ifelse(is_known, lemma_map[lookup], ifelse(is_num, tok, tok))
  pos   <- ifelse(is_known, pos_map[lookup],   ifelse(is_num, number_pos, unknown_pos))
  
  tibble::tibble(
    token = tok,
    lookup = lookup,
    lemma = unname(as.character(lemma)),
    pos = unname(as.character(pos)),
    is_known = as.logical(is_known),
    is_number = as.logical(is_num)
  )
}