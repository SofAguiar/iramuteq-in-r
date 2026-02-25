#' IRAMUTEQ Corpus Functions
#'
#' Core functions for reading and manipulating textual corpora in IRAMUTEQ format.
#' The IRAMUTEQ format uses "****" to mark segments, followed by variables
#' prefixed with "*" (e.g., "**** *var1 value1 *var2 value2").
#'
#' @docType package
#' @name corpus
NULL

#' @import dplyr
#' @import tidytext
#' @import Matrix
NULL

# ============================================================================
# MAIN FUNCTIONS
# ============================================================================

#' Read a corpus in IRAMUTEQ format
#'
#' Reads a text file formatted for IRAMUTEQ, where each segment starts with "****"
#' followed by variables prefixed with "*" and their values.
#'
#' @param file Path to the text file
#' @param encoding File encoding (default: "UTF-8")
#'
#' @return An object of class "iramuteq_corpus" containing:
#'   \item{texts}{Character vector of text segments}
#'   \item{variables}{Data frame with metadata variables}
#'   \item{raw}{Raw lines from the file}
#'   \item{n_segments}{Number of segments}
#'   \item{n_variables}{Number of variables}
#' @export
#'
#' @examples
#' \dontrun{
#' corpus <- read_corpus("meu_corpus.txt")
#' }
read_corpus <- function(file, encoding = "UTF-8") {
  # Check if file exists
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  # Read all lines
  lines <- readLines(file, encoding = encoding, warn = FALSE)
  lines <- trimws(lines)  # Remove extra whitespace
  
  # Remove empty lines at beginning/end
  lines <- lines[lines != ""]
  
  # Find segment boundaries (lines starting with "****")
  segment_starts <- grep("^\\*\\*\\*\\*", lines)
  
  if (length(segment_starts) == 0) {
    stop("No segments found. File must have lines starting with '****'")
  }
  
  # Extract texts and variables
  texts <- character()
  variables_list <- list()
  
  for (i in seq_along(segment_starts)) {
    start <- segment_starts[i]
    end <- if (i < length(segment_starts)) segment_starts[i + 1] - 1 else length(lines)
    
    # Header line with variables
    header <- lines[start]
    
    # Extract variables from header
    vars <- parse_iramuteq_header(header)
    
    # Extract text (lines between this header and next header)
    if (start < end) {
      text_lines <- lines[(start + 1):end]
      text <- paste(text_lines, collapse = " ")
    } else {
      text <- ""
    }
    text <- trimws(gsub("\\s+", " ", text))  # Normalize whitespace
    
    texts <- c(texts, text)
    variables_list[[i]] <- vars
  }
  
  # Convert variables list to data frame
  variables_df <- variables_list_to_df(variables_list)
  
  # Create corpus object
  corpus <- list(
    texts = texts,
    variables = variables_df,
    raw = lines,
    n_segments = length(texts),
    n_variables = ncol(variables_df)
  )
  
  class(corpus) <- "iramuteq_corpus"
  
  # Add metadata
  attr(corpus, "created") <- Sys.time()
  attr(corpus, "file") <- basename(file)
  
  return(corpus)
}

#' Print method for iramuteq_corpus objects
#'
#' @param x An object of class "iramuteq_corpus"
#' @param ... Additional arguments (ignored)
#' @export
print.iramuteq_corpus <- function(x, ...) {
  cat("IRAMUTEQ Corpus\n")
  cat("---------------\n")
  cat("File:", attr(x, "file"), "\n")
  cat("Created:", format(attr(x, "created"), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Number of segments:", x$n_segments, "\n")
  cat("Number of variables:", x$n_variables, "\n")
  
  if (x$n_variables > 0 && nrow(x$variables) > 0) {
    cat("\nVariables:\n")
    var_names <- names(x$variables)
    for (var in var_names) {
      n_unique <- length(unique(stats::na.omit(x$variables[[var]])))
      cat("  $", var, ": ", n_unique, " unique values\n", sep = "")
    }
  }
  
  if (x$n_segments > 0) {
    cat("\nFirst segment:\n")
    first_text <- substr(x$texts[1], 1, 100)
    if (nchar(x$texts[1]) > 100) {
      first_text <- paste0(first_text, "...")
    }
    cat("  ", first_text, "\n")
  }
  
  invisible(x)
}

#' Extract variables from corpus
#'
#' @param corpus An iramuteq_corpus object
#' @param var_names Optional vector of variable names to extract (extracts all if NULL)
#' @return A data frame with the specified variables
#' @export
#'
#' @examples
#' \dontrun{
#' vars <- extract_variables(corpus)
#' idade <- extract_variables(corpus, "idade")
#' }
extract_variables <- function(corpus, var_names = NULL) {
  if (!inherits(corpus, "iramuteq_corpus")) {
    stop("corpus must be an iramuteq_corpus object")
  }
  
  if (is.null(var_names)) {
    return(corpus$variables)
  }
  
  # Check if all requested variables exist
  missing_vars <- setdiff(var_names, names(corpus$variables))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }
  
  return(corpus$variables[, var_names, drop = FALSE])
}

#' Calculate basic statistics for a corpus
#'
#' @param corpus An iramuteq_corpus object
#'
#' @return A list with corpus statistics
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- corpus_stats(corpus)
#' }
corpus_stats <- function(corpus) {
  if (!inherits(corpus, "iramuteq_corpus")) {
    stop("corpus must be an iramuteq_corpus object")
  }
  
  # Combine all texts
  all_text <- paste(corpus$texts, collapse = " ")
  
  # Tokenize
  words <- unlist(strsplit(all_text, "\\s+"))
  words <- words[words != ""]
  
  # Calculate statistics
  word_freq <- table(words)
  
  stats <- list(
    n_segments = corpus$n_segments,
    n_occurrences = length(words),
    n_unique_forms = length(unique(words)),
    hapax = sum(word_freq == 1),
    mean_segment_length = mean(sapply(strsplit(corpus$texts, "\\s+"), length)),
    sd_segment_length = stats::sd(sapply(strsplit(corpus$texts, "\\s+"), length))
  )
  
  # Type-token ratio
  stats$type_token_ratio <- stats$n_unique_forms / stats$n_occurrences
  
  class(stats) <- "iramuteq_stats"
  
  return(stats)
}

#' Print method for iramuteq_stats objects
#'
#' @param x An object of class "iramuteq_stats"
#' @param ... Additional arguments (ignored)
#' @export
print.iramuteq_stats <- function(x, ...) {
  cat("IRAMUTEQ Corpus Statistics\n")
  cat("-------------------------\n")
  cat("Number of segments:", x$n_segments, "\n")
  cat("Total occurrences:", x$n_occurrences, "\n")
  cat("Unique forms:", x$n_unique_forms, "\n")
  
  hapax_pct <- if (x$n_unique_forms > 0) {
    sprintf("(%.1f%%)", 100 * x$hapax / x$n_unique_forms)
  } else {
    "(0.0%)"
  }
  cat("Hapax (forms appearing once):", x$hapax, hapax_pct, "\n")
  cat("Type-token ratio:", sprintf("%.4f", x$type_token_ratio), "\n")
  cat("Mean segment length:", sprintf("%.2f words", x$mean_segment_length), "\n")
  cat("SD segment length:", sprintf("%.2f", x$sd_segment_length), "\n")
  invisible(x)
}

##' Create a document-term matrix from corpus
#'
#' @param corpus An iramuteq_corpus object
#' @param min_freq Minimum frequency for terms to include (default: 1)
#' @param max_terms Maximum number of terms to keep (default: NULL = all)
#' @param remove_stopwords Whether to remove stopwords (default: FALSE)
#' @param language Language for stopwords if remove_stopwords is TRUE (default: "pt")
#'
#' @return A sparse document-term matrix
#' @export
#'
#' @examples
#' \dontrun{
#' dtm <- create_dtm(corpus)
#' dtm_filtered <- create_dtm(corpus, min_freq = 3)
#' }
create_dtm <- function(corpus, min_freq = 1, max_terms = NULL, 
                       remove_stopwords = FALSE, language = "pt") {
  
  # Verificar se o corpus é válido
  if (!inherits(corpus, "iramuteq_corpus")) {
    stop("corpus must be an iramuteq_corpus object")
  }
  
  # Criar data frame para o tidytext
  text_df <- data.frame(
    doc_id = seq_along(corpus$texts),
    text = corpus$texts,
    stringsAsFactors = FALSE
  )
  
  # Remover documentos vazios
  text_df <- text_df[text_df$text != "", ]
  
  if (nrow(text_df) == 0) {
    warning("No non-empty documents found")
    return(NULL)
  }
  
  # Tokenização
  tokens <- tidytext::unnest_tokens(text_df, word, text, token = "words")
  
  # Remover stopwords se solicitado
  if (remove_stopwords) {
    stopwords <- get_stopwords(language)
    tokens <- tokens[!tokens$word %in% stopwords, ]
  }
  
  # Contar frequências (sem usar %>%)
  word_counts <- dplyr::count(tokens, doc_id, word, sort = TRUE)
  
  # Filtrar por frequência mínima
  if (min_freq > 1) {
    # Calcular total por palavra
    word_freq <- aggregate(n ~ word, data = word_counts, FUN = sum)
    word_freq <- word_freq[word_freq$n >= min_freq, ]
    
    # Filtrar word_counts
    word_counts <- word_counts[word_counts$word %in% word_freq$word, ]
  }
  
  # Limitar número de termos
  if (!is.null(max_terms) && max_terms > 0 && nrow(word_counts) > 0) {
    # Calcular total por palavra e ordenar
    word_freq <- aggregate(n ~ word, data = word_counts, FUN = sum)
    word_freq <- word_freq[order(word_freq$n, decreasing = TRUE), ]
    
    # Pegar os top N termos
    top_words <- head(word_freq$word, max_terms)
    
    # Filtrar word_counts
    word_counts <- word_counts[word_counts$word %in% top_words, ]
  }
  
  # Criar matriz esparsa
  if (nrow(word_counts) == 0) {
    warning("No terms remaining after filtering")
    return(NULL)
  }
  
  dtm <- tidytext::cast_dtm(word_counts, doc_id, word, n)
  
  # Adicionar atributos
  attr(dtm, "corpus_file") <- attr(corpus, "file")
  attr(dtm, "n_docs") <- length(corpus$texts)
  attr(dtm, "n_terms") <- ncol(dtm)
  attr(dtm, "min_freq") <- min_freq
  
  return(dtm)
}

#' Get stopwords for a language
#'
#' @param language Language code ("pt", "en", "fr", "es", "de")
#' @return Character vector of stopwords
#' @keywords internal
get_stopwords <- function(language = "pt") {
  # Portuguese stopwords
  pt_stopwords <- c(
    "de", "a", "o", "que", "e", "do", "da", "em", "um", "para", 
    "com", "não", "uma", "os", "no", "se", "na", "por", "mais", 
    "as", "dos", "como", "mas", "ao", "ele", "das", "à", "seu", 
    "sua", "ou", "quando", "muito", "nos", "já", "eu", "também", 
    "só", "pelo", "pela", "até", "isso", "ela", "entre", "depois", 
    "sem", "mesmo", "aos", "seus", "quem", "nas", "me", "esse", 
    "eles", "você", "essa", "num", "nem", "suas", "meu", "às", 
    "minha", "numa", "pelos", "elas", "qual", "nós", "lhe", "deles", 
    "essas", "esses", "pelas", "este", "dele", "tu", "te", "vocês", 
    "vos", "lhes", "meus", "minhas", "teu", "tua", "teus", "tuas", 
    "nosso", "nossa", "nossos", "nossas", "dela", "delas", "esta", 
    "estes", "estas", "aquele", "aquela", "aqueles", "aquelas", 
    "isto", "aquilo", "ao", "aos", "à", "às"
  )
  
  # English stopwords (from tidytext)
  en_stopwords <- if (requireNamespace("tidytext", quietly = TRUE)) {
    tidytext::stop_words$word
  } else {
    c("the", "a", "an", "and", "or", "but", "if", "then", "else", "when", 
      "at", "from", "by", "on", "off", "for", "in", "out", "over", "under",
      "to", "into", "with")
  }
  
  # French stopwords
  fr_stopwords <- c(
    "le", "la", "les", "de", "des", "du", "et", "un", "une", "dans", 
    "pour", "en", "par", "sur", "avec", "est", "sont", "au", "aux", 
    "ce", "cet", "cette", "ces", "mon", "ton", "son", "ma", "ta", 
    "sa", "mes", "tes", "ses", "notre", "votre", "leur", "nos", 
    "vos", "leurs", "qui", "que", "dont", "ou", "mais", "donc", 
    "car", "ni", "or", "si", "puis", "quand", "comme", "chez", 
    "hors", "sans", "sous", "vers", "pendant", "depuis", "jusque"
  )
  
  # Stopwords list
  stopwords_list <- list(
    pt = pt_stopwords,
    en = en_stopwords,
    fr = fr_stopwords,
    es = c("el", "la", "los", "las", "de", "del", "que", "y", "a", "en", 
           "un", "una", "por", "para", "con", "no", "su", "al", "lo", 
           "como", "más", "pero", "sus", "le", "ya", "o", "este", "sí", 
           "porque", "esta", "entre", "cuando", "muy", "sin", "sobre", 
           "también", "me", "hasta", "hay", "donde", "quien", "desde", 
           "todo", "nos", "durante", "todos", "uno", "les", "ni", "contra", 
           "otros", "ese", "eso", "ante", "ellos", "e", "esto", "mí", 
           "antes", "algunos", "qué", "unos", "yo", "otro", "otras", 
           "otra", "él", "tanto", "nunca", "ambos", "propio"),
    de = c("der", "die", "das", "den", "dem", "des", "ein", "eine", 
           "einer", "eines", "einem", "einen", "und", "oder", "aber", 
           "mit", "von", "für", "auf", "ist", "sind", "war", "waren", 
           "wird", "werden", "im", "am", "um", "zur", "zum", "bei", 
           "nach", "aus", "durch", "über", "gegen", "bis", "seit", 
           "schon", "noch", "nicht", "kein", "keine", "sehr", "viel", 
           "wenig", "dann", "denn", "weil", "dass", "als", "wie", 
           "auch", "nur", "mir", "mich", "dir", "dich", "ihm", "ihn", 
           "ihr", "uns", "euch", "sich")
  )
  
  if (!language %in% names(stopwords_list)) {
    warning("Language '", language, "' not found. Using Portuguese stopwords.")
    language <- "pt"
  }
  
  return(unique(stopwords_list[[language]]))
}