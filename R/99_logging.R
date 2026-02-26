#' Log parameters, session info, and input checksums
#'
#' @param output_dir output directory
#' @param params list
#' @param input_files character vector of file paths
#' @param hash "sha256" (requires openssl) or "md5"
#' @return invisible(TRUE)
#' @export
log_provenance <- function(output_dir, params, input_files = character(), hash = c("sha256", "md5")) {
  hash <- match.arg(hash)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Params
  writeLines(capture.output(dput(params)), con = file.path(output_dir, "params.dput"))
  writeLines(capture.output(str(params)), con = file.path(output_dir, "params.txt"))
  
  # Session info
  writeLines(capture.output(sessionInfo()), con = file.path(output_dir, "sessionInfo.txt"))
  
  # Checksums
  checks_path <- file.path(output_dir, "checksums.txt")
  if (length(input_files) > 0) {
    input_files <- normalizePath(input_files, winslash = "/", mustWork = FALSE)
    
    lines <- character()
    for (f in input_files) {
      if (!file.exists(f)) {
        lines <- c(lines, paste("MISSING", f))
        next
      }
      
      if (hash == "sha256" && requireNamespace("openssl", quietly = TRUE)) {
        h <- openssl::sha256(file = f)
        lines <- c(lines, paste(as.character(h), f))
      } else {
        h <- tools::md5sum(f)
        lines <- c(lines, paste(unname(h), f))
      }
    }
    writeLines(lines, con = checks_path)
  } else {
    writeLines("No input files provided.", con = checks_path)
  }
  
  # Run log file (append-ready)
  log_path <- file.path(output_dir, "run.log")
  if (!file.exists(log_path)) writeLines(character(), con = log_path)
  
  invisible(TRUE)
}

#' Append a message to the run log
#'
#' @param output_dir output directory used in log_provenance()
#' @param message character scalar
#' @export
log_message <- function(output_dir, message) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(output_dir, "run.log")
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", message)
  cat(line, "\n", file = log_path, append = TRUE)
  invisible(TRUE)
}