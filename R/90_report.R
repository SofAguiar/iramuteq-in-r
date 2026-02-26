#' Render a transparent report (Quarto/RMarkdown if available)
#'
#' This function always writes a report source file into output_dir.
#' If Quarto is available, it renders to HTML; otherwise, it returns the .qmd path.
#'
#' @param output_dir output directory
#' @param params list of run parameters to embed/log in the report
#' @return Path to the rendered HTML (if rendered) or to the .qmd source file.
#' @export
render_report <- function(output_dir, params = list()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  qmd_path <- file.path(output_dir, "report.qmd")
  
  # Minimal, transparent template
  lines <- c(
    "---",
    "title: \"iramuteq-in-r report\"",
    "format: html",
    "---",
    "",
    "## Run parameters",
    "",
    "```r",
    "params <- NULL",
    "```",
    "",
    "Below is a `dput()` representation of the parameters used:",
    "",
    "```r",
    paste0("params <- ", paste(capture.output(dput(params)), collapse = "\n")),
    "params",
    "```",
    "",
    "## Session info",
    "",
    "```r",
    "sessionInfo()",
    "```"
  )
  
  writeLines(lines, con = qmd_path, useBytes = TRUE)
  
  # Try to render via Quarto if available
  if (requireNamespace("quarto", quietly = TRUE)) {
    out <- try(quarto::quarto_render(qmd_path, output_dir = output_dir), silent = TRUE)
    if (!inherits(out, "try-error")) {
      # quarto returns output path(s) as character
      return(as.character(out)[1])
    }
  }
  
  # Fallback: keep source only
  qmd_path
}