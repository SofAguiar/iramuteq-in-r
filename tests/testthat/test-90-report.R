test_that("render_report writes a report source file", {
  td <- tempdir()
  outdir <- file.path(td, "iramuteq_report_test")
  p <- render_report(outdir, params = list(a = 1, b = "x"))
  
  expect_true(file.exists(file.path(outdir, "report.qmd")))
  expect_true(is.character(p) && length(p) == 1)
})