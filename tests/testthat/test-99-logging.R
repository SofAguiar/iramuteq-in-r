test_that("log_provenance writes expected artifacts", {
  td <- tempdir()
  outdir <- file.path(td, "iramuteq_log_test")
  
  # Create a small temp input file
  f <- tempfile(fileext = ".txt")
  writeLines("hello", con = f)
  
  ok <- log_provenance(outdir, params = list(x = 1), input_files = c(f), hash = "md5")
  expect_true(isTRUE(ok) || isTRUE(invisible(ok)))
  
  expect_true(file.exists(file.path(outdir, "params.dput")))
  expect_true(file.exists(file.path(outdir, "params.txt")))
  expect_true(file.exists(file.path(outdir, "sessionInfo.txt")))
  expect_true(file.exists(file.path(outdir, "checksums.txt")))
  expect_true(file.exists(file.path(outdir, "run.log")))
  
  log_message(outdir, "Test message")
  expect_true(file.size(file.path(outdir, "run.log")) > 0)
})