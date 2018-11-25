context("test_classify_code_lines")
library(stringi)
test_that("classify_code_lines correctly classifies code lines", {
  library(rmsutilityr)
  library(stringi)
  files <- list.files(path = ".", full.names = TRUE)
  file_num <- seq_along(files)[stri_detect_fixed(files, "classify_code_lines.R")]
  test <- classify_code_lines(files[file_num])
  expect_equal(test[[2]], 16)
  expect_equal(test[[3]], 14)
  expect_equal(test[[4]], 2)
  expect_equal(test[[5]], 0)
  expect_equal(test[[6]], 0)
})


