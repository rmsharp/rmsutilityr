context("test_classify_code_lines")
library(stringi)
## testthat comment 
#' @description shows how to test classify_code_lines()
 

test_that("classify_code_lines correctly classifies code lines", {
  library(rmsutilityr)
  library(stringi)
  files <- list.files(path = ".", full.names = TRUE)
  file_num <- seq_along(files)[stri_detect_fixed(files, "classify_code_lines.R")]
  test <- classify_code_lines(files[file_num])
  test_names <- names(test)
  expect_equal(test_names, c("files", "lines", "code", "blank_lines", 
                             "roxygen_doc_lines", "comments"))
  expect_equal(test[[2]], 21)
  expect_equal(test[[3]], 17)
  expect_equal(test[[4]], 2)
  expect_equal(test[[5]], 1)
  expect_equal(test[[6]], 1)
})
