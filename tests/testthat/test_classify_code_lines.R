context("test_classify_code_lines")
library(stringi)
## testthat comment 
#' @description shows how to test classify_code_lines()
 

test_that("classify_code_lines correctly classifies code lines", {
  library(rmsutilityr)
  library(stringi)
  files <- list.files(path = ".", full.names = TRUE)
  file_num <- seq_along(files)[stri_detect_fixed(files, "classify_code_lines.R")]
  test_one <- classify_code_lines(files[file_num])
  test_names <- names(test_one)
  expect_equal(test_names, c("files", "lines", "code", "blank_lines", 
                             "roxygen_doc_lines", "comments"))
  expect_equal(test_one[[2]], 23)
  expect_equal(test_one[[3]], 19)
  expect_equal(test_one[[4]], 2)
  expect_equal(test_one[[5]], 1)
  expect_equal(test_one[[6]], 1)
  test_two <- classify_code_lines()
  expect_true(test_two[[2]] > 23)
})
