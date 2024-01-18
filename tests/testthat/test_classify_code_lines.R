context("test_classify_code_lines")
library(stringi)
## testthat comment 
#' @description shows how to test classify_code_lines()
 

test_that("classify_code_lines correctly classifies code lines", {
  file <- system.file("testdata", "classify_code_lines.R", 
                          package = "rmsutilityr")
  test_one <- classify_code_lines(file)
  test_names <- names(test_one)
  expect_equal(test_names, c("files", "lines", "code", "blank_lines", 
                             "roxygen_doc_lines", "comments"))
  expect_equal(test_one[[2]], 58)
  expect_equal(test_one[[3]], 44)
  expect_equal(test_one[[4]], 2)
  expect_equal(test_one[[5]], 12)
  expect_equal(test_one[[6]], 0)
  test_two <- classify_code_lines()
  expect_true(test_two[[2]] > 25)
  expect_error(classify_code_lines(path = "/"), "No files found")
})
test_that("classify_code_lines creates a list object", {
  file <- system.file("testdata", "classify_code_lines.R", 
                          package = "rmsutilityr")
  test_one <- classify_code_lines(file)
  expect_is(test_one, "list")
})
