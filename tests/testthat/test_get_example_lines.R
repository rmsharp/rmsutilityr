context("test_get_example_lines.R")
library(stringi)
library(testthat)
test_that("get_example_lines returns the correct vector of lines", {
  file = system.file("testdata","capitalize_first_letter.R", 
                     package = "rmsutilityr")
  expect_equal(
    get_example_lines(file),
    c("#' library(stringi, quietly = TRUE)",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      "#'                                  \"TO BE CAPITALIZED.\"))",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      paste0("#'                                  \"TO BE CAPITALIZED.\"), ",
             "lower = TRUE)")))

  file = system.file("testdata","blank_fill_ids.R", 
                     package = "rmsutilityr")
  expect_equal(
    get_example_lines(file),
    stri_c("#' new_ids <- blank_fill_ids(c(\"12345\", \"1X1234\", ", 
           "\"1234\",\"2 3456\"))"))
  file = system.file("testdata","get_and_or_list.R", 
                     package = "rmsutilityr")
  expect_equal(
    get_example_lines(file),
    c("#' \\donttest{", "#' get_and_or_list(c(\"Bob\", \"John\")) # \"Bob and John\"", 
      "#' get_and_or_list(c(\"Bob\", \"John\"), \"or\") # \"Bob or John\"", 
      "#' get_and_or_list(c(\"Bob\", \"John\", \"Sam\", \"Bill\"), \"or\")", 
      "#' # \"Bob, John, Sam, or Bill\"", "#' }", "#'"))
})
