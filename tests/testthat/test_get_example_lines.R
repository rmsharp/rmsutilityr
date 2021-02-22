context("test_get_example_lines.R")
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
    c("#' library(stringi, quietly = TRUE)",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      "#'                                  \"TO BE CAPITALIZED.\"))",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      paste0("#'                                  \"TO BE CAPITALIZED.\"), ",
             "lower = TRUE)")))
  file = system.file("testdata","get_and_or_list.R", 
                     package = "rmsutilityr")
  expect_equal(
    get_example_lines(file),
    c("#' library(stringi, quietly = TRUE)",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      "#'                                  \"TO BE CAPITALIZED.\"))",
      "#' capitalize_first_letter(words = c(\"a\", \"vector of words\",",
      paste0("#'                                  \"TO BE CAPITALIZED.\"), ",
             "lower = TRUE)")))
})
