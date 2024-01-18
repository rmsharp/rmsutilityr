context("test_write_files_after_deleting_selected_comments")
library(testthat)
library(rmsutilityr)
files <- system.file("testdata","find_html_comment_test_file_1.Rmd", 
                   package = "rmsutilityr")
files <- c(files, system.file("testdata", "sample_dir", 
                              "find_html_comment_test_file_2.Rmd", 
                              package = "rmsutilityr"))
test_that("files are written after deleting all comments ", {
  skip_on_cran()
  new_path <-  tempdir()
  label <- ""
  new_files <- write_files_after_deleting_selected_comments(
    files, new_path, label = label, isolated = TRUE, overwrite = TRUE)
  comments_left <- 0
  for (file in new_files$new_file) {
    lines <- readLines(file)
    lines_and_labels <- return_html_comment_text_lines_and_labels(lines, label)
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 0)
})
test_that("files are written correctly after deleting selected comments ", {
  skip_on_cran()
  new_path <-  tempdir()
  label <- "RMS"
  new_files <- write_files_after_deleting_selected_comments(
    files, new_path, label = label, isolated = TRUE, overwrite = TRUE)
  comments_left <- 0
  for (file in new_files$new_file) {
    lines <- readLines(file)
    lines_and_labels <- 
      return_html_comment_text_lines_and_labels(lines, label = "")
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 6)
  comments_left <- 0
  for (file in new_files$new_file) {
    lines <- readLines(file)
    lines_and_labels <- 
      return_html_comment_text_lines_and_labels(lines, label = "RMS")
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 0)
})
test_that(paste0("files are not written when they already exist if ", 
                 "'overwrite' is 'FALSE' "), {
  skip_on_cran()
  new_path <-  tempdir()
  label <- "RMS"
  expect_error(write_files_after_deleting_selected_comments(
    files, new_path, label = label, isolated = TRUE, overwrite = FALSE),
    paste0("find_html_comment_test_file_1.Rmd' exists and you requested ",
           "it not be replaced."))
})
