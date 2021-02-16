context("test_write_files_after_deleting_selected_comments")
library(testthat)
library(rmsutilityr)
files = system.file("testdata","find_html_comment_test_file.Rmd", 
                   package = "rmsutilityr")
test_that("files are written after deleting all comments ", {
  new_path <-  tempdir()
  label <- ""
  new_files <- write_files_after_deleting_selected_comments(
    new_path, files, label = label, isolated = TRUE)
  comments_left <- 0
  for (file in new_files) {
    lines <- readLines(file)
    lines_and_labels <- return_html_comment_text_lines_and_labels(lines, label)
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 0)
})
test_that("files are written correctly after deleting selected comments ", {
  new_path <-  tempdir()
  label <- "RMS"
  new_files <- write_files_after_deleting_selected_comments(
    new_path, files, label = label, isolated = TRUE)
  comments_left <- 0
  for (file in new_files) {
    lines <- readLines(file)
    lines_and_labels <- 
      return_html_comment_text_lines_and_labels(lines, label = "")
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 3)
  comments_left <- 0
  for (file in new_files) {
    lines <- readLines(file)
    lines_and_labels <- 
      return_html_comment_text_lines_and_labels(lines, label = "RMS")
    comments_left <- comments_left + length(lines_and_labels[[1]])
  }
  expect_equal(comments_left, 0)
})
