context("test_get_html_comment_text_lines_and_labels_from_files")
library(testthat)
library(rmsutilityr)
file = system.file("testdata","find_html_comment_test_file_1.Rmd", 
                       package = "rmsutilityr")
lines <- readLines(file)
test_that(paste0("delete_selected_comments_from_lines returns ",
                 "correct object with no label argument"), {
  lines_minus_all_comments <- delete_selected_comments_from_lines(lines)
  expect_equal(length(lines_minus_all_comments), 38)
 })                 
test_that(paste0("delete_selected_comments_from_lines returns ",
                 "correct object with one label"), {
  lines_minus_RMS_comments <- 
    delete_selected_comments_from_lines(lines, label = "RMS")
  expect_equal(length(lines_minus_RMS_comments), 42)
 })                 
test_that(paste0("delete_selected_comments_from_lines returns ",
                 "correct object with two labels"), {
  lines_minus_RMS_and_TJH_comments <- 
    delete_selected_comments_from_lines(lines, label = c("RMS", "TJH"))
  expect_equal(length(lines_minus_RMS_and_TJH_comments), 40)
 })                 
