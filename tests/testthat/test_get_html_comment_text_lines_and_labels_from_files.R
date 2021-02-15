context("test_get_html_comment_text_lines_and_labels_from_files")
library(testthat)
library(rmsutilityr)
files = system.file("testdata","find_html_comment_test_file.Rmd", 
                       package = "rmsutilityr")
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with no label argument"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files)
                   
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(25L, 16L, 29L, 33L, 24L, 21L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("I", "RMS", "RMS", "RMS", "SRR", "TJH"))
  
})                 
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with multiple labels"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files, label = c("RMS", "TJH"))
  
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(16L, 29L, 33L, 21L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("RMS", "RMS", "RMS", "TJH"))
 })                 
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with one label"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files, label = "RMS")
  
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(16L, 29L, 33L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("RMS", "RMS", "RMS"))
 })                 
