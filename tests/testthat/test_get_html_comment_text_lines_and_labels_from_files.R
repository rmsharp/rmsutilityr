## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
context("test_get_html_comment_text_lines_and_labels_from_files")
library(testthat)
library(rmsutilityr)
files <- system.file("testdata","find_html_comment_test_file_1.Rmd", 
                       package = "rmsutilityr")
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with no label argument"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files)
                   
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file_1.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(26L, 16L, 30L, 34L, 25L, 22L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("I", "RMS", "RMS", "RMS", "SRR", "TJH"))
  expect_equal(html_comment_lines_and_labels$file[1],
               "find_html_comment_test_file_1.Rmd")
  
})                 
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with multiple labels"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files, label = c("RMS", "TJH"))
  
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file_1.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(16L, 30L, 34L, 22L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("RMS", "RMS", "RMS", "TJH"))
 })                 
test_that(paste0("get_html_comment_text_lines_and_labels_from_files returns ",
                 "correct object with one label"), {
  html_comment_lines_and_labels <- 
    get_html_comment_text_lines_and_labels_from_files(files, label = "RMS")
  
  expect_equal(html_comment_lines_and_labels$file[1], 
               "find_html_comment_test_file_1.Rmd")
  expect_equal(html_comment_lines_and_labels$comment_start_line, 
               c(16L, 30L, 34L))
  expect_equal(html_comment_lines_and_labels$comment_label, 
               c("RMS", "RMS", "RMS"))
 })                 
