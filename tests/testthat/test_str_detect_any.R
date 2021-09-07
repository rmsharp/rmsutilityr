context("test_str_detect_any.R")
library(testthat)
test_that("identified match with multiple fixed patterns", {
  patterns_1 <- c("LGPL")
  patterns_2 <- c("LGPL", "AGPL")
  strings_1 <- c("License - AGPL")
  strings_2 <- c("License - GPL-2", strings_1)
  strings_3 <- c("MIT + file LICENSE", strings_2)
  expect_equal(str_detect_any(strings_1, patterns = patterns_1, 
                              type = "fixed"),
               c(FALSE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_1, 
                              type = "fixed"),
               c(FALSE, FALSE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_1, 
                              type = "fixed"),
               c(FALSE, FALSE, FALSE))
  expect_equal(str_detect_any(strings_1, patterns = patterns_2, 
                              type = "fixed"),
               c(TRUE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_2, 
                              type = "fixed"),
               c(FALSE, TRUE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_2, 
                              type = "fixed"),
               c(FALSE, FALSE, TRUE))
  
})
test_that("identified match with multiple regex patterns", {
  patterns_1 <- c("LGPL")
  patterns_2 <- c("LGPL", "AGPL")
  strings_1 <- c("License - AGPL")
  strings_2 <- c("License - GPL-2", strings_1)
  strings_3 <- c("MIT + file LICENSE", strings_2)
  expect_equal(str_detect_any(strings_1, patterns = patterns_1, 
                              type = "regex"),
               c(FALSE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_1, 
                              type = "regex"),
               c(FALSE, FALSE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_1, 
                              type = "regex"),
               c(FALSE, FALSE, FALSE))
  expect_equal(str_detect_any(strings_1, patterns = patterns_2, 
                              type = "regex"),
               c(TRUE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_2, 
                              type = "regex"),
               c(FALSE, TRUE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_2, 
                              type = "regex"),
               c(FALSE, FALSE, TRUE))
  
})
test_that("identified match with multiple regex patterns", {
  patterns_1 <- c(".GPL")
  patterns_2 <- c("LGPL", "AG[A-Z]")
  strings_1 <- c("License - AGPL")
  strings_2 <- c("License - GPL-2", strings_1)
  strings_3 <- c("MIT + file LICENSE", strings_2)
  strings_4 <- c("MIT + file LICENSE", "AGS", "AGSS")
  expect_equal(str_detect_any(strings_1, patterns = patterns_1, 
                              type = "regex"),
               c(TRUE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_1, 
                              type = "regex"),
               c(TRUE, TRUE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_1, 
                              type = "regex"),
               c(FALSE, TRUE, TRUE))
  expect_equal(str_detect_any(strings_4, patterns = patterns_1, 
                              type = "regex"),
               c(FALSE, FALSE, FALSE))
  expect_equal(str_detect_any(strings_1, patterns = patterns_2, 
                              type = "regex"),
               c(TRUE))
  expect_equal(str_detect_any(strings_2, patterns = patterns_2, 
                              type = "regex"),
               c(FALSE, TRUE))
  expect_equal(str_detect_any(strings_3, patterns = patterns_2, 
                              type = "regex"),
               c(FALSE, FALSE, TRUE))
  expect_equal(str_detect_any(strings_4, patterns = patterns_2, 
                              type = "regex"),
               c(FALSE, TRUE, TRUE))
  
})
