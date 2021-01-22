context("test_get_function_names")


test_that("get_function_names finds them", {
  # this is not a robust test. The "\n" do not act like new lines.
  lines <- c(
    "abc <- function (",
    "qrs <- function(",
    "tuv <-
   function(",
    "\nwxy   <- \nfunction(")
  function_names <- get_function_names(lines)
  expect_equal(function_names, c("abc", "qrs", "tuv", "wxy"))
})
test_that("get_function_names finds them", {
  # this is not a robust test. The "\n" do not act like new lines.
  lines <- c(
    "abc <- function (",
    "#qrs <- function(",
    "tuv <-
   function(",
    "\nwxy   <- \nfunction(")
  function_names <- get_function_names(lines)
  expect_equal(function_names, c("abc", "tuv", "wxy"))
})