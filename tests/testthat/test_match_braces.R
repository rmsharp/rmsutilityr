context("test_match_braces")

test_that("match_braces returns correct dataframe", {
  df <- match_braces("{123{456{78}9}10}")
  expect_equal(df$left, c(1, 5, 9))
  expect_equal(df$right, c(17, 14, 12))
})

test_that("is_even returns correct logical vector", {
  expect_equal(is_even(1:4), c(FALSE, TRUE, FALSE, TRUE))
})
test_that("is_odd returns correct logical vector", {
  expect_equal(is_odd(1:4), c(TRUE, FALSE, TRUE, FALSE))
})
