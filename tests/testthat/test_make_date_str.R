context("test_make_date_str")

library(stringi, quietly = TRUE)
library(lubridate, quietly = TRUE)

test_that("make_date_str makes the right string", {
  str_1 <- make_date_str(ymd("2010/3/21", quiet = TRUE))
  str_2 <- make_date_str(ymd("2010/3/21", quiet = TRUE), abbr = TRUE)
  str_3 <- make_date_str(ymd("2010/3/21", quiet = TRUE), label = FALSE)
  str_4 <- make_date_str(ymd("2010/3/21", quiet = TRUE), abbr = FALSE)
  expect_equal(str_1, "Mar 21, 2010")
  expect_equal(str_2, "Mar 21, 2010")
  expect_equal(str_3, "3 21, 2010")
  expect_equal(str_4, "March 21, 2010")
})
