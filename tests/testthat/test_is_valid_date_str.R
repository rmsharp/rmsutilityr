context("test_is_valid_date_str")
library(testthat)
is_valid_date_str(c("13-21-1995", "20-13-98", "5-28-1014", 
                    "1-21-15", "2-13-2098", "25-28-2014"), format = "%m-%d-%y")

dates <- c("13-21-1995", "20-13-98", "5-28-1014", "1-21-15", "2-13-2098", 
           "25-28-2014", "11111111", "99999999", "19821021", "12345678")
test_that("is_valid_date_str returns correct logical values", {
  expect_equal(is_valid_date_str(dates), c(FALSE, FALSE, TRUE, TRUE, TRUE, 
                                           FALSE, TRUE, FALSE, TRUE, FALSE))
})
