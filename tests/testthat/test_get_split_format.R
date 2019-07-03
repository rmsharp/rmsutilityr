context("test_get_split_format")
library(stringi)

fmt <- stri_c("p{1cm}p{2.5cm}p{1cm}p{1cm}p{2cm}p{1cm}p{6cm}p{1cm}p{2cm}")
test_that("get_split_format forms correct format statements", {
  format <- get_split_format(fmt, cm = 15)
  expect_equal(format$split_format[[1]], "p{0cm}p{1cm}p{2.5cm}p{1cm}p{1cm}p{2cm}p{1cm}")
  expect_equal(format$cols[1, ], c(1, 6))
})
