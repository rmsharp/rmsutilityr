context("test_make_license_df")

test_that("make_license_df forms the correct dataframe", {
  df <- make_license_df()
  expect_equal(nrow(df), 39)
  expect_equal(length(unique(df$license)), 16)
})


