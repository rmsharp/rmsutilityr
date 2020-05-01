context("test_make_true_false_flag")


# Create function list with `value` defaulted to TRUE
tf_flag <- make_true_false_flag(TRUE)
test_that("function list made by make_true_false_flag", {
  expect_true(class(tf_flag) == "list")
  expect_true(tf_flag$value())
  tf_flag$set(FALSE)
  expect_false(tf_flag$value())
  tf_flag <- make_true_false_flag(FALSE)
  expect_false(tf_flag$value())
  tf_flag$set(TRUE)
  expect_true(tf_flag$value())
})
