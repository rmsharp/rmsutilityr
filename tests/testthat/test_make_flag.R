context("test_make_flag")


test_that("function list made by make_flag", {
  # Create function list with `value` defaulted to TRUE
  tf_flag <- make_flag(TRUE)
  expect_true(class(tf_flag) == "list")
  expect_true(tf_flag$value())
  tf_flag$set(FALSE)
  expect_false(tf_flag$value())
  # Create function list with `value` defaulted to FALSE
  tf_flag <- make_flag(FALSE)
  expect_false(tf_flag$value())
  tf_flag$set(TRUE)
  expect_true(tf_flag$value())
})
test_that("function list made by make_flag", {
  # Create function list with `value` defaulted to "START"
  tf_flag <- make_flag("START", flags = c("START", "STOP"))
  expect_true(class(tf_flag) == "list")
  expect_true(tf_flag$value() == "START")
  tf_flag$set("STOP")
  expect_true(tf_flag$value() == "STOP")
  # Create function list with `value` defaulted to "STOP"
  tf_flag <- make_flag("STOP", flags = c("START", "STOP"))
  expect_true(tf_flag$value() == "STOP")
  tf_flag$set("START")
  expect_true(tf_flag$value() == "START")
})
test_that("function list made by make_flag", {
  # Create function list with `value` defaulted to "START"
  tf_flag <- make_flag("MIDDLE", flags = c("BEGINNING", "MIDDLE", "END"))
  expect_true(class(tf_flag) == "list")
  expect_true(tf_flag$value() == "MIDDLE")
  tf_flag$set("END")
  expect_true(tf_flag$value() == "END")
  # Create function list with `value` defaulted to "STOP"
  tf_flag <- make_flag("STOP", flags = c("START", "STOP"))
  expect_true(tf_flag$value() == "STOP")
  tf_flag$set("START")
  expect_true(tf_flag$value() == "START")
})
