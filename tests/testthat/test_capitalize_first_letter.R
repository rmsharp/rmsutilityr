context("test_capitalize_first_letter")

test_that("capitalize_first_letter folds all other letters when directed", {
  words_one <- capitalize_first_letter(words = c("a", "vector of words",
                                                 "TO BE CAPITALIZED."))
  words_two <- capitalize_first_letter(words = c("a", "vector of words",
                                                 "TO BE CAPITALIZED."), 
                                       lower = TRUE)
  expect_equal(words_one[1], "A")
  expect_equal(words_one[2], "Vector Of Words")
  expect_equal(words_one[3], "TO BE CAPITALIZED.")
  expect_equal(words_two[1], "A")
  expect_equal(words_two[2], "Vector Of Words")
  expect_equal(words_two[3], "To Be Capitalized.")
})


