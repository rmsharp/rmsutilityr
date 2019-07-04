context("test_combine_vectors")

test_that("combine_vectors returns the right vector", {
  expect_equal(combine_vectors(LETTERS, letters), LETTERS)
  expect_equal(combine_vectors(LETTERS, rep(NA, 26)), LETTERS)
  expect_equal(combine_vectors(rep(NA, 26), letters), letters)
  expect_true(is.na(combine_vectors(NA, NA)))
  expect_error(combine_vectors(LETTERS, "a"))
  vec2 <- c("a", NA, NA, "d")
  vec1 <- c("A", "B", NA, NA)
  expect_equal(combine_vectors(vec1, vec2), c("A", "B", NA, "d"))   
})
