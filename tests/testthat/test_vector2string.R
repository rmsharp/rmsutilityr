context("test_vector2string")

test_that("vector2string makes the right string", {
  binV <- c(1, 0, 0, 1)
  strS <- vector2string(binV, type = "int")
  expect_equal(strS, "1', '0', '0', '1")
  ## Here we convert a binary vector to string representing a binary sequence:
  binV <- c(1,0,0,1)
  seqS <- vector2string(binV, SS = " ", type = "char")
  expect_equal(seqS, "1 0 0 1")
  
  ## Here we convert a vector of substrings to colon-separated string:
  subsV <- c("I", "am", "done")
  strS <- vector2string(subsV, SS = ":", type = "char")
  expect_equal(strS, "I:am:done")
  
  ## Making an SQL IS IN statement
  ids <- c(" 12345", "4X3200", "1X2890")
  ids_str <- stri_c("and master.id in ('", vector2string(ids, "', '"), "') ")
  expect_equal(ids_str, "and master.id in (' 12345', '4X3200', '1X2890') ")
  expect_equal(vector2string("oneWord"), "oneWord")
})
