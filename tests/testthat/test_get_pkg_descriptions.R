context("test_get_pkg_descriptions")
library(stringr)


test_that("get_pkg_description retrieve correct information", {
  all_pkgs <- get_pkg_descriptions()
  stringr_not_recursive_desc <- get_pkg_descriptions("stringr", dependencies = FALSE)
  stringr_recursive_desc <- get_pkg_descriptions("stringr", dependencies = TRUE)
  expect_false(any(str_detect(stringr_not_recursive_desc$Package, "tools")))
  expect_false(any(str_detect(stringr_not_recursive_desc$Package, "stringi")))
  expect_true(any(str_detect(stringr_not_recursive_desc$Package, "stringr")))
  expect_true(any(str_detect(stringr_recursive_desc$Package, "stringi")))
})
