context("test_get_pkg_list")
library(stringi)

test_that("get_pkg_list retrieve package names", {
  pkgs <- get_pkg_list()
  expect_true(any(stri_detect_fixed(pkgs, "tools")))
})
test_that("get_pkg_list does not return base package when told not to", {
  pkgs <- get_pkg_list(base = TRUE)
  expect_true(any(stri_detect_fixed(pkgs, "stats")))
  pkgs <- get_pkg_list()
  expect_false(any(stri_detect_fixed(pkgs, "stats")))
  pkgs <- get_pkg_list(base = FALSE)
  expect_false(any(stri_detect_fixed(pkgs, "stats")))
})

