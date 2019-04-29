context("test_get_pkg_descriptions")
library(stringr)


test_that("get_pkg_descriptions retrieve correct information", {
  all_pkgs <- get_pkg_descriptions(pkgs = "stringr")
  which_imports_pkgs <- 
    get_pkg_descriptions(pkgs = "stringr", dependencies = TRUE,
                         which = "Imports", recursive = FALSE)
  which_all_pkgs <- get_pkg_descriptions(pkgs = "stringr", dependencies = TRUE,
                                         which = "all", recursive = FALSE)
  stringr_not_recursive_desc <- get_pkg_descriptions("stringr", dependencies = FALSE)
  stringr_recursive_desc <- get_pkg_descriptions("stringr", dependencies = TRUE)
  expect_false(any(str_detect(stringr_not_recursive_desc$Package, "tools")))
  expect_false(any(str_detect(stringr_not_recursive_desc$Package, "stringi")))
  expect_true(any(str_detect(stringr_not_recursive_desc$Package, "stringr")))
  expect_true(any(str_detect(stringr_recursive_desc$Package, "stringi")))
  expect_equal(nrow(all_pkgs), 1)
  expect_equal(nrow(which_imports_pkgs), 4)
  expect_true(all(which_all_pkgs$Package %in% c("stringr", "covr", "glue", "htmltools", "htmlwidgets", "knitr", 
                                                "magrittr", "rmarkdown", "stringi", "testthat")))
})
