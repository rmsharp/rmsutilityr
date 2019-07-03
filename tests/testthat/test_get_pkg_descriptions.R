context("test_get_pkg_descriptions")
library(stringi)


test_that("get_pkg_descriptions retrieve correct information", {
  get_cran_db <- get_cran_db_factory()
  all_pkgs <- get_pkg_descriptions(pkgs = "stringr")
  which_imports_pkgs <- 
    get_pkg_descriptions(pkgs = "stringr", dependencies = TRUE,
                         which = c("Imports"), recursive = FALSE, 
                         new_cran_db = FALSE, get_cran_db = get_cran_db)
  which_all_pkgs <- get_pkg_descriptions(pkgs = "stringr", dependencies = TRUE,
                                         which = "all", recursive = FALSE, 
                                         new_cran_db = FALSE, get_cran_db = get_cran_db)
  stringr_not_recursive_desc <- get_pkg_descriptions("stringr", dependencies = FALSE)
  stringr_recursive_desc <- get_pkg_descriptions("stringr", dependencies = TRUE, 
                                                 new_cran_db = FALSE, 
                                                 get_cran_db = get_cran_db)
  expect_false(any(stri_detect_fixed(stringr_not_recursive_desc$pkg_df$Package, "tools")))
  expect_false(any(stri_detect_fixed(stringr_not_recursive_desc$pkg_df$Package, "stringi")))
  expect_true(any(stri_detect_fixed(stringr_not_recursive_desc$pkg_df$Package, "stringr")))
  expect_true(any(stri_detect_fixed(stringr_recursive_desc$pkg_df$Package, "stringi")))
  expect_equal(nrow(all_pkgs$pkg_df), 1)
  expect_equal(nrow(which_imports_pkgs$pkg_df), 4)
  expect_true(all(which_all_pkgs$pkg_df$Package %in% c("stringr", "covr", "glue", "htmltools", "htmlwidgets", "knitr", 
                                                "magrittr", "rmarkdown", "stringi", "testthat")))
})
