context("test_get_relative_path_file_names")
library(testthat)
new_files <-
  structure(
    list(
      original_file = c(
        "man/inst/devdoc/my_model.Rmd",
        "man/inst/devdoc/dev_sec/section_1.Rmd",
        "man/inst/devdoc/dev_sec/section_2.Rmd",
        "man/inst/devdoc/dev_sec/section_3.Rmd",
        "man/inst/devdoc/dev_sec/dev_sec3/section_3a.Rmd",
        "man/inst/devdoc/references.bib"
      ),
      new_file = c(
        "./my_model.Rmd",
        "dev_sec/section_1.Rmd",
        "dev_sec/section_2.Rmd",
        "dev_sec/section_3.Rmd",
        "dev_sec/dev_sec3/section_3a.Rmd",
        "./references.bib"
      )
    ),
    class = "data.frame",
    row.names = c(NA,-6L)
  )
new_file_paths <-
  get_relative_path_file_names(new_files$original_file, new_path = ".")
test_that("get_relative_path_file_names creates correct file paths", {
  expect_true(all(new_file_paths$original_file == new_files$original_file))
  expect_true(all(new_file_paths$new_file == new_files$new_file))
})
