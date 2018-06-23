#' Make package dependency list
#'
#' Gathers the dependencies from each source package and combines the lists
#' without duplication.
#'
#' @param source_names character vector containing the names of the custom
#' packages to be installed from source.
#' @param path character vector of length one having the path to R's
#' \code{library} directory, which contains the packages being updated from
#' source. This may or may or may not be the same as the system wide
#' \code{library} directory. It could be a user directory.
#' @export
make_package_dependency_list <- function(source_names, path) {
  dependencies <- character(0)
  for (name in source_names) {
    dependencies <- unique(c(dependencies, get_deps(paste0(path, name))))
  }
  dependencies
}
