#' Install R package from package source
#'
#' Takes a list of packages (\code{source_names}) and the path
#' (\code{source_path}) to their common location and
#' installs them into the \code{install_path} directory.
#'
#' @param source_names character vector having one source package name per
#' cell.
#' @param source_path character vector of length one having the directory
#' path of where the package sources (*.tar.gz) reside.
#' @param install_path character vector of length one having the directory
#' path of where packages are to be installed.
#' @importFrom utils install.packages
#' @export
install_from_source <- function(source_names, source_path, install_path) {
  for (source_name in source_names) {
    source <- max(list.files(path = source_path,
                             pattern = paste0(source_name, ".*.tar.gz")))
    install.packages(paste0(source_path, source), type = "source", repos = NULL,
                     lib = install_path)
  }
}
