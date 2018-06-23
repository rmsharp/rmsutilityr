#' Check to see of a package is installed
#'
#' This function takes a package name and sees if it is installed on the
#' present system.
#'
#' @param mypkg is a character variable containing a package name
#' @examples
#' not_installed("stringi")
#' @import stringi
#' @importFrom utils installed.packages
#' @export
not_installed <- function(mypkg) !is.element(mypkg, 
                                             utils::installed.packages()[ , 1])
