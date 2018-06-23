#' Get Package Dependencies
#' 
#' @return Character vector of package dependencies based on the contents of
#' the \code{DESCRIPTION} file.
#' 
#'
#' Written by Josh O'Brien on stackoverflow on May 13 '15 at 21:42
#' @param path character vector of length one having the directory path to
#' where new version of the custom package exists. This can be the 
#' full path name of the directory in which the package exists or the full
#' path name of the source version of the package.
#' @examples 
#' \dontrun{
#' library(rmsutilityr)
#' ## Directory where package is placed when installed.
#' dependencies <- get_deps("c:/R library/snprcspf")
#' ## Source file of package
#' dependencies <- get_deps("c:/R data/snprcspf.1.0.1.tar.gz")
#' remove_these <- c("stats", "tools", "utils", "rmsutilityr", "animalr")
#' dependencies <- remove_these_str(dependencies, expunge = remove_these)
#' repo <- "https://cran.revolutionanalytics.com" ## or your mirror of choice
#' lib_path <- "c:/R Library/"
#' install.packages(dependencies, type = "binary", repos = repo, lib = lib_path)
#' }

#' @export
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val <- val[val != "R"]
  val <- remove_these_str(.str = val, 
                          expunge = c("base", "datasets", "graphics", 
                                      "grDevices", "methods", "stats", "tools", 
                                      "utils"), ignore_case = FALSE)
  val
}
