#' Provides a sorted CSV list of packages loaded in an R session
#' 
#' @return character string with package names separated by \",\\n\".
#' 
#' @examples 
#' write.csv(data.frame(packages = get_pkg_list(base = TRUE), stringsAsFactors = FALSE), 
#'           file = file.path(tempdir(), "package_list.csv"), 
#'           row.names = FALSE, quote = FALSE)
#' 
#' @param session_info object returned by \code{sessionInfo}. Defaults to
#' sessionInfo().
#' @param base logical value if TRUE base packages are included
#' @importFrom utils sessionInfo
#' @export
get_pkg_list <- function(session_info = sessionInfo(), base = FALSE) {
  pkg_group <- intersect(names(session_info), c("basePkgs", "otherPkgs", "loadedOnly"))
  csv_string <- c()
  for (group in pkg_group) {
    if (group == "basePkgs") {
      if (base)
        csv_string <- c(csv_string, session_info[["basePkgs"]])
    } else {
      csv_string <- c(csv_string, names(session_info[[group]]))
    }
  }
  #csv_string <- paste0(paste(sort(csv_string), collapse = ",\n"), "\n")
  csv_string <- sort(csv_string)
  csv_string
}
