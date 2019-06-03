#' Provides a dataframe of package description fields
#' 
#' @return dataframe with p
#' 
#' @examples 
#' write.csv(data.frame(get_pkg_descriptions(base = TRUE), stringsAsFactors = FALSE), 
#'           file = "package_list.csv", row.names = FALSE, quote = FALSE)
#' 
#' @param pkgs character vector of package names. Defaults to packages found with
#'  sessionInfo().
#' @param lib.loc a character vector of directory names of R libraries, or NULL. 
#' The default value of NULL corresponds to all libraries currently known. 
#' If the default is used, the loaded packages and namespaces are searched 
#' before the libraries.
#' @param fields a character vector giving the tags of fields to return 
#' (if other fields occur in the file they are ignored).
#' @param base logical value if TRUE base packages are included
#' @param dependencies logical value if TRUE, dependencies are recursively 
#' added.
#' @param which	a character vector listing the types of dependencies, 
#' a subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances").
#' Character string "all" is shorthand for that vector, character string 
#' "most" for the same vector without "Enhances".
#' @param recursive logical: should (reverse) dependencies of (reverse)
#' dependencies (and so on) be included?
#' @param reverse	logical: if FALSE (default), regular dependencies are 
#' calculated, otherwise reverse dependencies.
#' @param verbose	logical indicating if output should monitor the 
#' package search cycles.
#' @importFrom stringi stri_length
#' @importFrom tools package_dependencies
#' @importFrom utils packageDescription available.packages
#' @export
get_pkg_descriptions <- function(pkgs = NULL, lib.loc = NULL,
           fields = c("Package", "Type", "Title", "Version",  
                      "Author", "Creator", "Maintainer", "Description", 
                      "Depends", "Imports", "Suggests", "Encoding", "License",
                      "RoxygenNote", "LazyData", "VignetteBuilder",
                      "URL", "BugReports"),
           base = FALSE, 
           dependencies = FALSE,
           which = c("Depends", "Imports", "LinkingTo"),
           recursive = TRUE, reverse = FALSE, 
           verbose = getOption("verbose")) {
  possible_fields <- c("Package", "Type", "Title", "Version", "Author",
                       "Creator", "Maintainer", "Description", "Depends", 
                       "Imports", "Suggests", "Encoding", "License", 
                       "RoxygenNote", "LazyData", "VignetteBuilder", 
                       "URL", "BugReports")
  fields <- intersect(fields, possible_fields)
  if (is.null(pkgs))
    pkgs <- get_pkg_list(base = base)
  required <- pkgs
  if (dependencies) {
    required <- unique(
      c(required, sort(
        unique(
          unlist(tools::package_dependencies(
            pkgs, db = utils::available.packages(
              repos="http://cran.us.r-project.org"),
            which = which,
            recursive = recursive, reverse = reverse,
            verbose = verbose)))
      )))
  }
  meta_data <- list(length(required))
  con <- url("http://cran.r-project.org/src/contrib/PACKAGES")
  packages <- read.dcf(con, fields = fields, all = TRUE)
  close(con)
  
  for (pkg in required) {
    meta_data[[pkg]] <- as.list(packages[packages$Package == pkg, fields])
#    meta_data[[pkg]] <- utils::packageDescription(pkg, lib.loc = lib.loc, fields)
  }
  ## This is ugly code and likely fragile
  meta_df <- data.frame()
  for (pkg in names(meta_data)[stri_length(names(meta_data)) > 0]) {
    df <- c(meta_data[[pkg]][names(meta_data[[pkg]])])
    meta_df <- rbind(meta_df, df, stringsAsFactors = FALSE)
  }
  meta_df
}
