#' Provides a dataframe of package description fields
#' 
#' @return dataframe with p
#' 
#' @examples 
#' write.csv(get_pkg_descriptions(base = TRUE)$pkg_df, 
#'           file = file.path(tempdir(), "package_list.csv"), 
#'           row.names = FALSE, quote = FALSE)
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
#' @param new_cran_db logical variable when set to TRUE causes the function to 
#' perform a get from CRAN to get a fresh copy of the database. Default
#' is \code{FALSE}.
#' @param get_cran_db function to return a cran or cran like database. If
#' left \code{NULL}, it will be defined with     
#' get_cran_db <- get_cran_db_factory(new_cran_db = new_cran_db)
#' 
#' @importFrom stringi stri_length
#' @importFrom tools package_dependencies
#' @importFrom utils packageDescription available.packages
#' @export
get_pkg_descriptions <- function(pkgs = NULL, lib.loc = NULL,
           fields = NULL,
           base = FALSE, 
           dependencies = FALSE,
           which = c("Depends", "Imports", "LinkingTo"),
           recursive = TRUE, reverse = FALSE, 
           verbose = getOption("verbose"), new_cran_db = FALSE,
           get_cran_db = NULL) {
  possible_fields <- c("Package", "Version", "Priority", "Depends", "Imports", 
                       "LinkingTo", "Suggests", "Enhances", "License", 
                       "License_is_FOSS", "License_restricts_use", 
                       "OS_type", "Archs", "MD5sum", "NeedsCompilation", 
                       "File", "Repository"
  )
  if (is.null(fields)) {
    fields <- possible_fields
  } else {
    fields <- intersect(fields, possible_fields)
  }
  if (is.null(pkgs))
    pkgs <- get_pkg_list(base = base)
  if (is.null(get_cran_db))
    get_cran_db <- get_cran_db_factory()
  
  db = get_cran_db()
  required <- pkgs
  
  if (dependencies) {
    pkg_dependencies <- tools::package_dependencies(
      pkgs, db = db,
      which = which,
      recursive = recursive, reverse = reverse,
      verbose = verbose)
    required <- unique(c(required, sort(unique(unlist(pkg_dependencies)))))
    pkg_dependencies_df <- make_pkg_dep_df(pkg_dependencies)
  }
    
  packages <- as.data.frame(db, stringsAsFactors = FALSE)
  pkg_df <- packages[packages$Package %in% required, fields]
  
  if (dependencies) {
    pkg_dependencies_df <- merge(pkg_dependencies_df, pkg_df, by.x = "Dependency",
                               by.y = "Package", all.x = TRUE, sort = FALSE)
  }
  pkg_df <- pkg_df[order(pkg_df$Package), ]
  if (dependencies) {
    pkg_dependencies_df <-
      pkg_dependencies_df[order(pkg_dependencies_df$Package, 
                                pkg_dependencies_df$Dependency),
                          c("Package", "Dependency", "License")]
   pkg_dependencies_df <- pkg_dependencies_df[!duplicated(pkg_dependencies_df), ]
  }
  pkg_df <- pkg_df[!duplicated(pkg_df), ]

    if (dependencies) {
    list(pkg_df = pkg_df, pkg_dependencies_df = pkg_dependencies_df)
  } else {
    list(pkg_df = pkg_df)
  }
}
