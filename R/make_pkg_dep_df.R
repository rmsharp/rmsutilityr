#' Make a package dependency dataframe from a list
#' 
#' @return dataframe object with columns Package and Dependency, which are the 
#' package name and the name of one of its dependents respectively
#' 
#' @param pkg_dependencies a package dependency matrix frompkg_dependencies <- tools::package_dependencies tools::package_dependencies
#' @export
make_pkg_dep_df <- function(pkg_dependencies) {
  pkg_dependencies_df <- data.frame()
  for( pkg in names(pkg_dependencies)) {
    if (length(pkg_dependencies[[pkg]]) > 0) {
      pkg_dependencies_df <- 
        rbind(pkg_dependencies_df, 
              data.frame(Package = rep(pkg, length(pkg_dependencies[[pkg]])),
                         Dependency = pkg_dependencies[[pkg]], 
                         stringsAsFactors = FALSE))
    } else {
      pkg_dependencies_df <- 
        rbind(pkg_dependencies_df, 
              data.frame(Package = pkg,
                         Dependency = NA, 
                         stringsAsFactors = FALSE))
    }
  }
  pkg_dependencies_df
}
