#' Returns the CRAN database but only refreshes once a day unless forced
#'
#' An argument (\code{new_cran_db}; logical) can be used to control whether or 
#' not the CRAN database is refreshed each call. 
#' When set to TRUE the function performs a get from CRAN to get a fresh copy
#' of the database. Default is \code{FALSE}.

#' @return data base from CRAN
#' @export
get_cran_db_factory <- function() {
  first <- TRUE
  today <- NULL
  db <- c(Package = "A3", Version = "1.0.0", Priority = NA, 
          Depends = "R (>= 2.15.0), xtable, pbapply",
          Imports = NA, LinkingTo = NA, Suggests = "randomForest, e1071",
          Enhances = NA, License = "GPL (>= 2)", License_is_FOSS = NA,
          License_restricts_use = NA, OS_type = NA, Archs = NA, MD5sum = NA,
          NeedsCompilation = "no", File = NA, 
          Repository = "https://cran.us.r-project.org/src/contrib")
  ## new_cran_db logical variable when set to TRUE causes the function to
  ## perform a get from CRAN to get a fresh copy of the database. Default
  ## is \code{FALSE}.
  function(new_cran_db) {
    if (missing(new_cran_db))
      new_cran_db <- FALSE
    if (first) {
      db <<- utils::available.packages(repos="https://cran.us.r-project.org")
      first <<- FALSE
      today <<- Sys.Date()
    } else {
      if (today != Sys.Date() | new_cran_db)
        db <<- utils::available.packages(repos="https://cran.us.r-project.org")
      today <<- Sys.Date()
    }
    db
  }
}
