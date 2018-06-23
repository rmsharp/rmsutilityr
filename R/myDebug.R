#' Runs debug once on a function that is in memory
#'
#' myDebug() is by Andrea Span
#' 
#' @param f name of function to be debuged.
#' @export
myDebug <-  function(f) {
  fname <- deparse(substitute(f))
  dump(fname, file = "tmp.R")
  source("tmp.R")
  do.call("debugonce", args = list(fname), envir = globalenv())
  invisible(NULL)
}
