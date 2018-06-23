#' Wrapper function around \code{colfmt()} to make the font red.
#'
#' In addition to adding color, underscores are prepended and
#' append for HTML and \code{\\emph{}} is used for LaTeX
#'
#' @param x character string with comment
#' @export
rcomment <- function(x) {
  colfmt(x, color = "red")
}
