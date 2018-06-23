#' Coverts values from a vector to a concatenation of substrings separated
#' by user-specified string separator.
#'
#' LIMITATION: The function interprets all substrings in the vector either
#' as of type 'int' or 'char'. A function that interprets the type of each
#' substring dynamically may one day be written by an R-guru.
#'
#' Franc Brglez, Wed Dec 9 15:43:59 EST 2009
#'
#' @param vector integer or character vector
#' @param SS User-specified string separator. (default value: SS = "', '").
#' @param type Tell function the class of the \code{vector}.
#' @examples
#' library(stringi, quietly = TRUE)
#' ## Here we convert a binary vector to a binary string representing an integer:
#' binV <- c(1, 0, 0, 1)
#' strS <- vector2string(binV, type = "int")
#' ## Here we convert a binary vector to string representing a binary sequence:
#' binV <- c(1,0,0,1)
#' seqS <- vector2string(binV, SS=" ", type="char")
#' ## Here we convert a vector of substrings to colon-separated string:
#' subsV <- c("I", "am", "done")
#' strS <- vector2string(subsV, SS = ":", type = "char")
#'
#' ## Making an SQL IS IN statement
#' ids <- c(" 12345", "4X3200", "1X2890")
#' stri_c("and master.id in ('", vector2string(ids, "', '"), "') ")
#' @import stringi
#' @export
vector2string <- function(vector=c(" 12345", "4X3200", "1X2890"),
                          SS = "', '", type = "char") {
  if (type == "int") {
    string <- stri_c(stri_split_fixed(stri_c(vector), " "), collapse = SS)
  } else if (length(vector) == 1) {
    return(vector)
  } else {
    n <- length(vector)
    nm1 <- n - 1
    string <- ""
    for (i in 1:nm1) {
      tmp <- noquote(vector[i])
      string <- stri_c(string, tmp, SS, sep = "")
    }
    tmp <- noquote(vector[n])
    string <- stri_c(string, tmp, sep = "")
  }
  return(string)
} # vector2string
