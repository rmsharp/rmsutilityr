#' Make a list of functions used flags
#' 
#' @return A list of two functions: `set` and `value`.
#'         The function `set` takes a logical variable of length 1 that is used
#'         to set the logical value returned by `value`.
#'         The function `value` does not take  an argument and returns either
#'         the default value provided when the function was created or the 
#'         last value provided to the `set` function.
#' @examples 
#' # Create function list with `value` defaulted to TRUE
#' tf_flag <- make_true_false_flag(TRUE)
#' tf_flag$value()
#' # reset `value` 
#' tf_flag$set(FALSE)
#' tf_flag$value()
#' # Create function list with `value` defaulted to FALSE
#' tf_flag <- make_true_false_flag(FALSE)
#' tf_flag$value()
#' # reset `value` 
#' tf_flag$set(TRUE)
#' tf_flag$value()
#' 
#' @param set_value logical value used to set what is returned
#' @export
make_true_false_flag <- function(set_value) {
  if (is.null(set_value) | !"logical" %in% class(set_value))
    stop("Must provide seed value of TRUE or FALSE")
  
  list(
    set = function(value) {
      set_value <<- value[[1]] # protection against > 1 value
    },
    value = function() {
      return(set_value)
    }
  )
}
