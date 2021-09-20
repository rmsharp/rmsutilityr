#' Returns the words that describe the integer values of the number provided
#' as an argument.
#'
#' From GitHubGistpsychemedia/numbers2words.R
#' Function by John Fox found here:
#' http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
#' Tweaks by AJH to add commas and "and" have been commented out since they
#' are wrong

#' @param x scaler that may be integer or number (real).
#' @export
numbers2words <- function(x) {
  helper <- function(x, first_x_len = NULL) {
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (is.null(first_x_len))
      first_x_len <- nDigits
    
    if (nDigits == 1)
      if (x == "0") {
        if (first_x_len > 1)
          txt <- ""
        else
          txt <- as.vector("zero")
      }
      else
        txt <- as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19)
        as.vector(teens[digits[1]])
    else
      trim(paste(tens[digits[2]],
                 Recall(as.numeric(digits[1]), first_x_len)))
    else if (nDigits == 3)
      trim(paste(ones[digits[3]], "hundred",
                 # else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                 Recall(makeNumber(digits[2:1]), first_x_len)))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes))
        stop(paste(x, "is too large!"))
      trim(paste(
        Recall(makeNumber(digits[nDigits:(3 * nSuffix + 1)]), first_x_len),
        suffixes[nSuffix], ",", Recall(makeNumber(digits[(3 * nSuffix):1]),
                                       first_x_len)))
    }
  }
  trim <- function(text) {
    #Tidy leading/trailing whitespace, space before comma
    text <- gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,", ",", text)))
    #Clear any trailing " and"
    text <- gsub(" and$", "", text)
    #Clear any trailing comma
    gsub("\ *,$", "", text)
  }
  makeNumber <- function(...)
    as.numeric(paste(..., collapse = ""))
  #Disable scientific notation
  opts <- options(scipen = 100)
  on.exit(options(opts))
  ones <-
    c("",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine")
  names(ones) <- 0:9
  teens <-
    c(
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen"
    )
  names(teens) <- 0:9
  tens <-
    c("twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety")
  names(tens) <- 2:9
  sign_mult <- ifelse(x < 0, -1, 1) 
  x <- x * sign_mult
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1)
     x <- trim(sapply(x, helper))
  else 
    x <- helper(x)
  
  sign_word <- ifelse(sign_mult < 0, "negative ", "")
  return(paste0(sign_word, x))
}
